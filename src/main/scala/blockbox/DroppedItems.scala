package blockbox

import scala.collection.mutable
import blockbox.net.BlockboxNet

/** Host-owned item physics and atomic pickup reservations. Rendering never awards inventory. */
final class DroppedItems:
  import DroppedItems.*
  val entities = mutable.LinkedHashMap.empty[Int, Entity]
  private val offers = mutable.LinkedHashMap.empty[Long, Offer]
  private var serial = 1
  private var receipt = 1L
  def clear(): Unit = { entities.clear(); offers.clear(); serial = 1; receipt = 1 }
  def canDrop(block: Block, count: Int, pos: Vec3): Boolean =
    canDropAll(Vector(block -> count), pos)
  def canDropAll(stacks: Seq[(Block, Int)], pos: Vec3): Boolean =
    if !finite(pos) || stacks.exists((block, count) => !valid(block) || count <= 0) then false
    else
      val required = stacks
        .groupMapReduce(_._1)(_._2.toLong)(_ + _)
        .iterator
        .map { (block, count) =>
          val spare = entities.valuesIterator
            .filter(e => e.block == block && (e.pos - pos).lengthSquared < 0.8f)
            .map(e => 64 - e.count)
            .sum
          ((count - spare).max(0L) + 63L) / 64L
        }
        .sum
      required <= (maxEntities - entities.size).max(0)

  def spawn(
      block: Block,
      count: Int,
      position: Vec3,
      velocity: Vec3 = Vec3(0, 1.8f, 0),
      owner: String = ""
  ): Option[Entity] =
    if count <= 0 || count > 64 || !finite(velocity) || !canDrop(block, count, position) then None
    else
      var remaining = count
      var result: Entity = null
      entities.valuesIterator
        .filter(e => e.block == block && e.count < 64 && (e.pos - position).lengthSquared < 0.8f)
        .foreach { e =>
          if remaining > 0 then
            val amount = remaining.min(64 - e.count)
            e.count += amount; remaining -= amount; e.age = 0f; e.delay = 0.65f; result = e
        }
      if remaining > 0 then
        val e = Entity(
          serial,
          block,
          remaining,
          position,
          position,
          velocity,
          0f,
          if owner.isEmpty then 0.45f else 1.1f
        )
        serial += 1; entities(e.id) = e; result = e
      Option(result)

  def update(dt: Float, at: (Int, Int, Int) => Block, loaded: Vec3 => Boolean): Unit =
    require(dt.isFinite && dt >= 0, "Invalid item tick")
    var remaining = dt.min(0.5f)
    while remaining > 0 do
      val step = remaining.min(0.025f); remaining -= step
      entities.valuesIterator.foreach { e =>
        e.age += step; e.delay = (e.delay - step).max(0)
        val resting =
          e.grounded && e.velocity.x * e.velocity.x + e.velocity.z * e.velocity.z < 0.0001f && at(
            math.floor(e.pos.x).toInt,
            math.floor(e.pos.y - 0.15f).toInt,
            math.floor(e.pos.z).toInt
          ).solid
        if !resting && loaded(e.pos) then
          val wet = at(
            math.floor(e.pos.x).toInt,
            math.floor(e.pos.y).toInt,
            math.floor(e.pos.z).toInt
          ) == Block.Water
          val drag = math.exp(-step * (if wet then 4f else if e.grounded then 8f else 1f)).toFloat
          var waterTop = math.floor(e.pos.y).toInt
          if wet then
            while waterTop < Terrain.worldHeight - 1 && at(
                math.floor(e.pos.x).toInt,
                waterTop + 1,
                math.floor(e.pos.z).toInt
              ) == Block.Water
            do waterTop += 1
          val buoyancy = if wet then
            (waterTop.toFloat + WaterSurface.fullHeight - 0.04f - e.pos.y) * 12f - e.velocity.y * 5f
          else -18f
          e.velocity = Vec3(
            e.velocity.x * drag,
            (e.velocity.y + step * buoyancy).max(-18f).min(if wet then 1.3f else 18f),
            e.velocity.z * drag
          )
          def blocked(p: Vec3): Boolean =
            val r = 0.12f
            (for
              x <- Vector(p.x - r, p.x + r); y <- Vector(p.y - r, p.y + r);
              z <- Vector(p.z - r, p.z + r)
            yield at(math.floor(x).toInt, math.floor(y).toInt, math.floor(z).toInt).solid)
              .exists(identity)
          val dx = Vec3(e.velocity.x * step, 0, 0)
          if !blocked(e.pos + dx) then e.pos += dx
          else e.velocity = Vec3(0, e.velocity.y, e.velocity.z)
          val dz = Vec3(0, 0, e.velocity.z * step)
          if !blocked(e.pos + dz) then e.pos += dz
          else e.velocity = Vec3(e.velocity.x, e.velocity.y, 0)
          val dy = Vec3(0, e.velocity.y * step, 0)
          if !blocked(e.pos + dy) then { e.pos += dy; e.grounded = false }
          else
            e.grounded = e.velocity.y < 0
            e.velocity = Vec3(e.velocity.x, 0, e.velocity.z)
      }
    entities.filterInPlace((_, e) => e.age < 300f && e.pos.y > -16)

  def reserve(player: String, id: Int, maximum: Int, feet: Vec3): Option[Offer] =
    offers.valuesIterator.find(_.player.equalsIgnoreCase(player)).orElse {
      if offers.size >= 64 then None
      else
        entities
          .get(id)
          .filter(e => e.delay <= 0 && maximum > 0 && (e.pos - feet).lengthSquared < 2.8f * 2.8f)
          .map { e =>
            val amount = maximum.min(e.count).min(64)
            val offer = Offer(receipt, player, e.block, amount, e.pos); receipt += 1
            e.count -= amount
            if e.count == 0 then entities.remove(id)
            offers(offer.id) = offer
            offer
          }
    }
  def forPlayer(player: String): Vector[Offer] =
    offers.valuesIterator.filter(_.player.equalsIgnoreCase(player)).toVector
  def resolve(player: String, id: Long, accepted: Boolean): Unit =
    offers.get(id).filter(_.player.equalsIgnoreCase(player)).foreach { offer =>
      if accepted || spawn(offer.block, offer.count, offer.position).nonEmpty then offers.remove(id)
    }
  def snapshot: String = "ITEMS|" + entities.valuesIterator
    .map { e =>
      s"${e.id},${e.block.ordinal},${e.count},${e.pos.x},${e.pos.y},${e.pos.z},${e.age},${e.delay}"
    }
    .mkString(";")
  def saveSnapshot: String =
    val live = entities.values.toVector
    val reserved = offers.values.toVector.zipWithIndex.map { (o, i) =>
      Entity(serial + i, o.block, o.count, o.position, o.position, Vec3(0, 0, 0), 0, 0.5f)
    }
    "ITEMS|" + (live ++ reserved)
      .take(maxEntities + 64)
      .map(e =>
        s"${e.id},${e.block.ordinal},${e.count},${e.pos.x},${e.pos.y},${e.pos.z},${e.age},${e.delay}"
      )
      .mkString(";")
  def receive(snapshot: String, interpolate: Boolean): Unit =
    require(snapshot.startsWith("ITEMS|"), "Invalid item snapshot")
    val rows = snapshot.stripPrefix("ITEMS|").split(';').filter(_.nonEmpty)
    require(rows.length <= maxEntities + 64, "Too many item entities")
    val parsed = rows.toVector.map { row =>
      val p = row.split(','); require(p.length == 8)
      val id = BlockboxNet.parseInt(p(0), 1, Int.MaxValue - 1)
      val block = Block.fromOrdinal(BlockboxNet.parseInt(p(1), 0, Block.values.length - 1));
      require(valid(block))
      val count = BlockboxNet.parseInt(p(2), 1, 64)
      val pos = Vec3(
        BlockboxNet.parseFloat(p(3)),
        BlockboxNet.parseFloat(p(4)),
        BlockboxNet.parseFloat(p(5))
      )
      val age = BlockboxNet.parseFloat(p(6)); val delay = BlockboxNet.parseFloat(p(7))
      require(
        finite(pos) && math.abs(pos.x) <= 30000000 && math.abs(
          pos.z
        ) <= 30000000 && age >= 0 && age <= 300 && delay >= 0 && delay <= 2
      )
      Entity(id, block, count, pos, pos, Vec3(0, 0, 0), age, delay)
    }
    require(parsed.map(_.id).distinct.length == parsed.length, "Duplicate item ids")
    val alive = parsed.map(_.id).toSet
    entities.filterInPlace((id, _) => alive(id))
    parsed.foreach { e =>
      entities.get(e.id) match
        case Some(old) if interpolate =>
          old.target = e.pos; old.count = e.count; old.delay = e.delay
        case _ => entities(e.id) = e
      serial = serial.max(e.id + 1)
    }

object DroppedItems:
  val maxEntities = 512
  def messages(snapshot: String, sequence: Long): Vector[String] =
    val rows = snapshot.stripPrefix("ITEMS|").split(';').filter(_.nonEmpty).toVector
    Vector(s"ITEMBEGIN|$sequence|${rows.length}") ++ rows
      .grouped(48)
      .map(group => s"ITEMPART|$sequence|${group.mkString(";")}")
      .toVector ++ Vector(s"ITEMEND|$sequence")
  final case class Entity(
      id: Int,
      block: Block,
      var count: Int,
      var pos: Vec3,
      var target: Vec3,
      var velocity: Vec3,
      var age: Float,
      var delay: Float
  ):
    var grounded = false
  final case class Offer(id: Long, player: String, block: Block, count: Int, position: Vec3)
  def valid(block: Block): Boolean =
    block != Block.Air && block != Block.Water && block != Block.Bedrock && block != Block.FurnaceLit && CustomBlocks
      .registered(block)
  private def finite(v: Vec3): Boolean = v.x.isFinite && v.y.isFinite && v.z.isFinite
