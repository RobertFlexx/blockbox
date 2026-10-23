package blockbox

import scala.collection.mutable

/** Bounded, host-owned decay. Unloaded neighbors are conservatively treated as support. */
final class LeafDecay:
  private final case class Pending(position: (Int, Int, Int), due: Float)
  private val queue = mutable.Queue.empty[Pending]
  private val queued = mutable.HashSet.empty[(Int, Int, Int)]
  private var clock = 0f
  def clear(): Unit = { queue.clear(); queued.clear(); clock = 0 }
  def write(out: java.io.DataOutputStream): Unit =
    out.writeInt(queue.size)
    queue.foreach { p =>
      out.writeInt(p.position._1); out.writeInt(p.position._2); out.writeInt(p.position._3)
      out.writeFloat((p.due - clock).max(0).min(8))
    }
  def read(in: java.io.DataInputStream): Unit =
    val count = in.readInt(); require(count >= 0 && count <= 8192)
    val saved = Vector.fill(count) {
      val x = in.readInt(); val y = in.readInt(); val z = in.readInt(); val delay = in.readFloat()
      require(
        math.abs(x.toLong) <= 30000000 && math.abs(z.toLong) <= 30000000 &&
          y >= 0 && y < Terrain.worldHeight && delay.isFinite && delay >= 0 && delay <= 8
      )
      Pending((x, y, z), delay)
    }
    clear(); saved.foreach { p => if queued.add(p.position) then queue.enqueue(p) }
  def disturb(x: Int, y: Int, z: Int, at: (Int, Int, Int) => Block): Unit =
    for
      dx <- -6 to 6; dy <- -6 to 6; dz <- -6 to 6
      if math.abs(dx) + math.abs(dy) + math.abs(dz) <= 7 && queued.size < 8192
    do
      val p = (x + dx, y + dy, z + dz)
      if BlockTints.isFoliage(at(p._1, p._2, p._3)) && queued.add(p) then
        val jitter = (TerrainUv.rotation(p._1 + p._2, p._3) + 1) * 0.7f
        queue.enqueue(Pending(p, clock + 1.5f + jitter))

  def supported(
      x: Int,
      y: Int,
      z: Int,
      at: (Int, Int, Int) => Block,
      loaded: (Int, Int) => Boolean
  ): Boolean =
    val seen = mutable.HashSet((x, y, z))
    val search = mutable.Queue((x, y, z, 0))
    while search.nonEmpty do
      val (px, py, pz, distance) = search.dequeue()
      var direction = 0
      while direction < LeafDecay.directions.length do
        val (dx, dy, dz) = LeafDecay.directions(direction)
        val nx = px + dx; val ny = py + dy; val nz = pz + dz
        if !loaded(nx, nz) then return true
        val block = at(nx, ny, nz)
        if Block.isLog(block) then return true
        if distance < 5 && BlockTints.isFoliage(block) && seen.add((nx, ny, nz)) then
          search.enqueue((nx, ny, nz, distance + 1))
        direction += 1
    false

  def update(
      dt: Float,
      at: (Int, Int, Int) => Block,
      loaded: (Int, Int) => Boolean,
      persistent: (Int, Int, Int) => Boolean,
      remove: (Int, Int, Int) => Unit
  ): Unit =
    clock += dt.max(0).min(0.5f)
    val checks = queue.size.min(24)
    var removed = 0
    for _ <- 0 until checks do
      val pending = queue.dequeue()
      val (x, y, z) = pending.position
      if pending.due > clock || removed >= 2 || !loaded(x, z) then queue.enqueue(pending)
      else
        queued.remove(pending.position)
        if BlockTints.isFoliage(at(x, y, z)) && !persistent(x, y, z) &&
          !supported(x, y, z, at, loaded)
        then
          remove(x, y, z); removed += 1
          for (dx, dy, dz) <- LeafDecay.directions do
            val p = (x + dx, y + dy, z + dz)
            if queued.size < 8192 && BlockTints.isFoliage(at(p._1, p._2, p._3)) && queued.add(p)
            then queue.enqueue(Pending(p, clock + 1.5f))

object LeafDecay:
  val directions: Vector[(Int, Int, Int)] =
    Vector((1, 0, 0), (-1, 0, 0), (0, 1, 0), (0, -1, 0), (0, 0, 1), (0, 0, -1))
