package blockbox

import java.io.{DataInputStream, DataOutputStream}
import scala.collection.mutable

final case class JournalEntry(title: String, hint: String, progress: Int, target: Int):
  def completed: Boolean = progress >= target

/** Per-world survival and exploration goals, with stable save fields. */
final class FieldJournal:
  private val biomes = mutable.Set.empty[Int]
  private var timber = 0
  private var furnace = 0
  private var iron = 0
  private var deepest = 0
  private var placed = 0
  private var announced = 0

  def observe(biome: Int, depth: Int, survival: Boolean, count: Block => Int): Unit =
    if biome >= 0 && biome < TerrainBiome.values.length then biomes += biome
    deepest = deepest.max(depth).min(128)
    if survival then
      timber = timber.max(Seq(Block.Wood,Block.BirchWood,Block.PineWood,Block.AcaciaWood).map(count).sum).min(8)
      furnace = furnace.max(count(Block.Furnace)).min(1)
      iron = iron.max(count(Block.IronIngot)).min(1)

  def collected(block: Block,amount: Int): Unit =
    if Block.isLog(block) then timber = (timber+amount.max(0)).min(8)
    else if block==Block.Furnace then furnace = 1
    else if block==Block.IronIngot then iron = 1

  def placedBlock(): Unit = placed = (placed+1).min(24)

  def entries: Vector[JournalEntry] = Vector(
    JournalEntry("Gather your bearings","Collect 8 logs, then turn them into planks.",timber,8),
    JournalEntry("A place to stay","Place 24 blocks to build your first shelter.",placed,24),
    JournalEntry("Start a workshop","Craft a furnace from stone in the 3x3 grid.",furnace,1),
    JournalEntry("Into the hillside","Follow a cave at least 20 blocks below the surface.",deepest,20),
    JournalEntry("The iron trail","Smelt iron ore using coal or wood as fuel.",iron,1),
    JournalEntry("Beyond the familiar","Discover 6 different surface biomes.",biomes.size,6)
  )

  def nextAnnouncement(): Option[String] =
    val all = entries
    val next = all.indices.find(i => all(i).completed && (announced & (1<<i)) == 0)
    next.map { i => announced |= 1<<i; all(i).title }

  def write(out: DataOutputStream): Unit =
    out.writeInt(timber); out.writeInt(furnace); out.writeInt(iron)
    out.writeInt(deepest); out.writeInt(placed); out.writeInt(announced)
    out.writeInt(biomes.size)
    biomes.toVector.sorted.foreach(out.writeInt)

  def read(in: DataInputStream): Unit =
    timber = in.readInt().max(0).min(8)
    furnace = in.readInt().max(0).min(1); iron = in.readInt().max(0).min(1)
    deepest = in.readInt().max(0).min(128); placed = in.readInt().max(0).min(24)
    announced = in.readInt() & 63
    val count = in.readInt()
    if count < 0 || count > TerrainBiome.values.length then throw java.io.IOException("Invalid journal biome count")
    biomes.clear()
    var i = 0
    while i < count do
      val biome = in.readInt()
      if biome >= 0 && biome < TerrainBiome.values.length then biomes += biome
      i += 1
