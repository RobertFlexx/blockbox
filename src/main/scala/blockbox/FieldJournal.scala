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
  private var pickTier = 0
  private var diamonds = 0
  private var zombies = 0
  private var torches = 0
  private var glassSmelted = 0
  private var trailLights = 0
  private var walking = 0f
  private var swimming = 0f
  private var victories = 0
  private var claimed = 0
  private var tracked = 0
  private var caches = 0
  def salvagedCache(): Unit = caches = (caches + 1).min(3)
  def writeDiscoveries(out: DataOutputStream): Unit = out.writeInt(caches)
  def readDiscoveries(in: DataInputStream): Unit = caches = in.readInt().max(0).min(3)

  def observe(biome: Int, depth: Int, survival: Boolean, count: Block => Int): Unit =
    if biome >= 0 && biome < TerrainBiome.values.length then biomes += biome
    deepest = deepest.max(depth).min(128)
    if survival then
      timber = timber
        .max(Seq(Block.Wood, Block.BirchWood, Block.PineWood, Block.AcaciaWood).map(count).sum)
        .min(8)
      furnace = furnace.max(count(Block.Furnace)).min(1)
      iron = iron.max(count(Block.IronIngot)).min(1)
      ToolSystem.materials.zipWithIndex.foreach { case ((_, items), tier) =>
        if items.exists(count(_) > 0) then pickTier = pickTier.max(tier + 1)
      }
      diamonds = diamonds.max(count(Block.DiamondGem)).min(3)
      torches = torches.max(count(Block.Torch)).min(4)

  def collected(block: Block, amount: Int): Unit =
    if Block.isLog(block) then timber = (timber + amount.max(0)).min(8)
    else if block == Block.Furnace then furnace = 1
    else if block == Block.IronIngot then iron = 1
    else if block == Block.DiamondGem then diamonds = (diamonds + amount.max(0)).min(3)
    else if block == Block.Torch then torches = (torches + amount.max(0)).min(4)
    ToolSystem.spec(block).foreach(t => pickTier = pickTier.max(t.tier))

  def defeatedZombie(): Unit =
    zombies = (zombies + 1).min(3)
    victories = (victories + 1).min(20)

  def placedBlock(block: Block = Block.Air): Unit =
    placed = (placed + 1).min(24)
    if block == Block.Torch then trailLights = (trailLights + 1).min(12)

  def smelted(block: Block, amount: Int): Unit =
    if block == Block.Glass then glassSmelted = (glassSmelted + amount.max(0).min(64)).min(8)

  /** Ignore teleports, flight and invalid deltas; callers supply real movement. */
  def travel(distance: Float, wet: Boolean): Unit =
    if distance.isFinite && distance > 0f && distance <= 8f then
      if wet then swimming = (swimming + distance).min(150f)
      else walking = (walking + distance).min(1000f)

  def pageCount: Int = (entries.length + 5) / 6
  def track(index: Int): Unit = if index >= 0 && index < entries.length then tracked = index
  def currentObjective: Option[JournalEntry] =
    val all = entries
    all.lift(tracked).filterNot(_.completed).orElse(all.find(e => !e.completed))
  private val rewards = Vector(
    (Block.Planks, 16),
    (Block.Glass, 8),
    (Block.CoalLump, 4),
    (Block.Torch, 8),
    (Block.IronIngot, 2),
    (Block.RainbowBlock, 8),
    (Block.Stick, 8),
    (Block.Torch, 8),
    (Block.IronIngot, 4),
    (Block.CoalLump, 4),
    (Block.DiamondGem, 1),
    (Block.IronIngot, 2),
    (Block.Glass, 16),
    (Block.Torch, 12),
    (Block.RainbowBlock, 8),
    (Block.Glass, 16),
    (Block.GoldIngot, 4),
    (Block.RainbowBlock, 16),
    (Block.GoldIngot, 8)
  )
  def reward(index: Int): Option[(Block, Int)] = rewards.lift(index)
  def rewardClaimed(index: Int): Boolean =
    index >= 0 && index < rewards.length && (claimed & (1 << index)) != 0
  def claim(index: Int, room: Block => Int): Option[(Block, Int)] =
    reward(index)
      .filter { (block, amount) =>
        entries.lift(index).exists(_.completed) && !rewardClaimed(index) && room(block) >= amount
      }
      .map { reward => claimed |= 1 << index; reward }

  def entries: Vector[JournalEntry] = Vector(
    JournalEntry("Gather your bearings", "Collect 8 logs, then turn them into planks.", timber, 8),
    JournalEntry("A place to stay", "Place 24 blocks to build your first shelter.", placed, 24),
    JournalEntry(
      "Start a workshop",
      "8 stone in a ring -> furnace. Place it, then right click.",
      furnace,
      1
    ),
    JournalEntry(
      "Into the hillside",
      "Follow a cave at least 20 blocks below the surface.",
      deepest,
      20
    ),
    JournalEntry("The iron trail", "Smelt iron ore using coal or wood as fuel.", iron, 1),
    JournalEntry("Beyond the familiar", "Discover 6 different surface biomes.", biomes.size, 6),
    JournalEntry(
      "Your first tools",
      "2 planks vertically -> sticks. 3 planks across + 2 sticks -> pick.",
      pickTier.min(1),
      1
    ),
    JournalEntry(
      "The stone age",
      "Use a wooden pick on stone. Swap the head for stone.",
      pickTier.min(2),
      2
    ),
    JournalEntry(
      "Forged in fire",
      "Smelt iron. 3 ingots across, 2 sticks below -> iron pick.",
      pickTier.min(3),
      3
    ),
    JournalEntry(
      "A light in the deep",
      "Coal above a stick -> 4 torches. Light your mine.",
      torches,
      4
    ),
    JournalEntry("Diamond country", "Iron pick required. Find 3 diamonds below Y 20.", diamonds, 3),
    JournalEntry(
      "Hold back the night",
      "2 planks, stone or ingots over a stick -> sword. Defeat 3 zombies.",
      zombies,
      3
    ),
    JournalEntry(
      "A clear view",
      "Smelt 8 sand into glass for windows and greenhouses.",
      glassSmelted,
      8
    ),
    JournalEntry(
      "Leave a light on",
      "Place 12 torches to mark a safe route home.",
      trailLights,
      12
    ),
    JournalEntry(
      "The long way home",
      "Walk 1,000 blocks. Find a new place to build.",
      walking.toInt,
      1000
    ),
    JournalEntry(
      "Across the water",
      "Swim 150 blocks along rivers and coastlines.",
      swimming.toInt,
      150
    ),
    JournalEntry("Night watch", "Defeat 20 zombies and keep your settlement safe.", victories, 20),
    JournalEntry(
      "Diamond work",
      "Craft a diamond tool or weapon to complete your workshop.",
      pickTier.min(4),
      4
    ),
    JournalEntry(
      "Salvage the old world",
      "Find ruined masonry and break 3 sealed supply caches.",
      caches,
      3
    )
  )

  def nextAnnouncement(): Option[String] =
    val all = entries
    val next = all.indices.find(i => all(i).completed && (announced & (1 << i)) == 0)
    next.map { i => announced |= 1 << i; all(i).title }

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
    if count < 0 || count > TerrainBiome.values.length then
      throw java.io.IOException("Invalid journal biome count")
    biomes.clear()
    var i = 0
    while i < count do
      val biome = in.readInt()
      if biome >= 0 && biome < TerrainBiome.values.length then biomes += biome
      i += 1

  /** Optional SUR1 tail; the original journal fields remain readable in old worlds. */
  def writeProgress(out: DataOutputStream): Unit =
    out.writeInt(pickTier); out.writeInt(diamonds); out.writeInt(zombies); out.writeInt(announced);
    out.writeInt(torches)

  def readProgress(in: DataInputStream): Unit =
    pickTier = in.readInt().max(0).min(4); diamonds = in.readInt().max(0).min(3);
    zombies = in.readInt().max(0).min(3)
    announced = in.readInt() & 4095
    torches = in.readInt().max(0).min(4)
    victories = zombies

  /** ADV1 is appended after SUR1, so older world readers can ignore it. */
  def writeAdventure(out: DataOutputStream): Unit =
    out.writeInt(glassSmelted); out.writeInt(trailLights)
    out.writeFloat(walking); out.writeFloat(swimming); out.writeInt(victories)
    out.writeInt(claimed); out.writeInt(tracked); out.writeInt(announced)

  def readAdventure(in: DataInputStream): Unit =
    glassSmelted = in.readInt().max(0).min(8); trailLights = in.readInt().max(0).min(12)
    val walk = in.readFloat(); val swim = in.readFloat()
    require(walk.isFinite && swim.isFinite, "Invalid journal travel progress")
    walking = walk.max(0).min(1000); swimming = swim.max(0).min(150)
    victories = in.readInt().max(0).min(20)
    val mask = (1 << entries.length) - 1
    claimed = in.readInt() & mask; tracked = in.readInt().max(0).min(entries.length - 1)
    announced = in.readInt() & mask
