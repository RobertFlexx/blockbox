package blockbox

/** Item capabilities shared by crafting, mining, combat, networking and rendering. */
object ToolSystem:
  enum Kind:
    case Pickaxe, Axe, Shovel, Sword
  final case class Tool(kind: Kind, tier: Int, speed: Float, damage: Float)
  val materials: Vector[(Block, Vector[Block])] = Vector(
    Block.Planks -> Vector(
      Block.WoodenPickaxe,
      Block.WoodenAxe,
      Block.WoodenShovel,
      Block.WoodenSword
    ),
    Block.Stone -> Vector(Block.StonePickaxe, Block.StoneAxe, Block.StoneShovel, Block.StoneSword),
    Block.IronIngot -> Vector(Block.IronPickaxe, Block.IronAxe, Block.IronShovel, Block.IronSword),
    Block.DiamondGem -> Vector(
      Block.DiamondPickaxe,
      Block.DiamondAxe,
      Block.DiamondShovel,
      Block.DiamondSword
    )
  )
  private val tools = materials.zipWithIndex.flatMap { case ((_, items), tier) =>
    items.zipWithIndex.map { case (item, kind) =>
      item -> Tool(
        Kind.fromOrdinal(kind),
        tier + 1,
        Vector(2.5f, 4.5f, 7f, 10f)(tier),
        if kind == 3 then Vector(4f, 5f, 6f, 8f)(tier)
        else if kind == 1 then 3f + tier
        else 2f + tier
      )
    }
  }.toMap
  def spec(item: Block): Option[Tool] = tools.get(item)
  def isTool(item: Block): Boolean = tools.contains(item)
  def isPlanks(block: Block): Boolean =
    block == Block.Planks || block == Block.BirchPlanks || block == Block.PinePlanks || block == Block.AcaciaPlanks

  def effectiveKind(block: Block): Option[Kind] = block match
    case Block.Stone | Block.Coal | Block.Copper | Block.IronOre | Block.GoldOre | Block.Diamond |
        Block.Brick | Block.Furnace | Block.FurnaceLit =>
      Some(Kind.Pickaxe)
    case b if Block.isLog(b) || isPlanks(b) || b == Block.SupplyCrate    => Some(Kind.Axe)
    case Block.Dirt | Block.Grass | Block.Sand | Block.Snow | Block.Clay => Some(Kind.Shovel)
    case Block.Leaves | Block.BirchLeaves | Block.PineLeaves | Block.AcaciaLeaves =>
      Some(Kind.Sword)
    case _ => None

  def requiredTier(block: Block): Int = block match
    case Block.GoldOre | Block.Diamond                                             => 3
    case Block.IronOre | Block.Copper                                              => 2
    case Block.Stone | Block.Coal | Block.Brick | Block.Furnace | Block.FurnaceLit => 1
    case Block.Bedrock                                                             => Int.MaxValue
    case _                                                                         => 0

  def canHarvest(block: Block, item: Block): Boolean =
    val required = requiredTier(block)
    required == 0 || spec(item).exists(t => t.kind == Kind.Pickaxe && t.tier >= required)

  def miningSpeed(block: Block, item: Block): Float =
    spec(item).filter(t => effectiveKind(block).contains(t.kind)).map(_.speed).getOrElse(1f) *
      (if requiredTier(block) > 0 && !canHarvest(block, item) then 0.24f else 1f)

  def damage(item: Block): Float = spec(item).map(_.damage).getOrElse(1f)
  def drop(block: Block): (Block, Int) = block match
    case Block.Grass       => (Block.Dirt, 1)
    case Block.SupplyCrate => (Block.Planks, 4)
    case Block.Coal        => (Block.CoalLump, 1)
    case Block.Diamond     => (Block.DiamondGem, 1)
    case Block.FurnaceLit  => (Block.Furnace, 1)
    case Block.Glass       => (Block.Air, 0)
    case _                 => (block, 1)

/** Breathing is simulation state, so pausing or drawing extra frames cannot consume air. */
final class AirSupply:
  val capacity = 15f
  var remaining = capacity
  private var drowning = 0f
  def reset(): Unit = { remaining = capacity; drowning = 0f }
  def update(dt: Float, submerged: Boolean, survival: Boolean): Float =
    if !survival then { reset(); return 0f }
    val elapsed = dt.max(0f)
    if !submerged then
      remaining = (remaining + elapsed * 5f).min(capacity); drowning = 0f
      0f
    else
      val exhausted = (elapsed - remaining).max(0f)
      remaining = (remaining - elapsed).max(0f)
      drowning += exhausted
      val ticks = math.floor(drowning).toInt
      drowning -= ticks
      ticks * 2f
  def bubbles: Int = math.ceil(remaining / capacity * 10).toInt.max(0).min(10)

object WaterAppearance:
  /** Beer-Lambert absorption: red disappears first, leaving a blue-green silhouette. */
  def tint(light: (Float, Float, Float), depth: Float): (Float, Float, Float) =
    val d = depth.max(0f).min(24f)
    (
      light._1 * math.exp(-d * 0.24).toFloat,
      light._2 * math.exp(-d * 0.085).toFloat,
      light._3 * math.exp(-d * 0.035).toFloat
    )
