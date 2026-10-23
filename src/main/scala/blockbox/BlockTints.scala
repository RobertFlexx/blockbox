package blockbox

/** Biome colours for the grey tint-mask textures: grass tops, the grass-side fringe and leaves. The
  * mesher passes the local grass colour; everything drawn outside the world (inventory icons, the
  * held block, break particles) uses plains.
  */
object BlockTints:
  val plainsGrass: (Float, Float, Float) = (0.52f, 0.76f, 0.34f)

  def isFoliage(block: Block): Boolean =
    block == Block.Leaves || block == Block.BirchLeaves || block == Block.PineLeaves || block == Block.AcaciaLeaves

  /** Birch and spruce keep fixed colours like Minecraft; oak and acacia follow the biome. */
  def foliage(block: Block, grass: (Float, Float, Float)): (Float, Float, Float) = block match
    case Block.BirchLeaves  => (0.55f, 0.70f, 0.33f)
    case Block.PineLeaves   => (0.38f, 0.57f, 0.40f)
    case Block.AcaciaLeaves =>
      (grass._1 * 0.45f + 0.34f, grass._2 * 0.45f + 0.35f, grass._3 * 0.45f + 0.10f)
    case _ => (grass._1 * 0.62f + 0.13f, grass._2 * 0.62f + 0.24f, grass._3 * 0.62f + 0.06f)

  /** The colour a tint mask was authored against, used to turn coloured art into masks. */
  def reference(block: Block): (Float, Float, Float) =
    if isFoliage(block) then foliage(block, plainsGrass) else plainsGrass

  def item(block: Block, face: FaceKind): (Float, Float, Float) =
    if isFoliage(block) then foliage(block, plainsGrass)
    else if block == Block.Grass && face == FaceKind.Top then plainsGrass
    else (1f, 1f, 1f)
