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
  def foliage(block: Block, grass: (Float, Float, Float)): (Float, Float, Float) =
    val out = foliageScratch.get()
    writeFoliage(block, grass._1, grass._2, grass._3, out, 0)
    (out(0), out(1), out(2))

  /** Same colour as `foliage`, written into `out` so chunk meshing does not allocate a tuple per leaf. */
  def writeFoliage(
      block: Block,
      grassR: Float,
      grassG: Float,
      grassB: Float,
      out: Array[Float],
      offset: Int
  ): Unit =
    block match
      case Block.BirchLeaves =>
        out(offset) = 0.55f; out(offset + 1) = 0.70f; out(offset + 2) = 0.33f
      case Block.PineLeaves =>
        out(offset) = 0.38f; out(offset + 1) = 0.57f; out(offset + 2) = 0.40f
      case Block.AcaciaLeaves =>
        out(offset) = grassR * 0.45f + 0.34f
        out(offset + 1) = grassG * 0.45f + 0.35f
        out(offset + 2) = grassB * 0.45f + 0.10f
      case _ =>
        out(offset) = grassR * 0.62f + 0.13f
        out(offset + 1) = grassG * 0.62f + 0.24f
        out(offset + 2) = grassB * 0.62f + 0.06f

  private val foliageScratch = ThreadLocal.withInitial(() => new Array[Float](3))

  /** The colour a tint mask was authored against, used to turn coloured art into masks. */
  def reference(block: Block): (Float, Float, Float) =
    if isFoliage(block) then foliage(block, plainsGrass) else plainsGrass

  def item(block: Block, face: FaceKind): (Float, Float, Float) =
    if isFoliage(block) then foliage(block, plainsGrass)
    else if FloraModel.grassTinted(block) then plainsGrass
    else if block == Block.Grass && face == FaceKind.Top then plainsGrass
    else (1f, 1f, 1f)
