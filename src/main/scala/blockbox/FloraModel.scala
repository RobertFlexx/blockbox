package blockbox

/** Small, cached cutout meshes. Plants never become collision cubes or occlude their banks. */
object FloraModel:
  final case class Vertex(
      x: Float,
      y: Float,
      z: Float,
      nx: Float,
      ny: Float,
      nz: Float,
      u: Float,
      v: Float
  )
  val blocks: Set[Block] = Set(
    Block.Reeds,
    Block.LilyPad,
    Block.LeafLitter,
    Block.GrassTuft,
    Block.Fern,
    Block.Wildflower,
    Block.DryShrub
  )
  def contains(block: Block): Boolean =
    blocks.contains(block)
  def grassTinted(block: Block): Boolean = block == Block.GrassTuft || block == Block.Fern
  private val meshes = blocks.map(block => block -> build(block)).toMap
  private val variants = (for
    block <- blocks if block != Block.LilyPad && block != Block.LeafLitter
    variant <- 0 until 8
  yield
    val angle = variant * math.Pi / 4
    val c = math.cos(angle).toFloat; val s = math.sin(angle).toFloat
    val scale = if block == Block.Reeds then 1f else 0.68f + (variant % 4) * 0.09f
    val dx = ((variant * 3) % 5 - 2) * 0.025f
    val dz = ((variant * 7) % 5 - 2) * 0.025f
    (block, variant) -> meshes(block).map(v =>
      v.copy(
        x = 0.5f + ((v.x - 0.5f) * c - (v.z - 0.5f) * s) * 0.72f + dx,
        z = 0.5f + ((v.x - 0.5f) * s + (v.z - 0.5f) * c) * 0.72f + dz,
        y = v.y * scale,
        nx = v.nx * c - v.nz * s,
        nz = v.nx * s + v.nz * c
      )
    )
  ).toMap
  def vertices(block: Block, variant: Int = 0): Vector[Vertex] =
    variants.getOrElse((block, variant & 7), meshes.getOrElse(block, Vector.empty))
  def bounds(block: Block): TorchModel.Bounds =
    val offset = if block == Block.LilyPad then WaterSurface.fullHeight - 1f else 0f
    TorchModel.Bounds(
      0.05f,
      offset,
      0.05f,
      0.95f,
      offset + (if block == Block.LilyPad || block == Block.LeafLitter then 0.04f else 1f),
      0.95f
    )

  def supported(block: Block, x: Int, y: Int, z: Int, at: (Int, Int, Int) => Block): Boolean =
    if !contains(block) then true
    else
      val floor = at(x, y - 1, z)
      block match
        case Block.LilyPad => floor == Block.Water
        case Block.Reeds   =>
          floor == Block.Grass || floor == Block.Dirt || floor == Block.Clay || floor == Block.Sand
        case Block.GrassTuft | Block.Fern | Block.Wildflower =>
          floor == Block.Grass || floor == Block.Dirt
        case Block.DryShrub =>
          floor == Block.Clay || floor == Block.Sand || floor == Block.Dirt || floor == Block.Grass
        case _ => floor.solid && !floor.cutout && floor != Block.Cactus && !Block.isLog(floor)

  private def build(block: Block): Vector[Vertex] =
    val result = Vector.newBuilder[Vertex]
    def quad(points: Vector[Vec3], uv: Vector[(Float, Float)]): Unit =
      val a = points(1) - points(0); val b = points(2) - points(0)
      val normal =
        Vec3(a.y * b.z - a.z * b.y, a.z * b.x - a.x * b.z, a.x * b.y - a.y * b.x).normalized
      for i <- Vector(0, 1, 2, 2, 3, 0) do
        val p = points(i); val t = uv(i)
        result += Vertex(p.x, p.y, p.z, normal.x, normal.y, normal.z, t._1, t._2)
    val uv = Vector((0f, 1f), (1f, 1f), (1f, 0f), (0f, 0f))
    if block != Block.LilyPad && block != Block.LeafLitter then
      for sign <- Vector(-1f, 1f) do
        val p = Vector(
          Vec3(0.08f, 0f, 0.5f - 0.42f * sign),
          Vec3(0.92f, 0f, 0.5f + 0.42f * sign),
          Vec3(0.92f, 1f, 0.5f + 0.42f * sign),
          Vec3(0.08f, 1f, 0.5f - 0.42f * sign)
        )
        // Cutout passes already render both faces. Duplicate reversed coplanar
        // triangles would fight each other with opposite lighting normals.
        quad(p, uv)
    else
      val y = if block == Block.LilyPad then WaterSurface.fullHeight - 1f + 0.012f else 0.03125f
      val p = Vector(Vec3(0, y, 1), Vec3(1, y, 1), Vec3(1, y, 0), Vec3(0, y, 0))
      quad(p, uv)
    result.result()
