package blockbox

import scala.math.*

/** Procedural 16x16 block art for every block a texture pack does not supply.
  *
  * Materials are tileable value noise quantised onto small hand-picked palettes, which
  * keeps the clean look of Minecraft-style pixel art instead of per-pixel static. Grass
  * and leaves are grey masks that the mesher tints per biome. Ores are overlays drawn
  * on top of whatever stone texture the active pack uses.
  */
object BlockboxTextures:
  type Pixel = (Int, Int, Int, Int)
  private val Size = 16

  private def hash(x: Int, y: Int, seed: Int): Float =
    var n = x * 374761393 + y * 668265263 + seed * 1442695041
    n = (n ^ (n >>> 13)) * 1274126177
    ((n ^ (n >>> 16)) & 0xFFFF) / 65535f

  /** Value noise on a lattice of cellX x cellY pixels that wraps every 16 pixels. */
  private def noise(x: Int, y: Int, cellX: Int, cellY: Int, seed: Int): Float =
    val cx = Size / cellX; val cy = Size / cellY
    val fx = x.toFloat / cellX; val fy = y.toFloat / cellY
    val x0 = floor(fx).toInt; val y0 = floor(fy).toInt
    val tx = fx - x0; val ty = fy - y0
    val sx = tx * tx * (3f - 2f * tx); val sy = ty * ty * (3f - 2f * ty)
    def at(ix: Int, iy: Int) = hash(Math.floorMod(ix, cx), Math.floorMod(iy, cy), seed)
    val top = at(x0, y0) + (at(x0 + 1, y0) - at(x0, y0)) * sx
    val bottom = at(x0, y0 + 1) + (at(x0 + 1, y0 + 1) - at(x0, y0 + 1)) * sx
    top + (bottom - top) * sy

  private def fbm(x: Int, y: Int, seed: Int): Float =
    noise(x, y, 8, 8, seed) * 0.5f + noise(x, y, 4, 4, seed + 1) * 0.3f + noise(x, y, 2, 2, seed + 2) * 0.2f

  private def stretch(v: Float, contrast: Float): Float = (v - 0.5f) * contrast + 0.5f
  private def rgb(hex: Int, alpha: Int = 255): Pixel = ((hex >> 16) & 255, (hex >> 8) & 255, hex & 255, alpha)
  private def gray(v: Int, alpha: Int = 255): Pixel = (v, v, v, alpha)
  private def pick(palette: Array[Int], t: Float): Int =
    palette((t.max(0f).min(0.9999f) * palette.length).toInt)

  /** Procedural art for a block face, or None to fall back to the legacy generator. */
  def pixel(block: Block, face: FaceKind, x: Int, y: Int): Option[Pixel] =
    val vertical = face == FaceKind.Top || face == FaceKind.Bottom
    block match
      case Block.Stone => Some(stone(x, y))
      case Block.Dirt => Some(dirt(x, y))
      case Block.Grass =>
        if face == FaceKind.Top then Some(grassTop(x, y))
        else if face == FaceKind.Bottom then Some(dirt(x, y))
        else Some(grassSide(x, y))
      case Block.Sand => Some(sand(x, y))
      case Block.Snow => Some(snow(x, y))
      case Block.Clay => Some(clay(x, y))
      case Block.Bedrock => Some(bedrock(x, y))
      case Block.Brick => Some(brick(x, y))
      case Block.Glass => Some(glass(x, y))
      case Block.Planks => Some(planks(x, y, oakPlanks, 31))
      case Block.BirchPlanks => Some(planks(x, y, birchPlanks, 32))
      case Block.PinePlanks => Some(planks(x, y, sprucePlanks, 33))
      case Block.AcaciaPlanks => Some(planks(x, y, acaciaPlanks, 34))
      case Block.Wood => Some(if vertical then logTop(x, y, oakPlanks, oakBark, 41) else oakLog(x, y))
      case Block.BirchWood => Some(if vertical then logTop(x, y, birchPlanks, 0x4d4740, 42) else birchLog(x, y))
      case Block.PineWood => Some(if vertical then logTop(x, y, sprucePlanks, spruceBark(1), 43) else spruceLog(x, y))
      case Block.AcaciaWood => Some(if vertical then logTop(x, y, acaciaPlanks, acaciaBark(1), 44) else acaciaLog(x, y))
      case Block.Leaves => Some(leaves(x, y, 51, holes = 0.26f, cell = 4))
      case Block.BirchLeaves => Some(leaves(x, y, 52, holes = 0.22f, cell = 4))
      case Block.PineLeaves => Some(needles(x, y))
      case Block.AcaciaLeaves => Some(leaves(x, y, 54, holes = 0.30f, cell = 2))
      case Block.Cactus => Some(if vertical then cactusTop(x, y, face == FaceKind.Top) else cactusSide(x, y))
      case Block.Furnace => Some(furnace(x, y, face, lit = false))
      case Block.FurnaceLit => Some(furnace(x, y, face, lit = true))
      case _ => None

  def isOre(block: Block): Boolean =
    block == Block.Coal || block == Block.Copper || block == Block.IronOre || block == Block.GoldOre || block == Block.Diamond

  // ---- Stone family -------------------------------------------------------------------

  private val stonePalette = Array(0x5c5d60, 0x696a6d, 0x77787b, 0x848588, 0x909194)
  def stone(x: Int, y: Int): Pixel =
    val v = stretch(fbm(x, y, 11), 2.3f) + (hash(x, y, 12) - 0.5f) * 0.20f
    rgb(pick(stonePalette, v))

  private final case class Ore(dark: Int, mid: Int, light: Int, accent: Int, seed: Int, density: Float)
  private val ores: Map[Block, Ore] = Map(
    Block.Coal -> Ore(0x1b1b1d, 0x2c2c30, 0x47474c, 0x2c2c30, 61, 0.95f),
    Block.Copper -> Ore(0x7a3a1a, 0xbf6430, 0xe59557, 0x4f9c84, 62, 0.82f),
    Block.IronOre -> Ore(0x86604a, 0xc9a185, 0xebd2bd, 0xc9a185, 63, 0.78f),
    Block.GoldOre -> Ore(0xa27410, 0xeec232, 0xfff09a, 0xeec232, 64, 0.70f),
    Block.Diamond -> Ore(0x168479, 0x48ddcb, 0xd3fff7, 0x48ddcb, 65, 0.62f)
  )
  private val oreSpots = Array((1, 2, 0), (8, 1, 1), (12, 6, 2), (4, 7, 3), (9, 10, 0), (1, 12, 2), (5, 13, 1), (13, 13, 3))
  private val oreShapes = Array(
    Array((0, 0), (1, 0), (0, 1), (1, 1), (2, 1)),
    Array((1, 0), (0, 1), (1, 1), (2, 1), (1, 2)),
    Array((0, 0), (1, 0), (1, 1), (2, 1)),
    Array((1, 0), (2, 0), (0, 1), (1, 1), (1, 2), (2, 2))
  )
  private val oreMasks = scala.collection.concurrent.TrieMap.empty[Block, Array[Boolean]]
  private def oreMask(block: Block, ore: Ore): Array[Boolean] = oreMasks.getOrElseUpdate(block, {
    val mask = new Array[Boolean](Size * Size)
    oreSpots.zipWithIndex.foreach { case ((sx, sy, shape), i) =>
      if hash(i, 7, ore.seed) < ore.density then
        oreShapes((shape + (hash(i, 9, ore.seed) * 4).toInt) % oreShapes.length).foreach { case (dx, dy) =>
          mask(Math.floorMod(sy + dy, Size) * Size + Math.floorMod(sx + dx, Size)) = true
        }
    }
    mask
  })

  /** Ore pixels to paint over the pack's stone; None leaves the stone visible. */
  def oreOverlay(block: Block, x: Int, y: Int): Option[Pixel] =
    ores.get(block).flatMap { ore =>
      val mask = oreMask(block, ore)
      def in(px: Int, py: Int) = mask(Math.floorMod(py, Size) * Size + Math.floorMod(px, Size))
      if !in(x, y) then None
      else if !in(x - 1, y) || !in(x, y - 1) then Some(rgb(ore.light))
      else if !in(x + 1, y) || !in(x, y + 1) then Some(rgb(ore.dark))
      else Some(rgb(if hash(x, y, ore.seed + 5) > 0.72f then ore.accent else ore.mid))
    }

  private val bedrockPalette = Array(0x141415, 0x262628, 0x3a3a3d, 0x525256, 0x6c6c70)
  def bedrock(x: Int, y: Int): Pixel =
    rgb(pick(bedrockPalette, stretch(fbm(x, y, 71), 3.2f) + (hash(x, y, 72) - 0.5f) * 0.35f))

  // ---- Soil ---------------------------------------------------------------------------

  private val dirtPalette = Array(0x553620, 0x654128, 0x744b2f, 0x835637, 0x936241)
  def dirt(x: Int, y: Int): Pixel =
    if hash(x, y, 23) > 0.955f then rgb(0x9a8670)
    else rgb(pick(dirtPalette, stretch(fbm(x, y, 21), 2.0f) + (hash(x, y, 22) - 0.5f) * 0.40f))

  def grassTop(x: Int, y: Int): Pixel =
    val v = stretch(noise(x, y, 4, 4, 25) * 0.55f + noise(x, y, 2, 2, 26) * 0.45f, 2.2f) + (hash(x, y, 27) - 0.5f) * 0.45f
    gray(pick(Array(0x9a, 0xac, 0xbd, 0xcd, 0xdc), v))

  /** Dirt with a grey fringe in the top rows; the atlas tints only the fringe band. */
  def grassSide(x: Int, y: Int): Pixel =
    val drip = 2 + (hash(x, 0, 28) * 2.4f).toInt + (if hash(x, 1, 28) > 0.82f then 1 else 0)
    if y < drip then
      gray(pick(Array(0xa4, 0xb6, 0xc8, 0xd6), (hash(x, y, 29) + (if y == drip - 1 then -0.35f else 0f)).max(0f)))
    else dirt(x, y)

  private val sandPalette = Array(0xcdb682, 0xd6c08c, 0xdec997, 0xe5d2a2, 0xebdaad)
  def sand(x: Int, y: Int): Pixel =
    if hash(x, y, 33) > 0.94f then rgb(0xbba272)
    else rgb(pick(sandPalette, stretch(fbm(x, y, 31), 1.6f) + (hash(x, y, 32) - 0.5f) * 0.45f))

  private val snowPalette = Array(0xdfe6ee, 0xe9eef4, 0xf1f5f9, 0xf8fafc, 0xffffff)
  def snow(x: Int, y: Int): Pixel =
    rgb(pick(snowPalette, stretch(fbm(x, y, 35), 1.7f) + (hash(x, y, 36) - 0.5f) * 0.30f))

  // Warm terracotta: badlands use clay as their main terrain block.
  private val clayPalette = Array(0x8a553a, 0x976043, 0xa36a4a, 0xae7453, 0xb87e5c)
  def clay(x: Int, y: Int): Pixel =
    val band = noise(x, y, 16, 4, 81) - 0.5f
    rgb(pick(clayPalette, stretch(fbm(x, y, 82), 1.5f) + band * 0.45f + (hash(x, y, 83) - 0.5f) * 0.25f))

  // ---- Built blocks -------------------------------------------------------------------

  private val brickPalette = Array(0x7a2d21, 0x8f3828, 0xa24231, 0xb34e3b, 0xc15c47)
  def brick(x: Int, y: Int): Pixel =
    val row = y / 4
    val inRow = y % 4
    val shifted = x + (if row % 2 == 0 then 0 else 4)
    val bx = Math.floorMod(shifted, 8)
    if inRow == 3 || bx == 7 then rgb(pick(Array(0x8b857c, 0x99938a, 0xa59f95), hash(x, y, 91)))
    else
      val id = row * 3 + Math.floorDiv(shifted, 8)
      var t = 0.5f + (hash(id, 1, 92) - 0.5f) * 0.45f + (hash(x, y, 93) - 0.5f) * 0.30f
      if inRow == 0 || bx == 0 then t += 0.18f
      if inRow == 2 || bx == 6 then t -= 0.22f
      rgb(pick(brickPalette, t))

  def glass(x: Int, y: Int): Pixel =
    val edge = x == 0 || y == 0 || x == 15 || y == 15
    val corner = (x == 0 || x == 15) && (y == 0 || y == 15)
    val streak = (x + y == 5 && x >= 1 && x <= 4) || (x + y == 8 && x >= 2 && x <= 4) || (x + y == 22 && x >= 9 && x <= 13)
    if corner then rgb(0xb7cbd4, 235)
    else if edge then rgb(0xdbe9ef, 220)
    else if streak then rgb(0xf6fcff, 175)
    else rgb(0xcfe7f2, 34)

  private val oakPlanks = Array(0x5c4327, 0x765834, 0x8b693e, 0x9f7a49, 0xb08853)
  private val birchPlanks = Array(0x9b8459, 0xae976a, 0xbfa979, 0xcdb887, 0xd9c594)
  private val sprucePlanks = Array(0x3c2916, 0x4c341c, 0x5b3f23, 0x6a4a2a, 0x775432)
  private val acaciaPlanks = Array(0x86391b, 0xa04824, 0xb4552b, 0xc36132, 0xd06d3a)

  /** Four boards per tile with dark seams, staggered end joints and running grain. */
  def planks(x: Int, y: Int, palette: Array[Int], seed: Int): Pixel =
    val board = y / 4
    val row = y % 4
    val joint = Array(4, 11, 1, 8)(board)
    if row == 3 then rgb(palette(0))
    else if x == joint then rgb(palette(1))
    else
      var t = 0.52f + (hash(board, 2, seed) - 0.5f) * 0.32f + (noise(x, board * 4, 4, 4, seed) - 0.5f) * 0.42f
      if row == 0 then t += 0.10f
      if hash(x / 2, y, seed + 1) > 0.80f then t -= 0.28f
      rgb(pick(palette, t))

  // ---- Trees --------------------------------------------------------------------------

  private val oakBark = 0x4b3520
  def oakLog(x: Int, y: Int): Pixel =
    val v = stretch(noise(x, y, 2, 8, 101) * 0.65f + noise(x, y, 1, 4, 102) * 0.35f, 2.4f)
    rgb(pick(Array(0x3f2c1a, 0x4f3822, 0x5f452a, 0x6f5233, 0x7d5d3a), v + (hash(x, y, 103) - 0.5f) * 0.20f))

  def birchLog(x: Int, y: Int): Pixel =
    val mark = hash(x / 3, y, 111) > 0.80f && hash(x, y / 2, 112) > 0.30f
    if mark then rgb(if hash(x, y, 113) > 0.5f then 0x37322d else 0x4f4943)
    else rgb(pick(Array(0xc9c3b3, 0xd6d1c2, 0xe1ddd0, 0xebe8dd), stretch(noise(x, y, 4, 2, 114), 1.8f) + (hash(x, y, 115) - 0.5f) * 0.30f))

  private def spruceBark(i: Int): Int = Array(0x2b1c0f, 0x3a2715, 0x48311b, 0x553a21)(i)
  def spruceLog(x: Int, y: Int): Pixel =
    val v = stretch(noise(x, y, 2, 8, 121) * 0.70f + noise(x, y, 1, 2, 122) * 0.30f, 2.6f)
    rgb(spruceBark(((v.max(0f).min(0.999f)) * 4).toInt))

  private def acaciaBark(i: Int): Int = Array(0x4f4840, 0x5e564c, 0x6c6358, 0x7a7064)(i)
  def acaciaLog(x: Int, y: Int): Pixel =
    val crack = hash(x, y / 3, 131) > 0.86f
    if crack then rgb(0x3f3932)
    else rgb(acaciaBark(((stretch(noise(x, y, 2, 4, 132), 2.0f) + (hash(x, y, 133) - 0.5f) * 0.3f).max(0f).min(0.999f) * 4).toInt))

  /** End grain: growth rings in the plank colours inside a one-pixel bark rim. */
  def logTop(x: Int, y: Int, palette: Array[Int], bark: Int, seed: Int): Pixel =
    if x == 0 || y == 0 || x == 15 || y == 15 then rgb(bark)
    else
      val dx = x - 7.5f; val dy = y - 7.5f
      val r = sqrt(dx * dx + dy * dy).toFloat + (noise(x, y, 4, 4, seed) - 0.5f) * 1.4f
      val ring = (r * 0.75f) % 1f
      rgb(pick(palette, if ring < 0.28f then 0.22f else 0.70f + (hash(x, y, seed + 1) - 0.5f) * 0.25f))

  /** Grey leaf mask with see-through gaps; the mesher supplies the colour. */
  def leaves(x: Int, y: Int, seed: Int, holes: Float, cell: Int): Pixel =
    val v = stretch(noise(x, y, cell, cell, seed) * 0.55f + noise(x, y, 2, 2, seed + 1) * 0.45f, 2.4f)
    val h = hash(x, y, seed + 2)
    if v < holes && h < 0.80f then (0, 0, 0, 0)
    else gray(pick(Array(0x5c, 0x78, 0x93, 0xad, 0xc6), v + (h - 0.5f) * 0.32f))

  def needles(x: Int, y: Int): Pixel =
    val v = stretch(noise(x, y, 2, 4, 141) * 0.6f + noise(x, y, 4, 4, 142) * 0.4f, 2.4f)
    val h = hash(x, y, 143)
    if v < 0.15f && h < 0.70f then (0, 0, 0, 0)
    else gray(pick(Array(0x5a, 0x74, 0x8e, 0xa6, 0xbc), v + (h - 0.5f) * 0.16f))

  def cactusSide(x: Int, y: Int): Pixel =
    val rib = x == 2 || x == 7 || x == 12
    val spine = (x == 1 || x == 6 || x == 11 || x == 14) && Math.floorMod(y + x * 3, 5) == 0
    if spine then rgb(0xdbe5b0)
    else if rib then rgb(0x1f5a27)
    else if x == 0 || x == 15 then rgb(0x2a6c31)
    else rgb(pick(Array(0x2c7333, 0x35853c, 0x3e9545, 0x4aa250), stretch(noise(x, y, 2, 8, 151), 1.8f) + (hash(x, y, 152) - 0.5f) * 0.3f))

  def cactusTop(x: Int, y: Int, top: Boolean): Pixel =
    val edge = x == 0 || y == 0 || x == 15 || y == 15
    val inner = x >= 2 && x <= 13 && y >= 2 && y <= 13
    val cross = top && (x == 7 || x == 8 || y == 7 || y == 8) && inner
    if edge then rgb(0x245f2a)
    else if cross then rgb(0x5aae5e)
    else if !inner then rgb(0x2f7a36)
    else rgb(pick(Array(0x358a3d, 0x3d9746, 0x47a34f), hash(x, y, 153)))

  def furnace(x: Int, y: Int, face: FaceKind, lit: Boolean): Pixel =
    val edge = x == 0 || y == 0 || x == 15 || y == 15
    face match
      case FaceKind.Top | FaceKind.Bottom =>
        if edge then rgb(0x5b5b5d)
        else if x == 1 || y == 1 then rgb(0x9c9c9e)
        else rgb(pick(Array(0x7c7c7e, 0x858587, 0x8f8f91), hash(x, y, 161)))
      case FaceKind.North | FaceKind.South =>
        val slot = x >= 4 && x <= 11 && y >= 3 && y <= 4
        val mouth = x >= 4 && x <= 11 && y >= 8 && y <= 13
        if edge then rgb(0x515153)
        else if slot then rgb(0x202022)
        else if mouth then
          if !lit then rgb(if y == 8 then 0x141415 else 0x1d1d1f)
          else if y >= 11 then rgb(pick(Array(0xe0571b, 0xff9a26, 0xffd24a), hash(x, y, 162) * 0.6f + (y - 11) * 0.22f))
          else rgb(if hash(x, y, 163) > 0.6f then 0x8a3612 else 0x5a220c)
        else if (y == 7 || y == 14) && x >= 3 && x <= 12 then rgb(0x9c9c9e)
        else stone(x, y)
      case _ =>
        if edge then rgb(0x515153) else stone(x, y)
