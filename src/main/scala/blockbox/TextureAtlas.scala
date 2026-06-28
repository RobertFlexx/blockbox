package blockbox

import org.lwjgl.BufferUtils
import org.lwjgl.opengl.GL11.*
import java.awt.image.BufferedImage
import java.io.File
import javax.imageio.ImageIO
import scala.math.*

final class TextureAtlas(texturePack: TexturePack):
  val tileSize = 16
  private val faceCount = FaceKind.values.length
  private val columns = 16
  private val tileCount = Block.values.length * faceCount
  private val rows = (tileCount + columns - 1) / columns
  private val textureWidth = columns * tileSize
  private val textureHeight = rows * tileSize
  private var textureId = 0
  private val imageCache = scala.collection.mutable.HashMap.empty[String, Option[BufferedImage]]

  build()

  def bind(): Unit = glBindTexture(GL_TEXTURE_2D, textureId)

  def destroy(): Unit =
    if textureId != 0 then glDeleteTextures(textureId)
    textureId = 0

  def uv(block: Block, face: FaceKind, u: Float, v: Float): (Float, Float) =
    val tile = block.ordinal * faceCount + face.ordinal
    val tx = tile % columns
    val ty = tile / columns
    val inset = 0.001f
    val u0 = (tx * tileSize + inset) / textureWidth
    val v0 = (ty * tileSize + inset) / textureHeight
    val u1 = ((tx + 1) * tileSize - inset) / textureWidth
    val v1 = ((ty + 1) * tileSize - inset) / textureHeight
    (u0 + (u1 - u0) * u, v0 + (v1 - v0) * v)

  private def build(): Unit =
    val pixels = BufferUtils.createByteBuffer(textureWidth * textureHeight * 4)
    for tile <- 0 until tileCount do
      val block = Block.values(tile / faceCount)
      val face = FaceKind.values(tile % faceCount)
      val ox = (tile % columns) * tileSize
      val oy = (tile / columns) * tileSize
      val source = imageFor(block, face)
      for py <- 0 until tileSize; px <- 0 until tileSize do
        val (r, g, b, a) = source match
          case Some(img) => imagePixel(img, px, py)
          case None => pixel(block, face, px, py)
        val index = ((oy + py) * textureWidth + (ox + px)) * 4
        pixels.put(index, r.toByte)
        pixels.put(index + 1, g.toByte)
        pixels.put(index + 2, b.toByte)
        pixels.put(index + 3, a.toByte)
    pixels.position(0)

    textureId = glGenTextures()
    glBindTexture(GL_TEXTURE_2D, textureId)
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST)
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST)
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP)
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP)
    glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, textureWidth, textureHeight, 0, GL_RGBA, GL_UNSIGNED_BYTE, pixels)
    glBindTexture(GL_TEXTURE_2D, 0)

  private def imageFor(block: Block, face: FaceKind): Option[BufferedImage] =
    if texturePack.legacy then None else textureNames(block, face).view.flatMap(loadTexture).headOption

  private def textureNames(block: Block, face: FaceKind): List[String] = block match
    case Block.Grass =>
      face match
        case FaceKind.Top => List("grass_top.png", "grass..png", "grass.png")
        case FaceKind.Bottom => List("dirt.png")
        case _ => List("grass_side.png")
    case Block.Dirt => List("dirt.png")
    case Block.Stone => List("stone.png")
    case Block.Sand => List("sand.png")
    case Block.Snow => List("snow.png")
    case Block.Water => List("water.png")
    case Block.Wood =>
      face match
        case FaceKind.Top | FaceKind.Bottom => List("log_top_bottom.png")
        case _ => List("log_side.png")
    case _ => Nil

  private def loadTexture(name: String): Option[BufferedImage] =
    imageCache.getOrElseUpdate(name, {
      val resource = Option(getClass.getResourceAsStream(s"/textures/$name"))
      val image = resource.flatMap { in =>
        try Option(ImageIO.read(in)) finally in.close()
      }.orElse(loadTextureFile(name))
      image
    })

  private def loadTextureFile(name: String): Option[BufferedImage] =
    val files = texturePack.dir.toList.flatMap { dir =>
      List(File(dir, name), File(File(dir, "textures"), name))
    }
    files.collectFirst { case file if file.exists() => ImageIO.read(file) }

  private def imagePixel(img: BufferedImage, x: Int, y: Int): (Int, Int, Int, Int) =
    val sx = (x * img.getWidth / tileSize).max(0).min(img.getWidth - 1)
    val sy = (y * img.getHeight / tileSize).max(0).min(img.getHeight - 1)
    val argb = img.getRGB(sx, sy)
    ((argb >>> 16) & 0xFF, (argb >>> 8) & 0xFF, argb & 0xFF, (argb >>> 24) & 0xFF)

  private def pixel(block: Block, face: FaceKind, x: Int, y: Int): (Int, Int, Int, Int) = block match
    case Block.Air => (0, 0, 0, 0)
    case Block.Grass =>
      face match
        case FaceKind.Top =>
          val base = if ((x ^ y) & 1) == 0 then 214 else 238
          val shade = if (x * 3 + y * 7) % 5 < 2 then -18 else 0
          val mask = (base + shade).max(172).min(248)
          (mask, mask, mask, 255)
        case FaceKind.Bottom => dirtPixel(x, y, 23)
        case _ =>
          if y < 4 then
            val shade = if ((x + y) & 1) == 0 then 0 else -10
            val mask = (224 + shade).max(184).min(242)
            (mask, mask, mask, 255)
          else dirtPixel(x, y, 24)
    case Block.Dirt => dirtPixel(x, y, 31)
    case Block.Stone =>
      val grid = (x % 8 == 0 || y % 8 == 0) && (x % 8 != 7 && y % 8 != 7)
      val crack = if noise(x, y, 41) > 210 then 12 else 0
      val base =
        if crack > 0 then (90 + crack, 92 + crack, 95 + crack)
        else if grid then (86, 88, 90)
        else if ((x / 4 + y / 4) % 2) == 0 then (120, 122, 125)
        else (105, 107, 110)
      vary(base, 8, noise(x, y, 42), 255)
    case Block.Sand =>
      val dot = if noise(x, y, 51) > 182 then 12 else 0
      (220 - dot, 204 - dot, 142 - dot, 255)
    case Block.Water =>
      // Clean, readable water texture with enough opacity for falling columns to read.
      val n1 = noise(x, y, 61)
      val n2 = noise(x + 9, y + 3, 62)
      val wave = ((sin((x + n1 * 0.02f) * 0.85f) + cos((y + n2 * 0.02f) * 0.70f)) * 0.5f + 1f) * 0.5f
      val stripe = if ((x + y * 2 + n1 / 48) % 13) == 0 then 8 else 0
      val sparkle = if n1 > 246 || n2 > 250 then 12 else 0
      val r = (18 + stripe / 4 + sparkle / 4).max(10).min(58)
      val g = (88 + (wave * 22f).toInt + stripe / 2 + sparkle / 4).max(64).min(140)
      val b = (180 + (wave * 34f).toInt + stripe + sparkle).max(145).min(245)
      val alpha = if face == FaceKind.Top then 238 else 232
      (r, g, b, alpha)
    case Block.Wood =>
      face match
        case FaceKind.Top | FaceKind.Bottom =>
          val dx = x - 8; val dy = y - 8
          val dist = sqrt((dx * dx + dy * dy).toDouble).toInt
          val ring = (dist + noise(x, y, 71) / 80) % 3 == 0
          val base = if ring then (105, 68, 32) else (140, 92, 44)
          vary(base, 10, noise(x, y, 72), 255)
        case _ =>
          val stripe = if x % 4 == 0 && y % 6 != 0 then (90, 54, 26) else (128, 76, 34)
          vary(stripe, 12, noise(x, y, 73), 255)
    case Block.BirchWood =>
      face match
        case FaceKind.Top | FaceKind.Bottom =>
          val dx = x - 8; val dy = y - 8
          val dist = sqrt((dx * dx + dy * dy).toDouble).toInt
          val ring = (dist + noise(x, y, 2071) / 72) % 3 == 0
          val base = if ring then (174, 144, 92) else (216, 194, 142)
          vary(base, 10, noise(x, y, 2072), 255)
        case _ =>
          val darkFleck = noise(x, y, 2073) > 214 && y % 5 != 0
          val barkLine = x == 2 || x == 11 || (x + noise(x, y, 2074) / 64) % 7 == 0
          if darkFleck then (74, 58, 44, 255)
          else if barkLine then vary((186, 174, 140), 8, noise(x, y, 2075), 255)
          else vary((226, 218, 184), 12, noise(x, y, 2076), 255)
    case Block.PineWood =>
      face match
        case FaceKind.Top | FaceKind.Bottom =>
          val dx = x - 8; val dy = y - 8
          val dist = sqrt((dx * dx + dy * dy).toDouble).toInt
          val ring = (dist + noise(x, y, 2081) / 76) % 3 == 0
          val base = if ring then (70, 42, 20) else (100, 62, 30)
          vary(base, 10, noise(x, y, 2082), 255)
        case _ =>
          val stripe = if x % 3 == 0 || noise(x, y, 2083) > 222 then (56, 34, 18) else (88, 52, 25)
          vary(stripe, 10, noise(x, y, 2084), 255)
    case Block.AcaciaWood =>
      face match
        case FaceKind.Top | FaceKind.Bottom =>
          val dx = x - 8; val dy = y - 8
          val dist = sqrt((dx * dx + dy * dy).toDouble).toInt
          val ring = (dist + noise(x, y, 2091) / 72) % 3 == 0
          val base = if ring then (134, 64, 30) else (180, 88, 38)
          vary(base, 12, noise(x, y, 2092), 255)
        case _ =>
          val stripe = if (x + y / 2) % 5 == 0 then (116, 54, 28) else (166, 82, 36)
          vary(stripe, 14, noise(x, y, 2093), 255)
    case Block.RainbowBlock =>
      val palette = Array(
        (238, 64, 72),
        (246, 145, 55),
        (250, 221, 70),
        (80, 205, 88),
        (64, 165, 245),
        (146, 92, 245),
        (238, 82, 196)
      )
      val band = Math.floorMod(x / 3 + y / 4 + face.ordinal * 2, palette.length)
      val sparkle = if noise(x, y, 2201 + face.ordinal) > 238 then 34 else 0
      val edge = if x == 0 || y == 0 || x == 15 || y == 15 then -28 else 0
      val (rr, gg, bb) = palette(band)
      ((rr + sparkle + edge).max(0).min(255), (gg + sparkle + edge).max(0).min(255), (bb + sparkle + edge).max(0).min(255), 255)
    case Block.Planks =>
      val seamY = y == 3 || y == 7 || y == 11
      val seamX = x == 0 || x == 15
      val plankBase = if seamY || seamX then (92, 60, 28) else (168, 116, 58)
      vary(plankBase, 12, noise(x, y, 81), 255)
    case Block.BirchPlanks =>
      val seamY = y == 3 || y == 7 || y == 11
      val seamX = x == 0 || x == 15
      val knot = noise(x, y, 2181) > 232
      val plankBase = if seamY || seamX then (138, 112, 74) else if knot then (172, 140, 94) else (206, 184, 132)
      vary(plankBase, 13, noise(x, y, 2182), 255)
    case Block.PinePlanks =>
      val seamY = y == 3 || y == 7 || y == 11
      val seamX = x == 0 || x == 15
      val grain = if (x * 3 + y + noise(x, y, 2191) / 64) % 9 == 0 then -18 else 0
      val plankBase = if seamY || seamX then (58, 34, 18) else (106 + grain, 68 + grain / 2, 34)
      vary(plankBase, 11, noise(x, y, 2192), 255)
    case Block.AcaciaPlanks =>
      val seamY = y == 3 || y == 7 || y == 11
      val seamX = x == 0 || x == 15
      val stripe = if (x + y / 2 + noise(x, y, 2201) / 80) % 6 == 0 then -20 else 0
      val plankBase = if seamY || seamX then (126, 58, 26) else (196 + stripe, 92 + stripe / 2, 42)
      vary(plankBase, 15, noise(x, y, 2202), 255)
    case Block.Leaves =>
      val hole = (x * 5 + y * 3) % 9 < 2 && x > 1 && x < 14 && y > 1 && y < 14
      val dark = (x * 7 + y * 11) % 13 < 5
      val base = if dark then (30, 80, 28) else (48, 132, 40)
      val (r, g, b, _) = vary(base, 15, noise(x, y, 93), if hole then 80 else 230)
      (r, g, b, if hole then 80 else 230)
    case Block.BirchLeaves =>
      val hole = (x * 5 + y * 3 + 2) % 10 < 2 && x > 1 && x < 14 && y > 1 && y < 14
      val dark = (x * 7 + y * 11) % 15 < 4
      val base = if dark then (46, 106, 34) else (92, 166, 58)
      val (r, g, b, _) = vary(base, 14, noise(x, y, 2101), if hole then 78 else 232)
      (r, g, b, if hole then 78 else 232)
    case Block.PineLeaves =>
      val hole = (x * 3 + y * 7 + noise(x, y, 2111) / 48) % 14 == 0 && x > 1 && x < 14 && y > 1 && y < 14
      val dark = (x * 5 + y * 13) % 17 < 8
      val base = if dark then (12, 58, 28) else (22, 92, 40)
      val (r, g, b, _) = vary(base, 11, noise(x, y, 2112), if hole then 72 else 236)
      (r, g, b, if hole then 72 else 236)
    case Block.AcaciaLeaves =>
      val hole = (x * 5 + y * 3 + 4) % 11 < 2 && x > 1 && x < 14 && y > 1 && y < 14
      val dark = (x * 7 + y * 9) % 16 < 5
      val base = if dark then (64, 94, 30) else (104, 132, 48)
      val (r, g, b, _) = vary(base, 15, noise(x, y, 2121), if hole then 78 else 228)
      (r, g, b, if hole then 78 else 228)
    case Block.Brick =>
      val off = if (y / 4) % 2 == 0 then 0 else 4
      val shiftedX = x + off
      val mortar = (y % 4 == 0) || (shiftedX % 8 == 0)
      val brick = ((y / 4) * 3 + (shiftedX / 8)) % 2 == 0
      if mortar then (68, 55, 50, 255)
      else if brick then vary((152, 48, 36), 16, noise(x, y, 101), 255)
      else vary((170, 62, 48), 18, noise(x, y, 102), 255)
    case Block.Snow =>
      val ice = if y > 10 && noise(x, y, 111) > 180 then 14 else 0
      vary((238 - ice, 242 - ice, 248 - ice), 6, noise(x, y, 112), 255)
    case Block.Clay => vary((168, 130, 100), 14, noise(x, y, 121), 255)
    case Block.Coal =>
      if noise(x, y, 131) > 180 then (38, 36, 38, 255)
      else if noise(x, y, 132) > 220 then (62, 64, 68, 255)
      else vary((50, 52, 55), 10, noise(x, y, 133), 255)
    case Block.Copper =>
      if noise(x, y, 141) > 170 then vary((210, 112, 48), 20, noise(x, y, 142), 255)
      else if noise(x, y, 143) > 238 then vary((52, 140, 120), 14, noise(x, y, 144), 255)
      else stonePixel(x, y, 145)
    case Block.Glass =>
      val edge = x == 0 || x == 15 || y == 0 || y == 15
      val streak = (x == y) || (x == 15 - y)
      if edge then (208, 238, 252, 140)
      else if streak then (190, 228, 248, 100)
      else (160, 218, 245, 72)
    case Block.Furnace =>
      val stoneBase = if noise(x, y, 1001) > 180 then (92, 86, 80) else (100, 94, 88)
      val (sbr, sbg, sbb) = stoneBase
      face match
        case FaceKind.Top | FaceKind.Bottom =>
          val brick = (x / 4 + y / 4) % 2 == 0
          val (tr, tg, tb) = if brick then (110, 100, 92) else (95, 88, 82)
          (tr, tg, tb, 255)
        case FaceKind.North | FaceKind.South =>
          val isFront = (x >= 3 && x <= 12 && y >= 4 && y <= 11)
          if isFront then
            val hot = if noise(x - 3, y, 1002) > 180 then 20 else 0
            (55 + hot, 50 + hot, 48 + hot, 255)
          else (sbr, sbg, sbb, 255)
        case _ => (sbr, sbg, sbb, 255)
    case Block.FurnaceLit =>
      val stoneBase = if noise(x, y, 1003) > 180 then (92, 86, 80) else (100, 94, 88)
      val (sbr, sbg, sbb) = stoneBase
      face match
        case FaceKind.Top | FaceKind.Bottom =>
          val brick = (x / 4 + y / 4) % 2 == 0
          val (tr, tg, tb) = if brick then (110, 100, 92) else (95, 88, 82)
          (tr, tg, tb, 255)
        case FaceKind.North | FaceKind.South =>
          val isFront = (x >= 3 && x <= 12 && y >= 4 && y <= 11)
          if isFront then
            val flicker = noise(x * 3, y * 3, 1004) / 3
            (180 + flicker, 90 + flicker, 20 + flicker, 255)
          else (sbr, sbg, sbb, 255)
        case _ => (sbr, sbg, sbb, 255)
    case Block.IronOre =>
      val ore = if noise(x, y, 1005) > 182 then (168, 120, 78) else (92, 70, 62)
      stonePixel(x, y, 1006) match
        case (r, g, b, a) =>
          val blend = if noise(x, y, 1007) > 200 then 0.6f else 0.0f
          ((r * (1 - blend) + ore._1 * blend).toInt.max(0).min(255),
           (g * (1 - blend) + ore._2 * blend).toInt.max(0).min(255),
           (b * (1 - blend) + ore._3 * blend).toInt.max(0).min(255), a)
    case Block.GoldOre =>
      val ore = if noise(x, y, 1008) > 176 then (200, 160, 40) else (160, 130, 50)
      stonePixel(x, y, 1009) match
        case (r, g, b, a) =>
          val blend = if noise(x, y, 1010) > 200 then 0.7f else 0.0f
          ((r * (1 - blend) + ore._1 * blend).toInt.max(0).min(255),
           (g * (1 - blend) + ore._2 * blend).toInt.max(0).min(255),
           (b * (1 - blend) + ore._3 * blend).toInt.max(0).min(255), a)
    case Block.Diamond =>
      val shimmer = (noise(x, y, 1011) / 3).toInt
      val base = (60 + shimmer, 210 + shimmer, 190 + shimmer)
      val bright = noise(x, y, 1012) > 220
      if bright then (120 + shimmer, 240 + shimmer, 230 + shimmer, 255)
      else (base._1, base._2, base._3, 255)
    case Block.IronIngot =>
      val edge = x <= 1 || x >= 14 || y <= 2 || y >= 13
      val shine = x + y < 10 || noise(x, y, 1015) > 230
      if edge then (120, 122, 120, 235)
      else if shine then (235, 238, 232, 245)
      else vary((178, 184, 178), 12, noise(x, y, 1016), 240)
    case Block.GoldIngot =>
      val edge = x <= 1 || x >= 14 || y <= 2 || y >= 13
      val shine = x + y < 9 || noise(x, y, 1017) > 224
      if edge then (155, 108, 20, 235)
      else if shine then (255, 230, 92, 245)
      else vary((220, 166, 38), 16, noise(x, y, 1018), 240)
    case Block.Cactus =>
      val rib = x == 3 || x == 8 || x == 13
      val thorn = (x * 7 + y * 11 + noise(x, y, 2024)) % 23 == 0
      if face == FaceKind.Top || face == FaceKind.Bottom then
        val ring = x == 0 || x == 15 || y == 0 || y == 15
        if ring then (28, 96, 40, 255) else vary((38, 130, 54), 12, noise(x, y, 2025), 255)
      else if thorn then (220, 235, 185, 255)
      else if rib then vary((24, 104, 42), 10, noise(x, y, 2026), 255)
      else vary((42, 150, 62), 14, noise(x, y, 2027), 255)
    case Block.Bedrock =>
      val crack = if noise(x, y, 1013) > 210 then 10 else 0
      val base = if noise(x, y, 1014) > 190 then (60 - crack, 52 - crack, 48 - crack) else (45 - crack, 38 - crack, 35 - crack)
      (base._1.max(20).min(90), base._2.max(16).min(80), base._3.max(14).min(75), 255)

  private def dirtPixel(x: Int, y: Int, seed: Int): (Int, Int, Int, Int) =
    val pebble = noise(x, y, seed + 1) > 220
    val base = if pebble then (113, 82, 51) else (101, 65, 36)
    vary(base, 22, noise(x, y, seed), 255)

  private def stonePixel(x: Int, y: Int, seed: Int): (Int, Int, Int, Int) =
    val mortar = x % 8 == 0 || y % 8 == 0
    val base =
      if mortar then (83, 84, 86)
      else if ((x / 4 + y / 4 + noise(x, y, seed) / 86) % 2) == 0 then (119, 121, 124)
      else (98, 100, 103)
    vary(base, 12, noise(x, y, seed + 1), 255)

  private def vary(base: (Int, Int, Int), amount: Int, n: Int, alpha: Int): (Int, Int, Int, Int) =
    val delta = ((n - 128) * amount) / 128
    (clampByte(base._1 + delta), clampByte(base._2 + delta), clampByte(base._3 + delta), alpha)

  private def noise(x: Int, y: Int, seed: Int): Int =
    var n = x * 374761393 + y * 668265263 + seed * 1442695041
    n = (n ^ (n >>> 13)) * 1274126177
    (n ^ (n >>> 16)) & 255

  private def clampByte(v: Int): Int = v.max(0).min(255)
