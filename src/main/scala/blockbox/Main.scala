//> using scala "3.8.3"
//> using dep "org.lwjgl:lwjgl:3.4.1"
//> using dep "org.lwjgl:lwjgl-glfw:3.4.1"
//> using dep "org.lwjgl:lwjgl-opengl:3.4.1"
//> using dep "org.lwjgl:lwjgl-stb:3.4.1"
//> using dep "org.apache.groovy:groovy:5.0.6"

package blockbox

import org.lwjgl.BufferUtils
import org.lwjgl.glfw.GLFW.*
import org.lwjgl.glfw.GLFWErrorCallback
import org.lwjgl.glfw.GLFWFramebufferSizeCallback
import org.lwjgl.glfw.GLFWKeyCallback
import org.lwjgl.glfw.GLFWCharCallback
import org.lwjgl.glfw.GLFWScrollCallback
import org.lwjgl.opengl.GL
import org.lwjgl.opengl.GL11.*
import org.lwjgl.opengl.GL15.*
import org.lwjgl.stb.STBEasyFont
import org.lwjgl.system.MemoryUtil.NULL
import groovy.lang.GroovyClassLoader
import groovy.lang.Closure

import java.nio.FloatBuffer
import java.nio.charset.StandardCharsets
import java.security.MessageDigest
import java.util.jar.JarFile
import java.util.function.Consumer
import java.io.*
import java.net.*
import java.util.concurrent.LinkedBlockingQueue
import javax.sound.sampled.AudioFormat
import javax.sound.sampled.AudioSystem
import scala.collection.mutable.{ArrayBuffer, Queue, HashSet}
import scala.math.*
import scala.util.Random

@main def runBlockbox(): Unit = Blockbox().run()

final case class Vec3(x: Float, y: Float, z: Float):
  def +(o: Vec3): Vec3 = Vec3(x + o.x, y + o.y, z + o.z)
  def -(o: Vec3): Vec3 = Vec3(x - o.x, y - o.y, z - o.z)
  def *(s: Float): Vec3 = Vec3(x * s, y * s, z * s)
  def lengthSquared: Float = x * x + y * y + z * z
  def normalized: Vec3 =
    val len = sqrt(x * x + y * y + z * z).toFloat
    if len <= 0.0001f then Vec3(0, 0, 0) else Vec3(x / len, y / len, z / len)

enum Screen:
  case MainMenu, CreateWorld, LoadWorld, Mods, Settings, Playing, Paused, Inventory, Catalog, FurnaceUI, JoinGame, HostGame

enum GameMode:
  case Survival, Creative
  def toInt: Int = ordinal

enum Block(val solid: Boolean, val rgb: (Float, Float, Float), val translucent: Boolean = false, val liquid: Boolean = false, val cutout: Boolean = false):
  case Air extends Block(false, (0f, 0f, 0f), true)
  case Grass extends Block(true, (0.22f, 0.62f, 0.18f))
  case Dirt extends Block(true, (0.42f, 0.25f, 0.12f))
  case Stone extends Block(true, (0.46f, 0.47f, 0.49f))
  case Sand extends Block(true, (0.76f, 0.68f, 0.42f))
  case Water extends Block(false, (0.18f, 0.36f, 0.82f), true, true)
  case Wood extends Block(true, (0.47f, 0.28f, 0.12f))
  case Planks extends Block(true, (0.64f, 0.43f, 0.22f))
  case Leaves extends Block(true, (0.13f, 0.45f, 0.13f), false, false, true)
  case Brick extends Block(true, (0.62f, 0.20f, 0.15f))
  case Snow extends Block(true, (0.92f, 0.94f, 0.95f))
  case Clay extends Block(true, (0.48f, 0.33f, 0.27f))
  case Coal extends Block(true, (0.10f, 0.10f, 0.11f))
  case Copper extends Block(true, (0.75f, 0.38f, 0.18f))
  case Glass extends Block(true, (0.62f, 0.82f, 0.95f), true)
  case Furnace extends Block(true, (0.35f, 0.32f, 0.30f))
  case FurnaceLit extends Block(true, (0.45f, 0.34f, 0.22f))
  case IronOre extends Block(true, (0.72f, 0.58f, 0.42f))
  case GoldOre extends Block(true, (0.85f, 0.72f, 0.28f))
  case Diamond extends Block(true, (0.22f, 0.85f, 0.78f))
  case Bedrock extends Block(true, (0.12f, 0.12f, 0.12f))
  case IronIngot extends Block(false, (0.78f, 0.80f, 0.78f), true)
  case GoldIngot extends Block(false, (0.95f, 0.76f, 0.22f), true)
  case Cactus extends Block(true, (0.18f, 0.55f, 0.18f))
  case BirchWood extends Block(true, (0.78f, 0.72f, 0.58f))
  case BirchLeaves extends Block(true, (0.20f, 0.55f, 0.18f), false, false, true)
  case PineWood extends Block(true, (0.36f, 0.22f, 0.11f))
  case PineLeaves extends Block(true, (0.07f, 0.32f, 0.13f), false, false, true)
  case AcaciaWood extends Block(true, (0.64f, 0.34f, 0.16f))
  case AcaciaLeaves extends Block(true, (0.30f, 0.45f, 0.13f), false, false, true)
  // First official demo mod block. Appended so old world block ids stay stable.
  case RainbowBlock extends Block(true, (0.95f, 0.26f, 0.92f))
  // Variant planks are appended after the demo mod block so old block ids stay stable.
  case BirchPlanks extends Block(true, (0.76f, 0.63f, 0.42f))
  case PinePlanks extends Block(true, (0.44f, 0.28f, 0.14f))
  case AcaciaPlanks extends Block(true, (0.78f, 0.38f, 0.16f))
  def id: Byte = ordinal.toByte
object Block:
  private val valuesArray = values
  def normalizedName(value: String): String =
    Option(value).getOrElse("").trim
      .stripPrefix("Block.")
      .stripPrefix("blockbox:")
      .stripPrefix("minecraft:")
      .replace("_", "")
      .replace("-", "")
      .filter(_.isLetterOrDigit)
      .toLowerCase
  def fromId(id: Byte): Block =
    val i = id.toInt & 0xFF
    if i >= 0 && i < valuesArray.length then valuesArray(i) else Air
  def find(name: String): Option[Block] =
    val key = normalizedName(name)
    if key.isEmpty then None else valuesArray.find(b => normalizedName(b.toString) == key)
  def fromName(name: String): Block = find(name).getOrElse(Air)
  def isKnown(name: String): Boolean = find(name).nonEmpty
  def all: Array[Block] = valuesArray.clone()
  def names: Array[String] = valuesArray.map(_.toString)

enum FaceKind:
  case Top, Bottom, North, South, East, West

final case class RayHit(block: (Int, Int, Int), place: (Int, Int, Int), normal: (Int, Int, Int), distance: Float)
final case class RemotePlayer(name: String, var pos: Vec3, var yaw: Float, var pitch: Float, var lastSeen: Double, var colorId: Int)

final class TextureAtlas:
  val tileSize = 16
  private val faceCount = FaceKind.values.length
  private val columns = 16
  private val tileCount = Block.values.length * faceCount
  private val rows = (tileCount + columns - 1) / columns
  private val textureWidth = columns * tileSize
  private val textureHeight = rows * tileSize
  private var textureId = 0

  build()

  def bind(): Unit = glBindTexture(GL_TEXTURE_2D, textureId)

  def destroy(): Unit =
    if textureId != 0 then glDeleteTextures(textureId)
    textureId = 0

  def uv(block: Block, face: FaceKind, u: Float, v: Float): (Float, Float) =
    val tile = block.ordinal * faceCount + face.ordinal
    val tx = tile % columns
    val ty = tile / columns
    val inset = 2f
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
      for py <- 0 until tileSize; px <- 0 until tileSize do
        val (r, g, b, a) = pixel(block, face, px, py)
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

object Terrain:
  val seaLevel = 32
  val worldHeight = 128
  val chunkSize = 16

final class TerrainGenerator(val seed: Long):
  private val seedHash = (seed ^ (seed >>> 32)).toInt

  private def hash(x: Int, y: Int, z: Int, base: Int): Float =
    var n = x * 374761393 + y * 668265263 + z * 1442695041 + base * 1274126177 + seedHash * 60493
    n = (n ^ (n >>> 13)) * 1274126177
    ((n ^ (n >>> 16)) & 0x7fffffff).toFloat / 2147483647f

  private def valueNoise2D(x: Double, z: Double, base: Int): Float =
    val xi = floor(x).toInt; val zi = floor(z).toInt
    val xf = x - xi.toDouble; val zf = z - zi.toDouble
    val u = xf * xf * (3.0 - 2.0 * xf)
    val v = zf * zf * (3.0 - 2.0 * zf)
    val a = hash(xi, zi, 0, base).toDouble * 2.0 - 1.0
    val b = hash(xi + 1, zi, 0, base).toDouble * 2.0 - 1.0
    val c = hash(xi, zi + 1, 0, base).toDouble * 2.0 - 1.0
    val d = hash(xi + 1, zi + 1, 0, base).toDouble * 2.0 - 1.0
    val x1 = a + (b - a) * u
    val x2 = c + (d - c) * u
    (x1 + (x2 - x1) * v).toFloat

  private def fbm2D(x: Double, z: Double, octaves: Int, base: Int): Float =
    var total = 0.0; var amp = 1.0; var freq = 1.0; var norm = 0.0
    for _ <- 0 until octaves do
      total += amp * valueNoise2D(x * freq, z * freq, base).toDouble
      norm += amp; amp *= 0.52; freq *= 2.05
    (total / norm).toFloat

  private def valueNoise3D(x: Double, y: Double, z: Double, base: Int): Float =
    val xi = floor(x).toInt; val yi = floor(y).toInt; val zi = floor(z).toInt
    val xf = x - xi.toDouble; val yf = y - yi.toDouble; val zf = z - zi.toDouble
    val u = xf * xf * (3.0 - 2.0 * xf)
    val v = yf * yf * (3.0 - 2.0 * yf)
    val w = zf * zf * (3.0 - 2.0 * zf)
    val aaa = hash(xi, yi, zi, base).toDouble * 2.0 - 1.0; val baa = hash(xi+1, yi, zi, base).toDouble * 2.0 - 1.0
    val aba = hash(xi, yi+1, zi, base).toDouble * 2.0 - 1.0; val bba = hash(xi+1, yi+1, zi, base).toDouble * 2.0 - 1.0
    val aab = hash(xi, yi, zi+1, base).toDouble * 2.0 - 1.0; val bab = hash(xi+1, yi, zi+1, base).toDouble * 2.0 - 1.0
    val abb = hash(xi, yi+1, zi+1, base).toDouble * 2.0 - 1.0; val bbb = hash(xi+1, yi+1, zi+1, base).toDouble * 2.0 - 1.0
    val x11 = aaa + (baa - aaa) * u; val x12 = aba + (bba - aba) * u
    val x21 = aab + (bab - aab) * u; val x22 = abb + (bbb - abb) * u
    val y1 = x11 + (x12 - x11) * v; val y2 = x21 + (x22 - x21) * v
    (y1 + (y2 - y1) * w).toFloat

  private def ridgedNoise(x: Double, z: Double, base: Int, octaves: Int): Float =
    var total = 0.0; var amp = 1.0; var freq = 1.0; var norm = 0.0
    for _ <- 0 until octaves do
      val n = 1.0 - abs(valueNoise2D(x * freq, z * freq, base).toDouble)
      total += amp * n * n
      norm += amp; amp *= 0.5; freq *= 2.2
    (total / norm).toFloat

  private def moistureAt(x: Int, z: Int): Float =
    val fx = x.toDouble; val fz = z.toDouble
    // Broad, slow climate cells keep biomes coherent. A small local term breaks
    // hard borders without making snow/desert confetti.
    val broad = fbm2D((fx + 500.0) * 0.00165, (fz - 300.0) * 0.00165, 5, 51)
    val regional = fbm2D((fx - 930.0) * 0.0038, (fz + 760.0) * 0.0038, 4, 53) * 0.26f
    val local = fbm2D((fx - 180.0) * 0.0080, (fz + 90.0) * 0.0080, 3, 52) * 0.12f
    (broad + regional + local).max(-1f).min(1f)

  private def temperatureAt(x: Int, z: Int, h: Int): Float =
    val fx = x.toDouble; val fz = z.toDouble
    // Broad temperature bands make deserts and snow fields read as actual regions.
    // Height cools the value, so mountains can snow without covering normal plains.
    val climateMacro = fbm2D((fx + 50.0) * 0.00095, (fz + 50.0) * 0.00095, 5, 301)
    val climateRegional = fbm2D((fx - 620.0) * 0.0028, (fz + 240.0) * 0.0028, 4, 302) * 0.24f
    climateMacro + climateRegional - ((h - Terrain.seaLevel).toFloat / 158f)

  private def beachAt(h: Int): Boolean = h <= Terrain.seaLevel + 2

  private def coldRegionAt(x: Int, z: Int): Float =
    val broad = fbm2D((x - 2100.0) * 0.00078, (z + 900.0) * 0.00078, 5, 721)
    val regional = fbm2D((x + 740.0) * 0.0018, (z - 1200.0) * 0.0018, 4, 722) * 0.18f
    broad + regional

  private def desertRegionAt(x: Int, z: Int): Float =
    // Broad desert continents first, with only a small detail term. This prevents the old
    // sand-in-the-middle-of-plains look and makes deserts read as their own land.
    val broad = fbm2D((x + 1400.0) * 0.00058, (z - 900.0) * 0.00058, 5, 701)
    val regional = fbm2D((x - 400.0) * 0.00135, (z + 330.0) * 0.00135, 4, 702) * 0.18f
    val edge = fbm2D((x + 90.0) * 0.0032, (z - 260.0) * 0.0032, 3, 703) * 0.08f
    broad + regional + edge

  private def forestRegionAt(x: Int, z: Int): Float =
    val broad = fbm2D((x + 840.0) * 0.0026, (z - 310.0) * 0.0026, 4, 612)
    val groves = fbm2D((x - 340.0) * 0.0085, (z + 190.0) * 0.0085, 3, 613) * 0.16f
    broad + groves

  private def riverStrengthAt(x: Int, z: Int): Float =
    val fx = x.toDouble; val fz = z.toDouble
    val channelA = abs(fbm2D((fx + 260.0) * 0.0019, (fz - 530.0) * 0.0019, 5, 2401))
    val channelB = abs(fbm2D((fx - 930.0) * 0.0032, (fz + 760.0) * 0.0032, 4, 2402))
    val broadGate = smooth01(((fbm2D((fx + 80.0) * 0.00075, (fz - 110.0) * 0.00075, 4, 2403) + 0.42f) / 0.92f))
    val main = smooth01(((0.090f - channelA) / 0.090f))
    val branch = smooth01(((0.052f - channelB) / 0.052f)) * 0.62f
    ((main + branch) * broadGate).max(0f).min(1f)

  private def lakeStrengthAt(x: Int, z: Int): Float =
    val fx = x.toDouble; val fz = z.toDouble
    val basin = fbm2D((fx - 1700.0) * 0.00105, (fz + 980.0) * 0.00105, 5, 2501)
    val shape = fbm2D((fx + 310.0) * 0.0044, (fz - 630.0) * 0.0044, 3, 2502) * 0.22f
    smooth01(((basin + shape - 0.38f) / 0.34f))

  private def freshwaterAt(x: Int, z: Int, h: Int): Boolean =
    h <= Terrain.seaLevel + 7 && (riverStrengthAt(x, z) > 0.62f || lakeStrengthAt(x, z) > 0.58f)

  private def freshwaterShoreAt(x: Int, z: Int, h: Int): Boolean =
    h <= Terrain.seaLevel + 9 && (riverStrengthAt(x, z) > 0.40f || lakeStrengthAt(x, z) > 0.44f)

  private def waterSurfaceAt(x: Int, z: Int, h: Int): Int =
    // Keep every generated natural water body on one stable surface plane.
    // The old freshwater +1 level made rivers/lakes meet oceans at different
    // heights, which rendered as huge transparent sheets and stair-stepped water.
    Terrain.seaLevel

  private def coldAt(x: Int, z: Int, h: Int): Boolean =
    val t = temperatureAt(x, z, h)
    val coldRegion = coldRegionAt(x, z)
    // Snow should read as a real biome or alpine cap, not random high-elevation paint.
    val alpine = h > Terrain.seaLevel + 76 && t < -0.08f && coldRegion < 0.42f
    val snowField = t < -0.62f && coldRegion < 0.05f
    val deepSnow = t < -0.78f && coldRegion < 0.25f
    alpine || snowField || deepSnow

  private def desertAt(x: Int, z: Int, h: Int): Boolean =
    val m = moistureAt(x, z)
    val t = temperatureAt(x, z, h)
    val score = desertRegionAt(x, z)
    val coherentDesert = score > 0.16f && m < 0.08f && t > -0.06f
    val hotCore = score > 0.28f && m < 0.20f && t > 0.03f
    !beachAt(h) && !freshwaterAt(x, z, h) && h < Terrain.seaLevel + 52 && (coherentDesert || hotCore)

  private def savannaAt(x: Int, z: Int, h: Int): Boolean =
    !desertAt(x, z, h) && h > Terrain.seaLevel + 3 && h < Terrain.seaLevel + 58 && temperatureAt(x, z, h) > 0.06f && moistureAt(x, z) < 0.05f && desertRegionAt(x, z) > -0.12f

  private def birchGroveAt(x: Int, z: Int, h: Int): Boolean =
    !coldAt(x, z, h) && moistureAt(x, z) > 0.12f && forestRegionAt(x, z) > 0.18f && hash(Math.floorDiv(x, 22), 0, Math.floorDiv(z, 22), 6200) > 0.44f

  def savannaBlendAt(x: Int, z: Int, h: Int): Float =
    val t = temperatureAt(x, z, h)
    val m = moistureAt(x, z)
    val edgeNoise = fbm2D((x + 410.0) * 0.0022, (z - 260.0) * 0.0022, 3, 3301) * 0.09f
    val dry = smooth01((0.22f - m + edgeNoise) / 0.50f)
    val warm = smooth01((t + 0.03f + edgeNoise * 0.35f) / 0.38f)
    val region = smooth01((desertRegionAt(x, z) + 0.18f + edgeNoise) / 0.62f)
    if desertAt(x, z, h) || h <= Terrain.seaLevel + 2 then 0f else (dry * warm * region).max(0f).min(1f)

  def birchBlendAt(x: Int, z: Int, h: Int): Float =
    val m = moistureAt(x, z)
    val forest = forestRegionAt(x, z)
    val edgeNoise = fbm2D((x - 720.0) * 0.0020, (z + 540.0) * 0.0020, 3, 3302) * 0.10f
    val wet = smooth01((m - 0.02f + edgeNoise) / 0.48f)
    val grove = smooth01((forest - 0.06f + edgeNoise) / 0.46f)
    val coolEnough = 1f - smooth01((temperatureAt(x, z, h) - 0.30f + edgeNoise * 0.30f) / 0.34f)
    if coldAt(x, z, h) || desertAt(x, z, h) || h <= Terrain.seaLevel + 2 then 0f else (wet * grove * coolEnough).max(0f).min(1f)

  def grassTintAt(x: Int, z: Int, h: Int): (Float, Float, Float) =
    val savanna = savannaBlendAt(x, z, h).max(0f).min(1f)
    val birch = (birchBlendAt(x, z, h) * (1f - savanna * 0.82f)).max(0f).min(1f)
    val plains = (1f - savanna * 0.86f - birch * 0.82f).max(0.18f)
    val cold = if coldAt(x, z, h) then 0.45f else 0f
    val total = (plains + savanna + birch).max(0.001f)
    // Stronger biome color anchors keep the grass blocks visibly different after
    // texture modulation and lighting: plains is rich green, birch is pale/lush,
    // and savanna is dry olive.
    var r = (plains * 0.42f + birch * 0.66f + savanna * 0.54f) / total
    var g = (plains * 0.82f + birch * 0.94f + savanna * 0.58f) / total
    var b = (plains * 0.25f + birch * 0.36f + savanna * 0.13f) / total
    if cold > 0f then
      val w = (cold * 0.82f).max(0f).min(1f)
      r = r * (1f - w) + 0.50f * w
      g = g * (1f - w) + 0.82f * w
      b = b * (1f - w) + 0.92f * w
    (r.max(0.30f).min(0.82f), g.max(0.44f).min(1.00f), b.max(0.10f).min(0.52f))

  private def rockyAt(x: Int, z: Int, h: Int): Boolean =
    if h < Terrain.seaLevel + 42 then false
    else
      val ridge = ridgedNoise((x + 310.0) * 0.0075, (z - 520.0) * 0.0075, 931, 3)
      val exposure = fbm2D((x - 120.0) * 0.014, (z + 270.0) * 0.014, 3, 932)
      h > Terrain.seaLevel + 90 || (ridge > 0.84f && exposure > 0.20f)

  private def localSlopeAt(x: Int, z: Int, h: Int): Int =
    val h1 = heightAt(x + 1, z)
    val h2 = heightAt(x - 1, z)
    val h3 = heightAt(x, z + 1)
    val h4 = heightAt(x, z - 1)
    max(max(abs(h - h1), abs(h - h2)), max(abs(h - h3), abs(h - h4)))

  private def cliffAt(x: Int, z: Int, h: Int): Boolean =
    h > Terrain.seaLevel + 34 && localSlopeAt(x, z, h) >= 5

  private def spruceAt(x: Int, z: Int, h: Int): Boolean =
    coldAt(x, z, h) && moistureAt(x, z) > -0.28f

  private def caveMouthDepthAt(x: Int, z: Int, h: Int): Int =
    if h <= Terrain.seaLevel + 8 || h >= Terrain.worldHeight - 16 then 0
    else
      // Sparse side-of-hill entrances. Wider irregular throats make caves read like
      // natural Minecraft-style hillside mouths instead of circular top-down gashes.
      val cellSize = 60
      val cellX = Math.floorDiv(x, cellSize)
      val cellZ = Math.floorDiv(z, cellSize)
      val gate = hash(cellX, cellZ, 0, 8600)
        if gate < 0.840f then 0
      else
        val anchorX = cellX * cellSize + 12 + (hash(cellX, 0, cellZ, 8601) * (cellSize - 24)).toInt
        val anchorZ = cellZ * cellSize + 12 + (hash(cellX, 1, cellZ, 8602) * (cellSize - 24)).toInt
        val dx = (x - anchorX).toFloat
        val dz = (z - anchorZ).toFloat
        val angle = hash(cellX, 4, cellZ, 8606) * Pi.toFloat * 2f
        val dirX = cos(angle).toFloat
        val dirZ = sin(angle).toFloat
        val forward = dx * dirX + dz * dirZ
        val side = abs(-dx * dirZ + dz * dirX)
        val length = 24f + hash(cellX, 5, cellZ, 8607) * 38f
        val throatRadius = 1.75f + hash(cellX, 6, cellZ, 8608) * 1.85f
        val taper = 1.58f - forward / (length * 1.75f)
        val slopeSignal = ridgedNoise((x + 120.0) * 0.006, (z - 280.0) * 0.006, 8604, 3)
        val raggedEdge = valueNoise2D((x + 300.0) * 0.12, (z - 170.0) * 0.12, 8611) * 0.38f
        val terrainOk = h > Terrain.seaLevel + 14 && (slopeSignal > 0.38f || h > Terrain.seaLevel + 38)
        if terrainOk && forward >= -2f && forward <= length && side <= throatRadius * taper.max(0.46f) + raggedEdge then
          val sideEdge = 1f - (side / throatRadius.max(0.001f)).min(1f)
          val entrance = 3.0f + sideEdge * 3.4f
          val slopedDown = forward.max(0f) * (0.28f + hash(cellX, 7, cellZ, 8609) * 0.20f)
          (entrance + slopedDown).toInt.max(2).min(32)
        else 0

  private def caveWaterSourceAt(x: Int, y: Int, z: Int): Boolean =
    if y <= 8 || y >= Terrain.seaLevel - 6 then false
    else
      val cell = 11
      val cx = Math.floorDiv(x, cell)
      val cy = Math.floorDiv(y, 5)
      val cz = Math.floorDiv(z, cell)
      hash(cx, cy, cz, 8700) > 0.976f && valueNoise3D((x + 500.0) * 0.026, (y - 40.0) * 0.022, (z - 300.0) * 0.026, 8701) > 0.14f

  private def caveNoise(x: Int, y: Int, z: Int, h: Int): Boolean =
    caveNoise(x, y, z, h, caveMouthDepthAt(x, z, h))

  private def caveNoise(x: Int, y: Int, z: Int, h: Int, mouthDepth: Int): Boolean =
    if y <= 4 || y >= Terrain.worldHeight - 4 then return false
    val depth = (h - y).max(0).toFloat
    val mouth = mouthDepth > 0 && depth >= 0f && depth <= mouthDepth.toFloat
    if mouth then
      val throat = valueNoise3D(x.toDouble * 0.024, y.toDouble * 0.030, z.toDouble * 0.024, 8610)
      val ceilingBreak = valueNoise3D((x + 90).toDouble * 0.055, (y - 20).toDouble * 0.045, (z + 40).toDouble * 0.055, 8612)
      return depth < 4f || throat > -0.52f || ceilingBreak > 0.38f || depth < mouthDepth.toFloat * 0.52f
    if y >= h - 10 then return false
    val lowEnough = y > 7 && depth > 13f
    val dx = x.toDouble; val dy = y.toDouble; val dz = z.toDouble
    val region = valueNoise3D(dx * 0.0037, dy * 0.0026, dz * 0.0037, 77)
    if region < -0.68f then return false
    val roomA = valueNoise3D(dx * 0.0080, dy * 0.0054, dz * 0.0080, 99)
    val roomB = valueNoise3D((dx + 700.0) * 0.0108, dy * 0.0079, (dz - 500.0) * 0.0108, 199)
    val roomC = valueNoise3D((dx - 230.0) * 0.0047, dy * 0.0035, (dz + 830.0) * 0.0047, 777)
    val cavern = lowEnough && depth > 20f && region > -0.26f && roomA > 0.20f && roomB > -0.34f && roomC > -0.38f
    val bigRoom = lowEnough && depth > 34f && region > -0.04f && roomA > 0.32f && roomB > -0.22f
    val giantPocket = lowEnough && depth > 46f && roomC > 0.28f && region > -0.06f && roomA > 0.00f
    val tubeA = abs(valueNoise2D(dx * 0.0055, dz * 0.0055, 333))
    val tubeB = abs(valueNoise2D((dx + 120.0) * 0.0064, (dz - 90.0) * 0.0064, 444))
    val verticalBand = valueNoise3D(dx * 0.0047, dy * 0.0104, dz * 0.0047, 555)
    val tubeC = abs(valueNoise2D((dx - 360.0) * 0.0044, (dz + 210.0) * 0.0044, 445))
    val tunnel = lowEnough && depth > 13f && y < Terrain.seaLevel + 70 && ((tubeA < 0.037f && verticalBand > -0.52f) || (tubeB < 0.033f && verticalBand > -0.45f) || (tubeC < 0.024f && verticalBand > -0.30f))
    cavern || bigRoom || giantPocket || tunnel

  def heightAt(x: Int, z: Int): Int =
    val fx = x.toDouble; val fz = z.toDouble

    // Minecraft-like terrain needs multiple scales working together: broad continents,
    // rolling local relief, occasional hills, rarer mountain chains, and carved low divots.
    // Keep all coordinate math in Double so far-from-spawn terrain stays stable.
    val warpX = fbm2D((fx + 810.0) * 0.00070, (fz - 410.0) * 0.00070, 4, 1201).toDouble * 96.0
    val warpZ = fbm2D((fx - 260.0) * 0.00070, (fz + 680.0) * 0.00070, 4, 1202).toDouble * 96.0
    val wx = fx + warpX
    val wz = fz + warpZ

    val continent = fbm2D(wx * 0.00092, wz * 0.00092, 6, 1).toDouble
    val regional = fbm2D((wx + 300.0) * 0.0022, (wz - 250.0) * 0.0022, 5, 151).toDouble * 12.0
    val localRoll = fbm2D((wx - 670.0) * 0.0088, (wz + 430.0) * 0.0088, 3, 153).toDouble * 5.2
    val detail = fbm2D((wx + 120.0) * 0.024, (wz - 210.0) * 0.024, 2, 154).toDouble * 2.4

    val hillMask = smooth01(((fbm2D((wx + 900.0) * 0.00165, (wz - 400.0) * 0.00165, 4, 101) + 0.48f) / 0.90f))
    val highlandMask = smooth01(((fbm2D((wx - 540.0) * 0.00130, (wz + 220.0) * 0.00130, 5, 1001) + 0.30f) / 0.82f))
    val mountainRegion = smooth01(((fbm2D((wx - 1500.0) * 0.00078, (wz + 700.0) * 0.00078, 5, 202) + 0.34f) / 0.80f))
    val mountainMask = mountainRegion * smooth01(((continent + 0.66) / 1.08).toFloat)

    val rolling = (ridgedNoise((wx - 110.0) * 0.0038, (wz + 210.0) * 0.0038, 171, 4).toDouble * 28.0 + localRoll) * hillMask.toDouble
    val highlands = ridgedNoise((wx + 130.0) * 0.0025, (wz - 710.0) * 0.0025, 1002, 5).toDouble * 27.0 * highlandMask.toDouble
    val mountainCore = ridgedNoise((wx + 410.0) * 0.0028, (wz - 70.0) * 0.0028, 201, 5).toDouble
    val mountainDetail = ridgedNoise((wx - 240.0) * 0.0072, (wz + 520.0) * 0.0072, 301, 3).toDouble * 12.0
    val peakShape = pow(mountainCore.max(0.001), 1.55)
    val mountains = (peakShape * 62.0 + mountainDetail) * mountainMask.toDouble

    // Divots and soft valleys break up the flat green-sheet look without creating ugly holes.
    val valley = abs(fbm2D((wx - 90.0) * 0.0027, (wz + 120.0) * 0.0027, 5, 251).toDouble)
    val riverish = smooth01((max(0.0, 0.132 - valley) / 0.132).toFloat).toDouble
    val riverCarve = riverStrengthAt(x, z).toDouble
    val lakeCarve = lakeStrengthAt(x, z).toDouble
    val valleyCut = riverish * (7.5 + 10.0 * mountainMask + 4.0 * highlandMask) + riverCarve * (5.0 + 7.0 * highlandMask) + lakeCarve * 4.5
    val dimple = smooth01((max(0.0, 0.095 - abs(fbm2D((wx + 70.0) * 0.0068, (wz - 190.0) * 0.0068, 4, 252).toDouble)) / 0.095).toFloat).toDouble
    val divots = dimple * (2.0 + 4.0 * hillMask)

    val desertish = desertRegionAt(x, z) > 0.10f && moistureAt(x, z) < 0.12f && temperatureAt(x, z, Terrain.seaLevel + 8) > -0.02f
    val dryRelief =
      if desertish then
        val dune = fbm2D((wx + 220.0) * 0.010, (wz - 180.0) * 0.010, 3, 811).toDouble * 4.2
        val mesaMask = smooth01(((fbm2D((wx - 600.0) * 0.00155, (wz + 330.0) * 0.00155, 4, 813) + 0.20f) / 0.70f)).toDouble
        val mesa = ridgedNoise((wx - 500.0) * 0.0048, (wz + 260.0) * 0.0048, 812, 3).toDouble * 12.0 * mesaMask
        dune + mesa
      else 0.0

    // Biome-specific relief, blended softly so there are no hard biome cliffs.
    val temp0 = temperatureAt(x, z, Terrain.seaLevel + 10)
    val moist0 = moistureAt(x, z)
    val savannaReliefBlend = smooth01((temp0 + 0.02f) / 0.32f).toDouble * smooth01((0.18f - moist0) / 0.42f).toDouble * smooth01((desertRegionAt(x, z) + 0.18f) / 0.62f).toDouble * (if desertish then 0.15 else 1.0)
    val birchReliefBlend = smooth01((moist0 - 0.04f) / 0.46f).toDouble * smooth01((forestRegionAt(x, z) - 0.04f) / 0.48f).toDouble * (1.0 - savannaReliefBlend.min(1.0) * 0.75)
    val savannaRoll = (ridgedNoise((wx + 320.0) * 0.0045, (wz - 160.0) * 0.0045, 824, 3).toDouble * 7.5 - 2.0) * savannaReliefBlend
    val birchRoll = fbm2D((wx - 130.0) * 0.0064, (wz + 360.0) * 0.0064, 3, 825).toDouble * 3.4 * birchReliefBlend

    val base =
      Terrain.seaLevel + 8.0 +
      continent * 22.0 +
      regional +
      rolling +
      highlands +
      mountains +
      dryRelief +
      savannaRoll +
      birchRoll +
      detail -
      valleyCut -
      divots

    base.round.toInt.max(1).min(Terrain.worldHeight - 8)

  def surfaceBlock(x: Int, y: Int, z: Int, h: Int): Block =
    val beach = beachAt(h)
    val freshwaterShore = freshwaterShoreAt(x, z, h)
    if beach || freshwaterShore || desertAt(x, z, h) then Block.Sand
    else if coldAt(x, z, h) then Block.Snow
    else if cliffAt(x, z, h) then Block.Stone
    else if rockyAt(x, z, h) && hash(x, h, z, 733) > 0.82f then Block.Stone
    else Block.Grass

  def fillBlock(x: Int, y: Int, z: Int, h: Int): Block =
    val beach = beachAt(h)
    val freshwaterShore = freshwaterShoreAt(x, z, h)
    val desert = desertAt(x, z, h)
    val surface = surfaceBlock(x, y, z, h)
    if y == h then surface
    else if y > h - (if desert then 2 else 3) && (beach || freshwaterShore || desert) then Block.Sand
    else if surface == Block.Stone && y > h - (if cliffAt(x, z, h) then 7 else 4) then Block.Stone
    else if y > h - 6 && cliffAt(x, z, h) && hash(x, y, z, 734) > 0.20f then Block.Stone
    else if y > h - 18 then Block.Dirt
    else if y < 6 && h > Terrain.seaLevel then Block.Stone
    else if y < h - 28 && y < 12 && hash(x, y, z, 17) > 0.92f then Block.Clay
    else if y < h - 28 && y < 60 && hash(x, y, z, 37) > 0.88f then Block.Coal
    else if y < h - 28 && y < 48 && hash(x, y, z, 71) > 0.91f then Block.Copper
    else if y < h - 28 && y < 40 && hash(x, y, z, 81) > 0.955f then Block.IronOre
    else if y < h - 28 && y < 30 && hash(x, y, z, 91) > 0.97f then Block.GoldOre
    else if y < h - 28 && y < 16 && hash(x, y, z, 101) > 0.985f then Block.Diamond
    else if y <= 4 then Block.Bedrock
    else Block.Stone

  def furnaceAt(x: Int, z: Int, h: Int, surface: Block): Option[(Int, Int, Int)] =
    // Furnaces should be player-made utility blocks, not random world decoration.
    None

  def treeAt(x: Int, z: Int, h: Int, surface: Block): Option[(Int, Int, Int, Int)] =
    if h <= Terrain.seaLevel + 2 || surface == Block.Sand || surface == Block.Stone || caveMouthDepthAt(x, z, h) > 0 then return None
    val forestNoise = forestRegionAt(x, z)
    val spruce = spruceAt(x, z, h)
    val savanna = savannaAt(x, z, h)
    val birch = birchGroveAt(x, z, h)
    val spacingCell = if spruce then 7 else if savanna then 10 else if birch then 8 else if forestNoise > 0.34f then 8 else 11
    val cellX = Math.floorDiv(x, spacingCell)
    val cellZ = Math.floorDiv(z, spacingCell)
    val cellGate = hash(cellX, cellZ, 0, 913)
    val treeChance = hash(x, z, 0, 19)
    val dense = forestNoise > 0.22f && moistureAt(x, z) > -0.42f
    val threshold =
      if spruce then 0.972f
      else if savanna then 0.976f
      else if birch then 0.974f
      else if dense then 0.978f
      else if forestNoise > -0.08f then 0.991f
      else 0.9970f
    val height =
      if spruce then 8 + (hash(x, z, 0, 42) * 5).toInt
      else if savanna then 5 + (hash(x, z, 0, 45) * 4).toInt
      else if birch then 6 + (hash(x, z, 0, 46) * 3).toInt
      else if hash(x, z, 0, 43) > 0.78f then 7 + (hash(x, z, 0, 44) * 4).toInt
      else 4 + (hash(x, z, 0, 41) * 4).toInt
    if cellGate > 0.44f && treeChance > threshold then Some((x, h + 1, z, height))
    else None

  def cactusAt(x: Int, z: Int, h: Int, surface: Block): Option[(Int, Int, Int, Int)] =
    if surface != Block.Sand || !desertAt(x, z, h) || h <= Terrain.seaLevel + 2 || caveMouthDepthAt(x, z, h) > 0 then None
    else
      val cellOk = hash(Math.floorDiv(x, 8), Math.floorDiv(z, 8), 0, 3033) > 0.66f
      val chance = hash(x, z, 0, 3034)
      if cellOk && chance > 0.982f then Some((x, h + 1, z, 2 + (hash(x, z, 0, 3035) * 3).toInt))
      else None

  def bushAt(x: Int, z: Int, h: Int, surface: Block): Option[(Int, Int, Int, Int)] =
    if surface != Block.Grass || h <= Terrain.seaLevel + 2 || coldAt(x, z, h) || cliffAt(x, z, h) || caveMouthDepthAt(x, z, h) > 0 then None
    else
      val grove = forestRegionAt(x, z)
      val cellOk = hash(Math.floorDiv(x, 9), Math.floorDiv(z, 9), 0, 4040) > 0.88f
      val chance = hash(x, z, 0, 4041)
      if grove > 0.02f && cellOk && chance > 0.994f then Some((x, h + 1, z, 1))
      else None

  def boulderAt(x: Int, z: Int, h: Int, surface: Block): Option[(Int, Int, Int, Int)] =
    if h <= Terrain.seaLevel + 4 || surface == Block.Sand || surface == Block.Snow || caveMouthDepthAt(x, z, h) > 0 then None
    else
      val rockyPatch = rockyAt(x, z, h) || fbm2D((x + 40.0) * 0.006, (z - 90.0) * 0.006, 3, 5050) > 0.42f
      val cellOk = hash(Math.floorDiv(x, 13), Math.floorDiv(z, 13), 0, 5051) > 0.86f
      val chance = hash(x, z, 0, 5052)
      if rockyPatch && cellOk && chance > 0.991f then Some((x, h + 1, z, if hash(x, z, 0, 5053) > 0.78f then 2 else 1))
      else None

  private def smooth01(v: Float): Float =
    val t = v.max(0f).min(1f); t * t * (3f - 2f * t)

  def fillChunkBlocks(cx: Int, cz: Int): Array[Byte] =
    val baseX = cx * Terrain.chunkSize
    val baseZ = cz * Terrain.chunkSize
    val endX = baseX + Terrain.chunkSize - 1
    val endZ = baseZ + Terrain.chunkSize - 1
    val blocks = new Array[Byte](Terrain.chunkSize * Terrain.worldHeight * Terrain.chunkSize)
    def idx(lx: Int, y: Int, lz: Int): Int = (y * Terrain.chunkSize + lz) * Terrain.chunkSize + lx

    // Cache a slightly expanded height field so cliff/surface decoration does not call
    // heightAt repeatedly. Complex terrain keeps quality, but chunk creation hitches less.
    val columnCount = Terrain.chunkSize * Terrain.chunkSize
    val heights = new Array[Int](columnCount)
    val surfaces = new Array[Block](columnCount)
    val desertFlags = new Array[Boolean](columnCount)
    val beachFlags = new Array[Boolean](columnCount)
    val shoreFlags = new Array[Boolean](columnCount)
    val cliffFlags = new Array[Boolean](columnCount)
    val waterLines = new Array[Int](columnCount)
    val mouthDepths = new Array[Int](columnCount)
    def cidx(lx: Int, lz: Int): Int = lz * Terrain.chunkSize + lx

    val extPad = 4
    val extSize = Terrain.chunkSize + extPad * 2 + 1
    val extHeights = new Array[Int](extSize * extSize)
    def eidx(ex: Int, ez: Int): Int = ez * extSize + ex
    var ex = 0
    while ex < extSize do
      var ez = 0
      while ez < extSize do
        val wx = baseX + ex - extPad
        val wz = baseZ + ez - extPad
        extHeights(eidx(ex, ez)) = heightAt(wx, wz)
        ez += 1
      ex += 1
    def hLocal(lx: Int, lz: Int): Int = extHeights(eidx(lx + extPad, lz + extPad))
    def cachedSlope(lx: Int, lz: Int, h: Int): Int =
      val h1 = hLocal(lx + 1, lz)
      val h2 = hLocal(lx - 1, lz)
      val h3 = hLocal(lx, lz + 1)
      val h4 = hLocal(lx, lz - 1)
      max(max(abs(h - h1), abs(h - h2)), max(abs(h - h3), abs(h - h4)))
    def cachedCliff(lx: Int, lz: Int, h: Int): Boolean = h > Terrain.seaLevel + 34 && cachedSlope(lx, lz, h) >= 5

    var lx = 0
    while lx < Terrain.chunkSize do
      val wx = baseX + lx
      var lz = 0
      while lz < Terrain.chunkSize do
        val wz = baseZ + lz
        val ci = cidx(lx, lz)
        val h = hLocal(lx, lz)
        val waterLine = waterSurfaceAt(wx, wz, h)
        val beach = beachAt(h)
        val shore = freshwaterShoreAt(wx, wz, h)
        val desert = desertAt(wx, wz, h)
        val cold = coldAt(wx, wz, h)
        val rocky = rockyAt(wx, wz, h)
        val cliff = cachedCliff(lx, lz, h)
        val surface =
          if beach || shore || desert then Block.Sand
          else if cold then Block.Snow
          else if cliff then Block.Stone
          else if rocky && hash(wx, h, wz, 733) > 0.82f then Block.Stone
          else Block.Grass
        heights(ci) = h
        surfaces(ci) = surface
        desertFlags(ci) = desert
        beachFlags(ci) = beach
        shoreFlags(ci) = shore
        cliffFlags(ci) = cliff
        waterLines(ci) = waterLine
        mouthDepths(ci) = caveMouthDepthAt(wx, wz, h)
        lz += 1
      lx += 1

    lx = 0
    while lx < Terrain.chunkSize do
      val wx = baseX + lx
      var lz = 0
      while lz < Terrain.chunkSize do
        val wz = baseZ + lz
        val ci = cidx(lx, lz)
        val h = heights(ci)
        val surface = surfaces(ci)
        val desert = desertFlags(ci)
        val beach = beachFlags(ci)
        val shore = shoreFlags(ci)
        val cliff = cliffFlags(ci)
        val mouthDepth = mouthDepths(ci)
        val waterLine = waterLines(ci)
        // Only visit columns up to real terrain/water height. This is the big cheap win:
        // do not loop through 256 air cells just to decide they are empty.
        val maxY = max(h, waterLine).min(Terrain.worldHeight - 1)
        var y = 1
        while y <= maxY do
          if y <= h then
            val block =
              if y == h then surface
              else if y > h - (if desert then 2 else 3) && (beach || shore || desert) then Block.Sand
              else if surface == Block.Stone && y > h - (if cliff then 7 else 4) then Block.Stone
              else if y > h - 6 && cliff && hash(wx, y, wz, 734) > 0.20f then Block.Stone
              else if y > h - 18 then Block.Dirt
              else if y < 6 && h > Terrain.seaLevel then Block.Stone
              else if y < h - 28 && y < 12 && hash(wx, y, wz, 17) > 0.92f then Block.Clay
              else if y < h - 28 && y < 60 && hash(wx, y, wz, 37) > 0.88f then Block.Coal
              else if y < h - 28 && y < 48 && hash(wx, y, wz, 71) > 0.91f then Block.Copper
              else if y < h - 28 && y < 40 && hash(wx, y, wz, 81) > 0.955f then Block.IronOre
              else if y < h - 28 && y < 30 && hash(wx, y, wz, 91) > 0.97f then Block.GoldOre
              else if y < h - 28 && y < 16 && hash(wx, y, wz, 101) > 0.985f then Block.Diamond
              else if y <= 4 then Block.Bedrock
              else Block.Stone
            val canCave = y > 4 && (y < h - 12 || (mouthDepth > 0 && y >= h - mouthDepth))
            val cave = canCave && caveNoise(wx, y, wz, h, mouthDepth)
            if !cave then blocks(idx(lx, y, lz)) = block.id
          else if y <= waterLine && y > h then
            blocks(idx(lx, y, lz)) = Block.Water.id
          y += 1
        lz += 1
      lx += 1

    // Fill vertical air pockets that are directly connected to natural surface water.
    // This fixes lakes/oceans looking like they have missing chunks or hollow blue panes
    // when caves carve into shallow lakebeds. It is a cheap column pass, not a runtime scan.
    lx = 0
    while lx < Terrain.chunkSize do
      var lz = 0
      while lz < Terrain.chunkSize do
        val ci = cidx(lx, lz)
        var y = waterLines(ci).min(Terrain.worldHeight - 2)
        var waterAbove = false
        while y >= 1 do
          val i = idx(lx, y, lz)
          val block = blocks(i)
          if block == Block.Water.id then waterAbove = true
          else if block == Block.Air.id && waterAbove then blocks(i) = Block.Water.id
          else if block != Block.Air.id then waterAbove = false
          y -= 1
        lz += 1
      lx += 1

    // Small dormant cave pools. Step by 2 vertically and use deterministic widening so it
    // looks natural without scanning every cave air cell at full resolution.
    lx = 0
    while lx < Terrain.chunkSize do
      val wx = baseX + lx
      var lz = 0
      while lz < Terrain.chunkSize do
        val wz = baseZ + lz
        val maxY = (Terrain.seaLevel - 6).min(Terrain.worldHeight - 3)
        var y = 8
        while y <= maxY do
          val i = idx(lx, y, lz)
          val below = idx(lx, y - 1, lz)
          if blocks(i) == Block.Air.id && blocks(below) != Block.Air.id && blocks(below) != Block.Water.id && caveWaterSourceAt(wx, y, wz) then
            blocks(i) = Block.Water.id
            if lx + 1 < Terrain.chunkSize && blocks(idx(lx + 1, y, lz)) == Block.Air.id then blocks(idx(lx + 1, y, lz)) = Block.Water.id
            if lx > 0 && blocks(idx(lx - 1, y, lz)) == Block.Air.id && hash(wx, y, wz, 8702) > 0.42f then blocks(idx(lx - 1, y, lz)) = Block.Water.id
            if lz + 1 < Terrain.chunkSize && blocks(idx(lx, y, lz + 1)) == Block.Air.id then blocks(idx(lx, y, lz + 1)) = Block.Water.id
            if lz > 0 && blocks(idx(lx, y, lz - 1)) == Block.Air.id && hash(wx, y, wz, 8703) > 0.42f then blocks(idx(lx, y, lz - 1)) = Block.Water.id
          y += 2
        lz += 1
      lx += 1

    def cachedChunkHeight(wx: Int, wz: Int): Int =
      val lx0 = wx - baseX
      val lz0 = wz - baseZ
      if lx0 >= 0 && lx0 < Terrain.chunkSize && lz0 >= 0 && lz0 < Terrain.chunkSize then heights(cidx(lx0, lz0))
      else
        val ex0 = wx - baseX + extPad
        val ez0 = wz - baseZ + extPad
        if ex0 >= 0 && ex0 < extSize && ez0 >= 0 && ez0 < extSize then extHeights(eidx(ex0, ez0)) else heightAt(wx, wz)
    def cachedChunkSurface(wx: Int, wz: Int, h: Int): Block =
      val lx0 = wx - baseX
      val lz0 = wz - baseZ
      if lx0 >= 0 && lx0 < Terrain.chunkSize && lz0 >= 0 && lz0 < Terrain.chunkSize then surfaces(cidx(lx0, lz0)) else surfaceBlock(wx, h, wz, h)
    def columnIsDecoratable(wx: Int, wz: Int, h: Int): Boolean =
      if caveMouthDepthAt(wx, wz, h) > 0 then false
      else
        val lx0 = wx - baseX
        val lz0 = wz - baseZ
        if lx0 >= 0 && lx0 < Terrain.chunkSize && lz0 >= 0 && lz0 < Terrain.chunkSize && h >= 0 && h < Terrain.worldHeight - 2 then
          val here = blocks(idx(lx0, h, lz0))
          val above = blocks(idx(lx0, h + 1, lz0))
          here != Block.Air.id && here != Block.Water.id && above == Block.Air.id
        else true

    for wx <- baseX - 4 to endX + 4; wz <- baseZ - 4 to endZ + 4 do
      val h = cachedChunkHeight(wx, wz)
      val surface = cachedChunkSurface(wx, wz, h)
      if columnIsDecoratable(wx, wz, h) then
        treeAt(wx, wz, h, surface).foreach { case (tx, ty, tz, th) =>
          placeTreeWorld(blocks, baseX, baseZ, tx, ty, tz, th, spruceAt(wx, wz, h))
        }
        cactusAt(wx, wz, h, surface).foreach { case (cx0, cy0, cz0, ch) =>
          placeCactusWorld(blocks, baseX, baseZ, cx0, cy0, cz0, ch)
        }
        bushAt(wx, wz, h, surface).foreach { case (bx0, by0, bz0, br) =>
          placeBushWorld(blocks, baseX, baseZ, bx0, by0, bz0, br)
        }
        boulderAt(wx, wz, h, surface).foreach { case (bx0, by0, bz0, br) =>
          placeBoulderWorld(blocks, baseX, baseZ, bx0, by0, bz0, br)
        }
    blocks

  private def placeTreeWorld(blocks: Array[Byte], baseX: Int, baseZ: Int, x: Int, y: Int, z: Int, height: Int, spruce: Boolean): Unit =
    val dry = savannaAt(x, z, y - 1)
    val birch = !spruce && !dry && birchGroveAt(x, z, y - 1)
    val wood = if spruce then Block.PineWood else if dry then Block.AcaciaWood else if birch then Block.BirchWood else Block.Wood
    val leaves = if spruce then Block.PineLeaves else if dry then Block.AcaciaLeaves else if birch then Block.BirchLeaves else Block.Leaves
    def set(wx: Int, wy: Int, wz: Int, block: Block): Unit =
      val lx = wx - baseX; val lz = wz - baseZ
      if lx >= 0 && lx < Terrain.chunkSize && lz >= 0 && lz < Terrain.chunkSize && wy >= 0 && wy < Terrain.worldHeight then
        val i = idx(lx, wy, lz)
        if block != leaves || blocks(i) == Block.Air.id then blocks(i) = block.id
    val leanX = if dry then (if hash(x, z, 0, 6005) > 0.5f then 1 else -1) else if !spruce && hash(x, z, 0, 6001) > 0.88f then (if hash(x, z, 0, 6002) > 0.5f then 1 else -1) else 0
    val leanZ = if dry then (if hash(x, z, 0, 6006) > 0.5f then 1 else -1) else if !spruce && hash(x, z, 0, 6003) > 0.88f then (if hash(x, z, 0, 6004) > 0.5f then 1 else -1) else 0
    for dy <- 0 until height do
      val sx = x + (if dy > height / 2 then leanX else 0)
      val sz = z + (if dy > height / 2 then leanZ else 0)
      set(sx, y + dy, sz, wood)
    val topX = x + leanX
    val topZ = z + leanZ
    if spruce then
      for dy <- 2 to height + 4 do
        val layer = dy - 2
        val radius = (4 - layer / 2).max(1)
        for dx <- -radius to radius; dz <- -radius to radius do
          val dist = abs(dx) + abs(dz)
          if dist <= radius + 1 && !(dx == 0 && dz == 0 && dy < height) then
            set(topX + dx, y + dy, topZ + dz, leaves)
      set(topX, y + height + 4, topZ, leaves)
    else if dry then
      // Sparse acacia-ish flat canopy.
      for dx <- -3 to 3; dz <- -3 to 3; dy <- height - 1 to height + 1 do
        val dist = abs(dx) + abs(dz)
        if dist <= 4 && hash(topX + dx, y + dy, topZ + dz, 6110) > 0.18f then set(topX + dx, y + dy, topZ + dz, leaves)
    else
      val wide = hash(x, z, 0, 6101) > 0.68f
      val leafStart = if wide then height - 3 else height - 2
      val leafSpread = if wide then 4 else if height >= 7 then 4 else 3
      for dx <- -leafSpread to leafSpread; dz <- -leafSpread to leafSpread; dy <- leafStart to height + 2 do
        val dist = abs(dx) + abs(dz) + max(0, dy - height)
        val edgeNoise = hash(topX + dx, y + dy, topZ + dz, 6102)
        if dist <= leafSpread + (if edgeNoise > 0.62f then 1 else 0) && !(dx == 0 && dz == 0 && dy > height - 1) then
          set(topX + dx, y + dy, topZ + dz, leaves)

  private def placeCactusWorld(blocks: Array[Byte], baseX: Int, baseZ: Int, x: Int, y: Int, z: Int, height: Int): Unit =
    def set(wx: Int, wy: Int, wz: Int, block: Block): Unit =
      val lx = wx - baseX; val lz = wz - baseZ
      if lx >= 0 && lx < Terrain.chunkSize && lz >= 0 && lz < Terrain.chunkSize && wy >= 0 && wy < Terrain.worldHeight then
        blocks(idx(lx, wy, lz)) = block.id
    for dy <- 0 until height do set(x, y + dy, z, Block.Cactus)

  private def placeBushWorld(blocks: Array[Byte], baseX: Int, baseZ: Int, x: Int, y: Int, z: Int, radius: Int): Unit =
    def set(wx: Int, wy: Int, wz: Int, block: Block): Unit =
      val lx = wx - baseX; val lz = wz - baseZ
      if lx >= 0 && lx < Terrain.chunkSize && lz >= 0 && lz < Terrain.chunkSize && wy >= 0 && wy < Terrain.worldHeight then
        val i = idx(lx, wy, lz)
        if blocks(i) == Block.Air.id then blocks(i) = block.id
    // Small hand-placed shrub shape. This prevents the old dense leaf blanket look.
    set(x, y, z, Block.Leaves)
    if hash(x, z, 0, 4044) > 0.35f then set(x + 1, y, z, Block.Leaves)
    if hash(x, z, 0, 4045) > 0.35f then set(x - 1, y, z, Block.Leaves)
    if hash(x, z, 0, 4046) > 0.35f then set(x, y, z + 1, Block.Leaves)
    if hash(x, z, 0, 4047) > 0.35f then set(x, y, z - 1, Block.Leaves)
    if hash(x, z, 0, 4048) > 0.72f then set(x, y + 1, z, Block.Leaves)

  private def placeBoulderWorld(blocks: Array[Byte], baseX: Int, baseZ: Int, x: Int, y: Int, z: Int, radius: Int): Unit =
    def set(wx: Int, wy: Int, wz: Int, block: Block): Unit =
      val lx = wx - baseX; val lz = wz - baseZ
      if lx >= 0 && lx < Terrain.chunkSize && lz >= 0 && lz < Terrain.chunkSize && wy >= 0 && wy < Terrain.worldHeight then
        blocks(idx(lx, wy, lz)) = block.id
    for dx <- -radius to radius; dz <- -radius to radius; dy <- 0 to radius do
      val ell = dx * dx + dz * dz + dy * dy
      if ell <= radius * radius + 1 && hash(x + dx, y + dy, z + dz, 5054) > 0.16f then
        set(x + dx, y + dy, z + dz, if hash(x + dx, y + dy, z + dz, 5055) > 0.74f then Block.Clay else Block.Stone)

  private def placeTree(blocks: Array[Byte], lx: Int, y: Int, lz: Int, height: Int): Unit =
    val baseX = 0; val baseZ = 0
    for dy <- 0 until height do
      val ly = y + dy
      if ly < Terrain.worldHeight then blocks(idx(lx, ly, lz)) = Block.Wood.id
    val leafStart = height - 2
    val leafSpread = if height >= 6 then 4 else 3
    for dx <- -leafSpread to leafSpread; dz <- -leafSpread to leafSpread; dy <- leafStart to height + 1 do
      val ax = lx + dx; val ay = y + dy; val az = lz + dz
      if ax >= 0 && ax < Terrain.chunkSize && az >= 0 && az < Terrain.chunkSize && ay >= 0 && ay < Terrain.worldHeight then
        val dist = abs(dx) + abs(dz) + max(0, dy - height)
        if dist <= leafSpread + 1 && !(dx == 0 && dz == 0 && dy > height - 1) then
          blocks(idx(ax, ay, az)) = Block.Leaves.id

  private def idx(lx: Int, y: Int, lz: Int): Int =
    (y * Terrain.chunkSize + lz) * Terrain.chunkSize + lx

final class Chunk(val cx: Int, val cz: Int, atlas: TextureAtlas, gen: TerrainGenerator):
  var blocks: Array[Byte] = gen.fillChunkBlocks(cx, cz)
  private[blockbox] val edits = scala.collection.mutable.HashMap.empty[(Int, Int, Int), Block]
  // 0 means default full-height water for generated/static water.
  // 1..8 are dynamic flowing-water levels used by the cellular automata.
  private val waterLevelsLocal = new Array[Byte](Terrain.chunkSize * Terrain.worldHeight * Terrain.chunkSize)
  private val waterExtraMagic = 0x574C5631 // "WLV1"

  // Legacy/freshwater repair: old experimental terrain builds could save static
  // generated water above sea level. Those blocks render as raised transparent
  // plates over beaches because static water has no flowing level data. Keep edited
  // water and dynamic water, but clamp natural raw-level-0 water to sea level.
  private def normalizeStaticNaturalWater(): Unit =
    var changed = false
    var lx = 0
    while lx < Terrain.chunkSize do
      var lz = 0
      while lz < Terrain.chunkSize do
        var y = Terrain.seaLevel + 1
        while y < Terrain.worldHeight do
          val i = localIndex(lx, y, lz)
          if blocks(i) == Block.Water.id && waterLevelsLocal(i) == 0.toByte then
            val edited = edits.synchronized { edits.contains((lx, y, lz)) }
            if !edited then
              blocks(i) = Block.Air.id
              changed = true
          y += 1
        lz += 1
      lx += 1
    if changed then markDirtyMesh()

  val baseX = cx * Terrain.chunkSize
  val baseZ = cz * Terrain.chunkSize

  private val floatsPerVertex = 9
  private var opaqueVbo = 0; private var opaqueCount = 0
  private var cutoutVbo = 0; private var cutoutCount = 0
  private var translucentVbo = 0; private var translucentCount = 0
  private var waterVbo = 0; private var waterCount = 0

  // Threaded mesh data: worker fills these, main thread uploads them.
  // The mesh queue flag/revision pair prevents the same chunk from being meshed by
  // multiple worker threads at once. That was the big source of the "random raised
  // chunk / ugly artifact" looking glitches: stale meshes could finish after newer
  // world edits or after a chunk was unloaded.
  @volatile private var pendingData: Array[ArrayBuffer[Float]] = null
  @volatile private var pendingRevision = -1
  @volatile var meshReady: Boolean = false
  @volatile private var disposed = false
  @volatile private var meshRevision = 0
  private val meshQueued = java.util.concurrent.atomic.AtomicBoolean(false)

  normalizeStaticNaturalWater()

  def hasMesh: Boolean =
    opaqueCount > 0 || cutoutCount > 0 || translucentCount > 0 || waterCount > 0

  def isDisposed: Boolean = disposed

  def markDirtyMesh(): Unit =
    meshRevision += 1

  def tryQueueMesh(): Boolean =
    !disposed && meshQueued.compareAndSet(false, true)

  def releaseMeshQueue(): Unit =
    meshQueued.set(false)

  def dispose(): Unit =
    disposed = true
    pendingData = null
    meshReady = false
    meshQueued.set(false)
    destroy()

  private def localIndex(lx: Int, y: Int, lz: Int): Int =
    (y * Terrain.chunkSize + lz) * Terrain.chunkSize + lx

  private def validLocal(lx: Int, y: Int, lz: Int): Boolean =
    lx >= 0 && lx < Terrain.chunkSize && y >= 0 && y < Terrain.worldHeight && lz >= 0 && lz < Terrain.chunkSize

  def getBlock(lx: Int, y: Int, lz: Int): Block =
    val found: Option[Block] = edits.synchronized { edits.get((lx, y, lz)) }
    found match
      case Some(b: Block) => b
      case None =>
        if validLocal(lx, y, lz) then Block.fromId(blocks(localIndex(lx, y, lz)))
        else Block.Air

  def getWaterLevel(lx: Int, y: Int, lz: Int): Int =
    if validLocal(lx, y, lz) && getBlock(lx, y, lz) == Block.Water then
      val raw = waterLevelsLocal(localIndex(lx, y, lz)).toInt & 0xFF
      if raw <= 0 then 8 else raw.max(1).min(8)
    else 0

  def getWaterRawLevel(lx: Int, y: Int, lz: Int): Int =
    if validLocal(lx, y, lz) && getBlock(lx, y, lz) == Block.Water then
      waterLevelsLocal(localIndex(lx, y, lz)).toInt & 0xFF
    else 0

  def foreachDynamicWaterCell(f: (Int, Int, Int, Int) => Unit): Unit =
    var y = 0
    while y < Terrain.worldHeight do
      var lz = 0
      while lz < Terrain.chunkSize do
        var lx = 0
        while lx < Terrain.chunkSize do
          val i = localIndex(lx, y, lz)
          val raw = waterLevelsLocal(i).toInt & 0xFF
          if raw > 0 && Block.fromId(blocks(i)) == Block.Water then f(lx, y, lz, raw.max(1).min(8))
          lx += 1
        lz += 1
      y += 1

  def setWaterLevel(lx: Int, y: Int, lz: Int, level: Int): Unit =
    if validLocal(lx, y, lz) then
      val i = localIndex(lx, y, lz)
      val next = level.max(0).min(8).toByte
      if waterLevelsLocal(i) != next then
        waterLevelsLocal(i) = next
        markDirtyMesh()

  def setBlock(lx: Int, y: Int, lz: Int, block: Block): Unit =
    if validLocal(lx, y, lz) then
      val i = localIndex(lx, y, lz)
      val oldId = blocks(i)
      blocks(i) = block.id
      if block == Block.Water && waterLevelsLocal(i) == 0 then waterLevelsLocal(i) = 8.toByte
      if block != Block.Water then waterLevelsLocal(i) = 0.toByte
      edits.synchronized { edits((lx, y, lz)) = block }
      if oldId != block.id then markDirtyMesh()

  // Thread-safe: called by worker thread, no GL calls. Unknown neighbor-chunk
  // space is treated as air and borders are rebuilt when neighbors load/change,
  // which prevents caves/coastlines from losing visible faces at chunk seams.
  def computeMesh(): Unit =
    if disposed then
      meshReady = false
      releaseMeshQueue()
      return
    val buildRevision = meshRevision
    val opaqueVerts = ArrayBuffer.empty[Float]; val cutoutVerts = ArrayBuffer.empty[Float]
    val translucentVerts = ArrayBuffer.empty[Float]; val waterVerts = ArrayBuffer.empty[Float]

    // Snapshot chunk block data before meshing. The main thread can edit blocks while
    // worker threads are building VBO data (water flow, block edits, multiplayer BLOC
    // packets). Reading the live HashMap/Array from a worker can create random stale or
    // half-built meshes: the dark-blue empty world / missing terrain effect. Snapshotting
    // makes every mesh build deterministic for one revision.
    val blocksSnapshot = blocks.clone()
    val waterLevelsSnapshot = waterLevelsLocal.clone()
    val editsSnapshot: scala.collection.mutable.HashMap[(Int, Int, Int), Block] =
      edits.synchronized { edits.clone() }
    def snapshotBlock(lx: Int, y: Int, lz: Int): Block =
      val found: Option[Block] = editsSnapshot.get((lx, y, lz))
      found match
        case Some(b: Block) => b
        case None =>
          if lx >= 0 && lx < Terrain.chunkSize && y >= 0 && y < Terrain.worldHeight && lz >= 0 && lz < Terrain.chunkSize then
            Block.fromId(blocksSnapshot((y * Terrain.chunkSize + lz) * Terrain.chunkSize + lx))
          else Block.Air
    def localBlock(nlx: Int, ny: Int, nlz: Int): Block =
      if ny < 0 || ny >= Terrain.worldHeight then Block.Air
      else if nlx >= 0 && nlx < Terrain.chunkSize && nlz >= 0 && nlz < Terrain.chunkSize then
        snapshotBlock(nlx, ny, nlz)
      else
        // Important: do NOT guess neighbor terrain here. Earlier builds used deterministic
        // generated terrain outside the chunk, but that ignores caves, saved edits, water
        // changes, and chunk-edge structures. That made real air pockets look solid, so
        // faces disappeared at chunk borders. Treat unknown neighbor space as air and
        // rebuild border chunks when neighbors load/change; it is safer to briefly draw an
        // extra hidden face than to delete a visible one.
        Block.Air

    def localWaterRawLevel(nlx: Int, ny: Int, nlz: Int): Int =
      if ny < 0 || ny >= Terrain.worldHeight || nlx < 0 || nlx >= Terrain.chunkSize || nlz < 0 || nlz >= Terrain.chunkSize then 0
      else waterLevelsSnapshot((ny * Terrain.chunkSize + nlz) * Terrain.chunkSize + nlx).toInt & 0xFF

    def localWaterLevel(nlx: Int, ny: Int, nlz: Int): Int =
      if ny < 0 || ny >= Terrain.worldHeight || nlx < 0 || nlx >= Terrain.chunkSize || nlz < 0 || nlz >= Terrain.chunkSize then 0
      else if snapshotBlock(nlx, ny, nlz) != Block.Water then 0
      else
        val raw = localWaterRawLevel(nlx, ny, nlz)
        if raw <= 0 then 8 else raw.max(1).min(8)

    def rawWaterTopY(nlx: Int, ny: Int, nlz: Int, fy: Float): Float =
      if ny < 0 || ny >= Terrain.worldHeight || nlx < 0 || nlx >= Terrain.chunkSize || nlz < 0 || nlz >= Terrain.chunkSize then fy
      else if snapshotBlock(nlx, ny, nlz) != Block.Water then fy
      else
        val raw = localWaterRawLevel(nlx, ny, nlz)
        // Generated/static water has raw level 0. Draw it as a full flat block-top
        // surface so all natural oceans, rivers, ponds and lakes share one exact
        // plane. Do not use 0.94 or mixed freshwater heights here; tiny offsets plus
        // transparency caused the visible sheet/step artifacts in large beaches.
        if raw <= 0 || raw >= 8 then fy + 1f
        else
          val level = raw.max(1).min(7).toFloat
          fy + 0.36f + 0.52f * (level / 7f)
    def waterCornerY(cells: Array[(Int, Int, Int)], fy: Float, fallback: Float): Float =
      var sum = 0f
      var count = 0
      var hasFullColumn = false
      var i = 0
      while i < cells.length do
        val (cx, cy, cz) = cells(i)
        if localBlock(cx, cy, cz) == Block.Water then
          if localBlock(cx, cy + 1, cz) == Block.Water then hasFullColumn = true
          sum += rawWaterTopY(cx, cy, cz, fy)
          count += 1
        i += 1
      if hasFullColumn then fy + 1f
      else if count > 0 then sum / count.toFloat
      else fallback
    def isWaterSideVisible(nlx: Int, ny: Int, nlz: Int): Boolean =
      // Never guess a neighbor chunk as air for water sides. Opaque blocks can tolerate
      // temporary border faces, but transparent water cannot: those faces show up as huge
      // blue sheets along chunk/grid seams. Neighbor chunks draw their own water tops, and
      // real exposed sides are only emitted when the neighbor is known inside this chunk.
      if nlx < 0 || nlx >= Terrain.chunkSize || nlz < 0 || nlz >= Terrain.chunkSize || ny < 0 || ny >= Terrain.worldHeight then false
      else
        val nb = localBlock(nlx, ny, nlz)
        nb != Block.Water && (nb == Block.Air || nb.cutout || (nb.translucent && nb != Block.Water))

    def shouldDrawWaterSide(lx: Int, y: Int, lz: Int, nlx: Int, ny: Int, nlz: Int): Boolean =
      if !isWaterSideVisible(nlx, ny, nlz) then false
      else
        val raw = localWaterRawLevel(lx, y, lz)
        if raw <= 0 then false
        else
          // Static generated water keeps clean top-only lakes/oceans. Dynamic water draws
          // every exposed side, including stacked waterfall sides when water exists above.
          true
    def isVisible(nlx: Int, ny: Int, nlz: Int, block: Block): Boolean =
      val nb = localBlock(nlx, ny, nlz)
      if nb == Block.Air then true
      else if block == Block.Water then isWaterSideVisible(nlx, ny, nlz)
      else if nb == Block.Water then true
      else if block.cutout && nb.cutout then false
      else if nb.cutout then block != nb
      else if block.solid && nb.solid && !block.translucent && !nb.translucent && !block.cutout && !nb.cutout then false
      else nb.translucent && nb != block
    for lx <- 0 until Terrain.chunkSize; lz <- 0 until Terrain.chunkSize; y <- 0 until Terrain.worldHeight do
      val block = snapshotBlock(lx, y, lz)
      if block != Block.Air then
        val atlasRef = atlas
        // Keep chunk mesh vertices local to the chunk. The renderer applies a
        // camera-relative chunk transform, which delays float precision "far lands"
        // artifacts far beyond where full world-coordinate VBOs started wobbling.
        val fx = lx.toFloat; val fy = y.toFloat; val fz = lz.toFloat
        // Water is still one block in storage, but flowing water renders with a variable
        // surface height from the per-chunk level array. Static generated oceans use the
        // default full level without entering the simulation queue.
        val topY = if block == Block.Water then rawWaterTopY(lx, y, lz, fy) else fy + 1f
        val yNorm = y.toFloat / Terrain.worldHeight.toFloat
        val ambient = 0.45f + yNorm * 0.10f
        def grassCornerTint(cfx: Float, cfz: Float): (Float, Float, Float) =
          val sx = floor(baseX.toFloat + cfx).toInt
          val sz = floor(baseZ.toFloat + cfz).toInt
          gen.grassTintAt(sx, sz, y)
        def addFace(shade: Float, corners: Array[(Float, Float, Float, Float, Float)], kind: FaceKind, tintGrass: Boolean = false): Unit =
          val buf = if block.cutout then cutoutVerts
            else if block == Block.Water then waterVerts
            else if block.translucent then translucentVerts
            else opaqueVerts
          val light = if block == Block.Water then (0.55f + yNorm * 0.22f) * (0.82f + shade * 0.18f) else shade * ambient
          val idx = Array(0, 1, 2, 2, 3, 0)
          for i <- idx do
            val (cfx, cfy, cfz, tu, tv) = corners(i)
            val (u, v) = atlasRef.uv(block, kind, tu, tv)
            val (tintR, tintG, tintB) = if tintGrass then grassCornerTint(cfx, cfz) else (1f, 1f, 1f)
            val alpha = block match
              case Block.Water => 1f
              case Block.Glass => 0.68f
              case _ => 1f
            buf += cfx; buf += cfy; buf += cfz
            buf += (light * tintR).min(1.35f); buf += (light * tintG).min(1.35f); buf += (light * tintB).min(1.35f); buf += alpha
            buf += u; buf += v
        def addGrassSide(shade: Float, corners: Array[(Float, Float, Float, Float, Float)], kind: FaceKind): Unit =
          val bottomA = corners(0)
          val bottomB = corners(1)
          val topB = corners(2)
          val topA = corners(3)
          def band(bottom: (Float, Float, Float, Float, Float), top: (Float, Float, Float, Float, Float)): (Float, Float, Float, Float, Float) =
            val t = 0.75f
            (
              bottom._1 + (top._1 - bottom._1) * t,
              bottom._2 + (top._2 - bottom._2) * t,
              bottom._3 + (top._3 - bottom._3) * t,
              bottom._4,
              0.25f
            )
          val bandA = band(bottomA, topA)
          val bandB = band(bottomB, topB)
          addFace(shade, Array(bottomA, bottomB, bandB, bandA), kind, false)
          addFace(shade, Array(bandA, bandB, topB, topA), kind, true)
        if block == Block.Water then
          // Render dynamic water as actual block-volume water with internal culling.
          // Neighboring water cells hide shared faces; exposed streams and waterfalls show
          // full-height sides like Minecraft-style flowing water.
          val fullAbove = localBlock(lx, y + 1, lz) == Block.Water
          val nwTop = if fullAbove then fy + 1f else waterCornerY(Array((lx, y, lz), (lx - 1, y, lz), (lx, y, lz - 1), (lx - 1, y, lz - 1)), fy, topY)
          val neTop = if fullAbove then fy + 1f else waterCornerY(Array((lx, y, lz), (lx + 1, y, lz), (lx, y, lz - 1), (lx + 1, y, lz - 1)), fy, topY)
          val seTop = if fullAbove then fy + 1f else waterCornerY(Array((lx, y, lz), (lx + 1, y, lz), (lx, y, lz + 1), (lx + 1, y, lz + 1)), fy, topY)
          val swTop = if fullAbove then fy + 1f else waterCornerY(Array((lx, y, lz), (lx - 1, y, lz), (lx, y, lz + 1), (lx - 1, y, lz + 1)), fy, topY)
          if !fullAbove then
            addFace(1.00f, Array((fx, nwTop, fz, 0f, 0f), (fx + 1, neTop, fz, 1f, 0f), (fx + 1, seTop, fz + 1, 1f, 1f), (fx, swTop, fz + 1, 0f, 1f)), FaceKind.Top)
          // Static generated oceans/lakes are drawn as clean surface water only. Dynamic
          // placed/flowing water gets full sides so waterfalls and streams read as volume.
          if shouldDrawWaterSide(lx, y, lz, lx + 1, y, lz) then
            addFace(0.82f, Array((fx + 1, fy, fz, 0f, 1f), (fx + 1, fy, fz + 1, 1f, 1f), (fx + 1, seTop, fz + 1, 1f, 0f), (fx + 1, neTop, fz, 0f, 0f)), FaceKind.East)
          if shouldDrawWaterSide(lx, y, lz, lx - 1, y, lz) then
            addFace(0.55f, Array((fx, fy, fz + 1, 0f, 1f), (fx, fy, fz, 1f, 1f), (fx, nwTop, fz, 1f, 0f), (fx, swTop, fz + 1, 0f, 0f)), FaceKind.West)
          if shouldDrawWaterSide(lx, y, lz, lx, y, lz + 1) then
            addFace(0.74f, Array((fx + 1, fy, fz + 1, 0f, 1f), (fx, fy, fz + 1, 1f, 1f), (fx, swTop, fz + 1, 1f, 0f), (fx + 1, seTop, fz + 1, 0f, 0f)), FaceKind.South)
          if shouldDrawWaterSide(lx, y, lz, lx, y, lz - 1) then
            addFace(0.52f, Array((fx, fy, fz, 0f, 1f), (fx + 1, fy, fz, 1f, 1f), (fx + 1, neTop, fz, 1f, 0f), (fx, nwTop, fz, 0f, 0f)), FaceKind.North)
        else
          if isVisible(lx, y + 1, lz, block) then
            addFace(1.00f, Array((fx, topY, fz, 0f, 0f), (fx + 1, topY, fz, 1f, 0f), (fx + 1, topY, fz + 1, 1f, 1f), (fx, topY, fz + 1, 0f, 1f)), FaceKind.Top, block == Block.Grass)
          if isVisible(lx, y - 1, lz, block) then
            addFace(0.38f, Array((fx, fy, fz + 1, 0f, 0f), (fx + 1, fy, fz + 1, 1f, 0f), (fx + 1, fy, fz, 1f, 1f), (fx, fy, fz, 0f, 1f)), FaceKind.Bottom)
          if isVisible(lx + 1, y, lz, block) then
            val corners = Array((fx + 1, fy, fz, 0f, 1f), (fx + 1, fy, fz + 1, 1f, 1f), (fx + 1, topY, fz + 1, 1f, 0f), (fx + 1, topY, fz, 0f, 0f))
            if block == Block.Grass then addGrassSide(0.82f, corners, FaceKind.East) else addFace(0.82f, corners, FaceKind.East)
          if isVisible(lx - 1, y, lz, block) then
            val corners = Array((fx, fy, fz + 1, 0f, 1f), (fx, fy, fz, 1f, 1f), (fx, topY, fz, 1f, 0f), (fx, topY, fz + 1, 0f, 0f))
            if block == Block.Grass then addGrassSide(0.55f, corners, FaceKind.West) else addFace(0.55f, corners, FaceKind.West)
          if isVisible(lx, y, lz + 1, block) then
            val corners = Array((fx + 1, fy, fz + 1, 0f, 1f), (fx, fy, fz + 1, 1f, 1f), (fx, topY, fz + 1, 1f, 0f), (fx + 1, topY, fz + 1, 0f, 0f))
            if block == Block.Grass then addGrassSide(0.74f, corners, FaceKind.South) else addFace(0.74f, corners, FaceKind.South)
          if isVisible(lx, y, lz - 1, block) then
            val corners = Array((fx, fy, fz, 0f, 1f), (fx + 1, fy, fz, 1f, 1f), (fx + 1, topY, fz, 1f, 0f), (fx, topY, fz, 0f, 0f))
            if block == Block.Grass then addGrassSide(0.52f, corners, FaceKind.North) else addFace(0.52f, corners, FaceKind.North)
    if disposed then
      pendingData = null
      pendingRevision = -1
      meshReady = false
      releaseMeshQueue()
    else
      pendingData = Array(opaqueVerts, cutoutVerts, translucentVerts, waterVerts)
      pendingRevision = buildRevision
      meshReady = true

  // Must be called on the GL thread. Returns false if the mesh was stale and
  // should be queued again.
  def uploadMesh(): Boolean =
    val pd = pendingData
    if disposed then
      pendingData = null; pendingRevision = -1; meshReady = false; releaseMeshQueue()
      false
    else if pd != null && pendingRevision == meshRevision then
      destroy()
      val (ov, oc) = upload(pd(0)); opaqueVbo = ov; opaqueCount = oc
      val (cv, cc) = upload(pd(1)); cutoutVbo = cv; cutoutCount = cc
      val (tv, tc) = upload(pd(2)); translucentVbo = tv; translucentCount = tc
      val (wv, wc) = upload(pd(3)); waterVbo = wv; waterCount = wc
      pendingData = null; pendingRevision = -1; meshReady = false; releaseMeshQueue()
      true
    else
      pendingData = null; pendingRevision = -1; meshReady = false; releaseMeshQueue()
      false

  // Emergency path for the nearest chunks: build and upload a mesh on the GL thread.
  // This is intentionally used only for the spawn/camera neighborhood so the player never
  // sees an empty sky-only world while async workers catch up.
  def buildNowAndUpload(): Boolean =
    if disposed then false
    else if meshReady then uploadMesh()
    else if tryQueueMesh() then
      computeMesh()
      if meshReady then uploadMesh() else false
    else false

  // Main-thread emergency path for the local camera ring. This intentionally
  // ignores a stale queued flag because v17 runs chunk meshing on the GL thread;
  // if a chunk is queued but not built yet, showing a dark void is worse than
  // rebuilding it immediately. Bumping the revision invalidates any old pending
  // mesh result before creating a fresh one.
  def forceBuildNowAndUpload(): Boolean =
    if disposed then false
    else
      pendingData = null
      pendingRevision = -1
      meshReady = false
      meshQueued.set(false)
      markDirtyMesh()
      if tryQueueMesh() then
        computeMesh()
        if meshReady then uploadMesh() else false
      else false

  // Batched rendering: caller manages state, chunks just draw their VBOs.
  // Vertices are chunk-local; the per-chunk transform is camera-relative.
  def drawOpaque(camera: Vec3): Unit = if opaqueCount > 0 then drawBufferAt(opaqueVbo, opaqueCount, camera)
  def drawCutout(camera: Vec3): Unit = if cutoutCount > 0 then drawBufferAt(cutoutVbo, cutoutCount, camera)
  def drawTranslucent(camera: Vec3): Unit = if translucentCount > 0 then drawBufferAt(translucentVbo, translucentCount, camera)
  def drawWater(camera: Vec3): Unit = if waterCount > 0 then drawBufferAt(waterVbo, waterCount, camera)

  def destroy(): Unit =
    if opaqueVbo != 0 then glDeleteBuffers(opaqueVbo); opaqueVbo = 0; opaqueCount = 0
    if cutoutVbo != 0 then glDeleteBuffers(cutoutVbo); cutoutVbo = 0; cutoutCount = 0
    if translucentVbo != 0 then glDeleteBuffers(translucentVbo); translucentVbo = 0; translucentCount = 0
    if waterVbo != 0 then glDeleteBuffers(waterVbo); waterVbo = 0; waterCount = 0

  def save(dir: java.io.File): Unit =
    dir.mkdirs()
    val file = new java.io.File(dir, s"chunk_${cx}_${cz}.dat")
    val tmp = new java.io.File(dir, s"chunk_${cx}_${cz}.dat.tmp")
    val out = new java.io.DataOutputStream(new java.io.BufferedOutputStream(new java.io.FileOutputStream(tmp)))
    try
      out.write(blocks)
      val editCopy = edits.synchronized { edits.toList }
      out.writeInt(editCopy.size)
      editCopy.foreach { case ((lx, ly, lz), block) =>
        out.writeInt(lx); out.writeInt(ly); out.writeInt(lz)
        out.writeByte(block.id)
      }
      val waterEntries = ArrayBuffer.empty[(Int, Byte)]
      var wi = 0
      while wi < waterLevelsLocal.length do
        val level = waterLevelsLocal(wi)
        if level != 0 then waterEntries += ((wi, level))
        wi += 1
      out.writeInt(waterExtraMagic)
      out.writeInt(waterEntries.length)
      waterEntries.foreach { case (i, level) =>
        out.writeInt(i)
        out.writeByte(level)
      }
    finally out.close()
    try
      java.nio.file.Files.move(tmp.toPath, file.toPath, java.nio.file.StandardCopyOption.REPLACE_EXISTING, java.nio.file.StandardCopyOption.ATOMIC_MOVE)
    catch
      case _: Exception =>
        java.nio.file.Files.move(tmp.toPath, file.toPath, java.nio.file.StandardCopyOption.REPLACE_EXISTING)

  def load(dir: java.io.File): Unit =
    val file = new java.io.File(dir, s"chunk_${cx}_${cz}.dat")
    if file.exists() then
      val in = new java.io.DataInputStream(new java.io.BufferedInputStream(new java.io.FileInputStream(file)))
      try
        val loadedBlocks = new Array[Byte](Terrain.chunkSize * Terrain.worldHeight * Terrain.chunkSize)
        in.readFully(loadedBlocks)
        // Fast load path: use the saved block array and reapply edits below.
        // Regenerating every saved chunk on load fixed old broken worlds once, but it was too
        // expensive while walking/hosting. New worlds already use the fixed generator; old cursed
        // chunk files should be deleted once instead of regenerated every frame.
        blocks = loadedBlocks
        edits.synchronized {
          edits.clear()
          val editsCount = in.readInt().max(0).min(Terrain.chunkSize * Terrain.worldHeight * Terrain.chunkSize)
          for _ <- 0 until editsCount do
            val lx = in.readInt(); val ly = in.readInt(); val lz = in.readInt()
            val blockId = in.readByte()
            if lx >= 0 && lx < Terrain.chunkSize && ly >= 0 && ly < Terrain.worldHeight && lz >= 0 && lz < Terrain.chunkSize then
              val b = Block.fromId(blockId)
              edits((lx, ly, lz)) = b
              blocks((ly * Terrain.chunkSize + lz) * Terrain.chunkSize + lx) = b.id
        }
        java.util.Arrays.fill(waterLevelsLocal, 0.toByte)
        if in.available() >= 8 then
          val magic = in.readInt()
          if magic == waterExtraMagic then
            val waterCount = in.readInt().max(0).min(waterLevelsLocal.length)
            for _ <- 0 until waterCount do
              val wi = in.readInt()
              val level = in.readByte()
              if wi >= 0 && wi < waterLevelsLocal.length then waterLevelsLocal(wi) = (level.toInt & 0xFF).max(0).min(8).toByte
        normalizeStaticNaturalWater()
        markDirtyMesh()
      finally in.close()

  private def upload(data: ArrayBuffer[Float]): (Int, Int) =
    if data == null || data.isEmpty then (0, 0)
    else
      val buffer = BufferUtils.createFloatBuffer(data.length)
      data.foreach(buffer.put); buffer.flip()
      val vbo = glGenBuffers()
      glBindBuffer(GL_ARRAY_BUFFER, vbo)
      glBufferData(GL_ARRAY_BUFFER, buffer, GL_STATIC_DRAW)
      glBindBuffer(GL_ARRAY_BUFFER, 0)
      (vbo, buffer.limit() / floatsPerVertex)

  private def drawBufferAt(vbo: Int, count: Int, camera: Vec3): Unit =
    if vbo != 0 && count > 0 then
      val stride = floatsPerVertex * 4
      glPushMatrix()
      glTranslatef(baseX.toFloat - camera.x, 0f, baseZ.toFloat - camera.z)
      glBindBuffer(GL_ARRAY_BUFFER, vbo)
      glVertexPointer(3, GL_FLOAT, stride, 0L)
      glColorPointer(4, GL_FLOAT, stride, 12L)
      glTexCoordPointer(2, GL_FLOAT, stride, 28L)
      glDrawArrays(GL_TRIANGLES, 0, count)
      glPopMatrix()

// Multiplayer networking

trait BlockboxMod:
  def onInit(api: ModApi): Unit = ()
  def onWorldLoaded(api: ModApi): Unit = ()
  def onShutdown(api: ModApi): Unit = ()

final case class ModDescriptor(
  id: String,
  name: String,
  version: String,
  description: String,
  side: String,
  serverInteractive: Boolean,
  source: String,
  mainClass: String,
  sha256: String,
  var loaded: Boolean = false,
  var status: String = "pending"
):
  def sideLabel: String = if serverInteractive then s"$side/server" else side

final case class ModBlockBreakEvent(api: ModApi, playerName: String, x: Int, y: Int, z: Int, block: Block, var cancelled: Boolean = false, var dropItem: Boolean = true)
final case class ModBlockPlaceEvent(api: ModApi, playerName: String, x: Int, y: Int, z: Int, var block: Block, var cancelled: Boolean = false)
final case class ModWorldTickEvent(api: ModApi, dt: Float)
final case class ModHudRenderEvent(api: ModApi, gui: ModGuiApi, dt: Float)
final case class ModMouseClickEvent(api: ModApi, gui: ModGuiApi, mouseX: Float, mouseY: Float, button: Int, var consumed: Boolean = false)
final case class ModKeyEvent(api: ModApi, key: Int, screen: String, var cancelled: Boolean = false)
final case class ModChatEvent(api: ModApi, playerName: String, var message: String, var cancelled: Boolean = false)

final class ModEventBus:
  private final case class Handler[A](fn: Consumer[A], label: String, var failures: Int = 0, var disabled: Boolean = false)
  private val blockBreakHandlers = ArrayBuffer.empty[Handler[ModBlockBreakEvent]]
  private val blockPlaceHandlers = ArrayBuffer.empty[Handler[ModBlockPlaceEvent]]
  private val worldTickHandlers = ArrayBuffer.empty[Handler[ModWorldTickEvent]]
  private val hudRenderHandlers = ArrayBuffer.empty[Handler[ModHudRenderEvent]]
  private val mouseClickHandlers = ArrayBuffer.empty[Handler[ModMouseClickEvent]]
  private val keyHandlers = ArrayBuffer.empty[Handler[ModKeyEvent]]
  private val chatHandlers = ArrayBuffer.empty[Handler[ModChatEvent]]
  private def add[A](handlers: ArrayBuffer[Handler[A]], handler: Consumer[A], label: String): Unit =
    if handler != null then handlers += Handler(handler, label)
  def onBlockBreak(handler: Consumer[ModBlockBreakEvent]): Unit = add(blockBreakHandlers, handler, "blockBreak")
  def onBlockBreak(handler: Closure[?]): Unit = onBlockBreak(ModRuntime.consumer[ModBlockBreakEvent](handler))
  def onBlockPlace(handler: Consumer[ModBlockPlaceEvent]): Unit = add(blockPlaceHandlers, handler, "blockPlace")
  def onBlockPlace(handler: Closure[?]): Unit = onBlockPlace(ModRuntime.consumer[ModBlockPlaceEvent](handler))
  def onWorldTick(handler: Consumer[ModWorldTickEvent]): Unit = add(worldTickHandlers, handler, "worldTick")
  def onWorldTick(handler: Closure[?]): Unit = onWorldTick(ModRuntime.consumer[ModWorldTickEvent](handler))
  def onHudRender(handler: Consumer[ModHudRenderEvent]): Unit = add(hudRenderHandlers, handler, "hudRender")
  def onHudRender(handler: Closure[?]): Unit = onHudRender(ModRuntime.consumer[ModHudRenderEvent](handler))
  def onMouseClick(handler: Consumer[ModMouseClickEvent]): Unit = add(mouseClickHandlers, handler, "mouseClick")
  def onMouseClick(handler: Closure[?]): Unit = onMouseClick(ModRuntime.consumer[ModMouseClickEvent](handler))
  def onKeyPress(handler: Consumer[ModKeyEvent]): Unit = add(keyHandlers, handler, "keyPress")
  def onKeyPress(handler: Closure[?]): Unit = onKeyPress(ModRuntime.consumer[ModKeyEvent](handler))
  def onChat(handler: Consumer[ModChatEvent]): Unit = add(chatHandlers, handler, "chat")
  def onChat(handler: Closure[?]): Unit = onChat(ModRuntime.consumer[ModChatEvent](handler))
  private def safeRun[A](handlers: ArrayBuffer[Handler[A]], event: A): A =
    handlers.foreach { h =>
      if !h.disabled then
        try h.fn.accept(event)
        catch
          case e: Throwable =>
            h.failures += 1
            val msg = Option(e.getMessage).filter(_.nonEmpty).getOrElse(e.getClass.getSimpleName)
            System.err.println(s"Blockbox mod event '${h.label}' failed (${h.failures}/8): $msg")
            if h.failures >= 8 then
              h.disabled = true
              System.err.println(s"Blockbox disabled failing mod event handler '${h.label}' for this session.")
    }
    event
  def fireBlockBreak(event: ModBlockBreakEvent): ModBlockBreakEvent = safeRun(blockBreakHandlers, event)
  def fireBlockPlace(event: ModBlockPlaceEvent): ModBlockPlaceEvent = safeRun(blockPlaceHandlers, event)
  def fireWorldTick(event: ModWorldTickEvent): ModWorldTickEvent = safeRun(worldTickHandlers, event)
  def fireHudRender(event: ModHudRenderEvent): ModHudRenderEvent = safeRun(hudRenderHandlers, event)
  def fireMouseClick(event: ModMouseClickEvent): ModMouseClickEvent = safeRun(mouseClickHandlers, event)
  def fireKeyPress(event: ModKeyEvent): ModKeyEvent = safeRun(keyHandlers, event)
  def fireChat(event: ModChatEvent): ModChatEvent = safeRun(chatHandlers, event)

final class CommandContext(val api: ModApi, val command: String, val args: Array[String], val playerName: String, val remote: Boolean):
  def reply(message: String): Unit = api.say(Option(message).getOrElse(""))
  def player: PlayerRef = api.player(playerName)
  def arg(index: Int, defaultValue: String = ""): String = if index >= 0 && index < args.length then args(index) else defaultValue
  def intArg(index: Int, defaultValue: Int = 0): Int = scala.util.Try(arg(index).toInt).getOrElse(defaultValue)
  def floatArg(index: Int, defaultValue: Float = 0f): Float = scala.util.Try(arg(index).toFloat).getOrElse(defaultValue)
  def isOp: Boolean = api.game.modIsOpped(playerName) || api.game.modCanUseCheats
  def requireOp(): Boolean =
    if isOp then true
    else
      reply("This mod command requires cheats/op.")
      false

final class ModCommandRegistry(manager: ModManager):
  private final case class Entry(handler: Consumer[CommandContext], serverInteractive: Boolean, usage: String, description: String, var failures: Int = 0, var disabled: Boolean = false)
  private val commands = scala.collection.mutable.LinkedHashMap.empty[String, Entry]
  def register(name: String, handler: Consumer[CommandContext]): Unit = register(name, true, handler)
  def register(name: String, handler: Closure[?]): Unit = register(name, true, ModRuntime.consumer[CommandContext](handler))
  def registerClient(name: String, handler: Consumer[CommandContext]): Unit = register(name, false, handler)
  def registerClient(name: String, handler: Closure[?]): Unit = register(name, false, ModRuntime.consumer[CommandContext](handler))
  def register(name: String, serverInteractive: Boolean, handler: Closure[?]): Unit = register(name, serverInteractive, ModRuntime.consumer[CommandContext](handler))
  def register(name: String, serverInteractive: Boolean, handler: Consumer[CommandContext]): Unit = register(name, serverInteractive, "/" + Option(name).getOrElse("").trim.stripPrefix("/"), "mod command", handler)
  def register(name: String, serverInteractive: Boolean, usage: String, description: String, handler: Closure[?]): Unit = register(name, serverInteractive, usage, description, ModRuntime.consumer[CommandContext](handler))
  def register(name: String, serverInteractive: Boolean, usage: String, description: String, handler: Consumer[CommandContext]): Unit =
    val clean = Option(name).getOrElse("").trim.toLowerCase.stripPrefix("/")
    if clean.nonEmpty && handler != null then commands(clean) = Entry(handler, serverInteractive, Option(usage).getOrElse("/" + clean), Option(description).getOrElse("mod command"))
  def has(name: String): Boolean = commands.contains(Option(name).getOrElse("").trim.toLowerCase.stripPrefix("/"))
  def isServerInteractive(name: String): Boolean = commands.get(Option(name).getOrElse("").trim.toLowerCase.stripPrefix("/")).exists(_.serverInteractive)
  def names: Seq[String] = commands.keys.toSeq
  def help: Seq[String] = commands.toSeq.map { case (name, e) => s"/${name} - ${e.description}" }
  def execute(name: String, args: Array[String], playerName: String, remote: Boolean): Boolean =
    commands.get(Option(name).getOrElse("").trim.toLowerCase.stripPrefix("/")) match
      case Some(entry) if entry.disabled =>
        manager.api.say(s"Mod command /$name is disabled after repeated errors.")
        true
      case Some(entry) =>
        try entry.handler.accept(CommandContext(manager.api, name, args, playerName, remote))
        catch
          case e: Throwable =>
            entry.failures += 1
            val msg = Option(e.getMessage).filter(_.nonEmpty).getOrElse(e.getClass.getSimpleName)
            manager.api.say("Mod command failed: " + msg)
            System.err.println(s"Blockbox mod command '/$name' failed (${entry.failures}/5): $msg")
            if entry.failures >= 5 then
              entry.disabled = true
              manager.api.say(s"Disabled failing mod command /$name for this session.")
        true
      case None => false

final class PlayerRef(private val api0: ModApi, val name: String):
  def sendMessage(message: String): Unit = api0.say("[mod] " + Option(message).getOrElse(""))
  def give(block: Block, count: Int): Unit = api0.game.modGiveItem(block, count)
  def give(blockName: String, count: Int): Unit = give(api0.block(blockName), count)
  def give(block: Object, count: Int): Unit =
    block match
      case b: Block => give(b, count)
      case s: String => give(s, count)
      case _ => sendMessage("cannot give unknown block value: " + Option(block).fold("null")(_.toString))
  def x: Float = api0.game.modCamera.x
  def y: Float = api0.game.modCamera.y
  def z: Float = api0.game.modCamera.z
  def blockX: Int = floor(x).toInt
  def blockY: Int = floor(y).toInt
  def blockZ: Int = floor(z).toInt
  def position: Vec3 = Vec3(x, y, z)
  def teleport(x: Float, y: Float, z: Float): Unit = api0.game.modTeleportSelf(Vec3(x, y, z))
  def teleport(x: Number, y: Number, z: Number): Unit = teleport(x.floatValue(), y.floatValue(), z.floatValue())
  def health: Float = api0.game.modHealth
  def setHealth(value: Number): Unit = api0.game.modSetHealth(if value == null then 20f else value.floatValue())
  def food: Float = api0.game.modFood
  def setFood(value: Number): Unit = api0.game.modSetFood(if value == null then 20f else value.floatValue())
  def isOp: Boolean = api0.game.modIsOpped(name)

final class WorldApi(private[blockbox] val game: Blockbox):
  private def i(value: Number): Int = if value == null then 0 else value.intValue()
  def getBlock(x: Int, y: Int, z: Int): Block = game.modGetBlock(x, y, z)
  def getBlock(x: Number, y: Number, z: Number): Block = getBlock(i(x), i(y), i(z))
  def setBlock(x: Int, y: Int, z: Int, block: Block): Unit = game.modSetBlock(x, y, z, block)
  def setBlock(x: Int, y: Int, z: Int, blockName: String): Unit = setBlock(x, y, z, game.modResolveBlock(blockName))
  def setBlock(x: Number, y: Number, z: Number, block: Block): Unit = setBlock(i(x), i(y), i(z), block)
  def setBlock(x: Number, y: Number, z: Number, blockName: String): Unit = setBlock(i(x), i(y), i(z), blockName)
  def isAir(x: Int, y: Int, z: Int): Boolean = getBlock(x, y, z) == Block.Air
  def isSolid(x: Int, y: Int, z: Int): Boolean = getBlock(x, y, z).solid
  def fill(x1: Int, y1: Int, z1: Int, x2: Int, y2: Int, z2: Int, block: Block, limit: Int = 4096): Int =
    val minX = x1.min(x2); val maxX = x1.max(x2)
    val minY = y1.min(y2).max(0); val maxY = y1.max(y2).min(Terrain.worldHeight - 1)
    val minZ = z1.min(z2); val maxZ = z1.max(z2)
    var changed = 0
    var x = minX
    while x <= maxX && changed < limit do
      var y = minY
      while y <= maxY && changed < limit do
        var z = minZ
        while z <= maxZ && changed < limit do
          setBlock(x, y, z, block)
          changed += 1
          z += 1
        y += 1
      x += 1
    changed
  def fill(x1: Number, y1: Number, z1: Number, x2: Number, y2: Number, z2: Number, blockName: String): Int = fill(i(x1), i(y1), i(z1), i(x2), i(y2), i(z2), game.modResolveBlock(blockName))
  def seed: Long = game.modWorldSeed
  def name: String = game.modWorldName

final class ModGuiApi(private[blockbox] val game: Blockbox):
  private def f(value: Number): Float = if value == null then 0f else value.floatValue()
  def width: Float = game.modFramebufferWidth
  def height: Float = game.modFramebufferHeight
  def scale: Float = game.modUiScale
  def mouseX: Float = game.modMouseX
  def mouseY: Float = game.modMouseY
  def leftDown: Boolean = game.modLeftDown
  def leftClicked: Boolean = game.modLeftClicked
  def cursorMode: Boolean = game.modCursorMode
  def setCursorMode(enabled: Boolean): Unit = game.modSetCursorMode(enabled)
  def toggleCursorMode(): Unit = game.modSetCursorMode(!game.modCursorMode)
  def isPlaying: Boolean = game.modScreenName == "playing"
  def rect(x: Float, y: Float, w: Float, h: Float, r: Float, g: Float, b: Float, a: Float): Unit = game.modDrawRect(x, y, w, h, r, g, b, a)
  def rect(x: Number, y: Number, w: Number, h: Number, r: Number, g: Number, b: Number, a: Number): Unit = rect(f(x), f(y), f(w), f(h), f(r), f(g), f(b), f(a))
  def text(x: Float, y: Float, text: String, r: Float, g: Float, b: Float, scale: Float): Unit = game.modDrawText(x, y, Option(text).getOrElse(""), r, g, b, scale)
  def text(x: Number, y: Number, text: String, r: Number, g: Number, b: Number, scale: Number): Unit = this.text(f(x), f(y), text, f(r), f(g), f(b), f(scale))
  def textShadow(x: Float, y: Float, text: String, r: Float, g: Float, b: Float, scale: Float): Unit = game.modDrawTextShadow(x, y, Option(text).getOrElse(""), r, g, b, scale)
  def textShadow(x: Number, y: Number, text: String, r: Number, g: Number, b: Number, scale: Number): Unit = textShadow(f(x), f(y), text, f(r), f(g), f(b), f(scale))
  def button(x: Float, y: Float, w: Float, h: Float, label: String): Unit = game.modDrawButton(x, y, w, h, Option(label).getOrElse(""))
  def button(x: Number, y: Number, w: Number, h: Number, label: String): Unit = button(f(x), f(y), f(w), f(h), label)
  def buttonClicked(x: Number, y: Number, w: Number, h: Number, label: String): Boolean =
    val fx = f(x); val fy = f(y); val fw = f(w); val fh = f(h)
    button(fx, fy, fw, fh, label)
    cursorMode && leftClicked && inRect(mouseX, mouseY, fx, fy, fw, fh)
  def panel(x: Number, y: Number, w: Number, h: Number, title: String): Unit =
    val fx = f(x); val fy = f(y); val fw = f(w); val fh = f(h)
    rect(fx, fy, fw, fh, 0.02f, 0.025f, 0.04f, 0.78f)
    rect(fx, fy, fw, 24f * scale, 0.08f, 0.10f, 0.16f, 0.92f)
    textShadow(fx + 10f * scale, fy + 7f * scale, Option(title).getOrElse("mod panel"), 0.85f, 0.93f, 1f, 0.58f * scale)
  def inRect(mx: Float, my: Float, x: Float, y: Float, w: Float, h: Float): Boolean = game.modInRect(mx, my, x, y, w, h)
  def inRect(mx: Number, my: Number, x: Number, y: Number, w: Number, h: Number): Boolean = inRect(f(mx), f(my), f(x), f(y), f(w), f(h))

final case class ModBlockDef(id: String, name: String, description: String, backing: Block, serverInteractive: Boolean = true)

final class ModContentRegistry(private val manager: ModManager):
  def registerBlock(id: String, backing: Block): ModBlockDef = registerBlock(id, id, "", backing)
  def registerBlock(id: String, backingName: String): ModBlockDef = registerBlock(id, id, "", manager.resolveBlockName(backingName))
  def registerBlock(id: String, name: String, description: String, backing: Block): ModBlockDef =
    val defn = ModBlockDef(ModRuntime.normalizeId(id), Option(name).filter(_.nonEmpty).getOrElse(id), Option(description).getOrElse(""), if backing == null then Block.Air else backing)
    manager.registerBlockDefinition(defn)
    defn
  def registerBlock(id: String, name: String, description: String, backingName: String): ModBlockDef = registerBlock(id, name, description, manager.resolveBlockName(backingName))
  def blocks: Seq[ModBlockDef] = manager.registeredBlocks
  def blockIds: Seq[String] = blocks.map(_.id)

final class ModFilesApi:
  private def safeId(id: String): String = ModRuntime.normalizeId(id)
  def modsDir: File = File("mods")
  def configDir(modId: String): File =
    val d = File(File("config"), "blockbox-mods/" + safeId(modId))
    d.mkdirs(); d
  def dataDir(modId: String): File =
    val d = File(File("worlds/moddata"), safeId(modId))
    d.mkdirs(); d
  def readText(file: File, defaultValue: String = ""): String =
    try if file != null && file.isFile then String(java.nio.file.Files.readAllBytes(file.toPath), StandardCharsets.UTF_8) else defaultValue
    catch case _: Throwable => defaultValue
  def writeText(file: File, text: String): Unit =
    if file != null then
      val parent = file.getParentFile
      if parent != null then parent.mkdirs()
      java.nio.file.Files.writeString(file.toPath, Option(text).getOrElse(""), StandardCharsets.UTF_8)

final class ModTask(private val cancelImpl: () => Unit):
  def cancel(): Unit = cancelImpl()

final class ModScheduler:
  private final case class Task(var remaining: Float, interval: Float, repeats: Boolean, action: Runnable, var cancelled: Boolean = false)
  private val tasks = ArrayBuffer.empty[Task]
  private def f(n: Number): Float = if n == null then 0f else n.floatValue().max(0f)
  def runLater(seconds: Number, action: Runnable): ModTask =
    val t = Task(f(seconds), 0f, false, action)
    if action != null then tasks += t
    ModTask(() => t.cancelled = true)
  def runLater(seconds: Number, action: Closure[?]): ModTask = runLater(seconds, ModRuntime.runnable(action))
  def every(seconds: Number, action: Runnable): ModTask =
    val interval = f(seconds).max(0.01f)
    val t = Task(interval, interval, true, action)
    if action != null then tasks += t
    ModTask(() => t.cancelled = true)
  def every(seconds: Number, action: Closure[?]): ModTask = every(seconds, ModRuntime.runnable(action))
  private[blockbox] def tick(dt: Float): Unit =
    tasks.foreach { t =>
      if !t.cancelled then
        t.remaining -= dt
        if t.remaining <= 0f then
          try t.action.run() catch case e: Throwable => System.err.println("Blockbox scheduled mod task failed: " + Option(e.getMessage).getOrElse(e.getClass.getSimpleName))
          if t.repeats then t.remaining += t.interval else t.cancelled = true
    }
    tasks.filterInPlace(!_.cancelled)

final class ModLogger(private val prefix: String):
  private def line(level: String, message: String): Unit = System.out.println(s"[Blockbox/$level/$prefix] " + Option(message).getOrElse(""))
  def info(message: String): Unit = line("INFO", message)
  def warn(message: String): Unit = line("WARN", message)
  def error(message: String): Unit = line("ERROR", message)

final class ModApi(private[blockbox] val game: Blockbox, private val manager: ModManager):
  val world = WorldApi(game)
  def gui: ModGuiApi = manager.gui
  def events: ModEventBus = manager.events
  def commands: ModCommandRegistry = manager.commands
  def content: ModContentRegistry = manager.content
  def files: ModFilesApi = manager.files
  def scheduler: ModScheduler = manager.scheduler
  def log: ModLogger = manager.log
  def block(name: String): Block = manager.resolveBlockName(name)
  def blocks: Array[Block] = Block.all
  def blockNames: Array[String] = Block.names
  def registeredBlocks: Seq[ModBlockDef] = manager.registeredBlocks
  def player(name: String): PlayerRef = PlayerRef(this, name)
  def localPlayer: PlayerRef = PlayerRef(this, game.modLocalPlayerName)
  def say(message: String): Unit = game.modAddChatMessage(Option(message).getOrElse(""))
  def loadedMods: Seq[ModDescriptor] = manager.loadedMods
  def serverModpackHash: String = manager.serverModpackHash
  def apiVersion: String = "v51"

object ModJson:
  private def unescape(s: String): String = s.replace("\\\"", "\"").replace("\\n", "\n").replace("\\r", "\r").replace("\\\\", "\\")
  def string(json: String, key: String): Option[String] =
    val r = ("\\\"" + java.util.regex.Pattern.quote(key) + "\\\"\\s*:\\s*\\\"((?:\\\\.|[^\\\"])*)\\\"").r
    r.findFirstMatchIn(json).map(m => unescape(m.group(1)))
  def bool(json: String, key: String): Option[Boolean] =
    val r = ("\\\"" + java.util.regex.Pattern.quote(key) + "\\\"\\s*:\\s*(true|false)").r
    r.findFirstMatchIn(json).map(m => m.group(1).equalsIgnoreCase("true"))

object ModRuntime:
  val VanillaHash = "vanilla"
  def sha256Bytes(bytes: Array[Byte]): String =
    val md = MessageDigest.getInstance("SHA-256")
    md.digest(bytes).map(b => f"${b & 0xff}%02x").mkString
  def sha256File(file: File): String =
    val md = MessageDigest.getInstance("SHA-256")
    val in = BufferedInputStream(FileInputStream(file))
    val buf = new Array[Byte](8192)
    try
      var n = in.read(buf)
      while n >= 0 do
        if n > 0 then md.update(buf, 0, n)
        n = in.read(buf)
    finally in.close()
    md.digest().map(b => f"${b & 0xff}%02x").mkString
  def normalizeId(raw: String): String =
    val clean = Option(raw).getOrElse("mod").trim.toLowerCase.filter(ch => ch.isLetterOrDigit || ch == '_' || ch == '-' || ch == '.')
    if clean.nonEmpty then clean.take(64) else "mod"
  def normalizeSide(raw: String): String = Option(raw).getOrElse("both").trim.toLowerCase match
    case "client" | "client-only" => "client"
    case "server" | "server-only" => "server"
    case _ => "both"
  def consumer[A](closure: Closure[?]): Consumer[A] = new Consumer[A]:
    override def accept(value: A): Unit =
      if closure != null then
        if closure.getMaximumNumberOfParameters == 0 then closure.call()
        else closure.call(value.asInstanceOf[Object])
  def runnable(closure: Closure[?]): Runnable = new Runnable:
    override def run(): Unit = if closure != null then closure.call()

final class ModManager(private val game: Blockbox):
  val events = ModEventBus()
  val commands = ModCommandRegistry(this)
  val gui = ModGuiApi(game)
  val files = ModFilesApi()
  val scheduler = ModScheduler()
  val content = ModContentRegistry(this)
  val log = ModLogger("mods")
  val api = ModApi(game, this)
  private val descriptors = ArrayBuffer.empty[ModDescriptor]
  private val liveMods = ArrayBuffer.empty[BlockboxMod]
  private val blockDefinitions = scala.collection.mutable.LinkedHashMap.empty[String, ModBlockDef]
  private var loaded = false
  private var serverHashCached = ModRuntime.VanillaHash
  private var serverListCached = ""
  def loadedMods: Seq[ModDescriptor] = descriptors.toSeq
  def serverModpackHash: String = serverHashCached
  def serverModpackList: String = serverListCached
  def clientJoinHash: String = serverHashCached
  def registerBlockDefinition(defn: ModBlockDef): Unit =
    if defn != null && defn.id.nonEmpty then blockDefinitions(defn.id) = defn
  def registeredBlocks: Seq[ModBlockDef] = blockDefinitions.values.toSeq
  def resolveBlockName(name: String): Block =
    val raw = Option(name).getOrElse("")
    val key = ModRuntime.normalizeId(raw.stripPrefix("blockbox:"))
    blockDefinitions.get(key).map(_.backing).orElse(Block.find(raw)).getOrElse(Block.Air)
  def loadAll(): Unit =
    if loaded then return
    loaded = true
    val root = File("mods")
    root.mkdirs()
    writeTemplate(root)
    val entries = Option(root.listFiles()).getOrElse(Array.empty[File]).sortBy(_.getName.toLowerCase)
    entries.foreach { f =>
      try
        if f.isFile && f.getName.toLowerCase.endsWith(".jar") then loadJar(f)
        else if f.isFile && f.getName.toLowerCase.endsWith(".groovy") then loadGroovyFile(f, None)
        else if f.isDirectory then loadFolder(f)
      catch
        case e: Throwable =>
          val id = ModRuntime.normalizeId(f.getName.stripSuffix(".jar").stripSuffix(".groovy"))
          descriptors += ModDescriptor(id, id, "unknown", "failed to scan", "both", true, f.getPath, "", hashPath(f), loaded = false, status = "scan failed: " + Option(e.getMessage).getOrElse(e.getClass.getSimpleName))
    }
    recomputeServerHash()
    System.out.println(s"Blockbox: loaded ${descriptors.count(_.loaded)} mods, server modpack ${serverHashCached}")
  def shutdown(): Unit = liveMods.foreach { m => try m.onShutdown(api) catch case e: Throwable => System.err.println("Mod shutdown failed: " + e.getMessage) }
  def fireWorldLoaded(): Unit = liveMods.foreach { m => try m.onWorldLoaded(api) catch case e: Throwable => System.err.println("Mod world-load failed: " + e.getMessage) }
  def fireWorldTick(dt: Float): Unit =
    scheduler.tick(dt)
    events.fireWorldTick(ModWorldTickEvent(api, dt))
  def fireHudRender(dt: Float): Unit = events.fireHudRender(ModHudRenderEvent(api, gui, dt))
  def fireMouseClick(mx: Float, my: Float, button: Int): ModMouseClickEvent = events.fireMouseClick(ModMouseClickEvent(api, gui, mx, my, button))
  def fireKeyPress(e: ModKeyEvent): ModKeyEvent = events.fireKeyPress(e)
  def fireChat(e: ModChatEvent): ModChatEvent = events.fireChat(e)
  def fireBlockBreak(e: ModBlockBreakEvent): ModBlockBreakEvent = events.fireBlockBreak(e)
  def fireBlockPlace(e: ModBlockPlaceEvent): ModBlockPlaceEvent = events.fireBlockPlace(e)
  def hasCommand(name: String): Boolean = commands.has(name)
  def commandIsServerInteractive(name: String): Boolean = commands.isServerInteractive(name)
  def executeCommand(name: String, args: Array[String], playerName: String, remote: Boolean): Boolean = commands.execute(name, args, playerName, remote)
  def commandNames: Seq[String] = commands.names
  private def hashPath(f: File): String =
    if f.isFile then ModRuntime.sha256File(f)
    else
      val bytes = Option(f.listFiles()).getOrElse(Array.empty[File]).sortBy(_.getName).flatMap { c =>
        if c.isFile then (c.getName + ":" + ModRuntime.sha256File(c)).getBytes(StandardCharsets.UTF_8) else Array.emptyByteArray
      }
      ModRuntime.sha256Bytes(bytes)
  private def readAll(in: InputStream): String =
    try String(in.readAllBytes(), StandardCharsets.UTF_8) finally in.close()
  private def descriptorFromJson(json: String, source: String, fallbackId: String, fallbackHash: String): ModDescriptor =
    val id = ModRuntime.normalizeId(ModJson.string(json, "id").getOrElse(fallbackId))
    val name = ModJson.string(json, "name").getOrElse(id)
    val version = ModJson.string(json, "version").getOrElse("1.0.0")
    val description = ModJson.string(json, "description").getOrElse("No description provided.")
    val side = ModRuntime.normalizeSide(ModJson.string(json, "side").getOrElse("both"))
    val serverInteractive = ModJson.bool(json, "serverInteractive").getOrElse(side != "client")
    val main = ModJson.string(json, "main").getOrElse("")
    ModDescriptor(id, name, version, description, side, serverInteractive, source, main, fallbackHash)
  private def loadJar(jarFile: File): Unit =
    val hash = ModRuntime.sha256File(jarFile)
    val jf = JarFile(jarFile)
    val json = try
      val entry = Option(jf.getEntry("blockbox.mod.json")).orElse(Option(jf.getEntry("mod.json")))
      entry.map(e => readAll(jf.getInputStream(e))).getOrElse("{}")
    finally jf.close()
    val fallback = jarFile.getName.stripSuffix(".jar")
    val desc = descriptorFromJson(json, jarFile.getPath, fallback, hash)
    val main = desc.mainClass.trim.stripPrefix("/")
    if main.isEmpty then
      desc.loaded = false; desc.status = "metadata read, but no main class"
      descriptors += desc
    else if main.toLowerCase.endsWith(".groovy") then
      loadGroovyFromJar(jarFile, desc, main)
      descriptors += desc
    else
      val loader = URLClassLoader(Array(jarFile.toURI.toURL), getClass.getClassLoader)
      instantiateMod(desc, Class.forName(main, true, loader))
      descriptors += desc

  private def loadGroovyFromJar(jarFile: File, desc: ModDescriptor, main: String): Unit =
    val jf = JarFile(jarFile)
    try
      val clean = main.stripPrefix("/")
      val candidates = Seq(clean, s"scripts/$clean", s"groovy/$clean", clean.split('/').lastOption.getOrElse(clean)).distinct
      val entry = candidates.view.flatMap(name => Option(jf.getEntry(name))).headOption
      entry match
        case Some(e) =>
          val source = readAll(jf.getInputStream(e))
          val loader = GroovyClassLoader(getClass.getClassLoader)
          instantiateMod(desc, loader.parseClass(source, e.getName))
        case None =>
          desc.loaded = false
          desc.status = "main groovy script not found in jar: " + clean
    catch
      case e: Throwable =>
        desc.loaded = false
        val msg = Option(e.getMessage).getOrElse(e.getClass.getSimpleName)
        val hint = if msg.contains("Unsupported class file major version") then " (Groovy/JVM classfile mismatch; Blockbox requires Groovy 5+ on Java 25)" else ""
        desc.status = "jar groovy load failed: " + msg + hint
    finally jf.close()
  private def loadFolder(dir: File): Unit =
    val jsonFile = File(dir, "blockbox.mod.json")
    val altJson = File(dir, "mod.json")
    val metaFile = if jsonFile.isFile then jsonFile else altJson
    if metaFile.isFile then
      val json = String(java.nio.file.Files.readAllBytes(metaFile.toPath), StandardCharsets.UTF_8)
      val main = ModJson.string(json, "main").getOrElse("main.groovy")
      loadGroovyFile(File(dir, main), Some((json, dir.getPath, hashPath(dir))))
  private def loadGroovyFile(file: File, meta: Option[(String, String, String)]): Unit =
    if !file.isFile then return
    val hash = meta.map(_._3).getOrElse(ModRuntime.sha256File(file))
    val fallback = file.getName.stripSuffix(".groovy")
    val desc = meta.map(m => descriptorFromJson(m._1, m._2, fallback, hash)).getOrElse {
      val text = String(java.nio.file.Files.readAllBytes(file.toPath), StandardCharsets.UTF_8)
      val side = if text.contains("blockbox-side: client") then "client" else "both"
      ModDescriptor(ModRuntime.normalizeId(fallback), fallback, "1.0.0", "Loose Groovy script mod", side, side != "client", file.getPath, file.getName, hash)
    }
    try
      val loader = GroovyClassLoader(getClass.getClassLoader)
      instantiateMod(desc, loader.parseClass(file))
    catch
      case e: Throwable =>
        desc.loaded = false
        val msg = Option(e.getMessage).getOrElse(e.getClass.getSimpleName)
        val hint = if msg.contains("Unsupported class file major version") then " (Groovy/JVM classfile mismatch; Blockbox requires Groovy 5+ on Java 25)" else ""
        desc.status = "loose groovy load failed: " + msg + hint
    descriptors += desc
  private def instantiateMod(desc: ModDescriptor, cls: Class[?]): Unit =
    val obj = cls.getDeclaredConstructor().newInstance()
    obj match
      case mod: BlockboxMod =>
        mod.onInit(api); liveMods += mod; desc.loaded = true; desc.status = "loaded"
      case other =>
        cls.getMethods.filter(_.getName == "init").find(_.getParameterCount == 1) match
          case Some(m) => m.invoke(other, api); desc.loaded = true; desc.status = "loaded script init(api)"
          case None => desc.loaded = false; desc.status = "no BlockboxMod implementation or init(api) method"
  private def recomputeServerHash(): Unit =
    val required = descriptors.filter(d => d.loaded && d.serverInteractive).sortBy(d => (d.id, d.version))
    serverListCached = required.map(d => s"${d.id}@${d.version}:${d.sha256.take(12)}").mkString(",")
    serverHashCached = if serverListCached.isEmpty then ModRuntime.VanillaHash else ModRuntime.sha256Bytes(serverListCached.getBytes(StandardCharsets.UTF_8)).take(24)
  private def writeTemplate(root: File): Unit =
    val docs = File(root, "README_modding.txt")
    if !docs.exists() then
      val text = """blockbox mods folder

Blockbox mods are trusted full-access JVM code, like Minecraft Java mods. Only install mods you trust.
Groovy is the primary scripting style; jar mods can be Java, Kotlin, Scala, Groovy, Clojure, or any JVM language.

preferred Groovy folder mod:
  mods/coolmod/
    blockbox.mod.json
    main.groovy
    assets/
    config/

blockbox.mod.json:
  {
    "id": "coolmod",
    "name": "Cool Mod",
    "version": "1.0.0",
    "description": "what this mod does",
    "side": "both",
    "serverInteractive": true,
    "main": "main.groovy"
  }

side values:
  client = gui/visual/client-only, does not block joining
  server = gameplay/server authority, must match host
  both = client + server mod

serverInteractive true means players must have the same server modpack hash to join a host.

main.groovy example:
  def init(api) {
    api.log.info("loaded")

    api.commands.register("hello", false, "/hello", "client-safe greeting") { ctx ->
      ctx.reply("hello from " + api.apiVersion)
    }

    api.commands.register("stonebox", true, "/stonebox", "server-side world edit demo") { ctx ->
      if (!ctx.requireOp()) return
      def p = ctx.player
      api.world.fill(p.blockX - 1, p.blockY - 2, p.blockZ - 1, p.blockX + 1, p.blockY - 2, p.blockZ + 1, "stone")
    }

    api.events.onBlockBreak { e ->
      e.playerName
    }

    api.events.onHudRender { e ->
      def g = e.gui
      if (g.cursorMode && g.buttonClicked(20, 90, 180, 28, "mod button")) {
        api.localPlayer.sendMessage("clicked")
      }
    }
  }

useful api:
  api.block("stone")
  api.content.registerBlock("coolmod:my_block", "My Block", "uses rainbow backing for now", "RainbowBlock")
  api.localPlayer.give("stone", 16)
  api.world.setBlock(x, y, z, "stone")
  api.scheduler.runLater(1.0) { api.say("one second later") }
  api.scheduler.every(5.0) { api.say("every five seconds") }
  api.files.configDir("coolmod")
  api.gui.buttonClicked(x, y, w, h, "label")

jar mods:
  put coolmod.jar in mods/. The jar should contain blockbox.mod.json at jar root.
  If main ends with .groovy, Blockbox loads that Groovy script from inside the jar.
  Otherwise main should be a JVM class that implements blockbox.BlockboxMod or has init(api).

Press F8 in-game to unlock the mouse cursor for clickable mod UI.
"""
      java.nio.file.Files.writeString(docs.toPath, text, StandardCharsets.UTF_8)

final class GameServer(
  port: Int,
  worldSeed: Long,
  spawn: Vec3,
  hostName: String,
  hostColorId: Int,
  onBlockChange: (Int, Int, Int, Byte) => Unit,
  worldSnapshot: () => Seq[(Int, Int, Int, Byte)],
  opSnapshot: () => Seq[String],
  hostPosition: () => Vec3,
  requiredModHash: String,
  requiredModList: String
):
  import GameServer.*
  private val serverSocket =
    val s = ServerSocket()
    s.setReuseAddress(true)
    // Keep this explicitly IPv4 for local/LAN testing. 0.0.0.0 accepts 127.0.0.1, LAN, and ZeroTier IPv4.
    s.bind(InetSocketAddress(InetAddress.getByName("0.0.0.0"), port))
    s
  private val clients = collection.mutable.ListBuffer[ClientHandler]()
  private val serverEvents = new LinkedBlockingQueue[String]()
  private val running = java.util.concurrent.atomic.AtomicBoolean(true)
  private val acceptThread = Thread(() =>
    try
      while running.get do
        try
          val sock = serverSocket.accept()
          sock.setTcpNoDelay(true)
          sock.setKeepAlive(true)
          sock.setSoTimeout(0)
          val handler = ClientHandler(sock)
          handler.start()
        catch case _: SocketTimeoutException => ()
    catch
      case _: SocketException => running.set(false)
      case _: IOException => running.set(false)
  , "blockbox-server-accept")
  acceptThread.setDaemon(true)
  acceptThread.start()

  def localPort: Int = serverSocket.getLocalPort
  def isListening: Boolean = running.get && serverSocket.isBound && !serverSocket.isClosed

  def broadcast(msg: String): Unit =
    clients.synchronized(clients.filter(_.active).foreach(_.send(msg)))

  def pollMessage(): Option[String] = Option(serverEvents.poll())

  def stop(): Unit =
    running.set(false)
    clients.synchronized(clients.foreach(_.close()))
    try serverSocket.close() catch case _: IOException => ()

  private def safeName(value: String): String =
    val cleaned = Option(value).getOrElse("Player").trim.filter(ch => ch.isLetterOrDigit || ch == '_' || ch == '-').take(16)
    if cleaned.nonEmpty then cleaned else "Player"

  private val safeHostName = safeName(hostName)
  private val colorCount = 10
  private val safeHostColor = Math.floorMod(hostColorId, colorCount)

  private def playerToken(name: String, color: Int): String =
    safeName(name) + ":" + Math.floorMod(color, colorCount).toString

  private def activePlayerTokensSnapshot: List[String] =
    clients.synchronized {
      (playerToken(safeHostName, safeHostColor) ::
        clients.filter(c => c.active && c.registered).map(c => playerToken(c.onlineName, c.colorId)).toList).distinct
    }

  private def assignColor(): Int =
    clients.synchronized {
      val used = (safeHostColor :: clients.filter(c => c.active && c.registered).map(_.colorId).toList).map(c => Math.floorMod(c, colorCount)).toSet
      (0 until colorCount).find(c => !used.contains(c)).getOrElse((used.size + 1) % colorCount)
    }

  private def isNameInUse(name: String): Boolean =
    val safe = safeName(name)
    safe.equalsIgnoreCase(safeHostName) || clients.synchronized {
      clients.exists(c => c.active && c.registered && c.onlineName.equalsIgnoreCase(safe))
    }

  private def registerClient(client: ClientHandler): Unit =
    clients.synchronized {
      if !clients.exists(_ eq client) then clients += client
    }

  private def unregisterClient(client: ClientHandler): Unit =
    clients.synchronized {
      clients -= client
    }

  private def blockIntersectsPlayer(wx: Int, wy: Int, wz: Int, pos: Vec3): Boolean =
    val half = 0.38f
    val minX = floor(pos.x - half).toInt; val maxX = floor(pos.x + half).toInt
    val minY = floor(pos.y - 1.70f).toInt; val maxY = floor(pos.y + 0.22f).toInt
    val minZ = floor(pos.z - half).toInt; val maxZ = floor(pos.z + half).toInt
    wx >= minX && wx <= maxX && wy >= minY && wy <= maxY && wz >= minZ && wz <= maxZ

  private def canAcceptBlockChange(wx: Int, wy: Int, wz: Int, blockId: Byte): Boolean =
    val id = blockId.toInt & 0xFF
    if id == 0 then true
    else
      val hostBlocked = blockIntersectsPlayer(wx, wy, wz, hostPosition())
      if hostBlocked then false
      else
        clients.synchronized {
          !clients.exists(c => c.active && c.registered && c.hasPosition && blockIntersectsPlayer(wx, wy, wz, c.lastPos))
        }

  private class ClientHandler(sock: Socket) extends Thread("blockbox-client-handler"):
    private val out = PrintWriter(BufferedWriter(OutputStreamWriter(sock.getOutputStream)), true)
    private val in = BufferedReader(InputStreamReader(sock.getInputStream))
    @volatile var active = true
    @volatile var registered = false
    @volatile var onlineName = "Player"
    @volatile var colorId = 0
    @volatile var hasPosition = false
    @volatile var lastPos: Vec3 = spawn

    override def run(): Unit =
      try
        var line = in.readLine()
        while line != null && active do
          val parts = line.split("\\|", -1)
          if line.startsWith("PING") then
            send("PONG|BLOCKBOX|1")
            active = false
          else if line.startsWith("HELO|") then
            val requestedName =
              if parts.length >= 2 && parts(1).nonEmpty then safeName(parts(1))
              else "Player"
            val clientModHash = if parts.length >= 3 && parts(2).nonEmpty then parts(2) else ModRuntime.VanillaHash
            if requiredModHash != ModRuntime.VanillaHash && clientModHash != requiredModHash then
              val list = if requiredModList.trim.nonEmpty then requiredModList else requiredModHash
              send("ERR|MOD_MISMATCH|Server requires modpack " + requiredModHash + " [" + list + "]")
              active = false
            else if isNameInUse(requestedName) then
              send("ERR|NAME_TAKEN|That name is already in this world")
              active = false
            else
              onlineName = requestedName
              colorId = assignColor()
              val worldMsg = "WORLD|" + worldSeed.toString + "|" + f"${spawn.x}%.3f" + "|" + f"${spawn.y}%.3f" + "|" + f"${spawn.z}%.3f" + "|" + colorId.toString + "|" + safeHostName + "|" + safeHostColor.toString
              send(worldMsg)
              send("PLAYERS|" + activePlayerTokensSnapshot.mkString("|"))
              val opList =
                try opSnapshot().map(safeName).filter(_.nonEmpty).distinct
                catch case _: Exception => Seq.empty[String]
              send("OPLIST|" + opList.mkString("|"))
              val snapshot =
                try worldSnapshot().take(250000)
                catch case _: Exception => Seq.empty[(Int, Int, Int, Byte)]
              send("SNAPBEGIN|" + snapshot.length.toString)
              snapshot.foreach { case (sx, sy, sz, sid) => send("BLOC|" + sx + "|" + sy + "|" + sz + "|" + sid.toString) }
              send("SNAPEND")
              registered = true
              registerClient(this)
              val joinMsg = s"JOIN|$onlineName|$colorId"
              serverEvents.put(joinMsg)
              broadcast(joinMsg)
          else if registered && line.startsWith("BLOC|") then
            if parts.length >= 5 then
              val x = parts(1).toInt; val y = parts(2).toInt; val z = parts(3).toInt
              val blockId = parts(4).toByte
              if canAcceptBlockChange(x, y, z, blockId) then
                val level =
                  if blockId == Block.Water.id && parts.length >= 6 then
                    try parts(5).toInt.max(1).min(8) catch case _: Exception => 8
                  else if blockId == Block.Water.id then 8 else 0
                val clean = "BLOC|" + x + "|" + y + "|" + z + "|" + blockId.toString + (if blockId == Block.Water.id then "|" + level.toString else "")
                onBlockChange(x, y, z, blockId)
                serverEvents.put(clean)
                broadcast(clean)
          else if registered && line.startsWith("POS|") then
            if parts.length >= 7 then
              val px = parts(2).toFloat; val py = parts(3).toFloat; val pz = parts(4).toFloat
              lastPos = Vec3(px, py, pz)
              hasPosition = true
              val clean = "POS|" + onlineName + "|" + parts(2) + "|" + parts(3) + "|" + parts(4) + "|" + parts(5) + "|" + parts(6) + "|" + colorId.toString
              serverEvents.put(clean)
              broadcast(clean)
          else if registered && line.startsWith("CMD|") then
            val text = if parts.length >= 2 then parts.drop(1).mkString("|") else ""
            if text.nonEmpty then serverEvents.put("RCMD|" + onlineName + "|" + text)
          else if registered && line.startsWith("CHAT|") then
            val text = if parts.length >= 3 then parts.drop(2).mkString("|") else ""
            val clean = "CHAT|" + onlineName + "|" + text
            serverEvents.put(clean)
            broadcast(clean)
          if active then line = in.readLine() else line = null
      catch case _: Exception => ()
      finally
        val wasRegistered = registered
        registered = false
        unregisterClient(this)
        if wasRegistered && onlineName != null && onlineName.nonEmpty then
          val leftMsg = s"LEFT|$onlineName"
          serverEvents.put(leftMsg)
          broadcast(leftMsg)
        close()

    def send(msg: String): Unit =
      try { out.println(msg); out.flush() }
      catch case _: Exception => active = false

    def close(): Unit =
      active = false
      try sock.close() catch case _: IOException => ()

object GameServer:
  private val threadGroup = Thread.currentThread.getThreadGroup

final class GameClient(host: String, port: Int):
  private var sock: Socket = null
  private var out: PrintWriter = null
  private var in: BufferedReader = null
  private val msgQueue = new LinkedBlockingQueue[String]()
  @volatile private var connected = false
  private var readerThread: Thread = null
  @volatile private var lastError = ""

  def errorMessage: String = lastError

  def connect(playerName: String, localModHash: String): Boolean =
    lastError = ""
    try
      sock = Socket()
      sock.connect(InetSocketAddress(host, port), 3500)
      sock.setTcpNoDelay(true)
      sock.setKeepAlive(true)
      // Short timeout only for the login/world handshake. After that the reader blocks normally.
      sock.setSoTimeout(5000)
      out = PrintWriter(sock.getOutputStream, true)
      in = BufferedReader(InputStreamReader(sock.getInputStream))
      out.println(s"HELO|$playerName|${Option(localModHash).getOrElse(ModRuntime.VanillaHash)}")
      out.flush()

      val firstLine = in.readLine()
      if firstLine == null then
        lastError = "Server closed the connection before sending world data."
        disconnect()
        return false
      if firstLine.startsWith("ERR|") then
        val parts = firstLine.split("\\|", -1)
        lastError = if parts.length >= 3 then parts.drop(2).mkString(" ") else firstLine
        disconnect()
        return false
      if !firstLine.startsWith("WORLD|") then
        lastError = "Connected, but the server did not speak Blockbox protocol."
        disconnect()
        return false

      msgQueue.put(firstLine)
      connected = true
      sock.setSoTimeout(0)
      readerThread = Thread(() =>
        try
          var line = in.readLine()
          while line != null && connected do
            msgQueue.put(line)
            line = in.readLine()
        catch
          case _: SocketException => connected = false
          case _: IOException => connected = false
          case _: Exception => connected = false
      , "client-reader")
      readerThread.setDaemon(true)
      readerThread.start()
      true
    catch
      case e: SocketTimeoutException =>
        lastError = "Timed out. Check the IP, port 25565, firewall, ZeroTier/LAN route, or port forward."
        disconnect(); false
      case e: ConnectException =>
        lastError = "Connection refused. The host is reachable, but no Blockbox server is listening on that port."
        disconnect(); false
      case e: UnknownHostException =>
        lastError = "Unknown host. Check the server IP/name."
        disconnect(); false
      case e: NoRouteToHostException =>
        lastError = "No route to host. Check LAN/ZeroTier/VPN/firewall."
        disconnect(); false
      case e: IOException =>
        lastError = Option(e.getMessage).filter(_.nonEmpty).getOrElse("I/O error while connecting.")
        disconnect(); false
      case e: Exception =>
        lastError = Option(e.getMessage).filter(_.nonEmpty).getOrElse(e.getClass.getSimpleName)
        disconnect(); false

  def disconnect(): Unit =
    connected = false
    try if out != null then out.println("QUIT") catch case _: Exception => ()
    try if sock != null then sock.close() catch case _: Exception => ()
    msgQueue.clear()

  def send(cmd: String): Unit =
    if connected && out != null then
      try { out.println(cmd); out.flush() } catch case _: Exception => connected = false

  def pollMessage(): Option[String] = Option(msgQueue.poll())

  def isConnected: Boolean = connected

final class Blockbox:
  private val width = 1280
  private val height = 720
  private var framebufferWidth = width
  private var framebufferHeight = height
  private var window = 0L
  private var screen = Screen.MainMenu
  private var vsync = true
  private var fastMove = false
  private var soundEnabled = true
  private var fogDensity = 1.6f
  private var gameMode = GameMode.Survival
  private var lastTime = 0.0
  private var lastFrameTime = 0.0
  private var camera = Vec3(96f, 48f, 130f)
  private var velocity = Vec3(0f, 0f, 0f)
  private var onGround = false
  private var yaw = 0f
  private var pitch = 0f
  private var lastMouseX = width / 2.0
  private var lastMouseY = height / 2.0
  private var firstMouse = true
  private var leftWasDown = false
  private var rightWasDown = false
  private var menuLeftWasDown = false
  private var modGuiLeftWasDown = false
  private var modUiCursorMode = false
  private var modLastMouseX = 0f
  private var modLastMouseY = 0f
  private var modLeftDownNow = false
  private var modLeftClickedThisFrame = false
  private var breakingBlock: (Int, Int, Int) | Null = null
  private var breakingProgress = 0f
  private val placeableBlocks = Array(Block.Grass, Block.Dirt, Block.Stone, Block.Sand, Block.Cactus, Block.Wood, Block.Planks, Block.BirchPlanks, Block.PinePlanks, Block.AcaciaPlanks, Block.Leaves, Block.BirchWood, Block.BirchLeaves, Block.PineWood, Block.PineLeaves, Block.AcaciaWood, Block.AcaciaLeaves, Block.Brick, Block.Glass, Block.Snow, Block.Clay, Block.Coal, Block.Copper, Block.IronOre, Block.GoldOre, Block.Diamond, Block.RainbowBlock, Block.Furnace)
  private val inventory = Array.fill(Block.values.length)(0)
  private val hotbarBlocks: Array[Block] = Array.fill(10)(Block.Air)
  private val hotbarCounts: Array[Int] = Array.fill(10)(0)
  private val maxStackSize = 64
  private var selectedBlock = 0
  private var heldInventoryBlock: Block = Block.Air
  private var heldInventoryCount = 0
  private var heldFromHotbar = false
  private var catalogScroll = 0
  private var craftingScroll = 0
  private val craftGridBlocks: Array[Block] = Array.fill(9)(Block.Air)
  private var furnaceInput: Block = Block.Air
  private var furnaceFuel: Block = Block.Air
  private var furnaceProgress = 0f
  private var furnaceFuelRemaining = 0f
  private var furnaceOutput: Block = Block.Air
  private var furnaceOutputCount = 0
  private val smeltableInputs: Array[Block] = Array(Block.Sand, Block.Clay, Block.Stone, Block.Wood, Block.BirchWood, Block.PineWood, Block.AcaciaWood, Block.IronOre, Block.GoldOre)
  private val fuelBlocks: Array[Block] = Array(Block.Coal, Block.Wood, Block.BirchWood, Block.PineWood, Block.AcaciaWood, Block.Planks, Block.BirchPlanks, Block.PinePlanks, Block.AcaciaPlanks)
  private var debugMode = false
  private var wireframeMode = false
  private var showChunkBorders = false
  private var gameServer: GameServer = null
  private var gameClient: GameClient = null
  private var playerName = "Player"
  private val remotePlayers = scala.collection.mutable.HashMap.empty[String, RemotePlayer]
  // Multiplayer clients must not load local chunk files for the host seed. BLOC packets can
  // arrive before the matching chunk has streamed in, so keep them here and apply them when
  // that chunk is created. This prevents seed-correct clients from looking different just
  // because old local chunk saves or early packets were used/lost.
  private val pendingNetworkBlocks = scala.collection.mutable.HashMap.empty[(Int, Int, Int), Block]
  // Every BLOC packet seen during a multiplayer session is kept here, even after the
  // target chunk unloads. Without this, client-only worlds regenerated from seed and
  // lost remote edits when walking away and returning.
  private val networkBlockOverrides = scala.collection.mutable.HashMap.empty[(Int, Int, Int), Block]
  private val networkWaterLevelOverrides = scala.collection.mutable.HashMap.empty[(Int, Int, Int), Byte]
  private val worldExtraMagic = 0x42425831 // "BBX1": optional tail after old world.dat fields
  private val worldExtraVersion = 2
  private val knownPlayerNames = scala.collection.mutable.HashSet.empty[String]
  private val playerColors = scala.collection.mutable.HashMap.empty[String, Int]
  private val oppedPlayerNames = scala.collection.mutable.HashSet.empty[String]
  private var localColorId = 0
  private val playerColorPalette: Array[(Float, Float, Float)] = Array(
    (0.22f, 0.46f, 0.95f), // blue
    (0.95f, 0.28f, 0.22f), // red
    (0.20f, 0.78f, 0.35f), // green
    (0.95f, 0.76f, 0.20f), // gold
    (0.62f, 0.36f, 0.94f), // purple
    (0.95f, 0.44f, 0.82f), // pink
    (0.12f, 0.80f, 0.80f), // cyan
    (1.00f, 0.50f, 0.18f), // orange
    (0.80f, 0.82f, 0.88f), // silver
    (0.52f, 0.88f, 0.18f)  // lime
  )
  private var hostNameFocused = true
  private var joinNameFocused = true
  private var joinStatusMessage = "Enter host IP. You can use IP:port; default port is 25565."
  private var joinStatusError = false
  private var hostStatusMessage = "Hosts on 0.0.0.0:25565. Share your LAN/ZeroTier IP."
  private var hostStatusError = false
  private var serverHost = "localhost"
  private var serverPort = 25565
  private var multiplayerMode = false
  private var joinIpInput = ""
  private var lastPosSend = 0.0
  private var renderDistance = 6 // chunks
  private val minRenderDistance = 2
  private val maxRenderDistance = 18
  private def renderDistanceBlocks: Int = renderDistance * Terrain.chunkSize
  private var worldSeed = 0L
  private var createWorldMode = GameMode.Survival
  private var createWorldCheats = false
  private var worldCheatsEnabled = false
  private var terrainGen = TerrainGenerator(0L)
  private var chunks = scala.collection.mutable.AnyRefMap.empty[(Int, Int), Chunk]
  private var textureAtlas: TextureAtlas | Null = null
  private var playerHealth = 20f
  private var playerFood = 20f
  private val maxPlayerHealth = 20f
  private val maxPlayerFood = 20f
  private val healthRegenDelay = 7.0f
  private val healthRegenRate = 0.85f
  private var timeSinceLastDamage = healthRegenDelay
  private var fallPeakY = camera.y
  private var cactusDamageCooldown = 0f
  private var errorCallback: GLFWErrorCallback | Null = null
  private var worldName = "World"
  private var worldNameInput = "New World"
  private var createWorldNameFocused = true
  private var customSeedInput = ""
  private var enterCustomSeed = false
  private var loadWorldSelection = 0
  private var loadWorldScroll = 0
  private var hostUseSavedWorld = false
  private var hostWorldSelection = 0
  private var hostWorldScroll = 0
  private var saveDirectory: String = ""
  private var settingsReturnTo: Screen = Screen.MainMenu
  private var pauseEscReturnsToGame = true
  private var chatOpen = false
  private var chatInput = ""
  private var suppressNextChatChar = false
  private val chatMessages = ArrayBuffer.empty[(String, Float)]
  private val chatHistory = ArrayBuffer.empty[String]
  private var chatHistoryIndex = -1
  private var commandSuggestionIndex = 0
  private val modManager = ModManager(this)
  private var modsScreenScroll = 0
  private val sandFallQueue = Queue.empty[(Int, Int, Int)]
  private val maxSandUpdatesPerFrame = 6
  // Dynamic water is queue-driven. Natural generated water is dormant and costs
  // nothing per tick until a nearby block update wakes it, which is the important
  // trick for Minecraft-like water without beach/ocean microstutters.
  private val waterLevels = scala.collection.mutable.HashMap.empty[(Int, Int, Int), Byte]
  private val waterFlowQueue = Queue.empty[(Int, Int, Int)]
  private val waterFlowQueued = HashSet.empty[(Int, Int, Int)]
  private var waterFlowTimer = 0f
  private val waterFlowInterval = 0.05f
  private val maxWaterUpdatesPerFrame = 36
  private val maxQueuedWaterCells = 12000
  private var fov = 70f
  private var fullscreen = false
  private var windowedX = 0; private var windowedY = 0
  private var windowedW = 1280; private var windowedH = 720
  private val fallingSandParticles = ArrayBuffer.empty[(Float, Float, Float, Float)]
  private val sandParticleLifetime = 0.25f
  private var sandParticleTimer = 0f
  private var timeOverride: Option[Float] = None
  private var flyEnabled = false
  private var lastSpacePressTime = 0.0
  private val doubleTapInterval = 0.3
  private var wasSpaceDown = false
  private val bubbleParticles = ArrayBuffer.empty[(Float, Float, Float, Float)]
  private var sliderActive: String | Null = null
  private val dirtyChunks = scala.collection.mutable.Set.empty[(Int, Int)]
  private val terrainHeightCache = scala.collection.mutable.HashMap.empty[Long, Int]
  private var starPositions: Array[(Float, Float, Float, Float)] = null
  // Threaded chunk generation
  private val chunkBuildQueue = new java.util.concurrent.ConcurrentLinkedQueue[Chunk]()
  private val chunkUploadQueue = new java.util.concurrent.ConcurrentLinkedQueue[Chunk]()
  @volatile private var chunkGenRunning = false
  private var chunkGenPool: java.util.concurrent.ExecutorService = null

  // High-quality seed source. Using currentTimeMillis() alone made quick restarts and
  // repeated New World clicks look suspiciously identical, and Scala Int/hashCode seeds
  // collapsed custom text seeds down to only 32 bits. Keep this self-contained so new
  // worlds, hosted worlds, and the Random Seed button all use the same behavior.
  private val seedRng = new java.security.SecureRandom()

  private def mix64(input: Long): Long =
    var z = input
    z = (z ^ (z >>> 30)) * -4658895280553007687L
    z = (z ^ (z >>> 27)) * -7723592293110705685L
    z ^ (z >>> 31)

  private def freshWorldSeed(): Long =
    val bytes = new Array[Byte](8)
    seedRng.nextBytes(bytes)
    var randomPart = 0L
    bytes.foreach { b => randomPart = (randomPart << 8) ^ (b.toLong & 0xffL) }
    val uuid = java.util.UUID.randomUUID()
    val mixed = mix64(
      randomPart ^
      System.nanoTime() ^
      java.lang.Long.rotateLeft(System.currentTimeMillis(), 17) ^
      java.lang.Long.rotateLeft(uuid.getMostSignificantBits, 29) ^
      uuid.getLeastSignificantBits ^
      System.identityHashCode(this).toLong
    )
    if mixed == 0L then 0x6a09e667f3bcc909L else mixed

  private def seedFromText(text: String): Long =
    val trimmed = Option(text).getOrElse("").trim
    if trimmed.isEmpty then freshWorldSeed()
    else
      var h = -3750763034362895579L // 64-bit FNV offset basis as a signed Long
      trimmed.foreach { ch =>
        h ^= ch.toLong
        h *= 1099511628211L
      }
      val mixed = mix64(h ^ java.lang.Long.rotateLeft(trimmed.length.toLong, 32))
      if mixed == 0L then 0x510e527fade682d1L else mixed

  private def worldFolderNameForSeed(seed: Long): String =
    "World-" + java.lang.Long.toUnsignedString(seed, 36).toUpperCase

  private def applyWorldSeed(seed: Long, freshWorldName: Boolean): Unit =
    worldSeed = if seed == 0L then freshWorldSeed() else seed
    terrainGen = TerrainGenerator(worldSeed)
    terrainHeightCache.clear()
    if freshWorldName then worldName = worldFolderNameForSeed(worldSeed)

  private def chooseCreateSeed(): Long =
    if enterCustomSeed && customSeedInput.nonEmpty then seedFromText(customSeedInput) else freshWorldSeed()

  private def previewRandomSeed(): Unit =
    customSeedInput = ""
    applyWorldSeed(freshWorldSeed(), freshWorldName = false)

  private def sanitizeWorldName(raw: String): String =
    val cleaned = Option(raw).getOrElse("").trim
      .map(ch => if ch.isLetterOrDigit || ch == ' ' || ch == '_' || ch == '-' then ch else '_')
      .mkString
      .replaceAll("\\s+", " ")
      .take(40)
    if cleaned.nonEmpty then cleaned else "New World"

  private def uniqueWorldFolderName(raw: String): String =
    val base = sanitizeWorldName(raw)
    var candidate = base
    var n = 2
    while new java.io.File(worldsRootDir, candidate).exists() do
      candidate = s"$base ($n)"
      n += 1
    candidate

  def run(): Unit =
    try
      initWindow()
      modManager.loadAll()
      applyWorldSeed(freshWorldSeed(), freshWorldName = false)
      val spawn = findSpawn()
      camera = spawn
      fallPeakY = camera.y
      lastTime = glfwGetTime()
      startChunkGenThread()
      while !glfwWindowShouldClose(window) do loop()
    finally
      stopChunkGenThread()
      modManager.shutdown()
      saveWorld()
      chunks.values.foreach(_.dispose())
      chunks.clear()
      val atlas = textureAtlas
      if atlas != null then atlas.destroy()
      if window != NULL then glfwDestroyWindow(window)
      glfwTerminate()
      val cb = errorCallback
      if cb != null then cb.free()

  private def initWindow(): Unit =
    errorCallback = GLFWErrorCallback.createPrint(System.err)
    glfwSetErrorCallback(errorCallback)
    configureGlfwPlatform()
    if !glfwInit() then throw RuntimeException("GLFW initialization failed")
    glfwWindowHint(GLFW_CONTEXT_VERSION_MAJOR, 2)
    glfwWindowHint(GLFW_CONTEXT_VERSION_MINOR, 1)
    glfwWindowHint(GLFW_RESIZABLE, GLFW_TRUE)
    glfwWindowHint(GLFW_SAMPLES, 4)
    window = glfwCreateWindow(width, height, "Blockbox - Scala Voxel Sandbox", NULL, NULL)
    if window == NULL then throw RuntimeException("Window creation failed")
    glfwMakeContextCurrent(window)
    glfwSwapInterval(if vsync then 1 else 0)
    glfwShowWindow(window)
    GL.createCapabilities()
    glEnable(0x809D)
    textureAtlas = TextureAtlas()
    windowedW = width; windowedH = height
    queryWindowPos()
    updateFramebufferSize()
    glfwSetFramebufferSizeCallback(window, new GLFWFramebufferSizeCallback {
      override def invoke(window: Long, w: Int, h: Int): Unit =
        framebufferWidth = w.max(1)
        framebufferHeight = h.max(1)
    })
    glEnable(GL_DEPTH_TEST)
    glDisable(GL_CULL_FACE)
    glEnable(GL_BLEND)
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA)
    glCullFace(GL_BACK)
    glClearColor(0.52f, 0.72f, 0.95f, 1f)
    glfwSetKeyCallback(window, new GLFWKeyCallback {
      override def invoke(window: Long, key: Int, scancode: Int, action: Int, mods: Int): Unit =
        if action == GLFW_PRESS then onKey(key)
    })
    glfwSetCharCallback(window, new GLFWCharCallback {
      override def invoke(window: Long, codepoint: Int): Unit = onChar(codepoint)
    })
    glfwSetScrollCallback(window, new GLFWScrollCallback {
      override def invoke(window: Long, xoffset: Double, yoffset: Double): Unit = onScroll(xoffset, yoffset)
    })

  private def queryWindowPos(): Unit =
    val prevCb = glfwSetErrorCallback(null)
    try
      val wx = BufferUtils.createIntBuffer(1); val wy = BufferUtils.createIntBuffer(1)
      glfwGetWindowPos(window, wx, wy)
      windowedX = wx.get(0); windowedY = wy.get(0)
    finally glfwSetErrorCallback(prevCb)

  private def configureGlfwPlatform(): Unit =
    sys.env.get("GLFW_PLATFORM").map(_.toLowerCase) match
      case Some("x11") => glfwInitHint(GLFW_PLATFORM, GLFW_PLATFORM_X11)
      case Some("wayland") =>
        glfwInitHint(GLFW_PLATFORM, GLFW_PLATFORM_WAYLAND)
      case Some("null") => glfwInitHint(GLFW_PLATFORM, GLFW_PLATFORM_NULL)
      case Some(other) => System.err.println(s"Blockbox: unknown GLFW_PLATFORM '$other', using GLFW default")
      case None => ()

  private def findSpawn(): Vec3 =
    val x = 0; val z = 0
    val h = terrainGen.heightAt(x, z)
    Vec3(x + 0.5f, (h + 3).max(Terrain.seaLevel + 3).toFloat, z + 0.5f)

  private def loop(): Unit =
    val now = glfwGetTime()
    lastFrameTime = (now - lastTime).min(0.1)
    val dt = lastFrameTime.toFloat.min(0.05f)
    lastTime = now
    if screen == Screen.Playing then
      updateGame(dt)
      modManager.fireWorldTick(dt)
    else
      // In multiplayer, menu/pause screens are client-side only.
      // Keep sockets, chat, player positions, and chunk uploads alive so pausing one window
      // does not freeze/desync everyone else. Singleplayer still fully pauses here.
      if shouldRunMultiplayerBackgroundTick then updateMultiplayerBackgroundTick(dt)
      handleMenuMouse()
    render()
    glfwSwapBuffers(window)
    glfwPollEvents()

  private def onChar(codepoint: Int): Unit =
    if chatOpen then
      if suppressNextChatChar then
        suppressNextChatChar = false
      else if codepoint >= 32 && codepoint <= 126 then
        chatInput += codepoint.toChar
        commandSuggestionIndex = 0
    else if screen == Screen.CreateWorld then
      if codepoint >= 32 && codepoint <= 126 then
        val ch = codepoint.toChar
        if createWorldNameFocused then worldNameInput = (worldNameInput + ch).take(40)
        else if enterCustomSeed then customSeedInput = (customSeedInput + ch).take(64)
    else if screen == Screen.JoinGame then
      if codepoint >= 32 && codepoint <= 126 then
        val ch = codepoint.toChar
        if joinNameFocused then appendPlayerNameChar(ch) else joinIpInput = (joinIpInput + ch).take(64)
    else if screen == Screen.HostGame then
      if codepoint >= 32 && codepoint <= 126 then appendPlayerNameChar(codepoint.toChar)

  private def onScroll(xoffset: Double, yoffset: Double): Unit =
    val delta = if yoffset > 0 then -1 else if yoffset < 0 then 1 else 0
    if delta == 0 then return
    screen match
      case Screen.Playing =>
        selectedBlock = Math.floorMod(selectedBlock + delta, hotbarBlocks.length)
      case Screen.Catalog =>
        catalogScroll = (catalogScroll + delta).max(0)
      case Screen.Mods =>
        modsScreenScroll = (modsScreenScroll + delta).max(0)
      case _ => ()

  private def onKey(key: Int): Unit =
    if key == GLFW_KEY_F11 then toggleFullscreen()
    else screen match
      case Screen.MainMenu =>
        if key == GLFW_KEY_ENTER then screen = Screen.CreateWorld
        else if key == GLFW_KEY_S then screen = Screen.Settings
        else if key == GLFW_KEY_M then screen = Screen.Mods
        else if key == GLFW_KEY_L then openLoadWorldMenu()
        else if key == GLFW_KEY_ESCAPE then glfwSetWindowShouldClose(window, true)
      case Screen.Mods =>
        if key == GLFW_KEY_ESCAPE || key == GLFW_KEY_ENTER then screen = Screen.MainMenu
        else if key == GLFW_KEY_UP then modsScreenScroll = (modsScreenScroll - 1).max(0)
        else if key == GLFW_KEY_DOWN then modsScreenScroll += 1
      case Screen.CreateWorld =>
        if key == GLFW_KEY_ENTER then startNewWorld()
        else if key == GLFW_KEY_R then
          previewRandomSeed()
        else if key == GLFW_KEY_ESCAPE then
          enterCustomSeed = false
          screen = Screen.MainMenu
        else if key == GLFW_KEY_TAB then
          createWorldNameFocused = !createWorldNameFocused
          if !createWorldNameFocused then enterCustomSeed = true
        else if key == GLFW_KEY_N then createWorldNameFocused = true
        else if key == GLFW_KEY_C then
          enterCustomSeed = true
          createWorldNameFocused = false
        else if key == GLFW_KEY_M then createWorldMode = if createWorldMode == GameMode.Survival then GameMode.Creative else GameMode.Survival
        else if key == GLFW_KEY_H then createWorldCheats = !createWorldCheats
        else if key == GLFW_KEY_BACKSPACE then
          if createWorldNameFocused && worldNameInput.nonEmpty then worldNameInput = worldNameInput.init
          else if !createWorldNameFocused && customSeedInput.nonEmpty then customSeedInput = customSeedInput.init
      case Screen.LoadWorld =>
        val saves = worldSaveDirs
        if key == GLFW_KEY_ESCAPE then screen = Screen.MainMenu
        else if key == GLFW_KEY_ENTER then loadSelectedWorld()
        else if key == GLFW_KEY_UP then loadWorldSelection = (loadWorldSelection - 1).max(0)
        else if key == GLFW_KEY_DOWN then loadWorldSelection = (loadWorldSelection + 1).min((saves.length - 1).max(0))
      case Screen.Settings =>
        if key == GLFW_KEY_ESCAPE then
          sliderActive = null
          screen = settingsReturnTo
          if settingsReturnTo == Screen.Playing then
            glfwSetInputMode(window, GLFW_CURSOR, GLFW_CURSOR_DISABLED)
            firstMouse = true
        else if key == GLFW_KEY_F then fastMove = !fastMove
        else if key == GLFW_KEY_V then
          vsync = !vsync
          glfwSwapInterval(if vsync then 1 else 0)
        else if key == GLFW_KEY_LEFT || key == GLFW_KEY_MINUS then changeRenderDistance(-1)
        else if key == GLFW_KEY_RIGHT || key == GLFW_KEY_EQUAL then changeRenderDistance(1)
        else if key == GLFW_KEY_M && canUseCheatAuthority && worldCheatsEnabled then toggleGameMode()
        else if key == GLFW_KEY_P then pauseEscReturnsToGame = !pauseEscReturnsToGame
      case Screen.Playing =>
        if chatOpen then
          if key == GLFW_KEY_ENTER then
            submitChat()
          else if key == GLFW_KEY_ESCAPE then
            chatOpen = false; chatInput = ""; chatHistoryIndex = -1
          else if key == GLFW_KEY_BACKSPACE && chatInput.nonEmpty then
            chatInput = chatInput.init
            commandSuggestionIndex = 0
          else if key == GLFW_KEY_TAB then
            applyCommandSuggestion()
          else if key == GLFW_KEY_UP then
            val suggestions = commandSuggestions(chatInput)
            if suggestions.nonEmpty then commandSuggestionIndex = (commandSuggestionIndex - 1 + suggestions.length) % suggestions.length
            else if chatHistory.nonEmpty then
              chatHistoryIndex = (chatHistoryIndex - 1).max(0)
              chatInput = chatHistory(chatHistory.length - 1 - chatHistoryIndex)
          else if key == GLFW_KEY_DOWN then
            val suggestions = commandSuggestions(chatInput)
            if suggestions.nonEmpty then commandSuggestionIndex = (commandSuggestionIndex + 1) % suggestions.length
            else if chatHistoryIndex > 0 then
              chatHistoryIndex -= 1
              chatInput = chatHistory(chatHistory.length - 1 - chatHistoryIndex)
            else
              chatHistoryIndex = -1; chatInput = ""
        else if key == GLFW_KEY_ESCAPE then leaveGame(Screen.Paused)
        else if key == GLFW_KEY_E then
          if gameMode == GameMode.Creative then openCatalog() else openInventory()
        else if key == GLFW_KEY_SLASH || key == GLFW_KEY_T then
          chatOpen = true
          chatInput = if key == GLFW_KEY_SLASH then "/" else ""
          chatHistoryIndex = -1
          suppressNextChatChar = true
        else if key >= GLFW_KEY_1 && key <= GLFW_KEY_9 then selectedBlock = key - GLFW_KEY_1
        else if key == GLFW_KEY_0 then selectedBlock = 9
        else if key == GLFW_KEY_M && canUseCheatAuthority && worldCheatsEnabled then toggleGameMode()
        else if key == GLFW_KEY_F3 then debugMode = !debugMode
        else if key == GLFW_KEY_F4 then
          wireframeMode = !wireframeMode
          if wireframeMode then glPolygonMode(GL_FRONT_AND_BACK, GL_LINE) else glPolygonMode(GL_FRONT_AND_BACK, GL_FILL)
        else if key == GLFW_KEY_F5 then showChunkBorders = !showChunkBorders
        else if key == GLFW_KEY_F8 then modSetCursorMode(!modUiCursorMode)
        else if key == GLFW_KEY_F2 then
          settingsReturnTo = Screen.Playing
          screen = Screen.Settings
          glfwSetInputMode(window, GLFW_CURSOR, GLFW_CURSOR_NORMAL)
      case Screen.JoinGame =>
        if key == GLFW_KEY_ENTER then
          val ip = if joinIpInput.trim.isEmpty then "127.0.0.1" else joinIpInput.trim
          joinStatusError = false
          joinStatusMessage = s"Connecting to $ip ..."
          joinGame(ip)
        else if key == GLFW_KEY_ESCAPE then screen = Screen.MainMenu
        else if key == GLFW_KEY_TAB then joinNameFocused = !joinNameFocused
        else if key == GLFW_KEY_BACKSPACE then
          if joinNameFocused && playerName.nonEmpty then playerName = playerName.init
          else if !joinNameFocused && joinIpInput.nonEmpty then joinIpInput = joinIpInput.init
      case Screen.HostGame =>
        val saves = worldSaveDirs
        if key == GLFW_KEY_ENTER then hostGame()
        else if key == GLFW_KEY_ESCAPE then screen = Screen.MainMenu
        else if key == GLFW_KEY_L then hostUseSavedWorld = !hostUseSavedWorld
        else if key == GLFW_KEY_UP then hostWorldSelection = (hostWorldSelection - 1).max(0)
        else if key == GLFW_KEY_DOWN then hostWorldSelection = (hostWorldSelection + 1).min((saves.length - 1).max(0))
        else if key == GLFW_KEY_BACKSPACE && playerName.nonEmpty then playerName = playerName.init
      case Screen.Paused =>
        if key == GLFW_KEY_ENTER then enterGame()
        else if key == GLFW_KEY_ESCAPE then
          if pauseEscReturnsToGame then enterGame() else leaveGame(Screen.MainMenu)
        else if key == GLFW_KEY_S then
          settingsReturnTo = Screen.Paused
          screen = Screen.Settings
      case Screen.Inventory | Screen.Catalog | Screen.FurnaceUI =>
        if key == GLFW_KEY_E || key == GLFW_KEY_ESCAPE then enterGame()

  private def handleMenuMouse(): Unit =
    val leftDown = glfwGetMouseButton(window, GLFW_MOUSE_BUTTON_LEFT) == GLFW_PRESS
    if leftDown && !menuLeftWasDown then
      val (mx, my) = mouseFramebufferPos()
      screen match
        case Screen.MainMenu => handleMainMenuClick(mx, my)
        case Screen.CreateWorld => handleCreateWorldClick(mx, my)
        case Screen.LoadWorld => handleLoadWorldClick(mx, my)
        case Screen.Mods => handleModsClick(mx, my)
        case Screen.Settings => handleSettingsClick(mx, my)
        case Screen.Paused => handlePauseClick(mx, my)
        case Screen.Inventory => handleInventoryClick(mx, my)
        case Screen.Catalog => handleCatalogClick(mx, my)
        case Screen.FurnaceUI => handleFurnaceClick(mx, my)
        case Screen.JoinGame => handleJoinClick(mx, my)
        case Screen.HostGame => handleHostClick(mx, my)
        case Screen.Playing => ()
    // Slider dragging: update every frame while holding on Settings
    if leftDown && screen == Screen.Settings && sliderActive != null then
      val (mx, my) = mouseFramebufferPos()
      updateSlider(mx)
    if !leftDown then sliderActive = null
    menuLeftWasDown = leftDown

  private def updateSlider(mx: Float): Unit =
    val s = uiScale
    val pW = 520f * s
    val pX = framebufferWidth / 2f - pW / 2f
    val settingX = pX + 40f * s
    val value = ((mx - settingX) / (440f * s)).max(0f).min(1f)
    sliderActive match
      case "rd" =>
        val next = (minRenderDistance.toFloat + value * (maxRenderDistance - minRenderDistance)).round
        if next != renderDistance then
          renderDistance = next.max(minRenderDistance).min(maxRenderDistance)
          // Do not nuke/reload every chunk while the slider is being dragged.
          // Rapid render-distance changes used to save/dispose/recreate the whole world
          // repeatedly, which could leave workers fighting stale chunks and make terrain
          // appear raised or scrambled. Let the normal streamer add/remove only deltas.
          syncChunks()
      case "fog" => fogDensity = 0.6f + value * 2.4f
      case "fov" => fov = 50f + value * 50f
      case _ => ()

  private def handleMainMenuClick(mx: Float, my: Float): Unit =
    val layout = mainMenuLayout()
    val bx = layout._1; val bw = layout._2; val bh = layout._3
    val ys = layout._4
    if inRect(mx, my, bx, ys(0), bw, bh) then screen = Screen.CreateWorld
    else if inRect(mx, my, bx, ys(1), bw, bh) then
      hostNameFocused = true
      hostStatusError = false
      hostStatusMessage = "Hosts on 0.0.0.0:25565. Share your LAN/ZeroTier IP."
      screen = Screen.HostGame
    else if inRect(mx, my, bx, ys(2), bw, bh) then
      joinIpInput = ""
      joinNameFocused = true
      joinStatusError = false
      joinStatusMessage = "Enter host IP. You can use IP:port; default port is 25565."
      screen = Screen.JoinGame
    else if inRect(mx, my, bx, ys(3), bw, bh) then openLoadWorldMenu()
    else if inRect(mx, my, bx, ys(4), bw, bh) then screen = Screen.Mods
    else if inRect(mx, my, bx, ys(5), bw, bh) then
      screen = Screen.Settings
      settingsReturnTo = Screen.MainMenu
    else if inRect(mx, my, bx, ys(6), bw, bh) then glfwSetWindowShouldClose(window, true)

  private def handleCreateWorldClick(mx: Float, my: Float): Unit =
    val w = framebufferWidth.toFloat
    val h = framebufferHeight.toFloat
    val s = uiScale
    val cx = w / 2f
    val margin = (18f * s).max(12f)
    val pW = (620f * s).min(w - margin * 2f).max(math.min(360f, w - margin * 2f))
    val pH = (560f * s).min(h - margin * 2f).max(math.min(420f, h - margin * 2f))
    val pX = cx - pW / 2f
    val pY = ((h - pH) / 2f).max(margin)
    val settingW = (pW - 80f * s).max(260f.min(pW - 32f * s))
    val settingH = (36f * s).min(38f).max(28f)
    val settingX = cx - settingW / 2f
    val nameY = pY + 104f * s
    val seedY = pY + 166f * s
    val modeY = pY + 236f * s
    val cheatsY = pY + 282f * s
    if inRect(mx, my, settingX, nameY, settingW, 34f * s) then
      createWorldNameFocused = true
    else if inRect(mx, my, settingX, seedY, settingW, 34f * s) then
      enterCustomSeed = true
      createWorldNameFocused = false
    else if inRect(mx, my, settingX, modeY, settingW, settingH) then
      createWorldMode = if createWorldMode == GameMode.Survival then GameMode.Creative else GameMode.Survival
    else if inRect(mx, my, settingX, cheatsY, settingW, settingH) then
      createWorldCheats = !createWorldCheats
    else
      val buttonW = (pW - 120f * s).max(240f.min(pW - 40f * s))
      val buttonH = (38f * s).min(42f).max(30f)
      val gap = (10f * s).max(6f).min(12f)
      val buttonX = cx - buttonW / 2f
      val buttonStartY = pY + pH - (buttonH * 4f + gap * 3f) - 18f * s
      if inRect(mx, my, buttonX, buttonStartY, buttonW, buttonH) then startNewWorld()
      else if inRect(mx, my, buttonX, buttonStartY + buttonH + gap, buttonW, buttonH) then
        previewRandomSeed()
      else if inRect(mx, my, buttonX, buttonStartY + (buttonH + gap) * 2f, buttonW, buttonH) then enterCustomSeed = !enterCustomSeed
      else if inRect(mx, my, buttonX, buttonStartY + (buttonH + gap) * 3f, buttonW, buttonH) then
        enterCustomSeed = false
        screen = Screen.MainMenu

  private def renderNameField(x: Float, y: Float, w: Float, h: Float, label: String, value: String, focused: Boolean): Unit =
    val s = uiScale
    renderTextShadow(x, y - 21f * s, label, 0.78f, 0.84f, 0.96f, (0.92f * s).max(0.82f))
    rect(x - 1f * s, y - 1f * s, w + 2f * s, h + 2f * s, if focused then 0.95f else 0.03f, if focused then 0.75f else 0.03f, if focused then 0.28f else 0.04f, if focused then 0.42f else 0.82f)
    rect(x, y, w, h, 0.055f, 0.070f, 0.105f, 0.96f)
    rect(x + 2f * s, y + 2f * s, w - 4f * s, 2f * s, 0.22f, 0.25f, 0.32f, 0.18f)
    val shown = value.trim
    val display = if shown.isEmpty then "..." else if shown.length > 34 then shown.take(16) + "..." + shown.takeRight(14) else shown
    centeredTextFit(x + w / 2f, y + h / 2f - 6f * s, display, 0.92f, 0.96f, 1f, (0.98f * s).max(0.86f), w - 18f * s)

  private def renderHostGame(): Unit =
    glDisable(GL_DEPTH_TEST); glDisable(GL_CULL_FACE); setupOrtho()
    val w = framebufferWidth.toFloat; val h = framebufferHeight.toFloat; val cx = w / 2f; val s = uiScale
    glBegin(GL_QUADS)
    glColor4f(0.045f, 0.055f, 0.115f, 1f); glVertex2f(0, 0); glVertex2f(w, 0)
    glColor4f(0.12f, 0.20f, 0.34f, 1f); glVertex2f(w, h); glVertex2f(0, h)
    glEnd()
    rect(0, h * 0.62f, w, h * 0.38f, 0.06f, 0.18f, 0.06f, 0.88f)
    val pW = (620f * s).min(w * 0.92f); val pH = (470f * s).min(h * 0.88f)
    val pX = cx - pW / 2f; val pY = h / 2f - pH / 2f
    drawPanel(pX, pY, pW, pH)
    centeredTextFit(cx, pY + 34f * s, "HOST LAN GAME", 1f, 0.90f, 0.55f, 1.95f * s, pW - 60f * s)
    rect(pX + 40f * s, pY + 64f * s, pW - 80f * s, 1f, 0.30f, 0.30f, 0.35f, 0.30f)
    val fieldW = pW - 96f * s; val fieldH = 34f * s; val fieldX = pX + 48f * s
    renderNameField(fieldX, pY + 102f * s, fieldW, fieldH, "Online name", playerName, hostNameFocused)
    val saves = worldSaveDirs
    val sourceY = pY + 166f * s
    val sourceLabel = if hostUseSavedWorld then "World source: saved world" else "World source: new world"
    val (mx, my) = mouseFramebufferPos()
    val sourceHover = inRect(mx, my, fieldX, sourceY, fieldW, 30f * s)
    rect(fieldX, sourceY, fieldW, 30f * s, if sourceHover then 0.16f else 0.09f, if sourceHover then 0.18f else 0.10f, if sourceHover then 0.23f else 0.15f, 0.82f)
    renderTextShadow(fieldX + 12f * s, sourceY + 7f * s, sourceLabel, 0.88f, 0.92f, 1f, 0.58f * s)
    centeredTextFit(fieldX + fieldW - 54f * s, sourceY + 7f * s, "L", 1f, 0.86f, 0.42f, 0.56f * s, 40f * s)
    if hostUseSavedWorld then
      val listY = pY + 208f * s
      val rowH = (34f * s).max(27f)
      val visible = 4
      if saves.isEmpty then centeredTextFit(cx, listY + 34f * s, "No saved worlds in worlds/", 0.70f, 0.78f, 0.92f, 0.62f * s, fieldW)
      else
        if hostWorldSelection < hostWorldScroll then hostWorldScroll = hostWorldSelection
        if hostWorldSelection >= hostWorldScroll + visible then hostWorldScroll = hostWorldSelection - visible + 1
        hostWorldScroll = hostWorldScroll.max(0).min((saves.length - visible).max(0))
        saves.zipWithIndex.drop(hostWorldScroll).take(visible).foreach { case (dir, idx) =>
          val y = listY + (idx - hostWorldScroll) * rowH
          val selected = idx == hostWorldSelection
          rect(fieldX, y, fieldW, rowH - 4f * s, if selected then 0.22f else 0.075f, if selected then 0.19f else 0.085f, if selected then 0.11f else 0.12f, 0.84f)
          if selected then rect(fieldX, y, 4f * s, rowH - 4f * s, 1f, 0.82f, 0.30f, 0.80f)
          renderTextShadow(fieldX + 12f * s, y + 7f * s, dir.getName, 0.90f, 0.94f, 1f, 0.55f * s)
        }
    val hsr = if hostStatusError then 1.0f else 0.52f
    val hsg = if hostStatusError then 0.36f else 0.70f
    val hsb = if hostStatusError then 0.30f else 0.82f
    centeredTextFit(cx, pY + pH - 118f * s, hostStatusMessage, hsr, hsg, hsb, 0.54f * s, pW - 72f * s)
    val bw = (230f * s).min(pW - 90f * s); val bh = (38f * s).min(42f).max(30f)
    drawButton(cx - bw / 2f, pY + pH - 86f * s, bw, bh, if hostUseSavedWorld then "Host Selected World" else "Start Hosted World", accent = true)
    drawButton(cx - bw / 2f, pY + pH - 42f * s, bw, bh, "Back")

  private def handleHostClick(mx: Float, my: Float): Unit =
    val w = framebufferWidth.toFloat; val h = framebufferHeight.toFloat; val cx = w / 2f; val s = uiScale
    val pW = (620f * s).min(w * 0.92f); val pH = (470f * s).min(h * 0.88f)
    val pX = cx - pW / 2f; val pY = h / 2f - pH / 2f
    val fieldW = pW - 96f * s; val fieldH = 34f * s; val fieldX = pX + 48f * s
    val bw = (230f * s).min(pW - 90f * s); val bh = (38f * s).min(42f).max(30f)
    if inRect(mx, my, fieldX, pY + 102f * s, fieldW, fieldH) then hostNameFocused = true
    else if inRect(mx, my, fieldX, pY + 166f * s, fieldW, 30f * s) then hostUseSavedWorld = !hostUseSavedWorld
    else if hostUseSavedWorld then
      val saves = worldSaveDirs
      val listY = pY + 208f * s
      val rowH = (34f * s).max(27f)
      saves.zipWithIndex.drop(hostWorldScroll).take(4).foreach { case (_, idx) =>
        val y = listY + (idx - hostWorldScroll) * rowH
        if inRect(mx, my, fieldX, y, fieldW, rowH - 4f * s) then hostWorldSelection = idx
      }
      if inRect(mx, my, cx - bw / 2f, pY + pH - 86f * s, bw, bh) then hostGame()
      else if inRect(mx, my, cx - bw / 2f, pY + pH - 42f * s, bw, bh) then screen = Screen.MainMenu
    else if inRect(mx, my, cx - bw / 2f, pY + pH - 86f * s, bw, bh) then hostGame()
    else if inRect(mx, my, cx - bw / 2f, pY + pH - 42f * s, bw, bh) then screen = Screen.MainMenu

  private def renderJoinGame(): Unit =
    glDisable(GL_DEPTH_TEST); glDisable(GL_CULL_FACE); setupOrtho()
    val w = framebufferWidth.toFloat; val h = framebufferHeight.toFloat; val cx = w / 2f; val s = uiScale
    glBegin(GL_QUADS)
    glColor4f(0.045f, 0.055f, 0.115f, 1f); glVertex2f(0, 0); glVertex2f(w, 0)
    glColor4f(0.12f, 0.20f, 0.34f, 1f); glVertex2f(w, h); glVertex2f(0, h)
    glEnd()
    rect(0, h * 0.62f, w, h * 0.38f, 0.06f, 0.18f, 0.06f, 0.88f)
    val pW = (540f * s).min(w * 0.92f); val pH = (350f * s).min(h * 0.86f)
    val pX = cx - pW / 2f; val pY = h / 2f - pH / 2f
    drawPanel(pX, pY, pW, pH)
    centeredTextFit(cx, pY + 34f * s, "JOIN LAN GAME", 1f, 0.90f, 0.55f, 1.95f * s, pW - 60f * s)
    rect(pX + 40f * s, pY + 64f * s, pW - 80f * s, 1f, 0.30f, 0.30f, 0.35f, 0.30f)
    val fieldW = pW - 96f * s; val fieldH = 36f * s; val fieldX = pX + 48f * s
    renderNameField(fieldX, pY + 112f * s, fieldW, fieldH, "Online name", playerName, joinNameFocused)
    val ipDisplay = if joinIpInput.trim.isEmpty then "127.0.0.1" else joinIpInput.trim
    renderNameField(fieldX, pY + 184f * s, fieldW, fieldH, "Server IP address", ipDisplay, !joinNameFocused)
    val jsr = if joinStatusError then 1.0f else 0.52f
    val jsg = if joinStatusError then 0.34f else 0.70f
    val jsb = if joinStatusError then 0.28f else 0.82f
    centeredTextFit(cx, pY + 242f * s, joinStatusMessage, jsr, jsg, jsb, 0.56f * s, pW - 72f * s)
    val bw = (210f * s).min(pW - 90f * s); val bh = (40f * s).min(42f).max(30f)
    drawButton(cx - bw / 2f, pY + pH - 92f * s, bw, bh, "Connect", accent = true)
    drawButton(cx - bw / 2f, pY + pH - 44f * s, bw, bh, "Back")

  private def handleJoinClick(mx: Float, my: Float): Unit =
    val w = framebufferWidth.toFloat; val h = framebufferHeight.toFloat; val cx = w / 2f; val s = uiScale
    val pW = (540f * s).min(w * 0.92f); val pH = (350f * s).min(h * 0.86f)
    val pX = cx - pW / 2f; val pY = h / 2f - pH / 2f
    val fieldW = pW - 96f * s; val fieldH = 36f * s; val fieldX = pX + 48f * s
    val bw = (210f * s).min(pW - 90f * s); val bh = (40f * s).min(42f).max(30f)
    if inRect(mx, my, fieldX, pY + 112f * s, fieldW, fieldH) then joinNameFocused = true
    else if inRect(mx, my, fieldX, pY + 184f * s, fieldW, fieldH) then joinNameFocused = false
    else if inRect(mx, my, cx - bw / 2f, pY + pH - 92f * s, bw, bh) then
      val ip = if joinIpInput.trim.isEmpty then "127.0.0.1" else joinIpInput.trim
      joinStatusError = false
      joinStatusMessage = s"Connecting to $ip ..."
      joinGame(ip)
    else if inRect(mx, my, cx - bw / 2f, pY + pH - 44f * s, bw, bh) then screen = Screen.MainMenu

  private def handleSettingsClick(mx: Float, my: Float): Unit =
    val h = framebufferHeight.toFloat; val s = uiScale
    val pH = 590f * s; val pY = (h / 2f - pH / 2f).max(12f * s)
    val cx = framebufferWidth / 2f; val settingX = cx - 220f * s
    if inRect(mx, my, settingX, pY + 118 * uiScale, 440 * uiScale, 30 * uiScale) then
      updateSlider(mx); sliderActive = "rd"
    else if inRect(mx, my, settingX, pY + 185 * uiScale, 440 * uiScale, 30 * uiScale) then
      updateSlider(mx); sliderActive = "fog"
    else if inRect(mx, my, settingX, pY + 253 * uiScale, 440 * uiScale, 30 * uiScale) then
      updateSlider(mx); sliderActive = "fov"
    else
      var optY = pY + 285 * s
      if inRect(mx, my, settingX, optY, 440 * s, 28 * s) then fastMove = !fastMove
      else
        optY += 40 * s
        if inRect(mx, my, settingX, optY, 440 * s, 28 * s) then
          vsync = !vsync
          glfwSwapInterval(if vsync then 1 else 0)
        else
          optY += 40 * s
          val canHostMode = settingsReturnTo == Screen.Playing && gameServer != null && worldCheatsEnabled
          if canHostMode && inRect(mx, my, settingX, optY, 440 * s, 28 * s) then toggleGameMode()
          else
            if canHostMode then optY += 40 * s
            if inRect(mx, my, settingX, optY, 440 * s, 28 * s) then soundEnabled = !soundEnabled
            else
              optY += 40 * s
              if inRect(mx, my, settingX, optY, 440 * s, 28 * s) then toggleFullscreen()
              else
                optY += 40 * s
                if inRect(mx, my, settingX, optY, 440 * s, 28 * s) then pauseEscReturnsToGame = !pauseEscReturnsToGame
                else
                  val buttonW = 300f * s; val buttonX = cx - buttonW / 2f
                  if inRect(mx, my, buttonX, pY + 535 * s, buttonW, 44f * s) then
                    sliderActive = null
                    screen = settingsReturnTo
                    if settingsReturnTo == Screen.Playing then
                      glfwSetInputMode(window, GLFW_CURSOR, GLFW_CURSOR_DISABLED)
                      firstMouse = true

  private def handlePauseClick(mx: Float, my: Float): Unit =
    val cx = framebufferWidth / 2f; val h = framebufferHeight.toFloat; val s = uiScale
    val pw = (420f * s).min(framebufferWidth * 0.80f); val ph = (if multiplayerMode then 300f * s else 275f * s).min(h * 0.82f); val py = h / 2f - ph / 2f
    val bw = (320f * s).min(pw - 72f * s); val bh = (40f * s).min(ph / 6f).max(30f); val bx = cx - bw / 2f
    val gap = (14f * s).min(18f); val by0 = py + 92f * s
    if inRect(mx, my, bx, by0, bw, bh) then enterGame()
    else if inRect(mx, my, bx, by0 + bh + gap, bw, bh) then
      settingsReturnTo = Screen.Paused
      screen = Screen.Settings
    else if inRect(mx, my, bx, by0 + (bh + gap) * 2f, bw, bh) then leaveGame(Screen.MainMenu)

  private def handleInventoryClick(mx: Float, my: Float): Unit =
    val cx = framebufferWidth / 2f
    val cy = framebufferHeight / 2f
    val s = uiScale
    val pw = (760f * s).min(framebufferWidth * 0.94f)
    val ph = (500f * s).min(framebufferHeight * 0.88f)
    val px = cx - pw / 2f
    val py = cy - ph / 2f

    val bottomH = (34f * s).min(ph * 0.08f).max(28f)
    if inRect(mx, my, px + 22f * s, py + ph - 46f * s, 130f * s, bottomH) then
      releaseHeldItem()
      openFurnace()
      return
    else if inRect(mx, my, px + pw - 104f * s, py + ph - 46f * s, 82f * s, bottomH) then
      releaseHeldItem()
      enterGame()
      return

    val hotbarY = py + ph - 104f * s
    val hotbarPanelW = pw - 56f * s
    val hotbarX = px + 28f * s
    val hotGap = (5f * s).max(4f)
    val hotSlot = ((hotbarPanelW - hotGap * (hotbarBlocks.length - 1)) / hotbarBlocks.length).min(44f * s).max(28f)
    val hotBarW = hotSlot * hotbarBlocks.length + hotGap * (hotbarBlocks.length - 1)
    val hotStartX = hotbarX + (hotbarPanelW - hotBarW) / 2f
    for i <- 0 until hotbarBlocks.length do
      val sx = hotStartX + i * (hotSlot + hotGap)
      if inRect(mx, my, sx, hotbarY, hotSlot, hotSlot) then
        if heldInventoryBlock != Block.Air then
          placeHeldItemInHotbar(i)
        else
          selectedBlock = i
          takeHotbarSlot(i)
        return

    val gridW = pw * 0.56f
    val cols = 6
    val gap = (6f * s).max(4f)
    val slot = ((gridW - gap * (cols - 1)) / cols).min(44f * s).max(30f)
    val invX = px + 28f * s
    val invY = py + 76f * s
    val items = inventoryItems
    for i <- 0 until inventoryGridSlots do
      val col = i % cols
      val row = i / cols
      val sx = invX + col * (slot + gap)
      val sy = invY + row * (slot + gap)
      if inRect(mx, my, sx, sy, slot, slot) then
        if heldInventoryBlock != Block.Air then
          if gameMode == GameMode.Survival && heldFromHotbar then
            addBackpackItem(heldInventoryBlock, heldInventoryCount.max(1))
          clearHeldItem()
          return
        else if i < items.length then
          val block = items(i)
          setHeldItem(block, if gameMode == GameMode.Creative then maxStackSize else inventory(block.ordinal).min(maxStackSize))
        else
          clearHeldItem()
        return

    val craftPanelW = (pw - gridW - 78f * s).max(210f * s)
    val craftX = px + pw - 28f * s - craftPanelW
    val craftY = py + 76f * s
    val cGap = 6f * s
    val cSlot = ((craftPanelW - 56f * s) / 4f).min(42f * s).max(28f)
    val gridStartX = craftX + 12f * s
    val gridStartY = craftY + 26f * s
    var ci = 0
    while ci < 9 do
      val col = ci % 3
      val row0 = ci / 3
      val sx = gridStartX + col * (cSlot + cGap)
      val sy = gridStartY + row0 * (cSlot + cGap)
      if inRect(mx, my, sx, sy, cSlot, cSlot) then
        if heldInventoryBlock != Block.Air && validHotbarBlock(heldInventoryBlock) then
          craftGridBlocks(ci) = heldInventoryBlock
          if gameMode == GameMode.Survival && heldFromHotbar then
            addBackpackItem(heldInventoryBlock, heldInventoryCount.max(1))
            clearHeldItem()
        else craftGridBlocks(ci) = Block.Air
        return
      ci += 1
    val outSize = cSlot * 1.16f
    val outX = craftX + craftPanelW - outSize - 16f * s
    val outY = gridStartY + cSlot + cGap
    if inRect(mx, my, outX, outY, outSize, outSize) then
      tryCraftGrid()
      return

  private def handleCatalogClick(mx: Float, my: Float): Unit =
    val cx = framebufferWidth / 2f
    val cy = framebufferHeight / 2f
    val s = uiScale
    val panelW = (700f * s).min(framebufferWidth * 0.94f)
    val panelH = (500f * s).min(framebufferHeight * 0.88f)
    val panelX = cx - panelW / 2f
    val panelY = cy - panelH / 2f
    val closeW = 72f * s
    val closeH = 28f * s
    if inRect(mx, my, panelX + panelW - 88f * s, panelY + 12f * s, closeW, closeH) then
      catalogScroll = 0
      releaseHeldItem()
      enterGame()
      return

    val hotbarY = panelY + panelH - 76f * s
    val hotbarPanelW = panelW - 40f * s
    val hotbarX = panelX + 20f * s
    val hotGap = (5f * s).max(4f)
    val hotSlot = ((hotbarPanelW - hotGap * (hotbarBlocks.length - 1)) / hotbarBlocks.length).min(44f * s).max(28f)
    val hotBarW = hotSlot * hotbarBlocks.length + hotGap * (hotbarBlocks.length - 1)
    val hotStartX = hotbarX + (hotbarPanelW - hotBarW) / 2f
    for i <- 0 until hotbarBlocks.length do
      val sx = hotStartX + i * (hotSlot + hotGap)
      if inRect(mx, my, sx, hotbarY, hotSlot, hotSlot) then
        if heldInventoryBlock != Block.Air then
          placeHeldItemInHotbar(i)
        else selectedBlock = i
        return

    val slotSize = (48f * s).max(34f).min(54f)
    val gridX = panelX + 20f * s
    val gridY = panelY + 52f * s
    val cols = ((panelW - 54f * s) / slotSize).toInt.max(4).min(12)
    val items = catalogItems
    val visibleRows = ((hotbarY - gridY - 26f * s) / slotSize).toInt.max(1)
    val scrollMax = ((items.length - 1) / cols - visibleRows + 2).max(0)
    if catalogScroll > scrollMax then catalogScroll = scrollMax
    for i <- items.indices.drop(catalogScroll * cols).take(cols * visibleRows) do
      val idx = i - catalogScroll * cols
      val col = idx % cols
      val row = idx / cols
      val sx = gridX + col * slotSize
      val sy = gridY + row * slotSize
      if inRect(mx, my, sx, sy, slotSize, slotSize) then
        setHeldItem(items(i), maxStackSize)
        return
    val scrollUp = inRect(mx, my, panelX + panelW - 18f * s, panelY + 50f * s, 14f * s, 20f * s)
    val scrollDown = inRect(mx, my, panelX + panelW - 18f * s, hotbarY - 26f * s, 14f * s, 20f * s)
    if scrollUp then catalogScroll = (catalogScroll - 1).max(0)
    if scrollDown then catalogScroll = (catalogScroll + 1).min(scrollMax)
    if catalogScroll < 0 then catalogScroll = 0

  private def inRect(px: Float, py: Float, x: Float, y: Float, w: Float, h: Float): Boolean =
    px >= x && px <= x + w && py >= y && py <= y + h

  private def mouseFramebufferPos(): (Float, Float) =
    val mx = BufferUtils.createDoubleBuffer(1); val my = BufferUtils.createDoubleBuffer(1)
    val ww = BufferUtils.createIntBuffer(1); val wh = BufferUtils.createIntBuffer(1)
    glfwGetCursorPos(window, mx, my)
    glfwGetWindowSize(window, ww, wh)
    val sx = framebufferWidth.toFloat / ww.get(0).max(1)
    val sy = framebufferHeight.toFloat / wh.get(0).max(1)
    ((mx.get(0).toFloat * sx), (my.get(0).toFloat * sy))

  private def startNewWorld(): Unit =
    applyWorldSeed(chooseCreateSeed(), freshWorldName = false)
    worldName = uniqueWorldFolderName(worldNameInput)
    clearLoadedChunks(saveFirst = false)
    camera = findSpawn()
    fallPeakY = camera.y
    velocity = Vec3(0f, 0f, 0f)
    onGround = false
    yaw = 0f
    pitch = 0f
    gameMode = createWorldMode
    worldCheatsEnabled = createWorldCheats
    resetInventory()
    enterCustomSeed = false
    startGame()
    modManager.fireWorldLoaded()

  private def appendPlayerNameChar(ch: Char): Unit =
    if ch.isLetterOrDigit || ch == '_' || ch == '-' then
      playerName = (playerName + ch).take(16)

  private def cleanPlayerName(): String =
    val cleaned = playerName.trim.filter(ch => ch.isLetterOrDigit || ch == '_' || ch == '-').take(16)
    if cleaned.nonEmpty then cleaned else s"Player${((System.currentTimeMillis() / 1000) % 999).toInt}"

  private def networkEscape(value: String): String =
    value.replace("%", "%25").replace("|", "%7C").replace("\n", " ").replace("\r", " ")

  private def networkUnescape(value: String): String =
    value.replace("%7C", "|").replace("%25", "%")

  private def networkSafeName(value: String): String =
    val cleaned = networkUnescape(value).trim.filter(ch => ch.isLetterOrDigit || ch == '_' || ch == '-').take(16)
    if cleaned.nonEmpty then cleaned else "Player"

  private def normalizeColorId(id: Int): Int = Math.floorMod(id, playerColorPalette.length)

  private def fallbackColorForName(name: String): Int =
    normalizeColorId(Math.floorMod(networkSafeName(name).hashCode, playerColorPalette.length))

  private def colorForId(id: Int): (Float, Float, Float) = playerColorPalette(normalizeColorId(id))

  private def rememberPlayerColor(name: String, colorId: Int): Unit =
    val safe = networkSafeName(name)
    if safe.nonEmpty then playerColors(safe) = normalizeColorId(colorId)

  private def colorForPlayer(name: String): Int =
    val safe = networkSafeName(name)
    playerColors.getOrElseUpdate(safe, fallbackColorForName(safe))

  private def isOppedName(name: String): Boolean =
    val safe = networkSafeName(name)
    oppedPlayerNames.exists(_.equalsIgnoreCase(safe))

  private def setOppedName(name: String, value: Boolean, announce: Boolean = true): Unit =
    val safe = networkSafeName(name)
    if safe.nonEmpty then
      if value then oppedPlayerNames += safe else oppedPlayerNames.find(_.equalsIgnoreCase(safe)).foreach(n => oppedPlayerNames -= n)
      if announce then addChatMessage(if value then s"$safe is now an operator" else s"$safe is no longer an operator")

  private def broadcastOpState(name: String, value: Boolean): Unit =
    val safe = networkSafeName(name)
    if gameServer != null && safe.nonEmpty then gameServer.broadcast("OP|" + networkEscape(safe) + "|" + (if value then "1" else "0"))

  private def parsePlayerToken(token: String): Option[(String, Int)] =
    val raw = Option(token).getOrElse("").trim
    if raw.isEmpty then None
    else
      val idx = raw.lastIndexOf(':')
      if idx > 0 && idx < raw.length - 1 then
        val name = networkSafeName(raw.substring(0, idx))
        val color = try normalizeColorId(raw.substring(idx + 1).toInt) catch case _: Exception => fallbackColorForName(name)
        Some((name, color))
      else
        val name = networkSafeName(raw)
        Some((name, fallbackColorForName(name)))

  private def chooseRandomLocalColor(): Int =
    normalizeColorId(scala.util.Random.nextInt(playerColorPalette.length))

  private def parseServerAddress(rawInput: String): Either[String, (String, Int)] =
    val raw = Option(rawInput).getOrElse("").trim
    val value = if raw.isEmpty then "127.0.0.1" else raw
    if value.startsWith("[") && value.contains("]:") then
      val close = value.indexOf(']')
      val host = value.substring(1, close)
      val portText = value.substring(close + 2)
      parsePort(portText).map(port => (host, port))
    else
      val colon = value.lastIndexOf(':')
      val singleColon = colon > 0 && value.indexOf(':') == colon
      if singleColon && colon < value.length - 1 && value.substring(colon + 1).forall(_.isDigit) then
        val host = value.substring(0, colon).trim
        val portText = value.substring(colon + 1)
        if host.isEmpty then Left("Missing host before port.") else parsePort(portText).map(port => (host, port))
      else Right((value, serverPort))

  private def parsePort(text: String): Either[String, Int] =
    try
      val port = text.toInt
      if port >= 1 && port <= 65535 then Right(port) else Left("Port must be 1-65535.")
    catch case _: NumberFormatException => Left("Invalid port number.")

  private def prepareWorldForHost(): Unit =
    val saves = worldSaveDirs
    if hostUseSavedWorld && saves.nonEmpty then
      val chosen = saves(hostWorldSelection.max(0).min(saves.length - 1))
      if !loadWorldDataFromDir(chosen) then
        addChatMessage(s"Could not host saved world: ${chosen.getName}")
        hostUseSavedWorld = false
        prepareWorldForHost()
        return
    else
      applyWorldSeed(chooseCreateSeed(), freshWorldName = false)
      worldName = uniqueWorldFolderName(if worldNameInput.trim.nonEmpty then worldNameInput else "Hosted World")
      clearLoadedChunks(saveFirst = false)
      camera = findSpawn()
      fallPeakY = camera.y
      yaw = 0f
      pitch = 0f
      gameMode = createWorldMode
      worldCheatsEnabled = createWorldCheats
      resetInventory()
    velocity = Vec3(0f, 0f, 0f)
    onGround = false
    enterCustomSeed = false

  private def verifyLocalServer(port: Int): Either[String, Unit] =
    var sock: Socket = null
    try
      sock = Socket()
      sock.connect(InetSocketAddress("127.0.0.1", port), 1500)
      sock.setSoTimeout(1500)
      val out = PrintWriter(BufferedWriter(OutputStreamWriter(sock.getOutputStream)), true)
      val in = BufferedReader(InputStreamReader(sock.getInputStream))
      out.println("PING|probe")
      out.flush()
      val line = in.readLine()
      if line != null && line.startsWith("PONG|BLOCKBOX") then Right(())
      else Left("Server socket opened, but Blockbox handshake probe failed.")
    catch
      case e: Exception =>
        val msg = Option(e.getMessage).filter(_.nonEmpty).getOrElse(e.getClass.getSimpleName)
        Left("Server failed local port self-test on 127.0.0.1:" + port + " — " + msg)
    finally
      try if sock != null then sock.close() catch case _: Exception => ()

  private def hostGame(): Unit =
    playerName = cleanPlayerName()
    stopNetworking()
    try
      // Prepare the world first, but do not enter play mode until the socket is confirmed reachable.
      // This prevents the host from thinking it is online when nothing is actually listening.
      prepareWorldForHost()
      localColorId = chooseRandomLocalColor()
      playerColors.clear()
      oppedPlayerNames.clear(); oppedPlayerNames += playerName
      rememberPlayerColor(playerName, localColorId)
      val server = GameServer(
        serverPort,
        worldSeed,
        camera,
        playerName,
        localColorId,
        (x, y, z, blockId) =>
          val block = Block.fromId(blockId)
          if block == Block.Air || canPlaceBlockAt(x, y, z) then
            networkBlockOverrides((x, y, z)) = block
            setActiveBlock(x, y, z, block)
            dirtyChunkAt(x, z),
        () => snapshotWorldEditsForNetwork(),
        () => oppedPlayerNames.toSeq,
        () => camera,
        modManager.serverModpackHash,
        modManager.serverModpackList
      )
      gameServer = server
      verifyLocalServer(server.localPort) match
        case Right(_) =>
          gameClient = null
          multiplayerMode = true
          knownPlayerNames.clear(); knownPlayerNames += playerName
          playerColors.clear()
          oppedPlayerNames.clear(); oppedPlayerNames += playerName
          rememberPlayerColor(playerName, localColorId)
          hostStatusError = false
          hostStatusMessage = s"Hosting as $playerName. Server modpack: ${modManager.serverModpackHash}. Join locally with 127.0.0.1:${server.localPort}, or share your LAN/ZeroTier IP."
          addChatMessage(s"Hosting world as $playerName on port ${server.localPort}")
          startGame()
          modManager.fireWorldLoaded()
        case Left(error) =>
          stopNetworking()
          hostStatusError = true
          hostStatusMessage = error.toString
          screen = Screen.HostGame
          glfwSetInputMode(window, GLFW_CURSOR, GLFW_CURSOR_NORMAL)
    catch
      case e: java.net.BindException =>
        stopNetworking()
        hostStatusError = true
        hostStatusMessage = s"Port $serverPort is already in use. Close the old server or use another port."
        screen = Screen.HostGame
        glfwSetInputMode(window, GLFW_CURSOR, GLFW_CURSOR_NORMAL)
      case e: Exception =>
        stopNetworking()
        hostStatusError = true
        val msg = Option(e.getMessage).filter(_.nonEmpty).getOrElse(e.getClass.getSimpleName)
        hostStatusMessage = "Could not host: " + msg
        screen = Screen.HostGame
        glfwSetInputMode(window, GLFW_CURSOR, GLFW_CURSOR_NORMAL)

  private def joinGame(ip: String): Unit =
    playerName = cleanPlayerName()
    stopNetworking()
    parseServerAddress(ip) match
      case Left(error) =>
        joinStatusError = true
        joinStatusMessage = error.toString
        gameClient = null
        screen = Screen.JoinGame
        glfwSetInputMode(window, GLFW_CURSOR, GLFW_CURSOR_NORMAL)
      case Right((host, port)) =>
        joinStatusError = false
        joinStatusMessage = s"Connecting to $host:$port ..."
        val client = GameClient(host, port)
        gameClient = client
        if client.connect(playerName, modManager.clientJoinHash) then
          multiplayerMode = true
          knownPlayerNames.clear(); knownPlayerNames += playerName
          playerColors.clear()
          oppedPlayerNames.clear()
          rememberPlayerColor(playerName, localColorId)
          joinStatusError = false
          joinStatusMessage = s"Connected to $host:$port as $playerName."
          worldSeed = 0L
          terrainGen = TerrainGenerator(worldSeed)
          terrainHeightCache.clear()
          clearLoadedChunks(saveFirst = false)
          camera = findSpawn()
          velocity = Vec3(0f, 0f, 0f); onGround = false; yaw = 0f; pitch = 0f
          gameMode = GameMode.Survival
          resetInventory()
          // Consume the authoritative WORLD handshake before rendering/physics starts.
          // Otherwise the client can briefly build chunks around a temporary seed/spawn,
          // which looks like a dark empty world and can desync collision.
          processNetworkMessages()
          modManager.fireWorldLoaded()
          syncChunks()
          startGame()
        else
          val err = client.errorMessage
          client.disconnect()
          gameClient = null
          multiplayerMode = false
          joinStatusError = true
          joinStatusMessage = if err.nonEmpty then err else s"Could not connect to $host:$port."
          screen = Screen.JoinGame
          glfwSetInputMode(window, GLFW_CURSOR, GLFW_CURSOR_NORMAL)

  private def stopNetworking(): Unit =
    if gameClient != null then gameClient.disconnect(); gameClient = null
    if gameServer != null then gameServer.stop(); gameServer = null
    remotePlayers.clear()
    knownPlayerNames.clear()
    playerColors.clear()
    oppedPlayerNames.clear()
    pendingNetworkBlocks.clear()
    networkBlockOverrides.clear()
    networkWaterLevelOverrides.clear()
    multiplayerMode = false

  private def startGame(): Unit =
    // Build a tiny ring of nearby meshes immediately on the GL thread before the first
    // gameplay frame. Async chunk workers still handle the rest, but this prevents the
    // client from seeing sky-only/dark-blue frames with only remote name tags visible.
    syncChunks()
    forceCameraChunkRing(0)
    processChunkWorkMainThread(0, 12)
    ensureNearbyMeshesReady(1)
    screen = Screen.Playing
    modUiCursorMode = false
    firstMouse = true
    glfwSetInputMode(window, GLFW_CURSOR, GLFW_CURSOR_DISABLED)

  private def enterGame(): Unit =
    if screen == Screen.Inventory || screen == Screen.Catalog || screen == Screen.FurnaceUI then releaseHeldItem()
    startGame()

  private def leaveGame(next: Screen): Unit =
    saveWorld()
    // Pausing must NOT close the multiplayer server. The host needs to be able to tab away,
    // open the pause menu, or focus another local test client while the socket keeps listening.
    // Only tear networking down when actually returning to title/main menu.
    if next == Screen.MainMenu then stopNetworking()
    screen = next
    glfwSetInputMode(window, GLFW_CURSOR, GLFW_CURSOR_NORMAL)

  private def isClientOnlyMultiplayer: Boolean =
    multiplayerMode && gameClient != null && gameServer == null

  private def canUseLocalChunkSaves: Boolean = !isClientOnlyMultiplayer

  private def worldsRootDir: java.io.File = new java.io.File("worlds")
  private def currentWorldDir: java.io.File = new java.io.File(worldsRootDir, worldName)
  private def currentChunksDir: java.io.File = new java.io.File(currentWorldDir, "chunks")

  private def worldSaveDirs: List[java.io.File] =
    val root = worldsRootDir
    val files = Option(root.listFiles()).getOrElse(Array.empty[java.io.File])
    files.filter(d => d.isDirectory && new java.io.File(d, "world.dat").isFile).toList.sortWith((a, b) => a.lastModified() > b.lastModified())

  private def writeWorldIndex(): Unit =
    try
      val root = worldsRootDir
      root.mkdirs()
      val out = new java.io.PrintWriter(new java.io.BufferedWriter(new java.io.FileWriter(new java.io.File(root, "index.txt"))))
      try
        worldSaveDirs.foreach { d =>
          out.println(d.getName + "|" + new java.io.File(d, "world.dat").lastModified().toString)
        }
      finally out.close()
    catch case e: Exception => System.err.println(s"World index save failed: $e")

  private def writeWorldExtras(out: java.io.DataOutputStream): Unit =
    compactHotbarAssignments()
    out.writeInt(worldExtraMagic)
    out.writeInt(worldExtraVersion)
    out.writeInt(inventory.length)
    inventory.foreach(v => out.writeInt(v.max(0)))
    out.writeInt(hotbarBlocks.length)
    hotbarBlocks.foreach(b => out.writeByte(b.ordinal.toByte))
    hotbarCounts.foreach(c => out.writeInt(c.max(0).min(maxStackSize)))
    out.writeInt(selectedBlock.max(0).min(hotbarBlocks.length - 1))
    out.writeInt(playerHealth.round)
    out.writeInt(playerFood.round)
    out.writeByte(furnaceInput.ordinal.toByte)
    out.writeByte(furnaceFuel.ordinal.toByte)
    out.writeFloat(furnaceProgress)
    out.writeFloat(furnaceFuelRemaining)
    out.writeByte(furnaceOutput.ordinal.toByte)
    out.writeInt(furnaceOutputCount)

  private def readWorldExtras(in: java.io.DataInputStream): Unit =
    try
      if in.available() >= 8 then
        val magic = in.readInt()
        val version = in.readInt()
        if magic == worldExtraMagic && version >= 1 then
          val invLen = in.readInt().max(0).min(4096)
          var i = 0
          while i < invLen do
            val value = in.readInt().max(0)
            if i < inventory.length then inventory(i) = value
            i += 1
          val hotLen = in.readInt().max(0).min(64)
          i = 0
          while i < hotLen do
            val block = Block.fromId(in.readByte())
            if i < hotbarBlocks.length && block != Block.Water then hotbarBlocks(i) = block
            i += 1
          if version >= 2 && in.available() >= hotLen * 4 then
            i = 0
            while i < hotLen do
              val count = in.readInt().max(0).min(maxStackSize)
              if i < hotbarCounts.length then hotbarCounts(i) = count
              i += 1
          else
            i = 0
            while i < hotbarBlocks.length do
              hotbarCounts(i) = if gameMode == GameMode.Creative && hotbarBlocks(i) != Block.Air then maxStackSize else 0
              i += 1
          compactHotbarAssignments()
          selectedBlock = in.readInt().max(0).min(hotbarBlocks.length - 1)
          playerHealth = in.readInt().max(0).min(20).toFloat
          playerFood = in.readInt().max(0).min(20).toFloat
          furnaceInput = Block.fromId(in.readByte())
          furnaceFuel = Block.fromId(in.readByte())
          furnaceProgress = in.readFloat().max(0f)
          furnaceFuelRemaining = in.readFloat().max(0f)
          furnaceOutput = Block.fromId(in.readByte())
          furnaceOutputCount = in.readInt().max(0)
    catch case e: Exception => System.err.println(s"World extras load skipped: $e")

  private def snapshotWorldEditsForNetwork(): Seq[(Int, Int, Int, Byte)] =
    val loadedEdits = chunks.values.toSeq.flatMap { chunk =>
      val bx = chunk.baseX
      val bz = chunk.baseZ
      chunk.edits.synchronized {
        chunk.edits.toList.map { case ((lx, ly, lz), block) => (bx + lx, ly, bz + lz, block.id) }
      }
    }
    val rememberedEdits = networkBlockOverrides.toSeq.map { case ((x, y, z), block) => (x, y, z, block.id) }
    (loadedEdits ++ rememberedEdits).reverse.distinctBy(t => (t._1, t._2, t._3)).reverse

  private def saveLoadedChunks(): Unit =
    if !canUseLocalChunkSaves then return
    try
      val chunksDir = currentChunksDir
      chunksDir.mkdirs()
      chunks.values.foreach(_.save(chunksDir))
    catch case e: Exception => System.err.println(s"Chunk save failed: $e")

  private def clearLoadedChunks(saveFirst: Boolean): Unit =
    if saveFirst then saveLoadedChunks()
    chunkBuildQueue.clear(); chunkUploadQueue.clear()
    waterFlowQueue.clear()
    waterFlowQueued.clear()
    chunks.values.foreach(_.dispose())
    chunks.clear()

  private def saveWorld(): Unit =
    if !canUseLocalChunkSaves then return
    try
      val dir = currentWorldDir
      dir.mkdirs()
      val chunksDir = new java.io.File(dir, "chunks")
      chunksDir.mkdirs()
      val file = new java.io.File(dir, "world.dat")
      val tmp = new java.io.File(dir, "world.dat.tmp")
      val meta = new java.io.DataOutputStream(new java.io.BufferedOutputStream(new java.io.FileOutputStream(tmp)))
      try
        meta.writeLong(worldSeed)
        meta.writeFloat(camera.x); meta.writeFloat(camera.y); meta.writeFloat(camera.z)
        meta.writeFloat(yaw); meta.writeFloat(pitch)
        meta.writeByte(gameMode.ordinal.toByte)
        meta.writeBoolean(worldCheatsEnabled)
        meta.writeInt(chunks.size)
        chunks.foreach { case ((cx, cz), _) =>
          meta.writeInt(cx); meta.writeInt(cz)
          saveChunk(cx, cz)
        }
        writeWorldExtras(meta)
      finally meta.close()
      try
        java.nio.file.Files.move(tmp.toPath, file.toPath, java.nio.file.StandardCopyOption.REPLACE_EXISTING, java.nio.file.StandardCopyOption.ATOMIC_MOVE)
      catch
        case _: Exception =>
          java.nio.file.Files.move(tmp.toPath, file.toPath, java.nio.file.StandardCopyOption.REPLACE_EXISTING)
      writeWorldIndex()
    catch case e: Exception => System.err.println(s"Save failed: $e")

  private def openLoadWorldMenu(): Unit =
    loadWorldSelection = loadWorldSelection.max(0).min((worldSaveDirs.length - 1).max(0))
    loadWorldScroll = loadWorldSelection.max(0)
    screen = Screen.LoadWorld
    glfwSetInputMode(window, GLFW_CURSOR, GLFW_CURSOR_NORMAL)

  private def loadWorld(): Unit =
    openLoadWorldMenu()

  private def loadSelectedWorld(): Unit =
    val saves = worldSaveDirs
    if saves.isEmpty then
      addChatMessage("No saved worlds found")
      screen = Screen.MainMenu
    else
      loadWorldFromDir(saves(loadWorldSelection.max(0).min(saves.length - 1)))

  private def loadWorldDataFromDir(worldDir: java.io.File): Boolean =
    try
      worldName = worldDir.getName
      val meta = new java.io.DataInputStream(new java.io.BufferedInputStream(new java.io.FileInputStream(new java.io.File(worldDir, "world.dat"))))
      try
        worldSeed = meta.readLong()
        val x = meta.readFloat(); val y = meta.readFloat(); val z = meta.readFloat()
        camera = Vec3(x, y, z)
        yaw = meta.readFloat(); pitch = meta.readFloat()
        gameMode = GameMode.fromOrdinal(meta.readByte().toInt)
        worldCheatsEnabled = meta.readBoolean()
        terrainGen = TerrainGenerator(worldSeed)
        terrainHeightCache.clear()
        clearLoadedChunks(saveFirst = false)
        resetInventory()
        networkBlockOverrides.clear()
        val chunkCount = meta.readInt().max(0).min(200000)
        for _ <- 0 until chunkCount do
          val cx = meta.readInt(); val cz = meta.readInt()
          loadChunkIfSaved(cx, cz)
        readWorldExtras(meta)
      finally meta.close()
      true
    catch
      case e: Exception =>
        System.err.println(s"Load failed for ${worldDir.getName}: $e")
        false

  private def loadWorldFromDir(worldDir: java.io.File): Unit =
    if loadWorldDataFromDir(worldDir) then
      addChatMessage(s"Loaded world: $worldName")
      enterGame()
      modManager.fireWorldLoaded()
    else
      addChatMessage(s"Could not load world: ${worldDir.getName}")
      screen = Screen.LoadWorld

  private def resetHotbarDefaults(): Unit =
    var i = 0
    while i < hotbarBlocks.length do
      hotbarBlocks(i) = Block.Air
      hotbarCounts(i) = 0
      i += 1
    selectedBlock = selectedBlock.max(0).min(hotbarBlocks.length - 1)
    clearHeldItem()

  private def resetInventory(): Unit =
    java.util.Arrays.fill(inventory, 0)
    resetHotbarDefaults()
    if gameMode == GameMode.Creative then
      for b <- placeableBlocks do inventory(b.ordinal) = maxStackSize
      inventory(Block.Coal.ordinal) = inventory(Block.Coal.ordinal).max(maxStackSize)
    furnaceInput = Block.Air
    furnaceFuel = Block.Air
    furnaceProgress = 0f
    furnaceFuelRemaining = 0f
    furnaceOutput = Block.Air
    furnaceOutputCount = 0
    var cg = 0
    while cg < craftGridBlocks.length do
      craftGridBlocks(cg) = Block.Air
      cg += 1

  private def validHotbarBlock(block: Block): Boolean =
    block != Block.Air && block != Block.Water && block != Block.Bedrock && block != Block.FurnaceLit

  private def clearHeldItem(): Unit =
    heldInventoryBlock = Block.Air
    heldInventoryCount = 0
    heldFromHotbar = false

  private def releaseHeldItem(): Unit =
    if gameMode == GameMode.Survival && heldFromHotbar && heldInventoryBlock != Block.Air then addBackpackItem(heldInventoryBlock, heldInventoryCount.max(1))
    clearHeldItem()

  private def setHeldItem(block: Block, count: Int, fromHotbar: Boolean = false): Unit =
    if validHotbarBlock(block) && count > 0 then
      heldInventoryBlock = block
      heldInventoryCount = count.min(maxStackSize)
      heldFromHotbar = fromHotbar
    else clearHeldItem()

  private def addBackpackItem(block: Block, amount: Int): Int =
    if !validHotbarBlock(block) || amount <= 0 then 0
    else
      val room = 999 - inventory(block.ordinal)
      val moved = amount.min(room.max(0))
      if moved > 0 then inventory(block.ordinal) += moved
      moved

  private def hotbarItemCount(block: Block): Int =
    if !validHotbarBlock(block) then 0
    else
      var total = 0
      var i = 0
      while i < hotbarBlocks.length do
        if hotbarBlocks(i) == block then total += hotbarCounts(i).max(0)
        i += 1
      total

  private def totalItemCount(block: Block): Int =
    if gameMode == GameMode.Creative && validHotbarBlock(block) then maxStackSize
    else if validHotbarBlock(block) then inventory(block.ordinal).max(0) + hotbarItemCount(block) else 0

  private def compactHotbarAssignments(): Unit =
    // The hotbar is real stack storage now, separate from the backpack inventory.
    // It should never show a stale ghost item or duplicate shortcut clone.
    val seen = scala.collection.mutable.HashSet.empty[Block]
    var i = 0
    while i < hotbarBlocks.length do
      val b = hotbarBlocks(i)
      if !validHotbarBlock(b) then
        hotbarBlocks(i) = Block.Air
        hotbarCounts(i) = 0
      else if gameMode == GameMode.Creative then
        if seen.contains(b) then
          hotbarBlocks(i) = Block.Air
          hotbarCounts(i) = 0
        else
          seen += b
          hotbarCounts(i) = maxStackSize
      else if hotbarCounts(i) <= 0 then
        hotbarBlocks(i) = Block.Air
        hotbarCounts(i) = 0
      else if seen.contains(b) then
        addBackpackItem(b, hotbarCounts(i))
        hotbarBlocks(i) = Block.Air
        hotbarCounts(i) = 0
      else
        seen += b
        hotbarCounts(i) = hotbarCounts(i).min(maxStackSize)
      i += 1
    if gameMode == GameMode.Survival && heldInventoryBlock != Block.Air && heldInventoryCount <= 0 then clearHeldItem()

  private def assignBlockToHotbar(block: Block, preferredSlot: Int): Unit =
    if validHotbarBlock(block) then
      val slot = preferredSlot.max(0).min(hotbarBlocks.length - 1)
      val oldBlock = hotbarBlocks(slot)
      val oldCount = hotbarCounts(slot)
      if gameMode == GameMode.Survival && validHotbarBlock(oldBlock) && oldCount > 0 then addBackpackItem(oldBlock, oldCount)
      var i = 0
      while i < hotbarBlocks.length do
        if i != slot && hotbarBlocks(i) == block then
          if gameMode == GameMode.Survival && hotbarCounts(i) > 0 then addBackpackItem(block, hotbarCounts(i))
          hotbarBlocks(i) = Block.Air
          hotbarCounts(i) = 0
        i += 1
      if gameMode == GameMode.Creative then
        hotbarBlocks(slot) = block
        hotbarCounts(slot) = maxStackSize
        selectedBlock = slot
      else
        val moved = inventory(block.ordinal).max(0).min(maxStackSize)
        if moved > 0 then
          inventory(block.ordinal) -= moved
          hotbarBlocks(slot) = block
          hotbarCounts(slot) = moved
          selectedBlock = slot
        else
          hotbarBlocks(slot) = Block.Air
          hotbarCounts(slot) = 0
      compactHotbarAssignments()

  private def gainItem(block: Block, amount: Int): Unit =
    if validHotbarBlock(block) && amount > 0 then
      var remaining = amount
      if gameMode == GameMode.Survival then
        var i = 0
        while i < hotbarBlocks.length && remaining > 0 do
          if hotbarBlocks(i) == block && hotbarCounts(i) < maxStackSize then
            val moved = remaining.min(maxStackSize - hotbarCounts(i))
            hotbarCounts(i) += moved
            remaining -= moved
          i += 1
        i = 0
        while i < hotbarBlocks.length && remaining > 0 do
          if hotbarBlocks(i) == Block.Air then
            val moved = remaining.min(maxStackSize)
            hotbarBlocks(i) = block
            hotbarCounts(i) = moved
            remaining -= moved
          i += 1
      if remaining > 0 then addBackpackItem(block, remaining)
      compactHotbarAssignments()

  private def consumeInventory(block: Block, amount: Int): Boolean =
    if amount <= 0 then true
    else if gameMode == GameMode.Creative then true
    else if !validHotbarBlock(block) || totalItemCount(block) < amount then false
    else
      var remaining = amount
      val fromBackpack = remaining.min(inventory(block.ordinal).max(0))
      inventory(block.ordinal) -= fromBackpack
      remaining -= fromBackpack
      var i = 0
      while i < hotbarBlocks.length && remaining > 0 do
        if hotbarBlocks(i) == block && hotbarCounts(i) > 0 then
          val used = remaining.min(hotbarCounts(i))
          hotbarCounts(i) -= used
          remaining -= used
        i += 1
      compactHotbarAssignments()
      true

  private def consumeSelectedHotbar(amount: Int): Boolean =
    if amount <= 0 then true
    else if gameMode == GameMode.Creative then true
    else
      val slot = selectedBlock.max(0).min(hotbarBlocks.length - 1)
      if hotbarBlocks(slot) == Block.Air || hotbarCounts(slot) < amount then false
      else
        hotbarCounts(slot) -= amount
        compactHotbarAssignments()
        true

  private def takeHotbarSlot(slot0: Int): Unit =
    val slot = slot0.max(0).min(hotbarBlocks.length - 1)
    val block = hotbarBlocks(slot)
    val count = if gameMode == GameMode.Creative then maxStackSize else hotbarCounts(slot)
    if validHotbarBlock(block) && count > 0 then
      setHeldItem(block, count, fromHotbar = gameMode == GameMode.Survival)
      if gameMode == GameMode.Survival then
        hotbarBlocks(slot) = Block.Air
        hotbarCounts(slot) = 0
        compactHotbarAssignments()

  private def placeHeldItemInHotbar(slot0: Int): Unit =
    if heldInventoryBlock == Block.Air then return
    val slot = slot0.max(0).min(hotbarBlocks.length - 1)
    val oldBlock = hotbarBlocks(slot)
    val oldCount = hotbarCounts(slot)
    if gameMode == GameMode.Creative then
      hotbarBlocks(slot) = heldInventoryBlock
      hotbarCounts(slot) = maxStackSize
      selectedBlock = slot
      clearHeldItem()
    else
      if validHotbarBlock(oldBlock) && oldCount > 0 then addBackpackItem(oldBlock, oldCount)
      val moveCount = heldInventoryCount.max(1).min(maxStackSize)
      val canMove = heldFromHotbar || consumeInventory(heldInventoryBlock, moveCount)
      if canMove then
        hotbarBlocks(slot) = heldInventoryBlock
        hotbarCounts(slot) = moveCount
        selectedBlock = slot
        clearHeldItem()
        compactHotbarAssignments()
      else addChatMessage("Missing item stack")

  private final case class CraftShape(width: Int, height: Int, cells: Vector[Block])
  private final case class CraftRecipe(width: Int, height: Int, cells: Vector[Block], output: Block, outputCount: Int, label: String)

  private def craftRecipe(rows: Seq[Seq[Block]], output: Block, outputCount: Int, label: String): CraftRecipe =
    val h = rows.length.max(1)
    val w = rows.map(_.length).max.max(1)
    val cells = (0 until h).flatMap { y =>
      val row = rows(y)
      (0 until w).map(x => if x < row.length then row(x) else Block.Air)
    }.toVector
    CraftRecipe(w, h, cells, output, outputCount, label)

  private def row(blocks: Block*): Seq[Block] = blocks

  private val craftingRecipes: Array[CraftRecipe] = Array(
    craftRecipe(Seq(row(Block.Wood)), Block.Planks, 4, "oak wood -> planks"),
    craftRecipe(Seq(row(Block.BirchWood)), Block.BirchPlanks, 4, "birch wood -> planks"),
    craftRecipe(Seq(row(Block.PineWood)), Block.PinePlanks, 4, "pine wood -> planks"),
    craftRecipe(Seq(row(Block.AcaciaWood)), Block.AcaciaPlanks, 4, "acacia wood -> planks"),
    craftRecipe(Seq(row(Block.Wood, Block.Wood), row(Block.Wood, Block.Wood)), Block.Planks, 18, "oak log pack -> planks"),
    craftRecipe(Seq(row(Block.BirchWood, Block.BirchWood), row(Block.BirchWood, Block.BirchWood)), Block.BirchPlanks, 18, "birch log pack -> planks"),
    craftRecipe(Seq(row(Block.PineWood, Block.PineWood), row(Block.PineWood, Block.PineWood)), Block.PinePlanks, 18, "pine log pack -> planks"),
    craftRecipe(Seq(row(Block.AcaciaWood, Block.AcaciaWood), row(Block.AcaciaWood, Block.AcaciaWood)), Block.AcaciaPlanks, 18, "acacia log pack -> planks"),
    craftRecipe(Seq(row(Block.Stone, Block.Stone), row(Block.Stone, Block.Stone)), Block.Brick, 8, "stone square -> brick"),
    craftRecipe(Seq(row(Block.Clay, Block.Clay), row(Block.Clay, Block.Clay)), Block.Brick, 6, "clay square -> brick"),
    craftRecipe(Seq(row(Block.Sand, Block.Clay), row(Block.Clay, Block.Sand)), Block.Brick, 4, "sand clay mix -> brick"),
    craftRecipe(Seq(row(Block.Sand, Block.Sand), row(Block.Sand, Block.Sand)), Block.Glass, 4, "sand square -> glass"),
    craftRecipe(Seq(row(Block.Glass, Block.Glass), row(Block.Glass, Block.Glass)), Block.Glass, 4, "glass polish"),
    craftRecipe(Seq(row(Block.Sand, Block.Sand, Block.Sand), row(Block.Sand, Block.Clay, Block.Sand), row(Block.Sand, Block.Sand, Block.Sand)), Block.Glass, 12, "clear glass batch"),
    craftRecipe(Seq(row(Block.Stone, Block.Stone, Block.Stone), row(Block.Stone, Block.Air, Block.Stone), row(Block.Stone, Block.Stone, Block.Stone)), Block.Furnace, 1, "stone furnace"),
    craftRecipe(Seq(row(Block.Planks, Block.Planks), row(Block.Planks, Block.Planks)), Block.Furnace, 1, "plank crate -> furnace"),
    craftRecipe(Seq(row(Block.BirchPlanks, Block.BirchPlanks), row(Block.BirchPlanks, Block.BirchPlanks)), Block.Furnace, 1, "birch crate -> furnace"),
    craftRecipe(Seq(row(Block.PinePlanks, Block.PinePlanks), row(Block.PinePlanks, Block.PinePlanks)), Block.Furnace, 1, "pine crate -> furnace"),
    craftRecipe(Seq(row(Block.AcaciaPlanks, Block.AcaciaPlanks), row(Block.AcaciaPlanks, Block.AcaciaPlanks)), Block.Furnace, 1, "acacia crate -> furnace"),
    craftRecipe(Seq(row(Block.Dirt, Block.Dirt), row(Block.Dirt, Block.Dirt)), Block.Grass, 1, "dirt patch -> grass"),
    craftRecipe(Seq(row(Block.Grass), row(Block.Dirt)), Block.Dirt, 2, "turn grass to dirt"),
    craftRecipe(Seq(row(Block.Snow, Block.Snow), row(Block.Snow, Block.Snow)), Block.Snow, 4, "packed snow"),
    craftRecipe(Seq(row(Block.Cactus, Block.Cactus), row(Block.Cactus, Block.Cactus)), Block.Cactus, 4, "cactus bundle"),
    craftRecipe(Seq(row(Block.Coal, Block.Coal), row(Block.Coal, Block.Coal)), Block.Coal, 4, "coal bundle"),
    craftRecipe(Seq(row(Block.Copper, Block.Copper), row(Block.Copper, Block.Copper)), Block.Copper, 4, "copper pack"),
    craftRecipe(Seq(row(Block.IronOre, Block.IronOre), row(Block.IronOre, Block.IronOre)), Block.IronOre, 4, "iron ore pack"),
    craftRecipe(Seq(row(Block.GoldOre, Block.GoldOre), row(Block.GoldOre, Block.GoldOre)), Block.GoldOre, 4, "gold ore pack"),
    craftRecipe(Seq(row(Block.Diamond, Block.Diamond), row(Block.Diamond, Block.Diamond)), Block.Diamond, 4, "diamond cluster"),
    craftRecipe(Seq(row(Block.IronOre, Block.Coal)), Block.IronIngot, 1, "quick iron bloom"),
    craftRecipe(Seq(row(Block.GoldOre, Block.Coal)), Block.GoldIngot, 1, "quick gold bloom"),
    craftRecipe(Seq(row(Block.IronIngot, Block.IronIngot), row(Block.IronIngot, Block.IronIngot)), Block.IronOre, 1, "iron blocky ore pack"),
    craftRecipe(Seq(row(Block.GoldIngot, Block.GoldIngot), row(Block.GoldIngot, Block.GoldIngot)), Block.GoldOre, 1, "gold blocky ore pack"),
    craftRecipe(Seq(row(Block.Brick, Block.Brick), row(Block.Brick, Block.Brick)), Block.Brick, 4, "brick stack"),
    craftRecipe(Seq(row(Block.RainbowBlock, Block.Glass), row(Block.Glass, Block.RainbowBlock)), Block.RainbowBlock, 4, "rainbow glass mix"),
    craftRecipe(Seq(row(Block.Leaves, Block.Leaves), row(Block.Leaves, Block.Leaves)), Block.Leaves, 4, "leaf bundle"),
    craftRecipe(Seq(row(Block.BirchLeaves, Block.BirchLeaves), row(Block.BirchLeaves, Block.BirchLeaves)), Block.BirchLeaves, 4, "birch leaf bundle"),
    craftRecipe(Seq(row(Block.PineLeaves, Block.PineLeaves), row(Block.PineLeaves, Block.PineLeaves)), Block.PineLeaves, 4, "pine leaf bundle"),
    craftRecipe(Seq(row(Block.AcaciaLeaves, Block.AcaciaLeaves), row(Block.AcaciaLeaves, Block.AcaciaLeaves)), Block.AcaciaLeaves, 4, "acacia leaf bundle")
  )

  private def normalizeCraftGrid(cells: Array[Block]): Option[CraftShape] =
    var minX = 3; var minY = 3; var maxX = -1; var maxY = -1
    var i = 0
    while i < cells.length do
      if cells(i) != Block.Air then
        val x = i % 3; val y = i / 3
        minX = minX.min(x); minY = minY.min(y); maxX = maxX.max(x); maxY = maxY.max(y)
      i += 1
    if maxX < 0 then None
    else
      val w = maxX - minX + 1
      val h = maxY - minY + 1
      val out = ArrayBuffer.empty[Block]
      var y = minY
      while y <= maxY do
        var x = minX
        while x <= maxX do
          out += cells(y * 3 + x)
          x += 1
        y += 1
      Some(CraftShape(w, h, out.toVector))

  private def currentCraftingResult: Option[CraftRecipe] =
    normalizeCraftGrid(craftGridBlocks).flatMap { shape =>
      craftingRecipes.find(r => r.width == shape.width && r.height == shape.height && r.cells == shape.cells)
    }

  private def craftInputCounts: Map[Block, Int] =
    val counts = scala.collection.mutable.HashMap.empty[Block, Int]
    var i = 0
    while i < craftGridBlocks.length do
      val b = craftGridBlocks(i)
      if b != Block.Air then counts(b) = counts.getOrElse(b, 0) + 1
      i += 1
    counts.toMap

  private def canCraftCurrent(recipe: CraftRecipe): Boolean =
    gameMode == GameMode.Creative || craftInputCounts.forall { case (b, n) => totalItemCount(b) >= n }

  private def tryCraftGrid(): Unit =
    currentCraftingResult match
      case Some(recipe) if canCraftCurrent(recipe) =>
        val counts = craftInputCounts
        var ok = true
        counts.foreach { case (b, n) => if !consumeInventory(b, n) then ok = false }
        if ok then
          gainItem(recipe.output, recipe.outputCount)
          playPlaceSound()
        else addChatMessage("Missing crafting ingredients")
      case Some(_) => addChatMessage("Missing crafting ingredients")
      case None => addChatMessage("No matching recipe")

  private def submitChat(): Unit =
    val text = chatInput.trim
    if text.nonEmpty then
      chatHistory += chatInput
      chatHistoryIndex = -1
      val event = modManager.fireChat(ModChatEvent(modManager.api, playerName, text, cancelled = false))
      val finalText = Option(event.message).getOrElse("").trim
      if !event.cancelled && finalText.nonEmpty then
        if finalText.startsWith("/") then parseCommand(finalText)
        else sendChatMessage(finalText)
    chatOpen = false
    chatInput = ""

  private def sendChatMessage(text: String): Unit =
    addChatMessage(s"<You> $text")
    if multiplayerMode then
      val msg = s"CHAT|${networkEscape(playerName)}|${networkEscape(text)}"
      if gameClient != null && gameClient.isConnected then gameClient.send(msg)
      else if gameServer != null then gameServer.broadcast(msg)

  private def addChatMessage(msg: String): Unit =
    chatMessages += ((msg, 8f))
    if chatMessages.length > 100 then chatMessages.remove(0, chatMessages.length - 100)

  private def allPlayerNames: Seq[String] =
    (Seq(playerName) ++ knownPlayerNames.toSeq ++ remotePlayers.keys.toSeq).map(networkSafeName).filter(_.nonEmpty).distinct

  private def canUseCheatAuthority: Boolean =
    !multiplayerMode || gameServer != null || isOppedName(playerName)

  private def isClientOnlyOperator: Boolean =
    multiplayerMode && gameClient != null && gameServer == null && isOppedName(playerName)

  private def commandSuggestions(input: String): Seq[String] =
    if input == null || !input.startsWith("/") then Seq.empty
    else
      val raw = input.drop(1)
      val parts = raw.split("\\s+", -1)
      val commands = (Seq(
        "/help", "/enablecheats", "/op", "/deop", "/gamemode", "/gm",
        "/timeset", "/time", "/fly", "/tp", "/teleport", "/spawn",
        "/give", "/heal", "/feed", "/where", "/biome", "/seed", "/save", "/clear"
      ) ++ modManager.commandNames.map("/" + _)).distinct
      if !raw.contains(" ") then
        val prefix = "/" + raw.toLowerCase
        commands.filter(_.startsWith(prefix)).take(8)
      else
        parts.headOption.map(_.toLowerCase).getOrElse("") match
          case "gamemode" | "gm" =>
            val prefix = parts.lastOption.getOrElse("").toLowerCase
            Seq("survival", "creative").filter(_.startsWith(prefix)).map(m => "/" + parts.head + " " + m)
          case "timeset" | "time" =>
            val prefix = parts.lastOption.getOrElse("").toLowerCase
            Seq("day", "noon", "sunset", "night", "midnight", "sunrise", "reset").filter(_.startsWith(prefix)).map(v => "/" + parts.head + " " + v)
          case "give" =>
            if parts.length <= 2 then
              val last = parts.lastOption.getOrElse("")
              val prefix = last.toLowerCase
              Block.all.filter(validHotbarBlock).map(_.toString.toLowerCase).filter(_.startsWith(prefix)).take(8).map(name => "/" + raw.dropRight(last.length) + name)
            else Seq("/" + parts.head + " " + parts(1) + " 64")
          case "op" | "deop" =>
            val last = parts.lastOption.getOrElse("")
            val prefix = last.toLowerCase
            allPlayerNames.filter(_.toLowerCase.startsWith(prefix)).take(8).map(name => "/" + raw.dropRight(last.length) + name)
          case "tp" | "teleport" | "spawn" | "heal" | "feed" =>
            val last = parts.lastOption.getOrElse("")
            val prefix = last.toLowerCase
            allPlayerNames.filter(_.toLowerCase.startsWith(prefix)).take(8).map(name => "/" + raw.dropRight(last.length) + name)
          case _ => Seq.empty

  private def applyCommandSuggestion(): Unit =
    val suggestions = commandSuggestions(chatInput)
    if suggestions.nonEmpty then
      val picked = suggestions(commandSuggestionIndex.max(0).min(suggestions.length - 1))
      chatInput = picked + (if picked.count(_ == ' ') == 0 then " " else "")
      commandSuggestionIndex = 0

  private def findPlayerName(query: String): Option[String] =
    val q = Option(query).getOrElse("").trim.toLowerCase
    if q.isEmpty then None
    else allPlayerNames.find(_.toLowerCase == q).orElse(allPlayerNames.find(_.toLowerCase.startsWith(q)))

  private def positionOfPlayer(name: String): Option[Vec3] =
    val clean = networkSafeName(name)
    if clean.equalsIgnoreCase(playerName) then Some(camera)
    else remotePlayers.collectFirst { case (n, rp) if n.equalsIgnoreCase(clean) => rp.pos }

  private def teleportPlayer(target: String, dest: Vec3): Unit =
    val clean = networkSafeName(target)
    if clean.equalsIgnoreCase(playerName) then
      camera = dest
      velocity = Vec3(0f, 0f, 0f)
      addChatMessage(s"Teleported $clean")
    else if gameServer != null then
      val msg = "TPOS|" + networkEscape(clean) + "|" + f"${dest.x}%.3f" + "|" + f"${dest.y}%.3f" + "|" + f"${dest.z}%.3f"
      gameServer.broadcast(msg)
      addChatMessage(s"Teleported $clean")
    else addChatMessage("Only the host can teleport other players")


  private def setGameModeForPlayer(name: String, mode: GameMode): Unit =
    val safe = networkSafeName(name)
    if safe.equalsIgnoreCase(playerName) then
      if gameMode == mode then addChatMessage(s"Already in $mode mode")
      else toggleTo(mode)
    else if gameServer != null then
      gameServer.broadcast("GMODE|" + networkEscape(safe) + "|" + mode.ordinal.toString)
      addChatMessage(s"Set $safe to $mode")
    else addChatMessage("Only the host can change another player's gamemode")

  private def toggleFlyForPlayer(name: String): Unit =
    val safe = networkSafeName(name)
    if safe.equalsIgnoreCase(playerName) then
      if gameMode == GameMode.Survival then addChatMessage("Flight is only available in Creative mode")
      else
        flyEnabled = !flyEnabled
        if flyEnabled then
          velocity = Vec3(0f, 0f, 0f)
          addChatMessage("Flight enabled")
        else
          onGround = false
          velocity = Vec3(0f, -0.5f, 0f)
          addChatMessage("Flight disabled")
    else if gameServer != null then
      gameServer.broadcast("FLY|" + networkEscape(safe) + "|TOGGLE")
      addChatMessage(s"Toggled flight for $safe")
    else addChatMessage("Only the host can toggle another player's flight")

  private def setTimeCommand(mode: String): Boolean =
    val normalized = Option(mode).getOrElse("").trim.toLowerCase
    val applied = normalized match
      case "sunrise" =>
        timeOverride = Some(0f)
        addChatMessage("Set time to sunrise")
        true
      case "day" =>
        timeOverride = Some(dayLengthSeconds * 0.25f)
        addChatMessage("Set time to day")
        true
      case "noon" =>
        timeOverride = Some(dayLengthSeconds * 0.50f)
        addChatMessage("Set time to noon")
        true
      case "sunset" =>
        timeOverride = Some(dayLengthSeconds * 0.92f)
        addChatMessage("Set time to sunset")
        true
      case "night" =>
        timeOverride = Some(dayLengthSeconds + nightLengthSeconds * 0.18f)
        addChatMessage("Set time to night")
        true
      case "midnight" =>
        timeOverride = Some(dayLengthSeconds + nightLengthSeconds * 0.50f)
        addChatMessage("Set time to midnight")
        true
      case "reset" =>
        timeOverride = None
        addChatMessage("Time override cleared")
        true
      case _ => false
    if applied && gameServer != null then gameServer.broadcast("TIME|" + normalized)
    applied

  private def parseGiveBlock(text: String): Option[Block] =
    Block.find(text).filter(validHotbarBlock)

  private def giveItemForPlayer(target: String, block: Block, count: Int): Unit =
    val safe = networkSafeName(target)
    val amount = count.max(1).min(999)
    if safe.equalsIgnoreCase(playerName) then
      gainItem(block, amount)
      addChatMessage(s"Gave $amount ${blockName(block)} to $safe")
    else if gameServer != null then
      gameServer.broadcast("GIVE|" + networkEscape(safe) + "|" + block.id.toInt.toString + "|" + amount.toString)
      addChatMessage(s"Gave $amount ${blockName(block)} to $safe")
    else addChatMessage("Only the host can give items to another player")

  private def setVitalsForPlayer(target: String, health: Option[Float], food: Option[Float], label: String): Unit =
    val safe = networkSafeName(target)
    val hp = health.map(_.max(0f).min(maxPlayerHealth))
    val fd = food.map(_.max(0f).min(maxPlayerFood))
    if safe.equalsIgnoreCase(playerName) then
      hp.foreach(v => playerHealth = v)
      fd.foreach(v => playerFood = v)
      addChatMessage(label + " " + safe)
    else if gameServer != null then
      val hpText = hp.map(v => f"$v%.2f").getOrElse("-1")
      val foodText = fd.map(v => f"$v%.2f").getOrElse("-1")
      gameServer.broadcast("VITAL|" + networkEscape(safe) + "|" + hpText + "|" + foodText + "|" + networkEscape(label))
      addChatMessage(label + " " + safe)
    else addChatMessage("Only the host can change another player's health or food")

  private def biomeNameAt(x: Int, z: Int, y: Int): String =
    val savanna = terrainGen.savannaBlendAt(x, z, y)
    val birch = terrainGen.birchBlendAt(x, z, y)
    if savanna > 0.58f && savanna >= birch then "Savanna"
    else if birch > 0.58f && birch > savanna then "Birch Grove"
    else if savanna > 0.24f && birch > 0.24f then "Mixed Grassland"
    else if savanna > 0.24f then "Savanna Edge"
    else if birch > 0.24f then "Birch Edge"
    else "Plains"

  private def addPositionReport(name: String, pos: Vec3): Unit =
    val bx = floor(pos.x).toInt
    val by = floor(pos.y).toInt
    val bz = floor(pos.z).toInt
    val biome = biomeNameAt(bx, bz, by)
    addChatMessage(s"$name: x=$bx y=$by z=$bz chunk=(${chunkCoordBlock(bx)}, ${chunkCoordBlock(bz)}) biome=$biome")

  private def parseCoord(text: String, current: Float): Option[Float] =
    try
      if text == "~" then Some(current)
      else if text.startsWith("~") then Some(current + text.drop(1).toFloat)
      else Some(text.toFloat)
    catch case _: Exception => None

  private def parseCommand(cmd: String, remoteSender: Option[String] = None): Unit =
    val trimmed = cmd.trim
    if !trimmed.startsWith("/") then
      sendChatMessage(trimmed)
      return

    val args = trimmed.drop(1).split("\\s+").filter(_.nonEmpty)
    if args.isEmpty then return
    val command = args(0).toLowerCase
    val actorName = remoteSender.map(networkSafeName).getOrElse(playerName)
    val actorIsHostLocal = remoteSender.isEmpty && (!multiplayerMode || gameServer != null)
    val actorIsOp = actorIsHostLocal || remoteSender.exists(isOppedName) || (remoteSender.isEmpty && isOppedName(playerName))
    if modManager.hasCommand(command) then
      if remoteSender.isEmpty && gameClient != null && gameServer == null && modManager.commandIsServerInteractive(command) then
        if isOppedName(playerName) then
          gameClient.send("CMD|" + networkEscape(trimmed))
        else addChatMessage("That mod command is server-side. Ask the host for /op.")
      else modManager.executeCommand(command, args.drop(1), actorName, remoteSender.nonEmpty)
      return
    val isCheat = Set(
      "gamemode", "gm", "timeset", "time", "fly", "tp", "teleport",
      "spawn", "give", "heal", "feed", "op", "deop"
    ).contains(command)

    if remoteSender.isEmpty && isClientOnlyOperator && isCheat && command != "enablecheats" && command != "cheats" && command != "op" && command != "deop" then
      gameClient.send("CMD|" + networkEscape(trimmed))
      return

    if command == "enablecheats" || command == "cheats" then
      if actorIsHostLocal then
        if worldCheatsEnabled then addChatMessage("Cheats are already enabled")
        else
          worldCheatsEnabled = true
          addChatMessage("Cheats enabled")
      else addChatMessage("Only the host can enable cheats")
      return

    if (command == "op" || command == "deop") then
      if !actorIsHostLocal then
        addChatMessage("Only the host can op or deop players")
        return
      if !worldCheatsEnabled then
        addChatMessage("Cheats are not enabled. Run /enablecheats first")
        return
      if args.length < 2 then
        addChatMessage("Usage: /" + command + " <player>")
        return
      val target = findPlayerName(args(1)).getOrElse(networkSafeName(args(1)))
      val value = command == "op"
      if value && isOppedName(target) then addChatMessage(s"$target is already an operator")
      else if !value && !isOppedName(target) then addChatMessage(s"$target is not an operator")
      else
        setOppedName(target, value)
        broadcastOpState(target, value)
      return

    if isCheat && !actorIsOp then
      addChatMessage(if remoteSender.isDefined then s"$actorName tried to use a command but is not opped" else "You are not opped")
      return
    if isCheat && !worldCheatsEnabled then
      addChatMessage("Cheats are not enabled. Host can run /enablecheats")
      return

    command match
      case "gamemode" | "gm" =>
        val mode = if args.length > 1 then args(1).toLowerCase else ""
        val targetName = if args.length > 2 then findPlayerName(args(2)).getOrElse(networkSafeName(args(2))) else actorName
        val parsedMode = mode match
          case "survival" | "s" | "0" => Some(GameMode.Survival)
          case "creative" | "c" | "1" => Some(GameMode.Creative)
          case _ => None
        parsedMode match
          case Some(nextMode) => setGameModeForPlayer(targetName, nextMode)
          case None => addChatMessage("Usage: /gamemode <survival|creative> [player]")
      case "timeset" | "time" =>
        val when = if args.length > 1 then args(1).toLowerCase else ""
        if !setTimeCommand(when) then addChatMessage("Usage: /time <sunrise|day|noon|sunset|night|midnight|reset>")
      case "fly" =>
        val targetName = if args.length > 1 then findPlayerName(args(1)).getOrElse(networkSafeName(args(1))) else actorName
        toggleFlyForPlayer(targetName)
      case "spawn" =>
        val targetName = if args.length > 1 then findPlayerName(args(1)).getOrElse(networkSafeName(args(1))) else actorName
        teleportPlayer(targetName, findSpawn())
      case "give" =>
        if args.length < 2 then addChatMessage("Usage: /give <block> [count] [player]")
        else
          parseGiveBlock(args(1)) match
            case Some(block) =>
              val amount = if args.length > 2 then args(2).toIntOption.getOrElse(64) else 64
              val targetName = if args.length > 3 then findPlayerName(args(3)).getOrElse(networkSafeName(args(3))) else actorName
              giveItemForPlayer(targetName, block, amount)
            case None => addChatMessage("Unknown or ungiveable block: " + args(1))
      case "heal" =>
        val targetName = if args.length > 1 then findPlayerName(args(1)).getOrElse(networkSafeName(args(1))) else actorName
        setVitalsForPlayer(targetName, Some(maxPlayerHealth), None, "Healed")
      case "feed" =>
        val targetName = if args.length > 1 then findPlayerName(args(1)).getOrElse(networkSafeName(args(1))) else actorName
        setVitalsForPlayer(targetName, None, Some(maxPlayerFood), "Fed")
      case "tp" | "teleport" =>
        if args.length == 2 then
          findPlayerName(args(1)).flatMap(positionOfPlayer) match
            case Some(dest) => teleportPlayer(actorName, dest)
            case None => addChatMessage("Usage: /tp <player> OR /tp <target> <destination> OR /tp <x> <y> <z>")
        else if args.length == 3 then
          val targetOpt = findPlayerName(args(1))
          val destOpt = findPlayerName(args(2)).flatMap(positionOfPlayer)
          (targetOpt, destOpt) match
            case (Some(target), Some(dest)) => teleportPlayer(target, dest)
            case _ => addChatMessage("Usage: /tp <target> <destination>")
        else if args.length == 4 then
          val base = positionOfPlayer(actorName).getOrElse(camera)
          val x = parseCoord(args(1), base.x)
          val y = parseCoord(args(2), base.y)
          val z = parseCoord(args(3), base.z)
          (x, y, z) match
            case (Some(px), Some(py), Some(pz)) => teleportPlayer(actorName, Vec3(px, py, pz))
            case _ => addChatMessage("Usage: /tp <x> <y> <z>")
        else if args.length == 5 then
          val targetOpt = findPlayerName(args(1))
          val base = targetOpt.flatMap(positionOfPlayer).getOrElse(camera)
          val x = parseCoord(args(2), base.x)
          val y = parseCoord(args(3), base.y)
          val z = parseCoord(args(4), base.z)
          (targetOpt, x, y, z) match
            case (Some(target), Some(px), Some(py), Some(pz)) => teleportPlayer(target, Vec3(px, py, pz))
            case _ => addChatMessage("Usage: /tp <target> <x> <y> <z>")
        else addChatMessage("Usage: /tp <player> | /tp <target> <destination> | /tp <x> <y> <z>")
      case "where" =>
        val targetName = if args.length > 1 then findPlayerName(args(1)).getOrElse(networkSafeName(args(1))) else actorName
        positionOfPlayer(targetName) match
          case Some(pos) => addPositionReport(targetName, pos)
          case None => addChatMessage("Unknown player: " + targetName)
      case "biome" =>
        val pos = positionOfPlayer(actorName).getOrElse(camera)
        val bx = floor(pos.x).toInt
        val by = floor(pos.y).toInt
        val bz = floor(pos.z).toInt
        val savanna = terrainGen.savannaBlendAt(bx, bz, by)
        val birch = terrainGen.birchBlendAt(bx, bz, by)
        val tint = terrainGen.grassTintAt(bx, bz, by)
        addChatMessage(f"Biome: ${biomeNameAt(bx, bz, by)} savanna=$savanna%.2f birch=$birch%.2f grassRGB=${tint._1}%.2f/${tint._2}%.2f/${tint._3}%.2f")
      case "seed" =>
        addChatMessage(s"World: $worldName seed=$worldSeed")
      case "save" =>
        if !actorIsHostLocal then addChatMessage("Only the host can save the world")
        else
          saveWorld()
          addChatMessage(s"Saved world: $worldName")
      case "clear" =>
        chatMessages.clear()
      case "help" =>
        addChatMessage("Commands: /help, /where, /biome, /seed, /clear, /save")
        addChatMessage("Admin: /enablecheats, /op, /deop, /gamemode, /time, /fly, /tp, /spawn, /give, /heal, /feed")
      case _ =>
        addChatMessage(s"Unknown command: /${args(0)}")

  private def toggleTo(mode: GameMode): Unit =
    gameMode = mode
    if mode == GameMode.Survival then flyEnabled = false
    velocity = Vec3(0f, 0f, 0f); onGround = false
    addChatMessage(s"Game mode set to $mode")

  private def openInventory(): Unit =
    catalogScroll = 0
    screen = Screen.Inventory
    glfwSetInputMode(window, GLFW_CURSOR, GLFW_CURSOR_NORMAL)
    breakingBlock = null
    breakingProgress = 0f

  private def openCatalog(): Unit =
    catalogScroll = 0
    screen = Screen.Catalog
    glfwSetInputMode(window, GLFW_CURSOR, GLFW_CURSOR_NORMAL)
    breakingBlock = null
    breakingProgress = 0f

  private def openFurnace(): Unit =
    screen = Screen.FurnaceUI
    glfwSetInputMode(window, GLFW_CURSOR, GLFW_CURSOR_NORMAL)
    breakingBlock = null
    breakingProgress = 0f

  private def playPlaceSound(): Unit = playTone(140f, 0.045f, 0.28f, 0.35f)
  private def playBreakSound(): Unit = playNoise(0.075f, 0.26f)
  private def playTone(freq: Float, seconds: Float, volume: Float, decay: Float): Unit =
    if soundEnabled then playSound(seconds) { (i, rate) =>
      val t = i.toFloat / rate
      val env = pow((1f - t / seconds).max(0f), decay.toDouble).toFloat
      sin(2.0 * Pi * freq * t).toFloat * volume * env
    }

  private def playNoise(seconds: Float, volume: Float): Unit =
    if soundEnabled then playSound(seconds) { (i, _) =>
      var n = i * 1103515245 + 12345
      n = (n ^ (n >>> 16)) * 2246822519L.toInt
      val env = (1f - i.toFloat / (44100f * seconds)).max(0f)
      (((n & 0xffff).toFloat / 32768f) - 1f) * volume * env
    }

  private def playSound(seconds: Float)(sample: (Int, Float) => Float): Unit =
    Thread(() =>
      try
        val rate = 44100f
        val frames = (rate * seconds).toInt.max(1)
        val bytes = new Array[Byte](frames * 2)
        var i = 0
        while i < frames do
          val s = (sample(i, rate).max(-1f).min(1f) * Short.MaxValue).toInt
          bytes(i * 2) = (s & 0xff).toByte
          bytes(i * 2 + 1) = ((s >>> 8) & 0xff).toByte
          i += 1
        val format = AudioFormat(rate, 16, 1, true, false)
        val line = AudioSystem.getSourceDataLine(format)
        line.open(format, bytes.length)
        line.start()
        line.write(bytes, 0, bytes.length)
        line.drain()
        line.close()
      catch case _: Throwable => ()
    ).start()

  private def updateGame(dt: Float): Unit =
    processNetworkMessages()
    syncChunks()
    // Keep the camera neighborhood deterministic. If the local terrain is not
    // ready, build the 5x5 ring right now instead of rendering a sky-only void.
    if !centerChunkReady then forceCameraChunkRing(0)
    processChunkWorkMainThread(0, 12)
    for i <- chatMessages.indices do
      val (msg, t) = chatMessages(i)
      chatMessages(i) = (msg, t - dt)
    chatMessages.filterInPlace((_, t) => t > -2f)
    cleanupRemotePlayers()
    if chatOpen then
      // Chat is also a client-side UI state in multiplayer; keep the player present/alive.
      sendPlayerPositionNetwork()
      return
    if modUiCursorMode then
      // Client mod GUI focus: keep networking/chunk updates alive, but do not steal mouse-look or break/place blocks.
      handleModGuiMouse()
      sendPlayerPositionNetwork()
      return
    val x = BufferUtils.createDoubleBuffer(1)
    val y = BufferUtils.createDoubleBuffer(1)
    glfwGetCursorPos(window, x, y)
    if firstMouse then
      lastMouseX = x.get(0); lastMouseY = y.get(0); firstMouse = false
    val dx = (x.get(0) - lastMouseX).toFloat
    val dy = (y.get(0) - lastMouseY).toFloat
    lastMouseX = x.get(0); lastMouseY = y.get(0)
    yaw += dx * 0.09f
    pitch = (pitch + dy * 0.09f).max(-89f).min(89f)
    val forward = Vec3(sin(toRadians(yaw)).toFloat, 0f, -cos(toRadians(yaw)).toFloat).normalized
    val right = Vec3(cos(toRadians(yaw)).toFloat, 0f, sin(toRadians(yaw)).toFloat).normalized
    var move = Vec3(0, 0, 0)
    if down(GLFW_KEY_W) then move += forward
    if down(GLFW_KEY_S) then move -= forward
    if down(GLFW_KEY_D) then move += right
    if down(GLFW_KEY_A) then move -= right
    val moving = move.lengthSquared > 0.001f
    val sprinting = moving && (down(GLFW_KEY_LEFT_CONTROL) || down(GLFW_KEY_RIGHT_CONTROL))
    val crouching = onGround && moving && (down(GLFW_KEY_LEFT_SHIFT) || down(GLFW_KEY_RIGHT_SHIFT))
    gameMode match
      case GameMode.Creative =>
        val spaceDown = down(GLFW_KEY_SPACE)
        val spacePressed = spaceDown && !wasSpaceDown
        wasSpaceDown = spaceDown
        val now = glfwGetTime()
        if spacePressed then
          if now - lastSpacePressTime < doubleTapInterval then
            flyEnabled = !flyEnabled
            if flyEnabled then
              velocity = Vec3(0f, 0f, 0f)
              addChatMessage("Flight enabled")
            else
              onGround = false
              velocity = Vec3(0f, -0.5f, 0f)
              addChatMessage("Flight disabled")
          lastSpacePressTime = now
        if flyEnabled then
          val speed = if fastMove then 16f else 9f
          if down(GLFW_KEY_SPACE) then move += Vec3(0, 1, 0)
          if down(GLFW_KEY_LEFT_SHIFT) then move -= Vec3(0, 1, 0)
          camera = camera + move.normalized * speed * dt
        else
          val submerged = waterSubmersion
          val headInWater = blockAt(camera) == Block.Water
          val bodyInWater = blockAt(camera - Vec3(0f, 0.8f, 0f)) == Block.Water
          val swimming = headInWater || bodyInWater || submerged > 0f
          val speed =
            if swimming then if fastMove then 6.5f else 3.0f
            else if fastMove then 16f else 9f
          val hMove = move.normalized * speed * dt
          movePlayer(hMove.x, 0f, hMove.z)
          if swimming then
            updateSwimming(dt, swimming, headInWater, bodyInWater, submerged)
            moveVertical(velocity.y * dt)
            onGround = false
          else
            if spacePressed && onGround then
              velocity = Vec3(0f, 12.5f, 0f)
              onGround = false
            velocity = Vec3(0f, (velocity.y - 52f * dt).max(-78f), 0f)
            moveVertical(velocity.y * dt)
      case GameMode.Survival =>
        val submerged = waterSubmersion
        val headInWater = blockAt(camera) == Block.Water
        val bodyInWater = blockAt(camera - Vec3(0f, 0.8f, 0f)) == Block.Water
        val swimming = headInWater || bodyInWater || submerged > 0f
        val speed =
          if fastMove then 13f
          else if crouching then 3.2f
          else if swimming then if sprinting then 3.8f else 2.8f
          else if sprinting then 7.2f
          else 5.5f
        val airControl = if onGround || swimming then 1f else 0.72f
        val horizontal = move.normalized * speed * airControl * dt
        if crouching && onGround then
          val hx = if hasGroundBelow(camera + Vec3(horizontal.x, 0f, 0f)) then horizontal.x else 0f
          val hz = if hasGroundBelow(camera + Vec3(0f, 0f, horizontal.z)) then horizontal.z else 0f
          movePlayer(hx, 0f, hz)
        else
          movePlayer(horizontal.x, 0f, horizontal.z)
        updateSwimming(dt, swimming, headInWater, bodyInWater, submerged)
        moveVertical(velocity.y * dt)
        if swimming then onGround = false
    updateEnvironmentalDamage(dt)
    updateHealthRegen(dt)
    updateWater(dt)
    updateBubbles(dt)
    updateSandFalling()
    updateSandParticles(dt)
    processChunkWorkMainThread(0, 12)
    flushDirtyChunks()
    sendPlayerPositionNetwork()
    handleMouseButtons()

  private def handleModGuiMouse(): Unit =
    val (mx, my) = mouseFramebufferPos()
    modLastMouseX = mx
    modLastMouseY = my
    val leftDown = glfwGetMouseButton(window, GLFW_MOUSE_BUTTON_LEFT) == GLFW_PRESS
    modLeftDownNow = leftDown
    modLeftClickedThisFrame = leftDown && !modGuiLeftWasDown
    if modLeftClickedThisFrame then modManager.fireMouseClick(mx, my, GLFW_MOUSE_BUTTON_LEFT)
    modGuiLeftWasDown = leftDown

  private def shouldRunMultiplayerBackgroundTick: Boolean =
    multiplayerMode &&
      (screen == Screen.Paused || screen == Screen.Inventory || screen == Screen.Catalog || screen == Screen.FurnaceUI ||
        (screen == Screen.Settings && (settingsReturnTo == Screen.Playing || settingsReturnTo == Screen.Paused)))

  private def updateMultiplayerBackgroundTick(dt: Float): Unit =
    // Do NOT run local movement/physics here. This is only the live multiplayer heartbeat for UI screens.
    processNetworkMessages()
    syncChunks()
    if !centerChunkReady then forceCameraChunkRing(0)
    processChunkWorkMainThread(0, 8)
    for i <- chatMessages.indices do
      val (msg, t) = chatMessages(i)
      chatMessages(i) = (msg, t - dt)
    chatMessages.filterInPlace((_, t) => t > -2f)
    cleanupRemotePlayers()
    // Keep chunk meshes alive while multiplayer menus are open, but cap work so UI remains responsive.
    processChunkWorkMainThread(0, 8)
    flushDirtyChunks()
    sendPlayerPositionNetwork()

  private def updateSwimming(dt: Float, swimming: Boolean, headInWater: Boolean, bodyInWater: Boolean, submerged: Float): Unit =
    if !swimming then
      if onGround && down(GLFW_KEY_SPACE) then
        velocity = Vec3(0f, 12.5f, 0f)
        onGround = false
      else velocity = Vec3(0f, (velocity.y - 52f * dt).max(-78f), 0f)
      return
    val swimUp = down(GLFW_KEY_SPACE)
    val swimDown = down(GLFW_KEY_LEFT_SHIFT) || down(GLFW_KEY_RIGHT_SHIFT)
    if headInWater && submerged >= 0.3f then
      val buoyancy = 8f
      val gravity = -4f
      val inputLift = (if swimUp then 14f else 0f) - (if swimDown then 10f else 0f)
      val damping = pow(0.35, (dt * 3).toDouble).toFloat
      val targetVy = if swimUp then 3f else if swimDown then -3f else 0f
      velocity = Vec3(0f, velocity.y + (gravity + buoyancy + inputLift - velocity.y * 2f) * dt, 0f)
      velocity = Vec3(0f, velocity.y.max(-4.5f).min(4.5f), 0f)
      onGround = false
    else if bodyInWater then
      if swimUp then
        velocity = Vec3(0f, 6.5f, 0f)
        onGround = false
      else if swimDown then
        velocity = Vec3(0f, -4.5f, 0f)
        onGround = false
      else
        velocity = Vec3(0f, (velocity.y * 0.85f - 1f * dt).max(-2f).min(2f), 0f)
        onGround = false
    else
      velocity = Vec3(0f, (velocity.y - 52f * dt).max(-78f), 0f)
      if onGround && swimUp then velocity = Vec3(0f, 8.4f, 0f)
      if swimUp && !onGround then velocity = Vec3(0f, velocity.y.max(3f), 0f)

  private def handleMouseButtons(): Unit =
    val leftDown = glfwGetMouseButton(window, GLFW_MOUSE_BUTTON_LEFT) == GLFW_PRESS
    val rightDown = glfwGetMouseButton(window, GLFW_MOUSE_BUTTON_RIGHT) == GLFW_PRESS
    if leftDown && !leftWasDown && gameMode == GameMode.Creative then breakTargetBlock(dropItem = false)
    if gameMode == GameMode.Survival then updateBlockBreaking(lastFrameTime.toFloat, leftDown)
    else if !leftDown then resetBreaking()
    if rightDown && !rightWasDown then placeTargetBlock()
    leftWasDown = leftDown
    rightWasDown = rightDown

  private def updateBlockBreaking(dt: Float, leftDown: Boolean): Unit =
    if !leftDown then
      resetBreaking()
      return
    raycast().foreach { hit =>
      val target = hit.block
      val block = activeBlockAt(target._1, target._2, target._3)
      if !block.solid then resetBreaking()
      else
        val sameTarget = breakingBlock != null && breakingBlock == target
        if !sameTarget then
          breakingBlock = target
          breakingProgress = 0f
        breakingProgress += dt / blockHardness(block)
        if breakingProgress >= 1f then
          breakTargetBlock(dropItem = true)
          resetBreaking()
    }

  private def resetBreaking(): Unit =
    breakingBlock = null
    breakingProgress = 0f

  private def triggerSandFallAbove(x: Int, y: Int, z: Int): Unit =
    // Falling sand is disabled for now: sand behaves as a normal solid block until
    // its visuals and physics are rebuilt to a higher standard.
    ()

  private def triggerSandFallBelow(x: Int, y: Int, z: Int): Unit =
    // Falling sand is disabled for now: sand behaves as a normal solid block until
    // its visuals and physics are rebuilt to a higher standard.
    ()

  private def loadedChunkForBlock(x: Int, z: Int): Option[Chunk] =
    chunks.get((chunkCoordBlock(x), chunkCoordBlock(z)))

  private def waterLevelAt(x: Int, y: Int, z: Int): Int =
    if y < 0 || y >= Terrain.worldHeight then 0
    else
      loadedChunkForBlock(x, z) match
        case Some(chunk) =>
          val cx = chunkCoordBlock(x); val cz = chunkCoordBlock(z)
          chunk.getWaterLevel(x - cx * Terrain.chunkSize, y, z - cz * Terrain.chunkSize)
        case None => 0

  private def waterRawLevelAt(x: Int, y: Int, z: Int): Int =
    if y < 0 || y >= Terrain.worldHeight then 0
    else
      loadedChunkForBlock(x, z) match
        case Some(chunk) =>
          val cx = chunkCoordBlock(x); val cz = chunkCoordBlock(z)
          chunk.getWaterRawLevel(x - cx * Terrain.chunkSize, y, z - cz * Terrain.chunkSize)
        case None => 0

  private def setWaterLevelAt(x: Int, y: Int, z: Int, level: Int): Unit =
    if y >= 0 && y < Terrain.worldHeight then
      loadedChunkForBlock(x, z).foreach { chunk =>
        val cx = chunkCoordBlock(x); val cz = chunkCoordBlock(z)
        val next = level.max(0).min(8)
        chunk.setWaterLevel(x - cx * Terrain.chunkSize, y, z - cz * Terrain.chunkSize, next)
        if next > 0 then waterLevels((x, y, z)) = next.toByte else waterLevels.remove((x, y, z))
      }

  private def markWaterActive(x: Int, y: Int, z: Int): Unit =
    if y >= 0 && y < Terrain.worldHeight && loadedChunkForBlock(x, z).isDefined && activeBlockAt(x, y, z) == Block.Water then
      val key = (x, y, z)
      if waterFlowQueued.size < maxQueuedWaterCells && !waterFlowQueued.contains(key) then
        waterFlowQueue.enqueue(key)
        waterFlowQueued += key

  private def wakeWaterAround(x: Int, y: Int, z: Int): Unit =
    markWaterActive(x, y, z)
    markWaterActive(x + 1, y, z)
    markWaterActive(x - 1, y, z)
    markWaterActive(x, y, z + 1)
    markWaterActive(x, y, z - 1)
    markWaterActive(x, y + 1, z)
    markWaterActive(x, y - 1, z)

  private def waterCanEnter(block: Block): Boolean =
    block == Block.Air || block == Block.Water

  private def setFlowWater(x: Int, y: Int, z: Int, level: Int): Unit =
    if y >= 0 && y < Terrain.worldHeight && loadedChunkForBlock(x, z).isDefined then
      if activeBlockAt(x, y, z) != Block.Water then setActiveBlock(x, y, z, Block.Water)
      val nextLevel = level.max(1).min(8)
      setWaterLevelAt(x, y, z, nextLevel)
      networkBlockOverrides((x, y, z)) = Block.Water
      networkWaterLevelOverrides((x, y, z)) = nextLevel.toByte
      if multiplayerMode && gameServer != null then gameServer.broadcast(s"BLOC|$x|$y|$z|${Block.Water.id}|$nextLevel")
      markWaterActive(x, y, z)
      dirtyChunkAt(x, z)

  private def clearFlowWater(x: Int, y: Int, z: Int): Unit =
    if loadedChunkForBlock(x, z).isDefined && activeBlockAt(x, y, z) == Block.Water then
      setWaterLevelAt(x, y, z, 0)
      setActiveBlock(x, y, z, Block.Air)
      networkBlockOverrides((x, y, z)) = Block.Air
      networkWaterLevelOverrides.remove((x, y, z))
      if multiplayerMode && gameServer != null then gameServer.broadcast(s"BLOC|$x|$y|$z|0")
      dirtyChunkAt(x, z)
      wakeWaterAround(x, y, z)

  private def updateWater(dt: Float): Unit =
    waterFlowTimer += dt
    if waterFlowTimer < waterFlowInterval then return
    waterFlowTimer = 0f
    if waterFlowQueue.isEmpty then return

    var processed = 0
    val budget = math.min(maxWaterUpdatesPerFrame, waterFlowQueue.size)
    while processed < budget && waterFlowQueue.nonEmpty do
      val (wx, wy, wz) = waterFlowQueue.dequeue()
      waterFlowQueued -= ((wx, wy, wz))
      simulateWaterCell(wx, wy, wz)
      processed += 1

  private def simulateWaterCell(wx: Int, wy: Int, wz: Int): Unit =
    if loadedChunkForBlock(wx, wz).isEmpty || activeBlockAt(wx, wy, wz) != Block.Water then
      waterLevels.remove((wx, wy, wz))
      return

    val rawLevel = waterRawLevelAt(wx, wy, wz)
    val level = (if rawLevel <= 0 then 8 else rawLevel).max(1).min(8)
    val supportedFromAbove = activeBlockAt(wx, wy + 1, wz) == Block.Water
    val source = rawLevel <= 0 || (rawLevel >= 8 && !supportedFromAbove)
    val dirs = Array((1, 0), (-1, 0), (0, 1), (0, -1))
    def fedByHorizontalFlow: Boolean =
      dirs.exists { case (dx, dz) =>
        val nx = wx + dx
        val nz = wz + dz
        activeBlockAt(nx, wy, nz) == Block.Water && waterLevelAt(nx, wy, nz) > level
      }

    if wy > 0 then
      val below = activeBlockAt(wx, wy - 1, wz)
      if below == Block.Air then
        val downLevel = if source then 7 else level
        setFlowWater(wx, wy - 1, wz, downLevel)
        if !source && !supportedFromAbove && !fedByHorizontalFlow then
          clearFlowWater(wx, wy, wz)
          wakeWaterAround(wx, wy - 1, wz)
          return
        else if !source && supportedFromAbove && !fedByHorizontalFlow then
          setWaterLevelAt(wx, wy, wz, level)
          markWaterActive(wx, wy, wz)
          wakeWaterAround(wx, wy - 1, wz)
          return
        else
          setWaterLevelAt(wx, wy, wz, if source then 8 else level)
          markWaterActive(wx, wy, wz)
        wakeWaterAround(wx, wy - 1, wz)
      else if below == Block.Water then
        val belowLevel = waterLevelAt(wx, wy - 1, wz)
        val desiredBelow = if source then 7 else level
        if belowLevel > 0 && belowLevel < desiredBelow then
          setWaterLevelAt(wx, wy - 1, wz, desiredBelow)
          dirtyChunkAt(wx, wz)
          markWaterActive(wx, wy - 1, wz)
          if source || supportedFromAbove || waterLevelAt(wx, wy, wz) > 1 then markWaterActive(wx, wy, wz)
          return
        if !source && supportedFromAbove && !fedByHorizontalFlow then
          markWaterActive(wx, wy, wz)
          return

    val targetLevel = if source then 7 else level - 1
    var changed = false
    if targetLevel > 0 then
      val rot = ((wx * 31 + wz * 17 + wy * 13) & 3)
      var i = 0
      while i < 4 do
        val (dx, dz) = dirs((i + rot) & 3)
        val nx = wx + dx; val nz = wz + dz
        val nb = activeBlockAt(nx, wy, nz)
        if nb == Block.Air then
          setFlowWater(nx, wy, nz, targetLevel)
          changed = true
        else if nb == Block.Water then
          val nLevel = waterLevelAt(nx, wy, nz)
          if nLevel > 0 && nLevel + 1 < targetLevel then
            setWaterLevelAt(nx, wy, nz, targetLevel)
            dirtyChunkAt(nx, nz)
            markWaterActive(nx, wy, nz)
            changed = true
        i += 1

    if changed then
      dirtyChunkAt(wx, wz)
      if source || waterLevelAt(wx, wy, wz) > 1 then markWaterActive(wx, wy, wz)
    else if !source then
      // Drain disconnected non-source water gradually when it has no stronger neighbor.
      // This keeps old flows from hovering forever after a source is removed.
      val supported =
        supportedFromAbove ||
        dirs.exists { case (dx, dz) =>
          val nx = wx + dx; val nz = wz + dz
          activeBlockAt(nx, wy, nz) == Block.Water && waterLevelAt(nx, wy, nz) > level
        }
      if !supported then
        val next = level - 1
        if next <= 0 then clearFlowWater(wx, wy, wz)
        else
          setWaterLevelAt(wx, wy, wz, next)
          dirtyChunkAt(wx, wz)
          markWaterActive(wx, wy, wz)

  private def updateSandFalling(): Unit =
    sandFallQueue.clear()

  private def visibleSandFall(x: Float, y: Float, z: Float, startY: Float): Unit =
    ()

  private def updateSandParticles(dt: Float): Unit =
    fallingSandParticles.clear()

  private def blockHardness(block: Block): Float = block match
    case Block.Leaves | Block.BirchLeaves | Block.PineLeaves | Block.AcaciaLeaves | Block.Snow => 0.18f
    case Block.Dirt | Block.Sand | Block.Grass => 0.42f
    case Block.Wood | Block.BirchWood | Block.PineWood | Block.AcaciaWood | Block.Planks | Block.BirchPlanks | Block.PinePlanks | Block.AcaciaPlanks => 0.75f
    case Block.Glass => 0.35f
    case Block.Stone | Block.Coal | Block.Copper | Block.Clay | Block.IronOre => 1.15f
    case Block.GoldOre => 1.50f
    case Block.Brick | Block.Furnace | Block.FurnaceLit => 1.35f
    case Block.Diamond => 2.50f
    case Block.Bedrock => 9999f
    case _ => 0.5f

  private def damagePlayer(amount: Float, reason: String): Unit =
    if gameMode == GameMode.Survival && amount > 0f then
      val old = playerHealth
      playerHealth = (playerHealth - amount).max(0f)
      if old > playerHealth then
        timeSinceLastDamage = 0f
        val shown = (math.ceil(amount.toDouble * 2.0) / 2.0).toFloat
        addChatMessage(s"Took $shown damage${if reason.nonEmpty then s" from $reason" else ""}")
      if playerHealth <= 0f then
        val spawn = findSpawn()
        camera = spawn
        velocity = Vec3(0f, 0f, 0f)
        onGround = false
        fallPeakY = camera.y
        playerHealth = maxPlayerHealth
        playerFood = 20f
        timeSinceLastDamage = healthRegenDelay
        addChatMessage("You woke up at spawn")

  private def updateHealthRegen(dt: Float): Unit =
    if gameMode == GameMode.Survival && playerHealth > 0f && playerHealth < maxPlayerHealth then
      timeSinceLastDamage += dt
      if timeSinceLastDamage >= healthRegenDelay then playerHealth = (playerHealth + healthRegenRate * dt).min(maxPlayerHealth)
    else if playerHealth >= maxPlayerHealth then
      playerHealth = maxPlayerHealth
      timeSinceLastDamage = healthRegenDelay

  private def handleLandingImpact(): Unit =
    if gameMode == GameMode.Survival && waterSubmersion < 0.25f then
      val distance = (fallPeakY - camera.y).max(0f)
      if distance > 4.2f then
        damagePlayer((distance - 3.2f) * 0.85f, "falling")

  private def playerTouching(block: Block, extra: Float = 0.05f): Boolean =
    val half = 0.32f + extra
    val minX = floor(camera.x - half).toInt; val maxX = floor(camera.x + half).toInt
    val minY = floor(camera.y - 1.62f).toInt; val maxY = floor(camera.y + 0.20f).toInt
    val minZ = floor(camera.z - half).toInt; val maxZ = floor(camera.z + half).toInt
    (minX to maxX).exists(x => (minY to maxY).exists(y => (minZ to maxZ).exists(z => activeBlockAt(x, y, z) == block)))

  private def updateEnvironmentalDamage(dt: Float): Unit =
    if cactusDamageCooldown > 0f then cactusDamageCooldown = (cactusDamageCooldown - dt).max(0f)
    if gameMode == GameMode.Survival && cactusDamageCooldown <= 0f && playerTouching(Block.Cactus, 0.08f) then
      cactusDamageCooldown = 0.70f
      damagePlayer(1f, "cactus")

  private def movePlayer(dx: Float, dy: Float, dz: Float): Unit =
    val step = 0.35f
    if dx != 0f then
      val next = camera + Vec3(dx, 0f, 0f)
      if !collidesPlayer(next) then camera = next
      else
        val nextHigh = camera + Vec3(dx, step, 0f)
        if dy >= 0f && !collidesPlayer(nextHigh) then
          camera = nextHigh
          onGround = false
    if dz != 0f then
      val next = camera + Vec3(0f, 0f, dz)
      if !collidesPlayer(next) then camera = next
      else
        val nextHigh = camera + Vec3(0f, step, dz)
        if dy >= 0f && !collidesPlayer(nextHigh) then
          camera = nextHigh
          onGround = false
    if dy != 0f then
      val next = camera + Vec3(0f, dy, 0f)
      if !collidesPlayer(next) then
        camera = next
        if dy > 0f || velocity.y > 0f then fallPeakY = camera.y.max(fallPeakY)
        else if !onGround then fallPeakY = camera.y.max(fallPeakY)
        onGround = false
      else if dy < 0f then
        handleLandingImpact()
        onGround = true
        fallPeakY = camera.y
        velocity = Vec3(0f, 0f, 0f)
      else
        velocity = Vec3(0f, 0f, 0f)

  private def moveVertical(dy: Float): Unit =
    var remaining = dy
    while abs(remaining) > 0.001f do
      val step = remaining.max(-0.22f).min(0.22f)
      movePlayer(0f, step, 0f)
      if step < 0f && onGround then return
      if step > 0f && velocity.y == 0f then return
      remaining -= step

  private def collidesPlayer(pos: Vec3): Boolean =
    val half = 0.32f
    val minX = floor(pos.x - half).toInt; val maxX = floor(pos.x + half).toInt
    val minY = floor(pos.y - 1.62f).toInt; val maxY = floor(pos.y + 0.20f).toInt
    val minZ = floor(pos.z - half).toInt; val maxZ = floor(pos.z + half).toInt
    (minX to maxX).exists(x => (minY to maxY).exists(y => (minZ to maxZ).exists(z => activeBlockAt(x, y, z).solid)))

  private def hasGroundBelow(pos: Vec3): Boolean =
    val half = 0.30f; val checkY = floor(pos.y - 1.63f).toInt
    val minX = floor(pos.x - half).toInt; val maxX = floor(pos.x + half).toInt
    val minZ = floor(pos.z - half).toInt; val maxZ = floor(pos.z + half).toInt
    (minX to maxX).exists(x => (minZ to maxZ).exists(z => activeBlockAt(x, checkY, z).solid))

  private def toggleGameMode(): Unit =
    gameMode = if gameMode == GameMode.Survival then GameMode.Creative else GameMode.Survival
    if gameMode == GameMode.Survival then flyEnabled = false
    velocity = Vec3(0f, 0f, 0f); onGround = false

  private def toggleFullscreen(): Unit =
    fullscreen = !fullscreen
    if fullscreen then
      val mode = glfwGetVideoMode(glfwGetPrimaryMonitor())
      queryWindowPos()
      glfwSetWindowMonitor(window, glfwGetPrimaryMonitor(), 0, 0, mode.width, mode.height, mode.refreshRate)
    else
      glfwSetWindowMonitor(window, NULL, windowedX.max(0), windowedY.max(0), windowedW, windowedH, GLFW_DONT_CARE)

  private def down(key: Int): Boolean = glfwGetKey(window, key) == GLFW_PRESS

  private def blockAt(pos: Vec3): Block = activeBlockAt(floor(pos.x).toInt, floor(pos.y).toInt, floor(pos.z).toInt)

  private def isUnderwater: Boolean = blockAt(camera) == Block.Water

  private def waterSubmersion: Float =
    val probes = Array(camera, camera + Vec3(0f, -0.72f, 0f), camera + Vec3(0f, -1.45f, 0f))
    probes.count(p => blockAt(p) == Block.Water).toFloat / probes.length

  private def chunkCoordBlock(v: Int): Int = Math.floorDiv(v, Terrain.chunkSize)
  private def chunkCoordPos(v: Float): Int = floor(v / Terrain.chunkSize.toFloat).toInt

  private def activeBlockAt(x: Int, y: Int, z: Int): Block =
    if y < 0 || y >= Terrain.worldHeight then return Block.Air
    val cx = chunkCoordBlock(x)
    val cz = chunkCoordBlock(z)
    val lx = x - cx * Terrain.chunkSize
    val lz = z - cz * Terrain.chunkSize
    chunks.get((cx, cz)) match
      case Some(chunk) => chunk.getBlock(lx, y, lz)
      case None =>
        val key = (java.lang.Long.rotateLeft(worldSeed, 21) ^ (x.toLong << 32) ^ (z.toLong & 0xffffffffL))
        val h = terrainHeightCache.getOrElseUpdate(key, terrainGen.heightAt(x, z))
        if y > h then
          if y <= Terrain.seaLevel then Block.Water else Block.Air
        else if y == h then terrainGen.surfaceBlock(x, y, z, h)
        else terrainGen.fillBlock(x, y, z, h)

  private def setActiveBlock(x: Int, y: Int, z: Int, block: Block): Unit =
    if y < 0 || y >= Terrain.worldHeight then return
    val cx = chunkCoordBlock(x)
    val cz = chunkCoordBlock(z)
    val lx = x - cx * Terrain.chunkSize
    val lz = z - cz * Terrain.chunkSize
    chunks.get((cx, cz)).foreach { chunk =>
      val oldBlock = chunk.getBlock(lx, y, lz)
      chunk.setBlock(lx, y, lz, block)
      if block == Block.Water then
        setWaterLevelAt(x, y, z, 8)
        markWaterActive(x, y, z)
      else
        setWaterLevelAt(x, y, z, 0)
        waterLevels.remove((x, y, z))
      if oldBlock != block then wakeWaterAround(x, y, z)
    }

  private def getWorldBlock(x: Int, y: Int, z: Int): Block =
    if y < 0 || y >= Terrain.worldHeight then Block.Air
    else
      val cx = chunkCoordBlock(x)
      val cz = chunkCoordBlock(z)
      chunks.get((cx, cz)) match
        case Some(c) => c.getBlock(x - cx * Terrain.chunkSize, y, z - cz * Terrain.chunkSize)
        case None => Block.Air

  private def processNetworkMessages(): Unit =
    def handleNetworkLine(line: String): Unit =
      val parts = line.split("\\|", -1)
      try
        if line.startsWith("WORLD|") then
          if parts.length >= 5 && gameServer == null then
            val seed = parts(1).toLong
            val sx = parts(2).toFloat; val sy = parts(3).toFloat; val sz = parts(4).toFloat
            if parts.length >= 6 then
              localColorId = normalizeColorId(parts(5).toInt)
              rememberPlayerColor(playerName, localColorId)
            if parts.length >= 8 then
              val hostOnlineName = networkSafeName(parts(6))
              val hostColor = normalizeColorId(parts(7).toInt)
              knownPlayerNames += hostOnlineName
              rememberPlayerColor(hostOnlineName, hostColor)
            if seed != worldSeed then
              worldSeed = seed
              worldName = "Multiplayer-" + java.lang.Long.toUnsignedString(worldSeed, 36).toUpperCase
              terrainGen = TerrainGenerator(worldSeed)
              terrainHeightCache.clear()
              pendingNetworkBlocks.clear()
              networkBlockOverrides.clear()
              networkWaterLevelOverrides.clear()
              waterLevels.clear()
              waterFlowQueue.clear()
              waterFlowQueued.clear()
              clearLoadedChunks(saveFirst = false)
            camera = Vec3(sx, sy, sz)
            fallPeakY = camera.y
            velocity = Vec3(0f, 0f, 0f); onGround = false
            syncChunks()
            forceCameraChunkRing(0)
            processChunkWorkMainThread(0, 12)
            ensureNearbyMeshesReady(1)
            addChatMessage("Joined host world")
        else if line.startsWith("BLOC|") then
          if parts.length >= 5 then
            val x = parts(1).toInt; val y = parts(2).toInt; val z = parts(3).toInt
            val blockId = parts(4).toByte
            val block = Block.fromId(blockId)
            val waterLevel =
              if block == Block.Water && parts.length >= 6 then
                try parts(5).toInt.max(1).min(8) catch case _: Exception => 8
              else if block == Block.Water then 8 else 0
            networkBlockOverrides((x, y, z)) = block
            if block == Block.Water then networkWaterLevelOverrides((x, y, z)) = waterLevel.toByte
            else networkWaterLevelOverrides.remove((x, y, z))
            val cx = chunkCoordBlock(x); val cz = chunkCoordBlock(z)
            if chunks.contains((cx, cz)) then
              setActiveBlock(x, y, z, block)
              if block == Block.Water then setWaterLevelAt(x, y, z, waterLevel)
              dirtyChunkAt(x, z)
            else
              pendingNetworkBlocks((x, y, z)) = block
        else if line.startsWith("SNAPBEGIN|") || line.startsWith("SNAPEND") then
          ()
        else if line.startsWith("OPLIST|") then
          if gameServer == null then
            oppedPlayerNames.clear()
            parts.drop(1).foreach(n => setOppedName(n, value = true, announce = false))
        else if line.startsWith("OP|") then
          if parts.length >= 3 then
            val name = networkSafeName(parts(1))
            val value = parts(2) == "1" || parts(2).equalsIgnoreCase("true")
            setOppedName(name, value)
        else if line.startsWith("GMODE|") then
          if parts.length >= 3 then
            val name = networkSafeName(parts(1))
            if name.equalsIgnoreCase(playerName) then
              gameMode = GameMode.fromOrdinal(parts(2).toInt)
              velocity = Vec3(0f, 0f, 0f); onGround = false
              addChatMessage(s"Game mode set to $gameMode")
        else if line.startsWith("FLY|") then
          if parts.length >= 3 then
            val name = networkSafeName(parts(1))
            if name.equalsIgnoreCase(playerName) then toggleFlyForPlayer(playerName)
        else if line.startsWith("GIVE|") then
          if parts.length >= 4 then
            val name = networkSafeName(parts(1))
            if name.equalsIgnoreCase(playerName) then
              val block = Block.fromId(parts(2).toInt.toByte)
              val count = parts(3).toInt.max(1).min(999)
              gainItem(block, count)
              addChatMessage(s"Received $count ${blockName(block)}")
        else if line.startsWith("VITAL|") then
          if parts.length >= 4 then
            val name = networkSafeName(parts(1))
            if name.equalsIgnoreCase(playerName) then
              val hp = parts(2).toFloat
              val food = parts(3).toFloat
              if hp >= 0f then playerHealth = hp.max(0f).min(maxPlayerHealth)
              if food >= 0f then playerFood = food.max(0f).min(maxPlayerFood)
              val label = if parts.length >= 5 then networkUnescape(parts.drop(4).mkString("|")) else "Updated"
              addChatMessage(label + " " + playerName)
        else if line.startsWith("TIME|") then
          if parts.length >= 2 then setTimeCommand(parts(1).toLowerCase)
        else if line.startsWith("RCMD|") then
          if gameServer != null && parts.length >= 3 then
            val sender = networkSafeName(parts(1))
            val commandText = networkUnescape(parts.drop(2).mkString("|"))
            parseCommand(commandText, Some(sender))
        else if line.startsWith("POS|") then
          if parts.length >= 7 then
            val name = networkSafeName(parts(1))
            if name.nonEmpty then knownPlayerNames += name
            if name != playerName then
              val x = parts(2).toFloat; val y = parts(3).toFloat; val z = parts(4).toFloat
              val pyaw = parts(5).toFloat; val ppitch = parts(6).toFloat
              val colorId = if parts.length >= 8 then normalizeColorId(parts(7).toInt) else colorForPlayer(name)
              rememberPlayerColor(name, colorId)
              remotePlayers(name) = RemotePlayer(name, Vec3(x, y, z), pyaw, ppitch, glfwGetTime(), colorId)
        else if line.startsWith("TPOS|") then
          if parts.length >= 5 then
            val name = networkSafeName(parts(1))
            val x = parts(2).toFloat; val y = parts(3).toFloat; val z = parts(4).toFloat
            if name.equalsIgnoreCase(playerName) then
              camera = Vec3(x, y, z)
              fallPeakY = camera.y
              velocity = Vec3(0f, 0f, 0f)
              addChatMessage("Teleported")
            else
              remotePlayers.get(name).foreach { rp =>
                remotePlayers(name) = RemotePlayer(name, Vec3(x, y, z), rp.yaw, rp.pitch, glfwGetTime(), rp.colorId)
              }
        else if line.startsWith("CHAT|") then
          if parts.length >= 3 then
            val name = networkSafeName(parts(1))
            val text = networkUnescape(parts.drop(2).mkString("|")).take(220)
            if name != playerName then addChatMessage(s"<$name> $text")
        else if line.startsWith("PLAYERS|") then
          parts.drop(1).foreach { p =>
            parsePlayerToken(p).foreach { case (name, colorId) =>
              if name.nonEmpty then knownPlayerNames += name
              rememberPlayerColor(name, colorId)
            }
          }
        else if line.startsWith("JOIN|") then
          if parts.length >= 2 then
            val name = networkSafeName(parts(1))
            val colorId = if parts.length >= 3 then normalizeColorId(parts(2).toInt) else colorForPlayer(name)
            if name.nonEmpty then knownPlayerNames += name
            rememberPlayerColor(name, colorId)
            if name != playerName then addChatMessage(s"$name joined the game")
        else if line.startsWith("LEFT|") then
          if parts.length >= 2 then
            val name = networkSafeName(parts(1))
            knownPlayerNames -= name
            remotePlayers.remove(name)
            if name != playerName then addChatMessage(s"$name left the game")
      catch case _: Exception => ()

    if gameClient != null then
      var msg = gameClient.pollMessage()
      while msg.isDefined do
        handleNetworkLine(msg.get)
        msg = gameClient.pollMessage()

    if gameServer != null then
      var msg = gameServer.pollMessage()
      while msg.isDefined do
        handleNetworkLine(msg.get)
        msg = gameServer.pollMessage()

  private def sendPlayerPositionNetwork(): Unit =
    if multiplayerMode then
      val now = glfwGetTime()
      if now - lastPosSend >= 0.10 then
        lastPosSend = now
        val msg = "POS|" + networkEscape(playerName) + "|" + f"${camera.x}%.3f" + "|" + f"${camera.y}%.3f" + "|" + f"${camera.z}%.3f" + "|" + f"$yaw%.2f" + "|" + f"$pitch%.2f" + "|" + localColorId.toString
        if gameClient != null && gameClient.isConnected then gameClient.send(msg)
        else if gameServer != null then gameServer.broadcast(msg)

  private def cleanupRemotePlayers(): Unit =
    val now = glfwGetTime()
    val stale = remotePlayers.collect { case (name, rp) if now - rp.lastSeen > 8.0 => name }.toList
    stale.foreach { name =>
      remotePlayers.remove(name)
      knownPlayerNames -= name
      if multiplayerMode && name != playerName then addChatMessage(s"$name left the game")
    }

  private def render(): Unit =
    updateFramebufferSize()
    // Hard reset the render state every frame. Immediate-mode OpenGL state is global;
    // one debug/wireframe/menu/name-tag pass leaking state can make the next screen look
    // like a dark blue void or shrink/warp the UI. Reset first, then each pass opts in.
    resetGlArraysAndBuffers()
    glPolygonMode(GL_FRONT_AND_BACK, GL_FILL)
    glDisable(GL_FOG)
    glDisable(GL_TEXTURE_2D)
    glDisable(GL_CULL_FACE)
    glDepthMask(true)
    glEnable(GL_BLEND)
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA)
    val (clearR, clearG, clearB) = clearColor
    glClearColor(clearR, clearG, clearB, 1f)
    glViewport(0, 0, framebufferWidth, framebufferHeight)
    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT)
    screen match
      case Screen.Playing =>
        renderWorld()
        renderPlayerNameTags()
        if isUnderwater then renderUnderwaterOverlay()
        renderHud()
        renderHotbar()
        renderCrosshair()
        renderChat()
        modManager.fireHudRender(lastFrameTime.toFloat)
        modLeftClickedThisFrame = false
        if modUiCursorMode then
          glDisable(GL_DEPTH_TEST); glDisable(GL_CULL_FACE); setupOrtho()
          renderTextShadow(18f * uiScale, framebufferHeight - 32f * uiScale, "F8: close mod cursor", 0.82f, 0.92f, 1f, (0.78f * uiScale).max(0.68f))
        if debugMode then renderDebugOverlay()
      case Screen.MainMenu => renderMainMenu()
      case Screen.CreateWorld => renderCreateWorld()
      case Screen.LoadWorld => renderLoadWorldMenu()
      case Screen.Mods => renderModsScreen()
      case Screen.Settings => renderSettings()
      case Screen.Paused =>
        renderWorld()
        renderPlayerNameTags()
        if multiplayerMode then
          renderHud()
          renderChat()
        renderPauseMenu()
      case Screen.Inventory => renderWorld(); renderSurvivalInventory()
      case Screen.Catalog => renderWorld(); renderCatalog()
      case Screen.FurnaceUI => renderWorld(); renderFurnaceUI()
      case Screen.JoinGame => renderJoinGame()
      case Screen.HostGame => renderHostGame()

  private def renderWorld(): Unit =
    if !centerChunkReady then
      forceCameraChunkRing(0)
      processChunkWorkMainThread(0, 8)
    resetGlArraysAndBuffers()
    glPolygonMode(GL_FRONT_AND_BACK, if wireframeMode then GL_LINE else GL_FILL)
    glDepthMask(true)
    glEnable(GL_DEPTH_TEST)
    glDisable(GL_CULL_FACE)
    glEnable(GL_FOG)
    val (fogR, fogG, fogB) = clearColor
    val isUnder = isUnderwater
    if isUnder then
      glFogi(GL_FOG_MODE, GL_LINEAR); glFogf(GL_FOG_START, 2f); glFogf(GL_FOG_END, 18f)
    else
      val visibleDist = renderDistanceBlocks.toFloat
      glFogi(GL_FOG_MODE, GL_LINEAR)
      glFogf(GL_FOG_START, visibleDist * 0.65f)
      glFogf(GL_FOG_END, visibleDist * 0.95f)
    glFogfv(GL_FOG_COLOR, floatBuffer(fogR, fogG, fogB, 1f))
    glMatrixMode(GL_PROJECTION); glLoadIdentity()
    val aspect = framebufferWidth.toDouble / framebufferHeight.toDouble
    // Keep the near plane away from zero so the depth buffer has useful
    // precision. Tiny near values caused early z-fighting at distance.
    val near = 0.25; val far = (renderDistanceBlocks + Terrain.chunkSize * 2).toDouble
    val sprintFov = if screen == Screen.Playing then
      val moving = down(GLFW_KEY_W) || down(GLFW_KEY_S) || down(GLFW_KEY_A) || down(GLFW_KEY_D)
      if moving && (down(GLFW_KEY_LEFT_CONTROL) || down(GLFW_KEY_RIGHT_CONTROL)) then 8.5f else 0f
    else 0f
    val top = tan(toRadians(fov + sprintFov) / 2.0) * near
    glFrustum(-top * aspect, top * aspect, -top, top, near, far)
    glMatrixMode(GL_MODELVIEW); glLoadIdentity()
    glRotatef(pitch, 1f, 0f, 0f)
    glRotatef(yaw, 0f, 1f, 0f)
    // X/Z camera-relative rendering: chunk VBOs are local and translated by
    // (chunkBase - camera). Only vertical camera motion remains in the view matrix.
    glTranslatef(0f, -camera.y, 0f)
    if !isUnder then renderSky()
    renderBubbles()
    renderFallingSand()
    restoreChunkRenderState(isUnder, fogR, fogG, fogB)
    val chunkList = chunks.values.toBuffer
    // Batch opaque pass - bind texture + client states once for all chunks
    glDisable(GL_BLEND)
    glDepthMask(true)
    glEnable(GL_DEPTH_TEST)
    glEnable(GL_TEXTURE_2D); activeAtlas.bind()
    glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_MODULATE)
    glEnableClientState(GL_VERTEX_ARRAY); glEnableClientState(GL_COLOR_ARRAY); glEnableClientState(GL_TEXTURE_COORD_ARRAY)
    chunkList.foreach(_.drawOpaque(camera))
    // Batch cutout pass
    glEnable(GL_ALPHA_TEST); glAlphaFunc(GL_GREATER, 0.5f)
    chunkList.foreach(_.drawCutout(camera))
    glDisable(GL_ALPHA_TEST)
    // Batch translucent + water pass
    glEnable(GL_BLEND)
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA)
    glDepthMask(false)
    val farToNearChunks = chunkList.sortBy { c =>
      val dx = (c.cx * Terrain.chunkSize + Terrain.chunkSize * 0.5f) - camera.x
      val dz = (c.cz * Terrain.chunkSize + Terrain.chunkSize * 0.5f) - camera.z
      -(dx * dx + dz * dz)
    }
    farToNearChunks.foreach(_.drawTranslucent(camera))
    // Water should read as a solid block volume. Keep the same texture/color, but draw
    // it depth-writing and unblended so stacked water cannot show internal faces.
    glDisable(GL_BLEND)
    glDepthMask(true)
    chunkList.foreach(_.drawWater(camera))
    glDepthMask(true)
    glDisableClientState(GL_TEXTURE_COORD_ARRAY); glDisableClientState(GL_COLOR_ARRAY); glDisableClientState(GL_VERTEX_ARRAY)
    glBindBuffer(GL_ARRAY_BUFFER, 0); glBindTexture(GL_TEXTURE_2D, 0)
    renderRemotePlayers3D()
    renderTargetOutline()
    if debugMode then renderRayLine()
    if showChunkBorders then renderChunkBorders()
    glPolygonMode(GL_FRONT_AND_BACK, GL_FILL)
    val darkness = nightDarkness
    if darkness > 0.01f then
      setupOrtho()
      glDisable(GL_DEPTH_TEST)
      glDisable(GL_FOG)
      rect(0, 0, framebufferWidth, framebufferHeight, 0.02f, 0.04f, 0.12f, darkness)
    glPolygonMode(GL_FRONT_AND_BACK, GL_FILL)
    glDepthMask(true)
    glDisable(GL_FOG)

  private def restoreChunkRenderState(isUnder: Boolean, fogR: Float, fogG: Float, fogB: Float): Unit =
    // renderSky(), particles, name tags, and UI all mutate global OpenGL state.
    // The world chunk pass must explicitly restore depth writes/testing or random translucent
    // quads / water / player boxes can draw as big black sheets and terrain can appear to vanish.
    resetGlArraysAndBuffers()
    glPolygonMode(GL_FRONT_AND_BACK, if wireframeMode then GL_LINE else GL_FILL)
    glViewport(0, 0, framebufferWidth, framebufferHeight)
    glMatrixMode(GL_PROJECTION)
    // projection/modelview are already set by renderWorld; just restore render state here.
    glMatrixMode(GL_MODELVIEW)
    glEnable(GL_DEPTH_TEST)
    glDepthMask(true)
    glDepthFunc(GL_LEQUAL)
    glDisable(GL_CULL_FACE)
    glDisable(GL_ALPHA_TEST)
    glDisable(GL_BLEND)
    glEnable(GL_FOG)
    if isUnder then
      glFogi(GL_FOG_MODE, GL_LINEAR); glFogf(GL_FOG_START, 2f); glFogf(GL_FOG_END, 18f)
    else
      val visibleDist = renderDistanceBlocks.toFloat
      glFogi(GL_FOG_MODE, GL_LINEAR)
      glFogf(GL_FOG_START, visibleDist * 0.65f)
      glFogf(GL_FOG_END, visibleDist * 0.95f)
    glFogfv(GL_FOG_COLOR, floatBuffer(fogR, fogG, fogB, 1f))
    glColor4f(1f, 1f, 1f, 1f)

  private def renderRemotePlayers3D(): Unit =
    if remotePlayers.isEmpty then return
    resetGlArraysAndBuffers()
    val now = glfwGetTime()
    glDisable(GL_TEXTURE_2D)
    glDisable(GL_FOG)
    glDisable(GL_BLEND)
    glDepthMask(true)
    glEnable(GL_DEPTH_TEST)
    for (_, rp) <- remotePlayers do
      if now - rp.lastSeen <= 8.0 then
        val p = rp.pos
        val feetY = p.y - 1.62f
        val (cr, cg, cb) = colorForId(rp.colorId)
        glPushMatrix()
        glTranslatef(p.x - camera.x, 0f, p.z - camera.z)
        glRotatef(-rp.yaw, 0f, 1f, 0f)
        drawColoredBox(-0.30f, feetY, -0.20f, 0.30f, feetY + 1.05f, 0.20f, cr, cg, cb, 1f)
        drawColoredBox(-0.23f, feetY + 1.05f, -0.23f, 0.23f, feetY + 1.48f, 0.23f, 0.90f, 0.74f, 0.55f, 1f)
        drawColoredBox(-0.18f, feetY - 0.02f, -0.18f, -0.02f, feetY + 0.62f, 0.18f, cr * 0.72f, cg * 0.72f, cb * 0.72f, 1f)
        drawColoredBox(0.02f, feetY - 0.02f, -0.18f, 0.18f, feetY + 0.62f, 0.18f, cr * 0.72f, cg * 0.72f, cb * 0.72f, 1f)
        glPopMatrix()
    glEnable(GL_FOG)

  private def drawColoredBox(x0: Float, y0: Float, z0: Float, x1: Float, y1: Float, z1: Float, r: Float, g: Float, b: Float, a: Float): Unit =
    glColor4f(r, g, b, a)
    glBegin(GL_QUADS)
    glVertex3f(x0, y0, z1); glVertex3f(x1, y0, z1); glVertex3f(x1, y1, z1); glVertex3f(x0, y1, z1)
    glVertex3f(x1, y0, z0); glVertex3f(x0, y0, z0); glVertex3f(x0, y1, z0); glVertex3f(x1, y1, z0)
    glVertex3f(x0, y0, z0); glVertex3f(x0, y0, z1); glVertex3f(x0, y1, z1); glVertex3f(x0, y1, z0)
    glVertex3f(x1, y0, z1); glVertex3f(x1, y0, z0); glVertex3f(x1, y1, z0); glVertex3f(x1, y1, z1)
    glVertex3f(x0, y1, z1); glVertex3f(x1, y1, z1); glVertex3f(x1, y1, z0); glVertex3f(x0, y1, z0)
    glVertex3f(x0, y0, z0); glVertex3f(x1, y0, z0); glVertex3f(x1, y0, z1); glVertex3f(x0, y0, z1)
    glEnd()

  private def projectWorldToScreen(pos: Vec3): Option[(Float, Float, Float)] =
    val rel = pos - camera
    val yr = toRadians(yaw).toFloat
    val pr = toRadians(pitch).toFloat
    val cy = cos(yr).toFloat; val sy = sin(yr).toFloat
    val cp = cos(pr).toFloat; val sp = sin(pr).toFloat
    val x1 = cy * rel.x + sy * rel.z
    val z1 = -sy * rel.x + cy * rel.z
    val y2 = cp * rel.y - sp * z1
    val z2 = sp * rel.y + cp * z1
    if z2 >= -0.12f then return None
    val aspect = framebufferWidth.toFloat / framebufferHeight.toFloat.max(1f)
    val tanHalf = tan(toRadians(fov) / 2.0).toFloat
    val ndcX = (x1 / -z2) / (tanHalf * aspect)
    val ndcY = (y2 / -z2) / tanHalf
    if ndcX < -1.35f || ndcX > 1.35f || ndcY < -1.35f || ndcY > 1.35f then None
    else Some(((ndcX * 0.5f + 0.5f) * framebufferWidth, (0.5f - ndcY * 0.5f) * framebufferHeight, -z2))

  private def renderPlayerNameTags(): Unit =
    if remotePlayers.isEmpty then return
    glDisable(GL_DEPTH_TEST); glDisable(GL_CULL_FACE); setupOrtho()
    val now = glfwGetTime()
    for (_, rp) <- remotePlayers.toSeq.sortBy { case (_, rp) => (rp.pos - camera).lengthSquared }.reverse do
      if now - rp.lastSeen <= 8.0 then
        projectWorldToScreen(rp.pos + Vec3(0f, 0.72f, 0f)).foreach { case (sx, sy, dist) =>
          val label = rp.name
          val scale = (1.22f - dist * 0.008f).max(0.86f).min(1.18f) * uiScale.max(0.92f)
          val tw = textWidth(label, scale)
          val boxW = tw + 12f * uiScale
          val boxH = 18f * scale + 7f * uiScale
          val x = (sx - boxW / 2f).max(4f).min(framebufferWidth.toFloat - boxW - 4f)
          val y = (sy - boxH / 2f).max(4f).min(framebufferHeight.toFloat - boxH - 4f)
          val (cr, cg, cb) = colorForId(rp.colorId)
          rect(x + 2f, y + 2f, boxW, boxH, 0f, 0f, 0f, 0.36f)
          rect(x, y, boxW, boxH, cr * 0.10f, cg * 0.10f, cb * 0.10f, 0.78f)
          centeredTextFit(x + boxW / 2f, y + 4f * uiScale, label, 1f, 0.95f, 0.64f, scale, boxW - 8f * uiScale)
        }
    glEnable(GL_DEPTH_TEST)

  private def renderHud(): Unit =
    glDisable(GL_DEPTH_TEST); glDisable(GL_CULL_FACE); setupOrtho()
    val s = uiScale
    val hudText = s"Blockbox | ${gameMode} | ESC pause | F2 settings"
    val hudScale = (1.10f * s).max(1.02f)
    // Intentionally no translucent panel here: it was the source of an intermittent stray world-space sheet artifact.
    renderTextShadow(18f * s, 16f * s, hudText, 1f, 1f, 1f, hudScale)
    if gameMode == GameMode.Survival then renderHealth()
    renderBlockInfo()
    if down(GLFW_KEY_TAB) then renderPlayerListOverlay()

  private def renderPlayerListOverlay(): Unit =
    if !multiplayerMode && remotePlayers.isEmpty then return
    glDisable(GL_DEPTH_TEST); glDisable(GL_CULL_FACE); setupOrtho()
    val s = uiScale
    val names = (Seq(playerName) ++ knownPlayerNames.toSeq ++ remotePlayers.keys.toSeq)
      .map(networkSafeName)
      .filter(_.nonEmpty)
      .distinct
      .sortBy(n => if n == playerName then "" else n.toLowerCase)
    val rowH = (30f * s).max(24f)
    val panelW = (300f * s).max(240f).min(framebufferWidth.toFloat - 32f)
    val panelH = (52f * s + rowH * names.length).max(84f).min(framebufferHeight.toFloat - 32f)
    val x = framebufferWidth.toFloat / 2f - panelW / 2f
    val y = 44f * s
    rect(x + 4f * s, y + 4f * s, panelW, panelH, 0f, 0f, 0f, 0.32f)
    rect(x, y, panelW, panelH, 0.045f, 0.055f, 0.082f, 0.88f)
    rect(x, y, panelW, 3f * s, 0.95f, 0.78f, 0.26f, 0.75f)
    centeredTextFit(x + panelW / 2f, y + 14f * s, s"Players (${names.length})", 1f, 0.92f, 0.58f, (1.10f * s).max(0.96f), panelW - 24f * s)
    var yy = y + 40f * s
    for name <- names.take(((panelH - 48f * s) / rowH).toInt.max(1)) do
      val isSelf = name == playerName
      rect(x + 12f * s, yy - 3f * s, panelW - 24f * s, rowH - 2f * s, if isSelf then 0.16f else 0.08f, if isSelf then 0.13f else 0.09f, if isSelf then 0.06f else 0.12f, if isSelf then 0.55f else 0.38f)
      renderTextShadow(x + 22f * s, yy, if isSelf then s"$name (You)" else name, if isSelf then 1f else 0.86f, if isSelf then 0.88f else 0.92f, if isSelf then 0.55f else 1f, (1.02f * s).max(0.90f))
      yy += rowH
    glEnable(GL_DEPTH_TEST)

  private def drawHeartIcon(x: Float, y: Float, size: Float, fillFrac: Float, bgAlpha: Float): Unit =
    val s = size.max(10f)
    val fill = fillFrac.max(0f).min(1f)
    rect(x - 1f, y - 1f, s + 2f, s + 2f, 0f, 0f, 0f, 0.30f)
    rect(x, y, s, s, 0.10f, 0.02f, 0.02f, bgAlpha)
    val cols = 7
    val rows = 6
    val cell = (s / cols.toFloat).max(1f)
    val heart = Array(
      Array(0, 1, 1, 0, 1, 1, 0),
      Array(1, 1, 1, 1, 1, 1, 1),
      Array(1, 1, 1, 1, 1, 1, 1),
      Array(0, 1, 1, 1, 1, 1, 0),
      Array(0, 0, 1, 1, 1, 0, 0),
      Array(0, 0, 0, 1, 0, 0, 0)
    )
    val fillW = s * fill
    var ry = 0
    while ry < rows do
      var rx = 0
      while rx < cols do
        if heart(ry)(rx) == 1 then
          val px = x + rx * cell
          val py = y + ry * cell
          val pw = (cell + 0.55f).min(s - rx * cell)
          val ph = (cell + 0.55f).min(s - ry * cell)
          val filled = px + pw <= x + fillW + 0.01f
          val partial = !filled && fill > 0f && px < x + fillW
          if filled then
            rect(px, py, pw, ph, 0.92f, 0.08f, 0.10f, 0.96f)
            if ry <= 1 then rect(px, py, pw, ph * 0.38f, 1f, 0.45f, 0.48f, 0.22f)
          else if partial then
            val visible = (x + fillW - px).max(0f).min(pw)
            rect(px, py, pw, ph, 0.18f, 0.03f, 0.03f, bgAlpha + 0.08f)
            rect(px, py, visible, ph, 0.92f, 0.08f, 0.10f, 0.96f)
          else
            rect(px, py, pw, ph, 0.18f, 0.03f, 0.03f, bgAlpha + 0.08f)
        rx += 1
      ry += 1

  private def renderHealth(): Unit =
    glDisable(GL_DEPTH_TEST); glDisable(GL_CULL_FACE); setupOrtho()
    val s = uiScale
    val unit = (18f * s).max(14f).min(22f)
    val gap = (unit + 4f * s).max(unit + 3f).min(unit + 7f)
    val totalWidth = 10f * gap - (gap - unit)
    val startX = framebufferWidth / 2f - totalWidth / 2f
    val hotbarSlot = (46f * s).min(((framebufferWidth.toFloat - 36f * s) / 10f - 5f * s).max(24f)).max(30f)
    val barY = framebufferHeight.toFloat - hotbarSlot - 42f * s
    val bgAlpha = 0.46f
    for i <- 0 until 10 do
      val x = startX + i * gap
      val threshold = i * 2 + 2
      val fill =
        if playerHealth >= threshold then 1f
        else if playerHealth == threshold - 1 then 0.5f
        else 0f
      drawHeartIcon(x, barY, unit, fill, bgAlpha)

  private def renderBlockInfo(): Unit =
    if framebufferHeight < 200 then return
    raycast(8f).foreach { hit =>
      val block = activeBlockAt(hit.block._1, hit.block._2, hit.block._3)
      val by = framebufferHeight.toFloat
      val hp = hit.block; val waterLvl = if block == Block.Water then waterLevelAt(hp._1, hp._2, hp._3) else -1
      val blockLabel = if waterLvl >= 0 then s"> ${block} (level $waterLvl/8)" else s"> ${block}"
      renderTextShadow(18, by - 90, blockLabel, 0.85f, 0.92f, 1f, 0.80f)
      renderTextShadow(18, by - 110, s"${hit.block._1}, ${hit.block._2}, ${hit.block._3}", 0.70f, 0.80f, 0.90f, 0.65f)
      if breakingProgress > 0f then
        rect(18, by - 132, 112, 8, 0.02f, 0.02f, 0.02f, 0.70f)
        rect(20, by - 130, 108 * breakingProgress.max(0f).min(1f), 4, 0.85f, 0.85f, 0.85f, 0.85f)
    }

  private var frameTimeAccum = 0.0
  private var frameCount = 0
  private var fpsDisplay = 0

  private def renderDebugOverlay(): Unit =
    glDisable(GL_DEPTH_TEST); glDisable(GL_CULL_FACE); setupOrtho()
    frameCount += 1; frameTimeAccum += lastFrameTime
    if frameTimeAccum >= 1.0 then
      fpsDisplay = (frameCount / frameTimeAccum).round.toInt
      frameCount = 0; frameTimeAccum = 0.0
    val ccx = chunkCoordPos(camera.x)
    val ccz = chunkCoordPos(camera.z)
    val fwd = viewDirection
    var ly = 20; val lx = 18
    val bg = 0.05f; val a = 0.65f
    rect(lx - 6, ly - 4, 340, 180 + 22 * 3, bg, bg, bg, a)
    renderTextShadow(lx, ly, s"F3: Debug  [F4:wire ${onOff(wireframeMode)}] [F5:borders ${onOff(showChunkBorders)}]", 0.6f, 0.8f, 1f, 0.88f * uiScale); ly += (20 * uiScale).toInt.max(18)
    renderTextShadow(lx, ly, s"FPS: $fpsDisplay  Frame: ${(lastFrameTime * 1000).toInt}ms  Chunks: ${chunks.size}", 0.4f, 1f, 0.4f, 0.88f * uiScale); ly += (19 * uiScale).toInt.max(16)
    renderTextShadow(lx, ly, s"XYZ: ${camera.x}%.3f / ${camera.y}%.3f / ${camera.z}%.3f", 0.4f, 1f, 0.4f, 0.88f * uiScale); ly += (19 * uiScale).toInt.max(16)
    renderTextShadow(lx, ly, s"Yaw: $yaw%.2f  Pitch: $pitch%.2f", 0.4f, 1f, 0.4f, 0.88f * uiScale); ly += (19 * uiScale).toInt.max(16)
    renderTextShadow(lx, ly, s"Chunk: ($ccx, $ccz)  Seed: $worldSeed  RD: ${renderDistance}ch/${renderDistanceBlocks}b", 0.4f, 1f, 0.4f, 0.88f * uiScale); ly += (19 * uiScale).toInt.max(16)
    renderTextShadow(lx, ly, s"World: $worldName", 0.4f, 1f, 0.4f, 0.72f * uiScale); ly += (17 * uiScale).toInt.max(14)
    renderTextShadow(lx, ly, s"Forward: (${fwd.x}%.4f, ${fwd.y}%.4f, ${fwd.z}%.4f)", 0.4f, 1f, 0.4f, 0.88f * uiScale); ly += (19 * uiScale).toInt.max(16)
    raycast(8f).foreach { hit =>
      ly += 4; rect(lx - 6, ly - 4, 340, 62, bg, bg, bg, a)
      val block = activeBlockAt(hit.block._1, hit.block._2, hit.block._3)
      renderTextShadow(lx, ly, s"Hit: (${hit.block._1}, ${hit.block._2}, ${hit.block._3}) $block", 1f, 0.8f, 0.3f, 0.88f * uiScale); ly += (19 * uiScale).toInt.max(16)
      renderTextShadow(lx, ly, s"Normal: (${hit.normal._1}, ${hit.normal._2}, ${hit.normal._3})  D: ${hit.distance}%.3f", 1f, 0.8f, 0.3f, 0.88f * uiScale); ly += (19 * uiScale).toInt.max(16)
      renderTextShadow(lx, ly, s"Place: (${hit.place._1}, ${hit.place._2}, ${hit.place._3})", 1f, 0.8f, 0.3f, 0.88f * uiScale); ly += (19 * uiScale).toInt.max(16)
    }
    glEnable(GL_DEPTH_TEST)

  private def renderHotbar(): Unit =
    compactHotbarAssignments()
    glDisable(GL_DEPTH_TEST); glDisable(GL_CULL_FACE); setupOrtho()
    val s = uiScale; val total = hotbarBlocks.length
    val maxSlotByWidth = ((framebufferWidth.toFloat - 36f * s) / total.toFloat - 5f * s).max(24f)
    val slotSize = (52f * s).min(maxSlotByWidth).max(34f)
    val gap = (5f * s).min(8f).max(3f)
    val barW = slotSize * total + gap * (total - 1)
    val startX = ((framebufferWidth.toFloat - barW) / 2f).max(8f)
    val y = (framebufferHeight.toFloat - slotSize - 12f * s).max(8f)
    val labelBandH = (16f * s).max(13f).min(slotSize * 0.34f)
    rect(startX - 8f * s, y - 8f * s, barW + 16f * s, slotSize + 16f * s, 0.01f, 0.015f, 0.025f, 0.34f)
    for i <- 0 until total do
      val sx = startX + i * (slotSize + gap)
      val selected = i == selectedBlock
      val block = hotbarBlocks(i)
      val hasItem = block != Block.Air && (gameMode == GameMode.Creative || hotbarCounts(i) > 0)
      rect(sx - 2f * s, y - 2f * s, slotSize + 4f * s, slotSize + 4f * s, 0f, 0f, 0f, 0.45f)
      rect(sx, y, slotSize, slotSize, if selected then 0.16f else 0.07f, if selected then 0.17f else 0.075f, if selected then 0.20f else 0.09f, if selected then 0.96f else 0.72f)
      rect(sx + 1f * s, y + 1f * s, slotSize - 2f * s, labelBandH, 0.28f, 0.30f, 0.35f, if selected then 0.42f else 0.20f)
      rect(sx + 1f * s, y + 1f * s + labelBandH, slotSize - 2f * s, 1f * s, 0f, 0f, 0f, 0.22f)
      if selected then
        rect(sx - 3f * s, y - 3f * s, slotSize + 6f * s, 2f * s, 1f, 0.92f, 0.45f, 0.75f)
        rect(sx - 3f * s, y + slotSize + 1f * s, slotSize + 6f * s, 2f * s, 1f, 0.92f, 0.45f, 0.75f)
        rect(sx - 3f * s, y - 3f * s, 2f * s, slotSize + 6f * s, 1f, 0.92f, 0.45f, 0.75f)
        rect(sx + slotSize + 1f * s, y - 3f * s, 2f * s, slotSize + 6f * s, 1f, 0.92f, 0.45f, 0.75f)
      if hasItem then
        val icon = (slotSize * 0.50f).min(24f * s).max(16f)
        val iconY = y + labelBandH + ((slotSize - labelBandH) - icon) / 2f + 2f * s
        renderBlockIcon(block, sx + (slotSize - icon) / 2f, iconY, icon)
        if gameMode == GameMode.Survival then
          val count = hotbarCounts(i)
          if count > 0 then
            val ns = count.toString
            val countScale = (1.12f * s).max(0.94f).min(slotSize / 16f)
            val tw = textWidth(ns, countScale)
            val nx = sx + slotSize - tw - 5f * s
            val ny = y + slotSize - 12f * countScale - 2f * s
            rect(nx - 2f * s, ny - 1f * s, tw + 4f * s, 11f * countScale + 3f * s, 0f, 0f, 0f, 0.64f)
            renderTextShadow(nx, ny, ns, 1f, 1f, 1f, countScale)
      val label = hotbarLabel(i)
      centeredTextBox(sx + 1f * s, y + 1f * s, slotSize - 2f * s, labelBandH, label, 0.97f, 0.97f, 0.99f, (1.14f * s).max(0.96f).min(slotSize / 12.8f))

  private def hotbarLabel(index: Int): String = if index < 9 then (index + 1).toString else if index == 9 then "0" else (index - 9).toString

  private def renderBlockIcon(block: Block, x: Float, y: Float, size: Float): Unit =
    glEnable(GL_TEXTURE_2D); activeAtlas.bind(); glColor4f(1f, 1f, 1f, 1f)
    val (u0, v0) = activeAtlas.uv(block, FaceKind.Top, 0f, 0f)
    val (u1, v1) = activeAtlas.uv(block, FaceKind.Top, 1f, 1f)
    glBegin(GL_QUADS)
    glTexCoord2f(u0, v0); glVertex2f(x, y)
    glTexCoord2f(u1, v0); glVertex2f(x + size, y)
    glTexCoord2f(u1, v1); glVertex2f(x + size, y + size)
    glTexCoord2f(u0, v1); glVertex2f(x, y + size)
    glEnd()
    glBindTexture(GL_TEXTURE_2D, 0); glDisable(GL_TEXTURE_2D)

  private def renderCrosshair(): Unit =
    glDisable(GL_DEPTH_TEST); glDisable(GL_CULL_FACE); setupOrtho()
    val cx = framebufferWidth / 2f; val cy = framebufferHeight / 2f
    val s = ((framebufferHeight / 720f).max(0.75f)).min(2f)
    val gap = (4f * s).toInt.toFloat; val len = (8f * s).toInt.toFloat
    glLineWidth((1.5f * s).max(1f))
    glColor4f(0f, 0f, 0f, 0.55f)
    glBegin(GL_LINES)
    glVertex2f(cx - len - gap, cy); glVertex2f(cx - gap, cy)
    glVertex2f(cx + gap, cy); glVertex2f(cx + len + gap, cy)
    glVertex2f(cx, cy - len - gap); glVertex2f(cx, cy - gap)
    glVertex2f(cx, cy + gap); glVertex2f(cx, cy + len + gap)
    glEnd()
    glColor4f(1f, 1f, 1f, 0.80f)
    glBegin(GL_LINES)
    glVertex2f(cx - len - gap + 1f, cy); glVertex2f(cx - gap + 1f, cy)
    glVertex2f(cx + gap - 1f, cy); glVertex2f(cx + len + gap - 1f, cy)
    glVertex2f(cx, cy - len - gap + 1f); glVertex2f(cx, cy - gap + 1f)
    glVertex2f(cx, cy + gap - 1f); glVertex2f(cx, cy + len + gap - 1f)
    glEnd()
    glColor4f(1f, 1f, 1f, 0.30f)
    val dot = (1f * s).max(0.5f)
    glBegin(GL_QUADS)
    glVertex2f(cx - dot, cy - dot); glVertex2f(cx + dot, cy - dot)
    glVertex2f(cx + dot, cy + dot); glVertex2f(cx - dot, cy + dot)
    glEnd()
    glLineWidth(1f)

  private def renderUnderwaterOverlay(): Unit =
    glDisable(GL_DEPTH_TEST); glDisable(GL_CULL_FACE); setupOrtho()
    val t = glfwGetTime().toFloat
    val wave = (sin(t * 1.5f).toFloat * 0.015f + 0.20f).max(0.12f).min(0.32f)
    rect(0, 0, framebufferWidth, framebufferHeight, 0.01f, 0.12f + wave * 0.3f, 0.30f + wave * 0.3f, 0.24f)
    rect(0, 0, framebufferWidth, framebufferHeight * 0.15f, 0.01f, 0.15f, 0.35f, 0.10f)
    rect(0, framebufferHeight * 0.85f, framebufferWidth, framebufferHeight * 0.15f, 0.01f, 0.15f, 0.35f, 0.10f)
    val vignette = 0.20f
    rect(0, 0, framebufferWidth * 0.08f, framebufferHeight, 0f, 0.08f, 0.20f, vignette)
    rect(framebufferWidth * 0.92f, 0, framebufferWidth * 0.08f, framebufferHeight, 0f, 0.08f, 0.20f, vignette)

  private def clearColor: (Float, Float, Float) =
    if isUnderwater then (0.02f, 0.14f, 0.30f)
    else
      val dp = dayPhase
      val daylight = smooth01(daylightFactor)
      val sunrise = smooth01(max(0f, 1f - abs(dp - 0.78f) * 12f))
      val sunset = smooth01(max(0f, 1f - abs(dp - 0.28f) * 12f))
      val rWarm = if sunrise > 0f || sunset > 0f then 0.25f * (sunrise + sunset) else 0f
      val r = (0.12f + 0.50f * daylight + rWarm).max(0.04f).min(0.95f)
      val g = (0.22f + 0.60f * daylight).max(0.04f).min(0.95f)
      val b = (0.34f + 0.65f * daylight).max(0.06f).min(0.98f)
      (r, g, b)

  private val dayLengthSeconds = 10f * 60f
  private val nightLengthSeconds = 8f * 60f
  private val dayNightCycleSeconds = dayLengthSeconds + nightLengthSeconds

  private def gameTime: Float = timeOverride.getOrElse(glfwGetTime().toFloat)

  private def cycleClock: Float =
    val raw = gameTime % dayNightCycleSeconds
    if raw < 0f then raw + dayNightCycleSeconds else raw

  private def dayPhase: Float =
    val t = cycleClock
    if t < dayLengthSeconds then (t / dayLengthSeconds) * 0.5f
    else 0.5f + ((t - dayLengthSeconds) / nightLengthSeconds) * 0.5f

  private def daylightFactor: Float =
    val sun = sin(dayPhase * Pi.toFloat * 2f).toFloat
    val eased = smooth01((sun + 0.18f) / 0.58f)
    (0.18f + 0.82f * eased).max(0.18f).min(1f)

  private def smooth01(v: Float): Float =
    val t = v.max(0f).min(1f); t * t * (3f - 2f * t)

  private def nightDarkness: Float =
    // Multiplayer windows joining/leaving should never create a temporary dark
    // screen while their terrain is warming up. Keep the world readable online;
    // singleplayer still has the old day/night dimming.
    if multiplayerMode then 0f
    else
      val d = daylightFactor
      val night = (1f - (d - 0.18f) / 0.82f) * 0.55f
      night.max(0f).min(0.55f)

  private def generateStars(): Unit =
    if starPositions != null then return
    val rng = new scala.util.Random(42L)
    starPositions = Array.tabulate(600) { _ =>
      val theta = rng.nextFloat() * Pi.toFloat * 2f
      val phi = acos((rng.nextFloat() * 2f - 1f).toDouble).toFloat
      val radius = 75f
      val x = (radius * sin(phi) * cos(theta)).toFloat
      val y = radius * cos(phi).toFloat
      val z = (radius * sin(phi) * sin(theta)).toFloat
      val brightness = 0.35f + rng.nextFloat() * 0.65f
      (x, y, z, brightness)
    }

  private def renderSky(): Unit =
    generateStars()
    resetGlArraysAndBuffers()
    val walk = dayPhase * 2f * Pi.toFloat
    glDisable(GL_FOG)
    glDisable(GL_DEPTH_TEST)
    glDepthMask(false)
    glDisable(GL_TEXTURE_2D)
    glDisable(GL_CULL_FACE)
    glEnable(GL_BLEND)
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA)
    val phase = dayPhase
    val daylight = daylightFactor
    val starVis = smooth01((0.88f - daylight) / 0.34f)

    // Draw sky in camera space. Do not translate by world X/Z, otherwise large
    // coordinates drag the sky into the same precision problem as terrain.
    glMatrixMode(GL_MODELVIEW)
    glPushMatrix()
    glLoadIdentity()
    glRotatef(pitch, 1f, 0f, 0f)
    glRotatef(yaw, 0f, 1f, 0f)

    // Stars rotate with the sky dome
    glPushMatrix()
    glRotatef(walk * -6f, 0f, 0f, 1f)
    glRotatef(35f, 1f, 0f, 0f)
    glPointSize(2.5f)
    glBegin(GL_POINTS)
    if starVis > 0.01f then
      starPositions.foreach { (sx, sy, sz, sb) =>
        val alpha = (sb * starVis).max(0f).min(1f)
        glColor4f(1f, 1f, 1f, alpha)
        glVertex3f(sx, sy, sz)
      }
    glEnd()
    glPointSize(1f)
    glPopMatrix()

    // Sun and moon are intentionally disabled for now. The lighting cycle and stars stay,
    // but the old discs looked cheap and distracted from the terrain.
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA)
    glPopMatrix()
    glDepthMask(true)
    glEnable(GL_DEPTH_TEST)
    glEnable(GL_FOG)

  private def updateBubbles(dt: Float): Unit =
    if isUnderwater then
      while bubbleParticles.length > 80 do bubbleParticles.remove(bubbleParticles.length - 1)
      if (glfwGetTime() * 15).toInt % 3 == 0 then
        val rng = new Random()
        val offsetX = (rng.nextFloat() - 0.5f) * 0.6f
        val offsetZ = (rng.nextFloat() - 0.5f) * 0.6f
        bubbleParticles += ((camera.x + offsetX, camera.y + rng.nextFloat() * 1.4f, camera.z + offsetZ, 1.5f + rng.nextFloat() * 2f))
    for i <- bubbleParticles.indices do
      val (x, y, z, life) = bubbleParticles(i)
      val ny = y + 2.5f * dt
      val npos = Vec3(x, ny, z)
      if ny > camera.y + 2f || blockAt(npos).solid || life <= 0f then
        bubbleParticles(i) = (x, ny, z, life - dt)
      else bubbleParticles(i) = (x, ny, z, life)
    bubbleParticles.filterInPlace((_, _, _, life) => life > 0f)

  private def renderBubbles(): Unit =
    if bubbleParticles.isEmpty then return
    glDisable(GL_TEXTURE_2D)
    glDisable(GL_FOG)
    glEnable(GL_POINT_SMOOTH)
    glPointSize(4f)
    val t = glfwGetTime().toFloat
    glBegin(GL_POINTS)
    for (x, y, z, life) <- bubbleParticles do
      val bob = sin(t * 3f + x * 10f + z * 10f).toFloat * 0.05f
      val alpha = (life / 2f).max(0.15f).min(0.6f)
      glColor4f(0.85f, 0.92f, 1f, alpha)
      glVertex3f(x - camera.x + bob, y, z - camera.z + bob)
    glEnd()
    glPointSize(1f)
    glDisable(GL_POINT_SMOOTH)
    glEnable(GL_TEXTURE_2D)
    glEnable(GL_FOG)

  private def renderFallingSand(): Unit =
    // Falling sand rendering is disabled with the physics toggle above.
    ()

  private def renderChat(): Unit =
    glDisable(GL_DEPTH_TEST); glDisable(GL_CULL_FACE); setupOrtho()
    val w = framebufferWidth.toFloat; val h = framebufferHeight.toFloat
    val chatWidth = w.min(600f)
    var ly = h - 40f
    val maxDisplay = 12
    var count = 0
    for (msg, t) <- chatMessages.view.reverse do
      if count < maxDisplay && t > 0f then
        val alpha = t.min(1f)
        val chatScale = (1.28f * uiScale).max(1.08f)
        if ly > 60 then
          rect(10f, ly - 4f, textWidth(msg, chatScale) + 12f, 18f * chatScale + 4f, 0f, 0f, 0f, 0.30f * alpha)
          renderTextShadow(14, ly, msg, 1f, 1f, 1f, chatScale)
        ly -= 22f * chatScale.max(0.9f)
        count += 1
    if chatOpen then
      val inputY = h - 38f
      rect(10, inputY, chatWidth - 10, 30, 0f, 0f, 0f, 0.55f)
      rect(12, inputY + 2, chatWidth - 14, 2, 0.30f, 0.30f, 0.35f, 0.25f)
      val display = s"> $chatInput${if (glfwGetTime() * 2).toInt % 2 == 0 then "_" else " "}"
      renderTextShadow(18, inputY + 5, display, 1f, 1f, 1f, (1.34f * uiScale).max(1.14f))
      val suggestions = commandSuggestions(chatInput)
      if suggestions.nonEmpty then
        val boxY = inputY - (suggestions.length * 22f * uiScale + 8f)
        rect(10, boxY, chatWidth - 10, inputY - boxY - 4f, 0f, 0f, 0f, 0.50f)
        suggestions.zipWithIndex.foreach { case (sug, idx) =>
          val selected = idx == commandSuggestionIndex.max(0).min(suggestions.length - 1)
          val y = boxY + 6f + idx * 22f * uiScale
          if selected then rect(14, y - 2f, chatWidth - 22f, 18f * uiScale, 0.20f, 0.22f, 0.28f, 0.72f)
          renderTextShadow(20, y, sug, if selected then 1f else 0.78f, if selected then 0.92f else 0.84f, if selected then 0.55f else 0.96f, (1.0f * uiScale).max(0.88f))
        }

  private def mainMenuLayout(): (Float, Float, Float, Array[Float], Float, Float, Float, Float, Float) =
    val w = framebufferWidth.toFloat
    val h = framebufferHeight.toFloat
    val s = uiScale
    val cx = w / 2f
    val margin = (18f * s).max(12f)
    val footerReserve = (30f * s).max(20f)

    // Fit everything vertically first. This keeps the title card + 7 buttons inside the window.
    val desiredTitleH = 136f * s
    val minTitleH = 82f
    val maxTitleH = (h * 0.25f).min(140f * s).max(minTitleH)
    val titleH = desiredTitleH.min(maxTitleH).max(minTitleH.min(h * 0.20f))
    val titleW = (620f * s).min(w * 0.68f).max(math.min(340f, w - margin * 2f))
    val titleX = cx - titleW / 2f
    val titleY = margin

    val labelH = (18f * s).max(12f)
    val gap = (9f * s).max(5f).min(12f)
    val firstY = titleY + titleH + (20f * s).max(12f)
    val available = (h - firstY - footerReserve - labelH - gap * 8f).max(7f * 26f)
    val buttonH = (38f * s).min(available / 7f).max(26f)
    val buttonW = (360f * s).min(w * 0.48f).max(math.min(260f, w - margin * 2f))
    val bx = cx - buttonW / 2f

    val y0 = firstY
    val y1 = y0 + buttonH + gap + labelH + gap
    val ys = Array(y0, y1, y1 + (buttonH + gap), y1 + (buttonH + gap) * 2f, y1 + (buttonH + gap) * 3f, y1 + (buttonH + gap) * 4f, y1 + (buttonH + gap) * 5f)
    (bx, buttonW, buttonH, ys, titleX, titleY, titleW, titleH, s)

  private def titleGlyph(c: Char): Array[String] = c match
    case 'B' => Array("11110", "10001", "10001", "11110", "10001", "10001", "11110")
    case 'L' => Array("10000", "10000", "10000", "10000", "10000", "10000", "11111")
    case 'O' => Array("01110", "10001", "10001", "10001", "10001", "10001", "01110")
    case 'C' => Array("01111", "10000", "10000", "10000", "10000", "10000", "01111")
    case 'K' => Array("10001", "10010", "10100", "11000", "10100", "10010", "10001")
    case 'X' => Array("10001", "10001", "01010", "00100", "01010", "10001", "10001")
    case _   => Array("00000", "00000", "00000", "00000", "00000", "00000", "00000")

  private def drawPixelLogo(cx: Float, y: Float, text: String, maxW: Float, maxH: Float): Unit =
    if text == null || text.isEmpty then return
    val chars = text.toUpperCase.filter(ch => ch == ' ' || "BLOCKX".indexOf(ch) >= 0)
    if chars.isEmpty then return
    var cols = 0
    for ch <- chars do cols += (if ch == ' ' then 3 else 5)
    cols += (chars.length - 1).max(0)
    val cell = (maxW / cols.toFloat).min(maxH / 7f).max(1.5f)
    val totalW = cols * cell
    var x = cx - totalW / 2f
    val shadow = (cell * 0.22f).max(1.2f)
    def drawPass(offX: Float, offY: Float, r: Float, g: Float, b: Float, a: Float): Unit =
      var px = x + offX
      for ch <- chars do
        if ch == ' ' then px += 4f * cell
        else
          val glyph = titleGlyph(ch)
          for row <- 0 until 7 do
            val line = glyph(row)
            for col <- 0 until 5 do
              if line.charAt(col) == '1' then
                rect(px + col * cell + cell * 0.04f, y + offY + row * cell + cell * 0.04f, cell * 0.92f, cell * 0.92f, r, g, b, a)
          px += 6f * cell
    drawPass(shadow, shadow * 1.4f, 0f, 0f, 0f, 0.70f)
    drawPass(0f, 0f, 1f, 0.92f, 0.40f, 1f)
    rect(cx - totalW / 2f, y + 7.45f * cell, totalW, cell * 0.18f, 1f, 0.90f, 0.42f, 0.22f)

  private def renderMainMenu(): Unit =
    glDisable(GL_DEPTH_TEST); glDisable(GL_CULL_FACE); setupOrtho()
    val w = framebufferWidth.toFloat; val h = framebufferHeight.toFloat
    val cx = w / 2f; val t = glfwGetTime().toFloat
    glBegin(GL_QUADS)
    glColor4f(0.035f, 0.050f, 0.115f, 1f); glVertex2f(0, 0)
    glColor4f(0.055f, 0.085f, 0.155f, 1f); glVertex2f(w, 0)
    glColor4f(0.100f, 0.160f, 0.260f, 1f); glVertex2f(w, h * 0.60f)
    glColor4f(0.060f, 0.085f, 0.160f, 1f); glVertex2f(0, h * 0.60f)
    glEnd()
    glBegin(GL_QUADS)
    glColor4f(0.065f, 0.135f, 0.055f, 1f); glVertex2f(0, h * 0.60f)
    glColor4f(0.095f, 0.175f, 0.065f, 1f); glVertex2f(w, h * 0.60f)
    glColor4f(0.145f, 0.210f, 0.075f, 1f); glVertex2f(w, h)
    glColor4f(0.085f, 0.145f, 0.045f, 1f); glVertex2f(0, h)
    glEnd()
    val gridSize = (38f * uiScale).max(22f)
    glLineWidth(1f); glBegin(GL_LINES)
    var gx = 0f
    while gx < w do
      glColor4f(0.30f, 0.36f, 0.24f, 0.035f)
      glVertex2f(gx, h * 0.60f); glVertex2f(gx, h)
      gx += gridSize
    var gy = h * 0.60f
    while gy < h do
      glColor4f(0.30f, 0.36f, 0.24f, 0.035f)
      glVertex2f(0f, gy); glVertex2f(w, gy)
      gy += gridSize
    glEnd()

    val layout = mainMenuLayout()
    val bx = layout._1; val bw = layout._2; val bh = layout._3; val ys = layout._4
    val titleX = layout._5; val titleY = layout._6; val titleW = layout._7; val titleH = layout._8; val s = layout._9
    drawPanel(titleX, titleY, titleW, titleH)
    val innerX = titleX + 18f * s; val innerY = titleY + 14f * s; val innerW = titleW - 36f * s; val innerH = (72f * s).min(titleH - 48f * s).max(54f)
    rect(innerX, innerY, innerW, innerH, 0.055f, 0.075f, 0.125f, 0.62f)
    rect(innerX, innerY, innerW, 2f, 0.70f, 0.55f, 0.24f, 0.46f)
    rect(innerX, innerY + innerH - 2f, innerW, 2f, 0.25f, 0.20f, 0.12f, 0.34f)
    drawPixelLogo(cx, innerY + innerH * 0.13f, "BLOCKBOX", innerW - 60f * s, innerH * 0.50f)
    centeredTextFit(cx + 1f * s, innerY + innerH * 0.80f + 1f * s, "Voxel Sandbox", 0f, 0f, 0f, 0.72f * s, innerW - 28f * s)
    centeredTextFit(cx, innerY + innerH * 0.80f, "Voxel Sandbox", 0.80f, 0.86f, 1f, 0.72f * s, innerW - 28f * s)
    rect(titleX + 34f * s, titleY + titleH - 45f * s, titleW - 68f * s, 1f, 0.30f, 0.34f, 0.42f, 0.22f)
    centeredTextFit(cx, titleY + titleH - 31f * s, "Built with Scala 3 + LWJGL", 0.45f, 0.53f, 0.66f, 0.54f * s, titleW - 70f * s)

    drawButton(bx, ys(0), bw, bh, "Singleplayer", accent = true)
    centeredTextFit(cx, ys(1) - 22f * s, "MULTIPLAYER", 0.50f, 0.56f, 0.68f, 0.46f * s, bw)
    rect(cx - bw * 0.32f, ys(1) - 10f * s, bw * 0.64f, 1f, 0.28f, 0.32f, 0.40f, 0.20f)
    drawButton(bx, ys(1), bw, bh, "Host LAN Game")
    drawButton(bx, ys(2), bw, bh, "Join LAN Game")
    drawButton(bx, ys(3), bw, bh, "Load World")
    drawButton(bx, ys(4), bw, bh, "Mods")
    drawButton(bx, ys(5), bw, bh, "Options")
    drawButton(bx, ys(6), bw, bh, "Quit Game")
    centeredTextFit(cx, h - 18f * s, "v1.0 | Scala 3 + LWJGL | Open Source", 0.38f, 0.50f, 0.66f, 0.52f * s, w - 40f * s)

  private def textMetrics(text: String): (java.nio.ByteBuffer, Int, Float, Float, Float, Float) =
    val safeText = if text == null then "" else text
    val buf = BufferUtils.createByteBuffer((safeText.length.max(1)) * 320)
    val quads = STBEasyFont.stb_easy_font_print(0, 0, safeText, null, buf)
    if quads <= 0 then return (buf, quads, 0f, 0f, 0f, 0f)
    var minX = Float.MaxValue; var maxX = Float.MinValue
    var minY = Float.MaxValue; var maxY = Float.MinValue
    var i = 0
    while i < quads * 4 do
      val px = buf.getFloat(i * 16)
      val py = buf.getFloat(i * 16 + 4)
      if px < minX then minX = px
      if px > maxX then maxX = px
      if py < minY then minY = py
      if py > maxY then maxY = py
      i += 1
    (buf, quads, minX, maxX, minY, maxY)

  private def textWidth(text: String, scale: Float): Float =
    if text == null || text.isEmpty then 0f
    else
      val (_, quads, minX, maxX, _, _) = textMetrics(text)
      if quads <= 0 then text.length * 8f * scale else (maxX - minX).max(1f) * scale

  private def centeredTextFit(cx: Float, y: Float, text: String, r: Float, g: Float, b: Float, scale: Float, maxWidth: Float): Unit =
    if text == null || text.isEmpty then return
    val (buf, quads, minX, maxX, minY, _) = textMetrics(text)
    if quads <= 0 then return
    val rawW = (maxX - minX).max(1f)
    val safeW = maxWidth.max(12f).min(framebufferWidth.toFloat - 12f)
    val eff = scale.min(safeW / rawW).max(0.30f)
    val left = cx - rawW * eff / 2f - minX * eff
    val top = y - minY * eff
    val so = (1.25f * eff).max(0.75f)
    renderTextBuf(buf, quads, left + so, top + so, 0f, 0f, 0f, eff)
    renderTextBuf(buf, quads, left, top, r, g, b, eff)

  private def centeredText(cx: Float, y: Float, text: String, r: Float, g: Float, b: Float, scale: Float): Unit =
    val safeW = (math.min(cx, framebufferWidth.toFloat - cx) * 2f - 12f).max(24f)
    centeredTextFit(cx, y, text, r, g, b, scale, safeW)

  private def centeredTextBox(x: Float, y: Float, w: Float, h: Float, text: String, r: Float, g: Float, b: Float, scale: Float): Unit =
    if text == null || text.isEmpty then return
    val (buf, quads, minX, maxX, minY, maxY) = textMetrics(text)
    if quads <= 0 then return
    val rawW = (maxX - minX).max(1f)
    val rawH = (maxY - minY).max(1f)
    val eff = scale.min((w - 14f * uiScale).max(8f) / rawW).min((h - 8f * uiScale).max(8f) / rawH).max(0.30f)
    val tx = x + w / 2f - rawW * eff / 2f - minX * eff
    val ty = y + h / 2f - rawH * eff / 2f - minY * eff
    val so = (1.20f * eff).max(0.65f)
    renderTextBuf(buf, quads, tx + so, ty + so, 0f, 0f, 0f, eff)
    renderTextBuf(buf, quads, tx, ty, r, g, b, eff)

  private def renderText(x: Float, y: Float, text: String, r: Float, g: Float, b: Float, scale: Float = 1f): Unit =
    if text == null || text.isEmpty then return
    val lineH = 12f * scale
    if x >= framebufferWidth - 4f || x + 2f < 0 || y >= framebufferHeight - 2f || y + lineH < 0 then return
    val maxChars = ((framebufferWidth - x - 12f).max(4f) / (8f * scale).max(1f)).toInt
    val display = if maxChars <= 0 then "" else if maxChars >= text.length then text else text.take(maxChars.max(4) - 1) + "…"
    if display.isEmpty then return
    val (buf, quads, _, _, _, _) = textMetrics(display)
    if quads <= 0 then return
    renderTextBuf(buf, quads, x, y, r, g, b, scale)

  private def renderTextBuf(buf: java.nio.ByteBuffer, quads: Int, x: Float, y: Float, r: Float, g: Float, b: Float, scale: Float): Unit =
    buf.rewind()
    glDisable(GL_DEPTH_TEST); glDisable(GL_CULL_FACE); glDisable(GL_TEXTURE_2D); setupOrtho()
    glBindBuffer(GL_ARRAY_BUFFER, 0)
    glDisableClientState(GL_COLOR_ARRAY); glDisableClientState(GL_TEXTURE_COORD_ARRAY)
    glEnable(GL_BLEND); glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA)
    glPushMatrix(); glTranslatef(x, y, 0); glScalef(scale, scale, 1); glColor4f(r, g, b, 1f)
    glEnableClientState(GL_VERTEX_ARRAY); glVertexPointer(2, GL_FLOAT, 16, buf)
    glDrawArrays(GL_QUADS, 0, quads * 4); glDisableClientState(GL_VERTEX_ARRAY); glPopMatrix()

  private def renderCreateWorld(): Unit =
    glDisable(GL_DEPTH_TEST); glDisable(GL_CULL_FACE); setupOrtho()
    val w = framebufferWidth.toFloat
    val h = framebufferHeight.toFloat
    glBegin(GL_QUADS)
    glColor4f(0.05f, 0.06f, 0.12f, 1f); glVertex2f(0, 0); glVertex2f(w, 0)
    glColor4f(0.14f, 0.26f, 0.44f, 1f); glVertex2f(w, h); glVertex2f(0, h)
    glEnd()
    rect(0, h * 0.62f, w, h * 0.38f, 0.06f, 0.18f, 0.06f, 0.85f)
    rect(0, h * 0.76f, w, h * 0.24f, 0.22f, 0.16f, 0.08f, 0.80f)

    val cx = w / 2f
    val s = uiScale
    val margin = (18f * s).max(12f)
    val pW = (620f * s).min(w - margin * 2f).max(math.min(360f, w - margin * 2f))
    val pH = (560f * s).min(h - margin * 2f).max(math.min(420f, h - margin * 2f))
    val pX = cx - pW / 2f
    val pY = ((h - pH) / 2f).max(margin)
    drawPanel(pX, pY, pW, pH)

    centeredTextFit(cx, pY + 34f * s, "NEW WORLD", 1f, 0.90f, 0.55f, 2.15f * s, pW - 80f * s)
    rect(pX + 40f * s, pY + 68f * s, pW - 80f * s, 1f, 0.30f, 0.30f, 0.35f, 0.30f)

    val leftX = pX + 40f * s
    val contentW = pW - 80f * s
    val settingW = contentW.max(260f.min(pW - 32f * s))
    val settingH = (36f * s).min(38f).max(28f)
    val settingX = cx - settingW / 2f
    val nameY = pY + 104f * s
    val seedY = pY + 166f * s
    val modeY = pY + 236f * s
    val cheatsY = pY + 282f * s
    renderNameField(settingX, nameY, settingW, 34f * s, "World name", worldNameInput, createWorldNameFocused)
    val seedDisplay = if customSeedInput.nonEmpty then customSeedInput else worldSeed.toString
    renderNameField(settingX, seedY, settingW, 34f * s, "Seed (optional)", seedDisplay, !createWorldNameFocused)
    def settingRow(y: Float, label: String, value: String): Unit =
      val (mx, my) = mouseFramebufferPos()
      val hover = inRect(mx, my, settingX, y, settingW, settingH)
      val br = if hover then 0.13f else 0.09f
      rect(settingX, y, settingW, settingH, br, br * 1.08f, br * 1.22f, 0.66f)
      rect(settingX + 2f * s, y + 2f * s, settingW - 4f * s, 2f * s, 1f, 1f, 1f, 0.05f)
      renderTextShadow(settingX + 16f * s, y + settingH / 2f - 5f * s, label, 0.82f, 0.86f, 0.92f, 0.62f * s)
      centeredTextFit(settingX + settingW - 82f * s, y + settingH / 2f - 5f * s, value, 1f, 0.93f, 0.54f, 0.62f * s, 150f * s)
    settingRow(modeY, "Game Mode", createWorldMode.toString)
    settingRow(cheatsY, "Allow Cheats", onOff(createWorldCheats))

    rect(pX + 40f * s, pY + pH - 186f * s, pW - 80f * s, 1f, 0.30f, 0.30f, 0.35f, 0.22f)
    val buttonW = (pW - 120f * s).max(240f.min(pW - 40f * s))
    val buttonH = (38f * s).min(42f).max(30f)
    val gap = (10f * s).max(6f).min(12f)
    val buttonX = cx - buttonW / 2f
    val buttonStartY = pY + pH - (buttonH * 4f + gap * 3f) - 18f * s
    drawButton(buttonX, buttonStartY, buttonW, buttonH, "Create New World", accent = true)
    drawButton(buttonX, buttonStartY + buttonH + gap, buttonW, buttonH, "Random Seed")
    drawButton(buttonX, buttonStartY + (buttonH + gap) * 2f, buttonW, buttonH, if enterCustomSeed then "Hide Custom Seed" else "Custom Seed")
    drawButton(buttonX, buttonStartY + (buttonH + gap) * 3f, buttonW, buttonH, "Back")
    centeredTextFit(cx, pY + pH - 10f * s, "Tab switches fields | N name | C seed | M mode | H cheats", 0.42f, 0.48f, 0.60f, 0.46f * s, pW - 48f * s)

  private def renderLoadWorldMenu(): Unit =
    glDisable(GL_DEPTH_TEST); glDisable(GL_CULL_FACE); setupOrtho()
    val w = framebufferWidth.toFloat; val h = framebufferHeight.toFloat; val cx = w / 2f; val s = uiScale
    glBegin(GL_QUADS)
    glColor4f(0.05f, 0.06f, 0.12f, 1f); glVertex2f(0, 0); glVertex2f(w, 0)
    glColor4f(0.14f, 0.26f, 0.44f, 1f); glVertex2f(w, h); glVertex2f(0, h)
    glEnd()
    rect(0, h * 0.62f, w, h * 0.38f, 0.06f, 0.18f, 0.06f, 0.85f)
    val pW = (700f * s).min(w * 0.92f); val pH = (520f * s).min(h * 0.90f)
    val pX = cx - pW / 2f; val pY = h / 2f - pH / 2f
    drawPanel(pX, pY, pW, pH)
    centeredTextFit(cx, pY + 34f * s, "SELECT WORLD", 1f, 0.90f, 0.55f, 2.0f * s, pW - 80f * s)
    rect(pX + 34f * s, pY + 64f * s, pW - 68f * s, 1f, 0.30f, 0.30f, 0.35f, 0.30f)
    val saves = worldSaveDirs
    if saves.isEmpty then
      centeredTextFit(cx, pY + 150f * s, "No saved worlds found in worlds/", 0.78f, 0.84f, 0.94f, 0.9f * s, pW - 80f * s)
    else
      val listX = pX + 42f * s
      val listY = pY + 86f * s
      val rowH = (46f * s).max(34f)
      val visible = ((pH - 160f * s) / rowH).toInt.max(3)
      if loadWorldSelection < loadWorldScroll then loadWorldScroll = loadWorldSelection
      if loadWorldSelection >= loadWorldScroll + visible then loadWorldScroll = loadWorldSelection - visible + 1
      loadWorldScroll = loadWorldScroll.max(0).min((saves.length - visible).max(0))
      saves.zipWithIndex.drop(loadWorldScroll).take(visible).foreach { case (dir, idx) =>
        val y = listY + (idx - loadWorldScroll) * rowH
        val selected = idx == loadWorldSelection
        val worldDat = new java.io.File(dir, "world.dat")
        rect(listX, y, pW - 84f * s, rowH - 6f * s, if selected then 0.22f else 0.08f, if selected then 0.20f else 0.09f, if selected then 0.10f else 0.13f, 0.88f)
        if selected then rect(listX, y, 4f * s, rowH - 6f * s, 1f, 0.82f, 0.32f, 0.82f)
        renderTextShadow(listX + 14f * s, y + 7f * s, dir.getName, 0.94f, 0.96f, 1f, 0.76f * s)
        renderTextShadow(listX + 14f * s, y + 25f * s, s"Saved: ${new java.util.Date(worldDat.lastModified()).toString}", 0.50f, 0.58f, 0.70f, 0.48f * s)
      }
    val bw = (220f * s).min(pW * 0.36f); val bh = (38f * s).max(30f)
    drawButton(cx - bw - 12f * s, pY + pH - 56f * s, bw, bh, "Load Selected", accent = true)
    drawButton(cx + 12f * s, pY + pH - 56f * s, bw, bh, "Back")
    centeredTextFit(cx, pY + pH - 12f * s, "Up/Down selects | Enter loads | ESC backs out", 0.42f, 0.48f, 0.60f, 0.46f * s, pW - 80f * s)

  private def handleLoadWorldClick(mx: Float, my: Float): Unit =
    val w = framebufferWidth.toFloat; val h = framebufferHeight.toFloat; val cx = w / 2f; val s = uiScale
    val pW = (700f * s).min(w * 0.92f); val pH = (520f * s).min(h * 0.90f)
    val pX = cx - pW / 2f; val pY = h / 2f - pH / 2f
    val saves = worldSaveDirs
    val listX = pX + 42f * s; val listY = pY + 86f * s; val rowH = (46f * s).max(34f)
    val visible = ((pH - 160f * s) / rowH).toInt.max(3)
    saves.zipWithIndex.drop(loadWorldScroll).take(visible).foreach { case (_, idx) =>
      val y = listY + (idx - loadWorldScroll) * rowH
      if inRect(mx, my, listX, y, pW - 84f * s, rowH - 6f * s) then loadWorldSelection = idx
    }
    val bw = (220f * s).min(pW * 0.36f); val bh = (38f * s).max(30f)
    if inRect(mx, my, cx - bw - 12f * s, pY + pH - 56f * s, bw, bh) then loadSelectedWorld()
    else if inRect(mx, my, cx + 12f * s, pY + pH - 56f * s, bw, bh) then screen = Screen.MainMenu


  private def renderModsScreen(): Unit =
    glDisable(GL_DEPTH_TEST); glDisable(GL_CULL_FACE); setupOrtho()
    val w = framebufferWidth.toFloat; val h = framebufferHeight.toFloat; val cx = w / 2f; val s = uiScale
    glBegin(GL_QUADS)
    glColor4f(0.04f, 0.05f, 0.10f, 1f); glVertex2f(0, 0); glVertex2f(w, 0)
    glColor4f(0.10f, 0.18f, 0.30f, 1f); glVertex2f(w, h); glVertex2f(0, h)
    glEnd()
    val pW = (760f * s).min(w * 0.92f); val pH = (560f * s).min(h * 0.90f)
    val pX = cx - pW / 2f; val pY = h / 2f - pH / 2f
    drawPanel(pX, pY, pW, pH)
    centeredTextFit(cx, pY + 32f * s, "MODS", 1f, 0.94f, 0.55f, 2.0f * s, pW - 80f * s)
    centeredTextFit(cx, pY + 58f * s, s"server modpack: ${modManager.serverModpackHash}", 0.62f, 0.74f, 0.95f, 0.56f * s, pW - 80f * s)
    rect(pX + 34f * s, pY + 76f * s, pW - 68f * s, 1f, 0.30f, 0.34f, 0.42f, 0.30f)
    val mods = modManager.loadedMods
    if mods.isEmpty then
      centeredTextFit(cx, pY + 138f * s, "No mods loaded. Put .jar or Groovy mods in mods/", 0.82f, 0.88f, 1f, 0.78f * s, pW - 90f * s)
    else
      val rowH = (58f * s).max(42f)
      val listX = pX + 42f * s
      val listY = pY + 92f * s
      val visible = ((pH - 170f * s) / rowH).toInt.max(3)
      modsScreenScroll = modsScreenScroll.max(0).min((mods.length - visible).max(0))
      mods.zipWithIndex.drop(modsScreenScroll).take(visible).foreach { case (m, idx) =>
        val y = listY + (idx - modsScreenScroll) * rowH
        val ok = m.loaded
        rect(listX, y, pW - 84f * s, rowH - 6f * s, if ok then 0.08f else 0.20f, if ok then 0.11f else 0.08f, if ok then 0.16f else 0.08f, 0.86f)
        rect(listX, y, 4f * s, rowH - 6f * s, if ok then 0.25f else 0.95f, if ok then 0.75f else 0.25f, if ok then 0.35f else 0.20f, 0.85f)
        renderTextShadow(listX + 14f * s, y + 7f * s, s"${m.name} ${m.version}  [${m.sideLabel}]", 0.96f, 0.97f, 1f, 0.68f * s)
        renderTextShadow(listX + 14f * s, y + 25f * s, m.description.take(96), 0.62f, 0.70f, 0.82f, 0.50f * s)
        renderTextShadow(listX + 14f * s, y + 41f * s, s"${m.id} | ${m.status}", if ok then 0.52f else 1f, if ok then 0.70f else 0.55f, if ok then 0.58f else 0.50f, 0.45f * s)
      }
    val bw = (260f * s).min(pW * 0.44f); val bh = (38f * s).max(30f)
    drawButton(cx - bw / 2f, pY + pH - 56f * s, bw, bh, "Back")
    centeredTextFit(cx, pY + pH - 13f * s, "mods are trusted full-access JVM code. server mods must match host.", 0.48f, 0.55f, 0.68f, 0.46f * s, pW - 70f * s)

  private def handleModsClick(mx: Float, my: Float): Unit =
    val w = framebufferWidth.toFloat; val h = framebufferHeight.toFloat; val cx = w / 2f; val s = uiScale
    val pW = (760f * s).min(w * 0.92f); val pH = (560f * s).min(h * 0.90f)
    val pY = h / 2f - pH / 2f
    val bw = (260f * s).min(pW * 0.44f); val bh = (38f * s).max(30f)
    if inRect(mx, my, cx - bw / 2f, pY + pH - 56f * s, bw, bh) then screen = Screen.MainMenu

  private def renderSettings(): Unit =
    glDisable(GL_DEPTH_TEST); glDisable(GL_CULL_FACE); setupOrtho()
    val w = framebufferWidth.toFloat; val h = framebufferHeight.toFloat
    glBegin(GL_QUADS)
    glColor4f(0.05f, 0.06f, 0.12f, 1f); glVertex2f(0, 0); glVertex2f(w, 0)
    glColor4f(0.14f, 0.26f, 0.44f, 1f); glVertex2f(w, h); glVertex2f(0, h)
    glEnd()
    rect(0, h * 0.62f, w, h * 0.38f, 0.06f, 0.18f, 0.06f, 0.85f)
    rect(0, h * 0.76f, w, h * 0.24f, 0.22f, 0.16f, 0.08f, 0.80f)
    val cx = w / 2f; val s = uiScale
    val pW = (520f * s).min(w * 0.92f); val pH = (590f * s).min(h * 0.92f); val pX = cx - pW / 2f; val pY = (h / 2f - pH / 2f).max(12f * s)
    drawPanel(pX, pY, pW, pH)
    centeredText(cx, pY + 36 * s, "OPTIONS", 1f, 0.95f, 0.55f, 2.6f * s)
    rect(pX + 40 * s, pY + 68 * s, pW - 80 * s, 1, 0.30f, 0.30f, 0.35f, 0.30f)
    val settingX = pX + 40 * s
    val (smx, smy) = mouseFramebufferPos()
    val rowW = 440f * s; val rowH = 28f * s
    def clickRow(y: Float, label: String): Unit =
      val hover = inRect(smx, smy, settingX, y, rowW, rowH)
      rect(settingX, y, rowW, rowH, if hover then 0.18f else 0.10f, if hover then 0.20f else 0.12f, if hover then 0.26f else 0.16f, 0.85f)
      renderTextShadow(settingX + 12 * s, y + 5 * s, label, 1f, 1f, 1f, (0.82f * s).min(rowH / 15f))
    clickRow(pY + 85 * s, s"Render distance: $renderDistance chunks (${renderDistanceBlocks} blocks)")
    slider(settingX, pY + 118 * s, rowW, (renderDistance - minRenderDistance).toFloat / (maxRenderDistance - minRenderDistance).toFloat)
    clickRow(pY + 150 * s, s"Fog density: $fogDensity%.2f")
    slider(settingX, pY + 185 * s, rowW, (fogDensity - 0.6f) / 2.4f)
    clickRow(pY + 218 * s, f"FOV: $fov%.0f")
    slider(settingX, pY + 253 * s, rowW, (fov - 50f) / 50f)
    var optY = pY + 285 * s
    clickRow(optY, s"Fast movement: ${onOff(fastMove)}"); optY += 40 * s
    clickRow(optY, s"VSync: ${onOff(vsync)}"); optY += 40 * s
    if settingsReturnTo == Screen.Playing && gameServer != null && worldCheatsEnabled then
      clickRow(optY, s"Host game mode: ${gameMode}"); optY += 40 * s
    clickRow(optY, s"Sound effects: ${onOff(soundEnabled)}"); optY += 40 * s
    clickRow(optY, s"Fullscreen: ${onOff(fullscreen)}"); optY += 40 * s
    clickRow(optY, s"Pause ESC: ${if pauseEscReturnsToGame then "Resume game" else "Quit to title"}")
    rect(pX + 40 * s, pY + 525 * s, pW - 80 * s, 1, 0.30f, 0.30f, 0.35f, 0.20f)
    val buttonW = 300f * s; val buttonH = 44f * s; val buttonX = cx - buttonW / 2f
    drawButton(buttonX, pY + 535 * s, buttonW, buttonH, "Done")

  private def renderPauseMenu(): Unit =
    glDisable(GL_DEPTH_TEST); glDisable(GL_CULL_FACE); setupOrtho()
    if multiplayerMode then
      // Multiplayer pause is a local overlay only. Do not black out the world; players/chats keep updating behind it.
      rect(0, 0, framebufferWidth, framebufferHeight, 0f, 0f, 0f, 0.08f)
    else
      dimBackground()
    val cx = framebufferWidth / 2f; val h = framebufferHeight.toFloat; val s = uiScale
    val pw = (420f * s).min(framebufferWidth * 0.80f); val ph = (if multiplayerMode then 300f * s else 275f * s).min(h * 0.82f); val px = cx - pw / 2f; val py = h / 2f - ph / 2f
    drawPanel(px, py, pw, ph)
    centeredText(cx, py + 34 * s, if multiplayerMode then "MULTIPLAYER" else "PAUSED", 1f, 0.95f, 0.55f, if multiplayerMode then 2.05f * s else 2.6f * s)
    if multiplayerMode then centeredTextFit(cx, py + 60f * s, "Client-side pause - server keeps running", 0.68f, 0.78f, 0.95f, 0.72f * s, pw - 70f * s)
    rect(px + 40 * s, py + 76 * s, pw - 80 * s, 1, 0.30f, 0.30f, 0.35f, 0.30f)
    val bw = (320f * s).min(pw - 72f * s); val bh = (40f * s).min(ph / 6f).max(30f); val bx = cx - bw / 2f
    val gap = (14f * s).min(18f)
    val by0 = py + 92f * s
    drawButton(bx, by0, bw, bh, "Back to Game", accent = true)
    drawButton(bx, by0 + bh + gap, bw, bh, "Options")
    drawButton(bx, by0 + (bh + gap) * 2f, bw, bh, if multiplayerMode then "Disconnect to Title" else "Save and Quit to Title")

  private def renderSurvivalInventory(): Unit =
    compactHotbarAssignments()
    glDisable(GL_DEPTH_TEST); glDisable(GL_CULL_FACE); setupOrtho()
    dimBackground()
    val cx = framebufferWidth / 2f; val cy = framebufferHeight / 2f; val s = uiScale
    val pw = (760f * s).min(framebufferWidth * 0.94f)
    val ph = (500f * s).min(framebufferHeight * 0.88f)
    val px = cx - pw / 2f; val py = cy - ph / 2f
    drawPanel(px, py, pw, ph)
    centeredTextFit(cx, py + 24f * s, "Inventory", 1f, 0.95f, 0.65f, 1.18f * s, pw - 80f * s)
    rect(px + 18f * s, py + 44f * s, pw - 36f * s, 1f, 0.30f, 0.30f, 0.35f, 0.25f)

    val (mx, my) = mouseFramebufferPos()
    val gridW = pw * 0.56f
    val cols = 6
    val gap = (6f * s).max(4f)
    val slot = ((gridW - gap * (cols - 1)) / cols).min(44f * s).max(30f)
    val invX = px + 28f * s
    val invY = py + 76f * s
    val items = inventoryItems
    var hoveredBlock: Block | Null = null
    var hoveredX = 0f; var hoveredY = 0f

    rect(invX - 10f * s, invY - 28f * s, gridW + 18f * s, ph - 140f * s, 0.045f, 0.060f, 0.085f, 0.68f)
    renderTextShadow(invX, invY - 20f * s, "Blocks & Items", 0.78f, 0.84f, 0.96f, 0.70f * s)
    for i <- 0 until inventoryGridSlots do
      val col = i % cols; val row = i / cols
      val sx = invX + col * (slot + gap); val sy = invY + row * (slot + gap)
      val hasItem = i < items.length
      val block = if hasItem then items(i) else Block.Air
      val count = if hasItem then inventory(block.ordinal) else 0
      val isSelected = false
      val hover = inRect(mx, my, sx, sy, slot, slot)
      if hover && hasItem then
        hoveredBlock = block; hoveredX = sx + slot / 2f; hoveredY = sy
      val base = if hover then 0.115f else 0.075f
      rect(sx - 1f * s, sy - 1f * s, slot + 2f * s, slot + 2f * s, 0.015f, 0.017f, 0.022f, 0.78f)
      rect(sx, sy, slot, slot, base, base + 0.01f, base + 0.035f, 0.90f)
      rect(sx + 1f * s, sy + 1f * s, slot - 2f * s, 2f * s, 0.18f, 0.20f, 0.24f, 0.16f)
      if isSelected then
        rect(sx - 2f * s, sy - 2f * s, slot + 4f * s, 2f * s, 1f, 0.92f, 0.42f, 0.55f)
        rect(sx - 2f * s, sy + slot, slot + 4f * s, 2f * s, 1f, 0.92f, 0.42f, 0.55f)
        rect(sx - 2f * s, sy - 2f * s, 2f * s, slot + 4f * s, 1f, 0.92f, 0.42f, 0.55f)
        rect(sx + slot, sy - 2f * s, 2f * s, slot + 4f * s, 1f, 0.92f, 0.42f, 0.55f)
      if hasItem && count > 0 then
        val icon = (slot * 0.58f).min(26f * s).max(16f)
        renderBlockIcon(block, sx + (slot - icon) / 2f, sy + (slot - icon) / 2f, icon)
        val ns = count.toString
        val countScale = (0.72f * s).max(0.56f).min(slot / 32f)
        val tw = textWidth(ns, countScale)
        val nx = sx + slot - tw - 5f * s
        val ny = sy + slot - 12f * countScale - 2f * s
        rect(nx - 2f * s, ny - 1f * s, tw + 4f * s, 11f * countScale + 3f * s, 0f, 0f, 0f, 0.64f)
        renderText(nx, ny, ns, 1f, 1f, 1f, countScale)

    val craftPanelW = (pw - gridW - 78f * s).max(210f * s)
    val craftX = px + pw - 28f * s - craftPanelW
    val craftY = py + 76f * s
    rect(craftX - 10f * s, craftY - 28f * s, craftPanelW + 18f * s, ph - 140f * s, 0.045f, 0.060f, 0.085f, 0.68f)
    centeredTextFit(craftX + craftPanelW / 2f, craftY - 20f * s, "Free-form Crafting", 0.82f, 0.88f, 1f, 0.70f * s, craftPanelW)
    centeredTextFit(craftX + craftPanelW / 2f, craftY - 3f * s, "click item, fill any grid shape, click output", 0.52f, 0.58f, 0.68f, 0.43f * s, craftPanelW)
    val cGap = 6f * s
    val cSlot = ((craftPanelW - 56f * s) / 4f).min(42f * s).max(28f)
    val gridStartX = craftX + 12f * s
    val gridStartY = craftY + 28f * s
    var ci = 0
    while ci < 9 do
      val col = ci % 3
      val row0 = ci / 3
      val sx = gridStartX + col * (cSlot + cGap)
      val sy = gridStartY + row0 * (cSlot + cGap)
      val b = craftGridBlocks(ci)
      val hoverCraft = inRect(mx, my, sx, sy, cSlot, cSlot)
      rect(sx - 1f * s, sy - 1f * s, cSlot + 2f * s, cSlot + 2f * s, 0.01f, 0.012f, 0.018f, 0.82f)
      rect(sx, sy, cSlot, cSlot, if hoverCraft then 0.15f else 0.085f, if hoverCraft then 0.16f else 0.095f, if hoverCraft then 0.19f else 0.125f, 0.92f)
      if b != Block.Air then
        val icon = (cSlot * 0.60f).min(25f * s).max(16f)
        renderBlockIcon(b, sx + (cSlot - icon) / 2f, sy + (cSlot - icon) / 2f, icon)
        if hoverCraft then
          hoveredBlock = b; hoveredX = sx + cSlot / 2f; hoveredY = sy
      ci += 1
    val outSize = cSlot * 1.16f
    val outX = craftX + craftPanelW - outSize - 16f * s
    val outY = gridStartY + cSlot + cGap
    val result = currentCraftingResult
    val canCraft = result.exists(canCraftCurrent)
    val hoverOut = inRect(mx, my, outX, outY, outSize, outSize)
    rect(outX - 1f * s, outY - 1f * s, outSize + 2f * s, outSize + 2f * s, 0f, 0f, 0f, 0.86f)
    rect(outX, outY, outSize, outSize, if canCraft then (if hoverOut then 0.26f else 0.18f) else 0.08f, if canCraft then (if hoverOut then 0.30f else 0.22f) else 0.09f, if canCraft then (if hoverOut then 0.24f else 0.16f) else 0.11f, 0.95f)
    centeredTextFit(outX + outSize / 2f, outY - 15f * s, "output", 0.62f, 0.66f, 0.74f, 0.44f * s, outSize + 26f * s)
    result.foreach { r =>
      val icon = (outSize * 0.58f).min(27f * s).max(16f)
      renderBlockIcon(r.output, outX + (outSize - icon) / 2f, outY + (outSize - icon) / 2f, icon)
      centeredTextFit(outX + outSize / 2f, outY + outSize + 8f * s, s"${r.outputCount}x ${blockName(r.output)}", if canCraft then 0.80f else 0.45f, if canCraft then 0.86f else 0.48f, if canCraft then 0.72f else 0.50f, 0.48f * s, craftPanelW * 0.46f)
      if hoverOut then renderTooltip(outX + outSize / 2f, outY - 6f * s, r.label)
    }
    if result.isEmpty && normalizeCraftGrid(craftGridBlocks).nonEmpty then
      centeredTextFit(craftX + craftPanelW / 2f, craftY + 178f * s, "no matching recipe", 0.72f, 0.48f, 0.45f, 0.48f * s, craftPanelW - 18f * s)
    else if result.nonEmpty && !canCraft then
      centeredTextFit(craftX + craftPanelW / 2f, craftY + 178f * s, "missing ingredients", 0.72f, 0.48f, 0.45f, 0.48f * s, craftPanelW - 18f * s)

    val hotbarPanelY = py + ph - 104f * s
    drawInventoryHotbar(px + 28f * s, hotbarPanelY, pw - 56f * s, s, mx, my)
    rect(px + 18f * s, py + ph - 54f * s, pw - 36f * s, 1f, 0.30f, 0.30f, 0.35f, 0.20f)
    drawButton(px + 22f * s, py + ph - 46f * s, 130f * s, (34f * s).min(ph * 0.08f).max(28f), "Furnace")
    drawButton(px + pw - 104f * s, py + ph - 46f * s, 82f * s, (34f * s).min(ph * 0.08f).max(28f), "Close")
    if hoveredBlock != null then renderTooltip(hoveredX, hoveredY - 8f * s, blockName(hoveredBlock.asInstanceOf[Block]))
    renderHeldInventoryCursor(mx, my)

  private def renderCatalog(): Unit =
    glDisable(GL_DEPTH_TEST); glDisable(GL_CULL_FACE); setupOrtho()
    dimBackground()
    val cx = framebufferWidth / 2f
    val cy = framebufferHeight / 2f
    val s = uiScale
    val panelW = (700f * s).min(framebufferWidth * 0.94f)
    val panelH = (500f * s).min(framebufferHeight * 0.88f)
    val panelX = cx - panelW / 2f
    val panelY = cy - panelH / 2f
    drawPanel(panelX, panelY, panelW, panelH)
    rect(panelX + 8f * s, panelY + 8f * s, panelW - 16f * s, 36f * s, 0.14f, 0.16f, 0.22f, 0.80f)
    renderTextShadow(panelX + 18f * s, panelY + 16f * s, "Block Catalog", 1f, 0.95f, 0.65f, 1.0f * s)
    drawButton(panelX + panelW - 88f * s, panelY + 12f * s, 72f * s, 28f * s, "X")

    val slotSize = (48f * s).max(34f).min(54f)
    val gridX = panelX + 20f * s
    val gridY = panelY + 52f * s
    val hotbarPanelY = panelY + panelH - 76f * s
    val cols = ((panelW - 54f * s) / slotSize).toInt.max(4).min(12)
    val items = catalogItems
    val visibleRows = ((hotbarPanelY - gridY - 26f * s) / slotSize).toInt.max(1)
    val scrollMax = ((items.length - 1) / cols - visibleRows + 2).max(0)
    if catalogScroll > scrollMax then catalogScroll = scrollMax
    val (mx, my) = mouseFramebufferPos()
    for i <- items.indices.drop(catalogScroll * cols).take(cols * visibleRows) do
      val idx = i - catalogScroll * cols
      val col = idx % cols
      val row = idx / cols
      val sx = gridX + col * slotSize
      val sy = gridY + row * slotSize
      val block = items(i)
      val isSelected = hotbarBlocks.isDefinedAt(selectedBlock) && hotbarBlocks(selectedBlock) == block
      val hover = inRect(mx, my, sx, sy, slotSize, slotSize)
      val base = if hover then 0.13f else 0.09f
      rect(sx - 1f * s, sy - 1f * s, slotSize + 2f * s, slotSize + 2f * s, 0.02f, 0.02f, 0.02f, 0.70f)
      rect(sx, sy, slotSize, slotSize, base, base + 0.01f, base + 0.03f, 0.86f)
      rect(sx + 1f * s, sy + 1f * s, slotSize - 2f * s, 2f * s, 0.12f, 0.14f, 0.16f, 0.16f)
      if isSelected then
        rect(sx - 2f * s, sy - 2f * s, slotSize + 4f * s, 2f * s, 1f, 0.92f, 0.45f, 0.55f)
        rect(sx - 2f * s, sy + slotSize, slotSize + 4f * s, 2f * s, 1f, 0.92f, 0.45f, 0.55f)
        rect(sx - 2f * s, sy - 2f * s, 2f * s, slotSize + 4f * s, 1f, 0.92f, 0.45f, 0.55f)
        rect(sx + slotSize, sy - 2f * s, 2f * s, slotSize + 4f * s, 1f, 0.92f, 0.45f, 0.55f)
      val icon = (slotSize * 0.56f).min(28f * s).max(18f)
      renderBlockIcon(block, sx + (slotSize - icon) / 2f, sy + (slotSize - icon) / 2f, icon)
      if heldInventoryBlock == block then
        rect(sx + 5f * s, sy + slotSize - 8f * s, slotSize - 10f * s, 3f * s, 1f, 0.86f, 0.30f, 0.85f)
    if scrollMax > 0 then
      val scrollBarX = panelX + panelW - 16f * s
      val scrollBarH = hotbarPanelY - gridY - 8f * s
      val thumbH = (scrollBarH / (scrollMax + 1)).max(10f * s)
      val thumbY = panelY + 52f * s + (catalogScroll.toFloat / scrollMax.max(1)) * (scrollBarH - thumbH)
      rect(scrollBarX, panelY + 52f * s, 12f * s, scrollBarH, 0.02f, 0.02f, 0.02f, 0.40f)
      rect(scrollBarX, thumbY, 12f * s, thumbH, 0.35f, 0.40f, 0.50f, 0.70f)
    drawInventoryHotbar(panelX + 20f * s, hotbarPanelY, panelW - 40f * s, s, mx, my)
    renderHeldInventoryCursor(mx, my)
    val infoY = panelY + panelH - 18f * s
    rect(panelX + 12f * s, infoY - 2f * s, panelW - 24f * s, 18f * s, 0.06f, 0.08f, 0.12f, 0.50f)
    centeredTextFit(cx, infoY, "catalog: click block, then click hotbar slot | e or esc closes", 0.55f, 0.60f, 0.70f, 0.56f * s, panelW - 32f * s)

  private def blockName(block: Block): String =
    block.toString.replaceAll("([a-z])([A-Z])", "$1 $2")

  private def inventoryItems: Array[Block] =
    Block.values.filter(b => validHotbarBlock(b) && inventory(b.ordinal) > 0)

  private val inventoryGridSlots = 24

  private def drawInventoryHotbar(panelX: Float, panelY: Float, panelW: Float, s: Float, mx: Float, my: Float): Unit =
    val total = hotbarBlocks.length
    val gap = (5f * s).max(4f)
    val slot = ((panelW - gap * (total - 1)) / total).min(44f * s).max(28f)
    val barW = slot * total + gap * (total - 1)
    val startX = panelX + (panelW - barW) / 2f
    renderTextShadow(panelX, panelY - 19f * s, "Hotbar: click item/slot to assign, mouse wheel cycles in-game", 0.70f, 0.78f, 0.95f, 0.56f * s)
    var i = 0
    while i < total do
      val sx = startX + i * (slot + gap)
      val selected = i == selectedBlock
      val hover = inRect(mx, my, sx, panelY, slot, slot)
      val block = hotbarBlocks(i)
      val hasItem = block != Block.Air && (gameMode == GameMode.Creative || hotbarCounts(i) > 0)
      val base = if hover then 0.14f else 0.085f
      rect(sx - 1f * s, panelY - 1f * s, slot + 2f * s, slot + 2f * s, 0.015f, 0.017f, 0.022f, 0.78f)
      rect(sx, panelY, slot, slot, base, base + 0.01f, base + 0.035f, 0.92f)
      if selected then
        rect(sx - 2f * s, panelY - 2f * s, slot + 4f * s, 2f * s, 1f, 0.92f, 0.42f, 0.55f)
        rect(sx - 2f * s, panelY + slot, slot + 4f * s, 2f * s, 1f, 0.92f, 0.42f, 0.55f)
        rect(sx - 2f * s, panelY - 2f * s, 2f * s, slot + 4f * s, 1f, 0.92f, 0.42f, 0.55f)
        rect(sx + slot, panelY - 2f * s, 2f * s, slot + 4f * s, 1f, 0.92f, 0.42f, 0.55f)
      if hasItem then
        val icon = (slot * 0.58f).min(26f * s).max(16f)
        renderBlockIcon(block, sx + (slot - icon) / 2f, panelY + (slot - icon) / 2f, icon)
        if gameMode == GameMode.Survival then
          val ns = hotbarCounts(i).toString
          val cs = (0.66f * s).max(0.52f).min(slot / 27f)
          val tw = textWidth(ns, cs)
          val tx = (sx + slot - tw - 5f * s).max(sx + 3f * s)
          val ty = panelY + slot - 12f * cs - 2f * s
          rect(tx - 2f * s, ty - 1f * s, tw + 4f * s, 11f * cs + 3f * s, 0f, 0f, 0f, 0.66f)
          renderText(tx, ty, ns, 1f, 1f, 1f, cs)
        if hover && heldInventoryBlock == Block.Air then renderTooltip(sx + slot / 2f, panelY - 6f * s, blockName(block))
      centeredTextFit(sx + slot / 2f, panelY + 3f * s, hotbarLabel(i), 0.96f, 0.96f, 0.98f, 0.54f * s, slot - 6f * s)
      i += 1

  private def renderHeldInventoryCursor(mx: Float, my: Float): Unit =
    if heldInventoryBlock != Block.Air then
      val s = uiScale
      val size = (34f * s).max(24f)
      rect(mx + 10f * s, my + 10f * s, size + 8f * s, size + 8f * s, 0f, 0f, 0f, 0.45f)
      renderBlockIcon(heldInventoryBlock, mx + 14f * s, my + 14f * s, size)
      if gameMode == GameMode.Survival && heldInventoryCount > 1 then
        val ns = heldInventoryCount.toString
        val cs = (0.72f * s).max(0.56f)
        val tw = textWidth(ns, cs)
        renderTextShadow(mx + 14f * s + size - tw - 2f * s, my + 14f * s + size - 12f * cs, ns, 1f, 1f, 1f, cs)
      renderTooltip(mx + 34f * s, my + 12f * s, s"Holding ${blockName(heldInventoryBlock)}")

  private def renderTooltip(anchorX: Float, anchorY: Float, text: String): Unit =
    if text == null || text.isEmpty then return
    val s = uiScale
    val scale = (0.78f * s).max(0.60f)
    val tw = textWidth(text, scale)
    val padX = 7f * s
    val padY = 5f * s
    val boxW = tw + padX * 2f
    val boxH = 22f * scale + 6f * s
    val x = (anchorX - boxW / 2f).max(6f * s).min(framebufferWidth.toFloat - boxW - 6f * s)
    val y = (anchorY - boxH - 4f * s).max(6f * s)
    rect(x + 2f * s, y + 2f * s, boxW, boxH, 0f, 0f, 0f, 0.40f)
    rect(x, y, boxW, boxH, 0.035f, 0.040f, 0.060f, 0.94f)
    rect(x, y, boxW, 1.5f * s, 0.95f, 0.80f, 0.36f, 0.62f)
    renderTextShadow(x + padX, y + padY, text, 0.96f, 0.94f, 0.82f, scale)

  private def catalogItems: Array[Block] =
    Block.values.filter(b => b != Block.Air && b != Block.Water && b != Block.Bedrock && b != Block.FurnaceLit)

  private def renderFurnaceUI(): Unit =
    glDisable(GL_DEPTH_TEST); glDisable(GL_CULL_FACE); setupOrtho()
    dimBackground()
    val cx = framebufferWidth / 2f; val cy = framebufferHeight / 2f; val s = uiScale
    val pw = (680f * s).min(framebufferWidth * 0.94f)
    val ph = (430f * s).min(framebufferHeight * 0.88f)
    val px = cx - pw / 2f; val py = cy - ph / 2f
    drawPanel(px, py, pw, ph)
    centeredTextFit(cx, py + 25f * s, "Furnace", 1f, 0.95f, 0.65f, 1.35f * s, pw - 80f * s)
    rect(px + 28f * s, py + 48f * s, pw - 56f * s, 1f, 0.30f, 0.30f, 0.35f, 0.20f)

    val (mx, my) = mouseFramebufferPos()
    val listX = px + 28f * s
    val listY = py + 82f * s
    val listW = (210f * s).min(pw * 0.34f).max(160f * s)
    val rowH = (38f * s).max(30f)
    rect(listX - 8f * s, listY - 34f * s, listW + 16f * s, rowH * smeltableInputs.length + 48f * s, 0.045f, 0.060f, 0.085f, 0.72f)
    centeredTextFit(listX + listW / 2f, listY - 24f * s, "Choose input", 0.80f, 0.86f, 1f, 0.66f * s, listW)
    val furnaceItems = smeltableInputs.filter(b => smeltResult(b).nonEmpty && totalItemCount(b) > 0)
    for i <- 0 until inventoryGridSlots.min(6) do
      val b = if i < furnaceItems.length then furnaceItems(i) else Block.Air
      val y = listY + i * rowH
      val count = if b == Block.Air then 0 else totalItemCount(b)
      val selected = b != Block.Air && furnaceInput == b
      val hover = inRect(mx, my, listX, y, listW, rowH - 4f * s)
      val br = if selected then 0.30f else if hover then 0.18f else 0.095f
      rect(listX, y, listW, rowH - 4f * s, br, br * 1.06f, br * 1.16f, if b != Block.Air then 0.92f else 0.52f)
      rect(listX + 1f * s, y + 1f * s, listW - 2f * s, 2f * s, 1f, 1f, 1f, 0.06f)
      if b != Block.Air && count > 0 then
        val icon = (rowH * 0.52f).min(22f * s).max(16f)
        renderBlockIcon(b, listX + 8f * s, y + (rowH - icon) / 2f - 2f * s, icon)
        renderTextShadow(listX + 38f * s, y + 8f * s, blockName(b), 0.88f, 0.90f, 0.86f, 0.54f * s)
        centeredTextFit(listX + listW - 28f * s, y + 8f * s, s"x$count", 0.80f, 0.84f, 0.92f, 0.50f * s, 48f * s)

    val workX = listX + listW + 34f * s
    val workW = px + pw - 28f * s - workX
    val slot = (68f * s).min(74f).max(52f)
    def drawFurnaceSlot(x: Float, y: Float, label: String, block: Block, count: Int): Unit =
      rect(x - 1f * s, y - 1f * s, slot + 2f * s, slot + 2f * s, 0.02f, 0.02f, 0.02f, 0.74f)
      rect(x, y, slot, slot, 0.06f, 0.07f, 0.10f, 0.90f)
      rect(x + 2f * s, y + 2f * s, slot - 4f * s, 2f * s, 0.18f, 0.20f, 0.25f, 0.16f)
      centeredTextFit(x + slot / 2f, y + slot + 12f * s, label, 0.68f, 0.74f, 0.86f, 0.52f * s, slot + 30f * s)
      if block != Block.Air && count > 0 then
        val icon = (slot * 0.58f).max(20f)
        renderBlockIcon(block, x + (slot - icon) / 2f, y + (slot - icon) / 2f, icon)
        val c = s"x$count"
        val cs = 0.52f * s
        val tw = textWidth(c, cs)
        rect(x + slot - tw - 7f * s, y + slot - 14f * s, tw + 5f * s, 12f * s, 0f, 0f, 0f, 0.62f)
        renderText(x + slot - tw - 5f * s, y + slot - 13f * s, c, 1f, 1f, 1f, cs)
      else
        centeredTextFit(x + slot / 2f, y + slot / 2f - 5f * s, "empty", 0.34f, 0.38f, 0.48f, 0.48f * s, slot - 8f * s)

    val inputX = workX + 8f * s
    val inputY = py + 94f * s
    val outputX = workX + workW - slot - 8f * s
    val outputY = inputY
    val inputCount = if furnaceInput == Block.Air then 0 else totalItemCount(furnaceInput)
    drawFurnaceSlot(inputX, inputY, "Input", furnaceInput, inputCount)
    drawFurnaceSlot(outputX, outputY, "Output", furnaceOutput, furnaceOutputCount)

    val arrowX = inputX + slot + 30f * s
    val arrowW = (outputX - arrowX - 30f * s).max(80f * s)
    val arrowY = inputY + slot * 0.38f
    rect(arrowX, arrowY, arrowW, 13f * s, 0.02f, 0.02f, 0.025f, 0.72f)
    val progress = (furnaceProgress / 4f).max(0f).min(1f)
    rect(arrowX + 2f * s, arrowY + 2f * s, (arrowW - 4f * s) * progress, 9f * s, 0.95f, 0.58f, 0.16f, 0.82f)
    centeredTextFit(arrowX + arrowW / 2f, arrowY - 16f * s, "smelts instantly per click", 0.50f, 0.56f, 0.68f, 0.46f * s, arrowW + 40f * s)

    val fuelY = py + 212f * s
    rect(workX + 8f * s, fuelY, workW - 16f * s, 58f * s, 0.045f, 0.060f, 0.085f, 0.72f)
    val woodFuelCount = fuelBlocks.filter(_ != Block.Coal).map(totalItemCount).sum
    renderTextShadow(workX + 18f * s, fuelY + 10f * s, s"Fuel: Coal ${totalItemCount(Block.Coal)} | Wood/Planks $woodFuelCount", 0.76f, 0.82f, 0.92f, 0.58f * s)
    val fuelText = if furnaceFuelRemaining > 0f then f"Buffered fuel: $furnaceFuelRemaining%.0f ticks" else "Buffered fuel: empty"
    renderTextShadow(workX + 18f * s, fuelY + 32f * s, fuelText, 0.62f, 0.68f, 0.78f, 0.52f * s)
    val recipeText = furnaceInput match
      case Block.Air => "Pick an input from the list."
      case b => smeltResult(b) match
        case Some((out, amount)) => s"Recipe: ${blockName(b)} -> ${amount}x ${blockName(out)}"
        case None => "That item cannot be smelted."
    centeredTextFit(workX + workW / 2f, py + 294f * s, recipeText, 0.74f, 0.80f, 0.90f, 0.58f * s, workW - 20f * s)

    val bh = (36f * s).min(38f).max(30f)
    val buttonY = py + ph - 52f * s
    val buttonGap = 8f * s
    val totalButtonW = workW - 16f * s
    val btnW = ((totalButtonW - buttonGap * 3f) / 4f).max(72f * s)
    val b0 = workX + 8f * s
    drawButton(b0, buttonY, btnW, bh, "Smelt One", accent = true)
    drawButton(b0 + (btnW + buttonGap), buttonY, btnW, bh, "Smelt All")
    drawButton(b0 + (btnW + buttonGap) * 2f, buttonY, btnW, bh, "Take")
    drawButton(b0 + (btnW + buttonGap) * 3f, buttonY, btnW, bh, "Close")

  private def handleFurnaceClick(mx: Float, my: Float): Unit =
    val cx = framebufferWidth / 2f; val cy = framebufferHeight / 2f; val s = uiScale
    val pw = (680f * s).min(framebufferWidth * 0.94f)
    val ph = (430f * s).min(framebufferHeight * 0.88f)
    val px = cx - pw / 2f; val py = cy - ph / 2f
    val listX = px + 28f * s
    val listY = py + 82f * s
    val listW = (210f * s).min(pw * 0.34f).max(160f * s)
    val rowH = (38f * s).max(30f)
    val furnaceItems = smeltableInputs.filter(b => smeltResult(b).nonEmpty && totalItemCount(b) > 0)
    for i <- 0 until inventoryGridSlots.min(6) do
      val y = listY + i * rowH
      if inRect(mx, my, listX, y, listW, rowH - 4f * s) then
        if i < furnaceItems.length then
          val b = furnaceItems(i)
          furnaceInput = b
          furnaceProgress = 0f
        else furnaceInput = Block.Air
        return

    val workX = listX + listW + 34f * s
    val workW = px + pw - 28f * s - workX
    val slot = (68f * s).min(74f).max(52f)
    val inputX = workX + 8f * s
    val inputY = py + 94f * s
    val outputX = workX + workW - slot - 8f * s
    val outputY = inputY
    if inRect(mx, my, inputX, inputY, slot, slot) then
      if heldInventoryBlock != Block.Air && smeltResult(heldInventoryBlock).nonEmpty && totalItemCount(heldInventoryBlock) > 0 then
        furnaceInput = heldInventoryBlock
        furnaceProgress = 0f
      else if heldInventoryBlock == Block.Air then
        furnaceInput = Block.Air
        furnaceProgress = 0f
      else addChatMessage("That item cannot be smelted")
      return
    if inRect(mx, my, outputX, outputY, slot, slot) then
      takeFurnaceOutput()
      return
    val bh = (36f * s).min(38f).max(30f)
    val buttonY = py + ph - 52f * s
    val buttonGap = 8f * s
    val totalButtonW = workW - 16f * s
    val btnW = ((totalButtonW - buttonGap * 3f) / 4f).max(72f * s)
    val b0 = workX + 8f * s
    if inRect(mx, my, b0, buttonY, btnW, bh) then smeltFurnace()
    else if inRect(mx, my, b0 + (btnW + buttonGap), buttonY, btnW, bh) then smeltAllFurnace()
    else if inRect(mx, my, b0 + (btnW + buttonGap) * 2f, buttonY, btnW, bh) then takeFurnaceOutput()
    else if inRect(mx, my, b0 + (btnW + buttonGap) * 3f, buttonY, btnW, bh) then enterGame()

  private def pickFurnaceInput(): Unit =
    val picked: Option[Block] = smeltableInputs.find(b => totalItemCount(b) > 0)
    picked match
      case Some(b: Block) => furnaceInput = b
      case None => addChatMessage("No smeltable items in inventory")

  private def smeltResult(input: Block): Option[(Block, Int)] = input match
    case Block.Sand => Some((Block.Glass, 1))
    case Block.Clay => Some((Block.Brick, 1))
    case Block.Stone => Some((Block.Brick, 2))
    case Block.Wood | Block.BirchWood | Block.PineWood | Block.AcaciaWood => Some((Block.Coal, 1))
    case Block.IronOre => Some((Block.IronIngot, 1))
    case Block.GoldOre => Some((Block.GoldIngot, 1))
    case _ => None

  private def canConsumeFuelFor(input: Block): Boolean =
    totalItemCount(Block.Coal) > 0 || fuelBlocks.exists(b => b != Block.Coal && totalItemCount(b) > (if b == input then 1 else 0))

  private def consumeFuelFor(input: Block): Boolean =
    if furnaceFuelRemaining > 0f then true
    else if totalItemCount(Block.Coal) > 0 then
      consumeInventory(Block.Coal, 1)
      furnaceFuel = Block.Coal
      furnaceFuelRemaining = 8f
      true
    else
      val picked = fuelBlocks.find(b => b != Block.Coal && totalItemCount(b) > (if b == input then 1 else 0))
      picked match
        case Some(fuel) =>
          consumeInventory(fuel, 1)
          furnaceFuel = fuel
          furnaceFuelRemaining = if fuel == Block.Planks || fuel == Block.BirchPlanks || fuel == Block.PinePlanks || fuel == Block.AcaciaPlanks then 3f else 4f
          true
        case None => false

  private def takeFurnaceOutput(): Unit =
    if furnaceOutput != Block.Air && furnaceOutputCount > 0 then
      val room = (999 - totalItemCount(furnaceOutput)).max(0)
      val moved = math.min(room, furnaceOutputCount)
      if moved > 0 then
        gainItem(furnaceOutput, moved)
        furnaceOutputCount -= moved
        playPlaceSound()
      if furnaceOutputCount <= 0 then
        furnaceOutput = Block.Air
        furnaceOutputCount = 0
      else addChatMessage("Inventory stack is full")

  private def smeltOneInternal(showMessages: Boolean): Boolean =
    if furnaceInput == Block.Air then
      if showMessages then addChatMessage("Choose an input from the furnace list first")
      return false
    if totalItemCount(furnaceInput) <= 0 then
      if showMessages then addChatMessage(s"No ${blockName(furnaceInput)} left")
      furnaceInput = Block.Air
      furnaceProgress = 0f
      return false
    val result = smeltResult(furnaceInput)
    if result.isEmpty then
      if showMessages then addChatMessage("That item cannot be smelted")
      return false
    val (out, amount) = result.get
    if furnaceOutput != Block.Air && furnaceOutput != out then
      if showMessages then addChatMessage("Take the current output first")
      return false
    if furnaceOutputCount + amount > 999 then
      if showMessages then addChatMessage("Furnace output is full")
      return false
    if !consumeFuelFor(furnaceInput) then
      if showMessages then addChatMessage("Need fuel: Coal or Wood")
      return false
    consumeInventory(furnaceInput, 1)
    furnaceFuelRemaining -= 1f
    furnaceProgress = 4f
    furnaceOutput = out
    furnaceOutputCount += amount
    if totalItemCount(furnaceInput) <= 0 then furnaceInput = Block.Air
    if furnaceFuelRemaining <= 0f then
      furnaceFuelRemaining = 0f
      furnaceFuel = Block.Air
    playPlaceSound()
    true

  private def smeltFurnace(): Unit =
    if smeltOneInternal(showMessages = true) then furnaceProgress = 0f

  private def smeltAllFurnace(): Unit =
    var made = 0
    var keepGoing = true
    while keepGoing && made < 256 do
      keepGoing = smeltOneInternal(showMessages = false)
      if keepGoing then made += 1
    furnaceProgress = 0f
    if made == 0 then smeltOneInternal(showMessages = true)
    else addChatMessage(s"Smelted $made item${if made == 1 then "" else "s"}")

  private def drawSlot(x: Float, y: Float, size: Float, block: Block, count: Int): Unit =
    rect(x - 2, y - 2, size + 4, size + 4, 0.06f, 0.06f, 0.06f, 1f)
    rect(x, y, size, size, 0.30f, 0.30f, 0.30f, 1f)
    rect(x + 3, y + 3, size - 6, size - 6, 0.12f, 0.12f, 0.12f, 0.9f)
    if count > 0 then
      val icon = (size * 0.56f).min(24f * uiScale).max(16f)
      renderBlockIcon(block, x + (size - icon) / 2f, y + (size - icon) / 2f, icon)
      val ns = count.toString; val s = uiScale
      val countScale = (1.02f * s).max(0.84f).min(size / 20f)
      val tw = textWidth(ns, countScale); val nx = x + size - tw - 5f * s; val ny = y + size - 12f * countScale - 2f * s
      rect(nx - 2f * s, ny - 1f * s, tw + 4f * s, 11f * countScale + 3f * s, 0f, 0f, 0f, 0.64f)
      renderText(nx, ny, ns, 1f, 1f, 1f, countScale)

  private def drawPanel(x: Float, y: Float, w: Float, h: Float): Unit =
    val s = uiScale
    rect(x + 5f * s, y + 5f * s, w, h, 0f, 0f, 0f, 0.30f)
    rect(x - 1f * s, y - 1f * s, w + 2f * s, h + 2f * s, 0.025f, 0.030f, 0.040f, 0.88f)
    rect(x, y, w, h, 0.095f, 0.115f, 0.165f, 0.96f)
    rect(x + 2f * s, y + 2f * s, w - 4f * s, 2f * s, 0.20f, 0.23f, 0.30f, 0.20f)
    rect(x + 2f * s, y + h - 3f * s, w - 4f * s, 1.5f * s, 0f, 0f, 0f, 0.20f)
    rect(x + 1f * s, y + 1f * s, 1f * s, h - 2f * s, 0.18f, 0.20f, 0.26f, 0.10f)
    rect(x + w - 2f * s, y + 1f * s, 1f * s, h - 2f * s, 0f, 0f, 0f, 0.16f)

  private def uiScale: Float =
    // Slightly larger global UI scale so small labels stay readable without blowing up layouts.
    val byWidth = framebufferWidth.toFloat / 1280f
    val byHeight = framebufferHeight.toFloat / 720f
    math.min(byWidth, byHeight).max(0.98f).min(1.38f)

  private def dimBackground(): Unit = rect(0, 0, framebufferWidth, framebufferHeight, 0f, 0f, 0f, 0.55f)

  private def slider(x: Float, y: Float, w: Float, value: Float): Unit =
    val s = uiScale
    val trackH = 18f * s; val knobW = 14f * s; val knobH = 26f * s
    rect(x, y, w, trackH, 0.02f, 0.02f, 0.02f, 0.80f)
    rect(x + 2 * s, y + 2 * s, w - 4 * s, trackH - 4 * s, 0.20f, 0.22f, 0.26f, 0.85f)
    val fillW = (w - 4 * s - knobW) * value.max(0f).min(1f)
    rect(x + 2 * s, y + 2 * s, fillW.max(0f), trackH - 4 * s, 0.50f, 0.55f, 0.65f, 0.70f)
    val knob = x + 2 * s + fillW
    rect(knob, y - 4 * s, knobW, knobH, 0.85f, 0.88f, 0.95f, 1f)
    rect(knob + 2 * s, y - 2 * s, knobW - 4 * s, knobH - 8 * s, 1f, 1f, 1f, 0.30f)

  private def resetGlArraysAndBuffers(): Unit =
    glBindBuffer(GL_ARRAY_BUFFER, 0)
    glDisableClientState(GL_VERTEX_ARRAY)
    glDisableClientState(GL_COLOR_ARRAY)
    glDisableClientState(GL_TEXTURE_COORD_ARRAY)

  private def rect(x: Float, y: Float, w: Float, h: Float, r: Float, g: Float, b: Float, a: Float): Unit =
    if w <= 0f || h <= 0f || a <= 0f then return
    glBindBuffer(GL_ARRAY_BUFFER, 0)
    glDisable(GL_TEXTURE_2D)
    glEnable(GL_BLEND)
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA)
    glColor4f(r, g, b, a)
    glBegin(GL_QUADS); glVertex2f(x, y); glVertex2f(x + w, y); glVertex2f(x + w, y + h); glVertex2f(x, y + h); glEnd()

  private def drawButton(x: Float, y: Float, w: Float, h: Float, label: String, accent: Boolean = false): Unit =
    val s = uiScale
    val (mx, my) = mouseFramebufferPos()
    val hover = inRect(mx, my, x, y, w, h)
    val base = if accent then 0.36f else 0.275f
    val bright = if hover then base + 0.13f else base
    val textR = if accent then 1f else if hover then 1f else 0.92f
    val textG = if accent then 0.86f else if hover then 1f else 0.92f
    val textB = if accent then 0.36f else if hover then 1f else 0.94f
    rect(x + 3f * s, y + 3f * s, w, h, 0f, 0f, 0f, 0.24f)
    rect(x - 1f * s, y - 1f * s, w + 2f * s, h + 2f * s, 0.02f, 0.025f, 0.035f, 0.88f)
    rect(x, y, w, h, bright, bright * 1.02f, bright * 1.06f, 1f)
    rect(x, y, w, h * 0.46f, (bright + 0.11f).min(0.75f), (bright + 0.11f).min(0.75f), (bright + 0.14f).min(0.78f), 0.44f)
    rect(x + 2f * s, y + 1f * s, w - 4f * s, 2f * s, 1f, 1f, 1f, 0.10f)
    rect(x + 2f * s, y + h - 2f * s, w - 4f * s, 1f * s, 0f, 0f, 0f, 0.22f)
    if hover then
      rect(x - 1f * s, y - 1f * s, w + 2f * s, h + 2f * s, if accent then 0.95f else 0.58f, if accent then 0.78f else 0.60f, if accent then 0.28f else 0.70f, 0.16f)
      rect(x, y, w, h, 1f, 1f, 1f, 0.045f)
    centeredTextBox(x + 8f * s, y + 3f * s, w - 16f * s, h - 6f * s, label, textR, textG, textB, (1.05f * s).max(0.82f).min(h / 12.5f))

  private def renderTextShadow(x: Float, y: Float, text: String, r: Float, g: Float, b: Float, scale: Float = 1f): Unit =
    val o = (1.60f * scale).max(1.05f)
    renderText(x + o, y + o, text, 0f, 0f, 0f, scale)
    renderText(x - o, y, text, 0f, 0f, 0f, scale)
    renderText(x + o, y, text, 0f, 0f, 0f, scale)
    renderText(x, y - o, text, 0f, 0f, 0f, scale)
    renderText(x, y + o, text, 0f, 0f, 0f, scale)
    renderText(x - o, y - o, text, 0f, 0f, 0f, scale)
    renderText(x + o, y - o, text, 0f, 0f, 0f, scale)
    renderText(x - o, y + o, text, 0f, 0f, 0f, scale)
    renderText(x, y, text, r, g, b, scale)

  private def setupOrtho(): Unit =
    resetGlArraysAndBuffers()
    glPolygonMode(GL_FRONT_AND_BACK, GL_FILL)
    glDepthMask(true)
    glDisable(GL_FOG)
    glDisable(GL_CULL_FACE)
    glDisable(GL_DEPTH_TEST)
    glDisable(GL_TEXTURE_2D)
    glEnable(GL_BLEND)
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA)
    glMatrixMode(GL_PROJECTION); glLoadIdentity()
    glOrtho(0, framebufferWidth, framebufferHeight, 0, -1, 1)
    glMatrixMode(GL_MODELVIEW); glLoadIdentity()

  private def onOff(value: Boolean): String = if value then "ON" else "OFF"

  private def changeRenderDistance(delta: Int): Unit =
    val next = (renderDistance + delta).max(minRenderDistance).min(maxRenderDistance)
    if next != renderDistance then
      renderDistance = next
      // Keep existing chunk objects/meshes. Only stream in newly visible chunks and
      // unload chunks that fall outside the new radius. This avoids the old full-world
      // rebuild storm when tapping +/- or dragging the slider quickly.
      syncChunks()

  private def queueChunkMesh(chunk: Chunk): Unit =
    if !chunk.isDisposed && chunk.tryQueueMesh() then
      chunkBuildQueue.add(chunk)

  private var lastChunkX = 0; private var lastChunkZ = 0

  private def syncChunks(): Unit =
    val ccx = chunkCoordPos(camera.x)
    val ccz = chunkCoordPos(camera.z)
    // Keep streaming chunks every tick, not only when the player enters a new chunk.
    // The old logic loaded only a handful of chunks on the first pass, then stopped because
    // `chunks` was no longer empty. That caused temporary dark-blue empty worlds, missing
    // terrain, and players falling/desyncing while multiplayer screens were open.
    lastChunkX = ccx; lastChunkZ = ccz
    loadChunks(ccx, ccz)

  private def terrainReadyNearCamera: Boolean =
    val ccx = chunkCoordPos(camera.x)
    val ccz = chunkCoordPos(camera.z)
    val centerReady = chunks.get((ccx, ccz)).exists(c => !c.isDisposed && c.hasMesh)
    if !centerReady then false
    else
      var readyCount = 0
      var dx = -1
      while dx <= 1 do
        var dz = -1
        while dz <= 1 do
          chunks.get((ccx + dx, ccz + dz)).foreach { c => if !c.isDisposed && c.hasMesh then readyCount += 1 }
          dz += 1
        dx += 1
      readyCount >= 5

  private def centerChunkReady: Boolean =
    val ccx = chunkCoordPos(camera.x)
    val ccz = chunkCoordPos(camera.z)
    chunks.get((ccx, ccz)).exists(c => !c.isDisposed && c.hasMesh)

  private def ensureNearbyMeshesReady(limit: Int): Unit =
    if limit <= 0 then return
    val ccx = chunkCoordPos(camera.x)
    val ccz = chunkCoordPos(camera.z)
    val nearest = chunks.values.toSeq.sortBy { c =>
      val dx = c.cx - ccx; val dz = c.cz - ccz
      dx * dx + dz * dz
    }
    var built = 0
    var i = 0
    while i < nearest.length && built < limit do
      val c = nearest(i)
      if !c.isDisposed && (!c.hasMesh || c.meshReady) then
        if c.buildNowAndUpload() then built += 1
      i += 1

  private def refreshChunkAndNeighbors(cx: Int, cz: Int): Unit =
    val keys = Array((cx, cz), (cx - 1, cz), (cx + 1, cz), (cx, cz - 1), (cx, cz + 1))
    keys.foreach { key =>
      chunks.get(key).foreach { c =>
        c.markDirtyMesh()
        c.meshReady = false
        queueChunkMesh(c)
      }
    }

  private def applyNetworkBlocksToChunk(cx: Int, cz: Int): Unit =
    val chunkOpt = chunks.get((cx, cz))
    if chunkOpt.isEmpty then return
    val chunk = chunkOpt.get
    var changed = false
    def applyOne(wx: Int, wy: Int, wz: Int, block: Block): Unit =
      val lx = wx - cx * Terrain.chunkSize
      val lz = wz - cz * Terrain.chunkSize
      if lx >= 0 && lx < Terrain.chunkSize && wy >= 0 && wy < Terrain.worldHeight && lz >= 0 && lz < Terrain.chunkSize then
        chunk.setBlock(lx, wy, lz, block)
        if block == Block.Water then
          val level = networkWaterLevelOverrides.get((wx, wy, wz)).map(b => (b.toInt & 0xFF).max(1).min(8)).getOrElse(8)
          chunk.setWaterLevel(lx, wy, lz, level)
          waterLevels((wx, wy, wz)) = level.toByte
          markWaterActive(wx, wy, wz)
        else
          chunk.setWaterLevel(lx, wy, lz, 0)
          waterLevels.remove((wx, wy, wz))
        changed = true

    val overrideKeys = networkBlockOverrides.keys.filter { case (wx, _, wz) => chunkCoordBlock(wx) == cx && chunkCoordBlock(wz) == cz }.toList
    overrideKeys.foreach { case (wx, wy, wz) =>
      networkBlockOverrides.get((wx, wy, wz)).foreach(block => applyOne(wx, wy, wz, block))
    }

    val pendingKeys = pendingNetworkBlocks.keys.filter { case (wx, _, wz) => chunkCoordBlock(wx) == cx && chunkCoordBlock(wz) == cz }.toList
    pendingKeys.foreach { case (wx, wy, wz) =>
      pendingNetworkBlocks.remove((wx, wy, wz)).foreach { block =>
        networkBlockOverrides((wx, wy, wz)) = block
        applyOne(wx, wy, wz, block)
      }
    }
    if changed then chunk.markDirtyMesh()

  private def loadChunks(ccx: Int, ccz: Int): Unit =
    val worldRadius = renderDistanceBlocks + Terrain.chunkSize
    val radiusSq = worldRadius * worldRadius
    val chunkRadius = (worldRadius / 16) + 2
    val maxPerFrame = if chunks.isEmpty then 6 else if renderDistance >= 14 then 2 else if renderDistance >= 10 then 3 else 4
    val wanted = collection.mutable.Set.empty[(Int, Int)]
    var loaded = 0
    for ring <- 0 to chunkRadius; dx <- -ring to ring; dz <- -ring to ring do
      if abs(dx) == ring || abs(dz) == ring then
        val wx = (ccx + dx) * 16 + 8; val wz = (ccz + dz) * 16 + 8
        val cx = ccx + dx; val cz = ccz + dz
        val dxb = wx - ccx * 16; val dzb = wz - ccz * 16
        if dxb * dxb + dzb * dzb <= radiusSq then
          wanted += ((cx, cz))
          if !chunks.contains((cx, cz)) && loaded < maxPerFrame then
            val loadedFromDisk = canUseLocalChunkSaves && loadChunkIfSaved(cx, cz)
            if loadedFromDisk then
              applyNetworkBlocksToChunk(cx, cz)
              // Current/adjacent chunks should be ready immediately; far chunks can stream.
              if ring == 0 then chunks.get((cx, cz)).foreach(_.buildNowAndUpload()) else chunks.get((cx, cz)).foreach(queueChunkMesh)
              loaded += 1
            else
              val chunk = Chunk(cx, cz, activeAtlas, terrainGen)
              chunks((cx, cz)) = chunk
              initChunkWaterLevels(cx, cz)
              applyNetworkBlocksToChunk(cx, cz)
              if ring == 0 then chunk.buildNowAndUpload() else queueChunkMesh(chunk)
              loaded += 1
          else if chunks.contains((cx, cz)) then
            val chunk = chunks((cx, cz))
            if !chunk.hasMesh && !chunk.meshReady then
              queueChunkMesh(chunk)
    val toRemove = chunks.keys.filterNot(wanted.contains).toList
    val chunksDir = currentChunksDir
    if canUseLocalChunkSaves then chunksDir.mkdirs()
    toRemove.foreach { key =>
      chunks.get(key).foreach { chunk =>
        if canUseLocalChunkSaves then chunk.save(chunksDir)
        chunk.dispose()
      }
      chunks -= key
    }

  private def startChunkGenThread(): Unit =
    // v18 brings the worker back, but only for CPU mesh building. The Chunk mesher
    // snapshots block arrays/edits and uses revision checks before upload, so stale
    // worker results cannot replace newer meshes. This keeps joining clients from
    // hitching while still avoiding the old race-condition void bug.
    if chunkGenPool != null then return
    chunkGenRunning = true
    val cores = Runtime.getRuntime.availableProcessors().max(1)
    val threads = if cores >= 6 then 2 else 1
    chunkGenPool = java.util.concurrent.Executors.newFixedThreadPool(threads)
    for i <- 0 until threads do
      chunkGenPool.submit(new Runnable:
        override def run(): Unit =
          while chunkGenRunning do
            val chunk = chunkBuildQueue.poll()
            if chunk == null then
              try Thread.sleep(8) catch case _: InterruptedException => ()
            else
              try
                if !chunk.isDisposed then
                  chunk.computeMesh()
                  if chunk.meshReady then chunkUploadQueue.add(chunk)
                  else chunk.releaseMeshQueue()
                else chunk.releaseMeshQueue()
              catch case e: Exception =>
                chunk.releaseMeshQueue()
                e.printStackTrace()
      )

  private def stopChunkGenThread(): Unit =
    chunkGenRunning = false
    val pool = chunkGenPool
    if pool != null then
      pool.shutdown()
      try pool.awaitTermination(3, java.util.concurrent.TimeUnit.SECONDS) catch case _: Exception => ()
    chunkGenPool = null

  private def processChunkWorkMainThread(buildLimit: Int, uploadLimit: Int): Unit =
    // Upload any leftover completed meshes first. This also makes upgrading from an
    // older build safer if a stale queue exists during the first frame.
    var uploaded = 0
    while uploaded < uploadLimit do
      val ready = chunkUploadQueue.poll()
      if ready == null then uploaded = uploadLimit
      else
        val stillLoaded = chunks.get((ready.cx, ready.cz)).contains(ready)
        if stillLoaded && !ready.isDisposed then
          val fresh = ready.uploadMesh()
          if !fresh && !ready.isDisposed then queueChunkMesh(ready)
        else ready.releaseMeshQueue()
        uploaded += 1

    var built = 0
    while built < buildLimit do
      val chunk = chunkBuildQueue.poll()
      if chunk == null then built = buildLimit
      else
        val stillLoaded = chunks.get((chunk.cx, chunk.cz)).contains(chunk)
        if stillLoaded && !chunk.isDisposed then
          try
            chunk.computeMesh()
            if chunk.meshReady then
              val fresh = chunk.uploadMesh()
              if !fresh && !chunk.isDisposed then queueChunkMesh(chunk)
            else chunk.releaseMeshQueue()
          catch case e: Exception =>
            chunk.releaseMeshQueue()
            e.printStackTrace()
          built += 1
        else chunk.releaseMeshQueue()

  private def forceCameraChunkRing(radius: Int): Unit =
    val ccx = chunkCoordPos(camera.x)
    val ccz = chunkCoordPos(camera.z)
    var dx = -radius
    while dx <= radius do
      var dz = -radius
      while dz <= radius do
        val cx = ccx + dx
        val cz = ccz + dz
        val key = (cx, cz)
        val chunk = chunks.get(key) match
          case Some(existing) => existing
          case None =>
            val created = Chunk(cx, cz, activeAtlas, terrainGen)
            chunks(key) = created
            initChunkWaterLevels(cx, cz)
            applyNetworkBlocksToChunk(cx, cz)
            created
        if !chunk.isDisposed && !chunk.hasMesh then chunk.forceBuildNowAndUpload()
        dz += 1
      dx += 1

  private def viewDirection: Vec3 =
    val yawRad = toRadians(yaw); val pitchRad = toRadians(pitch)
    val cp = cos(pitchRad).toFloat; val sp = sin(pitchRad).toFloat
    val sy = sin(yawRad).toFloat; val cy = cos(yawRad).toFloat
    Vec3(sy * cp, -sp, -cy * cp).normalized

  private def raycast(maxDistance: Float = 8f): Option[RayHit] =
    val dir = viewDirection
    if dir.lengthSquared < 0.0001f then return None
    var x = floor(camera.x).toInt
    var y = floor(camera.y).toInt
    var z = floor(camera.z).toInt
    val stepX = if dir.x > 0f then 1 else if dir.x < 0f then -1 else 0
    val stepY = if dir.y > 0f then 1 else if dir.y < 0f then -1 else 0
    val stepZ = if dir.z > 0f then 1 else if dir.z < 0f then -1 else 0
    val inf = Float.PositiveInfinity
    val tDeltaX = if stepX == 0 then inf else abs(1f / dir.x)
    val tDeltaY = if stepY == 0 then inf else abs(1f / dir.y)
    val tDeltaZ = if stepZ == 0 then inf else abs(1f / dir.z)
    var tMaxX = if stepX > 0 then (x + 1f - camera.x) / dir.x else if stepX < 0 then (x.toFloat - camera.x) / dir.x else inf
    var tMaxY = if stepY > 0 then (y + 1f - camera.y) / dir.y else if stepY < 0 then (y.toFloat - camera.y) / dir.y else inf
    var tMaxZ = if stepZ > 0 then (z + 1f - camera.z) / dir.z else if stepZ < 0 then (z.toFloat - camera.z) / dir.z else inf
    if tMaxX < 0f then tMaxX = 0f; if tMaxY < 0f then tMaxY = 0f; if tMaxZ < 0f then tMaxZ = 0f
    var normal = (0, 0, 0); var distance = 0f; var i = 0
    while distance <= maxDistance && i < 200 do
      i += 1
      if activeBlockAt(x, y, z).solid then
        return Some(RayHit((x, y, z), (x + normal._1, y + normal._2, z + normal._3), normal, distance))
      if tMaxX <= tMaxY && tMaxX <= tMaxZ then
        x += stepX; distance = tMaxX; tMaxX += tDeltaX; normal = (-stepX, 0, 0)
      else if tMaxY <= tMaxZ then
        y += stepY; distance = tMaxY; tMaxY += tDeltaY; normal = (0, -stepY, 0)
      else
        z += stepZ; distance = tMaxZ; tMaxZ += tDeltaZ; normal = (0, 0, -stepZ)
    None

  private def sendBlockNetwork(x: Int, y: Int, z: Int, blockId: Byte): Unit =
    if multiplayerMode then
      val block = Block.fromId(blockId)
      networkBlockOverrides((x, y, z)) = block
      if block == Block.Water then networkWaterLevelOverrides((x, y, z)) = waterLevelAt(x, y, z).max(1).min(8).toByte
      else networkWaterLevelOverrides.remove((x, y, z))
      val levelPart = if block == Block.Water then "|" + waterLevelAt(x, y, z).max(1).min(8).toString else ""
      val msg = s"BLOC|$x|$y|$z|$blockId$levelPart"
      if gameClient != null && gameClient.isConnected then gameClient.send(msg)
      else if gameServer != null then gameServer.broadcast(msg)

  private def breakTargetBlock(dropItem: Boolean = true): Unit =
    raycast().foreach { hit =>
      val (x, y, z) = hit.block
      val block = activeBlockAt(x, y, z)
      val event = modManager.fireBlockBreak(ModBlockBreakEvent(modManager.api, playerName, x, y, z, block, cancelled = false, dropItem = dropItem))
      if event.cancelled then return
      if event.dropItem && block != Block.Air && block != Block.Water then gainItem(block, 1)
      setActiveBlock(x, y, z, Block.Air)
      dirtyChunkAt(x, z)
      triggerSandFallAbove(x, y, z)
      playBreakSound()
      sendBlockNetwork(x, y, z, 0)
    }

  private def placeTargetBlock(): Unit =
    raycast().foreach { hit =>
      val (bx, by, bz) = hit.block
      val targetBlock = activeBlockAt(bx, by, bz)
      if targetBlock == Block.Furnace then
        openFurnace()
      else
        val (x, y, z) = hit.place
        val block = hotbarBlocks(selectedBlock.max(0).min(hotbarBlocks.length - 1))
        val hasBlock = block != Block.Air && (gameMode == GameMode.Creative || hotbarCounts(selectedBlock.max(0).min(hotbarCounts.length - 1)) > 0)
        if hasBlock && canPlaceBlockAt(x, y, z) then
          val event = modManager.fireBlockPlace(ModBlockPlaceEvent(modManager.api, playerName, x, y, z, block, cancelled = false))
          if event.cancelled then return
          val finalBlock = event.block
          setActiveBlock(x, y, z, finalBlock)
          if gameMode == GameMode.Survival then consumeSelectedHotbar(1)
          dirtyChunkAt(x, z)
          // Sand currently behaves like a normal block.
          playPlaceSound()
          sendBlockNetwork(x, y, z, finalBlock.ordinal.toByte)
    }

  private def blockIntersectsPlayerBody(x: Int, y: Int, z: Int, pos: Vec3): Boolean =
    val half = 0.36f
    val minX = floor(pos.x - half).toInt; val maxX = floor(pos.x + half).toInt
    val minY = floor(pos.y - 1.70f).toInt; val maxY = floor(pos.y + 0.22f).toInt
    val minZ = floor(pos.z - half).toInt; val maxZ = floor(pos.z + half).toInt
    x >= minX && x <= maxX && y >= minY && y <= maxY && z >= minZ && z <= maxZ

  private def placementBlockedByAnyPlayer(x: Int, y: Int, z: Int): Boolean =
    blockIntersectsPlayerBody(x, y, z, camera) ||
      remotePlayers.values.exists(rp => glfwGetTime() - rp.lastSeen <= 8.0 && blockIntersectsPlayerBody(x, y, z, rp.pos))

  private def canPlaceBlockAt(x: Int, y: Int, z: Int): Boolean =
    !activeBlockAt(x, y, z).solid && !placementBlockedByAnyPlayer(x, y, z)

  private def renderRayLine(): Unit =
    val dir = viewDirection; val end = camera + dir * 8f
    glDisable(GL_DEPTH_TEST); glLineWidth(2f); glColor4f(0f, 1f, 0f, 0.6f)
    glBegin(GL_LINES); glVertex3f(0f, camera.y, 0f); glVertex3f(end.x - camera.x, end.y, end.z - camera.z); glEnd()
    glLineWidth(1f); glEnable(GL_DEPTH_TEST)

  private def renderChunkBorders(): Unit =
    glDisable(GL_DEPTH_TEST); glLineWidth(1f); glColor4f(1f, 0.3f, 0.3f, 0.30f)
    glBegin(GL_LINES)
    chunks.keys.foreach { case (cx, cz) =>
      val x = cx * 16; val z = cz * 16
      val rx = x.toFloat - camera.x; val rz = z.toFloat - camera.z
      glVertex3f(rx, 0f, rz); glVertex3f(rx + 16f, 0f, rz)
      glVertex3f(rx + 16f, 0f, rz); glVertex3f(rx + 16f, 0f, rz + 16f)
      glVertex3f(rx + 16f, 0f, rz + 16f); glVertex3f(rx, 0f, rz + 16f)
      glVertex3f(rx, 0f, rz + 16f); glVertex3f(rx, 0f, rz)
    }
    glEnd(); glEnable(GL_DEPTH_TEST); glLineWidth(1f)

  private def renderTargetOutline(): Unit =
    raycast(8f).foreach { hit =>
      val (x, y, z) = hit.block
      glDisable(GL_DEPTH_TEST)
      val e = 0.005f; val x0 = x - camera.x - e; val x1 = x - camera.x + 1f + e
      val y0 = y - e; val y1 = y + 1f + e; val z0 = z - camera.z - e; val z1 = z - camera.z + 1f + e
      glLineWidth(3f); glBegin(GL_LINES)
      glColor4f(0f, 0f, 0f, 0.5f)
      lineEdges(x0, y0, z0, x1, y1, z1)
      glColor4f(1f, 1f, 1f, 0.8f); glLineWidth(2f)
      lineEdges(x0, y0, z0, x1, y1, z1)
      glEnd(); glLineWidth(1f); glEnable(GL_DEPTH_TEST)
    }

  private def lineEdges(x0: Float, y0: Float, z0: Float, x1: Float, y1: Float, z1: Float): Unit =
    glVertex3f(x0, y0, z0); glVertex3f(x1, y0, z0)
    glVertex3f(x1, y0, z0); glVertex3f(x1, y1, z0)
    glVertex3f(x1, y1, z0); glVertex3f(x0, y1, z0)
    glVertex3f(x0, y1, z0); glVertex3f(x0, y0, z0)
    glVertex3f(x0, y0, z1); glVertex3f(x1, y0, z1)
    glVertex3f(x1, y0, z1); glVertex3f(x1, y1, z1)
    glVertex3f(x1, y1, z1); glVertex3f(x0, y1, z1)
    glVertex3f(x0, y1, z1); glVertex3f(x0, y0, z1)
    glVertex3f(x0, y0, z0); glVertex3f(x0, y0, z1)
    glVertex3f(x1, y0, z0); glVertex3f(x1, y0, z1)
    glVertex3f(x1, y1, z0); glVertex3f(x1, y1, z1)
    glVertex3f(x0, y1, z0); glVertex3f(x0, y1, z1)

  private def initChunkWaterLevels(cx: Int, cz: Int): Unit =
    // Do not register generated oceans/lakes into the dynamic water simulator.
    // Natural water is static terrain. Only player-placed/flowing water enters waterLevels.
    chunks.get((cx, cz)).foreach { chunk =>
      chunk.foreachDynamicWaterCell { (lx, y, lz, level) =>
        val wx = cx * Terrain.chunkSize + lx
        val wz = cz * Terrain.chunkSize + lz
        waterLevels((wx, y, wz)) = level.toByte
        markWaterActive(wx, y, wz)
      }
    }

  private def saveChunk(cx: Int, cz: Int): Unit =
    if !canUseLocalChunkSaves then return
    val chunk = chunks.get((cx, cz))
    chunk.foreach(_.save(currentChunksDir))

  private def loadChunkIfSaved(cx: Int, cz: Int): Boolean =
    if !canUseLocalChunkSaves then return false
    val chunksDir = currentChunksDir
    val file = new java.io.File(chunksDir, s"chunk_${cx}_${cz}.dat")
    if file.exists() then
      try
        val chunk = Chunk(cx, cz, activeAtlas, terrainGen)
        chunk.load(chunksDir)
        chunks((cx, cz)) = chunk
        initChunkWaterLevels(cx, cz)
        applyNetworkBlocksToChunk(cx, cz)
        true
      catch
        case e: Exception =>
          System.err.println(s"Corrupt chunk ignored $cx,$cz: $e")
          try file.renameTo(new java.io.File(chunksDir, s"chunk_${cx}_${cz}.dat.bad")) catch case _: Exception => ()
          chunks -= ((cx, cz))
          false
    else
      false

  private def dirtyChunkAt(x: Int, z: Int): Unit =
    val cx = chunkCoordBlock(x)
    val cz = chunkCoordBlock(z)
    dirtyChunks += ((cx, cz))
    val keys = collection.mutable.Set((cx, cz))
    val lx = x - cx * 16; val lz = z - cz * 16
    if lx == 0 then keys += ((cx - 1, cz))
    else if lx == 15 then keys += ((cx + 1, cz))
    if lz == 0 then keys += ((cx, cz - 1))
    else if lz == 15 then keys += ((cx, cz + 1))
    keys.foreach { key =>
      chunks.get(key).foreach { c =>
        c.markDirtyMesh()
        c.meshReady = false
        queueChunkMesh(c)
      }
    }

  private def dirtyQueued(wx: Int, wz: Int): Unit =
    val cx = chunkCoordBlock(wx)
    val cz = chunkCoordBlock(wz)
    dirtyChunks += ((cx, cz))

  private def flushDirtyChunks(): Unit =
    if dirtyChunks.nonEmpty then
      val allKeys = dirtyChunks.toSet
      dirtyChunks.clear()
      // Performance fix: do not save every dirty/water-touched chunk every frame.
      // Disk saves happen on chunk unload/world quit. Here we only queue mesh rebuilds.
      // Also avoid expanding every dirty chunk to all four neighbors; direct block edits
      // already call dirtyChunkAt(), which handles edge neighbors precisely.
      for key <- allKeys do
        chunks.get(key).foreach { c =>
          c.markDirtyMesh()
          c.meshReady = false
          queueChunkMesh(c)
        }

  private def floatBuffer(values: Float*): FloatBuffer =
    val buffer = BufferUtils.createFloatBuffer(values.length)
    values.foreach(buffer.put); buffer.flip(); buffer

  private def updateFramebufferSize(): Unit =
    val w = BufferUtils.createIntBuffer(1); val h = BufferUtils.createIntBuffer(1)
    glfwGetFramebufferSize(window, w, h)
    framebufferWidth = w.get(0).max(1); framebufferHeight = h.get(0).max(1)


  def modGetBlock(x: Int, y: Int, z: Int): Block = activeBlockAt(x, y, z)
  def modSetBlock(x: Int, y: Int, z: Int, block: Block): Unit =
    val safe = if block == null then Block.Air else block
    setActiveBlock(x, y, z, safe)
    dirtyChunkAt(x, z)
    sendBlockNetwork(x, y, z, safe.ordinal.toByte)
  def modResolveBlock(name: String): Block = modManager.resolveBlockName(name)
  def modAddChatMessage(message: String): Unit = addChatMessage(message)
  def modGiveItem(block: Block, count: Int): Unit = if block != null then gainItem(block, count.max(0))
  def modLocalPlayerName: String = playerName
  def modCamera: Vec3 = camera
  def modTeleportSelf(pos: Vec3): Unit =
    if pos != null then
      camera = pos
      velocity = Vec3(0f, 0f, 0f)
      onGround = false
  def modWorldSeed: Long = worldSeed
  def modWorldName: String = worldName
  def modHealth: Float = playerHealth
  def modSetHealth(value: Float): Unit = playerHealth = value.max(0f).min(20f)
  def modFood: Float = playerFood
  def modSetFood(value: Float): Unit = playerFood = value.max(0f).min(20f)
  def modCanUseCheats: Boolean = canUseCheatAuthority && worldCheatsEnabled
  def modIsOpped(name: String): Boolean = isOppedName(name)
  def modFramebufferWidth: Float = framebufferWidth.toFloat
  def modFramebufferHeight: Float = framebufferHeight.toFloat
  def modUiScale: Float = uiScale
  def modScreenName: String = screen.toString.toLowerCase
  def modMouseX: Float = modLastMouseX
  def modMouseY: Float = modLastMouseY
  def modLeftDown: Boolean = modLeftDownNow
  def modLeftClicked: Boolean = modLeftClickedThisFrame
  def modCursorMode: Boolean = modUiCursorMode
  def modSetCursorMode(enabled: Boolean): Unit =
    modUiCursorMode = enabled
    modGuiLeftWasDown = false
    modLeftClickedThisFrame = false
    if window != 0 && screen == Screen.Playing then
      glfwSetInputMode(window, GLFW_CURSOR, if enabled then GLFW_CURSOR_NORMAL else GLFW_CURSOR_DISABLED)
      firstMouse = true
  def modDrawRect(x: Float, y: Float, w: Float, h: Float, r: Float, g: Float, b: Float, a: Float): Unit =
    glDisable(GL_DEPTH_TEST); glDisable(GL_CULL_FACE); setupOrtho(); rect(x, y, w, h, r, g, b, a)
  def modDrawText(x: Float, y: Float, text: String, r: Float, g: Float, b: Float, scale: Float): Unit =
    glDisable(GL_DEPTH_TEST); glDisable(GL_CULL_FACE); setupOrtho(); renderText(x, y, Option(text).getOrElse(""), r, g, b, scale.max(0.1f))
  def modDrawTextShadow(x: Float, y: Float, text: String, r: Float, g: Float, b: Float, scale: Float): Unit =
    glDisable(GL_DEPTH_TEST); glDisable(GL_CULL_FACE); setupOrtho(); renderTextShadow(x, y, Option(text).getOrElse(""), r, g, b, scale.max(0.1f))
  def modDrawButton(x: Float, y: Float, w: Float, h: Float, label: String): Unit =
    glDisable(GL_DEPTH_TEST); glDisable(GL_CULL_FACE); setupOrtho(); drawButton(x, y, w, h, Option(label).getOrElse(""))
  def modInRect(mx: Float, my: Float, x: Float, y: Float, w: Float, h: Float): Boolean = inRect(mx, my, x, y, w, h)


  private def activeWorld: Nothing = throw IllegalStateException("World no longer used - use activeBlockAt")

  private def activeAtlas: TextureAtlas =
    val atlas = textureAtlas
    if atlas == null then throw IllegalStateException("Texture atlas is not initialized")
    atlas
