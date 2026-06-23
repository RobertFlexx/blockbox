//> using scala "3.8.3"
//> using dep "org.lwjgl:lwjgl:3.4.1"
//> using dep "org.lwjgl:lwjgl-glfw:3.4.1"
//> using dep "org.lwjgl:lwjgl-opengl:3.4.1"
//> using dep "org.lwjgl:lwjgl-stb:3.4.1"

package blockbox

import org.lwjgl.BufferUtils
import org.lwjgl.glfw.GLFW.*
import org.lwjgl.glfw.GLFWErrorCallback
import org.lwjgl.glfw.GLFWFramebufferSizeCallback
import org.lwjgl.glfw.GLFWKeyCallback
import org.lwjgl.glfw.GLFWCharCallback
import org.lwjgl.opengl.GL
import org.lwjgl.opengl.GL11.*
import org.lwjgl.opengl.GL15.*
import org.lwjgl.stb.STBEasyFont
import org.lwjgl.system.MemoryUtil.NULL

import java.nio.FloatBuffer
import java.io.*
import java.net.*
import java.util.concurrent.LinkedBlockingQueue
import javax.sound.sampled.AudioFormat
import javax.sound.sampled.AudioSystem
import scala.collection.mutable.{ArrayBuffer, Queue}
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
  case MainMenu, CreateWorld, Settings, Playing, Paused, Inventory, Catalog, FurnaceUI, JoinGame, HostGame

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
  def id: Byte = ordinal.toByte
object Block:
  private val valuesArray = values
  def fromId(id: Byte): Block =
    val i = id.toInt & 0xFF
    if i >= 0 && i < valuesArray.length then valuesArray(i) else Air

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
          val g = if ((x ^ y) & 1) == 0 then 148 else 172
          val g2 = if (x * 3 + y * 7) % 5 < 2 then g - 18 else g
          (52 + (x * 3) % 12, g2, 36 + (y * 2) % 16, 255)
        case FaceKind.Bottom => dirtPixel(x, y, 23)
        case _ =>
          if y < 4 then
            val shade = if ((x + y) & 1) == 0 then 0 else -10
            (82 + shade, 162 + shade, 52 + shade, 255)
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
      // Clean, readable water texture: calmer than noise soup, but still animated-looking in motion.
      val n1 = noise(x, y, 61)
      val n2 = noise(x + 9, y + 3, 62)
      val wave = ((sin((x + n1 * 0.02f) * 0.85f) + cos((y + n2 * 0.02f) * 0.70f)) * 0.5f + 1f) * 0.5f
      val stripe = if ((x + y * 2 + n1 / 48) % 13) == 0 then 8 else 0
      val sparkle = if n1 > 246 || n2 > 250 then 12 else 0
      val r = (18 + stripe / 4 + sparkle / 4).max(10).min(58)
      val g = (88 + (wave * 22f).toInt + stripe / 2 + sparkle / 4).max(64).min(140)
      val b = (180 + (wave * 34f).toInt + stripe + sparkle).max(145).min(245)
      val alpha = if face == FaceKind.Top then 154 else 128
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
    case Block.Planks =>
      val seamY = y == 3 || y == 7 || y == 11
      val seamX = x == 0 || x == 15
      val plankBase = if seamY || seamX then (92, 60, 28) else (168, 116, 58)
      vary(plankBase, 12, noise(x, y, 81), 255)
    case Block.Leaves =>
      val hole = (x * 5 + y * 3) % 9 < 2 && x > 1 && x < 14 && y > 1 && y < 14
      val dark = (x * 7 + y * 11) % 13 < 5
      val base = if dark then (30, 80, 28) else (48, 132, 40)
      val (r, g, b, _) = vary(base, 15, noise(x, y, 93), if hole then 80 else 230)
      (r, g, b, if hole then 80 else 230)
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

  private def valueNoise2D(x: Float, z: Float, base: Int): Float =
    val xi = floor(x).toInt; val zi = floor(z).toInt
    val xf = x - xi; val zf = z - zi
    val u = xf * xf * (3f - 2f * xf)
    val v = zf * zf * (3f - 2f * zf)
    val a = hash(xi, zi, 0, base) * 2f - 1f
    val b = hash(xi + 1, zi, 0, base) * 2f - 1f
    val c = hash(xi, zi + 1, 0, base) * 2f - 1f
    val d = hash(xi + 1, zi + 1, 0, base) * 2f - 1f
    val x1 = a + (b - a) * u
    val x2 = c + (d - c) * u
    x1 + (x2 - x1) * v

  private def fbm2D(x: Float, z: Float, octaves: Int, base: Int): Float =
    var total = 0f; var amp = 1f; var freq = 1f; var norm = 0f
    for _ <- 0 until octaves do
      total += amp * valueNoise2D(x * freq, z * freq, base)
      norm += amp; amp *= 0.52f; freq *= 2.05f
    total / norm

  private def valueNoise3D(x: Float, y: Float, z: Float, base: Int): Float =
    val xi = floor(x).toInt; val yi = floor(y).toInt; val zi = floor(z).toInt
    val xf = x - xi; val yf = y - yi; val zf = z - zi
    val u = xf * xf * (3f - 2f * xf)
    val v = yf * yf * (3f - 2f * yf)
    val w = zf * zf * (3f - 2f * zf)
    val aaa = hash(xi, yi, zi, base) * 2f - 1f; val baa = hash(xi+1, yi, zi, base) * 2f - 1f
    val aba = hash(xi, yi+1, zi, base) * 2f - 1f; val bba = hash(xi+1, yi+1, zi, base) * 2f - 1f
    val aab = hash(xi, yi, zi+1, base) * 2f - 1f; val bab = hash(xi+1, yi, zi+1, base) * 2f - 1f
    val abb = hash(xi, yi+1, zi+1, base) * 2f - 1f; val bbb = hash(xi+1, yi+1, zi+1, base) * 2f - 1f
    val x11 = aaa + (baa - aaa) * u; val x12 = aba + (bba - aba) * u
    val x21 = aab + (bab - aab) * u; val x22 = abb + (bbb - abb) * u
    val y1 = x11 + (x12 - x11) * v; val y2 = x21 + (x22 - x21) * v
    y1 + (y2 - y1) * w

  private def ridgedNoise(x: Float, z: Float, base: Int, octaves: Int): Float =
    var total = 0f; var amp = 1f; var freq = 1f; var norm = 0f
    for _ <- 0 until octaves do
      val n = 1f - abs(valueNoise2D(x * freq, z * freq, base))
      total += amp * n * n
      norm += amp; amp *= 0.5f; freq *= 2.2f
    total / norm

  private def caveNoise(x: Int, y: Int, z: Int, h: Int): Boolean =
    if y <= 2 || y >= Terrain.worldHeight - 3 then return false
    // Do not carve caves right under the surface. Previous builds could expose
    // huge ore-speckled vertical slices on beaches/cliffs that looked like chunk
    // corruption. Caves still exist deeper down, just not as accidental surface wounds.
    if y >= h - 18 then return false
    val depth = (h - y).max(0).toFloat
    val depthBoost = (depth / 40f).min(1f) * 0.20f
    val n3d = valueNoise3D(x * 0.020f, y * 0.015f, z * 0.020f, 99)
    val n3d2 = valueNoise3D(x * 0.035f, y * 0.025f, z * 0.035f, 199)
    val combined = n3d * 0.35f + n3d2 * 0.25f + depthBoost
    val blob = combined > 0.50f && combined < 0.72f
    val tunnel = abs(valueNoise2D(x * 0.012f, z * 0.012f, 333))
    val tunnelY = abs(valueNoise2D(x * 0.008f, z * 0.008f, 444))
    val tunnelCarve = tunnel < 0.030f && tunnelY > 0.10f && tunnelY < 0.90f && y > 6 && y < Terrain.seaLevel + 40 && depth > 20f
    val tunnel2 = abs(valueNoise2D((x + 50) * 0.020f, (z - 30) * 0.020f, 555))
    val tunnelY2 = abs(valueNoise2D(x * 0.015f, z * 0.015f, 666))
    val tunnelCarve2 = tunnel2 < 0.020f && tunnelY2 > 0.15f && tunnelY2 < 0.85f && y > 10 && y < Terrain.seaLevel + 30 && depth > 24f
    val deepPocket = depth > 25f && valueNoise3D(x * 0.014f, y * 0.010f, z * 0.014f, 777) > 0.60f
    blob || tunnelCarve || tunnelCarve2 || deepPocket

  def heightAt(x: Int, z: Int): Int =
    val fx = x.toFloat; val fz = z.toFloat
    val continent = fbm2D(fx * 0.005f, fz * 0.005f, 5, 1)
    val moisture = fbm2D((fx + 500) * 0.015f, (fz - 300) * 0.015f, 4, 51)
    val biome = smooth01((fbm2D((fx + 900) * 0.004f, (fz - 400) * 0.004f, 3, 101) + 1f) * 0.5f)
    val detail = fbm2D(fx * 0.030f, fz * 0.030f, 3, 151) * 1.5f
    val mountains = ridgedNoise((fx + 410) * 0.009f, (fz - 70) * 0.009f, 201, 5) * 32f * smooth01(biome * 1.5f)
    val river = abs(fbm2D((fx - 90) * 0.007f, (fz + 120) * 0.007f, 4, 251))
    val riverCut = smooth01(max(0f, 0.18f - river) / 0.18f) * 12f
    val coastSmoothing = smooth01((continent + 1f) * 1.5f.max(0f).min(1f))
    val h = (Terrain.seaLevel + 8 + continent * 10f + detail + mountains - riverCut).round.max(1).min(Terrain.worldHeight - 4)
    h

  def surfaceBlock(x: Int, y: Int, z: Int, h: Int): Block =
    val moisture = fbm2D((x + 500) * 0.015f, (z - 300) * 0.015f, 4, 51)
    val cold = h > 60 || fbm2D((x + 50) * 0.007f, (z + 50) * 0.007f, 3, 301) > 0.50f
    val beach = h <= Terrain.seaLevel + 2
    val desert = moisture < -0.35f && !cold && !beach
    if cold then Block.Snow
    else if beach || desert then Block.Sand
    else Block.Grass

  def fillBlock(x: Int, y: Int, z: Int, h: Int): Block =
    val moisture = fbm2D((x + 500) * 0.015f, (z - 300) * 0.015f, 4, 51)
    val beach = h <= Terrain.seaLevel + 2
    val desert = moisture < -0.35f && !beach
    if y == h then surfaceBlock(x, y, z, h)
    else if y > h - 18 then (if beach || desert then Block.Sand else Block.Dirt)
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
    if h <= Terrain.seaLevel + 1 || surface == Block.Sand then return None
    val chance = h > 55 && hash(x, z, 0, 1001) > 0.998f
    if chance then Some((x, h + 1, z))
    else None

  def treeAt(x: Int, z: Int, h: Int, surface: Block): Option[(Int, Int, Int, Int)] =
    if h <= Terrain.seaLevel + 2 || surface == Block.Sand then return None
    val treeChance = hash(x, z, 0, 19)
    val height = if surface == Block.Snow then 6 else 4 + (hash(x, z, 0, 41) * 4).toInt
    if treeChance > 0.975f then Some((x, h + 1, z, height))
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
    val heights = Array.ofDim[Int](Terrain.chunkSize, Terrain.chunkSize)
    for lx <- 0 until Terrain.chunkSize do
      val wx = baseX + lx
      for lz <- 0 until Terrain.chunkSize do
        val wz = baseZ + lz
        heights(lx)(lz) = heightAt(wx, wz)
    for lx <- 0 until Terrain.chunkSize do
      val wx = baseX + lx
      for lz <- 0 until Terrain.chunkSize do
        val wz = baseZ + lz
        val h = heights(lx)(lz)
        val moisture = fbm2D((wx + 500) * 0.015f, (wz - 300) * 0.015f, 3, 51)
        val cold = h > 60 || fbm2D((wx + 50) * 0.007f, (wz + 50) * 0.007f, 2, 301) > 0.50f
        val beach = h <= Terrain.seaLevel + 2
        val desert = moisture < -0.35f && !cold && !beach
        val surface = if cold then Block.Snow else if beach || desert then Block.Sand else Block.Grass
        val shallow = if beach || desert then Block.Sand else Block.Dirt
        for y <- 1 until Terrain.worldHeight do
          if y <= h then
            val block =
              if y == h then surface
              else if y > h - 18 then shallow
              else if y < 6 && h > Terrain.seaLevel then Block.Stone
              else if y < h - 28 && y < 12 && hash(wx, y, wz, 17) > 0.92f then Block.Clay
              else if y < h - 28 && y < 60 && hash(wx, y, wz, 37) > 0.88f then Block.Coal
              else if y < h - 28 && y < 48 && hash(wx, y, wz, 71) > 0.91f then Block.Copper
              else if y < h - 28 && y < 40 && hash(wx, y, wz, 81) > 0.955f then Block.IronOre
              else if y < h - 28 && y < 30 && hash(wx, y, wz, 91) > 0.97f then Block.GoldOre
              else if y < h - 28 && y < 16 && hash(wx, y, wz, 101) > 0.985f then Block.Diamond
              else if y <= 4 then Block.Bedrock
              else Block.Stone
            val cave = y < h - 18 && caveNoise(wx, y, wz, h)
            if !cave then blocks(idx(lx, y, lz)) = block.id
          else if y <= Terrain.seaLevel && y > h then
            blocks(idx(lx, y, lz)) = Block.Water.id
    for wx <- baseX - 4 to endX + 4; wz <- baseZ - 4 to endZ + 4 do
      val h = heightAt(wx, wz)
      val surface = surfaceBlock(wx, h, wz, h)
      treeAt(wx, wz, h, surface).foreach { case (tx, ty, tz, th) =>
        placeTreeWorld(blocks, baseX, baseZ, tx, ty, tz, th)
      }
      furnaceAt(wx, wz, h, surface).foreach { case (fx, fy, fz) =>
        val lx = fx - baseX; val lz = fz - baseZ
        if lx >= 0 && lx < Terrain.chunkSize && lz >= 0 && lz < Terrain.chunkSize && fy >= 0 && fy < Terrain.worldHeight then
          blocks(idx(lx, fy, lz)) = Block.Furnace.id
      }
    // Do not do chunk-local "floating cleanup" here. Treating outside-of-chunk
    // as air made edge columns disagree with their neighbors and caused visible
    // holes / chunk artifacts. Caves are allowed to make overhangs; the mesher
    // now handles chunk borders correctly instead.
    blocks

  private def placeTreeWorld(blocks: Array[Byte], baseX: Int, baseZ: Int, x: Int, y: Int, z: Int, height: Int): Unit =
    def set(wx: Int, wy: Int, wz: Int, block: Block): Unit =
      val lx = wx - baseX; val lz = wz - baseZ
      if lx >= 0 && lx < Terrain.chunkSize && lz >= 0 && lz < Terrain.chunkSize && wy >= 0 && wy < Terrain.worldHeight then
        blocks(idx(lx, wy, lz)) = block.id
    for dy <- 0 until height do set(x, y + dy, z, Block.Wood)
    val leafStart = height - 2
    val leafSpread = if height >= 6 then 4 else 3
    for dx <- -leafSpread to leafSpread; dz <- -leafSpread to leafSpread; dy <- leafStart to height + 1 do
      val dist = abs(dx) + abs(dz) + max(0, dy - height)
      if dist <= leafSpread + 1 && !(dx == 0 && dz == 0 && dy > height - 1) then
        set(x + dx, y + dy, z + dz, Block.Leaves)

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

  def getBlock(lx: Int, y: Int, lz: Int): Block =
    edits.synchronized { edits.get((lx, y, lz)) } match
      case Some(b) => b
      case None =>
        if lx >= 0 && lx < Terrain.chunkSize && y >= 0 && y < Terrain.worldHeight && lz >= 0 && lz < Terrain.chunkSize then
          Block.fromId(blocks((y * Terrain.chunkSize + lz) * Terrain.chunkSize + lx))
        else Block.Air

  def setBlock(lx: Int, y: Int, lz: Int, block: Block): Unit =
    if lx >= 0 && lx < Terrain.chunkSize && y >= 0 && y < Terrain.worldHeight && lz >= 0 && lz < Terrain.chunkSize then
      val i = (y * Terrain.chunkSize + lz) * Terrain.chunkSize + lx
      val oldId = blocks(i)
      blocks(i) = block.id
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
    val editsSnapshot = edits.synchronized { edits.clone() }
    def snapshotBlock(lx: Int, y: Int, lz: Int): Block =
      editsSnapshot.get((lx, y, lz)) match
        case Some(b) => b
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
    def isVisible(nlx: Int, ny: Int, nlz: Int, block: Block): Boolean =
      val nb = localBlock(nlx, ny, nlz)
      if nb == Block.Air then true
      else if block == Block.Water then nb != Block.Water && !nb.solid
      else if nb == Block.Water then true
      else if nb.cutout then block != nb
      else nb.translucent && nb != block
    for lx <- 0 until Terrain.chunkSize; lz <- 0 until Terrain.chunkSize; y <- 0 until Terrain.worldHeight do
      val block = snapshotBlock(lx, y, lz)
      if block != Block.Air then
        val baseXv = baseX; val baseZv = baseZ; val atlasRef = atlas
        val fx = (baseXv + lx).toFloat; val fy = y.toFloat; val fz = (baseZv + lz).toFloat
        // Water geometry must stay flat and conservative. Per-block height ripples and
        // internal translucent side faces look like broken chunk seams when many water
        // blocks overlap. Keep water movement in the texture/simulation, not in the mesh.
        val topY = if block == Block.Water then fy + 0.965f else fy + 1f
        val yNorm = y.toFloat / Terrain.worldHeight.toFloat
        val ambient = 0.45f + yNorm * 0.10f
        def addFace(shade: Float, corners: Array[(Float, Float, Float, Float, Float)], kind: FaceKind): Unit =
          val buf = if block.cutout then cutoutVerts
            else if block == Block.Water then waterVerts
            else if block.translucent then translucentVerts
            else opaqueVerts
          val light = if block == Block.Water then (0.55f + yNorm * 0.22f) * (0.82f + shade * 0.18f) else shade * ambient
          val idx = Array(0, 1, 2, 2, 3, 0)
          for i <- idx do
            val (cfx, cfy, cfz, tu, tv) = corners(i)
            val (u, v) = atlasRef.uv(block, kind, tu, tv)
            val alpha = block match
              case Block.Water => 0.52f
              case Block.Glass => 0.68f
              case _ => 1f
            buf += cfx; buf += cfy; buf += cfz
            buf += light; buf += light; buf += light; buf += alpha
            buf += u; buf += v
        if block == Block.Water then
          // Render only the visible water surface. Drawing every water side/bottom face
          // through transparent blending creates the ugly blue slabs/grids seen in lakes
          // and oceans. Source bodies and flowing water still exist physically; the mesh
          // just avoids rendering hidden internal faces.
          if localBlock(lx, y + 1, lz) != Block.Water then
            addFace(1.00f, Array((fx, topY, fz, 0f, 0f), (fx + 1, topY, fz, 1f, 0f), (fx + 1, topY, fz + 1, 1f, 1f), (fx, topY, fz + 1, 0f, 1f)), FaceKind.Top)
        else
          if isVisible(lx, y + 1, lz, block) then
            addFace(1.00f, Array((fx, topY, fz, 0f, 0f), (fx + 1, topY, fz, 1f, 0f), (fx + 1, topY, fz + 1, 1f, 1f), (fx, topY, fz + 1, 0f, 1f)), FaceKind.Top)
          if isVisible(lx, y - 1, lz, block) then
            addFace(0.38f, Array((fx, fy, fz + 1, 0f, 0f), (fx + 1, fy, fz + 1, 1f, 0f), (fx + 1, fy, fz, 1f, 1f), (fx, fy, fz, 0f, 1f)), FaceKind.Bottom)
          if isVisible(lx + 1, y, lz, block) then
            addFace(0.82f, Array((fx + 1, fy, fz, 0f, 1f), (fx + 1, fy, fz + 1, 1f, 1f), (fx + 1, topY, fz + 1, 1f, 0f), (fx + 1, topY, fz, 0f, 0f)), FaceKind.East)
          if isVisible(lx - 1, y, lz, block) then
            addFace(0.55f, Array((fx, fy, fz + 1, 0f, 1f), (fx, fy, fz, 1f, 1f), (fx, topY, fz, 1f, 0f), (fx, topY, fz + 1, 0f, 0f)), FaceKind.West)
          if isVisible(lx, y, lz + 1, block) then
            addFace(0.74f, Array((fx + 1, fy, fz + 1, 0f, 1f), (fx, fy, fz + 1, 1f, 1f), (fx, topY, fz + 1, 1f, 0f), (fx + 1, topY, fz + 1, 0f, 0f)), FaceKind.South)
          if isVisible(lx, y, lz - 1, block) then
            addFace(0.52f, Array((fx, fy, fz, 0f, 1f), (fx + 1, fy, fz, 1f, 1f), (fx + 1, topY, fz, 1f, 0f), (fx, topY, fz, 0f, 0f)), FaceKind.North)
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

  // Batched rendering: caller manages state, chunks just draw their VBOs
  def drawOpaque(): Unit = if opaqueCount > 0 then drawBuffer(opaqueVbo, opaqueCount)
  def drawCutout(): Unit = if cutoutCount > 0 then drawBuffer(cutoutVbo, cutoutCount)
  def drawTranslucent(): Unit = if translucentCount > 0 then drawBuffer(translucentVbo, translucentCount)
  def drawWater(): Unit = if waterCount > 0 then drawBuffer(waterVbo, waterCount)

  def destroy(): Unit =
    if opaqueVbo != 0 then glDeleteBuffers(opaqueVbo); opaqueVbo = 0; opaqueCount = 0
    if cutoutVbo != 0 then glDeleteBuffers(cutoutVbo); cutoutVbo = 0; cutoutCount = 0
    if translucentVbo != 0 then glDeleteBuffers(translucentVbo); translucentVbo = 0; translucentCount = 0
    if waterVbo != 0 then glDeleteBuffers(waterVbo); waterVbo = 0; waterCount = 0

  def save(dir: java.io.File): Unit =
    dir.mkdirs()
    val file = new java.io.File(dir, s"chunk_${cx}_${cz}.dat")
    val out = new java.io.DataOutputStream(new java.io.BufferedOutputStream(new java.io.FileOutputStream(file)))
    try
      out.write(blocks)
      val editCopy = edits.synchronized { edits.toList }
      out.writeInt(editCopy.size)
      editCopy.foreach { case ((lx, ly, lz), block) =>
        out.writeInt(lx); out.writeInt(ly); out.writeInt(lz)
        out.writeByte(block.id)
      }
    finally out.close()

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

  private def drawBuffer(vbo: Int, count: Int): Unit =
    if vbo != 0 && count > 0 then
      val stride = floatsPerVertex * 4
      glBindBuffer(GL_ARRAY_BUFFER, vbo)
      glVertexPointer(3, GL_FLOAT, stride, 0L)
      glColorPointer(4, GL_FLOAT, stride, 12L)
      glTexCoordPointer(2, GL_FLOAT, stride, 28L)
      glDrawArrays(GL_TRIANGLES, 0, count)

// Multiplayer networking
final class GameServer(
  port: Int,
  worldSeed: Long,
  spawn: Vec3,
  hostName: String,
  hostColorId: Int,
  onBlockChange: (Int, Int, Int, Byte) => Unit,
  worldSnapshot: () => Seq[(Int, Int, Int, Byte)],
  hostPosition: () => Vec3
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
            onlineName =
              if parts.length >= 2 && parts(1).nonEmpty then
                parts(1).filter(ch => ch.isLetterOrDigit || ch == '_' || ch == '-').take(16)
              else "Player"
            if onlineName.isEmpty then onlineName = "Player"
            colorId = assignColor()
            val worldMsg = "WORLD|" + worldSeed.toString + "|" + f"${spawn.x}%.3f" + "|" + f"${spawn.y}%.3f" + "|" + f"${spawn.z}%.3f" + "|" + colorId.toString + "|" + safeHostName + "|" + safeHostColor.toString
            send(worldMsg)
            send("PLAYERS|" + activePlayerTokensSnapshot.mkString("|"))
            val snapshot =
              try worldSnapshot().take(50000)
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
                val clean = "BLOC|" + x + "|" + y + "|" + z + "|" + blockId.toString
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

  def connect(playerName: String): Boolean =
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
      out.println(s"HELO|$playerName")
      out.flush()

      val firstLine = in.readLine()
      if firstLine == null then
        lastError = "Server closed the connection before sending world data."
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
  private var breakingBlock: (Int, Int, Int) | Null = null
  private var breakingProgress = 0f
  private val placeableBlocks = Array(Block.Grass, Block.Dirt, Block.Stone, Block.Sand, Block.Wood, Block.Planks, Block.Leaves, Block.Brick, Block.Glass, Block.Snow, Block.Clay, Block.Coal, Block.Copper, Block.IronOre, Block.GoldOre, Block.Diamond, Block.Furnace)
  private val inventory = Array.fill(Block.values.length)(0)
  private var selectedBlock = 0
  private var catalogScroll = 0
  private var craftingScroll = 0
  private var furnaceInput: Block = Block.Air
  private var furnaceFuel: Block = Block.Air
  private var furnaceProgress = 0f
  private var furnaceFuelRemaining = 0f
  private var furnaceOutput: Block = Block.Air
  private var furnaceOutputCount = 0
  private val smeltableInputs = Array(Block.Sand, Block.Clay, Block.Stone, Block.Wood, Block.IronOre, Block.GoldOre)
  private val fuelBlocks = Array(Block.Coal, Block.Wood)
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
  private val knownPlayerNames = scala.collection.mutable.HashSet.empty[String]
  private val playerColors = scala.collection.mutable.HashMap.empty[String, Int]
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
  private var renderDistance = 40
  private var worldSeed = 0L
  private var createWorldMode = GameMode.Survival
  private var createWorldCheats = false
  private var worldCheatsEnabled = false
  private var terrainGen = TerrainGenerator(0L)
  private var chunks = scala.collection.mutable.AnyRefMap.empty[(Int, Int), Chunk]
  private var textureAtlas: TextureAtlas | Null = null
  private var playerHealth = 20f
  private var playerFood = 20f
  private var errorCallback: GLFWErrorCallback | Null = null
  private var worldName = "World"
  private var customSeedInput = ""
  private var enterCustomSeed = false
  private var saveDirectory: String = ""
  private var settingsReturnTo: Screen = Screen.MainMenu
  private var pauseEscReturnsToGame = true
  private var chatOpen = false
  private var chatInput = ""
  private var suppressNextChatChar = false
  private val chatMessages = ArrayBuffer.empty[(String, Float)]
  private val chatHistory = ArrayBuffer.empty[String]
  private var chatHistoryIndex = -1
  private val sandFallQueue = Queue.empty[(Int, Int, Int)]
  private val maxSandUpdatesPerFrame = 6
  private val waterLevels = scala.collection.mutable.HashMap.empty[(Int, Int, Int), Byte]
  private val waterFlowQueue = Queue.empty[(Int, Int, Int)]
  private var waterFlowTimer = 0f
  private val waterFlowInterval = 0.12f
  private val maxWaterUpdatesPerFrame = 16
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

  def run(): Unit =
    try
      initWindow()
      applyWorldSeed(freshWorldSeed(), freshWorldName = false)
      val spawn = findSpawn()
      camera = spawn
      lastTime = glfwGetTime()
      startChunkGenThread()
      while !glfwWindowShouldClose(window) do loop()
    finally
      stopChunkGenThread()
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
    else if screen == Screen.JoinGame then
      if codepoint >= 32 && codepoint <= 126 then
        val ch = codepoint.toChar
        if joinNameFocused then appendPlayerNameChar(ch) else joinIpInput = (joinIpInput + ch).take(64)
    else if screen == Screen.HostGame then
      if codepoint >= 32 && codepoint <= 126 then appendPlayerNameChar(codepoint.toChar)
    else if enterCustomSeed then
      if codepoint >= 32 && codepoint <= 126 then
        customSeedInput += codepoint.toChar

  private def onKey(key: Int): Unit =
    if key == GLFW_KEY_F11 then toggleFullscreen()
    else screen match
      case Screen.MainMenu =>
        if key == GLFW_KEY_ENTER then screen = Screen.CreateWorld
        else if key == GLFW_KEY_S then screen = Screen.Settings
        else if key == GLFW_KEY_L then loadWorld()
        else if key == GLFW_KEY_ESCAPE then glfwSetWindowShouldClose(window, true)
      case Screen.CreateWorld =>
        if key == GLFW_KEY_ENTER then startNewWorld()
        else if key == GLFW_KEY_R then
          previewRandomSeed()
        else if key == GLFW_KEY_ESCAPE then
          enterCustomSeed = false
          screen = Screen.MainMenu
        else if key == GLFW_KEY_C then enterCustomSeed = !enterCustomSeed
        else if key == GLFW_KEY_M then createWorldMode = if createWorldMode == GameMode.Survival then GameMode.Creative else GameMode.Survival
        else if key == GLFW_KEY_H then createWorldCheats = !createWorldCheats
        else if key == GLFW_KEY_BACKSPACE && enterCustomSeed && customSeedInput.nonEmpty then
          customSeedInput = customSeedInput.init
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
        else if key == GLFW_KEY_LEFT || key == GLFW_KEY_MINUS then changeRenderDistance(-8)
        else if key == GLFW_KEY_RIGHT || key == GLFW_KEY_EQUAL then changeRenderDistance(8)
        else if key == GLFW_KEY_M then toggleGameMode()
        else if key == GLFW_KEY_P then pauseEscReturnsToGame = !pauseEscReturnsToGame
      case Screen.Playing =>
        if chatOpen then
          if key == GLFW_KEY_ENTER then
            submitChat()
          else if key == GLFW_KEY_ESCAPE then
            chatOpen = false; chatInput = ""; chatHistoryIndex = -1
          else if key == GLFW_KEY_BACKSPACE && chatInput.nonEmpty then
            chatInput = chatInput.init
          else if key == GLFW_KEY_UP then
            if chatHistory.nonEmpty then
              chatHistoryIndex = (chatHistoryIndex - 1).max(0)
              chatInput = chatHistory(chatHistory.length - 1 - chatHistoryIndex)
          else if key == GLFW_KEY_DOWN then
            if chatHistoryIndex > 0 then
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
        else if key == GLFW_KEY_0 && placeableBlocks.length >= 10 then selectedBlock = 9
        else if key == GLFW_KEY_M then toggleGameMode()
        else if key == GLFW_KEY_F3 then debugMode = !debugMode
        else if key == GLFW_KEY_F4 then
          wireframeMode = !wireframeMode
          if wireframeMode then glPolygonMode(GL_FRONT_AND_BACK, GL_LINE) else glPolygonMode(GL_FRONT_AND_BACK, GL_FILL)
        else if key == GLFW_KEY_F5 then showChunkBorders = !showChunkBorders
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
        if key == GLFW_KEY_ENTER then hostGame()
        else if key == GLFW_KEY_ESCAPE then screen = Screen.MainMenu
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
        val next = (32 + value * (96 - 32)).round / 8 * 8
        if next != renderDistance then
          renderDistance = next.max(32).min(96)
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
    else if inRect(mx, my, bx, ys(3), bw, bh) then loadWorld()
    else if inRect(mx, my, bx, ys(4), bw, bh) then
      screen = Screen.Settings
      settingsReturnTo = Screen.MainMenu
    else if inRect(mx, my, bx, ys(5), bw, bh) then glfwSetWindowShouldClose(window, true)

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
    val modeY = pY + 142f * s
    val cheatsY = pY + 188f * s
    if inRect(mx, my, settingX, modeY, settingW, settingH) then
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
    val display = if shown.isEmpty then "..." else shown
    centeredTextFit(x + w / 2f, y + h / 2f - 6f * s, display, 0.92f, 0.96f, 1f, (0.98f * s).max(0.86f), w - 18f * s)

  private def renderHostGame(): Unit =
    glDisable(GL_DEPTH_TEST); glDisable(GL_CULL_FACE); setupOrtho()
    val w = framebufferWidth.toFloat; val h = framebufferHeight.toFloat; val cx = w / 2f; val s = uiScale
    glBegin(GL_QUADS)
    glColor4f(0.045f, 0.055f, 0.115f, 1f); glVertex2f(0, 0); glVertex2f(w, 0)
    glColor4f(0.12f, 0.20f, 0.34f, 1f); glVertex2f(w, h); glVertex2f(0, h)
    glEnd()
    rect(0, h * 0.62f, w, h * 0.38f, 0.06f, 0.18f, 0.06f, 0.88f)
    val pW = (500f * s).min(w * 0.90f); val pH = (285f * s).min(h * 0.82f)
    val pX = cx - pW / 2f; val pY = h / 2f - pH / 2f
    drawPanel(pX, pY, pW, pH)
    centeredTextFit(cx, pY + 34f * s, "HOST LAN GAME", 1f, 0.90f, 0.55f, 1.95f * s, pW - 60f * s)
    rect(pX + 40f * s, pY + 64f * s, pW - 80f * s, 1f, 0.30f, 0.30f, 0.35f, 0.30f)
    val fieldW = pW - 96f * s; val fieldH = 36f * s; val fieldX = pX + 48f * s
    renderNameField(fieldX, pY + 112f * s, fieldW, fieldH, "Online name", playerName, hostNameFocused)
    val hsr = if hostStatusError then 1.0f else 0.52f
    val hsg = if hostStatusError then 0.36f else 0.70f
    val hsb = if hostStatusError then 0.30f else 0.82f
    centeredTextFit(cx, pY + 170f * s, hostStatusMessage, hsr, hsg, hsb, 0.58f * s, pW - 72f * s)
    val bw = (210f * s).min(pW - 90f * s); val bh = (40f * s).min(42f).max(30f)
    drawButton(cx - bw / 2f, pY + pH - 92f * s, bw, bh, "Start Hosted World", accent = true)
    drawButton(cx - bw / 2f, pY + pH - 44f * s, bw, bh, "Back")

  private def handleHostClick(mx: Float, my: Float): Unit =
    val w = framebufferWidth.toFloat; val h = framebufferHeight.toFloat; val cx = w / 2f; val s = uiScale
    val pW = (500f * s).min(w * 0.90f); val pH = (285f * s).min(h * 0.82f)
    val pX = cx - pW / 2f; val pY = h / 2f - pH / 2f
    val fieldW = pW - 96f * s; val fieldH = 36f * s; val fieldX = pX + 48f * s
    val bw = (210f * s).min(pW - 90f * s); val bh = (40f * s).min(42f).max(30f)
    if inRect(mx, my, fieldX, pY + 112f * s, fieldW, fieldH) then hostNameFocused = true
    else if inRect(mx, my, cx - bw / 2f, pY + pH - 92f * s, bw, bh) then hostGame()
    else if inRect(mx, my, cx - bw / 2f, pY + pH - 44f * s, bw, bh) then screen = Screen.MainMenu

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
    else if inRect(mx, my, settingX, pY + 285 * s, 440 * s, 28 * s) then fastMove = !fastMove
    else if inRect(mx, my, settingX, pY + 325 * s, 440 * s, 28 * s) then
      vsync = !vsync; glfwSwapInterval(if vsync then 1 else 0)
    else if inRect(mx, my, settingX, pY + 365 * s, 440 * s, 28 * s) then toggleGameMode()
    else if inRect(mx, my, settingX, pY + 405 * s, 440 * s, 28 * s) then soundEnabled = !soundEnabled
    else if inRect(mx, my, settingX, pY + 445 * s, 440 * s, 28 * s) then toggleFullscreen()
    else if inRect(mx, my, settingX, pY + 485 * s, 440 * s, 28 * s) then pauseEscReturnsToGame = !pauseEscReturnsToGame
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
    val gridW = pw * 0.56f
    val cols = 6
    val gap = (6f * s).max(4f)
    val slot = ((gridW - gap * (cols - 1)) / cols).min(44f * s).max(30f)
    val invX = px + 28f * s
    val invY = py + 76f * s
    val items = inventoryItems
    for i <- items.indices do
      val col = i % cols
      val row = i / cols
      val sx = invX + col * (slot + gap)
      val sy = invY + row * (slot + gap)
      if inRect(mx, my, sx, sy, slot, slot) then
        val hotbarIndex = placeableBlocks.indexOf(items(i))
        if hotbarIndex >= 0 then selectedBlock = hotbarIndex
        return

    val craftPanelW = (pw - gridW - 78f * s).max(210f * s)
    val craftX = px + pw - 28f * s - craftPanelW
    val craftY = py + 76f * s
    val craftSlotH = (58f * s).min(ph / 9f).max(38f * s)
    val maxCraftVisible = ((ph - 146f * s) / craftSlotH).toInt.max(2)
    for i <- craftingRecipes.indices.drop(craftingScroll).take(maxCraftVisible) do
      val by = craftY + (i - craftingScroll) * craftSlotH
      if inRect(mx, my, craftX, by, craftPanelW, craftSlotH - 5f * s) then
        tryCraft(i)
        return

    val bottomH = (34f * s).min(ph * 0.08f).max(28f)
    if inRect(mx, my, px + 22f * s, py + ph - 46f * s, 130f * s, bottomH) then openFurnace()
    else if inRect(mx, my, px + pw - 104f * s, py + ph - 46f * s, 82f * s, bottomH) then enterGame()

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
      enterGame()
      return
    val slotSize = (48f * s).max(34f).min(54f)
    val gridX = panelX + 20f * s
    val gridY = panelY + 52f * s
    val cols = ((panelW - 54f * s) / slotSize).toInt.max(4).min(12)
    val items = catalogItems
    val visibleRows = ((panelH - 90f * s) / slotSize).toInt.max(1)
    val scrollMax = ((items.length - 1) / cols - visibleRows + 2).max(0)
    if catalogScroll > scrollMax then catalogScroll = scrollMax
    for i <- items.indices.drop(catalogScroll * cols).take(cols * visibleRows) do
      val idx = i - catalogScroll * cols
      val col = idx % cols
      val row = idx / cols
      val sx = gridX + col * slotSize
      val sy = gridY + row * slotSize
      if inRect(mx, my, sx, sy, slotSize, slotSize) then
        if gameMode == GameMode.Creative then inventory(items(i).ordinal) += 64
        val hotbarIndex = placeableBlocks.indexOf(items(i))
        if hotbarIndex >= 0 then selectedBlock = hotbarIndex
        return
    val scrollUp = inRect(mx, my, panelX + panelW - 18f * s, panelY + 50f * s, 14f * s, 20f * s)
    val scrollDown = inRect(mx, my, panelX + panelW - 18f * s, panelY + panelH - 55f * s, 14f * s, 20f * s)
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
    applyWorldSeed(chooseCreateSeed(), freshWorldName = true)
    clearLoadedChunks(saveFirst = false)
    camera = findSpawn()
    velocity = Vec3(0f, 0f, 0f)
    onGround = false
    yaw = 0f
    pitch = 0f
    gameMode = createWorldMode
    worldCheatsEnabled = createWorldCheats
    resetInventory()
    enterCustomSeed = false
    startGame()

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
    applyWorldSeed(chooseCreateSeed(), freshWorldName = true)
    clearLoadedChunks(saveFirst = false)
    camera = findSpawn()
    velocity = Vec3(0f, 0f, 0f)
    onGround = false
    yaw = 0f
    pitch = 0f
    gameMode = createWorldMode
    worldCheatsEnabled = createWorldCheats
    resetInventory()
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
            setActiveBlock(x, y, z, block)
            dirtyChunkAt(x, z),
        () => snapshotWorldEditsForNetwork(),
        () => camera
      )
      gameServer = server
      verifyLocalServer(server.localPort) match
        case Right(_) =>
          gameClient = null
          multiplayerMode = true
          knownPlayerNames.clear(); knownPlayerNames += playerName
          playerColors.clear()
          rememberPlayerColor(playerName, localColorId)
          hostStatusError = false
          hostStatusMessage = s"Hosting as $playerName. Join locally with 127.0.0.1:${server.localPort}, or share your LAN/ZeroTier IP."
          addChatMessage(s"Hosting world as $playerName on port ${server.localPort}")
          startGame()
        case Left(error) =>
          stopNetworking()
          hostStatusError = true
          hostStatusMessage = error
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
        joinStatusMessage = error
        gameClient = null
        screen = Screen.JoinGame
        glfwSetInputMode(window, GLFW_CURSOR, GLFW_CURSOR_NORMAL)
      case Right((host, port)) =>
        joinStatusError = false
        joinStatusMessage = s"Connecting to $host:$port ..."
        val client = GameClient(host, port)
        gameClient = client
        if client.connect(playerName) then
          multiplayerMode = true
          knownPlayerNames.clear(); knownPlayerNames += playerName
          playerColors.clear()
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
    pendingNetworkBlocks.clear()
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
    firstMouse = true
    glfwSetInputMode(window, GLFW_CURSOR, GLFW_CURSOR_DISABLED)

  private def enterGame(): Unit = startGame()

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

  private def snapshotWorldEditsForNetwork(): Seq[(Int, Int, Int, Byte)] =
    chunks.values.toSeq.flatMap { chunk =>
      val bx = chunk.baseX
      val bz = chunk.baseZ
      chunk.edits.synchronized {
        chunk.edits.toList.map { case ((lx, ly, lz), block) => (bx + lx, ly, bz + lz, block.id) }
      }
    }.distinct

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
    chunks.values.foreach(_.dispose())
    chunks.clear()

  private def saveWorld(): Unit =
    if !canUseLocalChunkSaves then return
    try
      val dir = currentWorldDir
      dir.mkdirs()
      val chunksDir = new java.io.File(dir, "chunks")
      chunksDir.mkdirs()
      val meta = new java.io.DataOutputStream(new java.io.FileOutputStream(new java.io.File(dir, "world.dat")))
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
      meta.close()
    catch case e: Exception => System.err.println(s"Save failed: $e")

  private def loadWorld(): Unit =
    try
      val dir = new java.io.File("worlds")
      val worlds = dir.listFiles().filter(_.isDirectory).toList
      worlds.headOption.foreach { worldDir =>
        worldName = worldDir.getName
        val meta = new java.io.DataInputStream(new java.io.FileInputStream(new java.io.File(worldDir, "world.dat")))
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
        val chunksDir = new java.io.File(worldDir, "chunks")
        val chunkCount = meta.readInt()
        for _ <- 0 until chunkCount do
          val cx = meta.readInt(); val cz = meta.readInt()
          loadChunkIfSaved(cx, cz)
        meta.close()
        enterGame()
      }
    catch case e: Exception => System.err.println(s"Load failed: $e")

  private def resetInventory(): Unit =
    java.util.Arrays.fill(inventory, 0)
    if gameMode == GameMode.Creative then
      for b <- placeableBlocks do inventory(b.ordinal) = 64
      inventory(Block.Coal.ordinal) = inventory(Block.Coal.ordinal).max(64)
    furnaceInput = Block.Air
    furnaceFuel = Block.Air
    furnaceProgress = 0f
    furnaceFuelRemaining = 0f
    furnaceOutput = Block.Air
    furnaceOutputCount = 0

  private type Recipe = (Block, Int, Block, Int, String)
  private val craftingRecipes: Array[Recipe] = Array(
    (Block.Wood, 1, Block.Planks, 4, "Wood -> 4 Planks"),
    (Block.Stone, 3, Block.Brick, 6, "Stone -> 6 Brick"),
    (Block.Coal, 1, Block.Coal, 4, "Coal -> 4 Coal (compress)"),
    (Block.Dirt, 2, Block.Grass, 1, "2 Dirt -> Grass"),
    (Block.Sand, 2, Block.Glass, 2, "2 Sand -> 2 Glass"),
    (Block.Copper, 2, Block.Copper, 1, "2 Copper -> Block"),
  )

  private def tryCraft(index: Int): Unit =
    if index >= 0 && index < craftingRecipes.length then
      val (input, inputCount, output, outputCount, _) = craftingRecipes(index)
      if inventory(input.ordinal) >= inputCount then
        inventory(input.ordinal) -= inputCount
        inventory(output.ordinal) += outputCount
        playPlaceSound()

  private def submitChat(): Unit =
    val text = chatInput.trim
    if text.nonEmpty then
      chatHistory += chatInput
      chatHistoryIndex = -1
      if text.startsWith("/") then parseCommand(text)
      else sendChatMessage(text)
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

  private def parseCommand(cmd: String): Unit =
    val trimmed = cmd.trim
    if trimmed.startsWith("/") then
      val parts = trimmed.drop(1).split("\\s+", 2)
      val command = parts(0).toLowerCase
      val isCheat = command == "gamemode" || command == "gm" || command == "timeset" || command == "fly"
      if isCheat && !worldCheatsEnabled then
        addChatMessage("Cheats are not enabled for this world")
        return
      command match
        case "gamemode" | "gm" =>
          val mode = if parts.length > 1 then parts(1).toLowerCase else ""
          mode match
            case "survival" | "s" | "0" => toggleTo(GameMode.Survival)
            case "creative" | "c" | "1" => toggleTo(GameMode.Creative)
            case _ => addChatMessage(s"Usage: /gamemode <survival|creative>")
        case "timeset" =>
          val when = if parts.length > 1 then parts(1).toLowerCase else ""
          val currentTime = gameTime
          val currentPhase = ((currentTime * 0.018f) % (2f * Pi.toFloat)) / (2f * Pi.toFloat)
          when match
            case "day" =>
              val targetPhase = 0.50f
              val diff = (targetPhase - currentPhase + 1f) % 1f
              timeOverride = Some(currentTime + diff / 0.018f * Pi.toFloat)
              addChatMessage("Set time to day")
            case "night" =>
              val targetPhase = 0.0f
              val diff = (targetPhase - currentPhase + 1f) % 1f
              timeOverride = Some(currentTime + diff / 0.018f * Pi.toFloat)
              addChatMessage("Set time to night")
            case "reset" =>
              timeOverride = None
              addChatMessage("Time override cleared")
            case _ => addChatMessage(s"Usage: /timeset <day|night|reset>")
        case "fly" =>
          if gameMode == GameMode.Survival then
            addChatMessage("Flight is only available in Creative mode")
          else
            flyEnabled = !flyEnabled
            if flyEnabled then
              velocity = Vec3(0f, 0f, 0f)
              addChatMessage("Flight enabled")
            else
              onGround = true
              velocity = Vec3(0f, 0f, 0f)
              addChatMessage("Flight disabled")
        case "help" =>
          addChatMessage("Commands: /gamemode, /gm, /timeset <day|night|reset>, /fly, /cheats, /help")
        case "cheats" =>
          worldCheatsEnabled = !worldCheatsEnabled
          addChatMessage(s"Cheats: ${onOff(worldCheatsEnabled)}")
        case _ =>
          addChatMessage(s"Unknown command: /${parts(0)}")
    else
      sendChatMessage(trimmed)

  private def toggleTo(mode: GameMode): Unit =
    gameMode = mode
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
              onGround = true
              velocity = Vec3(0f, 0f, 0f)
              addChatMessage("Flight disabled")
          lastSpacePressTime = now
        if flyEnabled then
          val speed = if fastMove then 16f else 9f
          if down(GLFW_KEY_SPACE) then move += Vec3(0, 1, 0)
          if down(GLFW_KEY_LEFT_SHIFT) then move -= Vec3(0, 1, 0)
          camera = camera + move.normalized * speed * dt
        else
          val speed = if fastMove then 16f else 9f
          val hMove = move.normalized * speed * dt
          movePlayer(hMove.x, 0f, hMove.z)
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
          else if swimming then if sprinting then 5.0f else 3.5f
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
    updateWater(dt)
    updateBubbles(dt)
    updateSandFalling()
    updateSandParticles(dt)
    processChunkWorkMainThread(0, 12)
    flushDirtyChunks()
    sendPlayerPositionNetwork()
    handleMouseButtons()

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
        if abs(velocity.y) < 0.3f then onGround = velocity.y >= -0.1f
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

  private def wl(x: Int, y: Int, z: Int): Int = waterLevels.get((x, y, z)) match
    case Some(b) => b.toInt & 0xFF
    case None => 0

  private def updateWater(dt: Float): Unit =
    waterFlowTimer += dt
    if waterFlowTimer < waterFlowInterval then return
    waterFlowTimer = 0f
    var processed = 0
    val ccx = chunkCoordPos(camera.x)
    val ccz = chunkCoordPos(camera.z)
    val chunkReach = (renderDistance / Terrain.chunkSize + 4).max(6)

    // Repair obvious sea/lake surface holes near the player so natural bodies stay filled.
    val repairRadius = 18
    var rx = floor(camera.x).toInt - repairRadius
    while rx <= floor(camera.x).toInt + repairRadius && processed < maxWaterUpdatesPerFrame / 3 do
      var rz = floor(camera.z).toInt - repairRadius
      while rz <= floor(camera.z).toInt + repairRadius && processed < maxWaterUpdatesPerFrame / 3 do
        val key = (rx, Terrain.seaLevel, rz)
        if !waterLevels.contains(key) && activeBlockAt(rx, Terrain.seaLevel, rz) == Block.Water then
          waterLevels(key) = 7.toByte
          processed += 1
        rz += 3
      rx += 3

    val nearby = ArrayBuffer.empty[(Int, Int, Int, Int)]
    for ((wx, wy, wz), levelRaw) <- waterLevels do
      val wcx = chunkCoordBlock(wx); val wcz = chunkCoordBlock(wz)
      if abs(wcx - ccx) <= chunkReach && abs(wcz - ccz) <= chunkReach then
        val level = levelRaw.toInt & 0xFF
        if level > 0 && activeBlockAt(wx, wy, wz) == Block.Water then nearby += ((wx, wy, wz, level))

    def isNaturalSource(wx: Int, wy: Int, wz: Int, level: Int): Boolean =
      level >= 7 && wy <= Terrain.seaLevel && activeBlockAt(wx, wy, wz) == Block.Water

    var wi = 0
    while wi < nearby.length && processed < maxWaterUpdatesPerFrame do
      val (wx, wy, wz, level) = nearby(wi); wi += 1
      val source = isNaturalSource(wx, wy, wz, level)
      val below = activeBlockAt(wx, wy - 1, wz)

      if below == Block.Air then
        val downLevel = if source then 7 else level.max(1)
        setActiveBlock(wx, wy - 1, wz, Block.Water)
        waterLevels((wx, wy - 1, wz)) = downLevel.toByte
        if !source then
          val remaining = level - 1
          if remaining <= 0 then
            waterLevels.remove((wx, wy, wz)); setActiveBlock(wx, wy, wz, Block.Air)
          else waterLevels((wx, wy, wz)) = remaining.toByte
        dirtyQueued(wx, wz); processed += 1
      else if below == Block.Water && wl(wx, wy - 1, wz) < 7 then
        val belowLevel = wl(wx, wy - 1, wz)
        val add = (7 - belowLevel).min(if source then 7 else level)
        if add > 0 then
          waterLevels((wx, wy - 1, wz)) = (belowLevel + add).min(7).toByte
          if !source then
            val remaining = level - add
            if remaining <= 0 then
              waterLevels.remove((wx, wy, wz)); setActiveBlock(wx, wy, wz, Block.Air)
            else waterLevels((wx, wy, wz)) = remaining.toByte
          dirtyQueued(wx, wz); processed += 1
      else
        val spreadLevel = if source then 7 else level - 1
        if spreadLevel > 0 then
          val rot = ((wx * 31 + wz * 17 + wy * 13) & 3)
          val baseNeighbors = Array((1,0), (-1,0), (0,1), (0,-1))
          var ni = 0
          while ni < 4 && processed < maxWaterUpdatesPerFrame do
            val d = baseNeighbors((ni + rot) & 3); ni += 1
            val nx = wx + d._1; val nz = wz + d._2
            val nBlock = activeBlockAt(nx, wy, nz)
            if nBlock == Block.Air then
              setActiveBlock(nx, wy, nz, Block.Water)
              waterLevels((nx, wy, nz)) = spreadLevel.toByte
              if !source then
                val remaining = (wl(wx, wy, wz) - 1).max(0)
                if remaining <= 0 then
                  waterLevels.remove((wx, wy, wz)); setActiveBlock(wx, wy, wz, Block.Air)
                else waterLevels((wx, wy, wz)) = remaining.toByte
              dirtyQueued(nx, nz); dirtyQueued(wx, wz); processed += 1
            else if nBlock == Block.Water then
              val nLevel = wl(nx, wy, nz)
              val cur = wl(wx, wy, wz)
              if cur > nLevel + 1 && nLevel < 7 then
                waterLevels((nx, wy, nz)) = (nLevel + 1).min(7).toByte
                if !source then waterLevels((wx, wy, wz)) = (cur - 1).max(1).toByte
                dirtyQueued(nx, nz); dirtyQueued(wx, wz); processed += 1

    val toRemove = ArrayBuffer.empty[(Int, Int, Int)]
    for ((wx, wy, wz), levelRaw) <- waterLevels do
      val level = levelRaw.toInt & 0xFF
      if level <= 0 || activeBlockAt(wx, wy, wz) != Block.Water then toRemove += ((wx, wy, wz))
    for key <- toRemove do waterLevels.remove(key)

  private def updateSandFalling(): Unit =
    sandFallQueue.clear()

  private def visibleSandFall(x: Float, y: Float, z: Float, startY: Float): Unit =
    ()

  private def updateSandParticles(dt: Float): Unit =
    fallingSandParticles.clear()

  private def blockHardness(block: Block): Float = block match
    case Block.Leaves | Block.Snow => 0.18f
    case Block.Dirt | Block.Sand | Block.Grass => 0.42f
    case Block.Wood | Block.Planks => 0.75f
    case Block.Glass => 0.35f
    case Block.Stone | Block.Coal | Block.Copper | Block.Clay | Block.IronOre => 1.15f
    case Block.GoldOre => 1.50f
    case Block.Brick | Block.Furnace | Block.FurnaceLit => 1.35f
    case Block.Diamond => 2.50f
    case Block.Bedrock => 9999f
    case _ => 0.5f

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
        onGround = false
      else if dy < 0f then
        onGround = true
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
      chunk.setBlock(lx, y, lz, block)
      if block == Block.Water then waterLevels((x, y, z)) = 7
      else if block != Block.Water then waterLevels.remove((x, y, z))
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
              waterLevels.clear()
              clearLoadedChunks(saveFirst = false)
            camera = Vec3(sx, sy, sz)
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
            val cx = chunkCoordBlock(x); val cz = chunkCoordBlock(z)
            if chunks.contains((cx, cz)) then
              setActiveBlock(x, y, z, block)
              dirtyChunkAt(x, z)
            else
              pendingNetworkBlocks((x, y, z)) = block
        else if line.startsWith("SNAPBEGIN|") || line.startsWith("SNAPEND") then
          ()
        else if line.startsWith("POS|") then
          if parts.length >= 7 then
            val name = networkSafeName(parts(1))
            if name.nonEmpty then knownPlayerNames += name
            if name != playerName then
              val x = parts(2).toFloat; val y = parts(3).toFloat; val z = parts(4).toFloat
              val pyaw = parts(5).toFloat; val ppitch = parts(6).toFloat
              val colorId = if parts.length >= 8 then normalizeColorId(parts(7).toInt) else colorForPlayer(name)
              rememberPlayerColor(name, colorId)
              remotePlayers.get(name) match
                case Some(rp) =>
                  rp.pos = Vec3(x, y, z); rp.yaw = pyaw; rp.pitch = ppitch; rp.lastSeen = glfwGetTime(); rp.colorId = colorId
                case None => remotePlayers(name) = RemotePlayer(name, Vec3(x, y, z), pyaw, ppitch, glfwGetTime(), colorId)
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
    stale.foreach(remotePlayers.remove)

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
        if debugMode then renderDebugOverlay()
      case Screen.MainMenu => renderMainMenu()
      case Screen.CreateWorld => renderCreateWorld()
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
      val visibleDist = renderDistance.toFloat
      glFogi(GL_FOG_MODE, GL_LINEAR)
      glFogf(GL_FOG_START, visibleDist * 0.65f)
      glFogf(GL_FOG_END, visibleDist * 0.95f)
    glFogfv(GL_FOG_COLOR, floatBuffer(fogR, fogG, fogB, 1f))
    glMatrixMode(GL_PROJECTION); glLoadIdentity()
    val aspect = framebufferWidth.toDouble / framebufferHeight.toDouble
    val near = 0.08; val far = (renderDistance + 24).toDouble
    val sprintFov = if screen == Screen.Playing then
      val moving = down(GLFW_KEY_W) || down(GLFW_KEY_S) || down(GLFW_KEY_A) || down(GLFW_KEY_D)
      if moving && (down(GLFW_KEY_LEFT_CONTROL) || down(GLFW_KEY_RIGHT_CONTROL)) then 8.5f else 0f
    else 0f
    val top = tan(toRadians(fov + sprintFov) / 2.0) * near
    glFrustum(-top * aspect, top * aspect, -top, top, near, far)
    glMatrixMode(GL_MODELVIEW); glLoadIdentity()
    glRotatef(pitch, 1f, 0f, 0f)
    glRotatef(yaw, 0f, 1f, 0f)
    glTranslatef(-camera.x, -camera.y, -camera.z)
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
    chunkList.foreach(_.drawOpaque())
    // Batch cutout pass
    glEnable(GL_ALPHA_TEST); glAlphaFunc(GL_GREATER, 0.5f)
    chunkList.foreach(_.drawCutout())
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
    farToNearChunks.foreach(_.drawTranslucent())
    farToNearChunks.foreach(_.drawWater())
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
      val visibleDist = renderDistance.toFloat
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
        // Opaque player bodies avoid the black translucent sheet artifact that appeared
        // when a second local client joined and the blended player quads overlapped terrain.
        drawColoredBox(p.x - 0.30f, feetY, p.z - 0.20f, p.x + 0.30f, feetY + 1.05f, p.z + 0.20f, cr, cg, cb, 1f)
        drawColoredBox(p.x - 0.23f, feetY + 1.05f, p.z - 0.23f, p.x + 0.23f, feetY + 1.48f, p.z + 0.23f, 0.90f, 0.74f, 0.55f, 1f)
        drawColoredBox(p.x - 0.18f, feetY - 0.02f, p.z - 0.18f, p.x - 0.02f, feetY + 0.62f, p.z + 0.18f, cr * 0.72f, cg * 0.72f, cb * 0.72f, 1f)
        drawColoredBox(p.x + 0.02f, feetY - 0.02f, p.z - 0.18f, p.x + 0.18f, feetY + 0.62f, p.z + 0.18f, cr * 0.72f, cg * 0.72f, cb * 0.72f, 1f)
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
    if gameMode == GameMode.Survival then renderHealthAndFood()
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

  private def renderHealthAndFood(): Unit =
    glDisable(GL_DEPTH_TEST); glDisable(GL_CULL_FACE); setupOrtho()
    val s = uiScale
    val unit = (18f * s).max(14f).min(22f)
    val gap = (unit + 4f * s).max(unit + 3f).min(unit + 7f)
    val spacer = (34f * s).max(22f).min(38f)
    val totalWidth = 10f * gap - (gap - unit) + spacer + 10f * gap - (gap - unit)
    val startX = framebufferWidth / 2f - totalWidth / 2f
    val hotbarSlot = (46f * s).min(((framebufferWidth.toFloat - 36f * s) / 10f - 5f * s).max(24f)).max(30f)
    val barY = framebufferHeight.toFloat - hotbarSlot - 42f * s
    val bgAlpha = 0.38f
    for i <- 0 until 10 do
      val x = startX + i * gap
      val threshold = i * 2 + 2
      val fill =
        if playerHealth >= threshold then 1f
        else if playerHealth == threshold - 1 then 0.5f
        else 0f
      drawHeartIcon(x, barY, unit, fill, bgAlpha)
    val foodStartX = startX + 10f * gap - (gap - unit) + spacer
    for i <- 0 until 10 do
      val x = foodStartX + i * gap
      val threshold = i * 2 + 2
      rect(x - 1f * s, barY - 1f * s, unit + 2f * s, unit + 2f * s, 0f, 0f, 0f, 0.30f)
      if playerFood >= threshold then
        rect(x, barY, unit, unit, 0.85f, 0.70f, 0.12f, 0.90f)
        rect(x + 2f * s, barY + 2f * s, unit - 4f * s, 2.5f * s, 1f, 0.85f, 0.40f, 0.28f)
      else if playerFood >= threshold - 1 then rect(x, barY, unit / 2f, unit, 0.85f, 0.60f, 0.08f, 0.70f)
      else rect(x, barY, unit, unit, 0.10f, 0.07f, 0.02f, bgAlpha)

  private def renderBlockInfo(): Unit =
    if framebufferHeight < 200 then return
    raycast(8f).foreach { hit =>
      val block = activeBlockAt(hit.block._1, hit.block._2, hit.block._3)
      val by = framebufferHeight.toFloat
      val hp = hit.block; val waterLvl = if block == Block.Water then wl(hp._1, hp._2, hp._3) else -1
      val blockLabel = if waterLvl >= 0 then s"> ${block} (level $waterLvl/7)" else s"> ${block}"
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
    renderTextShadow(lx, ly, s"Chunk: ($ccx, $ccz)  Seed: $worldSeed  RD: $renderDistance", 0.4f, 1f, 0.4f, 0.88f * uiScale); ly += (19 * uiScale).toInt.max(16)
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
    glDisable(GL_DEPTH_TEST); glDisable(GL_CULL_FACE); setupOrtho()
    val s = uiScale; val total = 10
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
      val hasItem = i < placeableBlocks.length && inventory(placeableBlocks(i).ordinal) > 0
      rect(sx - 2f * s, y - 2f * s, slotSize + 4f * s, slotSize + 4f * s, 0f, 0f, 0f, 0.45f)
      rect(sx, y, slotSize, slotSize, if selected then 0.16f else 0.07f, if selected then 0.17f else 0.075f, if selected then 0.20f else 0.09f, if selected then 0.96f else 0.72f)
      rect(sx + 1f * s, y + 1f * s, slotSize - 2f * s, labelBandH, 0.28f, 0.30f, 0.35f, if selected then 0.42f else 0.20f)
      rect(sx + 1f * s, y + 1f * s + labelBandH, slotSize - 2f * s, 1f * s, 0f, 0f, 0f, 0.22f)
      if selected then
        rect(sx - 3f * s, y - 3f * s, slotSize + 6f * s, 2f * s, 1f, 0.92f, 0.45f, 0.75f)
        rect(sx - 3f * s, y + slotSize + 1f * s, slotSize + 6f * s, 2f * s, 1f, 0.92f, 0.45f, 0.75f)
        rect(sx - 3f * s, y - 3f * s, 2f * s, slotSize + 6f * s, 1f, 0.92f, 0.45f, 0.75f)
        rect(sx + slotSize + 1f * s, y - 3f * s, 2f * s, slotSize + 6f * s, 1f, 0.92f, 0.45f, 0.75f)
      if i < placeableBlocks.length && hasItem then
        val icon = (slotSize * 0.50f).min(24f * s).max(16f)
        val iconY = y + labelBandH + ((slotSize - labelBandH) - icon) / 2f + 2f * s
        renderBlockIcon(placeableBlocks(i), sx + (slotSize - icon) / 2f, iconY, icon)
        if gameMode == GameMode.Survival then
          val count = inventory(placeableBlocks(i).ordinal)
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

  private def gameTime: Float = timeOverride.getOrElse(glfwGetTime().toFloat)

  private def dayPhase: Float =
    ((gameTime * 0.018f) % (2f * Pi.toFloat)) / (2f * Pi.toFloat)

  private def daylightFactor: Float =
    (0.5f + 0.5f * cos(dayPhase * 2.0 * Pi).toFloat).max(0.18f).min(1f)

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
    val walk = gameTime * 0.018f
    glDisable(GL_FOG)
    glDisable(GL_DEPTH_TEST)
    glDepthMask(false)
    glDisable(GL_TEXTURE_2D)
    glDisable(GL_CULL_FACE)
    glEnable(GL_BLEND)
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA)
    val phase = ((walk % (2f * Pi.toFloat)) / (2f * Pi.toFloat))
    val daylight = (0.5f + 0.5f * cos(phase * 2.0 * Pi).toFloat).max(0.18f).min(1f)
    val starVis = (1f - (daylight - 0.18f) / 0.82f).max(0f).min(1f)

    // Stars rotate with the sky dome
    glPushMatrix()
    glTranslatef(camera.x, camera.y, camera.z)
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

    // Sun and moon in world space (not rotated with stars)
    val sunAngle = walk * -6f
    val angleRad = toRadians(sunAngle).toFloat
    val tiltRad = toRadians(35f).toFloat
    val sunR = 11f
    val moonR = 7f
    val sunDist = 60f
    val cosT = cos(tiltRad).toFloat
    val sinT = sin(tiltRad).toFloat

    // Sun orbits on a great circle tilted 35° (arc-like path across the sky)
    val sunX = sunR * sin(angleRad).toFloat
    val sunY = sunR * cos(angleRad).toFloat * cosT
    val sunZ = -sunDist + sunR * cos(angleRad).toFloat * sinT
    val isSunVisible = sunY > -sunR * 0.5f

    // Moon is opposite the sun (180° offset) so they alternate across the sky
    val moonAngleRad = angleRad + Pi.toFloat
    val moonX = sunR * sin(moonAngleRad).toFloat
    val moonY = sunR * cos(moonAngleRad).toFloat * cosT
    val moonZ = -sunDist + sunR * cos(moonAngleRad).toFloat * sinT
    val isMoonVisible = moonY > -sunR * 0.5f

    // Helper: draw a filled circle (triangle fan) at (cx, cy) with radius r
    def drawCircle3D(cx: Float, cy: Float, cz: Float, r: Float, segments: Int): Unit =
      glBegin(GL_TRIANGLE_FAN)
      glVertex3f(cx, cy, cz)
      var i = 0
      while i <= segments do
        val a = i.toFloat / segments * 2f * Pi.toFloat
        glVertex3f(cx + r * cos(a).toFloat, cy + r * sin(a).toFloat, cz)
        i += 1
      glEnd()

    // Sun with glow
    if isSunVisible then
      val warmth = if sunY < sunR * 0.8f then (sunY / (sunR * 0.8f)).max(0f) else 1f
      val (sr, sg, sb) = (1f, 0.70f + 0.30f * warmth, 0.30f + 0.70f * warmth)
      glBlendFunc(GL_SRC_ALPHA, GL_ONE)
      glColor4f(sr, sg, sb, 0.08f * warmth)
      drawCircle3D(sunX, sunY, sunZ, sunR * 2.2f, 24)
      glColor4f(sr, sg, sb, 0.15f * warmth)
      drawCircle3D(sunX, sunY, sunZ, sunR * 1.6f, 24)
      glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA)
      glColor4f(sr, sg, sb, 0.95f)
      drawCircle3D(sunX, sunY, sunZ, sunR, 24)
      glColor4f(1f, 1f, 1f, 0.85f)
      drawCircle3D(sunX, sunY, sunZ, sunR * 0.4f, 16)

    // Moon with crater details
    if isMoonVisible then
      val moonBright = (daylight - 0.18f) / 0.82f
      glBlendFunc(GL_SRC_ALPHA, GL_ONE)
      glColor4f(0.65f, 0.70f, 0.85f, 0.06f * (1f - moonBright))
      drawCircle3D(moonX, moonY, moonZ, moonR * 2.5f, 24)
      glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA)
      glColor4f(0.78f, 0.82f, 0.90f, 0.75f + 0.15f * (1f - moonBright))
      drawCircle3D(moonX, moonY, moonZ, moonR, 24)
      val craterRng = new scala.util.Random(moonX.toInt * 31 + moonY.toInt * 17)
      glColor4f(0.58f, 0.62f, 0.72f, 0.35f)
      var ci = 0
      while ci < 5 do
        val ca = craterRng.nextFloat() * 2f * Pi.toFloat
        val cd = craterRng.nextFloat() * moonR * 0.55f
        val cr = 1f + craterRng.nextFloat() * 2.5f
        drawCircle3D(moonX + cd * cos(ca).toFloat, moonY + cd * sin(ca).toFloat, moonZ, cr, 10)
        ci += 1

    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA)
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
      glVertex3f(x + bob, y, z + bob)
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

  private def mainMenuLayout(): (Float, Float, Float, Array[Float], Float, Float, Float, Float, Float) =
    val w = framebufferWidth.toFloat
    val h = framebufferHeight.toFloat
    val s = uiScale
    val cx = w / 2f
    val margin = (18f * s).max(12f)
    val footerReserve = (30f * s).max(20f)

    // Fit everything vertically first. This keeps the title card + 6 buttons inside the window.
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
    val available = (h - firstY - footerReserve - labelH - gap * 7f).max(6f * 28f)
    val buttonH = (40f * s).min(available / 6f).max(28f)
    val buttonW = (360f * s).min(w * 0.48f).max(math.min(260f, w - margin * 2f))
    val bx = cx - buttonW / 2f

    val y0 = firstY
    val y1 = y0 + buttonH + gap + labelH + gap
    val ys = Array(y0, y1, y1 + (buttonH + gap), y1 + (buttonH + gap) * 2f, y1 + (buttonH + gap) * 3f, y1 + (buttonH + gap) * 4f)
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
    drawButton(bx, ys(4), bw, bh, "Options")
    drawButton(bx, ys(5), bw, bh, "Quit Game")
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
    val seedText = if enterCustomSeed && customSeedInput.nonEmpty then s"Seed: $customSeedInput" else s"Seed: $worldSeed"
    renderTextShadow(leftX, pY + 92f * s, seedText, 0.85f, 0.90f, 1f, (0.72f * s).min(contentW / 190f).max(0.48f))
    if enterCustomSeed then
      val display = if customSeedInput.isEmpty then "type seed..." else customSeedInput
      val inputW = (contentW * 0.62f).max(220f).min(contentW)
      rect(cx - inputW / 2f, pY + 106f * s, inputW, 30f * s, 0f, 0f, 0f, 0.70f)
      rect(cx - inputW / 2f + 2f * s, pY + 108f * s, inputW - 4f * s, 26f * s, 0.12f, 0.14f, 0.18f, 0.85f)
      centeredTextFit(cx, pY + 124f * s, display, 0.85f, 0.90f, 1f, 0.72f * s, inputW - 14f * s)

    val settingW = contentW.max(260f.min(pW - 32f * s))
    val settingH = (36f * s).min(38f).max(28f)
    val settingX = cx - settingW / 2f
    val modeY = pY + 142f * s
    val cheatsY = pY + 188f * s
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
    centeredTextFit(cx, pY + pH - 10f * s, "Press C to type a custom seed | M changes mode | H toggles cheats", 0.42f, 0.48f, 0.60f, 0.46f * s, pW - 48f * s)

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
    clickRow(pY + 85 * s, s"Render distance: $renderDistance blocks")
    slider(settingX, pY + 118 * s, rowW, (renderDistance - 32).toFloat / (96 - 32).toFloat)
    clickRow(pY + 150 * s, s"Fog density: $fogDensity%.2f")
    slider(settingX, pY + 185 * s, rowW, (fogDensity - 0.6f) / 2.4f)
    clickRow(pY + 218 * s, f"FOV: $fov%.0f")
    slider(settingX, pY + 253 * s, rowW, (fov - 50f) / 50f)
    clickRow(pY + 285 * s, s"Fast movement: ${onOff(fastMove)}")
    clickRow(pY + 325 * s, s"VSync: ${onOff(vsync)}")
    clickRow(pY + 365 * s, s"Game mode: ${gameMode}")
    clickRow(pY + 405 * s, s"Sound effects: ${onOff(soundEnabled)}")
    clickRow(pY + 445 * s, s"Fullscreen: ${onOff(fullscreen)}")
    clickRow(pY + 485 * s, s"Pause ESC: ${if pauseEscReturnsToGame then "Resume game" else "Quit to title"}")
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
    for i <- items.indices do
      val col = i % cols; val row = i / cols
      val sx = invX + col * (slot + gap); val sy = invY + row * (slot + gap)
      val block = items(i)
      val count = inventory(block.ordinal)
      val hotbarIndex = placeableBlocks.indexOf(block)
      val isSelected = hotbarIndex >= 0 && hotbarIndex == selectedBlock
      val hover = inRect(mx, my, sx, sy, slot, slot)
      if hover then
        hoveredBlock = block; hoveredX = sx + slot / 2f; hoveredY = sy
      val base = if hover then 0.135f else 0.075f
      rect(sx - 1f * s, sy - 1f * s, slot + 2f * s, slot + 2f * s, 0.015f, 0.017f, 0.022f, 0.78f)
      rect(sx, sy, slot, slot, base, base + 0.01f, base + 0.035f, 0.90f)
      rect(sx + 1f * s, sy + 1f * s, slot - 2f * s, 2f * s, 0.18f, 0.20f, 0.24f, 0.16f)
      if isSelected then
        rect(sx - 2f * s, sy - 2f * s, slot + 4f * s, 2f * s, 1f, 0.92f, 0.42f, 0.55f)
        rect(sx - 2f * s, sy + slot, slot + 4f * s, 2f * s, 1f, 0.92f, 0.42f, 0.55f)
        rect(sx - 2f * s, sy - 2f * s, 2f * s, slot + 4f * s, 1f, 0.92f, 0.42f, 0.55f)
        rect(sx + slot, sy - 2f * s, 2f * s, slot + 4f * s, 1f, 0.92f, 0.42f, 0.55f)
      if count > 0 then
        val icon = (slot * 0.58f).min(26f * s).max(16f)
        renderBlockIcon(block, sx + (slot - icon) / 2f, sy + (slot - icon) / 2f, icon)
        val ns = count.toString
        val countScale = (0.72f * s).max(0.56f).min(slot / 32f)
        val tw = textWidth(ns, countScale)
        val nx = sx + slot - tw - 5f * s
        val ny = sy + slot - 12f * countScale - 2f * s
        rect(nx - 2f * s, ny - 1f * s, tw + 4f * s, 11f * countScale + 3f * s, 0f, 0f, 0f, 0.64f)
        renderText(nx, ny, ns, 1f, 1f, 1f, countScale)
      else
        centeredTextFit(sx + slot / 2f, sy + slot / 2f - 4f * s, "0", 0.28f, 0.30f, 0.34f, 0.46f * s, slot - 6f * s)

    val craftPanelW = (pw - gridW - 78f * s).max(210f * s)
    val craftX = px + pw - 28f * s - craftPanelW
    val craftY = py + 76f * s
    rect(craftX - 10f * s, craftY - 28f * s, craftPanelW + 18f * s, ph - 140f * s, 0.045f, 0.060f, 0.085f, 0.68f)
    centeredTextFit(craftX + craftPanelW / 2f, craftY - 20f * s, "Crafting", 0.82f, 0.88f, 1f, 0.70f * s, craftPanelW)
    val craftSlotH = (58f * s).min(ph / 9f).max(38f * s)
    val maxCraftVisible = ((ph - 146f * s) / craftSlotH).toInt.max(2)
    val craftMaxScroll = (craftingRecipes.length - maxCraftVisible).max(0)
    if craftingScroll > craftMaxScroll then craftingScroll = craftMaxScroll
    for i <- craftingRecipes.indices.drop(craftingScroll).take(maxCraftVisible) do
      val (input, inputCount, output, outputCount, label) = craftingRecipes(i)
      val by = craftY + (i - craftingScroll) * craftSlotH
      val canCraft = inventory(input.ordinal) >= inputCount
      val hoverCraft = inRect(mx, my, craftX, by, craftPanelW, craftSlotH - 5f * s)
      val br = if canCraft then (if hoverCraft then 0.32f else 0.22f) else (if hoverCraft then 0.16f else 0.10f)
      rect(craftX, by, craftPanelW, craftSlotH - 5f * s, br, br * 1.04f, br * 1.10f, 0.90f)
      rect(craftX + 1f * s, by + 1f * s, craftPanelW - 2f * s, 2f * s, 1f, 1f, 1f, 0.05f)
      renderTextShadow(craftX + 8f * s, by + 5f * s, label, 0.88f, 0.88f, 0.82f, (0.60f * s).min(craftPanelW / 34f))
      renderTextShadow(craftX + 8f * s, by + 26f * s, s"${inputCount}x ${blockName(input)} (${inventory(input.ordinal)})", if canCraft then 0.68f else 0.40f, if canCraft then 0.72f else 0.45f, if canCraft then 0.78f else 0.50f, (0.50f * s).min(craftPanelW / 42f))
      centeredTextFit(craftX + craftPanelW - 22f * s, by + 9f * s, s"+$outputCount", 0.82f, 0.88f, 0.78f, 0.55f * s, 38f * s)
    if craftMaxScroll > 0 then
      centeredTextFit(craftX + craftPanelW / 2f, craftY + maxCraftVisible * craftSlotH + 2f * s, s"${craftingScroll + 1}-${(craftingScroll + maxCraftVisible).min(craftingRecipes.length)}/${craftingRecipes.length}", 0.50f, 0.55f, 0.65f, 0.50f * s, craftPanelW)

    rect(px + 18f * s, py + ph - 54f * s, pw - 36f * s, 1f, 0.30f, 0.30f, 0.35f, 0.20f)
    drawButton(px + 22f * s, py + ph - 46f * s, 130f * s, (34f * s).min(ph * 0.08f).max(28f), "Furnace")
    drawButton(px + pw - 104f * s, py + ph - 46f * s, 82f * s, (34f * s).min(ph * 0.08f).max(28f), "Close")
    if hoveredBlock != null then renderTooltip(hoveredX, hoveredY - 8f * s, blockName(hoveredBlock.asInstanceOf[Block]))

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
    val cols = ((panelW - 54f * s) / slotSize).toInt.max(4).min(12)
    val gridX = panelX + 20f * s
    val gridY = panelY + 52f * s
    val items = catalogItems
    val visibleRows = ((panelH - 90f * s) / slotSize).toInt.max(1)
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
      val isSelected = placeableBlocks.isDefinedAt(selectedBlock) && placeableBlocks(selectedBlock) == block
      val count = inventory(block.ordinal)
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
      if count > 0 then
        val ns = count.toString
        val countScale = (0.72f * s).max(0.56f).min(slotSize / 32f)
        val tw = textWidth(ns, countScale)
        val nx = sx + slotSize - tw - 5f * s
        val ny = sy + slotSize - 12f * countScale - 2f * s
        rect(nx - 2f * s, ny - 1f * s, tw + 4f * s, 11f * countScale + 3f * s, 0f, 0f, 0f, 0.64f)
        renderText(nx, ny, ns, 1f, 1f, 1f, countScale)
    if scrollMax > 0 then
      val scrollBarX = panelX + panelW - 16f * s
      val scrollBarH = panelH - 90f * s
      val thumbH = (scrollBarH / (scrollMax + 1)).max(10f * s)
      val thumbY = panelY + 52f * s + (catalogScroll.toFloat / scrollMax.max(1)) * (scrollBarH - thumbH)
      rect(scrollBarX, panelY + 52f * s, 12f * s, scrollBarH, 0.02f, 0.02f, 0.02f, 0.40f)
      rect(scrollBarX, thumbY, 12f * s, thumbH, 0.35f, 0.40f, 0.50f, 0.70f)
    val infoY = panelY + panelH - 24f * s
    rect(panelX + 12f * s, infoY - 2f * s, panelW - 24f * s, 20f * s, 0.06f, 0.08f, 0.12f, 0.50f)
    centeredTextFit(cx, infoY, "E or ESC close | Click to add to inventory", 0.55f, 0.60f, 0.70f, 0.60f * s, panelW - 32f * s)

  private def blockName(block: Block): String =
    block.toString.replaceAll("([a-z])([A-Z])", "$1 $2")

  private def inventoryItems: Array[Block] =
    Block.values.filter(b => b != Block.Air && b != Block.Water)

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

  private def catalogItems: Array[Block] = placeableBlocks

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
    for i <- smeltableInputs.indices do
      val b = smeltableInputs(i)
      val y = listY + i * rowH
      val count = inventory(b.ordinal)
      val selected = furnaceInput == b
      val hover = inRect(mx, my, listX, y, listW, rowH - 4f * s)
      val br = if selected then 0.30f else if hover then 0.18f else 0.095f
      rect(listX, y, listW, rowH - 4f * s, br, br * 1.06f, br * 1.16f, if count > 0 then 0.92f else 0.52f)
      rect(listX + 1f * s, y + 1f * s, listW - 2f * s, 2f * s, 1f, 1f, 1f, 0.06f)
      val icon = (rowH * 0.52f).min(22f * s).max(16f)
      if count > 0 then renderBlockIcon(b, listX + 8f * s, y + (rowH - icon) / 2f - 2f * s, icon)
      renderTextShadow(listX + 38f * s, y + 8f * s, blockName(b), if count > 0 then 0.88f else 0.44f, if count > 0 then 0.90f else 0.46f, if count > 0 then 0.86f else 0.50f, 0.54f * s)
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
    val inputCount = if furnaceInput == Block.Air then 0 else inventory(furnaceInput.ordinal)
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
    renderTextShadow(workX + 18f * s, fuelY + 10f * s, s"Fuel: Coal ${inventory(Block.Coal.ordinal)} | Wood ${inventory(Block.Wood.ordinal)}", 0.76f, 0.82f, 0.92f, 0.58f * s)
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
    val smeltW = (130f * s).min(workW * 0.30f).max(100f)
    val allW = (120f * s).min(workW * 0.28f).max(92f)
    val takeW = (100f * s).min(workW * 0.24f).max(80f)
    drawButton(workX + 8f * s, buttonY, smeltW, bh, "Smelt One", accent = true)
    drawButton(workX + 18f * s + smeltW, buttonY, allW, bh, "Smelt All")
    drawButton(workX + 28f * s + smeltW + allW, buttonY, takeW, bh, "Take")
    drawButton(px + pw - 104f * s, py + ph - 52f * s, 82f * s, bh, "Close")

  private def handleFurnaceClick(mx: Float, my: Float): Unit =
    val cx = framebufferWidth / 2f; val cy = framebufferHeight / 2f; val s = uiScale
    val pw = (680f * s).min(framebufferWidth * 0.94f)
    val ph = (430f * s).min(framebufferHeight * 0.88f)
    val px = cx - pw / 2f; val py = cy - ph / 2f
    val listX = px + 28f * s
    val listY = py + 82f * s
    val listW = (210f * s).min(pw * 0.34f).max(160f * s)
    val rowH = (38f * s).max(30f)
    for i <- smeltableInputs.indices do
      val y = listY + i * rowH
      if inRect(mx, my, listX, y, listW, rowH - 4f * s) then
        val b = smeltableInputs(i)
        if inventory(b.ordinal) > 0 then
          furnaceInput = b
          furnaceProgress = 0f
        else addChatMessage(s"No ${blockName(b)} to smelt")
        return
    val workX = listX + listW + 34f * s
    val workW = px + pw - 28f * s - workX
    val bh = (36f * s).min(38f).max(30f)
    val buttonY = py + ph - 52f * s
    val smeltW = (130f * s).min(workW * 0.30f).max(100f)
    val allW = (120f * s).min(workW * 0.28f).max(92f)
    val takeW = (100f * s).min(workW * 0.24f).max(80f)
    if inRect(mx, my, workX + 8f * s, buttonY, smeltW, bh) then smeltFurnace()
    else if inRect(mx, my, workX + 18f * s + smeltW, buttonY, allW, bh) then smeltAllFurnace()
    else if inRect(mx, my, workX + 28f * s + smeltW + allW, buttonY, takeW, bh) then takeFurnaceOutput()
    else if inRect(mx, my, px + pw - 104f * s, py + ph - 52f * s, 82f * s, bh) then enterGame()

  private def pickFurnaceInput(): Unit =
    smeltableInputs.find(b => inventory(b.ordinal) > 0) match
      case Some(b) => furnaceInput = b
      case None => addChatMessage("No smeltable items in inventory")

  private def smeltResult(input: Block): Option[(Block, Int)] = input match
    case Block.Sand => Some((Block.Glass, 1))
    case Block.Clay => Some((Block.Brick, 1))
    case Block.Stone => Some((Block.Brick, 2))
    case Block.Wood => Some((Block.Coal, 1))
    case Block.IronOre => Some((Block.IronIngot, 1))
    case Block.GoldOre => Some((Block.GoldIngot, 1))
    case _ => None

  private def canConsumeFuelFor(input: Block): Boolean =
    inventory(Block.Coal.ordinal) > 0 || inventory(Block.Wood.ordinal) > (if input == Block.Wood then 1 else 0)

  private def consumeFuelFor(input: Block): Boolean =
    if furnaceFuelRemaining > 0f then true
    else if inventory(Block.Coal.ordinal) > 0 then
      inventory(Block.Coal.ordinal) -= 1
      furnaceFuel = Block.Coal
      furnaceFuelRemaining = 8f
      true
    else if inventory(Block.Wood.ordinal) > (if input == Block.Wood then 1 else 0) then
      inventory(Block.Wood.ordinal) -= 1
      furnaceFuel = Block.Wood
      furnaceFuelRemaining = 4f
      true
    else false

  private def takeFurnaceOutput(): Unit =
    if furnaceOutput != Block.Air && furnaceOutputCount > 0 then
      val room = (999 - inventory(furnaceOutput.ordinal)).max(0)
      val moved = math.min(room, furnaceOutputCount)
      if moved > 0 then
        inventory(furnaceOutput.ordinal) += moved
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
    if inventory(furnaceInput.ordinal) <= 0 then
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
    inventory(furnaceInput.ordinal) -= 1
    furnaceFuelRemaining -= 1f
    furnaceProgress = 4f
    furnaceOutput = out
    furnaceOutputCount += amount
    if inventory(furnaceInput.ordinal) <= 0 then furnaceInput = Block.Air
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
    math.min(byWidth, byHeight).max(0.90f).min(1.34f)

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
    val next = (renderDistance + delta).max(32).min(96)
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

  private def applyPendingNetworkBlocksToChunk(cx: Int, cz: Int): Unit =
    val chunkOpt = chunks.get((cx, cz))
    if chunkOpt.isEmpty || pendingNetworkBlocks.isEmpty then return
    val chunk = chunkOpt.get
    var changed = false
    val keys = pendingNetworkBlocks.keys.filter { case (wx, _, wz) => chunkCoordBlock(wx) == cx && chunkCoordBlock(wz) == cz }.toList
    keys.foreach { case (wx, wy, wz) =>
      pendingNetworkBlocks.remove((wx, wy, wz)).foreach { block =>
        val lx = wx - cx * Terrain.chunkSize
        val lz = wz - cz * Terrain.chunkSize
        chunk.setBlock(lx, wy, lz, block)
        if block == Block.Water then waterLevels((wx, wy, wz)) = 7.toByte
        else waterLevels.remove((wx, wy, wz))
        changed = true
      }
    }
    if changed then chunk.markDirtyMesh()

  private def loadChunks(ccx: Int, ccz: Int): Unit =
    val worldRadius = renderDistance + 16
    val radiusSq = worldRadius * worldRadius
    val chunkRadius = (worldRadius / 16) + 2
    val maxPerFrame = if chunks.isEmpty then 8 else 4
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
              applyPendingNetworkBlocksToChunk(cx, cz)
              // Current/adjacent chunks should be ready immediately; far chunks can stream.
              if ring == 0 then chunks.get((cx, cz)).foreach(_.buildNowAndUpload()) else chunks.get((cx, cz)).foreach(queueChunkMesh)
              loaded += 1
            else
              val chunk = Chunk(cx, cz, activeAtlas, terrainGen)
              chunks((cx, cz)) = chunk
              initChunkWaterLevels(cx, cz)
              applyPendingNetworkBlocksToChunk(cx, cz)
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
    val threads = 1
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
      val msg = s"BLOC|$x|$y|$z|$blockId"
      if gameClient != null && gameClient.isConnected then gameClient.send(msg)
      else if gameServer != null then gameServer.broadcast(msg)

  private def breakTargetBlock(dropItem: Boolean = true): Unit =
    raycast().foreach { hit =>
      val (x, y, z) = hit.block
      val block = activeBlockAt(x, y, z)
      if dropItem && block != Block.Air && block != Block.Water then inventory(block.ordinal) += 1
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
        val block = placeableBlocks(selectedBlock)
        val hasBlock = gameMode == GameMode.Creative || inventory(block.ordinal) > 0
        if hasBlock && canPlaceBlockAt(x, y, z) then
          setActiveBlock(x, y, z, block)
          if gameMode == GameMode.Survival then inventory(block.ordinal) -= 1
          dirtyChunkAt(x, z)
          // Sand currently behaves like a normal block.
          playPlaceSound()
          sendBlockNetwork(x, y, z, block.ordinal.toByte)
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
    glBegin(GL_LINES); glVertex3f(camera.x, camera.y, camera.z); glVertex3f(end.x, end.y, end.z); glEnd()
    glLineWidth(1f); glEnable(GL_DEPTH_TEST)

  private def renderChunkBorders(): Unit =
    glDisable(GL_DEPTH_TEST); glLineWidth(1f); glColor4f(1f, 0.3f, 0.3f, 0.30f)
    glBegin(GL_LINES)
    chunks.keys.foreach { case (cx, cz) =>
      val x = cx * 16; val z = cz * 16
      glVertex3f(x.toFloat, 0f, z.toFloat); glVertex3f((x + 16).toFloat, 0f, z.toFloat)
      glVertex3f((x + 16).toFloat, 0f, z.toFloat); glVertex3f((x + 16).toFloat, 0f, (z + 16).toFloat)
      glVertex3f((x + 16).toFloat, 0f, (z + 16).toFloat); glVertex3f(x.toFloat, 0f, (z + 16).toFloat)
      glVertex3f(x.toFloat, 0f, (z + 16).toFloat); glVertex3f(x.toFloat, 0f, z.toFloat)
    }
    glEnd(); glEnable(GL_DEPTH_TEST); glLineWidth(1f)

  private def renderTargetOutline(): Unit =
    raycast(8f).foreach { hit =>
      val (x, y, z) = hit.block
      glDisable(GL_DEPTH_TEST)
      val e = 0.005f; val x0 = x - e; val x1 = x + 1f + e
      val y0 = y - e; val y1 = y + 1f + e; val z0 = z - e; val z1 = z + 1f + e
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
    val baseX = cx * Terrain.chunkSize
    val baseZ = cz * Terrain.chunkSize
    val minY = (Terrain.seaLevel - 10).max(0)
    val maxY = (Terrain.seaLevel + 8).min(Terrain.worldHeight - 1)
    val chunk = chunks.get((cx, cz))
    for lx <- 0 until Terrain.chunkSize; lz <- 0 until Terrain.chunkSize do
      var y = minY
      while y <= maxY do
        val wx = baseX + lx; val wz = baseZ + lz
        chunk match
          case Some(c) => if c.getBlock(lx, y, lz) == Block.Water then waterLevels((wx, y, wz)) = 7
          case None => if activeBlockAt(wx, y, wz) == Block.Water then waterLevels((wx, y, wz)) = 7
        y += 1

  private def saveChunk(cx: Int, cz: Int): Unit =
    if !canUseLocalChunkSaves then return
    val chunk = chunks.get((cx, cz))
    chunk.foreach(_.save(currentChunksDir))

  private def loadChunkIfSaved(cx: Int, cz: Int): Boolean =
    if !canUseLocalChunkSaves then return false
    val chunksDir = currentChunksDir
    val file = new java.io.File(chunksDir, s"chunk_${cx}_${cz}.dat")
    if file.exists() then
      val chunk = Chunk(cx, cz, activeAtlas, terrainGen)
      chunk.load(chunksDir)
      chunks((cx, cz)) = chunk
      initChunkWaterLevels(cx, cz)
      true
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

  private def activeWorld: Nothing = throw IllegalStateException("World no longer used - use activeBlockAt")

  private def activeAtlas: TextureAtlas =
    val atlas = textureAtlas
    if atlas == null then throw IllegalStateException("Texture atlas is not initialized")
    atlas
