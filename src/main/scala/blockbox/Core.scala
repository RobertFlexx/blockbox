package blockbox

import scala.math.*

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

