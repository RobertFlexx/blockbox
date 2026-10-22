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
  case MainMenu, CreateWorld, LoadWorld, Mods, Resources, Settings, Playing, Paused, Inventory, Catalog, FurnaceUI, Journal, JoinGame, HostGame

final case class TexturePack(id: String, name: String, author: String, dir: Option[java.io.File], legacy: Boolean = false):
  def label: String = s"$name by $author"

enum GameMode:
  case Survival, Creative
  def toInt: Int = ordinal

enum Block(private val baseSolid: Boolean, private val baseRgb: (Float, Float, Float), private val baseTranslucent: Boolean = false, val liquid: Boolean = false, private val baseCutout: Boolean = false):
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
  case Torch extends Block(false, (1.00f, 0.74f, 0.28f), true, false, true)
  case Custom00 extends Block(false,(1f,1f,1f))
  case Custom01 extends Block(false,(1f,1f,1f))
  case Custom02 extends Block(false,(1f,1f,1f))
  case Custom03 extends Block(false,(1f,1f,1f))
  case Custom04 extends Block(false,(1f,1f,1f))
  case Custom05 extends Block(false,(1f,1f,1f))
  case Custom06 extends Block(false,(1f,1f,1f))
  case Custom07 extends Block(false,(1f,1f,1f))
  case Custom08 extends Block(false,(1f,1f,1f))
  case Custom09 extends Block(false,(1f,1f,1f))
  case Custom10 extends Block(false,(1f,1f,1f))
  case Custom11 extends Block(false,(1f,1f,1f))
  case Custom12 extends Block(false,(1f,1f,1f))
  case Custom13 extends Block(false,(1f,1f,1f))
  case Custom14 extends Block(false,(1f,1f,1f))
  case Custom15 extends Block(false,(1f,1f,1f))
  case Custom16 extends Block(false,(1f,1f,1f))
  case Custom17 extends Block(false,(1f,1f,1f))
  case Custom18 extends Block(false,(1f,1f,1f))
  case Custom19 extends Block(false,(1f,1f,1f))
  case Custom20 extends Block(false,(1f,1f,1f))
  case Custom21 extends Block(false,(1f,1f,1f))
  case Custom22 extends Block(false,(1f,1f,1f))
  case Custom23 extends Block(false,(1f,1f,1f))
  case Custom24 extends Block(false,(1f,1f,1f))
  case Custom25 extends Block(false,(1f,1f,1f))
  case Custom26 extends Block(false,(1f,1f,1f))
  case Custom27 extends Block(false,(1f,1f,1f))
  case Custom28 extends Block(false,(1f,1f,1f))
  case Custom29 extends Block(false,(1f,1f,1f))
  case Custom30 extends Block(false,(1f,1f,1f))
  case Custom31 extends Block(false,(1f,1f,1f))
  // Append items after all legacy/custom ids: existing worlds keep their block palette.
  case Stick extends Block(false, (0.48f,0.31f,0.15f), true)
  case CoalLump extends Block(false, (0.18f,0.19f,0.22f), true)
  case DiamondGem extends Block(false, (0.26f,0.91f,0.84f), true)
  case CopperIngot extends Block(false, (0.86f,0.48f,0.27f), true)
  case WoodenPickaxe extends Block(false, (0.63f,0.43f,0.22f), true)
  case WoodenAxe extends Block(false, (0.63f,0.43f,0.22f), true)
  case WoodenShovel extends Block(false, (0.63f,0.43f,0.22f), true)
  case WoodenSword extends Block(false, (0.63f,0.43f,0.22f), true)
  case StonePickaxe extends Block(false, (0.54f,0.56f,0.59f), true)
  case StoneAxe extends Block(false, (0.54f,0.56f,0.59f), true)
  case StoneShovel extends Block(false, (0.54f,0.56f,0.59f), true)
  case StoneSword extends Block(false, (0.54f,0.56f,0.59f), true)
  case IronPickaxe extends Block(false, (0.83f,0.87f,0.89f), true)
  case IronAxe extends Block(false, (0.83f,0.87f,0.89f), true)
  case IronShovel extends Block(false, (0.83f,0.87f,0.89f), true)
  case IronSword extends Block(false, (0.83f,0.87f,0.89f), true)
  case DiamondPickaxe extends Block(false, (0.26f,0.91f,0.84f), true)
  case DiamondAxe extends Block(false, (0.26f,0.91f,0.84f), true)
  case DiamondShovel extends Block(false, (0.26f,0.91f,0.84f), true)
  case DiamondSword extends Block(false, (0.26f,0.91f,0.84f), true)
  def solid: Boolean =
    val d=CustomBlocks.raw(this)
    if d==null then baseSolid else d.solid
  def rgb: (Float,Float,Float) =
    val d=CustomBlocks.raw(this)
    if d==null then baseRgb else d.tint
  def translucent: Boolean =
    val d=CustomBlocks.raw(this)
    if d==null then baseTranslucent else d.translucent
  def cutout: Boolean =
    val d=CustomBlocks.raw(this)
    if d==null then baseCutout else d.cutout
  def displayName: String = CustomBlocks.definition(this).map(_.name).getOrElse(toString)
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
    if key.isEmpty then None else CustomBlocks.find(name).orElse(valuesArray.find(b => CustomBlocks.registered(b) && normalizedName(b.toString) == key))
  def fromName(name: String): Block = find(name).getOrElse(Air)
  def isKnown(name: String): Boolean = find(name).nonEmpty
  def all: Array[Block] = valuesArray.filter(CustomBlocks.registered)
  def names: Array[String] = all.map(_.displayName)
  def isLog(b: Block): Boolean =
    b == Wood || b == BirchWood || b == PineWood || b == AcaciaWood
  def isItem(b: Block): Boolean =
    b == IronIngot || b == GoldIngot || b.ordinal >= Stick.ordinal

enum FaceKind:
  case Top, Bottom, North, South, East, West

/** Minecraft-style log orientation. Y is the default for standing trees and all
  * legacy chunks; X/Z mark logs that run east-west / north-south.
  */
enum LogAxis(val id: Int):
  case Y extends LogAxis(0)
  case X extends LogAxis(1)
  case Z extends LogAxis(2)

object LogAxis:
  def fromId(id: Int): LogAxis = id match
    case 1 => LogAxis.X
    case 2 => LogAxis.Z
    case _ => LogAxis.Y

  def fromNormal(n: (Int, Int, Int)): LogAxis = n match
    case (nx, _, _) if nx != 0 => LogAxis.X
    case (_, ny, _) if ny != 0 => LogAxis.Y
    case (_, _, nz) if nz != 0 => LogAxis.Z
    case _ => LogAxis.Y

  def isEndFace(physical: FaceKind, axis: LogAxis): Boolean = axis match
    case LogAxis.Y => physical == FaceKind.Top || physical == FaceKind.Bottom
    case LogAxis.X => physical == FaceKind.East || physical == FaceKind.West
    case LogAxis.Z => physical == FaceKind.North || physical == FaceKind.South

  /** Select the canonical atlas tile for a log face: end grain renders from the
    * Top tile and bark from the North tile (every wood species maps that way).
    */
  def textureFace(physical: FaceKind, axis: LogAxis): FaceKind =
    if isEndFace(physical, axis) then FaceKind.Top else FaceKind.North

  /** Bark grain runs along the log axis. Swap UVs only on the bark faces whose
    * texture width currently spans the axis direction, so stripe detail follows it.
    */
  def swapsUv(physical: FaceKind, axis: LogAxis): Boolean = axis match
    case LogAxis.Y => false
    case LogAxis.X =>
      physical == FaceKind.Top || physical == FaceKind.Bottom || physical == FaceKind.North || physical == FaceKind.South
    case LogAxis.Z => physical == FaceKind.East || physical == FaceKind.West

final case class RayHit(block: (Int, Int, Int), place: (Int, Int, Int), normal: (Int, Int, Int), distance: Float)
// Network interpolation updates these fields in place on every position packet.
// Keep them mutable: making them constructor vals breaks multiplayer compilation.
final case class RemotePlayer(
  name: String,
  var pos: Vec3,
  var targetPos: Vec3,
  var yaw: Float,
  var targetYaw: Float,
  var pitch: Float,
  var targetPitch: Float,
  var lastSeen: Double,
  var colorId: Int,
  var heldBlock: Block = Block.Air
):
  var bodyYaw: Float = yaw
  var gait: Float = 0f
  var stride: Float = 0f
  var swing: Float = 0f
  var hurt: Float = 0f
  var mode: GameMode = GameMode.Survival
