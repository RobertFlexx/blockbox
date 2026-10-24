package blockbox

import scala.math.*

/** Minecraft's first-person chains (ItemInHandRenderer, right hand, camera space) and the
  * third-person grip attachments.
  */
object FirstPersonPose:
  final case class Attachment(position: Vec3, rotation: Vec3, size: Float)

  /** The attack angles are applied Y, Z, X and followed by a fixed Y -45. */
  final case class ItemPose(position: Vec3, attack: Vec3, display: Attachment)

  /** The bare arm's offset and swing; the fixed vanilla arm chain follows it. */
  final case class HandPose(position: Vec3, yaw: Float, roll: Float)

  final case class WalkPose(position: Vec3, pitch: Float, roll: Float)

  /** Vanilla's side-to-side roll and twice-per-stride vertical motion, applied once to the complete
    * hand/item frame. The world camera remains stable.
    */
  def walkPose(phase: Float, amount: Float): WalkPose =
    val weight = amount.max(0f).min(1.15f)
    val side = sin(phase).toFloat
    WalkPose(
      Vec3(side * 0.035f * weight, -abs(cos(phase).toFloat) * 0.028f * weight, 0),
      abs(cos(phase - 0.2f).toFloat) * 0.65f * weight,
      side * 0.85f * weight
    )

  private def swingProgress(kick: Float): Float = if kick > 0 then 1f - kick.max(0).min(1) else 0f

  /** Vanilla renderPlayerArm and wall retraction. Walking uses the shared WalkPose.
    * @param equip
    *   0 while the hand is fully lowered for an item switch, 1 when raised
    */
  def handPose(kick: Float, obstruction: Float, equip: Float): HandPose =
    val progress = swingProgress(kick)
    val root = sqrt(progress).toFloat
    val blocked = obstruction.max(0).min(1)
    val lowered = 1f - equip.max(0).min(1)
    HandPose(
      Vec3(
        0.64f - 0.3f * sin(root * Pi).toFloat,
        -0.6f + 0.4f * sin(root * 2 * Pi).toFloat - lowered * 0.6f - blocked * 0.08f,
        -0.72f - 0.4f * sin(progress * Pi).toFloat + blocked * 0.06f
      ),
      45f + 70f * sin(root * Pi).toFloat,
      -20f * sin(progress * progress * Pi).toFloat
    )

  /** firstperson_righthand of Minecraft's item/generated and block/block models. */
  val generatedDisplay =
    Attachment(Vec3(1.13f / 16, 3.2f / 16, 1.13f / 16), Vec3(0, -90, 25), 0.68f)
  val blockDisplay = Attachment(Vec3(0, 0, 0), Vec3(0, 45, 0), 0.40f)

  /** Where the torch's local origin (its grip, 0.12 above the foot) sits in the fist. */
  val torchGripHeight = 0.12f

  // Third person: held cubes sit outside the fist; the torch passes through it and leans
  // forward out of the hand. Both attach inside the elbow transform, so walking and
  // recoil cannot change how they sit in the hand.
  def attachment(block: Block): Attachment =
    if block == Block.Torch then Attachment(Vec3(0f, -0.30f, -0.055f), Vec3(-28f, 0f, 0f), 0.72f)
    // Vanilla item/handheld thirdperson_righthand is (0,-90,55), scale .85.
    // The hand layer first applies X -90, Y 180, giving X -90, Y 90, Z 55 here.
    // Our mesh origin is already in the handle, so translate to the fist rather
    // than applying the vanilla sprite-center offset a second time. The .9
    // factor matches our 1.8-block-tall avatar. No reflection/negative scale:
    // the blade points along local -Z and its broad face lies beside the arm.
    // Reference: assets.mcasset.cloud/1.21.1/assets/minecraft/models/item/handheld.json
    else if ToolSystem.isTool(block) then
      Attachment(Vec3(-0.04f, -0.30f, -0.10f), Vec3(-90f, 90f, 55f), 0.85f * 0.9f)
    else if Block.isItem(block) then Attachment(Vec3(0f, -0.31f, -0.07f), Vec3(-70f, 0f, 0f), 0.55f)
    else Attachment(Vec3(0f, -0.30f, -0.32f), Vec3(0f, 20f, 0f), 0.30f)

  /** The same hand basis works with the zombie's raised arm: tools naturally stand upright. */
  def zombieAttachment(block: Block): Attachment = attachment(block)

  /** Vanilla applyItemArmTransform and applyItemArmAttackTransform, then the held thing's display
    * transform. Item sprites and the torch are centered on (0.5, 0.5) after it, blocks are a
    * centered unit cube; no arm is drawn, as in Minecraft.
    */
  def itemPose(item: Block, kick: Float, obstruction: Float, equip: Float): ItemPose =
    val progress = swingProgress(kick)
    val root = sqrt(progress).toFloat
    val swing = sin(root * Pi).toFloat
    val squared = sin(progress * progress * Pi).toFloat
    val blocked = obstruction.max(0).min(1)
    val lowered = 1f - equip.max(0).min(1)
    ItemPose(
      Vec3(
        0.56f - 0.4f * swing,
        -0.52f + 0.2f * sin(root * 2 * Pi).toFloat - lowered * 0.6f - blocked * 0.08f,
        -0.72f - 0.2f * sin(progress * Pi).toFloat + blocked * 0.06f
      ),
      Vec3(-80f * swing, 45f - 20f * squared, -20f * swing),
      if Block.isItem(item) || item == Block.Torch then generatedDisplay else blockDisplay
    )

  def obstructionLevel(clearance: Float): Float =
    val t = ((1.35f - clearance) / 0.95f).max(0f).min(1f)
    t * t * (3f - 2f * t)

  def obstruction(camera: Vec3, yaw: Float, pitch: Float, at: (Int, Int, Int) => Block): Float =
    val forward = BlockboxRaycast.viewDirection(yaw, pitch)
    val radians = toRadians(yaw)
    val right = Vec3(cos(radians).toFloat, 0f, sin(radians).toFloat)
    var closest = 1.6f
    def probe(offset: Vec3): Unit =
      val hit = BlockboxRaycast.raycast(
        camera + offset,
        forward,
        1.6f,
        (x, y, z) =>
          val block = at(x, y, z)
          if block.solid then block else Block.Air
      )
      hit.foreach(h => closest = closest.min(h.distance))
    probe(Vec3(0, -0.08f, 0)); probe(right * 0.22f + Vec3(0, -0.18f, 0))
    probe(right * 0.40f + Vec3(0, -0.28f, 0)); probe(right * 0.62f + Vec3(0, -0.28f, 0))
    obstructionLevel(closest)
