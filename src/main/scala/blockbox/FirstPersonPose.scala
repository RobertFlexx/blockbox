package blockbox

import scala.math.*

/** Camera pose and grip attachment shared by the hand and its held item. */
object FirstPersonPose:
  final case class Pose(position: Vec3, rotation: Vec3, elbow: Float, grip: Float)
  final case class Attachment(position: Vec3, rotation: Vec3, size: Float)

  /** @param equip 0 while the hand is fully lowered for an item switch, 1 when raised */
  def sample(block: Block, kick: Float, bobPhase: Float, bobAmount: Float, obstruction: Float, equip: Float = 1f): Pose =
    val action=kick.max(0f).min(1f)
    val swing=if action>0f then sin(sqrt(1f-action)*Pi).toFloat else 0f
    val twist=if action>0f then sin(pow(1f-action,2)*Pi).toFloat else 0f
    val blocked=obstruction.max(0f).min(1f)
    val bob=bobAmount.max(0f).min(1f)
    val lowered=1f-equip.max(0f).min(1f)
    val drop=lowered*lowered*(3f-2f*lowered)
    Pose(
      Vec3(0.64f+sin(bobPhase).toFloat*0.008f*bob-swing*0.12f,
        -0.48f+(0.5f-0.5f*cos(bobPhase*2).toFloat)*0.009f*bob-swing*0.035f-blocked*0.23f-drop*0.62f,
        -1.04f+blocked*0.23f),
      Vec3(-16f-swing*19f+blocked*22f+drop*24f,-17f+twist*11f,20f+swing*11f+blocked*9f),
      7f+swing*10f+blocked*10f,if block==Block.Air then 0.20f+swing*0.65f else 0.95f)

  /** Where the torch's local origin (its grip, 0.12 above the foot) sits in the fist. */
  val torchGripHeight = 0.12f

  // Held cubes sit outside the fist; the torch passes through it and leans forward out of
  // the hand. Both attach inside the elbow transform, so walking, recoil and wall
  // retraction cannot change how they sit in the hand.
  def attachment(block: Block): Attachment =
    if block==Block.Torch then Attachment(Vec3(0.004f,-0.305f,-0.012f),Vec3(156f,0f,-6f),0.96f)
    else Attachment(Vec3(-0.08f,-0.32f,0.38f),Vec3(-18f,25f,8f),0.32f)

  def obstructionLevel(clearance: Float): Float =
    val t=((1.35f-clearance)/0.95f).max(0f).min(1f)
    t*t*(3f-2f*t)

  def obstruction(camera: Vec3, yaw: Float, pitch: Float, at: (Int,Int,Int) => Block): Float =
    val forward=BlockboxRaycast.viewDirection(yaw,pitch)
    val radians=toRadians(yaw)
    val right=Vec3(cos(radians).toFloat,0f,sin(radians).toFloat)
    var closest=1.6f
    def probe(offset: Vec3): Unit =
      val hit=BlockboxRaycast.raycast(camera+offset,forward,1.6f,(x,y,z) =>
        val block=at(x,y,z)
        if block.solid then block else Block.Air)
      hit.foreach(h => closest=closest.min(h.distance))
    probe(Vec3(0,-0.08f,0)); probe(right*0.22f+Vec3(0,-0.18f,0))
    probe(right*0.40f+Vec3(0,-0.28f,0)); probe(right*0.62f+Vec3(0,-0.28f,0))
    obstructionLevel(closest)

