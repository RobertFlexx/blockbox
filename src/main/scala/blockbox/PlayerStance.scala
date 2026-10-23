package blockbox

/** Physics keeps a stable standing-eye origin; posture changes the rendered eye and upper body. */
object PlayerStance:
  val standingEye = 1.62f
  val eyeDrop = 0.30f
  val heightDrop = 0.32f
  def eye(origin: Vec3, crouch: Float): Vec3 = origin - Vec3(0, eyeDrop * crouch.max(0).min(1), 0)
  def approach(current: Float, wanted: Boolean, dt: Float): Float =
    val goal = if wanted then 1f else 0f
    val next = current + (goal - current) * (1f - math.exp(-dt.max(0) * 16f).toFloat)
    if math.abs(next - goal) < 0.001f then goal else next
