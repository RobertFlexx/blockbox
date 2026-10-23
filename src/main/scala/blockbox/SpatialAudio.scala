package blockbox

object SpatialAudio:
  final case class Mix(gain: Float, pan: Float)
  def mix(
      listener: Vec3,
      yaw: Float,
      source: Vec3,
      radius: Float,
      occluded: Boolean,
      submerged: Boolean
  ): Mix =
    val delta = source - listener
    val distance = math.sqrt(delta.lengthSquared.toDouble).toFloat
    val fade = (1f - distance / radius.max(1f)).max(0f)
    val angle = math.toRadians(yaw)
    val pan = ((delta.x * math.cos(angle) + delta.z * math.sin(angle)) / distance.max(0.1f)).toFloat
      .max(-1f)
      .min(1f)
    Mix(fade * fade * (if occluded then 0.28f else 1f) * (if submerged then 0.45f else 1f), pan)
