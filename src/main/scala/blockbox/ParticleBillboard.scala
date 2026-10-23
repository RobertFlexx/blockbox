package blockbox

object ParticleBillboard:
  def axes(yaw: Float, pitch: Float): (Vec3, Vec3) =
    val y = math.toRadians(yaw); val p = math.toRadians(pitch)
    (
      Vec3(math.cos(y).toFloat, 0, math.sin(y).toFloat),
      Vec3(
        (math.sin(y) * math.sin(p)).toFloat,
        math.cos(p).toFloat,
        (-math.cos(y) * math.sin(p)).toFloat
      )
    )
