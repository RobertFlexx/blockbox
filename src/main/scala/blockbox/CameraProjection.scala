package blockbox

/** Keep every near-plane corner inside the collision clearance around the eye. */
object CameraProjection:
  def nearPlane(verticalFov: Float, aspect: Double): Double =
    val tangent = math.tan(math.toRadians(verticalFov.max(45f).min(110f)) * 0.5)
    val corner = math.sqrt(1.0 + tangent * tangent * (1.0 + aspect * aspect))
    math.min(0.05, 0.12 / corner)
