package blockbox

import scala.math.*

object BlockboxRaycast:
  def viewDirection(yaw: Float, pitch: Float): Vec3 =
    val yawRad = toRadians(yaw); val pitchRad = toRadians(pitch)
    val cp = cos(pitchRad).toFloat; val sp = sin(pitchRad).toFloat
    val sy = sin(yawRad).toFloat; val cy = cos(yawRad).toFloat
    Vec3(sy * cp, -sp, -cy * cp).normalized

  def raycast(camera: Vec3, dir: Vec3, maxDistance: Float, blockAt: (Int, Int, Int) => Block): Option[RayHit] =
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
      if blockAt(x, y, z).solid then
        return Some(RayHit((x, y, z), (x + normal._1, y + normal._2, z + normal._3), normal, distance))
      if tMaxX <= tMaxY && tMaxX <= tMaxZ then
        x += stepX; distance = tMaxX; tMaxX += tDeltaX; normal = (-stepX, 0, 0)
      else if tMaxY <= tMaxZ then
        y += stepY; distance = tMaxY; tMaxY += tDeltaY; normal = (0, -stepY, 0)
      else
        z += stepZ; distance = tMaxZ; tMaxZ += tDeltaZ; normal = (0, 0, -stepZ)
    None
