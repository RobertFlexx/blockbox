package blockbox

import scala.math.*

object BlockboxRaycast:
  def viewDirection(yaw: Float, pitch: Float): Vec3 =
    val yawRad = toRadians(yaw); val pitchRad = toRadians(pitch)
    val cp = cos(pitchRad).toFloat; val sp = sin(pitchRad).toFloat
    val sy = sin(yawRad).toFloat; val cy = cos(yawRad).toFloat
    Vec3(sy * cp, -sp, -cy * cp).normalized

  def raycast(
      camera: Vec3,
      dir: Vec3,
      maxDistance: Float,
      blockAt: (Int, Int, Int) => Block,
      torchMountAt: (Int, Int, Int) => TorchMount = (_, _, _) => TorchMount.Floor
  ): Option[RayHit] =
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
    var tMaxX = if stepX > 0 then (x + 1f - camera.x) / dir.x
    else if stepX < 0 then (x.toFloat - camera.x) / dir.x
    else inf
    var tMaxY = if stepY > 0 then (y + 1f - camera.y) / dir.y
    else if stepY < 0 then (y.toFloat - camera.y) / dir.y
    else inf
    var tMaxZ = if stepZ > 0 then (z + 1f - camera.z) / dir.z
    else if stepZ < 0 then (z.toFloat - camera.z) / dir.z
    else inf
    if tMaxX < 0f then tMaxX = 0f; if tMaxY < 0f then tMaxY = 0f; if tMaxZ < 0f then tMaxZ = 0f
    var normal = (0, 0, 0); var distance = 0f; var i = 0
    while distance <= maxDistance && i < 200 do
      i += 1
      val block = blockAt(x, y, z)
      if block.solid then
        return Some(
          RayHit((x, y, z), (x + normal._1, y + normal._2, z + normal._3), normal, distance)
        )
      val padBelowCell = block == Block.Water && blockAt(x, y + 1, z) == Block.LilyPad
      if block == Block.Torch || FloraModel.contains(block) || padBelowCell then
        val modelY = if padBelowCell then y + 1 else y
        val box = if block == Block.Torch then TorchModel.bounds(torchMountAt(x, y, z))
        else FloraModel.bounds(if padBelowCell then Block.LilyPad else block)
        var entry = 0f; var exit = maxDistance; var face = normal; var valid = true
        def slab(
            origin: Float,
            direction: Float,
            low: Float,
            high: Float,
            nearFace: (Int, Int, Int)
        ): Unit =
          if abs(direction) < 0.000001f then
            if origin < low || origin > high then valid = false
          else
            val a = (low - origin) / direction; val b = (high - origin) / direction
            val near = a.min(b); val far = a.max(b)
            if near > entry then
              entry = near
              face = if direction > 0 then nearFace else (-nearFace._1, -nearFace._2, -nearFace._3)
            exit = exit.min(far)
            if entry > exit then valid = false
        slab(camera.x, dir.x, x + box.x0, x + box.x1, (-1, 0, 0))
        slab(camera.y, dir.y, modelY + box.y0, modelY + box.y1, (0, -1, 0))
        slab(camera.z, dir.z, z + box.z0, z + box.z1, (0, 0, -1))
        if valid && exit >= distance && entry <= tMaxX.min(tMaxY).min(tMaxZ) then
          return Some(
            RayHit((x, modelY, z), (x + face._1, modelY + face._2, z + face._3), face, entry)
          )
      if tMaxX <= tMaxY && tMaxX <= tMaxZ then
        x += stepX; distance = tMaxX; tMaxX += tDeltaX; normal = (-stepX, 0, 0)
      else if tMaxY <= tMaxZ then
        y += stepY; distance = tMaxY; tMaxY += tDeltaY; normal = (0, -stepY, 0)
      else
        z += stepZ; distance = tMaxZ; tMaxZ += tDeltaZ; normal = (0, 0, -stepZ)
    None
