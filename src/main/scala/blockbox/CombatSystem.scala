package blockbox

object CombatSystem:
  val reach = 3.5f
  val cooldown = 0.50
  def rayPlayer(
      origin: Vec3,
      direction: Vec3,
      eye: Vec3,
      range: Float = reach,
      crouch: Float = 0f
  ): Option[Float] =
    val bottom = eye.y - PlayerStance.standingEye
    val top = bottom + 1.82f - PlayerStance.heightDrop * crouch.max(0).min(1)
    var entry = 0f; var exit = range; var valid = direction.lengthSquared > 0.0001f
    def slab(o: Float, d: Float, low: Float, high: Float): Unit =
      if math.abs(d) < 0.000001f then
        if o < low || o > high then valid = false
      else
        val a = (low - o) / d; val b = (high - o) / d
        entry = entry.max(a.min(b)); exit = exit.min(a.max(b))
        if entry > exit then valid = false
    slab(origin.x, direction.x, eye.x - 0.3f, eye.x + 0.3f)
    slab(origin.y, direction.y, bottom, top)
    slab(origin.z, direction.z, eye.z - 0.3f, eye.z + 0.3f)
    if valid && exit >= 0 && entry <= range then Some(entry) else None

  /** Both the local picker and the host use the victim's body and the actual terrain. A client can
    * request a hit, but cannot choose its damage or ignore a wall.
    */
  def canHit(
      from: Vec3,
      yaw: Float,
      pitch: Float,
      target: Vec3,
      at: (Int, Int, Int) => Block,
      crouch: Float = 0f
  ): Boolean =
    val direction = BlockboxRaycast.viewDirection(yaw, pitch)
    val wall = BlockboxRaycast
      .raycast(
        from,
        direction,
        reach,
        (x, y, z) => if at(x, y, z).solid then at(x, y, z) else Block.Air
      )
      .map(_.distance)
      .getOrElse(reach)
    rayPlayer(from, direction, target, wall, crouch).nonEmpty
