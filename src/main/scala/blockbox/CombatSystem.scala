package blockbox

object CombatSystem:
  val reach = 3.5f
  val cooldown = 0.50
  def rayPlayer(origin: Vec3, direction: Vec3, eye: Vec3, range: Float = reach): Option[Float] =
    val body = ZombieSystem.Zombie(0, eye - Vec3(0, 1.62f, 0))
    ZombieSystem.rayDistance(origin, direction, body, range)

  /** Both the local picker and the host use the victim's body and the actual terrain. A client can
    * request a hit, but cannot choose its damage or ignore a wall.
    */
  def canHit(
      from: Vec3,
      yaw: Float,
      pitch: Float,
      target: Vec3,
      at: (Int, Int, Int) => Block
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
    rayPlayer(from, direction, target, wall).nonEmpty
