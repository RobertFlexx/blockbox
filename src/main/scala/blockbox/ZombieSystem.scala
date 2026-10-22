package blockbox

import blockbox.ai.VoxelNavigation
import scala.jdk.CollectionConverters.*
import scala.collection.mutable.ArrayBuffer
import scala.math.*

object ZombieSystem:
  final case class Target(name: String,eye: Vec3,vulnerable: Boolean = true)
  final case class Attack(name: String,damage: Float)
  final class Zombie(val id: Int,var pos: Vec3):
    var target=pos
    var velocity=Vec3(0,0,0)
    var yaw=0f
    var targetYaw=0f
    var health=20f
    var gait=0f
    var stride=0f
    var hurt=0f
    var burning=0f
    var attackCooldown=0f
    var hitCooldown=0f
    var grounded=false
    var voiceTimer=1.2f+Math.floorMod(id*13,37)*0.1f
    var fireTick=0f
    var heldItem=Block.Air
    var lastKnown=pos
    var memory=0f
    var route=Vector.empty[Vec3]
    var routeIndex=0
    var routeTimer=0f
    var routeGoal=pos
    var stuck=0f
    val avatar=RemotePlayer("Zombie",pos+Vec3(0,1.62f,0),pos+Vec3(0,1.62f,0),0,0,0,0,0,0)

  def canSpawn(feet: Vec3,daylight: Float,sky: Int,light: Int,at: (Int,Int,Int) => Block): Boolean =
    val x=floor(feet.x).toInt; val y=floor(feet.y).toInt; val z=floor(feet.z).toInt
    val ground=at(x,y-1,z)
    sky*daylight<7f && light<7 && ground.solid && !BlockTints.isFoliage(ground) &&
      at(x,y,z)==Block.Air && at(x,y+1,z)==Block.Air && !collides(feet,at)

  def collides(pos: Vec3,at: (Int,Int,Int) => Block): Boolean =
    (floor(pos.x-0.28f).toInt to floor(pos.x+0.28f).toInt).exists { x =>
      (floor(pos.y+0.01f).toInt to floor(pos.y+1.78f).toInt).exists { y =>
        (floor(pos.z-0.28f).toInt to floor(pos.z+0.28f).toInt).exists(z => at(x,y,z).solid)
      }
    }

  def visible(from: Vec3,to: Vec3,at: (Int,Int,Int) => Block): Boolean =
    val delta=to-from; val distance=sqrt(delta.lengthSquared).toFloat
    BlockboxRaycast.raycast(from,delta.normalized,distance,(x,y,z) => if at(x,y,z).solid then at(x,y,z) else Block.Air).isEmpty

  def update(z: Zombie,dt: Float,players: Seq[Target],sunlight: Boolean,raining: Boolean,at: (Int,Int,Int) => Block): Option[Attack] =
    z.hurt=(z.hurt-dt*3f).max(0f); z.hitCooldown=(z.hitCooldown-dt).max(0f)
    z.attackCooldown=(z.attackCooldown-dt).max(0f)
    val wet=at(floor(z.pos.x).toInt,floor(z.pos.y+0.8f).toInt,floor(z.pos.z).toInt)==Block.Water
    if wet || raining then z.burning=0f
    else if sunlight then z.burning=4f
    else z.burning=(z.burning-dt).max(0f)
    if z.burning>0f then
      z.fireTick+=dt
      if z.fireTick>=1f then
        z.fireTick-=1f; z.health-=2.5f; z.hurt=1f
    else z.fireTick=0f
    val worldAt=at
    z.memory=(z.memory-dt).max(0f); z.routeTimer-=dt
    val nearest=players.filter(p => p.vulnerable && (p.eye-z.pos).lengthSquared<40*40)
      .minByOption(p => (p.eye-z.pos-Vec3(0,1.2f,0)).lengthSquared)
    val before=z.pos
    var attacking: Option[Attack]=None
    nearest.foreach { player =>
      val delta=player.eye-z.pos-Vec3(0,1.2f,0)
      val sight=visible(z.pos+Vec3(0,1.45f,0),player.eye,at)
      // Nearby footsteps can attract a zombie behind a wall. At longer range,
      // it follows the last seen position and eventually loses the player.
      if sight || delta.lengthSquared<12*12 then
        z.lastKnown=player.eye-Vec3(0,1.62f,0); z.memory=6f
      if delta.lengthSquared<1.45f*1.45f && abs(delta.y)<1.3f && sight && z.attackCooldown<=0 then
        z.attackCooldown=1.1f
        attacking=Some(Attack(player.name,if z.heldItem==Block.Air then 3f else 1f+ToolSystem.damage(z.heldItem)*0.65f))
    }
    if z.memory>0f then
      val targetDelta=z.lastKnown-z.pos
      val distance=sqrt(targetDelta.x*targetDelta.x+targetDelta.z*targetDelta.z).toFloat
      if distance>1.1f || abs(targetDelta.y)>1.0f then
        if z.routeTimer<=0 || (z.routeGoal-z.lastKnown).lengthSquared>4f then
          val cells=new VoxelNavigation.Cells:
            def at(x: Int,y: Int,zz: Int): Int =
              val b=worldAt(x,y,zz)
              (if b.solid then VoxelNavigation.SOLID else 0) | (if b==Block.Water then VoxelNavigation.WATER else 0) |
                (if b==Block.Cactus then VoxelNavigation.HAZARD else 0)
          val path=VoxelNavigation.find(cells,floor(z.pos.x).toInt,round(z.pos.y),floor(z.pos.z).toInt,
            floor(z.lastKnown.x).toInt,round(z.lastKnown.y),floor(z.lastKnown.z).toInt,24,768)
          z.route=path.asScala.map(p => Vec3(p.x+0.5f,p.y,p.z+0.5f)).toVector
          z.routeIndex=0; z.routeTimer=0.9f+Math.floorMod(z.id*7,11)*0.06f; z.routeGoal=z.lastKnown
        while z.routeIndex<z.route.length &&
            Vec3(z.route(z.routeIndex).x-z.pos.x,0,z.route(z.routeIndex).z-z.pos.z).lengthSquared<0.12f &&
            abs(z.route(z.routeIndex).y-z.pos.y)<0.6f do z.routeIndex+=1
        val waypoint=if z.routeIndex<z.route.length then z.route(z.routeIndex) else z.lastKnown
        val direction=Vec3(waypoint.x-z.pos.x,0,waypoint.z-z.pos.z).normalized
        z.yaw=toDegrees(atan2(direction.x,-direction.z)).toFloat
        if waypoint.y>z.pos.y+0.45f && z.grounded && !collides(z.pos+Vec3(0,1.05f,0),at) then
          z.velocity=Vec3(z.velocity.x,6.7f,z.velocity.z); z.grounded=false
        val step=direction*(if wet then 0.85f else 1.65f)*dt
        def move(dx: Float,dz: Float): Boolean =
          val next=z.pos+Vec3(dx,0,dz)
          val nx=floor(next.x+direction.x*0.30f).toInt; val nz=floor(next.z+direction.z*0.30f).toInt
          val support=(1 to 3).exists(d => at(nx,floor(z.pos.y).toInt-d,nz).solid)
          val safe=wet || !z.grounded || support || at(nx,floor(z.pos.y).toInt,nz)==Block.Water
          val hazard=at(nx,floor(z.pos.y).toInt,nz)==Block.Cactus
          if safe && !hazard && !collides(next,at) then {z.pos=next; true} else false
        move(step.x,0); move(0,step.z)
        if (z.pos-before).lengthSquared<0.00001f then z.stuck+=dt else z.stuck=0f
        if z.stuck>0.65f then
          z.routeTimer=0f; z.stuck=0f
    else z.route=Vector.empty
    z.velocity=Vec3(z.velocity.x*exp(-dt*7).toFloat,(z.velocity.y-(if wet then 3f else 20f)*dt).max(-20f),z.velocity.z*exp(-dt*7).toFloat)
    if wet then z.velocity=Vec3(z.velocity.x,(z.velocity.y+dt*5).min(2f),z.velocity.z)
    val shove=Vec3(z.velocity.x*dt,0,z.velocity.z*dt)
    if !collides(z.pos+shove,at) then z.pos+=shove
    var remaining=z.velocity.y*dt
    while abs(remaining)>0.0001f do
      val amount=remaining.max(-0.15f).min(0.15f)
      val next=z.pos+Vec3(0,amount,0)
      if collides(next,at) then
        if amount<0 then z.grounded=true
        z.velocity=Vec3(z.velocity.x,0,z.velocity.z); remaining=0f
      else
        z.pos=next; remaining-=amount; z.grounded=false
    val moved=sqrt(pow(z.pos.x-before.x,2)+pow(z.pos.z-before.z,2)).toFloat
    z.stride+=( (moved/dt.max(0.001f)/1.65f).min(1f)-z.stride)*(1f-exp(-dt*10).toFloat)
    z.gait+=moved*4.2f
    attacking

  /** Slab intersection against the actual body, so a wall or a nearer block wins. */
  def rayDistance(origin: Vec3,direction: Vec3,z: Zombie,reach: Float): Option[Float] =
    var near=0f; var far=reach
    val lo=z.pos-Vec3(0.32f,0,0.32f); val hi=z.pos+Vec3(0.32f,1.85f,0.32f)
    for (o,d,a,b) <- Seq((origin.x,direction.x,lo.x,hi.x),(origin.y,direction.y,lo.y,hi.y),(origin.z,direction.z,lo.z,hi.z)) do
      if abs(d)<0.00001f then
        if o<a || o>b then far = -1f
      else
        val t0=(a-o)/d; val t1=(b-o)/d
        near=near.max(t0.min(t1)); far=far.min(t0.max(t1))
    if near<=far && far>=0f then Some(near) else None

  def spawnEquipment(rng: scala.util.Random): Block =
    if rng.nextFloat()>=0.08f then Block.Air
    else
      val roll=rng.nextFloat()
      val tier=if roll<0.42f then 0 else if roll<0.75f then 1 else if roll<0.95f then 2 else 3
      ToolSystem.materials(tier)._2(rng.nextInt(4))

  def separate(mobs: Seq[Zombie],at: (Int,Int,Int) => Block): Unit =
    for i <- mobs.indices; j <- i+1 until mobs.length do
      val a=mobs(i); val b=mobs(j)
      val delta=Vec3(a.pos.x-b.pos.x,0,a.pos.z-b.pos.z)
      val length=sqrt(delta.lengthSquared).toFloat
      if length<0.58f && abs(a.pos.y-b.pos.y)<1.5f then
        val direction=if length>0.001f then delta*(1f/length) else Vec3(if a.id%2==0 then 1 else -1,0,0)
        val push=direction*((0.58f-length)*0.25f)
        if !collides(a.pos+push,at) then a.pos+=push
        if !collides(b.pos-push,at) then b.pos-=push

  def snapshot(zombies: Seq[Zombie]): String =
    "MOBS"+zombies.map(z => s"|${z.id},${z.pos.x},${z.pos.y},${z.pos.z},${z.yaw},${z.health.max(0)},${z.burning},${z.hurt},${z.heldItem.ordinal}").mkString
  def parseSnapshot(line: String): Vector[(Int,Vec3,Float,Float,Float,Float,Block)] =
    val parts=line.split("\\|",-1)
    require(parts.head=="MOBS" && parts.length<=33,"Invalid mob snapshot")
    val result=parts.drop(1).map { token =>
      val fields=token.split(','); require(fields.length==8 || fields.length==9)
      val id=fields(0).toInt; val values=fields.slice(1,8).map(_.toFloat)
      require(id>=0 && values.forall(_.isFinite) && abs(values(0))<30000000 && abs(values(2))<30000000 && values(1)> -128 && values(1)<512)
      require(values(4)>=0 && values(4)<=20 && values(5)>=0 && values(5)<=4 && values(6)>=0 && values(6)<=1)
      val held=if fields.length==9 then Block.fromOrdinal(fields(8).toInt) else Block.Air
      require(held==Block.Air || ToolSystem.isTool(held),"Invalid zombie equipment")
      (id,Vec3(values(0),values(1),values(2)),values(3),values(4),values(5),values(6),held)
    }.toVector
    require(result.map(_._1).distinct.length==result.length,"Duplicate mob IDs")
    result
