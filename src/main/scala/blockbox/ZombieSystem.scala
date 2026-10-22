package blockbox

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
    val nearest=players.filter(_.vulnerable).minByOption(p => (p.eye-z.pos-Vec3(0,1.2f,0)).lengthSquared)
    val before=z.pos
    var attacking: Option[Attack]=None
    nearest.foreach { player =>
      val delta=player.eye-z.pos-Vec3(0,1.0f,0)
      val horizontal=Vec3(delta.x,0,delta.z)
      val distance=sqrt(horizontal.lengthSquared).toFloat
      if delta.lengthSquared<40*40 then
        z.yaw=toDegrees(atan2(delta.x,-delta.z)).toFloat
        if distance>1.25f then
          val direction=horizontal.normalized
          val speed=if wet then 0.75f else 1.65f
          val step=direction*speed*dt
          def move(dx: Float,dz: Float): Boolean =
            val next=z.pos+Vec3(dx,0,dz)
            if !collides(next,at) then { z.pos=next; true } else false
          val mx=move(step.x,0); val mz=move(0,step.z)
          if (!mx || !mz) && z.grounded && !collides(z.pos+Vec3(step.x,1.05f,step.z),at) then
            z.velocity=Vec3(z.velocity.x,6.0f,z.velocity.z); z.grounded=false
          else if !mx && !mz then
            val turn=if z.id%2==0 then 1f else -1f
            move(-step.z*turn,step.x*turn)
        else if abs(delta.y)<1.7f && z.attackCooldown<=0 && visible(z.pos+Vec3(0,1.4f,0),player.eye,at) then
          z.attackCooldown=1.1f
          attacking=Some(Attack(player.name,3f))
    }
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

  def snapshot(zombies: Seq[Zombie]): String =
    "MOBS"+zombies.map(z => s"|${z.id},${z.pos.x},${z.pos.y},${z.pos.z},${z.yaw},${z.health.max(0)},${z.burning},${z.hurt}").mkString
  def parseSnapshot(line: String): Vector[(Int,Vec3,Float,Float,Float,Float)] =
    val parts=line.split("\\|",-1)
    require(parts.head=="MOBS" && parts.length<=33,"Invalid mob snapshot")
    parts.drop(1).map { token =>
      val fields=token.split(','); require(fields.length==8)
      val id=fields(0).toInt; val values=fields.drop(1).map(_.toFloat)
      require(id>=0 && values.forall(_.isFinite) && abs(values(0))<30000000 && abs(values(2))<30000000 && values(1)> -128 && values(1)<512)
      require(values(4)>=0 && values(4)<=20 && values(5)>=0 && values(5)<=4 && values(6)>=0 && values(6)<=1)
      (id,Vec3(values(0),values(1),values(2)),values(3),values(4),values(5),values(6))
    }.toVector
