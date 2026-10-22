package blockbox

import org.lwjgl.BufferUtils
import org.lwjgl.opengl.GL11.*
import scala.collection.mutable
import scala.math.*

/** World-anchored rain curtains with a generated streak texture, roof occlusion,
  * depth testing, soft range fades and surface impacts. Nothing follows the eye. */
object RainRenderer:
  private var texture=0
  private var seed=Long.MinValue
  private val heights=mutable.HashMap.empty[(Int,Int),Int]
  private var refreshed= -100f
  private def hash(x: Int,z: Int): Int =
    var n=x*73428767 ^ z*912931 ^ seed.toInt
    n=(n^(n>>>13))*1274126177
    (n^(n>>>16)) & 0x7fffffff

  def invalidate(x: Int,z: Int): Unit = heights.remove((x,z))
  def reset(): Unit = { heights.clear(); refreshed = -100f }

  private def initialize(): Unit =
    val w=32; val h=64; val data=BufferUtils.createByteBuffer(w*h*4)
    for y <- 0 until h; x <- 0 until w do
      var alpha=0f
      for i <- 0 until 5 do
        val sx=(i*13+3)%w; val sy=(i*29+7)%h
        val dx=abs(x-sx).min(w-abs(x-sx)).toFloat
        val dy=Math.floorMod(y-sy,h).toFloat
        val length=9f+i%4*3f
        val streak=if dy<length then pow(sin(dy/length*Pi).max(0),0.65).toFloat else 0f
        alpha=alpha.max((1f-dx/1.2f).max(0f)*streak*(0.48f+i%3*0.18f))
      data.put(190.toByte).put(211.toByte).put(235.toByte).put((alpha*255).toInt.toByte)
    data.flip(); texture=glGenTextures(); glBindTexture(GL_TEXTURE_2D,texture)
    glTexImage2D(GL_TEXTURE_2D,0,GL_RGBA8,w,h,0,GL_RGBA,GL_UNSIGNED_BYTE,data)
    glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_MIN_FILTER,GL_LINEAR); glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_MAG_FILTER,GL_LINEAR)
    glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_WRAP_S,GL_REPEAT); glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_WRAP_T,GL_REPEAT)

  def render(camera: Vec3,yaw: Float,time: Float,daylight: Float,rain: Float,wind: Float,cells: Seq[WeatherField.Cell],worldSeed: Long,
             at: (Int,Int,Int) => Block,loaded: (Int,Int) => Boolean): Unit =
    if rain<0.015f || cells.isEmpty then return
    if seed!=worldSeed then { reset(); seed=worldSeed }
    // Recheck nearby roofs periodically too, including edits from other players.
    if time-refreshed>1.0f || time<refreshed then { heights.clear(); refreshed=time }
    if texture==0 then initialize()
    BlockboxRender2D.resetGlArraysAndBuffers()
    glEnable(GL_DEPTH_TEST); glDepthMask(false); glDisable(GL_CULL_FACE); glDisable(GL_ALPHA_TEST)
    glEnable(GL_BLEND); glBlendFunc(GL_SRC_ALPHA,GL_ONE_MINUS_SRC_ALPHA); glEnable(GL_FOG)
    glEnable(GL_TEXTURE_2D); glBindTexture(GL_TEXTURE_2D,texture)
    val radius=28; val bx=floor(camera.x).toInt; val bz=floor(camera.z).toInt
    val light=0.42f+daylight*0.52f
    val angle=toRadians(yaw); val rx=cos(angle).toFloat; val rz=sin(angle).toFloat
    glBegin(GL_QUADS)
    for x <- bx-radius to bx+radius; z <- bz-radius to bz+radius do
      val dx=x+0.5f-camera.x; val dz=z+0.5f-camera.z; val dist2=dx*dx+dz*dz
      if dist2<radius*radius && dist2>0.30f && loaded(x,z) then
        val h=hash(x,z)
        val strength=WeatherField.sample(cells,x+0.5f,z+0.5f).rain
        if strength>0.025f && h%100<(22+strength*23).toInt then
          val ground=heights.getOrElseUpdate((x,z),{
            var y=Terrain.worldHeight-1
            while y>0 && !at(x,y,z).solid && at(x,y,z)!=Block.Water do y-=1
            y+1
          }).toFloat
          val low=(camera.y-13f).max(ground); val high=(camera.y+15f).max(low)
          if high>low && ground<camera.y+15f then
            val distance=sqrt(dist2).toFloat
            val fade=1f-BlockboxSky.smooth01((distance-14f)/14f)
            val alpha=strength*fade*(0.25f+(h%5)*0.025f)
            val px=dx+((h>>>7)%100/100f-0.5f)*0.5f
            val pz=dz+((h>>>14)%100/100f-0.5f)*0.5f
            val width=0.38f
            val drift=wind*(high-low)*0.035f
            val offset=(h%997)/997f+time*(0.85f+(h%7)*0.023f)
            val u=(h%17)/17f
            glColor4f(light,light,light,alpha)
            glTexCoord2f(u,low*0.19f+offset); glVertex3f(px-rx*width,low,pz-rz*width)
            glTexCoord2f(u+1,low*0.19f+offset); glVertex3f(px+rx*width,low,pz+rz*width)
            glColor4f(light,light,light,alpha*0.25f)
            glTexCoord2f(u+1,high*0.19f+offset); glVertex3f(px+rx*width-drift,high,pz+rz*width-drift*0.25f)
            glTexCoord2f(u,high*0.19f+offset); glVertex3f(px-rx*width-drift,high,pz-rz*width-drift*0.25f)
    glEnd(); glBindTexture(GL_TEXTURE_2D,0); glDisable(GL_TEXTURE_2D)
    glBegin(GL_QUADS)
    for x <- bx-15 to bx+15; z <- bz-15 to bz+15 if loaded(x,z) && hash(x,z)%4==0 do
      heights.get((x,z)).foreach { top =>
        val h=hash(x,z)
        val phase=(time*(1.4f+(h%11)*0.07f)+(h%997)/997f)%1f
        val strength=WeatherField.sample(cells,x+0.5f,z+0.5f).rain
        if phase<0.7f && abs(top-camera.y)<14f then
          val dx=x+0.5f-camera.x; val dz=z+0.5f-camera.z
          val fade=(1f-(dx*dx+dz*dz)/225f).max(0f)*strength
          val y=top+0.012f
          val r=0.045f+phase*0.36f; val w=0.014f
          glColor4f(0.56f*light,0.74f*light,light,(1f-phase/0.7f)*fade*0.38f)
          for side <- 0 until 4 do
            val a=side*Pi/2; val b=(side+1)*Pi/2
            glVertex3f(dx+cos(a).toFloat*r,y,dz+sin(a).toFloat*r)
            glVertex3f(dx+cos(b).toFloat*r,y,dz+sin(b).toFloat*r)
            glVertex3f(dx+cos(b).toFloat*(r+w),y,dz+sin(b).toFloat*(r+w))
            glVertex3f(dx+cos(a).toFloat*(r+w),y,dz+sin(a).toFloat*(r+w))
          if phase<0.22f then
            val lift=sin(phase/0.22f*Pi).toFloat*0.18f
            glColor4f(0.65f*light,0.80f*light,light,fade*0.50f)
            for side <- Seq(-1,1) do
              val px=dx+side*phase*0.32f
              glVertex3f(px-rx*0.018f,y+lift,dz-rz*0.018f); glVertex3f(px+rx*0.018f,y+lift,dz+rz*0.018f)
              glVertex3f(px+rx*0.018f,y+lift+0.06f,dz+rz*0.018f); glVertex3f(px-rx*0.018f,y+lift+0.06f,dz-rz*0.018f)
      }
    glEnd(); glColor4f(1,1,1,1); glDepthMask(true); glDisable(GL_BLEND)

  def destroy(): Unit =
    if texture!=0 then glDeleteTextures(texture)
    texture=0; reset()
