package blockbox

import org.lwjgl.opengl.GL11.*
import scala.math.*

object ZombieFire:
  def render(z: ZombieSystem.Zombie,camera: Vec3,time: Float): Unit =
    glDisable(GL_TEXTURE_2D); glDisable(GL_FOG); glDisable(GL_CULL_FACE)
    glEnable(GL_BLEND); glBlendFunc(GL_SRC_ALPHA,GL_ONE_MINUS_SRC_ALPHA)
    glEnable(GL_DEPTH_TEST); glDepthMask(false)
    glPushMatrix(); glTranslatef(z.pos.x-camera.x,z.pos.y,z.pos.z-camera.z)
    glBegin(GL_TRIANGLES)
    for plane <- 0 until 4; i <- 0 until 6 do
      val angle=plane*Pi/4
      val rx=cos(angle).toFloat; val rz=sin(angle).toFloat
      val pos=(i-2.5f)*0.13f
      val height=0.60f+(0.5f+0.5f*sin(time*9+i*2.3f+z.id)).toFloat*1.20f
      val sway=sin(time*7+i*2.8f).toFloat*0.07f
      val depth=if plane%2==0 then 0.30f else -0.28f
      def point(p: Float,y: Float): Unit = glVertex3f(p*rx-rz*depth,y,p*rz+rx*depth)
      glColor4f(1f,0.72f,0.10f,0.78f); point(pos-0.11f,0.10f); point(pos+0.11f,0.10f)
      glColor4f(0.95f,0.22f,0.02f,0f); point(pos+sway,height)
      glColor4f(1f,0.91f,0.38f,0.85f); point(pos-0.055f,0.12f); point(pos+0.06f,0.12f)
      glColor4f(1f,0.56f,0.02f,0.1f); point(pos-sway*0.5f,height*0.65f)
    glEnd(); glPopMatrix()
    glColor4f(1,1,1,1); glDepthMask(true); glEnable(GL_FOG); glDisable(GL_BLEND)
