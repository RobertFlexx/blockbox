package blockbox

import org.lwjgl.opengl.GL11.*
import scala.math.*

/** Distant precipitation and spatial lightning remain visible outside the wet cell. */
object BlockboxWeather:
  def render(camera: Vec3,time: Float,daylight: Float,cells: Seq[WeatherField.Cell],strikes: Seq[WeatherField.Strike],distance: Int): Unit =
    if cells.isEmpty then return
    BlockboxRender2D.resetGlArraysAndBuffers()
    glDisable(GL_TEXTURE_2D); glDisable(GL_CULL_FACE); glDisable(GL_ALPHA_TEST)
    glEnable(GL_DEPTH_TEST); glDepthMask(false)
    glEnable(GL_BLEND); glBlendFunc(GL_SRC_ALPHA,GL_ONE_MINUS_SRC_ALPHA)
    glEnable(GL_FOG); glFogf(GL_FOG_START,distance*0.68f); glFogf(GL_FOG_END,distance.toFloat)
    val light=0.22f+daylight*0.58f
    cells.foreach { c =>
      val dx=c.x-camera.x; val dz=c.z-camera.z
      if dx*dx+dz*dz<(distance+c.radius)*(distance+c.radius) then
        // Broad overlapping sheets form a soft rain shaft under this cloud bank.
        val angle=atan2(dx,dz).toFloat
        val rx=cos(angle).toFloat; val rz= -sin(angle).toFloat
        glBegin(GL_QUADS)
        var i=0
        while i<18 do
          val fraction=(i+0.5f)/18f*2f-1f
          val offset=fraction*c.radius*0.82f
          val width=c.radius/18f*1.35f
          val depth=sin(i*2.39f+c.id.toFloat%31f).toFloat*c.radius*0.34f
          val x=dx+rx*offset+sin(angle).toFloat*depth
          val z=dz+rz*offset+cos(angle).toFloat*depth
          val alpha=c.strength*(1f-fraction*fraction)*0.075f
          glColor4f(light*0.75f,light*0.85f,light,alpha)
          glVertex3f(x-rx*width,163f,z-rz*width); glVertex3f(x+rx*width,163f,z+rz*width)
          glColor4f(light*0.75f,light*0.85f,light,0f)
          glVertex3f(x+rx*width,25f,z+rz*width); glVertex3f(x-rx*width,25f,z-rz*width)
          i+=1
        glEnd()
    }
    glDisable(GL_FOG)
    strikes.filter(_.age<0.28f).foreach { strike =>
      val alpha=exp(-strike.age*11).toFloat*strike.strength
      def bolt(width: Float,opacity: Float): Unit =
        glLineWidth(width); glColor4f(0.86f,0.93f,1f,opacity)
        glBegin(GL_LINE_STRIP)
        for i <- 0 to 9 do
          val jitter=if i==0 || i==9 then 0f else sin(i*4.17f+(strike.id%997).toFloat).toFloat*6f
          glVertex3f(strike.x-camera.x+jitter,163f-i*14f,strike.z-camera.z+jitter*0.45f)
        glEnd()
      bolt(5f,alpha*0.22f); bolt(1.7f,alpha)
    }
    glLineWidth(1f); glColor4f(1,1,1,1); glDepthMask(true); glDisable(GL_BLEND)
