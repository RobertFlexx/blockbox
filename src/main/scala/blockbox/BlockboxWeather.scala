package blockbox

import org.lwjgl.opengl.GL11.*
import scala.math.*

/** Spatial lightning. Rain belongs to the roof-occluded streak pass, never flat cloud curtains. */
object BlockboxWeather:
  def render(
      camera: Vec3,
      strikes: Seq[WeatherField.Strike]
  ): Unit =
    if !strikes.exists(_.age < 0.28f) then return
    BlockboxRender2D.resetGlArraysAndBuffers()
    glDisable(GL_TEXTURE_2D); glDisable(GL_CULL_FACE); glDisable(GL_ALPHA_TEST)
    glEnable(GL_DEPTH_TEST); glDepthMask(false)
    glEnable(GL_BLEND); glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA)
    glDisable(GL_FOG)
    strikes.filter(_.age < 0.28f).foreach { strike =>
      val alpha = exp(-strike.age * 11).toFloat * strike.strength
      def bolt(width: Float, opacity: Float): Unit =
        glLineWidth(width); glColor4f(0.86f, 0.93f, 1f, opacity)
        glBegin(GL_LINE_STRIP)
        for i <- 0 to 9 do
          val jitter =
            if i == 0 || i == 9 then 0f else sin(i * 4.17f + (strike.id % 997).toFloat).toFloat * 6f
          glVertex3f(
            strike.x - camera.x + jitter,
            163f - i * 14f,
            strike.z - camera.z + jitter * 0.45f
          )
        glEnd()
      bolt(5f, alpha * 0.22f); bolt(1.7f, alpha)
    }
    glLineWidth(1f); glColor4f(1, 1, 1, 1); glDepthMask(true); glDisable(GL_BLEND)
