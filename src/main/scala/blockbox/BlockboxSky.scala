package blockbox

import org.lwjgl.opengl.GL11.*

import scala.math.*

object BlockboxSky:
  val dayLengthSeconds = 10f * 60f
  val nightLengthSeconds = 8f * 60f
  val dayNightCycleSeconds = dayLengthSeconds + nightLengthSeconds

  def cycleClock(gameTime: Float): Float =
    val raw = gameTime % dayNightCycleSeconds
    if raw < 0f then raw + dayNightCycleSeconds else raw

  def dayPhase(gameTime: Float): Float =
    val t = cycleClock(gameTime)
    if t < dayLengthSeconds then (t / dayLengthSeconds) * 0.5f
    else 0.5f + ((t - dayLengthSeconds) / nightLengthSeconds) * 0.5f

  def daylightFactor(dayPhase: Float): Float =
    val sun = sin(dayPhase * Pi.toFloat * 2f).toFloat
    val eased = smooth01((sun + 0.18f) / 0.58f)
    (0.18f + 0.82f * eased).max(0.18f).min(1f)

  def smooth01(v: Float): Float =
    val t = v.max(0f).min(1f); t * t * (3f - 2f * t)

  def clearColor(underwater: Boolean, dayPhase: Float, daylightFactor: Float): (Float, Float, Float) =
    if underwater then (0.02f, 0.14f, 0.30f)
    else
      val daylight = smooth01(daylightFactor)
      val sunrise = smooth01(max(0f, 1f - abs(dayPhase - 0.78f) * 12f))
      val sunset = smooth01(max(0f, 1f - abs(dayPhase - 0.28f) * 12f))
      val rWarm = if sunrise > 0f || sunset > 0f then 0.25f * (sunrise + sunset) else 0f
      val r = (0.12f + 0.50f * daylight + rWarm).max(0.04f).min(0.95f)
      val g = (0.22f + 0.60f * daylight).max(0.04f).min(0.95f)
      val b = (0.34f + 0.65f * daylight).max(0.06f).min(0.98f)
      (r, g, b)

  def nightDarkness(multiplayerMode: Boolean, daylightFactor: Float): Float =
    // Multiplayer windows joining/leaving should never create a temporary dark
    // screen while their terrain is warming up. Keep the world readable online;
    // singleplayer still has the old day/night dimming.
    if multiplayerMode then 0f
    else
      val night = (1f - (daylightFactor - 0.18f) / 0.82f) * 0.55f
      night.max(0f).min(0.55f)

  def generateStars(): Array[(Float, Float, Float, Float)] =
    val rng = new scala.util.Random(42L)
    Array.tabulate(600) { _ =>
      val theta = rng.nextFloat() * Pi.toFloat * 2f
      val phi = acos((rng.nextFloat() * 2f - 1f).toDouble).toFloat
      val radius = 75f
      val x = (radius * sin(phi) * cos(theta)).toFloat
      val y = radius * cos(phi).toFloat
      val z = (radius * sin(phi) * sin(theta)).toFloat
      val brightness = 0.35f + rng.nextFloat() * 0.65f
      (x, y, z, brightness)
    }

  def renderSky(yaw: Float, pitch: Float, dayPhase: Float, daylightFactor: Float, stars: Array[(Float, Float, Float, Float)]): Unit =
    BlockboxRender2D.resetGlArraysAndBuffers()
    val walk = dayPhase * 2f * Pi.toFloat
    glDisable(GL_FOG)
    glDisable(GL_DEPTH_TEST)
    glDepthMask(false)
    glDisable(GL_TEXTURE_2D)
    glDisable(GL_CULL_FACE)
    glEnable(GL_BLEND)
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA)
    val starVis = smooth01((0.88f - daylightFactor) / 0.34f)

    // Draw sky in camera space. Do not translate by world X/Z, otherwise large
    // coordinates drag the sky into the same precision problem as terrain.
    glMatrixMode(GL_MODELVIEW)
    glPushMatrix()
    glLoadIdentity()
    glRotatef(pitch, 1f, 0f, 0f)
    glRotatef(yaw, 0f, 1f, 0f)

    glPushMatrix()
    glRotatef(walk * -6f, 0f, 0f, 1f)
    glRotatef(35f, 1f, 0f, 0f)
    glPointSize(2.5f)
    glBegin(GL_POINTS)
    if starVis > 0.01f then
      stars.foreach { (sx, sy, sz, sb) =>
        val alpha = (sb * starVis).max(0f).min(1f)
        glColor4f(1f, 1f, 1f, alpha)
        glVertex3f(sx, sy, sz)
      }
    glEnd()
    glPointSize(1f)
    glPopMatrix()

    // Sun and moon are intentionally disabled for now. The lighting cycle and stars stay,
    // but the old discs looked cheap and distracted from the terrain.
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA)
    glPopMatrix()
    glDepthMask(true)
    glEnable(GL_DEPTH_TEST)
    glEnable(GL_FOG)
