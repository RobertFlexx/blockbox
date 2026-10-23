package blockbox

import org.lwjgl.BufferUtils
import org.lwjgl.opengl.GL11.*
import org.lwjgl.opengl.GL12.GL_CLAMP_TO_EDGE
import scala.collection.mutable.ArrayBuffer
import scala.math.*
import scala.util.Random

/** Living torches: a soft flickering glow around each nearby flame, small flame wisps and drifting
  * smoke, like Minecraft's torch particles. Flame positions come from the chunks' light sources and
  * are refreshed a few times per second by the game.
  */
object TorchEffects:
  final case class Flame(x: Float, y: Float, z: Float)
  private final class Particle(
      var x: Float,
      var y: Float,
      var z: Float,
      var vx: Float,
      var vy: Float,
      var vz: Float,
      var life: Float,
      val maxLife: Float,
      val size: Float,
      val smoke: Boolean,
      val phase: Float
  )
  private val particles = ArrayBuffer.empty[Particle]
  private val rng = Random(0x70c4b1L)
  private var flames: Array[Flame] = Array.empty
  private var glowTexture = 0
  private val maxParticles = 480
  private val spawnRadius = 22f
  private val glowRadius = 48f

  def setFlames(next: Array[Flame]): Unit = flames = next
  def flameCount: Int = flames.length
  def particleCount: Int = particles.length
  def clear(): Unit = { particles.clear(); flames = Array.empty }

  /** Per-flame flicker around 1, desynchronised by position. */
  def flicker(f: Flame, time: Float): Float =
    val seed = f.x * 12.9898f + f.z * 78.233f + f.y * 3.17f
    (1.0 + 0.07 * sin(time * 9.1 + seed) + 0.05 * sin(time * 15.3 + seed * 1.7) + 0.03 * sin(
      time * 23.7 + seed * 0.3
    )).toFloat

  def update(dt: Float, camera: Vec3): Unit =
    var i = particles.length - 1
    while i >= 0 do
      val p = particles(i)
      p.life -= dt
      if p.life <= 0f then
        particles(i) = particles(particles.length - 1)
        particles.remove(particles.length - 1)
      else
        if p.smoke then
          // Smoke slows as it rises and curls sideways.
          val age = 1f - p.life / p.maxLife
          p.vx += sin(p.phase + age * 6f).toFloat * 0.12f * dt
          p.vz += cos(p.phase * 1.3f + age * 5f).toFloat * 0.12f * dt
          p.vy = (p.vy - 0.10f * dt).max(0.10f)
        p.x += p.vx * dt; p.y += p.vy * dt; p.z += p.vz * dt
      i -= 1
    if dt <= 0f then return
    val r2 = spawnRadius * spawnRadius
    var f = 0
    while f < flames.length && particles.length < maxParticles do
      val flame = flames(f)
      val dx = flame.x - camera.x; val dy = flame.y - camera.y; val dz = flame.z - camera.z
      if dx * dx + dy * dy + dz * dz < r2 then
        if rng.nextFloat() < dt * 1.7f then
          val life = 0.35f + rng.nextFloat() * 0.30f
          particles += Particle(
            flame.x + (rng.nextFloat() - 0.5f) * 0.05f,
            flame.y - 0.02f,
            flame.z + (rng.nextFloat() - 0.5f) * 0.05f,
            0f,
            0.20f + rng.nextFloat() * 0.12f,
            0f,
            life,
            life,
            0.030f + rng.nextFloat() * 0.018f,
            false,
            rng.nextFloat() * 6.28f
          )
        if rng.nextFloat() < dt * 0.85f then
          val life = 1.4f + rng.nextFloat() * 1.1f
          particles += Particle(
            flame.x + (rng.nextFloat() - 0.5f) * 0.06f,
            flame.y + 0.12f,
            flame.z + (rng.nextFloat() - 0.5f) * 0.06f,
            (rng.nextFloat() - 0.5f) * 0.05f,
            0.42f + rng.nextFloat() * 0.18f,
            (rng.nextFloat() - 0.5f) * 0.05f,
            life,
            life,
            0.045f + rng.nextFloat() * 0.025f,
            true,
            rng.nextFloat() * 6.28f
          )
      f += 1

  private def ensureTexture(): Unit =
    if glowTexture != 0 then return
    val size = 32
    val buffer = BufferUtils.createByteBuffer(size * size * 4)
    for y <- 0 until size; x <- 0 until size do
      val dx = (x + 0.5f) / size * 2f - 1f; val dy = (y + 0.5f) / size * 2f - 1f
      val r = sqrt(dx * dx + dy * dy).toFloat.min(1f)
      val a = pow(1f - r, 2.2).toFloat
      buffer.put(255.toByte).put(255.toByte).put(255.toByte).put((a * 255f).round.toByte)
    buffer.flip()
    glowTexture = glGenTextures()
    glBindTexture(GL_TEXTURE_2D, glowTexture)
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR)
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR)
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE)
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE)
    glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA8, size, size, 0, GL_RGBA, GL_UNSIGNED_BYTE, buffer)
    glBindTexture(GL_TEXTURE_2D, 0)

  /** Draws in the world pass: x/z relative to the camera, y absolute (as block particles).
    * @param night
    *   0 at noon .. 1 at night; the glow is subtle by day and strong at night
    * @param light
    *   terrain light colour at a world position, for smoke
    */
  def render(
      camera: Vec3,
      yaw: Float,
      pitch: Float,
      time: Float,
      night: Float,
      light: (Float, Float, Float) => (Float, Float, Float)
  ): Unit =
    if flames.isEmpty && particles.isEmpty then return
    ensureTexture()
    val forward = BlockboxRaycast.viewDirection(yaw, pitch)
    val ry = toRadians(yaw)
    val right = Vec3(cos(ry).toFloat, 0f, sin(ry).toFloat)
    val up = Vec3(
      right.y * forward.z - right.z * forward.y,
      right.z * forward.x - right.x * forward.z,
      right.x * forward.y - right.y * forward.x
    )
    def billboard(x: Float, y: Float, z: Float, s: Float): Unit =
      val rx = right.x * s; val rz = right.z * s
      val ux = up.x * s; val uy = up.y * s; val uz = up.z * s
      val px = x - camera.x; val pz = z - camera.z
      glTexCoord2f(0f, 0f); glVertex3f(px - rx - ux, y - uy, pz - rz - uz)
      glTexCoord2f(1f, 0f); glVertex3f(px + rx - ux, y - uy, pz + rz - uz)
      glTexCoord2f(1f, 1f); glVertex3f(px + rx + ux, y + uy, pz + rz + uz)
      glTexCoord2f(0f, 1f); glVertex3f(px - rx + ux, y + uy, pz - rz + uz)
    glEnable(GL_TEXTURE_2D); glBindTexture(GL_TEXTURE_2D, glowTexture)
    glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_MODULATE)
    glEnable(GL_DEPTH_TEST); glDepthMask(false); glEnable(GL_BLEND)
    glDisable(GL_CULL_FACE); glDisable(GL_ALPHA_TEST)
    val fogWasOn = glIsEnabled(GL_FOG)
    // Smoke first, alpha blended and fogged like the terrain behind it.
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA)
    glBegin(GL_QUADS)
    particles.foreach { p =>
      if p.smoke then
        val t = p.life / p.maxLife
        val (lr, lg, lb) = light(p.x, p.y, p.z)
        val grey = 0.30f + 0.08f * t
        glColor4f(grey * lr, grey * lg, grey * lb, 0.42f * t * (1f - t).min(0.25f) * 4f)
        billboard(p.x, p.y, p.z, p.size * (1.0f + (1f - t) * 1.6f))
    }
    glEnd()
    // Glow and flame wisps add light, so fog must not tint them; they fade with distance.
    glDisable(GL_FOG)
    glBlendFunc(GL_SRC_ALPHA, GL_ONE)
    glBegin(GL_QUADS)
    val strength = 0.14f + 0.30f * night.max(0f).min(1f)
    flames.foreach { f =>
      val dx = f.x - camera.x; val dy = f.y - camera.y; val dz = f.z - camera.z
      val d = sqrt(dx * dx + dy * dy + dz * dz).toFloat
      if d < glowRadius && d > 0.2f then
        val fade = (1f - d / glowRadius).min(1f) * (d / 1.2f).min(1f)
        val k = flicker(f, time)
        // Pull the sprite toward the camera so the support wall does not clip it.
        val pull = 0.32f.min(d * 0.5f) / d
        glColor4f(1.0f, 0.64f, 0.28f, strength * fade * k)
        billboard(f.x - dx * pull, f.y - dy * pull, f.z - dz * pull, 0.62f * k)
        glColor4f(1.0f, 0.86f, 0.55f, strength * 0.9f * fade * k)
        billboard(f.x - dx * pull, f.y - dy * pull, f.z - dz * pull, 0.20f * k)
    }
    particles.foreach { p =>
      if !p.smoke then
        val t = p.life / p.maxLife
        glColor4f(1.0f, 0.62f + 0.3f * t, 0.22f + 0.2f * t, 0.85f * t)
        billboard(p.x, p.y, p.z, p.size * (0.45f + 0.55f * t))
    }
    glEnd()
    if fogWasOn then glEnable(GL_FOG)
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA)
    glDisable(GL_BLEND); glDepthMask(true)
    glColor4f(1f, 1f, 1f, 1f); glBindTexture(GL_TEXTURE_2D, 0); glDisable(GL_TEXTURE_2D)

  def destroy(): Unit =
    if glowTexture != 0 then glDeleteTextures(glowTexture)
    glowTexture = 0
    clear()
