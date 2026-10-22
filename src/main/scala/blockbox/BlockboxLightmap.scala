package blockbox

/** Minecraft-style light colours, shared by the 16x16 terrain lightmap and by everything
  * lit on the CPU (players, the held item, particles), so they all agree with terrain.
  *
  * Sky light is tinted by the time of day: neutral at noon, warm while the sun is low,
  * blue moonlight at night, grey and dimmer in rain, and briefly white in a lightning
  * flash. Block light uses Minecraft's warm torch ramp (orange when dim, near white at
  * the source) with a small flicker.
  */
object BlockboxLightmap:
  /** @param daylight Main's daylight factor (0.18 at midnight .. 1 at noon)
    * @param dusk 0..1 weight of the low-sun warm tint
    * @param flicker multiplier around 1 applied to block light
    * @param darkNights the "world darkening" setting: off keeps nights and caves bright
    */
  final case class Environment(daylight: Float, dusk: Float, rain: Float, flash: Float, flicker: Float, darkNights: Boolean)

  object Environment:
    val Noon: Environment = Environment(1f, 0f, 0f, 0f, 1f, darkNights = true)
    val Midnight: Environment = Environment(0.18f, 0f, 0f, 0f, 1f, darkNights = true)

  private def clamp01(v: Float): Float = if v < 0f then 0f else if v > 1f then 1f else v
  private def smooth01(v: Float): Float = { val t = clamp01(v); t * t * (3f - 2f * t) }
  private def mix(a: Float, b: Float, t: Float): Float = a + (b - a) * t

  /** Minecraft's level/(4 - 3 level) falloff: light drops quickly away from its source. */
  def curve(level: Float): Float =
    val f = clamp01(level)
    f / (4f - 3f * f)

  /** Small, smooth torch flicker around 1 for a given time in seconds. */
  def flicker(time: Float): Float =
    val t = time.toDouble
    (1.0 + 0.026 * math.sin(t * 7.3) + 0.018 * math.sin(t * 12.9 + 1.7) + 0.010 * math.sin(t * 23.1 + 0.4)).toFloat

  /** Light colour for sky and block levels in 0..1 (level / 15). */
  def color(sky: Float, block: Float, e: Environment): (Float, Float, Float) =
    val day = smooth01((e.daylight - 0.18f) / 0.82f)
    val nightFloor = if e.darkNights then 0.15f else 0.62f
    val skyScale = (mix(nightFloor, 1f, day) * (1f - clamp01(e.rain) * 0.24f) + clamp01(e.flash) * 0.55f).min(1.15f)
    val dusk = clamp01(e.dusk)
    val dayWeight = smooth01(day * 1.5f)
    val rain = clamp01(e.rain) * 0.55f
    // Noon is almost neutral, a low sun is warm and the night side is moonlit blue.
    var tr = mix(0.52f, mix(1.00f, 1.00f, dusk), dayWeight)
    var tg = mix(0.62f, mix(0.98f, 0.80f, dusk), dayWeight)
    var tb = mix(1.00f, mix(0.94f, 0.64f, dusk), dayWeight)
    tr = mix(tr, 0.86f, rain); tg = mix(tg, 0.89f, rain); tb = mix(tb, 0.94f, rain)
    val s = curve(sky) * skyScale
    val b = curve(block) * e.flicker
    var r = s * tr + b
    var g = s * tg + b * ((b * 0.6f + 0.4f) * 0.6f + 0.4f) * 0.93f
    var bl = s * tb + b * (b * b * 0.6f + 0.4f) * 0.70f
    // A faint ambient floor keeps the darkest caves readable, then a partial
    // 1-(1-c)^4 lift mirrors Minecraft's brightness slider.
    val floor = if e.darkNights then 0.032f else 0.20f
    val lift = if e.darkNights then 0.36f else 0.72f
    def finish(c: Float, f: Float): Float =
      val base = clamp01(c * (1f - f) + f)
      val inv = 1f - base
      clamp01(mix(base, 1f - inv * inv * inv * inv, lift))
    r = finish(r, floor); g = finish(g, floor); bl = finish(bl, floor * 1.12f)
    (r, g, bl)
