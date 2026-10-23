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

  /** World-space celestial directions shared by the sky and terrain shader. */
  def sunDirection(dayPhase: Float): (Float, Float, Float) =
    val walk = dayPhase * Pi.toFloat * 2f
    normalized(cos(walk).toFloat, sin(walk).toFloat, -0.22f)

  def moonDirection(dayPhase: Float): (Float, Float, Float) =
    val s = sunDirection(dayPhase)
    (-s._1, -s._2, -s._3)

  def smooth01(v: Float): Float =
    val t = v.max(0f).min(1f); t * t * (3f - 2f * t)

  def clearColor(
      underwater: Boolean,
      dayPhase: Float,
      daylightFactor: Float,
      rainStrength: Float,
      cloudCover: Float,
      lightningFlash: Float
  ): (Float, Float, Float) =
    if underwater then
      val shift = 0.05f + sin(dayPhase * Pi.toFloat * 6f).toFloat * 0.01f
      (0.015f, 0.12f + shift * 0.18f, 0.22f + shift * 0.42f)
    else
      val color = BlockboxAtmosphere
        .palette(dayPhase, daylightFactor, rainStrength, cloudCover, lightningFlash)
        .horizon
      (color.x, color.y, color.z)

  def nightDarkness(multiplayerMode: Boolean, daylightFactor: Float): Float =
    val night = (1f - (daylightFactor - 0.18f) / 0.82f) * 0.65f
    night.max(0f).min(0.65f)

  def generateStars(): Array[(Float, Float, Float, Float)] =
    val rng = new scala.util.Random(42L)
    // More stars do not mean more draw calls: they still render in a couple of point
    // batches, but the denser field and wider brightness spread make the night sky read
    // like an actual sky instead of sparse white confetti.
    Array.tabulate(2400) { _ =>
      val theta = rng.nextFloat() * Pi.toFloat * 2f
      val phi = acos((rng.nextFloat() * 1.82f - 0.82f).toDouble).toFloat
      val radius = 75f
      val x = (radius * sin(phi) * cos(theta)).toFloat
      val y = radius * cos(phi).toFloat
      val z = (radius * sin(phi) * sin(theta)).toFloat
      val r = rng.nextFloat()
      val brightness = 0.20f + pow(r.toDouble, 2.2).toFloat * 0.80f
      (x, y, z, brightness)
    }

  def renderSky(
      yaw: Float,
      pitch: Float,
      dayPhase: Float,
      daylightFactor: Float,
      stars: Array[(Float, Float, Float, Float)],
      cloudsEnabled: Boolean,
      camera: Vec3,
      gameTime: Float,
      cloudRenderDistanceBlocks: Int,
      rainStrength: Float,
      cloudCover: Float,
      lightningFlash: Float,
      displayDither: Boolean = true
  ): Unit =
    BlockboxRender2D.resetGlArraysAndBuffers()
    val walk = dayPhase * 2f * Pi.toFloat
    glDisable(GL_FOG)
    glDisable(GL_DEPTH_TEST)
    glDepthMask(false)
    glDisable(GL_TEXTURE_2D)
    glDisable(GL_CULL_FACE)
    glEnable(GL_BLEND)
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA)
    glShadeModel(GL_SMOOTH)
    val storm = smooth01(rainStrength.max(cloudCover * 0.94f))
    val lightning = lightningFlash.max(0f).min(1f)
    val starVis = smooth01((-sin(walk).toFloat - 0.035f) / 0.28f) * (1f - storm * 0.97f)
    val daylight = smooth01((daylightFactor + lightning * 0.42f).min(1f))
    val sunrise = 1f - smooth01(abs(sin(dayPhase * Pi.toFloat * 2f).toFloat) / 0.28f)
    val sunset = 0f
    val warm = (sunrise + sunset).min(1f) * (1f - storm * 0.74f)

    // Draw sky in camera space. Do not translate by world X/Z, otherwise large
    // coordinates drag the sky into the same precision problem as terrain.
    glMatrixMode(GL_MODELVIEW)
    glPushMatrix()
    glLoadIdentity()
    glRotatef(pitch, 1f, 0f, 0f)
    glRotatef(yaw, 0f, 1f, 0f)

    val palette =
      BlockboxAtmosphere.palette(dayPhase, daylightFactor, rainStrength, cloudCover, lightningFlash)
    lastPalette = palette
    if !BlockboxAtmosphere.render(yaw, pitch, palette, displayDither, starVis) then
      drawSkyDome(palette)

    glPushMatrix()
    glRotatef(walk * -6f, 0f, 0f, 1f)
    glRotatef(35f, 1f, 0f, 0f)
    if starVis > 0.01f then
      drawStars(stars, starVis, gameTime)
      drawConstellations(starVis, gameTime)
    glPointSize(1f)
    glPopMatrix()

    if !BlockboxCelestials.render(dayPhase, daylightFactor, storm, gameTime, !displayDither) then
      drawSunAndMoon(walk, daylight, warm, storm, lightning)

    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA)
    glDisable(GL_BLEND)
    glDisable(GL_TEXTURE_2D)
    glPopMatrix()
    glShadeModel(GL_SMOOTH)
    glDepthMask(true)
    glEnable(GL_DEPTH_TEST)
    glEnable(GL_FOG)

  private def mix(a: Float, b: Float, t: Float): Float = a + (b - a) * t.max(0f).min(1f)

  private def colorMix(
      a: (Float, Float, Float),
      b: (Float, Float, Float),
      t: Float
  ): (Float, Float, Float) =
    (mix(a._1, b._1, t), mix(a._2, b._2, t), mix(a._3, b._3, t))

  private def drawSkyDome(palette: BlockboxAtmosphere.Palette): Unit =
    // Compatibility fallback uses the same continuous palette as the pixel shader.
    val radius = 82f; val rings = 64; val segments = 96
    var ring = 0
    while ring < rings do
      val a0 = (-Pi / 2 + Pi * ring / rings).toFloat
      val a1 = (-Pi / 2 + Pi * (ring + 1) / rings).toFloat
      val y0 = sin(a0).toFloat; val y1 = sin(a1).toFloat
      val r0 = cos(a0).toFloat; val r1 = cos(a1).toFloat
      glBegin(GL_QUAD_STRIP)
      var i = 0
      while i <= segments do
        val angle = i.toFloat / segments.toFloat * Pi.toFloat * 2
        val x = cos(angle).toFloat; val z = sin(angle).toFloat
        val d0 = Vec3(x * r0, y0, z * r0); val c0 = BlockboxAtmosphere.color(d0, palette)
        glColor4f(c0.x, c0.y, c0.z, 1); glVertex3f(d0.x * radius, d0.y * radius, d0.z * radius)
        val d1 = Vec3(x * r1, y1, z * r1); val c1 = BlockboxAtmosphere.color(d1, palette)
        glColor4f(c1.x, c1.y, c1.z, 1); glVertex3f(d1.x * radius, d1.y * radius, d1.z * radius)
        i += 1
      glEnd(); ring += 1

  private def drawSunAndMoon(
      walk: Float,
      daylight: Float,
      warm: Float,
      storm: Float,
      lightning: Float
  ): Unit =
    val phase = walk / (Pi.toFloat * 2f)
    val sunDir = sunDirection(phase)
    val moonDir = moonDirection(phase)
    val sunAlpha = smooth01((sunDir._2 + 0.10f) / 0.32f) * (1f - storm * 0.88f)
    val moonAlpha = smooth01(
      (moonDir._2 + 0.06f) / 0.28f
    ) * (1f - daylight * 0.42f) * (1f - storm * 0.78f + lightning * 0.12f)
    if sunAlpha > 0.01f then
      val basis = billboardBasis(sunDir)
      glBlendFunc(GL_SRC_ALPHA, GL_ONE)
      drawRadialDisc(
        sunDir,
        basis,
        74.4f,
        35f,
        (1.0f, 0.50f, 0.12f),
        sunAlpha * (0.10f + warm * 0.16f),
        72
      )
      drawRadialDisc(
        sunDir,
        basis,
        74.8f,
        21f,
        (1.0f, 0.74f, 0.24f),
        sunAlpha * (0.20f + warm * 0.13f),
        64
      )
      drawRadialDisc(sunDir, basis, 75.3f, 12f, (1.0f, 0.94f, 0.46f), sunAlpha * 0.42f, 56)
      glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA)
      drawSolidDisc(
        sunDir,
        basis,
        76f,
        7.0f,
        (1.0f, 0.98f - warm * 0.08f, 0.70f - warm * 0.12f),
        sunAlpha,
        52
      )
      drawRadialDisc(sunDir, basis, 75.8f, 4.0f, (1.0f, 1.0f, 0.92f), sunAlpha * 0.72f, 40)
    if moonAlpha > 0.01f then
      val basis = billboardBasis(moonDir)
      glBlendFunc(GL_SRC_ALPHA, GL_ONE)
      drawRadialDisc(moonDir, basis, 74.8f, 20f, (0.44f, 0.56f, 1.0f), moonAlpha * 0.12f, 64)
      drawRadialDisc(moonDir, basis, 75.2f, 11f, (0.62f, 0.75f, 1.0f), moonAlpha * 0.18f, 54)
      glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA)
      drawSolidDisc(moonDir, basis, 76f, 6.4f, (0.86f, 0.91f, 1.0f), moonAlpha * 0.95f, 48)
      drawMoonMare(moonDir, basis, moonAlpha)

  private def drawStars(
      stars: Array[(Float, Float, Float, Float)],
      starVis: Float,
      gameTime: Float
  ): Unit =
    glPointSize(1.7f)
    glBegin(GL_POINTS)
    stars.foreach { (sx, sy, sz, sb) =>
      val twinklePhase = sx * 0.17f + sz * 0.11f + sy * 0.07f
      val twinkle =
        0.78f + 0.22f * sin((twinklePhase + gameTime * (0.42f + sb * 0.36f)).toDouble).toFloat
      val alpha = (sb * starVis * twinkle).max(0f).min(1f)
      val warm = sin(((sx + sz) * 0.015f).toDouble).toFloat * 0.5f + 0.5f
      val cool = sin(((sy - sx) * 0.011f).toDouble).toFloat * 0.5f + 0.5f
      glColor4f(
        (0.72f + sb * 0.24f + warm * 0.06f).min(1f),
        (0.80f + sb * 0.18f + warm * 0.03f).min(1f),
        (0.92f + sb * 0.08f + cool * 0.08f).min(1f),
        alpha
      )
      glVertex3f(sx, sy, sz)
    }
    glEnd()
    glPointSize(3.0f)
    glBegin(GL_POINTS)
    stars.foreach { (sx, sy, sz, sb) =>
      if sb > 0.82f && sy > -12f then
        val alpha = ((sb - 0.82f) * 2.3f * starVis).max(0f).min(0.70f)
        glColor4f(0.9f, 0.94f, 1f, alpha)
        glVertex3f(sx, sy, sz)
    }
    glEnd()

  // Prebuilt once instead of allocating nested arrays and sky-point tuples every
  // frame. Constellations are tiny compared with terrain, but the sky renderer runs
  // forever, so zero-GC presentation code is worth keeping boring and deterministic.
  private val constellationPatterns: Array[Array[(Float, Float)]] = Array(
    Array(
      (-128f, 42f),
      (-114f, 47f),
      (-99f, 43f),
      (-83f, 50f),
      (-67f, 46f),
      (-52f, 53f),
      (-38f, 51f)
    ),
    Array(
      (22f, 30f),
      (33f, 20f),
      (43f, 8f),
      (49f, -4f),
      (57f, -17f),
      (45f, -9f),
      (36f, 0f),
      (33f, 20f)
    ),
    Array((101f, 47f), (111f, 55f), (122f, 48f), (134f, 57f), (147f, 49f)),
    Array((-13f, -26f), (-2f, -15f), (10f, -24f), (20f, -12f), (31f, -21f)),
    Array((165f, 17f), (176f, 31f), (-172f, 25f), (-160f, 39f), (-148f, 30f))
  )

  private def precomputeSkyPoint(
      longitudeDeg: Float,
      latitudeDeg: Float,
      radius: Float
  ): (Float, Float, Float) =
    val lon = toRadians(longitudeDeg.toDouble).toFloat
    val lat = toRadians(latitudeDeg.toDouble).toFloat
    val cl = cos(lat).toFloat
    (cos(lon).toFloat * cl * radius, sin(lat).toFloat * radius, sin(lon).toFloat * cl * radius)

  private val constellationLinePoints: Array[Array[(Float, Float, Float)]] =
    constellationPatterns.map(_.map { case (lon, lat) => precomputeSkyPoint(lon, lat, 74.7f) })
  private val constellationAnchorPoints: Array[(Float, Float, Float)] =
    constellationPatterns.flatten.map { case (lon, lat) => precomputeSkyPoint(lon, lat, 74.9f) }

  private def hash01(i: Int, salt: Int): Float =
    var n = i * 0x45d9f3b + salt * 0x27d4eb2d
    n ^= n >>> 16; n *= 0x45d9f3b; n ^= n >>> 16
    (n & 0xffff).toFloat / 65535f

  private def drawGalaxyBand(starVis: Float, gameTime: Float): Unit =
    // A broad, tilted Milky-Way-like band: two soft translucent strips plus one
    // batched dust field. It is entirely procedural and deterministic, so there is
    // no giant sky texture upload and essentially zero runtime allocation.
    glPushMatrix()
    glRotatef(63f, 1f, 0.15f, 0.35f)
    glRotatef(-18f, 0f, 1f, 0f)
    glBlendFunc(GL_SRC_ALPHA, GL_ONE)
    val radius = 73.6f
    val segments = 128
    var layer = 0
    while layer < 3 do
      val width = 4.8f + layer * 4.5f
      val alpha = starVis * (0.032f - layer * 0.007f)
      glBegin(GL_QUAD_STRIP)
      var i = 0
      while i <= segments do
        val a = i.toFloat / segments.toFloat * Pi.toFloat * 2f
        val wobble = sin((a * 3.0f + layer).toDouble).toFloat * (0.8f + layer * 0.25f)
        val ca = cos(a).toFloat; val sa = sin(a).toFloat
        val y0 = -width + wobble; val y1 = width + wobble
        val edgeAlpha = alpha * (0.76f + 0.24f * sin((a * 2.0f + 0.7f).toDouble).toFloat.abs)
        glColor4f(0.34f, 0.42f, 0.72f, edgeAlpha * 0.28f); glVertex3f(ca * radius, y0, sa * radius)
        glColor4f(0.56f, 0.62f, 0.92f, edgeAlpha); glVertex3f(ca * radius, y1, sa * radius)
        i += 1
      glEnd()
      layer += 1
    glPointSize(1.25f)
    glBegin(GL_POINTS)
    var i = 0
    while i < 520 do
      val a = hash01(i, 9101) * Pi.toFloat * 2f
      val spread = (hash01(i, 9102) - 0.5f) * 14f
      val rr = 74.0f
      val twinkle = 0.70f + 0.30f * sin((gameTime * 0.18f + hash01(i, 9103) * 13f).toDouble).toFloat
      val bright = (0.10f + hash01(i, 9104) * 0.32f) * starVis * twinkle
      glColor4f(0.65f, 0.72f, 1.0f, bright)
      glVertex3f(cos(a).toFloat * rr, spread, sin(a).toFloat * rr)
      i += 1
    glEnd()
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA)
    glPopMatrix()

  private def drawConstellations(starVis: Float, gameTime: Float): Unit =
    // Stylized familiar sky patterns. Line/anchor geometry is precomputed once above;
    // this hot path only submits vertices and computes restrained twinkle brightness.
    glEnable(GL_BLEND); glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA)
    glLineWidth(1.0f)
    var patternIndex = 0
    while patternIndex < constellationLinePoints.length do
      val pattern = constellationLinePoints(patternIndex)
      glBegin(GL_LINE_STRIP)
      var i = 0
      while i < pattern.length do
        val p = pattern(i)
        glColor4f(0.48f, 0.62f, 0.92f, starVis * 0.16f)
        glVertex3f(p._1, p._2, p._3)
        i += 1
      glEnd()
      patternIndex += 1
    glPointSize(3.2f)
    glBegin(GL_POINTS)
    var pi = 0
    while pi < constellationAnchorPoints.length do
      val p = constellationAnchorPoints(pi)
      val tw = 0.78f + 0.22f * sin((gameTime * 0.7f + pi * 1.71f).toDouble).toFloat
      glColor4f(0.88f, 0.93f, 1f, starVis * 0.74f * tw)
      glVertex3f(p._1, p._2, p._3)
      pi += 1
    glEnd(); glLineWidth(1f); glPointSize(1f)

  /** The palette the sky was last drawn with, so clouds dissolve into that exact sky. */
  private var lastPalette: BlockboxAtmosphere.Palette = null

  def renderClouds(
      camera: Vec3,
      gameTime: Float,
      distance: Int,
      phase: Float,
      daylight: Float,
      storm: Float,
      lightning: Float,
      seed: Long
  ): Unit =
    val sunHeight = abs(sin(phase * Pi.toFloat * 2f).toFloat)
    val warm = (1f - smooth01(sunHeight / 0.28f)) * (1f - storm * 0.74f)
    val palette = if lastPalette != null then lastPalette
    else BlockboxAtmosphere.palette(phase, daylight, storm, storm, lightning)
    BlockboxClouds.render(
      camera,
      gameTime,
      distance,
      daylight,
      warm,
      storm,
      lightning,
      seed,
      palette
    )

  def destroyClouds(): Unit = BlockboxClouds.destroy()

  private def valueNoise(x: Int, z: Int): Float =
    val n0 = x * 374761393 + z * 668265263 + 0x27d4eb2d
    val n1 = (n0 ^ (n0 >>> 13)) * 1274126177
    ((n1 ^ (n1 >>> 16)) & 0x7fffffff).toFloat / 2147483647f

  private def smoothValueNoise(x: Float, z: Float): Float =
    val x0 = floor(x).toInt; val z0 = floor(z).toInt
    val tx = smooth01(x - x0.toFloat); val tz = smooth01(z - z0.toFloat)
    val a = mix(valueNoise(x0, z0), valueNoise(x0 + 1, z0), tx)
    val b = mix(valueNoise(x0, z0 + 1), valueNoise(x0 + 1, z0 + 1), tx)
    mix(a, b, tz)

  private def normalized(x: Float, y: Float, z: Float): (Float, Float, Float) =
    val len = sqrt(x * x + y * y + z * z).toFloat.max(0.001f)
    (x / len, y / len, z / len)

  private def billboardBasis(
      dir: (Float, Float, Float)
  ): ((Float, Float, Float), (Float, Float, Float)) =
    val up = if abs(dir._2) > 0.94f then (0f, 0f, 1f) else (0f, 1f, 0f)
    val ux0 = up._2 * dir._3 - up._3 * dir._2
    val uy0 = up._3 * dir._1 - up._1 * dir._3
    val uz0 = up._1 * dir._2 - up._2 * dir._1
    val u = normalized(ux0, uy0, uz0)
    val v = normalized(
      dir._2 * u._3 - dir._3 * u._2,
      dir._3 * u._1 - dir._1 * u._3,
      dir._1 * u._2 - dir._2 * u._1
    )
    (u, v)

  private def drawRadialDisc(
      dir: (Float, Float, Float),
      basis: ((Float, Float, Float), (Float, Float, Float)),
      radius: Float,
      size: Float,
      color: (Float, Float, Float),
      alpha: Float,
      segments: Int
  ): Unit =
    val center = (dir._1 * radius, dir._2 * radius, dir._3 * radius)
    val (u, v) = basis
    glBegin(GL_TRIANGLE_FAN)
    glColor4f(color._1, color._2, color._3, alpha)
    glVertex3f(center._1, center._2, center._3)
    var i = 0
    while i <= segments do
      val a = i.toFloat / segments.toFloat * Pi.toFloat * 2f
      val ca = cos(a).toFloat * size
      val sa = sin(a).toFloat * size
      glColor4f(color._1, color._2, color._3, 0f)
      glVertex3f(
        center._1 + u._1 * ca + v._1 * sa,
        center._2 + u._2 * ca + v._2 * sa,
        center._3 + u._3 * ca + v._3 * sa
      )
      i += 1
    glEnd()

  private def drawSolidDisc(
      dir: (Float, Float, Float),
      basis: ((Float, Float, Float), (Float, Float, Float)),
      radius: Float,
      size: Float,
      color: (Float, Float, Float),
      alpha: Float,
      segments: Int
  ): Unit =
    val center = (dir._1 * radius, dir._2 * radius, dir._3 * radius)
    val (u, v) = basis
    glBegin(GL_TRIANGLE_FAN)
    glColor4f(color._1, color._2, color._3, alpha)
    glVertex3f(center._1, center._2, center._3)
    var i = 0
    while i <= segments do
      val a = i.toFloat / segments.toFloat * Pi.toFloat * 2f
      val ca = cos(a).toFloat * size
      val sa = sin(a).toFloat * size
      glColor4f(color._1, color._2, color._3, alpha * 0.88f)
      glVertex3f(
        center._1 + u._1 * ca + v._1 * sa,
        center._2 + u._2 * ca + v._2 * sa,
        center._3 + u._3 * ca + v._3 * sa
      )
      i += 1
    glEnd()

  private def drawMoonMare(
      dir: (Float, Float, Float),
      basis: ((Float, Float, Float), (Float, Float, Float)),
      alpha: Float
  ): Unit =
    val spots = Array(
      (-1.8f, 1.1f, 1.25f, 0.16f),
      (1.4f, 0.7f, 0.95f, 0.13f),
      (0.4f, -1.5f, 1.15f, 0.12f),
      (-0.9f, -0.5f, 0.70f, 0.10f),
      (2.1f, -1.1f, 0.55f, 0.09f)
    )
    val (u, v) = basis
    val base = (dir._1 * 76.15f, dir._2 * 76.15f, dir._3 * 76.15f)
    spots.foreach { (ox, oy, size, strength) =>
      val spotDir = normalized(
        base._1 + u._1 * ox + v._1 * oy,
        base._2 + u._2 * ox + v._2 * oy,
        base._3 + u._3 * ox + v._3 * oy
      )
      drawRadialDisc(spotDir, basis, 76.2f, size, (0.46f, 0.54f, 0.72f), alpha * strength, 24)
    }

  def destroy(): Unit =
    destroyClouds()
    BlockboxAtmosphere.destroy()
    BlockboxCelestials.destroy()
