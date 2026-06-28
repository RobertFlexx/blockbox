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
    val night = (1f - (daylightFactor - 0.18f) / 0.82f) * 0.65f
    night.max(0f).min(0.65f)

  def generateStars(): Array[(Float, Float, Float, Float)] =
    val rng = new scala.util.Random(42L)
    Array.tabulate(900) { _ =>
      val theta = rng.nextFloat() * Pi.toFloat * 2f
      val phi = acos((rng.nextFloat() * 1.72f - 0.72f).toDouble).toFloat
      val radius = 75f
      val x = (radius * sin(phi) * cos(theta)).toFloat
      val y = radius * cos(phi).toFloat
      val z = (radius * sin(phi) * sin(theta)).toFloat
      val brightness = 0.35f + rng.nextFloat() * 0.65f
      (x, y, z, brightness)
    }

  def renderSky(yaw: Float, pitch: Float, dayPhase: Float, daylightFactor: Float, stars: Array[(Float, Float, Float, Float)], cloudsEnabled: Boolean, camera: Vec3, gameTime: Float, cloudRenderDistanceBlocks: Int): Unit =
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
    val starVis = smooth01((0.88f - daylightFactor) / 0.34f)
    val daylight = smooth01(daylightFactor)
    val sunrise = smooth01(max(0f, 1f - abs(dayPhase - 0.78f) * 9f))
    val sunset = smooth01(max(0f, 1f - abs(dayPhase - 0.28f) * 9f))
    val warm = (sunrise + sunset).min(1f)

    // Draw sky in camera space. Do not translate by world X/Z, otherwise large
    // coordinates drag the sky into the same precision problem as terrain.
    glMatrixMode(GL_MODELVIEW)
    glPushMatrix()
    glLoadIdentity()
    glRotatef(pitch, 1f, 0f, 0f)
    glRotatef(yaw, 0f, 1f, 0f)

    drawSkyDome(daylight, warm)
    drawHorizonGlow(walk, warm, daylight)
    drawSunAndMoon(walk, daylight, warm)

    glPushMatrix()
    glRotatef(walk * -6f, 0f, 0f, 1f)
    glRotatef(35f, 1f, 0f, 0f)
    if starVis > 0.01f then
      drawStars(stars, starVis)
    glPointSize(1f)
    glPopMatrix()

    if cloudsEnabled then
      drawClouds(camera, gameTime, cloudRenderDistanceBlocks, daylight, warm)

    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA)
    glDisable(GL_BLEND)
    glDisable(GL_TEXTURE_2D)
    glPopMatrix()
    glShadeModel(GL_SMOOTH)
    glDepthMask(true)
    glEnable(GL_DEPTH_TEST)
    glEnable(GL_FOG)

  private def mix(a: Float, b: Float, t: Float): Float = a + (b - a) * t.max(0f).min(1f)

  private def colorMix(a: (Float, Float, Float), b: (Float, Float, Float), t: Float): (Float, Float, Float) =
    (mix(a._1, b._1, t), mix(a._2, b._2, t), mix(a._3, b._3, t))

  private def drawSkyDome(daylight: Float, warm: Float): Unit =
    val radius = 82f
    val nightTop = (0.012f, 0.018f, 0.050f)
    val nightMid = (0.030f, 0.045f, 0.105f)
    val dayTop = (0.25f, 0.52f, 0.96f)
    val dayMid = (0.58f, 0.78f, 1.00f)
    val top = colorMix(nightTop, dayTop, daylight)
    val midBase = colorMix(nightMid, dayMid, daylight)
    val mid = colorMix(midBase, (0.98f, 0.50f, 0.25f), warm * 0.45f)
    val horizonBase = colorMix((0.025f, 0.030f, 0.070f), (0.78f, 0.90f, 1.00f), daylight)
    val horizon = colorMix(horizonBase, (1.00f, 0.55f, 0.26f), warm * 0.70f)
    val bottom = colorMix((0.015f, 0.018f, 0.035f), (0.55f, 0.68f, 0.86f), daylight)
    val rings = Array(
      (1.00f, top, 1.00f),
      (0.62f, colorMix(top, mid, 0.55f), 1.00f),
      (0.24f, mid, 1.00f),
      (-0.05f, horizon, 1.00f),
      (-0.28f, bottom, 0.96f)
    )
    val segments = 72
    var ri = 0
    while ri < rings.length - 1 do
      val (y0, c0, a0) = rings(ri)
      val (y1, c1, a1) = rings(ri + 1)
      val r0 = sqrt((1f - y0 * y0).max(0f)).toFloat * radius
      val r1 = sqrt((1f - y1 * y1).max(0f)).toFloat * radius
      glBegin(GL_QUAD_STRIP)
      var i = 0
      while i <= segments do
        val a = i.toFloat / segments.toFloat * Pi.toFloat * 2f
        val ca = cos(a).toFloat; val sa = sin(a).toFloat
        glColor4f(c0._1, c0._2, c0._3, a0); glVertex3f(ca * r0, y0 * radius, sa * r0)
        glColor4f(c1._1, c1._2, c1._3, a1); glVertex3f(ca * r1, y1 * radius, sa * r1)
        i += 1
      glEnd()
      ri += 1

  private def drawHorizonGlow(walk: Float, warm: Float, daylight: Float): Unit =
    val alpha = (0.18f + warm * 0.46f + daylight * 0.08f).min(0.62f)
    if alpha <= 0.01f then return
    val dirX = cos(walk).toFloat
    val dirY = sin(walk).toFloat
    val dirZ = -0.18f
    val len = sqrt(dirX * dirX + dirY * dirY + dirZ * dirZ).toFloat.max(0.001f)
    val cx = dirX / len * 78f
    val cy = dirY / len * 78f
    val cz = dirZ / len * 78f
    val sizeX = 36f
    val sizeY = 12f
    glBegin(GL_TRIANGLE_FAN)
    glColor4f(1.0f, 0.52f, 0.24f, alpha * warm.max(0.25f))
    glVertex3f(cx, cy, cz)
    val steps = 40
    var i = 0
    while i <= steps do
      val a = i.toFloat / steps.toFloat * Pi.toFloat * 2f
      glColor4f(1.0f, 0.48f, 0.18f, 0f)
      glVertex3f(cx + cos(a).toFloat * sizeX, cy + sin(a).toFloat * sizeY, cz)
      i += 1
    glEnd()

  private def drawSunAndMoon(walk: Float, daylight: Float, warm: Float): Unit =
    val sunDir = normalized(cos(walk).toFloat, sin(walk).toFloat, -0.22f)
    val moonDir = (-sunDir._1, -sunDir._2, -sunDir._3)
    val sunAlpha = smooth01((sunDir._2 + 0.10f) / 0.32f)
    val moonAlpha = smooth01((moonDir._2 + 0.06f) / 0.28f) * (1f - daylight * 0.42f)
    if sunAlpha > 0.01f then
      val basis = billboardBasis(sunDir)
      glBlendFunc(GL_SRC_ALPHA, GL_ONE)
      drawRadialDisc(sunDir, basis, 74.4f, 35f, (1.0f, 0.50f, 0.12f), sunAlpha * (0.10f + warm * 0.16f), 72)
      drawRadialDisc(sunDir, basis, 74.8f, 21f, (1.0f, 0.74f, 0.24f), sunAlpha * (0.20f + warm * 0.13f), 64)
      drawRadialDisc(sunDir, basis, 75.3f, 12f, (1.0f, 0.94f, 0.46f), sunAlpha * 0.42f, 56)
      glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA)
      drawSolidDisc(sunDir, basis, 76f, 7.0f, (1.0f, 0.98f - warm * 0.08f, 0.70f - warm * 0.12f), sunAlpha, 52)
      drawRadialDisc(sunDir, basis, 75.8f, 4.0f, (1.0f, 1.0f, 0.92f), sunAlpha * 0.72f, 40)
    if moonAlpha > 0.01f then
      val basis = billboardBasis(moonDir)
      glBlendFunc(GL_SRC_ALPHA, GL_ONE)
      drawRadialDisc(moonDir, basis, 74.8f, 20f, (0.44f, 0.56f, 1.0f), moonAlpha * 0.12f, 64)
      drawRadialDisc(moonDir, basis, 75.2f, 11f, (0.62f, 0.75f, 1.0f), moonAlpha * 0.18f, 54)
      glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA)
      drawSolidDisc(moonDir, basis, 76f, 6.4f, (0.86f, 0.91f, 1.0f), moonAlpha * 0.95f, 48)
      drawMoonMare(moonDir, basis, moonAlpha)

  private def drawStars(stars: Array[(Float, Float, Float, Float)], starVis: Float): Unit =
    glPointSize(1.7f)
    glBegin(GL_POINTS)
    stars.foreach { (sx, sy, sz, sb) =>
      val twinkle = 0.82f + 0.18f * sin((sx * 0.17f + sz * 0.11f).toDouble).toFloat
      val alpha = (sb * starVis * twinkle).max(0f).min(1f)
      glColor4f(0.78f + sb * 0.22f, 0.84f + sb * 0.16f, 1f, alpha)
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

  private def drawClouds(camera: Vec3, gameTime: Float, cloudRenderDistanceBlocks: Int, daylight: Float, warm: Float): Unit =
    val cell = 12f
    val cloudY = 192f
    val thickness = 3.0f
    val drift = gameTime * 0.42f
    val radiusCells = ((cloudRenderDistanceBlocks.toFloat * 1.08f) / cell).toInt.max(32).min(160)
    val centerGX = floor((camera.x + drift) / cell).toInt
    val centerGZ = floor((camera.z - drift * 0.28f) / cell).toInt
    val gridMinX = centerGX - radiusCells - 1
    val gridMinZ = centerGZ - radiusCells - 1
    val gridSize = radiusCells * 2 + 3
    val cloudGrid = new Array[Boolean](gridSize * gridSize)
    var pgz = 0
    while pgz < gridSize do
      var pgx = 0
      while pgx < gridSize do
        cloudGrid(pgx * gridSize + pgz) = cloudAt(gridMinX + pgx, gridMinZ + pgz)
        pgx += 1
      pgz += 1
    def gridCloud(gx: Int, gz: Int): Boolean =
      val ix = gx - gridMinX
      val iz = gz - gridMinZ
      ix >= 0 && ix < gridSize && iz >= 0 && iz < gridSize && cloudGrid(ix * gridSize + iz)
    def gridSoftness(gx: Int, gz: Int): Float =
      var count = 0
      var dz = -1
      while dz <= 1 do
        var dx = -1
        while dx <= 1 do
          if gridCloud(gx + dx, gz + dz) then count += 1
          dx += 1
        dz += 1
      count.toFloat / 9f
    val baseR = mix(0.66f, 0.98f, daylight)
    val baseG = mix(0.70f, 0.99f, daylight)
    val baseB = mix(0.78f, 1.00f, daylight)
    val warmR = baseR + warm * 0.05f
    val warmG = baseG - warm * 0.02f
    val warmB = baseB - warm * 0.06f
    glDisable(GL_BLEND)
    glDisable(GL_TEXTURE_2D)
    glDisable(GL_CULL_FACE)
    glDisable(GL_FOG)
    glEnable(GL_DEPTH_TEST)
    glDepthFunc(GL_LEQUAL)
    glDepthMask(true)
    val cameraAboveClouds = camera.y > cloudY + thickness * 0.5f
    glBegin(GL_QUADS)
    var gz = centerGZ - radiusCells
    while gz <= centerGZ + radiusCells do
      var gx = centerGX - radiusCells
      while gx <= centerGX + radiusCells do
        if gridCloud(gx, gz) then
          val wx0 = gx * cell - drift
          val wz0 = gz * cell + drift * 0.28f
          val wx1 = wx0 + cell
          val wz1 = wz0 + cell
          val midX = (wx0 + wx1) * 0.5f
          val midZ = (wz0 + wz1) * 0.5f
          val dx = midX - camera.x
          val dz = midZ - camera.z
          val dist = sqrt(dx * dx + dz * dz).toFloat
          val fade = (1f - ((dist - cloudRenderDistanceBlocks * 0.88f) / (cloudRenderDistanceBlocks * 0.12f).max(1f))).max(0f).min(1f)
          if fade > 0.01f then
            val density = gridSoftness(gx, gz)
            val edge = (1f - density).max(0f).min(1f)
            // Keep top/bottom opacity consistent across neighboring cells. Big per-cell
            // alpha differences make the grid visible and read as cheap individual faces.
            val haze = 1f - fade
            val x0 = wx0 - camera.x; val x1 = wx1 - camera.x
            val y0 = cloudY - camera.y; val y1 = cloudY + thickness - camera.y
            val z0 = wz0 - camera.z; val z1 = wz1 - camera.z
            val topLight = (1.00f + edge * 0.025f).min(1.06f)
            val bottomLight = 0.57f + daylight * 0.17f
            val topR = mix(warmR * topLight, warmR * 0.82f, haze)
            val topG = mix(warmG * topLight, warmG * 0.86f, haze)
            val topB = mix(warmB * topLight, warmB * 0.92f, haze)
            val bottomR = mix(warmR * bottomLight, warmR * 0.66f, haze)
            val bottomG = mix(warmG * bottomLight, warmG * 0.70f, haze)
            val bottomB = mix(warmB * bottomLight, warmB * 0.78f, haze)
            if cameraAboveClouds then
              cloudQuad(x0, y1, z0, x1, y1, z0, x1, y1, z1, x0, y1, z1, topR, topG, topB, 1f)
            else
              cloudQuad(x0, y0, z1, x1, y0, z1, x1, y0, z0, x0, y0, z0, bottomR, bottomG, bottomB, 1f)
            if dist < cloudRenderDistanceBlocks * 0.58f then
              val sideLight = 0.72f + daylight * 0.17f
              val sideR = mix(warmR * sideLight, warmR * 0.72f, haze)
              val sideG = mix(warmG * sideLight, warmG * 0.76f, haze)
              val sideB = mix(warmB * sideLight, warmB * 0.84f, haze)
              if !gridCloud(gx - 1, gz) then cloudSideQuad(x0, y0, z0, x0, y1, z0, x0, y1, z1, x0, y0, z1, sideR, sideG, sideB, 1.02f, 0.82f)
              if !gridCloud(gx + 1, gz) then cloudSideQuad(x1, y0, z1, x1, y1, z1, x1, y1, z0, x1, y0, z0, sideR, sideG, sideB, 0.98f, 0.80f)
              if !gridCloud(gx, gz - 1) then cloudSideQuad(x1, y0, z0, x1, y1, z0, x0, y1, z0, x0, y0, z0, sideR, sideG, sideB, 0.99f, 0.78f)
              if !gridCloud(gx, gz + 1) then cloudSideQuad(x0, y0, z1, x0, y1, z1, x1, y1, z1, x1, y0, z1, sideR, sideG, sideB, 1.01f, 0.80f)
        gx += 1
      gz += 1
    glEnd()
    glDepthMask(true)
    glClear(GL_DEPTH_BUFFER_BIT)
    glDepthMask(false)
    glDisable(GL_DEPTH_TEST)
    glEnable(GL_BLEND)
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA)

  private def cloudQuad(x0: Float, y0: Float, z0: Float, x1: Float, y1: Float, z1: Float, x2: Float, y2: Float, z2: Float, x3: Float, y3: Float, z3: Float, r: Float, g: Float, b: Float, a: Float): Unit =
    glColor4f(r.min(1.15f), g.min(1.15f), b.min(1.15f), a.max(0f).min(1f))
    glVertex3f(x0, y0, z0); glVertex3f(x1, y1, z1); glVertex3f(x2, y2, z2); glVertex3f(x3, y3, z3)

  private def cloudSideQuad(x0: Float, y0: Float, z0: Float, x1: Float, y1: Float, z1: Float, x2: Float, y2: Float, z2: Float, x3: Float, y3: Float, z3: Float, r: Float, g: Float, b: Float, topMul: Float, bottomMul: Float): Unit =
    glColor4f((r * bottomMul).min(1.15f), (g * bottomMul).min(1.15f), (b * bottomMul).min(1.15f), 1f)
    glVertex3f(x0, y0, z0)
    glColor4f((r * topMul).min(1.15f), (g * topMul).min(1.15f), (b * topMul).min(1.15f), 1f)
    glVertex3f(x1, y1, z1); glVertex3f(x2, y2, z2)
    glColor4f((r * bottomMul).min(1.15f), (g * bottomMul).min(1.15f), (b * bottomMul).min(1.15f), 1f)
    glVertex3f(x3, y3, z3)

  private def cloudAt(gx: Int, gz: Int): Boolean =
    val broad = smoothValueNoise(gx.toFloat * 0.052f, gz.toFloat * 0.052f)
    val medium = smoothValueNoise(gx.toFloat * 0.118f + 19.3f, gz.toFloat * 0.118f - 11.7f)
    val detail = smoothValueNoise(gx.toFloat * 0.235f - 6.8f, gz.toFloat * 0.235f + 4.1f)
    val gap = smoothValueNoise(gx.toFloat * 0.076f + 41.2f, gz.toFloat * 0.076f - 23.4f)
    val shape = broad * 0.46f + medium * 0.39f + detail * 0.15f
    shape > 0.615f && gap > 0.34f

  private def cloudSoftness(gx: Int, gz: Int): Float =
    var count = 0
    var dz = -1
    while dz <= 1 do
      var dx = -1
      while dx <= 1 do
        if cloudAt(gx + dx, gz + dz) then count += 1
        dx += 1
      dz += 1
    (count.toFloat / 9f).max(0f).min(1f)

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

  private def billboardBasis(dir: (Float, Float, Float)): ((Float, Float, Float), (Float, Float, Float)) =
    val up = if abs(dir._2) > 0.94f then (0f, 0f, 1f) else (0f, 1f, 0f)
    val ux0 = up._2 * dir._3 - up._3 * dir._2
    val uy0 = up._3 * dir._1 - up._1 * dir._3
    val uz0 = up._1 * dir._2 - up._2 * dir._1
    val u = normalized(ux0, uy0, uz0)
    val v = normalized(dir._2 * u._3 - dir._3 * u._2, dir._3 * u._1 - dir._1 * u._3, dir._1 * u._2 - dir._2 * u._1)
    (u, v)

  private def drawRadialDisc(dir: (Float, Float, Float), basis: ((Float, Float, Float), (Float, Float, Float)), radius: Float, size: Float, color: (Float, Float, Float), alpha: Float, segments: Int): Unit =
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
      glVertex3f(center._1 + u._1 * ca + v._1 * sa, center._2 + u._2 * ca + v._2 * sa, center._3 + u._3 * ca + v._3 * sa)
      i += 1
    glEnd()

  private def drawSolidDisc(dir: (Float, Float, Float), basis: ((Float, Float, Float), (Float, Float, Float)), radius: Float, size: Float, color: (Float, Float, Float), alpha: Float, segments: Int): Unit =
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
      glVertex3f(center._1 + u._1 * ca + v._1 * sa, center._2 + u._2 * ca + v._2 * sa, center._3 + u._3 * ca + v._3 * sa)
      i += 1
    glEnd()

  private def drawMoonMare(dir: (Float, Float, Float), basis: ((Float, Float, Float), (Float, Float, Float)), alpha: Float): Unit =
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
