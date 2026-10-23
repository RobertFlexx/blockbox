package blockbox

import org.lwjgl.BufferUtils
import org.lwjgl.opengl.GL11.*
import org.lwjgl.opengl.GL13.*
import org.lwjgl.opengl.GL15.*

import scala.math.*

object BlockboxRender2D:
  def voxelPanel(p: UiRect, s: Float): Unit =
    rect(p.x, p.y, p.w, p.h, 0.09f, 0.09f, 0.09f, 1)
    rect(p.x + s, p.y + s, p.w - 2 * s, p.h - 2 * s, 0.95f, 0.95f, 0.95f, 1)
    rect(p.x + 3 * s, p.y + 3 * s, p.w - 4 * s, p.h - 4 * s, 0.35f, 0.35f, 0.35f, 1)
    rect(p.x + 3 * s, p.y + 3 * s, p.w - 6 * s, p.h - 6 * s, 0.77f, 0.77f, 0.77f, 1)
    rect(p.x + s, p.y + p.h - 2 * s, p.w - 2 * s, s, 0.23f, 0.23f, 0.23f, 1)
    rect(p.x + p.w - 2 * s, p.y + s, s, p.h - 2 * s, 0.23f, 0.23f, 0.23f, 1)

  def voxelSlot(p: UiRect, s: Float, hover: Boolean = false, selected: Boolean = false): Unit =
    rect(p.x, p.y, p.w, p.h, 0.24f, 0.24f, 0.24f, 1)
    rect(p.x + s, p.y + s, p.w - s, p.h - s, 1f, 1f, 1f, 1)
    val v = if hover then 0.72f else 0.55f
    rect(p.x + s, p.y + s, p.w - 2 * s, p.h - 2 * s, v, v, v, 1)
    if selected then
      val e = s.max(1f)
      rect(p.x - e, p.y - e, p.w + 2 * e, e, 1, 1, 1, 1);
      rect(p.x - e, p.y + p.h, p.w + 2 * e, e, 1, 1, 1, 1)
      rect(p.x - e, p.y, e, p.h, 1, 1, 1, 1); rect(p.x + p.w, p.y, e, p.h, 1, 1, 1, 1)
  private val textMeshCache = scala.collection.mutable.LinkedHashMap
    .empty[String, (java.nio.ByteBuffer, Int, Float, Float, Float, Float)]
  private val maxTextMeshCacheEntries = 512

  private val glyphRows: Map[Char, Array[String]] = Map(
    'A' -> Array("01110", "10001", "10001", "11111", "10001", "10001", "10001"),
    'B' -> Array("11110", "10001", "10001", "11110", "10001", "10001", "11110"),
    'C' -> Array("01111", "10000", "10000", "10000", "10000", "10000", "01111"),
    'D' -> Array("11110", "10001", "10001", "10001", "10001", "10001", "11110"),
    'E' -> Array("11111", "10000", "10000", "11110", "10000", "10000", "11111"),
    'F' -> Array("11111", "10000", "10000", "11110", "10000", "10000", "10000"),
    'G' -> Array("01111", "10000", "10000", "10111", "10001", "10001", "01111"),
    'H' -> Array("10001", "10001", "10001", "11111", "10001", "10001", "10001"),
    'I' -> Array("11111", "00100", "00100", "00100", "00100", "00100", "11111"),
    'J' -> Array("00111", "00010", "00010", "00010", "10010", "10010", "01100"),
    'K' -> Array("10001", "10010", "10100", "11000", "10100", "10010", "10001"),
    'L' -> Array("10000", "10000", "10000", "10000", "10000", "10000", "11111"),
    'M' -> Array("10001", "11011", "10101", "10101", "10001", "10001", "10001"),
    'N' -> Array("10001", "11001", "10101", "10011", "10001", "10001", "10001"),
    'O' -> Array("01110", "10001", "10001", "10001", "10001", "10001", "01110"),
    'P' -> Array("11110", "10001", "10001", "11110", "10000", "10000", "10000"),
    'Q' -> Array("01110", "10001", "10001", "10001", "10101", "10010", "01101"),
    'R' -> Array("11110", "10001", "10001", "11110", "10100", "10010", "10001"),
    'S' -> Array("01111", "10000", "10000", "01110", "00001", "00001", "11110"),
    'T' -> Array("11111", "00100", "00100", "00100", "00100", "00100", "00100"),
    'U' -> Array("10001", "10001", "10001", "10001", "10001", "10001", "01110"),
    'V' -> Array("10001", "10001", "10001", "10001", "10001", "01010", "00100"),
    'W' -> Array("10001", "10001", "10001", "10101", "10101", "10101", "01010"),
    'X' -> Array("10001", "10001", "01010", "00100", "01010", "10001", "10001"),
    'Y' -> Array("10001", "10001", "01010", "00100", "00100", "00100", "00100"),
    'Z' -> Array("11111", "00001", "00010", "00100", "01000", "10000", "11111"),
    'a' -> Array("00000", "00000", "01110", "00001", "01111", "10001", "01111"),
    'b' -> Array("10000", "10000", "10110", "11001", "10001", "10001", "11110"),
    'c' -> Array("00000", "00000", "01111", "10000", "10000", "10000", "01111"),
    'd' -> Array("00001", "00001", "01101", "10011", "10001", "10001", "01111"),
    'e' -> Array("00000", "00000", "01110", "10001", "11111", "10000", "01111"),
    'f' -> Array("00110", "01001", "01000", "11100", "01000", "01000", "01000"),
    'g' -> Array("00000", "00000", "01111", "10001", "01111", "00001", "01110"),
    'h' -> Array("10000", "10000", "10110", "11001", "10001", "10001", "10001"),
    'i' -> Array("00100", "00000", "01100", "00100", "00100", "00100", "01110"),
    'j' -> Array("00010", "00000", "00110", "00010", "00010", "10010", "01100"),
    'k' -> Array("10000", "10000", "10010", "10100", "11000", "10100", "10010"),
    'l' -> Array("01100", "00100", "00100", "00100", "00100", "00100", "01110"),
    'm' -> Array("00000", "00000", "11010", "10101", "10101", "10101", "10101"),
    'n' -> Array("00000", "00000", "10110", "11001", "10001", "10001", "10001"),
    'o' -> Array("00000", "00000", "01110", "10001", "10001", "10001", "01110"),
    'p' -> Array("00000", "00000", "11110", "10001", "11110", "10000", "10000"),
    'q' -> Array("00000", "00000", "01111", "10001", "01111", "00001", "00001"),
    'r' -> Array("00000", "00000", "10110", "11001", "10000", "10000", "10000"),
    's' -> Array("00000", "00000", "01111", "10000", "01110", "00001", "11110"),
    't' -> Array("01000", "01000", "11100", "01000", "01000", "01001", "00110"),
    'u' -> Array("00000", "00000", "10001", "10001", "10001", "10011", "01101"),
    'v' -> Array("00000", "00000", "10001", "10001", "10001", "01010", "00100"),
    'w' -> Array("00000", "00000", "10001", "10001", "10101", "10101", "01010"),
    'x' -> Array("00000", "00000", "10001", "01010", "00100", "01010", "10001"),
    'y' -> Array("00000", "00000", "10001", "10001", "01111", "00001", "01110"),
    'z' -> Array("00000", "00000", "11111", "00010", "00100", "01000", "11111"),
    '0' -> Array("01110", "10001", "10011", "10101", "11001", "10001", "01110"),
    '1' -> Array("00100", "01100", "00100", "00100", "00100", "00100", "01110"),
    '2' -> Array("01110", "10001", "00001", "00010", "00100", "01000", "11111"),
    '3' -> Array("11110", "00001", "00001", "01110", "00001", "00001", "11110"),
    '4' -> Array("00010", "00110", "01010", "10010", "11111", "00010", "00010"),
    '5' -> Array("11111", "10000", "10000", "11110", "00001", "00001", "11110"),
    '6' -> Array("01110", "10000", "10000", "11110", "10001", "10001", "01110"),
    '7' -> Array("11111", "00001", "00010", "00100", "01000", "01000", "01000"),
    '8' -> Array("01110", "10001", "10001", "01110", "10001", "10001", "01110"),
    '9' -> Array("01110", "10001", "10001", "01111", "00001", "00001", "01110"),
    '.' -> Array("00000", "00000", "00000", "00000", "00000", "00110", "00110"),
    ',' -> Array("00000", "00000", "00000", "00000", "00110", "00110", "00100"),
    ':' -> Array("00000", "00110", "00110", "00000", "00110", "00110", "00000"),
    ';' -> Array("00000", "00110", "00110", "00000", "00110", "00110", "00100"),
    '!' -> Array("00100", "00100", "00100", "00100", "00100", "00000", "00100"),
    '?' -> Array("01110", "10001", "00001", "00010", "00100", "00000", "00100"),
    '-' -> Array("00000", "00000", "00000", "11111", "00000", "00000", "00000"),
    '_' -> Array("00000", "00000", "00000", "00000", "00000", "00000", "11111"),
    '/' -> Array("00001", "00010", "00010", "00100", "01000", "01000", "10000"),
    '\\' -> Array("10000", "01000", "01000", "00100", "00010", "00010", "00001"),
    '(' -> Array("00010", "00100", "01000", "01000", "01000", "00100", "00010"),
    ')' -> Array("01000", "00100", "00010", "00010", "00010", "00100", "01000"),
    '[' -> Array("01110", "01000", "01000", "01000", "01000", "01000", "01110"),
    ']' -> Array("01110", "00010", "00010", "00010", "00010", "00010", "01110"),
    '+' -> Array("00000", "00100", "00100", "11111", "00100", "00100", "00000"),
    '=' -> Array("00000", "00000", "11111", "00000", "11111", "00000", "00000"),
    '%' -> Array("11001", "11010", "00010", "00100", "01000", "01011", "10011"),
    '#' -> Array("01010", "01010", "11111", "01010", "11111", "01010", "01010"),
    '*' -> Array("00000", "10101", "01110", "11111", "01110", "10101", "00000"),
    '<' -> Array("00010", "00100", "01000", "10000", "01000", "00100", "00010"),
    '>' -> Array("01000", "00100", "00010", "00001", "00010", "00100", "01000"),
    '\'' -> Array("00100", "00100", "00000", "00000", "00000", "00000", "00000"),
    '"' -> Array("01010", "01010", "00000", "00000", "00000", "00000", "00000"),
    '&' -> Array("01100", "10010", "10100", "01000", "10101", "10010", "01101"),
    '[' -> Array("01110", "01000", "01000", "01000", "01000", "01000", "01110"),
    ']' -> Array("01110", "00010", "00010", "00010", "00010", "00010", "01110"),
    '+' -> Array("00000", "00100", "00100", "11111", "00100", "00100", "00000"),
    '|' -> Array("00100", "00100", "00100", "00100", "00100", "00100", "00100"),
    '@' -> Array("01110", "10001", "10111", "10101", "10111", "10000", "01110")
  )

  private def textFitPad(scale: Float): Float = (2f * scale).max(2f)

  private def glyphFor(ch: Char): Array[String] =
    if ch == ' ' then Array.fill(7)("00000")
    else glyphRows.getOrElse(ch, glyphRows.getOrElse(ch.toUpper, glyphRows('?')))

  /** Builds quads in the same simple XY/stride-16 layout the old STB path used. The font itself is
    * a crisp 5x7 pixel face, so UI text no longer looks like a thin debug overlay and scales
    * cleanly at integer-ish GUI scales.
    */
  def textMetrics(text: String): (java.nio.ByteBuffer, Int, Float, Float, Float, Float) =
    val safe = Option(text).getOrElse("")
    textMeshCache.get(safe) match
      case Some(mesh) =>
        // Tiny LRU: move recently used strings to the end. Buffers are immutable
        // after construction; renderTextBuf only rewinds their position.
        textMeshCache.remove(safe)
        textMeshCache.put(safe, mesh)
        mesh
      case None =>
        var pixels = 0
        safe.foreach { ch => glyphFor(ch).foreach(row => pixels += row.count(_ == '1')) }
        val buf = BufferUtils.createByteBuffer((pixels.max(1)) * 4 * 16)
        var cursorX = 0f
        var quads = 0
        safe.foreach { ch =>
          val rows = glyphFor(ch)
          var yy = 0
          while yy < 7 do
            var xx = 0
            while xx < 5 do
              if rows(yy).charAt(xx) == '1' then
                val x0 = cursorX + xx.toFloat
                val y0 = yy.toFloat
                val x1 = x0 + 1f
                val y1 = y0 + 1f
                def v(x: Float, y: Float): Unit =
                  buf.putFloat(x); buf.putFloat(y); buf.putFloat(0f); buf.putFloat(0f)
                v(x0, y0); v(x1, y0); v(x1, y1); v(x0, y1)
                quads += 1
              xx += 1
            yy += 1
          cursorX += 6f
        }
        buf.flip()
        val maxX = if safe.isEmpty then 0f else (cursorX - 1f).max(1f)
        val mesh = (buf, quads, 0f, maxX, 0f, 7f)
        textMeshCache.put(safe, mesh)
        if textMeshCache.size > maxTextMeshCacheEntries then
          textMeshCache.remove(textMeshCache.head._1)
        mesh

  def textWidth(text: String, scale: Float): Float =
    if text == null || text.isEmpty then 0f
    else ((text.length * 6f - 1f).max(1f) * scale) + textFitPad(scale)

  def uiScale(framebufferWidth: Int, framebufferHeight: Int): Float =
    // Minecraft-like sizing: keep the UI compact at 720p/1080p and only grow
    // when the framebuffer genuinely has room. Oversized UI was making the game
    // read like a generic launcher instead of an in-game interface.
    val byWidth = framebufferWidth.toFloat / 1280f
    val byHeight = framebufferHeight.toFloat / 720f
    math.min(byWidth, byHeight).max(0.90f).min(1.28f)

  def resetGlArraysAndBuffers(): Unit =
    glBindBuffer(GL_ARRAY_BUFFER, 0)
    glDisableClientState(GL_VERTEX_ARRAY)
    glDisableClientState(GL_COLOR_ARRAY)
    glDisableClientState(GL_NORMAL_ARRAY)
    // Texture-coordinate client state is per client-active texture unit. Chunk VBOs
    // expose both atlas UV0 and Minecraft-style lightmap UV1, so explicitly reset
    // both before immediate-mode UI/viewmodel rendering.
    glClientActiveTexture(GL_TEXTURE1)
    glDisableClientState(GL_TEXTURE_COORD_ARRAY)
    glClientActiveTexture(GL_TEXTURE0)
    glDisableClientState(GL_TEXTURE_COORD_ARRAY)

  def setupOrtho(framebufferWidth: Int, framebufferHeight: Int): Unit =
    resetGlArraysAndBuffers()
    glPolygonMode(GL_FRONT_AND_BACK, GL_FILL)
    glDepthMask(true)
    glDisable(GL_FOG)
    glDisable(GL_CULL_FACE)
    glDisable(GL_DEPTH_TEST)
    glDisable(GL_TEXTURE_2D)
    glEnable(GL_BLEND)
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA)
    glMatrixMode(GL_PROJECTION); glLoadIdentity()
    glOrtho(0, framebufferWidth, framebufferHeight, 0, -1, 1)
    glMatrixMode(GL_MODELVIEW); glLoadIdentity()

  def rect(x: Float, y: Float, w: Float, h: Float, r: Float, g: Float, b: Float, a: Float): Unit =
    if w <= 0f || h <= 0f || a <= 0f then return
    glBindBuffer(GL_ARRAY_BUFFER, 0)
    glDisable(GL_TEXTURE_2D)
    glEnable(GL_BLEND)
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA)
    glColor4f(r, g, b, a)
    glBegin(GL_QUADS); glVertex2f(x, y); glVertex2f(x + w, y); glVertex2f(x + w, y + h);
    glVertex2f(x, y + h); glEnd()

  def gradientRect(
      x: Float,
      y: Float,
      w: Float,
      h: Float,
      top: (Float, Float, Float),
      bottom: (Float, Float, Float),
      a: Float
  ): Unit =
    if w <= 0f || h <= 0f || a <= 0f then return
    glBindBuffer(GL_ARRAY_BUFFER, 0)
    glDisable(GL_TEXTURE_2D)
    glEnable(GL_BLEND)
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA)
    glBegin(GL_QUADS)
    glColor4f(top._1, top._2, top._3, a); glVertex2f(x, y); glVertex2f(x + w, y)
    glColor4f(bottom._1, bottom._2, bottom._3, a); glVertex2f(x + w, y + h); glVertex2f(x, y + h)
    glEnd()

  def dimBackground(framebufferWidth: Int, framebufferHeight: Int): Unit =
    rect(0, 0, framebufferWidth, framebufferHeight, 0f, 0f, 0f, 0.55f)

  /** Quiet voxel title backdrop. Deliberately simple: square celestial body, block clouds and
    * stepped terrain. No neon rails, glass cards, or fake depth.
    */
  def drawMenuBackdrop(framebufferWidth: Int, framebufferHeight: Int, time: Float): Unit =
    val w = framebufferWidth.toFloat
    val h = framebufferHeight.toFloat
    glDisable(GL_TEXTURE_2D)
    glDisable(GL_DEPTH_TEST)

    // Golden-hour sky: deep blue overhead warming to a hazy gold horizon.
    gradientRect(0f, 0f, w, h * 0.42f, (0.15f, 0.29f, 0.48f), (0.36f, 0.53f, 0.68f), 1f)
    gradientRect(0f, h * 0.42f, w, h * 0.32f, (0.36f, 0.53f, 0.68f), (0.88f, 0.77f, 0.57f), 1f)
    rect(0f, h * 0.74f, w, h * 0.26f, 0.88f, 0.77f, 0.57f, 1f)

    def ridge(baseY: Float, height: Float, r: Float, g: Float, b: Float, phase: Float): Unit =
      glBegin(GL_QUAD_STRIP)
      var i = 0
      while i <= 48 do
        val x = i * w / 48f
        val shape =
          abs(sin(i * 0.21f + phase)).toFloat * 0.65f + sin(i * 0.53f + phase).toFloat * 0.16f
        glColor4f(r, g, b, 1f); glVertex2f(x, baseY - shape * height); glVertex2f(x, h)
        i += 1
      glEnd()
    ridge(h * 0.66f, h * 0.19f, 0.43f, 0.52f, 0.55f, 0.8f)
    ridge(h * 0.76f, h * 0.15f, 0.22f, 0.36f, 0.34f, 2.7f)
    val unit = (h / 720f).max(0.5f).min(1.5f)
    val sunSize = 42f * unit
    val sunX = w * 0.76f
    val sunY = h * 0.16f
    var ring = 0
    while ring < 12 do
      val s = sunSize * (3.6f - ring * 0.22f)
      rect(sunX + (sunSize - s) / 2f, sunY + (sunSize - s) / 2f, s, s, 1f, 0.92f, 0.70f, 0.022f)
      ring += 1
    rect(sunX, sunY, sunSize, sunSize, 1f, 0.93f, 0.62f, 1f)
    rect(
      sunX + sunSize * 0.14f,
      sunY + sunSize * 0.14f,
      sunSize * 0.72f,
      sunSize * 0.72f,
      1f,
      0.98f,
      0.84f,
      1f
    )

    // Slow block clouds. Their motion is subtle enough not to distract from text.
    val drift = ((time * 7f) % (w + 240f * unit)) - 120f * unit
    def cloud(x: Float, y: Float, scale: Float): Unit =
      val u = 18f * unit * scale
      rect(x, y + u, u * 6f, u * 1.5f, 0.98f, 0.97f, 0.95f, 1f)
      rect(x + u, y, u * 3f, u * 2.5f, 0.98f, 0.97f, 0.95f, 1f)
      rect(x + u * 4f, y + u * 0.5f, u * 2f, u * 2f, 0.98f, 0.97f, 0.95f, 1f)
      rect(x, y + u * 2.05f, u * 6f, u * 0.45f, 0.80f, 0.80f, 0.86f, 1f)
    cloud(drift, h * 0.23f, 0.72f)
    cloud((drift + w * 0.48f) % (w + 180f * unit) - 80f * unit, h * 0.34f, 0.55f)
    cloud((drift + w * 0.82f) % (w + 220f * unit) - 100f * unit, h * 0.18f, 0.90f)

    // Stepped silhouette. Fixed deterministic heights keep it from looking like
    // procedural UI decoration while still clearly belonging to a voxel game.
    val step = (42f * unit).max(28f)
    val heights = Array(0.18f, 0.24f, 0.22f, 0.31f, 0.36f, 0.30f, 0.27f, 0.34f, 0.43f, 0.39f, 0.31f,
      0.28f, 0.35f, 0.30f, 0.25f, 0.33f, 0.29f, 0.22f)
    var i = 0
    var x = 0f
    while x < w + step do
      val q = heights(i % heights.length)
      val top = h * (0.80f - q * 0.28f)
      val tone = 0.92f + ((i * 37) % 11) / 11f * 0.16f
      gradientRect(
        x,
        top,
        step + 1f,
        h - top,
        (0.25f * tone, 0.37f * tone, 0.17f * tone),
        (0.12f, 0.19f, 0.09f),
        1f
      )
      val grass = (8f * unit).max(5f)
      rect(x, top, step + 1f, grass, 0.36f * tone, 0.52f * tone, 0.22f * tone, 1f)
      rect(x, top, step + 1f, (2f * unit).max(1f), 0.50f, 0.66f, 0.30f, 1f)
      rect(x, top + grass, step + 1f, (2f * unit).max(1f), 0.10f, 0.16f, 0.07f, 0.45f)
      if i % 5 == 1 then
        rect(
          x + step * 0.30f,
          top - step * 0.95f,
          step * 0.32f,
          step * 0.95f,
          0.29f,
          0.20f,
          0.12f,
          1f
        )
        rect(
          x + step * 0.30f,
          top - step * 0.95f,
          step * 0.08f,
          step * 0.95f,
          0.36f,
          0.26f,
          0.16f,
          1f
        )
        rect(
          x + step * 0.05f,
          top - step * 1.35f,
          step * 0.82f,
          step * 0.55f,
          0.16f,
          0.34f,
          0.13f,
          1f
        )
        rect(
          x + step * 0.05f,
          top - step * 1.35f,
          step * 0.82f,
          step * 0.12f,
          0.24f,
          0.44f,
          0.18f,
          1f
        )
        rect(
          x + step * 0.05f,
          top - step * 0.90f,
          step * 0.82f,
          step * 0.10f,
          0.10f,
          0.24f,
          0.09f,
          1f
        )
      x += step
      i += 1
    rect(0f, h * 0.91f, w, h * 0.09f, 0.16f, 0.12f, 0.08f, 1f)
    rect(0f, h * 0.91f, w, (3f * unit).max(2f), 0.09f, 0.07f, 0.05f, 1f)

  def drawPanel(x: Float, y: Float, w: Float, h: Float, scale: Float): Unit =
    val e = (2f * scale).max(1f)
    rect(x + 5f * scale, y + 7f * scale, w, h, 0.01f, 0.02f, 0.025f, 0.40f)
    rect(x - e, y - e, w + e * 2f, h + e * 2f, 0.025f, 0.045f, 0.05f, 0.98f)
    gradientRect(x, y, w, h, (0.095f, 0.13f, 0.14f), (0.060f, 0.085f, 0.095f), 0.97f)
    rect(x + e, y + e, w - e * 2f, e, 0.28f, 0.37f, 0.36f, 0.75f)
    rect(x + e, y + e, e, h - e * 2f, 0.17f, 0.23f, 0.24f, 0.65f)
    rect(x + e, y + h - e, w - e * 2f, e, 0.035f, 0.05f, 0.055f, 0.95f)

  def drawSlotFrame(
      x: Float,
      y: Float,
      size: Float,
      selected: Boolean,
      hover: Boolean,
      scale: Float
  ): Unit =
    val e = (2f * scale).max(1f)
    val inner = if hover then 0.24f else 0.17f
    // Recessed inventory-slot bevel: dark top/left, light bottom/right.
    rect(x - e, y - e, size + e * 2f, size + e * 2f, 0.10f, 0.10f, 0.10f, 0.98f)
    rect(x, y, size, size, inner * 0.78f, inner, inner * 1.05f, 0.98f)
    rect(x, y, size, e, 0.18f, 0.18f, 0.18f, 0.98f)
    rect(x, y, e, size, 0.17f, 0.17f, 0.17f, 0.98f)
    rect(x, y + size - e, size, e, 0.60f, 0.60f, 0.60f, 0.92f)
    rect(x + size - e, y, e, size, 0.56f, 0.56f, 0.56f, 0.92f)
    if selected then
      val s = (2f * scale).max(1f)
      rect(x - s * 2f, y - s * 2f, size + s * 4f, s, 0.73f, 0.88f, 0.64f, 1f)
      rect(x - s * 2f, y + size + s, size + s * 4f, s, 0.73f, 0.88f, 0.64f, 1f)
      rect(x - s * 2f, y - s * 2f, s, size + s * 4f, 0.73f, 0.88f, 0.64f, 1f)
      rect(x + size + s, y - s * 2f, s, size + s * 4f, 0.73f, 0.88f, 0.64f, 1f)

  def slider(x: Float, y: Float, w: Float, value: Float, scale: Float): Unit =
    val v = value.max(0f).min(1f)
    val trackH = (12f * scale).max(8f)
    val knobW = (10f * scale).max(7f)
    val knobH = (20f * scale).max(14f)
    rect(
      x - 1f * scale,
      y - 1f * scale,
      w + 2f * scale,
      trackH + 2f * scale,
      0.08f,
      0.08f,
      0.08f,
      1f
    )
    rect(x, y, w, trackH, 0.23f, 0.23f, 0.23f, 1f)
    val fillW = (w - knobW).max(1f) * v
    rect(
      x + 1f * scale,
      y + 1f * scale,
      (fillW + knobW * 0.5f).min(w - 2f * scale),
      trackH - 2f * scale,
      0.48f,
      0.70f,
      0.33f,
      0.90f
    )
    val kx = x + fillW
    rect(
      kx - 1f * scale,
      y - 5f * scale,
      knobW + 2f * scale,
      knobH + 2f * scale,
      0.08f,
      0.08f,
      0.08f,
      1f
    )
    rect(kx, y - 4f * scale, knobW, knobH, 0.72f, 0.72f, 0.72f, 1f)
    rect(kx + 1f * scale, y - 3f * scale, knobW - 2f * scale, 2f * scale, 0.92f, 0.92f, 0.92f, 0.9f)

  def drawButton(
      x: Float,
      y: Float,
      w: Float,
      h: Float,
      label: String,
      accent: Boolean,
      hover: Boolean,
      scale: Float,
      framebufferWidth: Int,
      framebufferHeight: Int
  ): Unit =
    val e = (2f * scale).max(1f)
    rect(x, y + 3f * scale, w, h, 0.015f, 0.025f, 0.03f, 0.65f)
    val border = if hover then (0.65f, 0.79f, 0.65f)
    else if accent then (0.37f, 0.59f, 0.40f)
    else (0.23f, 0.32f, 0.32f)
    rect(x - e, y - e, w + e * 2f, h + e * 2f, border._1, border._2, border._3, 1f)
    val base = if accent then (0.24f, 0.39f, 0.26f) else (0.13f, 0.19f, 0.20f)
    val lift = if hover then 0.065f else 0f
    gradientRect(
      x,
      y,
      w,
      h,
      (base._1 + lift + 0.05f, base._2 + lift + 0.06f, base._3 + lift + 0.05f),
      (base._1 + lift - 0.035f, base._2 + lift - 0.04f, base._3 + lift - 0.035f),
      1f
    )
    rect(x + e, y + e, w - e * 2f, e, 0.80f, 0.92f, 0.78f, if hover then 0.32f else 0.17f)
    rect(x + e, y + e * 2f, e, h - e * 3.5f, 0.80f, 0.92f, 0.78f, if hover then 0.12f else 0.06f)
    rect(x + e, y + h - e * 1.5f, w - e * 2f, e * 1.5f, 0.0f, 0.02f, 0.02f, 0.50f)
    centeredTextBox(
      x + 8f * scale,
      y + 2f * scale,
      w - 16f * scale,
      h - 4f * scale,
      label,
      0.95f,
      0.96f,
      0.90f,
      (2.05f * scale).min(h / 8.5f),
      scale,
      framebufferWidth,
      framebufferHeight
    )

  def drawBubbleIcon(x: Float, y: Float, size: Float, popping: Boolean): Unit =
    val p = size / 9f
    val pattern = if popping then
      Array(
        "000000000",
        "000001000",
        "010000010",
        "000110000",
        "001001010",
        "000110000",
        "010000000",
        "000000100",
        "000000000"
      )
    else
      Array(
        "000111000",
        "001000100",
        "010000010",
        "100000001",
        "100000001",
        "100000001",
        "010000010",
        "001000100",
        "000111000"
      )
    for row <- pattern.indices; col <- 0 until 9 if pattern(row)(col) == '1' do
      rect(x + (col + 0.5f) * p, y + (row + 0.5f) * p, p, p, 0.01f, 0.07f, 0.13f, 0.8f)
      rect(x + col * p, y + row * p, p, p, 0.42f, 0.83f, 1f, 1f)
    if !popping then
      rect(x + 2 * p, y + 2 * p, 2 * p, p, 0.92f, 0.99f, 1f, 1f)
      rect(x + 2 * p, y + 3 * p, p, p, 0.78f, 0.95f, 1f, 1f)
      rect(x + 5 * p, y + 6 * p, 2 * p, p, 0.12f, 0.49f, 0.78f, 1f)

  def drawHeartIcon(x: Float, y: Float, size: Float, fillFrac: Float, bgAlpha: Float): Unit =
    val s = size.max(10f)
    val fill = fillFrac.max(0f).min(1f)
    rect(x - 1f, y - 1f, s + 2f, s + 2f, 0f, 0f, 0f, 0.30f)
    rect(x, y, s, s, 0.10f, 0.02f, 0.02f, bgAlpha)
    val cols = 7
    val rows = 6
    val cell = (s / cols.toFloat).max(1f)
    val heart = Array(
      Array(0, 1, 1, 0, 1, 1, 0),
      Array(1, 1, 1, 1, 1, 1, 1),
      Array(1, 1, 1, 1, 1, 1, 1),
      Array(0, 1, 1, 1, 1, 1, 0),
      Array(0, 0, 1, 1, 1, 0, 0),
      Array(0, 0, 0, 1, 0, 0, 0)
    )
    val fillW = s * fill
    var ry = 0
    while ry < rows do
      var rx = 0
      while rx < cols do
        if heart(ry)(rx) == 1 then
          val px = x + rx * cell
          val py = y + ry * cell
          val pw = (cell + 0.55f).min(s - rx * cell)
          val ph = (cell + 0.55f).min(s - ry * cell)
          val filled = px + pw <= x + fillW + 0.01f
          val partial = !filled && fill > 0f && px < x + fillW
          if filled then
            rect(px, py, pw, ph, 0.92f, 0.08f, 0.10f, 0.96f)
            if ry <= 1 then rect(px, py, pw, ph * 0.38f, 1f, 0.45f, 0.48f, 0.22f)
          else if partial then
            val visible = (x + fillW - px).max(0f).min(pw)
            rect(px, py, pw, ph, 0.18f, 0.03f, 0.03f, bgAlpha + 0.08f)
            rect(px, py, visible, ph, 0.92f, 0.08f, 0.10f, 0.96f)
          else rect(px, py, pw, ph, 0.18f, 0.03f, 0.03f, bgAlpha + 0.08f)
        rx += 1
      ry += 1

  def renderCrosshair(framebufferWidth: Int, framebufferHeight: Int): Unit =
    glDisable(GL_DEPTH_TEST); glDisable(GL_CULL_FACE);
    setupOrtho(framebufferWidth, framebufferHeight)
    val cx = framebufferWidth / 2f; val cy = framebufferHeight / 2f
    val s = ((framebufferHeight / 720f).max(0.75f)).min(2f)
    val gap = (4f * s).toInt.toFloat; val len = (8f * s).toInt.toFloat
    glLineWidth((1.5f * s).max(1f))
    glColor4f(0f, 0f, 0f, 0.55f)
    glBegin(GL_LINES)
    glVertex2f(cx - len - gap, cy); glVertex2f(cx - gap, cy)
    glVertex2f(cx + gap, cy); glVertex2f(cx + len + gap, cy)
    glVertex2f(cx, cy - len - gap); glVertex2f(cx, cy - gap)
    glVertex2f(cx, cy + gap); glVertex2f(cx, cy + len + gap)
    glEnd()
    glColor4f(1f, 1f, 1f, 0.80f)
    glBegin(GL_LINES)
    glVertex2f(cx - len - gap + 1f, cy); glVertex2f(cx - gap + 1f, cy)
    glVertex2f(cx + gap - 1f, cy); glVertex2f(cx + len + gap - 1f, cy)
    glVertex2f(cx, cy - len - gap + 1f); glVertex2f(cx, cy - gap + 1f)
    glVertex2f(cx, cy + gap - 1f); glVertex2f(cx, cy + len + gap - 1f)
    glEnd()
    glColor4f(1f, 1f, 1f, 0.30f)
    val dot = (1f * s).max(0.5f)
    glBegin(GL_QUADS)
    glVertex2f(cx - dot, cy - dot); glVertex2f(cx + dot, cy - dot)
    glVertex2f(cx + dot, cy + dot); glVertex2f(cx - dot, cy + dot)
    glEnd()
    glLineWidth(1f)

  def renderUnderwaterOverlay(framebufferWidth: Int, framebufferHeight: Int, time: Float): Unit =
    if UnderwaterEffect.render(framebufferWidth, framebufferHeight, time, 1f) then return
    glDisable(GL_DEPTH_TEST); glDisable(GL_CULL_FACE);
    setupOrtho(framebufferWidth, framebufferHeight)
    val w = framebufferWidth.toFloat
    val h = framebufferHeight.toFloat
    val wave = (sin(time * 1.35f).toFloat * 0.018f + 0.21f).max(0.12f).min(0.34f)
    rect(0, 0, w, h, 0.00f, 0.10f + wave * 0.28f, 0.24f + wave * 0.34f, 0.26f)
    rect(0, 0, w, h * 0.17f, 0.06f, 0.24f, 0.42f, 0.10f)
    rect(0, h * 0.83f, w, h * 0.17f, 0.03f, 0.18f, 0.34f, 0.14f)
    val vignette = 0.05f
    rect(0, 0, w * 0.09f, h, 0f, 0.06f, 0.16f, vignette)
    rect(w * 0.91f, 0, w * 0.09f, h, 0f, 0.06f, 0.16f, vignette)
    rect(0, 0, w, h * 0.07f, 0f, 0.10f, 0.22f, vignette * 0.62f)
    rect(0, h * 0.93f, w, h * 0.07f, 0f, 0.10f, 0.22f, vignette * 0.70f)

  def mainMenuLayout(
      framebufferWidth: Int,
      framebufferHeight: Int,
      scale: Float
  ): (Float, Float, Float, Array[Float], Float, Float, Float, Float, Float) =
    val w = framebufferWidth.toFloat
    val h = framebufferHeight.toFloat
    val cx = w / 2f
    val margin = (18f * scale).max(12f)
    val footerReserve = (30f * scale).max(20f)
    val desiredTitleH = 136f * scale
    val minTitleH = 82f
    val maxTitleH = (h * 0.25f).min(140f * scale).max(minTitleH)
    val titleH = desiredTitleH.min(maxTitleH).max(minTitleH.min(h * 0.20f))
    val titleW = (620f * scale).min(w * 0.68f).max(math.min(340f, w - margin * 2f))
    val titleX = cx - titleW / 2f
    val titleY = margin
    val labelH = (18f * scale).max(12f)
    val gap = (9f * scale).max(5f).min(12f)
    val firstY = titleY + titleH + (20f * scale).max(12f)
    val available = (h - firstY - footerReserve - labelH - gap * 9f).max(8f * 24f)
    val buttonH = (38f * scale).min(available / 8f).max(24f)
    val buttonW = (360f * scale).min(w * 0.48f).max(math.min(260f, w - margin * 2f))
    val bx = cx - buttonW / 2f
    val y0 = firstY
    val y1 = y0 + buttonH + gap + labelH + gap
    val ys = Array(
      y0,
      y1,
      y1 + (buttonH + gap),
      y1 + (buttonH + gap) * 2f,
      y1 + (buttonH + gap) * 3f,
      y1 + (buttonH + gap) * 4f,
      y1 + (buttonH + gap) * 5f,
      y1 + (buttonH + gap) * 6f
    )
    (bx, buttonW, buttonH, ys, titleX, titleY, titleW, titleH, scale)

  private def titleGlyph(c: Char): Array[String] = c match
    case 'B' => Array("111110", "110011", "110011", "111110", "110011", "110011", "111110")
    case 'L' => Array("110000", "110000", "110000", "110000", "110000", "110000", "111111")
    case 'O' => Array("011110", "110011", "110011", "110011", "110011", "110011", "011110")
    case 'C' => Array("011111", "110000", "110000", "110000", "110000", "110000", "011111")
    case 'K' => Array("110011", "110110", "111100", "111000", "111100", "110110", "110011")
    case 'X' => Array("110011", "110011", "011110", "001100", "011110", "110011", "110011")
    case _   => Array.fill(7)("000000")

  private def logoNoise(x: Int, y: Int): Float =
    var n = x * 374761393 + y * 668265263 + 0x5bd1e995
    n = (n ^ (n >>> 13)) * 1274126177
    ((n ^ (n >>> 16)) & 0xffff).toFloat / 65535f

  /** Bold block letters built like a voxel structure: an extruded dark side, a textured stone face
    * with bevelled outer edges, and grass growing on every upward-facing ledge.
    */
  def drawPixelLogo(cx: Float, y: Float, text: String, maxW: Float, maxH: Float): Unit =
    if text == null || text.isEmpty then return
    val chars = text.toUpperCase.filter(ch => ch == ' ' || "BLOCKX".indexOf(ch) >= 0)
    if chars.isEmpty then return
    val cells = scala.collection.mutable.ArrayBuffer.empty[(Int, Int)]
    var column = 0
    for ch <- chars do
      if ch == ' ' then column += 4
      else
        val glyph = titleGlyph(ch)
        for row <- 0 until 7; c <- 0 until 6 if glyph(row).charAt(c) == '1' do
          cells += ((column + c, row))
        column += 7
    val cols = (column - 1).max(1)
    val filled = cells.toSet
    def at(c: Int, r: Int) = filled.contains((c, r))
    val depth = 0.62f
    val cell = (maxW / (cols + depth * 0.55f)).min(maxH / (7f + depth)).max(1.5f)
    val x0 = cx - (cols + depth * 0.55f) * cell / 2f
    for (c, r) <- cells do
      rect(
        x0 + (c + depth * 0.55f + 0.30f) * cell,
        y + (r + depth + 0.42f) * cell,
        cell,
        cell,
        0f,
        0f,
        0f,
        0.30f
      )
    val steps = (depth * cell / 1.2f).toInt.max(3).min(16)
    var k = steps
    while k >= 1 do
      val t = k.toFloat / steps
      val shade = 0.31f - t * 0.17f
      for (c, r) <- cells do
        rect(
          x0 + c * cell + t * depth * 0.55f * cell,
          y + r * cell + t * depth * cell,
          cell + 0.5f,
          cell + 0.5f,
          shade,
          shade * 0.94f,
          shade * 0.89f,
          1f
        )
      k -= 1
    val half = cell / 2f
    val edge = (cell * 0.13f).max(1f)
    for (c, r) <- cells do
      val px = x0 + c * cell; val py = y + r * cell
      for sy <- 0 until 2; sx <- 0 until 2 do
        val v =
          0.91f - (r * 2 + sy) / 13f * 0.25f + logoNoise(c * 2 + sx, r * 2 + sy) * 0.08f - 0.04f
        rect(px + sx * half, py + sy * half, half + 0.5f, half + 0.5f, v, v * 0.97f, v * 0.92f, 1f)
      if !at(c - 1, r) then rect(px, py, edge, cell, 1f, 1f, 1f, 0.20f)
      if !at(c + 1, r) then rect(px + cell - edge, py, edge, cell, 0f, 0f, 0f, 0.20f)
      if !at(c, r + 1) then rect(px, py + cell - edge, cell, edge, 0f, 0f, 0f, 0.28f)
      if !at(c, r - 1) then
        for sx <- 0 until 2 do
          val drip = if logoNoise(c * 2 + sx, r + 91) > 0.58f then 0.20f else 0f
          rect(px + sx * half, py, half + 0.5f, cell * (0.27f + drip), 0.36f, 0.63f, 0.25f, 1f)
        rect(px, py, cell + 0.5f, edge, 0.58f, 0.84f, 0.40f, 1f)

  def renderTextBuf(
      buf: java.nio.ByteBuffer,
      quads: Int,
      x: Float,
      y: Float,
      r: Float,
      g: Float,
      b: Float,
      scale: Float,
      framebufferWidth: Int,
      framebufferHeight: Int
  ): Unit =
    buf.rewind()
    glDisable(GL_DEPTH_TEST); glDisable(GL_CULL_FACE); glDisable(GL_TEXTURE_2D);
    setupOrtho(framebufferWidth, framebufferHeight)
    glBindBuffer(GL_ARRAY_BUFFER, 0)
    glDisableClientState(GL_COLOR_ARRAY); glDisableClientState(GL_TEXTURE_COORD_ARRAY)
    glEnable(GL_BLEND); glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA)
    val snapX = Math.round(x * 2f) * 0.5f
    val snapY = Math.round(y * 2f) * 0.5f
    glPushMatrix(); glTranslatef(snapX, snapY, 0); glScalef(scale, scale, 1); glColor4f(r, g, b, 1f)
    glEnableClientState(GL_VERTEX_ARRAY); glVertexPointer(2, GL_FLOAT, 16, buf)
    glDrawArrays(GL_QUADS, 0, quads * 4); glDisableClientState(GL_VERTEX_ARRAY); glPopMatrix()

  def renderText(
      x: Float,
      y: Float,
      text: String,
      r: Float,
      g: Float,
      b: Float,
      scale: Float,
      framebufferWidth: Int,
      framebufferHeight: Int
  ): Unit =
    if text == null || text.isEmpty then return
    val lineH = 12f * scale
    if x >= framebufferWidth - 4f || x + 2f < 0 || y >= framebufferHeight - 2f || y + lineH < 0 then
      return
    // The built-in face advances six logical pixels per glyph. The old 9px
    // estimate truncated labels far earlier than necessary and made menus feel
    // cramped even when there was ample room.
    val maxChars =
      ((framebufferWidth - x - textFitPad(scale) - 12f).max(4f) / (6f * scale).max(1f)).toInt
    val display = if maxChars <= 0 then ""
    else if maxChars >= text.length then text
    else text.take(maxChars.max(4) - 3) + "..."
    if display.isEmpty then return
    val (buf, quads, _, _, _, _) = textMetrics(display)
    if quads <= 0 then return
    renderTextBuf(buf, quads, x, y, r, g, b, scale, framebufferWidth, framebufferHeight)

  def fitText(text: String, scale: Float, maxWidth: Float): String =
    if text == null || text.isEmpty then ""
    else if textWidth(text, scale) <= maxWidth.max(4f) then text
    else
      val suffix = "..."
      val suffixW = textWidth(suffix, scale)
      val target = (maxWidth - suffixW - textFitPad(scale)).max(0f)
      if target <= 4f then suffix
      else
        var lo = 0
        var hi = text.length
        while lo < hi do
          val mid = (lo + hi + 1) / 2
          if textWidth(text.take(mid), scale) <= target then lo = mid else hi = mid - 1
        text.take(lo.max(0)) + suffix

  def centeredTextFit(
      cx: Float,
      y: Float,
      text: String,
      r: Float,
      g: Float,
      b: Float,
      scale: Float,
      maxWidth: Float,
      framebufferWidth: Int,
      framebufferHeight: Int
  ): Unit =
    if text == null || text.isEmpty then return
    val (buf, quads, minX, maxX, minY, _) = textMetrics(text)
    if quads <= 0 then return
    val rawW = (maxX - minX).max(1f) + textFitPad(scale)
    val safeW = (maxWidth - textFitPad(scale)).max(12f).min(framebufferWidth.toFloat - 12f)
    val eff = scale.min(safeW / rawW).max(0.30f)
    val left = cx - rawW * eff / 2f - minX * eff
    val top = y - minY * eff
    val so = (1.25f * eff).max(0.75f)
    renderTextBuf(
      buf,
      quads,
      left + so,
      top + so,
      0f,
      0f,
      0f,
      eff,
      framebufferWidth,
      framebufferHeight
    )
    renderTextBuf(buf, quads, left, top, r, g, b, eff, framebufferWidth, framebufferHeight)

  def centeredText(
      cx: Float,
      y: Float,
      text: String,
      r: Float,
      g: Float,
      b: Float,
      scale: Float,
      framebufferWidth: Int,
      framebufferHeight: Int
  ): Unit =
    val safeW = (math.min(cx, framebufferWidth.toFloat - cx) * 2f - 12f).max(24f)
    centeredTextFit(cx, y, text, r, g, b, scale, safeW, framebufferWidth, framebufferHeight)

  def centeredTextBox(
      x: Float,
      y: Float,
      w: Float,
      h: Float,
      text: String,
      r: Float,
      g: Float,
      b: Float,
      scale: Float,
      uiScale: Float,
      framebufferWidth: Int,
      framebufferHeight: Int
  ): Unit =
    if text == null || text.isEmpty then return
    val (buf, quads, minX, maxX, minY, maxY) = textMetrics(text)
    if quads <= 0 then return
    val rawW = (maxX - minX).max(1f) + textFitPad(scale)
    val rawH = (maxY - minY).max(1f)
    val eff = scale
      .min((w - 20f * uiScale).max(8f) / rawW)
      .min((h - 8f * uiScale).max(8f) / rawH)
      .max(0.30f)
    val tx = x + w / 2f - rawW * eff / 2f - minX * eff
    val ty = y + h / 2f - rawH * eff / 2f - minY * eff
    val so = (1.20f * eff).max(0.65f)
    renderTextBuf(
      buf,
      quads,
      tx + so,
      ty + so,
      0f,
      0f,
      0f,
      eff,
      framebufferWidth,
      framebufferHeight
    )
    renderTextBuf(buf, quads, tx, ty, r, g, b, eff, framebufferWidth, framebufferHeight)

  def renderTextShadow(
      x: Float,
      y: Float,
      text: String,
      r: Float,
      g: Float,
      b: Float,
      scale: Float,
      framebufferWidth: Int,
      framebufferHeight: Int
  ): Unit =
    val o = (1.30f * scale).max(0.85f)
    renderText(x + o, y + o, text, 0f, 0f, 0f, scale, framebufferWidth, framebufferHeight)
    renderText(x, y, text, r, g, b, scale, framebufferWidth, framebufferHeight)

  def renderTextShadowFit(
      x: Float,
      y: Float,
      text: String,
      r: Float,
      g: Float,
      b: Float,
      scale: Float,
      maxWidth: Float,
      framebufferWidth: Int,
      framebufferHeight: Int
  ): Unit =
    renderTextShadow(
      x,
      y,
      fitText(text, scale, maxWidth),
      r,
      g,
      b,
      scale,
      framebufferWidth,
      framebufferHeight
    )
