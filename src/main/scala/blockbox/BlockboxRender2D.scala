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
  private final case class TextMesh(vertices: java.nio.ByteBuffer, quads: Int, width: Float):
    val metrics = (vertices, quads, 0f, width, 0f, PixelFont.height.toFloat)
  private val textMeshCache = scala.collection.mutable.LinkedHashMap.empty[String, TextMesh]
  private val maxTextMeshCacheEntries = 512

  private def textFitPad(scale: Float): Float = (2f * scale).max(2f)

  /** Build immutable glyph geometry once per string. Draw calls only position and tint it. */
  private def buildTextMesh(text: String): TextMesh =
    val glyphs = text.iterator.map(PixelFont.glyph).toVector
    val quads = glyphs.iterator.map(_.pixels.length).sum
    val vertices = BufferUtils.createByteBuffer(quads.max(1) * 4 * 16)
    def vertex(x: Float, y: Float): Unit =
      vertices.putFloat(x).putFloat(y).putFloat(0f).putFloat(0f)
    glyphs.zipWithIndex.foreach { (glyph, index) =>
      glyph.pixels.foreach { (px, py) =>
        val x = (index * PixelFont.advance + px).toFloat; val y = py.toFloat
        vertex(x, y); vertex(x + 1, y); vertex(x + 1, y + 1); vertex(x, y + 1)
      }
    }
    vertices.flip()
    TextMesh(vertices, quads, if text.isEmpty then 0f else text.length * PixelFont.advance - 1f)

  def textMetrics(text: String): (java.nio.ByteBuffer, Int, Float, Float, Float, Float) =
    val key = Option(text).getOrElse("")
    val mesh = textMeshCache.remove(key).getOrElse(buildTextMesh(key))
    textMeshCache(key) = mesh
    if textMeshCache.size > maxTextMeshCacheEntries then textMeshCache.remove(textMeshCache.head._1)
    mesh.metrics

  def textWidth(text: String, scale: Float): Float =
    if text == null || text.isEmpty then 0f
    else ((text.length * PixelFont.advance.toFloat - 1f).max(1f) * scale) + textFitPad(scale)

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
    rect(x + 3f * scale, y + 4f * scale, w, h, 0f, 0f, 0f, 0.45f)
    rect(x - e, y - e, w + 2 * e, h + 2 * e, 0.08f, 0.08f, 0.08f, 1)
    rect(x, y, w, h, 0.31f, 0.31f, 0.31f, 1)
    rect(x, y, w, e, 0.70f, 0.70f, 0.70f, 1)
    rect(x, y, e, h, 0.62f, 0.62f, 0.62f, 1)
    rect(x + w - e, y + e, e, h - e, 0.16f, 0.16f, 0.16f, 1)
    rect(x + e, y + h - e, w - e, e, 0.16f, 0.16f, 0.16f, 1)

  def drawSlotFrame(
      x: Float,
      y: Float,
      size: Float,
      selected: Boolean,
      hover: Boolean,
      scale: Float
  ): Unit =
    val e = (2f * scale).max(1f)
    val inner = if hover then 0.76f else 0.66f
    // Recessed inventory-slot bevel: dark top/left, light bottom/right.
    rect(x - e, y - e, size + e * 2f, size + e * 2f, 0.10f, 0.10f, 0.10f, 0.98f)
    rect(x, y, size, size, inner, inner, inner, 1f)
    rect(x, y, size, e, 0.18f, 0.18f, 0.18f, 0.98f)
    rect(x, y, e, size, 0.17f, 0.17f, 0.17f, 0.98f)
    rect(x, y + size - e, size, e, 0.60f, 0.60f, 0.60f, 0.92f)
    rect(x + size - e, y, e, size, 0.56f, 0.56f, 0.56f, 0.92f)
    if selected then drawSlotSelection(x, y, size, scale)

  /** Draw after adjacent slots and icons, so no neighboring bevel erases the selection. */
  def drawSlotSelection(x: Float, y: Float, size: Float, scale: Float): Unit =
    val s = (2f * scale).max(1f).round.toFloat
    rect(x - s * 2f, y - s * 2f, size + s * 4f, s, 0.98f, 0.98f, 0.98f, 1f)
    rect(x - s * 2f, y + size + s, size + s * 4f, s, 0.98f, 0.98f, 0.98f, 1f)
    rect(x - s * 2f, y - s * 2f, s, size + s * 4f, 0.98f, 0.98f, 0.98f, 1f)
    rect(x + size + s, y - s * 2f, s, size + s * 4f, 0.98f, 0.98f, 0.98f, 1f)

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
    rect(x - e, y - e, w + 2 * e, h + 2 * e, 0.08f, 0.08f, 0.08f, 1)
    val v = if hover then 0.62f else 0.48f
    rect(x, y, w, h, v, v, v, 1)
    rect(x, y, w - e, e, 0.86f, 0.86f, 0.86f, 1)
    rect(x, y, e, h - e, 0.75f, 0.75f, 0.75f, 1)
    rect(x + e, y + h - e, w - e, e, 0.25f, 0.25f, 0.25f, 1)
    rect(x + w - e, y + e, e, h - e, 0.25f, 0.25f, 0.25f, 1)
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
        ".........",
        ".....#...",
        ".#.....#.",
        "...##....",
        "..#..#.#.",
        "...##....",
        ".#.......",
        "......#..",
        "........."
      )
    else
      Array(
        "...###...",
        "..#...#..",
        ".#.....#.",
        "#.......#",
        "#.......#",
        "#.......#",
        ".#.....#.",
        "..#...#..",
        "...###..."
      )
    for row <- pattern.indices; col <- 0 until 9 if pattern(row)(col) == '#' do
      rect(x + (col + 0.5f) * p, y + (row + 0.5f) * p, p, p, 0.01f, 0.07f, 0.13f, 0.8f)
      rect(x + col * p, y + row * p, p, p, 0.42f, 0.83f, 1f, 1f)
    if !popping then
      rect(x + 2 * p, y + 2 * p, 2 * p, p, 0.92f, 0.99f, 1f, 1f)
      rect(x + 2 * p, y + 3 * p, p, p, 0.78f, 0.95f, 1f, 1f)
      rect(x + 5 * p, y + 6 * p, 2 * p, p, 0.12f, 0.49f, 0.78f, 1f)

  // # is the outline, R is red fill and H is its highlight; dots are transparent.
  private val heartPixels = Vector(
    ".##...##.",
    "#RR#.#RR#",
    "#RHR#RHR#",
    "#HRRRRRR#",
    ".#RRRRR#.",
    "..#RRR#..",
    "...#R#...",
    "....#...."
  ).zipWithIndex.flatMap((row, y) =>
    row.zipWithIndex.collect {
      case (pixel, x) if pixel != '.' => (x, y, pixel)
    }
  )

  def drawHeartIcon(x: Float, y: Float, size: Float, fillFrac: Float, bgAlpha: Float): Unit =
    val cell = math.round(size / 9f).max(1).toFloat
    val left = math.round(x + (size - 9 * cell) / 2).toFloat
    val top = math.round(y + (size - 8 * cell) / 2).toFloat
    val filledColumns = math.round(fillFrac.max(0).min(1) * 9)
    resetGlArraysAndBuffers()
    glDisable(GL_TEXTURE_2D); glEnable(GL_BLEND); glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA)
    glBegin(GL_QUADS)
    heartPixels.foreach { (column, row, pixel) =>
      if pixel == '#' then glColor4f(0.035f, 0.025f, 0.035f, 1f)
      else if column >= filledColumns then glColor4f(0.22f, 0.14f, 0.16f, (bgAlpha + 0.42f).min(1))
      else if pixel == 'H' then glColor4f(1f, 0.78f, 0.78f, 1f)
      else glColor4f(0.98f - row * 0.025f, 0.08f, 0.12f, 1f)
      val px = left + column * cell; val py = top + row * cell
      glVertex2f(px, py); glVertex2f(px + cell, py); glVertex2f(px + cell, py + cell);
      glVertex2f(px, py + cell)
    }
    glEnd(); glColor4f(1f, 1f, 1f, 1f)

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
        PixelFont.logoGlyph(ch).pixels.foreach { (c, row) => cells += ((column + c, row)) }
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
