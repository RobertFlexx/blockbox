package blockbox

import org.lwjgl.BufferUtils
import org.lwjgl.opengl.GL11.*
import org.lwjgl.opengl.GL15.*
import org.lwjgl.stb.STBEasyFont

import scala.math.*

object BlockboxRender2D:
  def textMetrics(text: String): (java.nio.ByteBuffer, Int, Float, Float, Float, Float) =
    val safeText = if text == null then "" else text
    val buf = BufferUtils.createByteBuffer((safeText.length.max(1)) * 320)
    val quads = STBEasyFont.stb_easy_font_print(0, 0, safeText, null, buf)
    if quads <= 0 then return (buf, quads, 0f, 0f, 0f, 0f)
    var minX = Float.MaxValue; var maxX = Float.MinValue
    var minY = Float.MaxValue; var maxY = Float.MinValue
    var i = 0
    while i < quads * 4 do
      val px = buf.getFloat(i * 16)
      val py = buf.getFloat(i * 16 + 4)
      if px < minX then minX = px
      if px > maxX then maxX = px
      if py < minY then minY = py
      if py > maxY then maxY = py
      i += 1
    (buf, quads, minX, maxX, minY, maxY)

  def textWidth(text: String, scale: Float): Float =
    if text == null || text.isEmpty then 0f
    else
      val (_, quads, minX, maxX, _, _) = textMetrics(text)
      if quads <= 0 then text.length * 8f * scale else (maxX - minX).max(1f) * scale

  def uiScale(framebufferWidth: Int, framebufferHeight: Int): Float =
    // Slightly larger global UI scale so small labels stay readable without blowing up layouts.
    val byWidth = framebufferWidth.toFloat / 1280f
    val byHeight = framebufferHeight.toFloat / 720f
    math.min(byWidth, byHeight).max(0.98f).min(1.38f)

  def resetGlArraysAndBuffers(): Unit =
    glBindBuffer(GL_ARRAY_BUFFER, 0)
    glDisableClientState(GL_VERTEX_ARRAY)
    glDisableClientState(GL_COLOR_ARRAY)
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
    glBegin(GL_QUADS); glVertex2f(x, y); glVertex2f(x + w, y); glVertex2f(x + w, y + h); glVertex2f(x, y + h); glEnd()

  def dimBackground(framebufferWidth: Int, framebufferHeight: Int): Unit =
    rect(0, 0, framebufferWidth, framebufferHeight, 0f, 0f, 0f, 0.55f)

  def drawPanel(x: Float, y: Float, w: Float, h: Float, scale: Float): Unit =
    rect(x + 5f * scale, y + 5f * scale, w, h, 0f, 0f, 0f, 0.30f)
    rect(x - 1f * scale, y - 1f * scale, w + 2f * scale, h + 2f * scale, 0.025f, 0.030f, 0.040f, 0.88f)
    rect(x, y, w, h, 0.095f, 0.115f, 0.165f, 0.96f)
    rect(x + 2f * scale, y + 2f * scale, w - 4f * scale, 2f * scale, 0.20f, 0.23f, 0.30f, 0.20f)
    rect(x + 2f * scale, y + h - 3f * scale, w - 4f * scale, 1.5f * scale, 0f, 0f, 0f, 0.20f)
    rect(x + 1f * scale, y + 1f * scale, 1f * scale, h - 2f * scale, 0.18f, 0.20f, 0.26f, 0.10f)
    rect(x + w - 2f * scale, y + 1f * scale, 1f * scale, h - 2f * scale, 0f, 0f, 0f, 0.16f)

  def slider(x: Float, y: Float, w: Float, value: Float, scale: Float): Unit =
    val trackH = 18f * scale; val knobW = 14f * scale; val knobH = 26f * scale
    rect(x, y, w, trackH, 0.02f, 0.02f, 0.02f, 0.80f)
    rect(x + 2 * scale, y + 2 * scale, w - 4 * scale, trackH - 4 * scale, 0.20f, 0.22f, 0.26f, 0.85f)
    val fillW = (w - 4 * scale - knobW) * value.max(0f).min(1f)
    rect(x + 2 * scale, y + 2 * scale, fillW.max(0f), trackH - 4 * scale, 0.50f, 0.55f, 0.65f, 0.70f)
    val knob = x + 2 * scale + fillW
    rect(knob, y - 4 * scale, knobW, knobH, 0.85f, 0.88f, 0.95f, 1f)
    rect(knob + 2 * scale, y - 2 * scale, knobW - 4 * scale, knobH - 8 * scale, 1f, 1f, 1f, 0.30f)

  def drawButton(x: Float, y: Float, w: Float, h: Float, label: String, accent: Boolean, hover: Boolean, scale: Float, framebufferWidth: Int, framebufferHeight: Int): Unit =
    val base = if accent then 0.36f else 0.275f
    val bright = if hover then base + 0.13f else base
    val textR = if accent then 1f else if hover then 1f else 0.92f
    val textG = if accent then 0.86f else if hover then 1f else 0.92f
    val textB = if accent then 0.36f else if hover then 1f else 0.94f
    rect(x + 3f * scale, y + 3f * scale, w, h, 0f, 0f, 0f, 0.24f)
    rect(x - 1f * scale, y - 1f * scale, w + 2f * scale, h + 2f * scale, 0.02f, 0.025f, 0.035f, 0.88f)
    rect(x, y, w, h, bright, bright * 1.02f, bright * 1.06f, 1f)
    rect(x, y, w, h * 0.46f, (bright + 0.11f).min(0.75f), (bright + 0.11f).min(0.75f), (bright + 0.14f).min(0.78f), 0.44f)
    rect(x + 2f * scale, y + 1f * scale, w - 4f * scale, 2f * scale, 1f, 1f, 1f, 0.10f)
    rect(x + 2f * scale, y + h - 2f * scale, w - 4f * scale, 1f * scale, 0f, 0f, 0f, 0.22f)
    if hover then
      rect(x - 1f * scale, y - 1f * scale, w + 2f * scale, h + 2f * scale, if accent then 0.95f else 0.58f, if accent then 0.78f else 0.60f, if accent then 0.28f else 0.70f, 0.16f)
      rect(x, y, w, h, 1f, 1f, 1f, 0.045f)
    centeredTextBox(x + 8f * scale, y + 3f * scale, w - 16f * scale, h - 6f * scale, label, textR, textG, textB, (1.05f * scale).max(0.82f).min(h / 12.5f), scale, framebufferWidth, framebufferHeight)

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
    glDisable(GL_DEPTH_TEST); glDisable(GL_CULL_FACE); setupOrtho(framebufferWidth, framebufferHeight)
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
    glDisable(GL_DEPTH_TEST); glDisable(GL_CULL_FACE); setupOrtho(framebufferWidth, framebufferHeight)
    val wave = (sin(time * 1.5f).toFloat * 0.015f + 0.20f).max(0.12f).min(0.32f)
    rect(0, 0, framebufferWidth, framebufferHeight, 0.01f, 0.12f + wave * 0.3f, 0.30f + wave * 0.3f, 0.24f)
    rect(0, 0, framebufferWidth, framebufferHeight * 0.15f, 0.01f, 0.15f, 0.35f, 0.10f)
    rect(0, framebufferHeight * 0.85f, framebufferWidth, framebufferHeight * 0.15f, 0.01f, 0.15f, 0.35f, 0.10f)
    val vignette = 0.20f
    rect(0, 0, framebufferWidth * 0.08f, framebufferHeight, 0f, 0.08f, 0.20f, vignette)
    rect(framebufferWidth * 0.92f, 0, framebufferWidth * 0.08f, framebufferHeight, 0f, 0.08f, 0.20f, vignette)

  def mainMenuLayout(framebufferWidth: Int, framebufferHeight: Int, scale: Float): (Float, Float, Float, Array[Float], Float, Float, Float, Float, Float) =
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
    val available = (h - firstY - footerReserve - labelH - gap * 8f).max(7f * 26f)
    val buttonH = (38f * scale).min(available / 7f).max(26f)
    val buttonW = (360f * scale).min(w * 0.48f).max(math.min(260f, w - margin * 2f))
    val bx = cx - buttonW / 2f
    val y0 = firstY
    val y1 = y0 + buttonH + gap + labelH + gap
    val ys = Array(y0, y1, y1 + (buttonH + gap), y1 + (buttonH + gap) * 2f, y1 + (buttonH + gap) * 3f, y1 + (buttonH + gap) * 4f, y1 + (buttonH + gap) * 5f)
    (bx, buttonW, buttonH, ys, titleX, titleY, titleW, titleH, scale)

  private def titleGlyph(c: Char): Array[String] = c match
    case 'B' => Array("11110", "10001", "10001", "11110", "10001", "10001", "11110")
    case 'L' => Array("10000", "10000", "10000", "10000", "10000", "10000", "11111")
    case 'O' => Array("01110", "10001", "10001", "10001", "10001", "10001", "01110")
    case 'C' => Array("01111", "10000", "10000", "10000", "10000", "10000", "01111")
    case 'K' => Array("10001", "10010", "10100", "11000", "10100", "10010", "10001")
    case 'X' => Array("10001", "10001", "01010", "00100", "01010", "10001", "10001")
    case _   => Array("00000", "00000", "00000", "00000", "00000", "00000", "00000")

  def drawPixelLogo(cx: Float, y: Float, text: String, maxW: Float, maxH: Float): Unit =
    if text == null || text.isEmpty then return
    val chars = text.toUpperCase.filter(ch => ch == ' ' || "BLOCKX".indexOf(ch) >= 0)
    if chars.isEmpty then return
    var cols = 0
    for ch <- chars do cols += (if ch == ' ' then 3 else 5)
    cols += (chars.length - 1).max(0)
    val cell = (maxW / cols.toFloat).min(maxH / 7f).max(1.5f)
    val totalW = cols * cell
    var x = cx - totalW / 2f
    val shadow = (cell * 0.22f).max(1.2f)
    def drawPass(offX: Float, offY: Float, r: Float, g: Float, b: Float, a: Float): Unit =
      var px = x + offX
      for ch <- chars do
        if ch == ' ' then px += 4f * cell
        else
          val glyph = titleGlyph(ch)
          for row <- 0 until 7 do
            val line = glyph(row)
            for col <- 0 until 5 do
              if line.charAt(col) == '1' then
                rect(px + col * cell + cell * 0.04f, y + offY + row * cell + cell * 0.04f, cell * 0.92f, cell * 0.92f, r, g, b, a)
          px += 6f * cell
    drawPass(shadow, shadow * 1.4f, 0f, 0f, 0f, 0.70f)
    drawPass(0f, 0f, 1f, 0.92f, 0.40f, 1f)
    rect(cx - totalW / 2f, y + 7.45f * cell, totalW, cell * 0.18f, 1f, 0.90f, 0.42f, 0.22f)

  def renderTextBuf(buf: java.nio.ByteBuffer, quads: Int, x: Float, y: Float, r: Float, g: Float, b: Float, scale: Float, framebufferWidth: Int, framebufferHeight: Int): Unit =
    buf.rewind()
    glDisable(GL_DEPTH_TEST); glDisable(GL_CULL_FACE); glDisable(GL_TEXTURE_2D); setupOrtho(framebufferWidth, framebufferHeight)
    glBindBuffer(GL_ARRAY_BUFFER, 0)
    glDisableClientState(GL_COLOR_ARRAY); glDisableClientState(GL_TEXTURE_COORD_ARRAY)
    glEnable(GL_BLEND); glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA)
    glPushMatrix(); glTranslatef(x, y, 0); glScalef(scale, scale, 1); glColor4f(r, g, b, 1f)
    glEnableClientState(GL_VERTEX_ARRAY); glVertexPointer(2, GL_FLOAT, 16, buf)
    glDrawArrays(GL_QUADS, 0, quads * 4); glDisableClientState(GL_VERTEX_ARRAY); glPopMatrix()

  def renderText(x: Float, y: Float, text: String, r: Float, g: Float, b: Float, scale: Float, framebufferWidth: Int, framebufferHeight: Int): Unit =
    if text == null || text.isEmpty then return
    val lineH = 12f * scale
    if x >= framebufferWidth - 4f || x + 2f < 0 || y >= framebufferHeight - 2f || y + lineH < 0 then return
    val maxChars = ((framebufferWidth - x - 12f).max(4f) / (8f * scale).max(1f)).toInt
    val display = if maxChars <= 0 then "" else if maxChars >= text.length then text else text.take(maxChars.max(4) - 1) + "…"
    if display.isEmpty then return
    val (buf, quads, _, _, _, _) = textMetrics(display)
    if quads <= 0 then return
    renderTextBuf(buf, quads, x, y, r, g, b, scale, framebufferWidth, framebufferHeight)

  def centeredTextFit(cx: Float, y: Float, text: String, r: Float, g: Float, b: Float, scale: Float, maxWidth: Float, framebufferWidth: Int, framebufferHeight: Int): Unit =
    if text == null || text.isEmpty then return
    val (buf, quads, minX, maxX, minY, _) = textMetrics(text)
    if quads <= 0 then return
    val rawW = (maxX - minX).max(1f)
    val safeW = maxWidth.max(12f).min(framebufferWidth.toFloat - 12f)
    val eff = scale.min(safeW / rawW).max(0.30f)
    val left = cx - rawW * eff / 2f - minX * eff
    val top = y - minY * eff
    val so = (1.25f * eff).max(0.75f)
    renderTextBuf(buf, quads, left + so, top + so, 0f, 0f, 0f, eff, framebufferWidth, framebufferHeight)
    renderTextBuf(buf, quads, left, top, r, g, b, eff, framebufferWidth, framebufferHeight)

  def centeredText(cx: Float, y: Float, text: String, r: Float, g: Float, b: Float, scale: Float, framebufferWidth: Int, framebufferHeight: Int): Unit =
    val safeW = (math.min(cx, framebufferWidth.toFloat - cx) * 2f - 12f).max(24f)
    centeredTextFit(cx, y, text, r, g, b, scale, safeW, framebufferWidth, framebufferHeight)

  def centeredTextBox(x: Float, y: Float, w: Float, h: Float, text: String, r: Float, g: Float, b: Float, scale: Float, uiScale: Float, framebufferWidth: Int, framebufferHeight: Int): Unit =
    if text == null || text.isEmpty then return
    val (buf, quads, minX, maxX, minY, maxY) = textMetrics(text)
    if quads <= 0 then return
    val rawW = (maxX - minX).max(1f)
    val rawH = (maxY - minY).max(1f)
    val eff = scale.min((w - 14f * uiScale).max(8f) / rawW).min((h - 8f * uiScale).max(8f) / rawH).max(0.30f)
    val tx = x + w / 2f - rawW * eff / 2f - minX * eff
    val ty = y + h / 2f - rawH * eff / 2f - minY * eff
    val so = (1.20f * eff).max(0.65f)
    renderTextBuf(buf, quads, tx + so, ty + so, 0f, 0f, 0f, eff, framebufferWidth, framebufferHeight)
    renderTextBuf(buf, quads, tx, ty, r, g, b, eff, framebufferWidth, framebufferHeight)

  def renderTextShadow(x: Float, y: Float, text: String, r: Float, g: Float, b: Float, scale: Float, framebufferWidth: Int, framebufferHeight: Int): Unit =
    val o = (1.60f * scale).max(1.05f)
    renderText(x + o, y + o, text, 0f, 0f, 0f, scale, framebufferWidth, framebufferHeight)
    renderText(x - o, y, text, 0f, 0f, 0f, scale, framebufferWidth, framebufferHeight)
    renderText(x + o, y, text, 0f, 0f, 0f, scale, framebufferWidth, framebufferHeight)
    renderText(x, y - o, text, 0f, 0f, 0f, scale, framebufferWidth, framebufferHeight)
    renderText(x, y + o, text, 0f, 0f, 0f, scale, framebufferWidth, framebufferHeight)
    renderText(x - o, y - o, text, 0f, 0f, 0f, scale, framebufferWidth, framebufferHeight)
    renderText(x + o, y - o, text, 0f, 0f, 0f, scale, framebufferWidth, framebufferHeight)
    renderText(x - o, y + o, text, 0f, 0f, 0f, scale, framebufferWidth, framebufferHeight)
    renderText(x, y, text, r, g, b, scale, framebufferWidth, framebufferHeight)
