package blockbox

import java.io.{DataOutputStream, OutputStream}
import java.security.{DigestOutputStream, MessageDigest}

object PixelFontTest:
  def run(): Unit =
    // Geometry captured before the font refactor, covering every original glyph and fallback.
    val sample =
      "!\"#%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]_abcdefghijklmnopqrstuvwxyz| \u2603"
    val (vertices, quads, _, width, _, height) = BlockboxRender2D.textMetrics(sample)
    assert(quads == 1104)
    val digest = MessageDigest.getInstance("SHA-256")
    val output = DataOutputStream(DigestOutputStream(OutputStream.nullOutputStream(), digest))
    val copy = vertices.duplicate().order(vertices.order()); copy.rewind()
    while copy.hasRemaining do output.writeFloat(copy.getFloat())
    output.flush()
    assert(
      java.util.HexFormat
        .of()
        .formatHex(
          digest.digest()
        ) == "f7975aa68d9fc2a08a391c28d30671baa600160a9e791822520e563e764e9530",
      "all original glyph vertices remain identical"
    )
    assert(width == sample.length * PixelFont.advance - 1 && height == PixelFont.height)
    assert(
      BlockboxRender2D.textMetrics(sample)._1 eq vertices,
      "text keeps its bounded geometry cache"
    )
    assert(PixelFont.glyph(' ').pixels.isEmpty)
    for ch <- "$^`{}~" do
      assert(
        PixelFont.glyph(ch).pixels.nonEmpty && PixelFont.glyph(ch) != PixelFont.glyph('?'),
        "previously missing printable symbols have real glyphs"
      )
    for width <- Vector(320f, 640f, 960f, 1920f) do
      val layout = BlockboxUiLayout.hotbar(width, 720, 1f)
      assert(layout.panel.x >= 0 && layout.panel.x + layout.panel.w <= width)
      assert(
        layout.slots.size == 9 && layout.slots
          .sliding(2)
          .forall(p => p.head.x + p.head.w < p.last.x)
      )
    println(
      "[pixel-font] original glyph geometry unchanged; complete readable ASCII atlas and fitted HUD layout passed"
    )
