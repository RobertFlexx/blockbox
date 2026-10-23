package blockbox

final case class UiRect(x: Float, y: Float, w: Float, h: Float):
  def contains(mx: Float, my: Float): Boolean = mx >= x && my >= y && mx < x + w && my < y + h

final case class HotbarLayout(panel: UiRect, slots: Vector[UiRect])

final case class MainMenuLayout(panel: UiRect, buttons: Vector[UiRect], scale: Float)
final case class SettingsLayout(panel: UiRect, tabs: Vector[UiRect], content: UiRect, done: UiRect)
final case class JournalLayout(
    panel: UiRect,
    rows: Vector[UiRect],
    previous: UiRect,
    next: UiRect,
    back: UiRect,
    scale: Float
)
final case class InventoryLayout(
    panel: UiRect,
    preview: UiRect,
    storage: Vector[UiRect],
    hotbar: Vector[UiRect],
    craft: Vector[UiRect],
    output: UiRect,
    furnace: UiRect,
    journal: UiRect,
    close: UiRect,
    scale: Float
)
final case class CatalogLayout(
    panel: UiRect,
    tabs: Vector[UiRect],
    search: UiRect,
    slots: Vector[UiRect],
    hotbar: Vector[UiRect],
    scroll: UiRect,
    close: UiRect,
    scale: Float
)
final case class FurnaceLayout(
    panel: UiRect,
    input: UiRect,
    fuel: UiRect,
    output: UiRect,
    progress: UiRect,
    one: UiRect,
    all: UiRect,
    stop: UiRect,
    close: UiRect,
    storage: Vector[UiRect],
    hotbar: Vector[UiRect],
    scale: Float
)

/** Drawing and hit testing share these layouts at every GUI scale. */
object BlockboxUiLayout:
  def hotbar(w: Float, h: Float, s: Float, count: Int = 9): HotbarLayout =
    require(count > 0)
    val gap = (3f * s).max(2f)
    val slot = (52f * s).min((w - 24f * s - gap * (count - 1)) / count).max(12f)
    val width = slot * count + gap * (count - 1)
    val x = (w - width) / 2f; val y = (h - slot - 12f * s).max(8f)
    HotbarLayout(
      UiRect(x - 8 * s, y - 8 * s, width + 16 * s, slot + 16 * s),
      Vector.tabulate(count)(i => UiRect(x + i * (slot + gap), y, slot, slot))
    )

  def journal(w: Float, h: Float, requested: Float): JournalLayout =
    val s = requested.max(0.8f).min((w - 24f) / 680f).min((h - 24f) / 496f).max(0.35f)
    val p = UiRect((w - 680 * s) / 2, (h - 496 * s) / 2, 680 * s, 496 * s)
    val count = if s < 0.85f then 3 else 6
    val rowH = (p.h - 146 * s) / count
    val rows = Vector.tabulate(count)(i =>
      UiRect(p.x + 24 * s, p.y + 86 * s + i * rowH, p.w - 48 * s, rowH - 5 * s)
    )
    JournalLayout(
      p,
      rows,
      UiRect(p.x + 24 * s, p.y + p.h - 50 * s, 100 * s, 34 * s),
      UiRect(p.x + p.w - 124 * s, p.y + p.h - 50 * s, 100 * s, 34 * s),
      UiRect(p.x + p.w / 2 - 100 * s, p.y + p.h - 50 * s, 200 * s, 34 * s),
      s
    )

  def furnace(w: Float, h: Float, requested: Float): FurnaceLayout =
    val s = (requested * 2.6f).min((w - 24f) / 216f).min((h - 24f) / 224f).max(0.35f)
    val p = UiRect((w - 216 * s) / 2, (h - 224 * s) / 2, 216 * s, 224 * s)
    def r(x: Float, y: Float, ww: Float, hh: Float): UiRect =
      UiRect(p.x + x * s, p.y + y * s, ww * s, hh * s)
    FurnaceLayout(
      p,
      r(52, 27, 20, 20),
      r(52, 62, 20, 20),
      r(138, 44, 26, 26),
      r(83, 48, 39, 12),
      r(9, 91, 59, 17),
      r(78, 91, 59, 17),
      r(147, 91, 59, 17),
      r(197, 5, 13, 12),
      Vector.tabulate(27)(i => r(9 + (i % 9) * 22, 128 + (i / 9) * 22, 20, 20)),
      Vector.tabulate(9)(i => r(9 + i * 22, 199, 20, 20)),
      s
    )
  def inventory(w: Float, h: Float, requested: Float): InventoryLayout =
    val s = (requested * 2.6f).min((w - 24f) / 216f).min((h - 24f) / 206f).max(0.6f)
    val p = UiRect((w - 216 * s) / 2, (h - 206 * s) / 2, 216 * s, 206 * s)
    def r(x: Float, y: Float, ww: Float, hh: Float): UiRect =
      UiRect(p.x + x * s, p.y + y * s, ww * s, hh * s)
    InventoryLayout(
      p,
      r(9, 22, 64, 76),
      Vector.tabulate(27)(i => r(9 + (i % 9) * 22, 112 + (i / 9) * 22, 20, 20)),
      Vector.tabulate(9)(i => r(9 + i * 22, 183, 20, 20)),
      Vector.tabulate(9)(i => r(98 + (i % 3) * 20, 30 + (i / 3) * 20, 18, 18)),
      r(184, 50, 20, 20),
      r(79, 94, 55, 14),
      r(139, 94, 67, 14),
      r(197, 5, 13, 12),
      s
    )

  def catalog(w: Float, h: Float, requested: Float): CatalogLayout =
    val s = (requested * 2.2f).min((w - 24) / 252f).min((h - 32) / 278f).max(0.6f)
    val p = UiRect((w - 252 * s) / 2, (h - 278 * s) / 2 + 24 * s, 252 * s, 250 * s)
    def r(x: Float, y: Float, ww: Float, hh: Float): UiRect =
      UiRect(p.x + x * s, p.y + y * s, ww * s, hh * s)
    CatalogLayout(
      p,
      Vector.tabulate(6)(i => r(8 + i * 36, -23, 32, 25)),
      r(12, 28, 216, 17),
      Vector.tabulate(54)(i => r(12 + (i % 9) * 24, 54 + (i / 9) * 24, 22, 22)),
      Vector.tabulate(9)(i => r(12 + i * 24, 222, 22, 22)),
      r(236, 54, 6, 142),
      r(234, 7, 12, 12),
      s
    )
  def mainMenu(w: Float, h: Float, requestedScale: Float): MainMenuLayout =
    val s = requestedScale.min((w - 32f) / 680f).min((h - 44f) / 490f).max(0.35f)
    val panel = UiRect((w - 680f * s) / 2f, (h - 490f * s) / 2f, 680f * s, 490f * s)
    val left = panel.x + 34f * s; val right = panel.x + 350f * s
    val y = panel.y + 218f * s; val step = 56f * s
    def button(x: Float, row: Int): UiRect = UiRect(x, y + step * row, 296f * s, 42f * s)
    MainMenuLayout(
      panel,
      Vector(
        button(left, 0),
        button(left, 1),
        button(right, 1),
        button(right, 0),
        button(right, 2),
        button(left, 2),
        button(left, 3),
        button(right, 3)
      ),
      s
    )

  def settings(w: Float, h: Float, s: Float): SettingsLayout =
    val pw = (680f * s).min(w - 24f); val ph = (610f * s).min(h - 28f)
    val panel = UiRect((w - pw) / 2f, (h - ph) / 2f, pw, ph)
    val padding = 26f * s; val contentW = pw - padding * 2f
    val tabW = (contentW - 12f * s) / 3f
    val tabs = Vector.tabulate(3)(i =>
      UiRect(panel.x + padding + i * (tabW + 6f * s), panel.y + 64f * s, tabW, 30f * s)
    )
    val content = UiRect(panel.x + padding, panel.y + 108f * s, contentW - 14f * s, ph - 178f * s)
    val done = UiRect(w / 2f - 100f * s, panel.y + ph - 52f * s, 200f * s, 34f * s)
    SettingsLayout(panel, tabs, content, done)
