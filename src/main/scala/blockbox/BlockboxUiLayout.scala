package blockbox

final case class UiRect(x: Float, y: Float, w: Float, h: Float):
  def contains(mx: Float, my: Float): Boolean = mx >= x && my >= y && mx < x + w && my < y + h

final case class MainMenuLayout(panel: UiRect, buttons: Vector[UiRect], scale: Float)
final case class SettingsLayout(panel: UiRect, tabs: Vector[UiRect], content: UiRect, done: UiRect)

/** Drawing and hit testing share these layouts at every GUI scale. */
object BlockboxUiLayout:
  def mainMenu(w: Float, h: Float, requestedScale: Float): MainMenuLayout =
    val s = requestedScale.min((w - 32f) / 680f).min((h - 44f) / 490f).max(0.35f)
    val panel = UiRect((w - 680f*s)/2f,(h - 490f*s)/2f,680f*s,490f*s)
    val left = panel.x + 34f*s; val right = panel.x + 350f*s
    val y = panel.y + 218f*s; val step = 56f*s
    def button(x: Float, row: Int): UiRect = UiRect(x,y+step*row,296f*s,42f*s)
    MainMenuLayout(panel,Vector(button(left,0),button(left,1),button(right,1),button(right,0),button(right,2),button(left,2),button(left,3),button(right,3)),s)

  def settings(w: Float, h: Float, s: Float): SettingsLayout =
    val pw = (680f*s).min(w - 24f); val ph = (610f*s).min(h - 28f)
    val panel = UiRect((w-pw)/2f,(h-ph)/2f,pw,ph)
    val padding = 26f*s; val contentW = pw - padding*2f
    val tabW = (contentW - 12f*s)/3f
    val tabs = Vector.tabulate(3)(i => UiRect(panel.x+padding+i*(tabW+6f*s),panel.y+64f*s,tabW,30f*s))
    val content = UiRect(panel.x+padding,panel.y+108f*s,contentW-14f*s,ph-178f*s)
    val done = UiRect(w/2f-100f*s,panel.y+ph-52f*s,200f*s,34f*s)
    SettingsLayout(panel,tabs,content,done)
