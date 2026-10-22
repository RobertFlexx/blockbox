package blockbox

import org.lwjgl.BufferUtils
import org.lwjgl.opengl.GL11.*
import scala.math.*

/** Articulated voxel character. All animation state advances during simulation.
  *
  * Every part is a cuboid textured from [[BlockboxSkin]] with Minecraft's box UV layout,
  * so faces, hair, clothes and hands carry pixel detail instead of flat colour.
  * Models are lit like Minecraft entities: two fixed directional lights plus ambient,
  * multiplied by the terrain light colour where the model stands, so a player under a
  * tree, in a cave or next to a torch matches the blocks around them.
  */
object BlockboxPlayerModel:
  private var lit = false
  private var tint = (1f, 1f, 1f)
  private val lightBuffer = BufferUtils.createFloatBuffer(4)
  private def put(a: Float, b: Float, c: Float, d: Float) =
    lightBuffer.clear(); lightBuffer.put(a).put(b).put(c).put(d).flip(); lightBuffer

  /** One skin pixel in world units: Minecraft's 32-pixel player is 1.8 blocks tall. */
  private val px = 0.05625f
  private val upperArm = 0.32f
  private val forearm = 0.36f
  private val elbowRow = upperArm / (upperArm + forearm)

  /** Enables entity lighting. Call while the modelview holds only the camera rotation
    * (world lights) or identity (`viewSpace`, for the first-person arm).
    */
  def beginLighting(viewSpace: Boolean): Unit =
    val (l0, l1) =
      if viewSpace then (Vec3(-0.35f, 0.90f, 0.62f).normalized, Vec3(0.62f, 0.25f, 0.35f).normalized)
      else (Vec3(0.2f, 1.0f, -0.7f).normalized, Vec3(-0.2f, 1.0f, 0.7f).normalized)
    glEnable(GL_LIGHTING)
    glLightModelfv(GL_LIGHT_MODEL_AMBIENT, put(0.40f, 0.40f, 0.40f, 1f))
    glEnable(GL_LIGHT0)
    glLightfv(GL_LIGHT0, GL_POSITION, put(l0.x, l0.y, l0.z, 0f))
    glLightfv(GL_LIGHT0, GL_DIFFUSE, put(0.60f, 0.60f, 0.60f, 1f))
    glLightfv(GL_LIGHT0, GL_AMBIENT, put(0f, 0f, 0f, 1f))
    glLightfv(GL_LIGHT0, GL_SPECULAR, put(0f, 0f, 0f, 1f))
    glEnable(GL_LIGHT1)
    glLightfv(GL_LIGHT1, GL_POSITION, put(l1.x, l1.y, l1.z, 0f))
    glLightfv(GL_LIGHT1, GL_DIFFUSE, put(if viewSpace then 0.34f else 0.60f, if viewSpace then 0.34f else 0.60f, if viewSpace then 0.34f else 0.60f, 1f))
    glLightfv(GL_LIGHT1, GL_AMBIENT, put(0f, 0f, 0f, 1f))
    glLightfv(GL_LIGHT1, GL_SPECULAR, put(0f, 0f, 0f, 1f))
    glEnable(GL_COLOR_MATERIAL)
    glColorMaterial(GL_FRONT_AND_BACK, GL_AMBIENT_AND_DIFFUSE)
    glEnable(GL_NORMALIZE)
    lit = true

  def endLighting(): Unit =
    glDisable(GL_LIGHT0); glDisable(GL_LIGHT1); glDisable(GL_LIGHTING)
    glDisable(GL_COLOR_MATERIAL); glDisable(GL_NORMALIZE)
    lit = false

  private def bindSkin(shirt: (Float, Float, Float), light: (Float, Float, Float)): Unit =
    tint = light
    glEnable(GL_TEXTURE_2D)
    glBindTexture(GL_TEXTURE_2D, BlockboxSkin.texture(shirt))
    glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_MODULATE)

  private def unbindSkin(): Unit =
    glColor4f(1f, 1f, 1f, 1f)
    glBindTexture(GL_TEXTURE_2D, 0)
    glDisable(GL_TEXTURE_2D)

  def render(player: RemotePlayer, camera: Vec3, color: (Float, Float, Float), light: (Float, Float, Float), time: Float = 0f): Unit =
    val stride = player.stride
    val walk = sin(player.gait).toFloat * 32f * stride
    val mine = sin((1f - player.swing) * Pi).toFloat * (if player.swing > 0f then 1f else 0f)
    val headYaw = ((player.yaw - player.bodyYaw + 540f) % 360f - 180f).max(-70f).min(70f)
    val idle = 1f - stride.max(0f).min(1f)
    val breath = sin(time * 1.9f).toFloat * 0.006f * idle
    val legLength = 12 * px
    val shoulder = 24 * px
    bindSkin(color, light)
    glPushMatrix()
    glTranslatef(player.pos.x - camera.x, player.pos.y - 1.62f + abs(cos(player.gait).toFloat) * 0.025f * stride, player.pos.z - camera.z)
    glRotatef(-player.bodyYaw, 0f, 1f, 0f)
    glRotatef(mine * -7f, 0f, 1f, 0f)
    // Torso: shirt, collar, pocket, belt and buckle are all painted on one 8x12x4 box.
    skinBox(-4 * px, legLength, -2 * px, 4 * px, shoulder + breath, 2 * px, 16, 16, 8, 12, 4)
    // Head turns around the neck, independently of the body's trailing yaw. The hair
    // layer sits half a pixel outside the head, like Minecraft's hat layer.
    glPushMatrix(); glTranslatef(0f, shoulder + breath, 0f)
    glRotatef(-headYaw, 0f, 1f, 0f); glRotatef(-player.pitch.max(-85f).min(85f), 1f, 0f, 0f)
    skinBox(-4 * px, 0f, -4 * px, 4 * px, 8 * px, 4 * px, 0, 0, 8, 8, 8)
    val o = 0.5f * px
    glEnable(GL_ALPHA_TEST); glAlphaFunc(GL_GREATER, 0.5f)
    skinBox(-4 * px - o, -o, -4 * px - o, 4 * px + o, 8 * px + o, 4 * px + o, 32, 0, 8, 8, 8)
    glDisable(GL_ALPHA_TEST)
    glPopMatrix()
    var side = -1
    while side <= 1 do
      val leg = walk * side
      val (lu, lv) = if side > 0 then (0, 16) else (16, 48)
      glPushMatrix(); glTranslatef(side * 2 * px, legLength, 0f); glRotatef(leg, 1f, 0f, 0f)
      skinBox(-2 * px, -legLength * 0.5f, -2 * px, 2 * px, 0f, 2 * px, lu, lv, 4, 12, 4, 0f, 0.5f)
      glTranslatef(0f, -legLength * 0.5f, 0f); glRotatef(-leg.max(0f) * 0.35f, 1f, 0f, 0f)
      jointSegment(2 * px, legLength * 0.5f, legLength, lu, lv, 0.5f)
      glPopMatrix()
      val action = if side == 1 then mine else 0f
      val sway = sin(time * 1.3f + side).toFloat * 2.2f * idle
      glPushMatrix(); glTranslatef(side * (6 * px + 0.003f), shoulder - 0.04f + breath, 0f)
      glRotatef(side * (3f + sway * 0.5f) + action * 12f, 0f, 0f, 1f)
      glRotatef(-leg * 0.85f + action * 85f + sway, 1f, 0f, 0f)
      armGeometry(side, -abs(leg) * 0.18f + action * 22f, 0.15f + action * 0.75f, None)
      glPopMatrix()
      side += 2
    glPopMatrix()
    unbindSkin()

  /** One skin-textured arm in both camera views. Held items share the elbow frame. */
  def arm(shirt: (Float,Float,Float), light: (Float,Float,Float), side: Int, elbow: Float, grip: Float,
          held: Option[() => Unit] = None): Unit =
    bindSkin(shirt, light)
    armGeometry(side, elbow, grip, held)
    unbindSkin()

  private def armGeometry(side: Int, elbow: Float, grip: Float, held: Option[() => Unit]): Unit =
    val (u, v) = if side >= 0 then (40, 16) else (32, 48)
    val w = 2 * px
    val elbowY = 0.04f - upperArm
    // Shoulder to elbow: sleeve, cuff and upper arm come from the top of the arm texture.
    skinBox(-w, elbowY, -w, w, 0.04f, w, u, v, 4, 12, 4, 0f, elbowRow)
    glPushMatrix(); glTranslatef(0, elbowY, 0); glRotatef(elbow, 1, 0, 0)
    jointSegment(w, forearm, upperArm + forearm, u, v, elbowRow)
    if grip > 0.55f then
      // A curled thumb over the grip, sampled from plain forearm skin.
      glPushMatrix(); glTranslatef(-side * 0.104f, -0.282f, -0.045f)
      glRotatef(side * 12f, 0, 0, 1)
      skinBox(-0.018f, -0.047f, -0.064f, 0.018f, 0.015f, 0.035f, u + 6.25f, v + 11.25f, 0f, 0f, 0f)
      glPopMatrix()
    held.foreach(_())
    glPopMatrix()

  /** The lower half of a jointed limb, hanging from its joint. It reaches a little way up
    * inside the upper half (a hair narrower, so the sides never z-fight) and fills the
    * wedge a bent elbow or knee would otherwise open.
    */
  private def jointSegment(halfWidth: Float, length: Float, limbLength: Float, u: Int, v: Int, from: Float): Unit =
    val overlap = 0.035f
    val w = halfWidth - 0.0015f
    skinBox(-w, -length, -w, w, overlap, w, u, v, 4, 12, 4, from - overlap / limbLength, 1f)

  /** A cuboid textured with Minecraft's box layout: (u, v) is the box origin and
    * (w, h, d) its size in skin pixels. `from`/`to` select a slice of the side faces for
    * jointed limb segments; hidden joint caps then sample the skin at the cut.
    * With entity lighting on, the lights shade each face instead of fixed constants.
    */
  private def skinBox(x0: Float, y0: Float, z0: Float, x1: Float, y1: Float, z1: Float,
                      u: Float, v: Float, w: Float, h: Float, d: Float, from: Float = 0f, to: Float = 1f): Unit =
    val k = 1f / 64f
    val top = (v + d + from * h) * k
    val bottom = (v + d + to * h) * k
    def color(shade: Float): Unit =
      val s = if lit then 1f else shade
      glColor4f((tint._1 * s).min(1f), (tint._2 * s).min(1f), (tint._3 * s).min(1f), 1f)
    def quad(nx: Float, ny: Float, nz: Float, ua: Float, va: Float, ub: Float, vb: Float,
             ax: Float, ay: Float, az: Float, bx: Float, by: Float, bz: Float,
             cx: Float, cy: Float, cz: Float, dx: Float, dy: Float, dz: Float): Unit =
      glNormal3f(nx, ny, nz)
      glTexCoord2f(ua, va); glVertex3f(ax, ay, az)
      glTexCoord2f(ub, va); glVertex3f(bx, by, bz)
      glTexCoord2f(ub, vb); glVertex3f(cx, cy, cz)
      glTexCoord2f(ua, vb); glVertex3f(dx, dy, dz)
    glBegin(GL_QUADS)
    // Corners run top-left, top-right, bottom-right, bottom-left as seen from outside.
    color(0.72f); quad(0f, 0f, -1f, (u + d) * k, top, (u + d + w) * k, bottom,
      x1, y1, z0, x0, y1, z0, x0, y0, z0, x1, y0, z0)
    color(0.90f); quad(0f, 0f, 1f, (u + 2 * d + w) * k, top, (u + 2 * d + 2 * w) * k, bottom,
      x0, y1, z1, x1, y1, z1, x1, y0, z1, x0, y0, z1)
    color(0.84f); quad(1f, 0f, 0f, u * k, top, (u + d) * k, bottom,
      x1, y1, z1, x1, y1, z0, x1, y0, z0, x1, y0, z1)
    color(0.78f); quad(-1f, 0f, 0f, (u + d + w) * k, top, (u + 2 * d + w) * k, bottom,
      x0, y1, z0, x0, y1, z1, x0, y0, z1, x0, y0, z0)
    color(1f)
    if from <= 0f then
      quad(0f, 1f, 0f, (u + d) * k, v * k, (u + d + w) * k, (v + d) * k,
        x1, y1, z1, x0, y1, z1, x0, y1, z0, x1, y1, z0)
    else
      val cu = (u + d + w * 0.5f) * k; val cv = top + 0.25f * k
      quad(0f, 1f, 0f, cu, cv, cu, cv, x1, y1, z1, x0, y1, z1, x0, y1, z0, x1, y1, z0)
    color(0.58f)
    if to >= 1f then
      quad(0f, -1f, 0f, (u + d + w) * k, v * k, (u + 2 * d + w) * k, (v + d) * k,
        x1, y0, z0, x0, y0, z0, x0, y0, z1, x1, y0, z1)
    else
      val cu = (u + d + w * 0.5f) * k; val cv = bottom - 0.25f * k
      quad(0f, -1f, 0f, cu, cv, cu, cv, x1, y0, z0, x0, y0, z0, x0, y0, z1, x1, y0, z1)
    glEnd()
