package blockbox

import org.lwjgl.BufferUtils
import org.lwjgl.opengl.GL11.*
import org.lwjgl.opengl.GL12.{GL_CLAMP_TO_EDGE, GL_TEXTURE_MAX_LEVEL}
import org.lwjgl.opengl.GL30.glGenerateMipmap
import scala.collection.mutable

/** Procedural player skin in Minecraft's 64x64 skin layout, painted at twice the pixel
  * density. The shirt colour is baked in, so each shirt colour has one cached texture.
  *
  * Layout (skin pixels): head (0,0) and its hair layer (32,0), right leg (0,16),
  * body (16,16), right arm (40,16), left leg (16,48) and left arm (32,48).
  */
object BlockboxSkin:
  val Density = 2
  val Size: Int = 64 * Density

  enum Face:
    case Top, Bottom, Right, Front, Left, Back

  private type Rgb = (Int, Int, Int)
  private val cache = mutable.HashMap.empty[Int, Int]
  private var undeadTexture=0
  private var undeadHurtTexture=0
  private var white=0
  def whiteTexture: Int =
    if white==0 then
      white=glGenTextures(); glBindTexture(GL_TEXTURE_2D,white)
      val pixel=BufferUtils.createByteBuffer(4); pixel.putInt(-1).flip()
      glTexImage2D(GL_TEXTURE_2D,0,GL_RGBA8,1,1,0,GL_RGBA,GL_UNSIGNED_BYTE,pixel)
      glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_MIN_FILTER,GL_NEAREST)
      glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_MAG_FILTER,GL_NEAREST)
    white

  private val skinTone: Rgb = (222, 172, 128)
  private val hairTone: Rgb = (68, 44, 29)
  private val denimTone: Rgb = (54, 66, 98)
  private val leatherTone: Rgb = (78, 54, 38)

  private def hash(x: Int, y: Int, seed: Int): Float =
    var h = x * 374761393 + y * 668265263 + seed * 982451653
    h = (h ^ (h >>> 13)) * 1274126177
    ((h ^ (h >>> 16)) & 0xffffff) / 16777215f
  private def channel(v: Float): Int = math.round(v).max(0).min(255)
  private def shade(c: Rgb, k: Float): Rgb = (channel(c._1 * k), channel(c._2 * k), channel(c._3 * k))
  private def mix(a: Rgb, b: Rgb, t: Float): Rgb =
    (channel(a._1 + (b._1 - a._1) * t), channel(a._2 + (b._2 - a._2) * t), channel(a._3 + (b._3 - a._3) * t))
  private def argb(c: Rgb, alpha: Int = 255): Int = (alpha << 24) | (c._1 << 16) | (c._2 << 8) | c._3
  private val Clear = 0

  private def skin(x: Int, y: Int, seed: Int, k: Float = 1f): Int =
    argb(shade(skinTone, k * (0.988f + hash(x, y, seed) * 0.024f)))
  /** Combed strands: a per-column streak, occasional light strands and dark roots. */
  private def hair(x: Int, y: Int, seed: Int, k: Float = 1f): Int =
    val streak = hash(x, 0, seed)
    val n = hash(x, y, seed + 1)
    val lift = if n > 0.90f then 0.16f else if n < 0.08f then -0.12f else 0f
    argb(shade(hairTone, k * (0.84f + streak * 0.24f + lift)))
  private def fabric(c: Rgb, x: Int, y: Int, seed: Int, k: Float): Int =
    argb(shade(c, k * (0.975f + hash(x, y, seed) * 0.05f)))
  /** Denim twill: fine diagonal ribs under a little noise. */
  private def denim(x: Int, y: Int, seed: Int, k: Float): Int =
    val twill = if Math.floorMod(x + y, 3) == 0 then 0.955f else 1f
    argb(shade(denimTone, k * twill * (0.97f + hash(x, y, seed) * 0.06f)))

  def pixels(shirt: (Float, Float, Float)): Array[Int] =
    val cloth: Rgb = (channel(shirt._1 * 255f), channel(shirt._2 * 255f), channel(shirt._3 * 255f))
    val img = new Array[Int](Size * Size)
    box(img, 0, 0, 8, 8, 8, mirror = false)(headTexel)
    box(img, 32, 0, 8, 8, 8, mirror = false)(hairLayerTexel)
    box(img, 16, 16, 8, 12, 4, mirror = false)(bodyTexel(cloth))
    box(img, 40, 16, 4, 12, 4, mirror = false)(armTexel(cloth))
    box(img, 32, 48, 4, 12, 4, mirror = true)(armTexel(cloth))
    box(img, 0, 16, 4, 12, 4, mirror = false)(legTexel)
    box(img, 16, 48, 4, 12, 4, mirror = true)(legTexel)
    img

  /** Paints every texel of a box at (u, v) sized (w, h, d) skin pixels. The painter gets
    * face-local texels as seen from outside that face. Mirrored boxes swap the outer and
    * inner sides, so left limbs reuse the right limb painters.
    */
  private def box(img: Array[Int], u: Int, v: Int, w: Int, h: Int, d: Int, mirror: Boolean)
                 (paint: (Face, Int, Int, Int, Int) => Int): Unit =
    def rect(face: Face, ru: Int, rv: Int, rw: Int, rh: Int): Unit =
      val fw = rw * Density; val fh = rh * Density
      var ty = 0
      while ty < fh do
        var tx = 0
        while tx < fw do
          val lx = if mirror then fw - 1 - tx else tx
          img((rv * Density + ty) * Size + ru * Density + tx) = paint(face, lx, ty, fw, fh)
          tx += 1
        ty += 1
    rect(Face.Top, u + d, v, w, d)
    rect(Face.Bottom, u + d + w, v, w, d)
    rect(if mirror then Face.Left else Face.Right, u, v + d, d, h)
    rect(Face.Front, u + d, v + d, w, h)
    rect(if mirror then Face.Right else Face.Left, u + d + w, v + d, d, h)
    rect(Face.Back, u + 2 * d + w, v + d, w, h)

  /** Distance from the front edge of a side face (0 at the front). */
  private def fromFront(face: Face, x: Int, w: Int): Int = if face == Face.Right then w - 1 - x else x

  private def headTexel(face: Face, x: Int, y: Int, w: Int, h: Int): Int = face match
    case Face.Top => hair(x, y, 11)
    case Face.Bottom => skin(x, y, 12, 0.74f)
    case Face.Front => faceTexel(x, y)
    case Face.Back =>
      val hairline = 12 + (if hash(x, 0, 13) > 0.55f then 1 else 0)
      if y < hairline then hair(x, y, 14, if y >= hairline - 1 then 0.86f else 1f)
      else skin(x, y, 15, if y == h - 1 then 0.80f else 0.88f)
    case _ =>
      val f = fromFront(face, x, w)
      val hairy = y <= 3 || (y <= 6 && f >= 4) || (y <= 9 && f >= 8) || (y <= 12 && f >= 11) ||
        (f >= 2 && f <= 3 && y <= 9)
      val ear = f >= 5 && f <= 7 && y >= 7 && y <= 10
      if ear then skin(x, y, 16, if f == 6 && (y == 8 || y == 9) then 0.80f else 0.94f)
      else if hairy then hair(x, y, 17, if y >= 9 then 0.9f else 1f)
      else skin(x, y, 18, (if f <= 1 then 0.96f else 0.92f) * (if y == h - 1 then 0.86f else 1f))

  private val eyeWhite: Rgb = (241, 238, 230)
  private val irisLight: Rgb = (96, 140, 176)
  private val iris: Rgb = (62, 98, 142)
  private val pupil: Rgb = (26, 30, 42)

  /** 16x16 face: side-parted fringe, brows, eyes looking forward, nose, mouth and chin. */
  private def faceTexel(x: Int, y: Int): Int =
    val fringe = if x <= 5 then 3 else if x <= 10 then 2 else if x <= 12 then 3 else 4
    if y < fringe || (y == fringe && hash(x, y, 22) > 0.6f) then return hair(x, y, 23, if y == fringe - 1 then 0.9f else 1f)
    if (x == 0 || x == 15) && y <= 8 then return hair(x, y, 24, 0.86f)
    val leftEye = x >= 3 && x <= 6
    val rightEye = x >= 9 && x <= 12
    if y == 6 && (leftEye || rightEye) then return argb(shade(hairTone, if x == 3 || x == 12 then 0.95f else 0.80f))
    if (y == 7 || y == 8) && (leftEye || rightEye) then
      // Whites outside, iris towards the nose, so the gaze reads as straight ahead.
      val outer = if leftEye then x - 3 else 12 - x
      val c =
        if outer <= 1 then if y == 8 && outer == 0 then shade(eyeWhite, 0.92f) else eyeWhite
        else if y == 7 then if outer == 2 then irisLight else iris
        else if outer == 2 then iris else pupil
      return argb(c)
    if y == 9 && (leftEye || rightEye) then return skin(x, y, 25, 0.95f)
    // Nose: lit bridge, darker sides and a soft shadow underneath.
    if y == 9 && (x == 7 || x == 8) then return skin(x, y, 26, 1.03f)
    if y == 10 && (x == 6 || x == 9) then return skin(x, y, 27, 0.94f)
    if y == 11 && (x == 7 || x == 8) then return skin(x, y, 28, 0.84f)
    if y == 11 && (x == 6 || x == 9) then return skin(x, y, 29, 0.93f)
    if (y == 10 || y == 11) && ((x >= 2 && x <= 4) || (x >= 11 && x <= 13)) then
      return argb(mix(shade(skinTone, 0.99f), (226, 146, 120), 0.28f))
    if y == 12 && x >= 6 && x <= 9 then return argb((150, 88, 72))
    if y == 12 && (x == 5 || x == 10) then return skin(x, y, 30, 0.90f)
    if y == 13 && x >= 6 && x <= 9 then return argb((196, 128, 106))
    val edge = if x == 1 || x == 14 then 0.95f else 1f
    skin(x, y, 31, edge * (if y == 15 then 0.88f else if y == 14 then 0.97f else 1f))

  /** Outer hair layer: volume on the crown, a ragged fringe and a fuller back. */
  private def hairLayerTexel(face: Face, x: Int, y: Int, w: Int, h: Int): Int = face match
    case Face.Top => hair(x, y, 41, 1.06f)
    case Face.Bottom => Clear
    case Face.Front =>
      val edge = if x >= 5 && x <= 8 then 1 else 2 + (if hash(x, 0, 42) > 0.45f then 1 else 0)
      if y < edge then hair(x, y, 43, 1.04f) else Clear
    case Face.Back =>
      val edge = 10 + (hash(x, 0, 44) * 3f).toInt
      if y < edge then hair(x, y, 45, if y >= edge - 1 then 0.88f else 1.02f) else Clear
    case _ =>
      val f = fromFront(face, x, w)
      val edge = if f <= 3 then 3 else if f <= 7 then 5 else if f <= 10 then 8 else 10
      if y < edge + (if hash(f, 0, 46) > 0.6f then 1 else 0) then hair(x, y, 47, 1.02f) else Clear

  private def bodyTexel(cloth: Rgb)(face: Face, x: Int, y: Int, w: Int, h: Int): Int =
    val beltTop = h - 3
    face match
      case Face.Top =>
        val neck = x >= 5 && x <= w - 6 && y >= 2 && y <= h - 3
        if neck then skin(x, y, 51, 0.82f) else fabric(cloth, x, y, 52, 0.97f)
      case Face.Bottom => denim(x, y, 53, 0.80f)
      case _ if y >= beltTop => beltTexel(face, x, y - beltTop, w)
      case _ =>
        val sideShade = if x == 0 || x == w - 1 then 0.88f else if x == 1 || x == w - 2 then 0.95f else 1f
        val hem = if y == beltTop - 1 then 0.84f else if y == beltTop - 2 then 0.95f else 1f
        face match
          case Face.Front =>
            if y <= 1 && x >= 6 && x <= 9 then return skin(x, y, 54, if y == 0 then 0.86f else 0.92f)
            val collar = (y <= 1 && (x == 5 || x == 10)) || (y == 2 && x >= 5 && x <= 10)
            if collar then return fabric(cloth, x, y, 55, 0.78f)
            val pocketTop = y == 6 && x >= 10 && x <= 13
            val pocketSide = (x == 10 || x == 13) && y >= 7 && y <= 9
            // Soft drape: the chest catches light, the waist above the belt falls into shade.
            val drape = if y >= 4 && y <= 8 then 1.02f else if y >= 15 then 0.97f else 1f
            val k = (if pocketTop then 0.84f else if pocketSide then 0.92f else 1f) * sideShade * hem * drape
            fabric(cloth, x, y, 57, k)
          case Face.Back =>
            val yoke = if y == 4 then 0.90f else 1f
            val spine = if x == 7 || x == 8 then 0.97f else 1f
            fabric(cloth, x, y, 58, yoke * spine * sideShade * hem * 0.97f)
          case _ =>
            val seam = if x == 3 || x == 4 then 0.92f else 1f
            fabric(cloth, x, y, 59, seam * hem * 0.95f)

  private def beltTexel(face: Face, x: Int, row: Int, w: Int): Int =
    val leather = row match
      case 0 => (102, 70, 44)
      case 1 => (80, 53, 34)
      case _ => (58, 39, 26)
    if face == Face.Front && x >= 6 && x <= 9 then
      val frame = x == 6 || x == 9 || row != 1
      argb(if frame then (if row == 0 then (226, 192, 104) else (198, 162, 80)) else (88, 66, 36))
    else if face == Face.Front && (x == 2 || x == 13) then argb(shade(leather, 0.80f))
    else argb(shade(leather, 0.96f + hash(x, row, 61) * 0.08f))

  private def armTexel(cloth: Rgb)(face: Face, x: Int, y: Int, w: Int, h: Int): Int = face match
    case Face.Top => fabric(cloth, x, y, 71, 0.98f)
    // The fist end is plain skin, like Minecraft's: first person looks straight at it.
    case Face.Bottom => skin(x, y, 72, 0.97f)
    case _ =>
      val sideK = face match
        case Face.Front => 1.02f
        case Face.Back => 0.90f
        case Face.Right => 0.96f
        case _ => 0.99f
      if y <= 7 then
        val k = if y == 7 then 0.80f else if y == 6 then 0.93f else 1f
        fabric(cloth, x, y, 73 + face.ordinal, k * (if face == Face.Back then 0.95f else 1f))
      else if y == 8 then skin(x, y, 79, sideK * 0.82f)
      else
        // Lit forearm with a soft highlight along the middle of each face.
        val ridge = if x == w / 2 || x == w / 2 - 1 then 1.02f else if x == 0 || x == w - 1 then 0.96f else 1f
        skin(x, y, 80 + face.ordinal, sideK * ridge)

  private def legTexel(face: Face, x: Int, y: Int, w: Int, h: Int): Int = face match
    case Face.Top => denim(x, y, 101, 0.92f)
    case Face.Bottom =>
      val tread = y % 3 == 1 && x >= 1 && x <= w - 2
      argb(shade((40, 34, 30), if tread then 0.74f else 1f))
    case _ if y >= 19 => shoeTexel(face, x, y - 19, w)
    case _ =>
      val cuff = if y == 18 then 0.74f else if y == 17 then 1.10f else if y == 16 then 1.04f else 1f
      val seam = face == Face.Right && (x == 3 || x == 4)
      val stitch = if seam then (if y % 2 == 0 then 1.12f else 0.92f) else 1f
      val knee = if face == Face.Front && y >= 10 && y <= 12 then 1.05f else 1f
      val crease = if face == Face.Front && (x == 3 || x == 4) && y > 1 && y < 16 then 1.03f else 1f
      val sideK = face match
        case Face.Back => 0.88f
        case Face.Left => 0.92f
        case _ => 1f
      denim(x, y, 102 + face.ordinal, cuff * stitch * knee * crease * sideK)

  private def shoeTexel(face: Face, x: Int, row: Int, w: Int): Int =
    if row == 4 then argb(shade((44, 38, 33), 0.96f + hash(x, row, 111) * 0.08f))
    else if face == Face.Front && row <= 2 && (x == 3 || x == 4) then argb(if (row + x) % 2 == 0 then (142, 132, 118) else (112, 102, 90))
    else
      val k = (if row == 0 then 0.88f else if row == 3 && face == Face.Front then 1.10f else 1f) *
        (if face == Face.Back then 0.88f else 1f)
      argb(shade(leatherTone, k * (0.96f + hash(x, row, 112) * 0.08f)))

  /** GL texture for a shirt colour, generated once and cached. */
  def texture(shirt: (Float, Float, Float)): Int =
    val key = (channel(shirt._1 * 255f) << 16) | (channel(shirt._2 * 255f) << 8) | channel(shirt._3 * 255f)
    cache.getOrElseUpdate(key, upload(pixels(shirt)))

  def zombiePixels: Array[Int] =
    val img=pixels((0.17f,0.40f,0.39f))
    def rot(x: Int,y: Int): Int =
      val patch=if hash(x/3,y/3,815)>0.72f then 0.76f else 1f
      argb(shade((91,132,59),patch*(0.88f+hash(x,y,816)*0.20f)))
    box(img,0,0,8,8,8,false) { (face,x,y,w,h) =>
      if face==Face.Front && y>=6 && y<=8 && ((x>=2 && x<=5)||(x>=10 && x<=13)) then
        argb(if y==6 then (28,41,20) else if x==4 || x==11 then (39,46,22) else (147,150,83))
      else if face==Face.Front && y>=12 && y<=13 && x>=5 && x<=10 then argb((39,55,26))
      else rot(x,y)
    }
    box(img,32,0,8,8,8,false)((_,_,_,_,_) => Clear)
    for (u,v,mirror) <- Seq((40,16,false),(32,48,true)) do
      box(img,u,v,4,12,4,mirror) { (face,x,y,w,h) =>
        val tear=7+(hash(x/2,0,820)*5).toInt
        if y>=tear || (y>=4 && x>=2 && x<=4 && hash(x/2,y/3,821)>0.55f) then rot(x,y)
        else fabric((44,100,95),x,y,822,0.80f)
      }
    box(img,16,16,8,12,4,false) { (face,x,y,w,h) =>
      val tear=y>h-5 && hash(x/3,0,823)>0.45f || x>=5 && x<=8 && y>=8 && y<=12
      if tear then rot(x,y) else fabric((44,100,95),x,y,824,if y>h-6 then 0.70f else 0.94f)
    }
    for (u,v,mirror) <- Seq((0,16,false),(16,48,true)) do
      box(img,u,v,4,12,4,mirror) { (face,x,y,w,h) =>
        if y>h-3 || (y>=10 && y<=15 && x>=2 && x<=4) then rot(x,y)
        else fabric((65,62,92),x,y,825,0.86f+hash(x/3,y/3,826)*0.14f)
      }
    img

  def zombieTexture: Int =
    if undeadTexture==0 then undeadTexture=upload(zombiePixels)
    undeadTexture

  private def redden(img: Array[Int]): Array[Int] = img.map { p =>
    val r=((p>>>16)&255); val g=((p>>>8)&255); val b=p&255
    (p&0xff000000) | (((r*0.32f+255*0.68f).toInt)<<16) | (((g*0.27f).toInt)<<8) | (b*0.27f).toInt
  }
  def hurtTexture(shirt: (Float,Float,Float),zombie: Boolean): Int =
    if zombie then
      if undeadHurtTexture==0 then undeadHurtTexture=upload(redden(zombiePixels))
      undeadHurtTexture
    else
      val key=0x1000000 | (channel(shirt._1*255)<<16) | (channel(shirt._2*255)<<8) | channel(shirt._3*255)
      cache.getOrElseUpdate(key,upload(redden(pixels(shirt))))

  private def upload(img: Array[Int]): Int =
    val buffer = BufferUtils.createByteBuffer(Size * Size * 4)
    var i = 0
    while i < img.length do
      val p = img(i)
      buffer.put(((p >> 16) & 255).toByte).put(((p >> 8) & 255).toByte).put((p & 255).toByte).put(((p >>> 24) & 255).toByte)
      i += 1
    buffer.flip()
    val id = glGenTextures()
    glBindTexture(GL_TEXTURE_2D, id)
    glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA8, Size, Size, 0, GL_RGBA, GL_UNSIGNED_BYTE, buffer)
    // A short mip chain keeps distant players from shimmering without smearing the face.
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAX_LEVEL, 2)
    glGenerateMipmap(GL_TEXTURE_2D)
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST_MIPMAP_LINEAR)
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST)
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE)
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE)
    glBindTexture(GL_TEXTURE_2D, 0)
    id

  def dispose(): Unit =
    if white!=0 then glDeleteTextures(white)
    white=0
    cache.valuesIterator.foreach(glDeleteTextures)
    cache.clear()
    if undeadTexture!=0 then glDeleteTextures(undeadTexture)
    undeadTexture=0
    if undeadHurtTexture!=0 then glDeleteTextures(undeadHurtTexture)
    undeadHurtTexture=0
