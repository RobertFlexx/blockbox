package blockbox

import scala.collection.mutable.ArrayBuffer

/** Five small cached meshes shared by chunk rendering, selection and the held item. */
object TorchModel:
  /** `flame` marks emissive vertices: the ember at the top of the stick and the flame. */
  final case class Vertex(x: Float, y: Float, z: Float, nx: Float, ny: Float, nz: Float, u: Float, v: Float, flame: Boolean)
  final case class Bounds(x0: Float, y0: Float, z0: Float, x1: Float, y1: Float, z1: Float)
  private final case class Built(vertices: Array[Vertex], bounds: Bounds, flame: (Float, Float, Float))
  private val built = TorchMount.values.map(build)
  def vertices(mount: TorchMount): Array[Vertex] = built(mount.id).vertices
  def bounds(mount: TorchMount): Bounds = built(mount.id).bounds
  /** Centre of the flame inside the block, for glow and flame/smoke particles. */
  def flameCenter(mount: TorchMount): (Float, Float, Float) = built(mount.id).flame

  private def build(mount: TorchMount): Built =
    val result = ArrayBuffer.empty[Vertex]
    val wall = mount != TorchMount.Floor
    val sx = mount.support._1.toFloat; val sz = mount.support._3.toFloat
    val tilt = if wall then 0.43f else 0f
    val sine = math.sin(tilt).toFloat; val cosine = math.cos(tilt).toFloat
    // Rotate around the foot: wall torches lean away from their own support.
    def transform(x: Float, y: Float, z: Float): (Float, Float, Float) =
      val along = x * sx + z * sz
      val delta = along * (cosine - 1f) - y * sine
      (0.5f + sx * (if wall then 0.43f else 0f) + x + sx * delta,
        (if wall then 0.20f else 0f) + y * cosine + along * sine,
        0.5f + sz * (if wall then 0.43f else 0f) + z + sz * delta)
    def quad(points: Array[(Float,Float,Float)], uv: Array[(Float,Float)], flame: Boolean = false, transformed: Boolean = true): Unit =
      val p = if transformed then points.map(t => transform(t._1,t._2,t._3)) else points
      val a = Vec3(p(1)._1-p(0)._1,p(1)._2-p(0)._2,p(1)._3-p(0)._3)
      val b = Vec3(p(2)._1-p(0)._1,p(2)._2-p(0)._2,p(2)._3-p(0)._3)
      val n = Vec3(a.y*b.z-a.z*b.y,a.z*b.x-a.x*b.z,a.x*b.y-a.y*b.x).normalized
      for i <- Array(0,1,2,2,3,0) do
        result += Vertex(p(i)._1,p(i)._2,p(i)._3,n.x,n.y,n.z,uv(i)._1,uv(i)._2,flame)
    // A 2x10 pixel stick like Minecraft's: wood below, and the top two texel rows are the
    // glowing ember the flame burns from, so they are emissive like the flame itself.
    val w = 0.0625f; val top = 0.625f; val ember = 0.5f
    def side(y0: Float, y1: Float, v0: Float, v1: Float, glow: Boolean): Unit =
      val uv = Array((6.5f/16f,v0/16f),(9.5f/16f,v0/16f),(9.5f/16f,v1/16f),(6.5f/16f,v1/16f))
      quad(Array((-w,y0,w),(w,y0,w),(w,y1,w),(-w,y1,w)),uv,glow)
      quad(Array((w,y0,-w),(-w,y0,-w),(-w,y1,-w),(w,y1,-w)),uv,glow)
      quad(Array((-w,y0,-w),(-w,y0,w),(-w,y1,w),(-w,y1,-w)),uv,glow)
      quad(Array((w,y0,w),(w,y0,-w),(w,y1,-w),(w,y1,w)),uv,glow)
    side(0f,ember,15.5f,7.5f,glow = false)
    side(ember,top,7.5f,5.5f,glow = true)
    val capUv = Array((6.5f/16f,6.5f/16f),(9.5f/16f,6.5f/16f),(9.5f/16f,5.5f/16f),(6.5f/16f,5.5f/16f))
    val footUv = Array((6.5f/16f,15.5f/16f),(9.5f/16f,15.5f/16f),(9.5f/16f,14.5f/16f),(6.5f/16f,14.5f/16f))
    quad(Array((-w,top,w),(w,top,w),(w,top,-w),(-w,top,-w)),capUv,true)
    quad(Array((-w,0f,-w),(w,0f,-w),(w,0f,w),(-w,0f,w)),footUv)
    val shaft = result.toArray
    // Flame stays upright even on an angled shaft, with two crossed, two-sided planes.
    val tip = transform(0f,top-0.04f,0f)
    val fw = 0.135f; val low = tip._2-0.045f; val high = tip._2+0.24f
    val flameUv = Array((4.5f/16f,4.5f/16f),(11.5f/16f,4.5f/16f),(11.5f/16f,0.5f/16f),(4.5f/16f,0.5f/16f))
    for sign <- Array(-1f,1f) do
      val p = Array((tip._1-fw,low,tip._3-fw*sign),(tip._1+fw,low,tip._3+fw*sign),
        (tip._1+fw,high,tip._3+fw*sign),(tip._1-fw,high,tip._3-fw*sign))
      quad(p,flameUv,true,false)
      quad(p.reverse,flameUv.reverse,true,false)
    val bounds = Bounds(shaft.map(_.x).min, shaft.map(_.y).min, shaft.map(_.z).min,
      shaft.map(_.x).max, (shaft.map(_.y).max + 0.16f).min(1f), shaft.map(_.z).max)
    Built(result.toArray, bounds, (tip._1, low + (high - low) * 0.42f, tip._3))
