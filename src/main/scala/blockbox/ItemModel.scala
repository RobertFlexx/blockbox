package blockbox

import org.lwjgl.opengl.GL11.*
import scala.collection.mutable

/** Original pixel silhouettes extruded into solid, bevel-shaded tools. The inventory
  * and both player views use the same art; no camera-facing billboard is used in 3D. */
object ItemModel:
  final case class Vertex(x: Float,y: Float,z: Float,nx: Float,ny: Float,nz: Float,r: Float,g: Float,b: Float)
  private val artCache=mutable.HashMap.empty[Block,Array[Int]]
  private val meshCache=mutable.HashMap.empty[Block,Vector[Vertex]]
  // Mesh vertices are relative to sprite grip (4.5,11.5). Display transforms
  // operate about sprite center (8,8); apply this AFTER model scale/rotation.
  val centerFromGrip=Vec3(-3.5f/16,-3.5f/16,0)

  def pixels(item: Block): Array[Int] = artCache.getOrElseUpdate(item,paint(item))
  private def paint(item: Block): Array[Int] =
    val mask=new Array[Int](256)
    def line(x0: Int,y0: Int,x1: Int,y1: Int,width: Float,material: Int): Unit =
      for y <- 0 until 16; x <- 0 until 16 do
        val dx=x1-x0; val dy=y1-y0
        val t=(((x-x0)*dx+(y-y0)*dy).toFloat/(dx*dx+dy*dy).max(1)).max(0f).min(1f)
        val ox=x-(x0+t*dx); val oy=y-(y0+t*dy)
        if ox*ox+oy*oy<=width*width then mask(y*16+x)=material
    def polygon(points: (Int,Int)*)(material: Int): Unit =
      for y <- 0 until 16; x <- 0 until 16 do
        var inside=false; var j=points.length-1
        for i <- points.indices do
          val (ax,ay)=points(i); val (bx,by)=points(j)
          if (ay>y)!=(by>y) && x<(bx-ax).toFloat*(y-ay)/(by-ay)+ax then inside = !inside
          j=i
        if inside then mask(y*16+x)=material
    ToolSystem.spec(item) match
      case Some(tool) =>
        line(2,14,10,6,0.95f,1)
        tool.kind match
          case ToolSystem.Kind.Pickaxe =>
            polygon((2,3),(5,1),(11,2),(15,6),(15,9),(12,6),(9,5),(4,4))(2)
            line(9,4,10,6,0.9f,2)
          case ToolSystem.Kind.Axe =>
            polygon((7,3),(10,1),(14,3),(15,7),(12,10),(9,8),(8,5),(6,5))(2)
          case ToolSystem.Kind.Shovel =>
            polygon((8,4),(10,1),(13,1),(15,4),(14,7),(11,9),(8,6))(2)
          case ToolSystem.Kind.Sword =>
            polygon((6,9),(12,2),(15,0),(15,4),(8,11))(2)
            line(4,8,9,13,0.9f,3)
            line(1,14,3,15,0.8f,3)
      case None => item match
        case Block.Stick => line(2,14,12,3,1f,1)
        case Block.CoalLump => polygon((2,6),(6,2),(11,2),(14,6),(13,12),(9,14),(4,13),(1,10))(2)
        case Block.DiamondGem => polygon((1,5),(4,2),(12,2),(15,5),(12,10),(8,15),(4,11))(2)
        case _ => polygon((1,7),(5,3),(12,3),(15,6),(14,10),(10,13),(2,12))(2)
    def occupied(x: Int,y: Int): Boolean = x>=0 && x<16 && y>=0 && y<16 && mask(y*16+x)!=0
    mask.indices.map { i =>
      val material=mask(i); val x=i%16; val y=i/16
      if material==0 then 0
      else
        val edge = !occupied(x-1,y) || !occupied(x+1,y) || !occupied(x,y-1) || !occupied(x,y+1)
        val highlight = !occupied(x,y-1) || !occupied(x-1,y)
        val base = if material==1 then (0.48f,0.30f,0.13f) else if material==3 then (0.36f,0.27f,0.14f) else item.rgb
        val shade = if edge then (if highlight then 0.72f else 0.42f) else if (x+y)%5==0 then 1.10f else 0.94f+(x%3)*0.035f
        val color = if item==Block.DiamondGem && y<6 && !edge then (0.66f,1f,0.96f) else base
        0xff000000 | ((color._1*shade*255).toInt.min(255)<<16) | ((color._2*shade*255).toInt.min(255)<<8) | (color._3*shade*255).toInt.min(255)
    }.toArray

  def mesh(item: Block): Vector[Vertex] = meshCache.getOrElseUpdate(item,{
    val art=pixels(item); val out=Vector.newBuilder[Vertex]; val unit=1f/16f; val half=0.032f
    def solid(x: Int,y: Int): Boolean = x>=0 && x<16 && y>=0 && y<16 && art(y*16+x)!=0
    for y <- 0 until 16; x <- 0 until 16 if solid(x,y) do
      val pixel=art(y*16+x)
      val r=((pixel>>16)&255)/255f; val g=((pixel>>8)&255)/255f; val b=(pixel&255)/255f
      val x0=(x-4.5f)*unit; val x1=x0+unit; val y1=(11.5f-y)*unit; val y0=y1-unit
      def face(nx: Float,ny: Float,nz: Float,points: (Float,Float,Float)*): Unit =
        points.foreach { (a,b0,c) => out+=Vertex(a,b0,c,nx,ny,nz,r,g,b) }
      face(0,0,1,(x0,y0,half),(x1,y0,half),(x1,y1,half),(x0,y1,half))
      face(0,0,-1,(x1,y0,-half),(x0,y0,-half),(x0,y1,-half),(x1,y1,-half))
      if !solid(x-1,y) then face(-1,0,0,(x0,y0,-half),(x0,y0,half),(x0,y1,half),(x0,y1,-half))
      if !solid(x+1,y) then face(1,0,0,(x1,y0,half),(x1,y0,-half),(x1,y1,-half),(x1,y1,half))
      if !solid(x,y-1) then face(0,1,0,(x0,y1,half),(x1,y1,half),(x1,y1,-half),(x0,y1,-half))
      if !solid(x,y+1) then face(0,-1,0,(x0,y0,-half),(x1,y0,-half),(x1,y0,half),(x0,y0,half))
    out.result()
  })

  def render(item: Block,light: (Float,Float,Float)): Unit =
    glDisable(GL_TEXTURE_2D)
    val lit=glIsEnabled(GL_LIGHTING)
    glBegin(GL_QUADS)
    mesh(item).foreach { v =>
      val shade=if lit then 1f else if v.ny>0 then 1f else if v.ny<0 then 0.55f else if v.nz!=0 then 0.92f else 0.72f
      glColor3f(v.r*light._1*shade,v.g*light._2*shade,v.b*light._3*shade)
      glNormal3f(v.nx,v.ny,v.nz); glVertex3f(v.x,v.y,v.z)
    }
    glEnd(); glColor4f(1,1,1,1)

  def icon(item: Block,x: Float,y: Float,size: Float): Unit =
    glDisable(GL_TEXTURE_2D)
    glBegin(GL_QUADS)
    val unit=size/16f
    val art=pixels(item)
    for py <- 0 until 16; px <- 0 until 16 if art(py*16+px)!=0 do
      val pixel=art(py*16+px)
      glColor3f(((pixel>>16)&255)/255f,((pixel>>8)&255)/255f,(pixel&255)/255f)
      val x0=x+px*unit; val y0=y+py*unit
      glVertex2f(x0,y0); glVertex2f(x0+unit,y0); glVertex2f(x0+unit,y0+unit); glVertex2f(x0,y0+unit)
    glEnd(); glColor4f(1,1,1,1)
