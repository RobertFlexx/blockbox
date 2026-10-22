package blockbox

import org.lwjgl.BufferUtils
import org.lwjgl.opengl.GL
import org.lwjgl.opengl.GL11.*
import org.lwjgl.opengl.GL20.*
import scala.math.*

/** Continuous sky color, evaluated per pixel rather than interpolated across dome rings.
  *
  * A vertical gradient carries the time of day. At sunrise and sunset a broad twilight
  * band hugs the horizon (orange toward the sun, mauve-pink opposite it) under a deeper
  * zenith, and forward scattering puts a soft corona around the sun all day.
  */
object BlockboxAtmosphere:
  /** @param glow strength of the corona around the sun (daylight or twilight, minus storm) */
  final case class Palette(top: Vec3, horizon: Vec3, below: Vec3, sun: Vec3, warmth: Float, glow: Float = 0f)
  private def lerp(a: Vec3,b: Vec3,t: Float): Vec3 = a+(b-a)*t.max(0f).min(1f)

  def palette(phase: Float,daylight: Float,rain: Float,cloud: Float,flash: Float): Palette =
    val lightning=flash.max(0f).min(1f)
    val day=BlockboxSky.smooth01((daylight+lightning*0.42f).min(1f))
    val storm=BlockboxSky.smooth01(rain.max(cloud*0.94f))
    val warm=(1f-BlockboxSky.smooth01(abs(sin(phase*Pi*2)).toFloat/0.28f))*(1f-storm*0.74f)
    val tint=lerp(Vec3(0.009f,0.014f,0.025f),Vec3(0.26f,0.29f,0.33f),day)+Vec3(lightning*0.30f,lightning*0.32f,lightning*0.38f)
    val top=lerp(lerp(Vec3(0.004f,0.008f,0.024f),Vec3(0.17f,0.43f,0.88f),day),tint*0.70f,storm*0.96f)
    val horizon=lerp(lerp(Vec3(0.028f,0.035f,0.070f),Vec3(0.74f,0.87f,0.99f),day),tint,storm*0.91f)
    val below=lerp(lerp(Vec3(0.010f,0.014f,0.026f),Vec3(0.46f,0.62f,0.80f),day),tint*0.86f,storm*0.88f)
    val sun=BlockboxSky.sunDirection(phase)
    Palette(lerp(top,Vec3(0.15f,0.22f,0.50f),warm*0.34f),lerp(horizon,Vec3(0.97f,0.64f,0.46f),warm*0.26f),below,
      Vec3(sun._1,sun._2,sun._3),warm,day.max(warm*0.9f)*(1f-storm*0.85f))

  /** CPU reference also serves the compatibility dome and regression tests. It must stay
    * identical to `skyFunction` below.
    */
  def color(direction: Vec3,p: Palette): Vec3 =
    val d=direction.normalized
    val base=if d.y>=0 then lerp(p.horizon,p.top,pow(BlockboxSky.smooth01(d.y),0.65).toFloat)
      else lerp(p.horizon,p.below,BlockboxSky.smooth01(-d.y/0.50f))
    val horizontal=Vec3(d.x+0.000001f,0,d.z+0.000001f).normalized
    val solar=Vec3(p.sun.x+0.000001f,0,p.sun.z+0.000001f).normalized
    val toward=(horizontal.x*solar.x+horizontal.z*solar.z)*0.5f+0.5f
    val toward2=toward*toward
    val band=if d.y>=0 then exp(-d.y*4.5f).toFloat else exp(d.y*9f).toFloat
    val warm=lerp(Vec3(0.80f,0.50f,0.62f),Vec3(1.0f,0.50f,0.20f),toward2)
    val banded=lerp(base,warm,p.warmth*band*(0.30f+0.62f*toward2))
    val mu=(d.x*p.sun.x+d.y*p.sun.y+d.z*p.sun.z).max(0f)
    val corona=(pow(mu,6).toFloat*0.16f+pow(mu,48).toFloat*0.30f)*p.glow
    val tint=lerp(Vec3(1.0f,0.97f,0.88f),Vec3(1.0f,0.62f,0.30f),p.warmth)
    Vec3(banded.x+tint.x*corona*(1f-banded.x),banded.y+tint.y*corona*(1f-banded.y),banded.z+tint.z*corona*(1f-banded.z))

  /** GLSL twin of [[color]]; shared by the sky and cloud shaders. */
  private[blockbox] val skyFunction = """
    uniform vec3 topColor, horizonColor, belowColor, sun;
    uniform float warmth, sunGlow;
    vec3 skyColor(vec3 d) {
      float upper=pow(smoothstep(0.0,1.0,max(d.y,0.0)),0.65);
      float lower=smoothstep(0.0,1.0,max(-d.y,0.0)/0.50);
      vec3 color=d.y>=0.0?mix(horizonColor,topColor,upper):mix(horizonColor,belowColor,lower);
      vec2 h=normalize(d.xz+vec2(0.000001));
      vec2 s=normalize(sun.xz+vec2(0.000001));
      float toward=dot(h,s)*0.5+0.5;
      float toward2=toward*toward;
      float band=d.y>=0.0?exp(-d.y*4.5):exp(d.y*9.0);
      vec3 warm=mix(vec3(0.80,0.50,0.62),vec3(1.0,0.50,0.20),toward2);
      color=mix(color,warm,clamp(warmth*band*(0.30+0.62*toward2),0.0,1.0));
      float mu=max(dot(d,sun),0.0);
      float corona=(pow(mu,6.0)*0.16+pow(mu,48.0)*0.30)*sunGlow;
      vec3 tint=mix(vec3(1.0,0.97,0.88),vec3(1.0,0.62,0.30),warmth);
      return color+tint*corona*(1.0-color);
    }
  """

  /** Uploads a palette to a program that includes [[skyFunction]]. */
  private[blockbox] def setPaletteUniforms(program: Int,p: Palette): Unit =
    def v(name: String,value: Vec3): Unit =
      val loc = glGetUniformLocation(program,name)
      if loc >= 0 then glUniform3f(loc,value.x,value.y,value.z)
    def f(name: String,value: Float): Unit =
      val loc = glGetUniformLocation(program,name)
      if loc >= 0 then glUniform1f(loc,value)
    v("topColor",p.top); v("horizonColor",p.horizon); v("belowColor",p.below); v("sun",p.sun)
    f("warmth",p.warmth)
    f("sunGlow",p.glow)

  private var program=0
  private var failed=false
  private var uniforms=Map.empty[String,Int]
  private val projection=BufferUtils.createFloatBuffer(16)

  def render(yaw: Float,pitch: Float,p: Palette,displayDither: Boolean,nightStars: Float=0f): Boolean =
    if failed || !GL.getCapabilities.OpenGL20 then return false
    if program==0 then
      try initialize()
      catch
        case e: Exception =>
          failed=true
          System.err.println("Sky shader unavailable; using smooth dome: "+e.getMessage)
          return false
    val previous=glGetInteger(GL_CURRENT_PROGRAM)
    glUseProgram(program)
    def v(name: String,value: Vec3): Unit = glUniform3f(uniforms(name),value.x,value.y,value.z)
    val yr=toRadians(yaw); val pr=toRadians(pitch)
    val cy=cos(yr).toFloat; val sy=sin(yr).toFloat; val cp=cos(pr).toFloat; val sp=sin(pr).toFloat
    v("right",Vec3(cy,0,sy)); v("up",Vec3(sy*sp,cp,-cy*sp)); v("forward",Vec3(sy*cp,-sp,-cy*cp))
    setPaletteUniforms(program,p)
    glUniform1f(uniforms("ditherAmount"),if displayDither then 1f else 0f)
    glUniform1f(uniforms("nightStars"),nightStars.max(0f).min(1f))
    projection.clear(); glGetFloatv(GL_PROJECTION_MATRIX,projection)
    glUniform2f(uniforms("halfFov"),1f/projection.get(0),1f/projection.get(5))
    glDisable(GL_BLEND)
    glBegin(GL_TRIANGLES); glVertex2f(-1,-1); glVertex2f(3,-1); glVertex2f(-1,3); glEnd()
    glUseProgram(previous); glEnable(GL_BLEND)
    true

  private def initialize(): Unit =
    program=buildProgram(vertexSource,fragmentSource)
    uniforms=Seq("right","up","forward","halfFov","ditherAmount","nightStars")
      .map(name => name -> glGetUniformLocation(program,name)).toMap

  /** Compiles and links a GLSL 1.20 program, throwing with the driver log on failure. */
  private[blockbox] def buildProgram(vertexSource: String,fragmentSource: String): Int =
    def compile(kind: Int,source: String): Int =
      val shader=glCreateShader(kind)
      glShaderSource(shader,source); glCompileShader(shader)
      if glGetShaderi(shader,GL_COMPILE_STATUS)==GL_FALSE then
        val reason=glGetShaderInfoLog(shader); glDeleteShader(shader)
        throw IllegalStateException(reason)
      shader
    var vertex=0; var fragment=0; var linked=0
    try
      vertex=compile(GL_VERTEX_SHADER,vertexSource); fragment=compile(GL_FRAGMENT_SHADER,fragmentSource)
      linked=glCreateProgram(); glAttachShader(linked,vertex); glAttachShader(linked,fragment); glLinkProgram(linked)
      if glGetProgrami(linked,GL_LINK_STATUS)==GL_FALSE then throw IllegalStateException(glGetProgramInfoLog(linked))
      linked
    catch
      case e: Exception =>
        if linked!=0 then glDeleteProgram(linked)
        throw e
    finally
      if vertex!=0 then glDeleteShader(vertex)
      if fragment!=0 then glDeleteShader(fragment)

  def destroy(): Unit =
    if program!=0 then glDeleteProgram(program)
    program=0; failed=false; uniforms=Map.empty

  private val vertexSource="""#version 120
    uniform vec3 right, up, forward;
    uniform vec2 halfFov;
    varying vec3 direction;
    void main() {
      gl_Position=vec4(gl_Vertex.xy,0.0,1.0);
      direction=forward+right*gl_Vertex.x*halfFov.x+up*gl_Vertex.y*halfFov.y;
    }
  """
  private val fragmentSource="""#version 120
    uniform float ditherAmount, nightStars;
    varying vec3 direction;
  """ + skyFunction + """
    void main() {
      vec3 d=normalize(direction);
      vec3 color=skyColor(d);
      if(nightStars>0.001) {
        float plane=dot(d,normalize(vec3(0.30,0.84,0.45)));
        float band=exp(-pow(plane/0.115,2.0));
        float structure=0.62+0.22*sin(d.x*25.0+d.z*13.0)+0.16*cos(d.z*37.0-d.y*17.0);
        float dust=exp(-pow((plane+0.018*sin(d.z*21.0))/0.029,2.0))*0.40;
        color+=vec3(0.023,0.026,0.038)*max(band*structure-dust,0.0)*nightStars*smoothstep(-0.08,0.20,d.y);
      }
      float noise=fract(52.9829189*fract(dot(gl_FragCoord.xy,vec2(0.06711056,0.00583715))))-0.5;
      gl_FragColor=vec4(clamp(color+noise*ditherAmount/255.0,0.0,1.0),1.0);
    }
  """
