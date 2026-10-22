package blockbox

import org.lwjgl.opengl.GL11.*
import org.lwjgl.opengl.GL20.*
import scala.math.*

/** Two small analytic billboards: HDR solar radiance and a shaded, cratered moon. */
object BlockboxCelestials:
  private var program=0
  private var failed=false
  private var bodyUniform=0; private var alphaUniform=0; private var warmUniform=0
  private var phaseUniform=0; private var hdrUniform=0

  def render(phase: Float,daylight: Float,storm: Float,time: Float,hdr: Boolean): Boolean =
    if failed then return false
    if program==0 then
      try initialize()
      catch
        case e: Exception =>
          failed=true; System.err.println("Celestial shader unavailable: "+e.getMessage); return false
    val old=glGetInteger(GL_CURRENT_PROGRAM)
    glUseProgram(program)
    glUniform1f(hdrUniform,if hdr then 1f else 0f)
    // A new world starts with a full moon. Phases advance over eight world days.
    glUniform1f(phaseUniform,(time/BlockboxSky.dayNightCycleSeconds/8f*2f*Pi.toFloat))
    def billboard(direction: (Float,Float,Float),size: Float,body: Float,alpha: Float): Unit =
      if alpha>0.001f then
        val d=Vec3(direction._1,direction._2,direction._3)
        val r=Vec3(-d.z,0,d.x).normalized
        val u=Vec3(d.y*r.z-d.z*r.y,d.z*r.x-d.x*r.z,d.x*r.y-d.y*r.x)
        val center=d*76f
        glUniform1f(bodyUniform,body); glUniform1f(alphaUniform,alpha)
        glUniform1f(warmUniform,1f-BlockboxSky.smooth01(abs(d.y)/0.32f))
        glBegin(GL_QUADS)
        for (x,y) <- Seq((-1f,-1f),(1f,-1f),(1f,1f),(-1f,1f)) do
          val p=center+r*(x*size)+u*(y*size)
          glTexCoord2f(x,y); glVertex3f(p.x,p.y,p.z)
        glEnd()
    glEnable(GL_BLEND); glBlendFunc(GL_SRC_ALPHA,GL_ONE_MINUS_SRC_ALPHA)
    val sun=BlockboxSky.sunDirection(phase); val moon=BlockboxSky.moonDirection(phase)
    billboard(sun,11f,0f,BlockboxSky.smooth01((sun._2+0.09f)/0.20f)*(1f-storm*0.94f))
    billboard(moon,3.6f,1f,BlockboxSky.smooth01((moon._2+0.05f)/0.20f)*(1f-storm*0.95f)*(1f-daylight*0.65f))
    glUseProgram(old)
    true

  private def initialize(): Unit =
    def compile(kind: Int,source: String): Int =
      val id=glCreateShader(kind)
      glShaderSource(id,source); glCompileShader(id)
      if glGetShaderi(id,GL_COMPILE_STATUS)==GL_FALSE then
        val error=glGetShaderInfoLog(id); glDeleteShader(id); throw IllegalStateException(error)
      id
    var v=0; var f=0; var p=0
    try
      v=compile(GL_VERTEX_SHADER,"#version 120\nvarying vec2 uv;void main(){uv=gl_MultiTexCoord0.xy;gl_Position=ftransform();}")
      f=compile(GL_FRAGMENT_SHADER,fragment)
      p=glCreateProgram(); glAttachShader(p,v); glAttachShader(p,f); glLinkProgram(p)
      if glGetProgrami(p,GL_LINK_STATUS)==GL_FALSE then throw IllegalStateException(glGetProgramInfoLog(p))
      bodyUniform=glGetUniformLocation(p,"body"); alphaUniform=glGetUniformLocation(p,"opacity")
      warmUniform=glGetUniformLocation(p,"warmth"); phaseUniform=glGetUniformLocation(p,"phase")
      hdrUniform=glGetUniformLocation(p,"hdr"); program=p
    catch
      case e: Exception =>
        if p!=0 then glDeleteProgram(p)
        throw e
    finally
      if v!=0 then glDeleteShader(v)
      if f!=0 then glDeleteShader(f)

  def destroy(): Unit =
    if program!=0 then glDeleteProgram(program)
    program=0; failed=false

  private val fragment="""#version 120
    uniform float body, opacity, warmth, phase, hdr;
    varying vec2 uv;
    float hash(vec2 p){return fract(sin(dot(p,vec2(127.1,311.7)))*43758.5453);}
    float noise(vec2 p){vec2 i=floor(p),f=fract(p);f=f*f*(3.0-2.0*f);
      return mix(mix(hash(i),hash(i+vec2(1,0)),f.x),mix(hash(i+vec2(0,1)),hash(i+vec2(1)),f.x),f.y);}
    float crater(vec2 p,vec2 center,float radius){float d=length(p-center)/radius;
      return -0.16*exp(-d*d*3.8)+0.13*exp(-pow((d-0.85)*9.0,2.0));}
    void main(){
      float r=length(uv);
      if(body<0.5){
        float edge=max(fwidth(r),0.001);
        float disk=1.0-smoothstep(0.155-edge,0.155+edge,r);
        float halo=exp(-r*r*11.0)*0.30+exp(-r*8.0)*0.20;
        float alpha=max(disk,halo*(1.0-smoothstep(0.75,1.0,r)))*opacity;
        vec3 tint=mix(vec3(1.0,0.975,0.91),vec3(1.0,0.64,0.30),warmth*0.72);
        vec3 color=mix(tint,mix(vec3(1.0),tint,0.18+warmth*0.45)*mix(1.0,10.0,hdr),disk);
        gl_FragColor=vec4(color,alpha);
      }else{
        vec2 p=uv/0.62;
        float rr=dot(p,p); if(rr>1.03)discard;
        float edge=max(fwidth(rr),0.002);
        vec3 n=vec3(p,sqrt(max(0.0,1.0-rr)));
        vec3 light=normalize(vec3(sin(phase),0.12,cos(phase)));
        float diffuse=max(dot(n,light),0.0);
        float mare=smoothstep(0.38,0.67,noise(p*3.8+7.3))*0.17;
        float terrain=0.83-mare+(noise(p*27.0)-0.5)*0.06;
        terrain+=crater(p,vec2(-0.32,0.24),0.26)+crater(p,vec2(0.42,-0.34),0.19);
        terrain+=crater(p,vec2(0.22,0.47),0.13)+crater(p,vec2(-0.46,-0.43),0.12);
        float lit=0.035+pow(diffuse,0.62)*0.96;
        vec3 color=vec3(0.98,0.98,0.94)*terrain*lit;
        gl_FragColor=vec4(color,(1.0-smoothstep(1.0-edge,1.0+edge,rr))*opacity);
      }
    }
  """
