package blockbox

import org.lwjgl.opengl.{GL,GL20}
import org.lwjgl.opengl.GL11.*

/** The pack-free renderer's lightmap and linear fog, with air excluded from the
  * optical path. This preserves alpha-tested leaves seen through the surface.
  */
object UnderwaterFog:
  private var program=0
  private var previous=0
  private var active=false
  private var failed=false

  def begin(eyeY: Float,surfaceY: Float): Unit =
    if active || failed || !eyeY.isFinite || !surfaceY.isFinite || !GL.getCapabilities.OpenGL20 then return
    try
      if program==0 then program=BlockboxAtmosphere.buildProgram(vertex,fragment)
      previous=glGetInteger(GL20.GL_CURRENT_PROGRAM)
      GL20.glUseProgram(program)
      GL20.glUniform1i(GL20.glGetUniformLocation(program,"atlas"),0)
      GL20.glUniform1i(GL20.glGetUniformLocation(program,"lightmap"),1)
      GL20.glUniform2f(GL20.glGetUniformLocation(program,"waterHeights"),eyeY,surfaceY)
      active=true
    catch
      case e: Exception =>
        GL20.glUseProgram(previous)
        failed=true
        System.err.println("Underwater fog uses compatibility fallback: "+e.getMessage)

  def end(): Unit =
    if active then
      GL20.glUseProgram(previous)
      active=false

  def destroy(): Unit =
    end()
    if program!=0 then GL20.glDeleteProgram(program)
    program=0; failed=false

  private val vertex="""#version 120
    uniform vec2 waterHeights;
    varying vec2 uv,lightUv;
    varying vec4 tint;
    varying float wetDistance;
    void main(){
      vec4 eye=gl_ModelViewMatrix*gl_Vertex;
      gl_Position=gl_ProjectionMatrix*eye;
      gl_ClipVertex=eye;
      uv=gl_MultiTexCoord0.xy;
      lightUv=clamp(gl_MultiTexCoord1.xy,vec2(0.0),vec2(240.0))/256.0+0.03125;
      tint=gl_Color;
      wetDistance=length(eye.xyz);
      if(gl_Vertex.y>waterHeights.y)
        wetDistance*=clamp((waterHeights.y-waterHeights.x)/max(gl_Vertex.y-waterHeights.x,0.001),0.0,1.0);
    }
  """
  private val fragment="""#version 120
    uniform sampler2D atlas,lightmap;
    varying vec2 uv,lightUv;
    varying vec4 tint;
    varying float wetDistance;
    void main(){
      vec4 texel=texture2D(atlas,uv);
      if(texel.a<=0.01) discard;
      vec3 color=texel.rgb*min(tint.rgb,vec3(1.0))*texture2D(lightmap,lightUv).rgb;
      float fog=clamp((gl_Fog.end-wetDistance)/max(gl_Fog.end-gl_Fog.start,0.001),0.0,1.0);
      gl_FragColor=vec4(mix(gl_Fog.color.rgb,color,fog),texel.a*tint.a);
    }
  """
