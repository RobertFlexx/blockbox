package blockbox

import org.lwjgl.opengl.{GL, GL20}
import org.lwjgl.opengl.GL11.*
import org.lwjgl.opengl.GL12.GL_CLAMP_TO_EDGE

/** A small refraction pass over the resolved world. It works with every shader pack and the
  * compatibility renderer, while keeping the air meter and menus sharp.
  */
object UnderwaterEffect:
  private var program = 0
  private var texture = 0
  private var width = 0
  private var height = 0
  private var failed = false

  def render(w: Int, h: Int, time: Float, depth: Float): Boolean =
    if failed || !GL.getCapabilities.OpenGL20 then return false
    try
      if program == 0 then program = BlockboxAtmosphere.buildProgram(vertex, fragment)
      if texture == 0 then texture = glGenTextures()
      glEnable(GL_TEXTURE_2D); glBindTexture(GL_TEXTURE_2D, texture)
      if width != w || height != h then
        width = w; height = h
        glTexImage2D(GL_TEXTURE_2D, 0, GL_RGB8, w, h, 0, GL_RGB, GL_UNSIGNED_BYTE, 0L)
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR)
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE)
      glCopyTexSubImage2D(GL_TEXTURE_2D, 0, 0, 0, 0, 0, w, h)
      val previous = glGetInteger(GL20.GL_CURRENT_PROGRAM)
      glDisable(GL_DEPTH_TEST); glDisable(GL_FOG); glDisable(GL_BLEND); glDisable(GL_CULL_FACE)
      GL20.glUseProgram(program)
      GL20.glUniform1i(GL20.glGetUniformLocation(program, "scene"), 0)
      GL20.glUniform1f(GL20.glGetUniformLocation(program, "time"), time)
      GL20.glUniform1f(GL20.glGetUniformLocation(program, "depth"), depth.max(0f).min(20f))
      GL20.glUniform2f(GL20.glGetUniformLocation(program, "size"), w.toFloat, h.toFloat)
      glBegin(GL_TRIANGLES); glVertex2f(-1, -1); glVertex2f(3, -1); glVertex2f(-1, 3); glEnd()
      GL20.glUseProgram(previous); glBindTexture(GL_TEXTURE_2D, 0); glDisable(GL_TEXTURE_2D)
      glEnable(GL_BLEND); glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA)
      true
    catch
      case e: Exception =>
        failed = true; GL20.glUseProgram(0)
        System.err.println("Water refraction unavailable: " + e.getMessage)
        false

  def destroy(): Unit =
    if program != 0 then GL20.glDeleteProgram(program)
    if texture != 0 then glDeleteTextures(texture)
    program = 0; texture = 0; width = 0; height = 0; failed = false

  private val vertex = """#version 120
    varying vec2 uv;
    void main(){gl_Position=vec4(gl_Vertex.xy,0.0,1.0);uv=gl_Vertex.xy*0.5+0.5;}
  """
  private val fragment = """#version 120
    uniform sampler2D scene;
    uniform float time,depth;
    uniform vec2 size;
    varying vec2 uv;
    void main(){
      vec2 p=uv;
      vec2 wave=vec2(sin(p.y*19.0+time*1.2)+sin(p.x*23.0-time*0.8),
                     cos(p.x*17.0+time*0.9)+sin(p.y*27.0-time*1.0));
      vec2 edge=min(p,1.0-p);
      p+=wave*vec2(0.0013,0.0017)*smoothstep(0.0,0.08,min(edge.x,edge.y));
      vec3 color=texture2D(scene,clamp(p,1.0/size,1.0-1.0/size)).rgb;
      color*=vec3(0.79,0.96,1.0)*mix(1.0,0.79,depth/20.0);
      color=mix(color,vec3(0.015,0.17,0.23),0.045);
      vec2 v=uv*2.0-1.0;
      float vignette=smoothstep(0.35,1.7,dot(v,v));
      color*=1.0-vignette*0.20;
      gl_FragColor=vec4(color,1.0);
    }
  """
