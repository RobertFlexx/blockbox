package blockbox

import org.lwjgl.opengl.GL
import org.lwjgl.opengl.GL11.*
import org.lwjgl.opengl.GL12.*
import org.lwjgl.opengl.GL13.*
import org.lwjgl.opengl.GL14.*
import org.lwjgl.opengl.GL20.*
import org.lwjgl.opengl.GL21.*
import org.lwjgl.opengl.GL30.*

/** Optional reduced-resolution world rendering with a bounded sharpening resolve.
  * Texture coordinates stay linear so edges, the crosshair and block picking agree.
  * The historical class/settings names remain compatible with existing settings.
  */
final class BlockboxFovea:
  // Wired to the in-game settings toggle.
  var enabled = true
  // Reduced internal resolution before the sharpening resolve.
  var internalScale = Option(System.getenv("BLOCKBOX_FOVEA_SCALE")).flatMap(v => scala.util.Try(v.trim.toFloat).toOption).filter(s => s > 0.3f && s <= 1f).getOrElse(0.85f)
  private var colorTex = 0
  private var depthRbo = 0
  private var fbo = 0
  private var texW = 0
  private var texH = 0
  private var program = 0
  private var backingW = 0
  private var backingH = 0
  private val clearVal = org.lwjgl.BufferUtils.createFloatBuffer(4)

  def supportsFbos: Boolean =
    val caps =
      try GL.getCapabilities
      catch case _: Throwable => return false
    caps.OpenGL30 || caps.GL_ARB_framebuffer_object

  def internalWidth(w: Int): Int = math.max(1, (w * internalScale).toInt)
  def internalHeight(h: Int): Int = math.max(1, (h * internalScale).toInt)

  /** Bind the reduced-resolution world buffer and clear it (for the no-shader path). */
  def beginScene(w: Int, h: Int): Boolean =
    if !enabled || !ensureResources(w, h) then return false
    backingW = w; backingH = h
    clearVal.clear()
    glGetFloatv(GL_COLOR_CLEAR_VALUE, clearVal)
    glBindFramebuffer(GL_FRAMEBUFFER, fbo)
    glViewport(0, 0, texW, texH)
    glDisable(GL_SCISSOR_TEST)
    glClearColor(clearVal.get(0), clearVal.get(1), clearVal.get(2), clearVal.get(3))
    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT)
    true

  /** Bind the reduced-resolution color target (depth unused) so the shader pipeline's
    * final composite can draw into it. Callers must later call upsampleToWindow.
    */
  def bindTarget(w: Int, h: Int): Boolean =
    if !enabled || !ensureResources(w, h) then return false
    backingW = w; backingH = h
    glBindFramebuffer(GL_FRAMEBUFFER, fbo)
    glViewport(0, 0, texW, texH)
    glDrawBuffer(GL_COLOR_ATTACHMENT0)
    true

  def targetWidth: Int = texW
  def targetHeight: Int = texH

  /** Finish a wrapped world draw: present the sharpening resolve to the window. */
  def endScene(): Unit = upsampleToWindow(backingW, backingH)

  /** Present the sharpening resolve of colorTex to the default framebuffer. */
  def upsampleToWindow(w: Int, h: Int): Unit =
    if fbo == 0 || colorTex == 0 || !compileProgram() then return
    glBindFramebuffer(GL_FRAMEBUFFER, 0)
    glViewport(0, 0, w.max(1), h.max(1))
    renderUpsample(w.max(1), h.max(1))

  private def ensureResources(w: Int, h: Int): Boolean =
    if !supportsFbos then return false
    val iw = internalWidth(w); val ih = internalHeight(h)
    if fbo != 0 && texW == iw && texH == ih then return true
    release()
    texW = iw; texH = ih
    colorTex = glGenTextures()
    glBindTexture(GL_TEXTURE_2D, colorTex)
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR)
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR)
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE)
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE)
    glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA8, iw, ih, 0, GL_RGBA, GL_UNSIGNED_BYTE, null: java.nio.ByteBuffer)
    depthRbo = glGenRenderbuffers()
    glBindRenderbuffer(GL_RENDERBUFFER, depthRbo)
    glRenderbufferStorage(GL_RENDERBUFFER, GL_DEPTH_COMPONENT24, iw, ih)
    fbo = glGenFramebuffers()
    glBindFramebuffer(GL_FRAMEBUFFER, fbo)
    glFramebufferTexture2D(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, GL_TEXTURE_2D, colorTex, 0)
    glFramebufferRenderbuffer(GL_FRAMEBUFFER, GL_DEPTH_ATTACHMENT, GL_RENDERBUFFER, depthRbo)
    val status = glCheckFramebufferStatus(GL_FRAMEBUFFER)
    glBindFramebuffer(GL_FRAMEBUFFER, 0)
    glBindRenderbuffer(GL_RENDERBUFFER, 0)
    glBindTexture(GL_TEXTURE_2D, 0)
    if status != GL_FRAMEBUFFER_COMPLETE then
      System.err.println(s"Blockbox fovea framebuffer incomplete: 0x${Integer.toHexString(status)}")
      release()
      return false
    true

  private def renderUpsample(w: Int, h: Int): Unit =
    glDisable(GL_BLEND)
    glDisable(GL_DEPTH_TEST)
    glDepthMask(false)
    glDisable(GL_CULL_FACE)
    glDisable(GL_FOG)
    glDisable(GL_ALPHA_TEST)
    glActiveTexture(GL_TEXTURE0)
    glEnable(GL_TEXTURE_2D)
    glBindTexture(GL_TEXTURE_2D, colorTex)
    glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_MODULATE)
    glUseProgram(program)
    glUniform1i(glGetUniformLocation(program, "uSource"), 0)
    glUniform2f(glGetUniformLocation(program, "uResolution"), texW.toFloat, texH.toFloat)
    glMatrixMode(GL_PROJECTION); glPushMatrix(); glLoadIdentity(); glOrtho(0, 1, 0, 1, -1, 1)
    glMatrixMode(GL_MODELVIEW); glPushMatrix(); glLoadIdentity()
    glColor4f(1f, 1f, 1f, 1f)
    glBegin(GL_QUADS)
    glTexCoord2f(0f, 0f); glVertex2f(0f, 0f)
    glTexCoord2f(1f, 0f); glVertex2f(1f, 0f)
    glTexCoord2f(1f, 1f); glVertex2f(1f, 1f)
    glTexCoord2f(0f, 1f); glVertex2f(0f, 1f)
    glEnd()
    glUseProgram(0)
    glMatrixMode(GL_PROJECTION); glPopMatrix()
    glMatrixMode(GL_MODELVIEW); glPopMatrix()
    glActiveTexture(GL_TEXTURE0)
    glBindTexture(GL_TEXTURE_2D, 0)
    glDisable(GL_TEXTURE_2D)
    glEnable(GL_BLEND)
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA)
    glDepthMask(true)

  private def compileProgram(): Boolean =
    if program != 0 then return true
    val vs = glCreateShader(GL_VERTEX_SHADER)
    glShaderSource(vs, vertexSrc)
    glCompileShader(vs)
    if glGetShaderi(vs, GL_COMPILE_STATUS) == GL_FALSE then
      System.err.println(s"Blockbox fovea vertex shader failed: ${glGetShaderInfoLog(vs, 4096)}")
      glDeleteShader(vs)
      return false
    val fs = glCreateShader(GL_FRAGMENT_SHADER)
    glShaderSource(fs, fragmentSrc)
    glCompileShader(fs)
    if glGetShaderi(fs, GL_COMPILE_STATUS) == GL_FALSE then
      System.err.println(s"Blockbox fovea fragment shader failed: ${glGetShaderInfoLog(fs, 4096)}")
      glDeleteShader(vs); glDeleteShader(fs)
      return false
    val prog = glCreateProgram()
    glAttachShader(prog, vs)
    glAttachShader(prog, fs)
    glLinkProgram(prog)
    glDeleteShader(vs); glDeleteShader(fs)
    if glGetProgrami(prog, GL_LINK_STATUS) == GL_FALSE then
      System.err.println(s"Blockbox fovea program failed: ${glGetProgramInfoLog(prog, 4096)}")
      glDeleteProgram(prog)
      return false
    program = prog
    true

  def release(): Unit =
    if program != 0 then glDeleteProgram(program); program = 0
    if depthRbo != 0 then glDeleteRenderbuffers(depthRbo); depthRbo = 0
    if colorTex != 0 then glDeleteTextures(colorTex); colorTex = 0
    if fbo != 0 then glDeleteFramebuffers(fbo); fbo = 0
    texW = 0; texH = 0

  private val vertexSrc =
    """#version 120
      |varying vec2 vUv;
      |void main() {
      |  vUv = gl_MultiTexCoord0.xy;
      |  gl_Position = gl_ProjectionMatrix * gl_ModelViewMatrix * gl_Vertex;
      |}
      |""".stripMargin

  private val fragmentSrc =
    """#version 120
      |uniform sampler2D uSource;
      |uniform vec2 uResolution;
      |varying vec2 vUv;
      |void main() {
      |  vec2 px = 1.0 / max(uResolution, vec2(1.0));
      |  vec3 c = texture2D(uSource, vUv).rgb;
      |  vec3 n = texture2D(uSource, vUv + vec2(0.0, px.y)).rgb;
      |  vec3 s = texture2D(uSource, vUv - vec2(0.0, px.y)).rgb;
      |  vec3 e = texture2D(uSource, vUv + vec2(px.x, 0.0)).rgb;
      |  vec3 w = texture2D(uSource, vUv - vec2(px.x, 0.0)).rgb;
      |  vec3 lo = min(c, min(min(n, s), min(e, w)));
      |  vec3 hi = max(c, max(max(n, s), max(e, w)));
      |  vec3 detail = c - (n + s + e + w) * 0.25;
      |  gl_FragColor = vec4(clamp(c + detail * 0.25, lo, hi), 1.0);
      |}
      |""".stripMargin
