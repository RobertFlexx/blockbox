package blockbox

import org.lwjgl.opengl.GL
import org.lwjgl.opengl.GL11.*
import org.lwjgl.opengl.GL12.GL_CLAMP_TO_EDGE
import org.lwjgl.opengl.GL13.GL_MULTISAMPLE
import org.lwjgl.opengl.GL14.GL_DEPTH_COMPONENT24
import org.lwjgl.opengl.GL14.glBlendFuncSeparate
import org.lwjgl.opengl.GL30.*

/** Small geometry pass with MSAA and a premultiplied-alpha resolve for clean hand edges. */
final class FirstPersonPass:
  private var multisampleFbo = 0; private var resolveFbo = 0
  private var colorBuffer = 0; private var depthBuffer = 0; private var texture = 0
  private var width = 0; private var height = 0
  private var renderWidth = 0
  private var failed = false

  /** NDC x of the pass's left edge; the projection's left plane must match it. */
  def leftEdge: Float = if width == 0 then -1f else -1f + 2f * (width - renderWidth) / width

  def begin(w: Int, h: Int): Boolean =
    if failed || !GL.getCapabilities.OpenGL30 || glGetInteger(GL_MAX_SAMPLES) < 2 then return false
    try
      if multisampleFbo == 0 || w != width || h != height then allocate(w, h)
      glBindFramebuffer(GL_FRAMEBUFFER, multisampleFbo); glViewport(0, 0, renderWidth, h)
      glEnable(GL_MULTISAMPLE); glDisable(GL_SCISSOR_TEST)
      glDepthMask(true); glClearColor(0, 0, 0, 0);
      glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT)
      glEnable(GL_BLEND);
      glBlendFuncSeparate(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA, GL_ONE, GL_ONE_MINUS_SRC_ALPHA)
      true
    catch
      case e: Exception =>
        destroy(); failed = true; glBindFramebuffer(GL_FRAMEBUFFER, 0); glViewport(0, 0, w, h)
        System.err.println("Hand antialiasing unavailable: " + e.getMessage)
        false

  def finish(): Unit =
    glBindFramebuffer(GL_READ_FRAMEBUFFER, multisampleFbo);
    glBindFramebuffer(GL_DRAW_FRAMEBUFFER, resolveFbo)
    glBlitFramebuffer(
      0,
      0,
      renderWidth,
      height,
      0,
      0,
      renderWidth,
      height,
      GL_COLOR_BUFFER_BIT,
      GL_NEAREST
    )
    glBindFramebuffer(GL_FRAMEBUFFER, 0); glViewport(0, 0, width, height)
    glDisable(GL_MULTISAMPLE); glDisable(GL_DEPTH_TEST); glDepthMask(false)
    glEnable(GL_BLEND); glBlendFunc(GL_ONE, GL_ONE_MINUS_SRC_ALPHA)
    glEnable(GL_TEXTURE_2D); glBindTexture(GL_TEXTURE_2D, texture)
    glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_MODULATE); glColor4f(1, 1, 1, 1)
    glMatrixMode(GL_PROJECTION); glPushMatrix(); glLoadIdentity()
    glMatrixMode(GL_MODELVIEW); glPushMatrix(); glLoadIdentity()
    glBegin(GL_QUADS)
    val left = leftEdge
    glTexCoord2f(0, 0); glVertex2f(left, -1); glTexCoord2f(1, 0); glVertex2f(1, -1)
    glTexCoord2f(1, 1); glVertex2f(1, 1); glTexCoord2f(0, 1); glVertex2f(left, 1)
    glEnd()
    glPopMatrix(); glMatrixMode(GL_PROJECTION); glPopMatrix(); glMatrixMode(GL_MODELVIEW)
    glBindTexture(GL_TEXTURE_2D, 0); glDisable(GL_TEXTURE_2D)
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA); glDepthMask(true)

  private def allocate(w: Int, h: Int): Unit =
    destroy(); width = w; height = h; renderWidth = (w * 3 + 3) / 4
    // Viewmodel geometry stays in the right three quarters: swings reach about 0.25 NDC
    // past the center on 5:4 screens. At 4K use two samples to bound memory and
    // bandwidth instead of allocating a full 4x scene-sized pass.
    val samples = glGetInteger(GL_MAX_SAMPLES).min(if w.toLong * h > 2560L * 1440 then 2 else 4)
    multisampleFbo = glGenFramebuffers(); glBindFramebuffer(GL_FRAMEBUFFER, multisampleFbo)
    colorBuffer = glGenRenderbuffers(); glBindRenderbuffer(GL_RENDERBUFFER, colorBuffer)
    glRenderbufferStorageMultisample(GL_RENDERBUFFER, samples, GL_RGBA8, renderWidth, h)
    glFramebufferRenderbuffer(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, GL_RENDERBUFFER, colorBuffer)
    depthBuffer = glGenRenderbuffers(); glBindRenderbuffer(GL_RENDERBUFFER, depthBuffer)
    glRenderbufferStorageMultisample(GL_RENDERBUFFER, samples, GL_DEPTH_COMPONENT24, renderWidth, h)
    glFramebufferRenderbuffer(GL_FRAMEBUFFER, GL_DEPTH_ATTACHMENT, GL_RENDERBUFFER, depthBuffer)
    require(
      glCheckFramebufferStatus(GL_FRAMEBUFFER) == GL_FRAMEBUFFER_COMPLETE,
      "incomplete multisample framebuffer"
    )
    resolveFbo = glGenFramebuffers(); glBindFramebuffer(GL_FRAMEBUFFER, resolveFbo)
    texture = glGenTextures(); glBindTexture(GL_TEXTURE_2D, texture)
    glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA8, renderWidth, h, 0, GL_RGBA, GL_UNSIGNED_BYTE, 0L)
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST)
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE)
    glFramebufferTexture2D(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, GL_TEXTURE_2D, texture, 0)
    require(
      glCheckFramebufferStatus(GL_FRAMEBUFFER) == GL_FRAMEBUFFER_COMPLETE,
      "incomplete resolve framebuffer"
    )
    glBindRenderbuffer(GL_RENDERBUFFER, 0); glBindTexture(GL_TEXTURE_2D, 0)

  def destroy(): Unit =
    if multisampleFbo != 0 then glDeleteFramebuffers(multisampleFbo)
    if resolveFbo != 0 then glDeleteFramebuffers(resolveFbo)
    if colorBuffer != 0 then glDeleteRenderbuffers(colorBuffer)
    if depthBuffer != 0 then glDeleteRenderbuffers(depthBuffer)
    if texture != 0 then glDeleteTextures(texture)
    multisampleFbo = 0; resolveFbo = 0; colorBuffer = 0; depthBuffer = 0; texture = 0; width = 0;
    height = 0
