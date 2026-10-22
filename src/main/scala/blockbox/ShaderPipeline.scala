package blockbox

import org.lwjgl.BufferUtils
import org.lwjgl.opengl.GL
import org.lwjgl.opengl.GL11.*
import org.lwjgl.opengl.GL12.*
import org.lwjgl.opengl.GL13.*
import org.lwjgl.opengl.GL14.*
import org.lwjgl.opengl.GL20.*
import org.lwjgl.opengl.GL30.*
import org.lwjgl.opengl.GL32.*

import java.io.File
import java.nio.FloatBuffer
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import scala.collection.mutable

/** High-throughput Blockbox shader-pack pipeline.
  *
  * The world geometry is submitted once. Packs can replace the geometry programs,
  * then run a chain of fullscreen deferred/composite passes on the GPU before a
  * final pass writes the 3D image to the window. Main renders HUD/menu overlays
  * after finishFrame(), so packs transform the world without mangling UI text.
  *
  * Supported stage names use Minecraft/Iris naming where the engine has equivalent
  * data: gbuffers_terrain[_solid/_cutout], gbuffers_textured[_lit],
  * gbuffers_translucent, gbuffers_water, deferred/deferred1..99,
  * composite/composite1..99 and final. Vertex/fragment aliases (.vsh/.fsh or
  * .vert/.frag) are accepted, plus optional geometry shaders (.gsh/.geom).
  * Overworld overrides in shaders/world0 and root-relative #include paths work too.
  *
  * This is a compatibility bridge, not a claim that arbitrary Iris packs can be
  * drop-in perfect: packs that depend on Minecraft-only generic terrain attributes,
  * custom images, compute/tessellation or renderer-specific hooks still need an
  * adapter. Blockbox does provide the compatibility lightmap, multi-target buffers,
  * shadow map pass and common Iris uniforms so ordinary compatibility packs have a
  * useful path instead of being bottlenecked by the base renderer.
  */
final class BlockboxShaderPipeline(rootDir: File,launchPack: Option[String]=Option(System.getenv("BLOCKBOX_SHADERPACK")).map(_.trim).filter(_.nonEmpty)):
  private final class Program(val id: Int, val label: String, val renderTargets: Option[Array[Int]], val mipmapBuffers: Array[Int]):
    private val uniformCache = mutable.HashMap.empty[String, Int]
    var customVersion = -1L
    def uniform(name: String): Int = uniformCache.getOrElseUpdate(name, glGetUniformLocation(id, name))

  private val fallbackVertexFile = File(rootDir, "assets/shaders/world.vert")
  private val fallbackFragmentFile = File(rootDir, "assets/shaders/world.frag")
  private val shaderpacksRoot = File(rootDir, "shaderpacks")

  private var worldPrograms = Map.empty[Int, Program]
  private var postPrograms = Vector.empty[Program]
  private var finalProgram: Option[Program] = None
  private var activeProgram: Program | Null = null
  private var currentStatus = "GLSL not initialized"
  private var activePackName = ""
  private var selectedPackName: Option[String] = None
  private var pendingPackName: Option[String] = None
  private val customFloats=mutable.LinkedHashMap.empty[String,Float]
  private var customVersion=0L

  def setCustomUniform(name: String,value: Float): Unit =
    require(name!=null && name.matches("mod_[A-Za-z0-9_]{1,56}") && value.isFinite,"Custom uniforms need a mod_ prefix and a finite value")
    require(customFloats.contains(name) || customFloats.size<64,"Custom uniform limit is 64")
    if !customFloats.get(name).contains(value) then
      customFloats(name)=value; customVersion+=1

  private def applyCustomUniforms(p: Program): Unit =
    if p.customVersion!=customVersion then
      customFloats.foreach((name,value) => set1f(p,name,value))
      p.customVersion=customVersion
  private var sourceStamp = Long.MinValue
  private var lastReloadCheck = -1000.0
  private var failedPackName = ""
  private var failedSourceStamp = Long.MinValue

  // HDR scene + depth and two color-only ping-pong targets. Geometry is rendered
  // exactly once into sceneFbo. Every additional effect is a fullscreen GPU pass.
  private var sceneFbo = 0
  private var sceneColor = 0
  private var sceneDepth = 0
  private var sceneNormal = 0
  private var sceneMaterial = 0
  // Iris-style post processing keeps an alternate texture for every logical
  // colortex buffer. A pass swaps only the buffers it writes, allowing the next
  // pass to read the previous contents without a read/write feedback hazard.
  private var postFbo = 0
  private var postAlternateColors = Array.fill[Int](8)(0)
  private val postDrawBuffers = BufferUtils.createIntBuffer(8)
  private var targetW = 0
  private var targetH = 0
  private var sceneBound = false

  // Directional Minecraft-style shadow pass. Two shadowcolor attachments are exposed
  // and one depth map is aliased as both shadowtex0/1 for broad pack compatibility.
  // (Blockbox does not yet split transparent-vs-opaque depth into distinct maps.)
  private var shadowPrograms = Map.empty[Int, Program]
  private var shadowFbo = 0
  private var shadowDepth = 0
  private var shadowColor0 = 0
  private var shadowColor1 = 0
  private var shadowResolution = 2048
  private var shadowTargetSize = 0
  private var shadowPassBound = false
  private val shadowDrawBuffers = BufferUtils.createIntBuffer(2)
  private val shadowProjection = identityMatrix()
  private val shadowProjectionInverse = identityMatrix()
  private val shadowModelView = identityMatrix()
  private val shadowModelViewInverse = identityMatrix()
  private val shadowMatrixBuffer = BufferUtils.createFloatBuffer(16)

  // Scene MRT state. Compatibility/immediate-mode draws (sky, particles, players,
  // selection outlines) must never scribble into the structured normal/material
  // attachments. Only GLSL G-buffer geometry enables all three attachments.
  private val MaxCompatColorBuffers = 8
  private var gbufferCount = 3
  // Allocate only the MRTs the active shader pack can actually touch. Older code
  // allocated all eight full-resolution attachments for every post-enabled pack,
  // wasting VRAM and memory bandwidth even when a pack only used color/normal/material.
  private var desiredGbufferCount = 3
  private var alternateTargets = Set.empty[Int]
  private var colorFormats = Array(GL_RGBA16F,GL_RGBA16F,GL_RGBA8,GL_RGBA8,GL_RGBA8,GL_RGBA8,GL_RGBA8,GL_RGBA8)
  private var celestialTime = 150f
  private var antialiasing = true
  def setAntialiasing(enabled: Boolean): Unit = antialiasing = enabled
  private var nightShading = true
  def setNightShading(enabled: Boolean): Unit = nightShading = enabled
  private var lightEnvironment = BlockboxLightmap.Environment.Noon
  /** Dusk tint, weather and torch flicker for this frame's lightmap. */
  def setLightEnvironment(environment: BlockboxLightmap.Environment): Unit = lightEnvironment = environment

  /** The compatibility renderer uses the same block/sky lightmap as shader packs. */
  def beginCompatibilityLighting(day: Float): Unit =
    if lightmapTexture==0 then lightmapTexture=createLightmapTexture()
    updateLightmapTexture(day)
    glActiveTexture(GL_TEXTURE1)
    glEnable(GL_TEXTURE_2D); glBindTexture(GL_TEXTURE_2D,lightmapTexture)
    glTexEnvi(GL_TEXTURE_ENV,GL_TEXTURE_ENV_MODE,GL_MODULATE)
    glMatrixMode(GL_TEXTURE); glLoadIdentity(); glTranslatef(0.03125f,0.03125f,0f); glScalef(1f/256f,1f/256f,1f)
    glMatrixMode(GL_MODELVIEW); glActiveTexture(GL_TEXTURE0)

  def endCompatibilityLighting(): Unit =
    glActiveTexture(GL_TEXTURE1); glDisable(GL_TEXTURE_2D); glBindTexture(GL_TEXTURE_2D,0)
    glMatrixMode(GL_TEXTURE); glLoadIdentity(); glMatrixMode(GL_MODELVIEW)
    glActiveTexture(GL_TEXTURE0)
  def setCelestialTime(time: Float): Unit = celestialTime = time
  private var sceneExtraColors = Array.fill[Int](MaxCompatColorBuffers - 3)(0)
  private val sceneDrawBuffers = BufferUtils.createIntBuffer(MaxCompatColorBuffers)
  private val clearColorBuffer = BufferUtils.createFloatBuffer(4)
  private val clearNormalBuffer = BufferUtils.createFloatBuffer(4)
  private val clearMaterialBuffer = BufferUtils.createFloatBuffer(4)
  private val clearAuxBuffer = BufferUtils.createFloatBuffer(4)
  private var noiseTexture = 0
  private var lightmapTexture = 0
  private var normalAtlasFallback = 0
  private var specularAtlasFallback = 0
  private var lastLightmapEnvironment: BlockboxLightmap.Environment = null
  private val lightmapPixels = BufferUtils.createByteBuffer(16 * 16 * 4)
  private var maxTextureUnits = 4

  // Frame/world state shared by all pack programs.
  private var fogR = 0f; private var fogG = 0f; private var fogB = 0f
  private var fogStart = 0f; private var fogEnd = 1f
  private var daylight = 1f; private var worldTime = 0f
  private var rainAmount = 0f; private var wetnessAmount = 0f; private var eyeInWater = 0
  private var configuredShadowDistance = 0f
  private var activeShadowDistance = 128f
  private var noiseTextureResolution = 256
  private var sunDir = (0f, 1f, 0f); private var moonDir = (0f, -1f, 0f)
  private var frameWidth = 1; private var frameHeight = 1
  private var frameIndex = 0L
  private var frameTimeSeconds = 1f / 60f
  private var lastFrameNanos = 0L
  private var nearPlane = 0.25f; private var farPlane = 512f
  private var cameraPos = (0f, 0f, 0f)
  private var previousCameraPos = (0f, 0f, 0f)
  private var havePreviousFrame = false

  private val projection = identityMatrix()
  private val modelView = identityMatrix()
  private val projectionInverse = identityMatrix()
  private val modelViewInverse = identityMatrix()
  private val previousProjection = identityMatrix()
  private val previousModelView = identityMatrix()
  private val matrixReadBuffer = BufferUtils.createFloatBuffer(16)
  private val matrixUploadBuffer = BufferUtils.createFloatBuffer(16)

  def available: Boolean = worldPrograms.nonEmpty
  def status: String = currentStatus
  def packName: String = if activePackName.nonEmpty then activePackName else "built-in"

  def availablePackNames: Vector[String] =
    if !shaderpacksRoot.isDirectory then Vector.empty
    else
      Option(shaderpacksRoot.listFiles()).toVector.flatten
        .filter(f => f.isDirectory && File(f, "shaders").isDirectory)
        .map(_.getName)
        .filter(name => name.nonEmpty && !name.startsWith("."))
        .sorted

  /** Cycle installed shader packs from the in-game Resources screen. */
  def cyclePack(): String =
    val packs = availablePackNames
    if packs.isEmpty then packName
    else
      val current = packs.indexOf(activePackName)
      val next = packs(if current < 0 then 0 else (current + 1) % packs.length)
      selectPack(next)
      packName

  /** An explicit UI selection overrides the launch default for this session. */
  def selectPack(name: String): Boolean =
    if !availablePackNames.contains(name) then return false
    if sceneBound || shadowPassBound then
      pendingPackName=Some(name)
      return true
    val previous = selectedPackName
    selectedPackName = Some(name)
    reload(force = true)
    if activePackName != name || failedSourceStamp != Long.MinValue then
      selectedPackName = previous
      false
    else
      try
        Files.createDirectories(shaderpacksRoot.toPath)
        Files.writeString(File(shaderpacksRoot, "active.txt").toPath,name+System.lineSeparator(),StandardCharsets.UTF_8)
      catch case e: Exception => System.err.println(s"Blockbox shaders: selection could not be saved: ${e.getMessage}")
      true

  def postProcessing: Boolean = (postPrograms.nonEmpty || finalProgram.nonEmpty) && GL.getCapabilities.OpenGL30
  def shadowMapping: Boolean = shadowPrograms.nonEmpty && GL.getCapabilities.OpenGL30

  def initialize(): Unit =
    lastLightmapEnvironment = null
    if !GL.getCapabilities.OpenGL20 then
      currentStatus = "GLSL unavailable; fixed-function fallback"
      System.err.println("Blockbox shaders: OpenGL 2.0 shader support is unavailable")
    else
      maxTextureUnits = glGetInteger(GL_MAX_TEXTURE_IMAGE_UNITS).max(1)
      noiseTexture = createNoiseTexture(noiseTextureResolution)
      lightmapTexture = createLightmapTexture()
      normalAtlasFallback = createSolidTexture(127, 127, 255, 255)
      specularAtlasFallback = createSolidTexture(0, 0, 0, 0)
      reload(force = true)

  def reloadIfChanged(now: Double): Unit =
    if !sceneBound && !shadowPassBound && pendingPackName.nonEmpty then
      val next=pendingPackName.get
      pendingPackName=None
      selectPack(next)
    if !GL.getCapabilities.OpenGL20 || now - lastReloadCheck < 0.75 then return
    lastReloadCheck = now
    val nextPack = resolvePackName()
    val nextStamp = computeSourceStamp(nextPack)
    if nextPack == failedPackName && nextStamp == failedSourceStamp then return
    if nextPack != activePackName || nextStamp != sourceStamp then reload(force = false)

  /** True while Main is drawing the world into the offscreen G-buffer. Lets the
    * compatibility renderer avoid double-wrapping the world when shaders are on.
    */
  def isSceneBound: Boolean = sceneBound

  def restoreSceneViewport(): Unit =
    if sceneBound then glViewport(0, 0, targetW, targetH)

  def shadowDistanceLimit(fallback: Float): Float =
    if configuredShadowDistance > 0f then fallback.min(configuredShadowDistance) else fallback

  private var foveaHook: BlockboxFovea | Null = null
  def setFovea(fovea: BlockboxFovea): Unit = foveaHook = fovea

  /** Bind the offscreen scene target before Main clears and draws a 3D screen. */
  def prepareFrame(width: Int, height: Int): Boolean =
    val scale = if foveaHook != null && foveaHook.enabled then foveaHook.internalScale else 1f
    frameWidth = (width * scale).round.max(1)
    frameHeight = (height * scale).round.max(1)
    val now = System.nanoTime()
    if lastFrameNanos != 0L then
      frameTimeSeconds = (((now - lastFrameNanos).toDouble / 1.0e9).max(0.0001).min(0.25)).toFloat
    lastFrameNanos = now
    frameIndex += 1
    if !postProcessing then
      sceneBound = false
      false
    else
      try
        ensureTargets(frameWidth, frameHeight)
        if sceneFbo == 0 then return false
        glBindFramebuffer(GL_FRAMEBUFFER, sceneFbo)
        glViewport(0, 0, frameWidth, frameHeight)
        sceneBound = true
        // Sky, particles and compatibility geometry are color-only until chunks bind
        // real G-buffer shaders.
        enableColorOnlyWrites()
        true
      catch
        case e: Throwable =>
          sceneBound = false
          glBindFramebuffer(GL_FRAMEBUFFER, 0)
          currentStatus = s"${packName}: direct shaders (post target failed)"
          System.err.println(s"Blockbox shaders: framebuffer fallback: ${e.getMessage}")
          false

  /** Clear every scene attachment with semantically valid defaults. */
  def clearPreparedFrame(r: Float, g: Float, b: Float): Unit =
    if !sceneBound || sceneFbo == 0 then return
    // glClearBuffer indices address the current DRAW_BUFFER mapping. Color-only
    // sky rendering leaves slots 1..N as NONE, so restore the mapping first.
    enableGBufferWrites()
    clearColorBuffer.clear(); clearColorBuffer.put(r).put(g).put(b).put(1f); clearColorBuffer.flip()
    clearNormalBuffer.clear(); clearNormalBuffer.put(0.5f).put(0.5f).put(1f).put(1f); clearNormalBuffer.flip()
    clearMaterialBuffer.clear(); clearMaterialBuffer.put(0f).put(0f).put(0f).put(0f); clearMaterialBuffer.flip()
    glClearBufferfv(GL_COLOR, 0, clearColorBuffer)
    glClearBufferfv(GL_COLOR, 1, clearNormalBuffer)
    glClearBufferfv(GL_COLOR, 2, clearMaterialBuffer)
    clearAuxBuffer.clear(); clearAuxBuffer.put(0f).put(0f).put(0f).put(0f); clearAuxBuffer.flip()
    var aux = 3
    while aux < gbufferCount do
      glClearBufferfv(GL_COLOR, aux, clearAuxBuffer)
      aux += 1
    glClear(GL_DEPTH_BUFFER_BIT)
    enableColorOnlyWrites()

  def enableColorOnlyWrites(): Unit =
    if sceneBound && sceneFbo != 0 then glDrawBuffer(GL_COLOR_ATTACHMENT0)

  def enableGBufferWrites(): Unit =
    if sceneBound && sceneFbo != 0 then
      sceneDrawBuffers.rewind()
      glDrawBuffers(sceneDrawBuffers)

  /** Run deferred/composite/final passes and return to the real framebuffer. */
  def finishFrame(width: Int, height: Int, time: Float): Unit =
    if !sceneBound then return
    sceneBound = false

    glDisable(GL_DEPTH_TEST)
    glDepthMask(false)
    glDisable(GL_CULL_FACE)
    glDisable(GL_BLEND)
    glDisable(GL_FOG)
    glDisable(GL_ALPHA_TEST)

    glMatrixMode(GL_PROJECTION); glPushMatrix(); glLoadIdentity(); glOrtho(-1, 1, -1, 1, -1, 1)
    glMatrixMode(GL_MODELVIEW); glPushMatrix(); glLoadIdentity()

    val currentColors = sceneColorArray()
    val alternates = postAlternateColors.clone()
    var i = 0
    while i < postPrograms.length do
      val prog = postPrograms(i)
      val requested = prog.renderTargets.getOrElse(Array(0))
      val written = bindPostTargets(requested, alternates)
      if written.nonEmpty then
        generateRequestedMipmaps(prog, currentColors)
        glViewport(0, 0, targetW, targetH)
        glClearColor(0f, 0f, 0f, 0f)
        glClear(GL_COLOR_BUFFER_BIT)
        drawFullscreen(prog, currentColors, sceneDepth, targetW, targetH, time)
        var w = 0
        while w < written.length do
          val logical = written(w)
          val old = currentColors(logical)
          currentColors(logical) = alternates(logical)
          alternates(logical) = old
          w += 1
      i += 1

    glBindFramebuffer(GL_FRAMEBUFFER, 0)
    glViewport(0, 0, width, height)
    finalProgram match
      case Some(p) =>
        generateRequestedMipmaps(p, currentColors)
        drawFinalThroughFovea(p, currentColors, sceneDepth, width, height, time)
      case None => drawFinalThroughFovea(null, currentColors, sceneDepth, width, height, time)

    glUseProgram(0)
    activeProgram = null
    // Post passes can occupy many texture units. Never let their state leak into
    // the compatibility viewmodel/HUD renderer.
    resetCompatibilityTextureUnits()
    commitPreviousFrame()
    glMatrixMode(GL_MODELVIEW); glPopMatrix()
    glMatrixMode(GL_PROJECTION); glPopMatrix(); glMatrixMode(GL_MODELVIEW)
    glDepthMask(true)
    glEnable(GL_BLEND); glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA)

  /** Render terrain once from the active celestial light's point of view. Main calls
    * this before the normal chunk pass while the atlas/client arrays are already set.
    * The scene framebuffer and camera matrices are restored by endShadow().
    */
  def beginShadow(
      cam: (Float, Float, Float),
      sd: (Float, Float, Float), md: (Float, Float, Float),
      dl: Float, radius: Float, time: Float, rain: Float, wetness: Float
  ): Boolean =
    if !shadowMapping then return false
    try
      ensureShadowTarget()
      if shadowFbo == 0 then return false
      cameraPos = cam; sunDir = sd; moonDir = md; daylight = dl; worldTime = time
      rainAmount = rain.max(0f).min(1f); wetnessAmount = wetness.max(0f).min(1f)
      val requestedRadius = radius.max(32f)
      activeShadowDistance = if configuredShadowDistance > 0f then requestedRadius.min(configuredShadowDistance.max(32f)) else requestedRadius
      updateLightmapTexture(daylight)
      val light = if sd._2 > -0.08f then sd else md
      glActiveTexture(GL_TEXTURE1)
      glMatrixMode(GL_TEXTURE); glLoadIdentity(); glTranslatef(0.03125f, 0.03125f, 0.03125f); glScalef(1f / 256f, 1f / 256f, 1f / 256f)
      glMatrixMode(GL_MODELVIEW); glActiveTexture(GL_TEXTURE0)

      glBindFramebuffer(GL_FRAMEBUFFER, shadowFbo)
      // Third-party shadow programs may have remapped the physical color
      // attachments last frame. Always restore the canonical shadowcolor0/1
      // layout before clearing, otherwise a one-target pass can leave attachment
      // state behind and make the next shadow frame incomplete or stale.
      bindShadowTargets(None)
      glViewport(0, 0, shadowTargetSize, shadowTargetSize)
      glClearColor(1f, 1f, 1f, 1f)
      glClearDepth(1.0)
      glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT)

      glMatrixMode(GL_PROJECTION); glPushMatrix(); glLoadIdentity()
      val r = activeShadowDistance
      glOrtho(-r, r, -r, r, -r * 3.5f, r * 3.5f)
      glMatrixMode(GL_MODELVIEW); glPushMatrix(); glLoadIdentity()
      loadDirectionalLightView(light)
      // Chunk VBOs subtract camera X/Z themselves; complete the camera-relative
      // transform vertically here so the shadow volume remains centered on the player.
      glTranslatef(0f, -cam._2, 0f)
      captureMatrix(GL_PROJECTION_MATRIX, shadowProjection)
      captureMatrix(GL_MODELVIEW_MATRIX, shadowModelView)
      invertMatrix(shadowProjection, shadowProjectionInverse)
      invertMatrix(shadowModelView, shadowModelViewInverse)

      shadowPassBound = true
      glEnable(GL_DEPTH_TEST); glDepthMask(true)
      glDisable(GL_BLEND)
      glEnable(GL_POLYGON_OFFSET_FILL); glPolygonOffset(1.15f, 2.0f)
      useShadowPass(0)
      true
    catch
      case e: Throwable =>
        System.err.println(s"Blockbox shaders: shadow pass disabled for this frame: ${e.getMessage}")
        if shadowPassBound then endShadow()
        false

  def setShadowPass(pass: Int): Unit =
    if shadowPassBound then useShadowPass(pass)

  def endShadow(): Unit =
    if !shadowPassBound then return
    shadowPassBound = false
    glUseProgram(0); activeProgram = null
    glDisable(GL_POLYGON_OFFSET_FILL)
    glMatrixMode(GL_MODELVIEW); glPopMatrix()
    glMatrixMode(GL_PROJECTION); glPopMatrix()
    glMatrixMode(GL_MODELVIEW)
    if sceneBound && sceneFbo != 0 then glBindFramebuffer(GL_FRAMEBUFFER, sceneFbo) else glBindFramebuffer(GL_FRAMEBUFFER, 0)
    glViewport(0, 0, frameWidth.max(1), frameHeight.max(1))
    // Shadow stages are allowed to use the compatibility lightmap matrix. Do not
    // leak it into fixed-function UI/viewmodel rendering between passes.
    glActiveTexture(GL_TEXTURE1)
    glMatrixMode(GL_TEXTURE); glLoadIdentity()
    glMatrixMode(GL_MODELVIEW)
    glActiveTexture(GL_TEXTURE0)

  private def useShadowPass(pass: Int): Unit =
    val p = shadowPrograms.getOrElse(pass, shadowPrograms.getOrElse(0, null))
    if p == null then return
    if activeProgram == null || activeProgram.asInstanceOf[Program].id != p.id then
      glUseProgram(p.id); activeProgram = p
    bindShadowTargets(p.renderTargets)
    set1i(p, "uAtlas", 0); set1i(p, "gtexture", 0)
    bindTextureUniform(p, "lightmap", 1, lightmapTexture)
    set1i(p, "renderStage", pass); set1i(p, "uPass", pass)
    set1i(p, "frameCounter", (frameIndex % 720720L).toInt)
    set1f(p, "frameTimeCounter", worldTime % 3600f); set1f(p, "frameTime", frameTimeSeconds)
    set1f(p, "sunBrightness", daylight)
    set1f(p, "rainStrength", rainAmount); set1f(p, "wetness", wetnessAmount)
    set3f(p, "cameraPosition", cameraPos._1, cameraPos._2, cameraPos._3)
    set3f(p, "sunPosition", sunDir._1, sunDir._2, sunDir._3)
    set3f(p, "moonPosition", moonDir._1, moonDir._2, moonDir._3)
    set1i(p, "shadowMapResolution", shadowTargetSize)
    set1f(p, "shadowDistance", activeShadowDistance)
    setMat4(p, "shadowProjection", shadowProjection); setMat4(p, "shadowProjectionInverse", shadowProjectionInverse)
    setMat4(p, "shadowModelView", shadowModelView); setMat4(p, "shadowModelViewInverse", shadowModelViewInverse)
    glActiveTexture(GL_TEXTURE0)

  private def bindShadowTargets(targets: Option[Array[Int]]): Unit =
    if shadowFbo == 0 then return
    targets match
      case Some(logicalTargets) if logicalTargets.nonEmpty =>
        val count = math.min(logicalTargets.length, 2)
        // Detach first so remapping logical shadowcolor1 to physical output 0 never
        // leaves the same image attached twice during framebuffer validation.
        var detach = 0
        while detach < 2 do
          glFramebufferTexture2D(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0 + detach, GL_TEXTURE_2D, 0, 0)
          detach += 1
        shadowDrawBuffers.clear()
        var location = 0
        while location < count do
          val logical = logicalTargets(location)
          val texture = if logical == 0 then shadowColor0 else if logical == 1 then shadowColor1 else 0
          glFramebufferTexture2D(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0 + location, GL_TEXTURE_2D, texture, 0)
          shadowDrawBuffers.put(if texture != 0 then GL_COLOR_ATTACHMENT0 + location else GL_NONE)
          location += 1
        shadowDrawBuffers.flip(); glDrawBuffers(shadowDrawBuffers)
      case _ =>
        glFramebufferTexture2D(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, GL_TEXTURE_2D, shadowColor0, 0)
        glFramebufferTexture2D(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT1, GL_TEXTURE_2D, shadowColor1, 0)
        shadowDrawBuffers.clear(); shadowDrawBuffers.put(GL_COLOR_ATTACHMENT0).put(GL_COLOR_ATTACHMENT1); shadowDrawBuffers.flip(); glDrawBuffers(shadowDrawBuffers)

  /** Lightmap sampler state plus the fixed-function texture matrix. Shader packs no longer
    * depend on that matrix, but the compatibility path still does, and a stale wrap mode
    * or an unbound unit samples black.
    */
  private def bindLightmapForDraw(): Unit =
    if lightmapTexture == 0 then lightmapTexture = createLightmapTexture()
    glActiveTexture(GL_TEXTURE1)
    glEnable(GL_TEXTURE_2D)
    glBindTexture(GL_TEXTURE_2D, lightmapTexture)
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR)
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR)
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE)
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE)
    glMatrixMode(GL_TEXTURE)
    glLoadIdentity()
    glTranslatef(0.03125f, 0.03125f, 0f)
    glScalef(1f / 256f, 1f / 256f, 1f)
    glMatrixMode(GL_MODELVIEW)
    glActiveTexture(GL_TEXTURE0)

  def beginWorld(
      fr: Float, fg: Float, fb: Float,
      fs: Float, fe: Float,
      dl: Float, time: Float,
      sd: (Float, Float, Float), md: (Float, Float, Float),
      cam: (Float, Float, Float), near: Float, far: Float,
      rain: Float, wetness: Float, underwater: Boolean
  ): Boolean =
    if worldPrograms.isEmpty then return false
    fogR = fr; fogG = fg; fogB = fb
    fogStart = fs; fogEnd = fe
    daylight = dl; worldTime = time
    rainAmount = rain.max(0f).min(1f)
    wetnessAmount = wetness.max(0f).min(1f)
    eyeInWater = if underwater then 1 else 0
    sunDir = sd; moonDir = md
    nearPlane = near; farPlane = far
    cameraPos = cam
    updateLightmapTexture(daylight)
    // gl_MultiTexCoord1 is supplied in Minecraft's familiar 0..240 light range.
    // Give compatibility-profile shaders the matching texture matrix that maps it
    // to texel centers in a 16x16 lightmap (roughly 0.03125..0.96875).
    bindLightmapForDraw()

    captureMatrix(GL_PROJECTION_MATRIX, projection)
    captureMatrix(GL_MODELVIEW_MATRIX, modelView)
    invertMatrix(projection, projectionInverse)
    invertMatrix(modelView, modelViewInverse)
    if !havePreviousFrame then
      copyMatrix(projection, previousProjection)
      copyMatrix(modelView, previousModelView)
      previousCameraPos = cameraPos
      havePreviousFrame = true

    usePass(0)
    bindLightmapForDraw()
    true

  def setPass(pass: Int): Unit = usePass(pass)

  def end(): Unit =
    if GL.getCapabilities.OpenGL20 then glUseProgram(0)
    activeProgram = null
    // Do not leak the Minecraft-style lightmap texture matrix into UI/viewmodel work.
    glActiveTexture(GL_TEXTURE1)
    glMatrixMode(GL_TEXTURE); glLoadIdentity()
    glMatrixMode(GL_MODELVIEW)
    glActiveTexture(GL_TEXTURE0)
    resetCompatibilityTextureUnits()
    if !sceneBound then commitPreviousFrame()

  private def commitPreviousFrame(): Unit =
    copyMatrix(projection, previousProjection)
    copyMatrix(modelView, previousModelView)
    previousCameraPos = cameraPos

  def destroy(): Unit =
    liveProgramIds.foreach(id => if id != 0 then glDeleteProgram(id))
    if noiseTexture != 0 then glDeleteTextures(noiseTexture)
    if lightmapTexture != 0 then glDeleteTextures(lightmapTexture)
    if normalAtlasFallback != 0 then glDeleteTextures(normalAtlasFallback)
    if specularAtlasFallback != 0 then glDeleteTextures(specularAtlasFallback)
    noiseTexture = 0; lightmapTexture = 0; normalAtlasFallback = 0; specularAtlasFallback = 0
    lastLightmapEnvironment = null
    worldPrograms = Map.empty
    shadowPrograms = Map.empty
    postPrograms = Vector.empty
    finalProgram = None
    activeProgram = null
    destroyTargets()
    destroyShadowTargets()

  private def usePass(pass: Int): Unit =
    val p = worldPrograms.getOrElse(pass, worldPrograms.getOrElse(0, null))
    if p == null then return
    if activeProgram == null || activeProgram.asInstanceOf[Program].id != p.id then
      glUseProgram(p.id)
      activeProgram = p
    if sceneBound then p.renderTargets.foreach(targets => bindSceneTargets(targets))
    applyWorldUniforms(p, pass)

  private def applyWorldUniforms(p: Program, pass: Int): Unit =
    val phase = BlockboxSky.dayPhase(celestialTime)
    val minecraftTicks = ((phase * 24000f).toInt % 24000 + 24000) % 24000
    val worldDay = math.floor(worldTime.toDouble / BlockboxSky.dayNightCycleSeconds.toDouble).toInt
    val cix = math.floor(cameraPos._1.toDouble).toInt
    val ciy = math.floor(cameraPos._2.toDouble).toInt
    val ciz = math.floor(cameraPos._3.toDouble).toInt
    set1i(p, "uAtlas", 0); set1i(p, "texture", 0); set1i(p, "gtexture", 0)
    bindTextureUniform(p, "lightmap", 1, lightmapTexture)
    bindTextureUniform(p, "normals", 2, normalAtlasFallback)
    bindTextureUniform(p, "specular", 3, specularAtlasFallback)
    set3f(p, "uFogColor", fogR, fogG, fogB); set3f(p, "fogColor", fogR, fogG, fogB); set3f(p, "skyColor", fogR, fogG, fogB)
    set1f(p, "uFogStart", fogStart); set1f(p, "uFogEnd", fogEnd)
    set1f(p, "uDaylight", daylight); set1f(p, "sunBrightness", daylight)
    set1f(p, "uNightDarkness", if nightShading then ((1f-daylight)/0.82f*0.65f).max(0f).min(0.65f) else 0f)
    set1f(p, "uTime", worldTime); set1f(p, "frameTimeCounter", worldTime % 3600f); set1f(p, "frameTime", frameTimeSeconds)
    set1i(p, "worldTime", minecraftTicks); set1i(p, "worldDay", worldDay)
    set1f(p, "rainStrength", rainAmount); set1f(p, "wetness", wetnessAmount); set1f(p, "screenBrightness", 1f); set1f(p, "nightVision", 0f); set1f(p, "blindness", 0f)
    set1i(p, "uPass", pass); set1i(p, "renderStage", pass)
    set1i(p, "frameCounter", (frameIndex % 720720L).toInt)
    set3f(p, "uSunDir", sunDir._1, sunDir._2, sunDir._3); set3f(p, "sunPosition", sunDir._1, sunDir._2, sunDir._3)
    set3f(p, "uMoonDir", moonDir._1, moonDir._2, moonDir._3); set3f(p, "moonPosition", moonDir._1, moonDir._2, moonDir._3)
    set3f(p, "cameraPosition", cameraPos._1, cameraPos._2, cameraPos._3)
    set3i(p, "cameraPositionInt", cix, ciy, ciz)
    set3f(p, "cameraPositionFract", cameraPos._1 - cix, cameraPos._2 - ciy, cameraPos._3 - ciz)
    set1f(p, "eyeAltitude", cameraPos._2)
    set1i(p, "isEyeInWater", eyeInWater)
    set2f(p, "eyeBrightness", 240f, (daylight.max(0.06f) * 240f).min(240f))
    set2f(p, "eyeBrightnessSmooth", 240f, (daylight.max(0.06f) * 240f).min(240f))
    set1i(p, "hideGUI", 0); set1i(p, "heldBlockLightValue", 0)
    set3f(p, "previousCameraPosition", previousCameraPos._1, previousCameraPos._2, previousCameraPos._3)
    set2f(p, "viewSize", frameWidth.toFloat, frameHeight.toFloat)
    set2f(p, "uResolution", frameWidth.toFloat, frameHeight.toFloat)
    set1f(p, "viewWidth", frameWidth.toFloat); set1f(p, "viewHeight", frameHeight.toFloat)
    set1f(p, "near", nearPlane); set1f(p, "far", farPlane)
    set1f(p, "aspectRatio", frameWidth.toFloat / frameHeight.max(1).toFloat)
    setMat4(p, "gbufferProjection", projection); setMat4(p, "projectionMatrix", projection)
    setMat4(p, "gbufferProjectionInverse", projectionInverse); setMat4(p, "projectionMatrixInverse", projectionInverse)
    setMat4(p, "gbufferModelView", modelView); setMat4(p, "modelViewMatrix", modelView)
    setMat4(p, "gbufferModelViewInverse", modelViewInverse); setMat4(p, "modelViewMatrixInverse", modelViewInverse)
    setMat4(p, "gbufferPreviousProjection", previousProjection)
    setMat4(p, "gbufferPreviousModelView", previousModelView)
    applyShadowUniforms(p)
    applyCustomUniforms(p)
    bindTextureUniform(p, "noisetex", 9, noiseTexture)
    glActiveTexture(GL_TEXTURE0)

  private def applyPostUniforms(p: Program, width: Int, height: Int, time: Float): Unit =
    set1i(p,"uAntialiasing",if antialiasing then 1 else 0)
    val phase = BlockboxSky.dayPhase(celestialTime)
    val minecraftTicks = ((phase * 24000f).toInt % 24000 + 24000) % 24000
    set1f(p, "frameTimeCounter", time % 3600f); set1f(p, "frameTime", frameTimeSeconds); set1f(p, "uTime", time)
    set1i(p, "worldTime", minecraftTicks)
    set1i(p, "frameCounter", (frameIndex % 720720L).toInt)
    set2f(p, "viewSize", width.toFloat, height.toFloat); set2f(p, "uResolution", width.toFloat, height.toFloat)
    set1f(p, "viewWidth", width.toFloat); set1f(p, "viewHeight", height.toFloat)
    set1f(p, "near", nearPlane); set1f(p, "far", farPlane)
    set1f(p, "aspectRatio", width.toFloat / height.max(1).toFloat)
    set3f(p, "cameraPosition", cameraPos._1, cameraPos._2, cameraPos._3)
    set3f(p, "previousCameraPosition", previousCameraPos._1, previousCameraPos._2, previousCameraPos._3)
    set3f(p, "sunPosition", sunDir._1, sunDir._2, sunDir._3)
    set3f(p, "moonPosition", moonDir._1, moonDir._2, moonDir._3)
    set1f(p, "sunBrightness", daylight)
    set1f(p, "rainStrength", rainAmount); set1f(p, "wetness", wetnessAmount); set1f(p, "screenBrightness", 1f); set1f(p, "nightVision", 0f); set1f(p, "blindness", 0f)
    set1f(p, "eyeAltitude", cameraPos._2)
    set1i(p, "isEyeInWater", eyeInWater)
    set2f(p, "eyeBrightness", 240f, (daylight.max(0.06f) * 240f).min(240f))
    set2f(p, "eyeBrightnessSmooth", 240f, (daylight.max(0.06f) * 240f).min(240f))
    set1i(p, "hideGUI", 0); set1i(p, "heldBlockLightValue", 0)
    set3f(p, "fogColor", fogR, fogG, fogB); set3f(p, "skyColor", fogR, fogG, fogB)
    set1f(p, "uFogStart", fogStart); set1f(p, "uFogEnd", fogEnd)
    setMat4(p, "gbufferProjection", projection); setMat4(p, "gbufferProjectionInverse", projectionInverse)
    setMat4(p, "gbufferModelView", modelView); setMat4(p, "gbufferModelViewInverse", modelViewInverse)
    setMat4(p, "gbufferPreviousProjection", previousProjection); setMat4(p, "gbufferPreviousModelView", previousModelView)
    applyShadowUniforms(p)

  private def applyShadowUniforms(p: Program): Unit =
    if shadowDepth != 0 then
      bindTextureUniform(p, "shadowtex0", 10, shadowDepth)
      bindTextureUniform(p, "shadowtex1", 10, shadowDepth)
      bindTextureUniform(p, "shadow", 10, shadowDepth)
      bindTextureUniform(p, "watershadow", 10, shadowDepth)
    if shadowColor0 != 0 then
      bindTextureUniform(p, "shadowcolor0", 11, shadowColor0)
      bindTextureUniform(p, "shadowcolor", 11, shadowColor0)
    if shadowColor1 != 0 then bindTextureUniform(p, "shadowcolor1", 12, shadowColor1)
    set1i(p, "shadowMapResolution", shadowTargetSize.max(1))
    set1f(p, "shadowDistance", activeShadowDistance)
    setMat4(p, "shadowProjection", shadowProjection); setMat4(p, "shadowProjectionInverse", shadowProjectionInverse)
    setMat4(p, "shadowModelView", shadowModelView); setMat4(p, "shadowModelViewInverse", shadowModelViewInverse)

  private def drawFullscreen(p: Program | Null, colors: Array[Int], depthTex: Int, width: Int, height: Int, time: Float): Unit =
    if p != null then
      val prog = p.asInstanceOf[Program]
      glUseProgram(prog.id)
      val color0 = if colors.nonEmpty then colors(0) else 0
      bindTextureUniform(prog, "colortex0", 0, color0); bindTextureUniform(prog, "gcolor", 0, color0); bindTextureUniform(prog, "uScene", 0, color0)
      bindTextureUniform(prog, "depthtex0", 1, depthTex); bindTextureUniform(prog, "depthtex1", 1, depthTex); bindTextureUniform(prog, "depthtex2", 1, depthTex); bindTextureUniform(prog, "uDepth", 1, depthTex)
      val color1 = if colors.length > 1 then colors(1) else sceneNormal
      val color2 = if colors.length > 2 then colors(2) else sceneMaterial
      bindTextureUniform(prog, "colortex1", 2, color1); bindTextureUniform(prog, "gdepth", 2, color1); bindTextureUniform(prog, "normals", 2, color1); bindTextureUniform(prog, "normaltex0", 2, color1)
      bindTextureUniform(prog, "colortex2", 3, color2); bindTextureUniform(prog, "gnormal", 3, color2); bindTextureUniform(prog, "materialtex0", 3, color2); bindTextureUniform(prog, "uMaterial", 3, color2)
      var colorIndex = 3
      while colorIndex < colors.length && colorIndex < gbufferCount do
        val tex = colors(colorIndex)
        val unit = colorIndex + 1
        bindTextureUniform(prog, s"colortex$colorIndex", unit, tex)
        if colorIndex == 3 then bindTextureUniform(prog, "composite", unit, tex)
        if colorIndex >= 4 && colorIndex <= 7 then bindTextureUniform(prog, s"gaux${colorIndex - 3}", unit, tex)
        colorIndex += 1
      bindTextureUniform(prog, "noisetex", 9, noiseTexture)
      applyPostUniforms(prog, width, height, time)
    else
      glUseProgram(0)
      // A pack may omit final. Fixed-function multitexturing multiplies every
      // enabled unit, so post samplers must be unbound before the color blit.
      resetCompatibilityTextureUnits()
      val color0 = if colors.nonEmpty then colors(0) else 0
      glActiveTexture(GL_TEXTURE0); glEnable(GL_TEXTURE_2D); glBindTexture(GL_TEXTURE_2D, color0)
      glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_MODULATE)

    glColor4f(1f, 1f, 1f, 1f)
    glBegin(GL_QUADS)
    glTexCoord2f(0f, 0f); glVertex2f(-1f, -1f)
    glTexCoord2f(1f, 0f); glVertex2f(1f, -1f)
    glTexCoord2f(1f, 1f); glVertex2f(1f, 1f)
    glTexCoord2f(0f, 1f); glVertex2f(-1f, 1f)
    glEnd()

  /** Final stage of finishFrame: resolve a scaled scene when enabled,
    * otherwise draw directly to the window.
    */
  private def drawFinalThroughFovea(p: Program | Null, colors: Array[Int], depthTex: Int, width: Int, height: Int, time: Float): Unit =
    val fovea = foveaHook
    if fovea != null && fovea.supportsFbos && fovea.enabled && fovea.bindTarget(width, height) then
      drawFullscreen(p, colors, depthTex, fovea.targetWidth, fovea.targetHeight, time)
      fovea.upsampleToWindow(width, height)
    else
      drawFullscreen(p, colors, depthTex, width, height, time)

  private def sceneColorArray(): Array[Int] =
    val out = new Array[Int](gbufferCount)
    if gbufferCount > 0 then out(0) = sceneColor
    if gbufferCount > 1 then out(1) = sceneNormal
    if gbufferCount > 2 then out(2) = sceneMaterial
    var i = 3
    while i < gbufferCount do
      out(i) = sceneExtraColors(i - 3)
      i += 1
    out

  /** Apply an Iris/OptiFine RENDERTARGETS or DRAWBUFFERS mapping to the scene FBO. */
  private def bindSceneTargets(targets: Array[Int]): Unit =
    if !sceneBound || sceneFbo == 0 || targets.isEmpty then return
    val count = math.min(targets.length, gbufferCount)
    postDrawBuffers.clear()
    var i = 0
    while i < count do
      val logical = targets(i)
      postDrawBuffers.put(if logical >= 0 && logical < gbufferCount then GL_COLOR_ATTACHMENT0 + logical else GL_NONE)
      i += 1
    postDrawBuffers.flip()
    glDrawBuffers(postDrawBuffers)

  /** Attach alternate logical buffers to physical output locations 0..N. The
    * returned logical indices are exactly the buffers that will flip after drawing.
    */
  private def bindPostTargets(targets: Array[Int], alternates: Array[Int]): Array[Int] =
    if postFbo == 0 || targets.isEmpty then return Array.empty[Int]
    glBindFramebuffer(GL_FRAMEBUFFER, postFbo)
    var detach = 0
    while detach < gbufferCount do
      glFramebufferTexture2D(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0 + detach, GL_TEXTURE_2D, 0, 0)
      detach += 1
    postDrawBuffers.clear()
    val written = mutable.ArrayBuffer.empty[Int]
    val count = math.min(targets.length, gbufferCount)
    var location = 0
    while location < count do
      val logical = targets(location)
      if logical >= 0 && logical < gbufferCount && alternates(logical) != 0 then
        glFramebufferTexture2D(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0 + location, GL_TEXTURE_2D, alternates(logical), 0)
        postDrawBuffers.put(GL_COLOR_ATTACHMENT0 + location)
        if !written.contains(logical) then written += logical
      else postDrawBuffers.put(GL_NONE)
      location += 1
    postDrawBuffers.flip()
    glDrawBuffers(postDrawBuffers)
    written.toArray

  private def generateRequestedMipmaps(p: Program, colors: Array[Int]): Unit =
    if p.mipmapBuffers.isEmpty then return
    p.mipmapBuffers.foreach { logical =>
      if logical >= 0 && logical < colors.length && colors(logical) != 0 then
        glActiveTexture(GL_TEXTURE0)
        glBindTexture(GL_TEXTURE_2D, colors(logical))
        glGenerateMipmap(GL_TEXTURE_2D)
    }
    glBindTexture(GL_TEXTURE_2D, 0)

  private def resetCompatibilityTextureUnits(): Unit =
    var unit = math.min(maxTextureUnits - 1, 12)
    while unit >= 0 do
      glActiveTexture(GL_TEXTURE0 + unit)
      glBindTexture(GL_TEXTURE_2D, 0)
      glDisable(GL_TEXTURE_2D)
      unit -= 1
    glActiveTexture(GL_TEXTURE0)

  private def bindTextureUniform(p: Program, name: String, unit: Int, tex: Int): Unit =
    if tex == 0 || unit < 0 || unit >= maxTextureUnits then return
    val u = p.uniform(name)
    if u >= 0 then
      glActiveTexture(GL_TEXTURE0 + unit)
      glEnable(GL_TEXTURE_2D)
      glBindTexture(GL_TEXTURE_2D, tex)
      glUniform1i(u, unit)

  private def set1i(p: Program, name: String, value: Int): Unit =
    val u = p.uniform(name); if u >= 0 then glUniform1i(u, value)

  private def set1f(p: Program, name: String, value: Float): Unit =
    val u = p.uniform(name); if u >= 0 then glUniform1f(u, value)

  private def set2f(p: Program, name: String, a: Float, b: Float): Unit =
    val u = p.uniform(name); if u >= 0 then glUniform2f(u, a, b)

  private def set3f(p: Program, name: String, a: Float, b: Float, c: Float): Unit =
    val u = p.uniform(name); if u >= 0 then glUniform3f(u, a, b, c)

  private def set3i(p: Program, name: String, a: Int, b: Int, c: Int): Unit =
    val u = p.uniform(name); if u >= 0 then glUniform3i(u, a, b, c)

  private def setMat4(p: Program, name: String, values: Array[Float]): Unit =
    val u = p.uniform(name)
    if u >= 0 then
      matrixUploadBuffer.clear(); matrixUploadBuffer.put(values); matrixUploadBuffer.flip()
      glUniformMatrix4fv(u, false, matrixUploadBuffer)

  private def reload(force: Boolean): Unit =
    val nextPackName = resolvePackName()
    val nextPackDir = resolvePackDir(nextPackName)
    val nextStamp = computeSourceStamp(nextPackName)
    if !force && nextPackName == activePackName && nextStamp == sourceStamp then return

    val built = mutable.ArrayBuffer.empty[Program]
    try
      val stages=mutable.HashMap.empty[String,Option[Program]]
      def optional(name: String): Option[Program]=stages.getOrElseUpdate(name,compileOptionalStage(name,nextPackDir))
      def firstStage(names: Seq[String]): Option[Program] =
        names.iterator.map(optional).collectFirst { case Some(p) => p }

      val terrain = firstStage(Seq("gbuffers_terrain_solid", "gbuffers_terrain")).getOrElse(compileStage("gbuffers_terrain", nextPackDir, fallbackVertex, fallbackFragment)); built += terrain
      val texturedOpt = firstStage(Seq("gbuffers_terrain_cutout", "gbuffers_textured_lit", "gbuffers_textured")); texturedOpt.foreach(built += _)
      val translucentOpt = firstStage(Seq("gbuffers_translucent", "gbuffers_textured_lit", "gbuffers_textured")); translucentOpt.foreach(p => if !built.exists(_.id == p.id) then built += p)
      val waterOpt = firstStage(Seq("gbuffers_water", "gbuffers_translucent")); waterOpt.foreach(p => if !built.exists(_.id == p.id) then built += p)
      val textured = texturedOpt.getOrElse(terrain)
      val translucent = translucentOpt.getOrElse(textured)
      val water = waterOpt.getOrElse(translucent)

      // Iris supplies a default shadow shader when a pack omits shadow.vsh/fsh.
      // Do the same for active external packs so composite shaders can still consume
      // a useful shadowtex0 instead of silently seeing texture 0.
      val explicitShadow=optional("shadow"); explicitShadow.foreach(built += _)
      val nextShadowResolution = resolveShadowResolution(nextPackDir)
      val nextShadowDistance = resolveShadowDistance(nextPackDir)
      val nextNoiseResolution = resolveNoiseResolution(nextPackDir)

      def numbered(prefix: String): Vector[String] = Vector(prefix) ++ (1 to 99).map(i => s"$prefix$i").toVector
      val postNames = numbered("deferred") ++ numbered("composite")
      val posts = postNames.flatMap { name =>
        val p = optional(name)
        p.foreach(built += _)
        p
      }
      val fin = optional("final"); fin.foreach(built += _)
      val shadowSamplers = Seq("shadowtex0", "shadowtex1", "shadow", "watershadow", "shadowcolor0", "shadowcolor1", "shadowcolor")
      val needsShadows = explicitShadow.nonEmpty || (Seq(terrain, textured, translucent, water) ++ posts ++ fin).exists(p => shadowSamplers.exists(name => p.uniform(name) >= 0))
      val shadowBase = explicitShadow.orElse {
        if needsShadows && nextPackName.nonEmpty then Some(compileStage("shadow", nextPackDir, fallbackShadowVertex, fallbackShadowFragment)) else None
      }
      shadowBase.foreach(p => if !built.exists(_.id == p.id) then built += p)
      val shadowSolidOpt = optional("shadow_solid"); shadowSolidOpt.foreach(p => if !built.exists(_.id == p.id) then built += p)
      val shadowCutoutOpt = optional("shadow_cutout"); shadowCutoutOpt.foreach(p => if !built.exists(_.id == p.id) then built += p)
      val nextShadowPrograms = shadowBase.toSeq.flatMap { base =>
        Seq(0 -> shadowSolidOpt.getOrElse(base), 1 -> shadowCutoutOpt.getOrElse(base))
      }.toMap

      val nextGbufferCount = discoverRequestedColorBufferCount(nextPackDir, built)
      val nextAlternateTargets = posts.flatMap(_.renderTargets.getOrElse(Array(0))).toSet
      val nextFormats = resolveColorFormats(nextPackDir)
      if desiredGbufferCount != nextGbufferCount || alternateTargets != nextAlternateTargets || !colorFormats.sameElements(nextFormats) then
        desiredGbufferCount = nextGbufferCount
        alternateTargets = nextAlternateTargets
        colorFormats = nextFormats
        // Force target reallocation on the next prepared frame; shader reloads stay
        // cheap when their requested attachment count did not change.
        destroyTargets()

      val oldIds = liveProgramIds
      glUseProgram(0); activeProgram=null
      havePreviousFrame=false
      worldPrograms = Map(0 -> terrain, 1 -> textured, 2 -> translucent, 3 -> water)
      shadowPrograms = if needsShadows then nextShadowPrograms else Map.empty
      if shadowResolution != nextShadowResolution then
        shadowResolution = nextShadowResolution
        destroyShadowTargets()
      configuredShadowDistance = nextShadowDistance
      if noiseTextureResolution != nextNoiseResolution then
        noiseTextureResolution = nextNoiseResolution
        if noiseTexture != 0 then glDeleteTextures(noiseTexture)
        noiseTexture = createNoiseTexture(noiseTextureResolution)
      postPrograms = posts
      finalProgram = fin
      activePackName = nextPackName
      sourceStamp = nextStamp
      failedSourceStamp = Long.MinValue
      val postCount = posts.length + (if fin.nonEmpty then 1 else 0)
      val shadowLabel = if shadowPrograms.nonEmpty then
        val dist = if nextShadowDistance > 0f then s", ${nextShadowDistance.toInt}b" else ""
        s"${nextShadowResolution}px shadows$dist"
      else ""
      val noiseLabel = if nextNoiseResolution != 256 then s"${nextNoiseResolution}px noise" else ""
      val details = Vector(
        if postCount > 0 then s"$postCount post pass${if postCount == 1 then "" else "es"}" else "",
        if nextGbufferCount > 3 then s"$nextGbufferCount MRTs" else "3 MRTs",
        shadowLabel,
        noiseLabel
      ).filter(_.nonEmpty)
      currentStatus = if nextPackName.nonEmpty then s"pack '$nextPackName' [Iris-compat bridge]${if details.nonEmpty then details.mkString(" (", ", ", ")") else ""}" else "built-in GLSL + hot reload"
      System.err.println(s"Blockbox shaders: $currentStatus")
      val newIds = liveProgramIds
      (oldIds -- newIds).filter(_ != 0).foreach(glDeleteProgram)
      (built.map(_.id).toSet -- newIds).filter(_ != 0).foreach(glDeleteProgram)
      if !postProcessing then destroyTargets()
    catch
      case e: Throwable =>
        built.map(_.id).toSet.filter(_ != 0).foreach(glDeleteProgram)
        currentStatus = if worldPrograms.nonEmpty then s"shader reload failed; keeping ${packName}" else "shader compile failed; fixed-function fallback"
        System.err.println(s"Blockbox shaders: $currentStatus: ${e.getMessage}")
        failedPackName = nextPackName
        failedSourceStamp = nextStamp

  private def liveProgramIds: Set[Int] =
    worldPrograms.values.map(_.id).toSet ++ shadowPrograms.values.map(_.id) ++ postPrograms.map(_.id) ++ finalProgram.map(_.id)

  private def resolvePackName(): String =
    selectedPackName.orElse(launchPack).getOrElse {
      val active = File(shaderpacksRoot, "active.txt")
      if active.isFile then
        try Files.readString(active.toPath, StandardCharsets.UTF_8).trim
        catch case _: Throwable => ""
      else if File(shaderpacksRoot, "default").isDirectory then "default"
      else ""
    }

  private def resolvePackDir(name: String): Option[File] =
    if name.isEmpty || name.equalsIgnoreCase("off") || name.equalsIgnoreCase("builtin") then None
    else
      val clean = name.replace('\\', '/').split('/').filter(part => part.nonEmpty && part != "." && part != "..").mkString("_")
      val dir = File(shaderpacksRoot, clean)
      if dir.isDirectory then Some(dir) else None

  private def resolveShadowResolution(pack: Option[File]): Int =
    val env = Option(System.getenv("BLOCKBOX_SHADOW_RESOLUTION")).flatMap(v => scala.util.Try(v.trim.toInt).toOption)
    env.map(_.max(256).min(4096)).getOrElse {
      val rx = raw"(?m)^\s*const\s+int\s+shadowMapResolution\s*=\s*([0-9]+)\s*;".r
      val requested = shaderDir(pack).flatMap { dir =>
        val stream = Files.walk(dir.toPath)
        try
          val it = stream.iterator()
          var found: Option[Int] = None
          while it.hasNext && found.isEmpty do
            val path = it.next()
            val name = path.getFileName.toString.toLowerCase
            if Files.isRegularFile(path) && (name.endsWith(".vsh") || name.endsWith(".fsh") || name.endsWith(".gsh") || name.endsWith(".vert") || name.endsWith(".frag") || name.endsWith(".geom")) then
              try found = rx.findFirstMatchIn(Files.readString(path, StandardCharsets.UTF_8)).map(_.group(1).toInt)
              catch case _: Throwable => ()
          found
        finally stream.close()
      }
      requested.getOrElse(2048).max(256).min(4096)
    }

  private def resolveShaderFloatConstant(pack: Option[File], constant: String): Option[Float] =
    val escaped = java.util.regex.Pattern.quote(constant)
    val rx = ("(?m)^\\s*const\\s+float\\s+" + escaped + "\\s*=\\s*([0-9]+(?:\\.[0-9]+)?)f?\\s*;").r
    shaderDir(pack).flatMap { dir =>
      val stream = Files.walk(dir.toPath)
      try
        val it = stream.iterator()
        var found: Option[Float] = None
        while it.hasNext && found.isEmpty do
          val path = it.next()
          val name = path.getFileName.toString.toLowerCase
          if Files.isRegularFile(path) && (name.endsWith(".vsh") || name.endsWith(".fsh") || name.endsWith(".gsh") || name.endsWith(".vert") || name.endsWith(".frag") || name.endsWith(".geom")) then
            try found = rx.findFirstMatchIn(Files.readString(path, StandardCharsets.UTF_8)).flatMap(m => scala.util.Try(m.group(1).toFloat).toOption)
            catch case _: Throwable => ()
        found
      finally stream.close()
    }

  private def resolveShaderIntConstant(pack: Option[File], constant: String): Option[Int] =
    val escaped = java.util.regex.Pattern.quote(constant)
    val rx = ("(?m)^\\s*const\\s+int\\s+" + escaped + "\\s*=\\s*([0-9]+)\\s*;").r
    shaderDir(pack).flatMap { dir =>
      val stream = Files.walk(dir.toPath)
      try
        val it = stream.iterator()
        var found: Option[Int] = None
        while it.hasNext && found.isEmpty do
          val path = it.next()
          val name = path.getFileName.toString.toLowerCase
          if Files.isRegularFile(path) && (name.endsWith(".vsh") || name.endsWith(".fsh") || name.endsWith(".gsh") || name.endsWith(".vert") || name.endsWith(".frag") || name.endsWith(".geom")) then
            try found = rx.findFirstMatchIn(Files.readString(path, StandardCharsets.UTF_8)).flatMap(m => scala.util.Try(m.group(1).toInt).toOption)
            catch case _: Throwable => ()
        found
      finally stream.close()
    }

  private def resolveShadowDistance(pack: Option[File]): Float =
    val env = Option(System.getenv("BLOCKBOX_SHADOW_DISTANCE")).flatMap(v => scala.util.Try(v.trim.toFloat).toOption)
    env.orElse(resolveShaderFloatConstant(pack, "shadowDistance")).map(_.max(32f).min(1024f)).getOrElse(0f)

  private def resolveNoiseResolution(pack: Option[File]): Int =
    val env = Option(System.getenv("BLOCKBOX_NOISE_RESOLUTION")).flatMap(v => scala.util.Try(v.trim.toInt).toOption)
    env.orElse(resolveShaderIntConstant(pack, "noiseTextureResolution")).map(_.max(16).min(2048)).getOrElse(256)

  private def shaderDir(pack: Option[File]): Option[File] = pack.map(p => File(p, "shaders")).filter(_.isDirectory)

  private def shaderSearchDirs(pack: Option[File]): Seq[File] =
    shaderDir(pack).toSeq.flatMap { dir =>
      val world0 = File(dir, "world0")
      if world0.isDirectory then Seq(world0, dir) else Seq(dir)
    }

  private def stageFile(pack: Option[File], base: String, kind: String): Option[File] =
    val extensions = kind match
      case "vertex" => Seq("vsh", "vert")
      case "fragment" => Seq("fsh", "frag")
      case "geometry" => Seq("gsh", "geom")
      case _ => Seq.empty
    shaderSearchDirs(pack).iterator.flatMap(dir => extensions.iterator.map(ext => File(dir, s"$base.$ext"))).find(_.isFile)

  private def compileOptionalStage(base: String, pack: Option[File]): Option[Program] =
    val vertex = stageFile(pack, base, "vertex")
    val fragment = stageFile(pack, base, "fragment")
    val geometry = stageFile(pack, base, "geometry")
    if vertex.isEmpty && fragment.isEmpty && geometry.isEmpty then None
    else
      val postStage = base == "final" || base.startsWith("composite") || base.startsWith("deferred")
      val shadowStage = base == "shadow" || base.startsWith("shadow_")
      val defaultVertex = if postStage then fullscreenVertex else if shadowStage then fallbackShadowVertex else fallbackVertex
      val defaultFragment = if postStage then passthroughFragment else if shadowStage then fallbackShadowFragment else fallbackFragment
      Some(compileStage(base, pack, vertex.map(readExpanded).getOrElse(defaultVertex), fragment.map(readExpanded).getOrElse(defaultFragment)))

  private def compileStage(base: String, pack: Option[File], defaultVertex: String, defaultFragment: String): Program =
    val vs = stageFile(pack, base, "vertex").map(readExpanded).getOrElse(defaultVertex)
    val fs = stageFile(pack, base, "fragment").map(readExpanded).getOrElse(defaultFragment)
    val gs = stageFile(pack, base, "geometry").map(readExpanded)
    if gs.nonEmpty && !GL.getCapabilities.OpenGL32 then
      throw IllegalStateException(s"$base uses a geometry shader but OpenGL 3.2 is unavailable")
    val stages = mutable.ArrayBuffer.empty[Int]
    var id = 0
    try
      stages += compileShader(GL_VERTEX_SHADER,vs,s"$base vertex")
      stages += compileShader(GL_FRAGMENT_SHADER,fs,s"$base fragment")
      gs.foreach(src => stages += compileShader(GL_GEOMETRY_SHADER,src,s"$base geometry"))
      id = glCreateProgram()
      stages.foreach(stage => glAttachShader(id,stage))
      glLinkProgram(id)
      if glGetProgrami(id,GL_LINK_STATUS) == 0 then throw IllegalStateException(s"$base link: ${glGetProgramInfoLog(id,16384)}")
      stages.foreach(stage => glDetachShader(id,stage))
      new Program(id,base,parseRenderTargets(fs),parseMipmapBuffers(fs))
    catch
      case e: Throwable =>
        if id != 0 then glDeleteProgram(id)
        throw e
    finally stages.foreach(glDeleteShader)

  private def parseRenderTargets(source: String): Option[Array[Int]] =
    val rt = raw"(?s)/\*\s*RENDERTARGETS\s*:\s*([0-9,\s]+)\*/".r.findFirstMatchIn(source).map { m =>
      m.group(1).split(',').iterator.map(_.trim).filter(_.nonEmpty).map(_.toInt).toArray
    }
    rt.orElse {
      raw"(?s)/\*\s*DRAWBUFFERS\s*:\s*([0-9]+)\s*\*/".r.findFirstMatchIn(source).map { m =>
        m.group(1).iterator.map(ch => ch.asDigit).toArray
      }
    }.filter(_.nonEmpty)

  private def parseMipmapBuffers(source: String): Array[Int] =
    val rx = raw"(?m)^\s*const\s+bool\s+colortex([0-9]+)MipmapEnabled\s*=\s*true\s*;".r
    rx.findAllMatchIn(source).map(_.group(1).toInt).filter(i => i >= 0 && i < MaxCompatColorBuffers).toArray.distinct

  private def compileShader(kind: Int, source: String, label: String): Int =
    val id = glCreateShader(kind)
    // Iris/OptiFine compatibility-profile packs frequently branch on the real GL
    // environment. Inject only facts Blockbox can truthfully provide; deliberately
    // do not pretend to be Iris or claim a Minecraft version.
    glShaderSource(id, injectCompatibilityDefines(source)); glCompileShader(id)
    if glGetShaderi(id, GL_COMPILE_STATUS) == 0 then
      val log = glGetShaderInfoLog(id, 16384)
      glDeleteShader(id)
      throw IllegalStateException(s"$label: $log")
    id


  private def glVersionMacro(raw: String): Int =
    val rx = raw"^\s*([0-9]+)\.([0-9]+)".r
    rx.findFirstMatchIn(Option(raw).getOrElse("")).map { m =>
      val minorText = m.group(2)
      val minor = if minorText.length == 1 then minorText.toInt * 10 else minorText.take(2).toInt
      m.group(1).toInt * 100 + minor
    }.getOrElse(0)

  private def injectCompatibilityDefines(source: String): String =
    val glVersion = glVersionMacro(glGetString(GL_VERSION))
    val glslVersion = glVersionMacro(glGetString(GL_SHADING_LANGUAGE_VERSION))
    val vendor = Option(glGetString(GL_VENDOR)).getOrElse("").toLowerCase
    val renderer = Option(glGetString(GL_RENDERER)).getOrElse("").toLowerCase
    val vendorMacro =
      if vendor.contains("nvidia") then "MC_GL_VENDOR_NVIDIA"
      else if vendor.contains("amd") then "MC_GL_VENDOR_AMD"
      else if vendor.contains("ati") then "MC_GL_VENDOR_ATI"
      else if vendor.contains("intel") then "MC_GL_VENDOR_INTEL"
      else if vendor.contains("mesa") then "MC_GL_VENDOR_MESA"
      else if vendor.contains("x.org") || vendor.contains("xorg") then "MC_GL_VENDOR_XORG"
      else "MC_GL_VENDOR_OTHER"
    val rendererMacro =
      if renderer.contains("geforce") then "MC_GL_RENDERER_GEFORCE"
      else if renderer.contains("quadro") then "MC_GL_RENDERER_QUADRO"
      else if renderer.contains("radeon") then "MC_GL_RENDERER_RADEON"
      else if renderer.contains("intel") then "MC_GL_RENDERER_INTEL"
      else if renderer.contains("gallium") then "MC_GL_RENDERER_GALLIUM"
      else if renderer.contains("mesa") || vendor.contains("mesa") then "MC_GL_RENDERER_MESA"
      else if renderer.contains("apple") then "MC_GL_RENDERER_APPLE"
      else "MC_GL_RENDERER_OTHER"
    val formatDefines = Seq("RGBA8"->GL_RGBA8,"RGBA16F"->GL_RGBA16F,"RGBA32F"->GL_RGBA32F,"RGB16F"->GL_RGB16F,"R16F"->GL_R16F,"R32F"->GL_R32F,"RG16F"->GL_RG16F,"RG32F"->GL_RG32F).map { case (name,value) => s"#ifndef $name\n#define $name $value\n#endif\n" }.mkString
    val defines = formatDefines +
      s"#define BLOCKBOX_SHADER_BRIDGE 1\n#define MC_GL_VERSION $glVersion\n#define MC_GLSL_VERSION $glslVersion\n#define $vendorMacro 1\n#define $rendererMacro 1\n"
    val lines = source.linesIterator.toVector
    val versionIndex = lines.indexWhere(_.trim.startsWith("#version"))
    if versionIndex >= 0 then
      (lines.take(versionIndex + 1) ++ Vector(defines) ++ lines.drop(versionIndex + 1)).mkString("\n") + "\n"
    else defines + source

  private def shaderSandbox(file: File): Path =
    var current: File | Null = file.getParentFile
    var found: File | Null = null
    while current != null && found == null do
      if current.asInstanceOf[File].getName == "shaders" then found = current
      else current = current.asInstanceOf[File].getParentFile
    (if found != null then found.asInstanceOf[File] else file.getParentFile).toPath.toAbsolutePath.normalize()

  private def readExpanded(file: File): String =
    blockbox.render.ShaderSources.read(file.toPath,shaderSandbox(file))

  private def discoverRequestedColorBufferCount(packDir: Option[File], programs: Iterable[Program]): Int =
    var maxLogical = 2 // Blockbox core color/normal/material
    programs.foreach { p =>
      p.renderTargets.foreach(_.foreach { logical =>
        if logical >= 0 && logical < MaxCompatColorBuffers then maxLogical = math.max(maxLogical, logical)
      })
    }
    // Also scan sampler/writer references. This catches packs that consume colortex7
    // without explicitly naming it in RENDERTARGETS and legacy gl_FragData writers.
    val colortex = "(?i)\\bcolortex([0-7])(?:Format)?\\b".r
    val fragData = "gl_FragData\\s*\\[\\s*([0-7])\\s*\\]".r
    val gaux = "(?i)\\bgaux([1-4])\\b".r
    packDir.foreach { pack =>
      val shaders = File(pack, "shaders")
      if shaders.isDirectory then
        val stream = Files.walk(shaders.toPath)
        try
          val it = stream.iterator()
          while it.hasNext do
            val file = it.next().toFile
            val name = file.getName.toLowerCase
            if file.isFile && (name.endsWith(".vsh") || name.endsWith(".fsh") || name.endsWith(".vert") || name.endsWith(".frag") || name.endsWith(".gsh") || name.endsWith(".geom")) then
              val src = Files.readString(file.toPath, StandardCharsets.UTF_8)
              colortex.findAllMatchIn(src).foreach(m => maxLogical = math.max(maxLogical, m.group(1).toInt))
              fragData.findAllMatchIn(src).foreach(m => maxLogical = math.max(maxLogical, m.group(1).toInt))
              gaux.findAllMatchIn(src).foreach(m => maxLogical = math.max(maxLogical, 3 + m.group(1).toInt))
        finally stream.close()
    }
    (maxLogical + 1).max(3).min(MaxCompatColorBuffers)

  private def computeSourceStamp(packName: String): Long =
    // Mix every file's metadata. Taking only the newest mtime missed deletions and
    // edits hidden by another file's future timestamp; active.txt is a selection,
    // not a shader source, and must not cause a second compile after each UI click.
    var stamp=0x6a09e667f3bcc909L
    def include(file: File): Unit=
      if file.isFile then
        var h=file.lastModified()^(file.length()*0x9e3779b97f4a7c15L)^file.getPath.hashCode.toLong
        h=(h^(h>>>30))*0xbf58476d1ce4e5b9L
        h=(h^(h>>>27))*0x94d049bb133111ebL
        stamp^=h^(h>>>31)
    include(fallbackVertexFile); include(fallbackFragmentFile)
    resolvePackDir(packName).foreach { pack =>
      val shaders=File(pack,"shaders")
      if shaders.isDirectory then
        val stream=Files.walk(shaders.toPath)
        try
          val iterator=stream.iterator()
          while iterator.hasNext do include(iterator.next().toFile)
        finally stream.close()
      include(File(pack,"pack.properties"))
    }
    stamp

  private def fileStamp(file: File): Long = if file.isFile then file.lastModified() else 0L

  private def resolveColorFormats(pack: Option[File]): Array[Int] =
    val result = Array(GL_RGBA16F,GL_RGBA16F,GL_RGBA8,GL_RGBA8,GL_RGBA8,GL_RGBA8,GL_RGBA8,GL_RGBA8)
    val formats = Map("RGBA8"->GL_RGBA8,"RGBA16F"->GL_RGBA16F,"RGBA32F"->GL_RGBA32F,"RGB16F"->GL_RGB16F,"R16F"->GL_R16F,"R32F"->GL_R32F,"RG16F"->GL_RG16F,"RG32F"->GL_RG32F)
    val pattern = raw"const\s+int\s+colortex([0-7])Format\s*=\s*([A-Z0-9]+)\s*;".r
    shaderDir(pack).foreach { dir =>
      val stream = Files.walk(dir.toPath)
      try
        import scala.jdk.CollectionConverters.*
        stream.iterator().asScala.filter(p => Files.isRegularFile(p) && Seq(".fsh",".frag",".glsl").exists(p.toString.endsWith)).foreach { path =>
          val source = readExpanded(path.toFile)
          pattern.findAllMatchIn(source).foreach { m => formats.get(m.group(2)).foreach(format => result(m.group(1).toInt) = format) }
        }
      finally stream.close()
    }
    result

  private def ensureTargets(width: Int, height: Int): Unit =
    if sceneFbo != 0 && targetW == width && targetH == height then return
    destroyTargets()
    targetW = width; targetH = height

    val supportedDrawBuffers = math.min(glGetInteger(GL_MAX_DRAW_BUFFERS), glGetInteger(GL_MAX_COLOR_ATTACHMENTS))
    gbufferCount = math.max(3, math.min(desiredGbufferCount, math.min(MaxCompatColorBuffers, supportedDrawBuffers)))
    sceneDrawBuffers.clear()
    var db = 0
    while db < gbufferCount do
      sceneDrawBuffers.put(GL_COLOR_ATTACHMENT0 + db)
      db += 1
    sceneDrawBuffers.flip()

    sceneColor = createColorTexture(width, height, hdr = true, format = colorFormats(0))
    sceneDepth = createDepthTexture(width, height)
    sceneNormal = createColorTexture(width, height, hdr = true, smooth = false, format = colorFormats(1))
    sceneMaterial = createColorTexture(width, height, hdr = false, smooth = false, format = colorFormats(2))
    sceneExtraColors = Array.fill[Int](MaxCompatColorBuffers - 3)(0)
    var extra = 0
    while extra < gbufferCount - 3 do
      sceneExtraColors(extra) = createColorTexture(width, height, hdr = false, format = colorFormats(extra+3))
      extra += 1
    sceneFbo = glGenFramebuffers(); glBindFramebuffer(GL_FRAMEBUFFER, sceneFbo)
    glFramebufferTexture2D(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, GL_TEXTURE_2D, sceneColor, 0)
    glFramebufferTexture2D(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT1, GL_TEXTURE_2D, sceneNormal, 0)
    glFramebufferTexture2D(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT2, GL_TEXTURE_2D, sceneMaterial, 0)
    extra = 0
    while extra < gbufferCount - 3 do
      glFramebufferTexture2D(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT3 + extra, GL_TEXTURE_2D, sceneExtraColors(extra), 0)
      extra += 1
    glFramebufferTexture2D(GL_FRAMEBUFFER, GL_DEPTH_ATTACHMENT, GL_TEXTURE_2D, sceneDepth, 0)
    sceneDrawBuffers.rewind()
    glDrawBuffers(sceneDrawBuffers)
    checkFramebuffer("scene")

    postAlternateColors = Array.fill[Int](MaxCompatColorBuffers)(0)
    var alt = 0
    while alt < gbufferCount do
      if alternateTargets.contains(alt) then
        postAlternateColors(alt) = createColorTexture(width, height, hdr = alt <= 1, smooth = alt == 0, format = colorFormats(alt))
      alt += 1
    postFbo = glGenFramebuffers()

    glBindTexture(GL_TEXTURE_2D, 0)
    glBindFramebuffer(GL_FRAMEBUFFER, 0)

  private def ensureShadowTarget(): Unit =
    if shadowFbo != 0 && shadowTargetSize == shadowResolution then return
    destroyShadowTargets()
    shadowTargetSize = shadowResolution
    shadowDepth = createDepthTexture(shadowTargetSize, shadowTargetSize)
    shadowColor0 = createColorTexture(shadowTargetSize, shadowTargetSize, hdr = false)
    shadowColor1 = createColorTexture(shadowTargetSize, shadowTargetSize, hdr = false)
    shadowFbo = glGenFramebuffers(); glBindFramebuffer(GL_FRAMEBUFFER, shadowFbo)
    glFramebufferTexture2D(GL_FRAMEBUFFER, GL_DEPTH_ATTACHMENT, GL_TEXTURE_2D, shadowDepth, 0)
    glFramebufferTexture2D(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, GL_TEXTURE_2D, shadowColor0, 0)
    glFramebufferTexture2D(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT1, GL_TEXTURE_2D, shadowColor1, 0)
    shadowDrawBuffers.clear(); shadowDrawBuffers.put(GL_COLOR_ATTACHMENT0).put(GL_COLOR_ATTACHMENT1); shadowDrawBuffers.flip()
    glDrawBuffers(shadowDrawBuffers)
    checkFramebuffer("shadow")
    glBindFramebuffer(GL_FRAMEBUFFER, 0)

  private def destroyShadowTargets(): Unit =
    if shadowDepth != 0 then glDeleteTextures(shadowDepth)
    if shadowColor0 != 0 then glDeleteTextures(shadowColor0)
    if shadowColor1 != 0 then glDeleteTextures(shadowColor1)
    if shadowFbo != 0 then glDeleteFramebuffers(shadowFbo)
    shadowDepth = 0; shadowColor0 = 0; shadowColor1 = 0; shadowFbo = 0; shadowTargetSize = 0; shadowPassBound = false

  private def loadDirectionalLightView(light: (Float, Float, Float)): Unit =
    val lx = light._1; val ly = light._2; val lz = light._3
    val len = math.sqrt((lx * lx + ly * ly + lz * lz).toDouble).toFloat.max(0.0001f)
    val fx = -lx / len; val fy = -ly / len; val fz = -lz / len
    val upx = 0f; val upy = if math.abs(fy) > 0.97f then 0f else 1f; val upz = if math.abs(fy) > 0.97f then 1f else 0f
    var sx = fy * upz - fz * upy
    var sy = fz * upx - fx * upz
    var sz = fx * upy - fy * upx
    val sl = math.sqrt((sx * sx + sy * sy + sz * sz).toDouble).toFloat.max(0.0001f)
    sx /= sl; sy /= sl; sz /= sl
    val ux = sy * fz - sz * fy
    val uy = sz * fx - sx * fz
    val uz = sx * fy - sy * fx
    val m = Array[Float](
      sx, ux, -fx, 0f,
      sy, uy, -fy, 0f,
      sz, uz, -fz, 0f,
      0f, 0f, 0f, 1f
    )
    // Quantize the light-space center to shadow texels to reduce crawling when
    // the camera moves by less than a shadow-map pixel.
    val texel = activeShadowDistance*2f/shadowTargetSize.max(1)
    val lightX = sx*cameraPos._1+sy*cameraPos._2+sz*cameraPos._3
    val lightY = ux*cameraPos._1+uy*cameraPos._2+uz*cameraPos._3
    m(12) = lightX - math.floor(lightX/texel).toFloat*texel
    m(13) = lightY - math.floor(lightY/texel).toFloat*texel
    shadowMatrixBuffer.clear(); shadowMatrixBuffer.put(m); shadowMatrixBuffer.flip()
    glLoadMatrixf(shadowMatrixBuffer)

  private def createColorTexture(width: Int, height: Int, hdr: Boolean, smooth: Boolean = true, format: Int = 0): Int =
    val tex = glGenTextures(); glBindTexture(GL_TEXTURE_2D, tex)
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, if smooth then GL_LINEAR else GL_NEAREST)
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, if smooth then GL_LINEAR else GL_NEAREST)
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE)
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE)
    val internal = if format != 0 then format else if hdr then GL_RGBA16F else GL_RGBA8
    glTexImage2D(GL_TEXTURE_2D, 0, internal, width, height, 0, GL_RGBA, GL_FLOAT, 0L)
    tex

  private def createSolidTexture(r: Int, g: Int, b: Int, a: Int): Int =
    val tex = glGenTextures(); glBindTexture(GL_TEXTURE_2D, tex)
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST)
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST)
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT)
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT)
    val pixel = BufferUtils.createByteBuffer(4)
    pixel.put((r & 0xff).toByte).put((g & 0xff).toByte).put((b & 0xff).toByte).put((a & 0xff).toByte).flip()
    glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA8, 1, 1, 0, GL_RGBA, GL_UNSIGNED_BYTE, pixel)
    glBindTexture(GL_TEXTURE_2D, 0)
    tex

  private def createLightmapTexture(): Int =
    val tex = glGenTextures(); glBindTexture(GL_TEXTURE_2D, tex)
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR)
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR)
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE)
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE)
    val empty = BufferUtils.createByteBuffer(16 * 16 * 4)
    glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA8, 16, 16, 0, GL_RGBA, GL_UNSIGNED_BYTE, empty)
    glBindTexture(GL_TEXTURE_2D, 0)
    tex

  private def updateLightmapTexture(day: Float): Unit =
    val environment = lightEnvironment.copy(daylight = day, darkNights = nightShading)
    if lightmapTexture == 0 || environment == lastLightmapEnvironment then return
    lastLightmapEnvironment = environment
    val pixels = lightmapPixels
    pixels.clear()
    var sky = 0
    while sky < 16 do
      var block = 0
      while block < 16 do
        val (rr, gg, bb) = BlockboxLightmap.color(sky.toFloat / 15f, block.toFloat / 15f, environment)
        pixels.put((rr * 255f).round.toByte).put((gg * 255f).round.toByte).put((bb * 255f).round.toByte).put(0xff.toByte)
        block += 1
      sky += 1
    pixels.flip()
    glActiveTexture(GL_TEXTURE1)
    glBindTexture(GL_TEXTURE_2D, lightmapTexture)
    glTexSubImage2D(GL_TEXTURE_2D, 0, 0, 0, 16, 16, GL_RGBA, GL_UNSIGNED_BYTE, pixels)
    glBindTexture(GL_TEXTURE_2D, 0)
    glActiveTexture(GL_TEXTURE0)

  private def createNoiseTexture(size: Int): Int =
    val tex = glGenTextures(); glBindTexture(GL_TEXTURE_2D, tex)
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST)
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST)
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT)
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT)
    val pixels = BufferUtils.createByteBuffer(size * size * 4)
    var state = 0x6d2b79f5
    var i = 0
    while i < size * size do
      state ^= state << 13; state ^= state >>> 17; state ^= state << 5
      val value = (state & 0xff).toByte
      pixels.put(value).put(value).put(value).put(0xff.toByte)
      i += 1
    pixels.flip()
    glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA8, size, size, 0, GL_RGBA, GL_UNSIGNED_BYTE, pixels)
    glBindTexture(GL_TEXTURE_2D, 0)
    tex

  private def createDepthTexture(width: Int, height: Int): Int =
    val tex = glGenTextures(); glBindTexture(GL_TEXTURE_2D, tex)
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST)
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST)
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE)
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE)
    glTexImage2D(GL_TEXTURE_2D, 0, GL_DEPTH_COMPONENT24, width, height, 0, GL_DEPTH_COMPONENT, GL_FLOAT, 0L)
    tex

  private def checkFramebuffer(label: String): Unit =
    val status = glCheckFramebufferStatus(GL_FRAMEBUFFER)
    if status != GL_FRAMEBUFFER_COMPLETE then throw IllegalStateException(f"$label framebuffer incomplete: 0x$status%04x")

  private def destroyTargets(): Unit =
    if sceneColor != 0 then glDeleteTextures(sceneColor)
    if sceneDepth != 0 then glDeleteTextures(sceneDepth)
    if sceneNormal != 0 then glDeleteTextures(sceneNormal)
    if sceneMaterial != 0 then glDeleteTextures(sceneMaterial)
    sceneExtraColors.filter(_ != 0).foreach(glDeleteTextures)
    sceneExtraColors = Array.fill[Int](MaxCompatColorBuffers - 3)(0)
    postAlternateColors.filter(_ != 0).foreach(glDeleteTextures)
    postAlternateColors = Array.fill[Int](MaxCompatColorBuffers)(0)
    if sceneFbo != 0 then glDeleteFramebuffers(sceneFbo)
    if postFbo != 0 then glDeleteFramebuffers(postFbo)
    sceneFbo = 0; sceneColor = 0; sceneDepth = 0; sceneNormal = 0; sceneMaterial = 0
    postFbo = 0
    targetW = 0; targetH = 0; sceneBound = false

  private def captureMatrix(kind: Int, out: Array[Float]): Unit =
    matrixReadBuffer.clear(); glGetFloatv(kind, matrixReadBuffer); matrixReadBuffer.rewind()
    var i = 0
    while i < 16 do
      out(i) = matrixReadBuffer.get(i)
      i += 1

  private def copyMatrix(from: Array[Float], to: Array[Float]): Unit =
    java.lang.System.arraycopy(from, 0, to, 0, 16)

  private def identityMatrix(): Array[Float] =
    val m = Array.fill[Float](16)(0f)
    m(0) = 1f; m(5) = 1f; m(10) = 1f; m(15) = 1f
    m

  /** General 4x4 inverse, column-major like OpenGL. Falls back to identity if the
    * matrix is singular rather than feeding NaNs into third-party shader packs.
    */
  private def invertMatrix(m: Array[Float], out: Array[Float]): Unit =
    val inv = new Array[Float](16)
    inv(0) = m(5)*m(10)*m(15)-m(5)*m(11)*m(14)-m(9)*m(6)*m(15)+m(9)*m(7)*m(14)+m(13)*m(6)*m(11)-m(13)*m(7)*m(10)
    inv(4) = -m(4)*m(10)*m(15)+m(4)*m(11)*m(14)+m(8)*m(6)*m(15)-m(8)*m(7)*m(14)-m(12)*m(6)*m(11)+m(12)*m(7)*m(10)
    inv(8) = m(4)*m(9)*m(15)-m(4)*m(11)*m(13)-m(8)*m(5)*m(15)+m(8)*m(7)*m(13)+m(12)*m(5)*m(11)-m(12)*m(7)*m(9)
    inv(12) = -m(4)*m(9)*m(14)+m(4)*m(10)*m(13)+m(8)*m(5)*m(14)-m(8)*m(6)*m(13)-m(12)*m(5)*m(10)+m(12)*m(6)*m(9)
    inv(1) = -m(1)*m(10)*m(15)+m(1)*m(11)*m(14)+m(9)*m(2)*m(15)-m(9)*m(3)*m(14)-m(13)*m(2)*m(11)+m(13)*m(3)*m(10)
    inv(5) = m(0)*m(10)*m(15)-m(0)*m(11)*m(14)-m(8)*m(2)*m(15)+m(8)*m(3)*m(14)+m(12)*m(2)*m(11)-m(12)*m(3)*m(10)
    inv(9) = -m(0)*m(9)*m(15)+m(0)*m(11)*m(13)+m(8)*m(1)*m(15)-m(8)*m(3)*m(13)-m(12)*m(1)*m(11)+m(12)*m(3)*m(9)
    inv(13) = m(0)*m(9)*m(14)-m(0)*m(10)*m(13)-m(8)*m(1)*m(14)+m(8)*m(2)*m(13)+m(12)*m(1)*m(10)-m(12)*m(2)*m(9)
    inv(2) = m(1)*m(6)*m(15)-m(1)*m(7)*m(14)-m(5)*m(2)*m(15)+m(5)*m(3)*m(14)+m(13)*m(2)*m(7)-m(13)*m(3)*m(6)
    inv(6) = -m(0)*m(6)*m(15)+m(0)*m(7)*m(14)+m(4)*m(2)*m(15)-m(4)*m(3)*m(14)-m(12)*m(2)*m(7)+m(12)*m(3)*m(6)
    inv(10) = m(0)*m(5)*m(15)-m(0)*m(7)*m(13)-m(4)*m(1)*m(15)+m(4)*m(3)*m(13)+m(12)*m(1)*m(7)-m(12)*m(3)*m(5)
    inv(14) = -m(0)*m(5)*m(14)+m(0)*m(6)*m(13)+m(4)*m(1)*m(14)-m(4)*m(2)*m(13)-m(12)*m(1)*m(6)+m(12)*m(2)*m(5)
    inv(3) = -m(1)*m(6)*m(11)+m(1)*m(7)*m(10)+m(5)*m(2)*m(11)-m(5)*m(3)*m(10)-m(9)*m(2)*m(7)+m(9)*m(3)*m(6)
    inv(7) = m(0)*m(6)*m(11)-m(0)*m(7)*m(10)-m(4)*m(2)*m(11)+m(4)*m(3)*m(10)+m(8)*m(2)*m(7)-m(8)*m(3)*m(6)
    inv(11) = -m(0)*m(5)*m(11)+m(0)*m(7)*m(9)+m(4)*m(1)*m(11)-m(4)*m(3)*m(9)-m(8)*m(1)*m(7)+m(8)*m(3)*m(5)
    inv(15) = m(0)*m(5)*m(10)-m(0)*m(6)*m(9)-m(4)*m(1)*m(10)+m(4)*m(2)*m(9)+m(8)*m(1)*m(6)-m(8)*m(2)*m(5)
    val det = m(0)*inv(0)+m(1)*inv(4)+m(2)*inv(8)+m(3)*inv(12)
    if math.abs(det) < 1e-8f then copyMatrix(identityMatrix(), out)
    else
      val d = 1f / det
      var i = 0
      while i < 16 do
        out(i) = inv(i) * d
        i += 1

  private def readFile(file: File, fallback: String): String =
    if file.isFile then Files.readString(file.toPath, StandardCharsets.UTF_8) else fallback

  private lazy val fallbackVertex: String = readFile(fallbackVertexFile,
    """#version 120
      |uniform mat4 gbufferModelViewInverse; uniform int isEyeInWater;
      |varying vec4 vColor; varying vec2 vUv; varying vec2 vLightUv; varying vec3 vNormal; varying float vFogDepth; varying vec3 vWorldPos; varying float vSkyLight; varying float vEmission;
      |void main(){vec4 eye=gl_ModelViewMatrix*gl_Vertex;gl_Position=gl_ProjectionMatrix*eye;vColor=gl_Color;vUv=gl_MultiTexCoord0.xy;vec2 lm=gl_MultiTexCoord1.xy;vLightUv=max(lm.x,lm.y)>1.5?clamp(lm,vec2(0.0),vec2(240.0))*(1.0/256.0)+vec2(0.03125):clamp(lm,vec2(0.0),vec2(1.0));vNormal=normalize(gl_Normal);vFogDepth=isEyeInWater!=0?length(eye.xyz):length((gbufferModelViewInverse*eye).xz);vWorldPos=gl_Vertex.xyz;vSkyLight=clamp(gl_MultiTexCoord1.y/240.0,0.0,1.0);vEmission=step(1.75,max(gl_Color.r,max(gl_Color.g,gl_Color.b)));}
      |""".stripMargin)

  private lazy val fallbackFragment: String = readFile(fallbackFragmentFile,
    """#version 120
      |uniform sampler2D uAtlas;uniform sampler2D lightmap;uniform vec3 uFogColor;uniform float uFogStart;uniform float uFogEnd;uniform float uDaylight;uniform float uTime;uniform int uPass;uniform vec3 uSunDir;uniform vec3 uMoonDir;
      |varying vec4 vColor;varying vec2 vUv;varying vec2 vLightUv;varying vec3 vNormal;varying float vFogDepth;varying vec3 vWorldPos;varying float vSkyLight;varying float vEmission;
      |float sat(float x){return clamp(x,0.0,1.0);}void main(){vec4 t=texture2D(uAtlas,vUv);vec4 b=t*vec4(min(vColor.rgb,vec3(1.0)),vColor.a);vec3 n=normalize(vNormal);float night=1.0-sat((uDaylight-.18)/.82);float s=max(dot(n,normalize(uSunDir)),0.0);float m=max(dot(n,normalize(uMoonDir)),0.0);vec3 l=texture2D(lightmap,vLightUv).rgb*mix(1.0,.86,vSkyLight*uDaylight)+(vec3(1.0,.9,.74)*s*uDaylight*.18+vec3(.45,.58,.86)*m*night*.05)*vSkyLight*vSkyLight*vSkyLight;b.rgb*=l;if(uPass==3){float w=sin((vWorldPos.x+vWorldPos.z)*.75+uTime*1.4)*.5+.5;b.rgb=mix(b.rgb,vec3(.08,.34,.62)*l,.05+w*.035);}b.rgb=mix(b.rgb,t.rgb*vec3(1.22,1.04,.66),vEmission);float f=sat((uFogEnd-vFogDepth)/max(uFogEnd-uFogStart,.001));b.rgb=mix(uFogColor,b.rgb,f);gl_FragColor=b;}
      |""".stripMargin)

  private val fallbackShadowVertex =
    """#version 120
      |varying vec2 vUv;
      |varying vec4 vColor;
      |void main() {
      |  gl_Position = ftransform();
      |  vUv = (gl_TextureMatrix[0] * gl_MultiTexCoord0).xy;
      |  vColor = gl_Color;
      |}
      |""".stripMargin

  private val fallbackShadowFragment =
    """#version 120
      |uniform sampler2D gtexture;
      |varying vec2 vUv;
      |varying vec4 vColor;
      |void main() {
      |  vec4 albedo = texture2D(gtexture, vUv) * vColor;
      |  if (max(vColor.r, max(vColor.g, vColor.b)) >= 1.75) discard;
      |  if (albedo.a < 0.10) discard;
      |  gl_FragColor = vec4(albedo.rgb, 1.0);
      |}
      |""".stripMargin

  private val fullscreenVertex =
    """#version 120
      |varying vec2 vUv;
      |void main(){gl_Position=gl_Vertex;vUv=gl_MultiTexCoord0.xy;}
      |""".stripMargin

  private val passthroughFragment =
    """#version 120
      |uniform sampler2D colortex0;varying vec2 vUv;
      |void main(){gl_FragColor=texture2D(colortex0,vUv);}
      |""".stripMargin
