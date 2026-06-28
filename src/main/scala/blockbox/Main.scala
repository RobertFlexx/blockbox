//> using scala "3.8.3"
//> using dep "org.lwjgl:lwjgl:3.4.1"
//> using dep "org.lwjgl:lwjgl-glfw:3.4.1"
//> using dep "org.lwjgl:lwjgl-opengl:3.4.1"
//> using dep "org.lwjgl:lwjgl-stb:3.4.1"
//> using dep "org.apache.groovy:groovy:5.0.6"

package blockbox

import blockbox.net.BlockboxNet
import blockbox.io.BlockboxFiles
import org.lwjgl.BufferUtils
import org.lwjgl.glfw.GLFW.*
import org.lwjgl.glfw.GLFWErrorCallback
import org.lwjgl.glfw.GLFWFramebufferSizeCallback
import org.lwjgl.glfw.GLFWKeyCallback
import org.lwjgl.glfw.GLFWCharCallback
import org.lwjgl.glfw.GLFWScrollCallback
import org.lwjgl.opengl.GL
import org.lwjgl.opengl.GL11.*
import org.lwjgl.opengl.GL15.*
import org.lwjgl.system.MemoryUtil.NULL
import groovy.lang.GroovyClassLoader
import groovy.lang.Closure

import java.nio.FloatBuffer
import java.nio.charset.StandardCharsets
import java.security.MessageDigest
import java.util.jar.JarFile
import java.util.function.Consumer
import java.io.*
import java.net.*
import java.util.concurrent.LinkedBlockingQueue
import scala.collection.mutable.{ArrayBuffer, Queue, HashSet}
import scala.math.*
import scala.util.Random

@main def runBlockbox(args: String*): Unit =
  if args.exists(_ == "--check-mods") then Blockbox().checkModsOnly()
  else Blockbox().run()

final class Blockbox extends BlockboxInventory with BlockboxAudio with BlockboxCommands:
  private val width = 1280
  private val height = 720
  private var framebufferWidth = width
  private var framebufferHeight = height
  private var window = 0L
  private var screen = Screen.MainMenu
  private var vsync = true
  private var fastMove = false
  protected var soundEnabled = true
  private var fogDensity = 1.6f
  protected var gameMode = GameMode.Survival
  private var lastTime = 0.0
  private var lastFrameTime = 0.0
  protected var camera = Vec3(96f, 48f, 130f)
  protected var velocity = Vec3(0f, 0f, 0f)
  protected var onGround = false
  private var yaw = 0f
  private var pitch = 0f
  private var lastMouseX = width / 2.0
  private var lastMouseY = height / 2.0
  private var firstMouse = true
  private var leftWasDown = false
  private var rightWasDown = false
  private var menuLeftWasDown = false
  private var modGuiLeftWasDown = false
  private var modUiCursorMode = false
  private var modLastMouseX = 0f
  private var modLastMouseY = 0f
  private var modLeftDownNow = false
  private var modLeftClickedThisFrame = false
  private var breakingBlock: (Int, Int, Int) | Null = null
  private var breakingProgress = 0f
  protected val placeableBlocks = Array(Block.Grass, Block.Dirt, Block.Stone, Block.Sand, Block.Cactus, Block.Wood, Block.Planks, Block.BirchPlanks, Block.PinePlanks, Block.AcaciaPlanks, Block.Leaves, Block.BirchWood, Block.BirchLeaves, Block.PineWood, Block.PineLeaves, Block.AcaciaWood, Block.AcaciaLeaves, Block.Brick, Block.Glass, Block.Snow, Block.Clay, Block.Coal, Block.Copper, Block.IronOre, Block.GoldOre, Block.Diamond, Block.RainbowBlock, Block.Furnace)
  protected val inventory = Array.fill(Block.values.length)(0)
  protected val hotbarBlocks: Array[Block] = Array.fill(10)(Block.Air)
  protected val hotbarCounts: Array[Int] = Array.fill(10)(0)
  protected val maxStackSize = 64
  protected var selectedBlock = 0
  protected var heldInventoryBlock: Block = Block.Air
  protected var heldInventoryCount = 0
  protected var heldFromHotbar = false
  private var catalogScroll = 0
  private var craftingScroll = 0
  protected val craftGridBlocks: Array[Block] = Array.fill(9)(Block.Air)
  protected var furnaceInput: Block = Block.Air
  protected var furnaceFuel: Block = Block.Air
  protected var furnaceProgress = 0f
  protected var furnaceFuelRemaining = 0f
  protected var furnaceOutput: Block = Block.Air
  protected var furnaceOutputCount = 0
  private var debugMode = false
  private var wireframeMode = false
  private var showChunkBorders = false
  protected var gameServer: GameServer = null
  protected var gameClient: GameClient = null
  protected var playerName = "Player"
  protected val remotePlayers = scala.collection.mutable.HashMap.empty[String, RemotePlayer]
  // Multiplayer clients must not load local chunk files for the host seed. BLOC packets can
  // arrive before the matching chunk has streamed in, so keep them here and apply them when
  // that chunk is created. This prevents seed-correct clients from looking different just
  // because old local chunk saves or early packets were used/lost.
  private val pendingNetworkBlocks = scala.collection.mutable.HashMap.empty[(Int, Int, Int), Block]
  // Every BLOC packet seen during a multiplayer session is kept here, even after the
  // target chunk unloads. Without this, client-only worlds regenerated from seed and
  // lost remote edits when walking away and returning.
  private val networkBlockOverrides = scala.collection.mutable.HashMap.empty[(Int, Int, Int), Block]
  private val networkWaterLevelOverrides = scala.collection.mutable.HashMap.empty[(Int, Int, Int), Byte]
  private val worldExtraMagic = 0x42425831 // "BBX1": optional tail after old world.dat fields
  private val worldExtraVersion = 2
  protected val knownPlayerNames = scala.collection.mutable.HashSet.empty[String]
  private val playerColors = scala.collection.mutable.HashMap.empty[String, Int]
  private val oppedPlayerNames = scala.collection.mutable.HashSet.empty[String]
  private var localColorId = 0
  private val playerColorPalette: Array[(Float, Float, Float)] = Array(
    (0.22f, 0.46f, 0.95f), // blue
    (0.95f, 0.28f, 0.22f), // red
    (0.20f, 0.78f, 0.35f), // green
    (0.95f, 0.76f, 0.20f), // gold
    (0.62f, 0.36f, 0.94f), // purple
    (0.95f, 0.44f, 0.82f), // pink
    (0.12f, 0.80f, 0.80f), // cyan
    (1.00f, 0.50f, 0.18f), // orange
    (0.80f, 0.82f, 0.88f), // silver
    (0.52f, 0.88f, 0.18f)  // lime
  )
  private var hostNameFocused = true
  private var joinNameFocused = true
  private var joinStatusMessage = "Enter host IP. You can use IP:port; default port is 25565."
  private var joinStatusError = false
  private var hostStatusMessage = "Hosts on 0.0.0.0:25565. Share your LAN/ZeroTier IP."
  private var hostStatusError = false
  private var serverHost = "localhost"
  private var serverPort = 25565
  protected var multiplayerMode = false
  private var joinIpInput = ""
  private var lastPosSend = 0.0
  private var lastMultiplayerHeartbeat = 0.0
  private var renderDistance = 6 // chunks
  private val minRenderDistance = 2
  private val maxRenderDistance = 18
  private def renderDistanceBlocks: Int = renderDistance * Terrain.chunkSize
  protected var worldSeed = 0L
  private var createWorldMode = GameMode.Survival
  private var createWorldCheats = false
  protected var worldCheatsEnabled = false
  protected var terrainGen = TerrainGenerator(0L)
  private var chunks = scala.collection.mutable.AnyRefMap.empty[(Int, Int), Chunk]
  private var textureAtlas: TextureAtlas | Null = null
  private var selectedTexturePackId = loadSelectedTexturePackId()
  private var resourcePacks: List[TexturePack] = Nil
  private var resourcesReturnTo: Screen = Screen.MainMenu
  private var resourcesScroll = 0
  protected var playerHealth = 20f
  protected var playerFood = 20f
  protected val maxPlayerHealth = 20f
  protected val maxPlayerFood = 20f
  private val healthRegenDelay = 7.0f
  private val healthRegenRate = 0.85f
  private var timeSinceLastDamage = healthRegenDelay
  private var fallPeakY = camera.y
  private var cactusDamageCooldown = 0f
  private var errorCallback: GLFWErrorCallback | Null = null
  protected var worldName = "World"
  private var worldNameInput = "New World"
  private var createWorldNameFocused = true
  private var customSeedInput = ""
  private var enterCustomSeed = false
  private var loadWorldSelection = 0
  private var loadWorldScroll = 0
  private var hostUseSavedWorld = false
  private var hostWorldSelection = 0
  private var hostWorldScroll = 0
  private var saveDirectory: String = ""
  private var settingsReturnTo: Screen = Screen.MainMenu
  private var pauseEscReturnsToGame = true
  private var chatOpen = false
  protected var chatInput = ""
  private var suppressNextChatChar = false
  protected val chatMessages = ArrayBuffer.empty[(String, Float)]
  private val chatHistory = ArrayBuffer.empty[String]
  private var chatHistoryIndex = -1
  protected var commandSuggestionIndex = 0
  protected val modManager = ModManager(this)
  private var modsScreenScroll = 0
  private val sandFallQueue = Queue.empty[(Int, Int, Int)]
  private val maxSandUpdatesPerFrame = 6
  // Dynamic water is queue-driven. Natural generated water is dormant and costs
  // nothing per tick until a nearby block update wakes it, which is the important
  // trick for Minecraft-like water without beach/ocean microstutters.
  private val waterLevels = scala.collection.mutable.HashMap.empty[(Int, Int, Int), Byte]
  private val waterFlowQueue = Queue.empty[(Int, Int, Int)]
  private val waterFlowQueued = HashSet.empty[(Int, Int, Int)]
  private var waterFlowTimer = 0f
  private val waterFlowInterval = 0.05f
  private val maxWaterUpdatesPerFrame = 36
  private val maxQueuedWaterCells = 12000
  private var fov = 70f
  private var fullscreen = false
  private var windowedX = 0; private var windowedY = 0
  private var windowedW = 1280; private var windowedH = 720
  private val fallingSandParticles = ArrayBuffer.empty[(Float, Float, Float, Float)]
  private val sandParticleLifetime = 0.25f
  private var sandParticleTimer = 0f
  protected var timeOverride: Option[Float] = None
  protected var flyEnabled = false
  private var lastSpacePressTime = 0.0
  private val doubleTapInterval = 0.3
  private var wasSpaceDown = false
  private val bubbleParticles = ArrayBuffer.empty[(Float, Float, Float, Float)]
  private var sliderActive: String | Null = null
  private val dirtyChunks = scala.collection.mutable.Set.empty[(Int, Int)]
  private val dirtyChunksForSave = scala.collection.mutable.Set.empty[(Int, Int)]
  private val terrainHeightCache = scala.collection.mutable.HashMap.empty[Long, Int]
  private var starPositions: Array[(Float, Float, Float, Float)] = null
  // Threaded chunk generation
  private val chunkBuildQueue = new java.util.concurrent.ConcurrentLinkedQueue[Chunk]()
  private val chunkUploadQueue = new java.util.concurrent.ConcurrentLinkedQueue[Chunk]()
  private val chunkCreateQueue = new java.util.concurrent.ConcurrentLinkedQueue[(Int, Int, Int)]()
  private val chunkReadyQueue = new java.util.concurrent.ConcurrentLinkedQueue[(Int, Chunk)]()
  private val pendingChunkCreates = java.util.concurrent.ConcurrentHashMap.newKeySet[(Int, Int)]()
  @volatile private var chunkGenRunning = false
  @volatile private var chunkStreamGeneration = 0
  private var chunkGenPool: java.util.concurrent.ExecutorService = null

  // High-quality seed source. Using currentTimeMillis() alone made quick restarts and
  // repeated New World clicks look suspiciously identical, and Scala Int/hashCode seeds
  // collapsed custom text seeds down to only 32 bits. Keep this self-contained so new
  // worlds, hosted worlds, and the Random Seed button all use the same behavior.
  private val seedRng = new java.security.SecureRandom()

  private def freshWorldSeed(): Long =
    BlockboxWorld.freshWorldSeed(seedRng, System.identityHashCode(this).toLong)

  private def seedFromText(text: String): Long =
    BlockboxWorld.seedFromText(text, freshWorldSeed())

  private def worldFolderNameForSeed(seed: Long): String =
    BlockboxWorld.worldFolderNameForSeed(seed)

  private def applyWorldSeed(seed: Long, freshWorldName: Boolean): Unit =
    worldSeed = if seed == 0L then freshWorldSeed() else seed
    terrainGen = TerrainGenerator(worldSeed)
    terrainHeightCache.clear()
    if freshWorldName then worldName = worldFolderNameForSeed(worldSeed)

  private def chooseCreateSeed(): Long =
    if enterCustomSeed && customSeedInput.nonEmpty then seedFromText(customSeedInput) else freshWorldSeed()

  private def previewRandomSeed(): Unit =
    customSeedInput = ""
    applyWorldSeed(freshWorldSeed(), freshWorldName = false)

  private def sanitizeWorldName(raw: String): String =
    BlockboxWorld.sanitizeWorldName(raw)

  private def uniqueWorldFolderName(raw: String): String =
    BlockboxWorld.uniqueWorldFolderName(raw)

  private def gameRootDir: java.io.File =
    sys.env.get("BLOCKBOX_PROJECT_ROOT").map(java.io.File(_)).filter(_.isDirectory).getOrElse(java.io.File("."))

  private def settingsFile: java.io.File = java.io.File("blockbox-settings.properties")

  private def loadSelectedTexturePackId(): String =
    val file = settingsFile
    if !file.isFile then "default"
    else
      try
        val props = java.util.Properties()
        val in = java.io.FileInputStream(file)
        try props.load(in) finally in.close()
        Option(props.getProperty("texturePack")).map(_.trim).filter(_.nonEmpty).getOrElse("default")
      catch case _: Exception => "default"

  private def saveSelectedTexturePackId(): Unit =
    try
      val props = java.util.Properties()
      props.setProperty("texturePack", selectedTexturePackId)
      BlockboxFiles.ensureDirectory(settingsFile.getAbsoluteFile.getParentFile.toPath)
      val out = java.io.ByteArrayOutputStream()
      props.store(out, "Blockbox settings")
      BlockboxFiles.writeAtomic(settingsFile.toPath, stream => stream.write(out.toByteArray))
    catch case e: Exception => System.err.println(s"Texture pack setting save failed: $e")

  private def defaultTexturePackDir: java.io.File = java.io.File(gameRootDir, "assets/textures")

  private def customTexturePackRoots: List[java.io.File] =
    List(java.io.File(gameRootDir, "resourcepacks"), java.io.File("resourcepacks")).distinctBy(_.getAbsolutePath)

  private def discoverResourcePacks(): List[TexturePack] =
    val builtIns = List(
      TexturePack("default", "Default Textures", "Kokonico", Some(defaultTexturePackDir)),
      TexturePack("legacy", "Legacy Textures", "RobertFlexx", None, legacy = true)
    )
    val custom = customTexturePackRoots.flatMap { root =>
      val dirs = Option(root.listFiles()).getOrElse(Array.empty[java.io.File]).filter(_.isDirectory).toList
      dirs.map { dir =>
        val safeId = "custom:" + dir.getName.trim.toLowerCase.replaceAll("[^a-z0-9._-]+", "-")
        val propsFile = java.io.File(dir, "pack.properties")
        val props = java.util.Properties()
        if propsFile.isFile then
          try
            val in = java.io.FileInputStream(propsFile)
            try props.load(in) finally in.close()
          catch case _: Exception => ()
        val name = Option(props.getProperty("name")).map(_.trim).filter(_.nonEmpty).getOrElse(dir.getName)
        val author = Option(props.getProperty("author")).map(_.trim).filter(_.nonEmpty).getOrElse("Custom")
        TexturePack(safeId, name, author, Some(dir))
      }
    }.distinctBy(_.id).sortBy(_.name.toLowerCase)
    builtIns ++ custom

  private def refreshResourcePacks(): Unit =
    resourcePacks = discoverResourcePacks()
    if !resourcePacks.exists(_.id == selectedTexturePackId) then selectedTexturePackId = "default"
    resourcesScroll = resourcesScroll.max(0).min((resourcePacks.length - 1).max(0))

  private def selectedTexturePack: TexturePack =
    if resourcePacks.isEmpty then refreshResourcePacks()
    resourcePacks.find(_.id == selectedTexturePackId).getOrElse(resourcePacks.head)

  private def applyTexturePack(pack: TexturePack): Unit =
    if pack.id == selectedTexturePackId && textureAtlas != null then return
    selectedTexturePackId = pack.id
    saveSelectedTexturePackId()
    val old = textureAtlas
    textureAtlas = TextureAtlas(pack)
    if old != null then old.destroy()
    chunks.values.foreach { chunk =>
      chunk.markDirtyMesh()
      chunk.meshReady = false
      queueChunkMesh(chunk)
    }

  def run(): Unit =
    try
      initWindow()
      modManager.loadAll()
      applyWorldSeed(freshWorldSeed(), freshWorldName = false)
      val spawn = findSpawn()
      camera = spawn
      fallPeakY = camera.y
      lastTime = glfwGetTime()
      startChunkGenThread()
      while !glfwWindowShouldClose(window) do loop()
    finally
      stopChunkGenThread()
      modManager.shutdown()
      saveWorld()
      chunks.values.foreach(_.dispose())
      chunks.clear()
      val atlas = textureAtlas
      if atlas != null then atlas.destroy()
      if window != NULL then glfwDestroyWindow(window)
      glfwTerminate()
      val cb = errorCallback
      if cb != null then cb.free()

  def checkModsOnly(): Unit =
    modManager.loadAll()
    modManager.loadedMods.foreach { m =>
      println(s"Blockbox mod check: ${m.id} ${m.version} loaded=${m.loaded} status=${m.status} side=${m.sideLabel}")
    }
    modManager.shutdown()

  private def initWindow(): Unit =
    errorCallback = GLFWErrorCallback.createPrint(System.err)
    glfwSetErrorCallback(errorCallback)
    BlockboxPlatform.configureGlfwPlatform()
    if !glfwInit() then throw RuntimeException("GLFW initialization failed")
    def resetWindowHints(samples: Int, requestLegacy21: Boolean, requestCore33: Boolean = false): Unit =
      glfwDefaultWindowHints()
      glfwWindowHint(GLFW_CLIENT_API, GLFW_OPENGL_API)
      if requestLegacy21 then
        glfwWindowHint(GLFW_CONTEXT_VERSION_MAJOR, 2)
        glfwWindowHint(GLFW_CONTEXT_VERSION_MINOR, 1)
      else if requestCore33 then
        glfwWindowHint(GLFW_CONTEXT_VERSION_MAJOR, 3)
        glfwWindowHint(GLFW_CONTEXT_VERSION_MINOR, 3)
        glfwWindowHint(GLFW_OPENGL_PROFILE, GLFW_OPENGL_CORE_PROFILE)
        glfwWindowHint(GLFW_OPENGL_FORWARD_COMPAT, GLFW_TRUE)
      glfwWindowHint(GLFW_RESIZABLE, GLFW_TRUE)
      glfwWindowHint(GLFW_SAMPLES, samples)
      glfwWindowHint(GLFW_SCALE_TO_MONITOR, GLFW_TRUE)
      glfwWindowHintString(GLFW_WAYLAND_APP_ID, "blockbox")

    def destroyWindowIfNeeded(): Unit =
      if window != NULL then
        val failedWindow = window
        window = NULL
        if failedWindow != NULL then glfwDestroyWindow(failedWindow)

    def tryCreateCapabilities(samples: Int, requestLegacy21: Boolean = false, requestCore33: Boolean = false): Boolean =
      destroyWindowIfNeeded()
      resetWindowHints(samples, requestLegacy21, requestCore33)
      window = glfwCreateWindow(width, height, "Blockbox - Scala Voxel Sandbox", NULL, NULL)
      if window == NULL then return false
      glfwMakeContextCurrent(window)
      if glfwGetCurrentContext() != window then return false
      glfwSwapInterval(if vsync then 1 else 0)
      glfwShowWindow(window)
      try
        GL.createCapabilities()
        true
      catch case _: IllegalStateException => false

    val contextOk =
      tryCreateCapabilities(4, true) ||
      tryCreateCapabilities(0, true) ||
      tryCreateCapabilities(0, false) ||
      tryCreateCapabilities(0, requestCore33 = true) ||
      tryCreateCapabilities(0, requestLegacy21 = true, requestCore33 = false)
    if !contextOk then
      destroyWindowIfNeeded()
      val platform = sys.env.getOrElse("GLFW_PLATFORM", "auto")
      val session = sys.env.getOrElse("XDG_SESSION_TYPE", "unknown")
      val info = s"GLFW_PLATFORM=$platform XDG_SESSION_TYPE=$session"
      System.err.println(s"Blockbox: OpenGL context setup failed after trying all fallbacks. $info")
      throw IllegalStateException(s"OpenGL context setup failed. $info Try: (1) Software fallback in the launcher display-backend menu, (2) disabling gamemoderun/GameMode, (3) leaving GLFW_PLATFORM unset for auto-detect on Wayland, or (4) updating your GPU driver.")
    glEnable(0x809D)
    System.err.println(s"Blockbox: OpenGL context created — vendor=${glGetString(GL_VENDOR)} renderer=${glGetString(GL_RENDERER)} version=${glGetString(GL_VERSION)}")
    refreshResourcePacks()
    textureAtlas = TextureAtlas(selectedTexturePack)
    windowedW = width; windowedH = height
    queryWindowPos()
    updateFramebufferSize()
    glfwSetFramebufferSizeCallback(window, new GLFWFramebufferSizeCallback {
      override def invoke(window: Long, w: Int, h: Int): Unit =
        framebufferWidth = w.max(1)
        framebufferHeight = h.max(1)
    })
    glEnable(GL_DEPTH_TEST)
    glDisable(GL_CULL_FACE)
    glEnable(GL_BLEND)
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA)
    glCullFace(GL_BACK)
    glClearColor(0.52f, 0.72f, 0.95f, 1f)
    glfwSetKeyCallback(window, new GLFWKeyCallback {
      override def invoke(window: Long, key: Int, scancode: Int, action: Int, mods: Int): Unit =
        if action == GLFW_PRESS then onKey(key)
    })
    glfwSetCharCallback(window, new GLFWCharCallback {
      override def invoke(window: Long, codepoint: Int): Unit = onChar(codepoint)
    })
    glfwSetScrollCallback(window, new GLFWScrollCallback {
      override def invoke(window: Long, xoffset: Double, yoffset: Double): Unit = onScroll(xoffset, yoffset)
    })

  private def queryWindowPos(): Unit =
    val prevCb = glfwSetErrorCallback(null)
    try
      val wx = BufferUtils.createIntBuffer(1); val wy = BufferUtils.createIntBuffer(1)
      glfwGetWindowPos(window, wx, wy)
      windowedX = wx.get(0); windowedY = wy.get(0)
    finally glfwSetErrorCallback(prevCb)

  protected def findSpawn(): Vec3 =
    val x = 0; val z = 0
    val h = terrainGen.heightAt(x, z)
    Vec3(x + 0.5f, (h + 3).max(Terrain.seaLevel + 3).toFloat, z + 0.5f)

  private def loop(): Unit =
    val now = glfwGetTime()
    lastFrameTime = (now - lastTime).min(0.1)
    val dt = lastFrameTime.toFloat.min(0.05f)
    lastTime = now
    if screen == Screen.Playing then
      updateGame(dt)
      modManager.fireWorldTick(dt)
    else
      // In multiplayer, menu/pause screens are client-side only.
      // Keep sockets, chat, player positions, and chunk uploads alive so pausing one window
      // does not freeze/desync everyone else. Singleplayer still fully pauses here.
      if shouldRunMultiplayerBackgroundTick then updateMultiplayerBackgroundTick(dt)
      handleMenuMouse()
    render()
    glfwSwapBuffers(window)
    glfwPollEvents()

  private def onChar(codepoint: Int): Unit =
    if chatOpen then
      if suppressNextChatChar then
        suppressNextChatChar = false
      else if codepoint >= 32 && codepoint <= 126 then
        chatInput += codepoint.toChar
        commandSuggestionIndex = 0
    else if screen == Screen.CreateWorld then
      if codepoint >= 32 && codepoint <= 126 then
        val ch = codepoint.toChar
        if createWorldNameFocused then worldNameInput = (worldNameInput + ch).take(40)
        else if enterCustomSeed then customSeedInput = (customSeedInput + ch).take(64)
    else if screen == Screen.JoinGame then
      if codepoint >= 32 && codepoint <= 126 then
        val ch = codepoint.toChar
        if joinNameFocused then appendPlayerNameChar(ch) else joinIpInput = (joinIpInput + ch).take(64)
    else if screen == Screen.HostGame then
      if codepoint >= 32 && codepoint <= 126 then appendPlayerNameChar(codepoint.toChar)

  private def onScroll(xoffset: Double, yoffset: Double): Unit =
    val delta = if yoffset > 0 then -1 else if yoffset < 0 then 1 else 0
    if delta == 0 then return
    screen match
      case Screen.Playing =>
        selectedBlock = Math.floorMod(selectedBlock + delta, hotbarBlocks.length)
      case Screen.Catalog =>
        catalogScroll = (catalogScroll + delta).max(0)
      case Screen.Mods =>
        modsScreenScroll = (modsScreenScroll + delta).max(0)
      case Screen.Resources =>
        resourcesScroll = (resourcesScroll + delta).max(0).min((resourcePacks.length - 1).max(0))
      case _ => ()

  private def onKey(key: Int): Unit =
    if key == GLFW_KEY_F11 then toggleFullscreen()
    else screen match
      case Screen.MainMenu =>
        if key == GLFW_KEY_ENTER then screen = Screen.CreateWorld
        else if key == GLFW_KEY_S then screen = Screen.Settings
        else if key == GLFW_KEY_M then screen = Screen.Mods
        else if key == GLFW_KEY_R then openResources(Screen.MainMenu)
        else if key == GLFW_KEY_L then openLoadWorldMenu()
        else if key == GLFW_KEY_ESCAPE then glfwSetWindowShouldClose(window, true)
      case Screen.Mods =>
        if key == GLFW_KEY_ESCAPE || key == GLFW_KEY_ENTER then screen = Screen.MainMenu
        else if key == GLFW_KEY_UP then modsScreenScroll = (modsScreenScroll - 1).max(0)
        else if key == GLFW_KEY_DOWN then modsScreenScroll += 1
      case Screen.Resources =>
        if key == GLFW_KEY_ESCAPE || key == GLFW_KEY_ENTER then screen = resourcesReturnTo
        else if key == GLFW_KEY_UP then resourcesScroll = (resourcesScroll - 1).max(0)
        else if key == GLFW_KEY_DOWN then resourcesScroll = (resourcesScroll + 1).min((resourcePacks.length - 1).max(0))
      case Screen.CreateWorld =>
        if key == GLFW_KEY_ENTER then startNewWorld()
        else if key == GLFW_KEY_R then
          previewRandomSeed()
        else if key == GLFW_KEY_ESCAPE then
          enterCustomSeed = false
          screen = Screen.MainMenu
        else if key == GLFW_KEY_TAB then
          createWorldNameFocused = !createWorldNameFocused
          if !createWorldNameFocused then enterCustomSeed = true
        else if key == GLFW_KEY_N then createWorldNameFocused = true
        else if key == GLFW_KEY_C then
          enterCustomSeed = true
          createWorldNameFocused = false
        else if key == GLFW_KEY_M then createWorldMode = if createWorldMode == GameMode.Survival then GameMode.Creative else GameMode.Survival
        else if key == GLFW_KEY_H then createWorldCheats = !createWorldCheats
        else if key == GLFW_KEY_BACKSPACE then
          if createWorldNameFocused && worldNameInput.nonEmpty then worldNameInput = worldNameInput.init
          else if !createWorldNameFocused && customSeedInput.nonEmpty then customSeedInput = customSeedInput.init
      case Screen.LoadWorld =>
        val saves = worldSaveDirs
        if key == GLFW_KEY_ESCAPE then screen = Screen.MainMenu
        else if key == GLFW_KEY_ENTER then loadSelectedWorld()
        else if key == GLFW_KEY_UP then loadWorldSelection = (loadWorldSelection - 1).max(0)
        else if key == GLFW_KEY_DOWN then loadWorldSelection = (loadWorldSelection + 1).min((saves.length - 1).max(0))
      case Screen.Settings =>
        if key == GLFW_KEY_ESCAPE then
          sliderActive = null
          screen = settingsReturnTo
          if settingsReturnTo == Screen.Playing then
            glfwSetInputMode(window, GLFW_CURSOR, GLFW_CURSOR_DISABLED)
            firstMouse = true
        else if key == GLFW_KEY_F then fastMove = !fastMove
        else if key == GLFW_KEY_V then
          vsync = !vsync
          glfwSwapInterval(if vsync then 1 else 0)
        else if key == GLFW_KEY_LEFT || key == GLFW_KEY_MINUS then changeRenderDistance(-1)
        else if key == GLFW_KEY_RIGHT || key == GLFW_KEY_EQUAL then changeRenderDistance(1)
        else if key == GLFW_KEY_M && canUseCheatAuthority && worldCheatsEnabled then toggleGameMode()
        else if key == GLFW_KEY_P then pauseEscReturnsToGame = !pauseEscReturnsToGame
        else if key == GLFW_KEY_R then openResources(Screen.Settings)
      case Screen.Playing =>
        if chatOpen then
          if key == GLFW_KEY_ENTER then
            submitChat()
          else if key == GLFW_KEY_ESCAPE then
            chatOpen = false; chatInput = ""; chatHistoryIndex = -1
          else if key == GLFW_KEY_BACKSPACE && chatInput.nonEmpty then
            chatInput = chatInput.init
            commandSuggestionIndex = 0
          else if key == GLFW_KEY_TAB then
            applyCommandSuggestion()
          else if key == GLFW_KEY_UP then
            val suggestions = commandSuggestions(chatInput)
            if suggestions.nonEmpty then commandSuggestionIndex = (commandSuggestionIndex - 1 + suggestions.length) % suggestions.length
            else if chatHistory.nonEmpty then
              chatHistoryIndex = (chatHistoryIndex - 1).max(0)
              chatInput = chatHistory(chatHistory.length - 1 - chatHistoryIndex)
          else if key == GLFW_KEY_DOWN then
            val suggestions = commandSuggestions(chatInput)
            if suggestions.nonEmpty then commandSuggestionIndex = (commandSuggestionIndex + 1) % suggestions.length
            else if chatHistoryIndex > 0 then
              chatHistoryIndex -= 1
              chatInput = chatHistory(chatHistory.length - 1 - chatHistoryIndex)
            else
              chatHistoryIndex = -1; chatInput = ""
        else if key == GLFW_KEY_ESCAPE then leaveGame(Screen.Paused)
        else if key == GLFW_KEY_E then
          if gameMode == GameMode.Creative then openCatalog() else openInventory()
        else if key == GLFW_KEY_SLASH || key == GLFW_KEY_T then
          chatOpen = true
          chatInput = if key == GLFW_KEY_SLASH then "/" else ""
          chatHistoryIndex = -1
          suppressNextChatChar = true
        else if key >= GLFW_KEY_1 && key <= GLFW_KEY_9 then selectedBlock = key - GLFW_KEY_1
        else if key == GLFW_KEY_0 then selectedBlock = 9
        else if key == GLFW_KEY_M && canUseCheatAuthority && worldCheatsEnabled then toggleGameMode()
        else if key == GLFW_KEY_F3 then debugMode = !debugMode
        else if key == GLFW_KEY_F4 then
          wireframeMode = !wireframeMode
          if wireframeMode then glPolygonMode(GL_FRONT_AND_BACK, GL_LINE) else glPolygonMode(GL_FRONT_AND_BACK, GL_FILL)
        else if key == GLFW_KEY_F5 then showChunkBorders = !showChunkBorders
        else if key == GLFW_KEY_F8 then modSetCursorMode(!modUiCursorMode)
        else if key == GLFW_KEY_F2 then
          settingsReturnTo = Screen.Playing
          screen = Screen.Settings
          glfwSetInputMode(window, GLFW_CURSOR, GLFW_CURSOR_NORMAL)
      case Screen.JoinGame =>
        if key == GLFW_KEY_ENTER then
          val ip = if joinIpInput.trim.isEmpty then "127.0.0.1" else joinIpInput.trim
          joinStatusError = false
          joinStatusMessage = s"Connecting to $ip ..."
          joinGame(ip)
        else if key == GLFW_KEY_ESCAPE then screen = Screen.MainMenu
        else if key == GLFW_KEY_TAB then joinNameFocused = !joinNameFocused
        else if key == GLFW_KEY_BACKSPACE then
          if joinNameFocused && playerName.nonEmpty then playerName = playerName.init
          else if !joinNameFocused && joinIpInput.nonEmpty then joinIpInput = joinIpInput.init
      case Screen.HostGame =>
        val saves = worldSaveDirs
        if key == GLFW_KEY_ENTER then hostGame()
        else if key == GLFW_KEY_ESCAPE then screen = Screen.MainMenu
        else if key == GLFW_KEY_L then hostUseSavedWorld = !hostUseSavedWorld
        else if key == GLFW_KEY_UP then hostWorldSelection = (hostWorldSelection - 1).max(0)
        else if key == GLFW_KEY_DOWN then hostWorldSelection = (hostWorldSelection + 1).min((saves.length - 1).max(0))
        else if key == GLFW_KEY_BACKSPACE && playerName.nonEmpty then playerName = playerName.init
      case Screen.Paused =>
        if key == GLFW_KEY_ENTER then enterGame()
        else if key == GLFW_KEY_ESCAPE then
          if pauseEscReturnsToGame then enterGame() else leaveGame(Screen.MainMenu)
        else if key == GLFW_KEY_S then
          settingsReturnTo = Screen.Paused
          screen = Screen.Settings
      case Screen.Inventory | Screen.Catalog | Screen.FurnaceUI =>
        if key == GLFW_KEY_E || key == GLFW_KEY_ESCAPE then enterGame()

  private def handleMenuMouse(): Unit =
    val leftDown = glfwGetMouseButton(window, GLFW_MOUSE_BUTTON_LEFT) == GLFW_PRESS
    if leftDown && !menuLeftWasDown then
      val (mx, my) = mouseFramebufferPos()
      screen match
        case Screen.MainMenu => handleMainMenuClick(mx, my)
        case Screen.CreateWorld => handleCreateWorldClick(mx, my)
        case Screen.LoadWorld => handleLoadWorldClick(mx, my)
        case Screen.Mods => handleModsClick(mx, my)
        case Screen.Resources => handleResourcesClick(mx, my)
        case Screen.Settings => handleSettingsClick(mx, my)
        case Screen.Paused => handlePauseClick(mx, my)
        case Screen.Inventory => handleInventoryClick(mx, my)
        case Screen.Catalog => handleCatalogClick(mx, my)
        case Screen.FurnaceUI => handleFurnaceClick(mx, my)
        case Screen.JoinGame => handleJoinClick(mx, my)
        case Screen.HostGame => handleHostClick(mx, my)
        case Screen.Playing => ()
    // Slider dragging: update every frame while holding on Settings
    if leftDown && screen == Screen.Settings && sliderActive != null then
      val (mx, my) = mouseFramebufferPos()
      updateSlider(mx)
    if !leftDown then sliderActive = null
    menuLeftWasDown = leftDown

  private def updateSlider(mx: Float): Unit =
    val s = uiScale
    val pW = 520f * s
    val pX = framebufferWidth / 2f - pW / 2f
    val settingX = pX + 40f * s
    val value = ((mx - settingX) / (440f * s)).max(0f).min(1f)
    sliderActive match
      case "rd" =>
        val next = (minRenderDistance.toFloat + value * (maxRenderDistance - minRenderDistance)).round
        if next != renderDistance then
          renderDistance = next.max(minRenderDistance).min(maxRenderDistance)
          // Do not nuke/reload every chunk while the slider is being dragged.
          // Rapid render-distance changes used to save/dispose/recreate the whole world
          // repeatedly, which could leave workers fighting stale chunks and make terrain
          // appear raised or scrambled. Let the normal streamer add/remove only deltas.
          syncChunks()
      case "fog" => fogDensity = 0.6f + value * 2.4f
      case "fov" => fov = 50f + value * 50f
      case _ => ()

  private def handleMainMenuClick(mx: Float, my: Float): Unit =
    val layout = mainMenuLayout()
    val bx = layout._1; val bw = layout._2; val bh = layout._3
    val ys = layout._4
    if inRect(mx, my, bx, ys(0), bw, bh) then screen = Screen.CreateWorld
    else if inRect(mx, my, bx, ys(1), bw, bh) then
      hostNameFocused = true
      hostStatusError = false
      hostStatusMessage = "Hosts on 0.0.0.0:25565. Share your LAN/ZeroTier IP."
      screen = Screen.HostGame
    else if inRect(mx, my, bx, ys(2), bw, bh) then
      joinIpInput = ""
      joinNameFocused = true
      joinStatusError = false
      joinStatusMessage = "Enter host IP. You can use IP:port; default port is 25565."
      screen = Screen.JoinGame
    else if inRect(mx, my, bx, ys(3), bw, bh) then openLoadWorldMenu()
    else if inRect(mx, my, bx, ys(4), bw, bh) then screen = Screen.Mods
    else if inRect(mx, my, bx, ys(5), bw, bh) then openResources(Screen.MainMenu)
    else if inRect(mx, my, bx, ys(6), bw, bh) then
      screen = Screen.Settings
      settingsReturnTo = Screen.MainMenu
    else if inRect(mx, my, bx, ys(7), bw, bh) then glfwSetWindowShouldClose(window, true)

  private def handleCreateWorldClick(mx: Float, my: Float): Unit =
    val w = framebufferWidth.toFloat
    val h = framebufferHeight.toFloat
    val s = uiScale
    val cx = w / 2f
    val margin = (18f * s).max(12f)
    val pW = (620f * s).min(w - margin * 2f).max(math.min(360f, w - margin * 2f))
    val pH = (560f * s).min(h - margin * 2f).max(math.min(420f, h - margin * 2f))
    val pX = cx - pW / 2f
    val pY = ((h - pH) / 2f).max(margin)
    val settingW = (pW - 80f * s).max(260f.min(pW - 32f * s))
    val settingH = (36f * s).min(38f).max(28f)
    val settingX = cx - settingW / 2f
    val nameY = pY + 104f * s
    val seedY = pY + 166f * s
    val modeY = pY + 236f * s
    val cheatsY = pY + 282f * s
    if inRect(mx, my, settingX, nameY, settingW, 34f * s) then
      createWorldNameFocused = true
    else if inRect(mx, my, settingX, seedY, settingW, 34f * s) then
      enterCustomSeed = true
      createWorldNameFocused = false
    else if inRect(mx, my, settingX, modeY, settingW, settingH) then
      createWorldMode = if createWorldMode == GameMode.Survival then GameMode.Creative else GameMode.Survival
    else if inRect(mx, my, settingX, cheatsY, settingW, settingH) then
      createWorldCheats = !createWorldCheats
    else
      val buttonW = (pW - 120f * s).max(240f.min(pW - 40f * s))
      val buttonH = (38f * s).min(42f).max(30f)
      val gap = (10f * s).max(6f).min(12f)
      val buttonX = cx - buttonW / 2f
      val buttonStartY = pY + pH - (buttonH * 4f + gap * 3f) - 18f * s
      if inRect(mx, my, buttonX, buttonStartY, buttonW, buttonH) then startNewWorld()
      else if inRect(mx, my, buttonX, buttonStartY + buttonH + gap, buttonW, buttonH) then
        previewRandomSeed()
      else if inRect(mx, my, buttonX, buttonStartY + (buttonH + gap) * 2f, buttonW, buttonH) then enterCustomSeed = !enterCustomSeed
      else if inRect(mx, my, buttonX, buttonStartY + (buttonH + gap) * 3f, buttonW, buttonH) then
        enterCustomSeed = false
        screen = Screen.MainMenu

  private def renderNameField(x: Float, y: Float, w: Float, h: Float, label: String, value: String, focused: Boolean): Unit =
    val s = uiScale
    renderTextShadow(x, y - 21f * s, label, 0.78f, 0.84f, 0.96f, (0.92f * s).max(0.82f))
    rect(x - 1f * s, y - 1f * s, w + 2f * s, h + 2f * s, if focused then 0.95f else 0.03f, if focused then 0.75f else 0.03f, if focused then 0.28f else 0.04f, if focused then 0.42f else 0.82f)
    rect(x, y, w, h, 0.055f, 0.070f, 0.105f, 0.96f)
    rect(x + 2f * s, y + 2f * s, w - 4f * s, 2f * s, 0.22f, 0.25f, 0.32f, 0.18f)
    val shown = value.trim
    val display = if shown.isEmpty then "..." else if shown.length > 34 then shown.take(16) + "..." + shown.takeRight(14) else shown
    centeredTextFit(x + w / 2f, y + h / 2f - 6f * s, display, 0.92f, 0.96f, 1f, (0.98f * s).max(0.86f), w - 18f * s)

  private def renderHostGame(): Unit =
    glDisable(GL_DEPTH_TEST); glDisable(GL_CULL_FACE); setupOrtho()
    val w = framebufferWidth.toFloat; val h = framebufferHeight.toFloat; val cx = w / 2f; val s = uiScale
    glBegin(GL_QUADS)
    glColor4f(0.045f, 0.055f, 0.115f, 1f); glVertex2f(0, 0); glVertex2f(w, 0)
    glColor4f(0.12f, 0.20f, 0.34f, 1f); glVertex2f(w, h); glVertex2f(0, h)
    glEnd()
    rect(0, h * 0.62f, w, h * 0.38f, 0.06f, 0.18f, 0.06f, 0.88f)
    val pW = (620f * s).min(w * 0.92f); val pH = (470f * s).min(h * 0.88f)
    val pX = cx - pW / 2f; val pY = h / 2f - pH / 2f
    drawPanel(pX, pY, pW, pH)
    centeredTextFit(cx, pY + 34f * s, "HOST LAN GAME", 1f, 0.90f, 0.55f, 1.95f * s, pW - 60f * s)
    rect(pX + 40f * s, pY + 64f * s, pW - 80f * s, 1f, 0.30f, 0.30f, 0.35f, 0.30f)
    val fieldW = pW - 96f * s; val fieldH = 34f * s; val fieldX = pX + 48f * s
    renderNameField(fieldX, pY + 102f * s, fieldW, fieldH, "Online name", playerName, hostNameFocused)
    val saves = worldSaveDirs
    val sourceY = pY + 166f * s
    val sourceLabel = if hostUseSavedWorld then "World source: saved world" else "World source: new world"
    val (mx, my) = mouseFramebufferPos()
    val sourceHover = inRect(mx, my, fieldX, sourceY, fieldW, 30f * s)
    rect(fieldX, sourceY, fieldW, 30f * s, if sourceHover then 0.16f else 0.09f, if sourceHover then 0.18f else 0.10f, if sourceHover then 0.23f else 0.15f, 0.82f)
    renderTextShadow(fieldX + 12f * s, sourceY + 7f * s, sourceLabel, 0.88f, 0.92f, 1f, 0.58f * s)
    centeredTextFit(fieldX + fieldW - 54f * s, sourceY + 7f * s, "L", 1f, 0.86f, 0.42f, 0.56f * s, 40f * s)
    if hostUseSavedWorld then
      val listY = pY + 208f * s
      val rowH = (34f * s).max(27f)
      val visible = 4
      if saves.isEmpty then centeredTextFit(cx, listY + 34f * s, "No saved worlds in worlds/", 0.70f, 0.78f, 0.92f, 0.62f * s, fieldW)
      else
        if hostWorldSelection < hostWorldScroll then hostWorldScroll = hostWorldSelection
        if hostWorldSelection >= hostWorldScroll + visible then hostWorldScroll = hostWorldSelection - visible + 1
        hostWorldScroll = hostWorldScroll.max(0).min((saves.length - visible).max(0))
        saves.zipWithIndex.drop(hostWorldScroll).take(visible).foreach { case (dir, idx) =>
          val y = listY + (idx - hostWorldScroll) * rowH
          val selected = idx == hostWorldSelection
          rect(fieldX, y, fieldW, rowH - 4f * s, if selected then 0.22f else 0.075f, if selected then 0.19f else 0.085f, if selected then 0.11f else 0.12f, 0.84f)
          if selected then rect(fieldX, y, 4f * s, rowH - 4f * s, 1f, 0.82f, 0.30f, 0.80f)
          renderTextShadow(fieldX + 12f * s, y + 7f * s, dir.getName, 0.90f, 0.94f, 1f, 0.55f * s)
        }
    val hsr = if hostStatusError then 1.0f else 0.52f
    val hsg = if hostStatusError then 0.36f else 0.70f
    val hsb = if hostStatusError then 0.30f else 0.82f
    centeredTextFit(cx, pY + pH - 118f * s, hostStatusMessage, hsr, hsg, hsb, 0.54f * s, pW - 72f * s)
    val bw = (230f * s).min(pW - 90f * s); val bh = (38f * s).min(42f).max(30f)
    drawButton(cx - bw / 2f, pY + pH - 86f * s, bw, bh, if hostUseSavedWorld then "Host Selected World" else "Start Hosted World", accent = true)
    drawButton(cx - bw / 2f, pY + pH - 42f * s, bw, bh, "Back")

  private def handleHostClick(mx: Float, my: Float): Unit =
    val w = framebufferWidth.toFloat; val h = framebufferHeight.toFloat; val cx = w / 2f; val s = uiScale
    val pW = (620f * s).min(w * 0.92f); val pH = (470f * s).min(h * 0.88f)
    val pX = cx - pW / 2f; val pY = h / 2f - pH / 2f
    val fieldW = pW - 96f * s; val fieldH = 34f * s; val fieldX = pX + 48f * s
    val bw = (230f * s).min(pW - 90f * s); val bh = (38f * s).min(42f).max(30f)
    if inRect(mx, my, fieldX, pY + 102f * s, fieldW, fieldH) then hostNameFocused = true
    else if inRect(mx, my, fieldX, pY + 166f * s, fieldW, 30f * s) then hostUseSavedWorld = !hostUseSavedWorld
    else if hostUseSavedWorld then
      val saves = worldSaveDirs
      val listY = pY + 208f * s
      val rowH = (34f * s).max(27f)
      saves.zipWithIndex.drop(hostWorldScroll).take(4).foreach { case (_, idx) =>
        val y = listY + (idx - hostWorldScroll) * rowH
        if inRect(mx, my, fieldX, y, fieldW, rowH - 4f * s) then hostWorldSelection = idx
      }
      if inRect(mx, my, cx - bw / 2f, pY + pH - 86f * s, bw, bh) then hostGame()
      else if inRect(mx, my, cx - bw / 2f, pY + pH - 42f * s, bw, bh) then screen = Screen.MainMenu
    else if inRect(mx, my, cx - bw / 2f, pY + pH - 86f * s, bw, bh) then hostGame()
    else if inRect(mx, my, cx - bw / 2f, pY + pH - 42f * s, bw, bh) then screen = Screen.MainMenu

  private def renderJoinGame(): Unit =
    glDisable(GL_DEPTH_TEST); glDisable(GL_CULL_FACE); setupOrtho()
    val w = framebufferWidth.toFloat; val h = framebufferHeight.toFloat; val cx = w / 2f; val s = uiScale
    glBegin(GL_QUADS)
    glColor4f(0.045f, 0.055f, 0.115f, 1f); glVertex2f(0, 0); glVertex2f(w, 0)
    glColor4f(0.12f, 0.20f, 0.34f, 1f); glVertex2f(w, h); glVertex2f(0, h)
    glEnd()
    rect(0, h * 0.62f, w, h * 0.38f, 0.06f, 0.18f, 0.06f, 0.88f)
    val pW = (540f * s).min(w * 0.92f); val pH = (350f * s).min(h * 0.86f)
    val pX = cx - pW / 2f; val pY = h / 2f - pH / 2f
    drawPanel(pX, pY, pW, pH)
    centeredTextFit(cx, pY + 34f * s, "JOIN LAN GAME", 1f, 0.90f, 0.55f, 1.95f * s, pW - 60f * s)
    rect(pX + 40f * s, pY + 64f * s, pW - 80f * s, 1f, 0.30f, 0.30f, 0.35f, 0.30f)
    val fieldW = pW - 96f * s; val fieldH = 36f * s; val fieldX = pX + 48f * s
    renderNameField(fieldX, pY + 112f * s, fieldW, fieldH, "Online name", playerName, joinNameFocused)
    val ipDisplay = if joinIpInput.trim.isEmpty then "127.0.0.1" else joinIpInput.trim
    renderNameField(fieldX, pY + 184f * s, fieldW, fieldH, "Server IP address", ipDisplay, !joinNameFocused)
    val jsr = if joinStatusError then 1.0f else 0.52f
    val jsg = if joinStatusError then 0.34f else 0.70f
    val jsb = if joinStatusError then 0.28f else 0.82f
    centeredTextFit(cx, pY + 242f * s, joinStatusMessage, jsr, jsg, jsb, 0.56f * s, pW - 72f * s)
    val bw = (210f * s).min(pW - 90f * s); val bh = (40f * s).min(42f).max(30f)
    drawButton(cx - bw / 2f, pY + pH - 92f * s, bw, bh, "Connect", accent = true)
    drawButton(cx - bw / 2f, pY + pH - 44f * s, bw, bh, "Back")

  private def handleJoinClick(mx: Float, my: Float): Unit =
    val w = framebufferWidth.toFloat; val h = framebufferHeight.toFloat; val cx = w / 2f; val s = uiScale
    val pW = (540f * s).min(w * 0.92f); val pH = (350f * s).min(h * 0.86f)
    val pX = cx - pW / 2f; val pY = h / 2f - pH / 2f
    val fieldW = pW - 96f * s; val fieldH = 36f * s; val fieldX = pX + 48f * s
    val bw = (210f * s).min(pW - 90f * s); val bh = (40f * s).min(42f).max(30f)
    if inRect(mx, my, fieldX, pY + 112f * s, fieldW, fieldH) then joinNameFocused = true
    else if inRect(mx, my, fieldX, pY + 184f * s, fieldW, fieldH) then joinNameFocused = false
    else if inRect(mx, my, cx - bw / 2f, pY + pH - 92f * s, bw, bh) then
      val ip = if joinIpInput.trim.isEmpty then "127.0.0.1" else joinIpInput.trim
      joinStatusError = false
      joinStatusMessage = s"Connecting to $ip ..."
      joinGame(ip)
    else if inRect(mx, my, cx - bw / 2f, pY + pH - 44f * s, bw, bh) then screen = Screen.MainMenu

  private def handleSettingsClick(mx: Float, my: Float): Unit =
    val h = framebufferHeight.toFloat; val s = uiScale
    val pH = (630f * s).min(h * 0.94f); val pY = (h / 2f - pH / 2f).max(12f * s)
    val cx = framebufferWidth / 2f; val settingX = cx - 220f * s
    if inRect(mx, my, settingX, pY + 118 * uiScale, 440 * uiScale, 30 * uiScale) then
      updateSlider(mx); sliderActive = "rd"
    else if inRect(mx, my, settingX, pY + 185 * uiScale, 440 * uiScale, 30 * uiScale) then
      updateSlider(mx); sliderActive = "fog"
    else if inRect(mx, my, settingX, pY + 253 * uiScale, 440 * uiScale, 30 * uiScale) then
      updateSlider(mx); sliderActive = "fov"
    else
      var optY = pY + 285 * s
      if inRect(mx, my, settingX, optY, 440 * s, 28 * s) then fastMove = !fastMove
      else
        optY += 40 * s
        if inRect(mx, my, settingX, optY, 440 * s, 28 * s) then
          vsync = !vsync
          glfwSwapInterval(if vsync then 1 else 0)
        else
          optY += 40 * s
          val canHostMode = settingsReturnTo == Screen.Playing && gameServer != null && worldCheatsEnabled
          if canHostMode && inRect(mx, my, settingX, optY, 440 * s, 28 * s) then toggleGameMode()
          else
            if canHostMode then optY += 40 * s
            if inRect(mx, my, settingX, optY, 440 * s, 28 * s) then soundEnabled = !soundEnabled
            else
              optY += 40 * s
              if inRect(mx, my, settingX, optY, 440 * s, 28 * s) then toggleFullscreen()
              else
                optY += 40 * s
                if inRect(mx, my, settingX, optY, 440 * s, 28 * s) then openResources(Screen.Settings)
                else
                  optY += 40 * s
                  if inRect(mx, my, settingX, optY, 440 * s, 28 * s) then pauseEscReturnsToGame = !pauseEscReturnsToGame
                  else
                    val buttonW = 300f * s; val buttonX = cx - buttonW / 2f
                    if inRect(mx, my, buttonX, pY + pH - 55 * s, buttonW, 44f * s) then
                      sliderActive = null
                      screen = settingsReturnTo
                      if settingsReturnTo == Screen.Playing then
                        glfwSetInputMode(window, GLFW_CURSOR, GLFW_CURSOR_DISABLED)
                        firstMouse = true

  private def handlePauseClick(mx: Float, my: Float): Unit =
    val cx = framebufferWidth / 2f; val h = framebufferHeight.toFloat; val s = uiScale
    val pw = (420f * s).min(framebufferWidth * 0.80f); val ph = (if multiplayerMode then 300f * s else 275f * s).min(h * 0.82f); val py = h / 2f - ph / 2f
    val bw = (320f * s).min(pw - 72f * s); val bh = (40f * s).min(ph / 6f).max(30f); val bx = cx - bw / 2f
    val gap = (14f * s).min(18f); val by0 = py + 92f * s
    if inRect(mx, my, bx, by0, bw, bh) then enterGame()
    else if inRect(mx, my, bx, by0 + bh + gap, bw, bh) then
      settingsReturnTo = Screen.Paused
      screen = Screen.Settings
    else if inRect(mx, my, bx, by0 + (bh + gap) * 2f, bw, bh) then leaveGame(Screen.MainMenu)

  private def handleInventoryClick(mx: Float, my: Float): Unit =
    val cx = framebufferWidth / 2f
    val cy = framebufferHeight / 2f
    val s = uiScale
    val pw = (760f * s).min(framebufferWidth * 0.94f)
    val ph = (500f * s).min(framebufferHeight * 0.88f)
    val px = cx - pw / 2f
    val py = cy - ph / 2f

    val bottomH = (34f * s).min(ph * 0.08f).max(28f)
    if inRect(mx, my, px + 22f * s, py + ph - 46f * s, 130f * s, bottomH) then
      releaseHeldItem()
      openFurnace()
      return
    else if inRect(mx, my, px + pw - 104f * s, py + ph - 46f * s, 82f * s, bottomH) then
      releaseHeldItem()
      enterGame()
      return

    val hotbarY = py + ph - 104f * s
    val hotbarPanelW = pw - 56f * s
    val hotbarX = px + 28f * s
    val hotGap = (5f * s).max(4f)
    val hotSlot = ((hotbarPanelW - hotGap * (hotbarBlocks.length - 1)) / hotbarBlocks.length).min(44f * s).max(28f)
    val hotBarW = hotSlot * hotbarBlocks.length + hotGap * (hotbarBlocks.length - 1)
    val hotStartX = hotbarX + (hotbarPanelW - hotBarW) / 2f
    for i <- 0 until hotbarBlocks.length do
      val sx = hotStartX + i * (hotSlot + hotGap)
      if inRect(mx, my, sx, hotbarY, hotSlot, hotSlot) then
        if heldInventoryBlock != Block.Air then
          placeHeldItemInHotbar(i)
        else
          selectedBlock = i
          takeHotbarSlot(i)
        return

    val gridW = pw * 0.56f
    val cols = 6
    val gap = (6f * s).max(4f)
    val slot = ((gridW - gap * (cols - 1)) / cols).min(44f * s).max(30f)
    val invX = px + 28f * s
    val invY = py + 76f * s
    val items = inventoryItems
    for i <- 0 until inventoryGridSlots do
      val col = i % cols
      val row = i / cols
      val sx = invX + col * (slot + gap)
      val sy = invY + row * (slot + gap)
      if inRect(mx, my, sx, sy, slot, slot) then
        if heldInventoryBlock != Block.Air then
          if gameMode == GameMode.Survival && heldFromHotbar then
            addBackpackItem(heldInventoryBlock, heldInventoryCount.max(1))
          clearHeldItem()
          return
        else if i < items.length then
          val block = items(i)
          setHeldItem(block, if gameMode == GameMode.Creative then maxStackSize else inventory(block.ordinal).min(maxStackSize))
        else
          clearHeldItem()
        return

    val craftPanelW = (pw - gridW - 78f * s).max(210f * s)
    val craftX = px + pw - 28f * s - craftPanelW
    val craftY = py + 76f * s
    val cGap = 6f * s
    val cSlot = ((craftPanelW - 56f * s) / 4f).min(42f * s).max(28f)
    val gridStartX = craftX + 12f * s
    val gridStartY = craftY + 26f * s
    var ci = 0
    while ci < 9 do
      val col = ci % 3
      val row0 = ci / 3
      val sx = gridStartX + col * (cSlot + cGap)
      val sy = gridStartY + row0 * (cSlot + cGap)
      if inRect(mx, my, sx, sy, cSlot, cSlot) then
        if heldInventoryBlock != Block.Air && validHotbarBlock(heldInventoryBlock) then
          craftGridBlocks(ci) = heldInventoryBlock
          if gameMode == GameMode.Survival && heldFromHotbar then
            addBackpackItem(heldInventoryBlock, heldInventoryCount.max(1))
            clearHeldItem()
        else craftGridBlocks(ci) = Block.Air
        return
      ci += 1
    val outSize = cSlot * 1.16f
    val outX = craftX + craftPanelW - outSize - 16f * s
    val outY = gridStartY + cSlot + cGap
    if inRect(mx, my, outX, outY, outSize, outSize) then
      tryCraftGrid()
      return

  private def handleCatalogClick(mx: Float, my: Float): Unit =
    val cx = framebufferWidth / 2f
    val cy = framebufferHeight / 2f
    val s = uiScale
    val panelW = (700f * s).min(framebufferWidth * 0.94f)
    val panelH = (500f * s).min(framebufferHeight * 0.88f)
    val panelX = cx - panelW / 2f
    val panelY = cy - panelH / 2f
    val closeW = 72f * s
    val closeH = 28f * s
    if inRect(mx, my, panelX + panelW - 88f * s, panelY + 12f * s, closeW, closeH) then
      catalogScroll = 0
      releaseHeldItem()
      enterGame()
      return

    val hotbarY = panelY + panelH - 76f * s
    val hotbarPanelW = panelW - 40f * s
    val hotbarX = panelX + 20f * s
    val hotGap = (5f * s).max(4f)
    val hotSlot = ((hotbarPanelW - hotGap * (hotbarBlocks.length - 1)) / hotbarBlocks.length).min(44f * s).max(28f)
    val hotBarW = hotSlot * hotbarBlocks.length + hotGap * (hotbarBlocks.length - 1)
    val hotStartX = hotbarX + (hotbarPanelW - hotBarW) / 2f
    for i <- 0 until hotbarBlocks.length do
      val sx = hotStartX + i * (hotSlot + hotGap)
      if inRect(mx, my, sx, hotbarY, hotSlot, hotSlot) then
        if heldInventoryBlock != Block.Air then
          placeHeldItemInHotbar(i)
        else selectedBlock = i
        return

    val slotSize = (48f * s).max(34f).min(54f)
    val gridX = panelX + 20f * s
    val gridY = panelY + 52f * s
    val cols = ((panelW - 54f * s) / slotSize).toInt.max(4).min(12)
    val items = catalogItems
    val visibleRows = ((hotbarY - gridY - 26f * s) / slotSize).toInt.max(1)
    val scrollMax = ((items.length - 1) / cols - visibleRows + 2).max(0)
    if catalogScroll > scrollMax then catalogScroll = scrollMax
    for i <- items.indices.drop(catalogScroll * cols).take(cols * visibleRows) do
      val idx = i - catalogScroll * cols
      val col = idx % cols
      val row = idx / cols
      val sx = gridX + col * slotSize
      val sy = gridY + row * slotSize
      if inRect(mx, my, sx, sy, slotSize, slotSize) then
        setHeldItem(items(i), maxStackSize)
        return
    val scrollUp = inRect(mx, my, panelX + panelW - 18f * s, panelY + 50f * s, 14f * s, 20f * s)
    val scrollDown = inRect(mx, my, panelX + panelW - 18f * s, hotbarY - 26f * s, 14f * s, 20f * s)
    if scrollUp then catalogScroll = (catalogScroll - 1).max(0)
    if scrollDown then catalogScroll = (catalogScroll + 1).min(scrollMax)
    if catalogScroll < 0 then catalogScroll = 0

  private def inRect(px: Float, py: Float, x: Float, y: Float, w: Float, h: Float): Boolean =
    px >= x && px <= x + w && py >= y && py <= y + h

  private def mouseFramebufferPos(): (Float, Float) =
    val mx = BufferUtils.createDoubleBuffer(1); val my = BufferUtils.createDoubleBuffer(1)
    val ww = BufferUtils.createIntBuffer(1); val wh = BufferUtils.createIntBuffer(1)
    glfwGetCursorPos(window, mx, my)
    glfwGetWindowSize(window, ww, wh)
    val sx = framebufferWidth.toFloat / ww.get(0).max(1)
    val sy = framebufferHeight.toFloat / wh.get(0).max(1)
    ((mx.get(0).toFloat * sx), (my.get(0).toFloat * sy))

  private def startNewWorld(): Unit =
    applyWorldSeed(chooseCreateSeed(), freshWorldName = false)
    worldName = uniqueWorldFolderName(worldNameInput)
    clearLoadedChunks(saveFirst = false)
    camera = findSpawn()
    fallPeakY = camera.y
    velocity = Vec3(0f, 0f, 0f)
    onGround = false
    yaw = 0f
    pitch = 0f
    gameMode = createWorldMode
    worldCheatsEnabled = createWorldCheats
    resetInventory()
    enterCustomSeed = false
    startGame()
    modManager.fireWorldLoaded()

  private def appendPlayerNameChar(ch: Char): Unit =
    if ch.isLetterOrDigit || ch == '_' || ch == '-' then
      playerName = (playerName + ch).take(16)

  private def cleanPlayerName(): String =
    BlockboxNetworkText.cleanPlayerName(playerName, ((System.currentTimeMillis() / 1000) % 999).toInt)

  protected def networkEscape(value: String): String =
    BlockboxNetworkText.escape(value)

  private def networkUnescape(value: String): String =
    BlockboxNetworkText.unescape(value)

  protected def networkFloat(value: Float): String = BlockboxNetworkText.floatText(value)

  private def parseNetworkFloat(value: String): Float = BlockboxNetworkText.parseFloat(value)

  protected def networkSafeName(value: String): String =
    BlockboxNetworkText.safeName(value)

  private def normalizeColorId(id: Int): Int = BlockboxNetworkText.normalizeColorId(id, playerColorPalette.length)

  private def fallbackColorForName(name: String): Int =
    BlockboxNetworkText.fallbackColorForName(name, playerColorPalette.length)

  private def colorForId(id: Int): (Float, Float, Float) = playerColorPalette(normalizeColorId(id))

  private def rememberPlayerColor(name: String, colorId: Int): Unit =
    val safe = networkSafeName(name)
    if safe.nonEmpty then playerColors(safe) = normalizeColorId(colorId)

  private def colorForPlayer(name: String): Int =
    val safe = networkSafeName(name)
    playerColors.getOrElseUpdate(safe, fallbackColorForName(safe))

  protected def isOppedName(name: String): Boolean =
    val safe = networkSafeName(name)
    oppedPlayerNames.exists(_.equalsIgnoreCase(safe))

  protected def setOppedName(name: String, value: Boolean, announce: Boolean = true): Unit =
    val safe = networkSafeName(name)
    if safe.nonEmpty then
      if value then oppedPlayerNames += safe else oppedPlayerNames.find(_.equalsIgnoreCase(safe)).foreach(n => oppedPlayerNames -= n)
      if announce then addChatMessage(if value then s"$safe is now an operator" else s"$safe is no longer an operator")

  protected def broadcastOpState(name: String, value: Boolean): Unit =
    val safe = networkSafeName(name)
    if gameServer != null && safe.nonEmpty then gameServer.broadcast("OP|" + networkEscape(safe) + "|" + (if value then "1" else "0"))

  private def parsePlayerToken(token: String): Option[(String, Int)] =
    BlockboxNetworkText.parsePlayerToken(token, playerColorPalette.length)

  private def chooseRandomLocalColor(): Int =
    normalizeColorId(scala.util.Random.nextInt(playerColorPalette.length))

  private def parseServerAddress(rawInput: String): Either[String, (String, Int)] =
    BlockboxNetworkText.parseServerAddress(rawInput, serverPort)

  private def parsePort(text: String): Either[String, Int] =
    BlockboxNetworkText.parsePort(text)

  private def prepareWorldForHost(): Unit =
    val saves = worldSaveDirs
    if hostUseSavedWorld && saves.nonEmpty then
      val chosen = saves(hostWorldSelection.max(0).min(saves.length - 1))
      if !loadWorldDataFromDir(chosen) then
        addChatMessage(s"Could not host saved world: ${chosen.getName}")
        hostUseSavedWorld = false
        prepareWorldForHost()
        return
    else
      applyWorldSeed(chooseCreateSeed(), freshWorldName = false)
      worldName = uniqueWorldFolderName(if worldNameInput.trim.nonEmpty then worldNameInput else "Hosted World")
      clearLoadedChunks(saveFirst = false)
      camera = findSpawn()
      fallPeakY = camera.y
      yaw = 0f
      pitch = 0f
      gameMode = createWorldMode
      worldCheatsEnabled = createWorldCheats
      resetInventory()
    velocity = Vec3(0f, 0f, 0f)
    onGround = false
    enterCustomSeed = false

  private def verifyLocalServer(port: Int): Either[String, Unit] =
    var sock: Socket = null
    try
      sock = Socket()
      sock.connect(InetSocketAddress("127.0.0.1", port), 1500)
      sock.setSoTimeout(1500)
      val out = PrintWriter(BufferedWriter(OutputStreamWriter(sock.getOutputStream)), true)
      val in = BufferedReader(InputStreamReader(sock.getInputStream))
      out.println("PING|probe")
      out.flush()
      val line = in.readLine()
      if line != null && line.startsWith("PONG|BLOCKBOX") then Right(())
      else Left("Server socket opened, but Blockbox handshake probe failed.")
    catch
      case e: Exception =>
        val msg = Option(e.getMessage).filter(_.nonEmpty).getOrElse(e.getClass.getSimpleName)
        Left("Server failed local port self-test on 127.0.0.1:" + port + " — " + msg)
    finally
      try if sock != null then sock.close() catch case _: Exception => ()

  private def hostGame(): Unit =
    playerName = cleanPlayerName()
    stopNetworking()
    try
      // Prepare the world first, but do not enter play mode until the socket is confirmed reachable.
      // This prevents the host from thinking it is online when nothing is actually listening.
      prepareWorldForHost()
      localColorId = chooseRandomLocalColor()
      playerColors.clear()
      oppedPlayerNames.clear(); oppedPlayerNames += playerName
      rememberPlayerColor(playerName, localColorId)
      val server = GameServer(
        serverPort,
        worldSeed,
        camera,
        playerName,
        localColorId,
        (x, y, z, blockId) =>
          val block = Block.fromId(blockId)
          if block == Block.Air || canPlaceBlockAt(x, y, z) then
            networkBlockOverrides((x, y, z)) = block
            setActiveBlock(x, y, z, block)
            dirtyChunkAt(x, z),
        () => snapshotWorldEditsForNetwork(),
        () => oppedPlayerNames.toSeq,
        () => camera,
        () => yaw,
        () => pitch,
        modManager.serverModpackHash,
        modManager.serverModpackList
      )
      gameServer = server
      verifyLocalServer(server.localPort) match
        case Right(_) =>
          gameClient = null
          multiplayerMode = true
          knownPlayerNames.clear(); knownPlayerNames += playerName
          playerColors.clear()
          oppedPlayerNames.clear(); oppedPlayerNames += playerName
          rememberPlayerColor(playerName, localColorId)
          hostStatusError = false
          hostStatusMessage = s"Hosting as $playerName. Server modpack: ${modManager.serverModpackHash}. Join locally with 127.0.0.1:${server.localPort}, or share your LAN/ZeroTier IP."
          addChatMessage(s"Hosting world as $playerName on port ${server.localPort}")
          startGame()
          modManager.fireWorldLoaded()
        case Left(error) =>
          stopNetworking()
          hostStatusError = true
          hostStatusMessage = error.toString
          screen = Screen.HostGame
          glfwSetInputMode(window, GLFW_CURSOR, GLFW_CURSOR_NORMAL)
    catch
      case e: java.net.BindException =>
        stopNetworking()
        hostStatusError = true
        hostStatusMessage = s"Port $serverPort is already in use. Close the old server or use another port."
        screen = Screen.HostGame
        glfwSetInputMode(window, GLFW_CURSOR, GLFW_CURSOR_NORMAL)
      case e: Exception =>
        stopNetworking()
        hostStatusError = true
        val msg = Option(e.getMessage).filter(_.nonEmpty).getOrElse(e.getClass.getSimpleName)
        hostStatusMessage = "Could not host: " + msg
        screen = Screen.HostGame
        glfwSetInputMode(window, GLFW_CURSOR, GLFW_CURSOR_NORMAL)

  private def joinGame(ip: String): Unit =
    playerName = cleanPlayerName()
    stopNetworking()
    parseServerAddress(ip) match
      case Left(error) =>
        joinStatusError = true
        joinStatusMessage = error.toString
        gameClient = null
        screen = Screen.JoinGame
        glfwSetInputMode(window, GLFW_CURSOR, GLFW_CURSOR_NORMAL)
      case Right((host, port)) =>
        joinStatusError = false
        joinStatusMessage = s"Connecting to $host:$port ..."
        val client = GameClient(host, port)
        gameClient = client
        if client.connect(playerName, modManager.clientJoinHash) then
          multiplayerMode = true
          knownPlayerNames.clear(); knownPlayerNames += playerName
          playerColors.clear()
          oppedPlayerNames.clear()
          rememberPlayerColor(playerName, localColorId)
          joinStatusError = false
          joinStatusMessage = s"Connected to $host:$port as $playerName."
          worldSeed = 0L
          terrainGen = TerrainGenerator(worldSeed)
          terrainHeightCache.clear()
          clearLoadedChunks(saveFirst = false)
          camera = findSpawn()
          velocity = Vec3(0f, 0f, 0f); onGround = false; yaw = 0f; pitch = 0f
          gameMode = GameMode.Survival
          resetInventory()
          // Consume the authoritative WORLD handshake before rendering/physics starts.
          // Otherwise the client can briefly build chunks around a temporary seed/spawn,
          // which looks like a dark empty world and can desync collision.
          processNetworkMessages()
          modManager.fireWorldLoaded()
          syncChunks()
          startGame()
        else
          val err = client.errorMessage
          client.disconnect()
          gameClient = null
          multiplayerMode = false
          joinStatusError = true
          joinStatusMessage = if err.nonEmpty then err else s"Could not connect to $host:$port."
          screen = Screen.JoinGame
          glfwSetInputMode(window, GLFW_CURSOR, GLFW_CURSOR_NORMAL)

  private def stopNetworking(): Unit =
    if gameClient != null then gameClient.disconnect(); gameClient = null
    if gameServer != null then gameServer.stop(); gameServer = null
    remotePlayers.clear()
    knownPlayerNames.clear()
    playerColors.clear()
    oppedPlayerNames.clear()
    pendingNetworkBlocks.clear()
    networkBlockOverrides.clear()
    networkWaterLevelOverrides.clear()
    lastMultiplayerHeartbeat = 0.0
    multiplayerMode = false

  private def startGame(): Unit =
    // Build a tiny ring of nearby meshes immediately on the GL thread before the first
    // gameplay frame. Async chunk workers still handle the rest, but this prevents the
    // client from seeing sky-only/dark-blue frames with only remote name tags visible.
    syncChunks()
    forceCameraChunkRing(0)
    processChunkWorkMainThread(0, 12)
    ensureNearbyMeshesReady(1)
    screen = Screen.Playing
    modUiCursorMode = false
    firstMouse = true
    glfwSetInputMode(window, GLFW_CURSOR, GLFW_CURSOR_DISABLED)

  private def enterGame(): Unit =
    if screen == Screen.Inventory || screen == Screen.Catalog || screen == Screen.FurnaceUI then releaseHeldItem()
    startGame()

  private def leaveGame(next: Screen): Unit =
    // Pausing must NOT close the multiplayer server. The host needs to be able to tab away,
    // open the pause menu, or focus another local test client while the socket keeps listening.
    // Only tear networking down when actually returning to title/main menu.
    if next == Screen.MainMenu then
      saveWorld()
      stopNetworking()
      clearLoadedChunks(saveFirst = false)
    screen = next
    glfwSetInputMode(window, GLFW_CURSOR, GLFW_CURSOR_NORMAL)

  private def isClientOnlyMultiplayer: Boolean =
    multiplayerMode && gameClient != null && gameServer == null

  private def canUseLocalChunkSaves: Boolean = !isClientOnlyMultiplayer

  private def worldsRootDir: java.io.File = BlockboxWorld.worldsRootDir
  private def currentWorldDir: java.io.File = BlockboxWorld.currentWorldDir(worldName)
  private def currentChunksDir: java.io.File = BlockboxWorld.currentChunksDir(worldName)

  private def worldSaveDirs: List[java.io.File] =
    BlockboxWorld.worldSaveDirs(worldsRootDir)

  private def writeWorldIndex(): Unit =
    try
      BlockboxWorld.writeWorldIndex(worldsRootDir)
    catch case e: Exception => System.err.println(s"World index save failed: $e")

  private def writeWorldExtras(out: java.io.DataOutputStream): Unit =
    compactHotbarAssignments()
    out.writeInt(worldExtraMagic)
    out.writeInt(worldExtraVersion)
    out.writeInt(inventory.length)
    inventory.foreach(v => out.writeInt(v.max(0)))
    out.writeInt(hotbarBlocks.length)
    hotbarBlocks.foreach(b => out.writeByte(b.ordinal.toByte))
    hotbarCounts.foreach(c => out.writeInt(c.max(0).min(maxStackSize)))
    out.writeInt(selectedBlock.max(0).min(hotbarBlocks.length - 1))
    out.writeInt(playerHealth.round)
    out.writeInt(playerFood.round)
    out.writeByte(furnaceInput.ordinal.toByte)
    out.writeByte(furnaceFuel.ordinal.toByte)
    out.writeFloat(furnaceProgress)
    out.writeFloat(furnaceFuelRemaining)
    out.writeByte(furnaceOutput.ordinal.toByte)
    out.writeInt(furnaceOutputCount)

  private def readWorldExtras(in: java.io.DataInputStream): Unit =
    try
      if in.available() >= 8 then
        val magic = in.readInt()
        val version = in.readInt()
        if magic == worldExtraMagic && version >= 1 then
          val invLen = in.readInt().max(0).min(4096)
          var i = 0
          while i < invLen do
            val value = in.readInt().max(0)
            if i < inventory.length then inventory(i) = value
            i += 1
          val hotLen = in.readInt().max(0).min(64)
          i = 0
          while i < hotLen do
            val block = Block.fromId(in.readByte())
            if i < hotbarBlocks.length && block != Block.Water then hotbarBlocks(i) = block
            i += 1
          if version >= 2 && in.available() >= hotLen * 4 then
            i = 0
            while i < hotLen do
              val count = in.readInt().max(0).min(maxStackSize)
              if i < hotbarCounts.length then hotbarCounts(i) = count
              i += 1
          else
            i = 0
            while i < hotbarBlocks.length do
              hotbarCounts(i) = if gameMode == GameMode.Creative && hotbarBlocks(i) != Block.Air then maxStackSize else 0
              i += 1
          compactHotbarAssignments()
          selectedBlock = in.readInt().max(0).min(hotbarBlocks.length - 1)
          playerHealth = in.readInt().max(0).min(20).toFloat
          playerFood = in.readInt().max(0).min(20).toFloat
          furnaceInput = Block.fromId(in.readByte())
          furnaceFuel = Block.fromId(in.readByte())
          furnaceProgress = in.readFloat().max(0f)
          furnaceFuelRemaining = in.readFloat().max(0f)
          furnaceOutput = Block.fromId(in.readByte())
          furnaceOutputCount = in.readInt().max(0)
    catch case e: Exception => System.err.println(s"World extras load skipped: $e")

  private def snapshotWorldEditsForNetwork(): Seq[(Int, Int, Int, Byte)] =
    val loadedEdits = chunks.values.toSeq.flatMap { chunk =>
      val bx = chunk.baseX
      val bz = chunk.baseZ
      chunk.edits.synchronized {
        chunk.edits.toList.map { case ((lx, ly, lz), block) => (bx + lx, ly, bz + lz, block.id) }
      }
    }
    val rememberedEdits = networkBlockOverrides.toSeq.map { case ((x, y, z), block) => (x, y, z, block.id) }
    (loadedEdits ++ rememberedEdits).reverse.distinctBy(t => (t._1, t._2, t._3)).reverse

  private def saveLoadedChunks(): Unit =
    if !canUseLocalChunkSaves then return
    try
      val chunksDir = currentChunksDir
      chunksDir.mkdirs()
      chunks.foreach { case ((cx, cz), chunk) =>
        chunk.save(chunksDir)
        dirtyChunksForSave -= ((cx, cz))
      }
    catch case e: Exception => System.err.println(s"Chunk save failed: $e")

  private def clearLoadedChunks(saveFirst: Boolean): Unit =
    if saveFirst then saveLoadedChunks()
    chunkStreamGeneration += 1
    chunkBuildQueue.clear(); chunkUploadQueue.clear()
    chunkCreateQueue.clear(); chunkReadyQueue.clear(); pendingChunkCreates.clear()
    waterFlowQueue.clear()
    waterFlowQueued.clear()
    dirtyChunks.clear()
    dirtyChunksForSave.clear()
    chunks.values.foreach(_.dispose())
    chunks.clear()

  protected def saveWorld(): Unit =
    if !canUseLocalChunkSaves then return
    try
      val dir = currentWorldDir
      BlockboxFiles.ensureDirectory(dir.toPath)
      val chunksDir = new java.io.File(dir, "chunks")
      BlockboxFiles.ensureDirectory(chunksDir.toPath)
      val file = new java.io.File(dir, "world.dat")
      val chunksNeedingSave = dirtyChunksForSave.toSet
      BlockboxFiles.writeAtomic(file.toPath, out0 =>
        val meta = new java.io.DataOutputStream(new java.io.BufferedOutputStream(out0))
        meta.writeLong(worldSeed)
        meta.writeFloat(camera.x); meta.writeFloat(camera.y); meta.writeFloat(camera.z)
        meta.writeFloat(yaw); meta.writeFloat(pitch)
        meta.writeByte(gameMode.ordinal.toByte)
        meta.writeBoolean(worldCheatsEnabled)
        meta.writeInt(chunks.size)
        chunks.foreach { case ((cx, cz), _) =>
          meta.writeInt(cx); meta.writeInt(cz)
          if chunksNeedingSave.contains((cx, cz)) then saveChunk(cx, cz)
        }
        writeWorldExtras(meta)
        meta.flush()
      )
      writeWorldIndex()
    catch case e: Exception => System.err.println(s"Save failed: $e")

  private def openLoadWorldMenu(): Unit =
    loadWorldSelection = loadWorldSelection.max(0).min((worldSaveDirs.length - 1).max(0))
    loadWorldScroll = loadWorldSelection.max(0)
    screen = Screen.LoadWorld
    glfwSetInputMode(window, GLFW_CURSOR, GLFW_CURSOR_NORMAL)

  private def openResources(returnTo: Screen): Unit =
    refreshResourcePacks()
    resourcesReturnTo = returnTo
    resourcesScroll = resourcePacks.indexWhere(_.id == selectedTexturePackId).max(0)
    screen = Screen.Resources
    glfwSetInputMode(window, GLFW_CURSOR, GLFW_CURSOR_NORMAL)

  private def loadWorld(): Unit =
    openLoadWorldMenu()

  private def loadSelectedWorld(): Unit =
    val saves = worldSaveDirs
    if saves.isEmpty then
      addChatMessage("No saved worlds found")
      screen = Screen.MainMenu
    else
      loadWorldFromDir(saves(loadWorldSelection.max(0).min(saves.length - 1)))

  private def loadWorldDataFromDir(worldDir: java.io.File): Boolean =
    try
      worldName = worldDir.getName
      val meta = new java.io.DataInputStream(new java.io.BufferedInputStream(new java.io.FileInputStream(new java.io.File(worldDir, "world.dat"))))
      try
        worldSeed = meta.readLong()
        val x = meta.readFloat(); val y = meta.readFloat(); val z = meta.readFloat()
        camera = Vec3(x, y, z)
        yaw = meta.readFloat(); pitch = meta.readFloat()
        gameMode = GameMode.fromOrdinal(meta.readByte().toInt)
        worldCheatsEnabled = meta.readBoolean()
        terrainGen = TerrainGenerator(worldSeed)
        terrainHeightCache.clear()
        clearLoadedChunks(saveFirst = false)
        resetInventory()
        networkBlockOverrides.clear()
        val chunkCount = meta.readInt().max(0).min(200000)
        for _ <- 0 until chunkCount do
          val cx = meta.readInt(); val cz = meta.readInt()
          loadChunkIfSaved(cx, cz)
        readWorldExtras(meta)
      finally meta.close()
      true
    catch
      case e: Exception =>
        System.err.println(s"Load failed for ${worldDir.getName}: $e")
        false

  private def loadWorldFromDir(worldDir: java.io.File): Unit =
    if loadWorldDataFromDir(worldDir) then
      addChatMessage(s"Loaded world: $worldName")
      enterGame()
      modManager.fireWorldLoaded()
    else
      addChatMessage(s"Could not load world: ${worldDir.getName}")
      screen = Screen.LoadWorld

  protected def totalItemCount(block: Block): Int =
    if gameMode == GameMode.Creative && validHotbarBlock(block) then maxStackSize
    else if validHotbarBlock(block) then inventory(block.ordinal).max(0) + hotbarItemCount(block) else 0

  protected def gainItem(block: Block, amount: Int): Unit =
    if validHotbarBlock(block) && amount > 0 then
      var remaining = amount
      if gameMode == GameMode.Survival then
        var i = 0
        while i < hotbarBlocks.length && remaining > 0 do
          if hotbarBlocks(i) == block && hotbarCounts(i) < maxStackSize then
            val moved = remaining.min(maxStackSize - hotbarCounts(i))
            hotbarCounts(i) += moved
            remaining -= moved
          i += 1
        i = 0
        while i < hotbarBlocks.length && remaining > 0 do
          if hotbarBlocks(i) == Block.Air then
            val moved = remaining.min(maxStackSize)
            hotbarBlocks(i) = block
            hotbarCounts(i) = moved
            remaining -= moved
          i += 1
      if remaining > 0 then addBackpackItem(block, remaining)
      compactHotbarAssignments()

  protected def consumeInventory(block: Block, amount: Int): Boolean =
    if amount <= 0 then true
    else if gameMode == GameMode.Creative then true
    else if !validHotbarBlock(block) || totalItemCount(block) < amount then false
    else
      var remaining = amount
      val fromBackpack = remaining.min(inventory(block.ordinal).max(0))
      inventory(block.ordinal) -= fromBackpack
      remaining -= fromBackpack
      var i = 0
      while i < hotbarBlocks.length && remaining > 0 do
        if hotbarBlocks(i) == block && hotbarCounts(i) > 0 then
          val used = remaining.min(hotbarCounts(i))
          hotbarCounts(i) -= used
          remaining -= used
        i += 1
      compactHotbarAssignments()
      true

  private def submitChat(): Unit =
    val text = chatInput.trim
    if text.nonEmpty then
      chatHistory += chatInput
      chatHistoryIndex = -1
      val event = modManager.fireChat(ModChatEvent(modManager.api, playerName, text, cancelled = false))
      val finalText = Option(event.message).getOrElse("").trim
      if !event.cancelled && finalText.nonEmpty then
        if finalText.startsWith("/") then parseCommand(finalText)
        else sendChatMessage(finalText)
    chatOpen = false
    chatInput = ""

  protected def sendChatMessage(text: String): Unit =
    addChatMessage(s"<You> $text")
    if multiplayerMode then
      val msg = s"CHAT|${networkEscape(playerName)}|${networkEscape(text)}"
      if gameClient != null && gameClient.isConnected then gameClient.send(msg)
      else if gameServer != null then gameServer.broadcast(msg)

  protected def addChatMessage(msg: String): Unit =
    chatMessages += ((msg, 8f))
    if chatMessages.length > 100 then chatMessages.remove(0, chatMessages.length - 100)

  private def openInventory(): Unit =
    catalogScroll = 0
    screen = Screen.Inventory
    glfwSetInputMode(window, GLFW_CURSOR, GLFW_CURSOR_NORMAL)
    breakingBlock = null
    breakingProgress = 0f

  private def openCatalog(): Unit =
    catalogScroll = 0
    screen = Screen.Catalog
    glfwSetInputMode(window, GLFW_CURSOR, GLFW_CURSOR_NORMAL)
    breakingBlock = null
    breakingProgress = 0f

  private def openFurnace(): Unit =
    screen = Screen.FurnaceUI
    glfwSetInputMode(window, GLFW_CURSOR, GLFW_CURSOR_NORMAL)
    breakingBlock = null
    breakingProgress = 0f

  private def updateGame(dt: Float): Unit =
    processNetworkMessages()
    syncChunks()
    // Keep the camera neighborhood deterministic. If the local terrain is not
    // ready, build the 5x5 ring right now instead of rendering a sky-only void.
    if !centerChunkReady then forceCameraChunkRing(0)
    processChunkWorkMainThread(0, 12)
    for i <- chatMessages.indices do
      val (msg, t) = chatMessages(i)
      chatMessages(i) = (msg, t - dt)
    chatMessages.filterInPlace((_, t) => t > -2f)
    cleanupRemotePlayers()
    if chatOpen then
      // Chat is also a client-side UI state in multiplayer; keep the player present/alive.
      sendPlayerPositionNetwork()
      return
    if modUiCursorMode then
      // Client mod GUI focus: keep networking/chunk updates alive, but do not steal mouse-look or break/place blocks.
      handleModGuiMouse()
      sendPlayerPositionNetwork()
      return
    val x = BufferUtils.createDoubleBuffer(1)
    val y = BufferUtils.createDoubleBuffer(1)
    glfwGetCursorPos(window, x, y)
    if firstMouse then
      lastMouseX = x.get(0); lastMouseY = y.get(0); firstMouse = false
    val dx = (x.get(0) - lastMouseX).toFloat
    val dy = (y.get(0) - lastMouseY).toFloat
    lastMouseX = x.get(0); lastMouseY = y.get(0)
    yaw += dx * 0.09f
    pitch = (pitch + dy * 0.09f).max(-89f).min(89f)
    val forward = Vec3(sin(toRadians(yaw)).toFloat, 0f, -cos(toRadians(yaw)).toFloat).normalized
    val right = Vec3(cos(toRadians(yaw)).toFloat, 0f, sin(toRadians(yaw)).toFloat).normalized
    var move = Vec3(0, 0, 0)
    if down(GLFW_KEY_W) then move += forward
    if down(GLFW_KEY_S) then move -= forward
    if down(GLFW_KEY_D) then move += right
    if down(GLFW_KEY_A) then move -= right
    val moving = move.lengthSquared > 0.001f
    val sprinting = moving && (down(GLFW_KEY_LEFT_CONTROL) || down(GLFW_KEY_RIGHT_CONTROL))
    val crouching = onGround && moving && (down(GLFW_KEY_LEFT_SHIFT) || down(GLFW_KEY_RIGHT_SHIFT))
    gameMode match
      case GameMode.Creative =>
        val spaceDown = down(GLFW_KEY_SPACE)
        val spacePressed = spaceDown && !wasSpaceDown
        wasSpaceDown = spaceDown
        val now = glfwGetTime()
        if spacePressed then
          if now - lastSpacePressTime < doubleTapInterval then
            flyEnabled = !flyEnabled
            if flyEnabled then
              velocity = Vec3(0f, 0f, 0f)
              addChatMessage("Flight enabled")
            else
              onGround = false
              velocity = Vec3(0f, -0.5f, 0f)
              addChatMessage("Flight disabled")
          lastSpacePressTime = now
        if flyEnabled then
          val speed = if fastMove then 16f else 9f
          if down(GLFW_KEY_SPACE) then move += Vec3(0, 1, 0)
          if down(GLFW_KEY_LEFT_SHIFT) then move -= Vec3(0, 1, 0)
          camera = camera + move.normalized * speed * dt
        else
          val submerged = waterSubmersion
          val headInWater = blockAt(camera) == Block.Water
          val bodyInWater = blockAt(camera - Vec3(0f, 0.8f, 0f)) == Block.Water
          val swimming = headInWater || bodyInWater || submerged > 0f
          val speed =
            if swimming then if fastMove then 6.5f else 3.0f
            else if fastMove then 16f else 9f
          val hMove = move.normalized * speed * dt
          movePlayer(hMove.x, 0f, hMove.z)
          if swimming then
            updateSwimming(dt, swimming, headInWater, bodyInWater, submerged)
            moveVertical(velocity.y * dt)
            onGround = false
          else
            if spacePressed && onGround then
              velocity = Vec3(0f, 12.5f, 0f)
              onGround = false
            velocity = Vec3(0f, (velocity.y - 52f * dt).max(-78f), 0f)
            moveVertical(velocity.y * dt)
      case GameMode.Survival =>
        val submerged = waterSubmersion
        val headInWater = blockAt(camera) == Block.Water
        val bodyInWater = blockAt(camera - Vec3(0f, 0.8f, 0f)) == Block.Water
        val swimming = headInWater || bodyInWater || submerged > 0f
        val speed =
          if fastMove then 13f
          else if crouching then 3.2f
          else if swimming then if sprinting then 3.8f else 2.8f
          else if sprinting then 7.2f
          else 5.5f
        val airControl = if onGround || swimming then 1f else 0.72f
        val horizontal = move.normalized * speed * airControl * dt
        if crouching && onGround then
          val hx = if hasGroundBelow(camera + Vec3(horizontal.x, 0f, 0f)) then horizontal.x else 0f
          val hz = if hasGroundBelow(camera + Vec3(0f, 0f, horizontal.z)) then horizontal.z else 0f
          movePlayer(hx, 0f, hz)
        else
          movePlayer(horizontal.x, 0f, horizontal.z)
        updateSwimming(dt, swimming, headInWater, bodyInWater, submerged)
        moveVertical(velocity.y * dt)
        if swimming then onGround = false
    updateEnvironmentalDamage(dt)
    updateHealthRegen(dt)
    updateWater(dt)
    updateBubbles(dt)
    updateSandFalling()
    updateSandParticles(dt)
    processChunkWorkMainThread(0, 12)
    flushDirtyChunks()
    sendPlayerPositionNetwork()
    handleMouseButtons()

  private def handleModGuiMouse(): Unit =
    val (mx, my) = mouseFramebufferPos()
    modLastMouseX = mx
    modLastMouseY = my
    val leftDown = glfwGetMouseButton(window, GLFW_MOUSE_BUTTON_LEFT) == GLFW_PRESS
    modLeftDownNow = leftDown
    modLeftClickedThisFrame = leftDown && !modGuiLeftWasDown
    if modLeftClickedThisFrame then modManager.fireMouseClick(mx, my, GLFW_MOUSE_BUTTON_LEFT)
    modGuiLeftWasDown = leftDown

  private def shouldRunMultiplayerBackgroundTick: Boolean =
    multiplayerMode &&
      (screen == Screen.Paused || screen == Screen.Inventory || screen == Screen.Catalog || screen == Screen.FurnaceUI ||
        (screen == Screen.Settings && (settingsReturnTo == Screen.Playing || settingsReturnTo == Screen.Paused)) ||
        (screen == Screen.Resources && resourcesReturnTo == Screen.Settings && (settingsReturnTo == Screen.Playing || settingsReturnTo == Screen.Paused)))

  private def updateMultiplayerBackgroundTick(dt: Float): Unit =
    // Do NOT run local movement/physics here. This is only the live multiplayer heartbeat for UI screens.
    processNetworkMessages()
    syncChunks()
    if !centerChunkReady then forceCameraChunkRing(0)
    processChunkWorkMainThread(0, 8)
    for i <- chatMessages.indices do
      val (msg, t) = chatMessages(i)
      chatMessages(i) = (msg, t - dt)
    chatMessages.filterInPlace((_, t) => t > -2f)
    cleanupRemotePlayers()
    // Keep chunk meshes alive while multiplayer menus are open, but cap work so UI remains responsive.
    processChunkWorkMainThread(0, 8)
    flushDirtyChunks()
    sendPlayerPositionNetwork()

  private def updateSwimming(dt: Float, swimming: Boolean, headInWater: Boolean, bodyInWater: Boolean, submerged: Float): Unit =
    if !swimming then
      if onGround && down(GLFW_KEY_SPACE) then
        velocity = Vec3(0f, 12.5f, 0f)
        onGround = false
      else velocity = Vec3(0f, (velocity.y - 52f * dt).max(-78f), 0f)
      return
    val swimUp = down(GLFW_KEY_SPACE)
    val swimDown = down(GLFW_KEY_LEFT_SHIFT) || down(GLFW_KEY_RIGHT_SHIFT)
    if headInWater && submerged >= 0.3f then
      val buoyancy = 8f
      val gravity = -4f
      val inputLift = (if swimUp then 14f else 0f) - (if swimDown then 10f else 0f)
      val damping = pow(0.35, (dt * 3).toDouble).toFloat
      val targetVy = if swimUp then 3f else if swimDown then -3f else 0f
      velocity = Vec3(0f, velocity.y + (gravity + buoyancy + inputLift - velocity.y * 2f) * dt, 0f)
      velocity = Vec3(0f, velocity.y.max(-4.5f).min(4.5f), 0f)
      onGround = false
    else if bodyInWater then
      if swimUp then
        velocity = Vec3(0f, 6.5f, 0f)
        onGround = false
      else if swimDown then
        velocity = Vec3(0f, -4.5f, 0f)
        onGround = false
      else
        velocity = Vec3(0f, (velocity.y * 0.85f - 1f * dt).max(-2f).min(2f), 0f)
        onGround = false
    else
      velocity = Vec3(0f, (velocity.y - 52f * dt).max(-78f), 0f)
      if onGround && swimUp then velocity = Vec3(0f, 8.4f, 0f)
      if swimUp && !onGround then velocity = Vec3(0f, velocity.y.max(3f), 0f)

  private def handleMouseButtons(): Unit =
    val leftDown = glfwGetMouseButton(window, GLFW_MOUSE_BUTTON_LEFT) == GLFW_PRESS
    val rightDown = glfwGetMouseButton(window, GLFW_MOUSE_BUTTON_RIGHT) == GLFW_PRESS
    if leftDown && !leftWasDown && gameMode == GameMode.Creative then breakTargetBlock(dropItem = false)
    if gameMode == GameMode.Survival then updateBlockBreaking(lastFrameTime.toFloat, leftDown)
    else if !leftDown then resetBreaking()
    if rightDown && !rightWasDown then placeTargetBlock()
    leftWasDown = leftDown
    rightWasDown = rightDown

  private def updateBlockBreaking(dt: Float, leftDown: Boolean): Unit =
    if !leftDown then
      resetBreaking()
      return
    raycast().foreach { hit =>
      val target = hit.block
      val block = activeBlockAt(target._1, target._2, target._3)
      if !block.solid then resetBreaking()
      else
        val sameTarget = breakingBlock != null && breakingBlock == target
        if !sameTarget then
          breakingBlock = target
          breakingProgress = 0f
        breakingProgress += dt / blockHardness(block)
        if breakingProgress >= 1f then
          breakTargetBlock(dropItem = true)
          resetBreaking()
    }

  private def resetBreaking(): Unit =
    breakingBlock = null
    breakingProgress = 0f

  private def triggerSandFallAbove(x: Int, y: Int, z: Int): Unit =
    // Falling sand is disabled for now: sand behaves as a normal solid block until
    // its visuals and physics are rebuilt to a higher standard.
    ()

  private def triggerSandFallBelow(x: Int, y: Int, z: Int): Unit =
    // Falling sand is disabled for now: sand behaves as a normal solid block until
    // its visuals and physics are rebuilt to a higher standard.
    ()

  private def loadedChunkForBlock(x: Int, z: Int): Option[Chunk] =
    chunks.get((chunkCoordBlock(x), chunkCoordBlock(z)))

  private def waterLevelAt(x: Int, y: Int, z: Int): Int =
    if y < 0 || y >= Terrain.worldHeight then 0
    else
      loadedChunkForBlock(x, z) match
        case Some(chunk) =>
          val cx = chunkCoordBlock(x); val cz = chunkCoordBlock(z)
          chunk.getWaterLevel(x - cx * Terrain.chunkSize, y, z - cz * Terrain.chunkSize)
        case None => 0

  private def waterRawLevelAt(x: Int, y: Int, z: Int): Int =
    if y < 0 || y >= Terrain.worldHeight then 0
    else
      loadedChunkForBlock(x, z) match
        case Some(chunk) =>
          val cx = chunkCoordBlock(x); val cz = chunkCoordBlock(z)
          chunk.getWaterRawLevel(x - cx * Terrain.chunkSize, y, z - cz * Terrain.chunkSize)
        case None => 0

  private def setWaterLevelAt(x: Int, y: Int, z: Int, level: Int): Unit =
    if y >= 0 && y < Terrain.worldHeight then
      loadedChunkForBlock(x, z).foreach { chunk =>
        val cx = chunkCoordBlock(x); val cz = chunkCoordBlock(z)
        val next = level.max(0).min(8)
        chunk.setWaterLevel(x - cx * Terrain.chunkSize, y, z - cz * Terrain.chunkSize, next)
        if next > 0 then waterLevels((x, y, z)) = next.toByte else waterLevels.remove((x, y, z))
      }

  private def markWaterActive(x: Int, y: Int, z: Int): Unit =
    if y >= 0 && y < Terrain.worldHeight && loadedChunkForBlock(x, z).isDefined && activeBlockAt(x, y, z) == Block.Water then
      val key = (x, y, z)
      if waterFlowQueued.size < maxQueuedWaterCells && !waterFlowQueued.contains(key) then
        waterFlowQueue.enqueue(key)
        waterFlowQueued += key

  private def wakeWaterAround(x: Int, y: Int, z: Int): Unit =
    markWaterActive(x, y, z)
    markWaterActive(x + 1, y, z)
    markWaterActive(x - 1, y, z)
    markWaterActive(x, y, z + 1)
    markWaterActive(x, y, z - 1)
    markWaterActive(x, y + 1, z)
    markWaterActive(x, y - 1, z)

  private def waterCanEnter(block: Block): Boolean =
    block == Block.Air || block == Block.Water

  private def setFlowWater(x: Int, y: Int, z: Int, level: Int): Unit =
    if y >= 0 && y < Terrain.worldHeight && loadedChunkForBlock(x, z).isDefined then
      if activeBlockAt(x, y, z) != Block.Water then setActiveBlock(x, y, z, Block.Water)
      val nextLevel = level.max(1).min(8)
      setWaterLevelAt(x, y, z, nextLevel)
      networkBlockOverrides((x, y, z)) = Block.Water
      networkWaterLevelOverrides((x, y, z)) = nextLevel.toByte
      if multiplayerMode && gameServer != null then gameServer.broadcastBlockChange(x, y, z, Block.Water.id, nextLevel)
      markWaterActive(x, y, z)
      dirtyChunkAt(x, z)

  private def clearFlowWater(x: Int, y: Int, z: Int): Unit =
    if loadedChunkForBlock(x, z).isDefined && activeBlockAt(x, y, z) == Block.Water then
      setWaterLevelAt(x, y, z, 0)
      setActiveBlock(x, y, z, Block.Air)
      networkBlockOverrides((x, y, z)) = Block.Air
      networkWaterLevelOverrides.remove((x, y, z))
      if multiplayerMode && gameServer != null then gameServer.broadcastBlockChange(x, y, z, Block.Air.id)
      dirtyChunkAt(x, z)
      wakeWaterAround(x, y, z)

  private def updateWater(dt: Float): Unit =
    waterFlowTimer += dt
    if waterFlowTimer < waterFlowInterval then return
    waterFlowTimer = 0f
    if waterFlowQueue.isEmpty then return

    var processed = 0
    val budget = math.min(maxWaterUpdatesPerFrame, waterFlowQueue.size)
    while processed < budget && waterFlowQueue.nonEmpty do
      val (wx, wy, wz) = waterFlowQueue.dequeue()
      waterFlowQueued -= ((wx, wy, wz))
      simulateWaterCell(wx, wy, wz)
      processed += 1

  private def simulateWaterCell(wx: Int, wy: Int, wz: Int): Unit =
    if loadedChunkForBlock(wx, wz).isEmpty || activeBlockAt(wx, wy, wz) != Block.Water then
      waterLevels.remove((wx, wy, wz))
      return

    val rawLevel = waterRawLevelAt(wx, wy, wz)
    val level = (if rawLevel <= 0 then 8 else rawLevel).max(1).min(8)
    val supportedFromAbove = activeBlockAt(wx, wy + 1, wz) == Block.Water
    val source = rawLevel <= 0 || (rawLevel >= 8 && !supportedFromAbove)
    val dirs = Array((1, 0), (-1, 0), (0, 1), (0, -1))
    def fedByHorizontalFlow: Boolean =
      dirs.exists { case (dx, dz) =>
        val nx = wx + dx
        val nz = wz + dz
        activeBlockAt(nx, wy, nz) == Block.Water && waterLevelAt(nx, wy, nz) > level
      }

    if wy > 0 then
      val below = activeBlockAt(wx, wy - 1, wz)
      if below == Block.Air then
        val downLevel = if source then 7 else level
        setFlowWater(wx, wy - 1, wz, downLevel)
        if !source && !supportedFromAbove && !fedByHorizontalFlow then
          clearFlowWater(wx, wy, wz)
          wakeWaterAround(wx, wy - 1, wz)
          return
        else if !source && supportedFromAbove && !fedByHorizontalFlow then
          setWaterLevelAt(wx, wy, wz, level)
          markWaterActive(wx, wy, wz)
          wakeWaterAround(wx, wy - 1, wz)
          return
        else
          setWaterLevelAt(wx, wy, wz, if source then 8 else level)
          markWaterActive(wx, wy, wz)
        wakeWaterAround(wx, wy - 1, wz)
      else if below == Block.Water then
        val belowLevel = waterLevelAt(wx, wy - 1, wz)
        val desiredBelow = if source then 7 else level
        if belowLevel > 0 && belowLevel < desiredBelow then
          setWaterLevelAt(wx, wy - 1, wz, desiredBelow)
          dirtyChunkAt(wx, wz)
          markWaterActive(wx, wy - 1, wz)
          if source || supportedFromAbove || waterLevelAt(wx, wy, wz) > 1 then markWaterActive(wx, wy, wz)
          return
        if !source && supportedFromAbove && !fedByHorizontalFlow then
          markWaterActive(wx, wy, wz)
          return

    val targetLevel = if source then 7 else level - 1
    var changed = false
    if targetLevel > 0 then
      val rot = ((wx * 31 + wz * 17 + wy * 13) & 3)
      var i = 0
      while i < 4 do
        val (dx, dz) = dirs((i + rot) & 3)
        val nx = wx + dx; val nz = wz + dz
        val nb = activeBlockAt(nx, wy, nz)
        if nb == Block.Air then
          setFlowWater(nx, wy, nz, targetLevel)
          changed = true
        else if nb == Block.Water then
          val nLevel = waterLevelAt(nx, wy, nz)
          if nLevel > 0 && nLevel + 1 < targetLevel then
            setWaterLevelAt(nx, wy, nz, targetLevel)
            dirtyChunkAt(nx, nz)
            markWaterActive(nx, wy, nz)
            changed = true
        i += 1

    if changed then
      dirtyChunkAt(wx, wz)
      if source || waterLevelAt(wx, wy, wz) > 1 then markWaterActive(wx, wy, wz)
    else if !source then
      // Drain disconnected non-source water gradually when it has no stronger neighbor.
      // This keeps old flows from hovering forever after a source is removed.
      val supported =
        supportedFromAbove ||
        dirs.exists { case (dx, dz) =>
          val nx = wx + dx; val nz = wz + dz
          activeBlockAt(nx, wy, nz) == Block.Water && waterLevelAt(nx, wy, nz) > level
        }
      if !supported then
        val next = level - 1
        if next <= 0 then clearFlowWater(wx, wy, wz)
        else
          setWaterLevelAt(wx, wy, wz, next)
          dirtyChunkAt(wx, wz)
          markWaterActive(wx, wy, wz)

  private def updateSandFalling(): Unit =
    sandFallQueue.clear()

  private def visibleSandFall(x: Float, y: Float, z: Float, startY: Float): Unit =
    ()

  private def updateSandParticles(dt: Float): Unit =
    fallingSandParticles.clear()

  private def blockHardness(block: Block): Float = block match
    case Block.Leaves | Block.BirchLeaves | Block.PineLeaves | Block.AcaciaLeaves | Block.Snow => 0.18f
    case Block.Dirt | Block.Sand | Block.Grass => 0.42f
    case Block.Wood | Block.BirchWood | Block.PineWood | Block.AcaciaWood | Block.Planks | Block.BirchPlanks | Block.PinePlanks | Block.AcaciaPlanks => 0.75f
    case Block.Glass => 0.35f
    case Block.Stone | Block.Coal | Block.Copper | Block.Clay | Block.IronOre => 1.15f
    case Block.GoldOre => 1.50f
    case Block.Brick | Block.Furnace | Block.FurnaceLit => 1.35f
    case Block.Diamond => 2.50f
    case Block.Bedrock => 9999f
    case _ => 0.5f

  private def damagePlayer(amount: Float, reason: String): Unit =
    if gameMode == GameMode.Survival && amount > 0f then
      val old = playerHealth
      playerHealth = (playerHealth - amount).max(0f)
      if old > playerHealth then
        timeSinceLastDamage = 0f
        val shown = (math.ceil(amount.toDouble * 2.0) / 2.0).toFloat
        addChatMessage(s"Took $shown damage${if reason.nonEmpty then s" from $reason" else ""}")
      if playerHealth <= 0f then
        val spawn = findSpawn()
        camera = spawn
        velocity = Vec3(0f, 0f, 0f)
        onGround = false
        fallPeakY = camera.y
        playerHealth = maxPlayerHealth
        playerFood = 20f
        timeSinceLastDamage = healthRegenDelay
        addChatMessage("You woke up at spawn")

  private def updateHealthRegen(dt: Float): Unit =
    if gameMode == GameMode.Survival && playerHealth > 0f && playerHealth < maxPlayerHealth then
      timeSinceLastDamage += dt
      if timeSinceLastDamage >= healthRegenDelay then playerHealth = (playerHealth + healthRegenRate * dt).min(maxPlayerHealth)
    else if playerHealth >= maxPlayerHealth then
      playerHealth = maxPlayerHealth
      timeSinceLastDamage = healthRegenDelay

  private def handleLandingImpact(): Unit =
    if gameMode == GameMode.Survival && waterSubmersion < 0.25f then
      val distance = (fallPeakY - camera.y).max(0f)
      if distance > 4.2f then
        damagePlayer((distance - 3.2f) * 0.85f, "falling")

  private def playerTouching(block: Block, extra: Float = 0.05f): Boolean =
    val half = 0.32f + extra
    val minX = floor(camera.x - half).toInt; val maxX = floor(camera.x + half).toInt
    val minY = floor(camera.y - 1.62f).toInt; val maxY = floor(camera.y + 0.20f).toInt
    val minZ = floor(camera.z - half).toInt; val maxZ = floor(camera.z + half).toInt
    (minX to maxX).exists(x => (minY to maxY).exists(y => (minZ to maxZ).exists(z => activeBlockAt(x, y, z) == block)))

  private def updateEnvironmentalDamage(dt: Float): Unit =
    if cactusDamageCooldown > 0f then cactusDamageCooldown = (cactusDamageCooldown - dt).max(0f)
    if gameMode == GameMode.Survival && cactusDamageCooldown <= 0f && playerTouching(Block.Cactus, 0.08f) then
      cactusDamageCooldown = 0.70f
      damagePlayer(1f, "cactus")

  private def movePlayer(dx: Float, dy: Float, dz: Float): Unit =
    val step = 0.35f
    if dx != 0f then
      val next = camera + Vec3(dx, 0f, 0f)
      if !collidesPlayer(next) then camera = next
      else
        val nextHigh = camera + Vec3(dx, step, 0f)
        if dy >= 0f && !collidesPlayer(nextHigh) then
          camera = nextHigh
          onGround = false
    if dz != 0f then
      val next = camera + Vec3(0f, 0f, dz)
      if !collidesPlayer(next) then camera = next
      else
        val nextHigh = camera + Vec3(0f, step, dz)
        if dy >= 0f && !collidesPlayer(nextHigh) then
          camera = nextHigh
          onGround = false
    if dy != 0f then
      val next = camera + Vec3(0f, dy, 0f)
      if !collidesPlayer(next) then
        camera = next
        if dy > 0f || velocity.y > 0f then fallPeakY = camera.y.max(fallPeakY)
        else if !onGround then fallPeakY = camera.y.max(fallPeakY)
        onGround = false
      else if dy < 0f then
        handleLandingImpact()
        onGround = true
        fallPeakY = camera.y
        velocity = Vec3(0f, 0f, 0f)
      else
        velocity = Vec3(0f, 0f, 0f)

  private def moveVertical(dy: Float): Unit =
    var remaining = dy
    while abs(remaining) > 0.001f do
      val step = remaining.max(-0.22f).min(0.22f)
      movePlayer(0f, step, 0f)
      if step < 0f && onGround then return
      if step > 0f && velocity.y == 0f then return
      remaining -= step

  private def collidesPlayer(pos: Vec3): Boolean =
    val half = 0.32f
    val minX = floor(pos.x - half).toInt; val maxX = floor(pos.x + half).toInt
    val minY = floor(pos.y - 1.62f).toInt; val maxY = floor(pos.y + 0.20f).toInt
    val minZ = floor(pos.z - half).toInt; val maxZ = floor(pos.z + half).toInt
    (minX to maxX).exists(x => (minY to maxY).exists(y => (minZ to maxZ).exists(z => activeBlockAt(x, y, z).solid)))

  private def hasGroundBelow(pos: Vec3): Boolean =
    val half = 0.30f; val checkY = floor(pos.y - 1.63f).toInt
    val minX = floor(pos.x - half).toInt; val maxX = floor(pos.x + half).toInt
    val minZ = floor(pos.z - half).toInt; val maxZ = floor(pos.z + half).toInt
    (minX to maxX).exists(x => (minZ to maxZ).exists(z => activeBlockAt(x, checkY, z).solid))

  private def toggleGameMode(): Unit =
    gameMode = if gameMode == GameMode.Survival then GameMode.Creative else GameMode.Survival
    if gameMode == GameMode.Survival then flyEnabled = false
    velocity = Vec3(0f, 0f, 0f); onGround = false

  private def toggleFullscreen(): Unit =
    fullscreen = !fullscreen
    if fullscreen then
      val mode = glfwGetVideoMode(glfwGetPrimaryMonitor())
      queryWindowPos()
      glfwSetWindowMonitor(window, glfwGetPrimaryMonitor(), 0, 0, mode.width, mode.height, mode.refreshRate)
    else
      glfwSetWindowMonitor(window, NULL, windowedX.max(0), windowedY.max(0), windowedW, windowedH, GLFW_DONT_CARE)

  private def down(key: Int): Boolean = glfwGetKey(window, key) == GLFW_PRESS

  private def blockAt(pos: Vec3): Block = activeBlockAt(floor(pos.x).toInt, floor(pos.y).toInt, floor(pos.z).toInt)

  private def isUnderwater: Boolean = blockAt(camera) == Block.Water

  private def waterSubmersion: Float =
    val probes = Array(camera, camera + Vec3(0f, -0.72f, 0f), camera + Vec3(0f, -1.45f, 0f))
    probes.count(p => blockAt(p) == Block.Water).toFloat / probes.length

  protected def chunkCoordBlock(v: Int): Int = Math.floorDiv(v, Terrain.chunkSize)
  private def chunkCoordPos(v: Float): Int = floor(v / Terrain.chunkSize.toFloat).toInt

  private def activeBlockAt(x: Int, y: Int, z: Int): Block =
    if y < 0 || y >= Terrain.worldHeight then return Block.Air
    val cx = chunkCoordBlock(x)
    val cz = chunkCoordBlock(z)
    val lx = x - cx * Terrain.chunkSize
    val lz = z - cz * Terrain.chunkSize
    chunks.get((cx, cz)) match
      case Some(chunk) => chunk.getBlock(lx, y, lz)
      case None =>
        val key = (java.lang.Long.rotateLeft(worldSeed, 21) ^ (x.toLong << 32) ^ (z.toLong & 0xffffffffL))
        val h = terrainHeightCache.getOrElseUpdate(key, terrainGen.heightAt(x, z))
        if y > h then
          if y <= Terrain.seaLevel then Block.Water else Block.Air
        else if y == h then terrainGen.surfaceBlock(x, y, z, h)
        else terrainGen.fillBlock(x, y, z, h)

  private def setActiveBlock(x: Int, y: Int, z: Int, block: Block): Unit =
    if y < 0 || y >= Terrain.worldHeight then return
    val cx = chunkCoordBlock(x)
    val cz = chunkCoordBlock(z)
    val lx = x - cx * Terrain.chunkSize
    val lz = z - cz * Terrain.chunkSize
    chunks.get((cx, cz)).foreach { chunk =>
      val oldBlock = chunk.getBlock(lx, y, lz)
      chunk.setBlock(lx, y, lz, block)
      if block == Block.Water then
        setWaterLevelAt(x, y, z, 8)
        markWaterActive(x, y, z)
      else
        setWaterLevelAt(x, y, z, 0)
        waterLevels.remove((x, y, z))
      if oldBlock != block then wakeWaterAround(x, y, z)
    }

  private def getWorldBlock(x: Int, y: Int, z: Int): Block =
    if y < 0 || y >= Terrain.worldHeight then Block.Air
    else
      val cx = chunkCoordBlock(x)
      val cz = chunkCoordBlock(z)
      chunks.get((cx, cz)) match
        case Some(c) => c.getBlock(x - cx * Terrain.chunkSize, y, z - cz * Terrain.chunkSize)
        case None => Block.Air

  private def processNetworkMessages(): Unit =
    def handleNetworkLine(line: String): Unit =
      val parts = line.split("\\|", -1)
      try
        if line.startsWith("WORLD|") then
          if parts.length >= 5 && gameServer == null then
            val seed = parts(1).toLong
            val sx = parseNetworkFloat(parts(2)); val sy = parseNetworkFloat(parts(3)); val sz = parseNetworkFloat(parts(4))
            if parts.length >= 6 then
              localColorId = normalizeColorId(parts(5).toInt)
              rememberPlayerColor(playerName, localColorId)
            if parts.length >= 8 then
              val hostOnlineName = networkSafeName(parts(6))
              val hostColor = normalizeColorId(parts(7).toInt)
              knownPlayerNames += hostOnlineName
              rememberPlayerColor(hostOnlineName, hostColor)
            if seed != worldSeed then
              worldSeed = seed
              worldName = "Multiplayer-" + java.lang.Long.toUnsignedString(worldSeed, 36).toUpperCase
              terrainGen = TerrainGenerator(worldSeed)
              terrainHeightCache.clear()
              pendingNetworkBlocks.clear()
              networkBlockOverrides.clear()
              networkWaterLevelOverrides.clear()
              waterLevels.clear()
              waterFlowQueue.clear()
              waterFlowQueued.clear()
              clearLoadedChunks(saveFirst = false)
            camera = Vec3(sx, sy, sz)
            fallPeakY = camera.y
            velocity = Vec3(0f, 0f, 0f); onGround = false
            syncChunks()
            forceCameraChunkRing(0)
            processChunkWorkMainThread(0, 12)
            ensureNearbyMeshesReady(1)
            addChatMessage("Joined host world")
        else if line.startsWith("BLOC|") then
          if parts.length >= 5 then
            val x = parts(1).toInt; val y = parts(2).toInt; val z = parts(3).toInt
            val blockId = parts(4).toByte
            val block = Block.fromId(blockId)
            val waterLevel =
              if block == Block.Water && parts.length >= 6 then
                try parts(5).toInt.max(1).min(8) catch case _: Exception => 8
              else if block == Block.Water then 8 else 0
            networkBlockOverrides((x, y, z)) = block
            if block == Block.Water then networkWaterLevelOverrides((x, y, z)) = waterLevel.toByte
            else networkWaterLevelOverrides.remove((x, y, z))
            val cx = chunkCoordBlock(x); val cz = chunkCoordBlock(z)
            if chunks.contains((cx, cz)) then
              setActiveBlock(x, y, z, block)
              if block == Block.Water then setWaterLevelAt(x, y, z, waterLevel)
              dirtyChunkAt(x, z)
            else
              pendingNetworkBlocks((x, y, z)) = block
        else if line.startsWith("SNAPBEGIN|") || line.startsWith("SNAPEND") then
          ()
        else if line.startsWith("OPLIST|") then
          if gameServer == null then
            oppedPlayerNames.clear()
            parts.drop(1).foreach(n => setOppedName(n, value = true, announce = false))
        else if line.startsWith("OP|") then
          if parts.length >= 3 then
            val name = networkSafeName(parts(1))
            val value = parts(2) == "1" || parts(2).equalsIgnoreCase("true")
            setOppedName(name, value)
        else if line.startsWith("GMODE|") then
          if parts.length >= 3 then
            val name = networkSafeName(parts(1))
            if name.equalsIgnoreCase(playerName) then
              gameMode = GameMode.fromOrdinal(parts(2).toInt)
              velocity = Vec3(0f, 0f, 0f); onGround = false
              addChatMessage(s"Game mode set to $gameMode")
        else if line.startsWith("FLY|") then
          if parts.length >= 3 then
            val name = networkSafeName(parts(1))
            if name.equalsIgnoreCase(playerName) then toggleFlyForPlayer(playerName)
        else if line.startsWith("GIVE|") then
          if parts.length >= 4 then
            val name = networkSafeName(parts(1))
            if name.equalsIgnoreCase(playerName) then
              val block = Block.fromId(parts(2).toInt.toByte)
              val count = parts(3).toInt.max(1).min(999)
              gainItem(block, count)
              addChatMessage(s"Received $count ${blockName(block)}")
        else if line.startsWith("VITAL|") then
          if parts.length >= 4 then
            val name = networkSafeName(parts(1))
            if name.equalsIgnoreCase(playerName) then
              val hp = parseNetworkFloat(parts(2))
              val food = parseNetworkFloat(parts(3))
              if hp >= 0f then playerHealth = hp.max(0f).min(maxPlayerHealth)
              if food >= 0f then playerFood = food.max(0f).min(maxPlayerFood)
              val label = if parts.length >= 5 then networkUnescape(parts.drop(4).mkString("|")) else "Updated"
              addChatMessage(label + " " + playerName)
        else if line.startsWith("TIME|") then
          if parts.length >= 2 then setTimeCommand(parts(1).toLowerCase)
        else if line.startsWith("RCMD|") then
          if gameServer != null && parts.length >= 3 then
            val sender = networkSafeName(parts(1))
            val commandText = networkUnescape(parts.drop(2).mkString("|"))
            parseCommand(commandText, Some(sender))
        else if line.startsWith("POS|") then
          if parts.length >= 7 then
            val name = networkSafeName(parts(1))
            if name.nonEmpty then knownPlayerNames += name
            if name != playerName then
              val x = parseNetworkFloat(parts(2)); val y = parseNetworkFloat(parts(3)); val z = parseNetworkFloat(parts(4))
              val pyaw = parseNetworkFloat(parts(5)); val ppitch = parseNetworkFloat(parts(6))
              val colorId = if parts.length >= 8 then normalizeColorId(parts(7).toInt) else colorForPlayer(name)
              rememberPlayerColor(name, colorId)
              remotePlayers(name) = RemotePlayer(name, Vec3(x, y, z), pyaw, ppitch, glfwGetTime(), colorId)
        else if line.startsWith("TPOS|") then
          if parts.length >= 5 then
            val name = networkSafeName(parts(1))
            val x = parseNetworkFloat(parts(2)); val y = parseNetworkFloat(parts(3)); val z = parseNetworkFloat(parts(4))
            if name.equalsIgnoreCase(playerName) then
              camera = Vec3(x, y, z)
              fallPeakY = camera.y
              velocity = Vec3(0f, 0f, 0f)
              addChatMessage("Teleported")
            else
              remotePlayers.get(name).foreach { rp =>
                remotePlayers(name) = RemotePlayer(name, Vec3(x, y, z), rp.yaw, rp.pitch, glfwGetTime(), rp.colorId)
              }
        else if line.startsWith("CHAT|") then
          if parts.length >= 3 then
            val name = networkSafeName(parts(1))
            val text = networkUnescape(parts.drop(2).mkString("|")).take(220)
            if name != playerName then addChatMessage(s"<$name> $text")
        else if line.startsWith("PLAYERS|") then
          parts.drop(1).foreach { p =>
            parsePlayerToken(p).foreach { case (name, colorId) =>
              if name.nonEmpty then knownPlayerNames += name
              rememberPlayerColor(name, colorId)
            }
          }
        else if line.startsWith("JOIN|") then
          if parts.length >= 2 then
            val name = networkSafeName(parts(1))
            val colorId = if parts.length >= 3 then normalizeColorId(parts(2).toInt) else colorForPlayer(name)
            if name.nonEmpty then knownPlayerNames += name
            rememberPlayerColor(name, colorId)
            if name != playerName then addChatMessage(s"$name joined the game")
        else if line.startsWith("LEFT|") then
          if parts.length >= 2 then
            val name = networkSafeName(parts(1))
            knownPlayerNames -= name
            remotePlayers.remove(name)
            if name != playerName then addChatMessage(s"$name left the game")
      catch
        case e: Exception => System.err.println(s"Ignored bad network packet '$line': ${Option(e.getMessage).getOrElse(e.getClass.getSimpleName)}")

    if gameClient != null then
      var msg = gameClient.pollMessage()
      while msg.isDefined do
        handleNetworkLine(msg.get)
        msg = gameClient.pollMessage()

    if gameServer != null then
      var msg = gameServer.pollMessage()
      while msg.isDefined do
        handleNetworkLine(msg.get)
        msg = gameServer.pollMessage()

  private def sendPlayerPositionNetwork(): Unit =
    if multiplayerMode then
      val now = glfwGetTime()
      if now - lastPosSend >= 0.10 then
        lastPosSend = now
        val msg = "POS|" + networkEscape(playerName) + "|" + networkFloat(camera.x) + "|" + networkFloat(camera.y) + "|" + networkFloat(camera.z) + "|" + networkFloat(yaw) + "|" + networkFloat(pitch) + "|" + localColorId.toString
        if gameClient != null && gameClient.isConnected then gameClient.send(msg)
        else if gameServer != null then gameServer.broadcast(msg)
      if gameServer != null && now - lastMultiplayerHeartbeat >= 2.0 then
        lastMultiplayerHeartbeat = now
        gameServer.broadcastPlayerList()

  private def cleanupRemotePlayers(): Unit =
    val now = glfwGetTime()
    val stale = remotePlayers.collect { case (name, rp) if now - rp.lastSeen > 8.0 => name }.toList
    stale.foreach { name =>
      remotePlayers.remove(name)
      knownPlayerNames -= name
      if multiplayerMode && name != playerName then addChatMessage(s"$name left the game")
    }

  private def render(): Unit =
    updateFramebufferSize()
    // Hard reset the render state every frame. Immediate-mode OpenGL state is global;
    // one debug/wireframe/menu/name-tag pass leaking state can make the next screen look
    // like a dark blue void or shrink/warp the UI. Reset first, then each pass opts in.
    resetGlArraysAndBuffers()
    glPolygonMode(GL_FRONT_AND_BACK, GL_FILL)
    glDisable(GL_FOG)
    glDisable(GL_TEXTURE_2D)
    glDisable(GL_CULL_FACE)
    glDepthMask(true)
    glEnable(GL_BLEND)
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA)
    val (clearR, clearG, clearB) = clearColor
    glClearColor(clearR, clearG, clearB, 1f)
    glViewport(0, 0, framebufferWidth, framebufferHeight)
    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT)
    screen match
      case Screen.Playing =>
        renderWorld()
        renderPlayerNameTags()
        if isUnderwater then renderUnderwaterOverlay()
        renderHud()
        renderHotbar()
        renderCrosshair()
        renderChat()
        modManager.fireHudRender(lastFrameTime.toFloat)
        modLeftClickedThisFrame = false
        if modUiCursorMode then
          glDisable(GL_DEPTH_TEST); glDisable(GL_CULL_FACE); setupOrtho()
          renderTextShadow(18f * uiScale, framebufferHeight - 32f * uiScale, "F8: close mod cursor", 0.82f, 0.92f, 1f, (0.78f * uiScale).max(0.68f))
        if debugMode then renderDebugOverlay()
      case Screen.MainMenu => renderMainMenu()
      case Screen.CreateWorld => renderCreateWorld()
      case Screen.LoadWorld => renderLoadWorldMenu()
      case Screen.Mods => renderModsScreen()
      case Screen.Resources => renderResourcesScreen()
      case Screen.Settings => renderSettings()
      case Screen.Paused =>
        renderWorld()
        renderPlayerNameTags()
        if multiplayerMode then
          renderHud()
          renderChat()
        renderPauseMenu()
      case Screen.Inventory => renderWorld(); renderSurvivalInventory()
      case Screen.Catalog => renderWorld(); renderCatalog()
      case Screen.FurnaceUI => renderWorld(); renderFurnaceUI()
      case Screen.JoinGame => renderJoinGame()
      case Screen.HostGame => renderHostGame()

  private def renderWorld(): Unit =
    if !centerChunkReady then
      forceCameraChunkRing(0)
      processChunkWorkMainThread(0, 8)
    resetGlArraysAndBuffers()
    glPolygonMode(GL_FRONT_AND_BACK, if wireframeMode then GL_LINE else GL_FILL)
    glDepthMask(true)
    glEnable(GL_DEPTH_TEST)
    glDisable(GL_CULL_FACE)
    glEnable(GL_FOG)
    val (fogR, fogG, fogB) = clearColor
    val isUnder = isUnderwater
    if isUnder then
      glFogi(GL_FOG_MODE, GL_LINEAR); glFogf(GL_FOG_START, 2f); glFogf(GL_FOG_END, 18f)
    else
      val visibleDist = renderDistanceBlocks.toFloat
      glFogi(GL_FOG_MODE, GL_LINEAR)
      glFogf(GL_FOG_START, visibleDist * 0.65f)
      glFogf(GL_FOG_END, visibleDist * 0.95f)
    glFogfv(GL_FOG_COLOR, floatBuffer(fogR, fogG, fogB, 1f))
    glMatrixMode(GL_PROJECTION); glLoadIdentity()
    val aspect = framebufferWidth.toDouble / framebufferHeight.toDouble
    // Keep the near plane away from zero so the depth buffer has useful
    // precision. Tiny near values caused early z-fighting at distance.
    val near = 0.25; val far = (renderDistanceBlocks + Terrain.chunkSize * 2).toDouble
    val sprintFov = if screen == Screen.Playing then
      val moving = down(GLFW_KEY_W) || down(GLFW_KEY_S) || down(GLFW_KEY_A) || down(GLFW_KEY_D)
      if moving && (down(GLFW_KEY_LEFT_CONTROL) || down(GLFW_KEY_RIGHT_CONTROL)) then 8.5f else 0f
    else 0f
    val top = tan(toRadians(fov + sprintFov) / 2.0) * near
    glFrustum(-top * aspect, top * aspect, -top, top, near, far)
    glMatrixMode(GL_MODELVIEW); glLoadIdentity()
    glRotatef(pitch, 1f, 0f, 0f)
    glRotatef(yaw, 0f, 1f, 0f)
    // X/Z camera-relative rendering: chunk VBOs are local and translated by
    // (chunkBase - camera). Only vertical camera motion remains in the view matrix.
    glTranslatef(0f, -camera.y, 0f)
    if !isUnder then renderSky()
    renderBubbles()
    renderFallingSand()
    restoreChunkRenderState(isUnder, fogR, fogG, fogB)
    val chunkList = chunks.values.toBuffer
    // Batch opaque pass - bind texture + client states once for all chunks
    glDisable(GL_BLEND)
    glDepthMask(true)
    glEnable(GL_DEPTH_TEST)
    glEnable(GL_TEXTURE_2D); activeAtlas.bind()
    glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_MODULATE)
    glEnableClientState(GL_VERTEX_ARRAY); glEnableClientState(GL_COLOR_ARRAY); glEnableClientState(GL_TEXTURE_COORD_ARRAY)
    chunkList.foreach(_.drawOpaque(camera))
    // Batch cutout pass
    glEnable(GL_ALPHA_TEST); glAlphaFunc(GL_GREATER, 0.5f)
    chunkList.foreach(_.drawCutout(camera))
    glDisable(GL_ALPHA_TEST)
    // Batch translucent + water pass
    glEnable(GL_BLEND)
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA)
    glDepthMask(false)
    val farToNearChunks = chunkList.sortBy { c =>
      val dx = (c.cx * Terrain.chunkSize + Terrain.chunkSize * 0.5f) - camera.x
      val dz = (c.cz * Terrain.chunkSize + Terrain.chunkSize * 0.5f) - camera.z
      -(dx * dx + dz * dz)
    }
    farToNearChunks.foreach(_.drawTranslucent(camera))
    // Water should read as a solid block volume. Keep the same texture/color, but draw
    // it depth-writing and unblended so stacked water cannot show internal faces.
    glDisable(GL_BLEND)
    glDepthMask(true)
    chunkList.foreach(_.drawWater(camera))
    glDepthMask(true)
    glDisableClientState(GL_TEXTURE_COORD_ARRAY); glDisableClientState(GL_COLOR_ARRAY); glDisableClientState(GL_VERTEX_ARRAY)
    glBindBuffer(GL_ARRAY_BUFFER, 0); glBindTexture(GL_TEXTURE_2D, 0)
    renderRemotePlayers3D()
    renderTargetOutline()
    if debugMode then renderRayLine()
    if showChunkBorders then renderChunkBorders()
    glPolygonMode(GL_FRONT_AND_BACK, GL_FILL)
    val darkness = nightDarkness
    if darkness > 0.01f then
      setupOrtho()
      glDisable(GL_DEPTH_TEST)
      glDisable(GL_FOG)
      rect(0, 0, framebufferWidth, framebufferHeight, 0.02f, 0.04f, 0.12f, darkness)
    glPolygonMode(GL_FRONT_AND_BACK, GL_FILL)
    glDepthMask(true)
    glDisable(GL_FOG)

  private def restoreChunkRenderState(isUnder: Boolean, fogR: Float, fogG: Float, fogB: Float): Unit =
    // renderSky(), particles, name tags, and UI all mutate global OpenGL state.
    // The world chunk pass must explicitly restore depth writes/testing or random translucent
    // quads / water / player boxes can draw as big black sheets and terrain can appear to vanish.
    resetGlArraysAndBuffers()
    glPolygonMode(GL_FRONT_AND_BACK, if wireframeMode then GL_LINE else GL_FILL)
    glViewport(0, 0, framebufferWidth, framebufferHeight)
    glMatrixMode(GL_PROJECTION)
    // projection/modelview are already set by renderWorld; just restore render state here.
    glMatrixMode(GL_MODELVIEW)
    glEnable(GL_DEPTH_TEST)
    glDepthMask(true)
    glDepthFunc(GL_LEQUAL)
    glDisable(GL_CULL_FACE)
    glDisable(GL_ALPHA_TEST)
    glDisable(GL_BLEND)
    glEnable(GL_FOG)
    if isUnder then
      glFogi(GL_FOG_MODE, GL_LINEAR); glFogf(GL_FOG_START, 2f); glFogf(GL_FOG_END, 18f)
    else
      val visibleDist = renderDistanceBlocks.toFloat
      glFogi(GL_FOG_MODE, GL_LINEAR)
      glFogf(GL_FOG_START, visibleDist * 0.65f)
      glFogf(GL_FOG_END, visibleDist * 0.95f)
    glFogfv(GL_FOG_COLOR, floatBuffer(fogR, fogG, fogB, 1f))
    glColor4f(1f, 1f, 1f, 1f)

  private def renderRemotePlayers3D(): Unit =
    if remotePlayers.isEmpty then return
    resetGlArraysAndBuffers()
    val now = glfwGetTime()
    glDisable(GL_TEXTURE_2D)
    glDisable(GL_FOG)
    glDisable(GL_BLEND)
    glDepthMask(true)
    glEnable(GL_DEPTH_TEST)
    for (_, rp) <- remotePlayers do
      if now - rp.lastSeen <= 8.0 then
        val p = rp.pos
        val feetY = p.y - 1.62f
        val (cr, cg, cb) = colorForId(rp.colorId)
        glPushMatrix()
        glTranslatef(p.x - camera.x, 0f, p.z - camera.z)
        glRotatef(-rp.yaw, 0f, 1f, 0f)
        drawColoredBox(-0.30f, feetY, -0.20f, 0.30f, feetY + 1.05f, 0.20f, cr, cg, cb, 1f)
        drawColoredBox(-0.23f, feetY + 1.05f, -0.23f, 0.23f, feetY + 1.48f, 0.23f, 0.90f, 0.74f, 0.55f, 1f)
        drawColoredBox(-0.18f, feetY - 0.02f, -0.18f, -0.02f, feetY + 0.62f, 0.18f, cr * 0.72f, cg * 0.72f, cb * 0.72f, 1f)
        drawColoredBox(0.02f, feetY - 0.02f, -0.18f, 0.18f, feetY + 0.62f, 0.18f, cr * 0.72f, cg * 0.72f, cb * 0.72f, 1f)
        glPopMatrix()
    glEnable(GL_FOG)

  private def drawColoredBox(x0: Float, y0: Float, z0: Float, x1: Float, y1: Float, z1: Float, r: Float, g: Float, b: Float, a: Float): Unit =
    glColor4f(r, g, b, a)
    glBegin(GL_QUADS)
    glVertex3f(x0, y0, z1); glVertex3f(x1, y0, z1); glVertex3f(x1, y1, z1); glVertex3f(x0, y1, z1)
    glVertex3f(x1, y0, z0); glVertex3f(x0, y0, z0); glVertex3f(x0, y1, z0); glVertex3f(x1, y1, z0)
    glVertex3f(x0, y0, z0); glVertex3f(x0, y0, z1); glVertex3f(x0, y1, z1); glVertex3f(x0, y1, z0)
    glVertex3f(x1, y0, z1); glVertex3f(x1, y0, z0); glVertex3f(x1, y1, z0); glVertex3f(x1, y1, z1)
    glVertex3f(x0, y1, z1); glVertex3f(x1, y1, z1); glVertex3f(x1, y1, z0); glVertex3f(x0, y1, z0)
    glVertex3f(x0, y0, z0); glVertex3f(x1, y0, z0); glVertex3f(x1, y0, z1); glVertex3f(x0, y0, z1)
    glEnd()

  private def projectWorldToScreen(pos: Vec3): Option[(Float, Float, Float)] =
    val rel = pos - camera
    val yr = toRadians(yaw).toFloat
    val pr = toRadians(pitch).toFloat
    val cy = cos(yr).toFloat; val sy = sin(yr).toFloat
    val cp = cos(pr).toFloat; val sp = sin(pr).toFloat
    val x1 = cy * rel.x + sy * rel.z
    val z1 = -sy * rel.x + cy * rel.z
    val y2 = cp * rel.y - sp * z1
    val z2 = sp * rel.y + cp * z1
    if z2 >= -0.12f then return None
    val aspect = framebufferWidth.toFloat / framebufferHeight.toFloat.max(1f)
    val tanHalf = tan(toRadians(fov) / 2.0).toFloat
    val ndcX = (x1 / -z2) / (tanHalf * aspect)
    val ndcY = (y2 / -z2) / tanHalf
    if ndcX < -1.35f || ndcX > 1.35f || ndcY < -1.35f || ndcY > 1.35f then None
    else Some(((ndcX * 0.5f + 0.5f) * framebufferWidth, (0.5f - ndcY * 0.5f) * framebufferHeight, -z2))

  private def renderPlayerNameTags(): Unit =
    if remotePlayers.isEmpty then return
    glDisable(GL_DEPTH_TEST); glDisable(GL_CULL_FACE); setupOrtho()
    val now = glfwGetTime()
    for (_, rp) <- remotePlayers.toSeq.sortBy { case (_, rp) => (rp.pos - camera).lengthSquared }.reverse do
      if now - rp.lastSeen <= 8.0 then
        projectWorldToScreen(rp.pos + Vec3(0f, 0.72f, 0f)).foreach { case (sx, sy, dist) =>
          val label = rp.name
          val scale = (1.22f - dist * 0.008f).max(0.86f).min(1.18f) * uiScale.max(0.92f)
          val tw = textWidth(label, scale)
          val boxW = tw + 12f * uiScale
          val boxH = 18f * scale + 7f * uiScale
          val x = (sx - boxW / 2f).max(4f).min(framebufferWidth.toFloat - boxW - 4f)
          val y = (sy - boxH / 2f).max(4f).min(framebufferHeight.toFloat - boxH - 4f)
          val (cr, cg, cb) = colorForId(rp.colorId)
          rect(x + 2f, y + 2f, boxW, boxH, 0f, 0f, 0f, 0.36f)
          rect(x, y, boxW, boxH, cr * 0.10f, cg * 0.10f, cb * 0.10f, 0.78f)
          centeredTextFit(x + boxW / 2f, y + 4f * uiScale, label, 1f, 0.95f, 0.64f, scale, boxW - 8f * uiScale)
        }
    glEnable(GL_DEPTH_TEST)

  private def renderHud(): Unit =
    glDisable(GL_DEPTH_TEST); glDisable(GL_CULL_FACE); setupOrtho()
    val s = uiScale
    val hudText = s"Blockbox | ${gameMode} | ESC pause | F2 settings"
    val hudScale = (1.10f * s).max(1.02f)
    // Intentionally no translucent panel here: it was the source of an intermittent stray world-space sheet artifact.
    renderTextShadow(18f * s, 16f * s, hudText, 1f, 1f, 1f, hudScale)
    if gameMode == GameMode.Survival then renderHealth()
    renderBlockInfo()
    if down(GLFW_KEY_TAB) then renderPlayerListOverlay()

  private def renderPlayerListOverlay(): Unit =
    if !multiplayerMode && remotePlayers.isEmpty then return
    glDisable(GL_DEPTH_TEST); glDisable(GL_CULL_FACE); setupOrtho()
    val s = uiScale
    val names = (Seq(playerName) ++ knownPlayerNames.toSeq ++ remotePlayers.keys.toSeq)
      .map(networkSafeName)
      .filter(_.nonEmpty)
      .distinct
      .sortBy(n => if n == playerName then "" else n.toLowerCase)
    val rowH = (30f * s).max(24f)
    val panelW = (300f * s).max(240f).min(framebufferWidth.toFloat - 32f)
    val panelH = (52f * s + rowH * names.length).max(84f).min(framebufferHeight.toFloat - 32f)
    val x = framebufferWidth.toFloat / 2f - panelW / 2f
    val y = 44f * s
    rect(x + 4f * s, y + 4f * s, panelW, panelH, 0f, 0f, 0f, 0.32f)
    rect(x, y, panelW, panelH, 0.045f, 0.055f, 0.082f, 0.88f)
    rect(x, y, panelW, 3f * s, 0.95f, 0.78f, 0.26f, 0.75f)
    centeredTextFit(x + panelW / 2f, y + 14f * s, s"Players (${names.length})", 1f, 0.92f, 0.58f, (1.10f * s).max(0.96f), panelW - 24f * s)
    var yy = y + 40f * s
    for name <- names.take(((panelH - 48f * s) / rowH).toInt.max(1)) do
      val isSelf = name == playerName
      rect(x + 12f * s, yy - 3f * s, panelW - 24f * s, rowH - 2f * s, if isSelf then 0.16f else 0.08f, if isSelf then 0.13f else 0.09f, if isSelf then 0.06f else 0.12f, if isSelf then 0.55f else 0.38f)
      renderTextShadow(x + 22f * s, yy, if isSelf then s"$name (You)" else name, if isSelf then 1f else 0.86f, if isSelf then 0.88f else 0.92f, if isSelf then 0.55f else 1f, (1.02f * s).max(0.90f))
      yy += rowH
    glEnable(GL_DEPTH_TEST)

  private def drawHeartIcon(x: Float, y: Float, size: Float, fillFrac: Float, bgAlpha: Float): Unit =
    BlockboxRender2D.drawHeartIcon(x, y, size, fillFrac, bgAlpha)

  private def renderHealth(): Unit =
    glDisable(GL_DEPTH_TEST); glDisable(GL_CULL_FACE); setupOrtho()
    val s = uiScale
    val unit = (18f * s).max(14f).min(22f)
    val gap = (unit + 4f * s).max(unit + 3f).min(unit + 7f)
    val totalWidth = 10f * gap - (gap - unit)
    val startX = framebufferWidth / 2f - totalWidth / 2f
    val hotbarSlot = (46f * s).min(((framebufferWidth.toFloat - 36f * s) / 10f - 5f * s).max(24f)).max(30f)
    val barY = framebufferHeight.toFloat - hotbarSlot - 42f * s
    val bgAlpha = 0.46f
    for i <- 0 until 10 do
      val x = startX + i * gap
      val threshold = i * 2 + 2
      val fill =
        if playerHealth >= threshold then 1f
        else if playerHealth == threshold - 1 then 0.5f
        else 0f
      drawHeartIcon(x, barY, unit, fill, bgAlpha)

  private def renderBlockInfo(): Unit =
    if framebufferHeight < 200 then return
    raycast(8f).foreach { hit =>
      val block = activeBlockAt(hit.block._1, hit.block._2, hit.block._3)
      val by = framebufferHeight.toFloat
      val hp = hit.block; val waterLvl = if block == Block.Water then waterLevelAt(hp._1, hp._2, hp._3) else -1
      val blockLabel = if waterLvl >= 0 then s"> ${block} (level $waterLvl/8)" else s"> ${block}"
      renderTextShadow(18, by - 90, blockLabel, 0.85f, 0.92f, 1f, 0.80f)
      renderTextShadow(18, by - 110, s"${hit.block._1}, ${hit.block._2}, ${hit.block._3}", 0.70f, 0.80f, 0.90f, 0.65f)
      if breakingProgress > 0f then
        rect(18, by - 132, 112, 8, 0.02f, 0.02f, 0.02f, 0.70f)
        rect(20, by - 130, 108 * breakingProgress.max(0f).min(1f), 4, 0.85f, 0.85f, 0.85f, 0.85f)
    }

  private var frameTimeAccum = 0.0
  private var frameCount = 0
  private var fpsDisplay = 0

  private def renderDebugOverlay(): Unit =
    glDisable(GL_DEPTH_TEST); glDisable(GL_CULL_FACE); setupOrtho()
    frameCount += 1; frameTimeAccum += lastFrameTime
    if frameTimeAccum >= 1.0 then
      fpsDisplay = (frameCount / frameTimeAccum).round.toInt
      frameCount = 0; frameTimeAccum = 0.0
    val ccx = chunkCoordPos(camera.x)
    val ccz = chunkCoordPos(camera.z)
    val fwd = viewDirection
    var ly = 20; val lx = 18
    val bg = 0.05f; val a = 0.65f
    rect(lx - 6, ly - 4, 340, 180 + 22 * 3, bg, bg, bg, a)
    renderTextShadow(lx, ly, s"F3: Debug  [F4:wire ${onOff(wireframeMode)}] [F5:borders ${onOff(showChunkBorders)}]", 0.6f, 0.8f, 1f, 0.88f * uiScale); ly += (20 * uiScale).toInt.max(18)
    renderTextShadow(lx, ly, s"FPS: $fpsDisplay  Frame: ${(lastFrameTime * 1000).toInt}ms  Chunks: ${chunks.size}", 0.4f, 1f, 0.4f, 0.88f * uiScale); ly += (19 * uiScale).toInt.max(16)
    renderTextShadow(lx, ly, s"XYZ: ${camera.x}%.3f / ${camera.y}%.3f / ${camera.z}%.3f", 0.4f, 1f, 0.4f, 0.88f * uiScale); ly += (19 * uiScale).toInt.max(16)
    renderTextShadow(lx, ly, s"Yaw: $yaw%.2f  Pitch: $pitch%.2f", 0.4f, 1f, 0.4f, 0.88f * uiScale); ly += (19 * uiScale).toInt.max(16)
    renderTextShadow(lx, ly, s"Chunk: ($ccx, $ccz)  Seed: $worldSeed  RD: ${renderDistance}ch/${renderDistanceBlocks}b", 0.4f, 1f, 0.4f, 0.88f * uiScale); ly += (19 * uiScale).toInt.max(16)
    renderTextShadow(lx, ly, s"World: $worldName", 0.4f, 1f, 0.4f, 0.72f * uiScale); ly += (17 * uiScale).toInt.max(14)
    renderTextShadow(lx, ly, s"Forward: (${fwd.x}%.4f, ${fwd.y}%.4f, ${fwd.z}%.4f)", 0.4f, 1f, 0.4f, 0.88f * uiScale); ly += (19 * uiScale).toInt.max(16)
    raycast(8f).foreach { hit =>
      ly += 4; rect(lx - 6, ly - 4, 340, 62, bg, bg, bg, a)
      val block = activeBlockAt(hit.block._1, hit.block._2, hit.block._3)
      renderTextShadow(lx, ly, s"Hit: (${hit.block._1}, ${hit.block._2}, ${hit.block._3}) $block", 1f, 0.8f, 0.3f, 0.88f * uiScale); ly += (19 * uiScale).toInt.max(16)
      renderTextShadow(lx, ly, s"Normal: (${hit.normal._1}, ${hit.normal._2}, ${hit.normal._3})  D: ${hit.distance}%.3f", 1f, 0.8f, 0.3f, 0.88f * uiScale); ly += (19 * uiScale).toInt.max(16)
      renderTextShadow(lx, ly, s"Place: (${hit.place._1}, ${hit.place._2}, ${hit.place._3})", 1f, 0.8f, 0.3f, 0.88f * uiScale); ly += (19 * uiScale).toInt.max(16)
    }
    glEnable(GL_DEPTH_TEST)

  private def renderHotbar(): Unit =
    compactHotbarAssignments()
    glDisable(GL_DEPTH_TEST); glDisable(GL_CULL_FACE); setupOrtho()
    val s = uiScale; val total = hotbarBlocks.length
    val maxSlotByWidth = ((framebufferWidth.toFloat - 36f * s) / total.toFloat - 5f * s).max(24f)
    val slotSize = (52f * s).min(maxSlotByWidth).max(34f)
    val gap = (5f * s).min(8f).max(3f)
    val barW = slotSize * total + gap * (total - 1)
    val startX = ((framebufferWidth.toFloat - barW) / 2f).max(8f)
    val y = (framebufferHeight.toFloat - slotSize - 12f * s).max(8f)
    val labelBandH = (16f * s).max(13f).min(slotSize * 0.34f)
    rect(startX - 8f * s, y - 8f * s, barW + 16f * s, slotSize + 16f * s, 0.01f, 0.015f, 0.025f, 0.34f)
    for i <- 0 until total do
      val sx = startX + i * (slotSize + gap)
      val selected = i == selectedBlock
      val block = hotbarBlocks(i)
      val hasItem = block != Block.Air && (gameMode == GameMode.Creative || hotbarCounts(i) > 0)
      rect(sx - 2f * s, y - 2f * s, slotSize + 4f * s, slotSize + 4f * s, 0f, 0f, 0f, 0.45f)
      rect(sx, y, slotSize, slotSize, if selected then 0.16f else 0.07f, if selected then 0.17f else 0.075f, if selected then 0.20f else 0.09f, if selected then 0.96f else 0.72f)
      rect(sx + 1f * s, y + 1f * s, slotSize - 2f * s, labelBandH, 0.28f, 0.30f, 0.35f, if selected then 0.42f else 0.20f)
      rect(sx + 1f * s, y + 1f * s + labelBandH, slotSize - 2f * s, 1f * s, 0f, 0f, 0f, 0.22f)
      if selected then
        rect(sx - 3f * s, y - 3f * s, slotSize + 6f * s, 2f * s, 1f, 0.92f, 0.45f, 0.75f)
        rect(sx - 3f * s, y + slotSize + 1f * s, slotSize + 6f * s, 2f * s, 1f, 0.92f, 0.45f, 0.75f)
        rect(sx - 3f * s, y - 3f * s, 2f * s, slotSize + 6f * s, 1f, 0.92f, 0.45f, 0.75f)
        rect(sx + slotSize + 1f * s, y - 3f * s, 2f * s, slotSize + 6f * s, 1f, 0.92f, 0.45f, 0.75f)
      if hasItem then
        val icon = (slotSize * 0.50f).min(24f * s).max(16f)
        val iconY = y + labelBandH + ((slotSize - labelBandH) - icon) / 2f + 2f * s
        renderBlockIcon(block, sx + (slotSize - icon) / 2f, iconY, icon)
        if gameMode == GameMode.Survival then
          val count = hotbarCounts(i)
          if count > 0 then
            val ns = count.toString
            val countScale = (1.12f * s).max(0.94f).min(slotSize / 16f)
            val tw = textWidth(ns, countScale)
            val nx = sx + slotSize - tw - 5f * s
            val ny = y + slotSize - 12f * countScale - 2f * s
            rect(nx - 2f * s, ny - 1f * s, tw + 4f * s, 11f * countScale + 3f * s, 0f, 0f, 0f, 0.64f)
            renderTextShadow(nx, ny, ns, 1f, 1f, 1f, countScale)
      val label = hotbarLabel(i)
      centeredTextBox(sx + 1f * s, y + 1f * s, slotSize - 2f * s, labelBandH, label, 0.97f, 0.97f, 0.99f, (1.14f * s).max(0.96f).min(slotSize / 12.8f))

  private def hotbarLabel(index: Int): String = if index < 9 then (index + 1).toString else if index == 9 then "0" else (index - 9).toString

  private def renderBlockIcon(block: Block, x: Float, y: Float, size: Float): Unit =
    glEnable(GL_TEXTURE_2D); activeAtlas.bind(); glColor4f(1f, 1f, 1f, 1f)
    val (u0, v0) = activeAtlas.uv(block, FaceKind.Top, 0f, 0f)
    val (u1, v1) = activeAtlas.uv(block, FaceKind.Top, 1f, 1f)
    glBegin(GL_QUADS)
    glTexCoord2f(u0, v0); glVertex2f(x, y)
    glTexCoord2f(u1, v0); glVertex2f(x + size, y)
    glTexCoord2f(u1, v1); glVertex2f(x + size, y + size)
    glTexCoord2f(u0, v1); glVertex2f(x, y + size)
    glEnd()
    glBindTexture(GL_TEXTURE_2D, 0); glDisable(GL_TEXTURE_2D)

  private def renderCrosshair(): Unit =
    BlockboxRender2D.renderCrosshair(framebufferWidth, framebufferHeight)

  private def renderUnderwaterOverlay(): Unit =
    BlockboxRender2D.renderUnderwaterOverlay(framebufferWidth, framebufferHeight, glfwGetTime().toFloat)

  private def clearColor: (Float, Float, Float) =
    BlockboxSky.clearColor(isUnderwater, dayPhase, daylightFactor)

  protected val dayLengthSeconds = BlockboxSky.dayLengthSeconds
  protected val nightLengthSeconds = BlockboxSky.nightLengthSeconds

  private def gameTime: Float = timeOverride.getOrElse(glfwGetTime().toFloat)

  private def cycleClock: Float =
    BlockboxSky.cycleClock(gameTime)

  private def dayPhase: Float =
    BlockboxSky.dayPhase(gameTime)

  private def daylightFactor: Float =
    BlockboxSky.daylightFactor(dayPhase)

  private def smooth01(v: Float): Float =
    BlockboxSky.smooth01(v)

  private def nightDarkness: Float =
    BlockboxSky.nightDarkness(multiplayerMode, daylightFactor)

  private def generateStars(): Unit =
    if starPositions != null then return
    starPositions = BlockboxSky.generateStars()

  private def renderSky(): Unit =
    generateStars()
    BlockboxSky.renderSky(yaw, pitch, dayPhase, daylightFactor, starPositions)

  private def updateBubbles(dt: Float): Unit =
    if isUnderwater then
      while bubbleParticles.length > 80 do bubbleParticles.remove(bubbleParticles.length - 1)
      if (glfwGetTime() * 15).toInt % 3 == 0 then
        val rng = new Random()
        val offsetX = (rng.nextFloat() - 0.5f) * 0.6f
        val offsetZ = (rng.nextFloat() - 0.5f) * 0.6f
        bubbleParticles += ((camera.x + offsetX, camera.y + rng.nextFloat() * 1.4f, camera.z + offsetZ, 1.5f + rng.nextFloat() * 2f))
    for i <- bubbleParticles.indices do
      val (x, y, z, life) = bubbleParticles(i)
      val ny = y + 2.5f * dt
      val npos = Vec3(x, ny, z)
      if ny > camera.y + 2f || blockAt(npos).solid || life <= 0f then
        bubbleParticles(i) = (x, ny, z, life - dt)
      else bubbleParticles(i) = (x, ny, z, life)
    bubbleParticles.filterInPlace((_, _, _, life) => life > 0f)

  private def renderBubbles(): Unit =
    if bubbleParticles.isEmpty then return
    glDisable(GL_TEXTURE_2D)
    glDisable(GL_FOG)
    glEnable(GL_POINT_SMOOTH)
    glPointSize(4f)
    val t = glfwGetTime().toFloat
    glBegin(GL_POINTS)
    for (x, y, z, life) <- bubbleParticles do
      val bob = sin(t * 3f + x * 10f + z * 10f).toFloat * 0.05f
      val alpha = (life / 2f).max(0.15f).min(0.6f)
      glColor4f(0.85f, 0.92f, 1f, alpha)
      glVertex3f(x - camera.x + bob, y, z - camera.z + bob)
    glEnd()
    glPointSize(1f)
    glDisable(GL_POINT_SMOOTH)
    glEnable(GL_TEXTURE_2D)
    glEnable(GL_FOG)

  private def renderFallingSand(): Unit =
    // Falling sand rendering is disabled with the physics toggle above.
    ()

  private def renderChat(): Unit =
    glDisable(GL_DEPTH_TEST); glDisable(GL_CULL_FACE); setupOrtho()
    val w = framebufferWidth.toFloat; val h = framebufferHeight.toFloat
    val chatWidth = w.min(600f)
    var ly = h - 40f
    val maxDisplay = 12
    var count = 0
    for (msg, t) <- chatMessages.view.reverse do
      if count < maxDisplay && t > 0f then
        val alpha = t.min(1f)
        val chatScale = (1.28f * uiScale).max(1.08f)
        if ly > 60 then
          rect(10f, ly - 4f, textWidth(msg, chatScale) + 12f, 18f * chatScale + 4f, 0f, 0f, 0f, 0.30f * alpha)
          renderTextShadow(14, ly, msg, 1f, 1f, 1f, chatScale)
        ly -= 22f * chatScale.max(0.9f)
        count += 1
    if chatOpen then
      val inputY = h - 38f
      rect(10, inputY, chatWidth - 10, 30, 0f, 0f, 0f, 0.55f)
      rect(12, inputY + 2, chatWidth - 14, 2, 0.30f, 0.30f, 0.35f, 0.25f)
      val display = s"> $chatInput${if (glfwGetTime() * 2).toInt % 2 == 0 then "_" else " "}"
      renderTextShadow(18, inputY + 5, display, 1f, 1f, 1f, (1.34f * uiScale).max(1.14f))
      val suggestions = commandSuggestions(chatInput)
      if suggestions.nonEmpty then
        val boxY = inputY - (suggestions.length * 22f * uiScale + 8f)
        rect(10, boxY, chatWidth - 10, inputY - boxY - 4f, 0f, 0f, 0f, 0.50f)
        suggestions.zipWithIndex.foreach { case (sug, idx) =>
          val selected = idx == commandSuggestionIndex.max(0).min(suggestions.length - 1)
          val y = boxY + 6f + idx * 22f * uiScale
          if selected then rect(14, y - 2f, chatWidth - 22f, 18f * uiScale, 0.20f, 0.22f, 0.28f, 0.72f)
          renderTextShadow(20, y, sug, if selected then 1f else 0.78f, if selected then 0.92f else 0.84f, if selected then 0.55f else 0.96f, (1.0f * uiScale).max(0.88f))
        }

  private def mainMenuLayout(): (Float, Float, Float, Array[Float], Float, Float, Float, Float, Float) =
    BlockboxRender2D.mainMenuLayout(framebufferWidth, framebufferHeight, uiScale)

  private def drawPixelLogo(cx: Float, y: Float, text: String, maxW: Float, maxH: Float): Unit =
    BlockboxRender2D.drawPixelLogo(cx, y, text, maxW, maxH)

  private def renderMainMenu(): Unit =
    glDisable(GL_DEPTH_TEST); glDisable(GL_CULL_FACE); setupOrtho()
    val w = framebufferWidth.toFloat; val h = framebufferHeight.toFloat
    val cx = w / 2f; val t = glfwGetTime().toFloat
    glBegin(GL_QUADS)
    glColor4f(0.035f, 0.050f, 0.115f, 1f); glVertex2f(0, 0)
    glColor4f(0.055f, 0.085f, 0.155f, 1f); glVertex2f(w, 0)
    glColor4f(0.100f, 0.160f, 0.260f, 1f); glVertex2f(w, h * 0.60f)
    glColor4f(0.060f, 0.085f, 0.160f, 1f); glVertex2f(0, h * 0.60f)
    glEnd()
    glBegin(GL_QUADS)
    glColor4f(0.065f, 0.135f, 0.055f, 1f); glVertex2f(0, h * 0.60f)
    glColor4f(0.095f, 0.175f, 0.065f, 1f); glVertex2f(w, h * 0.60f)
    glColor4f(0.145f, 0.210f, 0.075f, 1f); glVertex2f(w, h)
    glColor4f(0.085f, 0.145f, 0.045f, 1f); glVertex2f(0, h)
    glEnd()
    val gridSize = (38f * uiScale).max(22f)
    glLineWidth(1f); glBegin(GL_LINES)
    var gx = 0f
    while gx < w do
      glColor4f(0.30f, 0.36f, 0.24f, 0.035f)
      glVertex2f(gx, h * 0.60f); glVertex2f(gx, h)
      gx += gridSize
    var gy = h * 0.60f
    while gy < h do
      glColor4f(0.30f, 0.36f, 0.24f, 0.035f)
      glVertex2f(0f, gy); glVertex2f(w, gy)
      gy += gridSize
    glEnd()

    val layout = mainMenuLayout()
    val bx = layout._1; val bw = layout._2; val bh = layout._3; val ys = layout._4
    val titleX = layout._5; val titleY = layout._6; val titleW = layout._7; val titleH = layout._8; val s = layout._9
    drawPanel(titleX, titleY, titleW, titleH)
    val innerX = titleX + 18f * s; val innerY = titleY + 14f * s; val innerW = titleW - 36f * s; val innerH = (72f * s).min(titleH - 48f * s).max(54f)
    rect(innerX, innerY, innerW, innerH, 0.055f, 0.075f, 0.125f, 0.62f)
    rect(innerX, innerY, innerW, 2f, 0.70f, 0.55f, 0.24f, 0.46f)
    rect(innerX, innerY + innerH - 2f, innerW, 2f, 0.25f, 0.20f, 0.12f, 0.34f)
    drawPixelLogo(cx, innerY + innerH * 0.13f, "BLOCKBOX", innerW - 60f * s, innerH * 0.50f)
    centeredTextFit(cx + 1f * s, innerY + innerH * 0.80f + 1f * s, "Voxel Sandbox", 0f, 0f, 0f, 0.72f * s, innerW - 28f * s)
    centeredTextFit(cx, innerY + innerH * 0.80f, "Voxel Sandbox", 0.80f, 0.86f, 1f, 0.72f * s, innerW - 28f * s)
    rect(titleX + 34f * s, titleY + titleH - 45f * s, titleW - 68f * s, 1f, 0.30f, 0.34f, 0.42f, 0.22f)
    centeredTextFit(cx, titleY + titleH - 31f * s, "Built with Scala 3 + LWJGL", 0.45f, 0.53f, 0.66f, 0.54f * s, titleW - 70f * s)

    drawButton(bx, ys(0), bw, bh, "Singleplayer", accent = true)
    centeredTextFit(cx, ys(1) - 22f * s, "MULTIPLAYER", 0.50f, 0.56f, 0.68f, 0.46f * s, bw)
    rect(cx - bw * 0.32f, ys(1) - 10f * s, bw * 0.64f, 1f, 0.28f, 0.32f, 0.40f, 0.20f)
    drawButton(bx, ys(1), bw, bh, "Host LAN Game")
    drawButton(bx, ys(2), bw, bh, "Join LAN Game")
    drawButton(bx, ys(3), bw, bh, "Load World")
    drawButton(bx, ys(4), bw, bh, "Mods")
    drawButton(bx, ys(5), bw, bh, "Resources")
    drawButton(bx, ys(6), bw, bh, "Options")
    drawButton(bx, ys(7), bw, bh, "Quit Game")
    centeredTextFit(cx, h - 18f * s, "v1.0 | Scala 3 + LWJGL | Open Source", 0.38f, 0.50f, 0.66f, 0.52f * s, w - 40f * s)

  private def textMetrics(text: String): (java.nio.ByteBuffer, Int, Float, Float, Float, Float) =
    BlockboxRender2D.textMetrics(text)

  private def textWidth(text: String, scale: Float): Float =
    BlockboxRender2D.textWidth(text, scale)

  private def centeredTextFit(cx: Float, y: Float, text: String, r: Float, g: Float, b: Float, scale: Float, maxWidth: Float): Unit =
    BlockboxRender2D.centeredTextFit(cx, y, text, r, g, b, scale, maxWidth, framebufferWidth, framebufferHeight)

  private def centeredText(cx: Float, y: Float, text: String, r: Float, g: Float, b: Float, scale: Float): Unit =
    BlockboxRender2D.centeredText(cx, y, text, r, g, b, scale, framebufferWidth, framebufferHeight)

  private def centeredTextBox(x: Float, y: Float, w: Float, h: Float, text: String, r: Float, g: Float, b: Float, scale: Float): Unit =
    BlockboxRender2D.centeredTextBox(x, y, w, h, text, r, g, b, scale, uiScale, framebufferWidth, framebufferHeight)

  private def renderText(x: Float, y: Float, text: String, r: Float, g: Float, b: Float, scale: Float = 1f): Unit =
    BlockboxRender2D.renderText(x, y, text, r, g, b, scale, framebufferWidth, framebufferHeight)

  private def renderTextBuf(buf: java.nio.ByteBuffer, quads: Int, x: Float, y: Float, r: Float, g: Float, b: Float, scale: Float): Unit =
    BlockboxRender2D.renderTextBuf(buf, quads, x, y, r, g, b, scale, framebufferWidth, framebufferHeight)

  private def renderCreateWorld(): Unit =
    glDisable(GL_DEPTH_TEST); glDisable(GL_CULL_FACE); setupOrtho()
    val w = framebufferWidth.toFloat
    val h = framebufferHeight.toFloat
    glBegin(GL_QUADS)
    glColor4f(0.05f, 0.06f, 0.12f, 1f); glVertex2f(0, 0); glVertex2f(w, 0)
    glColor4f(0.14f, 0.26f, 0.44f, 1f); glVertex2f(w, h); glVertex2f(0, h)
    glEnd()
    rect(0, h * 0.62f, w, h * 0.38f, 0.06f, 0.18f, 0.06f, 0.85f)
    rect(0, h * 0.76f, w, h * 0.24f, 0.22f, 0.16f, 0.08f, 0.80f)

    val cx = w / 2f
    val s = uiScale
    val margin = (18f * s).max(12f)
    val pW = (620f * s).min(w - margin * 2f).max(math.min(360f, w - margin * 2f))
    val pH = (560f * s).min(h - margin * 2f).max(math.min(420f, h - margin * 2f))
    val pX = cx - pW / 2f
    val pY = ((h - pH) / 2f).max(margin)
    drawPanel(pX, pY, pW, pH)

    centeredTextFit(cx, pY + 34f * s, "NEW WORLD", 1f, 0.90f, 0.55f, 2.15f * s, pW - 80f * s)
    rect(pX + 40f * s, pY + 68f * s, pW - 80f * s, 1f, 0.30f, 0.30f, 0.35f, 0.30f)

    val leftX = pX + 40f * s
    val contentW = pW - 80f * s
    val settingW = contentW.max(260f.min(pW - 32f * s))
    val settingH = (36f * s).min(38f).max(28f)
    val settingX = cx - settingW / 2f
    val nameY = pY + 104f * s
    val seedY = pY + 166f * s
    val modeY = pY + 236f * s
    val cheatsY = pY + 282f * s
    renderNameField(settingX, nameY, settingW, 34f * s, "World name", worldNameInput, createWorldNameFocused)
    val seedDisplay = if customSeedInput.nonEmpty then customSeedInput else worldSeed.toString
    renderNameField(settingX, seedY, settingW, 34f * s, "Seed (optional)", seedDisplay, !createWorldNameFocused)
    def settingRow(y: Float, label: String, value: String): Unit =
      val (mx, my) = mouseFramebufferPos()
      val hover = inRect(mx, my, settingX, y, settingW, settingH)
      val br = if hover then 0.13f else 0.09f
      rect(settingX, y, settingW, settingH, br, br * 1.08f, br * 1.22f, 0.66f)
      rect(settingX + 2f * s, y + 2f * s, settingW - 4f * s, 2f * s, 1f, 1f, 1f, 0.05f)
      renderTextShadow(settingX + 16f * s, y + settingH / 2f - 5f * s, label, 0.82f, 0.86f, 0.92f, 0.62f * s)
      centeredTextFit(settingX + settingW - 82f * s, y + settingH / 2f - 5f * s, value, 1f, 0.93f, 0.54f, 0.62f * s, 150f * s)
    settingRow(modeY, "Game Mode", createWorldMode.toString)
    settingRow(cheatsY, "Allow Cheats", onOff(createWorldCheats))

    rect(pX + 40f * s, pY + pH - 186f * s, pW - 80f * s, 1f, 0.30f, 0.30f, 0.35f, 0.22f)
    val buttonW = (pW - 120f * s).max(240f.min(pW - 40f * s))
    val buttonH = (38f * s).min(42f).max(30f)
    val gap = (10f * s).max(6f).min(12f)
    val buttonX = cx - buttonW / 2f
    val buttonStartY = pY + pH - (buttonH * 4f + gap * 3f) - 18f * s
    drawButton(buttonX, buttonStartY, buttonW, buttonH, "Create New World", accent = true)
    drawButton(buttonX, buttonStartY + buttonH + gap, buttonW, buttonH, "Random Seed")
    drawButton(buttonX, buttonStartY + (buttonH + gap) * 2f, buttonW, buttonH, if enterCustomSeed then "Hide Custom Seed" else "Custom Seed")
    drawButton(buttonX, buttonStartY + (buttonH + gap) * 3f, buttonW, buttonH, "Back")
    centeredTextFit(cx, pY + pH - 10f * s, "Tab switches fields | N name | C seed | M mode | H cheats", 0.42f, 0.48f, 0.60f, 0.46f * s, pW - 48f * s)

  private def renderLoadWorldMenu(): Unit =
    glDisable(GL_DEPTH_TEST); glDisable(GL_CULL_FACE); setupOrtho()
    val w = framebufferWidth.toFloat; val h = framebufferHeight.toFloat; val cx = w / 2f; val s = uiScale
    glBegin(GL_QUADS)
    glColor4f(0.05f, 0.06f, 0.12f, 1f); glVertex2f(0, 0); glVertex2f(w, 0)
    glColor4f(0.14f, 0.26f, 0.44f, 1f); glVertex2f(w, h); glVertex2f(0, h)
    glEnd()
    rect(0, h * 0.62f, w, h * 0.38f, 0.06f, 0.18f, 0.06f, 0.85f)
    val pW = (700f * s).min(w * 0.92f); val pH = (520f * s).min(h * 0.90f)
    val pX = cx - pW / 2f; val pY = h / 2f - pH / 2f
    drawPanel(pX, pY, pW, pH)
    centeredTextFit(cx, pY + 34f * s, "SELECT WORLD", 1f, 0.90f, 0.55f, 2.0f * s, pW - 80f * s)
    rect(pX + 34f * s, pY + 64f * s, pW - 68f * s, 1f, 0.30f, 0.30f, 0.35f, 0.30f)
    val saves = worldSaveDirs
    if saves.isEmpty then
      centeredTextFit(cx, pY + 150f * s, "No saved worlds found in worlds/", 0.78f, 0.84f, 0.94f, 0.9f * s, pW - 80f * s)
    else
      val listX = pX + 42f * s
      val listY = pY + 86f * s
      val rowH = (46f * s).max(34f)
      val visible = ((pH - 160f * s) / rowH).toInt.max(3)
      if loadWorldSelection < loadWorldScroll then loadWorldScroll = loadWorldSelection
      if loadWorldSelection >= loadWorldScroll + visible then loadWorldScroll = loadWorldSelection - visible + 1
      loadWorldScroll = loadWorldScroll.max(0).min((saves.length - visible).max(0))
      saves.zipWithIndex.drop(loadWorldScroll).take(visible).foreach { case (dir, idx) =>
        val y = listY + (idx - loadWorldScroll) * rowH
        val selected = idx == loadWorldSelection
        val worldDat = new java.io.File(dir, "world.dat")
        rect(listX, y, pW - 84f * s, rowH - 6f * s, if selected then 0.22f else 0.08f, if selected then 0.20f else 0.09f, if selected then 0.10f else 0.13f, 0.88f)
        if selected then rect(listX, y, 4f * s, rowH - 6f * s, 1f, 0.82f, 0.32f, 0.82f)
        renderTextShadow(listX + 14f * s, y + 7f * s, dir.getName, 0.94f, 0.96f, 1f, 0.76f * s)
        renderTextShadow(listX + 14f * s, y + 25f * s, s"Saved: ${new java.util.Date(worldDat.lastModified()).toString}", 0.50f, 0.58f, 0.70f, 0.48f * s)
      }
    val bw = (220f * s).min(pW * 0.36f); val bh = (38f * s).max(30f)
    drawButton(cx - bw - 12f * s, pY + pH - 56f * s, bw, bh, "Load Selected", accent = true)
    drawButton(cx + 12f * s, pY + pH - 56f * s, bw, bh, "Back")
    centeredTextFit(cx, pY + pH - 12f * s, "Up/Down selects | Enter loads | ESC backs out", 0.42f, 0.48f, 0.60f, 0.46f * s, pW - 80f * s)

  private def handleLoadWorldClick(mx: Float, my: Float): Unit =
    val w = framebufferWidth.toFloat; val h = framebufferHeight.toFloat; val cx = w / 2f; val s = uiScale
    val pW = (700f * s).min(w * 0.92f); val pH = (520f * s).min(h * 0.90f)
    val pX = cx - pW / 2f; val pY = h / 2f - pH / 2f
    val saves = worldSaveDirs
    val listX = pX + 42f * s; val listY = pY + 86f * s; val rowH = (46f * s).max(34f)
    val visible = ((pH - 160f * s) / rowH).toInt.max(3)
    saves.zipWithIndex.drop(loadWorldScroll).take(visible).foreach { case (_, idx) =>
      val y = listY + (idx - loadWorldScroll) * rowH
      if inRect(mx, my, listX, y, pW - 84f * s, rowH - 6f * s) then loadWorldSelection = idx
    }
    val bw = (220f * s).min(pW * 0.36f); val bh = (38f * s).max(30f)
    if inRect(mx, my, cx - bw - 12f * s, pY + pH - 56f * s, bw, bh) then loadSelectedWorld()
    else if inRect(mx, my, cx + 12f * s, pY + pH - 56f * s, bw, bh) then screen = Screen.MainMenu


  private def renderModsScreen(): Unit =
    glDisable(GL_DEPTH_TEST); glDisable(GL_CULL_FACE); setupOrtho()
    val w = framebufferWidth.toFloat; val h = framebufferHeight.toFloat; val cx = w / 2f; val s = uiScale
    glBegin(GL_QUADS)
    glColor4f(0.04f, 0.05f, 0.10f, 1f); glVertex2f(0, 0); glVertex2f(w, 0)
    glColor4f(0.10f, 0.18f, 0.30f, 1f); glVertex2f(w, h); glVertex2f(0, h)
    glEnd()
    val pW = (760f * s).min(w * 0.92f); val pH = (560f * s).min(h * 0.90f)
    val pX = cx - pW / 2f; val pY = h / 2f - pH / 2f
    drawPanel(pX, pY, pW, pH)
    centeredTextFit(cx, pY + 32f * s, "MODS", 1f, 0.94f, 0.55f, 2.0f * s, pW - 80f * s)
    centeredTextFit(cx, pY + 58f * s, s"server modpack: ${modManager.serverModpackHash}", 0.62f, 0.74f, 0.95f, 0.56f * s, pW - 80f * s)
    rect(pX + 34f * s, pY + 76f * s, pW - 68f * s, 1f, 0.30f, 0.34f, 0.42f, 0.30f)
    val mods = modManager.loadedMods
    if mods.isEmpty then
      centeredTextFit(cx, pY + 138f * s, "No mods loaded. Put .jar or Groovy mods in mods/", 0.82f, 0.88f, 1f, 0.78f * s, pW - 90f * s)
    else
      val rowH = (58f * s).max(42f)
      val listX = pX + 42f * s
      val listY = pY + 92f * s
      val visible = ((pH - 170f * s) / rowH).toInt.max(3)
      modsScreenScroll = modsScreenScroll.max(0).min((mods.length - visible).max(0))
      mods.zipWithIndex.drop(modsScreenScroll).take(visible).foreach { case (m, idx) =>
        val y = listY + (idx - modsScreenScroll) * rowH
        val ok = m.loaded
        rect(listX, y, pW - 84f * s, rowH - 6f * s, if ok then 0.08f else 0.20f, if ok then 0.11f else 0.08f, if ok then 0.16f else 0.08f, 0.86f)
        rect(listX, y, 4f * s, rowH - 6f * s, if ok then 0.25f else 0.95f, if ok then 0.75f else 0.25f, if ok then 0.35f else 0.20f, 0.85f)
        renderTextShadow(listX + 14f * s, y + 7f * s, s"${m.name} ${m.version}  [${m.sideLabel}]", 0.96f, 0.97f, 1f, 0.68f * s)
        renderTextShadow(listX + 14f * s, y + 25f * s, m.description.take(96), 0.62f, 0.70f, 0.82f, 0.50f * s)
        renderTextShadow(listX + 14f * s, y + 41f * s, s"${m.id} | ${m.status}", if ok then 0.52f else 1f, if ok then 0.70f else 0.55f, if ok then 0.58f else 0.50f, 0.45f * s)
      }
    val bw = (260f * s).min(pW * 0.44f); val bh = (38f * s).max(30f)
    drawButton(cx - bw / 2f, pY + pH - 56f * s, bw, bh, "Back")
    centeredTextFit(cx, pY + pH - 13f * s, "mods are trusted full-access JVM code. server mods must match host.", 0.48f, 0.55f, 0.68f, 0.46f * s, pW - 70f * s)

  private def handleModsClick(mx: Float, my: Float): Unit =
    val w = framebufferWidth.toFloat; val h = framebufferHeight.toFloat; val cx = w / 2f; val s = uiScale
    val pW = (760f * s).min(w * 0.92f); val pH = (560f * s).min(h * 0.90f)
    val pY = h / 2f - pH / 2f
    val bw = (260f * s).min(pW * 0.44f); val bh = (38f * s).max(30f)
    if inRect(mx, my, cx - bw / 2f, pY + pH - 56f * s, bw, bh) then screen = Screen.MainMenu

  private def renderResourcesScreen(): Unit =
    if resourcePacks.isEmpty then refreshResourcePacks()
    glDisable(GL_DEPTH_TEST); glDisable(GL_CULL_FACE); setupOrtho()
    val w = framebufferWidth.toFloat; val h = framebufferHeight.toFloat; val cx = w / 2f; val s = uiScale
    glBegin(GL_QUADS)
    glColor4f(0.04f, 0.05f, 0.10f, 1f); glVertex2f(0, 0); glVertex2f(w, 0)
    glColor4f(0.11f, 0.19f, 0.30f, 1f); glVertex2f(w, h); glVertex2f(0, h)
    glEnd()
    val pW = (780f * s).min(w * 0.94f); val pH = (565f * s).min(h * 0.90f)
    val pX = cx - pW / 2f; val pY = h / 2f - pH / 2f
    drawPanel(pX, pY, pW, pH)
    centeredTextFit(cx, pY + 32f * s, "RESOURCES", 1f, 0.94f, 0.55f, 2.0f * s, pW - 80f * s)
    centeredTextFit(cx, pY + 58f * s, "Texture packs refresh immediately. Add custom packs in resourcepacks/<pack>/", 0.62f, 0.74f, 0.95f, 0.56f * s, pW - 80f * s)
    rect(pX + 34f * s, pY + 76f * s, pW - 68f * s, 1f, 0.30f, 0.34f, 0.42f, 0.30f)
    val rowH = (64f * s).max(48f)
    val listX = pX + 42f * s
    val listY = pY + 96f * s
    val visible = ((pH - 188f * s) / rowH).toInt.max(3)
    resourcesScroll = resourcesScroll.max(0).min((resourcePacks.length - visible).max(0))
    resourcePacks.zipWithIndex.drop(resourcesScroll).take(visible).foreach { case (pack, idx) =>
      val y = listY + (idx - resourcesScroll) * rowH
      val selected = pack.id == selectedTexturePackId
      val available = pack.legacy || pack.dir.exists(d => d.isDirectory)
      rect(listX, y, pW - 84f * s, rowH - 6f * s, if selected then 0.12f else 0.075f, if selected then 0.14f else 0.09f, if selected then 0.20f else 0.14f, 0.88f)
      rect(listX, y, 4f * s, rowH - 6f * s, if selected then 0.95f else if available then 0.30f else 0.75f, if selected then 0.78f else if available then 0.60f else 0.28f, if selected then 0.28f else if available then 0.85f else 0.22f, 0.90f)
      renderTextShadow(listX + 14f * s, y + 8f * s, pack.name, if selected then 1f else 0.94f, if selected then 0.92f else 0.96f, if selected then 0.60f else 1f, 0.76f * s)
      renderTextShadow(listX + 14f * s, y + 29f * s, s"by ${pack.author}${if selected then "  [ACTIVE]" else ""}", 0.62f, 0.72f, 0.86f, 0.54f * s)
      val pathText = if pack.legacy then "built-in procedural textures" else pack.dir.map(_.getPath).getOrElse("missing")
      renderTextShadow(listX + 14f * s, y + 46f * s, pathText.take(112), if available then 0.46f else 1f, if available then 0.56f else 0.48f, if available then 0.70f else 0.42f, 0.44f * s)
    }
    val bw = (250f * s).min(pW * 0.36f); val bh = (38f * s).max(30f)
    drawButton(cx - bw - 12f * s, pY + pH - 56f * s, bw, bh, "Refresh List")
    drawButton(cx + 12f * s, pY + pH - 56f * s, bw, bh, "Back")
    centeredTextFit(cx, pY + pH - 13f * s, "custom packs: dirt.png, stone.png, grass_side.png, grass_top.png, sand.png, snow.png, water.png", 0.48f, 0.55f, 0.68f, 0.46f * s, pW - 70f * s)

  private def handleResourcesClick(mx: Float, my: Float): Unit =
    val w = framebufferWidth.toFloat; val h = framebufferHeight.toFloat; val cx = w / 2f; val s = uiScale
    val pW = (780f * s).min(w * 0.94f); val pH = (565f * s).min(h * 0.90f)
    val pX = cx - pW / 2f; val pY = h / 2f - pH / 2f
    val rowH = (64f * s).max(48f)
    val listX = pX + 42f * s
    val listY = pY + 96f * s
    val visible = ((pH - 188f * s) / rowH).toInt.max(3)
    val clickedPack = resourcePacks.zipWithIndex.drop(resourcesScroll).take(visible).find { case (_, idx) =>
      val y = listY + (idx - resourcesScroll) * rowH
      inRect(mx, my, listX, y, pW - 84f * s, rowH - 6f * s)
    }.map(_._1)
    clickedPack match
      case Some(pack) => applyTexturePack(pack)
      case None =>
        val bw = (250f * s).min(pW * 0.36f); val bh = (38f * s).max(30f)
        if inRect(mx, my, cx - bw - 12f * s, pY + pH - 56f * s, bw, bh) then refreshResourcePacks()
        else if inRect(mx, my, cx + 12f * s, pY + pH - 56f * s, bw, bh) then screen = resourcesReturnTo

  private def renderSettings(): Unit =
    glDisable(GL_DEPTH_TEST); glDisable(GL_CULL_FACE); setupOrtho()
    val w = framebufferWidth.toFloat; val h = framebufferHeight.toFloat
    glBegin(GL_QUADS)
    glColor4f(0.05f, 0.06f, 0.12f, 1f); glVertex2f(0, 0); glVertex2f(w, 0)
    glColor4f(0.14f, 0.26f, 0.44f, 1f); glVertex2f(w, h); glVertex2f(0, h)
    glEnd()
    rect(0, h * 0.62f, w, h * 0.38f, 0.06f, 0.18f, 0.06f, 0.85f)
    rect(0, h * 0.76f, w, h * 0.24f, 0.22f, 0.16f, 0.08f, 0.80f)
    val cx = w / 2f; val s = uiScale
    val pW = (520f * s).min(w * 0.92f); val pH = (630f * s).min(h * 0.94f); val pX = cx - pW / 2f; val pY = (h / 2f - pH / 2f).max(12f * s)
    drawPanel(pX, pY, pW, pH)
    centeredText(cx, pY + 36 * s, "OPTIONS", 1f, 0.95f, 0.55f, 2.6f * s)
    rect(pX + 40 * s, pY + 68 * s, pW - 80 * s, 1, 0.30f, 0.30f, 0.35f, 0.30f)
    val settingX = pX + 40 * s
    val (smx, smy) = mouseFramebufferPos()
    val rowW = 440f * s; val rowH = 28f * s
    def clickRow(y: Float, label: String): Unit =
      val hover = inRect(smx, smy, settingX, y, rowW, rowH)
      rect(settingX, y, rowW, rowH, if hover then 0.18f else 0.10f, if hover then 0.20f else 0.12f, if hover then 0.26f else 0.16f, 0.85f)
      renderTextShadow(settingX + 12 * s, y + 5 * s, label, 1f, 1f, 1f, (0.82f * s).min(rowH / 15f))
    clickRow(pY + 85 * s, s"Render distance: $renderDistance chunks (${renderDistanceBlocks} blocks)")
    slider(settingX, pY + 118 * s, rowW, (renderDistance - minRenderDistance).toFloat / (maxRenderDistance - minRenderDistance).toFloat)
    clickRow(pY + 150 * s, s"Fog density: $fogDensity%.2f")
    slider(settingX, pY + 185 * s, rowW, (fogDensity - 0.6f) / 2.4f)
    clickRow(pY + 218 * s, f"FOV: $fov%.0f")
    slider(settingX, pY + 253 * s, rowW, (fov - 50f) / 50f)
    var optY = pY + 285 * s
    clickRow(optY, s"Fast movement: ${onOff(fastMove)}"); optY += 40 * s
    clickRow(optY, s"VSync: ${onOff(vsync)}"); optY += 40 * s
    if settingsReturnTo == Screen.Playing && gameServer != null && worldCheatsEnabled then
      clickRow(optY, s"Host game mode: ${gameMode}"); optY += 40 * s
    clickRow(optY, s"Sound effects: ${onOff(soundEnabled)}"); optY += 40 * s
    clickRow(optY, s"Fullscreen: ${onOff(fullscreen)}"); optY += 40 * s
    clickRow(optY, s"Resources: ${selectedTexturePack.name}"); optY += 40 * s
    clickRow(optY, s"Pause ESC: ${if pauseEscReturnsToGame then "Resume game" else "Quit to title"}")
    rect(pX + 40 * s, pY + pH - 65 * s, pW - 80 * s, 1, 0.30f, 0.30f, 0.35f, 0.20f)
    val buttonW = 300f * s; val buttonH = 44f * s; val buttonX = cx - buttonW / 2f
    drawButton(buttonX, pY + pH - 55 * s, buttonW, buttonH, "Done")

  private def renderPauseMenu(): Unit =
    glDisable(GL_DEPTH_TEST); glDisable(GL_CULL_FACE); setupOrtho()
    if multiplayerMode then
      // Multiplayer pause is a local overlay only. Do not black out the world; players/chats keep updating behind it.
      rect(0, 0, framebufferWidth, framebufferHeight, 0f, 0f, 0f, 0.08f)
    else
      dimBackground()
    val cx = framebufferWidth / 2f; val h = framebufferHeight.toFloat; val s = uiScale
    val pw = (420f * s).min(framebufferWidth * 0.80f); val ph = (if multiplayerMode then 300f * s else 275f * s).min(h * 0.82f); val px = cx - pw / 2f; val py = h / 2f - ph / 2f
    drawPanel(px, py, pw, ph)
    centeredText(cx, py + 34 * s, if multiplayerMode then "MULTIPLAYER" else "PAUSED", 1f, 0.95f, 0.55f, if multiplayerMode then 2.05f * s else 2.6f * s)
    if multiplayerMode then centeredTextFit(cx, py + 60f * s, "Client-side pause - server keeps running", 0.68f, 0.78f, 0.95f, 0.72f * s, pw - 70f * s)
    rect(px + 40 * s, py + 76 * s, pw - 80 * s, 1, 0.30f, 0.30f, 0.35f, 0.30f)
    val bw = (320f * s).min(pw - 72f * s); val bh = (40f * s).min(ph / 6f).max(30f); val bx = cx - bw / 2f
    val gap = (14f * s).min(18f)
    val by0 = py + 92f * s
    drawButton(bx, by0, bw, bh, "Back to Game", accent = true)
    drawButton(bx, by0 + bh + gap, bw, bh, "Options")
    drawButton(bx, by0 + (bh + gap) * 2f, bw, bh, if multiplayerMode then "Disconnect to Title" else "Save and Quit to Title")

  private def renderSurvivalInventory(): Unit =
    compactHotbarAssignments()
    glDisable(GL_DEPTH_TEST); glDisable(GL_CULL_FACE); setupOrtho()
    dimBackground()
    val cx = framebufferWidth / 2f; val cy = framebufferHeight / 2f; val s = uiScale
    val pw = (760f * s).min(framebufferWidth * 0.94f)
    val ph = (500f * s).min(framebufferHeight * 0.88f)
    val px = cx - pw / 2f; val py = cy - ph / 2f
    drawPanel(px, py, pw, ph)
    centeredTextFit(cx, py + 24f * s, "Inventory", 1f, 0.95f, 0.65f, 1.18f * s, pw - 80f * s)
    rect(px + 18f * s, py + 44f * s, pw - 36f * s, 1f, 0.30f, 0.30f, 0.35f, 0.25f)

    val (mx, my) = mouseFramebufferPos()
    val gridW = pw * 0.56f
    val cols = 6
    val gap = (6f * s).max(4f)
    val slot = ((gridW - gap * (cols - 1)) / cols).min(44f * s).max(30f)
    val invX = px + 28f * s
    val invY = py + 76f * s
    val items = inventoryItems
    var hoveredBlock: Block | Null = null
    var hoveredX = 0f; var hoveredY = 0f

    rect(invX - 10f * s, invY - 28f * s, gridW + 18f * s, ph - 140f * s, 0.045f, 0.060f, 0.085f, 0.68f)
    renderTextShadow(invX, invY - 20f * s, "Blocks & Items", 0.78f, 0.84f, 0.96f, 0.70f * s)
    for i <- 0 until inventoryGridSlots do
      val col = i % cols; val row = i / cols
      val sx = invX + col * (slot + gap); val sy = invY + row * (slot + gap)
      val hasItem = i < items.length
      val block = if hasItem then items(i) else Block.Air
      val count = if hasItem then inventory(block.ordinal) else 0
      val isSelected = false
      val hover = inRect(mx, my, sx, sy, slot, slot)
      if hover && hasItem then
        hoveredBlock = block; hoveredX = sx + slot / 2f; hoveredY = sy
      val base = if hover then 0.115f else 0.075f
      rect(sx - 1f * s, sy - 1f * s, slot + 2f * s, slot + 2f * s, 0.015f, 0.017f, 0.022f, 0.78f)
      rect(sx, sy, slot, slot, base, base + 0.01f, base + 0.035f, 0.90f)
      rect(sx + 1f * s, sy + 1f * s, slot - 2f * s, 2f * s, 0.18f, 0.20f, 0.24f, 0.16f)
      if isSelected then
        rect(sx - 2f * s, sy - 2f * s, slot + 4f * s, 2f * s, 1f, 0.92f, 0.42f, 0.55f)
        rect(sx - 2f * s, sy + slot, slot + 4f * s, 2f * s, 1f, 0.92f, 0.42f, 0.55f)
        rect(sx - 2f * s, sy - 2f * s, 2f * s, slot + 4f * s, 1f, 0.92f, 0.42f, 0.55f)
        rect(sx + slot, sy - 2f * s, 2f * s, slot + 4f * s, 1f, 0.92f, 0.42f, 0.55f)
      if hasItem && count > 0 then
        val icon = (slot * 0.58f).min(26f * s).max(16f)
        renderBlockIcon(block, sx + (slot - icon) / 2f, sy + (slot - icon) / 2f, icon)
        val ns = count.toString
        val countScale = (0.72f * s).max(0.56f).min(slot / 32f)
        val tw = textWidth(ns, countScale)
        val nx = sx + slot - tw - 5f * s
        val ny = sy + slot - 12f * countScale - 2f * s
        rect(nx - 2f * s, ny - 1f * s, tw + 4f * s, 11f * countScale + 3f * s, 0f, 0f, 0f, 0.64f)
        renderText(nx, ny, ns, 1f, 1f, 1f, countScale)

    val craftPanelW = (pw - gridW - 78f * s).max(210f * s)
    val craftX = px + pw - 28f * s - craftPanelW
    val craftY = py + 76f * s
    rect(craftX - 10f * s, craftY - 28f * s, craftPanelW + 18f * s, ph - 140f * s, 0.045f, 0.060f, 0.085f, 0.68f)
    centeredTextFit(craftX + craftPanelW / 2f, craftY - 20f * s, "Free-form Crafting", 0.82f, 0.88f, 1f, 0.70f * s, craftPanelW)
    centeredTextFit(craftX + craftPanelW / 2f, craftY - 3f * s, "click item, fill any grid shape, click output", 0.52f, 0.58f, 0.68f, 0.43f * s, craftPanelW)
    val cGap = 6f * s
    val cSlot = ((craftPanelW - 56f * s) / 4f).min(42f * s).max(28f)
    val gridStartX = craftX + 12f * s
    val gridStartY = craftY + 28f * s
    var ci = 0
    while ci < 9 do
      val col = ci % 3
      val row0 = ci / 3
      val sx = gridStartX + col * (cSlot + cGap)
      val sy = gridStartY + row0 * (cSlot + cGap)
      val b = craftGridBlocks(ci)
      val hoverCraft = inRect(mx, my, sx, sy, cSlot, cSlot)
      rect(sx - 1f * s, sy - 1f * s, cSlot + 2f * s, cSlot + 2f * s, 0.01f, 0.012f, 0.018f, 0.82f)
      rect(sx, sy, cSlot, cSlot, if hoverCraft then 0.15f else 0.085f, if hoverCraft then 0.16f else 0.095f, if hoverCraft then 0.19f else 0.125f, 0.92f)
      if b != Block.Air then
        val icon = (cSlot * 0.60f).min(25f * s).max(16f)
        renderBlockIcon(b, sx + (cSlot - icon) / 2f, sy + (cSlot - icon) / 2f, icon)
        if hoverCraft then
          hoveredBlock = b; hoveredX = sx + cSlot / 2f; hoveredY = sy
      ci += 1
    val outSize = cSlot * 1.16f
    val outX = craftX + craftPanelW - outSize - 16f * s
    val outY = gridStartY + cSlot + cGap
    val result = currentCraftingResult
    val canCraft = result.exists(canCraftCurrent)
    val hoverOut = inRect(mx, my, outX, outY, outSize, outSize)
    rect(outX - 1f * s, outY - 1f * s, outSize + 2f * s, outSize + 2f * s, 0f, 0f, 0f, 0.86f)
    rect(outX, outY, outSize, outSize, if canCraft then (if hoverOut then 0.26f else 0.18f) else 0.08f, if canCraft then (if hoverOut then 0.30f else 0.22f) else 0.09f, if canCraft then (if hoverOut then 0.24f else 0.16f) else 0.11f, 0.95f)
    centeredTextFit(outX + outSize / 2f, outY - 15f * s, "output", 0.62f, 0.66f, 0.74f, 0.44f * s, outSize + 26f * s)
    result.foreach { r =>
      val icon = (outSize * 0.58f).min(27f * s).max(16f)
      renderBlockIcon(r.output, outX + (outSize - icon) / 2f, outY + (outSize - icon) / 2f, icon)
      centeredTextFit(outX + outSize / 2f, outY + outSize + 8f * s, s"${r.outputCount}x ${blockName(r.output)}", if canCraft then 0.80f else 0.45f, if canCraft then 0.86f else 0.48f, if canCraft then 0.72f else 0.50f, 0.48f * s, craftPanelW * 0.46f)
      if hoverOut then renderTooltip(outX + outSize / 2f, outY - 6f * s, r.label)
    }
    if result.isEmpty && InventoryModel.normalizeCraftGrid(craftGridBlocks).nonEmpty then
      centeredTextFit(craftX + craftPanelW / 2f, craftY + 178f * s, "no matching recipe", 0.72f, 0.48f, 0.45f, 0.48f * s, craftPanelW - 18f * s)
    else if result.nonEmpty && !canCraft then
      centeredTextFit(craftX + craftPanelW / 2f, craftY + 178f * s, "missing ingredients", 0.72f, 0.48f, 0.45f, 0.48f * s, craftPanelW - 18f * s)

    val hotbarPanelY = py + ph - 104f * s
    drawInventoryHotbar(px + 28f * s, hotbarPanelY, pw - 56f * s, s, mx, my)
    rect(px + 18f * s, py + ph - 54f * s, pw - 36f * s, 1f, 0.30f, 0.30f, 0.35f, 0.20f)
    drawButton(px + 22f * s, py + ph - 46f * s, 130f * s, (34f * s).min(ph * 0.08f).max(28f), "Furnace")
    drawButton(px + pw - 104f * s, py + ph - 46f * s, 82f * s, (34f * s).min(ph * 0.08f).max(28f), "Close")
    if hoveredBlock != null then renderTooltip(hoveredX, hoveredY - 8f * s, blockName(hoveredBlock.asInstanceOf[Block]))
    renderHeldInventoryCursor(mx, my)

  private def renderCatalog(): Unit =
    glDisable(GL_DEPTH_TEST); glDisable(GL_CULL_FACE); setupOrtho()
    dimBackground()
    val cx = framebufferWidth / 2f
    val cy = framebufferHeight / 2f
    val s = uiScale
    val panelW = (700f * s).min(framebufferWidth * 0.94f)
    val panelH = (500f * s).min(framebufferHeight * 0.88f)
    val panelX = cx - panelW / 2f
    val panelY = cy - panelH / 2f
    drawPanel(panelX, panelY, panelW, panelH)
    rect(panelX + 8f * s, panelY + 8f * s, panelW - 16f * s, 36f * s, 0.14f, 0.16f, 0.22f, 0.80f)
    renderTextShadow(panelX + 18f * s, panelY + 16f * s, "Block Catalog", 1f, 0.95f, 0.65f, 1.0f * s)
    drawButton(panelX + panelW - 88f * s, panelY + 12f * s, 72f * s, 28f * s, "X")

    val slotSize = (48f * s).max(34f).min(54f)
    val gridX = panelX + 20f * s
    val gridY = panelY + 52f * s
    val hotbarPanelY = panelY + panelH - 76f * s
    val cols = ((panelW - 54f * s) / slotSize).toInt.max(4).min(12)
    val items = catalogItems
    val visibleRows = ((hotbarPanelY - gridY - 26f * s) / slotSize).toInt.max(1)
    val scrollMax = ((items.length - 1) / cols - visibleRows + 2).max(0)
    if catalogScroll > scrollMax then catalogScroll = scrollMax
    val (mx, my) = mouseFramebufferPos()
    for i <- items.indices.drop(catalogScroll * cols).take(cols * visibleRows) do
      val idx = i - catalogScroll * cols
      val col = idx % cols
      val row = idx / cols
      val sx = gridX + col * slotSize
      val sy = gridY + row * slotSize
      val block = items(i)
      val isSelected = hotbarBlocks.isDefinedAt(selectedBlock) && hotbarBlocks(selectedBlock) == block
      val hover = inRect(mx, my, sx, sy, slotSize, slotSize)
      val base = if hover then 0.13f else 0.09f
      rect(sx - 1f * s, sy - 1f * s, slotSize + 2f * s, slotSize + 2f * s, 0.02f, 0.02f, 0.02f, 0.70f)
      rect(sx, sy, slotSize, slotSize, base, base + 0.01f, base + 0.03f, 0.86f)
      rect(sx + 1f * s, sy + 1f * s, slotSize - 2f * s, 2f * s, 0.12f, 0.14f, 0.16f, 0.16f)
      if isSelected then
        rect(sx - 2f * s, sy - 2f * s, slotSize + 4f * s, 2f * s, 1f, 0.92f, 0.45f, 0.55f)
        rect(sx - 2f * s, sy + slotSize, slotSize + 4f * s, 2f * s, 1f, 0.92f, 0.45f, 0.55f)
        rect(sx - 2f * s, sy - 2f * s, 2f * s, slotSize + 4f * s, 1f, 0.92f, 0.45f, 0.55f)
        rect(sx + slotSize, sy - 2f * s, 2f * s, slotSize + 4f * s, 1f, 0.92f, 0.45f, 0.55f)
      val icon = (slotSize * 0.56f).min(28f * s).max(18f)
      renderBlockIcon(block, sx + (slotSize - icon) / 2f, sy + (slotSize - icon) / 2f, icon)
      if heldInventoryBlock == block then
        rect(sx + 5f * s, sy + slotSize - 8f * s, slotSize - 10f * s, 3f * s, 1f, 0.86f, 0.30f, 0.85f)
    if scrollMax > 0 then
      val scrollBarX = panelX + panelW - 16f * s
      val scrollBarH = hotbarPanelY - gridY - 8f * s
      val thumbH = (scrollBarH / (scrollMax + 1)).max(10f * s)
      val thumbY = panelY + 52f * s + (catalogScroll.toFloat / scrollMax.max(1)) * (scrollBarH - thumbH)
      rect(scrollBarX, panelY + 52f * s, 12f * s, scrollBarH, 0.02f, 0.02f, 0.02f, 0.40f)
      rect(scrollBarX, thumbY, 12f * s, thumbH, 0.35f, 0.40f, 0.50f, 0.70f)
    drawInventoryHotbar(panelX + 20f * s, hotbarPanelY, panelW - 40f * s, s, mx, my)
    renderHeldInventoryCursor(mx, my)
    val infoY = panelY + panelH - 18f * s
    rect(panelX + 12f * s, infoY - 2f * s, panelW - 24f * s, 18f * s, 0.06f, 0.08f, 0.12f, 0.50f)
    centeredTextFit(cx, infoY, "catalog: click block, then click hotbar slot | e or esc closes", 0.55f, 0.60f, 0.70f, 0.56f * s, panelW - 32f * s)

  protected def blockName(block: Block): String =
    block.toString.replaceAll("([a-z])([A-Z])", "$1 $2")

  private def inventoryItems: Array[Block] =
    Block.values.filter(b => validHotbarBlock(b) && inventory(b.ordinal) > 0)

  private val inventoryGridSlots = 24

  private def drawInventoryHotbar(panelX: Float, panelY: Float, panelW: Float, s: Float, mx: Float, my: Float): Unit =
    val total = hotbarBlocks.length
    val gap = (5f * s).max(4f)
    val slot = ((panelW - gap * (total - 1)) / total).min(44f * s).max(28f)
    val barW = slot * total + gap * (total - 1)
    val startX = panelX + (panelW - barW) / 2f
    renderTextShadow(panelX, panelY - 19f * s, "Hotbar: click item/slot to assign, mouse wheel cycles in-game", 0.70f, 0.78f, 0.95f, 0.56f * s)
    var i = 0
    while i < total do
      val sx = startX + i * (slot + gap)
      val selected = i == selectedBlock
      val hover = inRect(mx, my, sx, panelY, slot, slot)
      val block = hotbarBlocks(i)
      val hasItem = block != Block.Air && (gameMode == GameMode.Creative || hotbarCounts(i) > 0)
      val base = if hover then 0.14f else 0.085f
      rect(sx - 1f * s, panelY - 1f * s, slot + 2f * s, slot + 2f * s, 0.015f, 0.017f, 0.022f, 0.78f)
      rect(sx, panelY, slot, slot, base, base + 0.01f, base + 0.035f, 0.92f)
      if selected then
        rect(sx - 2f * s, panelY - 2f * s, slot + 4f * s, 2f * s, 1f, 0.92f, 0.42f, 0.55f)
        rect(sx - 2f * s, panelY + slot, slot + 4f * s, 2f * s, 1f, 0.92f, 0.42f, 0.55f)
        rect(sx - 2f * s, panelY - 2f * s, 2f * s, slot + 4f * s, 1f, 0.92f, 0.42f, 0.55f)
        rect(sx + slot, panelY - 2f * s, 2f * s, slot + 4f * s, 1f, 0.92f, 0.42f, 0.55f)
      if hasItem then
        val icon = (slot * 0.58f).min(26f * s).max(16f)
        renderBlockIcon(block, sx + (slot - icon) / 2f, panelY + (slot - icon) / 2f, icon)
        if gameMode == GameMode.Survival then
          val ns = hotbarCounts(i).toString
          val cs = (0.66f * s).max(0.52f).min(slot / 27f)
          val tw = textWidth(ns, cs)
          val tx = (sx + slot - tw - 5f * s).max(sx + 3f * s)
          val ty = panelY + slot - 12f * cs - 2f * s
          rect(tx - 2f * s, ty - 1f * s, tw + 4f * s, 11f * cs + 3f * s, 0f, 0f, 0f, 0.66f)
          renderText(tx, ty, ns, 1f, 1f, 1f, cs)
        if hover && heldInventoryBlock == Block.Air then renderTooltip(sx + slot / 2f, panelY - 6f * s, blockName(block))
      centeredTextFit(sx + slot / 2f, panelY + 3f * s, hotbarLabel(i), 0.96f, 0.96f, 0.98f, 0.54f * s, slot - 6f * s)
      i += 1

  private def renderHeldInventoryCursor(mx: Float, my: Float): Unit =
    if heldInventoryBlock != Block.Air then
      val s = uiScale
      val size = (34f * s).max(24f)
      rect(mx + 10f * s, my + 10f * s, size + 8f * s, size + 8f * s, 0f, 0f, 0f, 0.45f)
      renderBlockIcon(heldInventoryBlock, mx + 14f * s, my + 14f * s, size)
      if gameMode == GameMode.Survival && heldInventoryCount > 1 then
        val ns = heldInventoryCount.toString
        val cs = (0.72f * s).max(0.56f)
        val tw = textWidth(ns, cs)
        renderTextShadow(mx + 14f * s + size - tw - 2f * s, my + 14f * s + size - 12f * cs, ns, 1f, 1f, 1f, cs)
      renderTooltip(mx + 34f * s, my + 12f * s, s"Holding ${blockName(heldInventoryBlock)}")

  private def renderTooltip(anchorX: Float, anchorY: Float, text: String): Unit =
    if text == null || text.isEmpty then return
    val s = uiScale
    val scale = (0.78f * s).max(0.60f)
    val tw = textWidth(text, scale)
    val padX = 7f * s
    val padY = 5f * s
    val boxW = tw + padX * 2f
    val boxH = 22f * scale + 6f * s
    val x = (anchorX - boxW / 2f).max(6f * s).min(framebufferWidth.toFloat - boxW - 6f * s)
    val y = (anchorY - boxH - 4f * s).max(6f * s)
    rect(x + 2f * s, y + 2f * s, boxW, boxH, 0f, 0f, 0f, 0.40f)
    rect(x, y, boxW, boxH, 0.035f, 0.040f, 0.060f, 0.94f)
    rect(x, y, boxW, 1.5f * s, 0.95f, 0.80f, 0.36f, 0.62f)
    renderTextShadow(x + padX, y + padY, text, 0.96f, 0.94f, 0.82f, scale)

  private def catalogItems: Array[Block] =
    Block.values.filter(b => b != Block.Air && b != Block.Water && b != Block.Bedrock && b != Block.FurnaceLit)

  private def renderFurnaceUI(): Unit =
    glDisable(GL_DEPTH_TEST); glDisable(GL_CULL_FACE); setupOrtho()
    dimBackground()
    val cx = framebufferWidth / 2f; val cy = framebufferHeight / 2f; val s = uiScale
    val pw = (680f * s).min(framebufferWidth * 0.94f)
    val ph = (430f * s).min(framebufferHeight * 0.88f)
    val px = cx - pw / 2f; val py = cy - ph / 2f
    drawPanel(px, py, pw, ph)
    centeredTextFit(cx, py + 25f * s, "Furnace", 1f, 0.95f, 0.65f, 1.35f * s, pw - 80f * s)
    rect(px + 28f * s, py + 48f * s, pw - 56f * s, 1f, 0.30f, 0.30f, 0.35f, 0.20f)

    val (mx, my) = mouseFramebufferPos()
    val listX = px + 28f * s
    val listY = py + 82f * s
    val listW = (210f * s).min(pw * 0.34f).max(160f * s)
    val rowH = (38f * s).max(30f)
    rect(listX - 8f * s, listY - 34f * s, listW + 16f * s, rowH * smeltableInputs.length + 48f * s, 0.045f, 0.060f, 0.085f, 0.72f)
    centeredTextFit(listX + listW / 2f, listY - 24f * s, "Choose input", 0.80f, 0.86f, 1f, 0.66f * s, listW)
    val furnaceItems = smeltableInputs.filter(b => smeltResult(b).nonEmpty && totalItemCount(b) > 0)
    for i <- 0 until inventoryGridSlots.min(6) do
      val b = if i < furnaceItems.length then furnaceItems(i) else Block.Air
      val y = listY + i * rowH
      val count = if b == Block.Air then 0 else totalItemCount(b)
      val selected = b != Block.Air && furnaceInput == b
      val hover = inRect(mx, my, listX, y, listW, rowH - 4f * s)
      val br = if selected then 0.30f else if hover then 0.18f else 0.095f
      rect(listX, y, listW, rowH - 4f * s, br, br * 1.06f, br * 1.16f, if b != Block.Air then 0.92f else 0.52f)
      rect(listX + 1f * s, y + 1f * s, listW - 2f * s, 2f * s, 1f, 1f, 1f, 0.06f)
      if b != Block.Air && count > 0 then
        val icon = (rowH * 0.52f).min(22f * s).max(16f)
        renderBlockIcon(b, listX + 8f * s, y + (rowH - icon) / 2f - 2f * s, icon)
        renderTextShadow(listX + 38f * s, y + 8f * s, blockName(b), 0.88f, 0.90f, 0.86f, 0.54f * s)
        centeredTextFit(listX + listW - 28f * s, y + 8f * s, s"x$count", 0.80f, 0.84f, 0.92f, 0.50f * s, 48f * s)

    val workX = listX + listW + 34f * s
    val workW = px + pw - 28f * s - workX
    val slot = (68f * s).min(74f).max(52f)
    def drawFurnaceSlot(x: Float, y: Float, label: String, block: Block, count: Int): Unit =
      rect(x - 1f * s, y - 1f * s, slot + 2f * s, slot + 2f * s, 0.02f, 0.02f, 0.02f, 0.74f)
      rect(x, y, slot, slot, 0.06f, 0.07f, 0.10f, 0.90f)
      rect(x + 2f * s, y + 2f * s, slot - 4f * s, 2f * s, 0.18f, 0.20f, 0.25f, 0.16f)
      centeredTextFit(x + slot / 2f, y + slot + 12f * s, label, 0.68f, 0.74f, 0.86f, 0.52f * s, slot + 30f * s)
      if block != Block.Air && count > 0 then
        val icon = (slot * 0.58f).max(20f)
        renderBlockIcon(block, x + (slot - icon) / 2f, y + (slot - icon) / 2f, icon)
        val c = s"x$count"
        val cs = 0.52f * s
        val tw = textWidth(c, cs)
        rect(x + slot - tw - 7f * s, y + slot - 14f * s, tw + 5f * s, 12f * s, 0f, 0f, 0f, 0.62f)
        renderText(x + slot - tw - 5f * s, y + slot - 13f * s, c, 1f, 1f, 1f, cs)
      else
        centeredTextFit(x + slot / 2f, y + slot / 2f - 5f * s, "empty", 0.34f, 0.38f, 0.48f, 0.48f * s, slot - 8f * s)

    val inputX = workX + 8f * s
    val inputY = py + 94f * s
    val outputX = workX + workW - slot - 8f * s
    val outputY = inputY
    val inputCount = if furnaceInput == Block.Air then 0 else totalItemCount(furnaceInput)
    drawFurnaceSlot(inputX, inputY, "Input", furnaceInput, inputCount)
    drawFurnaceSlot(outputX, outputY, "Output", furnaceOutput, furnaceOutputCount)

    val arrowX = inputX + slot + 30f * s
    val arrowW = (outputX - arrowX - 30f * s).max(80f * s)
    val arrowY = inputY + slot * 0.38f
    rect(arrowX, arrowY, arrowW, 13f * s, 0.02f, 0.02f, 0.025f, 0.72f)
    val progress = (furnaceProgress / 4f).max(0f).min(1f)
    rect(arrowX + 2f * s, arrowY + 2f * s, (arrowW - 4f * s) * progress, 9f * s, 0.95f, 0.58f, 0.16f, 0.82f)
    centeredTextFit(arrowX + arrowW / 2f, arrowY - 16f * s, "smelts instantly per click", 0.50f, 0.56f, 0.68f, 0.46f * s, arrowW + 40f * s)

    val fuelY = py + 212f * s
    rect(workX + 8f * s, fuelY, workW - 16f * s, 58f * s, 0.045f, 0.060f, 0.085f, 0.72f)
    val woodFuelCount = fuelBlocks.filter(_ != Block.Coal).map(totalItemCount).sum
    renderTextShadow(workX + 18f * s, fuelY + 10f * s, s"Fuel: Coal ${totalItemCount(Block.Coal)} | Wood/Planks $woodFuelCount", 0.76f, 0.82f, 0.92f, 0.58f * s)
    val fuelText = if furnaceFuelRemaining > 0f then f"Buffered fuel: $furnaceFuelRemaining%.0f ticks" else "Buffered fuel: empty"
    renderTextShadow(workX + 18f * s, fuelY + 32f * s, fuelText, 0.62f, 0.68f, 0.78f, 0.52f * s)
    val recipeText = furnaceInput match
      case Block.Air => "Pick an input from the list."
      case b => smeltResult(b) match
        case Some((out, amount)) => s"Recipe: ${blockName(b)} -> ${amount}x ${blockName(out)}"
        case None => "That item cannot be smelted."
    centeredTextFit(workX + workW / 2f, py + 294f * s, recipeText, 0.74f, 0.80f, 0.90f, 0.58f * s, workW - 20f * s)

    val bh = (36f * s).min(38f).max(30f)
    val buttonY = py + ph - 52f * s
    val buttonGap = 8f * s
    val totalButtonW = workW - 16f * s
    val btnW = ((totalButtonW - buttonGap * 3f) / 4f).max(72f * s)
    val b0 = workX + 8f * s
    drawButton(b0, buttonY, btnW, bh, "Smelt One", accent = true)
    drawButton(b0 + (btnW + buttonGap), buttonY, btnW, bh, "Smelt All")
    drawButton(b0 + (btnW + buttonGap) * 2f, buttonY, btnW, bh, "Take")
    drawButton(b0 + (btnW + buttonGap) * 3f, buttonY, btnW, bh, "Close")

  private def handleFurnaceClick(mx: Float, my: Float): Unit =
    val cx = framebufferWidth / 2f; val cy = framebufferHeight / 2f; val s = uiScale
    val pw = (680f * s).min(framebufferWidth * 0.94f)
    val ph = (430f * s).min(framebufferHeight * 0.88f)
    val px = cx - pw / 2f; val py = cy - ph / 2f
    val listX = px + 28f * s
    val listY = py + 82f * s
    val listW = (210f * s).min(pw * 0.34f).max(160f * s)
    val rowH = (38f * s).max(30f)
    val furnaceItems = smeltableInputs.filter(b => smeltResult(b).nonEmpty && totalItemCount(b) > 0)
    for i <- 0 until inventoryGridSlots.min(6) do
      val y = listY + i * rowH
      if inRect(mx, my, listX, y, listW, rowH - 4f * s) then
        if i < furnaceItems.length then
          val b = furnaceItems(i)
          furnaceInput = b
          furnaceProgress = 0f
        else furnaceInput = Block.Air
        return

    val workX = listX + listW + 34f * s
    val workW = px + pw - 28f * s - workX
    val slot = (68f * s).min(74f).max(52f)
    val inputX = workX + 8f * s
    val inputY = py + 94f * s
    val outputX = workX + workW - slot - 8f * s
    val outputY = inputY
    if inRect(mx, my, inputX, inputY, slot, slot) then
      if heldInventoryBlock != Block.Air && smeltResult(heldInventoryBlock).nonEmpty && totalItemCount(heldInventoryBlock) > 0 then
        furnaceInput = heldInventoryBlock
        furnaceProgress = 0f
      else if heldInventoryBlock == Block.Air then
        furnaceInput = Block.Air
        furnaceProgress = 0f
      else addChatMessage("That item cannot be smelted")
      return
    if inRect(mx, my, outputX, outputY, slot, slot) then
      takeFurnaceOutput()
      return
    val bh = (36f * s).min(38f).max(30f)
    val buttonY = py + ph - 52f * s
    val buttonGap = 8f * s
    val totalButtonW = workW - 16f * s
    val btnW = ((totalButtonW - buttonGap * 3f) / 4f).max(72f * s)
    val b0 = workX + 8f * s
    if inRect(mx, my, b0, buttonY, btnW, bh) then smeltFurnace()
    else if inRect(mx, my, b0 + (btnW + buttonGap), buttonY, btnW, bh) then smeltAllFurnace()
    else if inRect(mx, my, b0 + (btnW + buttonGap) * 2f, buttonY, btnW, bh) then takeFurnaceOutput()
    else if inRect(mx, my, b0 + (btnW + buttonGap) * 3f, buttonY, btnW, bh) then enterGame()

  private def pickFurnaceInput(): Unit =
    val picked: Option[Block] = smeltableInputs.find(b => totalItemCount(b) > 0)
    picked match
      case Some(b: Block) => furnaceInput = b
      case None => addChatMessage("No smeltable items in inventory")

  private def drawSlot(x: Float, y: Float, size: Float, block: Block, count: Int): Unit =
    rect(x - 2, y - 2, size + 4, size + 4, 0.06f, 0.06f, 0.06f, 1f)
    rect(x, y, size, size, 0.30f, 0.30f, 0.30f, 1f)
    rect(x + 3, y + 3, size - 6, size - 6, 0.12f, 0.12f, 0.12f, 0.9f)
    if count > 0 then
      val icon = (size * 0.56f).min(24f * uiScale).max(16f)
      renderBlockIcon(block, x + (size - icon) / 2f, y + (size - icon) / 2f, icon)
      val ns = count.toString; val s = uiScale
      val countScale = (1.02f * s).max(0.84f).min(size / 20f)
      val tw = textWidth(ns, countScale); val nx = x + size - tw - 5f * s; val ny = y + size - 12f * countScale - 2f * s
      rect(nx - 2f * s, ny - 1f * s, tw + 4f * s, 11f * countScale + 3f * s, 0f, 0f, 0f, 0.64f)
      renderText(nx, ny, ns, 1f, 1f, 1f, countScale)

  private def drawPanel(x: Float, y: Float, w: Float, h: Float): Unit =
    BlockboxRender2D.drawPanel(x, y, w, h, uiScale)

  private def uiScale: Float =
    BlockboxRender2D.uiScale(framebufferWidth, framebufferHeight)

  private def dimBackground(): Unit = BlockboxRender2D.dimBackground(framebufferWidth, framebufferHeight)

  private def slider(x: Float, y: Float, w: Float, value: Float): Unit =
    BlockboxRender2D.slider(x, y, w, value, uiScale)

  private def resetGlArraysAndBuffers(): Unit =
    BlockboxRender2D.resetGlArraysAndBuffers()

  private def rect(x: Float, y: Float, w: Float, h: Float, r: Float, g: Float, b: Float, a: Float): Unit =
    BlockboxRender2D.rect(x, y, w, h, r, g, b, a)

  private def drawButton(x: Float, y: Float, w: Float, h: Float, label: String, accent: Boolean = false): Unit =
    val s = uiScale
    val (mx, my) = mouseFramebufferPos()
    val hover = inRect(mx, my, x, y, w, h)
    BlockboxRender2D.drawButton(x, y, w, h, label, accent, hover, s, framebufferWidth, framebufferHeight)

  private def renderTextShadow(x: Float, y: Float, text: String, r: Float, g: Float, b: Float, scale: Float = 1f): Unit =
    BlockboxRender2D.renderTextShadow(x, y, text, r, g, b, scale, framebufferWidth, framebufferHeight)

  private def setupOrtho(): Unit =
    BlockboxRender2D.setupOrtho(framebufferWidth, framebufferHeight)

  private def onOff(value: Boolean): String = if value then "ON" else "OFF"

  private def changeRenderDistance(delta: Int): Unit =
    val next = (renderDistance + delta).max(minRenderDistance).min(maxRenderDistance)
    if next != renderDistance then
      renderDistance = next
      // Keep existing chunk objects/meshes. Only stream in newly visible chunks and
      // unload chunks that fall outside the new radius. This avoids the old full-world
      // rebuild storm when tapping +/- or dragging the slider quickly.
      syncChunks()

  private def queueChunkMesh(chunk: Chunk): Unit =
    if !chunk.isDisposed && chunk.tryQueueMesh() then
      chunkBuildQueue.add(chunk)

  private def requestChunkCreate(cx: Int, cz: Int): Boolean =
    val key = (cx, cz)
    if chunks.contains(key) || pendingChunkCreates.contains(key) then false
    else if pendingChunkCreates.add(key) then
      chunkCreateQueue.add((cx, cz, chunkStreamGeneration))
      true
    else false

  private def chunkWanted(cx: Int, cz: Int, ccx: Int, ccz: Int): Boolean =
    val worldRadius = renderDistanceBlocks + Terrain.chunkSize
    val wx = cx * Terrain.chunkSize + Terrain.chunkSize / 2
    val wz = cz * Terrain.chunkSize + Terrain.chunkSize / 2
    val dxb = wx - ccx * Terrain.chunkSize
    val dzb = wz - ccz * Terrain.chunkSize
    dxb * dxb + dzb * dzb <= worldRadius * worldRadius

  private def processCreatedChunks(limit: Int): Unit =
    val ccx = chunkCoordPos(camera.x)
    val ccz = chunkCoordPos(camera.z)
    var processed = 0
    while processed < limit do
      val ready = chunkReadyQueue.poll()
      if ready == null then processed = limit
      else
        val (generation, chunk) = ready
        val key = (chunk.cx, chunk.cz)
        if generation != chunkStreamGeneration then chunk.dispose()
        else
          pendingChunkCreates.remove(key)
          if chunks.contains(key) || !chunkWanted(chunk.cx, chunk.cz, ccx, ccz) then chunk.dispose()
          else
            chunks(key) = chunk
            initChunkWaterLevels(chunk.cx, chunk.cz)
            applyNetworkBlocksToChunk(chunk.cx, chunk.cz)
            queueChunkMesh(chunk)
        processed += 1

  private var lastChunkX = 0; private var lastChunkZ = 0

  private def syncChunks(): Unit =
    val ccx = chunkCoordPos(camera.x)
    val ccz = chunkCoordPos(camera.z)
    // Keep streaming chunks every tick, not only when the player enters a new chunk.
    // The old logic loaded only a handful of chunks on the first pass, then stopped because
    // `chunks` was no longer empty. That caused temporary dark-blue empty worlds, missing
    // terrain, and players falling/desyncing while multiplayer screens were open.
    lastChunkX = ccx; lastChunkZ = ccz
    processCreatedChunks(8)
    loadChunks(ccx, ccz)

  private def terrainReadyNearCamera: Boolean =
    val ccx = chunkCoordPos(camera.x)
    val ccz = chunkCoordPos(camera.z)
    val centerReady = chunks.get((ccx, ccz)).exists(c => !c.isDisposed && c.hasMesh)
    if !centerReady then false
    else
      var readyCount = 0
      var dx = -1
      while dx <= 1 do
        var dz = -1
        while dz <= 1 do
          chunks.get((ccx + dx, ccz + dz)).foreach { c => if !c.isDisposed && c.hasMesh then readyCount += 1 }
          dz += 1
        dx += 1
      readyCount >= 5

  private def centerChunkReady: Boolean =
    val ccx = chunkCoordPos(camera.x)
    val ccz = chunkCoordPos(camera.z)
    chunks.get((ccx, ccz)).exists(c => !c.isDisposed && c.hasMesh)

  private def ensureNearbyMeshesReady(limit: Int): Unit =
    if limit <= 0 then return
    val ccx = chunkCoordPos(camera.x)
    val ccz = chunkCoordPos(camera.z)
    val nearest = chunks.values.toSeq.sortBy { c =>
      val dx = c.cx - ccx; val dz = c.cz - ccz
      dx * dx + dz * dz
    }
    var built = 0
    var i = 0
    while i < nearest.length && built < limit do
      val c = nearest(i)
      if !c.isDisposed && (!c.hasMesh || c.meshReady) then
        if c.meshReady then
          if c.uploadMesh() then built += 1 else queueChunkMesh(c)
        else
          queueChunkMesh(c)
          built += 1
      i += 1

  private def refreshChunkAndNeighbors(cx: Int, cz: Int): Unit =
    val keys = Array((cx, cz), (cx - 1, cz), (cx + 1, cz), (cx, cz - 1), (cx, cz + 1))
    keys.foreach { key =>
      chunks.get(key).foreach { c =>
        c.markDirtyMesh()
        c.meshReady = false
        queueChunkMesh(c)
      }
    }

  private def applyNetworkBlocksToChunk(cx: Int, cz: Int): Unit =
    val chunkOpt = chunks.get((cx, cz))
    if chunkOpt.isEmpty then return
    val chunk = chunkOpt.get
    var changed = false
    def applyOne(wx: Int, wy: Int, wz: Int, block: Block): Unit =
      val lx = wx - cx * Terrain.chunkSize
      val lz = wz - cz * Terrain.chunkSize
      if lx >= 0 && lx < Terrain.chunkSize && wy >= 0 && wy < Terrain.worldHeight && lz >= 0 && lz < Terrain.chunkSize then
        chunk.setBlock(lx, wy, lz, block)
        if block == Block.Water then
          val level = networkWaterLevelOverrides.get((wx, wy, wz)).map(b => (b.toInt & 0xFF).max(1).min(8)).getOrElse(8)
          chunk.setWaterLevel(lx, wy, lz, level)
          waterLevels((wx, wy, wz)) = level.toByte
          markWaterActive(wx, wy, wz)
        else
          chunk.setWaterLevel(lx, wy, lz, 0)
          waterLevels.remove((wx, wy, wz))
        changed = true

    val overrideKeys = networkBlockOverrides.keys.filter { case (wx, _, wz) => chunkCoordBlock(wx) == cx && chunkCoordBlock(wz) == cz }.toList
    overrideKeys.foreach { case (wx, wy, wz) =>
      networkBlockOverrides.get((wx, wy, wz)).foreach(block => applyOne(wx, wy, wz, block))
    }

    val pendingKeys = pendingNetworkBlocks.keys.filter { case (wx, _, wz) => chunkCoordBlock(wx) == cx && chunkCoordBlock(wz) == cz }.toList
    pendingKeys.foreach { case (wx, wy, wz) =>
      pendingNetworkBlocks.remove((wx, wy, wz)).foreach { block =>
        networkBlockOverrides((wx, wy, wz)) = block
        applyOne(wx, wy, wz, block)
      }
    }
    if changed then chunk.markDirtyMesh()

  private def loadChunks(ccx: Int, ccz: Int): Unit =
    val worldRadius = renderDistanceBlocks + Terrain.chunkSize
    val radiusSq = worldRadius * worldRadius
    val chunkRadius = (worldRadius / 16) + 2
    val maxPerFrame = if chunks.isEmpty then 6 else if renderDistance >= 14 then 2 else if renderDistance >= 10 then 3 else 4
    val wanted = collection.mutable.Set.empty[(Int, Int)]
    var loaded = 0
    for ring <- 0 to chunkRadius; dx <- -ring to ring; dz <- -ring to ring do
      if abs(dx) == ring || abs(dz) == ring then
        val wx = (ccx + dx) * 16 + 8; val wz = (ccz + dz) * 16 + 8
        val cx = ccx + dx; val cz = ccz + dz
        val dxb = wx - ccx * 16; val dzb = wz - ccz * 16
        if dxb * dxb + dzb * dzb <= radiusSq then
          wanted += ((cx, cz))
          if !chunks.contains((cx, cz)) && loaded < maxPerFrame then
            val loadedFromDisk = canUseLocalChunkSaves && loadChunkIfSaved(cx, cz)
            if loadedFromDisk then
              applyNetworkBlocksToChunk(cx, cz)
              chunks.get((cx, cz)).foreach(queueChunkMesh)
              loaded += 1
            else if requestChunkCreate(cx, cz) then loaded += 1
          else if chunks.contains((cx, cz)) then
            val chunk = chunks((cx, cz))
            if !chunk.hasMesh && !chunk.meshReady then
              queueChunkMesh(chunk)
    val toRemove = chunks.keys.filterNot(wanted.contains).toList
    val chunksDir = currentChunksDir
    if canUseLocalChunkSaves then chunksDir.mkdirs()
    toRemove.foreach { key =>
      chunks.get(key).foreach { chunk =>
        if canUseLocalChunkSaves then
          chunk.save(chunksDir)
          dirtyChunksForSave -= key
        chunk.dispose()
      }
      chunks -= key
    }

  private def startChunkGenThread(): Unit =
    // Workers do CPU-only chunk creation and mesh building. OpenGL uploads stay on
    // the main thread in processChunkWorkMainThread().
    if chunkGenPool != null then return
    chunkGenRunning = true
    val cores = Runtime.getRuntime.availableProcessors().max(1)
    val threads = (cores - 2).max(1).min(6)
    chunkGenPool = java.util.concurrent.Executors.newFixedThreadPool(threads)
    for i <- 0 until threads do
      chunkGenPool.submit(new Runnable:
        override def run(): Unit =
          var preferCreate = i % 2 == 0
          def processCreate(): Boolean =
            val createKey = chunkCreateQueue.poll()
            if createKey == null then false
            else
              try
                val chunk = Chunk(createKey._1, createKey._2, activeAtlas, terrainGen)
                if chunkGenRunning && createKey._3 == chunkStreamGeneration then chunkReadyQueue.add((createKey._3, chunk))
                else chunk.dispose()
              catch case e: Exception =>
                pendingChunkCreates.remove((createKey._1, createKey._2))
                e.printStackTrace()
              true
          def processMesh(): Boolean =
            val chunk = chunkBuildQueue.poll()
            if chunk == null then false
            else
              try
                if !chunk.isDisposed then
                  chunk.computeMesh()
                  if chunk.meshReady then chunkUploadQueue.add(chunk)
                  else chunk.releaseMeshQueue()
                else chunk.releaseMeshQueue()
              catch case e: Exception =>
                chunk.releaseMeshQueue()
                e.printStackTrace()
              true
          while chunkGenRunning do
            val didWork = if preferCreate then processCreate() || processMesh() else processMesh() || processCreate()
            preferCreate = !preferCreate
            if !didWork then try Thread.sleep(4) catch case _: InterruptedException => ()
      )

  private def stopChunkGenThread(): Unit =
    chunkGenRunning = false
    val pool = chunkGenPool
    if pool != null then
      pool.shutdown()
      try pool.awaitTermination(3, java.util.concurrent.TimeUnit.SECONDS) catch case _: Exception => ()
    chunkGenPool = null

  private def processChunkWorkMainThread(buildLimit: Int, uploadLimit: Int): Unit =
    processCreatedChunks(uploadLimit.max(1))
    // Upload any leftover completed meshes first. This also makes upgrading from an
    // older build safer if a stale queue exists during the first frame.
    var uploaded = 0
    while uploaded < uploadLimit do
      val ready = chunkUploadQueue.poll()
      if ready == null then uploaded = uploadLimit
      else
        val stillLoaded = chunks.get((ready.cx, ready.cz)).contains(ready)
        if stillLoaded && !ready.isDisposed then
          val fresh = ready.uploadMesh()
          if !fresh && !ready.isDisposed then queueChunkMesh(ready)
        else ready.releaseMeshQueue()
        uploaded += 1

    var built = 0
    while built < buildLimit do
      val chunk = chunkBuildQueue.poll()
      if chunk == null then built = buildLimit
      else
        val stillLoaded = chunks.get((chunk.cx, chunk.cz)).contains(chunk)
        if stillLoaded && !chunk.isDisposed then
          try
            chunk.computeMesh()
            if chunk.meshReady then
              val fresh = chunk.uploadMesh()
              if !fresh && !chunk.isDisposed then queueChunkMesh(chunk)
            else chunk.releaseMeshQueue()
          catch case e: Exception =>
            chunk.releaseMeshQueue()
            e.printStackTrace()
          built += 1
        else chunk.releaseMeshQueue()

  private def forceCameraChunkRing(radius: Int): Unit =
    val ccx = chunkCoordPos(camera.x)
    val ccz = chunkCoordPos(camera.z)
    var dx = -radius
    while dx <= radius do
      var dz = -radius
      while dz <= radius do
        val cx = ccx + dx
        val cz = ccz + dz
        val key = (cx, cz)
        chunks.get(key) match
          case Some(chunk) =>
            if !chunk.isDisposed && !chunk.hasMesh then queueChunkMesh(chunk)
          case None =>
            if canUseLocalChunkSaves && loadChunkIfSaved(cx, cz) then chunks.get(key).foreach(queueChunkMesh)
            else requestChunkCreate(cx, cz)
        dz += 1
      dx += 1

  private def viewDirection: Vec3 =
    BlockboxRaycast.viewDirection(yaw, pitch)

  private def raycast(maxDistance: Float = 8f): Option[RayHit] =
    BlockboxRaycast.raycast(camera, viewDirection, maxDistance, activeBlockAt)

  private def sendBlockNetwork(x: Int, y: Int, z: Int, blockId: Byte): Unit =
    if multiplayerMode then
      val block = Block.fromId(blockId)
      networkBlockOverrides((x, y, z)) = block
      if block == Block.Water then networkWaterLevelOverrides((x, y, z)) = waterLevelAt(x, y, z).max(1).min(8).toByte
      else networkWaterLevelOverrides.remove((x, y, z))
      val levelPart = if block == Block.Water then "|" + waterLevelAt(x, y, z).max(1).min(8).toString else ""
      val msg = s"BLOC|$x|$y|$z|$blockId$levelPart"
      if gameClient != null && gameClient.isConnected then gameClient.send(msg)
      else if gameServer != null then gameServer.broadcast(msg)

  private def breakTargetBlock(dropItem: Boolean = true): Unit =
    raycast().foreach { hit =>
      val (x, y, z) = hit.block
      val block = activeBlockAt(x, y, z)
      val event = modManager.fireBlockBreak(ModBlockBreakEvent(modManager.api, playerName, x, y, z, block, cancelled = false, dropItem = dropItem))
      if event.cancelled then return
      if event.dropItem && block != Block.Air && block != Block.Water then gainItem(block, 1)
      setActiveBlock(x, y, z, Block.Air)
      dirtyChunkAt(x, z)
      triggerSandFallAbove(x, y, z)
      playBreakSound()
      sendBlockNetwork(x, y, z, 0)
    }

  private def placeTargetBlock(): Unit =
    raycast().foreach { hit =>
      val (bx, by, bz) = hit.block
      val targetBlock = activeBlockAt(bx, by, bz)
      if targetBlock == Block.Furnace then
        openFurnace()
      else
        val (x, y, z) = hit.place
        val block = hotbarBlocks(selectedBlock.max(0).min(hotbarBlocks.length - 1))
        val hasBlock = block != Block.Air && (gameMode == GameMode.Creative || hotbarCounts(selectedBlock.max(0).min(hotbarCounts.length - 1)) > 0)
        if hasBlock && canPlaceBlockAt(x, y, z) then
          val event = modManager.fireBlockPlace(ModBlockPlaceEvent(modManager.api, playerName, x, y, z, block, cancelled = false))
          if event.cancelled then return
          val finalBlock = event.block
          setActiveBlock(x, y, z, finalBlock)
          if gameMode == GameMode.Survival then consumeSelectedHotbar(1)
          dirtyChunkAt(x, z)
          // Sand currently behaves like a normal block.
          playPlaceSound()
          sendBlockNetwork(x, y, z, finalBlock.ordinal.toByte)
    }

  private def blockIntersectsPlayerBody(x: Int, y: Int, z: Int, pos: Vec3): Boolean =
    val half = 0.36f
    val minX = floor(pos.x - half).toInt; val maxX = floor(pos.x + half).toInt
    val minY = floor(pos.y - 1.70f).toInt; val maxY = floor(pos.y + 0.22f).toInt
    val minZ = floor(pos.z - half).toInt; val maxZ = floor(pos.z + half).toInt
    x >= minX && x <= maxX && y >= minY && y <= maxY && z >= minZ && z <= maxZ

  private def placementBlockedByAnyPlayer(x: Int, y: Int, z: Int): Boolean =
    blockIntersectsPlayerBody(x, y, z, camera) ||
      remotePlayers.values.exists(rp => glfwGetTime() - rp.lastSeen <= 8.0 && blockIntersectsPlayerBody(x, y, z, rp.pos))

  private def canPlaceBlockAt(x: Int, y: Int, z: Int): Boolean =
    !activeBlockAt(x, y, z).solid && !placementBlockedByAnyPlayer(x, y, z)

  private def renderRayLine(): Unit =
    val dir = viewDirection; val end = camera + dir * 8f
    glDisable(GL_DEPTH_TEST); glLineWidth(2f); glColor4f(0f, 1f, 0f, 0.6f)
    glBegin(GL_LINES); glVertex3f(0f, camera.y, 0f); glVertex3f(end.x - camera.x, end.y, end.z - camera.z); glEnd()
    glLineWidth(1f); glEnable(GL_DEPTH_TEST)

  private def renderChunkBorders(): Unit =
    glDisable(GL_DEPTH_TEST); glLineWidth(1f); glColor4f(1f, 0.3f, 0.3f, 0.30f)
    glBegin(GL_LINES)
    chunks.keys.foreach { case (cx, cz) =>
      val x = cx * 16; val z = cz * 16
      val rx = x.toFloat - camera.x; val rz = z.toFloat - camera.z
      glVertex3f(rx, 0f, rz); glVertex3f(rx + 16f, 0f, rz)
      glVertex3f(rx + 16f, 0f, rz); glVertex3f(rx + 16f, 0f, rz + 16f)
      glVertex3f(rx + 16f, 0f, rz + 16f); glVertex3f(rx, 0f, rz + 16f)
      glVertex3f(rx, 0f, rz + 16f); glVertex3f(rx, 0f, rz)
    }
    glEnd(); glEnable(GL_DEPTH_TEST); glLineWidth(1f)

  private def renderTargetOutline(): Unit =
    raycast(8f).foreach { hit =>
      val (x, y, z) = hit.block
      glDisable(GL_DEPTH_TEST)
      val e = 0.005f; val x0 = x - camera.x - e; val x1 = x - camera.x + 1f + e
      val y0 = y - e; val y1 = y + 1f + e; val z0 = z - camera.z - e; val z1 = z - camera.z + 1f + e
      glLineWidth(3f); glBegin(GL_LINES)
      glColor4f(0f, 0f, 0f, 0.5f)
      lineEdges(x0, y0, z0, x1, y1, z1)
      glColor4f(1f, 1f, 1f, 0.8f); glLineWidth(2f)
      lineEdges(x0, y0, z0, x1, y1, z1)
      glEnd(); glLineWidth(1f); glEnable(GL_DEPTH_TEST)
    }

  private def lineEdges(x0: Float, y0: Float, z0: Float, x1: Float, y1: Float, z1: Float): Unit =
    glVertex3f(x0, y0, z0); glVertex3f(x1, y0, z0)
    glVertex3f(x1, y0, z0); glVertex3f(x1, y1, z0)
    glVertex3f(x1, y1, z0); glVertex3f(x0, y1, z0)
    glVertex3f(x0, y1, z0); glVertex3f(x0, y0, z0)
    glVertex3f(x0, y0, z1); glVertex3f(x1, y0, z1)
    glVertex3f(x1, y0, z1); glVertex3f(x1, y1, z1)
    glVertex3f(x1, y1, z1); glVertex3f(x0, y1, z1)
    glVertex3f(x0, y1, z1); glVertex3f(x0, y0, z1)
    glVertex3f(x0, y0, z0); glVertex3f(x0, y0, z1)
    glVertex3f(x1, y0, z0); glVertex3f(x1, y0, z1)
    glVertex3f(x1, y1, z0); glVertex3f(x1, y1, z1)
    glVertex3f(x0, y1, z0); glVertex3f(x0, y1, z1)

  private def initChunkWaterLevels(cx: Int, cz: Int): Unit =
    // Do not register generated oceans/lakes into the dynamic water simulator.
    // Natural water is static terrain. Only player-placed/flowing water enters waterLevels.
    chunks.get((cx, cz)).foreach { chunk =>
      chunk.foreachDynamicWaterCell { (lx, y, lz, level) =>
        val wx = cx * Terrain.chunkSize + lx
        val wz = cz * Terrain.chunkSize + lz
        waterLevels((wx, y, wz)) = level.toByte
        markWaterActive(wx, y, wz)
      }
    }

  private def saveChunk(cx: Int, cz: Int): Unit =
    if !canUseLocalChunkSaves then return
    val chunk = chunks.get((cx, cz))
    chunk.foreach { c =>
      c.save(currentChunksDir)
      dirtyChunksForSave -= ((cx, cz))
    }

  private def loadChunkIfSaved(cx: Int, cz: Int): Boolean =
    if !canUseLocalChunkSaves then return false
    val chunksDir = currentChunksDir
    val file = new java.io.File(chunksDir, s"chunk_${cx}_${cz}.dat")
    if file.exists() then
      try
        val emptyBlocks = new Array[Byte](Terrain.chunkSize * Terrain.worldHeight * Terrain.chunkSize)
        val chunk = Chunk(cx, cz, activeAtlas, terrainGen, emptyBlocks)
        chunk.load(chunksDir)
        chunks((cx, cz)) = chunk
        initChunkWaterLevels(cx, cz)
        applyNetworkBlocksToChunk(cx, cz)
        true
      catch
        case e: Exception =>
          System.err.println(s"Corrupt chunk ignored $cx,$cz: $e")
          try file.renameTo(new java.io.File(chunksDir, s"chunk_${cx}_${cz}.dat.bad")) catch case _: Exception => ()
          chunks -= ((cx, cz))
          false
    else
      false

  private def dirtyChunkAt(x: Int, z: Int): Unit =
    val cx = chunkCoordBlock(x)
    val cz = chunkCoordBlock(z)
    dirtyChunks += ((cx, cz))
    dirtyChunksForSave += ((cx, cz))
    val keys = collection.mutable.Set((cx, cz))
    val lx = x - cx * 16; val lz = z - cz * 16
    if lx == 0 then keys += ((cx - 1, cz))
    else if lx == 15 then keys += ((cx + 1, cz))
    if lz == 0 then keys += ((cx, cz - 1))
    else if lz == 15 then keys += ((cx, cz + 1))
    keys.foreach { key =>
      chunks.get(key).foreach { c =>
        c.markDirtyMesh()
        c.meshReady = false
        queueChunkMesh(c)
      }
    }

  private def dirtyQueued(wx: Int, wz: Int): Unit =
    val cx = chunkCoordBlock(wx)
    val cz = chunkCoordBlock(wz)
    dirtyChunks += ((cx, cz))
    dirtyChunksForSave += ((cx, cz))

  private def flushDirtyChunks(): Unit =
    if dirtyChunks.nonEmpty then
      val allKeys = dirtyChunks.toSet
      dirtyChunks.clear()
      // Performance fix: do not save every dirty/water-touched chunk every frame.
      // Disk saves happen on chunk unload/world quit. Here we only queue mesh rebuilds.
      // Also avoid expanding every dirty chunk to all four neighbors; direct block edits
      // already call dirtyChunkAt(), which handles edge neighbors precisely.
      for key <- allKeys do
        chunks.get(key).foreach { c =>
          c.markDirtyMesh()
          c.meshReady = false
          queueChunkMesh(c)
        }

  private def floatBuffer(values: Float*): FloatBuffer =
    val buffer = BufferUtils.createFloatBuffer(values.length)
    values.foreach(buffer.put); buffer.flip(); buffer

  private def updateFramebufferSize(): Unit =
    val w = BufferUtils.createIntBuffer(1); val h = BufferUtils.createIntBuffer(1)
    glfwGetFramebufferSize(window, w, h)
    framebufferWidth = w.get(0).max(1); framebufferHeight = h.get(0).max(1)


  def modGetBlock(x: Int, y: Int, z: Int): Block = activeBlockAt(x, y, z)
  def modSetBlock(x: Int, y: Int, z: Int, block: Block): Unit =
    val safe = if block == null then Block.Air else block
    setActiveBlock(x, y, z, safe)
    dirtyChunkAt(x, z)
    sendBlockNetwork(x, y, z, safe.ordinal.toByte)
  def modResolveBlock(name: String): Block = modManager.resolveBlockName(name)
  def modAddChatMessage(message: String): Unit = addChatMessage(message)
  def modGiveItem(block: Block, count: Int): Unit = if block != null then gainItem(block, count.max(0))
  def modLocalPlayerName: String = playerName
  def modCamera: Vec3 = camera
  def modTeleportSelf(pos: Vec3): Unit =
    if pos != null then
      camera = pos
      velocity = Vec3(0f, 0f, 0f)
      onGround = false
  def modWorldSeed: Long = worldSeed
  def modWorldName: String = worldName
  def modHealth: Float = playerHealth
  def modSetHealth(value: Float): Unit = playerHealth = value.max(0f).min(20f)
  def modFood: Float = playerFood
  def modSetFood(value: Float): Unit = playerFood = value.max(0f).min(20f)
  def modCanUseCheats: Boolean = canUseCheatAuthority && worldCheatsEnabled
  def modIsOpped(name: String): Boolean = isOppedName(name)
  def modFramebufferWidth: Float = framebufferWidth.toFloat
  def modFramebufferHeight: Float = framebufferHeight.toFloat
  def modUiScale: Float = uiScale
  def modScreenName: String = screen.toString.toLowerCase
  def modMouseX: Float = modLastMouseX
  def modMouseY: Float = modLastMouseY
  def modLeftDown: Boolean = modLeftDownNow
  def modLeftClicked: Boolean = modLeftClickedThisFrame
  def modCursorMode: Boolean = modUiCursorMode
  def modSetCursorMode(enabled: Boolean): Unit =
    modUiCursorMode = enabled
    modGuiLeftWasDown = false
    modLeftClickedThisFrame = false
    if window != 0 && screen == Screen.Playing then
      glfwSetInputMode(window, GLFW_CURSOR, if enabled then GLFW_CURSOR_NORMAL else GLFW_CURSOR_DISABLED)
      firstMouse = true
  def modDrawRect(x: Float, y: Float, w: Float, h: Float, r: Float, g: Float, b: Float, a: Float): Unit =
    glDisable(GL_DEPTH_TEST); glDisable(GL_CULL_FACE); setupOrtho(); rect(x, y, w, h, r, g, b, a)
  def modDrawText(x: Float, y: Float, text: String, r: Float, g: Float, b: Float, scale: Float): Unit =
    glDisable(GL_DEPTH_TEST); glDisable(GL_CULL_FACE); setupOrtho(); renderText(x, y, Option(text).getOrElse(""), r, g, b, scale.max(0.1f))
  def modDrawTextShadow(x: Float, y: Float, text: String, r: Float, g: Float, b: Float, scale: Float): Unit =
    glDisable(GL_DEPTH_TEST); glDisable(GL_CULL_FACE); setupOrtho(); renderTextShadow(x, y, Option(text).getOrElse(""), r, g, b, scale.max(0.1f))
  def modDrawButton(x: Float, y: Float, w: Float, h: Float, label: String): Unit =
    glDisable(GL_DEPTH_TEST); glDisable(GL_CULL_FACE); setupOrtho(); drawButton(x, y, w, h, Option(label).getOrElse(""))
  def modInRect(mx: Float, my: Float, x: Float, y: Float, w: Float, h: Float): Boolean = inRect(mx, my, x, y, w, h)


  private def activeWorld: Nothing = throw IllegalStateException("World no longer used - use activeBlockAt")

  private def activeAtlas: TextureAtlas =
    val atlas = textureAtlas
    if atlas == null then throw IllegalStateException("Texture atlas is not initialized")
    atlas
