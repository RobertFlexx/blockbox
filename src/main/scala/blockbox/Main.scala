//> using scala "3.8.3"
//> using dep "org.lwjgl:lwjgl:3.4.1"
//> using dep "org.lwjgl:lwjgl-glfw:3.4.1"
//> using dep "org.lwjgl:lwjgl-opengl:3.4.1"
//> using dep "org.lwjgl:lwjgl-stb:3.4.1"
//> using dep "org.apache.groovy:groovy:5.0.6"

package blockbox

import blockbox.net.BlockboxNet
import blockbox.net.BlockboxSockets
import blockbox.io.BlockboxFiles
import blockbox.io.BlockboxSaveFiles
import blockbox.render.ViewFrustum
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
import org.lwjgl.opengl.GL13.*
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
import java.util.concurrent.{ExecutorService, Executors, Future, LinkedBlockingQueue, TimeUnit}
import java.util.concurrent.atomic.{AtomicInteger, AtomicLong}
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
  @volatile protected var camera = Vec3(96f, 48f, 130f)
  protected var velocity = Vec3(0f, 0f, 0f)
  protected var onGround = false
  @volatile private var yaw = 0f
  @volatile private var pitch = 0f
  // Presentation-only first-person response; physics/network camera stays deterministic.
  private var viewFovOffset = 0f
  private var cameraBobPhase = 0f
  private var cameraBobAmount = 0f
  private var cameraRoll = 0f
  private var smoothedHorizontalSpeed = 0f
  private var footstepTravel = 0f
  private val supportProbeOffsets: Array[(Float, Float)] =
    Array((0f, 0f), (-0.22f, -0.22f), (0.22f, -0.22f), (-0.22f, 0.22f), (0.22f, 0.22f))
  private var wasSubmergedForAudio = false
  private var hurtOverlay = 0f
  private var selectedItemLabelTimer = 0f
  private var viewmodelKick = 0f
  private var viewmodelObstruction = 0f
  // The item in hand lowers out of view and the new one rises when the slot changes.
  private var equippedBlock: Block = Block.Air
  private var equipProgress = 1f
  // The hand trails fast camera turns slightly, then settles.
  private var viewmodelYaw = 0f
  private var viewmodelPitch = 0f
  private val firstPersonPass = FirstPersonPass()
  private val pendingPlayerEdits=scala.collection.mutable.HashMap.empty[(Int,Int,Int),(Block,Int,Boolean)]
  // Weather is presentation/game-state, not a post-process fake. The shader bridge
  // receives the same rain/wetness values used by the world renderer so packs can
  // darken porous surfaces, soften sunlight and add their own storm treatment.
  private var weatherEnabled = true
  private var rainStrength = 0f
  private var wetness = 0f
  private var weatherWind = 0f
  private var stormCloudCover = 0f
  private var lightningFlash = 0f
  private var localSnowfall=false
  private var localRain=0f
  private var localCloud=0f
  private var localWetness=0f
  private var localLightning=0f
  private var weatherCells=Vector.empty[WeatherField.Cell]
  private var weatherStrikes=Vector.empty[WeatherField.Strike]
  private val heardThunder=scala.collection.mutable.HashSet.empty[Long]
  private var shelterTimer=0f
  private var rainShelter=1f
  private var lastMouseX = width / 2.0
  private var lastMouseY = height / 2.0
  private var firstMouse = true
  private var leftWasDown = false
  private var rightWasDown = false
  private var menuLeftWasDown = false
  private var menuRightWasDown = false
  private var modGuiLeftWasDown = false
  private var modUiCursorMode = false
  private var modLastMouseX = 0f
  private var modLastMouseY = 0f
  private var modLeftDownNow = false
  private var modLeftClickedThisFrame = false
  private var breakingBlock: (Int, Int, Int) | Null = null
  private var breakingProgress = 0f
  protected val placeableBlocks = Array(Block.Grass, Block.Dirt, Block.Stone, Block.Sand, Block.Cactus, Block.Wood, Block.Planks, Block.BirchPlanks, Block.PinePlanks, Block.AcaciaPlanks, Block.Leaves, Block.BirchWood, Block.BirchLeaves, Block.PineWood, Block.PineLeaves, Block.AcaciaWood, Block.AcaciaLeaves, Block.Brick, Block.Glass, Block.Snow, Block.Clay, Block.Coal, Block.Copper, Block.IronOre, Block.GoldOre, Block.Diamond, Block.RainbowBlock, Block.Furnace, Block.Torch)
  protected val inventory = Array.fill(Block.values.length)(0)
  protected val backpackBlocks: Array[Block] = Array.fill(27)(Block.Air)
  protected val backpackCounts: Array[Int] = Array.fill(27)(0)
  protected val hotbarBlocks: Array[Block] = Array.fill(9)(Block.Air)
  protected val hotbarCounts: Array[Int] = Array.fill(9)(0)
  protected val maxStackSize = 64
  protected var selectedBlock = 0
  protected var heldInventoryBlock: Block = Block.Air
  protected var heldInventoryCount = 0
  protected var heldFromHotbar = false
  private var catalogScroll = 0
  private var craftingScroll = 0
  protected val craftGridBlocks: Array[Block] = Array.fill(9)(Block.Air)
  protected val craftGridCounts: Array[Int] = Array.fill(9)(0)
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
  // Minecraft-style log orientation (LogAxis id) for network block changes. Kept in
  // sync with the override maps above, so remote log edits keep their facing even
  // after a chunk streams out and returns.
  private val pendingNetworkLogAxes = scala.collection.mutable.HashMap.empty[(Int, Int, Int), Int]
  private val networkLogAxesOverrides = scala.collection.mutable.HashMap.empty[(Int, Int, Int), Int]
  private val networkTorchMounts = scala.collection.mutable.HashMap.empty[(Int, Int, Int), Int]
  private val worldExtraMagic = 0x42425831 // "BBX1": optional tail after old world.dat fields
  private val worldExtraVersion = 3
  protected val knownPlayerNames = scala.collection.mutable.HashSet.empty[String]
  private val playerColors = scala.collection.mutable.HashMap.empty[String, Int]
  private val oppedPlayerNames = scala.collection.mutable.HashSet.empty[String]
  private var localColorId = 0
  private val playerColorPalette = PlayerAppearance.shirts.toArray
  private var hostNameFocused = true
  private var joinNameFocused = true
  private var joinStatusMessage = "Enter host IP. You can use IP:port; default port is 25565."
  private var joinStatusError = false
  @volatile private var pendingJoin: GameClient = null
  @volatile private var joinResult: (GameClient, Boolean) | Null = null
  private var applyingJoin = false
  private var hostStatusMessage = "Hosts on 0.0.0.0:25565. Share your LAN/ZeroTier IP."
  private var hostStatusError = false
  private var serverHost = "localhost"
  private var serverPort = 25565
  protected var multiplayerMode = false
  private var joinIpInput = ""
  private var lastPosSend = 0.0
  private var lastPosKeepalive = 0.0
  private var lastNetworkSentPos = camera
  private var lastNetworkSentYaw = yaw
  private var lastNetworkSentPitch = pitch
  private var lastMultiplayerHeartbeat = 0.0
  private var renderDistance = 6 // chunks
  private val minRenderDistance = 2
  private val maxRenderDistance = 18
  private def renderDistanceBlocks: Int = renderDistance * Terrain.chunkSize
  private var cloudRenderDistance = 48 // chunks
  private val minCloudRenderDistance = 24
  private val maxCloudRenderDistance = 96
  private def cloudRenderDistanceBlocks: Int = cloudRenderDistance * Terrain.chunkSize
  protected var worldSeed = 0L
  private var createWorldMode = GameMode.Survival
  private var createWorldCheats = false
  protected var worldCheatsEnabled = false
  protected var terrainGen = TerrainGenerator(0L)
  private var chunks = scala.collection.mutable.AnyRefMap.empty[(Int, Int), Chunk]
  // Reused render scratch buffers. Avoid allocating/sorting fresh Scala collections
  // every frame when render distance is high.
  private val renderChunkScratch = ArrayBuffer.empty[Chunk]
  private val viewFrustum = ViewFrustum()
  private val cullProjection = new Array[Float](16)
  private val cullView = new Array[Float](16)
  private val cullMatrixBuffer = BufferUtils.createFloatBuffer(16)
  // Shadow casters use a distance-only set instead of the camera frustum. A tree
  // just outside the player's view can still cast a visible shadow into the view.
  private val shadowChunkScratch = ArrayBuffer.empty[Chunk]
  private val translucentChunkScratch = ArrayBuffer.empty[Chunk]
  private val waterChunkScratch = ArrayBuffer.empty[Chunk]
  private val cursorXBuffer = BufferUtils.createDoubleBuffer(1)
  private val cursorYBuffer = BufferUtils.createDoubleBuffer(1)
  private val windowWidthBuffer = BufferUtils.createIntBuffer(1)
  private val windowHeightBuffer = BufferUtils.createIntBuffer(1)
  private val framebufferWidthBuffer = BufferUtils.createIntBuffer(1)
  private val framebufferHeightBuffer = BufferUtils.createIntBuffer(1)
  private val fogColorBuffer = BufferUtils.createFloatBuffer(4)
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
  private var chatHistoryDraft = ""
  protected var commandSuggestionIndex = 0
  protected val modManager = ModManager(this)
  private var modsScreenScroll = 0
  private val sandFallQueue = Queue.empty[(Int, Int, Int)]
  private val maxSandUpdatesPerFrame = 6
  private var grassGrowthTimer = 0f
  private val grassGrowthInterval = 1.0f
  private val maxGrassGrowthAttemptsPerTick = 48
  private val grassGrowthRng = Random(0xB10C80L)
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
  private var anisotropicFiltering = true
  private var worldDarkening = true
  private var antiAliasing = true
  private var smoothLighting = true
  private var shadersEnabled = true
  private var shaderPipeline: BlockboxShaderPipeline = null
  // Optional world resolution scaling. The HUD remains native, and a bounded
  // sharpening resolve preserves straight edges and accurate block picking.
  private var foveatedRendering = false
  private var foveaPass: BlockboxFovea = null
  private var cloudsEnabled = true
  private var guiScaleSetting = 0
  private var msaaSamples = 0
  private var windowedX = 0; private var windowedY = 0
  private var windowedW = 1280; private var windowedH = 720
  private var settingsDirty = false
  private var lastSettingsSave = 0.0
  private val fallingSandParticles = ArrayBuffer.empty[(Float, Float, Float, Float)]
  private final case class BlockParticle(
      block: Block, var pos: Vec3, var velocity: Vec3, var life: Float, maxLife: Float,
      size: Float, face: FaceKind, u0: Float, v0: Float, u1: Float, v1: Float,
      var rotation: Float, spin: Float, gravity: Float, drag: Float, bounce: Float, brightness: Float,
      tint: (Float, Float, Float) = (1f, 1f, 1f)
  )
  private val blockParticles = ArrayBuffer.empty[BlockParticle]
  private val blockParticleRng = Random(0xB10CB0A7L)
  private val maxBlockParticles = 360
  private val sandParticleLifetime = 0.25f
  private var sandParticleTimer = 0f
  protected var timeOverride: Option[Float] = None
  private var worldClock = 150.0
  private var environmentReceived = false
  private var lastEnvironmentSend = -10.0
  @volatile private var publishedRules=""
  @volatile private var publishedEnvironment = "ENV|150|-1|0|0|0|0|0"
  @volatile private var publishedOps = Vector.empty[String]
  private var lastSwingSend = -10.0
  private var fieldJournal = FieldJournal()
  private var journalToast = ""
  private var journalToastTime = 0f
  private var journalTimer = 0f
  protected var flyEnabled = false
  private var lastSpacePressTime = 0.0
  private val doubleTapInterval = 0.3
  private var wasSpaceDown = false
  private val bubbleParticles = ArrayBuffer.empty[(Float, Float, Float, Float)]
  private val bubbleRng = Random(0xBABB1E5L)
  private var bubbleSpawnCooldown = 0f
  private var sliderActive: String | Null = null
  private var settingsScroll = 0f
  private var settingsTab = 0
  private val dirtyChunks = scala.collection.mutable.Set.empty[(Int, Int)]
  private val dirtyChunksForSave = scala.collection.mutable.Set.empty[(Int, Int)]
  private val terrainHeightCache = scala.collection.mutable.HashMap.empty[Long, Int]
  private var starPositions: Array[(Float, Float, Float, Float)] = null
  // Threaded chunk generation. CPU work is distance/view-prioritized instead of FIFO:
  // a newly-needed camera chunk should never sit behind dozens of far-away meshes.
  private final case class ChunkMeshJob(chunk: Chunk, priority: Int, serial: Long)
  private final case class ChunkCreateJob(cx: Int, cz: Int, generation: Int, priority: Int, serial: Long, savedChunksDir: Option[java.io.File])
  private val chunkJobSerial = AtomicLong(0L)
  private val chunkMeshComparator = new java.util.Comparator[ChunkMeshJob]:
    override def compare(a: ChunkMeshJob, b: ChunkMeshJob): Int =
      val p = java.lang.Integer.compare(a.priority, b.priority)
      if p != 0 then p else java.lang.Long.compare(a.serial, b.serial)
  private val chunkCreateComparator = new java.util.Comparator[ChunkCreateJob]:
    override def compare(a: ChunkCreateJob, b: ChunkCreateJob): Int =
      val p = java.lang.Integer.compare(a.priority, b.priority)
      if p != 0 then p else java.lang.Long.compare(a.serial, b.serial)
  private val chunkBuildQueue = new java.util.concurrent.PriorityBlockingQueue[ChunkMeshJob](64, chunkMeshComparator)
  private val chunkUploadQueue = new java.util.concurrent.ConcurrentLinkedQueue[Chunk]()
  private val chunkCreateQueue = new java.util.concurrent.PriorityBlockingQueue[ChunkCreateJob](64, chunkCreateComparator)
  private val chunkReadyQueue = new java.util.concurrent.ConcurrentLinkedQueue[(Int, Chunk)]()
  private val pendingChunkCreates = java.util.concurrent.ConcurrentHashMap.newKeySet[(Int, Int)]()
  @volatile private var chunkGenRunning = false
  @volatile private var chunkStreamGeneration = 0
  private var chunkGenPool: java.util.concurrent.ExecutorService = null

  // World saving is split into a tiny game-thread snapshot and ordered background
  // disk I/O. This keeps Save and Quit responsive even on filesystems where each
  // atomic rename/directory fsync can stall for milliseconds.
  private var worldSaveExecutor: ExecutorService = null
  @volatile private var latestSaveFuture: Future[?] = null
  private val pendingSaveJobs = AtomicInteger(0)
  private val saveJobSerial = AtomicLong(0L)
  private var activeWorldSession = false

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
    terrainGen = createTerrainGenerator(worldSeed)
    terrainHeightCache.clear()
    worldClock = 150.0; timeOverride = None
    rainStrength = 0f; wetness = 0f; stormCloudCover = 0f; lightningFlash = 0f
    localRain=0f; localCloud=0f; localWetness=0f; localLightning=0f
    weatherCells=Vector.empty; weatherStrikes=Vector.empty; heardThunder.clear(); pendingPlayerEdits.clear()
    updateRainAmbience(0f)
    fieldJournal = FieldJournal(); journalToastTime = 0f
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

  private def createTerrainGenerator(seed: Long): TerrainGenerator =
    TerrainGenerator(seed,(cx,cz,blocks) => modManager.worldgen.decorate(seed,cx,cz,blocks))

  private def gameRootDir: java.io.File =
    sys.env.get("BLOCKBOX_PROJECT_ROOT").map(java.io.File(_)).filter(_.isDirectory)
      .getOrElse(java.io.File(".").getAbsoluteFile)

  private def settingsFile: java.io.File = java.io.File(gameRootDir, "blockbox-settings.properties")

  private def loadSettingsProperties(): java.util.Properties =
    val props = java.util.Properties()
    val file = settingsFile
    if file.isFile then
      val in = java.io.FileInputStream(file)
      try props.load(in) finally in.close()
    props

  private def saveSettingsProperties(props: java.util.Properties): Unit =
    BlockboxFiles.ensureDirectory(settingsFile.getAbsoluteFile.getParentFile.toPath)
    val out = java.io.ByteArrayOutputStream()
    props.store(out, "Blockbox settings")
    BlockboxFiles.writeAtomic(settingsFile.toPath, stream => stream.write(out.toByteArray))

  private def propBool(props: java.util.Properties, key: String, fallback: Boolean): Boolean =
    Option(props.getProperty(key)).map(_.trim.toLowerCase) match
      case Some("true" | "1" | "yes" | "on") => true
      case Some("false" | "0" | "no" | "off") => false
      case _ => fallback

  private def propInt(props: java.util.Properties, key: String, fallback: Int, min: Int, max: Int): Int =
    Option(props.getProperty(key)).flatMap(s => scala.util.Try(s.trim.toInt).toOption).getOrElse(fallback).max(min).min(max)

  private def propFloat(props: java.util.Properties, key: String, fallback: Float, min: Float, max: Float): Float =
    Option(props.getProperty(key)).flatMap(s => scala.util.Try(s.trim.toFloat).toOption).getOrElse(fallback).max(min).min(max)

  private def loadSelectedTexturePackId(): String =
    try Option(loadSettingsProperties().getProperty("texturePack")).map(_.trim).filter(_.nonEmpty).getOrElse("default")
    catch case _: Exception => "default"

  private def loadUserSettings(): Unit =
    try
      val props = loadSettingsProperties()
      selectedTexturePackId = Option(props.getProperty("texturePack")).map(_.trim).filter(_.nonEmpty).getOrElse(selectedTexturePackId)
      renderDistance = propInt(props, "renderDistance", renderDistance, minRenderDistance, maxRenderDistance)
      cloudRenderDistance = propInt(props, "cloudRenderDistance", cloudRenderDistance, minCloudRenderDistance, maxCloudRenderDistance)
      fogDensity = propFloat(props, "fogDensity", fogDensity, 0.6f, 3.0f)
      fov = propFloat(props, "fov", fov, 50f, 100f)
      fastMove = propBool(props, "fastMove", fastMove)
      vsync = propBool(props, "vsync", vsync)
      soundEnabled = propBool(props, "soundEnabled", soundEnabled)
      fullscreen = propBool(props, "fullscreen", fullscreen)
      anisotropicFiltering = propBool(props, "anisotropicFiltering", anisotropicFiltering)
      worldDarkening = propBool(props, "worldDarkening", worldDarkening)
      antiAliasing = propBool(props, "antiAliasing", antiAliasing)
      smoothLighting = propBool(props, "smoothLighting", smoothLighting)
      shadersEnabled = propBool(props, "shadersEnabled", shadersEnabled)
      foveatedRendering = propBool(props, "worldResolution85", false)
      cloudsEnabled = propBool(props, "cloudsEnabled", cloudsEnabled)
      weatherEnabled = propBool(props, "weatherEnabled", weatherEnabled)
      guiScaleSetting = propInt(props, "guiScale", guiScaleSetting, 0, 5)
      pauseEscReturnsToGame = propBool(props, "pauseEscReturnsToGame", pauseEscReturnsToGame)
    catch case e: Exception => System.err.println(s"Settings load failed: $e")

  private def saveUserSettings(): Unit =
    try
      val props = loadSettingsProperties()
      props.setProperty("texturePack", selectedTexturePackId)
      props.setProperty("renderDistance", renderDistance.toString)
      props.setProperty("cloudRenderDistance", cloudRenderDistance.toString)
      props.setProperty("fogDensity", f"$fogDensity%.3f")
      props.setProperty("fov", f"$fov%.3f")
      props.setProperty("fastMove", fastMove.toString)
      props.setProperty("vsync", vsync.toString)
      props.setProperty("soundEnabled", soundEnabled.toString)
      props.setProperty("fullscreen", fullscreen.toString)
      props.setProperty("anisotropicFiltering", anisotropicFiltering.toString)
      props.setProperty("worldDarkening", worldDarkening.toString)
      props.setProperty("antiAliasing", antiAliasing.toString)
      props.setProperty("smoothLighting", smoothLighting.toString)
      props.setProperty("shadersEnabled", shadersEnabled.toString)
      props.remove("foveatedRendering")
      props.setProperty("worldResolution85", foveatedRendering.toString)
      props.setProperty("cloudsEnabled", cloudsEnabled.toString)
      props.setProperty("weatherEnabled", weatherEnabled.toString)
      props.setProperty("guiScale", guiScaleSetting.toString)
      props.setProperty("pauseEscReturnsToGame", pauseEscReturnsToGame.toString)
      saveSettingsProperties(props)
      settingsDirty = false
      lastSettingsSave = glfwGetTime()
    catch case e: Exception => System.err.println(s"Settings save failed: $e")

  private def markSettingsDirty(): Unit =
    settingsDirty = true

  private def flushUserSettings(force: Boolean = false): Unit =
    if settingsDirty && (force || glfwGetTime() - lastSettingsSave > 0.75) then saveUserSettings()

  private def applyAntiAliasingState(): Unit =
    glDisable(GL_MULTISAMPLE)

  private def applyWorldAntiAliasingState(): Unit =
    if antiAliasing && msaaSamples > 0 then glEnable(GL_MULTISAMPLE)
    else glDisable(GL_MULTISAMPLE)

  private def antiAliasingLabel: String =
    if antiAliasing && msaaSamples > 0 then s"ON (${msaaSamples}x)"
    else if antiAliasing then "ON after restart"
    else "OFF"

  private def guiScaleLabel: String =
    guiScaleSetting match
      case 0 => "Default"
      case 1 => "1x"
      case 2 => "1.5x"
      case 3 => "2x"
      case 4 => "3x"
      case _ => "4x"

  private def cycleGuiScale(): Unit =
    guiScaleSetting = (guiScaleSetting + 1) % 6

  private def settingsCanHostMode: Boolean =
    settingsReturnTo == Screen.Playing && gameServer != null && worldCheatsEnabled

  private final case class SettingEntry(id: String, label: String, action: () => Unit = () => ()):
    def height: Float = (if id == "rd" || id == "fog" || id == "fov" || id == "clouds" then 56f else 34f) * uiScale

  private def settingsEntries: Vector[SettingEntry] =
    def toggle(id: String, label: String)(action: => Unit): SettingEntry = SettingEntry(id,label,() => { action; markSettingsDirty() })
    settingsTab match
      case 0 => Vector(
        SettingEntry("rd",s"View distance   $renderDistance chunks"),
        SettingEntry("fov",s"Field of view   ${fov.toInt}"),
        SettingEntry("fog",f"Horizon fog   $fogDensity%.1f"),
        toggle("vsync",s"VSync   ${onOff(vsync)}") { vsync = !vsync; glfwSwapInterval(if vsync then 1 else 0) },
        toggle("aa",s"Antialiasing   ${onOff(antiAliasing)}") { antiAliasing = !antiAliasing; applyAntiAliasingState() },
        toggle("af",s"Anisotropic textures   ${onOff(anisotropicFiltering)}") { anisotropicFiltering = !anisotropicFiltering; rebuildTextureAtlas() },
        toggle("lighting",s"Smooth lighting   ${onOff(smoothLighting)}") { smoothLighting = !smoothLighting; applySmoothLightingSetting() },
        toggle("shaders",s"Shaders   ${onOff(shadersEnabled)}") { shadersEnabled = !shadersEnabled; if shadersEnabled && shaderPipeline != null && !shaderPipeline.available then shaderPipeline.initialize() },
        toggle("pack",s"Shader pack   ${Option(shaderPipeline).map(_.packName).getOrElse("default")}") { if shaderPipeline != null then shaderPipeline.cyclePack() },
        toggle("scale",s"World resolution   ${if foveatedRendering then s"${(Option(foveaPass).map(_.internalScale).getOrElse(0.85f)*100).round}%" else "100%"}") { foveatedRendering = !foveatedRendering }
      )
      case 1 => Vector(
        SettingEntry("clouds",s"Cloud distance   $cloudRenderDistance chunks"),
        toggle("cloudToggle",s"Clouds   ${onOff(cloudsEnabled)}") { cloudsEnabled = !cloudsEnabled },
        toggle("weather",s"Weather   ${if isClientOnlyMultiplayer then "Host controlled" else onOff(weatherEnabled)}") { if !isClientOnlyMultiplayer then weatherEnabled = !weatherEnabled },
        toggle("darkness",s"Night shading   ${onOff(worldDarkening)}") { worldDarkening = !worldDarkening },
        toggle("fast",s"Fast movement   ${onOff(fastMove)}") { fastMove = !fastMove }
      ) ++ (if settingsCanHostMode then Vector(toggle("mode",s"Game mode   $gameMode") { toggleGameMode() }) else Vector.empty)
      case _ => Vector(
        toggle("audio",s"Sound   ${onOff(soundEnabled)}") { soundEnabled = !soundEnabled },
        toggle("fullscreen",s"Fullscreen   ${onOff(fullscreen)}") { toggleFullscreen() },
        toggle("gui",s"Interface scale   $guiScaleLabel") { cycleGuiScale() },
        toggle("resources",s"Textures   ${selectedTexturePack.name}") { openResources(Screen.Settings) },
        toggle("escape",s"Escape key   ${if pauseEscReturnsToGame then "Resume game" else "Quit to title"}") { pauseEscReturnsToGame = !pauseEscReturnsToGame }
      )

  private def settingsContentHeight(scale: Float): Float = settingsEntries.map(_.height).sum

  private def settingsMaxScroll(panelH: Float, scale: Float): Float =
    (settingsContentHeight(scale) - BlockboxUiLayout.settings(framebufferWidth,framebufferHeight,uiScale).content.h).max(0f)

  private def saveSelectedTexturePackId(): Unit =
    markSettingsDirty()
    flushUserSettings(force = true)

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

  private def createTextureAtlas(pack: TexturePack): TextureAtlas =
    TextureAtlas(pack, anisotropicFiltering)

  private def rebuildTextureAtlas(): Unit =
    val old = textureAtlas
    textureAtlas = createTextureAtlas(selectedTexturePack)
    if old != null then old.destroy()
    chunks.values.foreach { chunk =>
      chunk.setAtlas(textureAtlas)
      chunk.meshReady = false
      queueChunkMesh(chunk)
    }

  private def applySmoothLightingSetting(): Unit =
    chunks.values.foreach { chunk =>
      chunk.setSmoothLightingEnabled(smoothLighting)
      chunk.meshReady = false
      queueChunkMesh(chunk)
    }

  private def applyTexturePack(pack: TexturePack): Unit =
    if pack.id == selectedTexturePackId && textureAtlas != null then return
    selectedTexturePackId = pack.id
    saveSelectedTexturePackId()
    val old = textureAtlas
    textureAtlas = createTextureAtlas(pack)
    if old != null then old.destroy()
    chunks.values.foreach { chunk =>
      chunk.setAtlas(textureAtlas)
      chunk.markDirtyMesh()
      chunk.meshReady = false
      queueChunkMesh(chunk)
    }

  def run(): Unit =
    try
      loadUserSettings()
      initWindow()
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
      flushUserSettings(force = true)
      if activeWorldSession then saveWorld()
      awaitPendingWorldSaves()
      shutdownWorldSaveThread()
      chunks.values.foreach(_.dispose())
      chunks.clear()
      if shaderPipeline != null then shaderPipeline.destroy()
      firstPersonPass.destroy()
      shutdownAudio()
      if foveaPass != null then foveaPass.release()
      BlockboxSky.destroy()
      TorchEffects.destroy()
      BlockboxSkin.dispose()
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

    val requestedPlatform = sys.env.getOrElse("GLFW_PLATFORM", "auto").toLowerCase
    if requestedPlatform == "x11" then
      val supported = glfwPlatformSupported(GLFW_PLATFORM_X11)
      System.err.println(s"Blockbox: GLFW native reports X11 support=$supported")

    if !glfwInit() then throw RuntimeException("GLFW initialization failed")

    val selectedPlatform = glfwGetPlatform()
    val selectedPlatformName = selectedPlatform match
      case GLFW_PLATFORM_X11 => "x11"
      case GLFW_PLATFORM_WAYLAND => "wayland"
      case GLFW_PLATFORM_NULL => "null"
      case _ => s"unknown($selectedPlatform)"
    System.err.println(s"Blockbox: GLFW selected platform=$selectedPlatformName")

    def contextApiName(useEgl: Boolean): String =
      if useEgl then "EGL"
      else if selectedPlatform == GLFW_PLATFORM_X11 then "GLX/native"
      else "native"

    def resetWindowHints(samples: Int, requestLegacy21: Boolean, requestCompat33: Boolean, useEgl: Boolean): Unit =
      glfwDefaultWindowHints()
      glfwWindowHint(GLFW_CLIENT_API, GLFW_OPENGL_API)
      glfwWindowHint(GLFW_CONTEXT_CREATION_API, if useEgl then GLFW_EGL_CONTEXT_API else GLFW_NATIVE_CONTEXT_API)
      if requestLegacy21 then
        glfwWindowHint(GLFW_CONTEXT_VERSION_MAJOR, 2)
        glfwWindowHint(GLFW_CONTEXT_VERSION_MINOR, 1)
      else if requestCompat33 then
        // Blockbox's renderer intentionally retains compatibility-profile entry
        // points (legacy matrix stack/UI + GLSL 1.20) while the modern chunk path
        // uses VBOs and shaders.
        glfwWindowHint(GLFW_CONTEXT_VERSION_MAJOR, 3)
        glfwWindowHint(GLFW_CONTEXT_VERSION_MINOR, 3)
        glfwWindowHint(GLFW_OPENGL_PROFILE, GLFW_OPENGL_COMPAT_PROFILE)
      glfwWindowHint(GLFW_RESIZABLE, GLFW_TRUE)
      glfwWindowHint(GLFW_SAMPLES, samples)
      glfwWindowHint(GLFW_SCALE_TO_MONITOR, GLFW_TRUE)
      glfwWindowHintString(GLFW_WAYLAND_APP_ID, "blockbox")
      // Give X11 a stable WM_CLASS without affecting Wayland.
      glfwWindowHintString(GLFW_X11_CLASS_NAME, "Blockbox")
      glfwWindowHintString(GLFW_X11_INSTANCE_NAME, "blockbox")

    def destroyWindowIfNeeded(): Unit =
      try GL.setCapabilities(null)
      catch case _: Throwable => ()
      if glfwGetCurrentContext() != NULL then glfwMakeContextCurrent(NULL)
      if window != NULL then
        val failedWindow = window
        window = NULL
        glfwDestroyWindow(failedWindow)

    def tryCreateCapabilities(label: String, samples: Int, requestLegacy21: Boolean = false, requestCompat33: Boolean = false, useEgl: Boolean = false): Boolean =
      destroyWindowIfNeeded()
      resetWindowHints(samples, requestLegacy21, requestCompat33, useEgl)
      System.err.println(s"Blockbox: trying OpenGL context '$label' via ${contextApiName(useEgl)}")
      if fullscreen then
        val monitor = glfwGetPrimaryMonitor()
        val mode = glfwGetVideoMode(monitor)
        if mode == null then
          System.err.println(s"Blockbox: context '$label' failed: primary monitor has no video mode")
          return false
        window = glfwCreateWindow(mode.width, mode.height, "Blockbox", monitor, NULL)
      else window = glfwCreateWindow(width, height, "Blockbox", NULL, NULL)
      if window == NULL then
        System.err.println(s"Blockbox: context '$label' failed: glfwCreateWindow returned NULL")
        return false
      glfwMakeContextCurrent(window)
      if glfwGetCurrentContext() != window then
        System.err.println(s"Blockbox: context '$label' failed: glfwMakeContextCurrent did not stick")
        return false
      try
        // Capabilities must be recreated for each newly-created context. Clearing
        // them between attempts prevents a failed GLX context from poisoning a
        // later EGL attempt on the same thread.
        GL.setCapabilities(null)
        val caps = GL.createCapabilities()
        if caps == null then
          System.err.println(s"Blockbox: context '$label' failed: GL.createCapabilities returned null")
          return false
        glfwSwapInterval(if vsync then 1 else 0)
        glfwShowWindow(window)
        msaaSamples = if samples > 0 then glGetInteger(GL_SAMPLES).max(0) else 0
        applyAntiAliasingState()
        System.err.println(s"Blockbox: context '$label' succeeded via ${contextApiName(useEgl)}")
        true
      catch
        case t: Throwable =>
          System.err.println(s"Blockbox: context '$label' failed inside LWJGL: ${t.getClass.getName}: ${Option(t.getMessage).getOrElse("(no message)")}")
          false

    val forcedContextApi = sys.env.getOrElse("BLOCKBOX_GL_CONTEXT_API", "auto").trim.toLowerCase
    // On an X11 GLFW platform, "auto" intentionally means native/GLX. LWJGL
    // otherwise notices the parent Wayland session and defaults its own OpenGL
    // context provider to EGL, which cannot see GLFW's current GLX context.
    // Switching GLX <-> EGL after org.lwjgl.opengl.GL has initialized is also
    // unsafe, so one process uses one context-management API consistently.
    val effectiveContextApi =
      if forcedContextApi == "auto" && selectedPlatform == GLFW_PLATFORM_X11 then "native"
      else forcedContextApi
    val nativeTrialsAllowed = effectiveContextApi != "egl"
    val eglTrialsAllowed = effectiveContextApi == "egl" && selectedPlatform == GLFW_PLATFORM_X11
    System.err.println(s"Blockbox: OpenGL context policy requested=$forcedContextApi effective=$effectiveContextApi")

    def tryNativeContexts(): Boolean =
      nativeTrialsAllowed && (
        // Start with 3.3 compatibility: it satisfies the shader pipeline and all
        // legacy UI entry points in one context. Fall back to 2.1 for old drivers.
        tryCreateCapabilities("3.3 compatibility + 4x MSAA", 4, requestCompat33 = true) ||
        tryCreateCapabilities("3.3 compatibility", 0, requestCompat33 = true) ||
        tryCreateCapabilities("2.1 compatibility", 0, requestLegacy21 = true) ||
        tryCreateCapabilities("driver default", 0)
      )

    def tryEglContexts(): Boolean =
      eglTrialsAllowed && selectedPlatform == GLFW_PLATFORM_X11 && (
        tryCreateCapabilities("3.3 compatibility", 0, requestCompat33 = true, useEgl = true) ||
        tryCreateCapabilities("2.1 compatibility", 0, requestLegacy21 = true, useEgl = true) ||
        tryCreateCapabilities("driver default", 0, useEgl = true)
      )

    val contextOk = tryNativeContexts() || tryEglContexts()
    if !contextOk then
      destroyWindowIfNeeded()
      val platform = sys.env.getOrElse("GLFW_PLATFORM", "auto")
      val session = sys.env.getOrElse("XDG_SESSION_TYPE", "unknown")
      val info = s"GLFW_PLATFORM=$platform selected=$selectedPlatformName XDG_SESSION_TYPE=$session contextApi=$effectiveContextApi"
      System.err.println(s"Blockbox: OpenGL context setup failed after trying all fallbacks. $info")
      throw IllegalStateException(s"OpenGL context setup failed. $info. On X11 try BLOCKBOX_GL_CONTEXT_API=egl to bypass GLX, or BLOCKBOX_GL_CONTEXT_API=glx to force GLX diagnostics.")
    glEnable(0x809D)
    System.err.println(s"Blockbox: OpenGL context created — vendor=${glGetString(GL_VENDOR)} renderer=${glGetString(GL_RENDERER)} version=${glGetString(GL_VERSION)}")
    refreshResourcePacks()
    modManager.loadAll()
    CustomBlocks.seal()
    textureAtlas = createTextureAtlas(selectedTexturePack)
    shaderPipeline = BlockboxShaderPipeline(gameRootDir)
    if shadersEnabled then shaderPipeline.initialize()
    foveaPass = new BlockboxFovea()
    shaderPipeline.setFovea(foveaPass)
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
        if action == GLFW_PRESS || (action == GLFW_REPEAT && repeatableKey(key)) then onKey(key)
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
    BlockboxWorld.findSafeSpawn(terrainGen,(x,y,z) => loadedChunkForBlock(x,z).map(c => c.getBlock(Math.floorMod(x,16),y,Math.floorMod(z,16))))

  private def loop(): Unit =
    val now = glfwGetTime()
    lastFrameTime = (now - lastTime).min(0.1)
    val dt = lastFrameTime.toFloat.min(0.05f)
    lastTime = now
    completePendingJoin()
    if activeWorldSession && (screen == Screen.Playing || shouldRunMultiplayerBackgroundTick) then worldClock += lastFrameTime*modManager.rules.get("timeScale")
    publishedEnvironment = environmentMessage
    publishedRules = modManager.rules.snapshot
    publishedOps = oppedPlayerNames.toVector
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
    flushUserSettings()
    glfwSwapBuffers(window)
    glfwPollEvents()

  private def onChar(codepoint: Int): Unit =
    if chatOpen then
      if suppressNextChatChar then
        suppressNextChatChar = false
      else if codepoint >= 32 && codepoint <= 126 then
        chatInput += codepoint.toChar
        markChatEdited()
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
        selectedItemLabelTimer = 1.8f
      case Screen.Catalog =>
        catalogScroll = (catalogScroll + delta).max(0)
      case Screen.Mods =>
        modsScreenScroll = (modsScreenScroll + delta).max(0)
      case Screen.Resources =>
        resourcesScroll = (resourcesScroll + delta).max(0).min((resourcePacks.length - 1).max(0))
      case Screen.Settings =>
        val s = uiScale
        val pH = (630f * s).min(framebufferHeight.toFloat * 0.94f)
        settingsScroll = (settingsScroll + delta * 34f * s).max(0f).min(settingsMaxScroll(pH, s))
      case _ => ()

  private def repeatableKey(key: Int): Boolean =
    key == GLFW_KEY_BACKSPACE || key == GLFW_KEY_UP || key == GLFW_KEY_DOWN ||
      key == GLFW_KEY_LEFT || key == GLFW_KEY_RIGHT || key == GLFW_KEY_MINUS || key == GLFW_KEY_EQUAL

  private def markChatEdited(): Unit =
    commandSuggestionIndex = 0
    if chatHistoryIndex != -1 then
      chatHistoryIndex = -1
      chatHistoryDraft = chatInput

  private def showPreviousChatHistory(): Unit =
    if chatHistory.nonEmpty then
      if chatHistoryIndex == -1 then
        chatHistoryDraft = chatInput
        chatHistoryIndex = chatHistory.length - 1
      else chatHistoryIndex = (chatHistoryIndex - 1).max(0)
      chatInput = chatHistory(chatHistoryIndex)
      commandSuggestionIndex = 0

  private def showNextChatHistory(): Unit =
    if chatHistoryIndex != -1 then
      if chatHistoryIndex < chatHistory.length - 1 then
        chatHistoryIndex += 1
        chatInput = chatHistory(chatHistoryIndex)
      else
        chatHistoryIndex = -1
        chatInput = chatHistoryDraft
      commandSuggestionIndex = 0

  private def isPlaceableInventoryBlock(block: Block): Boolean =
    placeableBlocks.contains(block) || (CustomBlocks.isSlot(block) && CustomBlocks.registered(block))

  private def onKey(key: Int): Unit =
    if modManager.fireKeyPress(ModKeyEvent(modManager.api,key,modScreenName)).cancelled then return
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
            setCursorCaptured(captured = true)
            firstMouse = true
        else if key == GLFW_KEY_F then
          fastMove = !fastMove
          markSettingsDirty()
        else if key == GLFW_KEY_V then
          vsync = !vsync
          glfwSwapInterval(if vsync then 1 else 0)
          markSettingsDirty()
        else if key == GLFW_KEY_LEFT || key == GLFW_KEY_MINUS then changeRenderDistance(-1)
        else if key == GLFW_KEY_RIGHT || key == GLFW_KEY_EQUAL then changeRenderDistance(1)
        else if key == GLFW_KEY_UP then
          val s = uiScale; val pH = (630f * s).min(framebufferHeight.toFloat * 0.94f)
          settingsScroll = (settingsScroll - 34f * s).max(0f).min(settingsMaxScroll(pH, s))
        else if key == GLFW_KEY_DOWN then
          val s = uiScale; val pH = (630f * s).min(framebufferHeight.toFloat * 0.94f)
          settingsScroll = (settingsScroll + 34f * s).max(0f).min(settingsMaxScroll(pH, s))
        else if key == GLFW_KEY_M && canUseCheatAuthority && worldCheatsEnabled then toggleGameMode()
        else if key == GLFW_KEY_P then
          pauseEscReturnsToGame = !pauseEscReturnsToGame
          markSettingsDirty()
        else if key == GLFW_KEY_R then openResources(Screen.Settings)
      case Screen.Playing =>
        if chatOpen then
          if key == GLFW_KEY_ENTER then
            submitChat()
          else if key == GLFW_KEY_ESCAPE then
            chatOpen = false; chatInput = ""; chatHistoryIndex = -1; chatHistoryDraft = ""
          else if key == GLFW_KEY_BACKSPACE && chatInput.nonEmpty then
            chatInput = chatInput.init
            markChatEdited()
          else if key == GLFW_KEY_TAB then
            applyCommandSuggestion()
          else if key == GLFW_KEY_UP then
            showPreviousChatHistory()
          else if key == GLFW_KEY_DOWN then
            showNextChatHistory()
        else if key == GLFW_KEY_ESCAPE then leaveGame(Screen.Paused)
        else if key == GLFW_KEY_E then
          if gameMode == GameMode.Creative then openCatalog() else openInventory()
        else if key == GLFW_KEY_SLASH || key == GLFW_KEY_T then
          chatOpen = true
          chatInput = if key == GLFW_KEY_SLASH then "/" else ""
          chatHistoryIndex = -1
          chatHistoryDraft = chatInput
          suppressNextChatChar = true
        else if key >= GLFW_KEY_1 && key <= GLFW_KEY_9 then
          selectedBlock = key - GLFW_KEY_1
          selectedItemLabelTimer = 1.8f
        else if key == GLFW_KEY_M && canUseCheatAuthority && worldCheatsEnabled then toggleGameMode()
        else if key == GLFW_KEY_J then
          screen = Screen.Journal
          setCursorCaptured(captured = false)
        else if key == GLFW_KEY_F3 then debugMode = !debugMode
        else if key == GLFW_KEY_F4 then
          wireframeMode = !wireframeMode
          if wireframeMode then glPolygonMode(GL_FRONT_AND_BACK, GL_LINE) else glPolygonMode(GL_FRONT_AND_BACK, GL_FILL)
        else if key == GLFW_KEY_F5 then showChunkBorders = !showChunkBorders
        else if key == GLFW_KEY_F8 then modSetCursorMode(!modUiCursorMode)
        else if key == GLFW_KEY_F2 then
          settingsReturnTo = Screen.Playing
          screen = Screen.Settings
          setCursorCaptured(captured = false)
      case Screen.Journal =>
        if key == GLFW_KEY_ESCAPE || key == GLFW_KEY_J || key == GLFW_KEY_ENTER then enterGame()
      case Screen.JoinGame =>
        if key == GLFW_KEY_ENTER then
          val ip = if joinIpInput.trim.isEmpty then "127.0.0.1" else joinIpInput.trim
          joinStatusError = false
          joinStatusMessage = s"Connecting to $ip ..."
          joinGame(ip)
        else if key == GLFW_KEY_ESCAPE then
          stopNetworking(); screen = Screen.MainMenu
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
    val rightDown = glfwGetMouseButton(window, GLFW_MOUSE_BUTTON_RIGHT) == GLFW_PRESS
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
        case Screen.Inventory => handleInventoryClick(mx, my, rightClick = false)
        case Screen.Catalog => handleCatalogClick(mx, my)
        case Screen.FurnaceUI => handleFurnaceClick(mx, my)
        case Screen.JoinGame => handleJoinClick(mx, my)
        case Screen.HostGame => handleHostClick(mx, my)
        case Screen.Journal =>
          val p = journalPanel
          if UiRect(p.x+p.w/2f-100f*uiScale,p.y+p.h-50f*uiScale,200f*uiScale,34f*uiScale).contains(mx,my) then enterGame()
        case Screen.Playing => ()
    if rightDown && !menuRightWasDown && screen == Screen.Inventory then
      val (mx, my) = mouseFramebufferPos()
      handleInventoryClick(mx, my, rightClick = true)
    if leftDown && screen == Screen.Settings && sliderActive != null then
      val (mx, my) = mouseFramebufferPos()
      updateSlider(mx)
    if !leftDown then sliderActive = null
    menuLeftWasDown = leftDown
    menuRightWasDown = rightDown

  private def updateSlider(mx: Float): Unit =
    val layout = BlockboxUiLayout.settings(framebufferWidth,framebufferHeight,uiScale)
    val value = ((mx-layout.content.x-8f*uiScale)/(layout.content.w-16f*uiScale)).max(0f).min(1f)
    sliderActive match
      case "rd" =>
        val next = (minRenderDistance+value*(maxRenderDistance-minRenderDistance)).round
        if next != renderDistance then
          renderDistance = next
          syncChunks()
      case "fog" => fogDensity = 0.6f+value*2.4f
      case "fov" => fov = 50f+value*50f
      case "clouds" => cloudRenderDistance = (minCloudRenderDistance+value*(maxCloudRenderDistance-minCloudRenderDistance)).round
      case _ => ()
    markSettingsDirty()

  private def handleMainMenuClick(mx: Float, my: Float): Unit =
    val layout = mainMenuLayout()
    def hit(i: Int): Boolean = layout.buttons(i).contains(mx,my)
    if hit(0) then screen = Screen.CreateWorld
    else if hit(1) then
      hostNameFocused = true
      hostStatusError = false
      hostStatusMessage = "Hosts on 0.0.0.0:25565. Share your LAN/ZeroTier IP."
      screen = Screen.HostGame
    else if hit(2) then
      joinIpInput = ""
      joinNameFocused = true
      joinStatusError = false
      joinStatusMessage = "Enter host IP. You can use IP:port; default port is 25565."
      screen = Screen.JoinGame
    else if hit(3) then openLoadWorldMenu()
    else if hit(4) then screen = Screen.Mods
    else if hit(5) then openResources(Screen.MainMenu)
    else if hit(6) then
      screen = Screen.Settings
      settingsReturnTo = Screen.MainMenu
    else if hit(7) then glfwSetWindowShouldClose(window, true)

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
    if label.toLowerCase.contains("seed") && display.nonEmpty && display.forall(ch => ch == '-' || ch.isDigit) then
      val cell = ((w - 34f * s) / ((display.length * 4 - 1).max(1)).toFloat).min((h - 13f * s) / 5f).max(1.5f)
      drawCompactNumber(x + w / 2f - compactNumberWidth(display, cell) / 2f, y + h / 2f - 2.5f * cell, display, cell, 0.92f, 0.96f, 1f)
    else
      val textScale = (0.98f * s).max(0.86f)
      centeredTextFit(x + w / 2f, y + h / 2f - 6f * s, display, 0.92f, 0.96f, 1f, textScale, w - 28f * s)

  private def renderHostGame(): Unit =
    glDisable(GL_DEPTH_TEST); glDisable(GL_CULL_FACE); setupOrtho()
    val w = framebufferWidth.toFloat; val h = framebufferHeight.toFloat; val cx = w / 2f; val s = uiScale
    BlockboxRender2D.drawMenuBackdrop(framebufferWidth, framebufferHeight, glfwGetTime().toFloat)
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
    BlockboxRender2D.drawMenuBackdrop(framebufferWidth, framebufferHeight, glfwGetTime().toFloat)
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
    drawButton(cx - bw / 2f, pY + pH - 92f * s, bw, bh, if pendingJoin != null || applyingJoin then "Connecting..." else "Connect", accent = true)
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
    else if inRect(mx, my, cx - bw / 2f, pY + pH - 44f * s, bw, bh) then
      stopNetworking(); screen = Screen.MainMenu

  private def closeSettings(): Unit =
    sliderActive = null
    screen = settingsReturnTo
    if settingsReturnTo == Screen.Playing then
      setCursorCaptured(captured = true)
      firstMouse = true

  private def handleSettingsClick(mx: Float, my: Float): Unit =
    val layout = BlockboxUiLayout.settings(framebufferWidth,framebufferHeight,uiScale)
    val tab = layout.tabs.indexWhere(_.contains(mx,my))
    if tab >= 0 then
      settingsTab = tab; settingsScroll = 0f; sliderActive = null
    else if layout.done.contains(mx,my) then closeSettings()
    else if layout.content.contains(mx,my) then
      var y = layout.content.y-settingsScroll
      var clicked: Option[SettingEntry] = None
      settingsEntries.foreach { entry =>
        val row = UiRect(layout.content.x,y,layout.content.w,entry.height-4f*uiScale)
        if clicked.isEmpty && row.contains(mx,my) then clicked = Some(entry)
        y += entry.height
      }
      clicked.foreach { entry =>
        if entry.id == "rd" || entry.id == "fov" || entry.id == "fog" || entry.id == "clouds" then
          sliderActive = entry.id; updateSlider(mx)
        else entry.action()
      }

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

  private def handleInventoryClick(mx: Float, my: Float, rightClick: Boolean): Unit =
    val cx = framebufferWidth / 2f
    val cy = framebufferHeight / 2f
    val s = uiScale
    val pw = (760f * s).min(framebufferWidth * 0.94f)
    val ph = (500f * s).min(framebufferHeight * 0.88f)
    val px = cx - pw / 2f
    val py = cy - ph / 2f

    val bottomH = (34f * s).min(ph * 0.08f).max(28f)
    if inRect(mx, my, px + 22f * s, py + ph - 46f * s, 130f * s, bottomH) then
      if releaseHeldItem() then openFurnace()
      return
    else if inRect(mx, my, px + pw - 104f * s, py + ph - 46f * s, 82f * s, bottomH) then
      if releaseHeldItem() then enterGame()
      return

    val hotbarY = py + ph - 104f * s
    val hotbarPanelW = pw - 56f * s
    val hotbarX = px + 28f * s
    val hotGap = (5f * s).max(4f)
    val hotSlot = ((hotbarPanelW - hotGap * (hotbarBlocks.length - 1)) / hotbarBlocks.length).min(44f * s).max(28f)
    val hotBarW = hotSlot * hotbarBlocks.length + hotGap * (hotbarBlocks.length - 1)
    val hotStartX = hotbarX + (hotbarPanelW - hotBarW) / 2f
    var i = 0
    while i < hotbarBlocks.length do
      val sx = hotStartX + i * (hotSlot + hotGap)
      if inRect(mx, my, sx, hotbarY, hotSlot, hotSlot) then
        if heldInventoryBlock != Block.Air then placeHeldItemInHotbar(i, rightClick)
        else
          selectedBlock = i
          takeHotbarSlot(i, rightClick)
        selectedItemLabelTimer = 1.2f
        return
      i += 1

    val gridW = pw * 0.58f
    val cols = 9
    val gap = (4f * s).max(3f)
    val slot = ((gridW - gap * (cols - 1)) / cols).min(44f * s).max(30f)
    val invX = px + 28f * s
    val invY = py + 76f * s
    val items = inventoryStacks
    i = 0
    while i < inventoryGridSlots do
      val col = i % cols
      val row = i / cols
      val sx = invX + col * (slot + gap)
      val sy = invY + row * (slot + gap)
      if inRect(mx, my, sx, sy, slot, slot) then
        handleBackpackSlotClick(i, rightClick)
        return
      i += 1

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
        handleCraftSlotClick(ci, rightClick)
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
      if releaseHeldItem() then
        catalogScroll = 0
        enterGame()
      return

    val hotbarY = panelY + panelH - 76f * s
    val hotbarPanelW = panelW - 40f * s
    val hotbarX = panelX + 20f * s
    val hotGap = (5f * s).max(4f)
    val hotSlot = ((hotbarPanelW - hotGap * (hotbarBlocks.length - 1)) / hotbarBlocks.length).min(44f * s).max(28f)
    val hotBarW = hotSlot * hotbarBlocks.length + hotGap * (hotbarBlocks.length - 1)
    val hotStartX = hotbarX + (hotbarPanelW - hotBarW) / 2f
    var i = 0
    while i < hotbarBlocks.length do
      val sx = hotStartX + i * (hotSlot + hotGap)
      if inRect(mx, my, sx, hotbarY, hotSlot, hotSlot) then
        if heldInventoryBlock != Block.Air then
          placeHeldItemInHotbar(i)
        else selectedBlock = i
        return
      i += 1

    val slotSize = (48f * s).max(34f).min(54f)
    val gridX = panelX + 20f * s
    val gridY = panelY + 52f * s
    val cols = ((panelW - 54f * s) / slotSize).toInt.max(4).min(12)
    val items = catalogItems
    val visibleRows = ((hotbarY - gridY - 26f * s) / slotSize).toInt.max(1)
    val scrollMax = ((items.length - 1) / cols - visibleRows + 2).max(0)
    if catalogScroll > scrollMax then catalogScroll = scrollMax
    i = catalogScroll * cols
    val catalogEnd = (i + cols * visibleRows).min(items.length)
    while i < catalogEnd do
      val idx = i - catalogScroll * cols
      val col = idx % cols
      val row = idx / cols
      val sx = gridX + col * slotSize
      val sy = gridY + row * slotSize
      if inRect(mx, my, sx, sy, slotSize, slotSize) then
        setHeldItem(items(i), maxStackSize)
        return
      i += 1
    val scrollUp = inRect(mx, my, panelX + panelW - 18f * s, panelY + 50f * s, 14f * s, 20f * s)
    val scrollDown = inRect(mx, my, panelX + panelW - 18f * s, hotbarY - 26f * s, 14f * s, 20f * s)
    if scrollUp then catalogScroll = (catalogScroll - 1).max(0)
    if scrollDown then catalogScroll = (catalogScroll + 1).min(scrollMax)
    if catalogScroll < 0 then catalogScroll = 0

  private def inRect(px: Float, py: Float, x: Float, y: Float, w: Float, h: Float): Boolean =
    px >= x && px <= x + w && py >= y && py <= y + h

  private def setCursorCaptured(captured: Boolean): Unit =
    glfwSetInputMode(window, GLFW_CURSOR, if captured then GLFW_CURSOR_DISABLED else GLFW_CURSOR_NORMAL)
    if glfwRawMouseMotionSupported() then
      glfwSetInputMode(window, GLFW_RAW_MOUSE_MOTION, if captured then GLFW_TRUE else GLFW_FALSE)

  private def mouseFramebufferPos(): (Float, Float) =
    glfwGetCursorPos(window, cursorXBuffer, cursorYBuffer)
    glfwGetWindowSize(window, windowWidthBuffer, windowHeightBuffer)
    val sx = framebufferWidth.toFloat / windowWidthBuffer.get(0).max(1)
    val sy = framebufferHeight.toFloat / windowHeightBuffer.get(0).max(1)
    ((cursorXBuffer.get(0).toFloat * sx), (cursorYBuffer.get(0).toFloat * sy))

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
      publishedEnvironment = environmentMessage
      publishedOps = oppedPlayerNames.toVector
      val server = GameServer(
        serverPort,
        worldSeed,
        camera,
        playerName,
        localColorId,
        () => snapshotWorldEditsForNetwork(),
        () => publishedOps,
        () => camera,
        () => yaw,
        () => pitch,
        modManager.serverModpackHash,
        modManager.serverModpackList,
        () => publishedEnvironment,
        () => publishedRules
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
          setCursorCaptured(captured = false)
    catch
      case e: java.net.BindException =>
        stopNetworking()
        hostStatusError = true
        hostStatusMessage = s"Port $serverPort is already in use. Close the old server or use another port."
        screen = Screen.HostGame
        setCursorCaptured(captured = false)
      case e: Exception =>
        stopNetworking()
        hostStatusError = true
        val msg = Option(e.getMessage).filter(_.nonEmpty).getOrElse(e.getClass.getSimpleName)
        hostStatusMessage = "Could not host: " + msg
        screen = Screen.HostGame
        setCursorCaptured(captured = false)

  private def joinGame(ip: String): Unit =
    if pendingJoin != null || applyingJoin then return
    playerName = cleanPlayerName()
    stopNetworking()
    parseServerAddress(ip) match
      case Left(error) =>
        joinStatusError = true; joinStatusMessage = error.toString
      case Right((host,port)) =>
        joinStatusError = false
        joinStatusMessage = s"Connecting to $host:$port ..."
        val client = GameClient(host,port)
        gameClient = client; pendingJoin = client
        val name = playerName; val hash = modManager.clientJoinHash
        BlockboxSockets.daemonThread("blockbox-join",() =>
          val connected = client.connect(name,hash)
          if pendingJoin eq client then joinResult = (client,connected)
          else client.disconnect()
        ).start()

  private def completePendingJoin(): Unit =
    val result = joinResult
    if result != null then
      joinResult = null
      val (client,success) = result
      if pendingJoin eq client then
        pendingJoin = null
        if success then
          multiplayerMode = true; applyingJoin = true
          knownPlayerNames.clear(); knownPlayerNames += playerName
          playerColors.clear(); oppedPlayerNames.clear()
          worldSeed = 0L; terrainGen = createTerrainGenerator(0L); terrainHeightCache.clear()
          clearLoadedChunks(saveFirst=false)
          fieldJournal = FieldJournal(); worldClock = 150.0; timeOverride = None
          gameMode = GameMode.Survival; resetInventory()
          velocity = Vec3(0f,0f,0f); onGround = false; yaw = 0f; pitch = 0f
          joinStatusMessage = "Synchronizing the world..."
        else
          val message = client.errorMessage
          client.disconnect(); gameClient = null
          joinStatusError = true
          joinStatusMessage = if message.nonEmpty then message else "Could not connect to that server."
    if applyingJoin && gameClient != null then
      processNetworkMessages()
      syncChunks(); processChunkWorkMainThread(0,8)
      if !gameClient.isConnected then
        joinStatusError = true; joinStatusMessage = "Server disconnected during world synchronization."
        applyingJoin = false
      else if gameClient.pendingMessages == 0 then
        applyingJoin = false
        startGame()
        modManager.fireWorldLoaded()
      else joinStatusMessage = s"Synchronizing the world... ${gameClient.pendingMessages} updates left"

  private def stopNetworking(): Unit =
    pendingJoin = null; joinResult = null; applyingJoin = false
    if gameClient != null then gameClient.disconnect(); gameClient = null
    if gameServer != null then gameServer.stop(); gameServer = null
    remotePlayers.clear()
    knownPlayerNames.clear()
    playerColors.clear()
    oppedPlayerNames.clear()
    pendingNetworkBlocks.clear()
    networkBlockOverrides.clear()
    networkWaterLevelOverrides.clear()
    pendingNetworkLogAxes.clear()
    networkLogAxesOverrides.clear()
    networkTorchMounts.clear()
    lastMultiplayerHeartbeat = 0.0
    lastEnvironmentSend = -10.0
    environmentReceived = false
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
    activeWorldSession = true
    modUiCursorMode = false
    firstMouse = true
    selectedItemLabelTimer = 1.6f
    footstepTravel = 0f
    cameraBobAmount = 0f
    setCursorCaptured(captured = true)

  private def enterGame(): Unit =
    if (screen == Screen.Inventory || screen == Screen.Catalog || screen == Screen.FurnaceUI) && !releaseHeldItem() then return
    startGame()

  private def leaveGame(next: Screen): Unit =
    // Pausing must NOT close the multiplayer server. The host needs to be able to tab away,
    // open the pause menu, or focus another local test client while the socket keeps listening.
    // Only tear networking down when actually returning to title/main menu.
    if next == Screen.MainMenu then
      saveWorld()
      modManager.fireWorldUnloaded()
      updateRainAmbience(0f)
      activeWorldSession = false
      stopNetworking()
      clearLoadedChunks(saveFirst = false)
    screen = next
    setCursorCaptured(captured = false)

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
    if gameMode == GameMode.Survival then syncInventoryTotalsFromBackpack()
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
    // v3 physical inventory/crafting state. The aggregate section above remains
    // for backward readers and migration recovery.
    out.writeInt(backpackBlocks.length)
    var bi = 0
    while bi < backpackBlocks.length do
      out.writeByte(backpackBlocks(bi).ordinal.toByte)
      out.writeInt(backpackCounts(bi).max(0).min(maxStackSize))
      bi += 1
    out.writeInt(craftGridBlocks.length)
    bi = 0
    while bi < craftGridBlocks.length do
      out.writeByte(craftGridBlocks(bi).ordinal.toByte)
      out.writeInt(craftGridCounts(bi).max(0).min(maxStackSize))
      bi += 1

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
          val savedHotbarBlocks = Array.fill(hotLen)(Block.Air)
          i = 0
          while i < hotLen do
            val block = Block.fromId(in.readByte())
            savedHotbarBlocks(i) = block
            if i < hotbarBlocks.length && block != Block.Water then hotbarBlocks(i) = block
            i += 1
          if version >= 2 && in.available() >= hotLen * 4 then
            i = 0
            while i < hotLen do
              val count = in.readInt().max(0).min(maxStackSize)
              if i < hotbarCounts.length then hotbarCounts(i) = count
              else if count > 0 && validHotbarBlock(savedHotbarBlocks(i)) then
                // Older builds had a tenth hotbar slot. Preserve it by moving
                // the stack into the backpack during the 9-slot migration.
                inventory(savedHotbarBlocks(i).ordinal) += count
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
          if version >= 3 && in.available() >= 4 then
            val backpackLen = in.readInt().max(0).min(256)
            var si = 0
            while si < backpackBlocks.length do
              backpackBlocks(si) = Block.Air; backpackCounts(si) = 0; si += 1
            si = 0
            while si < backpackLen && in.available() >= 5 do
              val b = Block.fromId(in.readByte()); val count = in.readInt().max(0).min(maxStackSize)
              if si < backpackBlocks.length && validHotbarBlock(b) && count > 0 then
                backpackBlocks(si) = b
                backpackCounts(si) = count
              si += 1
            if in.available() >= 4 then
              val craftLen = in.readInt().max(0).min(64)
              si = 0
              while si < craftGridBlocks.length do
                craftGridBlocks(si) = Block.Air; craftGridCounts(si) = 0; si += 1
              si = 0
              while si < craftLen && in.available() >= 5 do
                val b = Block.fromId(in.readByte()); val count = in.readInt().max(0).min(maxStackSize)
                if si < craftGridBlocks.length && validHotbarBlock(b) && count > 0 then
                  craftGridBlocks(si) = b
                  craftGridCounts(si) = count
                si += 1
            syncInventoryTotalsFromBackpack()
          else if gameMode == GameMode.Survival then rebuildBackpackFromInventoryTotals()
    catch case e: Exception => System.err.println(s"World extras load skipped: $e")

  private def snapshotWorldEditsForNetwork(): Seq[(Int, Int, Int, Byte, Int, Int, Int)] =
    val loadedEdits = chunks.values.toSeq.flatMap { chunk =>
      val bx = chunk.baseX
      val bz = chunk.baseZ
      chunk.edits.synchronized {
        chunk.edits.toList.map { case ((lx, ly, lz), block) =>
          (bx + lx, ly, bz + lz, block.id, chunk.getWaterRawLevel(lx,ly,lz), chunk.getLogAxis(lx, ly, lz).id, chunk.getTorchMount(lx,ly,lz).id)
        }
      }
    }
    val rememberedEdits = networkBlockOverrides.toSeq.map { case ((x, y, z), block) =>
      (x, y, z, block.id, networkWaterLevelOverrides.getOrElse((x,y,z),0.toByte).toInt & 0xff, networkLogAxesOverrides.getOrElse((x, y, z), 0), networkTorchMounts.getOrElse((x,y,z),0))
    }
    (loadedEdits ++ rememberedEdits).reverse.distinctBy(t => (t._1, t._2, t._3)).reverse

  private def startWorldSaveThread(): Unit =
    if worldSaveExecutor == null || worldSaveExecutor.isShutdown then
      worldSaveExecutor = Executors.newSingleThreadExecutor((job: Runnable) =>
        val t = Thread(job, "blockbox-save-io")
        t.setDaemon(true)
        t
      )

  private def queueWorldSaveJob(label: String)(job: => Unit): Unit =
    startWorldSaveThread()
    val serial = saveJobSerial.incrementAndGet()
    pendingSaveJobs.incrementAndGet()
    latestSaveFuture = worldSaveExecutor.submit(new Runnable:
      override def run(): Unit =
        try job
        catch case e: Throwable => System.err.println(s"Blockbox save #$serial ($label) failed: $e")
        finally pendingSaveJobs.decrementAndGet()
    )

  /** Wait only when correctness requires it (loading or process shutdown), never while
    * returning to the title screen. A single-thread executor means waiting for the
    * newest future also waits for every older save in order.
    */
  private def awaitPendingWorldSaves(): Unit =
    val f = latestSaveFuture
    if f != null then
      try f.get()
      catch
        case _: java.util.concurrent.CancellationException => ()
        case _: InterruptedException => Thread.currentThread().interrupt()
        case e: java.util.concurrent.ExecutionException => System.err.println(s"Blockbox save wait failed: ${e.getCause}")

  private def shutdownWorldSaveThread(): Unit =
    val exec = worldSaveExecutor
    if exec != null then
      exec.shutdown()
      try
        if !exec.awaitTermination(30, TimeUnit.SECONDS) then exec.shutdownNow()
      catch
        case _: InterruptedException =>
          exec.shutdownNow()
          Thread.currentThread().interrupt()
      worldSaveExecutor = null

  private def snapshotWorldDataBytes(): Array[Byte] =
    val raw = new ByteArrayOutputStream(16 * 1024)
    val meta = new DataOutputStream(new BufferedOutputStream(raw, 32 * 1024))
    meta.writeLong(worldSeed)
    meta.writeFloat(camera.x); meta.writeFloat(camera.y); meta.writeFloat(camera.z)
    meta.writeFloat(yaw); meta.writeFloat(pitch)
    meta.writeByte(gameMode.ordinal.toByte)
    meta.writeBoolean(worldCheatsEnabled)
    val keys = chunks.keys.toVector.sortBy(k => (k._1, k._2))
    meta.writeInt(keys.size)
    keys.foreach { case (cx, cz) => meta.writeInt(cx); meta.writeInt(cz) }
    writeWorldExtras(meta)
    meta.writeInt(0x454E5631) // ENV1: optional clock/weather/journal tail; v3 inventory stays compatible.
    meta.writeUTF(environmentMessage)
    fieldJournal.write(meta)
    meta.writeInt(0x52454731) // REG1 custom voxel ID mapping
    meta.writeUTF(CustomBlocks.manifest)
    meta.writeUTF(modManager.rules.snapshot)
    meta.flush()
    raw.toByteArray

  private def saveLoadedChunks(): Unit =
    if !canUseLocalChunkSaves then return
    try
      val prepared = BlockboxSaveFiles.prepareWorldForSave(currentWorldDir.toPath)
      val snapshots = chunks.toVector.map { case ((cx, cz), chunk) => (cx, cz, chunk.captureSaveSnapshot()) }
      snapshots.foreach { case (cx, cz, _) => dirtyChunksForSave -= ((cx, cz)) }
      queueWorldSaveJob(s"${prepared.worldDirectory().getFileName} chunk batch") {
        snapshots.foreach { case (cx, cz, snapshot) =>
          BlockboxSaveFiles.writeChunkBytesAtomic(prepared.chunksDirectory(), cx, cz, snapshot.toBytes(), false)
        }
        BlockboxSaveFiles.syncPreparedWorld(prepared)
      }
    catch case e: Exception => System.err.println(s"Chunk save snapshot failed: $e")

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
      val prepared = BlockboxSaveFiles.prepareWorldForSave(currentWorldDir.toPath)
      val worldBytes = snapshotWorldDataBytes()
      val dirtyKeys = dirtyChunksForSave.toVector.sortBy(k => (k._1, k._2))
      val chunkSnapshots = dirtyKeys.flatMap { case (cx, cz) =>
        chunks.get((cx, cz)).map(chunk => (cx, cz, chunk.captureSaveSnapshot()))
      }
      chunkSnapshots.foreach { case (cx, cz, _) => dirtyChunksForSave -= ((cx, cz)) }
      queueWorldSaveJob(s"${prepared.worldDirectory().getFileName}") {
        // Encode snapshots on the save worker, rename every chunk atomically, then pay
        // for one directory durability barrier instead of fsyncing once per chunk.
        chunkSnapshots.foreach { case (cx, cz, snapshot) =>
          BlockboxSaveFiles.writeChunkBytesAtomic(prepared.chunksDirectory(), cx, cz, snapshot.toBytes(), false)
        }
        BlockboxSaveFiles.writeWorldBytesAtomic(prepared.worldDirectory(), worldBytes, false)
        BlockboxSaveFiles.syncPreparedWorld(prepared)
        BlockboxSaveFiles.writeWorldIndex(prepared.worldsRoot())
      }
    catch case e: Exception => System.err.println(s"Save snapshot failed: $e")

  private def openLoadWorldMenu(): Unit =
    loadWorldSelection = loadWorldSelection.max(0).min((worldSaveDirs.length - 1).max(0))
    loadWorldScroll = loadWorldSelection.max(0)
    screen = Screen.LoadWorld
    setCursorCaptured(captured = false)

  private def openResources(returnTo: Screen): Unit =
    refreshResourcePacks()
    resourcesReturnTo = returnTo
    resourcesScroll = resourcePacks.indexWhere(_.id == selectedTexturePackId).max(0)
    screen = Screen.Resources
    setCursorCaptured(captured = false)

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
    // If the user immediately re-enters a world after Save and Quit, finish the
    // background write here rather than loading the previous on-disk snapshot.
    awaitPendingWorldSaves()
    try
      worldName = worldDir.getName
      BlockboxSaveFiles.readWorldData(worldDir.toPath, meta =>
        worldSeed = meta.readLong()
        val x = meta.readFloat(); val y = meta.readFloat(); val z = meta.readFloat()
        camera = Vec3(x, y, z)
        yaw = meta.readFloat(); pitch = meta.readFloat()
        gameMode = GameMode.fromOrdinal(meta.readByte().toInt)
        worldCheatsEnabled = meta.readBoolean()
        terrainGen = createTerrainGenerator(worldSeed)
        terrainHeightCache.clear()
        clearLoadedChunks(saveFirst = false)
        resetInventory()
        networkBlockOverrides.clear()
        val chunkCount = meta.readInt().max(0).min(200000)
        val storedChunks=Vector.fill(chunkCount)((meta.readInt(),meta.readInt()))
        readWorldExtras(meta)
        worldClock = 150.0; timeOverride = None; fieldJournal = FieldJournal()
        if meta.available() >= 4 && meta.readInt() == 0x454E5631 then
          val env = BlockboxNet.parseEnvironment(BlockboxNet.packet(meta.readUTF()))
          worldClock = env.time; timeOverride = if env.timeOverride < 0f then None else Some(env.timeOverride)
          rainStrength = env.rain; wetness = env.wetness; stormCloudCover = env.cloudCover
          weatherWind = env.wind; lightningFlash = env.lightning
          fieldJournal.read(meta)
          if meta.available()>=4 && meta.readInt()==0x52454731 then
            require(meta.readUTF()==CustomBlocks.manifest,"This world requires its original custom-block registry. Restore the world's modpack before loading it.")
            modManager.rules.receive(meta.readUTF())
        storedChunks.foreach((cx,cz) => loadChunkIfSaved(cx,cz))
      )
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
    else if validHotbarBlock(block) then backpackItemCount(block) + hotbarItemCount(block) else 0

  protected def gainItem(block: Block, amount: Int): Unit =
    if validHotbarBlock(block) && amount > 0 then
      if gameMode==GameMode.Survival then fieldJournal.collected(block,amount)
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
      if remaining > 0 then
        val stored = addBackpackItem(block, remaining)
        remaining -= stored
      if remaining > 0 && gameMode == GameMode.Survival then addChatMessage(s"Inventory full: $remaining ${blockName(block)} could not be stored")
      compactHotbarAssignments()

  protected def consumeInventory(block: Block, amount: Int): Boolean =
    if amount <= 0 then true
    else if gameMode == GameMode.Creative then true
    else if !validHotbarBlock(block) || totalItemCount(block) < amount then false
    else
      var remaining = amount
      val fromBackpack = removeBackpackItem(block, remaining)
      remaining -= fromBackpack
      var i = 0
      while i < hotbarBlocks.length && remaining > 0 do
        if hotbarBlocks(i) == block && hotbarCounts(i) > 0 then
          val used = remaining.min(hotbarCounts(i)); hotbarCounts(i) -= used; remaining -= used
        i += 1
      compactHotbarAssignments()
      remaining == 0

  private def submitChat(): Unit =
    val text = chatInput.trim
    if text.nonEmpty then
      if chatHistory.lastOption.forall(_ != chatInput) then chatHistory += chatInput
      chatHistoryIndex = -1
      chatHistoryDraft = ""
      val event = modManager.fireChat(ModChatEvent(modManager.api, playerName, text, cancelled = false))
      val finalText = Option(event.message).getOrElse("").trim
      if !event.cancelled && finalText.nonEmpty then
        if finalText.startsWith("/") then parseCommand(finalText)
        else sendChatMessage(finalText)
    chatOpen = false
    chatInput = ""
    chatHistoryDraft = ""

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
    setCursorCaptured(captured = false)
    breakingBlock = null
    breakingProgress = 0f

  private def openCatalog(): Unit =
    catalogScroll = 0
    screen = Screen.Catalog
    setCursorCaptured(captured = false)
    breakingBlock = null
    breakingProgress = 0f

  private def openFurnace(): Unit =
    screen = Screen.FurnaceUI
    setCursorCaptured(captured = false)
    breakingBlock = null
    breakingProgress = 0f

  private def handleLostConnection(): Boolean =
    if isClientOnlyMultiplayer && gameClient != null && !gameClient.isConnected then
      val message = gameClient.errorMessage
      leaveGame(Screen.MainMenu)
      screen = Screen.JoinGame
      joinStatusError = true
      joinStatusMessage = if message.nonEmpty then message else "The host closed the connection."
      true
    else false

  private def updateGame(dt: Float): Unit =
    if handleLostConnection() then return
    processNetworkMessages()
    smoothRemotePlayers(dt)
    syncChunks()
    // Keep the camera neighborhood deterministic. If the local terrain is not
    // ready, build the 5x5 ring right now instead of rendering a sky-only void.
    if !centerChunkReady then forceCameraChunkRing(0)
    processChunkWorkMainThread(0, chunkUploadBudget)
    updateGrassGrowth(dt)
    for i <- chatMessages.indices do
      val (msg, t) = chatMessages(i)
      chatMessages(i) = (msg, t - dt)
    chatMessages.filterInPlace((_, t) => t > -2f)
    cleanupRemotePlayers()
    if chatOpen || modUiCursorMode then
      updateWeather(dt)
      updateBlockParticles(dt)
      updateTorchEffects(dt)
      updateGameFeel(dt,camera,false,false,false)
    if chatOpen then
      // Chat is also a client-side UI state in multiplayer; keep the player present/alive.
      sendPlayerPositionNetwork()
      return
    if modUiCursorMode then
      handleModGuiMouse()
      sendPlayerPositionNetwork()
      return
    val frameStartCamera = camera
    glfwGetCursorPos(window, cursorXBuffer, cursorYBuffer)
    val mouseX = cursorXBuffer.get(0)
    val mouseY = cursorYBuffer.get(0)
    if firstMouse then
      lastMouseX = mouseX; lastMouseY = mouseY; firstMouse = false
    val dx = (mouseX - lastMouseX).toFloat
    val dy = (mouseY - lastMouseY).toFloat
    lastMouseX = mouseX; lastMouseY = mouseY
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
              velocity = Vec3(0f,modManager.rules.get("jumpSpeed"),0f)
              onGround = false
            velocity = Vec3(0f, (velocity.y - modManager.rules.get("gravity") * dt).max(-78f), 0f)
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
        val horizontal = move.normalized * speed * modManager.rules.get("moveSpeed") * airControl * dt
        if crouching && onGround then
          val hx = if hasGroundBelow(camera + Vec3(horizontal.x, 0f, 0f)) then horizontal.x else 0f
          val hz = if hasGroundBelow(camera + Vec3(0f, 0f, horizontal.z)) then horizontal.z else 0f
          movePlayer(hx, 0f, hz)
        else
          movePlayer(horizontal.x, 0f, horizontal.z)
        updateSwimming(dt, swimming, headInWater, bodyInWater, submerged)
        moveVertical(velocity.y * dt)
        if swimming then onGround = false
    if modManager.events.hasListeners("move") then
      val event=modManager.events.fireMove(ModMoveEvent(modManager.api,playerName,frameStartCamera,camera,dt))
      val dest=event.destination
      if event.cancelled then camera=frameStartCamera
      else if dest!=null && Seq(dest.x,dest.y,dest.z).forall(_.isFinite) then camera=dest
    updateJournal(dt)
    updateGameFeel(dt, frameStartCamera, moving, sprinting, crouching)
    updateEnvironmentalDamage(dt)
    updateHealthRegen(dt)
    updateWater(dt)
    updateWeather(dt)
    updateBubbles(dt)
    updateBlockParticles(dt)
    updateTorchEffects(dt)
    updateSandFalling()
    updateSandParticles(dt)
    processChunkWorkMainThread(0, chunkUploadBudget)
    flushDirtyChunks()
    sendPlayerPositionNetwork()
    handleMouseButtons()

  private def approachExp(current: Float, target: Float, sharpness: Float, dt: Float): Float =
    val k = (1.0 - math.exp((-sharpness * dt).toDouble)).toFloat
    current + (target - current) * k

  private def blockUnderPlayer: Block =
    // Probe the player's actual support footprint instead of one center point. A single
    // sample can resolve to Air when standing on an edge and was the main reason
    // footsteps randomly disappeared. Prefer the center, then four inset corners.
    val supportY = floor(camera.y - 1.67f).toInt
    val offsets = supportProbeOffsets
    var firstSolid = Block.Air
    var i = 0
    while i < offsets.length && firstSolid == Block.Air do
      val (ox, oz) = offsets(i)
      val b = activeBlockAt(floor(camera.x + ox).toInt, supportY, floor(camera.z + oz).toInt)
      if b.solid then firstSolid = b
      i += 1
    if firstSolid != Block.Air then firstSolid
    else
      // Handle tiny collision/rounding gaps on stepped terrain without sampling so far
      // down that a cave ceiling or unrelated block can become the footstep material.
      i = 0
      while i < offsets.length && firstSolid == Block.Air do
        val (ox, oz) = offsets(i)
        val b = activeBlockAt(floor(camera.x + ox).toInt, supportY - 1, floor(camera.z + oz).toInt)
        if b.solid then firstSolid = b
        i += 1
      firstSolid

  private def updateGameFeel(dt: Float, before: Vec3, moving: Boolean, sprinting: Boolean, crouching: Boolean): Unit =
    val dx = camera.x - before.x
    val dz = camera.z - before.z
    val horizontalDistance = math.sqrt((dx * dx + dz * dz).toDouble).toFloat
    val instantSpeed = if dt > 0.0001f then horizontalDistance / dt else 0f
    smoothedHorizontalSpeed = approachExp(smoothedHorizontalSpeed, instantSpeed.min(20f), 10f, dt)
    val submerged = waterSubmersion > 0.35f
    // `onGround` can be cleared for a frame while stepping up a voxel edge even when
    // the collision footprint is still supported. Use a cheap support probe as a
    // grace condition so footsteps/bob don't randomly vanish on stairs and rough land.
    // Flight suppresses the feet entirely: no bob, no roll into footfalls, no taps.
    val groundedForFeel = !flyEnabled && (onGround || (velocity.y <= 0.05f && hasGroundBelow(camera)))
    val groundedMotion = groundedForFeel && moving && horizontalDistance > 0.0001f && !submerged
    val bobTarget = if groundedMotion then (smoothedHorizontalSpeed / 5.5f).max(0f).min(1.15f) else 0f
    cameraBobAmount = approachExp(cameraBobAmount, bobTarget, if groundedMotion then 14f else 9f, dt)
    // Drive bob by distance, not frame rate. One gentle cycle per footfall keeps
    // motion readable without the old rapid seasick oscillation.
    if groundedMotion then cameraBobPhase += horizontalDistance * (if sprinting then 3.25f else 2.85f)
    val fovTarget = if submerged then 1.5f else if sprinting && moving && !crouching then 7.5f else 0f
    viewFovOffset = approachExp(viewFovOffset, fovTarget, 8.5f, dt)
    val strafe = (if down(GLFW_KEY_D) then 1f else 0f) - (if down(GLFW_KEY_A) then 1f else 0f)
    val rollTarget = if moving && !submerged then -strafe * (if sprinting then 0.38f else 0.24f) else 0f
    cameraRoll = approachExp(cameraRoll, rollTarget, 12f, dt)
    if groundedMotion then
      footstepTravel += horizontalDistance
      val stepDistance = if crouching then 1.90f else if sprinting then 1.30f else 1.58f
      if footstepTravel >= stepDistance then
        footstepTravel %= stepDistance
        playFootstepSound(blockUnderPlayer, sprinting)
    else if !onGround then footstepTravel = footstepTravel.min(0.6f)
    if submerged && !wasSubmergedForAudio then playSplashSound()
    wasSubmergedForAudio = submerged
    hurtOverlay = (hurtOverlay - dt * 2.8f).max(0f)
    selectedItemLabelTimer = (selectedItemLabelTimer - dt).max(0f)
    viewmodelKick = (viewmodelKick - dt * 5.4f).max(0f)
    val wanted = heldHotbarBlock
    if wanted != equippedBlock then
      equipProgress = (equipProgress - dt * 6.5f).max(0f)
      if equipProgress <= 0f then equippedBlock = wanted
    else equipProgress = (equipProgress + dt * 5.2f).min(1f)
    viewmodelYaw = (viewmodelYaw + (((yaw - viewmodelYaw) % 360f + 540f) % 360f - 180f) * (1f - exp(-dt * 16f).toFloat)) % 360f
    viewmodelPitch = approachExp(viewmodelPitch, pitch, 16f, dt)
    val obstruction=FirstPersonPose.obstruction(camera,yaw,pitch,getWorldBlock)
    viewmodelObstruction=approachExp(viewmodelObstruction,obstruction,if obstruction>viewmodelObstruction then 20f else 10f,dt)

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
      (screen == Screen.Paused || screen == Screen.Inventory || screen == Screen.Catalog || screen == Screen.FurnaceUI || screen == Screen.Journal ||
        (screen == Screen.Settings && (settingsReturnTo == Screen.Playing || settingsReturnTo == Screen.Paused)) ||
        (screen == Screen.Resources && resourcesReturnTo == Screen.Settings && (settingsReturnTo == Screen.Playing || settingsReturnTo == Screen.Paused)))

  private def updateMultiplayerBackgroundTick(dt: Float): Unit =
    if handleLostConnection() then return
    // Do NOT run local movement/physics here. This is only the live multiplayer heartbeat for UI screens.
    processNetworkMessages()
    updateWeather(dt)
    smoothRemotePlayers(dt)
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
        velocity = Vec3(0f,modManager.rules.get("jumpSpeed"),0f)
        onGround = false
      else velocity = Vec3(0f, (velocity.y - modManager.rules.get("gravity") * dt).max(-78f), 0f)
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
      velocity = Vec3(0f, (velocity.y - modManager.rules.get("gravity") * dt).max(-78f), 0f)
      if onGround && swimUp then velocity = Vec3(0f, 8.4f, 0f)
      if swimUp && !onGround then velocity = Vec3(0f, velocity.y.max(3f), 0f)

  private def sendSwing(): Unit =
    val now = glfwGetTime()
    if now - lastSwingSend >= 0.26 then
      lastSwingSend = now
      viewmodelKick = 1f
      if multiplayerMode then
        val message = "SWING|" + networkSafeName(playerName)
        if gameClient != null then gameClient.send(message)
        else if gameServer != null then gameServer.broadcast(message)

  private def handleMouseButtons(): Unit =
    val leftDown = glfwGetMouseButton(window, GLFW_MOUSE_BUTTON_LEFT) == GLFW_PRESS
    val rightDown = glfwGetMouseButton(window, GLFW_MOUSE_BUTTON_RIGHT) == GLFW_PRESS
    if leftDown && (!leftWasDown || breakingBlock != null) && !chatOpen then sendSwing()
    if rightDown && !rightWasDown && !chatOpen then sendSwing()
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
      if !block.solid && block != Block.Torch then resetBreaking()
      else
        val sameTarget = breakingBlock != null && breakingBlock == target
        if !sameTarget then
          breakingBlock = target
          breakingProgress = 0f
        breakingProgress += dt * modManager.rules.get("miningSpeed") / blockHardness(block)
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

  private def updateGrassGrowth(dt: Float): Unit =
    if multiplayerMode && gameServer == null then return
    grassGrowthTimer += dt
    if grassGrowthTimer < grassGrowthInterval || chunks.isEmpty then return
    val ticks = (grassGrowthTimer / grassGrowthInterval).toInt.min(4)
    grassGrowthTimer -= ticks * grassGrowthInterval
    val loaded = chunks.values.toArray
    if loaded.isEmpty then return
    val attempts = (loaded.length * 2).min(maxGrassGrowthAttemptsPerTick).max(4) * ticks
    var i = 0
    while i < attempts do
      val chunk = loaded(grassGrowthRng.nextInt(loaded.length))
      if !chunk.isDisposed then
        val lx = grassGrowthRng.nextInt(Terrain.chunkSize)
        val lz = grassGrowthRng.nextInt(Terrain.chunkSize)
        val y = grassGrowthRng.nextInt(Terrain.worldHeight - 1)
        val wx = chunk.cx * Terrain.chunkSize + lx
        val wz = chunk.cz * Terrain.chunkSize + lz
        if chunk.getBlock(lx, y, lz) == Block.Dirt && canGrowGrassAt(wx, y, wz) then
          setActiveBlock(wx, y, wz, Block.Grass)
          dirtyChunkAt(wx, wz)
          if multiplayerMode && gameServer != null then sendBlockNetwork(wx, y, wz, Block.Grass.ordinal.toByte)
      i += 1

  private def canGrowGrassAt(x: Int, y: Int, z: Int): Boolean =
    if y < 0 || y >= Terrain.worldHeight - 1 then false
    else
      val above = activeBlockAt(x, y + 1, z)
      if above.solid && !above.cutout && !above.translucent then false
      else
        var dx = -3
        var found = false
        while dx <= 3 && !found do
          var dz = -3
          while dz <= 3 && !found do
            if dx != 0 || dz != 0 then
              val dist = abs(dx) + abs(dz)
              if dist <= 4 then
                var dy = -1
                while dy <= 1 && !found do
                  val gy = y + dy
                  if gy >= 0 && gy < Terrain.worldHeight && activeBlockAt(x + dx, gy, z + dz) == Block.Grass then
                    val grassAbove = if gy + 1 < Terrain.worldHeight then activeBlockAt(x + dx, gy + 1, z + dz) else Block.Air
                    found = !grassAbove.solid || grassAbove.cutout || grassAbove.translucent
                  dy += 1
            dz += 1
          dx += 1
        found

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
    if !isClientOnlyMultiplayer && y >= 0 && y < Terrain.worldHeight && loadedChunkForBlock(x, z).isDefined && activeBlockAt(x, y, z) == Block.Water then
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
    block == Block.Air || block == Block.Water || block == Block.Torch

  private def setFlowWater(x: Int, y: Int, z: Int, level: Int): Unit =
    if y >= 0 && y < Terrain.worldHeight && loadedChunkForBlock(x, z).isDefined then
      if activeBlockAt(x, y, z) != Block.Water then setActiveBlock(x, y, z, Block.Water)
      val nextLevel = level.max(1).min(8)
      setWaterLevelAt(x, y, z, nextLevel)
      networkBlockOverrides((x, y, z)) = Block.Water
      networkWaterLevelOverrides((x, y, z)) = nextLevel.toByte
      networkTorchMounts.remove((x,y,z))
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
    if isClientOnlyMultiplayer then return
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

  private def blockHardness(block: Block): Float =
    CustomBlocks.definition(block).map(_.hardness).getOrElse(baseBlockHardness(block))

  private def baseBlockHardness(block: Block): Float = block match
    case Block.Leaves | Block.BirchLeaves | Block.PineLeaves | Block.AcaciaLeaves | Block.Snow => 0.18f
    case Block.Dirt | Block.Sand | Block.Grass => 0.42f
    case Block.Wood | Block.BirchWood | Block.PineWood | Block.AcaciaWood | Block.Planks | Block.BirchPlanks | Block.PinePlanks | Block.AcaciaPlanks => 0.75f
    case Block.Torch => 0.08f
    case Block.Glass => 0.35f
    case Block.Stone | Block.Coal | Block.Copper | Block.Clay | Block.IronOre => 1.15f
    case Block.GoldOre => 1.50f
    case Block.Brick | Block.Furnace | Block.FurnaceLit => 1.35f
    case Block.Diamond => 2.50f
    case Block.Bedrock => 9999f
    case _ => 0.5f

  private def damagePlayer(amount: Float, reason: String): Unit =
    if gameMode == GameMode.Survival && amount > 0f then
      val event=modManager.events.fireDamage(ModDamageEvent(modManager.api,playerName,reason,amount))
      if event.cancelled || !event.amount.isFinite then return
      val applied=event.amount.max(0f).min(1000f)
      val old = playerHealth
      playerHealth = (playerHealth - applied).max(0f)
      if old > playerHealth then
        timeSinceLastDamage = 0f
        hurtOverlay = 1f
        playHurtSound()
        val shown = (math.ceil(applied.toDouble * 2.0) / 2.0).toFloat
        addChatMessage(s"Took $shown damage${if reason.nonEmpty then s" from $reason" else ""}")
      if playerHealth <= 0f then
        modManager.events.fireDeath(ModPlayerEvent(modManager.api,playerName,reason))
        val spawn = findSpawn()
        camera = spawn
        velocity = Vec3(0f, 0f, 0f)
        onGround = false
        fallPeakY = camera.y
        playerHealth = maxPlayerHealth
        playerFood = 20f
        timeSinceLastDamage = healthRegenDelay
        addChatMessage("You woke up at spawn")
        modManager.events.fireSpawn(ModPlayerEvent(modManager.api,playerName,"respawn"))

  private def updateHealthRegen(dt: Float): Unit =
    if gameMode == GameMode.Survival && playerHealth > 0f && playerHealth < maxPlayerHealth then
      timeSinceLastDamage += dt
      if timeSinceLastDamage >= healthRegenDelay then playerHealth = (playerHealth + healthRegenRate * modManager.rules.get("regeneration") * dt).min(maxPlayerHealth)
    else if playerHealth >= maxPlayerHealth then
      playerHealth = maxPlayerHealth
      timeSinceLastDamage = healthRegenDelay

  private def handleLandingImpact(): Unit =
    if waterSubmersion < 0.25f then
      val distance = (fallPeakY - camera.y).max(0f)
      if distance > 0.65f then playLandSound(blockUnderPlayer, ((distance - 0.4f) / 3.2f).max(0.30f).min(1.25f))
      if gameMode == GameMode.Survival && distance > 4.2f then
        damagePlayer((distance - 3.2f) * 0.85f * modManager.rules.get("fallDamage"), "falling")

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
    markSettingsDirty()
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

  private def setActiveBlock(x: Int, y: Int, z: Int, block: Block, axis: LogAxis = LogAxis.Y, torchMount: TorchMount = TorchMount.Floor, refundTo: Option[String] = None): Unit =
    if y < 0 || y >= Terrain.worldHeight then return
    val cx = chunkCoordBlock(x)
    val cz = chunkCoordBlock(z)
    val lx = x - cx * Terrain.chunkSize
    val lz = z - cz * Terrain.chunkSize
    chunks.get((cx, cz)).foreach { chunk =>
      val oldBlock = chunk.getBlock(lx, y, lz)
      chunk.setBlock(lx, y, lz, block, axis, torchMount)
      if block == Block.Water then
        setWaterLevelAt(x, y, z, 8)
        markWaterActive(x, y, z)
      else
        setWaterLevelAt(x, y, z, 0)
        waterLevels.remove((x, y, z))
      if oldBlock != block then
        wakeWaterAround(x, y, z)
        val occlusionChanged = (oldBlock.solid && !oldBlock.translucent && !oldBlock.cutout) != (block.solid && !block.translucent && !block.cutout)
        if BlockLight.emission(oldBlock)>0 || BlockLight.emission(block)>0 || (occlusionChanged && hasLightNear(x,y,z)) then dirtyLightChunksAt(x,z)
        if gameClient == null then
          if oldBlock==Block.Torch && block==Block.Water then refundTorch(refundTo)
          removeUnsupportedTorchesAround(x,y,z,refundTo)
    }

  private def torchMountAt(x: Int, y: Int, z: Int): TorchMount =
    loadedChunkForBlock(x,z).map(_.getTorchMount(Math.floorMod(x,Terrain.chunkSize),y,Math.floorMod(z,Terrain.chunkSize)))
      .getOrElse(TorchMount.fromId(networkTorchMounts.getOrElse((x,y,z),0)))

  private def hasLightNear(x: Int, y: Int, z: Int): Boolean =
    val cx = chunkCoordBlock(x); val cz = chunkCoordBlock(z)
    (-1 to 1).exists(dx => (-1 to 1).exists(dz => chunks.get((cx+dx,cz+dz)).exists { chunk =>
      chunk.lightSourcesSnapshot.exists { source =>
        val distance = abs(chunk.baseX+source.x-x)+abs(source.y-y)+abs(chunk.baseZ+source.z-z)
        distance < source.level
      }
    }))

  private def refundTorch(recipient: Option[String]): Unit =
    if gameMode==GameMode.Survival then
      recipient.filterNot(_.equalsIgnoreCase(playerName)) match
        case Some(name) if gameServer!=null => gameServer.sendTo(name,s"GIVE|${networkSafeName(name)}|${Block.Torch.ordinal}|1")
        case _ => gainItem(Block.Torch,1)

  private def removeUnsupportedTorchesAround(x: Int, y: Int, z: Int, refundTo: Option[String]): Unit =
    TorchMount.values.foreach { mount =>
      val tx=x-mount.support._1; val ty=y-mount.support._2; val tz=z-mount.support._3
      if loadedChunkForBlock(tx,tz).nonEmpty && activeBlockAt(tx,ty,tz)==Block.Torch && torchMountAt(tx,ty,tz)==mount && !mount.supported(tx,ty,tz,activeBlockAt) then
        setActiveBlock(tx,ty,tz,Block.Air)
        spawnBlockBreakParticles(tx,ty,tz,Block.Torch)
        refundTorch(refundTo)
        sendBlockNetwork(tx,ty,tz,Block.Air.id)
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
              terrainGen = createTerrainGenerator(worldSeed)
              terrainHeightCache.clear()
              pendingNetworkBlocks.clear()
              networkBlockOverrides.clear()
              networkWaterLevelOverrides.clear()
              pendingNetworkLogAxes.clear()
              networkLogAxesOverrides.clear()
              networkTorchMounts.clear()
              waterLevels.clear()
              waterFlowQueue.clear()
              waterFlowQueued.clear()
              clearLoadedChunks(saveFirst = false)
            camera = Vec3(sx, sy, sz)
            fallPeakY = camera.y
            velocity = Vec3(0f, 0f, 0f); onGround = false
            syncChunks()
            addChatMessage("Joined host world")
        else if line.startsWith("ENV|") then
          if gameServer == null then
            val env = BlockboxNet.parseEnvironment(BlockboxNet.packet(line))
            val error = env.time.toDouble - worldClock
            worldClock = if !environmentReceived || abs(error) > 2.0 then env.time.toDouble else worldClock + error * 0.35
            timeOverride = if env.timeOverride < 0f then None else Some(env.timeOverride)
            rainStrength = env.rain; wetness = env.wetness; stormCloudCover = env.cloudCover
            weatherWind = env.wind
            lightningFlash = env.lightning
            environmentReceived = true
        else if line.startsWith("RULES|") && isClientOnlyMultiplayer then
          modManager.rules.receive(line.stripPrefix("RULES|"))
        else if line.startsWith("VANILLA_USE|") && isClientOnlyMultiplayer then
          if parts.length==4 then
            val target=(parts(1).toInt,parts(2).toInt,parts(3).toInt)
            if raycast().exists(_.block==target) then placeTargetBlock(skipModInteraction=true)
        else if line.startsWith("INTERACT|") && gameServer!=null then
          if parts.length==5 then
            val x=parts(2).toInt; val y=parts(3).toInt; val z=parts(4).toInt
            val event=modManager.events.fireBlockInteract(ModInteractionEvent(modManager.api,parts(1),x,y,z,activeBlockAt(x,y,z)))
            if !event.cancelled && !event.handled then gameServer.sendTo(parts(1),s"VANILLA_USE|$x|$y|$z")
        else if line.startsWith("SWING|") then
          if parts.length == 2 then remotePlayers.get(networkSafeName(parts(1))).foreach(_.swing = 1f)
        else if line.startsWith("EDIT|") && gameServer != null then
          if parts.length >= 7 then
            val sender = networkSafeName(parts(1))
            val edit = BlockboxNet.parseBlock(BlockboxNet.packet("BLOC|"+parts.drop(2).mkString("|")),Terrain.worldHeight)
            val block = Block.fromId(edit.blockId.toByte)
            val old = activeBlockAt(edit.x,edit.y,edit.z)
            val loaded = loadedChunkForBlock(edit.x,edit.z).nonEmpty
            val canBreak = block==Block.Air && old!=Block.Air && old!=Block.Bedrock
            val mount = TorchMount.fromId(edit.torchMount)
            val canPlace = block!=Block.Air && canPlaceBlockAt(edit.x,edit.y,edit.z,block) && (block!=Block.Torch || mount.supported(edit.x,edit.y,edit.z,activeBlockAt))
            val broken=if loaded && canBreak then Some(modManager.fireBlockBreak(ModBlockBreakEvent(modManager.api,sender,edit.x,edit.y,edit.z,old))) else None
            val placed=if loaded && canPlace then Some(modManager.fireBlockPlace(ModBlockPlaceEvent(modManager.api,sender,edit.x,edit.y,edit.z,block))) else None
            val finalBlock=placed.map(_.block).getOrElse(block)
            val allowBreak=broken.exists(!_.cancelled)
            val allowPlace=placed.exists(e => !e.cancelled && e.block!=null && CustomBlocks.registered(e.block) && canPlaceBlockAt(edit.x,edit.y,edit.z,e.block) && (e.block!=Block.Torch || mount.supported(edit.x,edit.y,edit.z,activeBlockAt)))
            val accepted=loaded && (allowBreak || allowPlace)
            if accepted then
              setActiveBlock(edit.x,edit.y,edit.z,finalBlock,LogAxis.fromId(edit.logAxis),mount,Some(sender))
              if finalBlock==Block.Water then setWaterLevelAt(edit.x,edit.y,edit.z,edit.waterLevel.max(1))
              dirtyChunkAt(edit.x,edit.z)
              gameServer.broadcastBlockChange(edit.x,edit.y,edit.z,finalBlock.id,if finalBlock==Block.Water then edit.waterLevel else 0,edit.logAxis,if finalBlock==Block.Torch then mount.id else 0)
            else
              val axis=loadedChunkForBlock(edit.x,edit.z).map(c => c.getLogAxis(Math.floorMod(edit.x,16),edit.y,Math.floorMod(edit.z,16)).id).getOrElse(0)
              gameServer.sendTo(sender,BlockboxNet.block(edit.x,edit.y,edit.z,old.ordinal,if old==Block.Water then waterLevelAt(edit.x,edit.y,edit.z) else 0,axis,torchMountAt(edit.x,edit.y,edit.z).id))
            val (loot,count)=if accepted && broken.exists(_.dropItem) then modManager.drops.getOrElse(old,(old,1)) else (Block.Air,0)
            gameServer.sendTo(sender,s"EDIT_RESULT|${edit.x}|${edit.y}|${edit.z}|${if accepted then 1 else 0}|${loot.ordinal}|$count")
        else if line.startsWith("EDIT_RESULT|") && isClientOnlyMultiplayer then
          if parts.length==7 then
            val key=(parts(1).toInt,parts(2).toInt,parts(3).toInt)
            val lootId=parts(5).toInt; val count=parts(6).toInt
            require(lootId>=0 && lootId<Block.values.length && count>=0 && count<=64,"Invalid edit result")
            pendingPlayerEdits.remove(key).foreach { case (spent,amount,drops) =>
              if parts(4)=="0" && amount>0 then gainItem(spent,amount)
              else if parts(4)=="1" && drops && count>0 then gainItem(Block.fromOrdinal(lootId),count)
            }
        else if line.startsWith("BLOC|") then
          if parts.length >= 5 then
            val packet = BlockboxNet.parseBlock(BlockboxNet.packet(line),Terrain.worldHeight)
            if packet.blockId >= Block.values.length then throw IllegalArgumentException("Unknown block id")
            val x = packet.x; val y = packet.y; val z = packet.z
            val blockId = packet.blockId.toByte
            val block = Block.fromId(blockId)
            val waterLevel =
              if block == Block.Water && parts.length >= 6 then
                try parts(5).toInt.max(1).min(8) catch case _: Exception => 8
              else if block == Block.Water then 8 else 0
            val logAxis =
              if Block.isLog(block) && parts.length >= 7 then
                try parts(6).toInt.max(0).min(2) catch case _: Exception => 0
              else 0
            networkBlockOverrides((x, y, z)) = block
            if block == Block.Water then networkWaterLevelOverrides((x, y, z)) = waterLevel.toByte
            else networkWaterLevelOverrides.remove((x, y, z))
            if Block.isLog(block) && logAxis != 0 then networkLogAxesOverrides((x, y, z)) = logAxis
            else networkLogAxesOverrides.remove((x, y, z))
            val mount = if block==Block.Torch then packet.torchMount else 0
            if block==Block.Torch then networkTorchMounts((x,y,z))=mount else networkTorchMounts.remove((x,y,z))
            val cx = chunkCoordBlock(x); val cz = chunkCoordBlock(z)
            if chunks.contains((cx, cz)) then
              setActiveBlock(x, y, z, block, LogAxis.fromId(logAxis),TorchMount.fromId(mount))
              if block == Block.Water then setWaterLevelAt(x, y, z, waterLevel)
              dirtyChunkAt(x, z)
            else
              pendingNetworkBlocks((x, y, z)) = block
              if Block.isLog(block) && logAxis != 0 then pendingNetworkLogAxes((x, y, z)) = logAxis
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
              val nextPos = Vec3(x, y, z)
              remotePlayers.get(name) match
                case Some(rp) =>
                  // Interpolate ordinary packets, but snap genuine teleports/large
                  // corrections so smoothing can never leave a player ghosting across
                  // half the map.
                  if (nextPos - rp.pos).lengthSquared > 144f then rp.pos = nextPos
                  rp.targetPos = nextPos
                  rp.targetYaw = pyaw
                  rp.targetPitch = ppitch
                  rp.lastSeen = glfwGetTime()
                  rp.colorId = colorId
                case None =>
                  remotePlayers(name) = RemotePlayer(name, nextPos, nextPos, pyaw, pyaw, ppitch, ppitch, glfwGetTime(), colorId)
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
                val nextPos = Vec3(x, y, z)
                rp.pos = nextPos
                rp.targetPos = nextPos
                rp.lastSeen = glfwGetTime()
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
            if gameServer != null && isOppedName(name) then
              setOppedName(name,false,announce=false)
              gameServer.broadcast("OP|"+name+"|0")
            if name != playerName then addChatMessage(s"$name left the game")
      catch
        case e: Exception => System.err.println(s"Ignored bad network packet '$line': ${Option(e.getMessage).getOrElse(e.getClass.getSimpleName)}")

    if gameClient != null then
      var processed = 0
      var msg = gameClient.pollMessage()
      while msg.isDefined && processed < 4096 do
        handleNetworkLine(msg.get)
        processed += 1
        msg = if processed < 4096 then gameClient.pollMessage() else None

    if gameServer != null then
      var processed = 0
      var msg = gameServer.pollMessage()
      while msg.isDefined && processed < 1024 do
        handleNetworkLine(msg.get)
        processed += 1
        msg = if processed < 1024 then gameServer.pollMessage() else None

  private def angleDeltaDegrees(from: Float, to: Float): Float =
    var d = (to - from) % 360f
    if d > 180f then d -= 360f
    else if d < -180f then d += 360f
    d

  private def smoothRemotePlayers(dt: Float): Unit =
    if remotePlayers.isEmpty then return
    val alpha = (1.0 - exp(-16.0 * dt.max(0f).min(0.1f))).toFloat.max(0f).min(1f)
    remotePlayers.values.foreach { rp =>
      val before = rp.pos
      val delta = rp.targetPos - rp.pos
      if delta.lengthSquared > 196f then rp.pos = rp.targetPos
      else rp.pos = rp.pos + delta * alpha
      rp.yaw += angleDeltaDegrees(rp.yaw, rp.targetYaw) * alpha
      rp.pitch += (rp.targetPitch - rp.pitch) * alpha
      val motion = rp.pos - before
      val distance = sqrt(motion.x * motion.x + motion.z * motion.z).toFloat
      val speed = distance / dt.max(0.001f)
      rp.stride = approachExp(rp.stride, (speed / 4.5f).min(1f), 12f, dt)
      rp.gait = (rp.gait + distance.min(1f) * 2.6f) % (Pi.toFloat * 2f)
      var desiredBody = rp.yaw
      if speed > 0.15f then
        var motionYaw = toDegrees(atan2(motion.x.toDouble, -motion.z.toDouble)).toFloat
        if abs(angleDeltaDegrees(rp.yaw, motionYaw)) > 90f then motionYaw += 180f
        desiredBody += angleDeltaDegrees(rp.yaw, motionYaw).max(-35f).min(35f)
      rp.bodyYaw += angleDeltaDegrees(rp.bodyYaw, desiredBody) * (1f - exp(-8f * dt).toFloat)
      val headOffset = angleDeltaDegrees(rp.bodyYaw, rp.yaw)
      if abs(headOffset) > 65f then rp.bodyYaw = rp.yaw - headOffset.max(-65f).min(65f)
      rp.swing = (rp.swing - dt * 3.4f).max(0f)
    }

  private def sendPlayerPositionNetwork(): Unit =
    if multiplayerMode then
      val now = glfwGetTime()
      val moved = (camera - lastNetworkSentPos).lengthSquared > 0.0004f
      val turned = abs(angleDeltaDegrees(lastNetworkSentYaw, yaw)) > 0.22f || abs(lastNetworkSentPitch - pitch) > 0.22f
      val keepaliveDue = now - lastPosKeepalive >= 0.75
      // Moving players update at up to 20 Hz for responsiveness. Stationary players
      // stop flooding the TCP stream and only send a small keepalive periodically.
      if now - lastPosSend >= 0.05 && (moved || turned || keepaliveDue) then
        lastPosSend = now
        if keepaliveDue then lastPosKeepalive = now
        lastNetworkSentPos = camera
        lastNetworkSentYaw = yaw
        lastNetworkSentPitch = pitch
        val msg = "POS|" + networkEscape(playerName) + "|" + networkFloat(camera.x) + "|" + networkFloat(camera.y) + "|" + networkFloat(camera.z) + "|" + networkFloat(yaw) + "|" + networkFloat(pitch) + "|" + localColorId.toString
        if gameClient != null && gameClient.isConnected then gameClient.send(msg)
        else if gameServer != null then gameServer.broadcast(msg)
      if gameServer != null && now - lastMultiplayerHeartbeat >= 2.0 then
        lastMultiplayerHeartbeat = now
        gameServer.broadcastPlayerList()
      if gameServer != null && now - lastEnvironmentSend >= 0.5 then
        lastEnvironmentSend = now
        publishedEnvironment = environmentMessage
        gameServer.broadcast(publishedEnvironment)

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
    glDisable(GL_MULTISAMPLE)
    glPolygonMode(GL_FRONT_AND_BACK, GL_FILL)
    glDisable(GL_FOG)
    glDisable(GL_TEXTURE_2D)
    glDisable(GL_CULL_FACE)
    // Entity lighting must not leak into the next world pass. With it left on,
    // some drivers feed the shader a black vertex colour and every pack goes black.
    glDisable(GL_LIGHTING)
    glDisable(GL_COLOR_MATERIAL)
    glDepthMask(true)
    glEnable(GL_BLEND)
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA)
    val (clearR, clearG, clearB) = clearColor
    glClearColor(clearR, clearG, clearB, 1f)
    glViewport(0, 0, framebufferWidth, framebufferHeight)
    val worldScreen = screen == Screen.Playing || screen == Screen.Paused || screen == Screen.Inventory || screen == Screen.Catalog || screen == Screen.FurnaceUI || screen == Screen.Journal
    if shadersEnabled && shaderPipeline != null then shaderPipeline.reloadIfChanged(glfwGetTime())
    if foveaPass != null then foveaPass.enabled = foveatedRendering
    val shaderScene = worldScreen && shadersEnabled && shaderPipeline != null && shaderPipeline.prepareFrame(framebufferWidth, framebufferHeight)
    if shaderScene && shaderPipeline != null then shaderPipeline.clearPreparedFrame(clearR, clearG, clearB)
    else glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT)
    def finishWorldPost(): Unit =
      if shaderScene && shaderPipeline != null then shaderPipeline.finishFrame(framebufferWidth, framebufferHeight, gameTime)
    screen match
      case Screen.Playing =>
        renderWorld()
        if isUnderwater then renderUnderwaterOverlay()
        finishWorldPost()
        renderPlayerNameTags()
        renderFirstPersonHeldBlock()
        renderHurtOverlay()
        renderHud()
        renderHotbar()
        renderSelectedItemName()
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
        finishWorldPost()
        renderPlayerNameTags()
        if multiplayerMode then
          renderHud()
          renderChat()
        renderPauseMenu()
      case Screen.Inventory => renderWorld(); finishWorldPost(); renderSurvivalInventory()
      case Screen.Catalog => renderWorld(); finishWorldPost(); renderCatalog()
      case Screen.FurnaceUI => renderWorld(); finishWorldPost(); renderFurnaceUI()
      case Screen.Journal => renderWorld(); finishWorldPost(); renderJournal()
      case Screen.JoinGame => renderJoinGame()
      case Screen.HostGame => renderHostGame()

  private def renderWorld(): Unit =
    if !centerChunkReady then
      forceCameraChunkRing(0)
      processChunkWorkMainThread(0, 8)
    // Both render paths share the same optional resolution scale and resolve.
    val useFovea = foveatedRendering && foveaPass != null && foveaPass.supportsFbos && !(shadersEnabled && shaderPipeline != null && shaderPipeline.isSceneBound) && foveaPass.beginScene(framebufferWidth, framebufferHeight)
    resetGlArraysAndBuffers()
    applyWorldAntiAliasingState()
    glPolygonMode(GL_FRONT_AND_BACK, if wireframeMode then GL_LINE else GL_FILL)
    glDepthMask(true)
    glEnable(GL_DEPTH_TEST)
    glDisable(GL_CULL_FACE)
    glEnable(GL_FOG)
    val (fogR, fogG, fogB) = clearColor
    val isUnder = isUnderwater
    val visibleDist = renderDistanceBlocks.toFloat
    val fogStart = if isUnder then 2f else visibleDist * (0.65f * 1.6f / fogDensity).max(0.25f).min(0.85f)
    val fogEnd = if isUnder then 18f else visibleDist * 0.95f
    applyTerrainFog(isUnder)
    setFogColor(fogR, fogG, fogB)
    glMatrixMode(GL_PROJECTION); glLoadIdentity()
    val aspect = framebufferWidth.toDouble / framebufferHeight.toDouble
    // Keep the near plane away from zero so the depth buffer has useful
    // precision. Tiny near values caused early z-fighting at distance.
    val farBlocks = if cloudsEnabled then renderDistanceBlocks.max(cloudRenderDistanceBlocks) else renderDistanceBlocks
    val near = 0.25
    // Render distance is horizontal. Flight above the world must not push the
    // terrain past the far plane, including when clouds are disabled.
    val verticalRange = abs(camera.y).max(abs(camera.y - Terrain.worldHeight))
    val far = sqrt(farBlocks.toDouble * farBlocks + verticalRange.toDouble * verticalRange) + Terrain.chunkSize * 2
    val dynamicFov = if screen == Screen.Playing then viewFovOffset else 0f
    val top = tan(toRadians((fov + dynamicFov).max(45f).min(110f)) / 2.0) * near
    glFrustum(-top * aspect, top * aspect, -top, top, near, far)
    glMatrixMode(GL_MODELVIEW); glLoadIdentity()
    val bobX = sin(cameraBobPhase).toFloat * 0.012f * cameraBobAmount
    val bobY = (0.5f - 0.5f * cos(cameraBobPhase * 2f).toFloat) * 0.018f * cameraBobAmount
    val bobRoll = sin(cameraBobPhase).toFloat * 0.12f * cameraBobAmount
    glTranslatef(bobX, -bobY, 0f)
    glRotatef(cameraRoll + bobRoll, 0f, 0f, 1f)
    glRotatef(pitch, 1f, 0f, 0f)
    glRotatef(yaw, 0f, 1f, 0f)
    // X/Z camera-relative rendering: chunk VBOs are local and translated by
    // (chunkBase - camera). Only vertical camera motion remains in the view matrix.
    glTranslatef(0f, -camera.y, 0f)
    if !isUnder then renderSky()
    renderBubbles()
    renderFallingSand()
    restoreChunkRenderState(isUnder, fogR, fogG, fogB)
    renderChunkScratch.clear()
    cullMatrixBuffer.clear(); glGetFloatv(GL_PROJECTION_MATRIX, cullMatrixBuffer)
    cullMatrixBuffer.get(cullProjection)
    cullMatrixBuffer.clear(); glGetFloatv(GL_MODELVIEW_MATRIX, cullMatrixBuffer)
    cullMatrixBuffer.get(cullView)
    viewFrustum.update(cullProjection, cullView)
    val chunkIter = chunks.valuesIterator
    while chunkIter.hasNext do
      val c = chunkIter.next()
      if chunkVisibleForCamera(c, visibleDist) then renderChunkScratch += c
    val chunkList = renderChunkScratch
    shadowChunkScratch.clear()
    val shadowCasterRadius = if shadersEnabled && shaderPipeline != null then shaderPipeline.shadowDistanceLimit(visibleDist + Terrain.chunkSize * 1.5f) else 0f
    val shadowCasterRadiusSq = shadowCasterRadius * shadowCasterRadius
    val shadowIter = chunks.valuesIterator
    while shadowIter.hasNext do
      val c = shadowIter.next()
      val dx = (c.cx * Terrain.chunkSize + Terrain.chunkSize * 0.5f) - camera.x
      val dz = (c.cz * Terrain.chunkSize + Terrain.chunkSize * 0.5f) - camera.z
      if dx * dx + dz * dz <= shadowCasterRadiusSq then shadowChunkScratch += c
    // Batch opaque pass - bind texture + client states once for all chunks
    glDisable(GL_BLEND)
    glDepthMask(true)
    glEnable(GL_DEPTH_TEST)
    glEnable(GL_TEXTURE_2D); activeAtlas.bind()
    glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_MODULATE)
    glEnableClientState(GL_VERTEX_ARRAY); glEnableClientState(GL_COLOR_ARRAY); glEnableClientState(GL_NORMAL_ARRAY); glEnableClientState(GL_TEXTURE_COORD_ARRAY)
    // Optional Iris-style directional shadow map. It reuses the already-built chunk
    // VBOs and atlas, then restores the scene framebuffer/matrices before the normal
    // G-buffer draw. This keeps shadows GPU-side without rebuilding terrain geometry.
    val sunDirection = BlockboxSky.sunDirection(dayPhase)
    val moonDirection = BlockboxSky.moonDirection(dayPhase)
    updateFrameLight()
    if shaderPipeline != null then
      shaderPipeline.setNightShading(worldDarkening)
      shaderPipeline.setLightEnvironment(frameLight)
    val shadowActive =
      if shadersEnabled && shaderPipeline != null then
        shaderPipeline.beginShadow((camera.x, camera.y, camera.z), sunDirection, moonDirection, daylightFactor, shadowCasterRadius, gameTime, localRain, localWetness)
      else false
    if shadowActive then
      shaderPipeline.setShadowPass(0)
      shadowChunkScratch.foreach(_.drawOpaque(camera))
      glEnable(GL_ALPHA_TEST); glAlphaFunc(GL_GREATER, 0.5f)
      shaderPipeline.setShadowPass(1)
      shadowChunkScratch.foreach(_.drawCutout(camera))
      glDisable(GL_ALPHA_TEST)
      shaderPipeline.endShadow()
      // endShadow restores texture unit 0 and the scene target; make the intended
      // world state explicit because third-party shadow shaders are allowed to alter it.
      glEnable(GL_TEXTURE_2D); activeAtlas.bind()
      glEnable(GL_DEPTH_TEST); glDepthMask(true); glDisable(GL_BLEND)
    if shaderPipeline != null then
      shaderPipeline.setCelestialTime(timeOverride.getOrElse(gameTime))
      shaderPipeline.setAntialiasing(antiAliasing)
      shaderPipeline.setNightShading(worldDarkening)
    val shaderActive =
      if shadersEnabled && shaderPipeline != null then
        shaderPipeline.beginWorld(fogR, fogG, fogB, fogStart, fogEnd, daylightFactor, gameTime, sunDirection, moonDirection, (camera.x, camera.y, camera.z), near.toFloat, far.toFloat, localRain, localWetness, isUnderwater)
      else false
    if shaderActive then
      glDisable(GL_FOG)
      shaderPipeline.enableGBufferWrites()
      shaderPipeline.setPass(0)
    else if shaderPipeline!=null then shaderPipeline.beginCompatibilityLighting(daylightFactor)
    chunkList.foreach(_.drawOpaque(camera))
    // Batch cutout pass
    glEnable(GL_ALPHA_TEST); glAlphaFunc(GL_GREATER, 0.5f)
    if shaderActive then shaderPipeline.setPass(1)
    chunkList.foreach(_.drawCutout(camera))
    glDisable(GL_ALPHA_TEST)
    // Batch translucent + water pass
    glEnable(GL_BLEND)
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA)
    glDepthMask(false)
    if shaderActive then
      // Translucent shaders are color-only; preserve opaque normals/material IDs
      // underneath instead of leaving unwritten MRT outputs undefined on drivers.
      shaderPipeline.enableColorOnlyWrites()
      shaderPipeline.setPass(2)
    translucentChunkScratch.clear()
    var ti = 0
    while ti < chunkList.length do
      val c = chunkList(ti)
      if c.hasTranslucentMesh then translucentChunkScratch += c
      ti += 1
    translucentChunkScratch.sortInPlaceBy { c =>
      val dx = (c.cx * Terrain.chunkSize + Terrain.chunkSize * 0.5f) - camera.x
      val dz = (c.cz * Terrain.chunkSize + Terrain.chunkSize * 0.5f) - camera.z
      -(dx * dx + dz * dz)
    }
    translucentChunkScratch.foreach(_.drawTranslucent(camera))
    // Transparent water keeps the upgraded rivers/oceans readable while internal faces
    // remain culled in the mesh itself. Draw it back-to-front by chunk and keep depth
    // writes off so underwater terrain and shorelines remain visible through the volume.
    glEnable(GL_BLEND)
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA)
    glDepthMask(false)
    if shaderActive then
      shaderPipeline.enableGBufferWrites()
      shaderPipeline.setPass(3)
    waterChunkScratch.clear()
    var wi = 0
    while wi < chunkList.length do
      val c = chunkList(wi)
      if c.hasWaterMesh then waterChunkScratch += c
      wi += 1
    waterChunkScratch.sortInPlaceBy { c =>
      val dx = (c.cx * Terrain.chunkSize + Terrain.chunkSize * 0.5f) - camera.x
      val dz = (c.cz * Terrain.chunkSize + Terrain.chunkSize * 0.5f) - camera.z
      -(dx * dx + dz * dz)
    }
    waterChunkScratch.foreach(_.drawWater(camera))
    if shaderActive then
      shaderPipeline.end()
      shaderPipeline.enableColorOnlyWrites()
    else if shaderPipeline!=null then shaderPipeline.endCompatibilityLighting()
    glEnable(GL_FOG)
    glDepthMask(true)
    glDisable(GL_BLEND)
    // Chunk VBOs expose a second client texture-coordinate stream for Minecraft-style
    // lightmap UVs; disable both units before returning to compatibility/immediate mode.
    glClientActiveTexture(GL_TEXTURE1); glDisableClientState(GL_TEXTURE_COORD_ARRAY)
    glClientActiveTexture(GL_TEXTURE0); glDisableClientState(GL_TEXTURE_COORD_ARRAY)
    glDisableClientState(GL_NORMAL_ARRAY); glDisableClientState(GL_COLOR_ARRAY); glDisableClientState(GL_VERTEX_ARRAY)
    glBindBuffer(GL_ARRAY_BUFFER, 0); glBindTexture(GL_TEXTURE_2D, 0)
    if cloudsEnabled && !isUnder then
      BlockboxSky.renderClouds(camera, gameTime, cloudRenderDistanceBlocks, dayPhase, daylightFactor, stormCloudCover, localLightning, worldSeed)
    BlockboxWeather.render(camera,gameTime,daylightFactor,weatherCells,weatherStrikes,cloudRenderDistanceBlocks)
    applyTerrainFog(isUnder)
    renderWeatherRain()
    renderBlockParticles()
    renderTorchEffects()
    renderRemotePlayers3D()
    if modManager.events.hasListeners("worldRender") then
      val oldProgram=glGetInteger(org.lwjgl.opengl.GL20.GL_CURRENT_PROGRAM)
      glPushAttrib(GL_ALL_ATTRIB_BITS); glPushClientAttrib(GL_CLIENT_ALL_ATTRIB_BITS)
      glMatrixMode(GL_PROJECTION); glPushMatrix()
      glMatrixMode(GL_MODELVIEW); glPushMatrix()
      try modManager.events.fireWorldRender(ModWorldRenderEvent(modManager.api,modManager.render,lastFrameTime.toFloat))
      finally
        glMatrixMode(GL_MODELVIEW); glPopMatrix()
        glMatrixMode(GL_PROJECTION); glPopMatrix(); glMatrixMode(GL_MODELVIEW)
        glPopClientAttrib(); glPopAttrib(); org.lwjgl.opengl.GL20.glUseProgram(oldProgram)
    renderTargetOutline()
    if debugMode then renderRayLine()
    if showChunkBorders then renderChunkBorders()
    renderWorldDarkness()
    glDisable(GL_MULTISAMPLE)
    glPolygonMode(GL_FRONT_AND_BACK, GL_FILL)
    glDepthMask(true)
    glDisable(GL_FOG)
    if useFovea then foveaPass.endScene()

  private def chunkVisibleForCamera(c: Chunk, visibleDistance: Float): Boolean =
    val minX = c.baseX - camera.x; val minZ = c.baseZ - camera.z
    val maxX = minX + Terrain.chunkSize; val maxZ = minZ + Terrain.chunkSize
    val dx = 0f.max(minX).max(-maxX)
    val dz = 0f.max(minZ).max(-maxZ)
    dx * dx + dz * dz <= visibleDistance * visibleDistance &&
      viewFrustum.intersects(minX - 1f, -1f, minZ - 1f, maxX + 1f, Terrain.worldHeight + 1f, maxZ + 1f)

  private def renderWorldDarkness(): Unit =
    // Terrain shades night per surface using block light. A fullscreen veil would
    // incorrectly dim flames and the surfaces those flames have illuminated.
    if !worldDarkening || shaderPipeline!=null then return
    val darkness = nightDarkness
    if darkness > 0.01f then
      resetGlArraysAndBuffers()
      setupOrtho()
      glDisable(GL_DEPTH_TEST)
      glDisable(GL_FOG)
      glDisable(GL_TEXTURE_2D)
      glDepthMask(false)
      glEnable(GL_BLEND)
      glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA)
      rect(0, 0, framebufferWidth, framebufferHeight, 0.01f, 0.015f, 0.035f, darkness)
      glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA)
      glDepthMask(true)

  private def restoreChunkRenderState(isUnder: Boolean, fogR: Float, fogG: Float, fogB: Float): Unit =
    // renderSky(), particles, name tags, and UI all mutate global OpenGL state.
    // The world chunk pass must explicitly restore depth writes/testing or random translucent
    // quads / water / player boxes can draw as big black sheets and terrain can appear to vanish.
    resetGlArraysAndBuffers()
    glPolygonMode(GL_FRONT_AND_BACK, if wireframeMode then GL_LINE else GL_FILL)
    if shadersEnabled && shaderPipeline != null && shaderPipeline.isSceneBound then shaderPipeline.restoreSceneViewport()
    else if foveatedRendering && foveaPass != null && foveaPass.targetWidth > 0 then glViewport(0, 0, foveaPass.targetWidth, foveaPass.targetHeight)
    else glViewport(0, 0, framebufferWidth, framebufferHeight)
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
    applyTerrainFog(isUnder)
    setFogColor(fogR, fogG, fogB)
    glColor4f(1f, 1f, 1f, 1f)

  private def applyTerrainFog(underwater: Boolean): Unit =
    val distance = renderDistanceBlocks.toFloat
    val shader = shadersEnabled && shaderPipeline!=null && shaderPipeline.available
    // Compatibility fog uses eye-space depth. Compensate for the downward
    // vertical view component so flight cannot fog the entire ground to sky color.
    val lift = if underwater || shader then 0f else (camera.y-Terrain.seaLevel).max(0f)*sin(toRadians(pitch)).toFloat.max(0f)
    glFogi(GL_FOG_MODE,GL_LINEAR)
    glFogf(GL_FOG_START,if underwater then 2f else distance*(0.65f*1.6f/fogDensity).max(0.25f).min(0.85f)+lift)
    glFogf(GL_FOG_END,if underwater then 18f else distance*0.95f+lift)

  private def renderRemotePlayers3D(): Unit =
    if remotePlayers.isEmpty then return
    resetGlArraysAndBuffers()
    val now = glfwGetTime()
    glDisable(GL_TEXTURE_2D); glEnable(GL_FOG); glDisable(GL_BLEND)
    glDepthMask(true); glEnable(GL_DEPTH_TEST)
    applyTerrainFog(isUnderwater)
    BlockboxPlayerModel.beginLighting(viewSpace = false)
    remotePlayers.valuesIterator.foreach { rp =>
      if now - rp.lastSeen <= 8.0 then
        BlockboxPlayerModel.render(rp, camera, colorForId(rp.colorId), lightColorAt(rp.pos.x, rp.pos.y - 0.6f, rp.pos.z), gameTime)
    }
    BlockboxPlayerModel.endLighting()

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

  private def updateJournal(dt: Float): Unit =
    journalToastTime = (journalToastTime-dt).max(0f)
    journalTimer -= dt
    if journalTimer <= 0f then
      journalTimer = 0.5f
      val x = floor(camera.x).toInt; val z = floor(camera.z).toInt
      val surface = terrainGen.heightAt(x,z)
      val biome = terrainGen.biomeAt(x,z,surface)
      // Underground travel does not discover biomes through the ceiling.
      val biomeId = if camera.y >= surface-4 then biome.ordinal else -1
      fieldJournal.observe(biomeId,(surface-camera.y).toInt.max(0),gameMode==GameMode.Survival,totalItemCount)
      if journalToastTime <= 0f then fieldJournal.nextAnnouncement().foreach { title => journalToast = title; journalToastTime = 4f }

  private def journalPanel: UiRect =
    val w = (680f*uiScale).min(framebufferWidth-24f); val h = (496f*uiScale).min(framebufferHeight-24f)
    UiRect((framebufferWidth-w)/2f,(framebufferHeight-h)/2f,w,h)

  private def renderJournal(): Unit =
    setupOrtho(); dimBackground()
    val s = uiScale; val p = journalPanel
    drawPanel(p.x,p.y,p.w,p.h)
    centeredText(framebufferWidth/2f,p.y+25f*s,"Field journal",0.96f,0.91f,0.72f,2.2f*s)
    centeredText(framebufferWidth/2f,p.y+57f*s,"A few trails to follow. Make the world your own.",0.64f,0.75f,0.70f,1f*s)
    val rowH = ((p.h-146f*s)/6f).min(54f*s)
    fieldJournal.entries.zipWithIndex.foreach { case (entry,i) =>
      val y = p.y+90f*s+i*rowH
      rect(p.x+24f*s,y,p.w-48f*s,rowH-5f*s,0.11f,0.16f,0.16f,0.95f)
      val label = if entry.completed then "DONE" else s"${entry.progress.min(entry.target)}/${entry.target}"
      renderTextShadow(p.x+36f*s,y+7f*s,entry.title,0.94f,0.93f,0.82f,1.2f*s)
      renderTextShadowFit(p.x+36f*s,y+27f*s,entry.hint,0.61f,0.74f,0.70f,0.95f*s,p.w-72f*s)
      val tw = textWidth(label,1.1f*s)
      renderTextShadow(p.x+p.w-38f*s-tw,y+8f*s,label,0.66f,0.86f,0.58f,1.1f*s)
    }
    drawButton(p.x+p.w/2f-100f*s,p.y+p.h-50f*s,200f*s,34f*s,"Back to world",accent=true)

  private def renderHud(): Unit =
    glDisable(GL_DEPTH_TEST); glDisable(GL_CULL_FACE); setupOrtho()
    // Normal play stays quiet. Detailed telemetry belongs on F3, not permanently
    // stamped over the world like an engine sample.
    if gameMode == GameMode.Survival then renderHealth()
    if down(GLFW_KEY_TAB) then renderPlayerListOverlay()
    if journalToastTime > 0f then
      val s = uiScale
      val tw = (textWidth(journalToast,1.2f*s)+44f*s).min(framebufferWidth-32f*s)
      rect(framebufferWidth/2f-tw/2f,24f*s,tw,48f*s,0.07f,0.13f,0.12f,0.94f)
      centeredText(framebufferWidth/2f,31f*s,"JOURNAL UPDATED",0.55f,0.77f,0.60f,0.8f*s)
      centeredText(framebufferWidth/2f,48f*s,journalToast,0.96f,0.94f,0.82f,1.2f*s)

  private def heldHotbarBlock: Block =
    if hotbarBlocks.isEmpty then Block.Air else hotbarBlocks(selectedBlock.max(0).min(hotbarBlocks.length-1))

  private def renderFirstPersonHeldBlock(): Unit =
    if hotbarBlocks.isEmpty || modUiCursorMode then return
    val block=if equipProgress>=1f then heldHotbarBlock else equippedBlock
    val pose=FirstPersonPose.sample(block,viewmodelKick,cameraBobPhase,cameraBobAmount,viewmodelObstruction,equipProgress)
    updateFrameLight()
    resetGlArraysAndBuffers()
    glDisable(GL_FOG); glDisable(GL_CULL_FACE); glDisable(GL_TEXTURE_2D)
    val resolved=antiAliasing && firstPersonPass.begin(framebufferWidth,framebufferHeight)
    glEnable(GL_DEPTH_TEST); glDepthMask(true)
    if !resolved then
      glClear(GL_DEPTH_BUFFER_BIT)
      glEnable(GL_BLEND); glBlendFunc(GL_SRC_ALPHA,GL_ONE_MINUS_SRC_ALPHA)
    glMatrixMode(GL_PROJECTION); glLoadIdentity()
    val aspect=framebufferWidth.toDouble/framebufferHeight.toDouble.max(1)
    val near=0.05; val top=tan(toRadians(72.0)*0.5)*near
    glFrustum(if resolved then 0 else -top*aspect,top*aspect,-top,top,near,8)
    glMatrixMode(GL_MODELVIEW); glLoadIdentity()
    BlockboxPlayerModel.beginLighting(viewSpace = true)
    val lagYaw=((((yaw-viewmodelYaw)%360f)+540f)%360f-180f).max(-40f).min(40f)
    val lagPitch=(pitch-viewmodelPitch).max(-40f).min(40f)
    glRotatef(lagPitch*0.14f,1,0,0); glRotatef(lagYaw*0.14f,0,1,0)
    glTranslatef(pose.position.x*(aspect.toFloat/1.2f).min(1f),pose.position.y,pose.position.z)
    glRotatef(pose.rotation.x,1,0,0); glRotatef(pose.rotation.y,0,1,0); glRotatef(pose.rotation.z,0,0,1)
    glTranslatef(0,-0.39f,0); glRotatef(180,1,0,0)
    val light=viewmodelLight(block)
    val attachment=if block==Block.Air then None else Some(() => drawHeldItemAtGrip(block,light))
    BlockboxPlayerModel.arm(colorForId(localColorId),light,1,pose.elbow,pose.grip,attachment)
    BlockboxPlayerModel.endLighting()
    if resolved then firstPersonPass.finish()
    glColor4f(1,1,1,1); glBindTexture(GL_TEXTURE_2D,0)
    glDisable(GL_TEXTURE_2D); glDisable(GL_DEPTH_TEST); glDepthMask(true)

  /** Terrain light at the camera for the arm and held item; a held torch lights its own hand. */
  private def viewmodelLight(block: Block): (Float, Float, Float) =
    val (sky, blockLevel) = lightLevelsAt(floor(camera.x).toInt, floor(camera.y).toInt, floor(camera.z).toInt)
    val glow = if block == Block.Torch then blockLevel.max(14) else blockLevel
    BlockboxLightmap.color(sky / 15f, glow / 15f, frameLight)

  private def drawHeldItemAtGrip(block: Block, light: (Float, Float, Float)): Unit =
    val grip=FirstPersonPose.attachment(block)
    glPushMatrix()
    glTranslatef(grip.position.x,grip.position.y,grip.position.z)
    glRotatef(grip.rotation.x,1,0,0); glRotatef(grip.rotation.y,0,1,0); glRotatef(grip.rotation.z,0,0,1)
    if block==Block.Torch then
      glScalef(grip.size,grip.size,grip.size)
      drawHeldTorch(light)
    else drawHeldBlockCube(block,grip.size,light)
    glPopMatrix()

  private def drawHeldTorch(light: (Float, Float, Float)): Unit =
    glEnable(GL_TEXTURE_2D); activeAtlas.bind()
    glTexEnvi(GL_TEXTURE_ENV,GL_TEXTURE_ENV_MODE,GL_MODULATE)
    val lit = glIsEnabled(GL_LIGHTING)
    // The stick takes the hand's lighting; the flame is emissive and drawn unlit.
    for flamePass <- Seq(false, true) do
      if flamePass && lit then glDisable(GL_LIGHTING)
      glBegin(GL_TRIANGLES)
      TorchModel.vertices(TorchMount.Floor).foreach { vertex =>
        if vertex.flame == flamePass then
          if flamePass then glColor4f(1f,1f,1f,1f)
          else
            val shade = if lit then 1f else 0.72f+vertex.ny.max(0f)*0.20f
            glColor4f(shade*light._1,shade*light._2,shade*light._3,1f)
          val uv = activeAtlas.uv(Block.Torch,FaceKind.North,vertex.u,vertex.v)
          glTexCoord2f(uv._1,uv._2); glNormal3f(vertex.nx,vertex.ny,vertex.nz)
          // Only the flame tips move; its base and the ember stay on the stick.
          val tip=vertex.flame && vertex.y>0.7f
          val sway=if tip then (sin(gameTime*7.3f)*0.6+sin(gameTime*12.1f+1.3f)*0.4).toFloat*0.018f else 0f
          val lift=if tip then sin(gameTime*9.563f).toFloat*0.016f else 0f
          // The handle is tilted inside the fist; a full-length butt would poke out of the palm.
          val y=vertex.y.max(FirstPersonPose.torchGripHeight-0.06f)
          glVertex3f(vertex.x-0.5f+sway,y-FirstPersonPose.torchGripHeight+lift,vertex.z-0.5f+sway*0.6f)
      }
      glEnd()
      if flamePass && lit then glEnable(GL_LIGHTING)
    glColor4f(1,1,1,1); glBindTexture(GL_TEXTURE_2D,0); glDisable(GL_TEXTURE_2D)

  private def drawHeldBlockCube(block: Block, size: Float, light: (Float, Float, Float) = (1f, 1f, 1f)): Unit =
    val h = size * 0.5f
    glEnable(GL_TEXTURE_2D)
    activeAtlas.bind()
    glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_MODULATE)
    val lit = glIsEnabled(GL_LIGHTING)

    def quad(shade: Float, tint: (Float, Float, Float), normal: (Float, Float, Float), lift: Float,
             verts: Array[(Float, Float, Float)], tex: Array[(Float, Float)], uvAt: (Float, Float) => (Float, Float)): Unit =
      val s = if lit then 1f else shade
      glColor4f(s * tint._1 * light._1, s * tint._2 * light._2, s * tint._3 * light._3, 1f)
      glNormal3f(normal._1, normal._2, normal._3)
      glBegin(GL_QUADS)
      var i = 0
      while i < 4 do
        val uv = uvAt(tex(i)._1, tex(i)._2)
        glTexCoord2f(uv._1, uv._2)
        glVertex3f(verts(i)._1 + normal._1 * lift, verts(i)._2 + normal._2 * lift, verts(i)._3 + normal._3 * lift)
        i += 1
      glEnd()

    def face(kind: FaceKind, shade: Float, normal: (Float, Float, Float), verts: Array[(Float, Float, Float)], tex: Array[(Float, Float)]): Unit =
      quad(shade, BlockTints.item(block, kind), normal, 0f, verts, tex, activeAtlas.uv(block, kind, _, _))
      if block == Block.Grass && kind != FaceKind.Top && kind != FaceKind.Bottom && activeAtlas.hasGrassOverlay then
        glEnable(GL_ALPHA_TEST); glAlphaFunc(GL_GREATER, 0.5f)
        quad(shade, BlockTints.plainsGrass, normal, h * 0.01f, verts, tex, activeAtlas.grassOverlayUv)
        glDisable(GL_ALPHA_TEST)

    val topUv = Array((0f,0f), (1f,0f), (1f,1f), (0f,1f))
    val verticalUv = Array((0f,1f), (1f,1f), (1f,0f), (0f,0f))

    // Match Chunk.addFace's exact orientation. This is important for directional
    // textures such as logs, furnaces and any resource-pack art with an obvious top.
    face(FaceKind.Top, 1.00f, (0f, 1f, 0f), Array((-h,h,-h), (h,h,-h), (h,h,h), (-h,h,h)), topUv)
    face(FaceKind.Bottom, 0.48f, (0f, -1f, 0f), Array((-h,-h,h), (h,-h,h), (h,-h,-h), (-h,-h,-h)), topUv)
    face(FaceKind.South, 0.82f, (0f, 0f, 1f), Array((h,-h,h), (-h,-h,h), (-h,h,h), (h,h,h)), verticalUv)
    face(FaceKind.North, 0.60f, (0f, 0f, -1f), Array((-h,-h,-h), (h,-h,-h), (h,h,-h), (-h,h,-h)), verticalUv)
    face(FaceKind.East, 0.74f, (1f, 0f, 0f), Array((h,-h,-h), (h,-h,h), (h,h,h), (h,h,-h)), verticalUv)
    face(FaceKind.West, 0.66f, (-1f, 0f, 0f), Array((-h,-h,h), (-h,-h,-h), (-h,h,-h), (-h,h,h)), verticalUv)

    glColor4f(1f, 1f, 1f, 1f)
    glBindTexture(GL_TEXTURE_2D, 0)
    glDisable(GL_TEXTURE_2D)

  private def renderSelectedItemName(): Unit =
    if selectedItemLabelTimer <= 0f || hotbarBlocks.isEmpty then return
    val slot = selectedBlock.max(0).min(hotbarBlocks.length - 1)
    val block = hotbarBlocks(slot)
    if block == Block.Air then return
    val alpha = (selectedItemLabelTimer / 0.35f).min(1f).max(0f)
    val s = uiScale
    val hotbarSlot = (46f * s).min(((framebufferWidth.toFloat - 36f * s) / hotbarBlocks.length.toFloat - 5f * s).max(24f)).max(30f)
    // Reserve independent HUD lanes: hotbar at the bottom, hearts above it, then
    // transient item labels. This prevents the old item-name/health overlap.
    val healthLaneY = framebufferHeight.toFloat - hotbarSlot - 42f * s
    val y = if gameMode == GameMode.Survival then healthLaneY - 26f * s else healthLaneY
    val label = blockName(block)
    val scale = (0.82f * s).max(0.72f)
    val tw = textWidth(label, scale)
    val x = framebufferWidth.toFloat / 2f - tw / 2f
    rect(x - 7f * s, y - 4f * s, tw + 14f * s, 15f * scale, 0f, 0f, 0f, 0.34f * alpha)
    renderTextShadow(x, y, label, 1f, 1f, 1f, scale)

  private def renderBreakingIndicator(): Unit =
    if breakingProgress <= 0f then return
    glDisable(GL_DEPTH_TEST); glDisable(GL_CULL_FACE); setupOrtho()
    val s = uiScale
    val w = 48f * s
    val h = (4f * s).max(3f)
    val x = framebufferWidth.toFloat / 2f - w / 2f
    val y = framebufferHeight.toFloat / 2f + 21f * s
    rect(x - 1f, y - 1f, w + 2f, h + 2f, 0f, 0f, 0f, 0.72f)
    rect(x, y, w * breakingProgress.max(0f).min(1f), h, 0.92f, 0.92f, 0.92f, 0.95f)

  private def renderHurtOverlay(): Unit =
    if hurtOverlay <= 0.001f then return
    glDisable(GL_DEPTH_TEST); glDisable(GL_CULL_FACE); setupOrtho()
    val a = (hurtOverlay * 0.22f).min(0.22f)
    val edge = framebufferWidth.min(framebufferHeight).toFloat * 0.10f
    rect(0f, 0f, framebufferWidth.toFloat, edge, 0.55f, 0.02f, 0.02f, a)
    rect(0f, framebufferHeight.toFloat - edge, framebufferWidth.toFloat, edge, 0.55f, 0.02f, 0.02f, a)
    rect(0f, edge, edge, framebufferHeight.toFloat - edge * 2f, 0.55f, 0.02f, 0.02f, a * 0.8f)
    rect(framebufferWidth.toFloat - edge, edge, edge, framebufferHeight.toFloat - edge * 2f, 0.55f, 0.02f, 0.02f, a * 0.8f)

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
    val hotbarSlot = (46f * s).min(((framebufferWidth.toFloat - 36f * s) / hotbarBlocks.length.toFloat - 5f * s).max(24f)).max(30f)
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
    val debugScale = (0.88f * uiScale).min(((framebufferWidth.toFloat - 44f) / textWidth("F3: Debug  [F4:wire OFF] [F5:borders OFF]", 1f)).max(0.58f))
    var ly = 20; val lx = 18
    val bg = 0.05f; val a = 0.65f
    val panelW = (framebufferWidth.toFloat - 24f).min(520f * uiScale).max(260f)
    val panelH = (framebufferHeight.toFloat - 34f).min(286f * uiScale).max(190f)
    rect(lx - 6, ly - 4, panelW, panelH, bg, bg, bg, a)
    renderTextShadowFit(lx, ly, s"F3: Debug  [F4:wire ${onOff(wireframeMode)}] [F5:borders ${onOff(showChunkBorders)}]", 0.6f, 0.8f, 1f, debugScale, panelW - 18f); ly += (20 * debugScale).toInt.max(15)
    renderTextShadowFit(lx, ly, s"FPS: $fpsDisplay  Frame: ${(lastFrameTime * 1000).toInt}ms  Chunks: ${chunks.size}", 0.4f, 1f, 0.4f, debugScale, panelW - 18f); ly += (19 * uiScale).toInt.max(16)
    renderTextShadowFit(lx, ly, s"XYZ: ${camera.x}%.3f / ${camera.y}%.3f / ${camera.z}%.3f", 0.4f, 1f, 0.4f, debugScale, panelW - 18f); ly += (19 * uiScale).toInt.max(16)
    renderTextShadowFit(lx, ly, s"Yaw: $yaw%.2f  Pitch: $pitch%.2f", 0.4f, 1f, 0.4f, debugScale, panelW - 18f); ly += (19 * uiScale).toInt.max(16)
    renderTextShadowFit(lx, ly, s"Chunk: ($ccx, $ccz)  Seed: $worldSeed  RD: ${renderDistance}ch/${renderDistanceBlocks}b", 0.4f, 1f, 0.4f, debugScale, panelW - 18f); ly += (19 * uiScale).toInt.max(16)
    renderTextShadowFit(lx, ly, s"World: $worldName", 0.4f, 1f, 0.4f, debugScale * 0.82f, panelW - 18f); ly += (17 * uiScale).toInt.max(14)
    renderTextShadowFit(lx, ly, s"Stream Q: create ${chunkCreateQueue.size}  mesh ${chunkBuildQueue.size}  upload ${chunkUploadQueue.size}  ready ${chunkReadyQueue.size}", 0.55f, 0.85f, 1f, debugScale * 0.93f, panelW - 18f); ly += (18 * uiScale).toInt.max(15)
    renderTextShadowFit(lx, ly, f"Weather: rain $rainStrength%.2f  wet $wetness%.2f  particles ${blockParticles.size}%d  torch fx ${TorchEffects.flameCount}%d/${TorchEffects.particleCount}%d  saveQ ${pendingSaveJobs.get()}%d", 0.55f, 0.85f, 1f, debugScale * 0.93f, panelW - 18f); ly += (18 * uiScale).toInt.max(15)
    renderTextShadowFit(lx, ly, s"Forward: (${fwd.x}%.4f, ${fwd.y}%.4f, ${fwd.z}%.4f)", 0.4f, 1f, 0.4f, debugScale, panelW - 18f); ly += (19 * uiScale).toInt.max(16)
    renderTextShadowFit(lx, ly, s"World resolution: ${if foveatedRendering && foveaPass != null then (foveaPass.internalScale*100).round else 100}%  Drawn chunks: ${renderChunkScratch.length}", 0.55f, 0.85f, 1f, debugScale * 0.93f, panelW - 18f); ly += (18 * uiScale).toInt.max(15)
    raycast(8f).foreach { hit =>
      ly += 4; rect(lx - 6, ly - 4, 340, 62, bg, bg, bg, a)
      val block = activeBlockAt(hit.block._1, hit.block._2, hit.block._3)
      renderTextShadowFit(lx, ly, s"Hit: (${hit.block._1}, ${hit.block._2}, ${hit.block._3}) $block", 1f, 0.8f, 0.3f, debugScale, panelW - 18f); ly += (19 * uiScale).toInt.max(16)
      renderTextShadowFit(lx, ly, s"Normal: (${hit.normal._1}, ${hit.normal._2}, ${hit.normal._3})  D: ${hit.distance}%.3f", 1f, 0.8f, 0.3f, debugScale, panelW - 18f); ly += (19 * uiScale).toInt.max(16)
      renderTextShadowFit(lx, ly, s"Place: (${hit.place._1}, ${hit.place._2}, ${hit.place._3})", 1f, 0.8f, 0.3f, debugScale, panelW - 18f); ly += (19 * uiScale).toInt.max(16)
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
    rect(startX - 8f * s, y - 8f * s, barW + 16f * s, slotSize + 16f * s, 0.01f, 0.015f, 0.025f, 0.34f)
    for i <- 0 until total do
      val sx = startX + i * (slotSize + gap)
      val selected = i == selectedBlock
      val block = hotbarBlocks(i)
      val hasItem = block != Block.Air && (gameMode == GameMode.Creative || hotbarCounts(i) > 0)
      BlockboxRender2D.drawSlotFrame(sx, y, slotSize, selected, hover = false, s)
      if hasItem then
        val icon = (slotSize * 0.50f).min(24f * s).max(16f)
        val iconY = y + (slotSize - icon) / 2f + 5f * s
        renderBlockIcon(block, sx + (slotSize - icon) / 2f, iconY, icon)
        if gameMode == GameMode.Survival then
          val count = hotbarCounts(i)
          if count > 0 then
            val ns = count.toString
            val cell = (2.35f * s).max(2f).min(slotSize / 16f)
            val tw = compactNumberWidth(ns, cell)
            val nx = sx + slotSize - tw - 5f * s
            val ny = y + slotSize - 5f * cell - 4f * s
            rect(nx - 2f * s, ny - 1f * s, tw + 4f * s, 5f * cell + 3f * s, 0f, 0f, 0f, 0.58f)
            drawCompactNumber(nx, ny, ns, cell, 1f, 1f, 1f)
      val label = hotbarLabel(i)
      val labelCell = (2.1f * s).max(1.8f).min(slotSize / 17f)
      drawCompactNumber(sx + slotSize / 2f - compactNumberWidth(label, labelCell) / 2f, y + 7f * s, label, labelCell, 0.97f, 0.97f, 0.99f)

  private def hotbarLabel(index: Int): String = (index + 1).toString

  private def compactDigitGlyph(c: Char): Array[String] = c match
    case '0' => Array("111", "101", "101", "101", "111")
    case '1' => Array("010", "110", "010", "010", "111")
    case '2' => Array("111", "001", "111", "100", "111")
    case '3' => Array("111", "001", "111", "001", "111")
    case '4' => Array("101", "101", "111", "001", "001")
    case '5' => Array("111", "100", "111", "001", "111")
    case '6' => Array("111", "100", "111", "101", "111")
    case '7' => Array("111", "001", "001", "010", "010")
    case '8' => Array("111", "101", "111", "101", "111")
    case '9' => Array("111", "101", "111", "001", "111")
    case '-' => Array("000", "000", "111", "000", "000")
    case _   => Array("000", "000", "000", "000", "000")

  private def compactNumberWidth(text: String, cell: Float): Float =
    val len = Option(text).getOrElse("").count(ch => ch == '-' || ch.isDigit)
    if len <= 0 then 0f else (len * 4 - 1) * cell

  private def drawCompactNumber(x: Float, y: Float, text: String, cell: Float, r: Float, g: Float, b: Float): Unit =
    val safe = Option(text).getOrElse("").filter(ch => ch == '-' || ch.isDigit)
    if safe.isEmpty then return
    def pass(ox: Float, oy: Float, pr: Float, pg: Float, pb: Float, pa: Float): Unit =
      var cx = x + ox
      for ch <- safe do
        val glyph = compactDigitGlyph(ch)
        for row <- 0 until 5 do
          val line = glyph(row)
          for col <- 0 until 3 do
            if line.charAt(col) == '1' then rect(cx + col * cell, y + oy + row * cell, cell * 0.82f, cell * 0.82f, pr, pg, pb, pa)
        cx += 4f * cell
    val shadow = (cell * 0.45f).max(1f)
    pass(shadow, shadow, 0f, 0f, 0f, 0.70f)
    pass(0f, 0f, r, g, b, 1f)

  private def renderBlockIcon(block: Block, x: Float, y: Float, size: Float): Unit =
    glEnable(GL_TEXTURE_2D); activeAtlas.bind()
    glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_MODULATE)

    def quad(shade: Float, tint: (Float, Float, Float), a: (Float, Float), b: (Float, Float), c: (Float, Float), d: (Float, Float),
             uvAt: (Float, Float) => (Float, Float)): Unit =
      val uv00 = uvAt(0f, 0f)
      val uv10 = uvAt(1f, 0f)
      val uv11 = uvAt(1f, 1f)
      val uv01 = uvAt(0f, 1f)
      glColor4f(shade * tint._1, shade * tint._2, shade * tint._3, 1f)
      glBegin(GL_QUADS)
      glTexCoord2f(uv00._1, uv00._2); glVertex2f(a._1, a._2)
      glTexCoord2f(uv10._1, uv10._2); glVertex2f(b._1, b._2)
      glTexCoord2f(uv11._1, uv11._2); glVertex2f(c._1, c._2)
      glTexCoord2f(uv01._1, uv01._2); glVertex2f(d._1, d._2)
      glEnd()

    // a-b is the top edge and d-c the bottom edge of each face.
    def face(kind: FaceKind, shade: Float, a: (Float, Float), b: (Float, Float), c: (Float, Float), d: (Float, Float)): Unit =
      quad(shade, BlockTints.item(block, kind), a, b, c, d, activeAtlas.uv(block, kind, _, _))
      if block == Block.Grass && kind != FaceKind.Top && kind != FaceKind.Bottom && activeAtlas.hasGrassOverlay then
        quad(shade, BlockTints.plainsGrass, a, b, c, d, activeAtlas.grassOverlayUv)

    // Three-face isometric voxel. This keeps hotbar/inventory icons readable at
    // small sizes and makes the first-person held block feel like a volume rather
    // than the old flat top-texture square.
    val cx = x + size * 0.50f
    val north = (cx, y + size * 0.04f)
    val east = (x + size * 0.94f, y + size * 0.27f)
    val south = (cx, y + size * 0.50f)
    val west = (x + size * 0.06f, y + size * 0.27f)
    val eastBottom = (east._1, y + size * 0.71f)
    val southBottom = (south._1, y + size * 0.94f)
    val westBottom = (west._1, y + size * 0.71f)

    face(FaceKind.West, 0.69f, west, south, southBottom, westBottom)
    face(FaceKind.South, 0.82f, south, east, eastBottom, southBottom)
    face(FaceKind.Top, 1.00f, north, east, south, west)

    glColor4f(1f, 1f, 1f, 1f)
    glBindTexture(GL_TEXTURE_2D, 0); glDisable(GL_TEXTURE_2D)

  private def renderCrosshair(): Unit =
    BlockboxRender2D.renderCrosshair(framebufferWidth, framebufferHeight)

  private def renderUnderwaterOverlay(): Unit =
    BlockboxRender2D.renderUnderwaterOverlay(framebufferWidth, framebufferHeight, glfwGetTime().toFloat)

  private def clearColor: (Float, Float, Float) =
    val base = BlockboxSky.clearColor(isUnderwater, dayPhase, daylightFactor, localRain, localCloud, localLightning)
    if isUnderwater || localRain <= 0.01f then base
    else
      val storm = localRain.max(0f).min(1f)
      val gray = 0.43f * daylightFactor + 0.09f
      (base._1 * (1f - 0.34f * storm) + gray * 0.20f * storm,
       base._2 * (1f - 0.31f * storm) + gray * 0.24f * storm,
       base._3 * (1f - 0.25f * storm) + gray * 0.30f * storm)

  protected val dayLengthSeconds = BlockboxSky.dayLengthSeconds
  protected val nightLengthSeconds = BlockboxSky.nightLengthSeconds

  private def gameTime: Float = worldClock.toFloat

  private def environmentMessage: String =
    Seq("ENV", networkFloat(gameTime), networkFloat(timeOverride.getOrElse(-1f)), networkFloat(rainStrength), networkFloat(wetness), networkFloat(stormCloudCover), networkFloat(weatherWind), networkFloat(lightningFlash)).mkString("|")

  private def cycleClock: Float =
    BlockboxSky.cycleClock(timeOverride.getOrElse(gameTime))

  private def dayPhase: Float =
    BlockboxSky.dayPhase(timeOverride.getOrElse(gameTime))

  private def daylightFactor: Float =
    BlockboxSky.daylightFactor(dayPhase)

  private def smooth01(v: Float): Float =
    BlockboxSky.smooth01(v)

  /** This frame's lightmap inputs; also used to light players, particles and the hand. */
  private var frameLight = BlockboxLightmap.Environment.Noon

  private def updateFrameLight(): Unit =
    val sunHeight = BlockboxSky.sunDirection(dayPhase)._2
    val dusk = 1f - smooth01(abs(sunHeight + 0.03f) / 0.36f)
    frameLight = BlockboxLightmap.Environment(daylightFactor, dusk, localRain, localLightning, BlockboxLightmap.flicker(gameTime), worldDarkening)

  /** Sky and block light levels (0..15) at a block, from the chunk's last mesh. */
  private def lightLevelsAt(x: Int, y: Int, z: Int): (Int, Int) =
    if y >= Terrain.worldHeight then (15, 0)
    else
      chunks.get((Math.floorDiv(x, Terrain.chunkSize), Math.floorDiv(z, Terrain.chunkSize)))
        .flatMap(_.lightAt(Math.floorMod(x, Terrain.chunkSize), y, Math.floorMod(z, Terrain.chunkSize)))
        .getOrElse((15, 0))

  /** Terrain light colour at a world position, matching what the lightmap gives blocks. */
  private def lightColorAt(x: Float, y: Float, z: Float): (Float, Float, Float) =
    val (sky, block) = lightLevelsAt(floor(x).toInt, floor(y).toInt, floor(z).toInt)
    BlockboxLightmap.color(sky / 15f, block / 15f, frameLight)

  private def nightDarkness: Float =
    BlockboxSky.nightDarkness(multiplayerMode, daylightFactor)

  private def generateStars(): Unit =
    if starPositions != null then return
    starPositions = BlockboxSky.generateStars()

  private def renderSky(): Unit =
    generateStars()
    BlockboxSky.renderSky(yaw, pitch, dayPhase, daylightFactor, starPositions, cloudsEnabled, camera, gameTime, cloudRenderDistanceBlocks, localRain, localCloud, localLightning, !(shaderPipeline!=null && shaderPipeline.isSceneBound))

  private def weatherTarget(time: Float): Float =
    val overrideRain=modManager.rules.get("weather")
    if overrideRain>=0 then overrideRain
    else if !weatherEnabled then 0f
    else
      val seedPhase = ((worldSeed ^ (worldSeed >>> 31)) & 0xffffL).toFloat / 65535f * (Pi * 2.0).toFloat
      val climate = sin(time * 0.0039f + seedPhase * 0.74f).toFloat * 0.40f
      val fronts = sin(time * 0.0117f + seedPhase * 1.63f + 1.4f).toFloat * 0.34f
      val texture = sin(time * 0.048f + seedPhase * 0.37f + 4.2f).toFloat * 0.16f
      val gusts = sin(time * 0.102f + seedPhase * 2.07f + 0.9f).toFloat * 0.09f
      val stormSignal = climate + fronts + texture + gusts
      val frontalBoost = smooth01((fronts + 0.20f) / 0.80f)
      smooth01((stormSignal - 0.10f) / 0.58f) * (0.80f + frontalBoost * 0.26f)

  private def updateWeather(dt: Float): Unit =
    if !(isClientOnlyMultiplayer && environmentReceived) then
      val target=weatherTarget(gameTime).max(0f).min(1f)
      rainStrength=approachExp(rainStrength,target,if target>rainStrength then 0.62f else 0.34f,dt)
      val wetTarget=if rainStrength>0.025f then (rainStrength*0.92f+0.08f).min(1f) else 0f
      wetness=approachExp(wetness,wetTarget,if wetTarget>wetness then 0.14f else 0.022f,dt)
      stormCloudCover=approachExp(stormCloudCover,(target*0.92f+weatherTarget(gameTime+18f)*0.12f).min(1f),0.18f,dt)
      weatherWind=approachExp(weatherWind,sin(gameTime*0.13f+(worldSeed&255L).toFloat).toFloat*(0.35f+rainStrength*0.65f),0.22f,dt)
    weatherCells=WeatherField.cells(worldSeed,gameTime,camera.x,camera.z,rainStrength,cloudRenderDistanceBlocks.toFloat)
    val local=WeatherField.sample(weatherCells,camera.x,camera.z)
    localRain=local.rain; localCloud=local.cloud
    localWetness=wetness*local.cloud
    weatherStrikes=WeatherField.strikes(weatherCells,gameTime)
    localLightning=WeatherField.flash(weatherStrikes,camera.x,camera.z)
    lightningFlash=0f
    shelterTimer-=dt
    if shelterTimer<=0f then
      rainShelter=if isUnderwater then 0.04f else if cameraHasOpenSky then 1f else 0.16f
      shelterTimer=0.25f
    val climate=terrainGen.biomeAt(floor(camera.x).toInt,floor(camera.z).toInt,terrainGen.heightAt(floor(camera.x).toInt,floor(camera.z).toInt))
    localSnowfall=climate==TerrainBiome.SnowyTaiga || climate==TerrainBiome.SnowyTundra || climate==TerrainBiome.JaggedPeaks
    updateRainAmbience(localRain*rainShelter*(if localSnowfall then 0.05f else 1f))
    val yawRadians=toRadians(yaw)
    weatherStrikes.foreach { strike =>
      val dx=strike.x-camera.x; val dz=strike.z-camera.z
      val distance=sqrt(dx*dx+dz*dz+10000).toFloat
      val delay=distance/343f
      if strike.age>=delay && strike.age<delay+0.25f && heardThunder.add(strike.id) then
        val pan=((dx*cos(yawRadians)+dz*sin(yawRadians))/distance).toFloat.max(-1f).min(1f)
        playThunderSound(strike.strength/(1f+distance/220f)*rainShelter.max(0.35f),pan)
    }
    if heardThunder.size>128 then
      val active=weatherStrikes.map(_.id).toSet
      heardThunder.filterInPlace(active.contains)

  private def cameraHasOpenSky: Boolean =
    val bx = floor(camera.x).toInt
    val bz = floor(camera.z).toInt
    var y = floor(camera.y).toInt + 1
    val top = Terrain.worldHeight - 1
    while y <= top do
      if activeBlockAt(bx, y, bz).solid then return false
      y += 1
    true

  private def renderWeatherRain(): Unit =
    if localRain < 0.025f || isUnderwater || !cameraHasOpenSky then return
    if localSnowfall then
      renderSnowfall()
      return
    resetGlArraysAndBuffers()
    glEnable(GL_DEPTH_TEST); glDepthMask(false)
    glEnable(GL_BLEND); glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA)
    glDisable(GL_TEXTURE_2D); glDisable(GL_CULL_FACE); glDisable(GL_ALPHA_TEST)
    glLineWidth((1.0f + localRain * 0.8f).min(1.8f))
    val radius = (8f + localRain * 9f).toInt
    val t = gameTime
    val baseX = floor(camera.x).toInt
    val baseZ = floor(camera.z).toInt
    glBegin(GL_LINES)
    var gx = -radius
    while gx <= radius do
      var gz = -radius
      while gz <= radius do
        val wx = baseX + gx
        val wz = baseZ + gz
        var h = wx * 73428767 ^ wz * 912931 ^ worldSeed.toInt
        h ^= h >>> 13; h *= 1274126177; h ^= h >>> 16
        if (h & 7) <= 2 then
          val jitterX = (((h >>> 8) & 255).toFloat / 255f - 0.5f) * 0.85f
          val jitterZ = (((h >>> 16) & 255).toFloat / 255f - 0.5f) * 0.85f
          val phase = (((h & 0xffff).toFloat / 65535f) + t * (0.78f + ((h >>> 24) & 15) * 0.012f)) % 1f
          val yTop = camera.y + 10f - phase * 22f
          val terrainTop = terrainGen.heightAt(wx, wz).toFloat + 1f
          if yTop > terrainTop - 0.5f then
            val y0 = yTop.max(terrainTop)
            val len = 0.65f + localRain * 1.35f
            val wind = weatherWind * 0.13f
            val alpha = (0.12f + localRain * 0.34f).min(0.46f)
            glColor4f(0.64f, 0.76f, 0.90f, alpha)
            glVertex3f(wx.toFloat + jitterX - camera.x, y0, wz.toFloat + jitterZ - camera.z)
            glVertex3f(wx.toFloat + jitterX - camera.x + wind, (y0 - len).max(terrainTop), wz.toFloat + jitterZ - camera.z + wind * 0.35f)
        gz += 1
      gx += 1
    glEnd()
    glLineWidth(1f); glDepthMask(true); glDisable(GL_BLEND)

  private def renderSnowfall(): Unit =
    resetGlArraysAndBuffers()
    glEnable(GL_DEPTH_TEST); glDepthMask(false); glDisable(GL_TEXTURE_2D)
    glEnable(GL_BLEND); glBlendFunc(GL_SRC_ALPHA,GL_ONE_MINUS_SRC_ALPHA)
    glEnable(GL_POINT_SMOOTH); glPointSize(3f)
    glBegin(GL_POINTS)
    val bx=floor(camera.x/2).toInt; val bz=floor(camera.z/2).toInt
    for x <- -10 to 10; z <- -10 to 10 do
      val wx=(bx+x)*2; val wz=(bz+z)*2
      var hash=wx*73428767^wz*912931^worldSeed.toInt
      hash^=hash>>>13; hash*=1274126177; hash^=hash>>>16
      val phase=(gameTime*0.07f+(hash&65535)/65535f)%1f
      val y=camera.y+9f-phase*20f
      val ground=terrainGen.heightAt(wx,wz)+1f
      if y>ground then
        val fade=(1f-(x*x+z*z)/180f).max(0f)*localRain*0.65f
        glColor4f(0.88f,0.93f,1f,fade)
        glVertex3f(wx-camera.x+sin(gameTime*0.7f+x+z).toFloat*0.7f+weatherWind*1.2f,y,wz-camera.z+cos(gameTime*0.6f+z).toFloat*0.5f)
    glEnd()
    glDisable(GL_POINT_SMOOTH); glPointSize(1); glDepthMask(true); glDisable(GL_BLEND)

  private def spawnBlockBreakParticles(x: Int, y: Int, z: Int, block: Block): Unit =
    val material = block match
      case Block.Glass => 3
      case Block.Wood | Block.BirchWood | Block.PineWood | Block.AcaciaWood | Block.Planks | Block.BirchPlanks | Block.PinePlanks | Block.AcaciaPlanks => 1
      case Block.Dirt | Block.Grass | Block.Clay | Block.Sand | Block.Snow => 2
      case Block.Leaves | Block.BirchLeaves | Block.PineLeaves | Block.AcaciaLeaves | Block.Cactus => 4
      case _ => 0
    val count = material match
      case 3 => 24 + blockParticleRng.nextInt(10)
      case 2 => 20 + blockParticleRng.nextInt(9)
      case 4 => 16 + blockParticleRng.nextInt(8)
      case _ => 18 + blockParticleRng.nextInt(9)
    // Grass and leaf debris uses the biome colour of the block that broke.
    val grass = if block == Block.Grass || BlockTints.isFoliage(block) then terrainGen.grassTintAt(x, z, y) else BlockTints.plainsGrass
    var i = 0
    while i < count do
      val px = x + 0.08f + blockParticleRng.nextFloat() * 0.84f
      val py = y + 0.10f + blockParticleRng.nextFloat() * 0.80f
      val pz = z + 0.08f + blockParticleRng.nextFloat() * 0.84f
      val radial = if material == 3 then 3.7f else if material == 2 then 2.8f else 3.1f
      val vx = (blockParticleRng.nextFloat() - 0.5f) * radial
      val vy = 1.45f + blockParticleRng.nextFloat() * (if material == 3 then 3.7f else 2.9f)
      val vz = (blockParticleRng.nextFloat() - 0.5f) * radial
      val life = (if material == 2 then 0.56f else 0.48f) + blockParticleRng.nextFloat() * 0.42f
      val sizeBase = if material == 3 then 0.030f else if material == 2 then 0.042f else 0.038f
      val size = sizeBase + blockParticleRng.nextFloat() * (if material == 3 then 0.045f else 0.058f)
      val face = FaceKind.values(blockParticleRng.nextInt(FaceKind.values.length))
      val crop = 0.16f + blockParticleRng.nextFloat() * 0.34f
      val u0 = blockParticleRng.nextFloat() * (1f - crop)
      val v0 = blockParticleRng.nextFloat() * (1f - crop)
      val gravity = if material == 4 then 8.0f else if material == 3 then 13.8f else 12.2f
      val drag = if material == 4 then 3.4f else if material == 2 then 2.5f else 1.8f
      val bounce = if material == 3 then 0.30f else if material == 0 then 0.22f else 0.15f
      val tint =
        if BlockTints.isFoliage(block) then BlockTints.foliage(block, grass)
        else if block == Block.Grass && face == FaceKind.Top then grass
        else (1f, 1f, 1f)
      blockParticles += BlockParticle(
        block, Vec3(px, py, pz), Vec3(vx, vy, vz), life, life, size,
        face, u0, v0, u0 + crop, v0 + crop,
        blockParticleRng.nextFloat() * Pi.toFloat * 2f,
        (blockParticleRng.nextFloat() - 0.5f) * (if material == 4 then 7f else 13f),
        gravity, drag, bounce, 0.80f + blockParticleRng.nextFloat() * 0.20f, tint
      )
      i += 1
    while blockParticles.length > maxBlockParticles do blockParticles.remove(0)

  private var torchScanTimer = 0f

  /** Flame positions of torches within three chunks, for glow and torch particles. */
  private def refreshTorchFlames(): Unit =
    val ccx = chunkCoordBlock(floor(camera.x).toInt); val ccz = chunkCoordBlock(floor(camera.z).toInt)
    val found = ArrayBuffer.empty[TorchEffects.Flame]
    for dx <- -3 to 3; dz <- -3 to 3 do
      chunks.get((ccx + dx, ccz + dz)).foreach { c =>
        c.lightSourcesSnapshot.foreach { s =>
          if c.getBlock(s.x, s.y, s.z) == Block.Torch then
            val (fx, fy, fz) = TorchModel.flameCenter(c.getTorchMount(s.x, s.y, s.z))
            found += TorchEffects.Flame(c.cx * Terrain.chunkSize + s.x + fx, s.y + fy, c.cz * Terrain.chunkSize + s.z + fz)
        }
      }
    TorchEffects.setFlames(found.toArray)

  private def updateTorchEffects(dt: Float): Unit =
    torchScanTimer -= dt
    if torchScanTimer <= 0f then
      torchScanTimer = 0.25f
      refreshTorchFlames()
    TorchEffects.update(dt, camera)

  private def renderTorchEffects(): Unit =
    val (sky, _) = lightLevelsAt(floor(camera.x).toInt, floor(camera.y).toInt, floor(camera.z).toInt)
    val day = ((daylightFactor - 0.18f) / 0.82f).max(0f).min(1f) * sky / 15f
    resetGlArraysAndBuffers()
    TorchEffects.render(camera, yaw, pitch, gameTime, 1f - day, lightColorAt)

  private def updateBlockParticles(dt: Float): Unit =
    var i = blockParticles.length - 1
    while i >= 0 do
      val p = blockParticles(i)
      p.life -= dt
      if p.life <= 0f then blockParticles.remove(i)
      else
        val dragMul = math.exp((-p.drag * dt).toDouble).toFloat
        p.velocity = Vec3(p.velocity.x * dragMul, p.velocity.y - p.gravity * dt, p.velocity.z * dragMul)
        p.rotation += p.spin * dt
        val next = p.pos + p.velocity * dt
        val bx = floor(next.x).toInt; val by = floor(next.y - p.size).toInt; val bz = floor(next.z).toInt
        if p.velocity.y < 0f && by >= 0 && activeBlockAt(bx, by, bz).solid then
          p.pos = Vec3(next.x, by + 1f + p.size, next.z)
          p.velocity = Vec3(p.velocity.x * 0.55f, -p.velocity.y * p.bounce, p.velocity.z * 0.55f)
        else p.pos = next
      i -= 1

  private def renderBlockParticles(): Unit =
    if blockParticles.isEmpty then return
    resetGlArraysAndBuffers()
    glEnable(GL_DEPTH_TEST); glDepthMask(false)
    glEnable(GL_BLEND); glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA)
    glEnable(GL_TEXTURE_2D); activeAtlas.bind(); glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_MODULATE)
    // One immediate-mode batch for the whole debris cloud. The old implementation did
    // glBegin/glEnd per particle, which amplified driver overhead exactly when many
    // blocks were being mined. Three crossed textured shards give much better volume
    // while keeping submission to a single batch.
    glBegin(GL_QUADS)
    blockParticles.foreach { p =>
      val lifeFrac = (p.life / p.maxLife).max(0f).min(1f)
      val fadeIn = (1f - lifeFrac).min(0.12f) / 0.12f
      val alpha = (lifeFrac * 1.25f).min(1f) * fadeIn.max(0.35f)
      val s0 = p.size * (0.72f + lifeFrac * 0.28f)
      val c = cos(p.rotation).toFloat; val sn = sin(p.rotation).toFloat
      val rx = c * s0; val rz = sn * s0
      val px = p.pos.x - camera.x; val py = p.pos.y; val pz = p.pos.z - camera.z
      val uv00 = activeAtlas.uv(p.block, p.face, p.u0, p.v0)
      val uv10 = activeAtlas.uv(p.block, p.face, p.u1, p.v0)
      val uv11 = activeAtlas.uv(p.block, p.face, p.u1, p.v1)
      val uv01 = activeAtlas.uv(p.block, p.face, p.u0, p.v1)
      val shade = p.brightness * (0.78f + lifeFrac * 0.22f)
      val (lr, lg, lb) = lightColorAt(p.pos.x, p.pos.y, p.pos.z)
      glColor4f(shade * p.tint._1 * lr, shade * p.tint._2 * lg, shade * p.tint._3 * lb, alpha)
      glTexCoord2f(uv00._1, uv00._2); glVertex3f(px-rx, py-s0, pz-rz)
      glTexCoord2f(uv10._1, uv10._2); glVertex3f(px+rx, py-s0, pz+rz)
      glTexCoord2f(uv11._1, uv11._2); glVertex3f(px+rx, py+s0, pz+rz)
      glTexCoord2f(uv01._1, uv01._2); glVertex3f(px-rx, py+s0, pz-rz)
      glTexCoord2f(uv00._1, uv00._2); glVertex3f(px-rz, py-s0*0.82f, pz+rx)
      glTexCoord2f(uv10._1, uv10._2); glVertex3f(px+rz, py-s0*0.82f, pz-rx)
      glTexCoord2f(uv11._1, uv11._2); glVertex3f(px+rz, py+s0*0.82f, pz-rx)
      glTexCoord2f(uv01._1, uv01._2); glVertex3f(px-rz, py+s0*0.82f, pz+rx)
      glTexCoord2f(uv00._1, uv00._2); glVertex3f(px-s0*0.72f, py, pz-s0*0.72f)
      glTexCoord2f(uv10._1, uv10._2); glVertex3f(px+s0*0.72f, py, pz-s0*0.72f)
      glTexCoord2f(uv11._1, uv11._2); glVertex3f(px+s0*0.72f, py, pz+s0*0.72f)
      glTexCoord2f(uv01._1, uv01._2); glVertex3f(px-s0*0.72f, py, pz+s0*0.72f)
    }
    glEnd()
    glColor4f(1f,1f,1f,1f); glBindTexture(GL_TEXTURE_2D,0); glDepthMask(true); glDisable(GL_BLEND)

  private def updateBubbles(dt: Float): Unit =
    bubbleSpawnCooldown -= dt
    if isUnderwater then
      while bubbleParticles.length > 80 do bubbleParticles.remove(bubbleParticles.length - 1)
      if bubbleSpawnCooldown <= 0f && bubbleParticles.length < 80 then
        val offsetX = (bubbleRng.nextFloat() - 0.5f) * 0.6f
        val offsetZ = (bubbleRng.nextFloat() - 0.5f) * 0.6f
        bubbleParticles += ((camera.x + offsetX, camera.y + bubbleRng.nextFloat() * 1.4f, camera.z + offsetZ, 1.5f + bubbleRng.nextFloat() * 2f))
        bubbleSpawnCooldown = 0.11f + bubbleRng.nextFloat() * 0.14f
    else if bubbleSpawnCooldown < -0.5f then bubbleSpawnCooldown = 0f
    for i <- bubbleParticles.indices do
      val (x, y, z, life) = bubbleParticles(i)
      val ny = y + 2.5f * dt
      val nextLife = life - dt
      val npos = Vec3(x, ny, z)
      if blockAt(npos).solid then bubbleParticles(i) = (x, ny, z, 0f)
      else bubbleParticles(i) = (x, ny, z, nextLife)
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
    val chatWidth = (w - 20f).min(600f * uiScale).max(260f)
    val hotbarSlot = (46f * uiScale).min(((w - 36f * uiScale) / hotbarBlocks.length.toFloat - 5f * uiScale).max(24f)).max(30f)
    val hudClearance = hotbarSlot + (if gameMode == GameMode.Survival then 88f * uiScale else 50f * uiScale)
    var ly = h - hudClearance
    val maxDisplay = 12
    var count = 0
    for (msg, t) <- chatMessages.view.reverse do
      if count < maxDisplay && t > 0f then
        val alpha = t.min(1f)
        val chatScale = (1.28f * uiScale).max(1.08f)
        if ly > 60 then
          val msgWidth = textWidth(msg, chatScale).min(chatWidth - 20f)
          rect(10f, ly - 4f, msgWidth + 12f, 18f * chatScale + 4f, 0f, 0f, 0f, 0.30f * alpha)
          renderTextShadowFit(14, ly, msg, 1f, 1f, 1f, chatScale, chatWidth - 24f)
        ly -= 22f * chatScale.max(0.9f)
        count += 1
    if chatOpen then
      val inputY = (h - hudClearance - 36f * uiScale).max(10f)
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

  private def mainMenuLayout(): MainMenuLayout =
    BlockboxUiLayout.mainMenu(framebufferWidth,framebufferHeight,uiScale)

  private def drawPixelLogo(cx: Float, y: Float, text: String, maxW: Float, maxH: Float): Unit =
    BlockboxRender2D.drawPixelLogo(cx,y,text,maxW,maxH)

  private def renderMainMenu(): Unit =
    glDisable(GL_DEPTH_TEST); glDisable(GL_CULL_FACE); setupOrtho()
    val w = framebufferWidth.toFloat; val h = framebufferHeight.toFloat
    BlockboxRender2D.drawMenuBackdrop(framebufferWidth,framebufferHeight,glfwGetTime().toFloat)
    val layout = mainMenuLayout(); val p = layout.panel; val s = layout.scale; val cx = w/2f
    drawPanel(p.x,p.y,p.w,p.h)
    centeredText(cx,p.y+28f*s,"A SANDBOX OF POSSIBILITIES",0.60f,0.76f,0.69f,1.0f*s)
    drawPixelLogo(cx,p.y+62f*s,"BLOCKBOX",p.w-104f*s,62f*s)
    centeredText(cx,p.y+142f*s,"Find your own way.",0.91f,0.89f,0.78f,1.45f*s)
    centeredText(cx,p.y+171f*s,"Build a home. Follow a cave. Bring a friend.",0.65f,0.72f,0.72f,1.05f*s)
    val labels = Vector("Create world","Host a world","Join friends","My worlds","Mods","Textures & shaders","Settings","Quit")
    layout.buttons.zipWithIndex.foreach { case (b,i) => drawButton(b.x,b.y,b.w,b.h,labels(i),accent = i==0) }
    centeredText(cx,p.y+p.h-27f*s,"WASD move   /   E craft   /   J journal   /   Esc menu",0.58f,0.65f,0.64f,0.85f*s)
    if pendingSaveJobs.get() > 0 then centeredText(cx,h-20f,"Saving world...",0.92f,0.78f,0.48f,0.9f*s)

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
    BlockboxRender2D.drawMenuBackdrop(framebufferWidth, framebufferHeight, glfwGetTime().toFloat)

    val cx = w / 2f
    val s = uiScale
    val margin = (18f * s).max(12f)
    val pW = (620f * s).min(w - margin * 2f).max(math.min(360f, w - margin * 2f))
    val pH = (560f * s).min(h - margin * 2f).max(math.min(420f, h - margin * 2f))
    val pX = cx - pW / 2f
    val pY = ((h - pH) / 2f).max(margin)
    drawPanel(pX, pY, pW, pH)

    centeredTextFit(cx, pY + 34f * s, "Create New World", 1f, 1f, 1f, 1.75f * s, pW - 80f * s)
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
      renderTextShadowFit(settingX + 16f * s, y + settingH / 2f - 6f * s, label, 0.82f, 0.86f, 0.92f, 0.82f * s, settingW - 178f * s)
      centeredTextFit(settingX + settingW - 82f * s, y + settingH / 2f - 6f * s, value, 1f, 0.93f, 0.54f, 0.82f * s, 150f * s)
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
    centeredTextFit(cx, pY + pH - 12f * s, "Tab switches fields | N name | C seed | M mode | H cheats", 0.42f, 0.48f, 0.60f, 0.64f * s, pW - 48f * s)

  private def renderLoadWorldMenu(): Unit =
    glDisable(GL_DEPTH_TEST); glDisable(GL_CULL_FACE); setupOrtho()
    val w = framebufferWidth.toFloat; val h = framebufferHeight.toFloat; val cx = w / 2f; val s = uiScale
    BlockboxRender2D.drawMenuBackdrop(framebufferWidth, framebufferHeight, glfwGetTime().toFloat)
    val pW = (700f * s).min(w * 0.92f); val pH = (520f * s).min(h * 0.90f)
    val pX = cx - pW / 2f; val pY = h / 2f - pH / 2f
    drawPanel(pX, pY, pW, pH)
    centeredTextFit(cx, pY + 34f * s, "Select World", 1f, 1f, 1f, 1.75f * s, pW - 80f * s)
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
        renderTextShadowFit(listX + 14f * s, y + 25f * s, s"Saved: ${new java.util.Date(worldDat.lastModified()).toString}", 0.50f, 0.58f, 0.70f, 0.62f * s, pW - 112f * s)
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
    BlockboxRender2D.drawMenuBackdrop(framebufferWidth, framebufferHeight, glfwGetTime().toFloat)
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
        renderTextShadowFit(listX + 14f * s, y + 7f * s, s"${m.name} ${m.version}  [${m.sideLabel}]", 0.96f, 0.97f, 1f, 0.78f * s, pW - 112f * s)
        renderTextShadowFit(listX + 14f * s, y + 25f * s, m.description, 0.62f, 0.70f, 0.82f, 0.64f * s, pW - 112f * s)
        renderTextShadowFit(listX + 14f * s, y + 42f * s, s"${m.id} | ${m.status}", if ok then 0.52f else 1f, if ok then 0.70f else 0.55f, if ok then 0.58f else 0.50f, 0.58f * s, pW - 112f * s)
      }
    val bw = (260f * s).min(pW * 0.44f); val bh = (38f * s).max(30f)
    drawButton(cx - bw / 2f, pY + pH - 56f * s, bw, bh, "Back")
    centeredTextFit(cx, pY + pH - 15f * s, "mods are trusted full-access JVM code. server mods must match host.", 0.48f, 0.55f, 0.68f, 0.64f * s, pW - 70f * s)

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
    BlockboxRender2D.drawMenuBackdrop(framebufferWidth, framebufferHeight, glfwGetTime().toFloat)
    val pW = (780f * s).min(w * 0.94f); val pH = (565f * s).min(h * 0.90f)
    val pX = cx - pW / 2f; val pY = h / 2f - pH / 2f
    drawPanel(pX, pY, pW, pH)
    centeredTextFit(cx, pY + 32f * s, "Resource Packs", 1f, 1f, 1f, 1.70f * s, pW - 80f * s)
    centeredTextFit(cx, pY + 58f * s, "Texture packs refresh immediately. Add custom packs in resourcepacks/<pack>/", 0.62f, 0.74f, 0.95f, 0.72f * s, pW - 80f * s)
    rect(pX + 34f * s, pY + 76f * s, pW - 68f * s, 1f, 0.30f, 0.34f, 0.42f, 0.30f)
    val rowH = (74f * s).max(56f)
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
      renderTextShadowFit(listX + 14f * s, y + 8f * s, pack.name, if selected then 1f else 0.94f, if selected then 0.92f else 0.96f, if selected then 0.60f else 1f, 0.92f * s, pW - 112f * s)
      renderTextShadowFit(listX + 14f * s, y + 32f * s, s"by ${pack.author}${if selected then "  [ACTIVE]" else ""}", 0.62f, 0.72f, 0.86f, 0.72f * s, pW - 112f * s)
      val pathText = if pack.legacy then "built-in procedural textures" else pack.dir.map(_.getPath).getOrElse("missing")
      renderTextShadowFit(listX + 14f * s, y + 53f * s, pathText, if available then 0.46f else 1f, if available then 0.56f else 0.48f, if available then 0.70f else 0.42f, 0.64f * s, pW - 112f * s)
    }
    val bw = (210f * s).min(pW * 0.28f); val bh = (38f * s).max(30f); val bg = 10f * s
    val leftX = cx - (bw * 1.5f + bg)
    drawButton(leftX, pY + pH - 56f * s, bw, bh, "Refresh Textures")
    drawButton(cx - bw / 2f, pY + pH - 56f * s, bw, bh, s"Shaders: ${Option(shaderPipeline).map(_.packName).getOrElse("built-in")}")
    drawButton(cx + bw / 2f + bg, pY + pH - 56f * s, bw, bh, "Back")
    centeredTextFit(cx, pY + pH - 15f * s, "resourcepacks/ for textures   shaderpacks/ for GLSL packs", 0.48f, 0.52f, 0.58f, 0.64f * s, pW - 70f * s)

  private def handleResourcesClick(mx: Float, my: Float): Unit =
    val w = framebufferWidth.toFloat; val h = framebufferHeight.toFloat; val cx = w / 2f; val s = uiScale
    val pW = (780f * s).min(w * 0.94f); val pH = (565f * s).min(h * 0.90f)
    val pX = cx - pW / 2f; val pY = h / 2f - pH / 2f
    val rowH = (74f * s).max(56f)
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
        val bw = (210f * s).min(pW * 0.28f); val bh = (38f * s).max(30f); val bg = 10f * s
        val leftX = cx - (bw * 1.5f + bg)
        if inRect(mx, my, leftX, pY + pH - 56f * s, bw, bh) then refreshResourcePacks()
        else if inRect(mx, my, cx - bw / 2f, pY + pH - 56f * s, bw, bh) then
          if shaderPipeline != null then shaderPipeline.cyclePack()
        else if inRect(mx, my, cx + bw / 2f + bg, pY + pH - 56f * s, bw, bh) then screen = resourcesReturnTo

  private def renderSettings(): Unit =
    glDisable(GL_DEPTH_TEST); glDisable(GL_CULL_FACE); setupOrtho()
    BlockboxRender2D.drawMenuBackdrop(framebufferWidth,framebufferHeight,glfwGetTime().toFloat)
    val s = uiScale; val layout = BlockboxUiLayout.settings(framebufferWidth,framebufferHeight,s)
    val p = layout.panel; val content = layout.content
    drawPanel(p.x,p.y,p.w,p.h)
    centeredText(framebufferWidth/2f,p.y+24f*s,"Settings",0.96f,0.94f,0.86f,2f*s)
    val names = Vector("Video","World","Interface")
    layout.tabs.zipWithIndex.foreach { case (tab,i) => drawButton(tab.x,tab.y,tab.w,tab.h,names(i),accent = i==settingsTab) }
    val scrollMax = (settingsContentHeight(s)-content.h).max(0f)
    settingsScroll = settingsScroll.max(0f).min(scrollMax)
    val (mx,my) = mouseFramebufferPos()
    glEnable(GL_SCISSOR_TEST)
    glScissor(content.x.toInt,(framebufferHeight-content.y-content.h).toInt,content.w.toInt.max(1),content.h.toInt.max(1))
    var y = content.y-settingsScroll
    settingsEntries.foreach { entry =>
      val rh = entry.height-4f*s
      if y+rh >= content.y && y <= content.y+content.h then
        val hover = content.contains(mx,my) && UiRect(content.x,y,content.w,rh).contains(mx,my)
        rect(content.x,y,content.w,rh,if hover then 0.16f else 0.095f,if hover then 0.22f else 0.13f,if hover then 0.21f else 0.14f,0.94f)
        renderTextShadowFit(content.x+12f*s,y+9f*s,entry.label,0.90f,0.94f,0.90f,1.15f*s,content.w-24f*s)
        val v = entry.id match
          case "rd" => Some((renderDistance-minRenderDistance).toFloat/(maxRenderDistance-minRenderDistance))
          case "fov" => Some((fov-50f)/50f)
          case "fog" => Some((fogDensity-0.6f)/2.4f)
          case "clouds" => Some((cloudRenderDistance-minCloudRenderDistance).toFloat/(maxCloudRenderDistance-minCloudRenderDistance))
          case _ => None
        v.foreach(value => slider(content.x+8f*s,y+31f*s,content.w-16f*s,value))
      y += entry.height
    }
    glDisable(GL_SCISSOR_TEST)
    if scrollMax > 0f then
      val thumbH = (content.h*content.h/settingsContentHeight(s)).max(20f*s)
      val thumbY = content.y+settingsScroll/scrollMax*(content.h-thumbH)
      rect(content.x+content.w+5f*s,content.y,4f*s,content.h,0.06f,0.08f,0.09f,1f)
      rect(content.x+content.w+5f*s,thumbY,4f*s,thumbH,0.48f,0.68f,0.56f,1f)
    val d = layout.done
    drawButton(d.x,d.y,d.w,d.h,"Done",accent = true)

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
    val titleText = "Game Menu"
    val titleScale = 2.0f * s
    val titleX = if multiplayerMode then cx else cx + ((6f * titleScale).max(5f) * titleScale) / 2f
    centeredText(titleX, py + 34 * s, titleText, 1f, 0.95f, 0.55f, titleScale)
    if multiplayerMode then centeredTextFit(cx, py + 60f * s, "Multiplayer session continues while paused", 0.72f, 0.72f, 0.72f, 0.70f * s, pw - 70f * s)
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
    val gridW = pw * 0.58f
    val cols = 9
    val gap = (4f * s).max(3f)
    val slot = ((gridW - gap * (cols - 1)) / cols).min(44f * s).max(30f)
    val invX = px + 28f * s
    val invY = py + 76f * s
    val items = inventoryStacks
    var hoveredBlock: Block | Null = null
    var hoveredX = 0f; var hoveredY = 0f

    rect(invX - 10f * s, invY - 28f * s, gridW + 18f * s, ph - 140f * s, 0.22f, 0.22f, 0.22f, 0.72f)
    renderTextShadow(invX, invY - 22f * s, "Storage", 0.90f, 0.90f, 0.90f, 0.84f * s)
    for i <- 0 until inventoryGridSlots do
      val col = i % cols; val row = i / cols
      val sx = invX + col * (slot + gap); val sy = invY + row * (slot + gap)
      val block = if i < items.length then items(i)._1 else Block.Air
      val count = if i < items.length then items(i)._2 else 0
      val hasItem = validHotbarBlock(block) && count > 0
      val isSelected = false
      val hover = inRect(mx, my, sx, sy, slot, slot)
      if hover && hasItem then
        hoveredBlock = block; hoveredX = sx + slot / 2f; hoveredY = sy
      BlockboxRender2D.drawSlotFrame(sx, sy, slot, isSelected, hover, s)
      if hasItem && count > 0 then
        val icon = (slot * 0.58f).min(26f * s).max(16f)
        renderBlockIcon(block, sx + (slot - icon) / 2f, sy + (slot - icon) / 2f, icon)
        val ns = count.toString
        val countScale = (1.6f * s).max(1.3f).min(slot / 18f)
        val tw = compactNumberWidth(ns, countScale)
        val nx = sx + slot - tw - 5f * s
        val ny = sy + slot - 5f * countScale - 4f * s
        rect(nx - 2f * s, ny - 1f * s, tw + 4f * s, 5f * countScale + 3f * s, 0f, 0f, 0f, 0.60f)
        drawCompactNumber(nx, ny, ns, countScale, 1f, 1f, 1f)

    val craftPanelW = (pw - gridW - 78f * s).max(210f * s)
    val craftX = px + pw - 28f * s - craftPanelW
    val craftY = py + 76f * s
    rect(craftX - 10f * s, craftY - 28f * s, craftPanelW + 18f * s, ph - 140f * s, 0.22f, 0.22f, 0.22f, 0.72f)
    centeredTextFit(craftX + craftPanelW / 2f, craftY - 22f * s, "Crafting", 0.90f, 0.90f, 0.90f, 0.88f * s, craftPanelW)
    centeredTextFit(craftX + craftPanelW / 2f, craftY - 2f * s, "3 x 3 Crafting", 0.62f, 0.62f, 0.62f, 0.64f * s, craftPanelW)
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
      BlockboxRender2D.drawSlotFrame(sx, sy, cSlot, selected = false, hoverCraft, s)
      if b != Block.Air && craftGridCounts(ci) > 0 then
        val icon = (cSlot * 0.60f).min(25f * s).max(16f)
        renderBlockIcon(b, sx + (cSlot - icon) / 2f, sy + (cSlot - icon) / 2f, icon)
        if craftGridCounts(ci) > 1 then
          val ns = craftGridCounts(ci).toString
          val cs = (1.45f * s).max(1.2f).min(cSlot / 18f)
          val tw = compactNumberWidth(ns, cs)
          drawCompactNumber(sx + cSlot - tw - 4f * s, sy + cSlot - 5f * cs - 3f * s, ns, cs, 1f, 1f, 1f)
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
    centeredTextFit(outX + outSize / 2f, outY - 18f * s, "output", 0.62f, 0.66f, 0.74f, 0.66f * s, outSize + 34f * s)
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
    centeredTextFit(cx, infoY, "Choose a block, then choose a hotbar slot", 0.55f, 0.60f, 0.70f, 0.56f * s, panelW - 32f * s)

  protected def blockName(block: Block): String =
    block.displayName.replaceAll("([a-z])([A-Z])", "$1 $2")

  override protected def resolveCraftingRecipe(cells: Array[Block]): Option[CraftRecipe] = modManager.recipes.result(cells)

  private def inventoryStacks: Array[(Block, Int)] =
    Array.tabulate(backpackBlocks.length)(i => (backpackBlocks(i), backpackCounts(i).max(0).min(maxStackSize)))

  private def inventoryGridSlots: Int = backpackBlocks.length

  private def drawInventoryHotbar(panelX: Float, panelY: Float, panelW: Float, s: Float, mx: Float, my: Float): Unit =
    val total = hotbarBlocks.length
    val gap = (5f * s).max(4f)
    val slot = ((panelW - gap * (total - 1)) / total).min(44f * s).max(28f)
    val barW = slot * total + gap * (total - 1)
    val startX = panelX + (panelW - barW) / 2f
    renderTextShadowFit(panelX, panelY - 23f * s, "Hotbar", 0.82f, 0.82f, 0.82f, 0.72f * s, panelW)
    var i = 0
    while i < total do
      val sx = startX + i * (slot + gap)
      val selected = i == selectedBlock
      val hover = inRect(mx, my, sx, panelY, slot, slot)
      val block = hotbarBlocks(i)
      val hasItem = block != Block.Air && (gameMode == GameMode.Creative || hotbarCounts(i) > 0)
      BlockboxRender2D.drawSlotFrame(sx, panelY, slot, selected, hover, s)
      if hasItem then
        val icon = (slot * 0.58f).min(26f * s).max(16f)
        renderBlockIcon(block, sx + (slot - icon) / 2f, panelY + (slot - icon) / 2f, icon)
        if gameMode == GameMode.Survival then
          val ns = hotbarCounts(i).toString
          val cs = (1.8f * s).max(1.4f).min(slot / 18f)
          val tw = compactNumberWidth(ns, cs)
          val tx = (sx + slot - tw - 5f * s).max(sx + 3f * s)
          val ty = panelY + slot - 5f * cs - 4f * s
          rect(tx - 2f * s, ty - 1f * s, tw + 4f * s, 5f * cs + 3f * s, 0f, 0f, 0f, 0.60f)
          drawCompactNumber(tx, ty, ns, cs, 1f, 1f, 1f)
        if hover && heldInventoryBlock == Block.Air then renderTooltip(sx + slot / 2f, panelY - 6f * s, blockName(block))
      centeredTextFit(sx + slot / 2f, panelY + 4f * s, hotbarLabel(i), 0.96f, 0.96f, 0.98f, 0.54f * s, slot - 12f * s)
      i += 1

  private def renderHeldInventoryCursor(mx: Float, my: Float): Unit =
    if heldInventoryBlock != Block.Air then
      val s = uiScale
      val size = (34f * s).max(24f)
      rect(mx + 10f * s, my + 10f * s, size + 8f * s, size + 8f * s, 0f, 0f, 0f, 0.45f)
      renderBlockIcon(heldInventoryBlock, mx + 14f * s, my + 14f * s, size)
      if gameMode == GameMode.Survival && heldInventoryCount > 1 then
        val ns = heldInventoryCount.toString
        val cs = (1.8f * s).max(1.4f).min(size / 16f)
        val tw = compactNumberWidth(ns, cs)
        drawCompactNumber(mx + 14f * s + size - tw - 2f * s, my + 14f * s + size - 5f * cs, ns, cs, 1f, 1f, 1f)
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
    rect(x, y, boxW, boxH, 0.08f, 0.08f, 0.08f, 0.94f)
    rect(x, y, boxW, 1.5f * s, 0.72f, 0.72f, 0.72f, 0.62f)
    renderTextShadow(x + padX, y + padY, text, 0.96f, 0.96f, 0.96f, scale)

  private def catalogItems: Array[Block] =
    Block.all.filter(b => b != Block.Air && b != Block.Water && b != Block.Bedrock && b != Block.FurnaceLit)

  private def renderFurnaceUI(): Unit =
    glDisable(GL_DEPTH_TEST); glDisable(GL_CULL_FACE); setupOrtho()
    dimBackground()
    val cx = framebufferWidth / 2f; val cy = framebufferHeight / 2f; val s = uiScale
    val pw = (680f * s).min(framebufferWidth * 0.94f)
    val ph = (430f * s).min(framebufferHeight * 0.88f)
    val px = cx - pw / 2f; val py = cy - ph / 2f
    drawPanel(px, py, pw, ph)
    centeredTextFit(cx, py + 25f * s, "Furnace", 1f, 1f, 1f, 1.35f * s, pw - 80f * s)
    rect(px + 28f * s, py + 48f * s, pw - 56f * s, 1f, 0.30f, 0.30f, 0.35f, 0.20f)

    val (mx, my) = mouseFramebufferPos()
    val listX = px + 28f * s
    val listY = py + 82f * s
    val listW = (210f * s).min(pw * 0.34f).max(160f * s)
    val rowH = (38f * s).max(30f)
    rect(listX - 8f * s, listY - 34f * s, listW + 16f * s, rowH * smeltableInputs.length + 48f * s, 0.22f, 0.22f, 0.22f, 0.72f)
    centeredTextFit(listX + listW / 2f, listY - 24f * s, "Choose input", 0.90f, 0.90f, 0.90f, 0.66f * s, listW)
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
      rect(x, y, slot, slot, 0.25f, 0.25f, 0.25f, 0.90f)
      rect(x + 2f * s, y + 2f * s, slot - 4f * s, 2f * s, 0.52f, 0.52f, 0.52f, 0.16f)
      centeredTextFit(x + slot / 2f, y + slot + 12f * s, label, 0.72f, 0.72f, 0.72f, 0.52f * s, slot + 30f * s)
      if block != Block.Air && count > 0 then
        val icon = (slot * 0.58f).max(20f)
        renderBlockIcon(block, x + (slot - icon) / 2f, y + (slot - icon) / 2f, icon)
        val c = s"x$count"
        val cs = 0.52f * s
        val tw = textWidth(c, cs)
        rect(x + slot - tw - 7f * s, y + slot - 14f * s, tw + 5f * s, 12f * s, 0f, 0f, 0f, 0.62f)
        renderText(x + slot - tw - 5f * s, y + slot - 13f * s, c, 1f, 1f, 1f, cs)
      else
        centeredTextFit(x + slot / 2f, y + slot / 2f - 5f * s, "empty", 0.46f, 0.46f, 0.46f, 0.48f * s, slot - 8f * s)

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
    rect(workX + 8f * s, fuelY, workW - 16f * s, 58f * s, 0.22f, 0.22f, 0.22f, 0.72f)
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
    var i = 0
    while i < inventoryGridSlots.min(6) do
      val y = listY + i * rowH
      if inRect(mx, my, listX, y, listW, rowH - 4f * s) then
        if i < furnaceItems.length then
          val b = furnaceItems(i)
          furnaceInput = b
          furnaceProgress = 0f
        else furnaceInput = Block.Air
        return
      i += 1

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
      val countScale = (2.0f * s).max(1.5f).min(size / 16f)
      val tw = compactNumberWidth(ns, countScale); val nx = x + size - tw - 5f * s; val ny = y + size - 5f * countScale - 4f * s
      rect(nx - 2f * s, ny - 1f * s, tw + 4f * s, 5f * countScale + 3f * s, 0f, 0f, 0f, 0.60f)
      drawCompactNumber(nx, ny, ns, countScale, 1f, 1f, 1f)

  private def drawPanel(x: Float, y: Float, w: Float, h: Float): Unit =
    BlockboxRender2D.drawPanel(x, y, w, h, uiScale)

  private def uiScale: Float =
    val maxByViewport = math.min(framebufferWidth.toFloat / 960f, framebufferHeight.toFloat / 600f).max(0.45f)
    val requested =
      guiScaleSetting match
        case 0 => math.min(framebufferWidth.toFloat / 1280f,framebufferHeight.toFloat / 720f).max(0.5f).min(1.5f)
        case 1 => 1.00f
        case 2 => 1.50f
        case 3 => 2.00f
        case 4 => 3.00f
        case _ => 4.00f
    requested.min(maxByViewport).max(0.45f)

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

  private def renderTextShadowFit(x: Float, y: Float, text: String, r: Float, g: Float, b: Float, scale: Float, maxWidth: Float): Unit =
    BlockboxRender2D.renderTextShadowFit(x, y, text, r, g, b, scale, maxWidth, framebufferWidth, framebufferHeight)

  private def setupOrtho(): Unit =
    BlockboxRender2D.setupOrtho(framebufferWidth, framebufferHeight)

  private def onOff(value: Boolean): String = if value then "ON" else "OFF"

  private def changeRenderDistance(delta: Int): Unit =
    val next = (renderDistance + delta).max(minRenderDistance).min(maxRenderDistance)
    if next != renderDistance then
      renderDistance = next
      markSettingsDirty()
      // Keep existing chunk objects/meshes. Only stream in newly visible chunks and
      // unload chunks that fall outside the new radius. This avoids the old full-world
      // rebuild storm when tapping +/- or dragging the slider quickly.
      syncChunks()

  private def chunkWorkPriority(cx: Int, cz: Int): Int =
    val centerX = cx * Terrain.chunkSize + Terrain.chunkSize * 0.5f
    val centerZ = cz * Terrain.chunkSize + Terrain.chunkSize * 0.5f
    val dx = centerX - camera.x
    val dz = centerZ - camera.z
    val distSq = dx * dx + dz * dz
    val dist = math.sqrt(distSq.toDouble).toFloat.max(0.001f)
    val fx = sin(toRadians(yaw)).toFloat
    val fz = -cos(toRadians(yaw)).toFloat
    val facing = (dx * fx + dz * fz) / dist
    val behindPenalty = if facing < -0.25f then Terrain.chunkSize * Terrain.chunkSize * 10 else if facing < 0.15f then Terrain.chunkSize * Terrain.chunkSize * 3 else 0
    (distSq + behindPenalty).min(Int.MaxValue.toFloat).toInt

  private def chunkUploadBudget: Int =
    val ms = (lastFrameTime * 1000.0).toFloat
    if ms <= 0.01f then 10
    else if ms >= 28f then 2
    else if ms >= 22f then 3
    else if ms >= 18f then 5
    else if ms >= 14f then 8
    else 12

  private def queueChunkMesh(chunk: Chunk): Unit =
    if !chunk.isDisposed && chunk.tryQueueMesh() then
      chunkBuildQueue.add(ChunkMeshJob(chunk, chunkWorkPriority(chunk.cx, chunk.cz), chunkJobSerial.incrementAndGet()))

  private def connectChunkNeighbors(chunk: Chunk): Unit =
    var dx = -1
    while dx <= 1 do
      var dz = -1
      while dz <= 1 do
        if dx != 0 || dz != 0 then
          chunks.get((chunk.cx + dx, chunk.cz + dz)).foreach { neighbor =>
            if neighbor ne chunk then
              chunk.setNeighbor(dx, dz, neighbor)
              neighbor.setNeighbor(-dx, -dz, chunk)
              neighbor.markDirtyMesh()
              neighbor.meshReady = false
              queueChunkMesh(neighbor)
          }
        dz += 1
      dx += 1
    if gameClient==null then
      for ox <- -1 to 1; oz <- -1 to 1; candidate <- chunks.get((chunk.cx+ox,chunk.cz+oz)) do
        reconcileChunkTorches(candidate)

  private def reconcileChunkTorches(chunk: Chunk): Unit =
    def loadedAt(x: Int,y: Int,z: Int): Option[Block] =
      if y<0 || y>=Terrain.worldHeight then Some(Block.Air)
      else loadedChunkForBlock(x,z).map(_.getBlock(Math.floorMod(x,Terrain.chunkSize),y,Math.floorMod(z,Terrain.chunkSize)))
    val oldMounts=chunk.torchPositions.map(p => p -> chunk.getTorchMount(p._1,p._2,p._3)).toMap
    chunk.migrateLegacyTorches(loadedAt)
    chunk.torchPositions.foreach { case (lx,y,lz) =>
      val x=chunk.baseX+lx; val z=chunk.baseZ+lz
      val support=chunk.getTorchMount(lx,y,lz).support
      if oldMounts.get((lx,y,lz)).exists(_!=chunk.getTorchMount(lx,y,lz)) then
        dirtyChunkAt(x,z)
        sendBlockNetwork(x,y,z,Block.Torch.id)
      if chunk.torchAttachmentResolved(lx,y,lz) && loadedAt(x+support._1,y+support._2,z+support._3).exists(b => !TorchMount.canSupport(b)) then
        setActiveBlock(x,y,z,Block.Air)
        refundTorch(None)
        sendBlockNetwork(x,y,z,Block.Air.id)
    }

  private def disconnectChunkNeighbors(chunk: Chunk): Unit =
    var dx = -1
    while dx <= 1 do
      var dz = -1
      while dz <= 1 do
        if dx != 0 || dz != 0 then
          chunks.get((chunk.cx + dx, chunk.cz + dz)).foreach { neighbor =>
            neighbor.clearNeighbor(-dx, -dz, chunk)
            neighbor.markDirtyMesh()
            neighbor.meshReady = false
            queueChunkMesh(neighbor)
          }
        dz += 1
      dx += 1

  private def requestChunkCreate(cx: Int, cz: Int): Boolean =
    val key = (cx, cz)
    if chunks.contains(key) || pendingChunkCreates.contains(key) then false
    else if pendingChunkCreates.add(key) then
      // Capture the save directory path only; workers perform existence checks and reads.
      // This keeps Files.exists/open/read completely off the render thread during streaming.
      val savedDir = if canUseLocalChunkSaves then Some(currentChunksDir) else None
      chunkCreateQueue.add(ChunkCreateJob(cx, cz, chunkStreamGeneration, chunkWorkPriority(cx, cz), chunkJobSerial.incrementAndGet(), savedDir))
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
            connectChunkNeighbors(chunk)
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
        val axis = if Block.isLog(block) then LogAxis.fromId(networkLogAxesOverrides.getOrElse((wx, wy, wz), 0)) else LogAxis.Y
        chunk.setBlock(lx, wy, lz, block, axis,TorchMount.fromId(networkTorchMounts.getOrElse((wx,wy,wz),0)))
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
        pendingNetworkLogAxes.remove((wx, wy, wz)).foreach { axisId =>
          if Block.isLog(block) && axisId != 0 then networkLogAxesOverrides((wx, wy, wz)) = axisId
        }
        applyOne(wx, wy, wz, block)
      }
    }
    if changed then refreshChunkAndNeighbors(cx, cz)

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
            // Saved-chunk reads and procedural generation both happen on chunk workers.
            // The old path did Files.exists + full chunk reads here on the frame thread.
            if requestChunkCreate(cx, cz) then loaded += 1
          else if chunks.contains((cx, cz)) then
            val chunk = chunks((cx, cz))
            if !chunk.hasMesh && !chunk.meshReady then
              queueChunkMesh(chunk)
    // Retain a small halo beyond the actual load/draw radius. Crossing a chunk
    // boundary should not immediately serialize and destroy the chunk behind you,
    // only to load it again two seconds later when you turn around. This dramatically
    // reduces save I/O and mesh churn during normal exploration.
    val retainRadius = worldRadius + Terrain.chunkSize * 2
    val retainRadiusSq = retainRadius * retainRadius
    val cameraBlockX = ccx * Terrain.chunkSize
    val cameraBlockZ = ccz * Terrain.chunkSize
    val toRemove = chunks.keys.filter { case (cx, cz) =>
      val wx = cx * Terrain.chunkSize + Terrain.chunkSize / 2
      val wz = cz * Terrain.chunkSize + Terrain.chunkSize / 2
      val dx = wx - cameraBlockX; val dz = wz - cameraBlockZ
      dx * dx + dz * dz > retainRadiusSq
    }.toList
    val preparedForUnload =
      if toRemove.nonEmpty && canUseLocalChunkSaves then
        try Some(BlockboxSaveFiles.prepareWorldForSave(currentWorldDir.toPath))
        catch
          case e: Exception =>
            System.err.println(s"Chunk unload save preparation failed: $e")
            None
      else None
    toRemove.foreach { key =>
      chunks.get(key).foreach { chunk =>
        preparedForUnload.foreach { prepared =>
          // Unloading a chunk must never turn a frame into a filesystem stall. Capture
          // immutable state while the chunk is still alive, then encode/write it on the
          // same ordered save worker used by Save and Quit.
          val snapshot = chunk.captureSaveSnapshot()
          val (saveCx, saveCz) = key
          dirtyChunksForSave -= key
          queueWorldSaveJob(s"${prepared.worldDirectory().getFileName} chunk $saveCx,$saveCz") {
            BlockboxSaveFiles.writeChunkBytesAtomic(prepared.chunksDirectory(), saveCx, saveCz, snapshot.toBytes(), false)
          }
        }
        disconnectChunkNeighbors(chunk)
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
                val chunk = createKey.savedChunksDir match
                  case Some(dir) =>
                    val saved = BlockboxSaveFiles.chunkFile(dir.toPath, createKey.cx, createKey.cz).toFile
                    if saved.isFile then
                      try
                        val emptyBlocks = new Array[Byte](Terrain.chunkSize * Terrain.worldHeight * Terrain.chunkSize)
                        val loaded = Chunk(createKey.cx, createKey.cz, activeAtlas, terrainGen, emptyBlocks, smoothLighting)
                        loaded.load(dir)
                        loaded
                      catch
                        case loadError: Exception =>
                          System.err.println(s"Corrupt chunk ignored ${createKey.cx},${createKey.cz}: $loadError")
                          try saved.renameTo(new java.io.File(dir, BlockboxSaveFiles.chunkFileName(createKey.cx, createKey.cz) + ".bad")) catch case _: Exception => ()
                          Chunk(createKey.cx, createKey.cz, activeAtlas, terrainGen, smoothLightingEnabled = smoothLighting)
                    else Chunk(createKey.cx, createKey.cz, activeAtlas, terrainGen, smoothLightingEnabled = smoothLighting)
                  case None => Chunk(createKey.cx, createKey.cz, activeAtlas, terrainGen, smoothLightingEnabled = smoothLighting)
                if chunkGenRunning && createKey.generation == chunkStreamGeneration then chunkReadyQueue.add((createKey.generation, chunk))
                else chunk.dispose()
              catch case e: Exception =>
                pendingChunkCreates.remove((createKey.cx, createKey.cz))
                e.printStackTrace()
              true
          def processMesh(): Boolean =
            val job = chunkBuildQueue.poll()
            if job == null then false
            else
              val chunk = job.chunk
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
      val meshJob = chunkBuildQueue.poll()
      if meshJob == null then built = buildLimit
      else
        val chunk = meshJob.chunk
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

  private def raycast(maxDistance: Float = modManager.rules.get("reach")): Option[RayHit] =
    BlockboxRaycast.raycast(camera, viewDirection, maxDistance, activeBlockAt,torchMountAt)

  private def sendBlockNetwork(x: Int, y: Int, z: Int, blockId: Byte, logAxis: Int = 0): Unit =
    if multiplayerMode then
      val block = Block.fromId(blockId)
      val water = if block==Block.Water then waterLevelAt(x,y,z).max(1).min(8) else 0
      val mount = if block==Block.Torch then torchMountAt(x,y,z).id else 0
      networkBlockOverrides((x,y,z))=block
      if block==Block.Water then networkWaterLevelOverrides((x,y,z))=water.toByte else networkWaterLevelOverrides.remove((x,y,z))
      if Block.isLog(block) && logAxis!=0 then networkLogAxesOverrides((x,y,z))=logAxis else networkLogAxesOverrides.remove((x,y,z))
      if block==Block.Torch then networkTorchMounts((x,y,z))=mount else networkTorchMounts.remove((x,y,z))
      val msg = BlockboxNet.block(x,y,z,blockId.toInt,water,logAxis,mount)
      if gameClient!=null && gameClient.isConnected then gameClient.send(msg)
      else if gameServer!=null then gameServer.broadcastBlockChange(x,y,z,blockId,water,logAxis,mount)

  private def breakTargetBlock(dropItem: Boolean = true): Unit =
    raycast() match
      case Some(hit) =>
        val (x, y, z) = hit.block
        val block = activeBlockAt(x, y, z)
        if pendingPlayerEdits.contains((x,y,z)) || pendingPlayerEdits.size>=128 then return
        val initial=ModBlockBreakEvent(modManager.api,playerName,x,y,z,block,cancelled=false,dropItem=dropItem)
        val event=if isClientOnlyMultiplayer then initial else modManager.fireBlockBreak(initial)
        if !event.cancelled then
          val (loot,quantity)=modManager.drops.getOrElse(block,(block,1))
          val needsDropRoom = event.dropItem && gameMode == GameMode.Survival && loot != Block.Air && block != Block.Water
          if needsDropRoom && inventoryRoomFor(loot) < quantity then
            addChatMessage("Inventory full")
          else
            if event.dropItem && block != Block.Air && block != Block.Water && !isClientOnlyMultiplayer then gainItem(loot, quantity)
            if isClientOnlyMultiplayer then pendingPlayerEdits((x,y,z))=(Block.Air,0,event.dropItem && gameMode==GameMode.Survival)
            if block != Block.Air && block != Block.Water then spawnBlockBreakParticles(x, y, z, block)
            setActiveBlock(x, y, z, Block.Air)
            dirtyChunkAt(x,z)
            triggerSandFallAbove(x, y, z)
            playBreakSound(block)
            viewmodelKick = 1f
            sendBlockNetwork(x, y, z, 0)
      case None => ()

  private def placeTargetBlock(skipModInteraction: Boolean=false): Unit =
    raycast() match
      case Some(hit) =>
        val (bx, by, bz) = hit.block
        val targetBlock = activeBlockAt(bx, by, bz)
        if !skipModInteraction && modManager.events.hasListeners("blockInteract") then
          if isClientOnlyMultiplayer then
            gameClient.send(s"USE|$bx|$by|$bz")
            return
          val interaction=modManager.events.fireBlockInteract(ModInteractionEvent(modManager.api,playerName,bx,by,bz,targetBlock))
          if interaction.cancelled || interaction.handled then return
        if targetBlock == Block.Furnace then
          openFurnace()
        else
          val (x, y, z) = hit.place
          if pendingPlayerEdits.contains((x,y,z)) || pendingPlayerEdits.size>=128 then return
          val block = hotbarBlocks(selectedBlock.max(0).min(hotbarBlocks.length - 1))
          val hasBlock = isPlaceableInventoryBlock(block) && (gameMode == GameMode.Creative || hotbarCounts(selectedBlock.max(0).min(hotbarCounts.length - 1)) > 0)
          if hasBlock && canPlaceBlockAt(x, y, z,block) then
            val initial=ModBlockPlaceEvent(modManager.api,playerName,x,y,z,block,cancelled=false)
            val event=if isClientOnlyMultiplayer then initial else modManager.fireBlockPlace(initial)
            if !event.cancelled then
              val finalBlock = event.block
              val mount = TorchMount.fromNormal(hit.normal)
              if canPlaceBlockAt(x,y,z,finalBlock) && (finalBlock!=Block.Torch || mount.exists(_.supported(x,y,z,activeBlockAt))) then
                val placedAxis = if Block.isLog(finalBlock) then LogAxis.fromNormal(hit.normal) else LogAxis.Y
                setActiveBlock(x, y, z, finalBlock, placedAxis,mount.getOrElse(TorchMount.Floor))
                if isClientOnlyMultiplayer then pendingPlayerEdits((x,y,z))=(block,if gameMode==GameMode.Survival then 1 else 0,false)
                if gameMode == GameMode.Survival then consumeSelectedHotbar(1)
                dirtyChunkAt(x,z)
                playPlaceSound(finalBlock)
                fieldJournal.placedBlock()
                viewmodelKick = 0.72f
                sendBlockNetwork(x, y, z, finalBlock.ordinal.toByte, placedAxis.id)
      case None => ()

  private def blockIntersectsPlayerBody(x: Int, y: Int, z: Int, pos: Vec3): Boolean =
    val half = 0.36f
    val minX = floor(pos.x - half).toInt; val maxX = floor(pos.x + half).toInt
    val minY = floor(pos.y - 1.70f).toInt; val maxY = floor(pos.y + 0.22f).toInt
    val minZ = floor(pos.z - half).toInt; val maxZ = floor(pos.z + half).toInt
    x >= minX && x <= maxX && y >= minY && y <= maxY && z >= minZ && z <= maxZ

  private def placementBlockedByAnyPlayer(x: Int, y: Int, z: Int): Boolean =
    blockIntersectsPlayerBody(x, y, z, camera) ||
      remotePlayers.values.exists(rp => glfwGetTime() - rp.lastSeen <= 8.0 && blockIntersectsPlayerBody(x, y, z, rp.pos))

  private def canPlaceBlockAt(x: Int, y: Int, z: Int, block: Block = Block.Stone): Boolean =
    y>=0 && y<Terrain.worldHeight && loadedChunkForBlock(x,z).nonEmpty && (activeBlockAt(x,y,z)==Block.Air || activeBlockAt(x,y,z)==Block.Water) && (!block.solid || !placementBlockedByAnyPlayer(x, y, z))

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
      glDisable(GL_TEXTURE_2D)
      // Keep the selection outline visible through tiny depth precision errors without
      // making it visible through walls. The old code disabled depth testing entirely.
      glEnable(GL_DEPTH_TEST); glDepthMask(false)
      val bounds = if activeBlockAt(x,y,z)==Block.Torch then TorchModel.bounds(torchMountAt(x,y,z)) else TorchModel.Bounds(0,0,0,1,1,1)
      val e = 0.004f; val x0 = x-camera.x+bounds.x0-e; val x1 = x-camera.x+bounds.x1+e
      val y0 = y+bounds.y0-e; val y1 = y+bounds.y1+e; val z0 = z-camera.z+bounds.z0-e; val z1 = z-camera.z+bounds.z1+e
      glLineWidth(3f); glBegin(GL_LINES)
      glColor4f(0f, 0f, 0f, 0.55f)
      lineEdges(x0, y0, z0, x1, y1, z1)
      glEnd()
      glLineWidth(1.6f); glBegin(GL_LINES)
      glColor4f(1f, 1f, 1f, 0.82f)
      lineEdges(x0, y0, z0, x1, y1, z1)
      glEnd(); glLineWidth(1f)
      if breakingBlock != null && breakingBlock == hit.block && breakingProgress > 0f then
        renderBreakingCracks(hit, breakingProgress.max(0f).min(1f))
      // World overlays are an explicit render-state boundary. Do not make the next
      // pass repair selection/crack state by accident; that kind of state leak was
      // responsible for several of the earlier viewmodel/HUD rendering glitches.
      glDepthMask(true)
      glDisable(GL_BLEND)
      glColor4f(1f, 1f, 1f, 1f)
      glLineWidth(1f)
      glEnable(GL_TEXTURE_2D)
    }

  private def renderBreakingCracks(hit: RayHit, progress: Float): Unit =
    val (bx, by, bz) = hit.block
    val (nx, ny, nz) = hit.normal
    val stage = (progress * 10f).toInt.max(1).min(10)
    val eps = 0.008f
    def surfacePoint(u: Float, v: Float): (Float, Float, Float) =
      if nx != 0 then
        (bx.toFloat - camera.x + (if nx > 0 then 1f + eps else -eps), by.toFloat + v, bz.toFloat - camera.z + u)
      else if ny != 0 then
        (bx.toFloat - camera.x + u, by.toFloat + (if ny > 0 then 1f + eps else -eps), bz.toFloat - camera.z + v)
      else
        (bx.toFloat - camera.x + u, by.toFloat + v, bz.toFloat - camera.z + (if nz > 0 then 1f + eps else -eps))
    val centers = Array((0.50f,0.50f), (0.31f,0.62f), (0.67f,0.37f))
    glEnable(GL_BLEND); glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA)
    glLineWidth((1.0f + progress * 1.4f).min(2.4f))
    glBegin(GL_LINES)
    var branch = 0
    val branchCount = 4 + stage * 2
    while branch < branchCount do
      val center = centers(branch % centers.length)
      val a = branch.toFloat * 2.3999632f + bx * 0.17f + bz * 0.11f
      val len = 0.10f + 0.034f * stage + (branch % 3) * 0.022f
      val bend = sin((branch * 17 + stage * 11).toDouble).toFloat * 0.055f
      val u0 = center._1 + cos(a).toFloat * 0.025f
      val v0 = center._2 + sin(a).toFloat * 0.025f
      val u1 = (center._1 + cos(a + bend).toFloat * len).max(0.035f).min(0.965f)
      val v1 = (center._2 + sin(a - bend).toFloat * len).max(0.035f).min(0.965f)
      val p0 = surfacePoint(u0, v0); val p1 = surfacePoint(u1, v1)
      glColor4f(0.03f, 0.03f, 0.035f, 0.48f + progress * 0.42f)
      glVertex3f(p0._1,p0._2,p0._3); glVertex3f(p1._1,p1._2,p1._3)
      if stage >= 5 && (branch & 1) == 0 then
        val sideA = a + 1.1f + (branch % 4) * 0.17f
        val su = (u1 + cos(sideA).toFloat * len * 0.42f).max(0.035f).min(0.965f)
        val sv = (v1 + sin(sideA).toFloat * len * 0.42f).max(0.035f).min(0.965f)
        val p2 = surfacePoint(su, sv)
        glVertex3f(p1._1,p1._2,p1._3); glVertex3f(p2._1,p2._2,p2._3)
      branch += 1
    glEnd(); glLineWidth(1f)

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
    val file = BlockboxSaveFiles.chunkFile(chunksDir.toPath, cx, cz).toFile
    if file.exists() then
      try
        val emptyBlocks = new Array[Byte](Terrain.chunkSize * Terrain.worldHeight * Terrain.chunkSize)
        val chunk = Chunk(cx, cz, activeAtlas, terrainGen, emptyBlocks, smoothLighting)
        chunk.load(chunksDir)
        chunks((cx, cz)) = chunk
        connectChunkNeighbors(chunk)
        initChunkWaterLevels(cx, cz)
        applyNetworkBlocksToChunk(cx, cz)
        true
      catch
        case e: Exception =>
          System.err.println(s"Corrupt chunk ignored $cx,$cz: $e")
          try file.renameTo(new java.io.File(chunksDir, BlockboxSaveFiles.chunkFileName(cx, cz) + ".bad")) catch case _: Exception => ()
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

  private def dirtyLightChunksAt(x: Int, z: Int): Unit =
    dirtyChunksForSave += ((chunkCoordBlock(x), chunkCoordBlock(z)))
    val radius = 14
    val minCx = chunkCoordBlock(x - radius)
    val maxCx = chunkCoordBlock(x + radius)
    val minCz = chunkCoordBlock(z - radius)
    val maxCz = chunkCoordBlock(z + radius)
    var cx = minCx
    while cx <= maxCx do
      var cz = minCz
      while cz <= maxCz do
        dirtyChunks += ((cx, cz))
        chunks.get((cx, cz)).foreach { c =>
          c.markDirtyMesh()
          c.meshReady = false
          queueChunkMesh(c)
        }
        cz += 1
      cx += 1

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

  private def setFogColor(r: Float, g: Float, b: Float): Unit =
    fogColorBuffer.clear(); fogColorBuffer.put(r).put(g).put(b).put(1f); fogColorBuffer.flip()
    glFogfv(GL_FOG_COLOR, fogColorBuffer)

  private def updateFramebufferSize(): Unit =
    glfwGetFramebufferSize(window, framebufferWidthBuffer, framebufferHeightBuffer)
    framebufferWidth = framebufferWidthBuffer.get(0).max(1)
    framebufferHeight = framebufferHeightBuffer.get(0).max(1)


  def modHasAuthority: Boolean = !isClientOnlyMultiplayer
  def modWorldActive: Boolean=activeWorldSession
  def modWorldDirectory: java.nio.file.Path=currentWorldDir.toPath
  def modConfigDirectory: java.nio.file.Path=gameRootDir.toPath.resolve("config/blockbox-mods")
  def modShaderPacks: Vector[String]=Option(shaderPipeline).map(_.availablePackNames).getOrElse(Vector.empty)
  def modSelectShaderPack(name: String): Boolean=
    if shaderPipeline==null then false
    else
      if !shaderPipeline.available then shaderPipeline.initialize()
      val applied=shaderPipeline.selectPack(name)
      if applied then shadersEnabled=true
      applied
  def modShaderUniform(name: String,value: Float): Unit=
    require(shaderPipeline!=null,"Shader uniforms are available after graphics initialization")
    shaderPipeline.setCustomUniform(name,value)
  def modRulesChanged(): Unit =
    publishedRules=modManager.rules.snapshot
    if gameServer!=null then gameServer.broadcast("RULES|"+publishedRules)
  def modPlayerPosition(name: String): Vec3=positionOfPlayer(name).getOrElse(throw IllegalArgumentException("Unknown player: "+name))
  def modTeleportPlayer(name: String,pos: Vec3): Unit=
    require(modHasAuthority,"Teleport players on the host")
    require(pos!=null && Seq(pos.x,pos.y,pos.z).forall(_.isFinite),"Position must be finite")
    teleportPlayer(name,pos)
  def modGivePlayer(name: String,block: Block,count: Int): Unit=
    require(modHasAuthority,"Give inventory rewards on the host")
    require(block!=null && CustomBlocks.registered(block),"Unknown item")
    if count>0 then giveItemForPlayer(name,block,count.min(999))
  def modGetBlock(x: Int, y: Int, z: Int): Block = activeBlockAt(x, y, z)
  def modSetBlock(x: Int, y: Int, z: Int, block: Block): Unit =
    require(modHasAuthority,"World mutations must run on the host")
    val safe = if block == null then Block.Air else block
    require(CustomBlocks.registered(safe) && y>0 && y<Terrain.worldHeight,"Invalid block edit")
    val mount=if safe==Block.Torch then TorchMount.values.find(_.supported(x,y,z,activeBlockAt)) else Some(TorchMount.Floor)
    mount.foreach { attachment =>
      setActiveBlock(x, y, z, safe,LogAxis.Y,attachment)
      dirtyChunkAt(x, z)
      sendBlockNetwork(x, y, z, safe.ordinal.toByte)
    }
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
      setCursorCaptured(captured = !enabled)
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
