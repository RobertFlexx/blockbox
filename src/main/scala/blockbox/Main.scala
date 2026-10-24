//> using scala "3.8.3"
//> using dep "org.lwjgl:lwjgl:3.4.1"
//> using dep "org.lwjgl:lwjgl-glfw:3.4.1"
//> using dep "org.lwjgl:lwjgl-opengl:3.4.1"
//> using dep "org.lwjgl:lwjgl-stb:3.4.1"
//> using dep "org.lwjgl:lwjgl-vulkan:3.4.1"
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
import org.lwjgl.glfw.GLFWWindowFocusCallback
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
  private var playerCrouching = false
  private var crouchAmount = 0f
  private var lastNetworkCrouching = false
  private def eyePosition: Vec3 = PlayerStance.eye(camera, crouchAmount)
  private var viewFovOffset = 0f
  private var cameraBobPhase = 0f
  private var cameraBobAmount = 0f
  private var cameraRoll = 0f
  private var smoothedHorizontalSpeed = 0f
  private val walkingGait = WalkingGait()
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
  private val pendingPlayerEdits =
    scala.collection.mutable.HashMap.empty[(Int, Int, Int), (Block, Int, Boolean)]
  // Weather is presentation/game-state, not a post-process fake. The shader bridge
  // receives the same rain/wetness values used by the world renderer so packs can
  // darken porous surfaces, soften sunlight and add their own storm treatment.
  private var weatherEnabled = true
  private var rainStrength = 0f
  private var wetness = 0f
  private var weatherWind = 0f
  private var stormCloudCover = 0f
  private var lightningFlash = 0f
  private var localSnowfall = false
  private var snowCover = 0f
  private var localRain = 0f
  private var localCloud = 0f
  private var localWetness = 0f
  private var localLightning = 0f
  private var weatherCells = Vector.empty[WeatherField.Cell]
  private var weatherStrikes = Vector.empty[WeatherField.Strike]
  private val heardThunder = scala.collection.mutable.HashSet.empty[Long]
  private var shelterTimer = 0f
  private var rainShelter = 1f
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
  private var miningSoundCooldown = 0f
  private var miningSoundVariant = 0
  protected val placeableBlocks = Array(
    Block.Grass,
    Block.Dirt,
    Block.Stone,
    Block.Sand,
    Block.Cactus,
    Block.Wood,
    Block.Planks,
    Block.BirchPlanks,
    Block.PinePlanks,
    Block.AcaciaPlanks,
    Block.Leaves,
    Block.BirchWood,
    Block.BirchLeaves,
    Block.PineWood,
    Block.PineLeaves,
    Block.AcaciaWood,
    Block.AcaciaLeaves,
    Block.Brick,
    Block.Glass,
    Block.Snow,
    Block.Clay,
    Block.Coal,
    Block.Copper,
    Block.IronOre,
    Block.GoldOre,
    Block.Diamond,
    Block.RainbowBlock,
    Block.Furnace,
    Block.Torch,
    Block.Reeds,
    Block.LilyPad,
    Block.LeafLitter,
    Block.SupplyCrate,
    Block.GrassTuft,
    Block.Fern,
    Block.Wildflower,
    Block.DryShrub
  )
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
  private var catalogCategory = 0
  private var catalogQuery = ""
  private var catalogSearchFocused = false
  private var craftingScroll = 0
  private var furnaceScroll = 0
  protected val craftGridBlocks: Array[Block] = Array.fill(9)(Block.Air)
  protected val craftGridCounts: Array[Int] = Array.fill(9)(0)
  protected var furnaceInput: Block = Block.Air
  protected var furnaceFuel: Block = Block.Air
  protected var furnaceProgress = 0f
  protected var furnaceFuelRemaining = 0f
  protected var furnaceOutput: Block = Block.Air
  protected var furnaceOutputCount = 0
  private var furnacePosition = Option.empty[(Int, Int, Int)]
  private var furnaceBurning = false
  private val remoteFurnaces = scala.collection.mutable.HashMap.empty[String, (Int, Int, Int)]
  private var nearbyFurnaceSounds = Array.empty[Vec3]
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
  private val pendingNetworkBlocks = new ChunkBlockMap[Block]
  // Every BLOC packet seen during a multiplayer session is kept here, even after the
  // target chunk unloads. Without this, client-only worlds regenerated from seed and
  // lost remote edits when walking away and returning.
  private val networkBlockOverrides = new ChunkBlockMap[Block]
  private val networkWaterLevelOverrides =
    scala.collection.mutable.HashMap.empty[(Int, Int, Int), Byte]
  // Minecraft-style log orientation (LogAxis id) for network block changes. Kept in
  // sync with the override maps above, so remote log edits keep their facing even
  // after a chunk streams out and returns.
  private val pendingNetworkLogAxes = scala.collection.mutable.HashMap.empty[(Int, Int, Int), Int]
  private val networkLogAxesOverrides = scala.collection.mutable.HashMap.empty[(Int, Int, Int), Int]
  private val networkTorchMounts = scala.collection.mutable.HashMap.empty[(Int, Int, Int), Int]
  private val worldExtraMagic = 0x42425831 // "BBX1": optional tail after old world.dat fields
  private val worldExtraVersion = 4
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
  private var lastNetworkHeldBlock = -1
  private var windowFocused = true
  private var ignoreInputUntil = 0.0
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
  private val reflectionChunkScratch = ArrayBuffer.empty[Chunk]
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
  private val zombies = ArrayBuffer.empty[ZombieSystem.Zombie]
  private val zombieRng = Random(0x20b1e5L)
  private val zombiePlanning = blockbox.ai.AiWorkBudget(48.0, 3)
  private var zombieSerial = 0
  private var zombieSpawnTimer = 2f
  private var zombieSyncTimer = 0f
  private var combatCooldown = 0f
  private val lastCombatHit = scala.collection.mutable.HashMap.empty[String, Double]
  private var playerKnockback = Vec3(0, 0, 0)
  private val droppedItems = DroppedItems()
  private var itemSyncTimer = 0f
  private var itemSnapshotSerial = 0L
  private var itemIncomingSerial = -1L
  private var itemIncomingCount = 0
  private val itemIncomingRows = ArrayBuffer.empty[String]
  private var pickupRequestTimer = 0f
  private val appliedItemOffers = collection.mutable.HashSet.empty[Long]
  private val pendingTosses = collection.mutable.HashMap.empty[Long, (Block, Int)]
  private val failedTosses = collection.mutable.HashSet.empty[Long]
  private final case class GuestCarry(
      bytes: Array[Byte],
      journal: FieldJournal,
      applied: Set[Long],
      pending: Map[Long, (Block, Int)],
      failed: Set[Long]
  )
  private val guestCarry = collection.mutable.LinkedHashMap.empty[String, GuestCarry]
  private var guestSessionKey = ""
  private var environmentTimeError = 0.0

  private val entityWaterPlane = BufferUtils.createDoubleBuffer(4)
  private val airSupply = AirSupply()
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
  private enum TextBox:
    case Chat, Catalog, WorldName, Seed, PlayerName, Address
  private val textEditors = collection.mutable.HashMap.empty[TextBox, TextLine]
  private final case class TextInputView(bounds: UiRect, scale: Float, start: Int, columns: Int)
  private val textInputViews = collection.mutable.HashMap.empty[TextBox, TextInputView]
  private var textMouseWasDown = false
  private var textDrag: Option[TextBox] = None
  private var textCaretReset = 0.0
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
  private val grassGrowthRng = Random(0xb10c80L)
  private val leafDecay = LeafDecay()
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
      block: Block,
      var pos: Vec3,
      var velocity: Vec3,
      var life: Float,
      maxLife: Float,
      size: Float,
      face: FaceKind,
      u0: Float,
      v0: Float,
      u1: Float,
      v1: Float,
      var rotation: Float,
      spin: Float,
      gravity: Float,
      drag: Float,
      bounce: Float,
      brightness: Float,
      tint: (Float, Float, Float) = (1f, 1f, 1f)
  )
  private val blockParticles = ArrayBuffer.empty[BlockParticle]
  private val blockParticleRng = Random(0xb10cb0a7L)
  private val maxBlockParticles = 360
  private val sandParticleLifetime = 0.25f
  private var sandParticleTimer = 0f
  protected var timeOverride: Option[Float] = None
  private var worldClock = 150.0
  private var environmentReceived = false
  private var lastEnvironmentSend = -10.0
  @volatile private var publishedRules = ""
  @volatile private var publishedEnvironment = "ENV|150|-1|0|0|0|0|0"
  @volatile private var publishedOps = Vector.empty[String]
  private var lastSwingSend = -10.0
  private var fieldJournal = FieldJournal()
  private var journalPage = 0
  private var journalToast = ""
  private var journalToastTime = 0f
  private var journalTimer = 0f
  protected var flyEnabled = false
  private var lastSpacePressTime = 0.0
  private val doubleTapInterval = 0.3
  private var wasSpaceDown = false
  private val bubbleParticles = ArrayBuffer.empty[(Float, Float, Float, Float)]
  private val bubbleRng = Random(0xbabb1e5L)
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
  private final case class ChunkCreateJob(
      cx: Int,
      cz: Int,
      generation: Int,
      priority: Int,
      serial: Long,
      savedChunksDir: Option[java.io.File]
  )
  private val chunkJobSerial = AtomicLong(0L)
  private val chunkMeshComparator = new java.util.Comparator[ChunkMeshJob]:
    override def compare(a: ChunkMeshJob, b: ChunkMeshJob): Int =
      val p = java.lang.Integer.compare(a.priority, b.priority)
      if p != 0 then p else java.lang.Long.compare(a.serial, b.serial)
  private val chunkCreateComparator = new java.util.Comparator[ChunkCreateJob]:
    override def compare(a: ChunkCreateJob, b: ChunkCreateJob): Int =
      val p = java.lang.Integer.compare(a.priority, b.priority)
      if p != 0 then p else java.lang.Long.compare(a.serial, b.serial)
  private val chunkBuildQueue =
    new java.util.concurrent.PriorityBlockingQueue[ChunkMeshJob](64, chunkMeshComparator)
  private val chunkUploadQueue = new java.util.concurrent.ConcurrentLinkedQueue[Chunk]()
  private val chunkCreateQueue =
    new java.util.concurrent.PriorityBlockingQueue[ChunkCreateJob](64, chunkCreateComparator)
  private val chunkReadyQueue = new java.util.concurrent.ConcurrentLinkedQueue[(Int, Chunk)]()
  private val pendingChunkCreates = java.util.concurrent.ConcurrentHashMap.newKeySet[(Int, Int)]()
  private val chunkUnloadScratch = scala.collection.mutable.ArrayBuffer.empty[(Int, Int)]
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
    worldSeed = seed
    terrainGen = createTerrainGenerator(worldSeed)
    terrainHeightCache.clear()
    worldClock = 150.0; timeOverride = None
    rainStrength = 0f; wetness = 0f; stormCloudCover = 0f; lightningFlash = 0f
    snowCover = 0f
    zombies.clear(); zombieSerial = 0; zombieSpawnTimer = 2f; airSupply.reset(); leafDecay.clear();
    droppedItems.clear(); appliedItemOffers.clear(); pendingTosses.clear(); failedTosses.clear()
    playerCrouching = false; crouchAmount = 0f
    RainRenderer.reset()
    lastCombatHit.clear(); combatCooldown = 0f; playerKnockback = Vec3(0, 0, 0)
    localRain = 0f; localCloud = 0f; localWetness = 0f; localLightning = 0f
    weatherCells = Vector.empty; weatherStrikes = Vector.empty; heardThunder.clear();
    pendingPlayerEdits.clear()
    updateRainAmbience(0f)
    updateFurnaceAmbience(0f)
    furnacePosition = None; furnaceBurning = false; remoteFurnaces.clear();
    nearbyFurnaceSounds = Array.empty
    fieldJournal = FieldJournal(); journalToastTime = 0f
    if freshWorldName then worldName = worldFolderNameForSeed(worldSeed)

  private def chooseCreateSeed(): Long =
    val text = customSeedInput.trim
    if text.nonEmpty then text.toLongOption.getOrElse(seedFromText(text)) else worldSeed

  private def previewRandomSeed(): Unit =
    applyWorldSeed(freshWorldSeed(), freshWorldName = false)
    customSeedInput = worldSeed.toString

  private def openCreateWorld(): Unit =
    previewRandomSeed()
    createWorldNameFocused = true
    screen = Screen.CreateWorld

  private def sanitizeWorldName(raw: String): String =
    BlockboxWorld.sanitizeWorldName(raw)

  private def uniqueWorldFolderName(raw: String): String =
    BlockboxWorld.uniqueWorldFolderName(raw)

  private def createTerrainGenerator(seed: Long): TerrainGenerator =
    TerrainGenerator(seed, (cx, cz, blocks) => modManager.worldgen.decorate(seed, cx, cz, blocks))

  private def gameRootDir: java.io.File =
    sys.env
      .get("BLOCKBOX_PROJECT_ROOT")
      .map(java.io.File(_))
      .filter(_.isDirectory)
      .getOrElse(java.io.File(".").getAbsoluteFile)

  private def settingsFile: java.io.File = java.io.File(gameRootDir, "blockbox-settings.properties")

  private def loadSettingsProperties(): java.util.Properties =
    val props = java.util.Properties()
    val file = settingsFile
    if file.isFile then
      val in = java.io.FileInputStream(file)
      try props.load(in)
      finally in.close()
    props

  private def saveSettingsProperties(props: java.util.Properties): Unit =
    BlockboxFiles.ensureDirectory(settingsFile.getAbsoluteFile.getParentFile.toPath)
    val out = java.io.ByteArrayOutputStream()
    props.store(out, "Blockbox settings")
    BlockboxFiles.writeAtomic(settingsFile.toPath, stream => stream.write(out.toByteArray))

  private def propBool(props: java.util.Properties, key: String, fallback: Boolean): Boolean =
    Option(props.getProperty(key)).map(_.trim.toLowerCase) match
      case Some("true" | "1" | "yes" | "on")  => true
      case Some("false" | "0" | "no" | "off") => false
      case _                                  => fallback

  private def propInt(
      props: java.util.Properties,
      key: String,
      fallback: Int,
      min: Int,
      max: Int
  ): Int =
    Option(props.getProperty(key))
      .flatMap(s => scala.util.Try(s.trim.toInt).toOption)
      .getOrElse(fallback)
      .max(min)
      .min(max)

  private def propFloat(
      props: java.util.Properties,
      key: String,
      fallback: Float,
      min: Float,
      max: Float
  ): Float =
    Option(props.getProperty(key))
      .flatMap(s => scala.util.Try(s.trim.toFloat).toOption)
      .getOrElse(fallback)
      .max(min)
      .min(max)

  private def loadSelectedTexturePackId(): String =
    try
      Option(loadSettingsProperties().getProperty("texturePack"))
        .map(_.trim)
        .filter(_.nonEmpty)
        .getOrElse("default")
    catch case _: Exception => "default"

  private def loadUserSettings(): Unit =
    try
      val props = loadSettingsProperties()
      selectedTexturePackId = Option(props.getProperty("texturePack"))
        .map(_.trim)
        .filter(_.nonEmpty)
        .getOrElse(selectedTexturePackId)
      renderDistance =
        propInt(props, "renderDistance", renderDistance, minRenderDistance, maxRenderDistance)
      cloudRenderDistance = propInt(
        props,
        "cloudRenderDistance",
        cloudRenderDistance,
        minCloudRenderDistance,
        maxCloudRenderDistance
      )
      fogDensity = propFloat(props, "fogDensity", fogDensity, 0.6f, 3.0f)
      fov = propFloat(props, "fov", fov, 50f, 120f)
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
    def height: Float =
      (if id == "rd" || id == "fog" || id == "fov" || id == "clouds" then 56f else 34f) * uiScale

  private def settingsEntries: Vector[SettingEntry] =
    def toggle(id: String, label: String)(action: => Unit): SettingEntry =
      SettingEntry(id, label, () => { action; markSettingsDirty() })
    settingsTab match
      case 0 =>
        Vector(
          SettingEntry("rd", s"View distance   $renderDistance chunks"),
          SettingEntry("fov", s"Field of view   ${fov.toInt}"),
          SettingEntry("fog", f"Horizon fog   $fogDensity%.1f"),
          toggle("vsync", s"VSync   ${onOff(vsync)}") {
            vsync = !vsync; glfwSwapInterval(if vsync then 1 else 0)
          },
          toggle("aa", s"Antialiasing   ${onOff(antiAliasing)}") {
            antiAliasing = !antiAliasing; applyAntiAliasingState()
          },
          toggle("af", s"Anisotropic textures   ${onOff(anisotropicFiltering)}") {
            anisotropicFiltering = !anisotropicFiltering; rebuildTextureAtlas()
          },
          toggle("lighting", s"Smooth lighting   ${onOff(smoothLighting)}") {
            smoothLighting = !smoothLighting; applySmoothLightingSetting()
          },
          toggle("shaders", s"Shaders   ${onOff(shadersEnabled)}") {
            shadersEnabled = !shadersEnabled;
            if shadersEnabled && shaderPipeline != null && !shaderPipeline.available then
              shaderPipeline.initialize()
          },
          toggle(
            "pack",
            s"Shader pack   ${Option(shaderPipeline).map(_.packName).getOrElse("default")}"
          ) { if shaderPipeline != null then shaderPipeline.cyclePack() },
          toggle(
            "scale",
            s"World resolution   ${
                if foveatedRendering
                then s"${(Option(foveaPass).map(_.internalScale).getOrElse(0.85f) * 100).round}%"
                else "100%"
              }"
          ) { foveatedRendering = !foveatedRendering }
        )
      case 1 =>
        Vector(
          SettingEntry("clouds", s"Cloud distance   $cloudRenderDistance chunks"),
          toggle("cloudToggle", s"Clouds   ${onOff(cloudsEnabled)}") {
            cloudsEnabled = !cloudsEnabled
          },
          toggle(
            "weather",
            s"Weather   ${
                if isClientOnlyMultiplayer then "Host controlled" else onOff(weatherEnabled)
              }"
          ) { if !isClientOnlyMultiplayer then weatherEnabled = !weatherEnabled },
          toggle("darkness", s"Night shading   ${onOff(worldDarkening)}") {
            worldDarkening = !worldDarkening
          },
          toggle("fast", s"Fast movement   ${onOff(fastMove)}") { fastMove = !fastMove }
        ) ++ (if settingsCanHostMode then
                Vector(toggle("mode", s"Game mode   $gameMode") { toggleGameMode() })
              else Vector.empty)
      case _ =>
        Vector(
          toggle("audio", s"Sound   ${onOff(soundEnabled)}") {
            soundEnabled = !soundEnabled
            if soundEnabled then prepareAudio()
          },
          toggle("fullscreen", s"Fullscreen   ${onOff(fullscreen)}") { toggleFullscreen() },
          toggle("gui", s"Interface scale   $guiScaleLabel") { cycleGuiScale() },
          toggle("resources", s"Textures   ${selectedTexturePack.name}") {
            openResources(Screen.Settings)
          },
          toggle(
            "escape",
            s"Escape key   ${if pauseEscReturnsToGame then "Resume game" else "Quit to title"}"
          ) { pauseEscReturnsToGame = !pauseEscReturnsToGame }
        )

  private def settingsContentHeight(scale: Float): Float = settingsEntries.map(_.height).sum

  private def settingsMaxScroll(panelH: Float, scale: Float): Float =
    (settingsContentHeight(scale) - BlockboxUiLayout
      .settings(framebufferWidth, framebufferHeight, uiScale)
      .content
      .h).max(0f)

  private def saveSelectedTexturePackId(): Unit =
    markSettingsDirty()
    flushUserSettings(force = true)

  private def defaultTexturePackDir: java.io.File = java.io.File(gameRootDir, "assets/textures")

  private def customTexturePackRoots: List[java.io.File] =
    List(java.io.File(gameRootDir, "resourcepacks"), java.io.File("resourcepacks"))
      .distinctBy(_.getAbsolutePath)

  private def discoverResourcePacks(): List[TexturePack] =
    val builtIns = List(
      TexturePack("default", "Default Textures", "Kokonico", Some(defaultTexturePackDir)),
      TexturePack("legacy", "Legacy Textures", "RobertFlexx", None, legacy = true)
    )
    val custom = customTexturePackRoots
      .flatMap { root =>
        val dirs =
          Option(root.listFiles()).getOrElse(Array.empty[java.io.File]).filter(_.isDirectory).toList
        dirs.map { dir =>
          val safeId = "custom:" + dir.getName.trim.toLowerCase.replaceAll("[^a-z0-9._-]+", "-")
          val propsFile = java.io.File(dir, "pack.properties")
          val props = java.util.Properties()
          if propsFile.isFile then
            try
              val in = java.io.FileInputStream(propsFile)
              try props.load(in)
              finally in.close()
            catch case _: Exception => ()
          val name =
            Option(props.getProperty("name")).map(_.trim).filter(_.nonEmpty).getOrElse(dir.getName)
          val author =
            Option(props.getProperty("author")).map(_.trim).filter(_.nonEmpty).getOrElse("Custom")
          TexturePack(safeId, name, author, Some(dir))
        }
      }
      .distinctBy(_.id)
      .sortBy(_.name.toLowerCase)
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
      if soundEnabled then prepareAudio()
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
      UnderwaterEffect.destroy(); RainRenderer.destroy()
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
      println(
        s"Blockbox mod check: ${m.id} ${m.version} loaded=${m.loaded} status=${m.status} side=${m.sideLabel}"
      )
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
      case GLFW_PLATFORM_X11     => "x11"
      case GLFW_PLATFORM_WAYLAND => "wayland"
      case GLFW_PLATFORM_NULL    => "null"
      case _                     => s"unknown($selectedPlatform)"
    System.err.println(s"Blockbox: GLFW selected platform=$selectedPlatformName")

    def contextApiName(useEgl: Boolean): String =
      if useEgl then "EGL"
      else if selectedPlatform == GLFW_PLATFORM_X11 then "GLX/native"
      else "native"

    def resetWindowHints(
        samples: Int,
        requestLegacy21: Boolean,
        requestCompat33: Boolean,
        useEgl: Boolean
    ): Unit =
      glfwDefaultWindowHints()
      glfwWindowHint(GLFW_CLIENT_API, GLFW_OPENGL_API)
      glfwWindowHint(
        GLFW_CONTEXT_CREATION_API,
        if useEgl then GLFW_EGL_CONTEXT_API else GLFW_NATIVE_CONTEXT_API
      )
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
      glfwWindowHint(GLFW_AUTO_ICONIFY, GLFW_FALSE)
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

    def tryCreateCapabilities(
        label: String,
        samples: Int,
        requestLegacy21: Boolean = false,
        requestCompat33: Boolean = false,
        useEgl: Boolean = false
    ): Boolean =
      destroyWindowIfNeeded()
      resetWindowHints(samples, requestLegacy21, requestCompat33, useEgl)
      System.err.println(s"Blockbox: trying OpenGL context '$label' via ${contextApiName(useEgl)}")
      if fullscreen then
        val monitor = glfwGetPrimaryMonitor()
        val mode = glfwGetVideoMode(monitor)
        if mode == null then
          System.err.println(
            s"Blockbox: context '$label' failed: primary monitor has no video mode"
          )
          return false
        window = glfwCreateWindow(mode.width, mode.height, "Blockbox", monitor, NULL)
      else window = glfwCreateWindow(width, height, "Blockbox", NULL, NULL)
      if window == NULL then
        System.err.println(s"Blockbox: context '$label' failed: glfwCreateWindow returned NULL")
        return false
      glfwMakeContextCurrent(window)
      if glfwGetCurrentContext() != window then
        System.err.println(
          s"Blockbox: context '$label' failed: glfwMakeContextCurrent did not stick"
        )
        return false
      try
        // Capabilities must be recreated for each newly-created context. Clearing
        // them between attempts prevents a failed GLX context from poisoning a
        // later EGL attempt on the same thread.
        GL.setCapabilities(null)
        val caps = GL.createCapabilities()
        if caps == null then
          System.err.println(
            s"Blockbox: context '$label' failed: GL.createCapabilities returned null"
          )
          return false
        glfwSwapInterval(if vsync then 1 else 0)
        glfwSetWindowAttrib(window, GLFW_AUTO_ICONIFY, GLFW_FALSE)
        glfwShowWindow(window)
        msaaSamples = if samples > 0 then glGetInteger(GL_SAMPLES).max(0) else 0
        applyAntiAliasingState()
        System.err.println(s"Blockbox: context '$label' succeeded via ${contextApiName(useEgl)}")
        true
      catch
        case t: Throwable =>
          System.err.println(
            s"Blockbox: context '$label' failed inside LWJGL: ${t.getClass.getName}: ${Option(t.getMessage).getOrElse("(no message)")}"
          )
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
    System.err.println(
      s"Blockbox: OpenGL context policy requested=$forcedContextApi effective=$effectiveContextApi"
    )

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
      val info =
        s"GLFW_PLATFORM=$platform selected=$selectedPlatformName XDG_SESSION_TYPE=$session contextApi=$effectiveContextApi"
      System.err.println(s"Blockbox: OpenGL context setup failed after trying all fallbacks. $info")
      throw IllegalStateException(
        s"OpenGL context setup failed. $info. On X11 try BLOCKBOX_GL_CONTEXT_API=egl to bypass GLX, or BLOCKBOX_GL_CONTEXT_API=glx to force GLX diagnostics."
      )
    glEnable(0x809d)
    System.err.println(
      s"Blockbox: OpenGL context created — vendor=${glGetString(GL_VENDOR)} renderer=${glGetString(GL_RENDERER)} version=${glGetString(GL_VERSION)}"
    )
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
    glfwSetFramebufferSizeCallback(
      window,
      new GLFWFramebufferSizeCallback {
        override def invoke(window: Long, w: Int, h: Int): Unit =
          framebufferWidth = w.max(1)
          framebufferHeight = h.max(1)
      }
    )
    glEnable(GL_DEPTH_TEST)
    glDisable(GL_CULL_FACE)
    glEnable(GL_BLEND)
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA)
    glCullFace(GL_BACK)
    glClearColor(0.52f, 0.72f, 0.95f, 1f)
    glfwSetKeyCallback(
      window,
      new GLFWKeyCallback {
        override def invoke(window: Long, key: Int, scancode: Int, action: Int, mods: Int): Unit =
          if glfwGetWindowAttrib(window, GLFW_FOCUSED) == GLFW_FALSE then return
          if action == GLFW_PRESS || (action == GLFW_REPEAT && repeatableKey(key)) then
            onKey(key, mods)
      }
    )
    glfwSetWindowFocusCallback(
      window,
      new GLFWWindowFocusCallback {
        override def invoke(window: Long, focused: Boolean): Unit = onWindowFocus(focused)
      }
    )
    glfwSetCharCallback(
      window,
      new GLFWCharCallback {
        override def invoke(window: Long, codepoint: Int): Unit = onChar(codepoint)
      }
    )
    glfwSetScrollCallback(
      window,
      new GLFWScrollCallback {
        override def invoke(window: Long, xoffset: Double, yoffset: Double): Unit =
          onScroll(xoffset, yoffset)
      }
    )

  private def queryWindowPos(): Unit =
    val prevCb = glfwSetErrorCallback(null)
    try
      val wx = BufferUtils.createIntBuffer(1); val wy = BufferUtils.createIntBuffer(1)
      glfwGetWindowPos(window, wx, wy)
      windowedX = wx.get(0); windowedY = wy.get(0)
    finally glfwSetErrorCallback(prevCb)

  protected def findSpawn(): Vec3 =
    BlockboxWorld.findSafeSpawn(
      terrainGen,
      (x, y, z) =>
        loadedChunkForBlock(x, z).map(c =>
          c.getBlock(Math.floorMod(x, 16), y, Math.floorMod(z, 16))
        )
    )

  private def loop(): Unit =
    val now = glfwGetTime()
    lastFrameTime = (now - lastTime).min(0.1)
    val dt = lastFrameTime.toFloat.min(0.05f)
    lastTime = now
    completePendingJoin()
    updateWorldSimulation(dt, lastFrameTime)
    if screen != Screen.Playing then handleMenuMouse()
    updateTextMouse()
    textInputViews.clear()
    render()
    flushUserSettings()
    glfwSwapBuffers(window)
    val screenBeforeEvents = screen
    glfwPollEvents()
    // Focus loss can still deliver the key that opened the pause menu. Put the
    // player back in the world; tabbing out is not quitting.
    if !windowFocused && screenBeforeEvents == Screen.Playing && screen == Screen.Paused then
      screen = Screen.Playing

  private def worldSimulationRunning: Boolean =
    activeWorldSession && (screen == Screen.Playing || shouldRunMultiplayerBackgroundTick)

  private def updateWorldSimulation(dt: Float, elapsed: Double): Unit =
    if worldSimulationRunning then
      worldClock += elapsed * modManager.rules.get("timeScale")
      if isClientOnlyMultiplayer && environmentReceived then
        val correction = (environmentTimeError * 0.9).max(-0.15).min(0.15) * elapsed
        worldClock += correction; environmentTimeError -= correction
    publishedEnvironment = environmentMessage
    publishedRules = modManager.rules.snapshot
    publishedOps = oppedPlayerNames.toVector
    if worldSimulationRunning then
      if screen == Screen.Playing then updateGame(dt) else updateMultiplayerBackgroundTick(dt)
      if activeWorldSession then
        modManager.fireWorldTick(dt)
        updateFurnace(dt)
    else updateFurnaceAmbience(0f)

  private def onWindowFocus(focused: Boolean): Unit =
    windowFocused = focused
    ignoreInputUntil = glfwGetTime() + 0.3
    if !focused then
      textDrag = None
      textMouseWasDown = false
      if vsync then glfwSwapInterval(0)
      if screen == Screen.Playing then setCursorCaptured(false)
    else
      glfwSwapInterval(if vsync then 1 else 0)
      if screen == Screen.Playing && !chatOpen && !modUiCursorMode then
        setCursorCaptured(true)
        firstMouse = true

  private def onChar(codepoint: Int): Unit =
    if chatOpen && suppressNextChatChar then suppressNextChatChar = false
    else if codepoint >= 32 && codepoint <= 126 then
      activeTextBox.foreach(id => editTextBox(id)(_.insert(codepoint.toChar.toString)))

  private def activeTextBox: Option[TextBox] = screen match
    case Screen.Playing if chatOpen             => Some(TextBox.Chat)
    case Screen.Catalog if catalogSearchFocused => Some(TextBox.Catalog)
    case Screen.CreateWorld                     =>
      Some(if createWorldNameFocused then TextBox.WorldName else TextBox.Seed)
    case Screen.JoinGame => Some(if joinNameFocused then TextBox.PlayerName else TextBox.Address)
    case Screen.HostGame if hostNameFocused => Some(TextBox.PlayerName)
    case _                                  => None

  private def textBoxValue(id: TextBox): String = id match
    case TextBox.Chat       => chatInput
    case TextBox.Catalog    => catalogQuery
    case TextBox.WorldName  => worldNameInput
    case TextBox.Seed       => customSeedInput
    case TextBox.PlayerName => playerName
    case TextBox.Address    => joinIpInput

  private def textEditor(id: TextBox): TextLine =
    val editor = textEditors.getOrElseUpdate(
      id,
      id match
        case TextBox.PlayerName                  => TextLine(16, TextLine.playerName)
        case TextBox.WorldName | TextBox.Catalog => TextLine(40)
        case TextBox.Seed | TextBox.Address      => TextLine(64)
        case TextBox.Chat                        => TextLine(1024)
    )
    editor.synchronize(textBoxValue(id))
    editor

  private def editTextBox(id: TextBox)(edit: TextLine => Unit): Unit =
    val editor = textEditor(id)
    val before = editor.text
    edit(editor)
    if editor.text != before then
      id match
        case TextBox.Chat       => chatInput = editor.text; markChatEdited()
        case TextBox.Catalog    => catalogQuery = editor.text; catalogScroll = 0
        case TextBox.WorldName  => worldNameInput = editor.text
        case TextBox.Seed       => customSeedInput = editor.text
        case TextBox.PlayerName => playerName = editor.text
        case TextBox.Address    => joinIpInput = editor.text
    textCaretReset = if window == 0L then 0.0 else glfwGetTime()

  private def handleTextKey(key: Int, mods: Int): Boolean =
    activeTextBox.exists { id =>
      var handled = false
      editTextBox(id) { editor =>
        handled = editor.key(
          key,
          mods,
          () => if window == 0L then "" else Option(glfwGetClipboardString(window)).getOrElse(""),
          value => if window != 0L then glfwSetClipboardString(window, value)
        )
      }
      // Character callbacks insert printable text. Menu letter shortcuts must not
      // consume the same keystroke while a text box has focus.
      handled || (key >= GLFW_KEY_SPACE && key <= GLFW_KEY_GRAVE_ACCENT)
    }

  private def updateTextMouse(): Unit =
    if window == 0L || !windowFocused then return
    val pressed = glfwGetMouseButton(window, GLFW_MOUSE_BUTTON_LEFT) == GLFW_PRESS
    val (mx, my) = mouseFramebufferPos()
    def position(id: TextBox, selecting: Boolean): Unit =
      textInputViews.get(id).foreach { view =>
        val column = math
          .round((mx - view.bounds.x) / (6f * view.scale))
          .max(-1)
          .min(view.columns + 1)
        editTextBox(id)(_.move(view.start + column, selecting))
      }
    if pressed && !textMouseWasDown then
      textDrag =
        activeTextBox.filter(id => textInputViews.get(id).exists(_.bounds.contains(mx, my)))
      textDrag.foreach(id => position(id, down(GLFW_KEY_LEFT_SHIFT) || down(GLFW_KEY_RIGHT_SHIFT)))
    else if pressed then
      textDrag
        .filter(id => activeTextBox.contains(id))
        .foreach(id => position(id, selecting = true))
    if !pressed then textDrag = None
    textMouseWasDown = pressed

  private def onScroll(xoffset: Double, yoffset: Double): Unit =
    val delta = if yoffset > 0 then -1 else if yoffset < 0 then 1 else 0
    if delta == 0 then return
    screen match
      case Screen.Playing =>
        selectedBlock = Math.floorMod(selectedBlock + delta, hotbarBlocks.length)
        selectedItemLabelTimer = 1.8f
      case Screen.Catalog =>
        catalogScroll = (catalogScroll + delta).max(0)
      case Screen.FurnaceUI => ()
      case Screen.Mods      =>
        modsScreenScroll = (modsScreenScroll + delta).max(0)
      case Screen.Resources =>
        resourcesScroll = (resourcesScroll + delta).max(0).min((resourcePacks.length - 1).max(0))
      case Screen.Settings =>
        val s = uiScale
        val pH = (630f * s).min(framebufferHeight.toFloat * 0.94f)
        settingsScroll = (settingsScroll + delta * 34f * s).max(0f).min(settingsMaxScroll(pH, s))
      case _ => ()

  private def repeatableKey(key: Int): Boolean =
    TextLine.repeatable(key) || key == GLFW_KEY_UP || key == GLFW_KEY_DOWN ||
      key == GLFW_KEY_MINUS || key == GLFW_KEY_EQUAL

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
    placeableBlocks
      .contains(block) || (CustomBlocks.isSlot(block) && CustomBlocks.registered(block))

  private def onKey(key: Int, mods: Int = 0): Unit =
    if !windowFocused || glfwGetTime() < ignoreInputUntil then return
    if modManager.fireKeyPress(ModKeyEvent(modManager.api, key, modScreenName)).cancelled then
      return
    if handleTextKey(key, mods) then return
    // Alt-tab and other desktop chords must not become menu/game actions.
    if (mods & GLFW_MOD_ALT) != 0 then return
    if key == GLFW_KEY_F11 then toggleFullscreen()
    else
      screen match
        case Screen.MainMenu =>
          if key == GLFW_KEY_ENTER then openCreateWorld()
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
          else if key == GLFW_KEY_DOWN then
            resourcesScroll = (resourcesScroll + 1).min((resourcePacks.length - 1).max(0))
        case Screen.CreateWorld =>
          if key == GLFW_KEY_ENTER then startNewWorld()
          else if key == GLFW_KEY_ESCAPE then screen = Screen.MainMenu
          else if key == GLFW_KEY_TAB then createWorldNameFocused = !createWorldNameFocused
        case Screen.LoadWorld =>
          val saves = worldSaveDirs
          if key == GLFW_KEY_ESCAPE then screen = Screen.MainMenu
          else if key == GLFW_KEY_ENTER then loadSelectedWorld()
          else if key == GLFW_KEY_UP then loadWorldSelection = (loadWorldSelection - 1).max(0)
          else if key == GLFW_KEY_DOWN then
            loadWorldSelection = (loadWorldSelection + 1).min((saves.length - 1).max(0))
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
          else if key == GLFW_KEY_M && canUseCheatAuthority && worldCheatsEnabled then
            toggleGameMode()
          else if key == GLFW_KEY_P then
            pauseEscReturnsToGame = !pauseEscReturnsToGame
            markSettingsDirty()
          else if key == GLFW_KEY_R then openResources(Screen.Settings)
        case Screen.Playing =>
          if chatOpen then
            if key == GLFW_KEY_ENTER then submitChat()
            else if key == GLFW_KEY_ESCAPE then
              chatOpen = false; chatInput = ""; chatHistoryIndex = -1; chatHistoryDraft = ""
              setCursorCaptured(captured = true); firstMouse = true
            else if key == GLFW_KEY_TAB then applyCommandSuggestion()
            else if key == GLFW_KEY_UP then showPreviousChatHistory()
            else if key == GLFW_KEY_DOWN then showNextChatHistory()
          else if key == GLFW_KEY_ESCAPE then leaveGame(Screen.Paused)
          else if key == GLFW_KEY_Q then
            dropHeldStack(down(GLFW_KEY_LEFT_CONTROL) || down(GLFW_KEY_RIGHT_CONTROL))
          else if key == GLFW_KEY_E then
            if gameMode == GameMode.Creative then openCatalog() else openInventory()
          else if key == GLFW_KEY_SLASH || key == GLFW_KEY_T then
            chatOpen = true
            chatInput = if key == GLFW_KEY_SLASH then "/" else ""
            chatHistoryIndex = -1
            chatHistoryDraft = chatInput
            suppressNextChatChar = true
            setCursorCaptured(captured = false)
          else if key >= GLFW_KEY_1 && key <= GLFW_KEY_9 then
            selectedBlock = key - GLFW_KEY_1
            selectedItemLabelTimer = 1.8f
          else if key == GLFW_KEY_M && canUseCheatAuthority && worldCheatsEnabled then
            toggleGameMode()
          else if key == GLFW_KEY_J then
            screen = Screen.Journal
            setCursorCaptured(captured = false)
          else if key == GLFW_KEY_F3 then debugMode = !debugMode
          else if key == GLFW_KEY_F4 then
            wireframeMode = !wireframeMode
            if wireframeMode then glPolygonMode(GL_FRONT_AND_BACK, GL_LINE)
            else glPolygonMode(GL_FRONT_AND_BACK, GL_FILL)
          else if key == GLFW_KEY_F5 then showChunkBorders = !showChunkBorders
          else if key == GLFW_KEY_F8 then modSetCursorMode(!modUiCursorMode)
          else if key == GLFW_KEY_F2 then
            settingsReturnTo = Screen.Playing
            screen = Screen.Settings
            setCursorCaptured(captured = false)
        case Screen.Journal =>
          if key == GLFW_KEY_RIGHT then journalPage = (journalPage + 1) % journalPageCount
          else if key == GLFW_KEY_LEFT then
            journalPage = Math.floorMod(journalPage - 1, journalPageCount)
          else if key == GLFW_KEY_ESCAPE || key == GLFW_KEY_J || key == GLFW_KEY_ENTER then
            enterGame()
        case Screen.JoinGame =>
          if key == GLFW_KEY_ENTER then
            val ip = if joinIpInput.trim.isEmpty then "127.0.0.1" else joinIpInput.trim
            joinStatusError = false
            joinStatusMessage = s"Connecting to $ip ..."
            joinGame(ip)
          else if key == GLFW_KEY_ESCAPE then
            stopNetworking(); screen = Screen.MainMenu
          else if key == GLFW_KEY_TAB then joinNameFocused = !joinNameFocused
        case Screen.HostGame =>
          val saves = worldSaveDirs
          if key == GLFW_KEY_ENTER then hostGame()
          else if key == GLFW_KEY_ESCAPE then screen = Screen.MainMenu
          else if key == GLFW_KEY_UP then hostWorldSelection = (hostWorldSelection - 1).max(0)
          else if key == GLFW_KEY_DOWN then
            hostWorldSelection = (hostWorldSelection + 1).min((saves.length - 1).max(0))
        case Screen.Paused =>
          if key == GLFW_KEY_ENTER then enterGame()
          else if key == GLFW_KEY_ESCAPE then
            if pauseEscReturnsToGame then enterGame() else leaveGame(Screen.MainMenu)
          else if key == GLFW_KEY_S then
            settingsReturnTo = Screen.Paused
            screen = Screen.Settings
        case Screen.Catalog =>
          if key == GLFW_KEY_ESCAPE || (key == GLFW_KEY_E && !catalogSearchFocused) then enterGame()
          else if key == GLFW_KEY_ENTER then catalogSearchFocused = false
          else if key == GLFW_KEY_DELETE && !catalogSearchFocused then
            if heldInventoryBlock != Block.Air then clearHeldItem()
            else { hotbarBlocks(selectedBlock) = Block.Air; hotbarCounts(selectedBlock) = 0 }
        case Screen.Inventory | Screen.FurnaceUI =>
          if key == GLFW_KEY_E || key == GLFW_KEY_ESCAPE then enterGame()

  private def handleMenuMouse(): Unit =
    val leftDown = glfwGetMouseButton(window, GLFW_MOUSE_BUTTON_LEFT) == GLFW_PRESS
    val rightDown = glfwGetMouseButton(window, GLFW_MOUSE_BUTTON_RIGHT) == GLFW_PRESS
    val shift = down(GLFW_KEY_LEFT_SHIFT) || down(GLFW_KEY_RIGHT_SHIFT)
    if leftDown && !menuLeftWasDown then
      val (mx, my) = mouseFramebufferPos()
      screen match
        case Screen.MainMenu    => handleMainMenuClick(mx, my)
        case Screen.CreateWorld => handleCreateWorldClick(mx, my)
        case Screen.LoadWorld   => handleLoadWorldClick(mx, my)
        case Screen.Mods        => handleModsClick(mx, my)
        case Screen.Resources   => handleResourcesClick(mx, my)
        case Screen.Settings    => handleSettingsClick(mx, my)
        case Screen.Paused      => handlePauseClick(mx, my)
        case Screen.Inventory => handleInventoryClick(mx, my, rightClick = false, quickMove = shift)
        case Screen.Catalog   => handleCatalogClick(mx, my, quickMove = shift)
        case Screen.FurnaceUI => handleFurnaceClick(mx, my, quickMove = shift)
        case Screen.JoinGame  => handleJoinClick(mx, my)
        case Screen.HostGame  => handleHostClick(mx, my)
        case Screen.Journal   => handleJournalClick(mx, my)
        case Screen.Playing   => ()
    if rightDown && !menuRightWasDown && screen == Screen.Inventory then
      val (mx, my) = mouseFramebufferPos()
      handleInventoryClick(mx, my, rightClick = true, quickMove = shift)
    if rightDown && !menuRightWasDown && screen == Screen.Catalog then
      val (mx, my) = mouseFramebufferPos()
      handleCatalogClick(mx, my, rightClick = true, quickMove = shift)
    if rightDown && !menuRightWasDown && screen == Screen.FurnaceUI then
      val (mx, my) = mouseFramebufferPos()
      handleFurnaceClick(mx, my, quickMove = shift, rightClick = true)
    if leftDown && screen == Screen.Settings && sliderActive != null then
      val (mx, my) = mouseFramebufferPos()
      updateSlider(mx)
    if !leftDown then sliderActive = null
    menuLeftWasDown = leftDown
    menuRightWasDown = rightDown

  private def updateSlider(mx: Float): Unit =
    val layout = BlockboxUiLayout.settings(framebufferWidth, framebufferHeight, uiScale)
    val value =
      ((mx - layout.content.x - 8f * uiScale) / (layout.content.w - 16f * uiScale)).max(0f).min(1f)
    sliderActive match
      case "rd" =>
        val next = (minRenderDistance + value * (maxRenderDistance - minRenderDistance)).round
        if next != renderDistance then
          renderDistance = next
          syncChunks()
      case "fog"    => fogDensity = 0.6f + value * 2.4f
      case "fov"    => fov = 50f + value * 70f
      case "clouds" =>
        cloudRenderDistance =
          (minCloudRenderDistance + value * (maxCloudRenderDistance - minCloudRenderDistance)).round
      case _ => ()
    markSettingsDirty()

  private def handleMainMenuClick(mx: Float, my: Float): Unit =
    val layout = mainMenuLayout()
    def hit(i: Int): Boolean = layout.buttons(i).contains(mx, my)
    if hit(0) then openCreateWorld()
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
    if inRect(mx, my, settingX, nameY, settingW, 34f * s) then createWorldNameFocused = true
    else if inRect(mx, my, settingX, seedY, settingW, 34f * s) then createWorldNameFocused = false
    else if inRect(mx, my, settingX, modeY, settingW, settingH) then
      createWorldMode =
        if createWorldMode == GameMode.Survival then GameMode.Creative else GameMode.Survival
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
      else if inRect(mx, my, buttonX, buttonStartY + (buttonH + gap) * 2f, buttonW, buttonH) then
        createWorldNameFocused = false
        editTextBox(TextBox.Seed)(editor => {
          editor.move(0); editor.move(editor.text.length, true)
        })
      else if inRect(mx, my, buttonX, buttonStartY + (buttonH + gap) * 3f, buttonW, buttonH) then
        screen = Screen.MainMenu

  private def renderNameField(
      id: TextBox,
      x: Float,
      y: Float,
      w: Float,
      h: Float,
      label: String,
      value: String,
      focused: Boolean
  ): Unit =
    val s = uiScale
    renderTextShadow(x, y - 21f * s, label, 0.9f, 0.9f, 0.9f, (1.10f * s).max(1f))
    rect(
      x - 1f * s,
      y - 1f * s,
      w + 2f * s,
      h + 2f * s,
      if focused then 0.85f else 0.08f,
      if focused then 0.85f else 0.08f,
      if focused then 0.85f else 0.08f,
      1f
    )
    rect(x, y, w, h, 0.06f, 0.06f, 0.06f, 1f)
    rect(x + 2f * s, y + 2f * s, w - 4f * s, 2f * s, 0.22f, 0.25f, 0.32f, 0.18f)
    renderEditableLine(
      id,
      UiRect(x + 12f * s, y, w - 24f * s, h),
      (1.55f * s).max(1.25f),
      focused,
      if value.nonEmpty then value else "..."
    )

  private def renderEditableLine(
      id: TextBox,
      bounds: UiRect,
      scale: Float,
      focused: Boolean,
      placeholder: String = "",
      shadow: Boolean = true
  ): Unit =
    val editor = textEditor(id)
    val columns = ((bounds.w - 2f) / (6f * scale)).toInt.max(1)
    val shown = editor.visible(columns)
    val first = editor.viewStart
    textInputViews(id) = TextInputView(bounds, scale, first, columns)
    val y = bounds.y + (bounds.h - 7f * scale) / 2f
    val lo = (editor.selectionStart - first).max(0).min(shown.length)
    val hi = (editor.selectionEnd - first).max(0).min(shown.length)
    if focused && hi > lo then
      rect(
        bounds.x + lo * 6f * scale,
        y - 2f * scale,
        (hi - lo) * 6f * scale,
        11f * scale,
        0.22f,
        0.43f,
        0.70f,
        0.85f
      )
    val isPlaceholder = editor.text.isEmpty
    val display = if isPlaceholder then placeholder.take(columns) else shown
    val color = if isPlaceholder then 0.57f else 1f
    if shadow then renderTextShadow(bounds.x, y, display, color, color, color, scale)
    else renderText(bounds.x, y, display, color, color, color, scale)
    if focused && ((glfwGetTime() - textCaretReset) * 2).toInt % 2 == 0 then
      val x = bounds.x + (editor.cursor - first) * 6f * scale
      rect(x, y - scale, scale.max(1f), 9f * scale, 1f, 1f, 1f, 1f)

  private def renderHostGame(): Unit =
    glDisable(GL_DEPTH_TEST); glDisable(GL_CULL_FACE); setupOrtho()
    val w = framebufferWidth.toFloat; val h = framebufferHeight.toFloat; val cx = w / 2f;
    val s = uiScale
    BlockboxRender2D.drawMenuBackdrop(framebufferWidth, framebufferHeight, glfwGetTime().toFloat)
    val pW = (620f * s).min(w * 0.92f); val pH = (470f * s).min(h * 0.88f)
    val pX = cx - pW / 2f; val pY = h / 2f - pH / 2f
    drawPanel(pX, pY, pW, pH)
    centeredTextFit(cx, pY + 34f * s, "HOST LAN GAME", 1f, 0.90f, 0.55f, 1.95f * s, pW - 60f * s)
    rect(pX + 40f * s, pY + 64f * s, pW - 80f * s, 1f, 0.30f, 0.30f, 0.35f, 0.30f)
    val fieldW = pW - 96f * s; val fieldH = 34f * s; val fieldX = pX + 48f * s
    renderNameField(
      TextBox.PlayerName,
      fieldX,
      pY + 102f * s,
      fieldW,
      fieldH,
      "Online name",
      playerName,
      hostNameFocused
    )
    val saves = worldSaveDirs
    val sourceY = pY + 166f * s
    val sourceLabel =
      if hostUseSavedWorld then "World source: saved world" else "World source: new world"
    val (mx, my) = mouseFramebufferPos()
    val sourceHover = inRect(mx, my, fieldX, sourceY, fieldW, 30f * s)
    rect(
      fieldX,
      sourceY,
      fieldW,
      30f * s,
      if sourceHover then 0.16f else 0.09f,
      if sourceHover then 0.18f else 0.10f,
      if sourceHover then 0.23f else 0.15f,
      0.82f
    )
    renderTextShadow(fieldX + 12f * s, sourceY + 7f * s, sourceLabel, 0.88f, 0.92f, 1f, 0.58f * s)
    centeredTextFit(
      fieldX + fieldW - 54f * s,
      sourceY + 7f * s,
      "L",
      1f,
      0.86f,
      0.42f,
      0.56f * s,
      40f * s
    )
    if hostUseSavedWorld then
      val listY = pY + 208f * s
      val rowH = (34f * s).max(27f)
      val visible = 4
      if saves.isEmpty then
        centeredTextFit(
          cx,
          listY + 34f * s,
          "No saved worlds in worlds/",
          0.70f,
          0.78f,
          0.92f,
          0.62f * s,
          fieldW
        )
      else
        if hostWorldSelection < hostWorldScroll then hostWorldScroll = hostWorldSelection
        if hostWorldSelection >= hostWorldScroll + visible then
          hostWorldScroll = hostWorldSelection - visible + 1
        hostWorldScroll = hostWorldScroll.max(0).min((saves.length - visible).max(0))
        saves.zipWithIndex.drop(hostWorldScroll).take(visible).foreach { case (dir, idx) =>
          val y = listY + (idx - hostWorldScroll) * rowH
          val selected = idx == hostWorldSelection
          rect(
            fieldX,
            y,
            fieldW,
            rowH - 4f * s,
            if selected then 0.22f else 0.075f,
            if selected then 0.19f else 0.085f,
            if selected then 0.11f else 0.12f,
            0.84f
          )
          if selected then rect(fieldX, y, 4f * s, rowH - 4f * s, 1f, 0.82f, 0.30f, 0.80f)
          renderTextShadow(fieldX + 12f * s, y + 7f * s, dir.getName, 0.90f, 0.94f, 1f, 0.55f * s)
        }
    val hsr = if hostStatusError then 1.0f else 0.52f
    val hsg = if hostStatusError then 0.36f else 0.70f
    val hsb = if hostStatusError then 0.30f else 0.82f
    centeredTextFit(
      cx,
      pY + pH - 118f * s,
      hostStatusMessage,
      hsr,
      hsg,
      hsb,
      0.54f * s,
      pW - 72f * s
    )
    val bw = (230f * s).min(pW - 90f * s); val bh = (38f * s).min(42f).max(30f)
    drawButton(
      cx - bw / 2f,
      pY + pH - 86f * s,
      bw,
      bh,
      if hostUseSavedWorld then "Host Selected World" else "Start Hosted World",
      accent = true
    )
    drawButton(cx - bw / 2f, pY + pH - 42f * s, bw, bh, "Back")

  private def handleHostClick(mx: Float, my: Float): Unit =
    val w = framebufferWidth.toFloat; val h = framebufferHeight.toFloat; val cx = w / 2f;
    val s = uiScale
    val pW = (620f * s).min(w * 0.92f); val pH = (470f * s).min(h * 0.88f)
    val pX = cx - pW / 2f; val pY = h / 2f - pH / 2f
    val fieldW = pW - 96f * s; val fieldH = 34f * s; val fieldX = pX + 48f * s
    val bw = (230f * s).min(pW - 90f * s); val bh = (38f * s).min(42f).max(30f)
    if inRect(mx, my, fieldX, pY + 102f * s, fieldW, fieldH) then hostNameFocused = true
    else if inRect(mx, my, fieldX, pY + 166f * s, fieldW, 30f * s) then
      hostUseSavedWorld = !hostUseSavedWorld
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
    val w = framebufferWidth.toFloat; val h = framebufferHeight.toFloat; val cx = w / 2f;
    val s = uiScale
    BlockboxRender2D.drawMenuBackdrop(framebufferWidth, framebufferHeight, glfwGetTime().toFloat)
    val pW = (540f * s).min(w * 0.92f); val pH = (350f * s).min(h * 0.86f)
    val pX = cx - pW / 2f; val pY = h / 2f - pH / 2f
    drawPanel(pX, pY, pW, pH)
    centeredTextFit(cx, pY + 34f * s, "JOIN LAN GAME", 1f, 0.90f, 0.55f, 1.95f * s, pW - 60f * s)
    rect(pX + 40f * s, pY + 64f * s, pW - 80f * s, 1f, 0.30f, 0.30f, 0.35f, 0.30f)
    val fieldW = pW - 96f * s; val fieldH = 36f * s; val fieldX = pX + 48f * s
    renderNameField(
      TextBox.PlayerName,
      fieldX,
      pY + 112f * s,
      fieldW,
      fieldH,
      "Online name",
      playerName,
      joinNameFocused
    )
    val ipDisplay = if joinIpInput.trim.isEmpty then "127.0.0.1" else joinIpInput.trim
    renderNameField(
      TextBox.Address,
      fieldX,
      pY + 184f * s,
      fieldW,
      fieldH,
      "Server IP address",
      ipDisplay,
      !joinNameFocused
    )
    val jsr = if joinStatusError then 1.0f else 0.52f
    val jsg = if joinStatusError then 0.34f else 0.70f
    val jsb = if joinStatusError then 0.28f else 0.82f
    centeredTextFit(cx, pY + 242f * s, joinStatusMessage, jsr, jsg, jsb, 0.56f * s, pW - 72f * s)
    val bw = (210f * s).min(pW - 90f * s); val bh = (40f * s).min(42f).max(30f)
    drawButton(
      cx - bw / 2f,
      pY + pH - 92f * s,
      bw,
      bh,
      if pendingJoin != null || applyingJoin then "Connecting..." else "Connect",
      accent = true
    )
    drawButton(cx - bw / 2f, pY + pH - 44f * s, bw, bh, "Back")

  private def handleJoinClick(mx: Float, my: Float): Unit =
    val w = framebufferWidth.toFloat; val h = framebufferHeight.toFloat; val cx = w / 2f;
    val s = uiScale
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
    val layout = BlockboxUiLayout.settings(framebufferWidth, framebufferHeight, uiScale)
    val tab = layout.tabs.indexWhere(_.contains(mx, my))
    if tab >= 0 then
      settingsTab = tab; settingsScroll = 0f; sliderActive = null
    else if layout.done.contains(mx, my) then closeSettings()
    else if layout.content.contains(mx, my) then
      var y = layout.content.y - settingsScroll
      var clicked: Option[SettingEntry] = None
      settingsEntries.foreach { entry =>
        val row = UiRect(layout.content.x, y, layout.content.w, entry.height - 4f * uiScale)
        if clicked.isEmpty && row.contains(mx, my) then clicked = Some(entry)
        y += entry.height
      }
      clicked.foreach { entry =>
        if entry.id == "rd" || entry.id == "fov" || entry.id == "fog" || entry.id == "clouds" then
          sliderActive = entry.id; updateSlider(mx)
        else entry.action()
      }

  private def handlePauseClick(mx: Float, my: Float): Unit =
    val cx = framebufferWidth / 2f; val h = framebufferHeight.toFloat; val s = uiScale
    val pw = (420f * s).min(framebufferWidth * 0.80f);
    val ph = (if multiplayerMode then 300f * s else 275f * s).min(h * 0.82f);
    val py = h / 2f - ph / 2f
    val bw = (320f * s).min(pw - 72f * s); val bh = (40f * s).min(ph / 6f).max(30f);
    val bx = cx - bw / 2f
    val gap = (14f * s).min(18f); val by0 = py + 92f * s
    if inRect(mx, my, bx, by0, bw, bh) then enterGame()
    else if inRect(mx, my, bx, by0 + bh + gap, bw, bh) then
      settingsReturnTo = Screen.Paused
      screen = Screen.Settings
    else if inRect(mx, my, bx, by0 + (bh + gap) * 2f, bw, bh) then leaveGame(Screen.MainMenu)

  /** @param quickMove shift held: Minecraft's quick move for the clicked slot, either button */
  private def handleInventoryClick(
      mx: Float,
      my: Float,
      rightClick: Boolean,
      quickMove: Boolean = false
  ): Unit =
    val l = BlockboxUiLayout.inventory(framebufferWidth, framebufferHeight, uiScale)
    if l.close.contains(mx, my) then
      if releaseHeldItem() then enterGame()
    else if l.furnace.contains(mx, my) then
      if releaseHeldItem() then openFurnace()
    else if l.journal.contains(mx, my) then
      if releaseHeldItem() then screen = Screen.Journal
    else if l.output.contains(mx, my) then tryCraftGrid(all = quickMove)
    else
      val hot = l.hotbar.indexWhere(_.contains(mx, my))
      val storage = l.storage.indexWhere(_.contains(mx, my))
      val craft = l.craft.indexWhere(_.contains(mx, my))
      if quickMove then
        if hot >= 0 then quickMoveHotbar(hot)
        else if storage >= 0 then quickMoveBackpack(storage)
        else if craft >= 0 then quickMoveCraft(craft)
      else if hot >= 0 then
        if heldInventoryBlock != Block.Air then placeHeldItemInHotbar(hot, rightClick)
        else
          selectedBlock = hot; takeHotbarSlot(hot, rightClick)
        selectedItemLabelTimer = 1.2f
      else if storage >= 0 then handleBackpackSlotClick(storage, rightClick)
      else if craft >= 0 then handleCraftSlotClick(craft, rightClick)

  private def handleCatalogClick(
      mx: Float,
      my: Float,
      rightClick: Boolean = false,
      quickMove: Boolean = false
  ): Unit =
    val l = BlockboxUiLayout.catalog(framebufferWidth, framebufferHeight, uiScale)
    if l.close.contains(mx, my) then
      if releaseHeldItem() then enterGame()
      return
    catalogSearchFocused = l.search.contains(mx, my)
    if catalogSearchFocused then return
    val tab = l.tabs.indexWhere(_.contains(mx, my))
    if tab >= 0 then
      if tab == 5 then
        if releaseHeldItem() then screen = Screen.Inventory
      else { catalogCategory = tab; catalogScroll = 0 }
      return
    val hot = l.hotbar.indexWhere(_.contains(mx, my))
    if hot >= 0 then
      selectedBlock = hot
      if quickMove then quickMoveHotbar(hot)
      else if heldInventoryBlock == Block.Air then takeHotbarSlot(hot, rightClick)
      else placeHeldItemInHotbar(hot, rightClick)
      return
    val items = catalogItems
    val scrollMax = ((items.length + 8) / 9 - 6).max(0)
    catalogScroll = catalogScroll.max(0).min(scrollMax)
    val slot = l.slots.indexWhere(_.contains(mx, my))
    val index = slot + catalogScroll * 9
    if slot >= 0 && index < items.length then
      if quickMove then creativeQuickGive(items(index)) else setHeldItem(items(index), maxStackSize)
    else if l.scroll.contains(mx, my) then
      catalogScroll = math.round((my - l.scroll.y) / l.scroll.h * scrollMax).max(0).min(scrollMax)

  private def inRect(px: Float, py: Float, x: Float, y: Float, w: Float, h: Float): Boolean =
    px >= x && px <= x + w && py >= y && py <= y + h

  private def setCursorCaptured(captured: Boolean): Unit =
    glfwSetInputMode(
      window,
      GLFW_CURSOR,
      if captured then GLFW_CURSOR_DISABLED else GLFW_CURSOR_NORMAL
    )
    if glfwRawMouseMotionSupported() then
      glfwSetInputMode(window, GLFW_RAW_MOUSE_MOTION, if captured then GLFW_TRUE else GLFW_FALSE)

  private def mouseFramebufferPos(): (Float, Float) =
    if window == 0L then return (framebufferWidth / 2f, framebufferHeight / 2f)
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
    startGame()
    modManager.fireWorldLoaded()

  private def cleanPlayerName(): String =
    BlockboxNetworkText.cleanPlayerName(
      playerName,
      ((System.currentTimeMillis() / 1000) % 999).toInt
    )

  protected def networkEscape(value: String): String =
    BlockboxNetworkText.escape(value)

  private def networkUnescape(value: String): String =
    BlockboxNetworkText.unescape(value)

  protected def networkFloat(value: Float): String = BlockboxNetworkText.floatText(value)

  private def parseNetworkFloat(value: String): Float = BlockboxNetworkText.parseFloat(value)

  protected def networkSafeName(value: String): String =
    BlockboxNetworkText.safeName(value)

  private def normalizeColorId(id: Int): Int =
    BlockboxNetworkText.normalizeColorId(id, playerColorPalette.length)

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
      if value then oppedPlayerNames += safe
      else oppedPlayerNames.find(_.equalsIgnoreCase(safe)).foreach(n => oppedPlayerNames -= n)
      if announce then
        addChatMessage(
          if value then s"$safe is now an operator" else s"$safe is no longer an operator"
        )

  protected def broadcastOpState(name: String, value: Boolean): Unit =
    val safe = networkSafeName(name)
    if gameServer != null && safe.nonEmpty then
      gameServer.broadcast("OP|" + networkEscape(safe) + "|" + (if value then "1" else "0"))

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
      applyWorldSeed(freshWorldSeed(), freshWorldName = false)
      worldName = uniqueWorldFolderName(
        if worldNameInput.trim.nonEmpty then worldNameInput else "Hosted World"
      )
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
      try if sock != null then sock.close()
      catch case _: Exception => ()

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
        () => publishedRules,
        () => heldHotbarBlock.id & 0xff,
        () => playerCrouching
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
          hostStatusMessage =
            s"Hosting as $playerName. Server modpack: ${modManager.serverModpackHash}. Join locally with 127.0.0.1:${server.localPort}, or share your LAN/ZeroTier IP."
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
        hostStatusMessage =
          s"Port $serverPort is already in use. Close the old server or use another port."
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
      case Right((host, port)) =>
        joinStatusError = false
        joinStatusMessage = s"Connecting to $host:$port ..."
        val client = GameClient(host, port)
        gameClient = client; pendingJoin = client
        val name = playerName; val hash = modManager.clientJoinHash
        BlockboxSockets
          .daemonThread(
            "blockbox-join",
            () =>
              val connected = client.connect(name, hash)
              if pendingJoin eq client then joinResult = (client, connected)
              else client.disconnect()
          )
          .start()

  private def completePendingJoin(): Unit =
    val result = joinResult
    if result != null then
      joinResult = null
      val (client, success) = result
      if pendingJoin eq client then
        pendingJoin = null
        if success then
          multiplayerMode = true; applyingJoin = true
          knownPlayerNames.clear(); knownPlayerNames += playerName
          playerColors.clear(); oppedPlayerNames.clear()
          worldSeed = 0L; terrainGen = createTerrainGenerator(0L); terrainHeightCache.clear()
          clearLoadedChunks(saveFirst = false)
          fieldJournal = FieldJournal(); worldClock = 150.0; timeOverride = None
          gameMode = GameMode.Survival; resetInventory()
          velocity = Vec3(0f, 0f, 0f); onGround = false; yaw = 0f; pitch = 0f
          joinStatusMessage = "Synchronizing the world..."
        else
          val message = client.errorMessage
          client.disconnect(); gameClient = null
          joinStatusError = true
          joinStatusMessage =
            if message.nonEmpty then message else "Could not connect to that server."
    if applyingJoin && gameClient != null then
      processNetworkMessages()
      syncChunks(); processChunkWorkMainThread(0, 8)
      if !gameClient.isConnected then
        joinStatusError = true;
        joinStatusMessage = "Server disconnected during world synchronization."
        applyingJoin = false
      else if gameClient.pendingMessages == 0 then
        applyingJoin = false
        startGame()
        modManager.fireWorldLoaded()
      else
        joinStatusMessage = s"Synchronizing the world... ${gameClient.pendingMessages} updates left"

  private def stopNetworking(): Unit =
    if gameClient != null && guestSessionKey.nonEmpty then
      val bytes = new java.io.ByteArrayOutputStream()
      val out = new java.io.DataOutputStream(bytes)
      writeWorldExtras(out); out.flush()
      guestCarry(guestSessionKey) = GuestCarry(
        bytes.toByteArray,
        fieldJournal,
        appliedItemOffers.toSet,
        pendingTosses.toMap,
        failedTosses.toSet
      )
      while guestCarry.size > 16 do guestCarry.remove(guestCarry.head._1)
    guestSessionKey = ""
    appliedItemOffers.clear(); pendingTosses.clear(); failedTosses.clear()
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
    walkingGait.reset()
    cameraBobAmount = 0f
    setCursorCaptured(captured = true)

  private def enterGame(): Unit =
    if (screen == Screen.Inventory || screen == Screen.Catalog || screen == Screen.FurnaceUI) && !releaseHeldItem()
    then return
    startGame()

  private def leaveGame(next: Screen): Unit =
    // Pausing must NOT close the multiplayer server. The host needs to be able to tab away,
    // open the pause menu, or focus another local test client while the socket keeps listening.
    // Only tear networking down when actually returning to title/main menu.
    if next == Screen.MainMenu then
      val remotePositions = remoteFurnaces.values.toVector
      remoteFurnaces.clear(); remotePositions.foreach(refreshFurnaceLight)
      saveWorld()
      modManager.fireWorldUnloaded()
      updateRainAmbience(0f)
      updateFurnaceAmbience(0f)
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
    try BlockboxWorld.writeWorldIndex(worldsRootDir)
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
    out.writeBoolean(furnaceSlots)
    out.writeInt(furnaceInputCount.max(0).min(maxStackSize))
    out.writeInt(furnaceFuelCount.max(0).min(maxStackSize))
    out.writeBoolean(furnacePaused)
    out.writeFloat(snowCover)
    out.writeByte(heldInventoryBlock.id)
    out.writeInt(heldInventoryCount.max(0).min(maxStackSize))
    out.writeBoolean(heldFromHotbar)
    leafDecay.write(out)

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
              hotbarCounts(i) =
                if gameMode == GameMode.Creative && hotbarBlocks(i) != Block.Air then maxStackSize
                else 0
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
                val b = Block.fromId(in.readByte());
                val count = in.readInt().max(0).min(maxStackSize)
                if si < craftGridBlocks.length && validHotbarBlock(b) && count > 0 then
                  craftGridBlocks(si) = b
                  craftGridCounts(si) = count
                si += 1
            if version >= 4 then
              furnaceSlots = in.readBoolean()
              furnaceInputCount = in.readInt().max(0).min(maxStackSize)
              furnaceFuelCount = in.readInt().max(0).min(maxStackSize)
              furnacePaused = in.readBoolean()
              val snow = in.readFloat()
              snowCover = if snow.isFinite then snow.max(0).min(1) else 0f
              val cursorBlock = Block.fromId(in.readByte())
              val cursorCount = in.readInt().max(0).min(maxStackSize)
              val physicalCursor = in.readBoolean()
              setHeldItem(cursorBlock, cursorCount, physicalCursor)
              leafDecay.read(in)
              if furnaceSlots then
                if furnaceInputCount == 0 then furnaceInput = Block.Air
                if furnaceFuelCount == 0 then furnaceFuel = Block.Air
            syncInventoryTotalsFromBackpack()
          else if gameMode == GameMode.Survival then rebuildBackpackFromInventoryTotals()
    catch case e: Exception => System.err.println(s"World extras load skipped: $e")

  private def snapshotWorldEditsForNetwork(): Seq[(Int, Int, Int, Byte, Int, Int, Int)] =
    val loadedEdits = chunks.values.toSeq.flatMap { chunk =>
      val bx = chunk.baseX
      val bz = chunk.baseZ
      chunk.edits.synchronized {
        chunk.edits.toList.map { case ((lx, ly, lz), block) =>
          (
            bx + lx,
            ly,
            bz + lz,
            block.id,
            chunk.getWaterRawLevel(lx, ly, lz),
            chunk.getLogAxis(lx, ly, lz).id,
            chunk.getTorchMount(lx, ly, lz).id
          )
        }
      }
    }
    val rememberedEdits = networkBlockOverrides.toSeq.map { case ((x, y, z), block) =>
      (
        x,
        y,
        z,
        block.id,
        networkWaterLevelOverrides.getOrElse((x, y, z), 0.toByte).toInt & 0xff,
        networkLogAxesOverrides.getOrElse((x, y, z), 0),
        networkTorchMounts.getOrElse((x, y, z), 0)
      )
    }
    (loadedEdits ++ rememberedEdits).reverse.distinctBy(t => (t._1, t._2, t._3)).reverse

  private def startWorldSaveThread(): Unit =
    if worldSaveExecutor == null || worldSaveExecutor.isShutdown then
      worldSaveExecutor = Executors.newSingleThreadExecutor((job: Runnable) =>
        val t = Thread(job, "blockbox-save-io")
        t.setDaemon(true)
        t.setPriority(Thread.NORM_PRIORITY - 1)
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
        finally pendingSaveJobs.decrementAndGet())

  /** Wait only when correctness requires it (loading or process shutdown), never while returning to
    * the title screen. A single-thread executor means waiting for the newest future also waits for
    * every older save in order.
    */
  private def awaitPendingWorldSaves(): Unit =
    val f = latestSaveFuture
    if f != null then
      try f.get()
      catch
        case _: java.util.concurrent.CancellationException => ()
        case _: InterruptedException                       => Thread.currentThread().interrupt()
        case e: java.util.concurrent.ExecutionException    =>
          System.err.println(s"Blockbox save wait failed: ${e.getCause}")

  private def shutdownWorldSaveThread(): Unit =
    val exec = worldSaveExecutor
    if exec != null then
      exec.shutdown()
      try if !exec.awaitTermination(30, TimeUnit.SECONDS) then exec.shutdownNow()
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
    meta.writeInt(
      0x454e5631
    ) // ENV1: optional clock/weather/journal tail; v3 inventory stays compatible.
    meta.writeUTF(environmentMessage)
    fieldJournal.write(meta)
    meta.writeInt(0x52454731) // REG1 custom voxel ID mapping
    meta.writeUTF(CustomBlocks.manifest)
    meta.writeUTF(modManager.rules.snapshot)
    meta.writeInt(
      0x53555231
    ) // SUR1: additive survival state; all older inventory ids remain stable.
    meta.writeFloat(airSupply.remaining)
    meta.writeInt(furnaceRequested)
    fieldJournal.writeProgress(meta)
    meta.writeUTF(ZombieSystem.snapshot(zombies.toVector))
    meta.writeInt(0x41445631) // ADV1: optional objectives, travel and claimed rewards.
    fieldJournal.writeAdventure(meta)
    meta.writeInt(0x46555231) // FUR1: position of this player's active furnace.
    meta.writeBoolean(furnacePosition.nonEmpty)
    furnacePosition.foreach { (x, y, z) => meta.writeInt(x); meta.writeInt(y); meta.writeInt(z) }
    meta.writeInt(0x49544d31) // ITM1: dropped world items, appended to legacy tails.
    meta.writeUTF(droppedItems.saveSnapshot)
    meta.writeInt(0x43434831) // CCH1: ruin salvage objective.
    fieldJournal.writeDiscoveries(meta)
    meta.flush()
    raw.toByteArray

  private def saveLoadedChunks(): Unit =
    if !canUseLocalChunkSaves then return
    try
      val prepared = BlockboxSaveFiles.prepareWorldForSave(currentWorldDir.toPath)
      val snapshots = chunks.toVector.map { case ((cx, cz), chunk) =>
        (cx, cz, chunk.captureSaveSnapshot())
      }
      snapshots.foreach { case (cx, cz, _) => dirtyChunksForSave -= ((cx, cz)) }
      queueWorldSaveJob(s"${prepared.worldDirectory().getFileName} chunk batch") {
        snapshots.foreach { case (cx, cz, snapshot) =>
          BlockboxSaveFiles.writeChunkBytesAtomic(
            prepared.chunksDirectory(),
            cx,
            cz,
            snapshot.toBytes(),
            false
          )
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
          BlockboxSaveFiles.writeChunkBytesAtomic(
            prepared.chunksDirectory(),
            cx,
            cz,
            snapshot.toBytes(),
            false
          )
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
    else loadWorldFromDir(saves(loadWorldSelection.max(0).min(saves.length - 1)))

  private def loadWorldDataFromDir(worldDir: java.io.File): Boolean =
    // If the user immediately re-enters a world after Save and Quit, finish the
    // background write here rather than loading the previous on-disk snapshot.
    awaitPendingWorldSaves()
    try
      worldName = worldDir.getName
      BlockboxSaveFiles.readWorldData(
        worldDir.toPath,
        meta =>
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
          airSupply.reset(); zombies.clear(); zombieSerial = 0; RainRenderer.reset();
          leafDecay.clear()
          networkBlockOverrides.clear()
          val chunkCount = meta.readInt().max(0).min(200000)
          val storedChunks = Vector.fill(chunkCount)((meta.readInt(), meta.readInt()))
          readWorldExtras(meta)
          worldClock = 150.0; timeOverride = None; fieldJournal = FieldJournal()
          furnacePosition = None; furnaceBurning = false; remoteFurnaces.clear();
          nearbyFurnaceSounds = Array.empty
          if meta.available() >= 4 && meta.readInt() == 0x454e5631 then
            val env = BlockboxNet.parseEnvironment(BlockboxNet.packet(meta.readUTF()))
            worldClock = env.time;
            timeOverride = if env.timeOverride < 0f then None else Some(env.timeOverride)
            rainStrength = env.rain; wetness = env.wetness; stormCloudCover = env.cloudCover
            weatherWind = env.wind; lightningFlash = env.lightning
            fieldJournal.read(meta)
            if meta.available() >= 4 && meta.readInt() == 0x52454731 then
              require(
                meta.readUTF() == CustomBlocks.manifest,
                "This world requires its original custom-block registry. Restore the world's modpack before loading it."
              )
              modManager.rules.receive(meta.readUTF())
              if meta.available() >= 4 && meta.readInt() == 0x53555231 then
                val air = meta.readFloat()
                airSupply.remaining =
                  if air.isFinite then air.max(0f).min(airSupply.capacity) else airSupply.capacity
                furnaceRequested = meta.readInt().max(0).min(64)
                fieldJournal.readProgress(meta)
                ZombieSystem.parseSnapshot(meta.readUTF()).foreach {
                  (id, pos, angle, hp, burn, hurt, gear) =>
                    val z = ZombieSystem.Zombie(id, pos); z.yaw = angle; z.bodyYaw = angle;
                    z.targetYaw = angle; z.health = hp; z.burning = burn; z.heldItem = gear
                    zombies += z; zombieSerial = zombieSerial.max(id + 1)
                }
                if meta.available() >= 4 && meta.readInt() == 0x41445631 then
                  fieldJournal.readAdventure(meta)
                if meta.available() >= 4 && meta.readInt() == 0x46555231 then
                  if meta.readBoolean() then
                    val x = meta.readInt(); val y = meta.readInt(); val z = meta.readInt()
                    require(y >= 0 && y < Terrain.worldHeight, "Invalid furnace position")
                    furnacePosition = Some((x, y, z))
                if meta.available() >= 4 && meta.readInt() == 0x49544d31 then
                  droppedItems.receive(meta.readUTF(), interpolate = false)
                if meta.available() >= 4 && meta.readInt() == 0x43434831 then
                  fieldJournal.readDiscoveries(meta)
          storedChunks.foreach((cx, cz) => loadChunkIfSaved(cx, cz))
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
    else if validHotbarBlock(block) then backpackItemCount(block) + hotbarItemCount(block)
    else 0

  protected def noteCollected(block: Block, amount: Int): Unit =
    if gameMode == GameMode.Survival then fieldJournal.collected(block, amount)

  protected def gainItem(block: Block, amount: Int): Unit =
    if validHotbarBlock(block) && amount > 0 then
      noteCollected(block, amount)
      var remaining = amount
      locally {
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
      }
      if remaining > 0 then
        val stored = addBackpackItem(block, remaining)
        remaining -= stored
      if remaining > 0 && gameMode == GameMode.Survival then
        addChatMessage(s"Inventory full: $remaining ${blockName(block)} could not be stored")
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
      val event =
        modManager.fireChat(ModChatEvent(modManager.api, playerName, text, cancelled = false))
      val finalText = Option(event.message).getOrElse("").trim
      if !event.cancelled && finalText.nonEmpty then
        if finalText.startsWith("/") then parseCommand(finalText)
        else sendChatMessage(finalText)
    chatOpen = false
    chatInput = ""
    chatHistoryDraft = ""
    if window != 0L then
      setCursorCaptured(captured = true); firstMouse = true

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
    if !releaseHeldItem() then return
    def isFurnace(p: (Int, Int, Int)): Boolean =
      val b = activeBlockAt(p._1, p._2, p._3); b == Block.Furnace || b == Block.FurnaceLit
    val nearby = (for x <- -4 to 4; y <- -3 to 2; z <- -4 to 4
    yield (floor(camera.x).toInt + x, floor(camera.y).toInt + y, floor(camera.z).toInt + z))
      .filter(isFurnace)
    val selected = raycast()
      .map(_.block)
      .filter(isFurnace)
      .orElse(nearby.minByOption { (x, y, z) =>
        val d = Vec3(x + 0.5f, y + 0.5f, z + 0.5f) - camera; d.lengthSquared
      })
    if selected.isEmpty then
      addChatMessage("Place a stone furnace nearby to smelt. Press J for the survival guide.")
      return
    if furnacePosition != selected then
      furnaceRequested = 0; furnaceProgress = 0f; noteFurnaceBurning(false)
      furnacePosition = selected
    furnaceScroll = 0
    enableFurnaceSlots()
    screen = Screen.FurnaceUI
    setCursorCaptured(captured = false)
    breakingBlock = null
    breakingProgress = 0f

  override protected def furnaceAvailable: Boolean =
    !activeWorldSession || furnacePosition.exists { (x, y, z) =>
      val b = getWorldBlock(x, y, z); b == Block.Furnace || b == Block.FurnaceLit
    }

  private def refreshFurnaceLight(p: (Int, Int, Int)): Unit =
    if !isClientOnlyMultiplayer then
      val (x, y, z) = p; val old = getWorldBlock(x, y, z)
      if old == Block.Furnace || old == Block.FurnaceLit then
        val lit =
          (furnaceBurning && furnacePosition.contains(p)) || remoteFurnaces.valuesIterator.contains(
            p
          )
        val block = if lit then Block.FurnaceLit else Block.Furnace
        if old != block then
          setActiveBlock(x, y, z, block); dirtyChunkAt(x, z)
          sendBlockNetwork(x, y, z, block.id)

  override protected def noteFurnaceBurning(burning: Boolean): Unit =
    if furnaceBurning != burning then
      furnaceBurning = burning
      furnacePosition.foreach { p =>
        if isClientOnlyMultiplayer then
          gameClient.send(s"FIRE|${p._1}|${p._2}|${p._3}|${if burning then 1 else 0}")
        else refreshFurnaceLight(p)
      }
    val source = nearbyFurnaceSounds
      .minByOption(p => (p - camera).lengthSquared)
      .orElse(
        furnacePosition.filter(_ => burning).map((x, y, z) => Vec3(x + 0.5f, y + 0.5f, z + 0.5f))
      )
    val (gain, pan) = source.map(furnaceAudioPosition).getOrElse((0f, 0f))
    updateFurnaceAmbience(gain, pan)

  private def furnaceAudioPosition(position: Vec3): (Float, Float) =
    val d = position - camera; val distance = sqrt(d.lengthSquared).toFloat
    val angle = toRadians(yaw)
    val pan = ((d.x * cos(angle) + d.z * sin(angle)) / distance.max(1f)).toFloat.max(-1f).min(1f)
    val gain = pow((1f - distance / 18f).max(0), 2).toFloat * (if isUnderwater then 0.18f else 1f)
    (gain, pan)

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
    updateDroppedItems(dt)
    syncChunks()
    // Keep the camera neighborhood deterministic. If the local terrain is not
    // ready, build the 5x5 ring right now instead of rendering a sky-only void.
    if !centerChunkReady then forceCameraChunkRing(0)
    processChunkWorkMainThread(0, chunkUploadBudget)
    updateGrassGrowth(dt)
    updateLeafDecay(dt)
    for i <- chatMessages.indices do
      val (msg, t) = chatMessages(i)
      chatMessages(i) = (msg, t - dt)
    chatMessages.filterInPlace((_, t) => t > -2f)
    cleanupRemotePlayers()
    updateEnvironmentalDamage(dt)
    updateBreathing(dt)
    updateHealthRegen(dt)
    updateZombies(dt)
    updatePlayerKnockback(dt)
    if chatOpen || modUiCursorMode then
      updatePassivePlayerPhysics(dt)
      updateWeather(dt)
      updateBlockParticles(dt)
      updateTorchEffects(dt)
      updateGameFeel(dt, camera, false, false, false)
    if chatOpen then
      // Chat is also a client-side UI state in multiplayer; keep the player present/alive.
      sendPlayerPositionNetwork()
      return
    if modUiCursorMode then
      handleModGuiMouse()
      sendPlayerPositionNetwork()
      return
    if !windowFocused then
      firstMouse = true
      updatePassivePlayerPhysics(dt)
      updateWeather(dt)
      updateBlockParticles(dt)
      updateTorchEffects(dt)
      updateGameFeel(dt, camera, false, false, false)
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
    val wantsCrouch = !flyEnabled && onGround && blockAt(
      camera - Vec3(0, 0.8f, 0)
    ) != Block.Water && (down(GLFW_KEY_LEFT_SHIFT) || down(GLFW_KEY_RIGHT_SHIFT))
    playerCrouching = wantsCrouch || (playerCrouching && collidesPlayer(camera, crouched = false))
    crouchAmount = PlayerStance.approach(crouchAmount, playerCrouching, dt)
    val crouching = playerCrouching
    gameMode match
      case GameMode.Creative =>
        val spaceDown = down(GLFW_KEY_SPACE)
        val spacePressed = spaceDown && !wasSpaceDown
        wasSpaceDown = spaceDown
        val now = glfwGetTime()
        if spacePressed then
          if now - lastSpacePressTime < doubleTapInterval then
            toggleFlyForPlayer(playerName)
            lastSpacePressTime = Double.NegativeInfinity
          else lastSpacePressTime = now
        if flyEnabled then
          val speed = MovementSpeeds.flight(fastMove)
          if down(GLFW_KEY_SPACE) then move += Vec3(0, 1, 0)
          if down(GLFW_KEY_LEFT_SHIFT) then move -= Vec3(0, 1, 0)
          val flight = move.normalized * speed * dt
          movePlayer(flight.x, flight.y, flight.z)
          onGround = false
        else
          val submerged = waterSubmersion
          val headInWater = blockAt(camera) == Block.Water
          val bodyInWater = blockAt(camera - Vec3(0f, 0.8f, 0f)) == Block.Water
          val swimming = headInWater || bodyInWater || submerged > 0f
          val speed = MovementSpeeds.ground(swimming, sprinting, crouching, fastMove)
          val hMove = move.normalized * speed * modManager.rules.get("moveSpeed") * dt
          // Sneaking on the ground stops at edges in either game mode, as in Minecraft.
          if crouching && onGround && !swimming then
            val hx = if hasGroundBelow(camera + Vec3(hMove.x, 0f, 0f)) then hMove.x else 0f
            val hz = if hasGroundBelow(camera + Vec3(0f, 0f, hMove.z)) then hMove.z else 0f
            movePlayer(hx, 0f, hz)
          else movePlayer(hMove.x, 0f, hMove.z)
          if swimming then
            updateSwimming(dt, swimming, headInWater, bodyInWater, submerged)
            moveVertical(velocity.y * dt)
            onGround = false
          else
            if spacePressed && onGround then
              velocity = Vec3(0f, modManager.rules.get("jumpSpeed"), 0f)
              onGround = false
            velocity = Vec3(0f, (velocity.y - modManager.rules.get("gravity") * dt).max(-78f), 0f)
            moveVertical(velocity.y * dt)
      case GameMode.Survival =>
        val submerged = waterSubmersion
        val headInWater = blockAt(camera) == Block.Water
        val bodyInWater = blockAt(camera - Vec3(0f, 0.8f, 0f)) == Block.Water
        val swimming = headInWater || bodyInWater || submerged > 0f
        val speed = MovementSpeeds.ground(swimming, sprinting, crouching, fastMove)
        val horizontal = move.normalized * speed * modManager.rules.get("moveSpeed") * dt
        if crouching && onGround then
          val hx = if hasGroundBelow(camera + Vec3(horizontal.x, 0f, 0f)) then horizontal.x else 0f
          val hz = if hasGroundBelow(camera + Vec3(0f, 0f, horizontal.z)) then horizontal.z else 0f
          movePlayer(hx, 0f, hz)
        else movePlayer(horizontal.x, 0f, horizontal.z)
        updateSwimming(dt, swimming, headInWater, bodyInWater, submerged)
        moveVertical(velocity.y * dt)
        if swimming then onGround = false
    if modManager.events.hasListeners("move") then
      val event = modManager.events.fireMove(
        ModMoveEvent(modManager.api, playerName, frameStartCamera, camera, dt)
      )
      val dest = event.destination
      if event.cancelled then camera = frameStartCamera
      else if dest != null && Seq(dest.x, dest.y, dest.z).forall(_.isFinite) then camera = dest
    updateJournal(dt)
    updateGameFeel(dt, frameStartCamera, moving, sprinting, crouching)
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

  override protected def onFlightChanged(): Unit =
    onGround = false
    smoothedHorizontalSpeed = 0f; walkingGait.reset(); cameraBobAmount = 0f
    cameraRoll = 0f; viewFovOffset = 0f

  private def updateGameFeel(
      dt: Float,
      before: Vec3,
      moving: Boolean,
      sprinting: Boolean,
      crouching: Boolean
  ): Unit =
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
    val groundedForFeel =
      !flyEnabled && (onGround || (velocity.y <= 0.05f && hasGroundBelow(camera)))
    val groundedMotion = groundedForFeel && moving && horizontalDistance > 0.0001f && !submerged
    if gameMode == GameMode.Survival && !flyEnabled && moving && (groundedForFeel || submerged) then
      fieldJournal.travel(horizontalDistance, submerged)
    val bobTarget =
      if groundedMotion then (smoothedHorizontalSpeed / 5.5f).max(0f).min(1.15f) else 0f
    cameraBobAmount =
      approachExp(cameraBobAmount, bobTarget, if groundedMotion then 14f else 9f, dt)
    val fovTarget =
      if submerged then 1.5f else if sprinting && moving && !crouching then 7.5f else 0f
    viewFovOffset = approachExp(viewFovOffset, fovTarget, 8.5f, dt)
    val strafe = (if down(GLFW_KEY_D) then 1f else 0f) - (if down(GLFW_KEY_A) then 1f else 0f)
    val rollTarget =
      if moving && !submerged then -strafe * (if sprinting then 0.38f else 0.24f) else 0f
    cameraRoll = approachExp(cameraRoll, rollTarget, 12f, dt)
    if groundedMotion then
      val stepDistance = if crouching then 1.90f else if sprinting then 1.30f else 1.58f
      val contact = walkingGait.advance(horizontalDistance, stepDistance)
      cameraBobPhase = walkingGait.phase
      if contact then playFootstepSound(blockUnderPlayer, sprinting)
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
    viewmodelYaw = (viewmodelYaw + (((yaw - viewmodelYaw) % 360f + 540f) % 360f - 180f) * (1f - exp(
      -dt * 16f
    ).toFloat)) % 360f
    viewmodelPitch = approachExp(viewmodelPitch, pitch, 16f, dt)
    val obstruction = FirstPersonPose.obstruction(eyePosition, yaw, pitch, getWorldBlock)
    viewmodelObstruction = approachExp(
      viewmodelObstruction,
      obstruction,
      if obstruction > viewmodelObstruction then 20f else 10f,
      dt
    )

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
    // Menus release input, not gravity, buoyancy or combat. The host must keep
    // simulating the world for guests even while its own inventory is open.
    processNetworkMessages()
    updateWeather(dt)
    smoothRemotePlayers(dt)
    updateDroppedItems(dt)
    syncChunks()
    if !centerChunkReady then forceCameraChunkRing(0)
    processChunkWorkMainThread(0, 8)
    for i <- chatMessages.indices do
      val (msg, t) = chatMessages(i)
      chatMessages(i) = (msg, t - dt)
    chatMessages.filterInPlace((_, t) => t > -2f)
    cleanupRemotePlayers()
    updateGrassGrowth(dt)
    updateLeafDecay(dt)
    updateBreathing(dt)
    updateEnvironmentalDamage(dt)
    updateHealthRegen(dt)
    updateZombies(dt)
    updatePlayerKnockback(dt)
    val before = camera
    updatePassivePlayerPhysics(dt)
    updateGameFeel(dt, before, false, false, playerCrouching)
    updateWater(dt)
    updateSandFalling()
    updateBubbles(dt)
    updateBlockParticles(dt)
    updateSandParticles(dt)
    updateTorchEffects(dt)
    // Keep chunk meshes alive while multiplayer menus are open, but cap work so UI remains responsive.
    processChunkWorkMainThread(0, 8)
    flushDirtyChunks()
    sendPlayerPositionNetwork()

  private def updatePassivePlayerPhysics(dt: Float): Unit =
    crouchAmount = PlayerStance.approach(crouchAmount, playerCrouching, dt)
    if flyEnabled then return
    val submerged = waterSubmersion
    val headInWater = blockAt(camera) == Block.Water
    val bodyInWater = blockAt(camera - Vec3(0, 0.8f, 0)) == Block.Water
    val swimming = headInWater || bodyInWater || submerged > 0
    updateSwimming(dt, swimming, headInWater, bodyInWater, submerged, allowInput = false)
    moveVertical(velocity.y * dt)
    if swimming then onGround = false

  private def updateSwimming(
      dt: Float,
      swimming: Boolean,
      headInWater: Boolean,
      bodyInWater: Boolean,
      submerged: Float,
      allowInput: Boolean = true
  ): Unit =
    if !swimming then
      if allowInput && onGround && down(GLFW_KEY_SPACE) then
        velocity = Vec3(0f, modManager.rules.get("jumpSpeed"), 0f)
        onGround = false
      else velocity = Vec3(0f, (velocity.y - modManager.rules.get("gravity") * dt).max(-78f), 0f)
      return
    val swimUp = allowInput && down(GLFW_KEY_SPACE)
    val swimDown = allowInput && (down(GLFW_KEY_LEFT_SHIFT) || down(GLFW_KEY_RIGHT_SHIFT))
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
    val attacking = leftDown && attackTargetEntity()
    if !attacking && leftDown && !leftWasDown && gameMode == GameMode.Creative then
      breakTargetBlock(dropItem = false)
    if attacking then resetBreaking()
    else if gameMode == GameMode.Survival then updateBlockBreaking(lastFrameTime.toFloat, leftDown)
    else if !leftDown then resetBreaking()
    if rightDown && !rightWasDown then placeTargetBlock()
    leftWasDown = leftDown
    rightWasDown = rightDown

  private def updateBlockBreaking(dt: Float, leftDown: Boolean): Unit =
    if !leftDown then
      resetBreaking()
      return
    val targetHit = raycast()
    if targetHit.isEmpty then { resetBreaking(); return }
    miningSoundCooldown = (miningSoundCooldown - dt).max(0)
    targetHit.foreach { hit =>
      val target = hit.block
      val block = activeBlockAt(target._1, target._2, target._3)
      if (!block.solid && block != Block.Torch && !FloraModel.contains(
          block
        )) || block == Block.Bedrock
      then resetBreaking()
      else
        val sameTarget = breakingBlock != null && breakingBlock == target
        if !sameTarget then
          breakingBlock = target
          breakingProgress = 0f
          miningSoundCooldown = 0f
        if miningSoundCooldown <= 0f then
          miningSoundVariant = (miningSoundVariant + 1) % 64
          playSpatialSound(BlockboxSounds.Action.Dig, block, miningSoundVariant, 0.40f, 0)
          if gameClient != null then
            gameClient.send(s"DIG|${target._1}|${target._2}|${target._3}|$miningSoundVariant")
          else
            broadcastActionSound(
              playerName,
              BlockboxSounds.Action.Dig,
              block,
              miningSoundVariant,
              Vec3(target._1 + 0.5f, target._2 + 0.5f, target._3 + 0.5f)
            )
          miningSoundCooldown = 0.23f
        breakingProgress += dt * modManager.rules.get("miningSpeed") * ToolSystem.miningSpeed(
          block,
          heldHotbarBlock
        ) * (if isUnderwater then 0.35f else 1f) / blockHardness(block)
        if breakingProgress >= 1f then
          breakTargetBlock(dropItem = true)
          resetBreaking()
    }

  private def resetBreaking(): Unit =
    breakingBlock = null
    breakingProgress = 0f
    miningSoundCooldown = 0f

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
        var top = Terrain.worldHeight - 2
        while top > 0 && chunk.getBlock(lx, top, lz) != Block.Dirt && chunk.getBlock(
            lx,
            top,
            lz
          ) != Block.Grass
        do top -= 1
        val y = if grassGrowthRng.nextInt(4) == 0 then
          grassGrowthRng.nextInt(Terrain.worldHeight - 1)
        else top
        val wx = chunk.cx * Terrain.chunkSize + lx
        val wz = chunk.cz * Terrain.chunkSize + lz
        if chunk.getBlock(lx, y, lz) == Block.Dirt && canGrowGrassAt(wx, y, wz) then
          setActiveBlock(wx, y, wz, Block.Grass)
          dirtyChunkAt(wx, wz)
          if multiplayerMode && gameServer != null then
            sendBlockNetwork(wx, y, wz, Block.Grass.ordinal.toByte)
        else if chunk.getBlock(lx, y, lz) == Block.Grass then
          val above = activeBlockAt(wx, y + 1, wz)
          if above == Block.Water || (above.solid && !above.cutout && !above.translucent) then
            setActiveBlock(wx, y, wz, Block.Dirt); dirtyChunkAt(wx, wz)
            if gameServer != null then sendBlockNetwork(wx, y, wz, Block.Dirt.id)
      i += 1

  private def updateLeafDecay(dt: Float): Unit =
    if gameClient == null then
      leafDecay.update(
        dt,
        activeBlockAt,
        (x, z) => loadedChunkForBlock(x, z).nonEmpty,
        (x, y, z) =>
          loadedChunkForBlock(x, z).exists { chunk =>
            chunk.edits.synchronized {
              chunk.edits
                .get((Math.floorMod(x, 16), y, Math.floorMod(z, 16)))
                .exists(BlockTints.isFoliage)
            }
          },
        (x, y, z) =>
          val block = activeBlockAt(x, y, z)
          spawnBlockBreakParticles(x, y, z, block)
          setActiveBlock(x, y, z, Block.Air); dirtyChunkAt(x, z)
          if gameServer != null then sendBlockNetwork(x, y, z, Block.Air.id)
          if Math.floorMod(x * 31 ^ y * 17 ^ z, 12) == 0 then
            spawnDrop(Block.Stick, 1, Vec3(x + 0.5f, y + 0.5f, z + 0.5f))
      )

  private def canGrowGrassAt(x: Int, y: Int, z: Int): Boolean =
    if y < 0 || y >= Terrain.worldHeight - 1 then false
    else
      val above = activeBlockAt(x, y + 1, z)
      if above == Block.Water || (above.solid && !above.cutout && !above.translucent) ||
        lightLevelsAt(x, y + 1, z)._1 < 9
      then false
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
                  if gy >= 0 && gy < Terrain.worldHeight && activeBlockAt(
                      x + dx,
                      gy,
                      z + dz
                    ) == Block.Grass
                  then
                    val grassAbove = if gy + 1 < Terrain.worldHeight then
                      activeBlockAt(x + dx, gy + 1, z + dz)
                    else Block.Air
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
    if !isClientOnlyMultiplayer && y >= 0 && y < Terrain.worldHeight && loadedChunkForBlock(
        x,
        z
      ).isDefined && activeBlockAt(x, y, z) == Block.Water
    then
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
    block == Block.Air || block == Block.Water || block == Block.Torch || FloraModel.contains(block)

  private def setFlowWater(x: Int, y: Int, z: Int, level: Int): Unit =
    if y >= 0 && y < Terrain.worldHeight && loadedChunkForBlock(x, z).isDefined then
      if activeBlockAt(x, y, z) != Block.Water then setActiveBlock(x, y, z, Block.Water)
      val nextLevel = level.max(1).min(8)
      setWaterLevelAt(x, y, z, nextLevel)
      networkBlockOverrides((x, y, z)) = Block.Water
      networkWaterLevelOverrides((x, y, z)) = nextLevel.toByte
      networkTorchMounts.remove((x, y, z))
      if multiplayerMode && gameServer != null then
        gameServer.broadcastBlockChange(x, y, z, Block.Water.id, nextLevel)
      markWaterActive(x, y, z)
      dirtyChunkAt(x, z)

  private def clearFlowWater(x: Int, y: Int, z: Int): Unit =
    if loadedChunkForBlock(x, z).isDefined && activeBlockAt(x, y, z) == Block.Water then
      setWaterLevelAt(x, y, z, 0)
      setActiveBlock(x, y, z, Block.Air)
      networkBlockOverrides((x, y, z)) = Block.Air
      networkWaterLevelOverrides.remove((x, y, z))
      if multiplayerMode && gameServer != null then
        gameServer.broadcastBlockChange(x, y, z, Block.Air.id)
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
          if source || supportedFromAbove || waterLevelAt(wx, wy, wz) > 1 then
            markWaterActive(wx, wy, wz)
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
    case b if FloraModel.contains(b) => 0.08f
    case Block.Leaves | Block.BirchLeaves | Block.PineLeaves | Block.AcaciaLeaves | Block.Snow =>
      0.18f
    case Block.Dirt | Block.Sand | Block.Grass => 0.42f
    case Block.Wood | Block.BirchWood | Block.PineWood | Block.AcaciaWood | Block.Planks |
        Block.BirchPlanks | Block.PinePlanks | Block.AcaciaPlanks =>
      0.75f
    case Block.Torch                                                          => 0.08f
    case Block.Glass                                                          => 0.35f
    case Block.Stone | Block.Coal | Block.Copper | Block.Clay | Block.IronOre => 1.15f
    case Block.GoldOre                                                        => 1.50f
    case Block.Brick | Block.Furnace | Block.FurnaceLit                       => 1.35f
    case Block.Diamond                                                        => 2.50f
    case Block.Bedrock                                                        => 9999f
    case _                                                                    => 0.5f

  private def damagePlayer(amount: Float, reason: String): Unit =
    if gameMode == GameMode.Survival && amount > 0f then
      val event =
        modManager.events.fireDamage(ModDamageEvent(modManager.api, playerName, reason, amount))
      if event.cancelled || !event.amount.isFinite then return
      val applied = event.amount.max(0f).min(1000f)
      val old = playerHealth
      playerHealth = (playerHealth - applied).max(0f)
      if old > playerHealth then
        timeSinceLastDamage = 0f
        hurtOverlay = 1f
        if isClientOnlyMultiplayer then gameClient.send("HURTME")
        else if gameServer != null then gameServer.broadcast(s"HURT|$playerName")
        playHurtSound()
        val shown = (math.ceil(applied.toDouble * 2.0) / 2.0).toFloat
        addChatMessage(s"Took $shown damage${if reason.nonEmpty then s" from $reason" else ""}")
      if playerHealth <= 0f then
        modManager.events.fireDeath(ModPlayerEvent(modManager.api, playerName, reason))
        val spawn = findSpawn()
        camera = spawn
        velocity = Vec3(0f, 0f, 0f)
        playerKnockback = Vec3(0, 0, 0)
        onGround = false
        fallPeakY = camera.y
        playerHealth = maxPlayerHealth
        playerFood = 20f
        airSupply.reset()
        timeSinceLastDamage = healthRegenDelay
        addChatMessage("You woke up at spawn")
        modManager.events.fireSpawn(ModPlayerEvent(modManager.api, playerName, "respawn"))

  private def updateHealthRegen(dt: Float): Unit =
    if gameMode == GameMode.Survival && playerHealth > 0f && playerHealth < maxPlayerHealth then
      timeSinceLastDamage += dt
      if timeSinceLastDamage >= healthRegenDelay then
        playerHealth = (playerHealth + healthRegenRate * modManager.rules.get("regeneration") * dt)
          .min(maxPlayerHealth)
    else if playerHealth >= maxPlayerHealth then
      playerHealth = maxPlayerHealth
      timeSinceLastDamage = healthRegenDelay

  private def handleLandingImpact(): Unit =
    if waterSubmersion < 0.25f then
      val distance = (fallPeakY - camera.y).max(0f)
      if distance > 0.65f then
        playLandSound(blockUnderPlayer, ((distance - 0.4f) / 3.2f).max(0.30f).min(1.25f))
      if gameMode == GameMode.Survival && distance > 4.2f then
        damagePlayer((distance - 3.2f) * 0.85f * modManager.rules.get("fallDamage"), "falling")

  private def playerTouching(block: Block, extra: Float = 0.05f): Boolean =
    val half = 0.32f + extra
    val minX = floor(camera.x - half).toInt; val maxX = floor(camera.x + half).toInt
    val minY = floor(camera.y - 1.62f).toInt; val maxY = floor(camera.y + 0.20f).toInt
    val minZ = floor(camera.z - half).toInt; val maxZ = floor(camera.z + half).toInt
    (minX to maxX).exists(x =>
      (minY to maxY).exists(y => (minZ to maxZ).exists(z => activeBlockAt(x, y, z) == block))
    )

  private def updateBreathing(dt: Float): Unit =
    val damage = airSupply.update(dt, isUnderwater, gameMode == GameMode.Survival)
    if damage > 0 then damagePlayer(damage, "drowning")

  private def updateEnvironmentalDamage(dt: Float): Unit =
    if cactusDamageCooldown > 0f then cactusDamageCooldown = (cactusDamageCooldown - dt).max(0f)
    if gameMode == GameMode.Survival && cactusDamageCooldown <= 0f && playerTouching(
        Block.Cactus,
        0.08f
      )
    then
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
      else velocity = Vec3(0f, 0f, 0f)

  private def moveVertical(dy: Float): Unit =
    var remaining = dy
    while abs(remaining) > 0.001f do
      val step = remaining.max(-0.22f).min(0.22f)
      movePlayer(0f, step, 0f)
      if step < 0f && onGround then return
      if step > 0f && velocity.y == 0f then return
      remaining -= step

  private def collidesPlayer(pos: Vec3, crouched: Boolean = playerCrouching): Boolean =
    val half = 0.32f
    val minX = floor(pos.x - half).toInt; val maxX = floor(pos.x + half).toInt
    val minY = floor(pos.y - 1.62f).toInt;
    val maxY = floor(pos.y + 0.20f - (if crouched then PlayerStance.heightDrop else 0f)).toInt
    val minZ = floor(pos.z - half).toInt; val maxZ = floor(pos.z + half).toInt
    (minX to maxX).exists(x =>
      (minY to maxY).exists(y => (minZ to maxZ).exists(z => activeBlockAt(x, y, z).solid))
    )

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
      glfwSetWindowMonitor(
        window,
        glfwGetPrimaryMonitor(),
        0,
        0,
        mode.width,
        mode.height,
        mode.refreshRate
      )
    else
      glfwSetWindowMonitor(
        window,
        NULL,
        windowedX.max(0),
        windowedY.max(0),
        windowedW,
        windowedH,
        GLFW_DONT_CARE
      )

  private def down(key: Int): Boolean = window != 0L && glfwGetKey(window, key) == GLFW_PRESS

  private def blockAt(pos: Vec3): Block =
    activeBlockAt(floor(pos.x).toInt, floor(pos.y).toInt, floor(pos.z).toInt)

  private def isUnderwater: Boolean =
    val camera = eyePosition
    val x = floor(camera.x).toInt; val y = floor(camera.y).toInt; val z = floor(camera.z).toInt
    activeBlockAt(x, y, z) == Block.Water && (activeBlockAt(
      x,
      y + 1,
      z
    ) == Block.Water || camera.y < y + WaterSurface.height(waterLevelAt(x, y, z)))

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
      case None        =>
        val key =
          (java.lang.Long.rotateLeft(worldSeed, 21) ^ (x.toLong << 32) ^ (z.toLong & 0xffffffffL))
        val h = terrainHeightCache.getOrElseUpdate(key, terrainGen.heightAt(x, z))
        if y > h then if y <= Terrain.seaLevel then Block.Water else Block.Air
        else if y == h then terrainGen.surfaceBlock(x, y, z, h)
        else terrainGen.fillBlock(x, y, z, h)

  private def setActiveBlock(
      x: Int,
      y: Int,
      z: Int,
      block: Block,
      axis: LogAxis = LogAxis.Y,
      torchMount: TorchMount = TorchMount.Floor,
      refundTo: Option[String] = None
  ): Unit =
    if y < 0 || y >= Terrain.worldHeight then return
    val cx = chunkCoordBlock(x)
    val cz = chunkCoordBlock(z)
    val lx = x - cx * Terrain.chunkSize
    val lz = z - cz * Terrain.chunkSize
    chunks.get((cx, cz)).foreach { chunk =>
      val oldBlock = chunk.getBlock(lx, y, lz)
      chunk.setBlock(lx, y, lz, block, axis, torchMount)
      if gameClient == null && Block.isLog(oldBlock) && !Block.isLog(block) then
        leafDecay.disturb(x, y, z, activeBlockAt)
      if block == Block.Water then
        setWaterLevelAt(x, y, z, 8)
        markWaterActive(x, y, z)
      else
        setWaterLevelAt(x, y, z, 0)
        waterLevels.remove((x, y, z))
      if oldBlock != block then
        if oldBlock == Block.Furnace && block == Block.FurnaceLit then
          val (gain, pan) = furnaceAudioPosition(Vec3(x + 0.5f, y + 0.5f, z + 0.5f))
          playFurnaceIgnition(gain, pan)
        wakeWaterAround(x, y, z)
        val occlusionChanged =
          (oldBlock.solid && !oldBlock.translucent && !oldBlock.cutout) != (block.solid && !block.translucent && !block.cutout)
        if BlockLight.emission(oldBlock) > 0 || BlockLight.emission(
            block
          ) > 0 || (occlusionChanged && hasLightNear(x, y, z))
        then dirtyLightChunksAt(x, z)
        if gameClient == null then
          if oldBlock == Block.Torch && block == Block.Water then
            spawnDrop(Block.Torch, 1, Vec3(x + 0.5f, y + 0.3f, z + 0.5f))
          removeUnsupportedTorchesAround(x, y, z, refundTo)
          val above = activeBlockAt(x, y + 1, z)
          if FloraModel.contains(above) && !FloraModel.supported(above, x, y + 1, z, activeBlockAt)
          then
            setActiveBlock(x, y + 1, z, Block.Air)
            spawnDrop(above, 1, Vec3(x + 0.5f, y + 1.2f, z + 0.5f))
            sendBlockNetwork(x, y + 1, z, Block.Air.id)
    }

  private def torchMountAt(x: Int, y: Int, z: Int): TorchMount =
    loadedChunkForBlock(x, z)
      .map(
        _.getTorchMount(Math.floorMod(x, Terrain.chunkSize), y, Math.floorMod(z, Terrain.chunkSize))
      )
      .getOrElse(TorchMount.fromId(networkTorchMounts.getOrElse((x, y, z), 0)))

  private def hasLightNear(x: Int, y: Int, z: Int): Boolean =
    val cx = chunkCoordBlock(x); val cz = chunkCoordBlock(z)
    (-1 to 1).exists(dx =>
      (-1 to 1).exists(dz =>
        chunks.get((cx + dx, cz + dz)).exists { chunk =>
          chunk.lightSourcesSnapshot.exists { source =>
            val distance =
              abs(chunk.baseX + source.x - x) + abs(source.y - y) + abs(chunk.baseZ + source.z - z)
            distance < source.level
          }
        }
      )
    )

  private def removeUnsupportedTorchesAround(
      x: Int,
      y: Int,
      z: Int,
      refundTo: Option[String]
  ): Unit =
    TorchMount.values.foreach { mount =>
      val tx = x - mount.support._1; val ty = y - mount.support._2; val tz = z - mount.support._3
      if loadedChunkForBlock(tx, tz).nonEmpty && activeBlockAt(
          tx,
          ty,
          tz
        ) == Block.Torch && torchMountAt(tx, ty, tz) == mount && !mount.supported(
          tx,
          ty,
          tz,
          activeBlockAt
        )
      then
        setActiveBlock(tx, ty, tz, Block.Air)
        spawnBlockBreakParticles(tx, ty, tz, Block.Torch)
        spawnDrop(Block.Torch, 1, Vec3(tx + 0.5f, ty + 0.3f, tz + 0.5f))
        sendBlockNetwork(tx, ty, tz, Block.Air.id)
    }

  private def getWorldBlock(x: Int, y: Int, z: Int): Block =
    if y < 0 || y >= Terrain.worldHeight then Block.Air
    else
      val cx = chunkCoordBlock(x)
      val cz = chunkCoordBlock(z)
      chunks.get((cx, cz)) match
        case Some(c) => c.getBlock(x - cx * Terrain.chunkSize, y, z - cz * Terrain.chunkSize)
        case None    => Block.Air

  private def processNetworkMessages(): Unit =
    def handleNetworkLine(line: String): Unit =
      val parts = line.split("\\|", -1)
      try
        if line.startsWith("WORLD|") then
          if parts.length >= 5 && gameServer == null then
            val seed = parts(1).toLong
            val sx = parseNetworkFloat(parts(2)); val sy = parseNetworkFloat(parts(3));
            val sz = parseNetworkFloat(parts(4))
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
              worldName =
                "Multiplayer-" + java.lang.Long.toUnsignedString(worldSeed, 36).toUpperCase
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
            guestSessionKey =
              s"$seed:${if parts.length >= 9 then parts(8) else ""}:${playerName.toLowerCase(java.util.Locale.ROOT)}"
            appliedItemOffers.clear(); pendingTosses.clear(); failedTosses.clear()
            guestCarry.get(guestSessionKey).foreach { state =>
              readWorldExtras(
                new java.io.DataInputStream(new java.io.ByteArrayInputStream(state.bytes))
              )
              fieldJournal = state.journal; appliedItemOffers ++= state.applied;
              pendingTosses ++= state.pending; failedTosses ++= state.failed
            }
            droppedItems.clear()
            camera = Vec3(sx, sy, sz)
            fallPeakY = camera.y
            velocity = Vec3(0f, 0f, 0f); onGround = false
            syncChunks()
            addChatMessage("Joined host world")
        else if line.startsWith("AUDIO|") && gameServer != null then
          if parts.length == 5 then
            val actor = networkSafeName(parts(1))
            remotePlayers.get(actor).foreach { player =>
              val action = BlockboxSounds.Action.fromOrdinal(parts(2).toInt)
              val at = player.targetPos - Vec3(0, 1.25f, 0)
              val support = activeBlockAt(
                floor(at.x).toInt,
                floor(player.targetPos.y - 1.7f).toInt,
                floor(at.z).toInt
              )
              val material = if action == BlockboxSounds.Action.Splash then Block.Water
              else if support != Block.Air then support
              else Block.fromOrdinal(parts(3).toInt)
              broadcastActionSound(actor, action, material, parts(4).toInt, at)
            }
        else if line.startsWith("DIG|") && gameServer != null then
          if parts.length == 6 then
            val actor = networkSafeName(parts(1))
            val target = (parts(2).toInt, parts(3).toInt, parts(4).toInt)
            combatant(actor).foreach { (eye, heading, look, _, _) =>
              BlockboxRaycast
                .raycast(
                  eye,
                  BlockboxRaycast.viewDirection(heading, look),
                  6f,
                  activeBlockAt,
                  torchMountAt
                )
                .filter(_.block == target)
                .foreach { hit =>
                  val block = activeBlockAt(target._1, target._2, target._3)
                  if block != Block.Air && block != Block.Water && block != Block.Bedrock then
                    broadcastActionSound(
                      actor,
                      BlockboxSounds.Action.Dig,
                      block,
                      parts(5).toInt,
                      Vec3(target._1 + 0.5f, target._2 + 0.5f, target._3 + 0.5f)
                    )
                }
            }
        else if line.startsWith("SOUND|") && isClientOnlyMultiplayer then
          if parts.length == 8 then
            val at = Vec3(
              parseNetworkFloat(parts(5)),
              parseNetworkFloat(parts(6)),
              parseNetworkFloat(parts(7))
            )
            hearActionSound(
              networkSafeName(parts(1)),
              BlockboxSounds.Action.fromOrdinal(parts(2).toInt),
              Block.fromOrdinal(parts(3).toInt),
              parts(4).toInt,
              at
            )
        else if line.startsWith("TOSS|") && gameServer != null then
          if parts.length == 5 then
            val actor = networkSafeName(parts(1)); val request = parts(2).toLong
            gameServer.tossResult(actor, request) match
              case Some(done) => gameServer.finishToss(actor, request, done)
              case None       =>
                val block = Block.fromOrdinal(parts(3).toInt); val count = parts(4).toInt
                val accepted = remotePlayers.get(actor).exists { player =>
                  val origin = player.eyePosition - Vec3(0, 0.25f, 0)
                  val direction =
                    BlockboxRaycast.viewDirection(player.targetYaw, player.targetPitch)
                  droppedItems
                    .spawn(block, count, origin, direction * 3.5f + Vec3(0, 1.2f, 0), actor)
                    .nonEmpty
                }
                gameServer.finishToss(actor, request, accepted); itemSyncTimer = 0f
        else if line.startsWith("TOSSACK|") && isClientOnlyMultiplayer then
          if parts.length == 3 then
            val request = parts(1).toLong
            pendingTosses.get(request).foreach { (block, count) =>
              if parts(2) == "1" then pendingTosses.remove(request)
              else if inventoryRoomFor(block) >= count then
                gainItem(block, count); pendingTosses.remove(request)
              else if failedTosses.add(request) then
                addChatMessage("Make inventory space to recover the dropped stack")
            }
        else if line.startsWith("PICK|") && gameServer != null then
          if parts.length == 4 then
            val actor = networkSafeName(parts(1))
            remotePlayers.get(actor).foreach { player =>
              droppedItems
                .reserve(actor, parts(2).toInt, parts(3).toInt, player.targetPos - Vec3(0, 1.0f, 0))
                .foreach(sendItemOffer)
            }
        else if line.startsWith("ITEMACK|") && gameServer != null then
          if parts.length == 4 then
            droppedItems.resolve(networkSafeName(parts(1)), parts(2).toLong, parts(3) == "1");
            itemSyncTimer = 0f
        else if line.startsWith("ITEMOFFER|") && isClientOnlyMultiplayer then
          if parts.length == 4 then
            val id = parts(1).toLong; val block = Block.fromOrdinal(parts(2).toInt);
            val count = parts(3).toInt
            require(id > 0 && DroppedItems.valid(block) && count > 0 && count <= 64)
            val already = appliedItemOffers(id)
            val accept = already || inventoryRoomFor(block) >= count
            if accept && !already then
              gainItem(block, count); appliedItemOffers += id
              while appliedItemOffers.size > 1024 do appliedItemOffers.remove(appliedItemOffers.min)
              playSpatialSound(BlockboxSounds.Action.Ui, block, 0, 0.18f, 0)
            gameClient.send(s"ITEMACK|$id|${if accept then 1 else 0}")
            pickupRequestTimer = 0f
        else if line.startsWith("ITEMBEGIN|") && isClientOnlyMultiplayer then
          if parts.length == 3 then
            itemIncomingSerial = parts(1).toLong; itemIncomingCount = parts(2).toInt
            require(itemIncomingCount >= 0 && itemIncomingCount <= DroppedItems.maxEntities + 64)
            itemIncomingRows.clear()
        else if line.startsWith("ITEMPART|") && isClientOnlyMultiplayer then
          if parts.length == 3 && parts(1).toLong == itemIncomingSerial then
            itemIncomingRows ++= parts(2).split(';').filter(_.nonEmpty)
            require(itemIncomingRows.length <= itemIncomingCount)
        else if line.startsWith("ITEMEND|") && isClientOnlyMultiplayer then
          if parts.length == 2 && parts(1).toLong == itemIncomingSerial then
            require(itemIncomingRows.length == itemIncomingCount)
            droppedItems.receive("ITEMS|" + itemIncomingRows.mkString(";"), interpolate = true)
            itemIncomingSerial = -1; itemIncomingRows.clear()
        else if line.startsWith("RESUME|") && isClientOnlyMultiplayer then
          if parts.length == 4 then
            yaw = parseNetworkFloat(parts(1));
            pitch = parseNetworkFloat(parts(2)).max(-89f).min(89f)
            playerCrouching = parts(3) == "1"; crouchAmount = if playerCrouching then 1f else 0f
            pendingTosses.filterNot((id, _) => failedTosses(id)).foreach {
              case (id, (block, count)) =>
                gameClient.send(s"TOSS|$id|${block.ordinal}|$count")
            }
        else if line.startsWith("ENV|") then
          if gameServer == null then
            val env = BlockboxNet.parseEnvironment(BlockboxNet.packet(line))
            val error = env.time.toDouble - worldClock
            if !environmentReceived || abs(error) > 2.0 then
              worldClock = env.time.toDouble; environmentTimeError = 0.0
            else environmentTimeError = error
            timeOverride = if env.timeOverride < 0f then None else Some(env.timeOverride)
            rainStrength = env.rain; wetness = env.wetness; stormCloudCover = env.cloudCover
            weatherWind = env.wind
            lightningFlash = env.lightning
            environmentReceived = true
        else if line.startsWith("SNOW|") && isClientOnlyMultiplayer then
          if parts.length == 2 then snowCover = parseNetworkFloat(parts(1)).max(0).min(1)
        else if line.startsWith("MOBS") && isClientOnlyMultiplayer then
          val snapshot = ZombieSystem.parseSnapshot(line)
          val alive = snapshot.map(_._1).toSet
          zombies.filterInPlace(z => alive(z.id))
          snapshot.foreach { (id, pos, angle, hp, burn, hurt, gear) =>
            val z = zombies.find(_.id == id).getOrElse {
              val created = ZombieSystem.Zombie(id, pos); created.yaw = angle;
              created.bodyYaw = angle; zombies += created; created
            }
            z.target = pos; z.targetYaw = angle; z.health = hp; z.burning = burn; z.hurt = hurt;
            z.heldItem = gear
          }
        else if line.startsWith("ZHIT|") && gameServer != null then
          if parts.length == 3 then hitZombie(parts(1), parts(2).toInt)
        else if line.startsWith("PHIT|") && gameServer != null then
          if parts.length == 3 then hitPlayer(parts(1), parts(2))
        else if line.startsWith("PDMG|") && isClientOnlyMultiplayer then
          if parts.length == 5 then
            val amount = parts(2).toFloat; val dx = parts(3).toFloat; val dz = parts(4).toFloat
            if amount.isFinite && amount > 0 && amount <= 12 && dx.isFinite && dz.isFinite && dx * dx + dz * dz <= 25
            then applyCombatDamage(amount, networkSafeName(parts(1)), Vec3(dx, 0, dz))
        else if line.startsWith("HURT|") then
          if parts.length == 2 then
            remotePlayers.get(networkSafeName(parts(1))).foreach(_.hurt = 1f)
        else if line.startsWith("ZVOICE|") && isClientOnlyMultiplayer then
          if parts.length == 6 then
            val event = parts(1).toInt; val variant = parts(2).toInt
            val pos = Vec3(parts(3).toFloat, parts(4).toFloat, parts(5).toFloat)
            if event >= 0 && event <= 3 && variant >= 0 && variant < 3 && Seq(pos.x, pos.y, pos.z)
                .forall(_.isFinite)
            then zombieVoice(event, variant, pos)
        else if line.startsWith("ZDAMAGE|") && isClientOnlyMultiplayer then
          if parts.length == 4 then
            val amount = parts(1).toFloat; val dx = parts(2).toFloat; val dz = parts(3).toFloat
            if amount.isFinite && amount >= 0 && amount <= 8 && dx.isFinite && dz.isFinite && dx * dx + dz * dz <= 25
            then applyCombatDamage(amount, "zombie", Vec3(dx, 0, dz))
        else if line == "ZKILL" && isClientOnlyMultiplayer then fieldJournal.defeatedZombie()
        else if line == "CACHEFOUND" && isClientOnlyMultiplayer then fieldJournal.salvagedCache()
        else if line.startsWith("ZLOOT|") && isClientOnlyMultiplayer then
          if parts.length == 3 then
            val item = Block.fromOrdinal(parts(1).toInt); val count = parts(2).toInt
            if (Set(Block.CoalLump, Block.IronIngot, Block.CopperIngot).contains(item) || ToolSystem
                .isTool(item)) && count >= 1 && count <= 2
            then
              gainItem(item, count); fieldJournal.defeatedZombie()
        else if line.startsWith("RULES|") && isClientOnlyMultiplayer then
          modManager.rules.receive(line.stripPrefix("RULES|"))
        else if line.startsWith("VANILLA_USE|") && isClientOnlyMultiplayer then
          if parts.length == 4 then
            val target = (parts(1).toInt, parts(2).toInt, parts(3).toInt)
            if raycast().exists(_.block == target) then placeTargetBlock(skipModInteraction = true)
        else if line.startsWith("INTERACT|") && gameServer != null then
          if parts.length == 5 then
            val x = parts(2).toInt; val y = parts(3).toInt; val z = parts(4).toInt
            val event = modManager.events.fireBlockInteract(
              ModInteractionEvent(modManager.api, parts(1), x, y, z, activeBlockAt(x, y, z))
            )
            if !event.cancelled && !event.handled then
              gameServer.sendTo(parts(1), s"VANILLA_USE|$x|$y|$z")
        else if line.startsWith("FIRE|") && gameServer != null then
          if parts.length == 6 then
            val name = networkSafeName(parts(1));
            val p = (parts(2).toInt, parts(3).toInt, parts(4).toInt)
            val old = getWorldBlock(p._1, p._2, p._3)
            if parts(5) == "1" && (old == Block.Furnace || old == Block.FurnaceLit) then
              val previous = remoteFurnaces.put(name, p)
              previous.filter(_ != p).foreach(refreshFurnaceLight)
              refreshFurnaceLight(p)
            else if parts(5) == "0" && remoteFurnaces.get(name).contains(p) then
              remoteFurnaces.remove(name); refreshFurnaceLight(p)
        else if line.startsWith("SWING|") then
          if parts.length == 2 then
            remotePlayers.get(networkSafeName(parts(1))).foreach(_.swing = 1f)
        else if line.startsWith("EDIT|") && gameServer != null then
          if parts.length >= 7 then
            val sender = networkSafeName(parts(1))
            val edit = BlockboxNet.parseBlock(
              BlockboxNet.packet("BLOC|" + parts.drop(2).mkString("|")),
              Terrain.worldHeight
            )
            val block = Block.fromId(edit.blockId.toByte)
            val old = activeBlockAt(edit.x, edit.y, edit.z)
            val loaded = loadedChunkForBlock(edit.x, edit.z).nonEmpty
            val canBreak = block == Block.Air && old != Block.Air && old != Block.Bedrock
            val mount = TorchMount.fromId(edit.torchMount)
            val canPlace = block != Block.Air && canPlaceBlockAt(
              edit.x,
              edit.y,
              edit.z,
              block
            ) && (block != Block.Torch || mount.supported(edit.x, edit.y, edit.z, activeBlockAt))
            val broken = if loaded && canBreak then
              Some(
                modManager.fireBlockBreak(
                  ModBlockBreakEvent(modManager.api, sender, edit.x, edit.y, edit.z, old)
                )
              )
            else None
            val placed = if loaded && canPlace then
              Some(
                modManager.fireBlockPlace(
                  ModBlockPlaceEvent(modManager.api, sender, edit.x, edit.y, edit.z, block)
                )
              )
            else None
            val finalBlock = placed.map(_.block).getOrElse(block)
            val allowBreak = broken.exists(!_.cancelled)
            val allowPlace = placed.exists(e =>
              !e.cancelled && e.block != null && CustomBlocks.registered(
                e.block
              ) && canPlaceBlockAt(
                edit.x,
                edit.y,
                edit.z,
                e.block
              ) && (e.block != Block.Torch || mount
                .supported(edit.x, edit.y, edit.z, activeBlockAt))
            )
            val proposedLoot = modManager.drops.getOrElse(old, ToolSystem.drop(old))
            val additionalLoot = if old == Block.SupplyCrate then
              SupplyCaches.contents(worldSeed, edit.x, edit.y, edit.z)
            else Vector.empty
            val hasDropRoom =
              !allowBreak || !broken.exists(_.dropItem) || proposedLoot._1 == Block.Air ||
                droppedItems.canDropAll(
                  Vector(proposedLoot) ++ additionalLoot,
                  Vec3(edit.x + 0.5f, edit.y + 0.5f, edit.z + 0.5f)
                )
            val accepted = loaded && (allowBreak || allowPlace) && hasDropRoom
            if accepted then
              setActiveBlock(
                edit.x,
                edit.y,
                edit.z,
                finalBlock,
                LogAxis.fromId(edit.logAxis),
                mount,
                Some(sender)
              )
              if finalBlock == Block.Water then
                setWaterLevelAt(edit.x, edit.y, edit.z, edit.waterLevel.max(1))
              dirtyChunkAt(edit.x, edit.z)
              gameServer.broadcastBlockChange(
                edit.x,
                edit.y,
                edit.z,
                finalBlock.id,
                if finalBlock == Block.Water then edit.waterLevel else 0,
                edit.logAxis,
                if finalBlock == Block.Torch then mount.id else 0
              )
            else
              val axis = loadedChunkForBlock(edit.x, edit.z)
                .map(c =>
                  c.getLogAxis(Math.floorMod(edit.x, 16), edit.y, Math.floorMod(edit.z, 16)).id
                )
                .getOrElse(0)
              gameServer.sendTo(
                sender,
                BlockboxNet.block(
                  edit.x,
                  edit.y,
                  edit.z,
                  old.ordinal,
                  if old == Block.Water then waterLevelAt(edit.x, edit.y, edit.z) else 0,
                  axis,
                  torchMountAt(edit.x, edit.y, edit.z).id
                )
              )
            if accepted then
              broadcastActionSound(
                sender,
                if edit.blockId == 0 then BlockboxSounds.Action.Break
                else BlockboxSounds.Action.Place,
                if edit.blockId == 0 then old else Block.fromOrdinal(edit.blockId),
                Math.floorMod(edit.x ^ edit.y ^ edit.z, 64),
                Vec3(edit.x + 0.5f, edit.y + 0.6f, edit.z + 0.5f)
              )
            val (loot, count) =
              if accepted && broken.exists(_.dropItem) && ToolSystem
                  .canHarvest(old, remotePlayers.get(sender).map(_.heldBlock).getOrElse(Block.Air))
              then modManager.drops.getOrElse(old, ToolSystem.drop(old))
              else (Block.Air, 0)
            if loot != Block.Air && count > 0 then
              spawnDrop(loot, count, Vec3(edit.x + 0.5f, edit.y + 0.5f, edit.z + 0.5f))
              if old == Block.SupplyCrate then salvageCache(sender, edit.x, edit.y, edit.z)
            gameServer.sendTo(
              sender,
              s"EDIT_RESULT|${edit.x}|${edit.y}|${edit.z}|${if accepted then 1 else 0}|${loot.ordinal}|$count"
            )
        else if line.startsWith("EDIT_RESULT|") && isClientOnlyMultiplayer then
          if parts.length == 7 then
            val key = (parts(1).toInt, parts(2).toInt, parts(3).toInt)
            val lootId = parts(5).toInt; val count = parts(6).toInt
            require(
              lootId >= 0 && lootId < Block.values.length && count >= 0 && count <= 64,
              "Invalid edit result"
            )
            pendingPlayerEdits.remove(key).foreach { case (spent, amount, drops) =>
              if parts(4) == "0" && amount > 0 then gainItem(spent, amount)
            // The host awards accepted breaks through physical item entities.
            }
        else if line.startsWith("BLOC|") then
          if parts.length >= 5 then
            val packet = BlockboxNet.parseBlock(BlockboxNet.packet(line), Terrain.worldHeight)
            if packet.blockId >= Block.values.length then
              throw IllegalArgumentException("Unknown block id")
            val x = packet.x; val y = packet.y; val z = packet.z
            val blockId = packet.blockId.toByte
            val block = Block.fromId(blockId)
            val waterLevel =
              if block == Block.Water && parts.length >= 6 then
                try parts(5).toInt.max(1).min(8)
                catch case _: Exception => 8
              else if block == Block.Water then 8
              else 0
            val logAxis =
              if Block.isLog(block) && parts.length >= 7 then
                try parts(6).toInt.max(0).min(2)
                catch case _: Exception => 0
              else 0
            networkBlockOverrides((x, y, z)) = block
            if block == Block.Water then networkWaterLevelOverrides((x, y, z)) = waterLevel.toByte
            else networkWaterLevelOverrides.remove((x, y, z))
            if Block.isLog(block) && logAxis != 0 then networkLogAxesOverrides((x, y, z)) = logAxis
            else networkLogAxesOverrides.remove((x, y, z))
            val mount = if block == Block.Torch then packet.torchMount else 0
            if block == Block.Torch then networkTorchMounts((x, y, z)) = mount
            else networkTorchMounts.remove((x, y, z))
            val cx = chunkCoordBlock(x); val cz = chunkCoordBlock(z)
            if chunks.contains((cx, cz)) then
              setActiveBlock(x, y, z, block, LogAxis.fromId(logAxis), TorchMount.fromId(mount))
              if block == Block.Water then setWaterLevelAt(x, y, z, waterLevel)
              dirtyChunkAt(x, z)
            else
              pendingNetworkBlocks((x, y, z)) = block
              if Block.isLog(block) && logAxis != 0 then pendingNetworkLogAxes((x, y, z)) = logAxis
        else if line.startsWith("SNAPBEGIN|") || line.startsWith("SNAPEND") then ()
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
            val mode = GameMode.fromOrdinal(parts(2).toInt)
            remotePlayers.get(name).foreach(_.mode = mode)
            if name.equalsIgnoreCase(playerName) then
              gameMode = mode
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
              val label = if parts.length >= 5 then networkUnescape(parts.drop(4).mkString("|"))
              else "Updated"
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
              val x = parseNetworkFloat(parts(2)); val y = parseNetworkFloat(parts(3));
              val z = parseNetworkFloat(parts(4))
              val pyaw = parseNetworkFloat(parts(5)); val ppitch = parseNetworkFloat(parts(6))
              val colorId =
                if parts.length >= 8 then normalizeColorId(parts(7).toInt) else colorForPlayer(name)
              val held = if parts.length >= 9 then Block.fromId((parts(8).toInt & 0xff).toByte)
              else Block.Air
              rememberPlayerColor(name, colorId)
              val nextPos = Vec3(x, y, z)
              remotePlayers.get(name) match
                case Some(rp) =>
                  // Interpolate ordinary packets, but snap genuine teleports/large
                  // corrections so smoothing can never leave a player ghosting across
                  // half the map. The held block is applied as soon as the packet arrives.
                  if (nextPos - rp.pos).lengthSquared > 144f then rp.pos = nextPos
                  rp.targetPos = nextPos
                  rp.targetYaw = pyaw
                  rp.targetPitch = ppitch
                  rp.lastSeen = glfwGetTime()
                  rp.colorId = colorId
                  rp.heldBlock = held
                case None =>
                  remotePlayers(name) = RemotePlayer(
                    name,
                    nextPos,
                    nextPos,
                    pyaw,
                    pyaw,
                    ppitch,
                    ppitch,
                    glfwGetTime(),
                    colorId,
                    held
                  )
              remotePlayers.get(name).foreach(_.crouching = parts.length >= 10 && parts(9) == "1")
        else if line.startsWith("TPOS|") then
          if parts.length >= 5 then
            val name = networkSafeName(parts(1))
            val x = parseNetworkFloat(parts(2)); val y = parseNetworkFloat(parts(3));
            val z = parseNetworkFloat(parts(4))
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
            val colorId =
              if parts.length >= 3 then normalizeColorId(parts(2).toInt) else colorForPlayer(name)
            if name.nonEmpty then knownPlayerNames += name
            rememberPlayerColor(name, colorId)
            if name != playerName then addChatMessage(s"$name joined the game")
            if gameServer != null then
              droppedItems.forPlayer(name).foreach(sendItemOffer)
              itemSyncTimer = 0f
              gameServer.broadcast(s"GMODE|$playerName|${gameMode.ordinal}")
              remotePlayers.valuesIterator.foreach(p =>
                gameServer.broadcast(s"GMODE|${p.name}|${p.mode.ordinal}")
              )
        else if line.startsWith("LEFT|") then
          if parts.length >= 2 then
            val name = networkSafeName(parts(1))
            knownPlayerNames -= name
            remotePlayers.remove(name)
            remoteFurnaces.remove(name).foreach(refreshFurnaceLight)
            if gameServer != null && isOppedName(name) then
              setOppedName(name, false, announce = false)
              gameServer.broadcast("OP|" + name + "|0")
            if name != playerName then addChatMessage(s"$name left the game")
      catch
        case e: Exception =>
          System.err.println(
            s"Ignored bad network packet '$line': ${Option(e.getMessage).getOrElse(e.getClass.getSimpleName)}"
          )

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
      rp.hurt = (rp.hurt - dt * 2f).max(0f)
      rp.crouch = PlayerStance.approach(rp.crouch, rp.crouching, dt)
    }

  private def sendPlayerPositionNetwork(): Unit =
    if multiplayerMode then
      val now = glfwGetTime()
      val moved = (camera - lastNetworkSentPos).lengthSquared > 0.0004f
      val turned = abs(angleDeltaDegrees(lastNetworkSentYaw, yaw)) > 0.22f || abs(
        lastNetworkSentPitch - pitch
      ) > 0.22f
      val keepaliveDue = now - lastPosKeepalive >= 0.75
      val heldId = heldHotbarBlock.id & 0xff
      val heldChanged = heldId != lastNetworkHeldBlock || lastNetworkCrouching != playerCrouching
      // Moving players update at up to 20 Hz. A hotbar change is sent on the same
      // frame, so the other client shows the new block with no extra wait.
      if heldChanged || (now - lastPosSend >= 0.05 && (moved || turned || keepaliveDue)) then
        lastPosSend = now
        if keepaliveDue then lastPosKeepalive = now
        lastNetworkSentPos = camera
        lastNetworkSentYaw = yaw
        lastNetworkSentPitch = pitch
        lastNetworkHeldBlock = heldId
        lastNetworkCrouching = playerCrouching
        val msg =
          "POS|" + networkEscape(playerName) + "|" + networkFloat(camera.x) + "|" + networkFloat(
            camera.y
          ) + "|" + networkFloat(camera.z) + "|" + networkFloat(yaw) + "|" + networkFloat(
            pitch
          ) + "|" + localColorId.toString + "|" + heldId.toString + "|" + (if playerCrouching then
                                                                             "1"
                                                                           else "0")
        if gameClient != null && gameClient.isConnected then gameClient.send(msg)
        else if gameServer != null then gameServer.broadcast(msg)
      if gameServer != null && now - lastMultiplayerHeartbeat >= 2.0 then
        lastMultiplayerHeartbeat = now
        gameServer.broadcastPlayerList()
      if gameServer != null && now - lastEnvironmentSend >= 0.5 then
        lastEnvironmentSend = now
        publishedEnvironment = environmentMessage
        gameServer.broadcast(publishedEnvironment)
        gameServer.broadcast(s"SNOW|$snowCover")

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
    renderFrame(framebufferWidth, framebufferHeight)

  /** The normal gameplay frame, also used by deterministic GPU reviews. Window size queries stay
    * outside so reviews exercise the real frame without GLFW.
    */
  private[blockbox] def renderFrame(width: Int, height: Int): Unit =
    require(width > 0 && height > 0, "Invalid framebuffer size")
    framebufferWidth = width; framebufferHeight = height
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
    val worldScreen =
      screen == Screen.Playing || screen == Screen.Paused || screen == Screen.Inventory || screen == Screen.Catalog || screen == Screen.FurnaceUI || screen == Screen.Journal
    if shadersEnabled && shaderPipeline != null then shaderPipeline.reloadIfChanged(glfwGetTime())
    if foveaPass != null then foveaPass.enabled = foveatedRendering
    val shaderScene = worldScreen && shadersEnabled && shaderPipeline != null && shaderPipeline
      .prepareFrame(framebufferWidth, framebufferHeight)
    if shaderScene && shaderPipeline != null then
      shaderPipeline.clearPreparedFrame(clearR, clearG, clearB)
    else glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT)
    def finishWorldPost(): Unit =
      if shaderScene && shaderPipeline != null then
        shaderPipeline.finishFrame(framebufferWidth, framebufferHeight, gameTime)
      if screen != Screen.Playing && isUnderwater then renderUnderwaterOverlay()
    screen match
      case Screen.Playing =>
        renderWorld()
        finishWorldPost()
        renderPlayerNameTags()
        renderFirstPersonHeldBlock()
        if isUnderwater then renderUnderwaterOverlay()
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
          renderTextShadow(
            18f * uiScale,
            framebufferHeight - 32f * uiScale,
            "F8: close mod cursor",
            0.82f,
            0.92f,
            1f,
            (0.78f * uiScale).max(0.68f)
          )
        if debugMode then renderDebugOverlay()
      case Screen.MainMenu    => renderMainMenu()
      case Screen.CreateWorld => renderCreateWorld()
      case Screen.LoadWorld   => renderLoadWorldMenu()
      case Screen.Mods        => renderModsScreen()
      case Screen.Resources   => renderResourcesScreen()
      case Screen.Settings    => renderSettings()
      case Screen.Paused      =>
        renderWorld()
        finishWorldPost()
        renderPlayerNameTags()
        if multiplayerMode then
          renderHud()
          renderChat()
        renderPauseMenu()
      case Screen.Inventory => renderWorld(); finishWorldPost(); renderSurvivalInventory()
      case Screen.Catalog   => renderWorld(); finishWorldPost(); renderCatalog()
      case Screen.FurnaceUI => renderWorld(); finishWorldPost(); renderFurnaceUI()
      case Screen.Journal   => renderWorld(); finishWorldPost(); renderJournal()
      case Screen.JoinGame  => renderJoinGame()
      case Screen.HostGame  => renderHostGame()

  private def renderWorld(): Unit =
    val camera = eyePosition
    if !centerChunkReady then
      forceCameraChunkRing(0)
      processChunkWorkMainThread(0, 8)
    // Both render paths share the same optional resolution scale and resolve.
    val useFovea =
      foveatedRendering && foveaPass != null && foveaPass.supportsFbos && !(shadersEnabled && shaderPipeline != null && shaderPipeline.isSceneBound) && foveaPass
        .beginScene(framebufferWidth, framebufferHeight)
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
    val fog = WorldFog.range(visibleDist, fogDensity, localRain, isUnder)
    val fogStart = fog.start; val fogEnd = fog.end
    applyTerrainFog(isUnder)
    setFogColor(fogR, fogG, fogB)
    glMatrixMode(GL_PROJECTION); glLoadIdentity()
    val aspect = framebufferWidth.toDouble / framebufferHeight.toDouble
    // The old 25cm near plane crossed nearby walls/ceilings when looking around.
    // Fit its corners within the eye's collision clearance, including ultrawide.
    val farBlocks = if cloudsEnabled then renderDistanceBlocks.max(cloudRenderDistanceBlocks)
    else renderDistanceBlocks
    val dynamicFov = viewFovOffset
    val near = CameraProjection.nearPlane(fov + dynamicFov, aspect)
    // Render distance is horizontal. Flight above the world must not push the
    // terrain past the far plane, including when clouds are disabled.
    val verticalRange = abs(camera.y).max(abs(camera.y - Terrain.worldHeight))
    val far = sqrt(
      farBlocks.toDouble * farBlocks + verticalRange.toDouble * verticalRange
    ) + Terrain.chunkSize * 2
    val top = tan(toRadians((fov + dynamicFov).max(45f).min(130f)) / 2.0) * near
    glFrustum(-top * aspect, top * aspect, -top, top, near, far)
    glMatrixMode(GL_MODELVIEW); glLoadIdentity()
    // Walking sway belongs to the viewmodel. Keep this camera identical for
    // terrain, atmosphere, clouds, reflections and ray-history reprojection.
    glRotatef(pitch, 1f, 0f, 0f)
    glRotatef(yaw, 0f, 1f, 0f)
    // X/Z camera-relative rendering: chunk VBOs are local and translated by
    // (chunkBase - camera). Only vertical camera motion remains in the view matrix.
    glTranslatef(0f, -camera.y, 0f)
    renderSky()
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
    reflectionChunkScratch.clear()
    val shadowCasterRadius = if shadersEnabled && shaderPipeline != null then
      shaderPipeline.shadowDistanceLimit(visibleDist + Terrain.chunkSize * 1.5f)
    else 0f
    val shadowCasterRadiusSq = shadowCasterRadius * shadowCasterRadius
    val shadowIter = chunks.valuesIterator
    while shadowIter.hasNext do
      val c = shadowIter.next()
      val dx = (c.cx * Terrain.chunkSize + Terrain.chunkSize * 0.5f) - camera.x
      val dz = (c.cz * Terrain.chunkSize + Terrain.chunkSize * 0.5f) - camera.z
      if dx * dx + dz * dz <= shadowCasterRadiusSq then shadowChunkScratch += c
      if dx * dx + dz * dz <= (visibleDist + Terrain.chunkSize) * (visibleDist + Terrain.chunkSize)
      then reflectionChunkScratch += c
    // Batch opaque pass - bind texture + client states once for all chunks
    glDisable(GL_BLEND)
    glDepthMask(true)
    glEnable(GL_DEPTH_TEST)
    glEnable(GL_TEXTURE_2D); activeAtlas.bind()
    glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_MODULATE)
    glEnableClientState(GL_VERTEX_ARRAY); glEnableClientState(GL_COLOR_ARRAY);
    glEnableClientState(GL_NORMAL_ARRAY); glEnableClientState(GL_TEXTURE_COORD_ARRAY)
    // Optional Iris-style directional shadow map. It reuses the already-built chunk
    // VBOs and atlas, then restores the scene framebuffer/matrices before the normal
    // G-buffer draw. This keeps shadows GPU-side without rebuilding terrain geometry.
    val sunDirection = BlockboxSky.sunDirection(dayPhase)
    val moonDirection = BlockboxSky.moonDirection(dayPhase)
    updateFrameLight()
    if shaderPipeline != null then
      shaderPipeline.setNightShading(worldDarkening)
      shaderPipeline.setLightEnvironment(frameLight)
      shaderPipeline.setWeatherCells(weatherCells, camera.x, camera.z)
      shaderPipeline.setWaterSurface(camera.y + waterDepthAt(camera))
    val shadowActive =
      if shadersEnabled && shaderPipeline != null then
        shaderPipeline.beginShadow(
          (camera.x, camera.y, camera.z),
          sunDirection,
          moonDirection,
          daylightFactor,
          shadowCasterRadius,
          gameTime,
          localRain,
          localWetness
        )
      else false
    if shadowActive then
      shaderPipeline.setShadowPass(0)
      shadowChunkScratch.foreach(_.drawOpaque(camera))
      glEnable(GL_ALPHA_TEST); glAlphaFunc(GL_GREATER, 0.5f)
      shaderPipeline.setShadowPass(1)
      shadowChunkScratch.foreach(_.drawCutout(camera))
      renderEntityShadows()
      glDisable(GL_ALPHA_TEST)
      shaderPipeline.endShadow()
      // endShadow restores texture unit 0 and the scene target; make the intended
      // world state explicit because third-party shadow shaders are allowed to alter it.
      glEnable(GL_TEXTURE_2D); activeAtlas.bind()
      glEnable(GL_DEPTH_TEST); glDepthMask(true); glDisable(GL_BLEND)
      glEnableClientState(GL_VERTEX_ARRAY); glEnableClientState(GL_COLOR_ARRAY)
      glEnableClientState(GL_NORMAL_ARRAY); glEnableClientState(GL_TEXTURE_COORD_ARRAY)
    if shaderPipeline != null then
      shaderPipeline.setCelestialTime(timeOverride.getOrElse(gameTime))
      shaderPipeline.setAntialiasing(antiAliasing)
      shaderPipeline.setNightShading(worldDarkening)
    val shaderActive =
      if shadersEnabled && shaderPipeline != null then
        shaderPipeline.beginWorld(
          fogR,
          fogG,
          fogB,
          fogStart,
          fogEnd,
          daylightFactor,
          gameTime,
          sunDirection,
          moonDirection,
          (camera.x, camera.y, camera.z),
          near.toFloat,
          far.toFloat,
          localRain,
          localWetness,
          isUnderwater
        )
      else false
    if shaderActive then
      glDisable(GL_FOG)
      if !isUnder && shaderPipeline.needsWaterReflection then
        val planes = scala.collection.mutable.HashMap.empty[Float, Float]
        chunkList.foreach { c =>
          c.flatWaterPlane.foreach { (height, count) =>
            if height < camera.y then
              val dx = c.cx * Terrain.chunkSize + Terrain.chunkSize * 0.5f - camera.x
              val dz = c.cz * Terrain.chunkSize + Terrain.chunkSize * 0.5f - camera.z
              planes.update(
                height,
                planes.getOrElse(height, 0f) + count / (1f + (dx * dx + dz * dz) / 1024f)
              )
          }
        }
        planes.maxByOption(_._2).foreach { (height, _) =>
          shaderPipeline.setWaterSurface(height)
          shaderPipeline.captureWaterReflection(height) {
            shaderPipeline.setPass(0)
            reflectionChunkScratch.foreach(_.drawOpaque(camera))
            glEnable(GL_ALPHA_TEST); glAlphaFunc(GL_GREATER, 0.5f)
            shaderPipeline.setPass(1)
            reflectionChunkScratch.foreach(_.drawCutout(camera))
            glDisable(GL_ALPHA_TEST)
            shaderPipeline.end()
            glClientActiveTexture(GL_TEXTURE1); glDisableClientState(GL_TEXTURE_COORD_ARRAY);
            glClientActiveTexture(GL_TEXTURE0)
            renderRemotePlayers3D()
            renderZombies3D()
            renderDroppedItems()
          }
          resetGlArraysAndBuffers()
          glEnable(GL_TEXTURE_2D); activeAtlas.bind()
          glEnable(GL_DEPTH_TEST); glDepthMask(true); glDisable(GL_BLEND); glDisable(GL_FOG)
          glEnableClientState(GL_VERTEX_ARRAY); glEnableClientState(GL_COLOR_ARRAY)
          glEnableClientState(GL_NORMAL_ARRAY); glEnableClientState(GL_TEXTURE_COORD_ARRAY)
        }
      shaderPipeline.enableGBufferWrites()
      shaderPipeline.setPass(0)
    else if shaderPipeline != null then
      shaderPipeline.beginCompatibilityLighting(
        daylightFactor,
        if isUnder then camera.y else Float.NaN,
        if isUnder then camera.y + waterDepthAt(camera) else Float.NaN
      )
    chunkList.foreach(_.drawOpaque(camera))
    // Batch cutout pass
    glEnable(GL_ALPHA_TEST); glAlphaFunc(GL_GREATER, 0.5f)
    if shaderActive then shaderPipeline.setPass(1)
    chunkList.foreach(_.drawCutout(camera))
    glDisable(GL_ALPHA_TEST)
    if shaderActive then
      shaderPipeline.end(); shaderPipeline.enableColorOnlyWrites()
    else if shaderPipeline != null then shaderPipeline.endCompatibilityLighting()
    glClientActiveTexture(GL_TEXTURE1); glDisableClientState(GL_TEXTURE_COORD_ARRAY);
    glClientActiveTexture(GL_TEXTURE0)
    renderRemotePlayers3D()
    renderZombies3D()
    renderDroppedItems()
    restoreChunkRenderState(isUnder, fogR, fogG, fogB)
    glEnable(GL_TEXTURE_2D); activeAtlas.bind()
    glEnableClientState(GL_VERTEX_ARRAY); glEnableClientState(GL_COLOR_ARRAY);
    glEnableClientState(GL_NORMAL_ARRAY); glEnableClientState(GL_TEXTURE_COORD_ARRAY)
    if shaderActive then
      shaderPipeline.beginWorld(
        fogR,
        fogG,
        fogB,
        fogStart,
        fogEnd,
        daylightFactor,
        gameTime,
        sunDirection,
        moonDirection,
        (camera.x, camera.y, camera.z),
        near.toFloat,
        far.toFloat,
        localRain,
        localWetness,
        isUnder
      )
      glDisable(GL_FOG)
    else if shaderPipeline != null then
      shaderPipeline.beginCompatibilityLighting(
        daylightFactor,
        if isUnder then camera.y else Float.NaN,
        if isUnder then camera.y + waterDepthAt(camera) else Float.NaN
      )
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
      shaderPipeline.beginWaterSurface()
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
      shaderPipeline.endWaterSurface()
      shaderPipeline.end()
      shaderPipeline.enableColorOnlyWrites()
    else if shaderPipeline != null then shaderPipeline.endCompatibilityLighting()
    glEnable(GL_FOG)
    glDepthMask(true)
    glDisable(GL_BLEND)
    // Chunk VBOs expose a second client texture-coordinate stream for Minecraft-style
    // lightmap UVs; disable both units before returning to compatibility/immediate mode.
    glClientActiveTexture(GL_TEXTURE1); glDisableClientState(GL_TEXTURE_COORD_ARRAY)
    glClientActiveTexture(GL_TEXTURE0); glDisableClientState(GL_TEXTURE_COORD_ARRAY)
    glDisableClientState(GL_NORMAL_ARRAY); glDisableClientState(GL_COLOR_ARRAY);
    glDisableClientState(GL_VERTEX_ARRAY)
    glBindBuffer(GL_ARRAY_BUFFER, 0); glBindTexture(GL_TEXTURE_2D, 0)
    if shaderActive && shaderPipeline != null && shaderPipeline.rayTracingEnabled then
      shaderPipeline.updateRayGeometry(chunks.values)
    val packClouds =
      shadersEnabled && shaderPipeline != null && shaderPipeline.rendersVolumetricClouds
    if cloudsEnabled && !isUnder && !packClouds then
      BlockboxSky.renderClouds(
        camera,
        gameTime,
        cloudRenderDistanceBlocks,
        dayPhase,
        daylightFactor,
        stormCloudCover,
        localLightning,
        worldSeed
      )
    BlockboxWeather.render(camera, weatherStrikes)
    applyTerrainFog(isUnder)
    renderWeatherRain()
    renderBlockParticles()
    renderTorchEffects()
    if modManager.events.hasListeners("worldRender") then
      val oldProgram = glGetInteger(org.lwjgl.opengl.GL20.GL_CURRENT_PROGRAM)
      glPushAttrib(GL_ALL_ATTRIB_BITS); glPushClientAttrib(GL_CLIENT_ALL_ATTRIB_BITS)
      glMatrixMode(GL_PROJECTION); glPushMatrix()
      glMatrixMode(GL_MODELVIEW); glPushMatrix()
      try
        modManager.events.fireWorldRender(
          ModWorldRenderEvent(modManager.api, modManager.render, lastFrameTime.toFloat)
        )
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
    viewFrustum.intersects(
      minX - 1f,
      -1f,
      minZ - 1f,
      maxX + 1f,
      Terrain.worldHeight + 1f,
      maxZ + 1f
    )

  private def renderWorldDarkness(): Unit =
    // Terrain shades night per surface using block light. A fullscreen veil would
    // incorrectly dim flames and the surfaces those flames have illuminated.
    if !worldDarkening || shaderPipeline != null then return
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

  private def restoreChunkRenderState(
      isUnder: Boolean,
      fogR: Float,
      fogG: Float,
      fogB: Float
  ): Unit =
    // renderSky(), particles, name tags, and UI all mutate global OpenGL state.
    // The world chunk pass must explicitly restore depth writes/testing or random translucent
    // quads / water / player boxes can draw as big black sheets and terrain can appear to vanish.
    resetGlArraysAndBuffers()
    glPolygonMode(GL_FRONT_AND_BACK, if wireframeMode then GL_LINE else GL_FILL)
    if shadersEnabled && shaderPipeline != null && shaderPipeline.isSceneBound then
      shaderPipeline.restoreSceneViewport()
    else if foveatedRendering && foveaPass != null && foveaPass.targetWidth > 0 then
      glViewport(0, 0, foveaPass.targetWidth, foveaPass.targetHeight)
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
    val shader = shadersEnabled && shaderPipeline != null && shaderPipeline.available
    // Compatibility fog uses eye-space depth. Compensate for the downward
    // vertical view component so flight cannot fog the entire ground to sky color.
    val lift = if underwater || shader then 0f
    else (camera.y - Terrain.seaLevel).max(0f) * sin(toRadians(pitch)).toFloat.max(0f)
    val fog = WorldFog.range(distance, fogDensity, localRain, underwater)
    glFogi(GL_FOG_MODE, GL_LINEAR)
    glFogf(GL_FOG_START, fog.start + lift)
    glFogf(GL_FOG_END, fog.end + lift)

  private def skyOpenAt(x: Int, y: Int, z: Int): Boolean =
    var h = y.max(0)
    while h < Terrain.worldHeight do
      if activeBlockAt(x, h, z).solid then return false
      h += 1
    true

  private def waterDepthAt(pos: Vec3): Float =
    val x = floor(pos.x).toInt; val z = floor(pos.z).toInt
    var y = floor(pos.y).toInt
    if activeBlockAt(x, y, z) != Block.Water then 0f
    else
      while y < Terrain.worldHeight - 1 && activeBlockAt(x, y + 1, z) == Block.Water do y += 1
      (y + WaterSurface.height(waterLevelAt(x, y, z)) - pos.y).max(0f)

  private def renderImmersedEntity(eye: Vec3, sceneShaded: Boolean = false)(
      draw: ((Float, Float, Float)) => Unit
  ): Unit =
    val light = if sceneShaded then (1f, 1f, 1f) else lightColorAt(eye.x, eye.y - 0.6f, eye.z)
    // The mirrored capture already clips every vertex at its water plane.
    if shaderPipeline != null && shaderPipeline.isReflectionBound then
      draw(light)
      return
    val feet = eye.y - 1.62f
    val x = floor(eye.x).toInt; val z = floor(eye.z).toInt
    val water = (floor(feet).toInt to ceil(eye.y + 0.2f).toInt).filter(y =>
      activeBlockAt(x, y, z) == Block.Water
    )
    if water.isEmpty then draw(light)
    else
      val top = water.max + WaterSurface.height(waterLevelAt(x, water.max, z))
      val depth = waterDepthAt(Vec3(eye.x, feet + 0.1f, eye.z)).max(1.2f)
      val tinted = WaterAppearance.tint(light, depth)
      def clip(sign: Double): Unit =
        entityWaterPlane.clear(); entityWaterPlane.put(0).put(sign).put(0).put(-top * sign).flip()
        glClipPlane(GL_CLIP_PLANE0, entityWaterPlane)
      glEnable(GL_CLIP_PLANE0)
      clip(1); draw(light)
      clip(-1); draw(tinted)
      glDisable(GL_CLIP_PLANE0)

  private def updateZombies(dt: Float): Unit =
    combatCooldown = (combatCooldown - dt).max(0f)
    if isClientOnlyMultiplayer then
      zombies.foreach { z =>
        val before = z.pos
        z.hurt = (z.hurt - dt * 2f).max(0f)
        z.pos += (z.target - z.pos) * (1f - exp(-dt * 14f).toFloat)
        z.yaw += (((z.targetYaw - z.yaw + 540f) % 360f) - 180f) * (1f - exp(-dt * 12f).toFloat)
        val distance = sqrt(pow(z.pos.x - before.x, 2) + pow(z.pos.z - before.z, 2)).toFloat
        z.bodyYaw += (((z.yaw - z.bodyYaw + 540f) % 360f) - 180f) * (1f - exp(-dt * 10f).toFloat)
        ZombieSystem.updateSwimming(z, dt, activeBlockAt, waterLevelAt)
        z.gait += distance * (if z.swimming > 0.5f then 3.2f else 3.6f)
        z.stride = approachExp(z.stride, (distance / dt.max(0.001f) / 1.65f).min(1f), 10f, dt)
      }
      return
    val targets = Vector(ZombieSystem.Target(playerName, camera, gameMode == GameMode.Survival)) ++
      remotePlayers.valuesIterator
        .map(p => ZombieSystem.Target(p.name, p.targetPos, p.mode == GameMode.Survival))
        .toVector
    zombieSpawnTimer -= dt
    if zombieSpawnTimer <= 0f && zombies.length < (targets.length * 8).min(32) then
      zombieSpawnTimer = 2.5f
      val cap = (targets.length * 8).min(32)
      val around = targets(zombieRng.nextInt(targets.length)).eye
      var tries = 0; var spawned = false
      while tries < 18 && !spawned do
        val angle = zombieRng.nextFloat() * Pi.toFloat * 2f
        // Sample area, rather than radius: distant chunks get their fair share
        // and mobs do not form an obvious dense ring just outside the player.
        val inner = ZombieSystem.spawnMinDistance + 2f
        val outer = ZombieSystem.spawnMaxDistance - 4f
        val radius = sqrt(
          inner * inner + zombieRng.nextFloat() * (outer * outer - inner * inner)
        ).toFloat
        val x = floor(around.x + sin(angle).toFloat * radius).toInt
        val z = floor(around.z + cos(angle).toFloat * radius).toInt
        if loadedChunkForBlock(x, z).nonEmpty then
          val surface = terrainGen.heightAt(x, z)
          val top = if tries % 3 == 0 then (around.y - 1.6f).toInt + zombieRng.nextInt(13) - 6
          else surface + 1
          var y = top.min(Terrain.worldHeight - 3).max(5)
          val bottom = (y - 10).max(5)
          while y >= bottom && !spawned do
            val feet = Vec3(x + 0.5f, y.toFloat, z + 0.5f)
            val (sky, block) = lightLevelsAt(x, y + 1, z)
            val exposed = skyOpenAt(x, y + 1, z)
            val effectiveSky = if exposed then 15 else sky
            if ZombieSystem.inSpawnRange(feet, targets) &&
              ZombieSystem.canSpawn(feet, daylightFactor, effectiveSky, block, activeBlockAt)
            then
              def addMob(where: Vec3): Unit =
                val mob = ZombieSystem.Zombie(zombieSerial, where)
                mob.yaw = zombieRng.nextFloat() * 360f; mob.bodyYaw = mob.yaw;
                mob.targetYaw = mob.yaw
                mob.heldItem = ZombieSystem.spawnEquipment(zombieRng); zombieSerial += 1
                zombies += mob
              addMob(feet); spawned = true
              // Small local packs share a habitat; every companion independently
              // checks support, lighting, player exclusion and loaded terrain.
              val companions = zombieRng.nextInt(3)
              for _ <- 0 until companions if zombies.length < cap do
                val px = x + zombieRng.nextInt(7) - 3; val pz = z + zombieRng.nextInt(7) - 3
                if loadedChunkForBlock(px, pz).nonEmpty then
                  (-1 to 1).iterator
                    .map(dy => Vec3(px + 0.5f, y + dy, pz + 0.5f))
                    .find { p =>
                      val py = p.y.toInt
                      val (psky, plight) = lightLevelsAt(px, py + 1, pz)
                      ZombieSystem.inSpawnRange(p, targets) && !zombies
                        .exists(z => (z.pos - p).lengthSquared < 2.25f) &&
                      ZombieSystem.canSpawn(
                        p,
                        daylightFactor,
                        if skyOpenAt(px, py + 1, pz) then 15 else psky,
                        plight,
                        activeBlockAt
                      )
                    }
                    .foreach(addMob)
            y -= 1
        tries += 1
    zombiePlanning.advance(dt)
    zombies.foreach { z =>
      val x = floor(z.pos.x).toInt; val y = floor(z.pos.y + 1.8f).toInt;
      val zz = floor(z.pos.z).toInt
      val exposed = daylightFactor > 0.72f && skyOpenAt(x, y, zz)
      val rain = WeatherField.sample(weatherCells, z.pos.x, z.pos.z).rain > 0.25f
      val beforeHealth = z.health
      z.voiceTimer -= dt
      if z.voiceTimer <= 0f then
        emitZombieVoice(z, 0); z.voiceTimer = 6f + zombieRng.nextFloat() * 8f
      ZombieSystem
        .update(z, dt, targets, exposed, rain, activeBlockAt, waterLevelAt, zombiePlanning)
        .foreach { attack =>
          emitZombieVoice(z, 2)
          val target = if attack.name == playerName then camera
          else remotePlayers.get(attack.name).map(_.targetPos).getOrElse(z.pos)
          val delta = Vec3(target.x - z.pos.x, 0, target.z - z.pos.z)
          val push = (if delta.lengthSquared > 0.001f then delta.normalized
                      else BlockboxRaycast.viewDirection(z.yaw, 0)) * 4.2f
          if attack.name == playerName then applyCombatDamage(attack.damage, "zombie", push)
          else if gameServer != null then
            gameServer.sendTo(attack.name, s"ZDAMAGE|${attack.damage}|${push.x}|${push.z}")
            gameServer.broadcast(s"HURT|${attack.name}")
        }
      if z.health < beforeHealth then emitZombieVoice(z, if z.health <= 0 then 3 else 1)
    }
    ZombieSystem.separate(zombies.toVector, activeBlockAt, dt)
    zombies.filterInPlace(z =>
      z.health > 0 && z.pos.y > 1 && targets.exists(p => (p.eye - z.pos).lengthSquared < 112 * 112)
    )
    zombieSyncTimer -= dt
    if gameServer != null && zombieSyncTimer <= 0 then
      gameServer.broadcast(ZombieSystem.snapshot(zombies.toVector)); zombieSyncTimer = 0.10f

  private def attackTargetEntity(): Boolean =
    val blockDistance = raycast(CombatSystem.reach).map(_.distance).getOrElse(CombatSystem.reach)
    val mobs = zombies
      .filter(_.health > 0)
      .flatMap(z =>
        ZombieSystem
          .rayDistance(eyePosition, viewDirection, z, blockDistance)
          .map(d => (d, Left(z.id)))
      )
    val players = remotePlayers.valuesIterator
      .filter(_.mode == GameMode.Survival)
      .flatMap(p =>
        CombatSystem
          .rayPlayer(eyePosition, viewDirection, p.pos, blockDistance, p.crouch)
          .map(d => (d, Right(p.name)))
      )
      .toVector
    (mobs ++ players).minByOption(_._1) match
      case None              => false
      case Some((_, target)) =>
        if combatCooldown <= 0f then
          combatCooldown = CombatSystem.cooldown.toFloat; sendSwing()
          target match
            case Left(id) =>
              if isClientOnlyMultiplayer then gameClient.send(s"ZHIT|$id")
              else hitZombie(playerName, id)
            case Right(name) =>
              if isClientOnlyMultiplayer then gameClient.send(s"PHIT|$name")
              else hitPlayer(playerName, name)
        true

  private def salvageCache(actor: String, x: Int, y: Int, z: Int): Unit =
    SupplyCaches.contents(worldSeed, x, y, z).foreach { (item, count) =>
      spawnDrop(item, count, Vec3(x + 0.5f, y + 0.5f, z + 0.5f))
    }
    if actor == playerName then fieldJournal.salvagedCache()
    else if gameServer != null then gameServer.sendTo(actor, "CACHEFOUND")

  private def spawnDrop(block: Block, count: Int, position: Vec3): Unit =
    var remaining = count.max(0)
    while remaining > 0 do
      val amount = remaining.min(64)
      droppedItems.spawn(block, amount, position)
      remaining -= amount
    itemSyncTimer = 0f

  private def dropHeldStack(all: Boolean): Unit =
    val slot = selectedBlock.max(0).min(hotbarBlocks.length - 1)
    val block = hotbarBlocks(slot)
    val count = if gameMode == GameMode.Creative then (if all then 64 else 1)
    else if all then hotbarCounts(slot)
    else hotbarCounts(slot).min(1)
    if DroppedItems.valid(block) && count > 0 && pendingTosses.size < 8 then
      val direction = viewDirection
      val origin = eyePosition - Vec3(0, 0.25f, 0)
      if isClientOnlyMultiplayer then
        sendPlayerPositionNetwork()
        val id = System.nanoTime().max(1)
        pendingTosses(id) = (block, count)
        gameClient.send(s"TOSS|$id|${block.ordinal}|$count")
        if gameMode == GameMode.Survival then consumeSelectedHotbar(count)
      else if droppedItems
          .spawn(block, count, origin, direction * 3.5f + Vec3(0, 1.2f, 0), playerName)
          .nonEmpty
      then
        if gameMode == GameMode.Survival then consumeSelectedHotbar(count)
        itemSyncTimer = 0f

  private def sendItemOffer(offer: DroppedItems.Offer): Unit =
    if gameServer != null then
      gameServer.sendTo(
        offer.player,
        s"ITEMOFFER|${offer.id}|${offer.block.ordinal}|${offer.count}"
      )
    itemSyncTimer = 0f

  private def updateDroppedItems(dt: Float): Unit =
    failedTosses.toVector.foreach { request =>
      pendingTosses.get(request).foreach { (block, count) =>
        if inventoryRoomFor(block) >= count then
          gainItem(block, count); pendingTosses.remove(request); failedTosses.remove(request)
      }
    }
    if isClientOnlyMultiplayer then
      droppedItems.entities.valuesIterator.foreach { e =>
        e.pos += (e.target - e.pos) * (1f - exp(-dt * 18f).toFloat)
        e.age += dt; e.delay = (e.delay - dt).max(0)
      }
      droppedItems.entities.filterInPlace((_, e) => e.age < DroppedItems.lifetime && e.pos.y > -16)
      pickupRequestTimer -= dt
      if pickupRequestTimer <= 0f then
        droppedItems.entities.valuesIterator
          .find(e =>
            e.delay <= 0 && (e.pos - (camera - Vec3(
              0,
              1f,
              0
            ))).lengthSquared < 3.6f && inventoryRoomFor(e.block) > 0
          )
          .foreach { e =>
            gameClient.send(s"PICK|${e.id}|${inventoryRoomFor(e.block).min(e.count)}")
            pickupRequestTimer = 0.20f
          }
    else
      droppedItems.update(
        dt,
        activeBlockAt,
        p => loadedChunkForBlock(floor(p.x).toInt, floor(p.z).toInt).nonEmpty
      )
      locally {
        droppedItems.entities.values.toVector.foreach { e =>
          if e.delay <= 0 && (e.pos - (camera - Vec3(0, 1f, 0))).lengthSquared < 3.6f then
            val room = inventoryRoomFor(e.block)
            if room > 0 then
              droppedItems
                .reserve(playerName, e.id, room.min(e.count), camera - Vec3(0, 1f, 0))
                .foreach { offer =>
                  gainItem(offer.block, offer.count);
                  droppedItems.resolve(playerName, offer.id, accepted = true)
                  playSpatialSound(BlockboxSounds.Action.Ui, offer.block, 0, 0.18f, 0)
                }
        }
      }
      itemSyncTimer -= dt
      if gameServer != null && itemSyncTimer <= 0 then
        itemSnapshotSerial += 1
        DroppedItems
          .messages(droppedItems.snapshot, itemSnapshotSerial)
          .foreach(gameServer.broadcast)
        itemSyncTimer = 0.15f

  /** Items blink through their last twenty seconds, then the five-minute timer removes them. */
  private def itemStillShown(age: Float): Boolean =
    if age < DroppedItems.lifetime - 20f then true
    else (age * 6f).toInt % 2 == 0

  private def renderDroppedItems(): Unit =
    val visible = droppedItems.entities.valuesIterator
      .filter(e => (e.pos - camera).lengthSquared < 48f * 48f && itemStillShown(e.age))
      .toVector
    if visible.isEmpty then return
    resetGlArraysAndBuffers()
    glEnable(GL_DEPTH_TEST); glDisable(GL_CULL_FACE)
    // Small soft contact shadows anchor resting items, including unshaded mode.
    glDisable(GL_TEXTURE_2D); glEnable(GL_BLEND); glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
    glDepthMask(false)
    visible
      .filter(e =>
        e.grounded || getWorldBlock(
          floor(e.pos.x).toInt,
          floor(e.pos.y - 0.15f).toInt,
          floor(e.pos.z).toInt
        ).solid
      )
      .foreach { e =>
        val y = floor(e.pos.y - 0.15f).toFloat + 1.003f
        glBegin(GL_TRIANGLE_FAN); glColor4f(0, 0, 0, 0.16f)
        glVertex3f(e.pos.x - camera.x, y, e.pos.z - camera.z)
        for i <- 0 to 16 do
          val a = i * Pi * 2 / 16
          glColor4f(0, 0, 0, 0f)
          glVertex3f(
            e.pos.x - camera.x + cos(a).toFloat * 0.24f,
            y,
            e.pos.z - camera.z + sin(a).toFloat * 0.24f
          )
        glEnd()
      }
    glDepthMask(true); glDisable(GL_BLEND)
    val shaded = shadersEnabled && shaderPipeline != null && shaderPipeline.beginEntityLighting()
    BlockboxPlayerModel.beginLighting(viewSpace = false, shaderColor = shaded)
    glEnable(GL_ALPHA_TEST); glAlphaFunc(GL_GREATER, 0.1f)
    visible.foreach { e =>
      val point = e.pos + Vec3(0, 0.10f + sin(e.age * 2.4f + e.id).toFloat * 0.035f, 0)
      if shaded then entityLightLevels(point, sampleBelow = 0f)
      val light = if shaded then (1f, 1f, 1f) else lightColorAt(point.x, point.y, point.z)
      glPushMatrix(); glTranslatef(point.x - camera.x, point.y, point.z - camera.z)
      glRotatef((e.age * 42f + e.id * 37f) % 360, 0, 1, 0)
      val copies =
        if e.count > 32 then 4 else if e.count > 16 then 3 else if e.count > 1 then 2 else 1
      for copy <- 0 until copies do
        glPushMatrix()
        glTranslatef(copy * 0.018f, copy * 0.018f, copy * 0.025f)
        if Block.isItem(e.block) then
          glScalef(0.42f, 0.42f, 0.42f)
          val center = ItemModel.centerFromGrip; glTranslatef(center.x, center.y, center.z)
          ItemModel.render(e.block, light)
        else if e.block == Block.Torch then
          glScalef(0.5f, 0.5f, 0.5f); drawHeldTorch(light, inFist = false)
        else if FloraModel.contains(e.block) then
          glScalef(0.45f, 0.45f, 0.45f); drawHeldFlora(e.block, light)
        else drawHeldBlockCube(e.block, 0.25f, light)
        glPopMatrix()
      glPopMatrix()
    }
    glDisable(GL_ALPHA_TEST); BlockboxPlayerModel.endLighting()
    if shaded then shaderPipeline.endEntityLighting()

  private def combatant(name: String): Option[(Vec3, Float, Float, Block, GameMode)] =
    if name == playerName then Some((eyePosition, yaw, pitch, heldHotbarBlock, gameMode))
    else
      remotePlayers
        .get(name)
        .map(p =>
          (PlayerStance.eye(p.targetPos, p.crouch), p.targetYaw, p.targetPitch, p.heldBlock, p.mode)
        )

  private def acceptCombatHit(attacker: String): Boolean =
    val now = System.nanoTime() * 1e-9
    if now - lastCombatHit.getOrElse(attacker, Double.NegativeInfinity) < CombatSystem.cooldown then
      false
    else
      lastCombatHit(attacker) = now
      true

  private def hitZombie(attacker: String, id: Int): Unit =
    for
      z <- zombies.find(z => z.id == id && z.health > 0);
      (from, ry, rp, held, _) <- combatant(attacker)
    do
      val direction = BlockboxRaycast.viewDirection(ry, rp)
      val wall = BlockboxRaycast
        .raycast(
          from,
          direction,
          CombatSystem.reach,
          (x, y, z) => if activeBlockAt(x, y, z).solid then activeBlockAt(x, y, z) else Block.Air
        )
        .map(_.distance)
        .getOrElse(CombatSystem.reach)
      if z.hitCooldown <= 0f && ZombieSystem
          .rayDistance(from, direction, z, wall)
          .nonEmpty && acceptCombatHit(attacker)
      then
        z.health -= ToolSystem.damage(held); z.hurt = 1f; z.hitCooldown = 0.45f
        val away = Vec3(z.pos.x - from.x, 0, z.pos.z - from.z).normalized
        z.velocity = away * 4f + Vec3(0, 3f, 0); z.grounded = false
        emitZombieVoice(z, if z.health <= 0f then 3 else 1)
        if z.health <= 0f then
          val loot = if z.heldItem != Block.Air && zombieRng.nextFloat() < 0.15f then z.heldItem
          else if zombieRng.nextFloat() < 0.12f then Block.IronIngot
          else Block.CoalLump
          spawnDrop(loot, 1, z.pos + Vec3(0, 0.5f, 0))
          if attacker == playerName then fieldJournal.defeatedZombie()
          else if gameServer != null then gameServer.sendTo(attacker, "ZKILL")

  private def hitPlayer(attacker: String, victim: String): Unit =
    if attacker == victim then return
    for (from, ry, rp, held, _) <- combatant(attacker); (target, _, _, _, mode) <- combatant(victim)
    do
      val targetCrouch = if victim == playerName then crouchAmount
      else remotePlayers.get(victim).map(_.crouch).getOrElse(0f)
      val canonicalTarget = target + Vec3(0, PlayerStance.eyeDrop * targetCrouch, 0)
      if mode == GameMode.Survival && CombatSystem.canHit(
          from,
          ry,
          rp,
          canonicalTarget,
          activeBlockAt,
          targetCrouch
        ) && acceptCombatHit(attacker)
      then
        val push = Vec3(target.x - from.x, 0, target.z - from.z).normalized * 4.2f
        val amount = ToolSystem.damage(held)
        if victim == playerName then applyCombatDamage(amount, attacker, push)
        else if gameServer != null then
          gameServer.sendTo(victim, s"PDMG|$attacker|$amount|${push.x}|${push.z}")
          gameServer.broadcast(s"HURT|$victim")
          remotePlayers.get(victim).foreach(_.hurt = 1f)

  private def applyCombatDamage(amount: Float, attacker: String, push: Vec3): Unit =
    val before = camera; val health = playerHealth
    damagePlayer(amount, attacker)
    if playerHealth < health && (camera - before).lengthSquared < 1f then
      playerKnockback = push
      velocity = Vec3(velocity.x, velocity.y.max(2.8f), velocity.z); onGround = false

  private def updatePlayerKnockback(dt: Float): Unit =
    if playerKnockback.lengthSquared > 0.001f then
      movePlayer(playerKnockback.x * dt, 0, playerKnockback.z * dt)
      playerKnockback = playerKnockback * exp(-dt * 7f).toFloat
    else playerKnockback = Vec3(0, 0, 0)

  override protected def soundEvent(
      action: BlockboxSounds.Action,
      block: Block,
      variant: Int
  ): Unit =
    if multiplayerMode && Set(
        BlockboxSounds.Action.Step,
        BlockboxSounds.Action.Land,
        BlockboxSounds.Action.Splash
      ).contains(action)
    then
      if gameClient != null then gameClient.send(s"SFX|${action.ordinal}|${block.ordinal}|$variant")
      else broadcastActionSound(playerName, action, block, variant, camera - Vec3(0, 1.25f, 0))

  private def broadcastActionSound(
      actor: String,
      action: BlockboxSounds.Action,
      block: Block,
      variant: Int,
      position: Vec3
  ): Unit =
    if gameServer != null then
      gameServer.broadcast(
        s"SOUND|${networkSafeName(actor)}|${action.ordinal}|${block.ordinal}|$variant|${position.x}|${position.y}|${position.z}"
      )
      if actor != playerName then hearActionSound(actor, action, block, variant, position)

  private def hearActionSound(
      actor: String,
      action: BlockboxSounds.Action,
      block: Block,
      variant: Int,
      position: Vec3
  ): Unit =
    if actor != playerName then
      val delta = position - eyePosition
      val distance = sqrt(delta.lengthSquared).toFloat
      val radius = if action == BlockboxSounds.Action.Step then 22f else 32f
      if distance < radius then
        val end = eyePosition + delta * ((distance - 0.8f).max(0f) / distance.max(0.001f))
        val mix = SpatialAudio.mix(
          eyePosition,
          yaw,
          position,
          radius,
          !ZombieSystem.visible(eyePosition, end, activeBlockAt),
          isUnderwater
        )
        playSpatialSound(
          action,
          block,
          variant,
          mix.gain * (if action == BlockboxSounds.Action.Step then 0.55f
                      else if action == BlockboxSounds.Action.Dig then 0.40f
                      else 0.85f),
          mix.pan
        )

  private def zombieVoice(event: Int, variant: Int, where: Vec3): Unit =
    val delta = where - camera
    val distance = sqrt(delta.lengthSquared).toFloat
    if distance < 28f then
      val right = Vec3(cos(toRadians(yaw)).toFloat, 0, sin(toRadians(yaw)).toFloat)
      val pan = ((delta.x * right.x + delta.z * right.z) / distance.max(1f)).max(-1f).min(1f)
      val cover = if ZombieSystem.visible(camera, where, activeBlockAt) then 1f else 0.32f
      val gain =
        0.85f * pow(1f - distance / 28f, 1.6).toFloat * cover * (if isUnderwater then 0.55f else 1f)
      playMonsterSound(event, variant, gain, pan)

  private def emitZombieVoice(z: ZombieSystem.Zombie, event: Int): Unit =
    val variant = zombieRng.nextInt(if event <= 1 then 3 else 2)
    val where = z.pos + Vec3(0, 1.35f, 0)
    zombieVoice(event, variant, where)
    if gameServer != null then
      gameServer.broadcast(s"ZVOICE|$event|$variant|${where.x}|${where.y}|${where.z}")

  private def damageFlash(value: Float): Boolean = value.isFinite && value > 0f

  /** The same animated bodies cast into the terrain shadow map. Keep this map for dynamic actors
    * even when Vulkan supplies the static-world ray shadows.
    */
  private def renderEntityShadows(): Unit =
    resetGlArraysAndBuffers()
    glDisable(GL_FOG); glDisable(GL_BLEND); glEnable(GL_DEPTH_TEST); glDepthMask(true)
    BlockboxPlayerModel.beginLighting(viewSpace = false, shaderColor = true)
    val local = RemotePlayer(
      playerName,
      camera,
      camera,
      yaw,
      yaw,
      pitch,
      pitch,
      0,
      localColorId,
      heldHotbarBlock
    )
    local.gait = cameraBobPhase; local.stride = cameraBobAmount; local.swing = viewmodelKick
    local.crouching = playerCrouching; local.crouch = crouchAmount
    val ownItem = if heldHotbarBlock == Block.Air then None
    else Some(() => drawHeldItemAtGrip(heldHotbarBlock, (1, 1, 1)))
    BlockboxPlayerModel.render(
      local,
      camera,
      colorForId(localColorId),
      (1, 1, 1),
      gameTime,
      ownItem
    )
    val now = glfwGetTime()
    remotePlayers.valuesIterator.filter(p => now - p.lastSeen <= 8).foreach { p =>
      val held = if p.heldBlock == Block.Air then None
      else Some(() => drawHeldItemAtGrip(p.heldBlock, (1, 1, 1)))
      BlockboxPlayerModel.render(p, camera, colorForId(p.colorId), (1, 1, 1), gameTime, held)
    }
    zombies.foreach { z =>
      val p = ZombieSystem.prepareAvatar(z)
      val held = if z.heldItem == Block.Air then None
      else Some(() => drawHeldItemAtGrip(z.heldItem, (1, 1, 1), zombie = true))
      BlockboxPlayerModel.render(
        p,
        camera,
        (0.17f, 0.4f, 0.39f),
        (1, 1, 1),
        gameTime,
        held,
        zombie = true,
        swimming = z.swimming,
        zombieStyle = z.id
      )
    }
    BlockboxPlayerModel.endLighting()

  private def entityLightLevels(eye: Vec3, sampleBelow: Float = 0.6f): Unit =
    val (sky, block) =
      lightLevelsAt(floor(eye.x).toInt, floor(eye.y - sampleBelow).toInt, floor(eye.z).toInt)
    org.lwjgl.opengl.GL13.glMultiTexCoord2f(GL_TEXTURE1, block * 16f, sky * 16f)

  private def renderZombies3D(): Unit =
    if zombies.isEmpty then return
    resetGlArraysAndBuffers()
    glEnable(GL_DEPTH_TEST); glDepthMask(true); glEnable(GL_FOG); glDisable(GL_BLEND)
    applyTerrainFog(isUnderwater)
    val sceneShaded =
      shadersEnabled && shaderPipeline != null && shaderPipeline.beginEntityLighting()
    BlockboxPlayerModel.beginLighting(viewSpace = false, shaderColor = sceneShaded)
    zombies.foreach { z =>
      val avatar = ZombieSystem.prepareAvatar(z)
      if sceneShaded then entityLightLevels(avatar.pos)
      renderImmersedEntity(avatar.pos, sceneShaded) { light =>
        val held = if z.heldItem == Block.Air then None
        else Some(() => drawHeldItemAtGrip(z.heldItem, light, zombie = true))
        BlockboxPlayerModel.render(
          avatar,
          camera,
          (0.17f, 0.40f, 0.39f),
          light,
          gameTime,
          held,
          zombie = true,
          hurt = damageFlash(z.hurt),
          swimming = z.swimming,
          zombieStyle = z.id
        )
      }
    }
    BlockboxPlayerModel.endLighting()
    if sceneShaded then shaderPipeline.endEntityLighting()
    zombies.filter(_.burning > 0).foreach(z => ZombieFire.render(z, camera, gameTime))

  private def renderRemotePlayers3D(): Unit =
    if remotePlayers.isEmpty then return
    resetGlArraysAndBuffers()
    val now = glfwGetTime()
    glDisable(GL_TEXTURE_2D); glEnable(GL_FOG); glDisable(GL_BLEND)
    glDepthMask(true); glEnable(GL_DEPTH_TEST)
    applyTerrainFog(isUnderwater)
    val sceneShaded =
      shadersEnabled && shaderPipeline != null && shaderPipeline.beginEntityLighting()
    BlockboxPlayerModel.beginLighting(viewSpace = false, shaderColor = sceneShaded)
    remotePlayers.valuesIterator.foreach { rp =>
      if now - rp.lastSeen <= 8.0 then
        if sceneShaded then entityLightLevels(rp.pos)
        renderImmersedEntity(rp.pos, sceneShaded) { light =>
          val held = if rp.heldBlock == Block.Air then None
          else Some(() => drawHeldItemAtGrip(rp.heldBlock, light))
          BlockboxPlayerModel.render(
            rp,
            camera,
            colorForId(rp.colorId),
            light,
            gameTime,
            held,
            hurt = damageFlash(rp.hurt)
          )
        }
    }
    BlockboxPlayerModel.endLighting()
    if sceneShaded then shaderPipeline.endEntityLighting()

  private def projectWorldToScreen(pos: Vec3): Option[(Float, Float, Float)] =
    val rel = pos - eyePosition
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
    val dynamicFov = viewFovOffset
    val tanHalf = tan(toRadians((fov + dynamicFov).max(45f).min(130f)) / 2.0).toFloat
    val ndcX = (x1 / -z2) / (tanHalf * aspect)
    val ndcY = (y2 / -z2) / tanHalf
    if ndcX < -1.35f || ndcX > 1.35f || ndcY < -1.35f || ndcY > 1.35f then None
    else
      Some(((ndcX * 0.5f + 0.5f) * framebufferWidth, (0.5f - ndcY * 0.5f) * framebufferHeight, -z2))

  private def renderPlayerNameTags(): Unit =
    if remotePlayers.isEmpty then return
    glDisable(GL_DEPTH_TEST); glDisable(GL_CULL_FACE); setupOrtho()
    val now = glfwGetTime()
    for (_, rp) <- remotePlayers.toSeq.sortBy { case (_, rp) =>
        (rp.pos - camera).lengthSquared
      }.reverse
    do
      if now - rp.lastSeen <= 8.0 then
        projectWorldToScreen(rp.eyePosition + Vec3(0f, 0.72f, 0f)).foreach { case (sx, sy, dist) =>
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
          centeredTextFit(
            x + boxW / 2f,
            y + 4f * uiScale,
            label,
            1f,
            0.95f,
            0.64f,
            scale,
            boxW - 8f * uiScale
          )
        }
    glEnable(GL_DEPTH_TEST)

  private def updateJournal(dt: Float): Unit =
    journalToastTime = (journalToastTime - dt).max(0f)
    journalTimer -= dt
    if journalTimer <= 0f then
      journalTimer = 0.5f
      val x = floor(camera.x).toInt; val z = floor(camera.z).toInt
      val surface = terrainGen.heightAt(x, z)
      val biome = terrainGen.biomeAt(x, z, surface)
      // Underground travel does not discover biomes through the ceiling.
      val biomeId = if camera.y >= surface - 4 then biome.ordinal else -1
      fieldJournal.observe(
        biomeId,
        (surface - camera.y).toInt.max(0),
        gameMode == GameMode.Survival,
        totalItemCount
      )
      if journalToastTime <= 0f then
        fieldJournal.nextAnnouncement().foreach { title =>
          journalToast = title; journalToastTime = 4f
        }

  private def journalLayout: JournalLayout =
    BlockboxUiLayout.journal(framebufferWidth, framebufferHeight, uiScale)

  private def journalPageCount: Int =
    (fieldJournal.entries.length + journalLayout.rows.length - 1) / journalLayout.rows.length

  private def journalAction(row: UiRect): UiRect =
    val s = journalLayout.scale
    UiRect(row.x + row.w - 122 * s, row.y + row.h - 24 * s, 112 * s, 19 * s)

  private def journalHintLines(text: String, scale: Float, width: Float): Vector[String] =
    val lines = ArrayBuffer.empty[String]
    var line = ""
    text.split(" ").foreach { word =>
      val next = if line.isEmpty then word else line + " " + word
      if line.nonEmpty && textWidth(next, scale) > width then
        lines += line; line = word
      else line = next
    }
    if line.nonEmpty then lines += line
    lines.toVector

  private def handleJournalClick(mx: Float, my: Float): Unit =
    val l = journalLayout
    journalPage = journalPage.max(0).min(journalPageCount - 1)
    if l.back.contains(mx, my) then enterGame()
    else if l.previous.contains(mx, my) then
      journalPage = Math.floorMod(journalPage - 1, journalPageCount)
    else if l.next.contains(mx, my) then journalPage = (journalPage + 1) % journalPageCount
    else
      for (row, i) <- l.rows.zipWithIndex do
        val index = journalPage * l.rows.length + i
        if row.contains(mx, my) then
          fieldJournal.track(index)
          if journalAction(row).contains(mx, my) && fieldJournal.entries
              .lift(index)
              .exists(_.completed) && !fieldJournal.rewardClaimed(
              index
            ) && gameMode == GameMode.Survival
          then
            fieldJournal.claim(index, inventoryRoomFor) match
              case Some((block, amount)) => gainItem(block, amount); playPlaceSound()
              case None => addChatMessage("Make room in your inventory to collect this reward.")

  private def renderJournal(): Unit =
    glDisable(GL_DEPTH_TEST); glDisable(GL_CULL_FACE); setupOrtho(); dimBackground()
    val l = journalLayout; val s = l.scale; val p = l.panel; val (mx, my) = mouseFramebufferPos()
    journalPage = journalPage.max(0).min(journalPageCount - 1)
    BlockboxRender2D.voxelPanel(p, s)
    centeredText(
      framebufferWidth / 2f,
      p.y + 22 * s,
      "Field journal",
      0.23f,
      0.23f,
      0.23f,
      2.2f * s
    )
    val completed = fieldJournal.entries.count(_.completed)
    centeredTextFit(
      framebufferWidth / 2f,
      p.y + 53 * s,
      s"$completed / ${fieldJournal.entries.length} goals  -  page ${journalPage + 1}/$journalPageCount",
      0.32f,
      0.32f,
      0.32f,
      s.max(1f),
      p.w - 36 * s
    )
    def button(r: UiRect, label: String, accent: Boolean = false): Unit =
      BlockboxRender2D.drawButton(
        r.x,
        r.y,
        r.w,
        r.h,
        label,
        accent,
        r.contains(mx, my),
        s,
        framebufferWidth,
        framebufferHeight
      )
    fieldJournal.entries
      .slice(journalPage * l.rows.length, (journalPage + 1) * l.rows.length)
      .zipWithIndex
      .foreach { (entry, i) =>
        val row = l.rows(i); val index = journalPage * l.rows.length + i
        BlockboxRender2D.voxelSlot(row, s)
        rect(row.x + s, row.y + s, row.w - 2 * s, row.h - 2 * s, 0.64f, 0.64f, 0.64f, 1)
        val titleScale = (1.15f * s).max(1f); val hintScale = (0.82f * s).max(1f)
        renderTextShadowFit(
          row.x + 12 * s,
          row.y + 7 * s,
          entry.title,
          0.17f,
          0.17f,
          0.17f,
          titleScale,
          row.w - 100 * s
        )
        val label =
          if entry.completed then "DONE" else s"${entry.progress.min(entry.target)}/${entry.target}"
        val progressScale = s.max(1f)
        renderTextShadow(
          row.x + row.w - 12 * s - textWidth(label, progressScale),
          row.y + 7 * s,
          label,
          0.18f,
          0.32f,
          0.15f,
          progressScale
        )
        journalHintLines(entry.hint, hintScale, row.w - 146 * s).take(2).zipWithIndex.foreach {
          (line, j) =>
            renderTextShadow(
              row.x + 12 * s,
              row.y + 27 * s + j * 10 * hintScale,
              line,
              0.24f,
              0.24f,
              0.24f,
              hintScale
            )
        }
        val reward = fieldJournal.reward(index).map((b, n) => s"$n ${blockName(b)}").getOrElse("")
        val text = if fieldJournal.rewardClaimed(index) then "Collected"
        else if entry.completed then "Claim " + reward
        else "Track"
        button(journalAction(row), text, entry.completed && !fieldJournal.rewardClaimed(index))
      }
    button(l.back, "Back to world", true); button(l.previous, "< Prev"); button(l.next, "Next >")

  private def renderHud(): Unit =
    glDisable(GL_DEPTH_TEST); glDisable(GL_CULL_FACE); setupOrtho()
    // Normal play stays quiet. Detailed telemetry belongs on F3, not permanently
    // stamped over the world like an engine sample.
    if gameMode == GameMode.Survival then renderHealth()
    if down(GLFW_KEY_TAB) then renderPlayerListOverlay()
    if gameMode == GameMode.Survival then
      fieldJournal.currentObjective.foreach { entry =>
        val s = uiScale; val w = (240 * s).min(framebufferWidth * 0.42f);
        val x = framebufferWidth - w - 16 * s
        rect(x, 16 * s, w, 39 * s, 0.055f, 0.09f, 0.08f, 0.70f)
        renderTextShadowFit(
          x + 9 * s,
          22 * s,
          entry.title,
          0.92f,
          0.91f,
          0.77f,
          0.88f * s,
          w - 18 * s
        )
        renderTextShadow(
          x + 9 * s,
          38 * s,
          s"${entry.progress.min(entry.target)} / ${entry.target}   J: journal",
          0.64f,
          0.79f,
          0.69f,
          0.72f * s
        )
      }
    if journalToastTime > 0f then
      val s = uiScale
      val tw = (textWidth(journalToast, 1.2f * s) + 44f * s).min(framebufferWidth - 32f * s)
      rect(framebufferWidth / 2f - tw / 2f, 24f * s, tw, 48f * s, 0.07f, 0.13f, 0.12f, 0.94f)
      centeredText(framebufferWidth / 2f, 31f * s, "JOURNAL UPDATED", 0.55f, 0.77f, 0.60f, 0.8f * s)
      centeredText(framebufferWidth / 2f, 48f * s, journalToast, 0.96f, 0.94f, 0.82f, 1.2f * s)

  private def heldHotbarBlock: Block =
    if hotbarBlocks.isEmpty then Block.Air
    else hotbarBlocks(selectedBlock.max(0).min(hotbarBlocks.length - 1))

  private def renderFirstPersonHeldBlock(): Unit =
    if hotbarBlocks.isEmpty || modUiCursorMode then return
    val block = if equipProgress >= 1f then heldHotbarBlock else equippedBlock
    updateFrameLight()
    resetGlArraysAndBuffers()
    glDisable(GL_FOG); glDisable(GL_CULL_FACE); glDisable(GL_TEXTURE_2D)
    val resolved = antiAliasing && firstPersonPass.begin(framebufferWidth, framebufferHeight)
    glEnable(GL_DEPTH_TEST); glDepthMask(true)
    if !resolved then
      glClear(GL_DEPTH_BUFFER_BIT)
      glEnable(GL_BLEND); glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA)
    glMatrixMode(GL_PROJECTION); glLoadIdentity()
    val aspect = framebufferWidth.toDouble / framebufferHeight.toDouble.max(1)
    val near = 0.05; val top = tan(toRadians(70.0) * 0.5) * near
    glFrustum(
      if resolved then firstPersonPass.leftEdge * top * aspect else -top * aspect,
      top * aspect,
      -top,
      top,
      near,
      8
    )
    glMatrixMode(GL_MODELVIEW); glLoadIdentity()
    val walk = FirstPersonPose.walkPose(cameraBobPhase, cameraBobAmount)
    glTranslatef(walk.position.x, walk.position.y, walk.position.z)
    glRotatef(walk.roll + cameraRoll * 0.5f, 0, 0, 1); glRotatef(walk.pitch, 1, 0, 0)
    BlockboxPlayerModel.beginLighting(viewSpace = true)
    val lagYaw = ((((yaw - viewmodelYaw) % 360f) + 540f) % 360f - 180f).max(-40f).min(40f)
    val lagPitch = (pitch - viewmodelPitch).max(-40f).min(40f)
    glRotatef(lagPitch * 0.14f, 1, 0, 0); glRotatef(lagYaw * 0.14f, 0, 1, 0)
    val light = viewmodelLight(block)
    val narrow = (aspect.toFloat / 1.2f).min(1f)
    if block == Block.Air then
      val hand = FirstPersonPose.handPose(viewmodelKick, viewmodelObstruction, equipProgress)
      glTranslatef(hand.position.x * narrow, hand.position.y, hand.position.z)
      glRotatef(hand.yaw, 0, 1, 0); glRotatef(hand.roll, 0, 0, 1)
      glTranslatef(-1f, 3.6f, 3.5f); glRotatef(120f, 0, 0, 1); glRotatef(200f, 1, 0, 0);
      glRotatef(-135f, 0, 1, 0)
      glTranslatef(5.6f - 5f / 16, 2f / 16, 0); glRotatef(0.1f * 180f / Pi.toFloat, 0, 0, 1)
      // The vanilla chain expects Minecraft's Y-down model space; Z 180 maps the skin box into it.
      glRotatef(180f, 0, 0, 1)
      BlockboxPlayerModel.firstPersonArm(colorForId(localColorId), light)
    else
      val held = FirstPersonPose.itemPose(block, viewmodelKick, viewmodelObstruction, equipProgress)
      glTranslatef(held.position.x * narrow, held.position.y, held.position.z)
      glRotatef(held.attack.y, 0, 1, 0); glRotatef(held.attack.z, 0, 0, 1);
      glRotatef(held.attack.x, 1, 0, 0)
      glRotatef(-45f, 0, 1, 0)
      val display = held.display
      glTranslatef(display.position.x, display.position.y, display.position.z)
      glRotatef(display.rotation.x, 1, 0, 0); glRotatef(display.rotation.y, 0, 1, 0);
      glRotatef(display.rotation.z, 0, 0, 1)
      glScalef(display.size, display.size, display.size)
      if Block.isItem(block) then
        val center = ItemModel.centerFromGrip
        glTranslatef(center.x, center.y, center.z)
        ItemModel.render(block, light)
      else if block == Block.Torch then drawHeldTorch(light, inFist = false)
      else if FloraModel.contains(block) then drawHeldFlora(block, light)
      else drawHeldBlockCube(block, 1f, light)
    BlockboxPlayerModel.endLighting()
    if resolved then firstPersonPass.finish()
    glColor4f(1, 1, 1, 1); glBindTexture(GL_TEXTURE_2D, 0)
    glDisable(GL_TEXTURE_2D); glDisable(GL_DEPTH_TEST); glDepthMask(true)

  /** Terrain light at the camera for the arm and held item; a held torch lights its own hand. */
  private def viewmodelLight(block: Block): (Float, Float, Float) =
    val (sky, blockLevel) =
      lightLevelsAt(floor(camera.x).toInt, floor(camera.y).toInt, floor(camera.z).toInt)
    val glow = if block == Block.Torch then blockLevel.max(14) else blockLevel
    BlockboxLightmap.color(sky / 15f, glow / 15f, frameLight)

  private def drawHeldItemAtGrip(
      block: Block,
      light: (Float, Float, Float),
      zombie: Boolean = false
  ): Unit =
    val grip =
      if zombie then FirstPersonPose.zombieAttachment(block) else FirstPersonPose.attachment(block)
    glPushMatrix()
    glTranslatef(grip.position.x, grip.position.y, grip.position.z)
    glRotatef(grip.rotation.x, 1, 0, 0); glRotatef(grip.rotation.y, 0, 1, 0);
    glRotatef(grip.rotation.z, 0, 0, 1)
    if block == Block.Torch then
      glScalef(grip.size, grip.size, grip.size)
      drawHeldTorch(light)
    else if Block.isItem(block) then
      glScalef(grip.size, grip.size, grip.size)
      ItemModel.render(block, light)
    else if FloraModel.contains(block) then
      glScalef(grip.size, grip.size, grip.size)
      drawHeldFlora(block, light)
    else drawHeldBlockCube(block, grip.size, light)
    glPopMatrix()

  private def drawHeldFlora(block: Block, light: (Float, Float, Float)): Unit =
    glEnable(GL_TEXTURE_2D); activeAtlas.bind()
    glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_MODULATE)
    val tint = BlockTints.item(block, FaceKind.North)
    glColor4f(light._1 * tint._1, light._2 * tint._2, light._3 * tint._3, 1f)
    glBegin(GL_TRIANGLES)
    FloraModel.vertices(block).foreach { vertex =>
      val uv = activeAtlas.uv(block, FaceKind.North, vertex.u, vertex.v)
      glTexCoord2f(uv._1, uv._2); glNormal3f(vertex.nx, vertex.ny, vertex.nz)
      glVertex3f(vertex.x - 0.5f, vertex.y - 0.5f, vertex.z - 0.5f)
    }
    glEnd()
    glColor4f(1, 1, 1, 1); glBindTexture(GL_TEXTURE_2D, 0); glDisable(GL_TEXTURE_2D)

  /** @param inFist
    *   origin at the grip with the butt cut inside the fist; otherwise the origin is the item
    *   sprite's center, where Minecraft's torch sprite has its stick.
    */
  private def drawHeldTorch(light: (Float, Float, Float), inFist: Boolean = true): Unit =
    val origin = if inFist then FirstPersonPose.torchGripHeight else 0.5f
    glEnable(GL_TEXTURE_2D); activeAtlas.bind()
    glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_MODULATE)
    val lit = glIsEnabled(GL_LIGHTING)
    // The stick takes the hand's lighting; the flame is emissive and drawn unlit.
    for flamePass <- Seq(false, true) do
      if flamePass && lit then glDisable(GL_LIGHTING)
      glBegin(GL_TRIANGLES)
      TorchModel.vertices(TorchMount.Floor).foreach { vertex =>
        if vertex.flame == flamePass then
          if flamePass then glColor4f(1f, 1f, 1f, 1f)
          else
            val shade = if lit then 1f else 0.72f + vertex.ny.max(0f) * 0.20f
            glColor4f(shade * light._1, shade * light._2, shade * light._3, 1f)
          val uv = activeAtlas.uv(Block.Torch, FaceKind.North, vertex.u, vertex.v)
          glTexCoord2f(uv._1, uv._2); glNormal3f(vertex.nx, vertex.ny, vertex.nz)
          // Only the flame tips move; its base and the ember stay on the stick.
          val tip = vertex.flame && vertex.y > 0.7f
          val sway = if tip then
            (sin(gameTime * 7.3f) * 0.6 + sin(gameTime * 12.1f + 1.3f) * 0.4).toFloat * 0.018f
          else 0f
          val lift = if tip then sin(gameTime * 9.563f).toFloat * 0.016f else 0f
          // The handle is tilted inside the fist; a full-length butt would poke out of the palm.
          val y = if inFist then vertex.y.max(FirstPersonPose.torchGripHeight - 0.06f) else vertex.y
          glVertex3f(vertex.x - 0.5f + sway, y - origin + lift, vertex.z - 0.5f + sway * 0.6f)
      }
      glEnd()
      if flamePass && lit then glEnable(GL_LIGHTING)
    glColor4f(1, 1, 1, 1); glBindTexture(GL_TEXTURE_2D, 0); glDisable(GL_TEXTURE_2D)

  private def drawHeldBlockCube(
      block: Block,
      size: Float,
      light: (Float, Float, Float) = (1f, 1f, 1f)
  ): Unit =
    val h = size * 0.5f
    glEnable(GL_TEXTURE_2D)
    activeAtlas.bind()
    glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_MODULATE)
    val lit = glIsEnabled(GL_LIGHTING)

    def quad(
        shade: Float,
        tint: (Float, Float, Float),
        normal: (Float, Float, Float),
        lift: Float,
        verts: Array[(Float, Float, Float)],
        tex: Array[(Float, Float)],
        uvAt: (Float, Float) => (Float, Float)
    ): Unit =
      val s = if lit then 1f else shade
      glColor4f(s * tint._1 * light._1, s * tint._2 * light._2, s * tint._3 * light._3, 1f)
      glNormal3f(normal._1, normal._2, normal._3)
      glBegin(GL_QUADS)
      var i = 0
      while i < 4 do
        val uv = uvAt(tex(i)._1, tex(i)._2)
        glTexCoord2f(uv._1, uv._2)
        glVertex3f(
          verts(i)._1 + normal._1 * lift,
          verts(i)._2 + normal._2 * lift,
          verts(i)._3 + normal._3 * lift
        )
        i += 1
      glEnd()

    def face(
        kind: FaceKind,
        shade: Float,
        normal: (Float, Float, Float),
        verts: Array[(Float, Float, Float)],
        tex: Array[(Float, Float)]
    ): Unit =
      quad(
        shade,
        BlockTints.item(block, kind),
        normal,
        0f,
        verts,
        tex,
        activeAtlas.uv(block, kind, _, _)
      )
      if block == Block.Grass && kind != FaceKind.Top && kind != FaceKind.Bottom && activeAtlas.hasGrassOverlay
      then
        glEnable(GL_ALPHA_TEST); glAlphaFunc(GL_GREATER, 0.5f)
        quad(
          shade,
          BlockTints.plainsGrass,
          normal,
          h * 0.01f,
          verts,
          tex,
          activeAtlas.grassOverlayUv
        )
        glDisable(GL_ALPHA_TEST)

    val topUv = Array((0f, 0f), (1f, 0f), (1f, 1f), (0f, 1f))
    val verticalUv = Array((0f, 1f), (1f, 1f), (1f, 0f), (0f, 0f))

    // Match Chunk.addFace's exact orientation. This is important for directional
    // textures such as logs, furnaces and any resource-pack art with an obvious top.
    face(
      FaceKind.Top,
      1.00f,
      (0f, 1f, 0f),
      Array((-h, h, -h), (h, h, -h), (h, h, h), (-h, h, h)),
      topUv
    )
    face(
      FaceKind.Bottom,
      0.48f,
      (0f, -1f, 0f),
      Array((-h, -h, h), (h, -h, h), (h, -h, -h), (-h, -h, -h)),
      topUv
    )
    face(
      FaceKind.South,
      0.82f,
      (0f, 0f, 1f),
      Array((h, -h, h), (-h, -h, h), (-h, h, h), (h, h, h)),
      verticalUv
    )
    face(
      FaceKind.North,
      0.60f,
      (0f, 0f, -1f),
      Array((-h, -h, -h), (h, -h, -h), (h, h, -h), (-h, h, -h)),
      verticalUv
    )
    face(
      FaceKind.East,
      0.74f,
      (1f, 0f, 0f),
      Array((h, -h, -h), (h, -h, h), (h, h, h), (h, h, -h)),
      verticalUv
    )
    face(
      FaceKind.West,
      0.66f,
      (-1f, 0f, 0f),
      Array((-h, -h, h), (-h, -h, -h), (-h, h, -h), (-h, h, h)),
      verticalUv
    )

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
    val hotbar =
      BlockboxUiLayout.hotbar(framebufferWidth, framebufferHeight, s, hotbarBlocks.length)
    // Reserve independent HUD lanes: hotbar at the bottom, hearts above it, then
    // transient item labels. This prevents the old item-name/health overlap.
    val healthLaneY = hotbar.slots.head.y - (18f * s).max(14f).min(22f) - 8f * s
    val y = if gameMode == GameMode.Survival then
      healthLaneY - (if isUnderwater || airSupply.remaining < airSupply.capacity then 54f
                     else 26f) * s
    else healthLaneY
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
    rect(
      0f,
      framebufferHeight.toFloat - edge,
      framebufferWidth.toFloat,
      edge,
      0.55f,
      0.02f,
      0.02f,
      a
    )
    rect(0f, edge, edge, framebufferHeight.toFloat - edge * 2f, 0.55f, 0.02f, 0.02f, a * 0.8f)
    rect(
      framebufferWidth.toFloat - edge,
      edge,
      edge,
      framebufferHeight.toFloat - edge * 2f,
      0.55f,
      0.02f,
      0.02f,
      a * 0.8f
    )

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
    centeredTextFit(
      x + panelW / 2f,
      y + 14f * s,
      s"Players (${names.length})",
      1f,
      0.92f,
      0.58f,
      (1.10f * s).max(0.96f),
      panelW - 24f * s
    )
    var yy = y + 40f * s
    for name <- names.take(((panelH - 48f * s) / rowH).toInt.max(1)) do
      val isSelf = name == playerName
      rect(
        x + 12f * s,
        yy - 3f * s,
        panelW - 24f * s,
        rowH - 2f * s,
        if isSelf then 0.16f else 0.08f,
        if isSelf then 0.13f else 0.09f,
        if isSelf then 0.06f else 0.12f,
        if isSelf then 0.55f else 0.38f
      )
      renderTextShadow(
        x + 22f * s,
        yy,
        if isSelf then s"$name (You)" else name,
        if isSelf then 1f else 0.86f,
        if isSelf then 0.88f else 0.92f,
        if isSelf then 0.55f else 1f,
        (1.02f * s).max(0.90f)
      )
      yy += rowH
    glEnable(GL_DEPTH_TEST)

  private def drawHeartIcon(
      x: Float,
      y: Float,
      size: Float,
      fillFrac: Float,
      bgAlpha: Float
  ): Unit =
    BlockboxRender2D.drawHeartIcon(x, y, size, fillFrac, bgAlpha)

  private def renderHealth(): Unit =
    glDisable(GL_DEPTH_TEST); glDisable(GL_CULL_FACE); setupOrtho()
    val s = uiScale
    val unit = (18f * s).max(14f).min(22f)
    val gap = (unit + 4f * s).max(unit + 3f).min(unit + 7f)
    val hotbar = BlockboxUiLayout.hotbar(framebufferWidth, framebufferHeight, s)
    val startX = hotbar.slots.head.x + 2f * s
    val barY = hotbar.slots.head.y - unit - 8f * s
    val bgAlpha = 0.46f
    val health = math.ceil(playerHealth.max(0).min(20)).toInt
    for i <- 0 until 10 do
      val x = startX + i * gap
      val fill = (health - i * 2).max(0).min(2) / 2f
      drawHeartIcon(x, barY, unit, fill, bgAlpha)
    if isUnderwater || airSupply.remaining < airSupply.capacity then
      for i <- 0 until airSupply.bubbles do
        val popping =
          i == airSupply.bubbles - 1 && (airSupply.remaining / airSupply.capacity * 10f) % 1f < 0.20f
        BlockboxRender2D.drawBubbleIcon(startX + i * gap, barY - unit - 7f * s, unit, popping)

  private def renderBlockInfo(): Unit =
    if framebufferHeight < 200 then return
    raycast(8f).foreach { hit =>
      val block = activeBlockAt(hit.block._1, hit.block._2, hit.block._3)
      val by = framebufferHeight.toFloat
      val hp = hit.block;
      val waterLvl = if block == Block.Water then waterLevelAt(hp._1, hp._2, hp._3) else -1
      val blockLabel = if waterLvl >= 0 then s"> ${block} (level $waterLvl/8)" else s"> ${block}"
      renderTextShadow(18, by - 90, blockLabel, 0.85f, 0.92f, 1f, 0.80f)
      renderTextShadow(
        18,
        by - 110,
        s"${hit.block._1}, ${hit.block._2}, ${hit.block._3}",
        0.70f,
        0.80f,
        0.90f,
        0.65f
      )
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
    val debugScale = (0.88f * uiScale).min(
      ((framebufferWidth.toFloat - 44f) / textWidth(
        "F3: Debug  [F4:wire OFF] [F5:borders OFF]",
        1f
      )).max(0.58f)
    )
    var ly = 20; val lx = 18
    val bg = 0.05f; val a = 0.65f
    val panelW = (framebufferWidth.toFloat - 24f).min(520f * uiScale).max(260f)
    val panelH = (framebufferHeight.toFloat - 34f).min(286f * uiScale).max(190f)
    rect(lx - 6, ly - 4, panelW, panelH, bg, bg, bg, a)
    renderTextShadowFit(
      lx,
      ly,
      s"F3: Debug  [F4:wire ${onOff(wireframeMode)}] [F5:borders ${onOff(showChunkBorders)}]",
      0.6f,
      0.8f,
      1f,
      debugScale,
      panelW - 18f
    );
    ly += (20 * debugScale).toInt.max(15)
    renderTextShadowFit(
      lx,
      ly,
      s"FPS: $fpsDisplay  Frame: ${(lastFrameTime * 1000).toInt}ms  Chunks: ${chunks.size}",
      0.4f,
      1f,
      0.4f,
      debugScale,
      panelW - 18f
    );
    ly += (19 * uiScale).toInt.max(16)
    renderTextShadowFit(
      lx,
      ly,
      s"XYZ: ${camera.x}%.3f / ${camera.y}%.3f / ${camera.z}%.3f",
      0.4f,
      1f,
      0.4f,
      debugScale,
      panelW - 18f
    );
    ly += (19 * uiScale).toInt.max(16)
    renderTextShadowFit(
      lx,
      ly,
      s"Yaw: $yaw%.2f  Pitch: $pitch%.2f",
      0.4f,
      1f,
      0.4f,
      debugScale,
      panelW - 18f
    );
    ly += (19 * uiScale).toInt.max(16)
    renderTextShadowFit(
      lx,
      ly,
      s"Chunk: ($ccx, $ccz)  Seed: $worldSeed  RD: ${renderDistance}ch/${renderDistanceBlocks}b",
      0.4f,
      1f,
      0.4f,
      debugScale,
      panelW - 18f
    );
    ly += (19 * uiScale).toInt.max(16)
    renderTextShadowFit(
      lx,
      ly,
      s"World: $worldName",
      0.4f,
      1f,
      0.4f,
      debugScale * 0.82f,
      panelW - 18f
    );
    ly += (17 * uiScale).toInt.max(14)
    renderTextShadowFit(
      lx,
      ly,
      s"Stream Q: create ${chunkCreateQueue.size}  mesh ${chunkBuildQueue.size}  upload ${chunkUploadQueue.size}  ready ${chunkReadyQueue.size}",
      0.55f,
      0.85f,
      1f,
      debugScale * 0.93f,
      panelW - 18f
    );
    ly += (18 * uiScale).toInt.max(15)
    renderTextShadowFit(
      lx,
      ly,
      f"Weather: rain $rainStrength%.2f  wet $wetness%.2f  particles ${blockParticles.size}%d  torch fx ${TorchEffects.flameCount}%d/${TorchEffects.particleCount}%d  saveQ ${pendingSaveJobs.get()}%d",
      0.55f,
      0.85f,
      1f,
      debugScale * 0.93f,
      panelW - 18f
    );
    ly += (18 * uiScale).toInt.max(15)
    renderTextShadowFit(
      lx,
      ly,
      s"Forward: (${fwd.x}%.4f, ${fwd.y}%.4f, ${fwd.z}%.4f)",
      0.4f,
      1f,
      0.4f,
      debugScale,
      panelW - 18f
    );
    ly += (19 * uiScale).toInt.max(16)
    renderTextShadowFit(
      lx,
      ly,
      s"World resolution: ${
          if foveatedRendering && foveaPass != null then (foveaPass.internalScale * 100).round
          else 100
        }%  Drawn chunks: ${renderChunkScratch.length}",
      0.55f,
      0.85f,
      1f,
      debugScale * 0.93f,
      panelW - 18f
    );
    ly += (18 * uiScale).toInt.max(15)
    raycast(8f).foreach { hit =>
      ly += 4; rect(lx - 6, ly - 4, 340, 62, bg, bg, bg, a)
      val block = activeBlockAt(hit.block._1, hit.block._2, hit.block._3)
      renderTextShadowFit(
        lx,
        ly,
        s"Hit: (${hit.block._1}, ${hit.block._2}, ${hit.block._3}) $block",
        1f,
        0.8f,
        0.3f,
        debugScale,
        panelW - 18f
      );
      ly += (19 * uiScale).toInt.max(16)
      renderTextShadowFit(
        lx,
        ly,
        s"Normal: (${hit.normal._1}, ${hit.normal._2}, ${hit.normal._3})  D: ${hit.distance}%.3f",
        1f,
        0.8f,
        0.3f,
        debugScale,
        panelW - 18f
      );
      ly += (19 * uiScale).toInt.max(16)
      renderTextShadowFit(
        lx,
        ly,
        s"Place: (${hit.place._1}, ${hit.place._2}, ${hit.place._3})",
        1f,
        0.8f,
        0.3f,
        debugScale,
        panelW - 18f
      );
      ly += (19 * uiScale).toInt.max(16)
    }
    glEnable(GL_DEPTH_TEST)

  private def renderHotbar(): Unit =
    compactHotbarAssignments()
    glDisable(GL_DEPTH_TEST); glDisable(GL_CULL_FACE); setupOrtho()
    val s = uiScale
    val layout =
      BlockboxUiLayout.hotbar(framebufferWidth, framebufferHeight, s, hotbarBlocks.length)
    BlockboxRender2D.voxelPanel(layout.panel, s)
    for (slot, i) <- layout.slots.zipWithIndex do
      val sx = slot.x; val y = slot.y; val slotSize = slot.w
      val block = hotbarBlocks(i)
      val hasItem = block != Block.Air && (gameMode == GameMode.Creative || hotbarCounts(i) > 0)
      BlockboxRender2D.drawSlotFrame(sx, y, slotSize, selected = false, hover = false, s)
      if hasItem then
        val icon = slotSize * 0.76f
        val iconY = y + (slotSize - icon) / 2f
        renderBlockIcon(block, sx + (slotSize - icon) / 2f, iconY, icon)
        val count = hotbarCounts(i)
        if count > 1 then
          val label = count.toString
          val scale = (1.8f * s).min(slotSize / 18f)
          renderTextShadow(
            sx + slotSize - textWidth(label, scale) - 2f * s,
            y + slotSize - PixelFont.height * scale - 3f * s,
            label,
            1,
            1,
            1,
            scale
          )
      else
        // Empty-slot hotkeys stay available without competing with item silhouettes.
        renderTextShadow(
          sx + 5f * s,
          y + 4f * s,
          hotbarLabel(i),
          0.85f,
          0.85f,
          0.85f,
          (0.9f * s).min(slotSize / 28f)
        )
    layout.slots.lift(selectedBlock).foreach { slot =>
      BlockboxRender2D.drawSlotSelection(slot.x, slot.y, slot.w, s)
    }

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

  private def drawCompactNumber(
      x: Float,
      y: Float,
      text: String,
      cell: Float,
      r: Float,
      g: Float,
      b: Float
  ): Unit =
    val safe = Option(text).getOrElse("").filter(ch => ch == '-' || ch.isDigit)
    if safe.isEmpty then return
    def pass(ox: Float, oy: Float, pr: Float, pg: Float, pb: Float, pa: Float): Unit =
      var cx = x + ox
      for ch <- safe do
        val glyph = compactDigitGlyph(ch)
        for row <- 0 until 5 do
          val line = glyph(row)
          for col <- 0 until 3 do
            if line.charAt(col) == '1' then
              rect(cx + col * cell, y + oy + row * cell, cell * 0.82f, cell * 0.82f, pr, pg, pb, pa)
        cx += 4f * cell
    val shadow = (cell * 0.45f).max(1f)
    pass(shadow, shadow, 0f, 0f, 0f, 0.70f)
    pass(0f, 0f, r, g, b, 1f)

  private def renderBlockIcon(block: Block, x: Float, y: Float, size: Float): Unit =
    if Block.isItem(block) then
      ItemModel.icon(block, x, y, size)
      return
    glEnable(GL_TEXTURE_2D); activeAtlas.bind()
    glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_MODULATE)

    def quad(
        shade: Float,
        tint: (Float, Float, Float),
        a: (Float, Float),
        b: (Float, Float),
        c: (Float, Float),
        d: (Float, Float),
        uvAt: (Float, Float) => (Float, Float)
    ): Unit =
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

    if block == Block.Torch || FloraModel.contains(block) then
      // Cutout scenery and torches use their actual transparent sprites in every UI.
      // Use the selected pack's actual art in every UI that calls this helper.
      glEnable(GL_BLEND); glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA)
      quad(
        1f,
        BlockTints.item(block, FaceKind.North),
        (x, y),
        (x + size, y),
        (x + size, y + size),
        (x, y + size),
        activeAtlas.uv(block, FaceKind.North, _, _)
      )
      glColor4f(1, 1, 1, 1); glBindTexture(GL_TEXTURE_2D, 0); glDisable(GL_TEXTURE_2D)
      return

    // a-b is the top edge and d-c the bottom edge of each face.
    def face(
        kind: FaceKind,
        shade: Float,
        a: (Float, Float),
        b: (Float, Float),
        c: (Float, Float),
        d: (Float, Float)
    ): Unit =
      quad(shade, BlockTints.item(block, kind), a, b, c, d, activeAtlas.uv(block, kind, _, _))
      if block == Block.Grass && kind != FaceKind.Top && kind != FaceKind.Bottom && activeAtlas.hasGrassOverlay
      then quad(shade, BlockTints.plainsGrass, a, b, c, d, activeAtlas.grassOverlayUv)

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
    if combatCooldown > 0f then
      val s = uiScale; val width = 16f * s
      val x = framebufferWidth * 0.5f - width * 0.5f; val y = framebufferHeight * 0.5f + 12f * s
      val ready = (1f - combatCooldown / CombatSystem.cooldown.toFloat).max(0).min(1)
      rect(x, y, width, 2f * s, 0.04f, 0.04f, 0.04f, 0.65f)
      rect(x, y, width * ready, 2f * s, 0.9f, 0.9f, 0.85f, 0.9f)

  private def renderUnderwaterOverlay(): Unit =
    if !UnderwaterEffect.render(framebufferWidth, framebufferHeight, gameTime, waterDepthAt(camera))
    then BlockboxRender2D.renderUnderwaterOverlay(framebufferWidth, framebufferHeight, gameTime)

  private def clearColor: (Float, Float, Float) =
    BlockboxSky.clearColor(
      isUnderwater,
      dayPhase,
      daylightFactor,
      localRain,
      localCloud,
      localLightning
    )

  protected val dayLengthSeconds = BlockboxSky.dayLengthSeconds
  protected val nightLengthSeconds = BlockboxSky.nightLengthSeconds

  private def gameTime: Float = worldClock.toFloat

  private def environmentMessage: String =
    Seq(
      "ENV",
      networkFloat(gameTime),
      networkFloat(timeOverride.getOrElse(-1f)),
      networkFloat(rainStrength),
      networkFloat(wetness),
      networkFloat(stormCloudCover),
      networkFloat(weatherWind),
      networkFloat(lightningFlash)
    ).mkString("|")

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
    frameLight = BlockboxLightmap.Environment(
      daylightFactor,
      dusk,
      localRain,
      localLightning,
      BlockboxLightmap.flicker(gameTime),
      worldDarkening
    )

  /** Sky and block light levels (0..15) at a block, from the chunk's last mesh. */
  private def lightLevelsAt(x: Int, y: Int, z: Int): (Int, Int) =
    if y >= Terrain.worldHeight then (15, 0)
    else
      chunks
        .get((Math.floorDiv(x, Terrain.chunkSize), Math.floorDiv(z, Terrain.chunkSize)))
        .flatMap(
          _.lightAt(Math.floorMod(x, Terrain.chunkSize), y, Math.floorMod(z, Terrain.chunkSize))
        )
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
    BlockboxSky.renderSky(
      yaw,
      pitch,
      dayPhase,
      daylightFactor,
      starPositions,
      cloudsEnabled,
      camera,
      gameTime,
      cloudRenderDistanceBlocks,
      localRain,
      localCloud,
      localLightning,
      !(shaderPipeline != null && shaderPipeline.isSceneBound)
    )

  private def weatherTarget(time: Float): Float =
    val overrideRain = modManager.rules.get("weather")
    if overrideRain >= 0 then overrideRain
    else if !weatherEnabled then 0f
    else
      val seedPhase =
        ((worldSeed ^ (worldSeed >>> 31)) & 0xffffL).toFloat / 65535f * (Pi * 2.0).toFloat
      val climate = sin(time * 0.0039f + seedPhase * 0.74f).toFloat * 0.40f
      val fronts = sin(time * 0.0117f + seedPhase * 1.63f + 1.4f).toFloat * 0.34f
      val texture = sin(time * 0.048f + seedPhase * 0.37f + 4.2f).toFloat * 0.16f
      val gusts = sin(time * 0.102f + seedPhase * 2.07f + 0.9f).toFloat * 0.09f
      val stormSignal = climate + fronts + texture + gusts
      val frontalBoost = smooth01((fronts + 0.20f) / 0.80f)
      smooth01((stormSignal - 0.10f) / 0.58f) * (0.80f + frontalBoost * 0.26f)

  private def updateWeather(dt: Float): Unit =
    if !isClientOnlyMultiplayer then snowCover = SnowCover.advance(snowCover, rainStrength, dt)
    // Quantized appearance changes reuse chunk meshes, with a strict two-chunk budget.
    chunks.valuesIterator.filter(_.needsSnowCover(snowCover)).take(2).foreach { chunk =>
      chunk.setSnowCover(snowCover); queueChunkMesh(chunk)
    }
    if !(isClientOnlyMultiplayer && environmentReceived) then
      val target = weatherTarget(gameTime).max(0f).min(1f)
      rainStrength =
        approachExp(rainStrength, target, if target > rainStrength then 0.62f else 0.34f, dt)
      val wetTarget = if rainStrength > 0.025f then (rainStrength * 0.92f + 0.08f).min(1f) else 0f
      wetness = approachExp(wetness, wetTarget, if wetTarget > wetness then 0.14f else 0.022f, dt)
      stormCloudCover = approachExp(
        stormCloudCover,
        (target * 0.92f + weatherTarget(gameTime + 18f) * 0.12f).min(1f),
        0.18f,
        dt
      )
      weatherWind = approachExp(
        weatherWind,
        sin(gameTime * 0.13f + (worldSeed & 255L).toFloat).toFloat * (0.35f + rainStrength * 0.65f),
        0.22f,
        dt
      )
    weatherCells = WeatherField.cells(
      worldSeed,
      gameTime,
      camera.x,
      camera.z,
      rainStrength,
      cloudRenderDistanceBlocks.toFloat
    )
    val local = WeatherField.sample(weatherCells, camera.x, camera.z)
    localRain = local.rain; localCloud = local.cloud
    val wetTarget = wetness * local.cloud
    localWetness =
      approachExp(localWetness, wetTarget, if wetTarget > localWetness then 0.4f else 0.035f, dt)
    weatherStrikes = WeatherField.strikes(weatherCells, gameTime)
    localLightning = WeatherField.flash(weatherStrikes, camera.x, camera.z)
    lightningFlash = 0f
    shelterTimer -= dt
    if shelterTimer <= 0f then
      rainShelter = if isUnderwater then 0.04f else if cameraHasOpenSky then 1f else 0.16f
      shelterTimer = 0.25f
    val climate = terrainGen.biomeAt(
      floor(camera.x).toInt,
      floor(camera.z).toInt,
      terrainGen.heightAt(floor(camera.x).toInt, floor(camera.z).toInt)
    )
    localSnowfall =
      climate == TerrainBiome.SnowyTaiga || climate == TerrainBiome.SnowyTundra || climate == TerrainBiome.JaggedPeaks
    updateRainAmbience(localRain * rainShelter * (if localSnowfall then 0.05f else 1f))
    val yawRadians = toRadians(yaw)
    weatherStrikes.foreach { strike =>
      val dx = strike.x - camera.x; val dz = strike.z - camera.z
      val distance = sqrt(dx * dx + dz * dz + 10000).toFloat
      val delay = distance / 343f
      if strike.age >= delay && strike.age < delay + 0.25f && heardThunder.add(strike.id) then
        val pan =
          ((dx * cos(yawRadians) + dz * sin(yawRadians)) / distance).toFloat.max(-1f).min(1f)
        playThunderSound(strike.strength / (1f + distance / 220f) * rainShelter.max(0.35f), pan)
    }
    if heardThunder.size > 128 then
      val active = weatherStrikes.map(_.id).toSet
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
    if isUnderwater then return
    if localSnowfall then
      if localRain > 0.025f then renderSnowfall()
    else
      RainRenderer.render(
        camera,
        yaw,
        gameTime,
        daylightFactor,
        localRain,
        weatherWind,
        weatherCells,
        worldSeed,
        activeBlockAt,
        waterLevelAt,
        (x, z) => loadedChunkForBlock(x, z).nonEmpty
      )

  private def renderSnowfall(): Unit =
    resetGlArraysAndBuffers()
    glEnable(GL_DEPTH_TEST); glDepthMask(false); glDisable(GL_TEXTURE_2D)
    glEnable(GL_BLEND); glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA)
    glEnable(GL_POINT_SMOOTH); glPointSize(3f)
    glBegin(GL_POINTS)
    val bx = floor(camera.x / 2).toInt; val bz = floor(camera.z / 2).toInt
    for x <- -10 to 10; z <- -10 to 10 do
      val wx = (bx + x) * 2; val wz = (bz + z) * 2
      var hash = wx * 73428767 ^ wz * 912931 ^ worldSeed.toInt
      hash ^= hash >>> 13; hash *= 1274126177; hash ^= hash >>> 16
      val phase = (gameTime * 0.07f + (hash & 65535) / 65535f) % 1f
      val y = camera.y + 9f - phase * 20f
      val ground = terrainGen.heightAt(wx, wz) + 1f
      if y > ground then
        val fade = (1f - (x * x + z * z) / 180f).max(0f) * localRain * 0.65f
        glColor4f(0.88f, 0.93f, 1f, fade)
        glVertex3f(
          wx - camera.x + sin(gameTime * 0.7f + x + z).toFloat * 0.7f + weatherWind * 1.2f,
          y,
          wz - camera.z + cos(gameTime * 0.6f + z).toFloat * 0.5f
        )
    glEnd()
    glDisable(GL_POINT_SMOOTH); glPointSize(1); glDepthMask(true); glDisable(GL_BLEND)

  private def spawnBlockBreakParticles(x: Int, y: Int, z: Int, block: Block): Unit =
    val material = block match
      case Block.Glass => 3
      case Block.Wood | Block.BirchWood | Block.PineWood | Block.AcaciaWood | Block.Planks |
          Block.BirchPlanks | Block.PinePlanks | Block.AcaciaPlanks =>
        1
      case Block.Dirt | Block.Grass | Block.Clay | Block.Sand | Block.Snow => 2
      case Block.Leaves | Block.BirchLeaves | Block.PineLeaves | Block.AcaciaLeaves |
          Block.Cactus =>
        4
      case _ => 0
    val count = material match
      case 3 => 24 + blockParticleRng.nextInt(10)
      case 2 => 20 + blockParticleRng.nextInt(9)
      case 4 => 16 + blockParticleRng.nextInt(8)
      case _ => 18 + blockParticleRng.nextInt(9)
    // Grass and leaf debris uses the biome colour of the block that broke.
    val grass = if block == Block.Grass || BlockTints.isFoliage(block) then
      terrainGen.grassTintAt(x, z, y)
    else BlockTints.plainsGrass
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
      val size =
        sizeBase + blockParticleRng.nextFloat() * (if material == 3 then 0.045f else 0.058f)
      val face = FaceKind.values(blockParticleRng.nextInt(FaceKind.values.length))
      val crop = 0.16f + blockParticleRng.nextFloat() * 0.34f
      val u0 = blockParticleRng.nextFloat() * (1f - crop)
      val v0 = blockParticleRng.nextFloat() * (1f - crop)
      val gravity = if material == 4 then 8.0f else if material == 3 then 13.8f else 12.2f
      val drag = if material == 4 then 3.4f else if material == 2 then 2.5f else 1.8f
      val bounce = if material == 3 then 0.30f else if material == 0 then 0.22f else 0.15f
      val tint =
        if BlockTints.isFoliage(block) then BlockTints.foliage(block, grass)
        else if (block == Block.Grass && face == FaceKind.Top) || FloraModel.grassTinted(block) then
          grass
        else (1f, 1f, 1f)
      blockParticles += BlockParticle(
        block,
        Vec3(px, py, pz),
        Vec3(vx, vy, vz),
        life,
        life,
        size,
        face,
        u0,
        v0,
        u0 + crop,
        v0 + crop,
        blockParticleRng.nextFloat() * Pi.toFloat * 2f,
        (blockParticleRng.nextFloat() - 0.5f) * (if material == 4 then 7f else 13f),
        gravity,
        drag,
        bounce,
        0.80f + blockParticleRng.nextFloat() * 0.20f,
        tint
      )
      i += 1
    while blockParticles.length > maxBlockParticles do blockParticles.remove(0)

  private var torchScanTimer = 0f

  /** Flame positions of torches within three chunks, for glow and torch particles. */
  private def refreshTorchFlames(): Unit =
    val ccx = chunkCoordBlock(floor(camera.x).toInt);
    val ccz = chunkCoordBlock(floor(camera.z).toInt)
    val found = ArrayBuffer.empty[TorchEffects.Flame]
    val furnaces = ArrayBuffer.empty[Vec3]
    for dx <- -3 to 3; dz <- -3 to 3 do
      chunks.get((ccx + dx, ccz + dz)).foreach { c =>
        c.lightSourcesSnapshot.foreach { s =>
          if c.getBlock(s.x, s.y, s.z) == Block.Torch then
            val (fx, fy, fz) = TorchModel.flameCenter(c.getTorchMount(s.x, s.y, s.z))
            found += TorchEffects.Flame(
              c.cx * Terrain.chunkSize + s.x + fx,
              s.y + fy,
              c.cz * Terrain.chunkSize + s.z + fz
            )
          else if c.getBlock(s.x, s.y, s.z) == Block.FurnaceLit then
            val x = c.cx * Terrain.chunkSize + s.x + 0.5f; val z = c.cz * Terrain.chunkSize + s.z
            furnaces += Vec3(x, s.y + 0.5f, z + 0.5f)
            found += TorchEffects.Flame(x, s.y + 0.42f, z - 0.02f)
            found += TorchEffects.Flame(x, s.y + 0.42f, z + 1.02f)
        }
      }
    TorchEffects.setFlames(found.toArray)
    nearbyFurnaceSounds = furnaces.toArray

  private def updateTorchEffects(dt: Float): Unit =
    torchScanTimer -= dt
    if torchScanTimer <= 0f then
      torchScanTimer = 0.25f
      refreshTorchFlames()
    TorchEffects.update(dt, camera)

  private def renderTorchEffects(): Unit =
    val (sky, _) =
      lightLevelsAt(floor(camera.x).toInt, floor(camera.y).toInt, floor(camera.z).toInt)
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
        p.velocity =
          Vec3(p.velocity.x * dragMul, p.velocity.y - p.gravity * dt, p.velocity.z * dragMul)
        p.rotation += p.spin * dt
        val next = p.pos + p.velocity * dt
        val bx = floor(next.x).toInt; val by = floor(next.y - p.size).toInt;
        val bz = floor(next.z).toInt
        if p.velocity.y < 0f && by >= 0 && activeBlockAt(bx, by, bz).solid then
          p.pos = Vec3(next.x, by + 1f + p.size, next.z)
          p.velocity = Vec3(p.velocity.x * 0.55f, -p.velocity.y * p.bounce, p.velocity.z * 0.55f)
        else p.pos = next
      i -= 1

  private def renderBlockParticles(): Unit =
    if blockParticles.isEmpty then return
    resetGlArraysAndBuffers()
    glPushAttrib(
      GL_ENABLE_BIT | GL_DEPTH_BUFFER_BIT | GL_COLOR_BUFFER_BIT | GL_CURRENT_BIT | GL_TEXTURE_BIT
    )
    glEnable(GL_DEPTH_TEST); glDepthMask(true)
    glDisable(GL_BLEND); glDisable(GL_LIGHTING)
    glEnable(GL_ALPHA_TEST); glAlphaFunc(GL_GREATER, 0.5f)
    val shaded = shadersEnabled && shaderPipeline != null && shaderPipeline.beginEntityLighting()
    if shaded then glDisable(GL_FOG)
    glEnable(GL_TEXTURE_2D); activeAtlas.bind();
    glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_MODULATE)
    // One camera-facing quad per particle, in one batch (a third of the old geometry).
    val (right, up) = ParticleBillboard.axes(yaw, pitch)
    val normal = viewDirection * -1f
    glBegin(GL_QUADS)
    glNormal3f(normal.x, normal.y, normal.z)
    blockParticles.foreach { p =>
      val lifeFrac = (p.life / p.maxLife).max(0f).min(1f)
      val s0 = p.size * (0.72f + lifeFrac * 0.28f) * (lifeFrac / 0.12f).min(1f)
      val c = cos(p.rotation).toFloat; val sn = sin(p.rotation).toFloat
      val horizontal = (right * c + up * sn) * s0
      val vertical = (up * c - right * sn) * s0
      val px = p.pos.x - camera.x; val py = p.pos.y; val pz = p.pos.z - camera.z
      val uv00 = activeAtlas.uv(p.block, p.face, p.u0, p.v0)
      val uv10 = activeAtlas.uv(p.block, p.face, p.u1, p.v0)
      val uv11 = activeAtlas.uv(p.block, p.face, p.u1, p.v1)
      val uv01 = activeAtlas.uv(p.block, p.face, p.u0, p.v1)
      val shade = p.brightness * (0.78f + lifeFrac * 0.22f)
      if shaded then entityLightLevels(p.pos, sampleBelow = 0f)
      val (lr, lg, lb) = if shaded then (1f, 1f, 1f) else lightColorAt(p.pos.x, p.pos.y, p.pos.z)
      glColor4f(shade * p.tint._1 * lr, shade * p.tint._2 * lg, shade * p.tint._3 * lb, 1f)
      def vertex(uv: (Float, Float), dx: Float, dy: Float): Unit =
        val offset = horizontal * dx + vertical * dy
        glTexCoord2f(uv._1, uv._2); glVertex3f(px + offset.x, py + offset.y, pz + offset.z)
      vertex(uv00, -1, -1); vertex(uv10, 1, -1)
      vertex(uv11, 1, 1); vertex(uv01, -1, 1)
    }
    glEnd()
    if shaded then shaderPipeline.endEntityLighting()
    glPopAttrib()

  private def updateBubbles(dt: Float): Unit =
    bubbleSpawnCooldown -= dt
    if isUnderwater then
      while bubbleParticles.length > 80 do bubbleParticles.remove(bubbleParticles.length - 1)
      if bubbleSpawnCooldown <= 0f && bubbleParticles.length < 80 then
        val offsetX = (bubbleRng.nextFloat() - 0.5f) * 0.6f
        val offsetZ = (bubbleRng.nextFloat() - 0.5f) * 0.6f
        bubbleParticles += ((
          camera.x + offsetX,
          camera.y + bubbleRng.nextFloat() * 1.4f,
          camera.z + offsetZ,
          1.5f + bubbleRng.nextFloat() * 2f
        ))
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
    val hotbarSlot = (46f * uiScale)
      .min(((w - 36f * uiScale) / hotbarBlocks.length.toFloat - 5f * uiScale).max(24f))
      .max(30f)
    val hudClearance =
      hotbarSlot + (if gameMode == GameMode.Survival then 88f * uiScale else 50f * uiScale)
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
      val inputScale = (1.34f * uiScale).max(1.14f)
      renderTextShadow(18, inputY + 10, ">", 1f, 1f, 1f, inputScale)
      renderEditableLine(
        TextBox.Chat,
        UiRect(18 + 12f * inputScale, inputY, chatWidth - 44f - 12f * inputScale, 30),
        inputScale,
        focused = true
      )
      val suggestions = commandSuggestions(chatInput)
      if suggestions.nonEmpty then
        val boxY = inputY - (suggestions.length * 22f * uiScale + 8f)
        rect(10, boxY, chatWidth - 10, inputY - boxY - 4f, 0f, 0f, 0f, 0.50f)
        suggestions.zipWithIndex.foreach { case (sug, idx) =>
          val selected = idx == commandSuggestionIndex.max(0).min(suggestions.length - 1)
          val y = boxY + 6f + idx * 22f * uiScale
          if selected then
            rect(14, y - 2f, chatWidth - 22f, 18f * uiScale, 0.20f, 0.22f, 0.28f, 0.72f)
          renderTextShadow(
            20,
            y,
            sug,
            if selected then 1f else 0.78f,
            if selected then 0.92f else 0.84f,
            if selected then 0.55f else 0.96f,
            (1.0f * uiScale).max(0.88f)
          )
        }

  private def mainMenuLayout(): MainMenuLayout =
    BlockboxUiLayout.mainMenu(framebufferWidth, framebufferHeight, uiScale)

  private def drawPixelLogo(cx: Float, y: Float, text: String, maxW: Float, maxH: Float): Unit =
    BlockboxRender2D.drawPixelLogo(cx, y, text, maxW, maxH)

  private def renderMainMenu(): Unit =
    glDisable(GL_DEPTH_TEST); glDisable(GL_CULL_FACE); setupOrtho()
    val w = framebufferWidth.toFloat; val h = framebufferHeight.toFloat
    BlockboxRender2D.drawMenuBackdrop(framebufferWidth, framebufferHeight, glfwGetTime().toFloat)
    val layout = mainMenuLayout(); val p = layout.panel; val s = layout.scale; val cx = w / 2f
    centeredText(cx, p.y + 28f * s, "A SANDBOX OF POSSIBILITIES", 0.60f, 0.76f, 0.69f, 1.0f * s)
    drawPixelLogo(cx, p.y + 62f * s, "BLOCKBOX", p.w - 104f * s, 62f * s)
    centeredText(cx, p.y + 142f * s, "Find your own way.", 0.91f, 0.89f, 0.78f, 1.45f * s)
    centeredText(
      cx,
      p.y + 171f * s,
      "Build a home. Follow a cave. Bring a friend.",
      0.65f,
      0.72f,
      0.72f,
      1.05f * s
    )
    val labels = Vector(
      "Create world",
      "Host a world",
      "Join friends",
      "My worlds",
      "Mods",
      "Textures & shaders",
      "Settings",
      "Quit"
    )
    layout.buttons.zipWithIndex.foreach { case (b, i) =>
      drawButton(b.x, b.y, b.w, b.h, labels(i), accent = i == 0)
    }
    centeredText(
      cx,
      p.y + p.h - 27f * s,
      "WASD move   /   E craft   /   J journal   /   Esc menu",
      0.58f,
      0.65f,
      0.64f,
      0.85f * s
    )
    if pendingSaveJobs.get() > 0 then
      centeredText(cx, h - 20f, "Saving world...", 0.92f, 0.78f, 0.48f, 0.9f * s)

  private def textMetrics(text: String): (java.nio.ByteBuffer, Int, Float, Float, Float, Float) =
    BlockboxRender2D.textMetrics(text)

  private def textWidth(text: String, scale: Float): Float =
    BlockboxRender2D.textWidth(text, scale)

  private def centeredTextFit(
      cx: Float,
      y: Float,
      text: String,
      r: Float,
      g: Float,
      b: Float,
      scale: Float,
      maxWidth: Float
  ): Unit =
    BlockboxRender2D.centeredTextFit(
      cx,
      y,
      text,
      r,
      g,
      b,
      scale,
      maxWidth,
      framebufferWidth,
      framebufferHeight
    )

  private def centeredText(
      cx: Float,
      y: Float,
      text: String,
      r: Float,
      g: Float,
      b: Float,
      scale: Float
  ): Unit =
    BlockboxRender2D.centeredText(cx, y, text, r, g, b, scale, framebufferWidth, framebufferHeight)

  private def centeredTextBox(
      x: Float,
      y: Float,
      w: Float,
      h: Float,
      text: String,
      r: Float,
      g: Float,
      b: Float,
      scale: Float
  ): Unit =
    BlockboxRender2D.centeredTextBox(
      x,
      y,
      w,
      h,
      text,
      r,
      g,
      b,
      scale,
      uiScale,
      framebufferWidth,
      framebufferHeight
    )

  private def renderText(
      x: Float,
      y: Float,
      text: String,
      r: Float,
      g: Float,
      b: Float,
      scale: Float = 1f
  ): Unit =
    BlockboxRender2D.renderText(x, y, text, r, g, b, scale, framebufferWidth, framebufferHeight)

  private def renderTextBuf(
      buf: java.nio.ByteBuffer,
      quads: Int,
      x: Float,
      y: Float,
      r: Float,
      g: Float,
      b: Float,
      scale: Float
  ): Unit =
    BlockboxRender2D.renderTextBuf(
      buf,
      quads,
      x,
      y,
      r,
      g,
      b,
      scale,
      framebufferWidth,
      framebufferHeight
    )

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
    renderNameField(
      TextBox.WorldName,
      settingX,
      nameY,
      settingW,
      34f * s,
      "World name",
      worldNameInput,
      createWorldNameFocused
    )
    val seedDisplay = if customSeedInput.nonEmpty then customSeedInput else worldSeed.toString
    renderNameField(
      TextBox.Seed,
      settingX,
      seedY,
      settingW,
      34f * s,
      "Seed (optional)",
      seedDisplay,
      !createWorldNameFocused
    )
    def settingRow(y: Float, label: String, value: String): Unit =
      val (mx, my) = mouseFramebufferPos()
      val hover = inRect(mx, my, settingX, y, settingW, settingH)
      val br = if hover then 0.13f else 0.09f
      rect(settingX, y, settingW, settingH, br, br * 1.08f, br * 1.22f, 0.66f)
      rect(settingX + 2f * s, y + 2f * s, settingW - 4f * s, 2f * s, 1f, 1f, 1f, 0.05f)
      renderTextShadowFit(
        settingX + 16f * s,
        y + settingH / 2f - 6f * s,
        label,
        0.82f,
        0.86f,
        0.92f,
        0.82f * s,
        settingW - 178f * s
      )
      centeredTextFit(
        settingX + settingW - 82f * s,
        y + settingH / 2f - 6f * s,
        value,
        1f,
        0.93f,
        0.54f,
        0.82f * s,
        150f * s
      )
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
    drawButton(
      buttonX,
      buttonStartY + (buttonH + gap) * 2f,
      buttonW,
      buttonH,
      "Edit seed"
    )
    drawButton(buttonX, buttonStartY + (buttonH + gap) * 3f, buttonW, buttonH, "Back")
    centeredTextFit(
      cx,
      pY + pH - 12f * s,
      "Tab switches fields | Ctrl+A selects all | Enter creates",
      0.42f,
      0.48f,
      0.60f,
      0.64f * s,
      pW - 48f * s
    )

  private def renderLoadWorldMenu(): Unit =
    glDisable(GL_DEPTH_TEST); glDisable(GL_CULL_FACE); setupOrtho()
    val w = framebufferWidth.toFloat; val h = framebufferHeight.toFloat; val cx = w / 2f;
    val s = uiScale
    BlockboxRender2D.drawMenuBackdrop(framebufferWidth, framebufferHeight, glfwGetTime().toFloat)
    val pW = (700f * s).min(w * 0.92f); val pH = (520f * s).min(h * 0.90f)
    val pX = cx - pW / 2f; val pY = h / 2f - pH / 2f
    drawPanel(pX, pY, pW, pH)
    centeredTextFit(cx, pY + 34f * s, "Select World", 1f, 1f, 1f, 1.75f * s, pW - 80f * s)
    rect(pX + 34f * s, pY + 64f * s, pW - 68f * s, 1f, 0.30f, 0.30f, 0.35f, 0.30f)
    val saves = worldSaveDirs
    if saves.isEmpty then
      centeredTextFit(
        cx,
        pY + 150f * s,
        "No saved worlds found in worlds/",
        0.78f,
        0.84f,
        0.94f,
        0.9f * s,
        pW - 80f * s
      )
    else
      val listX = pX + 42f * s
      val listY = pY + 86f * s
      val rowH = (46f * s).max(34f)
      val visible = ((pH - 160f * s) / rowH).toInt.max(3)
      if loadWorldSelection < loadWorldScroll then loadWorldScroll = loadWorldSelection
      if loadWorldSelection >= loadWorldScroll + visible then
        loadWorldScroll = loadWorldSelection - visible + 1
      loadWorldScroll = loadWorldScroll.max(0).min((saves.length - visible).max(0))
      saves.zipWithIndex.drop(loadWorldScroll).take(visible).foreach { case (dir, idx) =>
        val y = listY + (idx - loadWorldScroll) * rowH
        val selected = idx == loadWorldSelection
        val worldDat = new java.io.File(dir, "world.dat")
        rect(
          listX,
          y,
          pW - 84f * s,
          rowH - 6f * s,
          if selected then 0.22f else 0.08f,
          if selected then 0.20f else 0.09f,
          if selected then 0.10f else 0.13f,
          0.88f
        )
        if selected then rect(listX, y, 4f * s, rowH - 6f * s, 1f, 0.82f, 0.32f, 0.82f)
        renderTextShadow(listX + 14f * s, y + 7f * s, dir.getName, 0.94f, 0.96f, 1f, 0.76f * s)
        renderTextShadowFit(
          listX + 14f * s,
          y + 25f * s,
          s"Saved: ${new java.util.Date(worldDat.lastModified()).toString}",
          0.50f,
          0.58f,
          0.70f,
          0.62f * s,
          pW - 112f * s
        )
      }
    val bw = (220f * s).min(pW * 0.36f); val bh = (38f * s).max(30f)
    drawButton(cx - bw - 12f * s, pY + pH - 56f * s, bw, bh, "Load Selected", accent = true)
    drawButton(cx + 12f * s, pY + pH - 56f * s, bw, bh, "Back")
    centeredTextFit(
      cx,
      pY + pH - 12f * s,
      "Up/Down selects | Enter loads | ESC backs out",
      0.42f,
      0.48f,
      0.60f,
      0.46f * s,
      pW - 80f * s
    )

  private def handleLoadWorldClick(mx: Float, my: Float): Unit =
    val w = framebufferWidth.toFloat; val h = framebufferHeight.toFloat; val cx = w / 2f;
    val s = uiScale
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
    val w = framebufferWidth.toFloat; val h = framebufferHeight.toFloat; val cx = w / 2f;
    val s = uiScale
    BlockboxRender2D.drawMenuBackdrop(framebufferWidth, framebufferHeight, glfwGetTime().toFloat)
    val pW = (760f * s).min(w * 0.92f); val pH = (560f * s).min(h * 0.90f)
    val pX = cx - pW / 2f; val pY = h / 2f - pH / 2f
    drawPanel(pX, pY, pW, pH)
    centeredTextFit(cx, pY + 32f * s, "MODS", 1f, 0.94f, 0.55f, 2.0f * s, pW - 80f * s)
    centeredTextFit(
      cx,
      pY + 58f * s,
      s"server modpack: ${modManager.serverModpackHash}",
      0.62f,
      0.74f,
      0.95f,
      0.56f * s,
      pW - 80f * s
    )
    rect(pX + 34f * s, pY + 76f * s, pW - 68f * s, 1f, 0.30f, 0.34f, 0.42f, 0.30f)
    val mods = modManager.loadedMods
    if mods.isEmpty then
      centeredTextFit(
        cx,
        pY + 138f * s,
        "No mods loaded. Put .jar or Groovy mods in mods/",
        0.82f,
        0.88f,
        1f,
        0.78f * s,
        pW - 90f * s
      )
    else
      val rowH = (58f * s).max(42f)
      val listX = pX + 42f * s
      val listY = pY + 92f * s
      val visible = ((pH - 170f * s) / rowH).toInt.max(3)
      modsScreenScroll = modsScreenScroll.max(0).min((mods.length - visible).max(0))
      mods.zipWithIndex.drop(modsScreenScroll).take(visible).foreach { case (m, idx) =>
        val y = listY + (idx - modsScreenScroll) * rowH
        val ok = m.loaded
        rect(
          listX,
          y,
          pW - 84f * s,
          rowH - 6f * s,
          if ok then 0.08f else 0.20f,
          if ok then 0.11f else 0.08f,
          if ok then 0.16f else 0.08f,
          0.86f
        )
        rect(
          listX,
          y,
          4f * s,
          rowH - 6f * s,
          if ok then 0.25f else 0.95f,
          if ok then 0.75f else 0.25f,
          if ok then 0.35f else 0.20f,
          0.85f
        )
        renderTextShadowFit(
          listX + 14f * s,
          y + 7f * s,
          s"${m.name} ${m.version}  [${m.sideLabel}]",
          0.96f,
          0.97f,
          1f,
          0.78f * s,
          pW - 112f * s
        )
        renderTextShadowFit(
          listX + 14f * s,
          y + 25f * s,
          m.description,
          0.62f,
          0.70f,
          0.82f,
          0.64f * s,
          pW - 112f * s
        )
        renderTextShadowFit(
          listX + 14f * s,
          y + 42f * s,
          s"${m.id} | ${m.status}",
          if ok then 0.52f else 1f,
          if ok then 0.70f else 0.55f,
          if ok then 0.58f else 0.50f,
          0.58f * s,
          pW - 112f * s
        )
      }
    val bw = (260f * s).min(pW * 0.44f); val bh = (38f * s).max(30f)
    drawButton(cx - bw / 2f, pY + pH - 56f * s, bw, bh, "Back")
    centeredTextFit(
      cx,
      pY + pH - 15f * s,
      "mods are trusted full-access JVM code. server mods must match host.",
      0.48f,
      0.55f,
      0.68f,
      0.64f * s,
      pW - 70f * s
    )

  private def handleModsClick(mx: Float, my: Float): Unit =
    val w = framebufferWidth.toFloat; val h = framebufferHeight.toFloat; val cx = w / 2f;
    val s = uiScale
    val pW = (760f * s).min(w * 0.92f); val pH = (560f * s).min(h * 0.90f)
    val pY = h / 2f - pH / 2f
    val bw = (260f * s).min(pW * 0.44f); val bh = (38f * s).max(30f)
    if inRect(mx, my, cx - bw / 2f, pY + pH - 56f * s, bw, bh) then screen = Screen.MainMenu

  private def renderResourcesScreen(): Unit =
    if resourcePacks.isEmpty then refreshResourcePacks()
    glDisable(GL_DEPTH_TEST); glDisable(GL_CULL_FACE); setupOrtho()
    val w = framebufferWidth.toFloat; val h = framebufferHeight.toFloat; val cx = w / 2f;
    val s = uiScale
    BlockboxRender2D.drawMenuBackdrop(framebufferWidth, framebufferHeight, glfwGetTime().toFloat)
    val pW = (780f * s).min(w * 0.94f); val pH = (565f * s).min(h * 0.90f)
    val pX = cx - pW / 2f; val pY = h / 2f - pH / 2f
    drawPanel(pX, pY, pW, pH)
    centeredTextFit(cx, pY + 32f * s, "Resource Packs", 1f, 1f, 1f, 1.70f * s, pW - 80f * s)
    centeredTextFit(
      cx,
      pY + 58f * s,
      "Texture packs refresh immediately. Add custom packs in resourcepacks/<pack>/",
      0.62f,
      0.74f,
      0.95f,
      0.72f * s,
      pW - 80f * s
    )
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
      rect(
        listX,
        y,
        pW - 84f * s,
        rowH - 6f * s,
        if selected then 0.12f else 0.075f,
        if selected then 0.14f else 0.09f,
        if selected then 0.20f else 0.14f,
        0.88f
      )
      rect(
        listX,
        y,
        4f * s,
        rowH - 6f * s,
        if selected then 0.95f else if available then 0.30f else 0.75f,
        if selected then 0.78f else if available then 0.60f else 0.28f,
        if selected then 0.28f else if available then 0.85f else 0.22f,
        0.90f
      )
      renderTextShadowFit(
        listX + 14f * s,
        y + 8f * s,
        pack.name,
        if selected then 1f else 0.94f,
        if selected then 0.92f else 0.96f,
        if selected then 0.60f else 1f,
        0.92f * s,
        pW - 112f * s
      )
      renderTextShadowFit(
        listX + 14f * s,
        y + 32f * s,
        s"by ${pack.author}${if selected then "  [ACTIVE]" else ""}",
        0.62f,
        0.72f,
        0.86f,
        0.72f * s,
        pW - 112f * s
      )
      val pathText = if pack.legacy then "built-in procedural textures"
      else pack.dir.map(_.getPath).getOrElse("missing")
      renderTextShadowFit(
        listX + 14f * s,
        y + 53f * s,
        pathText,
        if available then 0.46f else 1f,
        if available then 0.56f else 0.48f,
        if available then 0.70f else 0.42f,
        0.64f * s,
        pW - 112f * s
      )
    }
    val bw = (210f * s).min(pW * 0.28f); val bh = (38f * s).max(30f); val bg = 10f * s
    val leftX = cx - (bw * 1.5f + bg)
    drawButton(leftX, pY + pH - 56f * s, bw, bh, "Refresh Textures")
    drawButton(
      cx - bw / 2f,
      pY + pH - 56f * s,
      bw,
      bh,
      s"Shaders: ${Option(shaderPipeline).map(_.packName).getOrElse("built-in")}"
    )
    drawButton(cx + bw / 2f + bg, pY + pH - 56f * s, bw, bh, "Back")
    centeredTextFit(
      cx,
      pY + pH - 15f * s,
      "resourcepacks/ for textures   shaderpacks/ for GLSL packs",
      0.48f,
      0.52f,
      0.58f,
      0.64f * s,
      pW - 70f * s
    )

  private def handleResourcesClick(mx: Float, my: Float): Unit =
    val w = framebufferWidth.toFloat; val h = framebufferHeight.toFloat; val cx = w / 2f;
    val s = uiScale
    val pW = (780f * s).min(w * 0.94f); val pH = (565f * s).min(h * 0.90f)
    val pX = cx - pW / 2f; val pY = h / 2f - pH / 2f
    val rowH = (74f * s).max(56f)
    val listX = pX + 42f * s
    val listY = pY + 96f * s
    val visible = ((pH - 188f * s) / rowH).toInt.max(3)
    val clickedPack = resourcePacks.zipWithIndex
      .drop(resourcesScroll)
      .take(visible)
      .find { case (_, idx) =>
        val y = listY + (idx - resourcesScroll) * rowH
        inRect(mx, my, listX, y, pW - 84f * s, rowH - 6f * s)
      }
      .map(_._1)
    clickedPack match
      case Some(pack) => applyTexturePack(pack)
      case None       =>
        val bw = (210f * s).min(pW * 0.28f); val bh = (38f * s).max(30f); val bg = 10f * s
        val leftX = cx - (bw * 1.5f + bg)
        if inRect(mx, my, leftX, pY + pH - 56f * s, bw, bh) then refreshResourcePacks()
        else if inRect(mx, my, cx - bw / 2f, pY + pH - 56f * s, bw, bh) then
          if shaderPipeline != null then shaderPipeline.cyclePack()
        else if inRect(mx, my, cx + bw / 2f + bg, pY + pH - 56f * s, bw, bh) then
          screen = resourcesReturnTo

  private def renderSettings(): Unit =
    glDisable(GL_DEPTH_TEST); glDisable(GL_CULL_FACE); setupOrtho()
    BlockboxRender2D.drawMenuBackdrop(framebufferWidth, framebufferHeight, glfwGetTime().toFloat)
    val s = uiScale; val layout = BlockboxUiLayout.settings(framebufferWidth, framebufferHeight, s)
    val p = layout.panel; val content = layout.content
    drawPanel(p.x, p.y, p.w, p.h)
    centeredText(framebufferWidth / 2f, p.y + 24f * s, "Settings", 0.96f, 0.94f, 0.86f, 2f * s)
    val names = Vector("Video", "World", "Interface")
    layout.tabs.zipWithIndex.foreach { case (tab, i) =>
      drawButton(tab.x, tab.y, tab.w, tab.h, names(i), accent = i == settingsTab)
    }
    val scrollMax = (settingsContentHeight(s) - content.h).max(0f)
    settingsScroll = settingsScroll.max(0f).min(scrollMax)
    val (mx, my) = mouseFramebufferPos()
    glEnable(GL_SCISSOR_TEST)
    glScissor(
      content.x.toInt,
      (framebufferHeight - content.y - content.h).toInt,
      content.w.toInt.max(1),
      content.h.toInt.max(1)
    )
    var y = content.y - settingsScroll
    settingsEntries.foreach { entry =>
      val rh = entry.height - 4f * s
      if y + rh >= content.y && y <= content.y + content.h then
        val hover = content.contains(mx, my) && UiRect(content.x, y, content.w, rh).contains(mx, my)
        rect(
          content.x,
          y,
          content.w,
          rh,
          if hover then 0.16f else 0.095f,
          if hover then 0.22f else 0.13f,
          if hover then 0.21f else 0.14f,
          0.94f
        )
        renderTextShadowFit(
          content.x + 12f * s,
          y + 9f * s,
          entry.label,
          0.90f,
          0.94f,
          0.90f,
          1.15f * s,
          content.w - 24f * s
        )
        val v = entry.id match
          case "rd" =>
            Some(
              (renderDistance - minRenderDistance).toFloat / (maxRenderDistance - minRenderDistance)
            )
          case "fov"    => Some((fov - 50f) / 70f)
          case "fog"    => Some((fogDensity - 0.6f) / 2.4f)
          case "clouds" =>
            Some(
              (cloudRenderDistance - minCloudRenderDistance).toFloat / (maxCloudRenderDistance - minCloudRenderDistance)
            )
          case _ => None
        v.foreach(value => slider(content.x + 8f * s, y + 31f * s, content.w - 16f * s, value))
      y += entry.height
    }
    glDisable(GL_SCISSOR_TEST)
    if scrollMax > 0f then
      val thumbH = (content.h * content.h / settingsContentHeight(s)).max(20f * s)
      val thumbY = content.y + settingsScroll / scrollMax * (content.h - thumbH)
      rect(content.x + content.w + 5f * s, content.y, 4f * s, content.h, 0.06f, 0.08f, 0.09f, 1f)
      rect(content.x + content.w + 5f * s, thumbY, 4f * s, thumbH, 0.48f, 0.68f, 0.56f, 1f)
    val d = layout.done
    drawButton(d.x, d.y, d.w, d.h, "Done", accent = true)

  private def renderPauseMenu(): Unit =
    glDisable(GL_DEPTH_TEST); glDisable(GL_CULL_FACE); setupOrtho()
    if multiplayerMode then
      // Multiplayer pause is a local overlay only. Do not black out the world; players/chats keep updating behind it.
      rect(0, 0, framebufferWidth, framebufferHeight, 0f, 0f, 0f, 0.08f)
    else dimBackground()
    val cx = framebufferWidth / 2f; val h = framebufferHeight.toFloat; val s = uiScale
    val pw = (420f * s).min(framebufferWidth * 0.80f);
    val ph = (if multiplayerMode then 300f * s else 275f * s).min(h * 0.82f); val px = cx - pw / 2f;
    val py = h / 2f - ph / 2f
    drawPanel(px, py, pw, ph)
    val titleText = "Game Menu"
    val titleScale = 2.0f * s
    val titleX = if multiplayerMode then cx else cx + ((6f * titleScale).max(5f) * titleScale) / 2f
    centeredText(titleX, py + 34 * s, titleText, 1f, 0.95f, 0.55f, titleScale)
    if multiplayerMode then
      centeredTextFit(
        cx,
        py + 60f * s,
        "Multiplayer session continues while paused",
        0.72f,
        0.72f,
        0.72f,
        0.70f * s,
        pw - 70f * s
      )
    rect(px + 40 * s, py + 76 * s, pw - 80 * s, 1, 0.30f, 0.30f, 0.35f, 0.30f)
    val bw = (320f * s).min(pw - 72f * s); val bh = (40f * s).min(ph / 6f).max(30f);
    val bx = cx - bw / 2f
    val gap = (14f * s).min(18f)
    val by0 = py + 92f * s
    drawButton(bx, by0, bw, bh, "Back to Game", accent = true)
    drawButton(bx, by0 + bh + gap, bw, bh, "Options")
    drawButton(
      bx,
      by0 + (bh + gap) * 2f,
      bw,
      bh,
      if multiplayerMode then "Disconnect to Title" else "Save and Quit to Title"
    )

  private def drawVoxelStack(
      block: Block,
      count: Int,
      p: UiRect,
      s: Float,
      showCount: Boolean = true
  ): Unit =
    if block == Block.Air || count <= 0 then return
    val size = p.w - 4 * s
    renderBlockIcon(block, p.x + 2 * s, p.y + 2 * s, size)
    if showCount && count > 1 then
      val label = count.toString; val cs = s * 0.95f
      val x = p.x + p.w - compactNumberWidth(label, cs) - s
      val y = p.y + p.h - 6 * cs
      drawCompactNumber(x + s, y + s, label, cs, 0.16f, 0.16f, 0.16f)
      drawCompactNumber(x, y, label, cs, 1f, 1f, 1f)

  private def voxelButton(p: UiRect, label: String, s: Float, mx: Float, my: Float): Unit =
    BlockboxRender2D.voxelPanel(p, s * 0.65f)
    if p.contains(mx, my) then rect(p.x + s, p.y + s, p.w - 3 * s, p.h - 3 * s, 1, 1, 1, 0.17f)
    centeredTextFit(
      p.x + p.w / 2,
      p.y + (p.h - 7 * s * 0.7f) / 2,
      label,
      0.20f,
      0.20f,
      0.20f,
      0.7f * s,
      p.w - 5 * s
    )

  private def renderInventoryPlayer(p: UiRect, mx: Float, my: Float): Unit =
    resetGlArraysAndBuffers()
    glPushAttrib(GL_ALL_ATTRIB_BITS)
    glViewport(p.x.toInt, (framebufferHeight - p.y - p.h).toInt, p.w.toInt.max(1), p.h.toInt.max(1))
    glEnable(GL_SCISSOR_TEST);
    glScissor(p.x.toInt, (framebufferHeight - p.y - p.h).toInt, p.w.toInt.max(1), p.h.toInt.max(1))
    glDepthMask(true); glClear(GL_DEPTH_BUFFER_BIT)
    glEnable(GL_DEPTH_TEST); glDisable(GL_FOG); glDisable(GL_BLEND); glDisable(GL_CULL_FACE)
    glMatrixMode(GL_PROJECTION); glPushMatrix(); glLoadIdentity()
    val halfW = 1.06f * p.w / p.h
    glOrtho(-halfW, halfW, -0.10, 2.02, 0.1, 20)
    glMatrixMode(GL_MODELVIEW); glPushMatrix(); glLoadIdentity(); glTranslatef(0, 0, -4)
    glRotatef(6f + (my - p.y - p.h / 2) / p.h * 8f, 1, 0, 0)
    BlockboxPlayerModel.beginLighting(viewSpace = true)
    // Minecraft's follow-the-cursor pose, measured from the eye (0.19 of the box down): the
    // head turns twice as far as the body. Yaw 180 faces the viewer; smaller yaw turns it to screen right.
    val reach = p.h * 0.63f
    val turn = atan((mx - p.x - p.w / 2) / reach).toFloat
    val lift = atan((my - p.y - p.h * 0.19f) / reach).toFloat
    val preview = RemotePlayer(
      playerName,
      Vec3(0, 1.62f, 0),
      Vec3(0, 1.62f, 0),
      180f - turn * 40f,
      180f - turn * 40f,
      lift * 20f,
      lift * 20f,
      0,
      localColorId,
      heldHotbarBlock
    )
    preview.bodyYaw = 180f - turn * 20f
    val held = if heldHotbarBlock == Block.Air then None
    else Some(() => drawHeldItemAtGrip(heldHotbarBlock, (1f, 1f, 1f)))
    BlockboxPlayerModel.render(
      preview,
      Vec3(0, 0, 0),
      colorForId(localColorId),
      (1f, 1f, 1f),
      0,
      held
    )
    BlockboxPlayerModel.endLighting()
    glMatrixMode(GL_MODELVIEW); glPopMatrix(); glMatrixMode(GL_PROJECTION); glPopMatrix();
    glMatrixMode(GL_MODELVIEW)
    glPopAttrib(); resetGlArraysAndBuffers()

  private def renderSurvivalInventory(): Unit =
    glDisable(GL_DEPTH_TEST); glDisable(GL_CULL_FACE); setupOrtho(); dimBackground()
    val l = BlockboxUiLayout.inventory(framebufferWidth, framebufferHeight, uiScale)
    val p = l.panel; val s = l.scale; val (mx, my) = mouseFramebufferPos()
    BlockboxRender2D.voxelPanel(p, s)
    renderText(p.x + 9 * s, p.y + 8 * s, "Inventory", 0.23f, 0.23f, 0.23f, s)
    renderText(p.x + 98 * s, p.y + 17 * s, "Crafting", 0.23f, 0.23f, 0.23f, s)
    rect(
      l.preview.x - s,
      l.preview.y - s,
      l.preview.w + 2 * s,
      l.preview.h + 2 * s,
      0.35f,
      0.35f,
      0.35f,
      1
    )
    rect(l.preview.x, l.preview.y, l.preview.w, l.preview.h, 0, 0, 0, 1)
    renderInventoryPlayer(l.preview, mx, my)
    var hovered: Option[(Block, UiRect)] = None
    def slot(block: Block, count: Int, r: UiRect, selected: Boolean = false): Unit =
      val hover = r.contains(mx, my)
      BlockboxRender2D.voxelSlot(r, s, hover, selected)
      drawVoxelStack(block, count, r, s)
      if hover && block != Block.Air && count > 0 then hovered = Some(block -> r)
    l.storage.indices.foreach(i => slot(backpackBlocks(i), backpackCounts(i), l.storage(i)))
    l.hotbar.indices.foreach(i =>
      slot(hotbarBlocks(i), hotbarCounts(i), l.hotbar(i), i == selectedBlock)
    )
    l.craft.indices.foreach(i => slot(craftGridBlocks(i), craftGridCounts(i), l.craft(i)))
    val result = currentCraftingResult
    slot(
      result.map(_.output).getOrElse(Block.Air),
      result.map(_.outputCount).getOrElse(0),
      l.output
    )
    val arrowX = p.x + 162 * s; val arrowY = p.y + 56 * s
    rect(arrowX, arrowY, 13 * s, 5 * s, 0.52f, 0.52f, 0.52f, 1)
    for i <- 0 until 5 do
      rect(arrowX + (12 + i) * s, arrowY + (i - 2) * s, s, (9 - 2 * i) * s, 0.52f, 0.52f, 0.52f, 1)
    voxelButton(l.furnace, "Furnace", s, mx, my); voxelButton(l.journal, "Journal", s, mx, my);
    voxelButton(l.close, "x", s, mx, my)
    if heldInventoryBlock == Block.Air then
      hovered.foreach { (block, r) => renderTooltip(r.x + r.w / 2, r.y - 2 * s, blockName(block)) }
    renderHeldInventoryCursor(mx, my)

  private val catalogTabs = Vector("All", "Blocks", "Tools", "Nature", "Materials", "Inventory")
  private def renderCatalog(): Unit =
    glDisable(GL_DEPTH_TEST); glDisable(GL_CULL_FACE); setupOrtho(); dimBackground()
    val l = BlockboxUiLayout.catalog(framebufferWidth, framebufferHeight, uiScale)
    val p = l.panel; val s = l.scale; val (mx, my) = mouseFramebufferPos()
    val icons = Vector(
      Block.Grass,
      Block.Brick,
      Block.IronSword,
      Block.Leaves,
      Block.DiamondGem,
      Block.SupplyCrate
    )
    l.tabs.zipWithIndex.foreach { (tab, i) =>
      val active = i == catalogCategory
      val r = if active then tab else UiRect(tab.x, tab.y + 3 * s, tab.w, tab.h - 3 * s)
      BlockboxRender2D.voxelPanel(r, s)
      if !active then rect(r.x + 3 * s, r.y + 3 * s, r.w - 6 * s, r.h - 6 * s, 0, 0, 0, 0.12f)
      renderBlockIcon(icons(i), r.x + (r.w - 16 * s) / 2, r.y + 4 * s, 16 * s)
    }
    BlockboxRender2D.voxelPanel(p, s)
    val activeTab = l.tabs(catalogCategory)
    rect(activeTab.x + 3 * s, p.y, activeTab.w - 6 * s, 4 * s, 0.77f, 0.77f, 0.77f, 1)
    renderText(p.x + 12 * s, p.y + 11 * s, catalogTabs(catalogCategory), 0.23f, 0.23f, 0.23f, s)
    voxelButton(l.close, "x", s, mx, my)
    val q = l.search
    BlockboxRender2D.voxelSlot(q, s)
    rect(q.x + s, q.y + s, q.w - 2 * s, q.h - 2 * s, 0.30f, 0.30f, 0.30f, 1)
    renderEditableLine(
      TextBox.Catalog,
      UiRect(q.x + 4 * s, q.y, q.w - 8 * s, q.h),
      0.9f * s,
      catalogSearchFocused,
      "Search items...",
      shadow = false
    )
    val items = catalogItems; val scrollMax = ((items.length + 8) / 9 - 6).max(0)
    catalogScroll = catalogScroll.max(0).min(scrollMax)
    var hovered: Option[(Block, UiRect)] = None
    l.slots.zipWithIndex.foreach { (r, i) =>
      val index = i + catalogScroll * 9; val hover = r.contains(mx, my)
      BlockboxRender2D.voxelSlot(r, s, hover)
      if index < items.length then
        drawVoxelStack(items(index), 1, r, s, false)
        if hover then hovered = Some(items(index) -> r)
    }
    if items.isEmpty then
      centeredText(p.x + p.w / 2, p.y + 120 * s, "No items found", 0.30f, 0.30f, 0.30f, s)
    val track = l.scroll
    BlockboxRender2D.voxelSlot(track, s)
    val thumbH =
      if scrollMax == 0 then track.h - 2 * s else (track.h * 6f / (scrollMax + 6)).max(12 * s)
    val thumbY = track.y + s + catalogScroll.toFloat / scrollMax.max(1) * (track.h - 2 * s - thumbH)
    BlockboxRender2D.voxelPanel(UiRect(track.x + s, thumbY, track.w - 2 * s, thumbH), s * 0.5f)
    rect(p.x + 10 * s, p.y + 213 * s, p.w - 20 * s, s, 0.38f, 0.38f, 0.38f, 1)
    l.hotbar.zipWithIndex.foreach { (r, i) =>
      val hover = r.contains(mx, my)
      BlockboxRender2D.voxelSlot(r, s, hover, i == selectedBlock)
      drawVoxelStack(hotbarBlocks(i), 1, r, s, false)
      if hover && hotbarBlocks(i) != Block.Air then hovered = Some(hotbarBlocks(i) -> r)
    }
    if heldInventoryBlock == Block.Air then
      hovered.foreach { (block, r) => renderTooltip(r.x + r.w / 2, r.y - 2 * s, blockName(block)) }
    l.tabs.zipWithIndex
      .find(_._1.contains(mx, my))
      .foreach((r, i) => renderTooltip(r.x + r.w / 2, r.y, catalogTabs(i)))
    renderHeldInventoryCursor(mx, my)

  protected def blockName(block: Block): String =
    block.displayName.replaceAll("([a-z])([A-Z])", "$1 $2")

  override protected def noteSmelted(block: Block, amount: Int): Unit =
    if gameMode == GameMode.Survival then fieldJournal.smelted(block, amount)

  override protected def resolveCraftingRecipe(cells: Array[Block]): Option[CraftRecipe] =
    modManager.recipes.result(cells)

  private def inventoryStacks: Array[(Block, Int)] =
    Array.tabulate(backpackBlocks.length)(i =>
      (backpackBlocks(i), backpackCounts(i).max(0).min(maxStackSize))
    )

  private def inventoryGridSlots: Int = backpackBlocks.length

  private def drawInventoryHotbar(
      panelX: Float,
      panelY: Float,
      panelW: Float,
      s: Float,
      mx: Float,
      my: Float
  ): Unit =
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
        if hover && heldInventoryBlock == Block.Air then
          renderTooltip(sx + slot / 2f, panelY - 6f * s, blockName(block))
      centeredTextFit(
        sx + slot / 2f,
        panelY + 4f * s,
        hotbarLabel(i),
        0.96f,
        0.96f,
        0.98f,
        0.54f * s,
        slot - 12f * s
      )
      i += 1

  private def renderHeldInventoryCursor(mx: Float, my: Float): Unit =
    if heldInventoryBlock != Block.Air then
      val s = if screen == Screen.Catalog then
        BlockboxUiLayout.catalog(framebufferWidth, framebufferHeight, uiScale).scale
      else BlockboxUiLayout.inventory(framebufferWidth, framebufferHeight, uiScale).scale
      drawVoxelStack(
        heldInventoryBlock,
        heldInventoryCount,
        UiRect(mx - 10 * s, my - 10 * s, 20 * s, 20 * s),
        s,
        gameMode == GameMode.Survival
      )

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
    Block.all.filter { b =>
      val available =
        b != Block.Air && b != Block.Water && b != Block.Bedrock && b != Block.FurnaceLit && CustomBlocks
          .registered(b)
      val category = catalogCategory match
        case 1 => b.solid && !BlockTints.isFoliage(b)
        case 2 => ToolSystem.isTool(b) || b == Block.Torch || b == Block.Furnace
        case 3 =>
          BlockTints.isFoliage(b) || FloraModel.contains(b) || Set(
            Block.Grass,
            Block.Dirt,
            Block.Sand,
            Block.Cactus,
            Block.Wood,
            Block.BirchWood,
            Block.PineWood,
            Block.AcaciaWood,
            Block.Snow,
            Block.Clay
          ).contains(b)
        case 4 => Block.isItem(b) && !ToolSystem.isTool(b)
        case _ => true
      available && category && blockName(b)
        .toLowerCase(java.util.Locale.ROOT)
        .contains(catalogQuery.toLowerCase(java.util.Locale.ROOT))
    }

  private def renderFurnaceUI(): Unit =
    glDisable(GL_DEPTH_TEST); glDisable(GL_CULL_FACE); setupOrtho(); dimBackground()
    val l = BlockboxUiLayout.furnace(framebufferWidth, framebufferHeight, uiScale)
    val p = l.panel; val s = l.scale; val (mx, my) = mouseFramebufferPos()
    BlockboxRender2D.voxelPanel(p, s)
    renderText(p.x + 9 * s, p.y + 8 * s, "Furnace", 0.23f, 0.23f, 0.23f, s)
    renderText(p.x + 9 * s, p.y + 115 * s, "Inventory", 0.23f, 0.23f, 0.23f, s)
    var hovered: Option[String] = None
    def slot(
        block: Block,
        count: Int,
        r: UiRect,
        selected: Boolean = false,
        label: String = ""
    ): Unit =
      val hover = r.contains(mx, my)
      BlockboxRender2D.voxelSlot(r, s, hover, selected)
      drawVoxelStack(block, count, r, s)
      if hover then
        hovered = Some(
          if label.nonEmpty then label else if block != Block.Air then blockName(block) else "Empty"
        )
    l.storage.indices.foreach(i =>
      slot(
        backpackBlocks(i),
        backpackCounts(i),
        l.storage(i),
        backpackBlocks(i) == furnaceInput && furnaceInput != Block.Air
      )
    )
    l.hotbar.indices.foreach(i =>
      slot(
        hotbarBlocks(i),
        hotbarCounts(i),
        l.hotbar(i),
        hotbarBlocks(i) == furnaceInput && furnaceInput != Block.Air
      )
    )
    slot(
      furnaceInput,
      if furnaceInput == Block.Air then 0 else furnaceInputAvailable,
      l.input,
      label = "Input - place ore here"
    )
    val fuel = if furnaceSlots || furnaceFuel != Block.Air then furnaceFuel
    else fuelBlocks.find(b => totalItemCount(b) > 0).getOrElse(Block.Air)
    slot(
      fuel,
      if fuel == Block.Air then 0
      else if furnaceSlots then furnaceFuelCount
      else totalItemCount(fuel),
      l.fuel,
      label = "Fuel - coal, logs, planks or sticks"
    )
    slot(
      furnaceOutput,
      furnaceOutputCount,
      l.output,
      label = if furnaceOutputCount > 0 then "Take smelted items" else "Output"
    )
    val arrow = l.progress
    rect(arrow.x, arrow.y, arrow.w, arrow.h, 0.39f, 0.39f, 0.39f, 1)
    val progress = (furnaceProgress / 4f).max(0).min(1)
    rect(
      arrow.x + s,
      arrow.y + s,
      (arrow.w - 2 * s) * progress,
      arrow.h - 2 * s,
      1f,
      0.65f,
      0.12f,
      1
    )
    val burning = if furnaceRequested > 0 then 1f else 0.2f
    rect(l.fuel.x + 7 * s, l.fuel.y - 12 * s, 6 * s, 9 * s, 1f, 0.43f, 0.05f, burning)
    rect(l.fuel.x + 9 * s, l.fuel.y - 9 * s, 2 * s, 6 * s, 1f, 0.85f, 0.2f, burning)
    val status = if furnaceRequested > 0 then s"Smelting $furnaceRequested"
    else if furnacePaused then "Paused - click Resume to continue"
    else "Shift-click moves ore and fuel into slots"
    renderTextShadowFit(
      p.x + 9 * s,
      p.y + 80 * s,
      status,
      0.25f,
      0.25f,
      0.25f,
      (0.62f * s).max(1f),
      p.w - 18 * s
    )
    val recipe = smeltResult(furnaceInput)
      .map((out, n) => s"${blockName(furnaceInput)} -> $n ${blockName(out)}")
      .getOrElse("Four seconds per item")
    renderTextShadowFit(p.x + 82 * s, p.y + 69 * s, recipe, 0.25f, 0.25f, 0.25f, 0.57f * s, 115 * s)
    voxelButton(l.one, "Take input", s, mx, my); voxelButton(l.all, "Take fuel", s, mx, my)
    voxelButton(l.stop, if furnacePaused then "Resume" else "Pause", s, mx, my);
    voxelButton(l.close, "x", s, mx, my)
    if heldInventoryBlock == Block.Air then
      hovered.foreach(text => renderTooltip(mx, my - 8 * s, text))
    renderHeldInventoryCursor(mx, my)

  private def handleFurnaceClick(
      mx: Float,
      my: Float,
      quickMove: Boolean = false,
      rightClick: Boolean = false
  ): Unit =
    val l = BlockboxUiLayout.furnace(framebufferWidth, framebufferHeight, uiScale)
    if l.close.contains(mx, my) then enterGame()
    else if l.one.contains(mx, my) then handleFurnaceSlot(false, false, true)
    else if l.all.contains(mx, my) then handleFurnaceSlot(true, false, true)
    else if l.stop.contains(mx, my) then {
      furnacePaused = !furnacePaused; furnaceRequested = 0; furnaceProgress = 0f
    } else if l.output.contains(mx, my) then clickFurnaceOutput(rightClick, quickMove)
    else if l.input.contains(mx, my) then handleFurnaceSlot(false, rightClick, quickMove)
    else if l.fuel.contains(mx, my) then handleFurnaceSlot(true, rightClick, quickMove)
    else
      val storage = l.storage.indexWhere(_.contains(mx, my))
      val hot = l.hotbar.indexWhere(_.contains(mx, my))
      if storage >= 0 then
        if quickMove && (smeltResult(backpackBlocks(storage)).nonEmpty || InventoryModel
            .fuelBurnTime(backpackBlocks(storage)) > 0)
        then quickFeedFurnace(backpackBlocks, backpackCounts, storage)
        else if quickMove then quickMoveBackpack(storage)
        else handleBackpackSlotClick(storage, rightClick)
      else if hot >= 0 then
        if quickMove && (smeltResult(hotbarBlocks(hot)).nonEmpty || InventoryModel.fuelBurnTime(
            hotbarBlocks(hot)
          ) > 0)
        then quickFeedFurnace(hotbarBlocks, hotbarCounts, hot)
        else if quickMove then quickMoveHotbar(hot)
        else if heldInventoryBlock == Block.Air then takeHotbarSlot(hot, rightClick)
        else placeHeldItemInHotbar(hot, rightClick)

  private def drawSlot(x: Float, y: Float, size: Float, block: Block, count: Int): Unit =
    rect(x - 2, y - 2, size + 4, size + 4, 0.06f, 0.06f, 0.06f, 1f)
    rect(x, y, size, size, 0.30f, 0.30f, 0.30f, 1f)
    rect(x + 3, y + 3, size - 6, size - 6, 0.12f, 0.12f, 0.12f, 0.9f)
    if count > 0 then
      val icon = (size * 0.56f).min(24f * uiScale).max(16f)
      renderBlockIcon(block, x + (size - icon) / 2f, y + (size - icon) / 2f, icon)
      val ns = count.toString; val s = uiScale
      val countScale = (2.0f * s).max(1.5f).min(size / 16f)
      val tw = compactNumberWidth(ns, countScale); val nx = x + size - tw - 5f * s;
      val ny = y + size - 5f * countScale - 4f * s
      rect(nx - 2f * s, ny - 1f * s, tw + 4f * s, 5f * countScale + 3f * s, 0f, 0f, 0f, 0.60f)
      drawCompactNumber(nx, ny, ns, countScale, 1f, 1f, 1f)

  private def drawPanel(x: Float, y: Float, w: Float, h: Float): Unit =
    BlockboxRender2D.drawPanel(x, y, w, h, uiScale)

  private def uiScale: Float =
    val maxByViewport =
      math.min(framebufferWidth.toFloat / 960f, framebufferHeight.toFloat / 600f).max(0.45f)
    val requested =
      guiScaleSetting match
        case 0 =>
          math
            .min(framebufferWidth.toFloat / 1280f, framebufferHeight.toFloat / 720f)
            .max(0.5f)
            .min(1.5f)
        case 1 => 1.00f
        case 2 => 1.50f
        case 3 => 2.00f
        case 4 => 3.00f
        case _ => 4.00f
    requested.min(maxByViewport).max(0.45f)

  private def dimBackground(): Unit =
    BlockboxRender2D.dimBackground(framebufferWidth, framebufferHeight)

  private def slider(x: Float, y: Float, w: Float, value: Float): Unit =
    BlockboxRender2D.slider(x, y, w, value, uiScale)

  private def resetGlArraysAndBuffers(): Unit =
    BlockboxRender2D.resetGlArraysAndBuffers()

  private def rect(
      x: Float,
      y: Float,
      w: Float,
      h: Float,
      r: Float,
      g: Float,
      b: Float,
      a: Float
  ): Unit =
    BlockboxRender2D.rect(x, y, w, h, r, g, b, a)

  private def drawButton(
      x: Float,
      y: Float,
      w: Float,
      h: Float,
      label: String,
      accent: Boolean = false
  ): Unit =
    val s = uiScale
    val (mx, my) = mouseFramebufferPos()
    val hover = inRect(mx, my, x, y, w, h)
    BlockboxRender2D.drawButton(
      x,
      y,
      w,
      h,
      label,
      accent,
      hover,
      s,
      framebufferWidth,
      framebufferHeight
    )

  private def renderTextShadow(
      x: Float,
      y: Float,
      text: String,
      r: Float,
      g: Float,
      b: Float,
      scale: Float = 1f
  ): Unit =
    BlockboxRender2D.renderTextShadow(
      x,
      y,
      text,
      r,
      g,
      b,
      scale,
      framebufferWidth,
      framebufferHeight
    )

  private def renderTextShadowFit(
      x: Float,
      y: Float,
      text: String,
      r: Float,
      g: Float,
      b: Float,
      scale: Float,
      maxWidth: Float
  ): Unit =
    BlockboxRender2D.renderTextShadowFit(
      x,
      y,
      text,
      r,
      g,
      b,
      scale,
      maxWidth,
      framebufferWidth,
      framebufferHeight
    )

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
    val behindPenalty = if facing < -0.25f then Terrain.chunkSize * Terrain.chunkSize * 10
    else if facing < 0.15f then Terrain.chunkSize * Terrain.chunkSize * 3
    else 0
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
      chunkBuildQueue.add(
        ChunkMeshJob(chunk, chunkWorkPriority(chunk.cx, chunk.cz), chunkJobSerial.incrementAndGet())
      )

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
    if gameClient == null then
      for ox <- -1 to 1; oz <- -1 to 1; candidate <- chunks.get((chunk.cx + ox, chunk.cz + oz)) do
        reconcileChunkTorches(candidate)

  private def reconcileChunkTorches(chunk: Chunk): Unit =
    def loadedAt(x: Int, y: Int, z: Int): Option[Block] =
      if y < 0 || y >= Terrain.worldHeight then Some(Block.Air)
      else
        loadedChunkForBlock(x, z).map(
          _.getBlock(Math.floorMod(x, Terrain.chunkSize), y, Math.floorMod(z, Terrain.chunkSize))
        )
    val oldMounts = chunk.torchPositions.map(p => p -> chunk.getTorchMount(p._1, p._2, p._3)).toMap
    chunk.migrateLegacyTorches(loadedAt)
    chunk.torchPositions.foreach { case (lx, y, lz) =>
      val x = chunk.baseX + lx; val z = chunk.baseZ + lz
      val support = chunk.getTorchMount(lx, y, lz).support
      if oldMounts.get((lx, y, lz)).exists(_ != chunk.getTorchMount(lx, y, lz)) then
        dirtyChunkAt(x, z)
        sendBlockNetwork(x, y, z, Block.Torch.id)
      if chunk.torchAttachmentResolved(lx, y, lz) && loadedAt(
          x + support._1,
          y + support._2,
          z + support._3
        ).exists(b => !TorchMount.canSupport(b))
      then
        setActiveBlock(x, y, z, Block.Air)
        spawnDrop(Block.Torch, 1, Vec3(x + 0.5f, y + 0.3f, z + 0.5f))
        sendBlockNetwork(x, y, z, Block.Air.id)
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
      chunkCreateQueue.add(
        ChunkCreateJob(
          cx,
          cz,
          chunkStreamGeneration,
          chunkWorkPriority(cx, cz),
          chunkJobSerial.incrementAndGet(),
          savedDir
        )
      )
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
          chunks.get((ccx + dx, ccz + dz)).foreach { c =>
            if !c.isDisposed && c.hasMesh then readyCount += 1
          }
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
        if c.meshReady then if c.uploadMesh() then built += 1 else queueChunkMesh(c)
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
    // Lit is a running state, not a second placeable block. Recover an
    // interrupted remote session without leaving an eternally burning furnace.
    if canUseLocalChunkSaves then
      chunk.lightSourcesSnapshot.foreach { source =>
        if chunk.getBlock(source.x, source.y, source.z) == Block.FurnaceLit then
          val p = (cx * Terrain.chunkSize + source.x, source.y, cz * Terrain.chunkSize + source.z)
          val running =
            (furnaceRequested > 0 && furnacePosition.contains(p)) || remoteFurnaces.valuesIterator
              .contains(p)
          if !running then
            chunk.setBlock(source.x, source.y, source.z, Block.Furnace)
            networkBlockOverrides.remove(p)
            changed = true
      }
    def applyOne(wx: Int, wy: Int, wz: Int, block: Block): Unit =
      val lx = wx - cx * Terrain.chunkSize
      val lz = wz - cz * Terrain.chunkSize
      if lx >= 0 && lx < Terrain.chunkSize && wy >= 0 && wy < Terrain.worldHeight && lz >= 0 && lz < Terrain.chunkSize
      then
        val axis = if Block.isLog(block) then
          LogAxis.fromId(networkLogAxesOverrides.getOrElse((wx, wy, wz), 0))
        else LogAxis.Y
        chunk.setBlock(
          lx,
          wy,
          lz,
          block,
          axis,
          TorchMount.fromId(networkTorchMounts.getOrElse((wx, wy, wz), 0))
        )
        if block == Block.Water then
          val level = networkWaterLevelOverrides
            .get((wx, wy, wz))
            .map(b => (b.toInt & 0xff).max(1).min(8))
            .getOrElse(8)
          chunk.setWaterLevel(lx, wy, lz, level)
          waterLevels((wx, wy, wz)) = level.toByte
          markWaterActive(wx, wy, wz)
        else
          chunk.setWaterLevel(lx, wy, lz, 0)
          waterLevels.remove((wx, wy, wz))
        changed = true

    val overrideKeys = networkBlockOverrides.positionsInChunk(cx, cz)
    overrideKeys.foreach { case (wx, wy, wz) =>
      networkBlockOverrides.get((wx, wy, wz)).foreach(block => applyOne(wx, wy, wz, block))
    }

    val pendingKeys = pendingNetworkBlocks.positionsInChunk(cx, cz)
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

  private var streamRadius = -1
  private var streamOffsets = Vector.empty[(Int, Int)]
  private def loadChunks(ccx: Int, ccz: Int): Unit =
    val worldRadius = renderDistanceBlocks + Terrain.chunkSize
    val radiusSq = worldRadius * worldRadius
    val chunkRadius = (worldRadius / 16) + 2
    // Keep workers supplied independently of host frame rate, with a bounded queue.
    val maxPerFrame = (64 - pendingChunkCreates.size()).max(0).min(12)
    var loaded = 0
    if streamRadius != chunkRadius then
      streamRadius = chunkRadius
      streamOffsets = (for dx <- -chunkRadius to chunkRadius; dz <- -chunkRadius to chunkRadius
      yield (dx, dz)).sortBy(p => (p._1 * p._1 + p._2 * p._2)).toVector
    for (dx, dz) <- streamOffsets do
      val wx = (ccx + dx) * 16 + 8; val wz = (ccz + dz) * 16 + 8
      val cx = ccx + dx; val cz = ccz + dz
      val dxb = wx - ccx * 16; val dzb = wz - ccz * 16
      if dxb * dxb + dzb * dzb <= radiusSq then
        if !chunks.contains((cx, cz)) && loaded < maxPerFrame then
          // Saved-chunk reads and procedural generation both happen on chunk workers.
          // The old path did Files.exists + full chunk reads here on the frame thread.
          if requestChunkCreate(cx, cz) then loaded += 1
        else if chunks.contains((cx, cz)) then
          val chunk = chunks((cx, cz))
          if !chunk.hasMesh && !chunk.meshReady then queueChunkMesh(chunk)
    // Retain a small halo beyond the actual load/draw radius. Crossing a chunk
    // boundary should not immediately serialize and destroy the chunk behind you,
    // only to load it again two seconds later when you turn around. This dramatically
    // reduces save I/O and mesh churn during normal exploration.
    val retainRadius = worldRadius + Terrain.chunkSize * 2
    val retainRadiusSq = retainRadius * retainRadius
    val cameraBlockX = ccx * Terrain.chunkSize
    val cameraBlockZ = ccz * Terrain.chunkSize
    chunkUnloadScratch.clear()
    chunks.foreach { case (key @ (cx, cz), _) =>
      val wx = cx * Terrain.chunkSize + Terrain.chunkSize / 2
      val wz = cz * Terrain.chunkSize + Terrain.chunkSize / 2
      val dx = wx - cameraBlockX; val dz = wz - cameraBlockZ
      val furnacePinned = (furnaceRequested > 0 && furnacePosition.exists(p =>
        chunkCoordBlock(p._1) == cx && chunkCoordBlock(p._3) == cz
      )) ||
        remoteFurnaces.valuesIterator.exists(p =>
          chunkCoordBlock(p._1) == cx && chunkCoordBlock(p._3) == cz
        )
      if !furnacePinned && dx * dx + dz * dz > retainRadiusSq then chunkUnloadScratch += key
    }
    val toRemove = chunkUnloadScratch
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
            BlockboxSaveFiles.writeChunkBytesAtomic(
              prepared.chunksDirectory(),
              saveCx,
              saveCz,
              snapshot.toBytes(),
              false
            )
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
    val chunkThreads = new java.util.concurrent.ThreadFactory:
      private val ids = new java.util.concurrent.atomic.AtomicInteger(0)
      override def newThread(job: Runnable): Thread =
        val t = new Thread(job, s"blockbox-chunk-${ids.incrementAndGet()}")
        t.setDaemon(true)
        // Below the audio mixer and the desktop audio process. Meshing stays
        // correct; it just yields the core when a sample is due.
        t.setPriority(Thread.NORM_PRIORITY - 2)
        t
    chunkGenPool = java.util.concurrent.Executors.newFixedThreadPool(threads, chunkThreads)
    for i <- 0 until threads do
      chunkGenPool.submit(new Runnable:
        override def run(): Unit =
          Thread.currentThread.setPriority(Thread.NORM_PRIORITY - 2)
          var preferCreate = i % 2 == 0
          def processCreate(): Boolean =
            val createKey = chunkCreateQueue.poll()
            if createKey == null then false
            else
              try
                val chunk = createKey.savedChunksDir match
                  case Some(dir) =>
                    val saved =
                      BlockboxSaveFiles.chunkFile(dir.toPath, createKey.cx, createKey.cz).toFile
                    if saved.isFile then
                      try
                        val emptyBlocks = new Array[Byte](
                          Terrain.chunkSize * Terrain.worldHeight * Terrain.chunkSize
                        )
                        val loaded = Chunk(
                          createKey.cx,
                          createKey.cz,
                          activeAtlas,
                          terrainGen,
                          emptyBlocks,
                          smoothLighting
                        )
                        loaded.load(dir)
                        loaded
                      catch
                        case loadError: Exception =>
                          System.err.println(
                            s"Corrupt chunk ignored ${createKey.cx},${createKey.cz}: $loadError"
                          )
                          try
                            saved.renameTo(
                              new java.io.File(
                                dir,
                                BlockboxSaveFiles.chunkFileName(createKey.cx, createKey.cz) + ".bad"
                              )
                            )
                          catch case _: Exception => ()
                          Chunk(
                            createKey.cx,
                            createKey.cz,
                            activeAtlas,
                            terrainGen,
                            smoothLightingEnabled = smoothLighting
                          )
                    else
                      Chunk(
                        createKey.cx,
                        createKey.cz,
                        activeAtlas,
                        terrainGen,
                        smoothLightingEnabled = smoothLighting
                      )
                  case None =>
                    Chunk(
                      createKey.cx,
                      createKey.cz,
                      activeAtlas,
                      terrainGen,
                      smoothLightingEnabled = smoothLighting
                    )
                if chunkGenRunning && createKey.generation == chunkStreamGeneration then
                  chunkReadyQueue.add((createKey.generation, chunk))
                else chunk.dispose()
              catch
                case e: Exception =>
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
              catch
                case e: Exception =>
                  chunk.releaseMeshQueue()
                  e.printStackTrace()
              true
          while chunkGenRunning do
            val didWork = if preferCreate then processCreate() || processMesh()
            else processMesh() || processCreate()
            preferCreate = !preferCreate
            if !didWork then
              try Thread.sleep(4)
              catch case _: InterruptedException => ())

  private def stopChunkGenThread(): Unit =
    chunkGenRunning = false
    val pool = chunkGenPool
    if pool != null then
      pool.shutdown()
      try pool.awaitTermination(3, java.util.concurrent.TimeUnit.SECONDS)
      catch case _: Exception => ()
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
          catch
            case e: Exception =>
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
          case None => requestChunkCreate(cx, cz)
        dz += 1
      dx += 1

  private def viewDirection: Vec3 =
    BlockboxRaycast.viewDirection(yaw, pitch)

  private def raycast(maxDistance: Float = modManager.rules.get("reach")): Option[RayHit] =
    BlockboxRaycast.raycast(eyePosition, viewDirection, maxDistance, activeBlockAt, torchMountAt)

  private def sendBlockNetwork(x: Int, y: Int, z: Int, blockId: Byte, logAxis: Int = 0): Unit =
    if multiplayerMode then
      val block = Block.fromId(blockId)
      val water = if block == Block.Water then waterLevelAt(x, y, z).max(1).min(8) else 0
      val mount = if block == Block.Torch then torchMountAt(x, y, z).id else 0
      networkBlockOverrides((x, y, z)) = block
      if block == Block.Water then networkWaterLevelOverrides((x, y, z)) = water.toByte
      else networkWaterLevelOverrides.remove((x, y, z))
      if Block.isLog(block) && logAxis != 0 then networkLogAxesOverrides((x, y, z)) = logAxis
      else networkLogAxesOverrides.remove((x, y, z))
      if block == Block.Torch then networkTorchMounts((x, y, z)) = mount
      else networkTorchMounts.remove((x, y, z))
      val msg = BlockboxNet.block(x, y, z, blockId.toInt, water, logAxis, mount)
      if gameClient != null && gameClient.isConnected then gameClient.send(msg)
      else if gameServer != null then
        gameServer.broadcastBlockChange(x, y, z, blockId, water, logAxis, mount)

  private def breakTargetBlock(dropItem: Boolean = true): Unit =
    raycast() match
      case Some(hit) =>
        val (x, y, z) = hit.block
        val block = activeBlockAt(x, y, z)
        if pendingPlayerEdits.contains((x, y, z)) || pendingPlayerEdits.size >= 128 then return
        if block == Block.Bedrock then return
        val harvest = dropItem && ToolSystem.canHarvest(block, heldHotbarBlock)
        val initial = ModBlockBreakEvent(
          modManager.api,
          playerName,
          x,
          y,
          z,
          block,
          cancelled = false,
          dropItem = harvest
        )
        val event = if isClientOnlyMultiplayer then initial else modManager.fireBlockBreak(initial)
        if !event.cancelled then
          val (loot, quantity) = modManager.drops.getOrElse(block, ToolSystem.drop(block))
          val needsDropRoom =
            event.dropItem && gameMode == GameMode.Survival && loot != Block.Air && block != Block.Water
          val stacks = Vector(loot -> quantity) ++ (if block == Block.SupplyCrate then
                                                      SupplyCaches.contents(worldSeed, x, y, z)
                                                    else Vector.empty)
          if needsDropRoom && !isClientOnlyMultiplayer && !droppedItems.canDropAll(
              stacks,
              Vec3(x + 0.5f, y + 0.5f, z + 0.5f)
            )
          then addChatMessage("Pick up nearby dropped items before mining more")
          else
            if event.dropItem && gameMode == GameMode.Survival && loot != Block.Air && block != Block.Air && block != Block.Water && !isClientOnlyMultiplayer
            then
              spawnDrop(loot, quantity, Vec3(x + 0.5f, y + 0.5f, z + 0.5f))
              if block == Block.SupplyCrate then salvageCache(playerName, x, y, z)
            if isClientOnlyMultiplayer then
              pendingPlayerEdits((x, y, z)) =
                (Block.Air, 0, event.dropItem && gameMode == GameMode.Survival)
            if block != Block.Air && block != Block.Water then
              spawnBlockBreakParticles(x, y, z, block)
            playBreakSound(block)
            broadcastActionSound(
              playerName,
              BlockboxSounds.Action.Break,
              block,
              Math.floorMod(x ^ y ^ z, 64),
              Vec3(x + 0.5f, y + 0.6f, z + 0.5f)
            )
            setActiveBlock(x, y, z, Block.Air)
            dirtyChunkAt(x, z)
            triggerSandFallAbove(x, y, z)
            viewmodelKick = 1f
            sendBlockNetwork(x, y, z, 0)
      case None => ()

  private def placeTargetBlock(skipModInteraction: Boolean = false): Unit =
    raycast() match
      case Some(hit) =>
        val (bx, by, bz) = hit.block
        val targetBlock = activeBlockAt(bx, by, bz)
        if !skipModInteraction && modManager.events.hasListeners("blockInteract") then
          if isClientOnlyMultiplayer then
            gameClient.send(s"USE|$bx|$by|$bz")
            return
          val interaction = modManager.events.fireBlockInteract(
            ModInteractionEvent(modManager.api, playerName, bx, by, bz, targetBlock)
          )
          if interaction.cancelled || interaction.handled then return
        if targetBlock == Block.Furnace || targetBlock == Block.FurnaceLit then openFurnace()
        else
          val (x, y, z) = hit.place
          if pendingPlayerEdits.contains((x, y, z)) || pendingPlayerEdits.size >= 128 then return
          val block = hotbarBlocks(selectedBlock.max(0).min(hotbarBlocks.length - 1))
          val hasBlock =
            isPlaceableInventoryBlock(block) && (gameMode == GameMode.Creative || hotbarCounts(
              selectedBlock.max(0).min(hotbarCounts.length - 1)
            ) > 0)
          if hasBlock && canPlaceBlockAt(x, y, z, block) then
            val initial =
              ModBlockPlaceEvent(modManager.api, playerName, x, y, z, block, cancelled = false)
            val event =
              if isClientOnlyMultiplayer then initial else modManager.fireBlockPlace(initial)
            if !event.cancelled then
              val finalBlock = event.block
              val mount = TorchMount.fromNormal(hit.normal)
              if canPlaceBlockAt(x, y, z, finalBlock) && (finalBlock != Block.Torch || mount.exists(
                  _.supported(x, y, z, activeBlockAt)
                ))
              then
                val placedAxis =
                  if Block.isLog(finalBlock) then LogAxis.fromNormal(hit.normal) else LogAxis.Y
                playPlaceSound(finalBlock)
                broadcastActionSound(
                  playerName,
                  BlockboxSounds.Action.Place,
                  finalBlock,
                  Math.floorMod(x ^ y ^ z, 64),
                  Vec3(x + 0.5f, y + 0.6f, z + 0.5f)
                )
                setActiveBlock(x, y, z, finalBlock, placedAxis, mount.getOrElse(TorchMount.Floor))
                if isClientOnlyMultiplayer then
                  pendingPlayerEdits((x, y, z)) =
                    (block, if gameMode == GameMode.Survival then 1 else 0, false)
                if gameMode == GameMode.Survival then consumeSelectedHotbar(1)
                dirtyChunkAt(x, z)
                if gameMode == GameMode.Survival then fieldJournal.placedBlock(finalBlock)
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
      remotePlayers.values.exists(rp =>
        glfwGetTime() - rp.lastSeen <= 8.0 && blockIntersectsPlayerBody(x, y, z, rp.pos)
      )

  private def canPlaceBlockAt(x: Int, y: Int, z: Int, block: Block = Block.Stone): Boolean =
    y >= 0 && y < Terrain.worldHeight && loadedChunkForBlock(x, z).nonEmpty && (activeBlockAt(
      x,
      y,
      z
    ) == Block.Air || activeBlockAt(
      x,
      y,
      z
    ) == Block.Water || FloraModel.contains(
      activeBlockAt(x, y, z)
    )) && (!block.solid || !placementBlockedByAnyPlayer(x, y, z)) && FloraModel.supported(
      block,
      x,
      y,
      z,
      activeBlockAt
    )

  private def renderRayLine(): Unit =
    val dir = viewDirection; val end = camera + dir * 8f
    glDisable(GL_DEPTH_TEST); glLineWidth(2f); glColor4f(0f, 1f, 0f, 0.6f)
    glBegin(GL_LINES); glVertex3f(0f, camera.y, 0f);
    glVertex3f(end.x - camera.x, end.y, end.z - camera.z); glEnd()
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
      val bounds = if activeBlockAt(x, y, z) == Block.Torch then
        TorchModel.bounds(torchMountAt(x, y, z))
      else if FloraModel.contains(activeBlockAt(x, y, z)) then
        FloraModel.bounds(activeBlockAt(x, y, z))
      else TorchModel.Bounds(0, 0, 0, 1, 1, 1)
      val e = 0.004f; val x0 = x - camera.x + bounds.x0 - e; val x1 = x - camera.x + bounds.x1 + e
      val y0 = y + bounds.y0 - e; val y1 = y + bounds.y1 + e; val z0 = z - camera.z + bounds.z0 - e;
      val z1 = z - camera.z + bounds.z1 + e
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
        (
          bx.toFloat - camera.x + (if nx > 0 then 1f + eps else -eps),
          by.toFloat + v,
          bz.toFloat - camera.z + u
        )
      else if ny != 0 then
        (
          bx.toFloat - camera.x + u,
          by.toFloat + (if ny > 0 then 1f + eps else -eps),
          bz.toFloat - camera.z + v
        )
      else
        (
          bx.toFloat - camera.x + u,
          by.toFloat + v,
          bz.toFloat - camera.z + (if nz > 0 then 1f + eps else -eps)
        )
    val centers = Array((0.50f, 0.50f), (0.31f, 0.62f), (0.67f, 0.37f))
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
      glVertex3f(p0._1, p0._2, p0._3); glVertex3f(p1._1, p1._2, p1._3)
      if stage >= 5 && (branch & 1) == 0 then
        val sideA = a + 1.1f + (branch % 4) * 0.17f
        val su = (u1 + cos(sideA).toFloat * len * 0.42f).max(0.035f).min(0.965f)
        val sv = (v1 + sin(sideA).toFloat * len * 0.42f).max(0.035f).min(0.965f)
        val p2 = surfacePoint(su, sv)
        glVertex3f(p1._1, p1._2, p1._3); glVertex3f(p2._1, p2._2, p2._3)
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
        val emptyBlocks =
          new Array[Byte](Terrain.chunkSize * Terrain.worldHeight * Terrain.chunkSize)
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
          try
            file.renameTo(
              new java.io.File(chunksDir, BlockboxSaveFiles.chunkFileName(cx, cz) + ".bad")
            )
          catch case _: Exception => ()
          chunks -= ((cx, cz))
          false
    else false

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
  def modWorldActive: Boolean = activeWorldSession
  def modWorldDirectory: java.nio.file.Path = currentWorldDir.toPath
  def modConfigDirectory: java.nio.file.Path = gameRootDir.toPath.resolve("config/blockbox-mods")
  def modShaderPacks: Vector[String] =
    Option(shaderPipeline).map(_.availablePackNames).getOrElse(Vector.empty)
  def modSelectShaderPack(name: String): Boolean =
    if shaderPipeline == null then false
    else
      if !shaderPipeline.available then shaderPipeline.initialize()
      val applied = shaderPipeline.selectPack(name)
      if applied then shadersEnabled = true
      applied
  def modShaderUniform(name: String, value: Float): Unit =
    require(shaderPipeline != null, "Shader uniforms are available after graphics initialization")
    shaderPipeline.setCustomUniform(name, value)
  def modRulesChanged(): Unit =
    publishedRules = modManager.rules.snapshot
    if gameServer != null then gameServer.broadcast("RULES|" + publishedRules)
  def modPlayerPosition(name: String): Vec3 =
    positionOfPlayer(name).getOrElse(throw IllegalArgumentException("Unknown player: " + name))
  def modTeleportPlayer(name: String, pos: Vec3): Unit =
    require(modHasAuthority, "Teleport players on the host")
    require(pos != null && Seq(pos.x, pos.y, pos.z).forall(_.isFinite), "Position must be finite")
    teleportPlayer(name, pos)
  def modGivePlayer(name: String, block: Block, count: Int): Unit =
    require(modHasAuthority, "Give inventory rewards on the host")
    require(block != null && CustomBlocks.registered(block), "Unknown item")
    if count > 0 then giveItemForPlayer(name, block, count.min(999))
  def modGetBlock(x: Int, y: Int, z: Int): Block = activeBlockAt(x, y, z)
  def modSetBlock(x: Int, y: Int, z: Int, block: Block): Unit =
    require(modHasAuthority, "World mutations must run on the host")
    val safe = if block == null then Block.Air else block
    require(CustomBlocks.registered(safe) && y > 0 && y < Terrain.worldHeight, "Invalid block edit")
    val mount = if safe == Block.Torch then
      TorchMount.values.find(_.supported(x, y, z, activeBlockAt))
    else Some(TorchMount.Floor)
    mount.foreach { attachment =>
      setActiveBlock(x, y, z, safe, LogAxis.Y, attachment)
      dirtyChunkAt(x, z)
      sendBlockNetwork(x, y, z, safe.ordinal.toByte)
    }
  def modResolveBlock(name: String): Block = modManager.resolveBlockName(name)
  def modAddChatMessage(message: String): Unit = addChatMessage(message)
  def modGiveItem(block: Block, count: Int): Unit =
    if block != null then gainItem(block, count.max(0))
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
  def modDrawRect(
      x: Float,
      y: Float,
      w: Float,
      h: Float,
      r: Float,
      g: Float,
      b: Float,
      a: Float
  ): Unit =
    glDisable(GL_DEPTH_TEST); glDisable(GL_CULL_FACE); setupOrtho(); rect(x, y, w, h, r, g, b, a)
  def modDrawText(
      x: Float,
      y: Float,
      text: String,
      r: Float,
      g: Float,
      b: Float,
      scale: Float
  ): Unit =
    glDisable(GL_DEPTH_TEST); glDisable(GL_CULL_FACE); setupOrtho();
    renderText(x, y, Option(text).getOrElse(""), r, g, b, scale.max(0.1f))
  def modDrawTextShadow(
      x: Float,
      y: Float,
      text: String,
      r: Float,
      g: Float,
      b: Float,
      scale: Float
  ): Unit =
    glDisable(GL_DEPTH_TEST); glDisable(GL_CULL_FACE); setupOrtho();
    renderTextShadow(x, y, Option(text).getOrElse(""), r, g, b, scale.max(0.1f))
  def modDrawButton(x: Float, y: Float, w: Float, h: Float, label: String): Unit =
    glDisable(GL_DEPTH_TEST); glDisable(GL_CULL_FACE); setupOrtho();
    drawButton(x, y, w, h, Option(label).getOrElse(""))
  def modInRect(mx: Float, my: Float, x: Float, y: Float, w: Float, h: Float): Boolean =
    inRect(mx, my, x, y, w, h)

  private def activeWorld: Nothing = throw IllegalStateException(
    "World no longer used - use activeBlockAt"
  )

  private def activeAtlas: TextureAtlas =
    val atlas = textureAtlas
    if atlas == null then throw IllegalStateException("Texture atlas is not initialized")
    atlas
