package blockbox

import groovy.lang.GroovyClassLoader
import groovy.lang.Closure
import java.io.*
import java.net.*
import java.nio.charset.StandardCharsets
import java.security.MessageDigest
import java.util.jar.JarFile
import java.util.function.Consumer
import scala.collection.mutable.ArrayBuffer
import scala.math.*

trait BlockboxMod:
  def onInit(api: ModApi): Unit = ()
  def onWorldLoaded(api: ModApi): Unit = ()
  def onShutdown(api: ModApi): Unit = ()

final case class ModDescriptor(
  id: String,
  name: String,
  version: String,
  description: String,
  side: String,
  serverInteractive: Boolean,
  source: String,
  mainClass: String,
  sha256: String,
  var loaded: Boolean = false,
  var status: String = "pending"
):
  def sideLabel: String = if serverInteractive then s"$side/server" else side

final case class ModBlockBreakEvent(api: ModApi, playerName: String, x: Int, y: Int, z: Int, block: Block, var cancelled: Boolean = false, var dropItem: Boolean = true)
final case class ModBlockPlaceEvent(api: ModApi, playerName: String, x: Int, y: Int, z: Int, var block: Block, var cancelled: Boolean = false)
final case class ModWorldTickEvent(api: ModApi, dt: Float)
final case class ModHudRenderEvent(api: ModApi, gui: ModGuiApi, dt: Float)
final case class ModMouseClickEvent(api: ModApi, gui: ModGuiApi, mouseX: Float, mouseY: Float, button: Int, var consumed: Boolean = false)
final case class ModKeyEvent(api: ModApi, key: Int, screen: String, var cancelled: Boolean = false)
final case class ModChatEvent(api: ModApi, playerName: String, var message: String, var cancelled: Boolean = false)

final class ModEventBus:
  private final case class Handler[A](fn: Consumer[A], label: String, var failures: Int = 0, var disabled: Boolean = false)
  private val blockBreakHandlers = ArrayBuffer.empty[Handler[ModBlockBreakEvent]]
  private val blockPlaceHandlers = ArrayBuffer.empty[Handler[ModBlockPlaceEvent]]
  private val worldTickHandlers = ArrayBuffer.empty[Handler[ModWorldTickEvent]]
  private val hudRenderHandlers = ArrayBuffer.empty[Handler[ModHudRenderEvent]]
  private val mouseClickHandlers = ArrayBuffer.empty[Handler[ModMouseClickEvent]]
  private val keyHandlers = ArrayBuffer.empty[Handler[ModKeyEvent]]
  private val chatHandlers = ArrayBuffer.empty[Handler[ModChatEvent]]
  private def add[A](handlers: ArrayBuffer[Handler[A]], handler: Consumer[A], label: String): Unit =
    if handler != null then handlers += Handler(handler, label)
  def onBlockBreak(handler: Consumer[ModBlockBreakEvent]): Unit = add(blockBreakHandlers, handler, "blockBreak")
  def onBlockBreak(handler: Closure[?]): Unit = onBlockBreak(ModRuntime.consumer[ModBlockBreakEvent](handler))
  def onBlockPlace(handler: Consumer[ModBlockPlaceEvent]): Unit = add(blockPlaceHandlers, handler, "blockPlace")
  def onBlockPlace(handler: Closure[?]): Unit = onBlockPlace(ModRuntime.consumer[ModBlockPlaceEvent](handler))
  def onWorldTick(handler: Consumer[ModWorldTickEvent]): Unit = add(worldTickHandlers, handler, "worldTick")
  def onWorldTick(handler: Closure[?]): Unit = onWorldTick(ModRuntime.consumer[ModWorldTickEvent](handler))
  def onHudRender(handler: Consumer[ModHudRenderEvent]): Unit = add(hudRenderHandlers, handler, "hudRender")
  def onHudRender(handler: Closure[?]): Unit = onHudRender(ModRuntime.consumer[ModHudRenderEvent](handler))
  def onMouseClick(handler: Consumer[ModMouseClickEvent]): Unit = add(mouseClickHandlers, handler, "mouseClick")
  def onMouseClick(handler: Closure[?]): Unit = onMouseClick(ModRuntime.consumer[ModMouseClickEvent](handler))
  def onKeyPress(handler: Consumer[ModKeyEvent]): Unit = add(keyHandlers, handler, "keyPress")
  def onKeyPress(handler: Closure[?]): Unit = onKeyPress(ModRuntime.consumer[ModKeyEvent](handler))
  def onChat(handler: Consumer[ModChatEvent]): Unit = add(chatHandlers, handler, "chat")
  def onChat(handler: Closure[?]): Unit = onChat(ModRuntime.consumer[ModChatEvent](handler))
  private def safeRun[A](handlers: ArrayBuffer[Handler[A]], event: A): A =
    handlers.foreach { h =>
      if !h.disabled then
        try h.fn.accept(event)
        catch
          case e: Throwable =>
            h.failures += 1
            val msg = Option(e.getMessage).filter(_.nonEmpty).getOrElse(e.getClass.getSimpleName)
            System.err.println(s"Blockbox mod event '${h.label}' failed (${h.failures}/8): $msg")
            if h.failures >= 8 then
              h.disabled = true
              System.err.println(s"Blockbox disabled failing mod event handler '${h.label}' for this session.")
    }
    event
  def fireBlockBreak(event: ModBlockBreakEvent): ModBlockBreakEvent = safeRun(blockBreakHandlers, event)
  def fireBlockPlace(event: ModBlockPlaceEvent): ModBlockPlaceEvent = safeRun(blockPlaceHandlers, event)
  def fireWorldTick(event: ModWorldTickEvent): ModWorldTickEvent = safeRun(worldTickHandlers, event)
  def fireHudRender(event: ModHudRenderEvent): ModHudRenderEvent = safeRun(hudRenderHandlers, event)
  def fireMouseClick(event: ModMouseClickEvent): ModMouseClickEvent = safeRun(mouseClickHandlers, event)
  def fireKeyPress(event: ModKeyEvent): ModKeyEvent = safeRun(keyHandlers, event)
  def fireChat(event: ModChatEvent): ModChatEvent = safeRun(chatHandlers, event)

final class CommandContext(val api: ModApi, val command: String, val args: Array[String], val playerName: String, val remote: Boolean):
  def reply(message: String): Unit = api.say(Option(message).getOrElse(""))
  def player: PlayerRef = api.player(playerName)
  def arg(index: Int, defaultValue: String = ""): String = if index >= 0 && index < args.length then args(index) else defaultValue
  def intArg(index: Int, defaultValue: Int = 0): Int = scala.util.Try(arg(index).toInt).getOrElse(defaultValue)
  def floatArg(index: Int, defaultValue: Float = 0f): Float = scala.util.Try(arg(index).toFloat).getOrElse(defaultValue)
  def isOp: Boolean = api.game.modIsOpped(playerName) || api.game.modCanUseCheats
  def requireOp(): Boolean =
    if isOp then true
    else
      reply("This mod command requires cheats/op.")
      false

final class ModCommandRegistry(manager: ModManager):
  private final case class Entry(handler: Consumer[CommandContext], serverInteractive: Boolean, usage: String, description: String, var failures: Int = 0, var disabled: Boolean = false)
  private val commands = scala.collection.mutable.LinkedHashMap.empty[String, Entry]
  def register(name: String, handler: Consumer[CommandContext]): Unit = register(name, true, handler)
  def register(name: String, handler: Closure[?]): Unit = register(name, true, ModRuntime.consumer[CommandContext](handler))
  def registerClient(name: String, handler: Consumer[CommandContext]): Unit = register(name, false, handler)
  def registerClient(name: String, handler: Closure[?]): Unit = register(name, false, ModRuntime.consumer[CommandContext](handler))
  def register(name: String, serverInteractive: Boolean, handler: Closure[?]): Unit = register(name, serverInteractive, ModRuntime.consumer[CommandContext](handler))
  def register(name: String, serverInteractive: Boolean, handler: Consumer[CommandContext]): Unit = register(name, serverInteractive, "/" + Option(name).getOrElse("").trim.stripPrefix("/"), "mod command", handler)
  def register(name: String, serverInteractive: Boolean, usage: String, description: String, handler: Closure[?]): Unit = register(name, serverInteractive, usage, description, ModRuntime.consumer[CommandContext](handler))
  def register(name: String, serverInteractive: Boolean, usage: String, description: String, handler: Consumer[CommandContext]): Unit =
    val clean = Option(name).getOrElse("").trim.toLowerCase.stripPrefix("/")
    if clean.nonEmpty && handler != null then commands(clean) = Entry(handler, serverInteractive, Option(usage).getOrElse("/" + clean), Option(description).getOrElse("mod command"))
  def has(name: String): Boolean = commands.contains(Option(name).getOrElse("").trim.toLowerCase.stripPrefix("/"))
  def isServerInteractive(name: String): Boolean = commands.get(Option(name).getOrElse("").trim.toLowerCase.stripPrefix("/")).exists(_.serverInteractive)
  def names: Seq[String] = commands.keys.toSeq
  def help: Seq[String] = commands.toSeq.map { case (name, e) => s"/${name} - ${e.description}" }
  def execute(name: String, args: Array[String], playerName: String, remote: Boolean): Boolean =
    commands.get(Option(name).getOrElse("").trim.toLowerCase.stripPrefix("/")) match
      case Some(entry) if entry.disabled =>
        manager.api.say(s"Mod command /$name is disabled after repeated errors.")
        true
      case Some(entry) =>
        try entry.handler.accept(CommandContext(manager.api, name, args, playerName, remote))
        catch
          case e: Throwable =>
            entry.failures += 1
            val msg = Option(e.getMessage).filter(_.nonEmpty).getOrElse(e.getClass.getSimpleName)
            manager.api.say("Mod command failed: " + msg)
            System.err.println(s"Blockbox mod command '/$name' failed (${entry.failures}/5): $msg")
            if entry.failures >= 5 then
              entry.disabled = true
              manager.api.say(s"Disabled failing mod command /$name for this session.")
        true
      case None => false

final class PlayerRef(private val api0: ModApi, val name: String):
  def sendMessage(message: String): Unit = api0.say("[mod] " + Option(message).getOrElse(""))
  def give(block: Block, count: Int): Unit = api0.game.modGiveItem(block, count)
  def give(blockName: String, count: Int): Unit = give(api0.block(blockName), count)
  def give(block: Object, count: Int): Unit =
    block match
      case b: Block => give(b, count)
      case s: String => give(s, count)
      case _ => sendMessage("cannot give unknown block value: " + Option(block).fold("null")(_.toString))
  def x: Float = api0.game.modCamera.x
  def y: Float = api0.game.modCamera.y
  def z: Float = api0.game.modCamera.z
  def blockX: Int = floor(x).toInt
  def blockY: Int = floor(y).toInt
  def blockZ: Int = floor(z).toInt
  def position: Vec3 = Vec3(x, y, z)
  def teleport(x: Float, y: Float, z: Float): Unit = api0.game.modTeleportSelf(Vec3(x, y, z))
  def teleport(x: Number, y: Number, z: Number): Unit = teleport(x.floatValue(), y.floatValue(), z.floatValue())
  def health: Float = api0.game.modHealth
  def setHealth(value: Number): Unit = api0.game.modSetHealth(if value == null then 20f else value.floatValue())
  def food: Float = api0.game.modFood
  def setFood(value: Number): Unit = api0.game.modSetFood(if value == null then 20f else value.floatValue())
  def isOp: Boolean = api0.game.modIsOpped(name)

final class WorldApi(private[blockbox] val game: Blockbox):
  private def i(value: Number): Int = if value == null then 0 else value.intValue()
  def getBlock(x: Int, y: Int, z: Int): Block = game.modGetBlock(x, y, z)
  def getBlock(x: Number, y: Number, z: Number): Block = getBlock(i(x), i(y), i(z))
  def setBlock(x: Int, y: Int, z: Int, block: Block): Unit = game.modSetBlock(x, y, z, block)
  def setBlock(x: Int, y: Int, z: Int, blockName: String): Unit = setBlock(x, y, z, game.modResolveBlock(blockName))
  def setBlock(x: Number, y: Number, z: Number, block: Block): Unit = setBlock(i(x), i(y), i(z), block)
  def setBlock(x: Number, y: Number, z: Number, blockName: String): Unit = setBlock(i(x), i(y), i(z), blockName)
  def isAir(x: Int, y: Int, z: Int): Boolean = getBlock(x, y, z) == Block.Air
  def isSolid(x: Int, y: Int, z: Int): Boolean = getBlock(x, y, z).solid
  def fill(x1: Int, y1: Int, z1: Int, x2: Int, y2: Int, z2: Int, block: Block, limit: Int = 4096): Int =
    val minX = x1.min(x2); val maxX = x1.max(x2)
    val minY = y1.min(y2).max(0); val maxY = y1.max(y2).min(Terrain.worldHeight - 1)
    val minZ = z1.min(z2); val maxZ = z1.max(z2)
    var changed = 0
    var x = minX
    while x <= maxX && changed < limit do
      var y = minY
      while y <= maxY && changed < limit do
        var z = minZ
        while z <= maxZ && changed < limit do
          setBlock(x, y, z, block)
          changed += 1
          z += 1
        y += 1
      x += 1
    changed
  def fill(x1: Number, y1: Number, z1: Number, x2: Number, y2: Number, z2: Number, blockName: String): Int = fill(i(x1), i(y1), i(z1), i(x2), i(y2), i(z2), game.modResolveBlock(blockName))
  def seed: Long = game.modWorldSeed
  def name: String = game.modWorldName

final class ModGuiApi(private[blockbox] val game: Blockbox):
  private def f(value: Number): Float = if value == null then 0f else value.floatValue()
  def width: Float = game.modFramebufferWidth
  def height: Float = game.modFramebufferHeight
  def scale: Float = game.modUiScale
  def mouseX: Float = game.modMouseX
  def mouseY: Float = game.modMouseY
  def leftDown: Boolean = game.modLeftDown
  def leftClicked: Boolean = game.modLeftClicked
  def cursorMode: Boolean = game.modCursorMode
  def setCursorMode(enabled: Boolean): Unit = game.modSetCursorMode(enabled)
  def toggleCursorMode(): Unit = game.modSetCursorMode(!game.modCursorMode)
  def isPlaying: Boolean = game.modScreenName == "playing"
  def rect(x: Float, y: Float, w: Float, h: Float, r: Float, g: Float, b: Float, a: Float): Unit = game.modDrawRect(x, y, w, h, r, g, b, a)
  def rect(x: Number, y: Number, w: Number, h: Number, r: Number, g: Number, b: Number, a: Number): Unit = rect(f(x), f(y), f(w), f(h), f(r), f(g), f(b), f(a))
  def text(x: Float, y: Float, text: String, r: Float, g: Float, b: Float, scale: Float): Unit = game.modDrawText(x, y, Option(text).getOrElse(""), r, g, b, scale)
  def text(x: Number, y: Number, text: String, r: Number, g: Number, b: Number, scale: Number): Unit = this.text(f(x), f(y), text, f(r), f(g), f(b), f(scale))
  def textShadow(x: Float, y: Float, text: String, r: Float, g: Float, b: Float, scale: Float): Unit = game.modDrawTextShadow(x, y, Option(text).getOrElse(""), r, g, b, scale)
  def textShadow(x: Number, y: Number, text: String, r: Number, g: Number, b: Number, scale: Number): Unit = textShadow(f(x), f(y), text, f(r), f(g), f(b), f(scale))
  def button(x: Float, y: Float, w: Float, h: Float, label: String): Unit = game.modDrawButton(x, y, w, h, Option(label).getOrElse(""))
  def button(x: Number, y: Number, w: Number, h: Number, label: String): Unit = button(f(x), f(y), f(w), f(h), label)
  def buttonClicked(x: Number, y: Number, w: Number, h: Number, label: String): Boolean =
    val fx = f(x); val fy = f(y); val fw = f(w); val fh = f(h)
    button(fx, fy, fw, fh, label)
    cursorMode && leftClicked && inRect(mouseX, mouseY, fx, fy, fw, fh)
  def panel(x: Number, y: Number, w: Number, h: Number, title: String): Unit =
    val fx = f(x); val fy = f(y); val fw = f(w); val fh = f(h)
    rect(fx, fy, fw, fh, 0.02f, 0.025f, 0.04f, 0.78f)
    rect(fx, fy, fw, 24f * scale, 0.08f, 0.10f, 0.16f, 0.92f)
    textShadow(fx + 10f * scale, fy + 7f * scale, Option(title).getOrElse("mod panel"), 0.85f, 0.93f, 1f, 0.58f * scale)
  def inRect(mx: Float, my: Float, x: Float, y: Float, w: Float, h: Float): Boolean = game.modInRect(mx, my, x, y, w, h)
  def inRect(mx: Number, my: Number, x: Number, y: Number, w: Number, h: Number): Boolean = inRect(f(mx), f(my), f(x), f(y), f(w), f(h))

final case class ModBlockDef(id: String, name: String, description: String, backing: Block, serverInteractive: Boolean = true)

final class ModContentRegistry(private val manager: ModManager):
  def registerBlock(id: String, backing: Block): ModBlockDef = registerBlock(id, id, "", backing)
  def registerBlock(id: String, backingName: String): ModBlockDef = registerBlock(id, id, "", manager.resolveBlockName(backingName))
  def registerBlock(id: String, name: String, description: String, backing: Block): ModBlockDef =
    val defn = ModBlockDef(ModRuntime.normalizeId(id), Option(name).filter(_.nonEmpty).getOrElse(id), Option(description).getOrElse(""), if backing == null then Block.Air else backing)
    manager.registerBlockDefinition(defn)
    defn
  def registerBlock(id: String, name: String, description: String, backingName: String): ModBlockDef = registerBlock(id, name, description, manager.resolveBlockName(backingName))
  def blocks: Seq[ModBlockDef] = manager.registeredBlocks
  def blockIds: Seq[String] = blocks.map(_.id)

final class ModFilesApi:
  private def safeId(id: String): String = ModRuntime.normalizeId(id)
  def modsDir: File = File("mods")
  def configDir(modId: String): File =
    val d = File(File("config"), "blockbox-mods/" + safeId(modId))
    d.mkdirs(); d
  def dataDir(modId: String): File =
    val d = File(File("worlds/moddata"), safeId(modId))
    d.mkdirs(); d
  def readText(file: File, defaultValue: String = ""): String =
    try if file != null && file.isFile then String(java.nio.file.Files.readAllBytes(file.toPath), StandardCharsets.UTF_8) else defaultValue
    catch case _: Throwable => defaultValue
  def writeText(file: File, text: String): Unit =
    if file != null then
      val parent = file.getParentFile
      if parent != null then parent.mkdirs()
      java.nio.file.Files.writeString(file.toPath, Option(text).getOrElse(""), StandardCharsets.UTF_8)

final class ModTask(private val cancelImpl: () => Unit):
  def cancel(): Unit = cancelImpl()

final class ModScheduler:
  private final case class Task(var remaining: Float, interval: Float, repeats: Boolean, action: Runnable, var cancelled: Boolean = false)
  private val tasks = ArrayBuffer.empty[Task]
  private def f(n: Number): Float = if n == null then 0f else n.floatValue().max(0f)
  def runLater(seconds: Number, action: Runnable): ModTask =
    val t = Task(f(seconds), 0f, false, action)
    if action != null then tasks += t
    ModTask(() => t.cancelled = true)
  def runLater(seconds: Number, action: Closure[?]): ModTask = runLater(seconds, ModRuntime.runnable(action))
  def every(seconds: Number, action: Runnable): ModTask =
    val interval = f(seconds).max(0.01f)
    val t = Task(interval, interval, true, action)
    if action != null then tasks += t
    ModTask(() => t.cancelled = true)
  def every(seconds: Number, action: Closure[?]): ModTask = every(seconds, ModRuntime.runnable(action))
  private[blockbox] def tick(dt: Float): Unit =
    tasks.foreach { t =>
      if !t.cancelled then
        t.remaining -= dt
        if t.remaining <= 0f then
          try t.action.run() catch case e: Throwable => System.err.println("Blockbox scheduled mod task failed: " + Option(e.getMessage).getOrElse(e.getClass.getSimpleName))
          if t.repeats then t.remaining += t.interval else t.cancelled = true
    }
    tasks.filterInPlace(!_.cancelled)

final class ModLogger(private val prefix: String):
  private def line(level: String, message: String): Unit = System.out.println(s"[Blockbox/$level/$prefix] " + Option(message).getOrElse(""))
  def info(message: String): Unit = line("INFO", message)
  def warn(message: String): Unit = line("WARN", message)
  def error(message: String): Unit = line("ERROR", message)

final class ModApi(private[blockbox] val game: Blockbox, private val manager: ModManager):
  val world = WorldApi(game)
  def gui: ModGuiApi = manager.gui
  def events: ModEventBus = manager.events
  def commands: ModCommandRegistry = manager.commands
  def content: ModContentRegistry = manager.content
  def files: ModFilesApi = manager.files
  def scheduler: ModScheduler = manager.scheduler
  def log: ModLogger = manager.log
  def block(name: String): Block = manager.resolveBlockName(name)
  def blocks: Array[Block] = Block.all
  def blockNames: Array[String] = Block.names
  def registeredBlocks: Seq[ModBlockDef] = manager.registeredBlocks
  def player(name: String): PlayerRef = PlayerRef(this, name)
  def localPlayer: PlayerRef = PlayerRef(this, game.modLocalPlayerName)
  def say(message: String): Unit = game.modAddChatMessage(Option(message).getOrElse(""))
  def loadedMods: Seq[ModDescriptor] = manager.loadedMods
  def serverModpackHash: String = manager.serverModpackHash
  def apiVersion: String = "v51"

object ModJson:
  private def unescape(s: String): String = s.replace("\\\"", "\"").replace("\\n", "\n").replace("\\r", "\r").replace("\\\\", "\\")
  def string(json: String, key: String): Option[String] =
    val r = ("\\\"" + java.util.regex.Pattern.quote(key) + "\\\"\\s*:\\s*\\\"((?:\\\\.|[^\\\"])*)\\\"").r
    r.findFirstMatchIn(json).map(m => unescape(m.group(1)))
  def bool(json: String, key: String): Option[Boolean] =
    val r = ("\\\"" + java.util.regex.Pattern.quote(key) + "\\\"\\s*:\\s*(true|false)").r
    r.findFirstMatchIn(json).map(m => m.group(1).equalsIgnoreCase("true"))

object ModRuntime:
  val VanillaHash = "vanilla"
  def sha256Bytes(bytes: Array[Byte]): String =
    val md = MessageDigest.getInstance("SHA-256")
    md.digest(bytes).map(b => f"${b & 0xff}%02x").mkString
  def sha256File(file: File): String =
    val md = MessageDigest.getInstance("SHA-256")
    val in = BufferedInputStream(FileInputStream(file))
    val buf = new Array[Byte](8192)
    try
      var n = in.read(buf)
      while n >= 0 do
        if n > 0 then md.update(buf, 0, n)
        n = in.read(buf)
    finally in.close()
    md.digest().map(b => f"${b & 0xff}%02x").mkString
  def normalizeId(raw: String): String =
    val clean = Option(raw).getOrElse("mod").trim.toLowerCase.filter(ch => ch.isLetterOrDigit || ch == '_' || ch == '-' || ch == '.')
    if clean.nonEmpty then clean.take(64) else "mod"
  def normalizeSide(raw: String): String = Option(raw).getOrElse("both").trim.toLowerCase match
    case "client" | "client-only" => "client"
    case "server" | "server-only" => "server"
    case _ => "both"
  def consumer[A](closure: Closure[?]): Consumer[A] = new Consumer[A]:
    override def accept(value: A): Unit =
      if closure != null then
        if closure.getMaximumNumberOfParameters == 0 then closure.call()
        else closure.call(value.asInstanceOf[Object])
  def runnable(closure: Closure[?]): Runnable = new Runnable:
    override def run(): Unit = if closure != null then closure.call()

final class ModManager(private val game: Blockbox):
  val events = ModEventBus()
  val commands = ModCommandRegistry(this)
  val gui = ModGuiApi(game)
  val files = ModFilesApi()
  val scheduler = ModScheduler()
  val content = ModContentRegistry(this)
  val log = ModLogger("mods")
  val api = ModApi(game, this)
  private val descriptors = ArrayBuffer.empty[ModDescriptor]
  private val liveMods = ArrayBuffer.empty[BlockboxMod]
  private val blockDefinitions = scala.collection.mutable.LinkedHashMap.empty[String, ModBlockDef]
  private var loaded = false
  private var serverHashCached = ModRuntime.VanillaHash
  private var serverListCached = ""
  def loadedMods: Seq[ModDescriptor] = descriptors.toSeq
  def serverModpackHash: String = serverHashCached
  def serverModpackList: String = serverListCached
  def clientJoinHash: String = serverHashCached
  def registerBlockDefinition(defn: ModBlockDef): Unit =
    if defn != null && defn.id.nonEmpty then blockDefinitions(defn.id) = defn
  def registeredBlocks: Seq[ModBlockDef] = blockDefinitions.values.toSeq
  def resolveBlockName(name: String): Block =
    val raw = Option(name).getOrElse("")
    val key = ModRuntime.normalizeId(raw.stripPrefix("blockbox:"))
    blockDefinitions.get(key).map(_.backing).orElse(Block.find(raw)).getOrElse(Block.Air)
  def loadAll(): Unit =
    if loaded then return
    loaded = true
    val root = File("mods")
    root.mkdirs()
    writeTemplate(root)
    val entries = Option(root.listFiles()).getOrElse(Array.empty[File]).sortBy(_.getName.toLowerCase)
    entries.foreach { f =>
      try
        if f.isFile && f.getName.toLowerCase.endsWith(".jar") then loadJar(f)
        else if f.isFile && f.getName.toLowerCase.endsWith(".groovy") then loadGroovyFile(f, None)
        else if f.isDirectory then loadFolder(f)
      catch
        case e: Throwable =>
          val id = ModRuntime.normalizeId(f.getName.stripSuffix(".jar").stripSuffix(".groovy"))
          descriptors += ModDescriptor(id, id, "unknown", "failed to scan", "both", true, f.getPath, "", hashPath(f), loaded = false, status = "scan failed: " + Option(e.getMessage).getOrElse(e.getClass.getSimpleName))
    }
    recomputeServerHash()
    System.out.println(s"Blockbox: loaded ${descriptors.count(_.loaded)} mods, server modpack ${serverHashCached}")
  def shutdown(): Unit = liveMods.foreach { m => try m.onShutdown(api) catch case e: Throwable => System.err.println("Mod shutdown failed: " + e.getMessage) }
  def fireWorldLoaded(): Unit = liveMods.foreach { m => try m.onWorldLoaded(api) catch case e: Throwable => System.err.println("Mod world-load failed: " + e.getMessage) }
  def fireWorldTick(dt: Float): Unit =
    scheduler.tick(dt)
    events.fireWorldTick(ModWorldTickEvent(api, dt))
  def fireHudRender(dt: Float): Unit = events.fireHudRender(ModHudRenderEvent(api, gui, dt))
  def fireMouseClick(mx: Float, my: Float, button: Int): ModMouseClickEvent = events.fireMouseClick(ModMouseClickEvent(api, gui, mx, my, button))
  def fireKeyPress(e: ModKeyEvent): ModKeyEvent = events.fireKeyPress(e)
  def fireChat(e: ModChatEvent): ModChatEvent = events.fireChat(e)
  def fireBlockBreak(e: ModBlockBreakEvent): ModBlockBreakEvent = events.fireBlockBreak(e)
  def fireBlockPlace(e: ModBlockPlaceEvent): ModBlockPlaceEvent = events.fireBlockPlace(e)
  def hasCommand(name: String): Boolean = commands.has(name)
  def commandIsServerInteractive(name: String): Boolean = commands.isServerInteractive(name)
  def executeCommand(name: String, args: Array[String], playerName: String, remote: Boolean): Boolean = commands.execute(name, args, playerName, remote)
  def commandNames: Seq[String] = commands.names
  private def hashPath(f: File): String =
    if f.isFile then ModRuntime.sha256File(f)
    else
      val bytes = Option(f.listFiles()).getOrElse(Array.empty[File]).sortBy(_.getName).flatMap { c =>
        if c.isFile then (c.getName + ":" + ModRuntime.sha256File(c)).getBytes(StandardCharsets.UTF_8) else Array.emptyByteArray
      }
      ModRuntime.sha256Bytes(bytes)
  private def readAll(in: InputStream): String =
    try String(in.readAllBytes(), StandardCharsets.UTF_8) finally in.close()
  private def descriptorFromJson(json: String, source: String, fallbackId: String, fallbackHash: String): ModDescriptor =
    val id = ModRuntime.normalizeId(ModJson.string(json, "id").getOrElse(fallbackId))
    val name = ModJson.string(json, "name").getOrElse(id)
    val version = ModJson.string(json, "version").getOrElse("1.0.0")
    val description = ModJson.string(json, "description").getOrElse("No description provided.")
    val side = ModRuntime.normalizeSide(ModJson.string(json, "side").getOrElse("both"))
    val serverInteractive = ModJson.bool(json, "serverInteractive").getOrElse(side != "client")
    val main = ModJson.string(json, "main").getOrElse("")
    ModDescriptor(id, name, version, description, side, serverInteractive, source, main, fallbackHash)
  private def loadJar(jarFile: File): Unit =
    val hash = ModRuntime.sha256File(jarFile)
    val jf = JarFile(jarFile)
    val json = try
      val entry = Option(jf.getEntry("blockbox.mod.json")).orElse(Option(jf.getEntry("mod.json")))
      entry.map(e => readAll(jf.getInputStream(e))).getOrElse("{}")
    finally jf.close()
    val fallback = jarFile.getName.stripSuffix(".jar")
    val desc = descriptorFromJson(json, jarFile.getPath, fallback, hash)
    val main = desc.mainClass.trim.stripPrefix("/")
    if main.isEmpty then
      desc.loaded = false; desc.status = "metadata read, but no main class"
      descriptors += desc
    else if main.toLowerCase.endsWith(".groovy") then
      loadGroovyFromJar(jarFile, desc, main)
      descriptors += desc
    else
      val loader = URLClassLoader(Array(jarFile.toURI.toURL), getClass.getClassLoader)
      instantiateMod(desc, Class.forName(main, true, loader))
      descriptors += desc

  private def loadGroovyFromJar(jarFile: File, desc: ModDescriptor, main: String): Unit =
    val jf = JarFile(jarFile)
    try
      val clean = main.stripPrefix("/")
      val candidates = Seq(clean, s"scripts/$clean", s"groovy/$clean", clean.split('/').lastOption.getOrElse(clean)).distinct
      val entry = candidates.view.flatMap(name => Option(jf.getEntry(name))).headOption
      entry match
        case Some(e) =>
          val source = readAll(jf.getInputStream(e))
          val loader = GroovyClassLoader(getClass.getClassLoader)
          instantiateMod(desc, loader.parseClass(source, e.getName))
        case None =>
          desc.loaded = false
          desc.status = "main groovy script not found in jar: " + clean
    catch
      case e: Throwable =>
        desc.loaded = false
        val msg = Option(e.getMessage).getOrElse(e.getClass.getSimpleName)
        val hint = if msg.contains("Unsupported class file major version") then " (Groovy/JVM classfile mismatch; Blockbox requires Groovy 5+ on Java 25)" else ""
        desc.status = "jar groovy load failed: " + msg + hint
    finally jf.close()
  private def loadFolder(dir: File): Unit =
    val jsonFile = File(dir, "blockbox.mod.json")
    val altJson = File(dir, "mod.json")
    val metaFile = if jsonFile.isFile then jsonFile else altJson
    if metaFile.isFile then
      val json = String(java.nio.file.Files.readAllBytes(metaFile.toPath), StandardCharsets.UTF_8)
      val main = ModJson.string(json, "main").getOrElse("main.groovy")
      loadGroovyFile(File(dir, main), Some((json, dir.getPath, hashPath(dir))))
  private def loadGroovyFile(file: File, meta: Option[(String, String, String)]): Unit =
    if !file.isFile then return
    val hash = meta.map(_._3).getOrElse(ModRuntime.sha256File(file))
    val fallback = file.getName.stripSuffix(".groovy")
    val desc = meta.map(m => descriptorFromJson(m._1, m._2, fallback, hash)).getOrElse {
      val text = String(java.nio.file.Files.readAllBytes(file.toPath), StandardCharsets.UTF_8)
      val side = if text.contains("blockbox-side: client") then "client" else "both"
      ModDescriptor(ModRuntime.normalizeId(fallback), fallback, "1.0.0", "Loose Groovy script mod", side, side != "client", file.getPath, file.getName, hash)
    }
    try
      val loader = GroovyClassLoader(getClass.getClassLoader)
      instantiateMod(desc, loader.parseClass(file))
    catch
      case e: Throwable =>
        desc.loaded = false
        val msg = Option(e.getMessage).getOrElse(e.getClass.getSimpleName)
        val hint = if msg.contains("Unsupported class file major version") then " (Groovy/JVM classfile mismatch; Blockbox requires Groovy 5+ on Java 25)" else ""
        desc.status = "loose groovy load failed: " + msg + hint
    descriptors += desc
  private def instantiateMod(desc: ModDescriptor, cls: Class[?]): Unit =
    val obj = cls.getDeclaredConstructor().newInstance()
    obj match
      case mod: BlockboxMod =>
        mod.onInit(api); liveMods += mod; desc.loaded = true; desc.status = "loaded"
      case other =>
        cls.getMethods.filter(_.getName == "init").find(_.getParameterCount == 1) match
          case Some(m) => m.invoke(other, api); desc.loaded = true; desc.status = "loaded script init(api)"
          case None => desc.loaded = false; desc.status = "no BlockboxMod implementation or init(api) method"
  private def recomputeServerHash(): Unit =
    val required = descriptors.filter(d => d.loaded && d.serverInteractive).sortBy(d => (d.id, d.version))
    serverListCached = required.map(d => s"${d.id}@${d.version}:${d.sha256.take(12)}").mkString(",")
    serverHashCached = if serverListCached.isEmpty then ModRuntime.VanillaHash else ModRuntime.sha256Bytes(serverListCached.getBytes(StandardCharsets.UTF_8)).take(24)
  private def writeTemplate(root: File): Unit =
    val docs = File(root, "README_modding.txt")
    if !docs.exists() then
      val text = """blockbox mods folder

Blockbox mods are trusted full-access JVM code, like Minecraft Java mods. Only install mods you trust.
Groovy is the primary scripting style; jar mods can be Java, Kotlin, Scala, Groovy, Clojure, or any JVM language.

preferred Groovy folder mod:
  mods/coolmod/
    blockbox.mod.json
    main.groovy
    assets/
    config/

blockbox.mod.json:
  {
    "id": "coolmod",
    "name": "Cool Mod",
    "version": "1.0.0",
    "description": "what this mod does",
    "side": "both",
    "serverInteractive": true,
    "main": "main.groovy"
  }

side values:
  client = gui/visual/client-only, does not block joining
  server = gameplay/server authority, must match host
  both = client + server mod

serverInteractive true means players must have the same server modpack hash to join a host.

main.groovy example:
  def init(api) {
    api.log.info("loaded")

    api.commands.register("hello", false, "/hello", "client-safe greeting") { ctx ->
      ctx.reply("hello from " + api.apiVersion)
    }

    api.commands.register("stonebox", true, "/stonebox", "server-side world edit demo") { ctx ->
      if (!ctx.requireOp()) return
      def p = ctx.player
      api.world.fill(p.blockX - 1, p.blockY - 2, p.blockZ - 1, p.blockX + 1, p.blockY - 2, p.blockZ + 1, "stone")
    }

    api.events.onBlockBreak { e ->
      e.playerName
    }

    api.events.onHudRender { e ->
      def g = e.gui
      if (g.cursorMode && g.buttonClicked(20, 90, 180, 28, "mod button")) {
        api.localPlayer.sendMessage("clicked")
      }
    }
  }

useful api:
  api.block("stone")
  api.content.registerBlock("coolmod:my_block", "My Block", "uses rainbow backing for now", "RainbowBlock")
  api.localPlayer.give("stone", 16)
  api.world.setBlock(x, y, z, "stone")
  api.scheduler.runLater(1.0) { api.say("one second later") }
  api.scheduler.every(5.0) { api.say("every five seconds") }
  api.files.configDir("coolmod")
  api.gui.buttonClicked(x, y, w, h, "label")

jar mods:
  put coolmod.jar in mods/. The jar should contain blockbox.mod.json at jar root.
  If main ends with .groovy, Blockbox loads that Groovy script from inside the jar.
  Otherwise main should be a JVM class that implements blockbox.BlockboxMod or has init(api).

Press F8 in-game to unlock the mouse cursor for clickable mod UI.
"""
      java.nio.file.Files.writeString(docs.toPath, text, StandardCharsets.UTF_8)
