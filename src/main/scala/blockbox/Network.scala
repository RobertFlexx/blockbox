package blockbox

import blockbox.net.BlockboxNet
import blockbox.net.BlockboxSockets
import blockbox.net.PacketBudget
import java.util.concurrent.ConcurrentHashMap
import java.io.*
import java.net.*
import java.util.concurrent.LinkedBlockingQueue
import java.util.UUID
import scala.math.*

final class GameServer(
  port: Int,
  worldSeed: Long,
  spawn: Vec3,
  hostName: String,
  hostColorId: Int,
  worldSnapshot: () => Seq[(Int, Int, Int, Byte, Int, Int, Int)],
  opSnapshot: () => Seq[String],
  hostPosition: () => Vec3,
  hostYaw: () => Float,
  hostPitch: () => Float,
  requiredModHash: String,
  requiredModList: String,
  environmentSnapshot: () => String,
  rulesSnapshot: () => String = () => ""
):
  import GameServer.*
  // Keep this explicitly IPv4 for local/LAN testing. 0.0.0.0 accepts 127.0.0.1, LAN, and ZeroTier IPv4.
  private val initialSnapshot = worldSnapshot()
  require(initialSnapshot.size<=250000,"World exceeds the 250000-edit multiplayer snapshot limit")
  private val serverSocket = BlockboxSockets.bindIpv4Server(port)
  private val clients = collection.mutable.ListBuffer[ClientHandler]()
  private val serverEvents = new LinkedBlockingQueue[String](32768)
  private val connectionCount = new java.util.concurrent.atomic.AtomicInteger(0)
  private val worldEdits = new ConcurrentHashMap[(Int, Int, Int), (Int, Int, Int, Byte, Int, Int, Int)]()
  initialSnapshot.foreach { edit => worldEdits.put((edit._1, edit._2, edit._3), edit) }
  private val running = java.util.concurrent.atomic.AtomicBoolean(true)
  private val sessionId = UUID.randomUUID().toString
  private val acceptThread = BlockboxSockets.daemonThread("blockbox-server-accept", () =>
    try
      while running.get do
        try
          val sock = serverSocket.accept()
          BlockboxSockets.configureTcp(sock)
          sock.setSoTimeout(15000)
          if connectionCount.incrementAndGet() > BlockboxNet.MAX_PLAYERS + 8 then
            connectionCount.decrementAndGet()
            BlockboxSockets.closeQuietly(sock)
          else
            val handler = ClientHandler(sock)
            registerClient(handler)
            handler.start()
        catch case _: SocketTimeoutException => ()
    catch
      case _: SocketException => running.set(false)
      case _: IOException => running.set(false)
  )

  def localPort: Int = serverSocket.getLocalPort
  def isListening: Boolean = running.get && serverSocket.isBound && !serverSocket.isClosed

  def broadcast(msg: String): Unit =
    val recipients = clients.synchronized(clients.filter(c => c.active && c.readyForBroadcast).toList)
    recipients.foreach(_.send(msg))

  def sendTo(name: String,message: String): Unit =
    val client = clients.synchronized(clients.find(c => c.active && c.readyForBroadcast && c.onlineName.equalsIgnoreCase(name)))
    client.foreach(_.send(message))

  def broadcastPlayerList(): Unit =
    broadcast("PLAYERS|" + activePlayerTokensSnapshot.mkString("|"))

  def broadcastBlockChange(x: Int, y: Int, z: Int, blockId: Byte, waterLevel: Int = 0, logAxis: Int = 0, torchMount: Int = 0): Unit =
    worldEdits.put((x, y, z), (x, y, z, blockId, waterLevel, logAxis, torchMount))
    broadcast(blockMessage(x, y, z, blockId, waterLevel, logAxis, torchMount))

  def pollMessage(): Option[String] = Option(serverEvents.poll())

  def stop(): Unit =
    running.set(false)
    clients.synchronized(clients.foreach(_.close()))
    BlockboxSockets.closeQuietly(serverSocket)

  private def safeName(value: String): String =
    BlockboxNet.safeName(value)

  private val safeHostName = safeName(hostName)
  private val colorCount = BlockboxNet.MAX_PLAYERS
  private val safeHostColor = Math.floorMod(hostColorId, colorCount)
  acceptThread.start()

  private def playerToken(name: String, color: Int): String =
    BlockboxNet.playerToken(name, color, colorCount)

  private def parseNetworkFloat(value: String): Float = BlockboxNet.parseFloat(value)

  private def posMessage(name: String, pos: Vec3, yaw: Float, pitch: Float, colorId: Int): String =
    BlockboxNet.pos(name, pos.x, pos.y, pos.z, yaw, pitch, colorId, colorCount)

  private def blockMessage(x: Int, y: Int, z: Int, blockId: Byte, waterLevel: Int = 0, logAxis: Int = 0, torchMount: Int = 0): String =
    BlockboxNet.block(x, y, z, blockId.toInt, waterLevel, logAxis, torchMount)

  private def readProtocolLine(in: BufferedReader): String =
    BlockboxNet.readProtocolLine(in)

  private def sendCurrentPositionsTo(client: ClientHandler): Unit =
    client.send(posMessage(safeHostName, hostPosition(), hostYaw(), hostPitch(), safeHostColor))
    clients.synchronized {
      clients.filter(c => (c ne client) && c.active && c.registered && c.hasPosition).foreach { c =>
        client.send(posMessage(c.onlineName, c.lastPos, c.lastYaw, c.lastPitch, c.colorId))
      }
    }

  private def activePlayerTokensSnapshot: List[String] =
    clients.synchronized {
      (playerToken(safeHostName, safeHostColor) ::
        clients.filter(c => c.active && c.registered).map(c => playerToken(c.onlineName, c.colorId)).toList).distinct
    }

  private def assignColor(): Int =
    clients.synchronized {
      val used = (safeHostColor :: clients.filter(c => c.active && c.registered).map(_.colorId).toList).map(c => Math.floorMod(c, colorCount)).toSet
      PlayerAppearance.nextFreeColor(used).getOrElse(throw IOException("No player shirt available"))
    }

  private def isNameInUse(name: String): Boolean =
    val safe = safeName(name)
    safe.equalsIgnoreCase(safeHostName) || clients.synchronized {
      clients.exists(c => c.active && c.registered && c.onlineName.equalsIgnoreCase(safe))
    }

  private def registeredClientCount: Int =
    clients.synchronized(clients.count(c => c.active && c.registered))

  private def registerClient(client: ClientHandler): Unit =
    clients.synchronized {
      if !clients.exists(_ eq client) then clients += client
    }

  private def unregisterClient(client: ClientHandler): Unit =
    clients.synchronized {
      clients -= client
    }

  private class ClientHandler(sock: Socket) extends Thread("blockbox-client-handler"):
    setDaemon(true)
    private val out = BlockboxSockets.utf8Writer(sock)
    private val in = BlockboxSockets.utf8Reader(sock)
    private val sendLock = Object()
    private val outgoing = new LinkedBlockingQueue[Seq[String]](2048)
    private val pendingLiveBlocks = collection.mutable.ArrayBuffer.empty[String]
    private var syncing = true
    private val packetBudget = PacketBudget(100, 200, System.nanoTime())
    private val editBudget = PacketBudget(24, 48, System.nanoTime())
    private val chatBudget = PacketBudget(3, 6, System.nanoTime())
    @volatile var active = true
    @volatile var registered = false
    @volatile var readyForBroadcast = false
    @volatile var onlineName = "Player"
    @volatile var colorId = 0
    @volatile var hasPosition = false
    @volatile var lastPos: Vec3 = spawn
    @volatile var lastYaw: Float = 0f
    @volatile var lastPitch: Float = 0f
    private var badPackets = 0

    private def noteBadPacket(kind: String, error: Exception): Unit =
      badPackets += 1
      System.err.println(s"Ignored bad $kind from ${BlockboxSockets.remoteAddress(sock)}: ${Option(error.getMessage).getOrElse(error.getClass.getSimpleName)}")
      if badPackets >= BlockboxNet.BAD_PACKET_LIMIT then active = false

    private val writerThread = BlockboxSockets.daemonThread("blockbox-server-writer", () =>
      try
        while active || !outgoing.isEmpty do
          val batch = outgoing.poll(1, java.util.concurrent.TimeUnit.SECONDS)
          if batch != null then
            batch.foreach { msg => out.write(msg); out.write('\n') }
            if out.checkError() then close()
      catch case _: Exception => close()
    )

    override def run(): Unit =
      writerThread.start()
      try
        var line = readProtocolLine(in)
        while line != null && active do
          if !packetBudget.allow(System.nanoTime()) then throw IOException("Packet rate exceeded")
          val packet = BlockboxNet.packet(line)
          val parts = packet.parts()
          if packet.is(BlockboxNet.PING) then
            send("PONG|BLOCKBOX|1")
            active = false
          else if packet.is(BlockboxNet.HELO) then
            if registered then throw IOException("Repeated handshake")
            val hello = BlockboxNet.parseHello(packet)
            val requestedName = hello.playerName
            val clientModHash = hello.modHash
            if registeredClientCount >= BlockboxNet.MAX_PLAYERS - 1 then
              send(BlockboxNet.error("SERVER_FULL", "Server is full"))
              active = false
            else if !hello.modern || hello.badProtocol then
              send(BlockboxNet.error("VERSION", "Server protocol " + BlockboxNet.PROTOCOL_VERSION.toString + " does not match client protocol " + hello.clientProtocol))
              active = false
            else if clientModHash != requiredModHash then
              val list = if requiredModList.trim.nonEmpty then requiredModList else requiredModHash
              send(BlockboxNet.error("MOD_MISMATCH", "Server requires modpack " + requiredModHash + " [" + list + "]"))
              active = false
            else if isNameInUse(requestedName) then
              send(BlockboxNet.error("NAME_TAKEN", "That name is already in this world"))
              active = false
            else
              clients.synchronized {
                if registeredClientCount >= BlockboxNet.MAX_PLAYERS - 1 || isNameInUse(requestedName) then
                  throw IOException("Player slot or name is already in use")
                onlineName = requestedName
                colorId = assignColor()
                registered = true
              }
              val worldMsg = BlockboxNet.world(worldSeed, spawn.x, spawn.y, spawn.z, colorId, safeHostName, safeHostColor, sessionId)
              send(worldMsg)
              readyForBroadcast = true
              send(environmentSnapshot())
              val rules=rulesSnapshot()
              if rules.nonEmpty then send("RULES|"+rules)
              send("PLAYERS|" + activePlayerTokensSnapshot.mkString("|"))
              val opList =
                try opSnapshot().map(safeName).filter(_.nonEmpty).distinct
                catch case _: Exception => Seq.empty[String]
              send("OPLIST|" + opList.mkString("|"))
              registered = true
              registerClient(this)
              val joinMsg = s"JOIN|$onlineName|$colorId"
              if !serverEvents.offer(joinMsg) then throw IOException("Server queue full")
              broadcast(joinMsg)
              sendCurrentPositionsTo(this)
              broadcast(posMessage(onlineName, spawn, 0f, 0f, colorId))
              val snapshot =
                try
                  import scala.jdk.CollectionConverters.*
                  if worldEdits.size()>250000 then throw IOException("World exceeds multiplayer snapshot limit")
                  worldEdits.values().asScala.toVector
                catch
                  case e: Exception =>
                    send(BlockboxNet.error("SNAPSHOT",Option(e.getMessage).getOrElse("Could not synchronize world")))
                    throw e
              send(BlockboxNet.snapBegin(snapshot.length))
              var checksum = 0L
              // Initial world sync can contain hundreds of thousands of block edits.
              // Buffer a few hundred protocol lines per socket flush instead of doing
              // a syscall/flush for every block. Packet format and ordering stay the same.
              val syncBatch = collection.mutable.ArrayBuffer.empty[String]
              snapshot.foreach { case (sx, sy, sz, sid, slevel, saxis, smount) =>
                checksum = BlockboxNet.blockChecksum(checksum, sx, sy, sz, sid.toInt, slevel, saxis, smount)
                syncBatch += blockMessage(sx, sy, sz, sid, slevel, saxis, smount)
                if syncBatch.length >= 256 then
                  sendBatch(syncBatch)
                  syncBatch.clear()
              }
              if syncBatch.nonEmpty then sendBatch(syncBatch)
              sendLock.synchronized {
                enqueue(Seq(BlockboxNet.snapEnd(snapshot.length, checksum), BlockboxNet.enter(sessionId)))
                syncing = false
                if pendingLiveBlocks.nonEmpty then enqueue(pendingLiveBlocks.toVector)
                pendingLiveBlocks.clear()
              }
          else if registered && packet.is(BlockboxNet.BLOC) then
            try
              val bp = BlockboxNet.parseBlock(packet, Terrain.worldHeight)
              val blockId = bp.blockId.toByte
              val block = Block.fromId(blockId)
              val target = Vec3(bp.x + 0.5f, bp.y + 0.5f, bp.z + 0.5f)
              val validBlock = bp.blockId < Block.values.length && CustomBlocks.registered(block) && (block == Block.Air || block.solid || block == Block.Water || block == Block.Torch || CustomBlocks.isSlot(block)) && block != Block.Bedrock
              if !validBlock || bp.y == 0 || (target - lastPos).lengthSquared > 100f || !editBudget.allow(System.nanoTime()) then
                throw IOException("Invalid block edit or reach")
              val level = if blockId == Block.Water.id then (if bp.waterLevel > 0 then bp.waterLevel else 8) else 0
              val clean = blockMessage(bp.x, bp.y, bp.z, blockId, level, bp.logAxis, if block==Block.Torch then bp.torchMount else 0)
              if worldEdits.size() >= 250000 && !worldEdits.containsKey((bp.x, bp.y, bp.z)) then throw IOException("World edit limit reached")
              // The game thread validates the current world state and commits
              // the edit before it is added to snapshots or broadcast to peers.
              if !serverEvents.offer("EDIT|"+onlineName+"|"+clean.stripPrefix("BLOC|")) then throw IOException("Server queue full")
            catch case e: Exception => noteBadPacket("BLOC", e)
          else if registered && packet.is("USE") then
            try
              packet.requireExact(4)
              val bp=BlockboxNet.parseBlock(BlockboxNet.packet(s"BLOC|${packet.part(1)}|${packet.part(2)}|${packet.part(3)}|0"),Terrain.worldHeight)
              val target=Vec3(bp.x+0.5f,bp.y+0.5f,bp.z+0.5f)
              if (target-lastPos).lengthSquared>100f || !editBudget.allow(System.nanoTime()) then throw IOException("Invalid interaction reach or rate")
              if !serverEvents.offer(s"INTERACT|$onlineName|${bp.x}|${bp.y}|${bp.z}") then throw IOException("Server queue full")
            catch case e: Exception => noteBadPacket("USE",e)
          else if registered && packet.is(BlockboxNet.POS) then
            try
              val pp = BlockboxNet.parsePosition(packet, colorCount)
              lastPos = Vec3(pp.x, pp.y, pp.z)
              hasPosition = true
              lastYaw = pp.yaw; lastPitch = pp.pitch
              val clean = posMessage(onlineName, lastPos, pp.yaw, pp.pitch, colorId)
              if !serverEvents.offer(clean) then throw IOException("Server queue full")
              broadcast(clean)
            catch case e: Exception => noteBadPacket("POS", e)
          else if registered && packet.is(BlockboxNet.SWING) then
            packet.requireExact(2)
            val clean = "SWING|" + onlineName
            if editBudget.allow(System.nanoTime()) then
              if !serverEvents.offer(clean) then throw IOException("Server queue full")
              broadcast(clean)
          else if registered && line.startsWith("CMD|") then
            if !chatBudget.allow(System.nanoTime()) then throw IOException("Command rate exceeded")
            val text = if parts.length >= 2 then parts.drop(1).mkString("|") else ""
            if text.nonEmpty && !serverEvents.offer("RCMD|" + onlineName + "|" + text.take(1024)) then throw IOException("Server queue full")
          else if registered && line.startsWith("CHAT|") then
            if !chatBudget.allow(System.nanoTime()) then throw IOException("Chat rate exceeded")
            val text = if parts.length >= 3 then parts.drop(2).mkString("|") else ""
            val clean = "CHAT|" + onlineName + "|" + text.take(1024)
            if !serverEvents.offer(clean) then throw IOException("Server queue full")
            broadcast(clean)
          if active then line = readProtocolLine(in) else line = null
      catch
        case e: Exception => System.err.println(s"Blockbox server client error from ${BlockboxSockets.remoteAddress(sock)}: ${Option(e.getMessage).getOrElse(e.getClass.getSimpleName)}")
      finally
        active = false
        try writerThread.join(250) catch case _: InterruptedException => Thread.currentThread().interrupt()
        val wasRegistered = registered
        registered = false
        unregisterClient(this)
        connectionCount.decrementAndGet()
        if wasRegistered && onlineName != null && onlineName.nonEmpty then
          val leftMsg = s"LEFT|$onlineName"
          serverEvents.offer(leftMsg)
          broadcast(leftMsg)
        close()

    private def enqueue(messages: Seq[String]): Unit =
      if active && !outgoing.offer(messages) then close()

    private def sendBatch(messages: collection.Seq[String]): Unit =
      if messages.nonEmpty then enqueue(messages.toVector)

    def send(msg: String): Unit =
      sendLock.synchronized {
        if syncing && msg.startsWith("BLOC|") then
          if pendingLiveBlocks.length >= 8192 then close()
          else pendingLiveBlocks += msg
        else enqueue(Seq(msg))
      }

    def close(): Unit =
      active = false
      BlockboxSockets.closeQuietly(sock)


object GameServer:
  private val threadGroup = Thread.currentThread.getThreadGroup

  def readProtocolLine(in: BufferedReader): String =
    BlockboxNet.readProtocolLine(in)

final class GameClient(host: String, port: Int):
  @volatile private var sock: Socket = null
  private var out: PrintWriter = null
  private var in: BufferedReader = null
  private val msgQueue = new LinkedBlockingQueue[String](BlockboxNet.CLIENT_QUEUE_LIMIT)
  @volatile private var connected = false
  private var readerThread: Thread = null
  @volatile private var lastError = ""
  private val sendLock = Object()
  private val outgoing = new LinkedBlockingQueue[String](512)
  private var writerThread: Thread = null
  @volatile private var cancelled = false

  def errorMessage: String = lastError

  def connect(playerName: String, localModHash: String): Boolean =
    lastError = ""
    try
      sock = BlockboxSockets.connectTcp(host, port, BlockboxNet.CONNECT_TIMEOUT_MS)
      if cancelled then
        BlockboxSockets.closeQuietly(sock)
        return false
      // Short timeout only for the login/world handshake. After that the reader blocks normally.
      sock.setSoTimeout(BlockboxNet.JOIN_TIMEOUT_MS)
      out = BlockboxSockets.utf8Writer(sock)
      in = BlockboxSockets.utf8Reader(sock)
      if !BlockboxSockets.sendLine(out, BlockboxNet.helo(playerName, Option(localModHash).getOrElse(ModRuntime.VanillaHash))) then
        lastError = "Could not send handshake to server."
        disconnect()
        return false

      val firstLine = GameServer.readProtocolLine(in)
      if firstLine == null then
        lastError = "Server closed the connection before sending world data."
        disconnect()
        return false
      val firstPacket = BlockboxNet.packet(firstLine)
      if firstPacket.is(BlockboxNet.ERR) then
        val parts = firstPacket.parts()
        lastError = if parts.length >= 3 then parts.drop(2).map(BlockboxNet.unescape).mkString(" ") else firstLine
        disconnect()
        return false
      if !firstPacket.is(BlockboxNet.WORLD) then
        lastError = "Connected, but the server did not speak Blockbox protocol."
        disconnect()
        return false

      if !BlockboxSockets.offerLine(msgQueue, firstLine) then
        lastError = "Local network queue is full during join."
        disconnect()
        return false
      var entered = false
      val snapshot = BlockboxNet.SnapshotVerifier()
      var line = GameServer.readProtocolLine(in)
      while line != null && !entered do
        if !BlockboxSockets.offerLine(msgQueue, line) then
          lastError = "Local network queue is full during join."
          disconnect()
          return false
        val packet = BlockboxNet.packet(line)
        if packet.is(BlockboxNet.SNAPBEGIN) then
          packet.require(2)
          snapshot.begin(BlockboxNet.parseInt(packet.part(1), 0, Int.MaxValue))
        else if packet.is(BlockboxNet.BLOC) then
          snapshot.add(BlockboxNet.parseBlock(packet, Terrain.worldHeight))
        else if packet.is(BlockboxNet.SNAPEND) then
          snapshot.end(packet)
        else if packet.is(BlockboxNet.ERR) then
          val parts = packet.parts()
          lastError = parts.drop(2).map(BlockboxNet.unescape).mkString(" ")
          disconnect()
          return false
        else if packet.is(BlockboxNet.ENTER) then entered = true
        if !entered then line = GameServer.readProtocolLine(in)
      if !entered then
        lastError = "Server closed before completing the initial world sync."
        disconnect()
        return false
      if !snapshot.valid() then
        lastError = "Initial world sync checksum failed. Reconnect to retry."
        disconnect()
        return false
      if cancelled then
        disconnect()
        return false
      connected = true
      sock.setSoTimeout(15000)
      writerThread = BlockboxSockets.daemonThread("blockbox-client-writer", () =>
        try
          while connected do
            val message = outgoing.poll(1,java.util.concurrent.TimeUnit.SECONDS)
            if message != null && !BlockboxSockets.sendLine(out,message) then disconnect()
        catch case _: Exception => disconnect()
      )
      writerThread.start()
      readerThread = BlockboxSockets.daemonThread("client-reader", () =>
        try
          var line = GameServer.readProtocolLine(in)
          while line != null && connected do
            if !BlockboxSockets.offerLine(msgQueue, line) then
              lastError = "Network queue full"
              disconnect()
            else line = GameServer.readProtocolLine(in)
          connected = false
          BlockboxSockets.closeQuietly(sock)
        catch
          case _: SocketException => connected = false
          case _: IOException => connected = false
          case _: Exception => connected = false
        finally BlockboxSockets.closeQuietly(sock)
      )
      readerThread.start()
      true
    catch
      case e: SocketTimeoutException =>
        lastError = "Timed out. Check the IP, port 25565, firewall, ZeroTier/LAN route, or port forward."
        disconnect(); false
      case e: ConnectException =>
        lastError = "Connection refused. The host is reachable, but no Blockbox server is listening on that port."
        disconnect(); false
      case e: UnknownHostException =>
        lastError = "Unknown host. Check the server IP/name."
        disconnect(); false
      case e: NoRouteToHostException =>
        lastError = "No route to host. Check LAN/ZeroTier/VPN/firewall."
        disconnect(); false
      case e: IOException =>
        lastError = Option(e.getMessage).filter(_.nonEmpty).getOrElse("I/O error while connecting.")
        disconnect(); false
      case e: Exception =>
        lastError = Option(e.getMessage).filter(_.nonEmpty).getOrElse(e.getClass.getSimpleName)
        disconnect(); false

  def disconnect(): Unit =
    cancelled = true
    connected = false
    BlockboxSockets.closeQuietly(sock)
    msgQueue.clear(); outgoing.clear()

  def send(cmd: String): Unit =
    if connected && !outgoing.offer(cmd) then
      lastError = "Outgoing network queue full"
      disconnect()

  def pendingMessages: Int = msgQueue.size()

  def pollMessage(): Option[String] = Option(msgQueue.poll())

  def isConnected: Boolean = connected

object BlockboxNetworkText:
  def escape(value: String): String = BlockboxNet.escape(value)
  def unescape(value: String): String = BlockboxNet.unescape(value)
  def floatText(value: Float): String = BlockboxNet.floatText(value)
  def parseFloat(value: String): Float = BlockboxNet.parseFloat(value)
  def safeName(value: String): String = BlockboxNet.safeName(value)

  def cleanPlayerName(value: String, fallbackNumber: => Int): String =
    val cleaned = Option(value).getOrElse("").trim.filter(ch => ch.isLetterOrDigit || ch == '_' || ch == '-').take(16)
    if cleaned.nonEmpty then cleaned else s"Player$fallbackNumber"

  def normalizeColorId(id: Int, colorCount: Int): Int =
    Math.floorMod(id, colorCount.max(1))

  def fallbackColorForName(name: String, colorCount: Int): Int =
    normalizeColorId(Math.floorMod(safeName(name).hashCode, colorCount.max(1)), colorCount)

  def parsePlayerToken(token: String, colorCount: Int): Option[(String, Int)] =
    val raw = Option(token).getOrElse("").trim
    if raw.isEmpty then None
    else
      val idx = raw.lastIndexOf(':')
      if idx > 0 && idx < raw.length - 1 then
        val name = safeName(raw.substring(0, idx))
        val color = try normalizeColorId(raw.substring(idx + 1).toInt, colorCount) catch case _: Exception => fallbackColorForName(name, colorCount)
        Some((name, color))
      else
        val name = safeName(raw)
        Some((name, fallbackColorForName(name, colorCount)))

  def parseServerAddress(rawInput: String, defaultPort: Int): Either[String, (String, Int)] =
    val raw = Option(rawInput).getOrElse("").trim
    val value = if raw.isEmpty then "127.0.0.1" else raw
    if value.startsWith("[") && value.contains("]:") then
      val close = value.indexOf(']')
      val host = value.substring(1, close)
      val portText = value.substring(close + 2)
      parsePort(portText).map(port => (host, port))
    else
      val colon = value.lastIndexOf(':')
      val singleColon = colon > 0 && value.indexOf(':') == colon
      if singleColon && colon < value.length - 1 && value.substring(colon + 1).forall(_.isDigit) then
        val host = value.substring(0, colon).trim
        val portText = value.substring(colon + 1)
        if host.isEmpty then Left("Missing host before port.") else parsePort(portText).map(port => (host, port))
      else Right((value, defaultPort))

  def parsePort(text: String): Either[String, Int] =
    try
      val port = text.toInt
      if port >= 1 && port <= 65535 then Right(port) else Left("Port must be 1-65535.")
    catch case _: NumberFormatException => Left("Invalid port number.")
