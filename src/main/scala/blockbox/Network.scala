package blockbox

import blockbox.net.BlockboxNet
import blockbox.net.BlockboxSockets
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
  onBlockChange: (Int, Int, Int, Byte) => Unit,
  worldSnapshot: () => Seq[(Int, Int, Int, Byte)],
  opSnapshot: () => Seq[String],
  hostPosition: () => Vec3,
  hostYaw: () => Float,
  hostPitch: () => Float,
  requiredModHash: String,
  requiredModList: String
):
  import GameServer.*
  // Keep this explicitly IPv4 for local/LAN testing. 0.0.0.0 accepts 127.0.0.1, LAN, and ZeroTier IPv4.
  private val serverSocket = BlockboxSockets.bindIpv4Server(port)
  private val clients = collection.mutable.ListBuffer[ClientHandler]()
  private val serverEvents = new LinkedBlockingQueue[String]()
  private val running = java.util.concurrent.atomic.AtomicBoolean(true)
  private val sessionId = UUID.randomUUID().toString
  private val acceptThread = BlockboxSockets.daemonThread("blockbox-server-accept", () =>
    try
      while running.get do
        try
          val sock = serverSocket.accept()
          BlockboxSockets.configureTcp(sock)
          sock.setSoTimeout(0)
          val handler = ClientHandler(sock)
          handler.start()
        catch case _: SocketTimeoutException => ()
    catch
      case _: SocketException => running.set(false)
      case _: IOException => running.set(false)
  )
  acceptThread.start()

  def localPort: Int = serverSocket.getLocalPort
  def isListening: Boolean = running.get && serverSocket.isBound && !serverSocket.isClosed

  def broadcast(msg: String): Unit =
    clients.synchronized(clients.filter(_.active).foreach(_.send(msg)))

  def broadcastPlayerList(): Unit =
    broadcast("PLAYERS|" + activePlayerTokensSnapshot.mkString("|"))

  def broadcastBlockChange(x: Int, y: Int, z: Int, blockId: Byte, waterLevel: Int = 0): Unit =
    broadcast(blockMessage(x, y, z, blockId, waterLevel))

  def pollMessage(): Option[String] = Option(serverEvents.poll())

  def stop(): Unit =
    running.set(false)
    clients.synchronized(clients.foreach(_.close()))
    BlockboxSockets.closeQuietly(serverSocket)

  private def safeName(value: String): String =
    BlockboxNet.safeName(value)

  private val safeHostName = safeName(hostName)
  private val colorCount = 10
  private val safeHostColor = Math.floorMod(hostColorId, colorCount)

  private def playerToken(name: String, color: Int): String =
    BlockboxNet.playerToken(name, color, colorCount)

  private def parseNetworkFloat(value: String): Float = BlockboxNet.parseFloat(value)

  private def posMessage(name: String, pos: Vec3, yaw: Float, pitch: Float, colorId: Int): String =
    BlockboxNet.pos(name, pos.x, pos.y, pos.z, yaw, pitch, colorId, colorCount)

  private def blockMessage(x: Int, y: Int, z: Int, blockId: Byte, waterLevel: Int = 0): String =
    BlockboxNet.block(x, y, z, blockId.toInt, waterLevel)

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
      (0 until colorCount).find(c => !used.contains(c)).getOrElse((used.size + 1) % colorCount)
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

  private def blockIntersectsPlayer(wx: Int, wy: Int, wz: Int, pos: Vec3): Boolean =
    val half = 0.38f
    val minX = floor(pos.x - half).toInt; val maxX = floor(pos.x + half).toInt
    val minY = floor(pos.y - 1.70f).toInt; val maxY = floor(pos.y + 0.22f).toInt
    val minZ = floor(pos.z - half).toInt; val maxZ = floor(pos.z + half).toInt
    wx >= minX && wx <= maxX && wy >= minY && wy <= maxY && wz >= minZ && wz <= maxZ

  private def canAcceptBlockChange(wx: Int, wy: Int, wz: Int, blockId: Byte): Boolean =
    val id = blockId.toInt & 0xFF
    if id == 0 then true
    else
      val hostBlocked = blockIntersectsPlayer(wx, wy, wz, hostPosition())
      if hostBlocked then false
      else
        clients.synchronized {
          !clients.exists(c => c.active && c.registered && c.hasPosition && blockIntersectsPlayer(wx, wy, wz, c.lastPos))
        }

  private class ClientHandler(sock: Socket) extends Thread("blockbox-client-handler"):
    setDaemon(true)
    private val out = BlockboxSockets.utf8Writer(sock)
    private val in = BlockboxSockets.utf8Reader(sock)
    private val sendLock = Object()
    @volatile var active = true
    @volatile var registered = false
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

    override def run(): Unit =
      try
        var line = readProtocolLine(in)
        while line != null && active do
          val packet = BlockboxNet.packet(line)
          val parts = packet.parts()
          if packet.is(BlockboxNet.PING) then
            send("PONG|BLOCKBOX|1")
            active = false
          else if packet.is(BlockboxNet.HELO) then
            val hello = BlockboxNet.parseHello(packet)
            val requestedName = hello.playerName
            val clientModHash = hello.modHash
            if registeredClientCount >= BlockboxNet.MAX_PLAYERS then
              send(BlockboxNet.error("SERVER_FULL", "Server is full"))
              active = false
            else if hello.badProtocol then
              send(BlockboxNet.error("VERSION", "Server protocol " + BlockboxNet.PROTOCOL_VERSION.toString + " does not match client protocol " + hello.clientProtocol))
              active = false
            else if requiredModHash != ModRuntime.VanillaHash && clientModHash != requiredModHash then
              val list = if requiredModList.trim.nonEmpty then requiredModList else requiredModHash
              send(BlockboxNet.error("MOD_MISMATCH", "Server requires modpack " + requiredModHash + " [" + list + "]"))
              active = false
            else if isNameInUse(requestedName) then
              send(BlockboxNet.error("NAME_TAKEN", "That name is already in this world"))
              active = false
            else
              onlineName = requestedName
              colorId = assignColor()
              val worldMsg = BlockboxNet.world(worldSeed, spawn.x, spawn.y, spawn.z, colorId, safeHostName, safeHostColor, sessionId)
              send(worldMsg)
              send("PLAYERS|" + activePlayerTokensSnapshot.mkString("|"))
              val opList =
                try opSnapshot().map(safeName).filter(_.nonEmpty).distinct
                catch case _: Exception => Seq.empty[String]
              send("OPLIST|" + opList.mkString("|"))
              registered = true
              registerClient(this)
              val joinMsg = s"JOIN|$onlineName|$colorId"
              serverEvents.put(joinMsg)
              broadcast(joinMsg)
              sendCurrentPositionsTo(this)
              broadcast(posMessage(onlineName, spawn, 0f, 0f, colorId))
              val snapshot =
                try worldSnapshot().take(250000)
                catch case _: Exception => Seq.empty[(Int, Int, Int, Byte)]
              send("SNAPBEGIN|" + snapshot.length.toString)
              var checksum = 0L
              snapshot.foreach { case (sx, sy, sz, sid) =>
                checksum = BlockboxNet.blockChecksum(checksum, sx, sy, sz, sid.toInt, 0)
                send(blockMessage(sx, sy, sz, sid))
              }
              send("SNAPEND|" + snapshot.length.toString + "|" + BlockboxNet.checksumText(checksum))
              send("ENTER|" + sessionId)
          else if registered && packet.is(BlockboxNet.BLOC) then
            try
              val bp = BlockboxNet.parseBlock(packet, Terrain.worldHeight)
              val blockId = bp.blockId.toByte
              if canAcceptBlockChange(bp.x, bp.y, bp.z, blockId) then
                val level = if blockId == Block.Water.id then (if bp.waterLevel > 0 then bp.waterLevel else 8) else 0
                val clean = blockMessage(bp.x, bp.y, bp.z, blockId, level)
                onBlockChange(bp.x, bp.y, bp.z, blockId)
                serverEvents.put(clean)
                broadcast(clean)
            catch case e: Exception => noteBadPacket("BLOC", e)
          else if registered && packet.is(BlockboxNet.POS) then
            try
              val pp = BlockboxNet.parsePosition(packet, colorCount)
              lastPos = Vec3(pp.x, pp.y, pp.z)
              hasPosition = true
              lastYaw = pp.yaw; lastPitch = pp.pitch
              val clean = posMessage(onlineName, lastPos, pp.yaw, pp.pitch, colorId)
              serverEvents.put(clean)
              broadcast(clean)
            catch case e: Exception => noteBadPacket("POS", e)
          else if registered && line.startsWith("CMD|") then
            val text = if parts.length >= 2 then parts.drop(1).mkString("|") else ""
            if text.nonEmpty then serverEvents.put("RCMD|" + onlineName + "|" + text)
          else if registered && line.startsWith("CHAT|") then
            val text = if parts.length >= 3 then parts.drop(2).mkString("|") else ""
            val clean = "CHAT|" + onlineName + "|" + text
            serverEvents.put(clean)
            broadcast(clean)
          if active then line = readProtocolLine(in) else line = null
      catch
        case e: Exception => System.err.println(s"Blockbox server client error from ${BlockboxSockets.remoteAddress(sock)}: ${Option(e.getMessage).getOrElse(e.getClass.getSimpleName)}")
      finally
        val wasRegistered = registered
        registered = false
        unregisterClient(this)
        if wasRegistered && onlineName != null && onlineName.nonEmpty then
          val leftMsg = s"LEFT|$onlineName"
          serverEvents.put(leftMsg)
          broadcast(leftMsg)
        close()

    def send(msg: String): Unit =
      try sendLock.synchronized { if !BlockboxSockets.sendLine(out, msg) then active = false }
      catch case _: Exception => active = false

    def close(): Unit =
      active = false
      BlockboxSockets.closeQuietly(sock)

object GameServer:
  private val threadGroup = Thread.currentThread.getThreadGroup

  def readProtocolLine(in: BufferedReader): String =
    BlockboxNet.readProtocolLine(in)

final class GameClient(host: String, port: Int):
  private var sock: Socket = null
  private var out: PrintWriter = null
  private var in: BufferedReader = null
  private val msgQueue = new LinkedBlockingQueue[String](BlockboxNet.CLIENT_QUEUE_LIMIT)
  @volatile private var connected = false
  private var readerThread: Thread = null
  @volatile private var lastError = ""
  private val sendLock = Object()

  def errorMessage: String = lastError

  def connect(playerName: String, localModHash: String): Boolean =
    lastError = ""
    try
      sock = BlockboxSockets.connectTcp(host, port, BlockboxNet.CONNECT_TIMEOUT_MS)
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
      if firstLine.startsWith("ERR|") then
        val parts = BlockboxNet.split(firstLine)
        lastError = if parts.length >= 3 then parts.drop(2).mkString(" ") else firstLine
        disconnect()
        return false
      if !firstLine.startsWith("WORLD|") then
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
      connected = true
      sock.setSoTimeout(0)
      readerThread = BlockboxSockets.daemonThread("client-reader", () =>
        try
          var line = GameServer.readProtocolLine(in)
          while line != null && connected do
            if !BlockboxSockets.offerLine(msgQueue, line) then connected = false
            line = GameServer.readProtocolLine(in)
        catch
          case _: SocketException => connected = false
          case _: IOException => connected = false
          case _: Exception => connected = false
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
    connected = false
    try if out != null then BlockboxSockets.sendLine(out, "QUIT") catch case _: Exception => ()
    BlockboxSockets.closeQuietly(sock)
    msgQueue.clear()

  def send(cmd: String): Unit =
    if connected && out != null then
      try sendLock.synchronized { if !BlockboxSockets.sendLine(out, cmd) then connected = false }
      catch case _: Exception => connected = false

  def pollMessage(): Option[String] = Option(msgQueue.poll())

  def isConnected: Boolean = connected
