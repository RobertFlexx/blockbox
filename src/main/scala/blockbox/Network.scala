package blockbox

import java.io.*
import java.net.*
import java.util.concurrent.LinkedBlockingQueue
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
  requiredModHash: String,
  requiredModList: String
):
  import GameServer.*
  private val serverSocket =
    val s = ServerSocket()
    s.setReuseAddress(true)
    // Keep this explicitly IPv4 for local/LAN testing. 0.0.0.0 accepts 127.0.0.1, LAN, and ZeroTier IPv4.
    s.bind(InetSocketAddress(InetAddress.getByName("0.0.0.0"), port))
    s
  private val clients = collection.mutable.ListBuffer[ClientHandler]()
  private val serverEvents = new LinkedBlockingQueue[String]()
  private val running = java.util.concurrent.atomic.AtomicBoolean(true)
  private val acceptThread = Thread(() =>
    try
      while running.get do
        try
          val sock = serverSocket.accept()
          sock.setTcpNoDelay(true)
          sock.setKeepAlive(true)
          sock.setSoTimeout(0)
          val handler = ClientHandler(sock)
          handler.start()
        catch case _: SocketTimeoutException => ()
    catch
      case _: SocketException => running.set(false)
      case _: IOException => running.set(false)
  , "blockbox-server-accept")
  acceptThread.setDaemon(true)
  acceptThread.start()

  def localPort: Int = serverSocket.getLocalPort
  def isListening: Boolean = running.get && serverSocket.isBound && !serverSocket.isClosed

  def broadcast(msg: String): Unit =
    clients.synchronized(clients.filter(_.active).foreach(_.send(msg)))

  def pollMessage(): Option[String] = Option(serverEvents.poll())

  def stop(): Unit =
    running.set(false)
    clients.synchronized(clients.foreach(_.close()))
    try serverSocket.close() catch case _: IOException => ()

  private def safeName(value: String): String =
    val cleaned = Option(value).getOrElse("Player").trim.filter(ch => ch.isLetterOrDigit || ch == '_' || ch == '-').take(16)
    if cleaned.nonEmpty then cleaned else "Player"

  private val safeHostName = safeName(hostName)
  private val colorCount = 10
  private val safeHostColor = Math.floorMod(hostColorId, colorCount)

  private def playerToken(name: String, color: Int): String =
    safeName(name) + ":" + Math.floorMod(color, colorCount).toString

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
    private val out = PrintWriter(BufferedWriter(OutputStreamWriter(sock.getOutputStream)), true)
    private val in = BufferedReader(InputStreamReader(sock.getInputStream))
    @volatile var active = true
    @volatile var registered = false
    @volatile var onlineName = "Player"
    @volatile var colorId = 0
    @volatile var hasPosition = false
    @volatile var lastPos: Vec3 = spawn

    override def run(): Unit =
      try
        var line = in.readLine()
        while line != null && active do
          val parts = line.split("\\|", -1)
          if line.startsWith("PING") then
            send("PONG|BLOCKBOX|1")
            active = false
          else if line.startsWith("HELO|") then
            val requestedName =
              if parts.length >= 2 && parts(1).nonEmpty then safeName(parts(1))
              else "Player"
            val clientModHash = if parts.length >= 3 && parts(2).nonEmpty then parts(2) else ModRuntime.VanillaHash
            if requiredModHash != ModRuntime.VanillaHash && clientModHash != requiredModHash then
              val list = if requiredModList.trim.nonEmpty then requiredModList else requiredModHash
              send("ERR|MOD_MISMATCH|Server requires modpack " + requiredModHash + " [" + list + "]")
              active = false
            else if isNameInUse(requestedName) then
              send("ERR|NAME_TAKEN|That name is already in this world")
              active = false
            else
              onlineName = requestedName
              colorId = assignColor()
              val worldMsg = "WORLD|" + worldSeed.toString + "|" + f"${spawn.x}%.3f" + "|" + f"${spawn.y}%.3f" + "|" + f"${spawn.z}%.3f" + "|" + colorId.toString + "|" + safeHostName + "|" + safeHostColor.toString
              send(worldMsg)
              send("PLAYERS|" + activePlayerTokensSnapshot.mkString("|"))
              val opList =
                try opSnapshot().map(safeName).filter(_.nonEmpty).distinct
                catch case _: Exception => Seq.empty[String]
              send("OPLIST|" + opList.mkString("|"))
              val snapshot =
                try worldSnapshot().take(250000)
                catch case _: Exception => Seq.empty[(Int, Int, Int, Byte)]
              send("SNAPBEGIN|" + snapshot.length.toString)
              snapshot.foreach { case (sx, sy, sz, sid) => send("BLOC|" + sx + "|" + sy + "|" + sz + "|" + sid.toString) }
              send("SNAPEND")
              registered = true
              registerClient(this)
              val joinMsg = s"JOIN|$onlineName|$colorId"
              serverEvents.put(joinMsg)
              broadcast(joinMsg)
          else if registered && line.startsWith("BLOC|") then
            if parts.length >= 5 then
              val x = parts(1).toInt; val y = parts(2).toInt; val z = parts(3).toInt
              val blockId = parts(4).toByte
              if canAcceptBlockChange(x, y, z, blockId) then
                val level =
                  if blockId == Block.Water.id && parts.length >= 6 then
                    try parts(5).toInt.max(1).min(8) catch case _: Exception => 8
                  else if blockId == Block.Water.id then 8 else 0
                val clean = "BLOC|" + x + "|" + y + "|" + z + "|" + blockId.toString + (if blockId == Block.Water.id then "|" + level.toString else "")
                onBlockChange(x, y, z, blockId)
                serverEvents.put(clean)
                broadcast(clean)
          else if registered && line.startsWith("POS|") then
            if parts.length >= 7 then
              val px = parts(2).toFloat; val py = parts(3).toFloat; val pz = parts(4).toFloat
              lastPos = Vec3(px, py, pz)
              hasPosition = true
              val clean = "POS|" + onlineName + "|" + parts(2) + "|" + parts(3) + "|" + parts(4) + "|" + parts(5) + "|" + parts(6) + "|" + colorId.toString
              serverEvents.put(clean)
              broadcast(clean)
          else if registered && line.startsWith("CMD|") then
            val text = if parts.length >= 2 then parts.drop(1).mkString("|") else ""
            if text.nonEmpty then serverEvents.put("RCMD|" + onlineName + "|" + text)
          else if registered && line.startsWith("CHAT|") then
            val text = if parts.length >= 3 then parts.drop(2).mkString("|") else ""
            val clean = "CHAT|" + onlineName + "|" + text
            serverEvents.put(clean)
            broadcast(clean)
          if active then line = in.readLine() else line = null
      catch case _: Exception => ()
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
      try { out.println(msg); out.flush() }
      catch case _: Exception => active = false

    def close(): Unit =
      active = false
      try sock.close() catch case _: IOException => ()

object GameServer:
  private val threadGroup = Thread.currentThread.getThreadGroup

final class GameClient(host: String, port: Int):
  private var sock: Socket = null
  private var out: PrintWriter = null
  private var in: BufferedReader = null
  private val msgQueue = new LinkedBlockingQueue[String]()
  @volatile private var connected = false
  private var readerThread: Thread = null
  @volatile private var lastError = ""

  def errorMessage: String = lastError

  def connect(playerName: String, localModHash: String): Boolean =
    lastError = ""
    try
      sock = Socket()
      sock.connect(InetSocketAddress(host, port), 3500)
      sock.setTcpNoDelay(true)
      sock.setKeepAlive(true)
      // Short timeout only for the login/world handshake. After that the reader blocks normally.
      sock.setSoTimeout(5000)
      out = PrintWriter(sock.getOutputStream, true)
      in = BufferedReader(InputStreamReader(sock.getInputStream))
      out.println(s"HELO|$playerName|${Option(localModHash).getOrElse(ModRuntime.VanillaHash)}")
      out.flush()

      val firstLine = in.readLine()
      if firstLine == null then
        lastError = "Server closed the connection before sending world data."
        disconnect()
        return false
      if firstLine.startsWith("ERR|") then
        val parts = firstLine.split("\\|", -1)
        lastError = if parts.length >= 3 then parts.drop(2).mkString(" ") else firstLine
        disconnect()
        return false
      if !firstLine.startsWith("WORLD|") then
        lastError = "Connected, but the server did not speak Blockbox protocol."
        disconnect()
        return false

      msgQueue.put(firstLine)
      connected = true
      sock.setSoTimeout(0)
      readerThread = Thread(() =>
        try
          var line = in.readLine()
          while line != null && connected do
            msgQueue.put(line)
            line = in.readLine()
        catch
          case _: SocketException => connected = false
          case _: IOException => connected = false
          case _: Exception => connected = false
      , "client-reader")
      readerThread.setDaemon(true)
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
    try if out != null then out.println("QUIT") catch case _: Exception => ()
    try if sock != null then sock.close() catch case _: Exception => ()
    msgQueue.clear()

  def send(cmd: String): Unit =
    if connected && out != null then
      try { out.println(cmd); out.flush() } catch case _: Exception => connected = false

  def pollMessage(): Option[String] = Option(msgQueue.poll())

  def isConnected: Boolean = connected
