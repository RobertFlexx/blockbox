package blockbox

import scala.collection.mutable.ArrayBuffer
import scala.math.floor

trait BlockboxCommands:
  protected def playerName: String
  protected def playerName_=(value: String): Unit
  protected def knownPlayerNames: scala.collection.mutable.HashSet[String]
  protected def remotePlayers: scala.collection.mutable.HashMap[String, RemotePlayer]
  protected def networkSafeName(value: String): String
  protected def networkEscape(value: String): String
  protected def networkFloat(value: Float): String
  protected def multiplayerMode: Boolean
  protected def gameServer: GameServer
  protected def gameClient: GameClient
  protected def modManager: ModManager
  protected def isOppedName(name: String): Boolean
  protected def setOppedName(name: String, value: Boolean, announce: Boolean = true): Unit
  protected def broadcastOpState(name: String, value: Boolean): Unit
  protected def worldCheatsEnabled: Boolean
  protected def worldCheatsEnabled_=(value: Boolean): Unit
  protected def gameMode: GameMode
  protected def gameMode_=(mode: GameMode): Unit
  protected def flyEnabled: Boolean
  protected def flyEnabled_=(value: Boolean): Unit
  protected def velocity: Vec3
  protected def velocity_=(value: Vec3): Unit
  protected def onGround: Boolean
  protected def onGround_=(value: Boolean): Unit
  protected def camera: Vec3
  protected def camera_=(value: Vec3): Unit
  protected def timeOverride: Option[Float]
  protected def timeOverride_=(value: Option[Float]): Unit
  protected def dayLengthSeconds: Float
  protected def nightLengthSeconds: Float
  protected def terrainGen: TerrainGenerator
  protected def playerHealth: Float
  protected def playerHealth_=(value: Float): Unit
  protected def playerFood: Float
  protected def playerFood_=(value: Float): Unit
  protected def maxPlayerHealth: Float
  protected def maxPlayerFood: Float
  protected def worldName: String
  protected def worldSeed: Long
  protected def chatInput: String
  protected def chatInput_=(value: String): Unit
  protected def commandSuggestionIndex: Int
  protected def commandSuggestionIndex_=(value: Int): Unit
  protected def chatMessages: ArrayBuffer[(String, Float)]
  protected def validHotbarBlock(block: Block): Boolean
  protected def gainItem(block: Block, amount: Int): Unit
  protected def blockName(block: Block): String
  protected def chunkCoordBlock(v: Int): Int
  protected def findSpawn(): Vec3
  protected def saveWorld(): Unit
  protected def sendChatMessage(text: String): Unit
  protected def addChatMessage(msg: String): Unit

  protected def allPlayerNames: Seq[String] =
    (Seq(playerName) ++ knownPlayerNames.toSeq ++ remotePlayers.keys.toSeq).map(networkSafeName).filter(_.nonEmpty).distinct

  protected def canUseCheatAuthority: Boolean =
    !multiplayerMode || gameServer != null || isOppedName(playerName)

  protected def isClientOnlyOperator: Boolean =
    multiplayerMode && gameClient != null && gameServer == null && isOppedName(playerName)

  protected def commandSuggestions(input: String): Seq[String] =
    if input == null || !input.startsWith("/") then Seq.empty
    else
      val raw = input.drop(1)
      val parts = raw.split("\\s+", -1)
      val commands = (Seq(
        "/help", "/enablecheats", "/op", "/deop", "/gamemode", "/gm",
        "/timeset", "/time", "/fly", "/tp", "/teleport", "/spawn",
        "/give", "/heal", "/feed", "/where", "/biome", "/seed", "/save", "/clear"
      ) ++ modManager.commandNames.map("/" + _)).distinct
      if !raw.contains(" ") then
        val prefix = "/" + raw.toLowerCase
        commands.filter(_.startsWith(prefix)).take(8)
      else
        parts.headOption.map(_.toLowerCase).getOrElse("") match
          case "gamemode" | "gm" =>
            val prefix = parts.lastOption.getOrElse("").toLowerCase
            Seq("survival", "creative").filter(_.startsWith(prefix)).map(m => "/" + parts.head + " " + m)
          case "timeset" | "time" =>
            val prefix = parts.lastOption.getOrElse("").toLowerCase
            Seq("day", "noon", "sunset", "night", "midnight", "sunrise", "reset").filter(_.startsWith(prefix)).map(v => "/" + parts.head + " " + v)
          case "give" =>
            if parts.length <= 2 then
              val last = parts.lastOption.getOrElse("")
              val prefix = last.toLowerCase
              Block.all.filter(validHotbarBlock).map(_.toString.toLowerCase).filter(_.startsWith(prefix)).take(8).map(name => "/" + raw.dropRight(last.length) + name)
            else Seq("/" + parts.head + " " + parts(1) + " 64")
          case "op" | "deop" =>
            val last = parts.lastOption.getOrElse("")
            val prefix = last.toLowerCase
            allPlayerNames.filter(_.toLowerCase.startsWith(prefix)).take(8).map(name => "/" + raw.dropRight(last.length) + name)
          case "tp" | "teleport" | "spawn" | "heal" | "feed" =>
            val last = parts.lastOption.getOrElse("")
            val prefix = last.toLowerCase
            allPlayerNames.filter(_.toLowerCase.startsWith(prefix)).take(8).map(name => "/" + raw.dropRight(last.length) + name)
          case _ => Seq.empty

  protected def applyCommandSuggestion(): Unit =
    val suggestions = commandSuggestions(chatInput)
    if suggestions.nonEmpty then
      val picked = suggestions(commandSuggestionIndex.max(0).min(suggestions.length - 1))
      chatInput = picked + (if picked.count(_ == ' ') == 0 then " " else "")
      commandSuggestionIndex = 0

  protected def findPlayerName(query: String): Option[String] =
    val q = Option(query).getOrElse("").trim.toLowerCase
    if q.isEmpty then None
    else allPlayerNames.find(_.toLowerCase == q).orElse(allPlayerNames.find(_.toLowerCase.startsWith(q)))

  protected def positionOfPlayer(name: String): Option[Vec3] =
    val clean = networkSafeName(name)
    if clean.equalsIgnoreCase(playerName) then Some(camera)
    else remotePlayers.collectFirst { case (n, rp) if n.equalsIgnoreCase(clean) => rp.pos }

  protected def teleportPlayer(target: String, dest: Vec3): Unit =
    val clean = networkSafeName(target)
    if clean.equalsIgnoreCase(playerName) then
      camera = dest
      velocity = Vec3(0f, 0f, 0f)
      addChatMessage(s"Teleported $clean")
    else if gameServer != null then
      val msg = "TPOS|" + networkEscape(clean) + "|" + networkFloat(dest.x) + "|" + networkFloat(dest.y) + "|" + networkFloat(dest.z)
      gameServer.broadcast(msg)
      addChatMessage(s"Teleported $clean")
    else addChatMessage("Only the host can teleport other players")

  protected def setGameModeForPlayer(name: String, mode: GameMode): Unit =
    val safe = networkSafeName(name)
    if safe.equalsIgnoreCase(playerName) then
      if gameMode == mode then addChatMessage(s"Already in $mode mode")
      else toggleTo(mode)
    else if gameServer != null then
      gameServer.broadcast("GMODE|" + networkEscape(safe) + "|" + mode.ordinal.toString)
      addChatMessage(s"Set $safe to $mode")
    else addChatMessage("Only the host can change another player's gamemode")

  protected def toggleFlyForPlayer(name: String): Unit =
    val safe = networkSafeName(name)
    if safe.equalsIgnoreCase(playerName) then
      if gameMode == GameMode.Survival then addChatMessage("Flight is only available in Creative mode")
      else
        flyEnabled = !flyEnabled
        if flyEnabled then
          velocity = Vec3(0f, 0f, 0f)
          addChatMessage("Flight enabled")
        else
          onGround = false
          velocity = Vec3(0f, -0.5f, 0f)
          addChatMessage("Flight disabled")
    else if gameServer != null then
      gameServer.broadcast("FLY|" + networkEscape(safe) + "|TOGGLE")
      addChatMessage(s"Toggled flight for $safe")
    else addChatMessage("Only the host can toggle another player's flight")

  protected def setTimeCommand(mode: String): Boolean =
    val normalized = Option(mode).getOrElse("").trim.toLowerCase
    val applied = normalized match
      case "sunrise" =>
        timeOverride = Some(0f)
        addChatMessage("Set time to sunrise")
        true
      case "day" =>
        timeOverride = Some(dayLengthSeconds * 0.25f)
        addChatMessage("Set time to day")
        true
      case "noon" =>
        timeOverride = Some(dayLengthSeconds * 0.50f)
        addChatMessage("Set time to noon")
        true
      case "sunset" =>
        timeOverride = Some(dayLengthSeconds * 0.92f)
        addChatMessage("Set time to sunset")
        true
      case "night" =>
        timeOverride = Some(dayLengthSeconds + nightLengthSeconds * 0.18f)
        addChatMessage("Set time to night")
        true
      case "midnight" =>
        timeOverride = Some(dayLengthSeconds + nightLengthSeconds * 0.50f)
        addChatMessage("Set time to midnight")
        true
      case "reset" =>
        timeOverride = None
        addChatMessage("Time override cleared")
        true
      case _ => false
    if applied && gameServer != null then gameServer.broadcast("TIME|" + normalized)
    applied

  protected def parseGiveBlock(text: String): Option[Block] =
    Block.find(text).filter(validHotbarBlock)

  protected def giveItemForPlayer(target: String, block: Block, count: Int): Unit =
    val safe = networkSafeName(target)
    val amount = count.max(1).min(999)
    if safe.equalsIgnoreCase(playerName) then
      gainItem(block, amount)
      addChatMessage(s"Gave $amount ${blockName(block)} to $safe")
    else if gameServer != null then
      gameServer.broadcast("GIVE|" + networkEscape(safe) + "|" + block.id.toInt.toString + "|" + amount.toString)
      addChatMessage(s"Gave $amount ${blockName(block)} to $safe")
    else addChatMessage("Only the host can give items to another player")

  protected def setVitalsForPlayer(target: String, health: Option[Float], food: Option[Float], label: String): Unit =
    val safe = networkSafeName(target)
    val hp = health.map(_.max(0f).min(maxPlayerHealth))
    val fd = food.map(_.max(0f).min(maxPlayerFood))
    if safe.equalsIgnoreCase(playerName) then
      hp.foreach(v => playerHealth = v)
      fd.foreach(v => playerFood = v)
      addChatMessage(label + " " + safe)
    else if gameServer != null then
      val hpText = hp.map(networkFloat).getOrElse("-1")
      val foodText = fd.map(networkFloat).getOrElse("-1")
      gameServer.broadcast("VITAL|" + networkEscape(safe) + "|" + hpText + "|" + foodText + "|" + networkEscape(label))
      addChatMessage(label + " " + safe)
    else addChatMessage("Only the host can change another player's health or food")

  protected def biomeNameAt(x: Int, z: Int, y: Int): String =
    val savanna = terrainGen.savannaBlendAt(x, z, y)
    val birch = terrainGen.birchBlendAt(x, z, y)
    if savanna > 0.58f && savanna >= birch then "Savanna"
    else if birch > 0.58f && birch > savanna then "Birch Grove"
    else if savanna > 0.24f && birch > 0.24f then "Mixed Grassland"
    else if savanna > 0.24f then "Savanna Edge"
    else if birch > 0.24f then "Birch Edge"
    else "Plains"

  protected def addPositionReport(name: String, pos: Vec3): Unit =
    val bx = floor(pos.x).toInt
    val by = floor(pos.y).toInt
    val bz = floor(pos.z).toInt
    val biome = biomeNameAt(bx, bz, by)
    addChatMessage(s"$name: x=$bx y=$by z=$bz chunk=(${chunkCoordBlock(bx)}, ${chunkCoordBlock(bz)}) biome=$biome")

  protected def parseCoord(text: String, current: Float): Option[Float] =
    try
      if text == "~" then Some(current)
      else if text.startsWith("~") then Some(current + text.drop(1).toFloat)
      else Some(text.toFloat)
    catch case _: Exception => None

  protected def parseCommand(cmd: String, remoteSender: Option[String] = None): Unit =
    val trimmed = cmd.trim
    if !trimmed.startsWith("/") then
      sendChatMessage(trimmed)
      return

    val args = trimmed.drop(1).split("\\s+").filter(_.nonEmpty)
    if args.isEmpty then return
    val command = args(0).toLowerCase
    val actorName = remoteSender.map(networkSafeName).getOrElse(playerName)
    val actorIsHostLocal = remoteSender.isEmpty && (!multiplayerMode || gameServer != null)
    val actorIsOp = actorIsHostLocal || remoteSender.exists(isOppedName) || (remoteSender.isEmpty && isOppedName(playerName))
    if modManager.hasCommand(command) then
      if remoteSender.isEmpty && gameClient != null && gameServer == null && modManager.commandIsServerInteractive(command) then
        if isOppedName(playerName) then gameClient.send("CMD|" + networkEscape(trimmed))
        else addChatMessage("That mod command is server-side. Ask the host for /op.")
      else modManager.executeCommand(command, args.drop(1), actorName, remoteSender.nonEmpty)
      return
    val isCheat = Set("gamemode", "gm", "timeset", "time", "fly", "tp", "teleport", "spawn", "give", "heal", "feed", "op", "deop").contains(command)

    if remoteSender.isEmpty && isClientOnlyOperator && isCheat && command != "enablecheats" && command != "cheats" && command != "op" && command != "deop" then
      gameClient.send("CMD|" + networkEscape(trimmed))
      return

    if command == "enablecheats" || command == "cheats" then
      if actorIsHostLocal then
        if worldCheatsEnabled then addChatMessage("Cheats are already enabled")
        else
          worldCheatsEnabled = true
          addChatMessage("Cheats enabled")
      else addChatMessage("Only the host can enable cheats")
      return

    if command == "op" || command == "deop" then
      if !actorIsHostLocal then
        addChatMessage("Only the host can op or deop players")
        return
      if !worldCheatsEnabled then
        addChatMessage("Cheats are not enabled. Run /enablecheats first")
        return
      if args.length < 2 then
        addChatMessage("Usage: /" + command + " <player>")
        return
      val target = findPlayerName(args(1)).getOrElse(networkSafeName(args(1)))
      val value = command == "op"
      if value && isOppedName(target) then addChatMessage(s"$target is already an operator")
      else if !value && !isOppedName(target) then addChatMessage(s"$target is not an operator")
      else
        setOppedName(target, value)
        broadcastOpState(target, value)
      return

    if isCheat && !actorIsOp then
      addChatMessage(if remoteSender.isDefined then s"$actorName tried to use a command but is not opped" else "You are not opped")
      return
    if isCheat && !worldCheatsEnabled then
      addChatMessage("Cheats are not enabled. Host can run /enablecheats")
      return

    command match
      case "gamemode" | "gm" =>
        val mode = if args.length > 1 then args(1).toLowerCase else ""
        val targetName = if args.length > 2 then findPlayerName(args(2)).getOrElse(networkSafeName(args(2))) else actorName
        val parsedMode = mode match
          case "survival" | "s" | "0" => Some(GameMode.Survival)
          case "creative" | "c" | "1" => Some(GameMode.Creative)
          case _ => None
        parsedMode match
          case Some(nextMode) => setGameModeForPlayer(targetName, nextMode)
          case None => addChatMessage("Usage: /gamemode <survival|creative> [player]")
      case "timeset" | "time" =>
        val when = if args.length > 1 then args(1).toLowerCase else ""
        if !setTimeCommand(when) then addChatMessage("Usage: /time <sunrise|day|noon|sunset|night|midnight|reset>")
      case "fly" =>
        val targetName = if args.length > 1 then findPlayerName(args(1)).getOrElse(networkSafeName(args(1))) else actorName
        toggleFlyForPlayer(targetName)
      case "spawn" =>
        val targetName = if args.length > 1 then findPlayerName(args(1)).getOrElse(networkSafeName(args(1))) else actorName
        teleportPlayer(targetName, findSpawn())
      case "give" =>
        if args.length < 2 then addChatMessage("Usage: /give <block> [count] [player]")
        else
          parseGiveBlock(args(1)) match
            case Some(block) =>
              val amount = if args.length > 2 then args(2).toIntOption.getOrElse(64) else 64
              val targetName = if args.length > 3 then findPlayerName(args(3)).getOrElse(networkSafeName(args(3))) else actorName
              giveItemForPlayer(targetName, block, amount)
            case None => addChatMessage("Unknown or ungiveable block: " + args(1))
      case "heal" =>
        val targetName = if args.length > 1 then findPlayerName(args(1)).getOrElse(networkSafeName(args(1))) else actorName
        setVitalsForPlayer(targetName, Some(maxPlayerHealth), None, "Healed")
      case "feed" =>
        val targetName = if args.length > 1 then findPlayerName(args(1)).getOrElse(networkSafeName(args(1))) else actorName
        setVitalsForPlayer(targetName, None, Some(maxPlayerFood), "Fed")
      case "tp" | "teleport" =>
        if args.length == 2 then
          findPlayerName(args(1)).flatMap(positionOfPlayer) match
            case Some(dest) => teleportPlayer(actorName, dest)
            case None => addChatMessage("Usage: /tp <player> OR /tp <target> <destination> OR /tp <x> <y> <z>")
        else if args.length == 3 then
          val targetOpt = findPlayerName(args(1))
          val destOpt = findPlayerName(args(2)).flatMap(positionOfPlayer)
          (targetOpt, destOpt) match
            case (Some(target), Some(dest)) => teleportPlayer(target, dest)
            case _ => addChatMessage("Usage: /tp <target> <destination>")
        else if args.length == 4 then
          val base = positionOfPlayer(actorName).getOrElse(camera)
          val x = parseCoord(args(1), base.x)
          val y = parseCoord(args(2), base.y)
          val z = parseCoord(args(3), base.z)
          (x, y, z) match
            case (Some(px), Some(py), Some(pz)) => teleportPlayer(actorName, Vec3(px, py, pz))
            case _ => addChatMessage("Usage: /tp <x> <y> <z>")
        else if args.length == 5 then
          val targetOpt = findPlayerName(args(1))
          val base = targetOpt.flatMap(positionOfPlayer).getOrElse(camera)
          val x = parseCoord(args(2), base.x)
          val y = parseCoord(args(3), base.y)
          val z = parseCoord(args(4), base.z)
          (targetOpt, x, y, z) match
            case (Some(target), Some(px), Some(py), Some(pz)) => teleportPlayer(target, Vec3(px, py, pz))
            case _ => addChatMessage("Usage: /tp <target> <x> <y> <z>")
        else addChatMessage("Usage: /tp <player> | /tp <target> <destination> | /tp <x> <y> <z>")
      case "where" =>
        val targetName = if args.length > 1 then findPlayerName(args(1)).getOrElse(networkSafeName(args(1))) else actorName
        positionOfPlayer(targetName) match
          case Some(pos) => addPositionReport(targetName, pos)
          case None => addChatMessage("Unknown player: " + targetName)
      case "biome" =>
        val pos = positionOfPlayer(actorName).getOrElse(camera)
        val bx = floor(pos.x).toInt
        val by = floor(pos.y).toInt
        val bz = floor(pos.z).toInt
        val savanna = terrainGen.savannaBlendAt(bx, bz, by)
        val birch = terrainGen.birchBlendAt(bx, bz, by)
        val tint = terrainGen.grassTintAt(bx, bz, by)
        addChatMessage(f"Biome: ${biomeNameAt(bx, bz, by)} savanna=$savanna%.2f birch=$birch%.2f grassRGB=${tint._1}%.2f/${tint._2}%.2f/${tint._3}%.2f")
      case "seed" => addChatMessage(s"World: $worldName seed=$worldSeed")
      case "save" =>
        if !actorIsHostLocal then addChatMessage("Only the host can save the world")
        else
          saveWorld()
          addChatMessage(s"Saved world: $worldName")
      case "clear" => chatMessages.clear()
      case "help" =>
        addChatMessage("Commands: /help, /where, /biome, /seed, /clear, /save")
        addChatMessage("Admin: /enablecheats, /op, /deop, /gamemode, /time, /fly, /tp, /spawn, /give, /heal, /feed")
      case _ => addChatMessage(s"Unknown command: /${args(0)}")

  protected def toggleTo(mode: GameMode): Unit =
    gameMode = mode
    if mode == GameMode.Survival then flyEnabled = false
    velocity = Vec3(0f, 0f, 0f); onGround = false
    addChatMessage(s"Game mode set to $mode")
