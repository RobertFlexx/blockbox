package blockbox

import blockbox.io.BlockboxFiles

object BlockboxWorld:
  private val NonZeroRandomFallback = 0x6a09e667f3bcc909L
  private val NonZeroTextFallback = 0x510e527fade682d1L

  def mix64(input: Long): Long =
    var z = input
    z = (z ^ (z >>> 30)) * -4658895280553007687L
    z = (z ^ (z >>> 27)) * -7723592293110705685L
    z ^ (z >>> 31)

  def freshWorldSeed(seedRng: java.security.SecureRandom, identitySalt: Long): Long =
    val bytes = new Array[Byte](8)
    seedRng.nextBytes(bytes)
    var randomPart = 0L
    bytes.foreach { b => randomPart = (randomPart << 8) ^ (b.toLong & 0xffL) }
    val uuid = java.util.UUID.randomUUID()
    val mixed = mix64(
      randomPart ^ System.nanoTime() ^
      java.lang.Long.rotateLeft(System.currentTimeMillis(), 17) ^
      java.lang.Long.rotateLeft(uuid.getMostSignificantBits, 29) ^
      uuid.getLeastSignificantBits ^ identitySalt
    )
    if mixed == 0L then NonZeroRandomFallback else mixed

  def seedFromText(text: String, freshSeed: => Long): Long =
    val trimmed = Option(text).getOrElse("").trim
    if trimmed.isEmpty then freshSeed
    else
      var h = -3750763034362895579L
      trimmed.foreach { ch =>
        h ^= ch.toLong
        h *= 1099511628211L
      }
      val mixed = mix64(h ^ java.lang.Long.rotateLeft(trimmed.length.toLong, 32))
      if mixed == 0L then NonZeroTextFallback else mixed

  def worldFolderNameForSeed(seed: Long): String =
    "World-" + java.lang.Long.toUnsignedString(seed, 36).toUpperCase

  def sanitizeWorldName(raw: String): String =
    val cleaned = Option(raw).getOrElse("").trim
      .map(ch => if ch.isLetterOrDigit || ch == ' ' || ch == '_' || ch == '-' then ch else '_')
      .mkString
      .replaceAll("\\s+", " ")
      .take(40)
    if cleaned.nonEmpty then cleaned else "New World"

  def worldsRootDir: java.io.File = java.io.File("worlds")

  def currentWorldDir(worldName: String): java.io.File =
    java.io.File(worldsRootDir, worldName)

  def currentChunksDir(worldName: String): java.io.File =
    java.io.File(currentWorldDir(worldName), "chunks")

  def worldSaveDirs(root: java.io.File = worldsRootDir): List[java.io.File] =
    val files = Option(root.listFiles()).getOrElse(Array.empty[java.io.File])
    files.filter(d => d.isDirectory && java.io.File(d, "world.dat").isFile).toList.sortWith((a, b) => a.lastModified() > b.lastModified())

  def uniqueWorldFolderName(raw: String): String =
    val base = sanitizeWorldName(raw)
    var candidate = base
    var n = 2
    while java.io.File(worldsRootDir, candidate).exists() do
      candidate = s"$base ($n)"
      n += 1
    candidate

  def writeWorldIndex(root: java.io.File = worldsRootDir): Unit =
    BlockboxFiles.ensureDirectory(root.toPath)
    val text = StringBuilder()
    worldSaveDirs(root).foreach { d =>
      text.append(d.getName).append('|').append(java.io.File(d, "world.dat").lastModified().toString).append('\n')
    }
    BlockboxFiles.writeTextAtomic(java.io.File(root, "index.txt").toPath, text.toString)
