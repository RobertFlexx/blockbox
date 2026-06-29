package blockbox

import blockbox.io.BlockboxSaveFiles

import scala.jdk.CollectionConverters.*

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
    BlockboxSaveFiles.sanitizeWorldName(raw)

  def worldsRootDir: java.io.File = BlockboxSaveFiles.defaultWorldsRoot().toFile

  def currentWorldDir(worldName: String): java.io.File =
    BlockboxSaveFiles.worldDirectory(worldsRootDir.toPath, worldName).toFile

  def currentChunksDir(worldName: String): java.io.File =
    BlockboxSaveFiles.chunksDirectory(currentWorldDir(worldName).toPath).toFile

  def worldSaveDirs(root: java.io.File = worldsRootDir): List[java.io.File] =
    BlockboxSaveFiles.listWorldSaves(root.toPath).asScala.toList.map(_.directory().toFile)

  def uniqueWorldFolderName(raw: String): String =
    BlockboxSaveFiles.uniqueWorldName(worldsRootDir.toPath, raw)

  def writeWorldIndex(root: java.io.File = worldsRootDir): Unit =
    BlockboxSaveFiles.writeWorldIndex(root.toPath)
