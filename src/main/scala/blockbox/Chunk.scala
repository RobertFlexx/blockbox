package blockbox

import blockbox.io.BlockboxFiles
import org.lwjgl.BufferUtils
import org.lwjgl.opengl.GL11.*
import org.lwjgl.opengl.GL15.*
import java.io.*
import scala.collection.mutable.ArrayBuffer
import scala.math.*

final class Chunk(val cx: Int, val cz: Int, atlas: TextureAtlas, gen: TerrainGenerator, initialBlocks: Array[Byte] = null, private var smoothLightingEnabled: Boolean = true):
  var blocks: Array[Byte] = if initialBlocks != null then initialBlocks else gen.fillChunkBlocks(cx, cz)
  private[blockbox] val edits = scala.collection.mutable.HashMap.empty[(Int, Int, Int), Block]
  @volatile private var neighbors = Map.empty[(Int, Int), Chunk]
  // 0 means default full-height water for generated/static water.
  // 1..8 are dynamic flowing-water levels used by the cellular automata.
  private val waterLevelsLocal = new Array[Byte](Terrain.chunkSize * Terrain.worldHeight * Terrain.chunkSize)
  private val waterExtraMagic = 0x574C5631 // "WLV1"

  // Legacy/freshwater repair: old experimental terrain builds could save static
  // generated water above sea level. Those blocks render as raised transparent
  // plates over beaches because static water has no flowing level data. Keep edited
  // water and dynamic water, but clamp natural raw-level-0 water to sea level.
  private def normalizeStaticNaturalWater(): Unit =
    var changed = false
    var lx = 0
    while lx < Terrain.chunkSize do
      var lz = 0
      while lz < Terrain.chunkSize do
        var y = Terrain.seaLevel + 1
        while y < Terrain.worldHeight do
          val i = localIndex(lx, y, lz)
          if blocks(i) == Block.Water.id && waterLevelsLocal(i) == 0.toByte then
            val edited = edits.synchronized { edits.contains((lx, y, lz)) }
            if !edited then
              blocks(i) = Block.Air.id
              changed = true
          y += 1
        lz += 1
      lx += 1
    if changed then markDirtyMesh()

  val baseX = cx * Terrain.chunkSize
  val baseZ = cz * Terrain.chunkSize

  private val floatsPerVertex = 9
  private var opaqueVbo = 0; private var opaqueCount = 0
  private var cutoutVbo = 0; private var cutoutCount = 0
  private var translucentVbo = 0; private var translucentCount = 0
  private var waterVbo = 0; private var waterCount = 0

  // Threaded mesh data: worker fills these, main thread uploads them.
  // The mesh queue flag/revision pair prevents the same chunk from being meshed by
  // multiple worker threads at once. That was the big source of the "random raised
  // chunk / ugly artifact" looking glitches: stale meshes could finish after newer
  // world edits or after a chunk was unloaded.
  @volatile private var pendingData: Array[ArrayBuffer[Float]] = null
  @volatile private var pendingRevision = -1
  @volatile var meshReady: Boolean = false
  @volatile private var disposed = false
  @volatile private var meshRevision = 0
  private val meshQueued = java.util.concurrent.atomic.AtomicBoolean(false)

  normalizeStaticNaturalWater()

  def hasMesh: Boolean =
    opaqueCount > 0 || cutoutCount > 0 || translucentCount > 0 || waterCount > 0

  def isDisposed: Boolean = disposed

  def markDirtyMesh(): Unit =
    meshRevision += 1

  def tryQueueMesh(): Boolean =
    !disposed && meshQueued.compareAndSet(false, true)

  def releaseMeshQueue(): Unit =
    meshQueued.set(false)

  private[blockbox] def setNeighbor(dx: Int, dz: Int, chunk: Chunk): Unit =
    if chunk != null && (dx != 0 || dz != 0) && dx >= -1 && dx <= 1 && dz >= -1 && dz <= 1 then
      neighbors = neighbors.updated((dx, dz), chunk)

  private[blockbox] def clearNeighbor(dx: Int, dz: Int, chunk: Chunk): Unit =
    val key = (dx, dz)
    if neighbors.get(key).contains(chunk) then neighbors = neighbors - key

  private[blockbox] def setSmoothLightingEnabled(value: Boolean): Unit =
    if smoothLightingEnabled != value then
      smoothLightingEnabled = value
      markDirtyMesh()

  def dispose(): Unit =
    disposed = true
    pendingData = null
    meshReady = false
    meshQueued.set(false)
    destroy()
    // This below is just pure bullshit. Ignore how some codebase is duct taped
  private def localIndex(lx: Int, y: Int, lz: Int): Int =
    (y * Terrain.chunkSize + lz) * Terrain.chunkSize + lx

  private def validLocal(lx: Int, y: Int, lz: Int): Boolean =
    lx >= 0 && lx < Terrain.chunkSize && y >= 0 && y < Terrain.worldHeight && lz >= 0 && lz < Terrain.chunkSize

  def getBlock(lx: Int, y: Int, lz: Int): Block =
    val found: Option[Block] = edits.synchronized { edits.get((lx, y, lz)) }
    found match
      case Some(b: Block) => b
      case None =>
        if validLocal(lx, y, lz) then Block.fromId(blocks(localIndex(lx, y, lz)))
        else Block.Air

  def getWaterLevel(lx: Int, y: Int, lz: Int): Int =
    if validLocal(lx, y, lz) && getBlock(lx, y, lz) == Block.Water then
      val raw = waterLevelsLocal(localIndex(lx, y, lz)).toInt & 0xFF
      if raw <= 0 then 8 else raw.max(1).min(8)
    else 0

  def getWaterRawLevel(lx: Int, y: Int, lz: Int): Int =
    if validLocal(lx, y, lz) && getBlock(lx, y, lz) == Block.Water then
      waterLevelsLocal(localIndex(lx, y, lz)).toInt & 0xFF
    else 0

  def foreachDynamicWaterCell(f: (Int, Int, Int, Int) => Unit): Unit =
    var y = 0
    while y < Terrain.worldHeight do
      var lz = 0
      while lz < Terrain.chunkSize do
        var lx = 0
        while lx < Terrain.chunkSize do
          val i = localIndex(lx, y, lz)
          val raw = waterLevelsLocal(i).toInt & 0xFF
          if raw > 0 && Block.fromId(blocks(i)) == Block.Water then f(lx, y, lz, raw.max(1).min(8))
          lx += 1
        lz += 1
      y += 1

  def setWaterLevel(lx: Int, y: Int, lz: Int, level: Int): Unit =
    if validLocal(lx, y, lz) then
      val i = localIndex(lx, y, lz)
      val next = level.max(0).min(8).toByte
      if waterLevelsLocal(i) != next then
        waterLevelsLocal(i) = next
        markDirtyMesh()

  def setBlock(lx: Int, y: Int, lz: Int, block: Block): Unit =
    if validLocal(lx, y, lz) then
      val i = localIndex(lx, y, lz)
      val oldId = blocks(i)
      blocks(i) = block.id
      if block == Block.Water && waterLevelsLocal(i) == 0 then waterLevelsLocal(i) = 8.toByte
      if block != Block.Water then waterLevelsLocal(i) = 0.toByte
      edits.synchronized { edits((lx, y, lz)) = block }
      if oldId != block.id then markDirtyMesh()

  // Thread-safe: called by worker thread, no GL calls. Unknown neighbor-chunk
  // space is treated as air and borders are rebuilt when neighbors load/change,
  // which prevents caves/coastlines from losing visible faces at chunk seams.
  def computeMesh(): Unit =
    if disposed then
      meshReady = false
      releaseMeshQueue()
      return
    val buildRevision = meshRevision
    val opaqueVerts = ArrayBuffer.empty[Float]; val cutoutVerts = ArrayBuffer.empty[Float]
    val translucentVerts = ArrayBuffer.empty[Float]; val waterVerts = ArrayBuffer.empty[Float]

    // Snapshot chunk block data before meshing. The main thread can edit blocks while
    // worker threads are building VBO data (water flow, block edits, multiplayer BLOC
    // packets). Reading the live HashMap/Array from a worker can create random stale or
    // half-built meshes: the dark-blue empty world / missing terrain effect. Snapshotting
    // makes every mesh build deterministic for one revision.
    val blocksSnapshot = blocks.clone()
    val waterLevelsSnapshot = waterLevelsLocal.clone()
    val editsSnapshot: scala.collection.mutable.HashMap[(Int, Int, Int), Block] =
      edits.synchronized { edits.clone() }
    final case class NeighborSnapshot(blocks: Array[Byte], water: Array[Byte], edits: scala.collection.mutable.HashMap[(Int, Int, Int), Block])
    val neighborSnapshots = neighbors.flatMap { case (offset, chunk) =>
      if chunk == null || chunk.isDisposed then None
      else
        val editsCopy = chunk.edits.synchronized { chunk.edits.clone() }
        Some(offset -> NeighborSnapshot(chunk.blocks.clone(), chunk.waterLevelsLocal.clone(), editsCopy))
    }
    def neighborCoords(lx: Int, lz: Int): (Int, Int, Int, Int) =
      val (dx, nx) =
        if lx < 0 then (-1, lx + Terrain.chunkSize)
        else if lx >= Terrain.chunkSize then (1, lx - Terrain.chunkSize)
        else (0, lx)
      val (dz, nz) =
        if lz < 0 then (-1, lz + Terrain.chunkSize)
        else if lz >= Terrain.chunkSize then (1, lz - Terrain.chunkSize)
        else (0, lz)
      (dx, dz, nx, nz)
    def snapshotFrom(data: NeighborSnapshot, lx: Int, y: Int, lz: Int): Block =
      data.edits.get((lx, y, lz)) match
        case Some(b: Block) => b
        case None => Block.fromId(data.blocks((y * Terrain.chunkSize + lz) * Terrain.chunkSize + lx))
    def snapshotBlock(lx: Int, y: Int, lz: Int): Block =
      val found: Option[Block] = editsSnapshot.get((lx, y, lz))
      found match
        case Some(b: Block) => b
        case None =>
          if lx >= 0 && lx < Terrain.chunkSize && y >= 0 && y < Terrain.worldHeight && lz >= 0 && lz < Terrain.chunkSize then
            Block.fromId(blocksSnapshot((y * Terrain.chunkSize + lz) * Terrain.chunkSize + lx))
          else Block.Air
    def localBlock(nlx: Int, ny: Int, nlz: Int): Block =
      if ny < 0 || ny >= Terrain.worldHeight then Block.Air
      else if nlx >= 0 && nlx < Terrain.chunkSize && nlz >= 0 && nlz < Terrain.chunkSize then
        snapshotBlock(nlx, ny, nlz)
      else
        val (dx, dz, lx, lz) = neighborCoords(nlx, nlz)
        neighborSnapshots.get((dx, dz)).map(snapshotFrom(_, lx, ny, lz)).getOrElse(Block.Air)

    def localWaterRawLevel(nlx: Int, ny: Int, nlz: Int): Int =
      if ny < 0 || ny >= Terrain.worldHeight then 0
      else if nlx >= 0 && nlx < Terrain.chunkSize && nlz >= 0 && nlz < Terrain.chunkSize then
        waterLevelsSnapshot((ny * Terrain.chunkSize + nlz) * Terrain.chunkSize + nlx).toInt & 0xFF
      else
        val (dx, dz, lx, lz) = neighborCoords(nlx, nlz)
        neighborSnapshots.get((dx, dz)).map(_.water((ny * Terrain.chunkSize + lz) * Terrain.chunkSize + lx).toInt & 0xFF).getOrElse(0)

    def localWaterLevel(nlx: Int, ny: Int, nlz: Int): Int =
      if ny < 0 || ny >= Terrain.worldHeight then 0
      else if localBlock(nlx, ny, nlz) != Block.Water then 0
      else
        val raw = localWaterRawLevel(nlx, ny, nlz)
        if raw <= 0 then 8 else raw.max(1).min(8)

    def rawWaterTopY(nlx: Int, ny: Int, nlz: Int, fy: Float): Float =
      if ny < 0 || ny >= Terrain.worldHeight then fy
      else if localBlock(nlx, ny, nlz) != Block.Water then fy
      else
        val raw = localWaterRawLevel(nlx, ny, nlz)
        // Generated/static water has raw level 0. Draw it as a full flat block-top
        // surface so all natural oceans, rivers, ponds and lakes share one exact
        // plane. Do not use 0.94 or mixed freshwater heights here; tiny offsets plus
        // transparency caused the visible sheet/step artifacts in large beaches.
        if raw <= 0 || raw >= 8 then fy + 1f
        else
          val level = raw.max(1).min(7).toFloat
          fy + 0.36f + 0.52f * (level / 7f)
    def waterCornerY(cells: Array[(Int, Int, Int)], fy: Float, fallback: Float): Float =
      var sum = 0f
      var count = 0
      var hasFullColumn = false
      var i = 0
      while i < cells.length do
        val (cx, cy, cz) = cells(i)
        if localBlock(cx, cy, cz) == Block.Water then
          if localBlock(cx, cy + 1, cz) == Block.Water then hasFullColumn = true
          sum += rawWaterTopY(cx, cy, cz, fy)
          count += 1
        i += 1
      if hasFullColumn then fy + 1f
      else if count > 0 then sum / count.toFloat
      else fallback
    def isWaterSideVisible(nlx: Int, ny: Int, nlz: Int): Boolean =
      if ny < 0 || ny >= Terrain.worldHeight then false
      else if (nlx < 0 || nlx >= Terrain.chunkSize || nlz < 0 || nlz >= Terrain.chunkSize) &&
        !neighborSnapshots.contains((neighborCoords(nlx, nlz)._1, neighborCoords(nlx, nlz)._2)) then false
      else
        val nb = localBlock(nlx, ny, nlz)
        nb != Block.Water && (nb == Block.Air || nb.cutout || (nb.translucent && nb != Block.Water))

    def shouldDrawWaterSide(lx: Int, y: Int, lz: Int, nlx: Int, ny: Int, nlz: Int): Boolean =
      if !isWaterSideVisible(nlx, ny, nlz) then false
      else
        val raw = localWaterRawLevel(lx, y, lz)
        if raw <= 0 then false
        else
          // Static generated water keeps clean top-only lakes/oceans. Dynamic water draws
          // every exposed side, including stacked waterfall sides when water exists above.
          true
    def isVisible(nlx: Int, ny: Int, nlz: Int, block: Block): Boolean =
      val nb = localBlock(nlx, ny, nlz)
      if nb == Block.Air then true
      else if block == Block.Water then isWaterSideVisible(nlx, ny, nlz)
      else if nb == Block.Water then true
      else if block.cutout && nb.cutout then false
      else if nb.cutout then block != nb
      else if block.solid && nb.solid && !block.translucent && !nb.translucent && !block.cutout && !nb.cutout then false
      else nb.translucent && nb != block
    def occludesLight(b: Block): Boolean = b.solid && !b.translucent && !b.cutout
    val skyMin = -1
    val skyMax = Terrain.chunkSize + 1
    val skySize = skyMax - skyMin + 1
    val skyBlockers = new Array[Int](skySize * skySize)
    var sgx = skyMin
    while sgx <= skyMax do
      var sgz = skyMin
      while sgz <= skyMax do
        var sy = Terrain.worldHeight - 1
        var found = -1
        while sy >= 0 && found < 0 do
          if occludesLight(localBlock(sgx, sy, sgz)) then found = sy
          sy -= 1
        skyBlockers((sgx - skyMin) * skySize + (sgz - skyMin)) = found
        sgz += 1
      sgx += 1
    def highestSkyBlocker(nlx: Int, nlz: Int): Int =
      val cx = nlx.max(skyMin).min(skyMax)
      val cz = nlz.max(skyMin).min(skyMax)
      skyBlockers((cx - skyMin) * skySize + (cz - skyMin))
    def columnSky(nlx: Int, probeY: Int, nlz: Int): Float =
      val topBlocker = highestSkyBlocker(nlx, nlz)
      if topBlocker < probeY then 1f else 0f
    def softSky(ix: Int, probeY: Int, iz: Int): Float =
      // Weighted 3x3 skylight samples produce Minecraft-like soft shadow transitions
      // around caves, cliffs, tree canopies and overhangs without runtime shadow maps.
      val center = columnSky(ix, probeY, iz) * 4f
      val cardinals =
        columnSky(ix - 1, probeY, iz) + columnSky(ix + 1, probeY, iz) +
        columnSky(ix, probeY, iz - 1) + columnSky(ix, probeY, iz + 1)
      val diagonals =
        columnSky(ix - 1, probeY, iz - 1) + columnSky(ix + 1, probeY, iz - 1) +
        columnSky(ix - 1, probeY, iz + 1) + columnSky(ix + 1, probeY, iz + 1)
      (center + cardinals * 2f + diagonals) * 0.0625f
    val lightMin = -1
    val lightMax = Terrain.chunkSize
    val lightSize = lightMax - lightMin + 1
    val lightCellCount = lightSize * Terrain.worldHeight * lightSize
    val blockLight = new Array[Byte](lightCellCount)
    val lightQueueSize = lightCellCount * 16
    val qx = new Array[Int](lightQueueSize)
    val qy = new Array[Int](lightQueueSize)
    val qz = new Array[Int](lightQueueSize)
    def lightIndex(x: Int, y: Int, z: Int): Int =
      (y * lightSize + (z - lightMin)) * lightSize + (x - lightMin)
    def lightSourceLevel(b: Block): Int = b match
      case Block.Torch => 15
      case Block.FurnaceLit => 13
      case _ => 0
    var head = 0
    var tail = 0
    def enqueueLight(x: Int, y: Int, z: Int, level: Int): Unit =
      if x >= lightMin && x <= lightMax && y >= 0 && y < Terrain.worldHeight && z >= lightMin && z <= lightMax && level > 0 then
        val idx = lightIndex(x, y, z)
        if level > (blockLight(idx).toInt & 0xFF) then
          blockLight(idx) = level.toByte
          if tail < lightQueueSize then
            qx(tail) = x; qy(tail) = y; qz(tail) = z; tail += 1
    var lxSeed = lightMin
    while lxSeed <= lightMax do
      var lzSeed = lightMin
      while lzSeed <= lightMax do
        var lySeed = 0
        while lySeed < Terrain.worldHeight do
          val level = lightSourceLevel(localBlock(lxSeed, lySeed, lzSeed))
          if level > 0 then enqueueLight(lxSeed, lySeed, lzSeed, level)
          lySeed += 1
        lzSeed += 1
      lxSeed += 1
    while head < tail do
      val x = qx(head); val y = qy(head); val z = qz(head); head += 1
      val next = (blockLight(lightIndex(x, y, z)).toInt & 0xFF) - 1
      if next > 0 then
        def spread(nx: Int, ny: Int, nz: Int): Unit =
          if nx >= lightMin && nx <= lightMax && ny >= 0 && ny < Terrain.worldHeight && nz >= lightMin && nz <= lightMax && !occludesLight(localBlock(nx, ny, nz)) then enqueueLight(nx, ny, nz, next)
        spread(x + 1, y, z); spread(x - 1, y, z); spread(x, y + 1, z); spread(x, y - 1, z); spread(x, y, z + 1); spread(x, y, z - 1)
    def blockLightAt(x: Int, y: Int, z: Int): Float =
      if x < lightMin || x > lightMax || y < 0 || y >= Terrain.worldHeight || z < lightMin || z > lightMax then 0f
      else (blockLight(lightIndex(x, y, z)).toInt & 0xFF).toFloat / 14f
    def aoFactor(sideA: Boolean, sideB: Boolean, corner: Boolean): Float =
      val level = if sideA && sideB then 3 else (if sideA then 1 else 0) + (if sideB then 1 else 0) + (if corner then 1 else 0)
      level match
        case 0 => 1.00f
        case 1 => 0.82f
        case 2 => 0.68f
        case _ => 0.54f
    def signAt(value: Float, origin: Float): Int = if value <= origin + 0.001f then -1 else 1
    for lx <- 0 until Terrain.chunkSize; lz <- 0 until Terrain.chunkSize; y <- 0 until Terrain.worldHeight do
      val block = snapshotBlock(lx, y, lz)
      if block != Block.Air then
        val atlasRef = atlas
        // Keep chunk mesh vertices local to the chunk. The renderer applies a
        // camera-relative chunk transform, which delays float precision "far lands"
        // artifacts far beyond where full world-coordinate VBOs started wobbling.
        val fx = lx.toFloat; val fy = y.toFloat; val fz = lz.toFloat
        // Water is still one block in storage, but flowing water renders with a variable
        // surface height from the per-chunk level array. Static generated oceans use the
        // default full level without entering the simulation queue.
        val topY = if block == Block.Water then rawWaterTopY(lx, y, lz, fy) else fy + 1f
        val yNorm = y.toFloat / Terrain.worldHeight.toFloat
        val ambient = 0.45f + yNorm * 0.10f
        def grassCornerTint(cfx: Float, cfz: Float): (Float, Float, Float) =
          val sx = floor(baseX.toFloat + cfx).toInt
          val sz = floor(baseZ.toFloat + cfz).toInt
          gen.grassTintAt(sx, sz, y)
        def cornerAo(kind: FaceKind, cfx: Float, cfy: Float, cfz: Float): Float =
          if !smoothLightingEnabled || block == Block.Water || block.translucent then 1f
          else
            val sx = signAt(cfx, fx)
            val sy = signAt(cfy, fy)
            val sz = signAt(cfz, fz)
            val (sideA, sideB, corner) = kind match
              case FaceKind.Top =>
                (occludesLight(localBlock(lx + sx, y + 1, lz)), occludesLight(localBlock(lx, y + 1, lz + sz)), occludesLight(localBlock(lx + sx, y + 1, lz + sz)))
              case FaceKind.Bottom =>
                (occludesLight(localBlock(lx + sx, y - 1, lz)), occludesLight(localBlock(lx, y - 1, lz + sz)), occludesLight(localBlock(lx + sx, y - 1, lz + sz)))
              case FaceKind.East =>
                (occludesLight(localBlock(lx + 1, y + sy, lz)), occludesLight(localBlock(lx + 1, y, lz + sz)), occludesLight(localBlock(lx + 1, y + sy, lz + sz)))
              case FaceKind.West =>
                (occludesLight(localBlock(lx - 1, y + sy, lz)), occludesLight(localBlock(lx - 1, y, lz + sz)), occludesLight(localBlock(lx - 1, y + sy, lz + sz)))
              case FaceKind.South =>
                (occludesLight(localBlock(lx + sx, y, lz + 1)), occludesLight(localBlock(lx, y + sy, lz + 1)), occludesLight(localBlock(lx + sx, y + sy, lz + 1)))
              case FaceKind.North =>
                (occludesLight(localBlock(lx + sx, y, lz - 1)), occludesLight(localBlock(lx, y + sy, lz - 1)), occludesLight(localBlock(lx + sx, y + sy, lz - 1)))
            aoFactor(sideA, sideB, corner)
        def cornerSkyLight(kind: FaceKind, cfx: Float, cfy: Float, cfz: Float): Float =
          if block == Block.Water || block.translucent then 1f
          else
            val probeY = (floor(cfy).toInt + 1).max(0).min(Terrain.worldHeight)
            val ix = floor(cfx).toInt
            val iz = floor(cfz).toInt
            val sky = softSky(ix, probeY, iz)
            // Minecraft-style skylight should not crush caves to black; AO and face shade
            // still provide contact shadow while this supplies broad soft sky exposure.
            val minSky = kind match
              case FaceKind.Top => 0.34f
              case FaceKind.Bottom => 0.24f
              case _ => 0.30f
            val eased = sky * sky * (3f - 2f * sky)
            minSky + (1f - minSky) * eased
        def cornerBlockLight(cfx: Float, cfy: Float, cfz: Float): Float =
          val ix = floor(cfx).toInt
          val iy = floor(cfy).toInt
          val iz = floor(cfz).toInt
          val fxCorner = abs(cfx - round(cfx).toFloat) < 0.001f
          val fyCorner = abs(cfy - round(cfy).toFloat) < 0.001f
          val fzCorner = abs(cfz - round(cfz).toFloat) < 0.001f
          val x0 = if fxCorner then ix - 1 else ix; val x1 = ix
          val y0 = if fyCorner then iy - 1 else iy; val y1 = iy
          val z0 = if fzCorner then iz - 1 else iz; val z1 = iz
          val sum =
            blockLightAt(x0, y0, z0) + blockLightAt(x1, y0, z0) + blockLightAt(x0, y1, z0) + blockLightAt(x1, y1, z0) +
            blockLightAt(x0, y0, z1) + blockLightAt(x1, y0, z1) + blockLightAt(x0, y1, z1) + blockLightAt(x1, y1, z1)
          sum * 0.125f
        def addFace(shade: Float, corners: Array[(Float, Float, Float, Float, Float)], kind: FaceKind, tintGrass: Boolean = false): Unit =
          val buf = if block.cutout then cutoutVerts
            else if block == Block.Water then waterVerts
            else if block.translucent then translucentVerts
            else opaqueVerts
          val light = if block == Block.Water then (0.55f + yNorm * 0.22f) * (0.82f + shade * 0.18f) else shade * ambient
          val c0 = corners(0); val c1 = corners(1); val c2 = corners(2); val c3 = corners(3)
          val ao0 = cornerAo(kind, c0._1, c0._2, c0._3); val ao1 = cornerAo(kind, c1._1, c1._2, c1._3)
          val ao2 = cornerAo(kind, c2._1, c2._2, c2._3); val ao3 = cornerAo(kind, c3._1, c3._2, c3._3)
          val sky0 = cornerSkyLight(kind, c0._1, c0._2, c0._3); val sky1 = cornerSkyLight(kind, c1._1, c1._2, c1._3)
          val sky2 = cornerSkyLight(kind, c2._1, c2._2, c2._3); val sky3 = cornerSkyLight(kind, c3._1, c3._2, c3._3)
          val bl0 = cornerBlockLight(c0._1, c0._2, c0._3); val bl1 = cornerBlockLight(c1._1, c1._2, c1._3)
          val bl2 = cornerBlockLight(c2._1, c2._2, c2._3); val bl3 = cornerBlockLight(c3._1, c3._2, c3._3)
          def emit(i: Int): Unit =
            val (cfx, cfy, cfz, tu, tv) = corners(i)
            val (u, v) = atlasRef.uv(block, kind, tu, tv)
            val (tintR, tintG, tintB) = if tintGrass then grassCornerTint(cfx, cfz) else (1f, 1f, 1f)
            val alpha = block match
              case Block.Water => 1f
              case Block.Glass => 0.68f
              case _ => 1f
            buf += cfx; buf += cfy; buf += cfz
            val ao = if i == 0 then ao0 else if i == 1 then ao1 else if i == 2 then ao2 else ao3
            val sky = if i == 0 then sky0 else if i == 1 then sky1 else if i == 2 then sky2 else sky3
            val blockGlow = if i == 0 then bl0 else if i == 1 then bl1 else if i == 2 then bl2 else bl3
            val lit = light * ao * sky
            val warm = blockGlow * blockGlow
            buf += (lit * tintR + warm * 0.82f).min(1.35f); buf += (lit * tintG + warm * 0.46f).min(1.35f); buf += (lit * tintB + warm * 0.18f).min(1.35f); buf += alpha
            buf += u; buf += v
          if smoothLightingEnabled && ao0 + ao2 > ao1 + ao3 then
            emit(0); emit(1); emit(3); emit(1); emit(2); emit(3)
          else
            emit(0); emit(1); emit(2); emit(2); emit(3); emit(0)
        def addGrassSide(shade: Float, corners: Array[(Float, Float, Float, Float, Float)], kind: FaceKind): Unit =
          val bottomA = corners(0)
          val bottomB = corners(1)
          val topB = corners(2)
          val topA = corners(3)
          def band(bottom: (Float, Float, Float, Float, Float), top: (Float, Float, Float, Float, Float)): (Float, Float, Float, Float, Float) =
            val t = 0.75f
            (
              bottom._1 + (top._1 - bottom._1) * t,
              bottom._2 + (top._2 - bottom._2) * t,
              bottom._3 + (top._3 - bottom._3) * t,
              bottom._4,
              0.25f
            )
          val bandA = band(bottomA, topA)
          val bandB = band(bottomB, topB)
          addFace(shade, Array(bottomA, bottomB, bandB, bandA), kind, false)
          addFace(shade, Array(bandA, bandB, topB, topA), kind, true)
        if block == Block.Torch then
          // High-quality 3D torch model with wall/floor placement like Minecraft.
          val torchKind = FaceKind.North
          // UV for stick region of the 16x16 texture
          val stickU0 = 6f / 16f; val stickU1 = 9f / 16f
          val stickVBot = 15f / 16f; val stickVTop = 5f / 16f
          // UV for flame region
          val flameU0 = 5f / 16f; val flameU1 = 10f / 16f
          val flameVBot = 4f / 16f; val flameVTop = 0f
          // Determine placement from neighbors (uses snapshot data, thread-safe)
          val floorBelow = localBlock(lx, y - 1, lz).solid
          val wallEast = localBlock(lx + 1, y, lz).solid
          val wallWest = localBlock(lx - 1, y, lz).solid
          val wallSouth = localBlock(lx, y, lz + 1).solid
          val wallNorth = localBlock(lx, y, lz - 1).solid
          // Emit a flame vertex with self-emissive warm glow
          def emitFlame(cfx: Float, cfy: Float, cfz: Float, tu: Float, tv: Float): Unit =
            val (u, v) = atlasRef.uv(Block.Torch, torchKind, tu, tv)
            cutoutVerts += cfx; cutoutVerts += cfy; cutoutVerts += cfz
            cutoutVerts += 2.0f; cutoutVerts += 1.3f; cutoutVerts += 0.35f; cutoutVerts += 1.0f
            cutoutVerts += u; cutoutVerts += v
          // Emit a flame quad (two triangles, emissive)
          def flameQuad(c0: (Float,Float,Float,Float,Float), c1: (Float,Float,Float,Float,Float), c2: (Float,Float,Float,Float,Float), c3: (Float,Float,Float,Float,Float)): Unit =
            emitFlame(c0._1, c0._2, c0._3, c0._4, c0._5)
            emitFlame(c1._1, c1._2, c1._3, c1._4, c1._5)
            emitFlame(c2._1, c2._2, c2._3, c2._4, c2._5)
            emitFlame(c2._1, c2._2, c2._3, c2._4, c2._5)
            emitFlame(c3._1, c3._2, c3._3, c3._4, c3._5)
            emitFlame(c0._1, c0._2, c0._3, c0._4, c0._5)
          if floorBelow then
            // bullshit shit floor torch below
            val cx = fx + 0.5f; val cz = fz + 0.5f
            // Post: 4 vertical faces, thin (0.125 wide), from y=0.0625 to y=0.8125
            val hw = 0.0625f
            val pBot = fy + 0.0625f; val pTop = fy + 0.8125f
            // North
            addFace(1.0f, Array((cx + hw, pBot, cz - hw, stickU1, stickVBot), (cx - hw, pBot, cz - hw, stickU0, stickVBot), (cx - hw, pTop, cz - hw, stickU0, stickVTop), (cx + hw, pTop, cz - hw, stickU1, stickVTop)), torchKind)
            // South
            addFace(1.0f, Array((cx - hw, pBot, cz + hw, stickU0, stickVBot), (cx + hw, pBot, cz + hw, stickU1, stickVBot), (cx + hw, pTop, cz + hw, stickU1, stickVTop), (cx - hw, pTop, cz + hw, stickU0, stickVTop)), torchKind)
            // West
            addFace(1.0f, Array((cx - hw, pBot, cz + hw, stickU1, stickVBot), (cx - hw, pBot, cz - hw, stickU0, stickVBot), (cx - hw, pTop, cz - hw, stickU0, stickVTop), (cx - hw, pTop, cz + hw, stickU1, stickVTop)), torchKind)
            // East
            addFace(1.0f, Array((cx + hw, pBot, cz - hw, stickU0, stickVBot), (cx + hw, pBot, cz + hw, stickU1, stickVBot), (cx + hw, pTop, cz + hw, stickU1, stickVTop), (cx + hw, pTop, cz - hw, stickU0, stickVTop)), torchKind)
            // Base: 4 slightly wider faces at the bottom
            val bw = 0.09375f
            addFace(1.0f, Array((cx + bw, fy, cz - bw, stickU1, stickVBot), (cx - bw, fy, cz - bw, stickU0, stickVBot), (cx - bw, pBot, cz - bw, stickU0, stickVTop), (cx + bw, pBot, cz - bw, stickU1, stickVTop)), torchKind)
            addFace(1.0f, Array((cx - bw, fy, cz + bw, stickU0, stickVBot), (cx + bw, fy, cz + bw, stickU1, stickVBot), (cx + bw, pBot, cz + bw, stickU1, stickVTop), (cx - bw, pBot, cz + bw, stickU0, stickVTop)), torchKind)
            addFace(1.0f, Array((cx - bw, fy, cz + bw, stickU1, stickVBot), (cx - bw, fy, cz - bw, stickU0, stickVBot), (cx - bw, pBot, cz - bw, stickU0, stickVTop), (cx - bw, pBot, cz + bw, stickU1, stickVTop)), torchKind)
            addFace(1.0f, Array((cx + bw, fy, cz - bw, stickU0, stickVBot), (cx + bw, fy, cz + bw, stickU1, stickVBot), (cx + bw, pBot, cz + bw, stickU1, stickVTop), (cx + bw, pBot, cz - bw, stickU0, stickVTop)), torchKind)
            // Base top cap
            addFace(1.0f, Array((cx - bw, pBot, cz - bw, stickU0, stickVTop), (cx + bw, pBot, cz - bw, stickU1, stickVTop), (cx + bw, pBot, cz + bw, stickU1, stickVBot), (cx - bw, pBot, cz + bw, stickU0, stickVBot)), torchKind)
            // Flame: 2 crossed quads with self-emissive colors at the top
            val fw = 0.1875f; val fh = 0.25f
            val fBot = fy + 0.75f; val fTop = fy + 1.0f
            flameQuad((cx - fw, fBot, cz - fw, flameU0, flameVBot), (cx + fw, fBot, cz + fw, flameU1, flameVBot), (cx + fw, fTop, cz + fw, flameU1, flameVTop), (cx - fw, fTop, cz - fw, flameU0, flameVTop))
            flameQuad((cx + fw, fBot, cz - fw, flameU0, flameVBot), (cx - fw, fBot, cz + fw, flameU1, flameVBot), (cx - fw, fTop, cz + fw, flameU1, flameVTop), (cx + fw, fTop, cz - fw, flameU0, flameVTop))
          else
            // bullshit really bad wall torch made in like 20 mins
            // Determine wall direction: the post extends from the solid wall face into the block
            val (wx, wz, wKind) =
              if wallEast then (1f, 0f, FaceKind.West)
              else if wallWest then (-1f, 0f, FaceKind.East)
              else if wallSouth then (0f, 1f, FaceKind.North)
              else (0f, -1f, FaceKind.South) // north or default
            val mountX = fx + 0.5f + wx * 0.4375f
            val mountZ = fz + 0.5f + wz * 0.4375f
            val tipX = fx + 0.5f - wx * 0.1875f
            val tipZ = fz + 0.5f - wz * 0.1875f
            val postYBot = fy + 0.5625f; val postYTop = fy + 0.6875f
            val postHW = 0.0625f // half-width perpendicular to post axis
            // Post vertices: 4 long faces along the post axis + 2 end caps
            // The post axis runs from (tipX, tipZ) to (mountX, mountZ)
            // Perpendicular direction for the post's cross-section
            val perpX = -wz; val perpZ = wx
            val p1x = tipX + perpX * postHW; val p1z = tipZ + perpZ * postHW
            val p2x = tipX - perpX * postHW; val p2z = tipZ - perpZ * postHW
            val p3x = mountX + perpX * postHW; val p3z = mountZ + perpZ * postHW
            val p4x = mountX - perpX * postHW; val p4z = mountZ - perpZ * postHW
            // Side 1 (tip-to-mount, +perp side): long face
            addFace(1.0f, Array((p3x, postYBot, p3z, stickU1, stickVBot), (p1x, postYBot, p1z, stickU0, stickVBot), (p1x, postYTop, p1z, stickU0, stickVTop), (p3x, postYTop, p3z, stickU1, stickVTop)), torchKind)
            // Side 2 (mount-to-tip, -perp side): long face
            addFace(1.0f, Array((p2x, postYBot, p2z, stickU0, stickVBot), (p4x, postYBot, p4z, stickU1, stickVBot), (p4x, postYTop, p4z, stickU1, stickVTop), (p2x, postYTop, p2z, stickU0, stickVTop)), torchKind)
            // Top face (along post axis)
            addFace(1.0f, Array((p1x, postYTop, p1z, stickU1, stickVTop), (p3x, postYTop, p3z, stickU0, stickVTop), (p4x, postYTop, p4z, stickU0, stickVBot), (p2x, postYTop, p2z, stickU1, stickVBot)), torchKind)
            // Bottom face (along post axis)
            addFace(1.0f, Array((p3x, postYBot, p3z, stickU0, stickVTop), (p1x, postYBot, p1z, stickU1, stickVTop), (p2x, postYBot, p2z, stickU1, stickVBot), (p4x, postYBot, p4z, stickU0, stickVBot)), torchKind)
            // Tip face (small, at the flame end)
            addFace(1.0f, Array((p2x, postYBot, p2z, stickU0, stickVBot), (p1x, postYBot, p1z, stickU1, stickVBot), (p1x, postYTop, p1z, stickU1, stickVTop), (p2x, postYTop, p2z, stickU0, stickVTop)), torchKind)
            // Mount face (small, near wall) - use same UV mapping, hidden but rendered for correctness
            addFace(1.0f, Array((p4x, postYBot, p4z, stickU1, stickVBot), (p3x, postYBot, p3z, stickU0, stickVBot), (p3x, postYTop, p3z, stickU0, stickVTop), (p4x, postYTop, p4z, stickU1, stickVTop)), torchKind)
            // Flame crossed quads at the tip — same diagonal-XZ pattern as the floor torch
            val fcX = tipX; val fcZ = tipZ; val fcY = fy + 0.625f
            val fw2 = 0.1875f; val fh2 = 0.25f
            flameQuad((fcX - fw2, fcY - fh2, fcZ - fw2, flameU0, flameVBot), (fcX + fw2, fcY - fh2, fcZ + fw2, flameU1, flameVBot), (fcX + fw2, fcY + fh2, fcZ + fw2, flameU1, flameVTop), (fcX - fw2, fcY + fh2, fcZ - fw2, flameU0, flameVTop))
            flameQuad((fcX + fw2, fcY - fh2, fcZ - fw2, flameU0, flameVBot), (fcX - fw2, fcY - fh2, fcZ + fw2, flameU1, flameVBot), (fcX - fw2, fcY + fh2, fcZ + fw2, flameU1, flameVTop), (fcX + fw2, fcY + fh2, fcZ - fw2, flameU0, flameVTop))
        else if block == Block.Water then
          // Render dynamic water as actual block-volume water with internal culling.
          // Neighboring water cells hide shared faces; exposed streams and waterfalls show
          // full-height sides like Minecraft-style flowing water.
          val fullAbove = localBlock(lx, y + 1, lz) == Block.Water
          val nwTop = if fullAbove then fy + 1f else waterCornerY(Array((lx, y, lz), (lx - 1, y, lz), (lx, y, lz - 1), (lx - 1, y, lz - 1)), fy, topY)
          val neTop = if fullAbove then fy + 1f else waterCornerY(Array((lx, y, lz), (lx + 1, y, lz), (lx, y, lz - 1), (lx + 1, y, lz - 1)), fy, topY)
          val seTop = if fullAbove then fy + 1f else waterCornerY(Array((lx, y, lz), (lx + 1, y, lz), (lx, y, lz + 1), (lx + 1, y, lz + 1)), fy, topY)
          val swTop = if fullAbove then fy + 1f else waterCornerY(Array((lx, y, lz), (lx - 1, y, lz), (lx, y, lz + 1), (lx - 1, y, lz + 1)), fy, topY)
          if !fullAbove then
            addFace(1.00f, Array((fx, nwTop, fz, 0f, 0f), (fx + 1, neTop, fz, 1f, 0f), (fx + 1, seTop, fz + 1, 1f, 1f), (fx, swTop, fz + 1, 0f, 1f)), FaceKind.Top)
          // Static generated oceans/lakes are drawn as clean surface water only. Dynamic
          // placed/flowing water gets full sides so waterfalls and streams read as volume.
          if shouldDrawWaterSide(lx, y, lz, lx + 1, y, lz) then
            addFace(0.82f, Array((fx + 1, fy, fz, 0f, 1f), (fx + 1, fy, fz + 1, 1f, 1f), (fx + 1, seTop, fz + 1, 1f, 0f), (fx + 1, neTop, fz, 0f, 0f)), FaceKind.East)
          if shouldDrawWaterSide(lx, y, lz, lx - 1, y, lz) then
            addFace(0.55f, Array((fx, fy, fz + 1, 0f, 1f), (fx, fy, fz, 1f, 1f), (fx, nwTop, fz, 1f, 0f), (fx, swTop, fz + 1, 0f, 0f)), FaceKind.West)
          if shouldDrawWaterSide(lx, y, lz, lx, y, lz + 1) then
            addFace(0.74f, Array((fx + 1, fy, fz + 1, 0f, 1f), (fx, fy, fz + 1, 1f, 1f), (fx, swTop, fz + 1, 1f, 0f), (fx + 1, seTop, fz + 1, 0f, 0f)), FaceKind.South)
          if shouldDrawWaterSide(lx, y, lz, lx, y, lz - 1) then
            addFace(0.52f, Array((fx, fy, fz, 0f, 1f), (fx + 1, fy, fz, 1f, 1f), (fx + 1, neTop, fz, 1f, 0f), (fx, nwTop, fz, 0f, 0f)), FaceKind.North)
        else
          if isVisible(lx, y + 1, lz, block) then
            addFace(1.00f, Array((fx, topY, fz, 0f, 0f), (fx + 1, topY, fz, 1f, 0f), (fx + 1, topY, fz + 1, 1f, 1f), (fx, topY, fz + 1, 0f, 1f)), FaceKind.Top, block == Block.Grass)
          if isVisible(lx, y - 1, lz, block) then
            addFace(0.38f, Array((fx, fy, fz + 1, 0f, 0f), (fx + 1, fy, fz + 1, 1f, 0f), (fx + 1, fy, fz, 1f, 1f), (fx, fy, fz, 0f, 1f)), FaceKind.Bottom)
          if isVisible(lx + 1, y, lz, block) then
            val corners = Array((fx + 1, fy, fz, 0f, 1f), (fx + 1, fy, fz + 1, 1f, 1f), (fx + 1, topY, fz + 1, 1f, 0f), (fx + 1, topY, fz, 0f, 0f))
            if block == Block.Grass then addGrassSide(0.82f, corners, FaceKind.East) else addFace(0.82f, corners, FaceKind.East)
          if isVisible(lx - 1, y, lz, block) then
            val corners = Array((fx, fy, fz + 1, 0f, 1f), (fx, fy, fz, 1f, 1f), (fx, topY, fz, 1f, 0f), (fx, topY, fz + 1, 0f, 0f))
            if block == Block.Grass then addGrassSide(0.55f, corners, FaceKind.West) else addFace(0.55f, corners, FaceKind.West)
          if isVisible(lx, y, lz + 1, block) then
            val corners = Array((fx + 1, fy, fz + 1, 0f, 1f), (fx, fy, fz + 1, 1f, 1f), (fx, topY, fz + 1, 1f, 0f), (fx + 1, topY, fz + 1, 0f, 0f))
            if block == Block.Grass then addGrassSide(0.74f, corners, FaceKind.South) else addFace(0.74f, corners, FaceKind.South)
          if isVisible(lx, y, lz - 1, block) then
            val corners = Array((fx, fy, fz, 0f, 1f), (fx + 1, fy, fz, 1f, 1f), (fx + 1, topY, fz, 1f, 0f), (fx, topY, fz, 0f, 0f))
            if block == Block.Grass then addGrassSide(0.52f, corners, FaceKind.North) else addFace(0.52f, corners, FaceKind.North)
    if disposed then
      pendingData = null
      pendingRevision = -1
      meshReady = false
      releaseMeshQueue()
    else
      pendingData = Array(opaqueVerts, cutoutVerts, translucentVerts, waterVerts)
      pendingRevision = buildRevision
      meshReady = true

  // Must be called on the GL thread. Returns false if the mesh was stale and
  // should be queued again.
  def uploadMesh(): Boolean =
    val pd = pendingData
    if disposed then
      pendingData = null; pendingRevision = -1; meshReady = false; releaseMeshQueue()
      false
    else if pd != null && pendingRevision == meshRevision then
      destroy()
      val (ov, oc) = upload(pd(0)); opaqueVbo = ov; opaqueCount = oc
      val (cv, cc) = upload(pd(1)); cutoutVbo = cv; cutoutCount = cc
      val (tv, tc) = upload(pd(2)); translucentVbo = tv; translucentCount = tc
      val (wv, wc) = upload(pd(3)); waterVbo = wv; waterCount = wc
      pendingData = null; pendingRevision = -1; meshReady = false; releaseMeshQueue()
      true
    else
      pendingData = null; pendingRevision = -1; meshReady = false; releaseMeshQueue()
      false

  // Emergency path for the nearest chunks: build and upload a mesh on the GL thread.
  // This is intentionally used only for the spawn/camera neighborhood so the player never
  // sees an empty sky-only world while async workers catch up.
  def buildNowAndUpload(): Boolean =
    if disposed then false
    else if meshReady then uploadMesh()
    else if tryQueueMesh() then
      computeMesh()
      if meshReady then uploadMesh() else false
    else false

  // Main-thread emergency path for the local camera ring. This intentionally
  // ignores a stale queued flag because v17 runs chunk meshing on the GL thread;
  // if a chunk is queued but not built yet, showing a dark void is worse than
  // rebuilding it immediately. Bumping the revision invalidates any old pending
  // mesh result before creating a fresh one.
  def forceBuildNowAndUpload(): Boolean =
    if disposed then false
    else
      pendingData = null
      pendingRevision = -1
      meshReady = false
      meshQueued.set(false)
      markDirtyMesh()
      if tryQueueMesh() then
        computeMesh()
        if meshReady then uploadMesh() else false
      else false

  // Batched rendering: caller manages state, chunks just draw their VBOs.
  // Vertices are chunk-local; the per-chunk transform is camera-relative.
  def drawOpaque(camera: Vec3): Unit = if opaqueCount > 0 then drawBufferAt(opaqueVbo, opaqueCount, camera)
  def drawCutout(camera: Vec3): Unit = if cutoutCount > 0 then drawBufferAt(cutoutVbo, cutoutCount, camera)
  def drawTranslucent(camera: Vec3): Unit = if translucentCount > 0 then drawBufferAt(translucentVbo, translucentCount, camera)
  def drawWater(camera: Vec3): Unit = if waterCount > 0 then drawBufferAt(waterVbo, waterCount, camera)

  def destroy(): Unit =
    if opaqueVbo != 0 then glDeleteBuffers(opaqueVbo); opaqueVbo = 0; opaqueCount = 0
    if cutoutVbo != 0 then glDeleteBuffers(cutoutVbo); cutoutVbo = 0; cutoutCount = 0
    if translucentVbo != 0 then glDeleteBuffers(translucentVbo); translucentVbo = 0; translucentCount = 0
    if waterVbo != 0 then glDeleteBuffers(waterVbo); waterVbo = 0; waterCount = 0

  def save(dir: java.io.File): Unit =
    BlockboxFiles.ensureDirectory(dir.toPath)
    val file = new java.io.File(dir, s"chunk_${cx}_${cz}.dat")
    BlockboxFiles.writeAtomic(file.toPath, out0 =>
      val out = new java.io.DataOutputStream(new java.io.BufferedOutputStream(out0))
      out.write(blocks)
      val editCopy = edits.synchronized { edits.toList }
      out.writeInt(editCopy.size)
      editCopy.foreach { case ((lx, ly, lz), block) =>
        out.writeInt(lx); out.writeInt(ly); out.writeInt(lz)
        out.writeByte(block.id)
      }
      val waterEntries = ArrayBuffer.empty[(Int, Byte)]
      var wi = 0
      while wi < waterLevelsLocal.length do
        val level = waterLevelsLocal(wi)
        if level != 0 then waterEntries += ((wi, level))
        wi += 1
      out.writeInt(waterExtraMagic)
      out.writeInt(waterEntries.length)
      waterEntries.foreach { case (i, level) =>
        out.writeInt(i)
        out.writeByte(level)
      }
      out.flush()
    )

  def load(dir: java.io.File): Unit =
    val file = new java.io.File(dir, s"chunk_${cx}_${cz}.dat")
    if file.exists() then
      val in = new java.io.DataInputStream(new java.io.BufferedInputStream(new java.io.FileInputStream(file)))
      try
        val loadedBlocks = new Array[Byte](Terrain.chunkSize * Terrain.worldHeight * Terrain.chunkSize)
        in.readFully(loadedBlocks)
        // Fast load path: use the saved block array and reapply edits below.
        // Regenerating every saved chunk on load fixed old broken worlds once, but it was too
        // expensive while walking/hosting. New worlds already use the fixed generator; old cursed
        // chunk files should be deleted once instead of regenerated every frame.
        blocks = loadedBlocks
        edits.synchronized {
          edits.clear()
          val editsCount = in.readInt().max(0).min(Terrain.chunkSize * Terrain.worldHeight * Terrain.chunkSize)
          for _ <- 0 until editsCount do
            val lx = in.readInt(); val ly = in.readInt(); val lz = in.readInt()
            val blockId = in.readByte()
            if lx >= 0 && lx < Terrain.chunkSize && ly >= 0 && ly < Terrain.worldHeight && lz >= 0 && lz < Terrain.chunkSize then
              val b = Block.fromId(blockId)
              edits((lx, ly, lz)) = b
              blocks((ly * Terrain.chunkSize + lz) * Terrain.chunkSize + lx) = b.id
        }
        java.util.Arrays.fill(waterLevelsLocal, 0.toByte)
        if in.available() >= 8 then
          val magic = in.readInt()
          if magic == waterExtraMagic then
            val waterCount = in.readInt().max(0).min(waterLevelsLocal.length)
            for _ <- 0 until waterCount do
              val wi = in.readInt()
              val level = in.readByte()
              if wi >= 0 && wi < waterLevelsLocal.length then waterLevelsLocal(wi) = (level.toInt & 0xFF).max(0).min(8).toByte
        normalizeStaticNaturalWater()
        markDirtyMesh()
      finally in.close()

  private def upload(data: ArrayBuffer[Float]): (Int, Int) =
    if data == null || data.isEmpty then (0, 0)
    else
      val buffer = BufferUtils.createFloatBuffer(data.length)
      data.foreach(buffer.put); buffer.flip()
      val vbo = glGenBuffers()
      glBindBuffer(GL_ARRAY_BUFFER, vbo)
      glBufferData(GL_ARRAY_BUFFER, buffer, GL_STATIC_DRAW)
      glBindBuffer(GL_ARRAY_BUFFER, 0)
      (vbo, buffer.limit() / floatsPerVertex)

  private def drawBufferAt(vbo: Int, count: Int, camera: Vec3): Unit =
    if vbo != 0 && count > 0 then
      val stride = floatsPerVertex * 4
      glPushMatrix()
      glTranslatef(baseX.toFloat - camera.x, 0f, baseZ.toFloat - camera.z)
      glBindBuffer(GL_ARRAY_BUFFER, vbo)
      glVertexPointer(3, GL_FLOAT, stride, 0L)
      glColorPointer(4, GL_FLOAT, stride, 12L)
      glTexCoordPointer(2, GL_FLOAT, stride, 28L)
      glDrawArrays(GL_TRIANGLES, 0, count)
      glPopMatrix()
