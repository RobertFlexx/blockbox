package blockbox

import blockbox.io.BlockboxSaveFiles
import org.lwjgl.BufferUtils
import org.lwjgl.opengl.GL11.*
import org.lwjgl.opengl.GL13.*
import org.lwjgl.opengl.GL15.*
import scala.collection.mutable.ArrayBuffer
import scala.math.*

/** Immutable chunk state captured on the game thread and encoded on the save worker. Keeping
  * serialization out of Save-and-Quit makes the UI transition independent of world size while still
  * guaranteeing that disposed chunks cannot mutate the save.
  */
final case class ChunkSaveSnapshot(
    blocks: Array[Byte],
    edits: List[((Int, Int, Int), Byte)],
    water: Array[Byte],
    logAxes: List[(Int, Byte)],
    torchMounts: List[(Int, Byte)] = Nil
):
  def toBytes(): Array[Byte] =
    val raw = new java.io.ByteArrayOutputStream(blocks.length + edits.size * 16 + 1024)
    val out = new java.io.DataOutputStream(new java.io.BufferedOutputStream(raw, 32 * 1024))
    out.write(blocks)
    out.writeInt(edits.size)
    edits.foreach { case ((lx, ly, lz), blockId) =>
      out.writeInt(lx); out.writeInt(ly); out.writeInt(lz); out.writeByte(blockId)
    }
    var waterCount = 0
    var wi = 0
    while wi < water.length do
      if water(wi) != 0 then waterCount += 1
      wi += 1
    out.writeInt(0x574c5631) // "WLV1"
    out.writeInt(waterCount)
    wi = 0
    while wi < water.length do
      val level = water(wi)
      if level != 0 then
        out.writeInt(wi); out.writeByte(level)
      wi += 1
    // Optional sparse log-axis tail. Only non-Y axes are stored, so old chunk
    // files (which lack this section entirely) load every log as vertical.
    out.writeInt(0x4c415831) // "LAX1"
    out.writeInt(logAxes.length)
    logAxes.foreach { case (flatIndex, axisId) =>
      out.writeInt(flatIndex); out.writeByte(axisId)
    }
    out.writeInt(0x54524331) // "TRC1": explicit floor/wall attachments
    out.writeInt(torchMounts.length)
    torchMounts.foreach { case (i, mount) => out.writeInt(i); out.writeByte(mount) }
    out.flush()
    raw.toByteArray

/** Growable vertex buffer for worker-side meshing. A generic ArrayBuffer[Float] boxed every
  * component and made chunk streaming allocate millions of short-lived objects.
  */
private final class FloatMeshBuilder(initialCapacity: Int):
  private var values = new Array[Float](initialCapacity.max(256))
  private var used = 0

  def size: Int = used
  def isEmpty: Boolean = used == 0
  def reset(): Unit = used = 0

  def +=(value: Float): FloatMeshBuilder =
    if used >= values.length then
      val next = new Array[Float]((values.length * 2).max(used + 1))
      System.arraycopy(values, 0, next, 0, used)
      values = next
    values(used) = value
    used += 1
    this

  def putInto(buffer: java.nio.FloatBuffer): Unit =
    buffer.put(values, 0, used)

  /** Exact-size copy the GL thread can upload after this builder is reused. */
  def compact(): Array[Float] =
    if used == 0 then FloatMeshBuilder.Empty else java.util.Arrays.copyOf(values, used)

private object FloatMeshBuilder:
  val Empty: Array[Float] = new Array[Float](0)

/** Per-worker mesh scratch. Chunk streaming used to allocate the neighbour water copies, the
  * foliage mask, the lighting tables and the vertex builders on every chunk, which triggered
  * young-gen pauses long enough to underrun the audio device. The scratch is thread-local, so
  * two workers never share a buffer, and each finished mesh publishes its own compact copy.
  */
private object ChunkMeshWorkspace:
  final class Buffers:
    val opaque = FloatMeshBuilder(131072)
    val cutout = FloatMeshBuilder(32768)
    val translucent = FloatMeshBuilder(16384)
    val water = FloatMeshBuilder(65536)
    val flatWater = scala.collection.mutable.HashMap.empty[Float, Int]
    val lights = ArrayBuffer.empty[BlockLight.Source]
    val face = new Array[Float](20)
    val uv = new Array[Float](2)
    val tint = new Array[Float](3)
    val loaded = new Array[Boolean](9)
    val waterView = new Array[Array[Byte]](9)
    val waterStore = Array.tabulate(9)(_ =>
      new Array[Byte](Terrain.chunkSize * Terrain.worldHeight * Terrain.chunkSize)
    )
    val foliageExposure =
      new Array[Byte](ChunkLighting.span * ChunkLighting.span * Terrain.worldHeight)
    val frostColumns = new Array[Byte](Terrain.chunkSize * Terrain.chunkSize)
    val cornerSky = new Array[Float](4)
    val cornerBlock = new Array[Float](4)
    val cornerAo = new Array[Float](4)
    val opaqueById = new Array[Boolean](256)
    val occluderById = new Array[Boolean](256)
    val skyCost = new Array[Int](256)
    val leafOpeningById = new Array[Boolean](256)
    val quadOrder = Array(0, 1, 2, 2, 3, 0)
    val flipOrder = Array(0, 1, 3, 1, 2, 3)
    def resetBuilders(): Unit =
      opaque.reset(); cutout.reset(); translucent.reset(); water.reset()

  val local: ThreadLocal[Buffers] = ThreadLocal.withInitial(() => Buffers())
  private val uploadBuffers = ThreadLocal.withInitial(() => BufferUtils.createFloatBuffer(4096))

  def uploadBuffer(samples: Int): java.nio.FloatBuffer =
    var buffer = uploadBuffers.get()
    if buffer.capacity < samples then
      buffer = BufferUtils.createFloatBuffer(samples)
      uploadBuffers.set(buffer)
    buffer.clear()
    buffer

final class Chunk(
    val cx: Int,
    val cz: Int,
    initialAtlas: TextureAtlas,
    gen: TerrainGenerator,
    initialBlocks: Array[Byte] = null,
    private var smoothLightingEnabled: Boolean = true
):
  @volatile private var atlas = initialAtlas
  @volatile private var snowCoverStep = 0
  private val snowClimate = (for x <- Vector(0, 8, 15); z <- Vector(0, 8, 15)
  yield SnowCover.cold(
    gen.biomeAt(cx * 16 + x, cz * 16 + z, gen.heightAt(cx * 16 + x, cz * 16 + z))
  )).exists(identity)
  def needsSnowCover(amount: Float): Boolean =
    snowClimate && snowCoverStep != SnowCover.level(amount)
  def setSnowCover(amount: Float): Unit =
    val next = SnowCover.level(amount)
    if snowClimate && next != snowCoverStep then { snowCoverStep = next; markDirtyMesh() }
  private val generated =
    if initialBlocks != null then
      (initialBlocks, scala.collection.immutable.Map.empty[Int, LogAxis])
    else gen.generateChunk(cx, cz)
  var blocks: Array[Byte] = generated._1
  // Sparse Minecraft-style log orientation keyed by local flat index. Absence means
  // Y (vertical), matching standing trees and every legacy saved/logged chunk.
  private val logAxes = scala.collection.mutable.HashMap.from(generated._2)
  private val torchMounts = scala.collection.mutable.HashMap.empty[Int, TorchMount]
  private val lightSourcesLocal = scala.collection.mutable.HashMap.empty[Int, BlockLight.Source]
  private val legacyTorchPositions = scala.collection.mutable.HashSet.empty[Int]
  private[blockbox] val edits = scala.collection.mutable.HashMap.empty[(Int, Int, Int), Block]
  @volatile private var neighbors = Map.empty[(Int, Int), Chunk]
  // 0 means default full-height water for generated/static water.
  // 1..8 are dynamic flowing-water levels used by the cellular automata.
  private val waterLevelsLocal =
    new Array[Byte](Terrain.chunkSize * Terrain.worldHeight * Terrain.chunkSize)
  private val waterExtraMagic = 0x574c5631 // "WLV1"

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

  // position(3) + color(4) + normal(3) + atlas UV(2) + Minecraft-style
  // lightmap coordinates(3). UV1.xy carries block/sky light on a 0..240 scale.
  // UV1.z carries the baked compatibility face shade, or the water wave weight.
  // HDR packs remove that face shade before applying directional lighting;
  // compatibility rendering retains it. Legacy pack lightmap UV1.xy stays valid.
  private val floatsPerVertex = 15
  private var opaqueVbo = 0; private var opaqueCount = 0
  private var cutoutVbo = 0; private var cutoutCount = 0
  private var translucentVbo = 0; private var translucentCount = 0
  private var waterVbo = 0; private var waterCount = 0
  // Published with the mesh, so reflection selection never scans live voxels.
  private var waterPlane: Option[(Float, Int)] = None
  @volatile private var pendingWaterPlane: Option[(Float, Int)] = None
  def flatWaterPlane: Option[(Float, Int)] = waterPlane

  // Threaded mesh data: worker fills these, main thread uploads them.
  // The mesh queue flag/revision pair prevents the same chunk from being meshed by
  // multiple worker threads at once. That was the big source of the "random raised
  // chunk / ugly artifact" looking glitches: stale meshes could finish after newer
  // world edits or after a chunk was unloaded.
  @volatile private var pendingData: Array[Array[Float]] = null
  @volatile private var pendingRevision = -1
  @volatile var meshReady: Boolean = false
  @volatile private var disposed = false
  @volatile private var meshRevision = 0
  private val meshQueued = java.util.concurrent.atomic.AtomicBoolean(false)

  normalizeStaticNaturalWater()
  if initialBlocks != null then rebuildLightSources()

  def hasMesh: Boolean =
    opaqueCount > 0 || cutoutCount > 0 || translucentCount > 0 || waterCount > 0

  def isDisposed: Boolean = disposed

  def markDirtyMesh(): Unit =
    meshRevision += 1

  /** Bumps when the voxel contents change, so the ray-tracing geometry can rebuild just that chunk.
    */
  def geometryRevision: Int = meshRevision

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

  /** Texture packs can change tile size and grass fringe depth, so meshes follow the active atlas.
    */
  private[blockbox] def setAtlas(next: TextureAtlas): Unit =
    if next != null && (next ne atlas) then
      atlas = next
      markDirtyMesh()

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
  private def localIndex(lx: Int, y: Int, lz: Int): Int =
    (y * Terrain.chunkSize + lz) * Terrain.chunkSize + lx

  private def validLocal(lx: Int, y: Int, lz: Int): Boolean =
    lx >= 0 && lx < Terrain.chunkSize && y >= 0 && y < Terrain.worldHeight && lz >= 0 && lz < Terrain.chunkSize

  def getBlock(lx: Int, y: Int, lz: Int): Block =
    val found: Option[Block] = edits.synchronized { edits.get((lx, y, lz)) }
    found match
      case Some(b: Block) => b
      case None           =>
        if validLocal(lx, y, lz) then Block.fromId(blocks(localIndex(lx, y, lz)))
        else Block.Air

  /** Minecraft-style log orientation for a voxel. Defaults to Y for non-log blocks and for any log
    * without an explicit entry (standing trees, older saves).
    */
  def getLogAxis(lx: Int, y: Int, lz: Int): LogAxis =
    if validLocal(lx, y, lz) && Block.isLog(getBlock(lx, y, lz)) then
      logAxes.getOrElse(localIndex(lx, y, lz), LogAxis.Y)
    else LogAxis.Y

  def getTorchMount(lx: Int, y: Int, lz: Int): TorchMount =
    if validLocal(lx, y, lz) && getBlock(lx, y, lz) == Block.Torch then
      torchMounts.synchronized { torchMounts.getOrElse(localIndex(lx, y, lz), TorchMount.Floor) }
    else TorchMount.Floor

  private def rebuildLightSources(): Unit = lightSourcesLocal.synchronized {
    lightSourcesLocal.clear()
    var i = 0
    while i < blocks.length do
      val level = BlockLight.emission(Block.fromId(blocks(i)))
      if level > 0 then
        lightSourcesLocal(i) = BlockLight.Source(
          i % Terrain.chunkSize,
          i / (Terrain.chunkSize * Terrain.chunkSize),
          (i / Terrain.chunkSize) % Terrain.chunkSize,
          level
        )
      i += 1
  }

  private[blockbox] def lightSourcesSnapshot: Vector[BlockLight.Source] =
    lightSourcesLocal.synchronized { lightSourcesLocal.values.toVector }

  private[blockbox] def foreachLightSource(f: BlockLight.Source => Unit): Unit =
    lightSourcesLocal.synchronized { lightSourcesLocal.values.foreach(f) }

  // Light from the last finished mesh, one byte per cell: sky level in the high nibble,
  // block level in the low one. Players, particles and the held item sample it so they
  // sit in the same light as the terrain around them.
  @volatile private var packedLight: Array[Byte] = null

  /** Sky and block light (0..15 each) at a local cell, or None before the first mesh. */
  def lightAt(lx: Int, y: Int, lz: Int): Option[(Int, Int)] =
    val data = packedLight
    if data == null || lx < 0 || lx >= Terrain.chunkSize || lz < 0 || lz >= Terrain.chunkSize then
      None
    else if y >= Terrain.worldHeight then Some((15, 0))
    else if y < 0 then Some((0, 0))
    else
      val v = data(localIndex(lx, y, lz)) & 0xff
      Some((v >> 4, v & 15))

  private[blockbox] def torchPositions: Vector[(Int, Int, Int)] =
    lightSourcesSnapshot.filter(_.level == 15).map(s => (s.x, s.y, s.z))

  private[blockbox] def torchAttachmentResolved(x: Int, y: Int, z: Int): Boolean =
    !legacyTorchPositions.contains(localIndex(x, y, z))

  /** Called after neighbor links exist, so old wall torches on a chunk seam migrate too. */
  private[blockbox] def migrateLegacyTorches(at: (Int, Int, Int) => Option[Block]): Unit =
    legacyTorchPositions.toList.foreach { i =>
      val x = i % Terrain.chunkSize; val y = i / (Terrain.chunkSize * Terrain.chunkSize);
      val z = (i / Terrain.chunkSize) % Terrain.chunkSize
      val candidates = TorchMount.values.filter(m =>
        at(baseX + x + m.support._1, y + m.support._2, baseZ + z + m.support._3)
          .exists(TorchMount.canSupport)
      )
      candidates.headOption match
        case Some(mount) =>
          torchMounts.synchronized { torchMounts(i) = mount }
          legacyTorchPositions.remove(i)
          markDirtyMesh()
        case None =>
          if TorchMount.values.forall(m =>
              at(baseX + x + m.support._1, y + m.support._2, baseZ + z + m.support._3).nonEmpty
            )
          then legacyTorchPositions.remove(i)
    }

  def getWaterLevel(lx: Int, y: Int, lz: Int): Int =
    if validLocal(lx, y, lz) && getBlock(lx, y, lz) == Block.Water then
      val raw = waterLevelsLocal(localIndex(lx, y, lz)).toInt & 0xff
      if raw <= 0 then 8 else raw.max(1).min(8)
    else 0

  def getWaterRawLevel(lx: Int, y: Int, lz: Int): Int =
    if validLocal(lx, y, lz) && getBlock(lx, y, lz) == Block.Water then
      waterLevelsLocal(localIndex(lx, y, lz)).toInt & 0xff
    else 0

  def foreachDynamicWaterCell(f: (Int, Int, Int, Int) => Unit): Unit =
    var y = 0
    while y < Terrain.worldHeight do
      var lz = 0
      while lz < Terrain.chunkSize do
        var lx = 0
        while lx < Terrain.chunkSize do
          val i = localIndex(lx, y, lz)
          val raw = waterLevelsLocal(i).toInt & 0xff
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

  def setBlock(
      lx: Int,
      y: Int,
      lz: Int,
      block: Block,
      axis: LogAxis = LogAxis.Y,
      torchMount: TorchMount = TorchMount.Floor
  ): Unit =
    if validLocal(lx, y, lz) then
      val i = localIndex(lx, y, lz)
      val oldId = blocks(i)
      val oldAxis =
        if Block.isLog(Block.fromId(oldId)) then logAxes.getOrElse(i, LogAxis.Y) else LogAxis.Y
      val oldMount = torchMounts.synchronized { torchMounts.getOrElse(i, TorchMount.Floor) }
      blocks(i) = block.id
      if block == Block.Water && waterLevelsLocal(i) == 0 then waterLevelsLocal(i) = 8.toByte
      if block != Block.Water then waterLevelsLocal(i) = 0.toByte
      if Block.isLog(block) then if axis != LogAxis.Y then logAxes(i) = axis else logAxes.remove(i)
      else logAxes.remove(i)
      torchMounts.synchronized {
        if block == Block.Torch then torchMounts(i) = torchMount else torchMounts.remove(i)
      }
      legacyTorchPositions.remove(i)
      lightSourcesLocal.synchronized {
        val level = BlockLight.emission(block)
        if level > 0 then lightSourcesLocal(i) = BlockLight.Source(lx, y, lz, level)
        else lightSourcesLocal.remove(i)
      }
      edits.synchronized { edits((lx, y, lz)) = block }
      val axisChanged = Block.isLog(block) && oldAxis != axis
      if oldId != block.id || axisChanged || (block == Block.Torch && oldMount != torchMount) then
        markDirtyMesh()

  // Thread-safe: called by worker thread, no GL calls. Unknown neighbor-chunk
  // space is treated as air and borders are rebuilt when neighbors load/change,
  // which prevents caves/coastlines from losing visible faces at chunk seams.
  def computeMesh(): Unit =
    if disposed then
      meshReady = false
      releaseMeshQueue()
      return
    val buildRevision = meshRevision
    val ws = ChunkMeshWorkspace.local.get()
    ws.resetBuilders()
    val opaqueVerts = ws.opaque
    val cutoutVerts = ws.cutout
    val translucentVerts = ws.translucent
    val waterVerts = ws.water
    val flatWaterFaces = ws.flatWater
    flatWaterFaces.clear()
    val cs = Terrain.chunkSize
    val height = Terrain.worldHeight
    val smooth = smoothLightingEnabled

    // The main thread keeps editing blocks (water flow, block edits, multiplayer BLOC
    // packets) while workers mesh. Every lookup below reads one dense snapshot of this
    // chunk and its eight neighbours, so a build is deterministic for its revision and
    // sky light can flood across chunk seams. `blocks` always mirrors `edits`.
    val scratch = ChunkLighting.scratch.get()
    val span = ChunkLighting.span
    val layer = span * span
    val region = scratch.blocks
    java.util.Arrays.fill(region, Block.Air.id)
    val loaded = ws.loaded
    java.util.Arrays.fill(loaded, false)
    val waterSnapshots = ws.waterView
    var waterSlot = 0
    while waterSlot < waterSnapshots.length do
      waterSnapshots(waterSlot) = null
      waterSlot += 1
    val lightSources = ws.lights
    lightSources.clear()
    foreachLightSource(source => lightSources += source)
    def capture(source: Array[Byte], water: Array[Byte], dx: Int, dz: Int): Unit =
      val slot = (dz + 1) * 3 + dx + 1
      loaded(slot) = true
      val stored = ws.waterStore(slot)
      System.arraycopy(water, 0, stored, 0, water.length)
      waterSnapshots(slot) = stored
      var y = 0
      while y < height do
        var z = 0
        while z < cs do
          System.arraycopy(
            source,
            (y * cs + z) * cs,
            region,
            y * layer + ((dz + 1) * cs + z) * span + (dx + 1) * cs,
            cs
          )
          z += 1
        y += 1
    capture(blocks, waterLevelsLocal, 0, 0)
    neighbors.foreach { case ((dx, dz), chunk) =>
      if chunk != null && !chunk.isDisposed then
        capture(chunk.blocks, chunk.waterLevelsLocal, dx, dz)
        chunk.foreachLightSource(s =>
          lightSources += s.copy(x = s.x + dx * cs, z = s.z + dz * cs)
        )
    }
    val logAxesSnapshot = logAxes.clone()
    val torchMountsSnapshot = torchMounts.synchronized { torchMounts.toMap }

    def inRegion(x: Int, z: Int): Boolean = x >= -cs && x < 2 * cs && z >= -cs && z < 2 * cs
    def cell(x: Int, y: Int, z: Int): Int = y * layer + (z + cs) * span + x + cs
    def chunkSlot(x: Int, z: Int): Int = (Math.floorDiv(z, cs) + 1) * 3 + Math.floorDiv(x, cs) + 1
    def localBlock(x: Int, y: Int, z: Int): Block =
      if y < 0 || y >= height || !inRegion(x, z) then Block.Air
      else Block.fromId(region(cell(x, y, z)))
    def localLogAxis(x: Int, y: Int, z: Int): LogAxis =
      logAxesSnapshot.getOrElse(localIndex(x, y, z), LogAxis.Y)
    def localWaterRawLevel(x: Int, y: Int, z: Int): Int =
      if y < 0 || y >= height || !inRegion(x, z) then 0
      else
        val water = waterSnapshots(chunkSlot(x, z))
        if water == null then 0
        else water(localIndex(Math.floorMod(x, cs), y, Math.floorMod(z, cs))).toInt & 0xff

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
        // plane. Every natural body shares this same slightly recessed surface.
        fy + WaterSurface.height(raw)
    def waterCornerY(
        yCell: Int,
        x0: Int,
        z0: Int,
        x1: Int,
        z1: Int,
        x2: Int,
        z2: Int,
        x3: Int,
        z3: Int,
        fy: Float,
        fallback: Float
    ): Float =
      var sum = 0f
      var count = 0
      var hasFullColumn = false
      def consider(cx: Int, cz: Int): Unit =
        if localBlock(cx, yCell, cz) == Block.Water then
          if localBlock(cx, yCell + 1, cz) == Block.Water then hasFullColumn = true
          sum += rawWaterTopY(cx, yCell, cz, fy)
          count += 1
      consider(x0, z0); consider(x1, z1); consider(x2, z2); consider(x3, z3)
      if hasFullColumn then fy + 1f
      else if count > 0 then sum / count.toFloat
      else fallback
    def isWaterSideVisible(nlx: Int, ny: Int, nlz: Int): Boolean =
      if ny < 0 || ny >= Terrain.worldHeight then false
      else if !inRegion(nlx, nlz) || !loaded(chunkSlot(nlx, nlz)) then false
      else
        val nb = localBlock(nlx, ny, nlz)
        nb != Block.Water && (nb == Block.Air || nb.cutout || (nb.translucent && nb != Block.Water))

    def shouldDrawWaterSide(lx: Int, y: Int, lz: Int, nlx: Int, ny: Int, nlz: Int): Boolean =
      isWaterSideVisible(nlx, ny, nlz)
    val foliageExposure = ws.foliageExposure
    java.util.Arrays.fill(foliageExposure, 0.toByte)
    val leafKinds = Block.values
    val leafOpeningById = ws.leafOpeningById
    java.util.Arrays.fill(leafOpeningById, true)
    var leafId = 0
    while leafId < leafKinds.length do
      val b = leafKinds(leafId)
      leafOpeningById(leafId) = !BlockTints.isFoliage(b) && (!b.solid || b.translucent || b.cutout)
      leafId += 1
    def leafOpening(x: Int, y: Int, z: Int): Boolean =
      leafOpeningById(localBlock(x, y, z).id & 0xff)
    def boundaryLeaf(x: Int, y: Int, z: Int): Boolean =
      val i = cell(x, y, z)
      if foliageExposure(i) == 0 then
        val boundary = leafOpening(x - 1, y, z) || leafOpening(x + 1, y, z) ||
          leafOpening(x, y - 1, z) || leafOpening(x, y + 1, z) ||
          leafOpening(x, y, z - 1) || leafOpening(x, y, z + 1)
        foliageExposure(i) = if boundary then 2 else 1
      foliageExposure(i) == 2
    def isVisible(nlx: Int, ny: Int, nlz: Int, block: Block): Boolean =
      val nb = localBlock(nlx, ny, nlz)
      if nb == Block.Air then true
      else if block == Block.Water then isWaterSideVisible(nlx, ny, nlz)
      else if nb == Block.Water then true
      // Keep one interior layer behind cutouts. A hollow shell exposes the sky
      // through the entire canopy and its reflection; buried core faces stay culled.
      else if BlockTints.isFoliage(block) && BlockTints.isFoliage(nb) then
        boundaryLeaf(nlx, ny, nlz)
      else if nb.cutout && !nb.solid then true
      else if block.cutout && nb.cutout then true
      else if nb.cutout then block != nb
      else if block.solid && nb.solid && !block.translucent && !nb.translucent && !block.cutout && !nb.cutout
      then false
      else nb.translucent && nb != block
    // Per-id lighting properties. Foliage is not opaque (it dims light and never turns a
    // face black) but still darkens the corners next to it like a solid neighbour.
    val kinds = Block.values
    val opaqueById = ws.opaqueById
    val occluderById = ws.occluderById
    val skyCost = ws.skyCost
    java.util.Arrays.fill(opaqueById, false)
    java.util.Arrays.fill(occluderById, false)
    java.util.Arrays.fill(skyCost, 0)
    var id = 0
    while id < kinds.length do
      val b = kinds(id)
      val opaque = b.solid && !b.translucent && !b.cutout
      opaqueById(id) = opaque
      occluderById(id) = b.solid && !b.translucent
      skyCost(id) =
        if opaque then 15 else if b == Block.Water || (b.solid && b.cutout) then 1 else 0
      id += 1
    ChunkLighting.floodSky(scratch, skyCost, height)
    val sky = scratch.sky
    val blockLight = BlockLight.build(lightSources.toSeq, cs, height, localBlock)
    def opaqueAt(x: Int, y: Int, z: Int): Boolean =
      y >= 0 && y < height && inRegion(x, z) && opaqueById(region(cell(x, y, z)) & 0xff)
    def occluderAt(x: Int, y: Int, z: Int): Boolean =
      y >= 0 && y < height && inRegion(x, z) && occluderById(region(cell(x, y, z)) & 0xff)
    def skyAt(x: Int, y: Int, z: Int): Int =
      if y >= height then 15
      else if y < 0 then 0
      else if !inRegion(x, z) then 15
      else sky(cell(x, y, z)).toInt
    def aoFactor(sideA: Boolean, sideB: Boolean, corner: Boolean): Float =
      val level = if sideA && sideB then 3
      else (if sideA then 1 else 0) + (if sideB then 1 else 0) + (if corner then 1 else 0)
      level match
        case 0 => 1.00f
        case 1 => 0.80f
        case 2 => 0.64f
        case _ => 0.50f

    // Smooth lighting: each vertex averages the cell in front of the face with the cells
    // around that corner. Opaque cells are skipped, and a diagonal hidden behind two
    // opaque sides cannot leak light around the edge.
    val cornerSky = ws.cornerSky
    val cornerBlock = ws.cornerBlock
    val cornerAo = ws.cornerAo
    def sampleCorner(
        slot: Int,
        bx: Int,
        by: Int,
        bz: Int,
        kind: FaceKind,
        sx: Int,
        sy: Int,
        sz: Int,
        occlusion: Boolean
    ): Unit =
      var nx = 0; var ny = 0; var nz = 0
      kind match
        case FaceKind.Top    => ny = 1
        case FaceKind.Bottom => ny = -1
        case FaceKind.East   => nx = 1
        case FaceKind.West   => nx = -1
        case FaceKind.South  => nz = 1
        case FaceKind.North  => nz = -1
      val fx = bx + nx; val fy = by + ny; val fz = bz + nz
      if !smooth then
        cornerSky(slot) = skyAt(fx, fy, fz) / 15f
        cornerBlock(slot) = blockLight.level(fx, fy, fz) / 15f
        cornerAo(slot) = 1f
      else
        var ax = 0; var ay = 0; var az = 0; var bx2 = 0; var by2 = 0; var bz2 = 0
        if ny != 0 then { ax = sx; bz2 = sz }
        else if nx != 0 then { ay = sy; bz2 = sz }
        else { ax = sx; by2 = sy }
        val openA = !opaqueAt(fx + ax, fy + ay, fz + az)
        val openB = !opaqueAt(fx + bx2, fy + by2, fz + bz2)
        val dx = fx + ax + bx2; val dy = fy + ay + by2; val dz = fz + az + bz2
        val openD = (openA || openB) && !opaqueAt(dx, dy, dz)
        var skySum = skyAt(fx, fy, fz)
        var blockSum = blockLight.level(fx, fy, fz)
        var count = 1
        if openA then
          skySum += skyAt(fx + ax, fy + ay, fz + az);
          blockSum += blockLight.level(fx + ax, fy + ay, fz + az); count += 1
        if openB then
          skySum += skyAt(fx + bx2, fy + by2, fz + bz2);
          blockSum += blockLight.level(fx + bx2, fy + by2, fz + bz2); count += 1
        if openD then
          skySum += skyAt(dx, dy, dz); blockSum += blockLight.level(dx, dy, dz); count += 1
        cornerSky(slot) = skySum / (15f * count)
        cornerBlock(slot) = blockSum / (15f * count)
        cornerAo(slot) =
          if occlusion then
            aoFactor(
              occluderAt(fx + ax, fy + ay, fz + az),
              occluderAt(fx + bx2, fy + by2, fz + bz2),
              occluderAt(dx, dy, dz)
            )
          else 1f
    def signAt(value: Float, origin: Float): Int = if value <= origin + 0.001f then -1 else 1
    val atlasRef = atlas
    val grassOverlay = atlasRef.hasGrassOverlay
    val quadOrder = ws.quadOrder
    val flipOrder = ws.flipOrder
    // Leaf tops in snowy biomes are frosted white, like Minecraft's snow-dusted spruces.
    // One biome lookup per column: 0 unknown, 1 bare, 2 frosted.
    val frostColumns = ws.frostColumns
    java.util.Arrays.fill(frostColumns, 0.toByte)
    // One predicate for every shoreline corner. Allocating it per water vertex was a
    // large share of the garbage produced while new chunks streamed in.
    def waterCellOpen(wx: Int, hy: Int, wz: Int): Boolean =
      localBlock(wx, hy, wz) == Block.Water && localBlock(wx, hy + 1, wz) != Block.LilyPad
    val waterCell: (Int, Int, Int) => Boolean = waterCellOpen
    val settledSnow = snowCoverStep / 8f
    def frostedColumn(lx: Int, lz: Int, y: Int): Boolean =
      val i = lz * cs + lx
      if frostColumns(i) == 0 then
        val biome = gen.biomeAt(baseX + lx, baseZ + lz, gen.heightAt(baseX + lx, baseZ + lz))
        frostColumns(i) =
          if SnowCover.cold(biome)
          then 2
          else 1
      frostColumns(i) == 2
    for lx <- 0 until cs; lz <- 0 until cs; y <- 0 until height do
      val block = localBlock(lx, y, lz)
      val isLogBlock = Block.isLog(block)
      val logAxis = if isLogBlock then localLogAxis(lx, y, lz) else LogAxis.Y
      if block != Block.Air then
        // Keep chunk mesh vertices local to the chunk. The renderer applies a
        // camera-relative chunk transform, which delays float precision "far lands"
        // artifacts far beyond where full world-coordinate VBOs started wobbling.
        val fx = lx.toFloat; val fy = y.toFloat; val fz = lz.toFloat
        // Water is still one block in storage, but flowing water renders with a variable
        // surface height from the per-chunk level array. Static generated oceans use the
        // default full level without entering the simulation queue.
        val topY = if block == Block.Water then rawWaterTopY(lx, y, lz, fy) else fy + 1f
        val foliage = BlockTints.isFoliage(block)
        val leafRotation = if foliage then
          Math.floorMod((baseX + lx) * 73428767 ^ y * 912931 ^ (baseZ + lz) * 19349663, 4)
        else 0
        def writeGrassTint(cfx: Float, cfz: Float, out: Array[Float]): Unit =
          val sx = floor(baseX.toFloat + cfx).toInt
          val sz = floor(baseZ.toFloat + cfz).toInt
          val color = gen.grassTintAt(sx, sz, y)
          out(0) = color._1; out(1) = color._2; out(2) = color._3
        def writeFoliageTint(cfx: Float, cfz: Float, out: Array[Float]): Unit =
          val sx = floor(baseX.toFloat + cfx).toInt
          val sz = floor(baseZ.toFloat + cfz).toInt
          val color = gen.foliageTintAt(sx, sz)
          BlockTints.writeFoliage(block, color._1, color._2, color._3, out, 0)
        val faceCorners = ws.face
        def putFace(
            x0: Float, y0: Float, z0: Float, u0: Float, v0: Float,
            x1: Float, y1: Float, z1: Float, u1: Float, v1: Float,
            x2: Float, y2: Float, z2: Float, u2: Float, v2: Float,
            x3: Float, y3: Float, z3: Float, u3: Float, v3: Float
        ): Unit =
          faceCorners(0) = x0; faceCorners(1) = y0; faceCorners(2) = z0
          faceCorners(3) = u0; faceCorners(4) = v0
          faceCorners(5) = x1; faceCorners(6) = y1; faceCorners(7) = z1
          faceCorners(8) = u1; faceCorners(9) = v1
          faceCorners(10) = x2; faceCorners(11) = y2; faceCorners(12) = z2
          faceCorners(13) = u2; faceCorners(14) = v2
          faceCorners(15) = x3; faceCorners(16) = y3; faceCorners(17) = z3
          faceCorners(18) = u3; faceCorners(19) = v3
        // Vertex colour carries face shade, ambient occlusion and biome tint only. Sky and
        // torch light travel in UV1 and are resolved by the lightmap (or a shader pack), so
        // night, dusk and caves darken in one place instead of being baked twice.
        def addFace(shade: Float, kind: FaceKind, tintMode: Int, grassOverlayFace: Boolean): Unit =
          val buf = if block.cutout then cutoutVerts
          else if block == Block.Water then waterVerts
          else if block.translucent then translucentVerts
          else opaqueVerts
          val occlusion = block != Block.Water && !block.translucent
          var corner = 0
          while corner < 4 do
            val o = corner * 5
            sampleCorner(
              corner,
              lx,
              y,
              lz,
              kind,
              signAt(faceCorners(o), fx),
              signAt(faceCorners(o + 1), fy),
              signAt(faceCorners(o + 2), fz),
              occlusion
            )
            corner += 1
          var nx = 0f; var ny = 0f; var nz = 0f
          kind match
            case FaceKind.Top    => ny = 1f
            case FaceKind.Bottom => ny = -1f
            case FaceKind.North  => nz = -1f
            case FaceKind.South  => nz = 1f
            case FaceKind.East   => nx = 1f
            case FaceKind.West   => nx = -1f
          // Keep the physical face for normals and lighting, but redirect the atlas tile
          // and rotate UVs for oriented logs (end grain only on the axis ends).
          val textureFace = if isLogBlock then LogAxis.textureFace(kind, logAxis) else kind
          val alpha = block match
            case Block.Water => if kind == FaceKind.Top then 0.70f else 0.58f
            case Block.Glass => 0.68f
            case _           => 1f
          val frost = if tintMode == 2 && frostedColumn(lx, lz, y) && skyAt(lx, y + 1, lz) >= 8 then
            (0.10f + settledSnow * 0.82f) * (if kind == FaceKind.Top then 1f else 0.35f)
          else 0f
          def emit(target: FloatMeshBuilder, i: Int, overlay: Boolean): Unit =
            val o = i * 5
            val cfx = faceCorners(o); val cfy = faceCorners(o + 1); val cfz = faceCorners(o + 2)
            val tu = faceCorners(o + 3); val tv = faceCorners(o + 4)
            // Rotate each leaf block's alpha mask in world coordinates. Geometry,
            // the mirror and shadow map all consume these same immutable UVs.
            var tu2 = tu
            var tv2 = tv
            if foliage then
              leafRotation match
                case 1 => tu2 = tv; tv2 = 1f - tu
                case 2 => tu2 = 1f - tu; tv2 = 1f - tv
                case 3 => tu2 = 1f - tv; tv2 = tu
                case _ => ()
            else if block == Block.Grass && kind == FaceKind.Top && !overlay then
              (TerrainUv.rotation(baseX + lx, baseZ + lz) & 3) match
                case 1 => tu2 = tv; tv2 = 1f - tu
                case 2 => tu2 = 1f - tu; tv2 = 1f - tv
                case 3 => tu2 = 1f - tv; tv2 = tu
                case _ => ()
            else if isLogBlock && LogAxis.swapsUv(kind, logAxis) then
              tu2 = tv; tv2 = tu
            val uv = ws.uv
            if overlay then atlasRef.writeGrassOverlayUv(tu2, tv2, uv, 0)
            else atlasRef.writeUv(block, textureFace, tu2, tv2, uv, 0)
            val u = uv(0); val v = uv(1)
            var tintR = 1f; var tintG = 1f; var tintB = 1f
            if overlay || tintMode == 1 then
              val tint = ws.tint
              writeGrassTint(cfx, cfz, tint)
              tintR = tint(0); tintG = tint(1); tintB = tint(2)
            else if tintMode == 2 then
              val tint = ws.tint
              writeFoliageTint(cfx, cfz, tint)
              val fr = tint(0); val fg = tint(1); val fb = tint(2)
              // Above 1 so the grey leaf mask reads as snow; far below the 1.75 emissive flag.
              if frost > 0 then
                tintR = fr + (1.24f - fr) * frost
                tintG = fg + (1.27f - fg) * frost
                tintB = fb + (1.32f - fb) * frost
              else
                tintR = fr; tintG = fg; tintB = fb
            val light = shade * cornerAo(i)
            // The overlay sits a hair in front of its face so it never depth-fights it.
            val lift = if overlay then 0.002f else 0f
            target += cfx + nx * lift; target += cfy + ny * lift; target += cfz + nz * lift
            target += light * tintR; target += light * tintG; target += light * tintB;
            target += alpha
            target += nx; target += ny; target += nz
            target += u; target += v
            // Lightmap input: x = block light, y = sky light, on Minecraft's 0..240 scale.
            target += cornerBlock(i) * 240f
            target += cornerSky(i) * 240f
            if block == Block.Water then
              val surface = cfy > fy + 0.1f && localBlock(lx, y + 1, lz) != Block.Water
              val weight =
                if surface then WaterSurface.waveWeight(math.round(cfx), y, math.round(cfz), waterCell)
                else 0f
              target += weight
            else target += shade
          // Split each quad along the diagonal that keeps light gradients symmetric.
          def weight(i: Int): Float =
            cornerAo(i) * (0.35f + 0.65f * cornerSky(i).max(cornerBlock(i)))
          val order =
            if weight(0) + weight(2) > weight(1) + weight(3) then flipOrder else quadOrder
          var k = 0
          while k < 6 do { emit(buf, order(k), false); k += 1 }
          if grassOverlayFace then
            k = 0
            while k < 6 do { emit(cutoutVerts, order(k), true); k += 1 }
        def emitFace(
            shade: Float,
            kind: FaceKind,
            tintMode: Int,
            x0: Float, y0: Float, z0: Float, u0: Float, v0: Float,
            x1: Float, y1: Float, z1: Float, u1: Float, v1: Float,
            x2: Float, y2: Float, z2: Float, u2: Float, v2: Float,
            x3: Float, y3: Float, z3: Float, u3: Float, v3: Float
        ): Unit =
          putFace(x0, y0, z0, u0, v0, x1, y1, z1, u1, v1, x2, y2, z2, u2, v2, x3, y3, z3, u3, v3)
          addFace(shade, kind, tintMode, false)
        if FloraModel.contains(block) then
          val glow = blockLight.level(lx, y, lz) / 15f
          val skyHere = skyAt(lx, y, lz) / 15f
          var plantR = 1f; var plantG = 1f; var plantB = 1f
          if FloraModel.grassTinted(block) then
            val color = gen.grassTintAt(baseX + lx, baseZ + lz, y - 1)
            plantR = color._1; plantG = color._2; plantB = color._3
          val variant = (TerrainUv.rotation(baseX + lx, baseZ + lz) + ((baseX + lx) & 4)) & 7
          val plantVerts = FloraModel.vertices(block, variant)
          var plantIndex = 0
          while plantIndex < plantVerts.length do
            val vertex = plantVerts(plantIndex)
            val uv = ws.uv
            atlasRef.writeUv(block, FaceKind.North, vertex.u, vertex.v, uv, 0)
            val shade = if vertex.ny < -0.5f then 0.7f else 1f
            cutoutVerts += fx + vertex.x; cutoutVerts += fy + vertex.y; cutoutVerts += fz + vertex.z
            cutoutVerts += shade * plantR; cutoutVerts += shade * plantG
            cutoutVerts += shade * plantB; cutoutVerts += 1f
            cutoutVerts += vertex.nx; cutoutVerts += vertex.ny; cutoutVerts += vertex.nz
            cutoutVerts += uv(0); cutoutVerts += uv(1)
            cutoutVerts += glow * 240f; cutoutVerts += skyHere * 240f; cutoutVerts += -(fy + 1f)
            plantIndex += 1
        else if block == Block.Torch then
          val mount = torchMountsSnapshot.getOrElse(localIndex(lx, y, lz), TorchMount.Floor)
          val glow = blockLight.level(lx, y, lz) / 15f
          val skyHere = skyAt(lx, y, lz) / 15f
          val torchVerts = TorchModel.vertices(mount)
          var torchIndex = 0
          while torchIndex < torchVerts.length do
            val vertex = torchVerts(torchIndex)
            val uv = ws.uv
            atlasRef.writeUv(Block.Torch, FaceKind.North, vertex.u, vertex.v, uv, 0)
            val shade =
              if vertex.ny > 0.5f then 1f
              else if vertex.ny < -0.5f then 0.5f
              else 0.6f + abs(vertex.nz) * 0.2f + vertex.ny.max(0f) * 0.3f
            cutoutVerts += fx + vertex.x; cutoutVerts += fy + vertex.y; cutoutVerts += fz + vertex.z
            // Flame vertices stay above 1.75 so shader packs can treat them as emissive.
            cutoutVerts += (if vertex.flame then 2.0f else shade)
            cutoutVerts += (if vertex.flame then 1.65f else shade)
            cutoutVerts += (if vertex.flame then 0.80f else shade)
            cutoutVerts += 1f
            cutoutVerts += vertex.nx; cutoutVerts += vertex.ny; cutoutVerts += vertex.nz
            cutoutVerts += uv(0); cutoutVerts += uv(1)
            cutoutVerts += (if vertex.flame then 240f else glow * 240f)
            cutoutVerts += skyHere * 240f
            cutoutVerts += (if vertex.flame then 1f else shade)
            torchIndex += 1
        else if block == Block.Water then
          // Render dynamic water as actual block-volume water with internal culling.
          // Neighboring water cells hide shared faces; exposed streams and waterfalls show
          // full-height sides like Minecraft-style flowing water.
          val fullAbove = localBlock(lx, y + 1, lz) == Block.Water
          val nwTop = if fullAbove then fy + 1f
          else waterCornerY(y, lx, lz, lx - 1, lz, lx, lz - 1, lx - 1, lz - 1, fy, topY)
          val neTop = if fullAbove then fy + 1f
          else waterCornerY(y, lx, lz, lx + 1, lz, lx, lz - 1, lx + 1, lz - 1, fy, topY)
          val seTop = if fullAbove then fy + 1f
          else waterCornerY(y, lx, lz, lx + 1, lz, lx, lz + 1, lx + 1, lz + 1, fy, topY)
          val swTop = if fullAbove then fy + 1f
          else waterCornerY(y, lx, lz, lx - 1, lz, lx, lz + 1, lx - 1, lz + 1, fy, topY)
          if !fullAbove then
            emitFace(
              1.00f,
              FaceKind.Top,
              0,
              fx, nwTop, fz, 0f, 0f,
              fx + 1, neTop, fz, 1f, 0f,
              fx + 1, seTop, fz + 1, 1f, 1f,
              fx, swTop, fz + 1, 0f, 1f
            )
            if math.abs(
                nwTop - (fy + WaterSurface.fullHeight)
              ) < 0.0001f && neTop == nwTop && seTop == nwTop && swTop == nwTop
            then flatWaterFaces.update(nwTop, flatWaterFaces.getOrElse(nwTop, 0) + 1)
          // Close every exposed side, including static water. Interior faces stay culled.
          if shouldDrawWaterSide(lx, y, lz, lx + 1, y, lz) then
            emitFace(
              0.72f,
              FaceKind.East,
              0,
              fx + 1, fy, fz, 0f, 1f,
              fx + 1, fy, fz + 1, 1f, 1f,
              fx + 1, seTop, fz + 1, 1f, 0f,
              fx + 1, neTop, fz, 0f, 0f
            )
          if shouldDrawWaterSide(lx, y, lz, lx - 1, y, lz) then
            emitFace(
              0.72f,
              FaceKind.West,
              0,
              fx, fy, fz + 1, 0f, 1f,
              fx, fy, fz, 1f, 1f,
              fx, nwTop, fz, 1f, 0f,
              fx, swTop, fz + 1, 0f, 0f
            )
          if shouldDrawWaterSide(lx, y, lz, lx, y, lz + 1) then
            emitFace(
              0.86f,
              FaceKind.South,
              0,
              fx + 1, fy, fz + 1, 0f, 1f,
              fx, fy, fz + 1, 1f, 1f,
              fx, swTop, fz + 1, 1f, 0f,
              fx + 1, seTop, fz + 1, 0f, 0f
            )
          if shouldDrawWaterSide(lx, y, lz, lx, y, lz - 1) then
            emitFace(
              0.86f,
              FaceKind.North,
              0,
              fx, fy, fz, 0f, 1f,
              fx + 1, fy, fz, 1f, 1f,
              fx + 1, neTop, fz, 1f, 0f,
              fx, nwTop, fz, 0f, 0f
            )
          if isWaterSideVisible(lx, y - 1, lz) then
            emitFace(
              0.5f,
              FaceKind.Bottom,
              0,
              fx, fy, fz + 1, 0f, 0f,
              fx + 1, fy, fz + 1, 1f, 0f,
              fx + 1, fy, fz, 1f, 1f,
              fx, fy, fz, 0f, 1f
            )
        else
          // Minecraft's directional face shade: tops full, north/south 0.8, east/west 0.6,
          // undersides 0.5. Shader packs replace this with real sun and sky terms.
          val tint = if foliage then 2 else 0
          if isVisible(lx, y + 1, lz, block) then
            emitFace(
              1.00f,
              FaceKind.Top,
              if block == Block.Grass then 1 else tint,
              fx, topY, fz, 0f, 0f,
              fx + 1, topY, fz, 1f, 0f,
              fx + 1, topY, fz + 1, 1f, 1f,
              fx, topY, fz + 1, 0f, 1f
            )
          if isVisible(lx, y - 1, lz, block) then
            emitFace(
              0.50f,
              FaceKind.Bottom,
              tint,
              fx, fy, fz + 1, 0f, 0f,
              fx + 1, fy, fz + 1, 1f, 0f,
              fx + 1, fy, fz, 1f, 1f,
              fx, fy, fz, 0f, 1f
            )
          if isVisible(lx + 1, y, lz, block) then
            putFace(
              fx + 1, fy, fz, 0f, 1f,
              fx + 1, fy, fz + 1, 1f, 1f,
              fx + 1, topY, fz + 1, 1f, 0f,
              fx + 1, topY, fz, 0f, 0f
            )
            if block == Block.Grass then addFace(0.60f, FaceKind.East, 0, grassOverlay)
            else addFace(0.60f, FaceKind.East, tint, false)
          if isVisible(lx - 1, y, lz, block) then
            putFace(
              fx, fy, fz + 1, 0f, 1f,
              fx, fy, fz, 1f, 1f,
              fx, topY, fz, 1f, 0f,
              fx, topY, fz + 1, 0f, 0f
            )
            if block == Block.Grass then addFace(0.60f, FaceKind.West, 0, grassOverlay)
            else addFace(0.60f, FaceKind.West, tint, false)
          if isVisible(lx, y, lz + 1, block) then
            putFace(
              fx + 1, fy, fz + 1, 0f, 1f,
              fx, fy, fz + 1, 1f, 1f,
              fx, topY, fz + 1, 1f, 0f,
              fx + 1, topY, fz + 1, 0f, 0f
            )
            if block == Block.Grass then addFace(0.80f, FaceKind.South, 0, grassOverlay)
            else addFace(0.80f, FaceKind.South, tint, false)
          if isVisible(lx, y, lz - 1, block) then
            putFace(
              fx, fy, fz, 0f, 1f,
              fx + 1, fy, fz, 1f, 1f,
              fx + 1, topY, fz, 1f, 0f,
              fx, topY, fz, 0f, 0f
            )
            if block == Block.Grass then addFace(0.80f, FaceKind.North, 0, grassOverlay)
            else addFace(0.80f, FaceKind.North, tint, false)
    val packed = new Array[Byte](cs * cs * height)
    var py = 0
    while py < height do
      var pz = 0
      while pz < cs do
        var px = 0
        while px < cs do
          packed(localIndex(px, py, pz)) =
            ((sky(cell(px, py, pz)) << 4) | blockLight.level(px, py, pz)).toByte
          px += 1
        pz += 1
      py += 1
    if disposed then
      pendingData = null
      pendingRevision = -1
      meshReady = false
      releaseMeshQueue()
    else
      packedLight = packed
      pendingWaterPlane = flatWaterFaces.maxByOption(_._2).map((y, count) => (y.toFloat, count))
      pendingData = Array(
        opaqueVerts.compact(),
        cutoutVerts.compact(),
        translucentVerts.compact(),
        waterVerts.compact()
      )
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
      waterPlane = pendingWaterPlane
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
  def drawOpaque(camera: Vec3): Unit =
    if opaqueCount > 0 then drawBufferAt(opaqueVbo, opaqueCount, camera)
  def drawCutout(camera: Vec3): Unit =
    if cutoutCount > 0 then drawBufferAt(cutoutVbo, cutoutCount, camera)
  def hasTranslucentMesh: Boolean = translucentCount > 0
  def drawTranslucent(camera: Vec3): Unit =
    if translucentCount > 0 then drawBufferAt(translucentVbo, translucentCount, camera)
  def hasWaterMesh: Boolean = waterCount > 0
  def drawWater(camera: Vec3): Unit =
    if waterCount > 0 then drawBufferAt(waterVbo, waterCount, camera)

  def destroy(): Unit =
    waterPlane = None
    if opaqueVbo != 0 then glDeleteBuffers(opaqueVbo); opaqueVbo = 0; opaqueCount = 0
    if cutoutVbo != 0 then glDeleteBuffers(cutoutVbo); cutoutVbo = 0; cutoutCount = 0
    if translucentVbo != 0 then glDeleteBuffers(translucentVbo); translucentVbo = 0;
    translucentCount = 0
    if waterVbo != 0 then glDeleteBuffers(waterVbo); waterVbo = 0; waterCount = 0

  /** Serialize an immutable chunk snapshot on the game thread. Disk I/O can then happen on the save
    * worker without racing block edits or water simulation.
    */
  def captureSaveSnapshot(): ChunkSaveSnapshot =
    val blockCopy = blocks.clone()
    val editCopy = edits.synchronized { edits.toList.map { case (pos, block) => (pos, block.id) } }
    val waterCopy = waterLevelsLocal.clone()
    val axisCopy = logAxes.toList.map { case (i, axis) => (i, axis.id.toByte) }
    // 255 preserves a legacy attachment waiting for an unloaded neighboring chunk.
    val mountCopy = torchMounts.synchronized {
      torchMounts.toList.filterNot(t => legacyTorchPositions.contains(t._1)).map {
        case (i, mount) => (i, mount.id.toByte)
      }
    } ++ legacyTorchPositions.toList.map(i => (i, 255.toByte))
    ChunkSaveSnapshot(blockCopy, editCopy, waterCopy, axisCopy, mountCopy)

  def snapshotSaveBytes(): Array[Byte] = captureSaveSnapshot().toBytes()

  def save(dir: java.io.File): Unit =
    BlockboxSaveFiles.writeChunkBytesAtomic(dir.toPath, cx, cz, snapshotSaveBytes(), true)

  def load(dir: java.io.File): Unit =
    if BlockboxSaveFiles.readChunkDataIfPresent(
        dir.toPath,
        cx,
        cz,
        in =>
          val loadedBlocks =
            new Array[Byte](Terrain.chunkSize * Terrain.worldHeight * Terrain.chunkSize)
          in.readFully(loadedBlocks)
          // Fast load path: use the saved block array and reapply edits below.
          // Regenerating every saved chunk on load fixed old broken worlds once, but it was too
          // expensive while walking/hosting. New worlds already use the fixed generator; old cursed
          // chunk files should be deleted once instead of regenerated every frame.
          blocks = loadedBlocks
          logAxes.clear()
          torchMounts.synchronized { torchMounts.clear() }
          legacyTorchPositions.clear()
          edits.synchronized {
            edits.clear()
            val editsCount =
              in.readInt().max(0).min(Terrain.chunkSize * Terrain.worldHeight * Terrain.chunkSize)
            for _ <- 0 until editsCount do
              val lx = in.readInt(); val ly = in.readInt(); val lz = in.readInt()
              val blockId = in.readByte()
              if lx >= 0 && lx < Terrain.chunkSize && ly >= 0 && ly < Terrain.worldHeight && lz >= 0 && lz < Terrain.chunkSize
              then
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
                if wi >= 0 && wi < waterLevelsLocal.length then
                  waterLevelsLocal(wi) = (level.toInt & 0xff).max(0).min(8).toByte
              // Optional sparse log-axis tail appended after the water section.
              if in.available() >= 8 then
                val axisMagic = in.readInt()
                if axisMagic == 0x4c415831 then // "LAX1"
                  val axisCount = in.readInt().max(0).min(blocks.length)
                  for _ <- 0 until axisCount do
                    val ai = in.readInt()
                    val axisId = in.readByte()
                    if ai >= 0 && ai < blocks.length && Block.isLog(Block.fromId(blocks(ai))) then
                      logAxes(ai) = LogAxis.fromId(axisId.toInt & 0xff)
          if in.available() >= 8 && in.readInt() == 0x54524331 then
            val count = in.readInt()
            if count < 0 || count > blocks.length then
              throw java.io.IOException("Invalid torch attachment count")
            for _ <- 0 until count do
              val i = in.readInt(); val id = in.readUnsignedByte()
              if i >= 0 && i < blocks.length && blocks(i) == Block.Torch.id && id <= 4 then
                torchMounts.synchronized { torchMounts(i) = TorchMount.fromId(id) }
              else if i >= 0 && i < blocks.length && blocks(i) == Block.Torch.id && id == 255 then
                legacyTorchPositions.add(i)
          else
            // Legacy files had no attachment state. Infer once, then save it explicitly.
            edits.synchronized {
              edits.foreach { case ((x, y, z), block) =>
                if block == Block.Torch then legacyTorchPositions.add(localIndex(x, y, z))
              }
            }
            migrateLegacyTorches((x, y, z) =>
              val lx = x - baseX; val lz = z - baseZ
              if validLocal(lx, y, lz) then Some(getBlock(lx, y, lz)) else None
            )
          rebuildLightSources()
          normalizeStaticNaturalWater()
          markDirtyMesh()
      )
    then ()

  private def upload(data: Array[Float]): (Int, Int) =
    if data == null || data.length == 0 then (0, 0)
    else
      require(data.length % floatsPerVertex == 0, "Incomplete chunk vertex")
      val buffer = ChunkMeshWorkspace.uploadBuffer(data.length)
      buffer.put(data, 0, data.length)
      buffer.flip()
      val vbo = glGenBuffers()
      glBindBuffer(GL_ARRAY_BUFFER, vbo)
      glBufferData(GL_ARRAY_BUFFER, buffer, GL_STATIC_DRAW)
      glBindBuffer(GL_ARRAY_BUFFER, 0)
      (vbo, data.length / floatsPerVertex)

  private def drawBufferAt(vbo: Int, count: Int, camera: Vec3): Unit =
    if vbo != 0 && count > 0 then
      val stride = floatsPerVertex * 4
      glPushMatrix()
      glTranslatef(baseX.toFloat - camera.x, 0f, baseZ.toFloat - camera.z)
      glBindBuffer(GL_ARRAY_BUFFER, vbo)
      glVertexPointer(3, GL_FLOAT, stride, 0L)
      glColorPointer(4, GL_FLOAT, stride, 12L)
      glNormalPointer(GL_FLOAT, stride, 28L)
      glClientActiveTexture(GL_TEXTURE0)
      glTexCoordPointer(2, GL_FLOAT, stride, 40L)
      glClientActiveTexture(GL_TEXTURE1)
      glEnableClientState(GL_TEXTURE_COORD_ARRAY)
      glTexCoordPointer(3, GL_FLOAT, stride, 48L)
      glClientActiveTexture(GL_TEXTURE0)
      glDrawArrays(GL_TRIANGLES, 0, count)
      glPopMatrix()
