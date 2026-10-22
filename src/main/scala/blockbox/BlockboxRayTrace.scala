package blockbox

import org.lwjgl.BufferUtils
import org.lwjgl.opengl.GL11.*
import org.lwjgl.opengl.GL12
import org.lwjgl.opengl.GL13.*
import org.lwjgl.opengl.GL20.*
import org.lwjgl.opengl.GL30.*
import org.lwjgl.opengl.EXTMemoryObject as GLMem
import org.lwjgl.opengl.EXTMemoryObjectFD as GLMemFd
import org.lwjgl.opengl.EXTSemaphore as GLSem
import org.lwjgl.opengl.EXTSemaphoreFD as GLSemFd
import org.lwjgl.system.MemoryStack
import org.lwjgl.system.MemoryUtil
import org.lwjgl.vulkan.*
import org.lwjgl.vulkan.KHRAccelerationStructure.*
import org.lwjgl.vulkan.KHRDeferredHostOperations.*
import org.lwjgl.vulkan.KHRExternalMemoryFd.*
import org.lwjgl.vulkan.KHRExternalSemaphoreFd.*
import org.lwjgl.vulkan.KHRRayQuery.*
import org.lwjgl.vulkan.VK10.*
import org.lwjgl.vulkan.VK11.*
import org.lwjgl.vulkan.VK12.*

import blockbox.render.RayFrameLayout
import java.io.File
import java.nio.ByteBuffer
import java.nio.FloatBuffer
import java.nio.LongBuffer
import scala.collection.mutable

/** Hardware ray tracing for the Ray Traced shader pack.
  *
  * OpenGL cannot drive the RT cores, so this builds a Vulkan ray-query scene from the
  * loaded chunks and traces sunlight, ambient occlusion, one bounce and reflections
  * into images shared with the GL post passes. If the GPU or the interop extensions
  * are missing, `ready` stays false and the pack falls back to its shadow maps.
  */
object BlockboxRayTrace:
  private var gpu: Device | Null = null
  private var failed = false
  private var message = "ray tracing not initialized"
  private var frameOk = false
  private var chunkList = Array.empty[Chunk]
  private val geometry = mutable.HashMap.empty[(Int, Int), ChunkGeometry]

  def ready: Boolean = gpu != null
  def status: String = message
  def frameReady: Boolean = frameOk && gpu != null
  def lightingTexture: Int = if gpu == null then 0 else gpu.lighting.glTex
  def reflectionTexture: Int = if gpu == null then 0 else gpu.reflection.glTex
  def volumeTexture: Int = if gpu == null then 0 else gpu.volume.glTex
  def targetWidth: Int = if gpu == null then 1 else gpu.halfW
  def targetHeight: Int = if gpu == null then 1 else gpu.halfH

  def updateWorld(chunks: Iterable[Chunk], cameraX: Float, cameraZ: Float): Unit =
    val ranked = chunks.iterator.filter(c => c != null && !c.isDisposed).toArray.sortBy { c =>
      val dx = c.baseX + 8.0f - cameraX
      val dz = c.baseZ + 8.0f - cameraZ
      dx * dx + dz * dz
    }
    chunkList = if ranked.length <= 360 then ranked else ranked.take(360)

  /** Trace this frame. Returns false when the pack should use its shadow-map fallback. */
  def trace(
      root: File, width: Int, height: Int,
      camera: (Float, Float, Float), previousCamera: (Float, Float, Float),
      sun: (Float, Float, Float), daylight: Float, rain: Float, time: Float,
      near: Float, far: Float,
      invProj: Array[Float], invView: Array[Float], prevProj: Array[Float], prevView: Array[Float],
      depthTex: Int, normalTex: Int, waterNormalTex: Int, waterDepthTex: Int
  ): Boolean =
    frameOk = false
    if failed || width < 2 || height < 2 || depthTex == 0 || normalTex == 0 then return false
    try
      val device = if gpu != null then gpu else
        val created = Device.open()
        gpu = created
        message = created.label
        System.err.println(s"Blockbox ray tracing: ${created.label}")
        created
      val drew = device.trace(root, width, height, camera, previousCamera, sun, daylight, rain, time, near, far, invProj, invView, prevProj, prevView, depthTex, normalTex, waterNormalTex, waterDepthTex, chunkList, geometry)
      frameOk = drew
      drew
    catch
      case e: Throwable =>
        frameOk = false
        message = s"ray tracing unavailable: ${e.getMessage}"
        System.err.println(s"Blockbox ray tracing disabled: ${e.getMessage}")
        e.printStackTrace(System.err)
        destroy()
        failed = true
        false

  def destroy(): Unit =
    val current=gpu
    gpu=null; frameOk=false
    try
      if current!=null then current.destroy(geometry.values)
    catch case e: Exception => System.err.println("Blockbox ray cleanup: "+e.getMessage)
    finally
      geometry.clear()
      chunkList=Array.empty

  private final class ChunkGeometry(val key: (Int, Int), var revision: Int, var border: Int, var triangles: Int, var as: Long, var asBuffer: Long, var asMemory: Long, var vertices: Long, var vertexMemory: Long, var faces: Long, var faceMemory: Long, var faceAddress: Long)

  private final class SharedImage(val image: Long, val memory: Long, val view: Long, val glMemory: Int, val glTex: Int, val width: Int, val height: Int, val size: Long)

  private final class Device(val instance: VkInstance, val physical: VkPhysicalDevice, val device: VkDevice, val queue: VkQueue, val family: Int, val label: String):
    private val memProps = VkPhysicalDeviceMemoryProperties.calloc()
    vkGetPhysicalDeviceMemoryProperties(physical, memProps)
    private val pool = createCommandPool()
    private val cmd = allocateCommand()
    private val rayLoop = new HardwareRayLoop(device,queue,cmd,family)
    def awaitFrame(): Unit = rayLoop.awaitFrame()
    private var sampler = 0L
    private var descriptorLayout = 0L
    private var descriptorPool = 0L
    private var descriptorSet = 0L
    private var pipelineLayout = 0L
    private var pipeline = 0L
    private var ubo = 0L
    private var uboMemory = 0L
    private var uboMapped = 0L
    private var infoBuffer = 0L
    private var infoMemory = 0L
    private var infoMapped = 0L
    private var instanceBuffer = 0L
    private var instanceMemory = 0L
    private var instanceMapped = 0L
    private var scratch = 0L
    private var scratchMemory = 0L
    private var scratchAddress = 0L
    private var scratchSize = 0L
    private var tlas = 0L
    private var tlasBuffer = 0L
    private var tlasMemory = 0L
    private var tlasCapacity = 0
    private var glToVk = 0L
    private var vkToGl = 0L
    private var glSignal = 0
    private var glWait = 0
    private var copyProgram = 0
    private var copyFbo = 0
    private var frameIndex = 0
    private var haveHistory = false
    private var historyRead = 0
    var gbuffer: SharedImage | Null = null
    var water: SharedImage | Null = null
    var lighting: SharedImage | Null = null
    var reflection: SharedImage | Null = null
    var volume: SharedImage | Null = null
    private var historyColor = Array.empty[SharedImage]
    private var historyDepth = Array.empty[SharedImage]
    var halfW = 1
    var halfH = 1
    private var scratchAlign = 256

    private def createCommandPool(): Long =
      val stack = MemoryStack.stackPush()
      try
        val info = VkCommandPoolCreateInfo.calloc(stack).sType(VK_STRUCTURE_TYPE_COMMAND_POOL_CREATE_INFO).flags(VK_COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT).queueFamilyIndex(family)
        val out = stack.mallocLong(1)
        check(vkCreateCommandPool(device, info, null, out), "command pool")
        out.get(0)
      finally stack.close()

    private def allocateCommand(): VkCommandBuffer =
      val stack = MemoryStack.stackPush()
      try
        val info = VkCommandBufferAllocateInfo.calloc(stack).sType(VK_STRUCTURE_TYPE_COMMAND_BUFFER_ALLOCATE_INFO).commandPool(pool).level(VK_COMMAND_BUFFER_LEVEL_PRIMARY).commandBufferCount(1)
        val out = stack.mallocPointer(1)
        check(vkAllocateCommandBuffers(device, info, out), "command buffer")
        new VkCommandBuffer(out.get(0), device)
      finally stack.close()

    def trace(
        root: File, width: Int, height: Int,
        camera: (Float, Float, Float), previousCamera: (Float, Float, Float),
        sun: (Float, Float, Float), daylight: Float, rain: Float, time: Float,
        near: Float, far: Float,
        invProj: Array[Float], invView: Array[Float], prevProj: Array[Float], prevView: Array[Float],
        depthTex: Int, normalTex: Int, waterNormalTex: Int, waterDepthTex: Int,
        chunks: Array[Chunk], geometries: mutable.HashMap[(Int, Int), ChunkGeometry]
    ): Boolean =
      rayLoop.awaitFrame()
      ensurePipeline(root)
      val nextW = math.max(1, width / 2)
      val nextH = math.max(1, height / 2)
      if gbuffer == null || halfW != nextW || halfH != nextH then recreateTargets(nextW, nextH)
      val moved = math.abs(camera._1 - previousCamera._1) + math.abs(camera._2 - previousCamera._2) + math.abs(camera._3 - previousCamera._3)
      if moved > 48f then haveHistory = false
      rebuildGeometry(chunks, geometries)
      val instances = geometries.values.filter(_.triangles > 0).toArray
      if instances.isEmpty then
        haveHistory = false
        return false
      copyGBuffer(depthTex, normalTex, waterNormalTex, waterDepthTex)
      recordAndSubmit(instances, camera, previousCamera, sun, daylight, rain, time, near, far, invProj, invView, prevProj, prevView)
      haveHistory = true
      frameIndex += 1
      true

    private def ensurePipeline(root: File): Unit =
      if pipeline != 0L then return
      val stack = MemoryStack.stackPush()
      try
        sampler = createSampler(stack)
        descriptorLayout = createLayout(stack)
        pipelineLayout = createPipelineLayout(stack, descriptorLayout)
        pipeline = createCompute(stack, pipelineLayout, readSpirv(new File(root, "assets/shaders/rt/trace.spv")))
        descriptorPool = createPool(stack)
        descriptorSet = allocateSet(stack, descriptorPool, descriptorLayout)
        val uboPair = createBuffer(65536, VK_BUFFER_USAGE_UNIFORM_BUFFER_BIT, hostFlags, map = true)
        ubo = uboPair._1; uboMemory = uboPair._2; uboMapped = uboPair._3
        val infoPair = createBuffer(360 * 16, VK_BUFFER_USAGE_STORAGE_BUFFER_BIT | VK_BUFFER_USAGE_SHADER_DEVICE_ADDRESS_BIT, hostFlags, map = true)
        infoBuffer = infoPair._1; infoMemory = infoPair._2; infoMapped = infoPair._3
        val instPair = createBuffer(360 * 64, VK_BUFFER_USAGE_ACCELERATION_STRUCTURE_BUILD_INPUT_READ_ONLY_BIT_KHR | VK_BUFFER_USAGE_SHADER_DEVICE_ADDRESS_BIT, hostFlags, map = true)
        instanceBuffer = instPair._1; instanceMemory = instPair._2; instanceMapped = instPair._3
        val props = VkPhysicalDeviceAccelerationStructurePropertiesKHR.calloc(stack).sType(VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_ACCELERATION_STRUCTURE_PROPERTIES_KHR)
        val props2 = VkPhysicalDeviceProperties2.calloc(stack).sType(VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PROPERTIES_2).pNext(props.address())
        vkGetPhysicalDeviceProperties2(physical, props2)
        scratchAlign = math.max(1, props.minAccelerationStructureScratchOffsetAlignment)
      finally stack.close()

    private def recreateTargets(w: Int, h: Int): Unit =
      vkDeviceWaitIdle(device)
      glFinish()
      destroyTargets()
      halfW = w; halfH = h; haveHistory = false; historyRead = 0
      // Raw projected depth must survive the GL/Vulkan copy without FP16
      // rounding. Lighting/history radiance can remain half precision.
      gbuffer = createShared(w, h, false, rgba32=true)
      water = createShared(w, h, false, rgba32=true)
      lighting = createShared(w, h, true)
      reflection = createShared(w, h, true)
      volume = createShared(w, h, true)
      historyColor = Array(createShared(w, h, false, interop=false), createShared(w, h, false, interop=false))
      historyDepth = Array(createShared(w, h, false, r32 = true, interop=false), createShared(w, h, false, r32 = true, interop=false))
      if copyProgram == 0 then copyProgram = buildCopyProgram()
      if copyFbo == 0 then copyFbo = glGenFramebuffers()
      if glSignal == 0 then
        val pair = createSemaphorePair()
        glToVk = pair._1; glSignal = pair._2
        val back = createSemaphorePair()
        vkToGl = back._1; glWait = back._2

    private def copyGBuffer(depthTex: Int, normalTex: Int, waterNormalTex: Int, waterDepthTex: Int): Unit =
      val previous = glGetInteger(GL_FRAMEBUFFER_BINDING)
      val previousProgram = glGetInteger(GL_CURRENT_PROGRAM)
      val viewport = BufferUtils.createIntBuffer(4)
      glGetIntegerv(GL_VIEWPORT, viewport)
      glMatrixMode(GL_PROJECTION); glPushMatrix(); glLoadIdentity()
      glOrtho(-1, 1, -1, 1, -1, 1)
      glMatrixMode(GL_MODELVIEW); glPushMatrix(); glLoadIdentity()
      glBindFramebuffer(GL_FRAMEBUFFER, copyFbo)
      glFramebufferTexture2D(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, GL_TEXTURE_2D, gbuffer.glTex, 0)
      glFramebufferTexture2D(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT1, GL_TEXTURE_2D, water.glTex, 0)
      val bufs = BufferUtils.createIntBuffer(2)
      bufs.put(GL_COLOR_ATTACHMENT0).put(GL_COLOR_ATTACHMENT1).flip()
      glDrawBuffers(bufs)
      glViewport(0, 0, halfW, halfH)
      glDisable(GL_DEPTH_TEST); glDisable(GL_BLEND); glDisable(GL_CULL_FACE)
      glUseProgram(copyProgram)
      def bind(name: String, unit: Int, tex: Int): Unit =
        val loc = glGetUniformLocation(copyProgram, name)
        if loc >= 0 && tex != 0 then
          glActiveTexture(GL_TEXTURE0 + unit)
          glBindTexture(GL_TEXTURE_2D, tex)
          glUniform1i(loc, unit)
      bind("depthTex", 0, depthTex)
      bind("normalTex", 1, normalTex)
      bind("waterNTex", 2, if waterNormalTex != 0 then waterNormalTex else normalTex)
      bind("waterDTex", 3, if waterDepthTex != 0 then waterDepthTex else depthTex)
      val hasWater = glGetUniformLocation(copyProgram, "hasWater")
      if hasWater >= 0 then glUniform1i(hasWater, if waterNormalTex != 0 && waterDepthTex != 0 then 1 else 0)
      glActiveTexture(GL_TEXTURE0)
      glBegin(GL_QUADS)
      glTexCoord2f(0f, 0f); glVertex2f(-1f, -1f)
      glTexCoord2f(1f, 0f); glVertex2f(1f, -1f)
      glTexCoord2f(1f, 1f); glVertex2f(1f, 1f)
      glTexCoord2f(0f, 1f); glVertex2f(-1f, 1f)
      glEnd()
      val textures = BufferUtils.createIntBuffer(5)
      textures.put(gbuffer.glTex).put(water.glTex).put(lighting.glTex).put(reflection.glTex).put(volume.glTex).flip()
      val layouts = BufferUtils.createIntBuffer(5)
      for _ <- 0 until 5 do layouts.put(GLSem.GL_LAYOUT_GENERAL_EXT)
      layouts.flip()
      GLSem.glSignalSemaphoreEXT(glSignal, emptyInts, textures, layouts)
      glFlush()
      glMatrixMode(GL_MODELVIEW); glPopMatrix()
      glMatrixMode(GL_PROJECTION); glPopMatrix()
      glMatrixMode(GL_MODELVIEW)
      glBindFramebuffer(GL_FRAMEBUFFER, previous)
      glUseProgram(previousProgram)
      glViewport(viewport.get(0), viewport.get(1), viewport.get(2), viewport.get(3))
      glActiveTexture(GL_TEXTURE0)

    private def recordAndSubmit(
        instances: Array[ChunkGeometry],
        camera: (Float, Float, Float), previousCamera: (Float, Float, Float),
        sun: (Float, Float, Float), daylight: Float, rain: Float, time: Float,
        near: Float, far: Float,
        invProj: Array[Float], invView: Array[Float], prevProj: Array[Float], prevView: Array[Float]
    ): Unit =
      writeFrame(camera, previousCamera, sun, daylight, rain, time, near, far, invProj, invView, prevProj, prevView, instances.length)
      writeInstances(instances)
      rayLoop.begin(gbuffer.image,water.image,lighting.image,reflection.image,volume.image)
      val stack=MemoryStack.stackPush()
      try
        buildTlas(stack,instances)
        updateDescriptors(stack,instances.length)
      finally stack.close()
      rayLoop.dispatch(pipeline,pipelineLayout,descriptorSet,glToVk,vkToGl,halfW,halfH,
        glWait,gbuffer.glTex,water.glTex,lighting.glTex,reflection.glTex,volume.glTex)
      historyRead ^= 1

    private def writeFrame(
        camera: (Float, Float, Float), previousCamera: (Float, Float, Float),
        sun: (Float, Float, Float), daylight: Float, rain: Float, time: Float,
        near: Float, far: Float,
        invProj: Array[Float], invView: Array[Float], prevProj: Array[Float], prevView: Array[Float],instanceCount: Int
    ): Unit =
      val data = MemoryUtil.memByteBuffer(uboMapped, 512)
      data.clear()
      putMat(data, invProj); putMat(data, invView); putMat(data, prevProj); putMat(data, prevView)
      put4(data, camera._1, camera._2, camera._3, daylight)
      put4(data, previousCamera._1, previousCamera._2, previousCamera._3, rain)
      put4(data, sun._1, sun._2, sun._3, time)
      put4(data, near, far, instanceCount.toFloat, 0f)
      put4(data, halfW.toFloat, halfH.toFloat, frameIndex.toFloat, if haveHistory then 1f else 0f)

    private def putMat(data: ByteBuffer,m: Array[Float]): Unit = RayFrameLayout.matrix(data,m)

    private def put4(data: ByteBuffer,a: Float,b: Float,c: Float,d: Float): Unit = RayFrameLayout.vector(data,a,b,c,d)

    private def writeInstances(instances: Array[ChunkGeometry]): Unit =
      val infos = MemoryUtil.memByteBuffer(infoMapped, math.max(instances.length, 1) * 16)
      infos.order(java.nio.ByteOrder.LITTLE_ENDIAN)
      infos.clear()
      var i = 0
      while i < instances.length do
        val g = instances(i)
        val inst = VkAccelerationStructureInstanceKHR.create(instanceMapped + i.toLong * 64L)
        val matrix = inst.transform().matrix()
        matrix.position(0)
        val originX = g.key._1 * Terrain.chunkSize.toFloat
        val originZ = g.key._2 * Terrain.chunkSize.toFloat
        matrix.put(1f).put(0f).put(0f).put(originX)
        matrix.put(0f).put(1f).put(0f).put(0f)
        matrix.put(0f).put(0f).put(1f).put(originZ)
        inst.instanceCustomIndex(i)
        inst.mask(0xff)
        inst.instanceShaderBindingTableRecordOffset(0)
        inst.flags(VK_GEOMETRY_INSTANCE_TRIANGLE_FACING_CULL_DISABLE_BIT_KHR)
        inst.accelerationStructureReference(deviceAddressOfAcceleration(g.as))
        infos.putInt((g.faceAddress & 0xffffffffL).toInt)
        infos.putInt((g.faceAddress >>> 32).toInt)
        infos.putInt(g.triangles)
        infos.putInt(0)
        i += 1

    private def buildTlas(stack: MemoryStack, instances: Array[ChunkGeometry]): Unit =
      if tlas == 0L || tlasCapacity < instances.length then
        if tlas != 0L then
          vkDestroyAccelerationStructureKHR(device, tlas, null)
          vkDestroyBuffer(device, tlasBuffer, null)
          vkFreeMemory(device, tlasMemory, null)
          tlas = 0L
        tlasCapacity = math.max(instances.length, 8)
        val sizes = tlasSizes(stack, tlasCapacity)
        val storage = createBuffer(sizes.accelerationStructureSize, VK_BUFFER_USAGE_ACCELERATION_STRUCTURE_STORAGE_BIT_KHR | VK_BUFFER_USAGE_SHADER_DEVICE_ADDRESS_BIT, VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT, map = false)
        tlasBuffer = storage._1; tlasMemory = storage._2
        ensureScratch(sizes.buildScratchSize)
        val info = VkAccelerationStructureCreateInfoKHR.calloc(stack).sType(VK_STRUCTURE_TYPE_ACCELERATION_STRUCTURE_CREATE_INFO_KHR).buffer(tlasBuffer).size(sizes.accelerationStructureSize).`type`(VK_ACCELERATION_STRUCTURE_TYPE_TOP_LEVEL_KHR)
        val out = stack.mallocLong(1)
        check(vkCreateAccelerationStructureKHR(device, info, null, out), "top-level acceleration structure")
        tlas = out.get(0)
      else ensureScratch(tlasSizes(stack, instances.length).buildScratchSize)
      val geometry = VkAccelerationStructureGeometryKHR.calloc(1, stack)
      geometry.get(0).sType(VK_STRUCTURE_TYPE_ACCELERATION_STRUCTURE_GEOMETRY_KHR).geometryType(VK_GEOMETRY_TYPE_INSTANCES_KHR).flags(VK_GEOMETRY_OPAQUE_BIT_KHR)
      geometry.get(0).geometry().instances().sType(VK_STRUCTURE_TYPE_ACCELERATION_STRUCTURE_GEOMETRY_INSTANCES_DATA_KHR).arrayOfPointers(false).data().deviceAddress(bufferAddress(instanceBuffer))
      val build = VkAccelerationStructureBuildGeometryInfoKHR.calloc(1, stack)
      build.get(0).sType(VK_STRUCTURE_TYPE_ACCELERATION_STRUCTURE_BUILD_GEOMETRY_INFO_KHR).`type`(VK_ACCELERATION_STRUCTURE_TYPE_TOP_LEVEL_KHR).flags(VK_BUILD_ACCELERATION_STRUCTURE_PREFER_FAST_BUILD_BIT_KHR).mode(VK_BUILD_ACCELERATION_STRUCTURE_MODE_BUILD_KHR).dstAccelerationStructure(tlas).geometryCount(1).pGeometries(geometry).scratchData().deviceAddress(scratchAddress)
      val range = VkAccelerationStructureBuildRangeInfoKHR.calloc(1, stack)
      range.get(0).primitiveCount(instances.length).primitiveOffset(0).firstVertex(0).transformOffset(0)
      val pp = stack.mallocPointer(1)
      pp.put(range.address()).flip()
      vkCmdBuildAccelerationStructuresKHR(cmd, build, pp)

    private def tlasSizes(stack: MemoryStack, count: Int): VkAccelerationStructureBuildSizesInfoKHR =
      val geometry = VkAccelerationStructureGeometryKHR.calloc(1, stack)
      geometry.get(0).sType(VK_STRUCTURE_TYPE_ACCELERATION_STRUCTURE_GEOMETRY_KHR).geometryType(VK_GEOMETRY_TYPE_INSTANCES_KHR)
      geometry.get(0).geometry().instances().sType(VK_STRUCTURE_TYPE_ACCELERATION_STRUCTURE_GEOMETRY_INSTANCES_DATA_KHR).arrayOfPointers(false)
      val build = VkAccelerationStructureBuildGeometryInfoKHR.calloc(stack).sType(VK_STRUCTURE_TYPE_ACCELERATION_STRUCTURE_BUILD_GEOMETRY_INFO_KHR).`type`(VK_ACCELERATION_STRUCTURE_TYPE_TOP_LEVEL_KHR).pGeometries(geometry).geometryCount(1)
      val sizes = VkAccelerationStructureBuildSizesInfoKHR.calloc(stack).sType(VK_STRUCTURE_TYPE_ACCELERATION_STRUCTURE_BUILD_SIZES_INFO_KHR)
      vkGetAccelerationStructureBuildSizesKHR(device, VK_ACCELERATION_STRUCTURE_BUILD_TYPE_DEVICE_KHR, build, stack.ints(math.max(count, 1)), sizes)
      sizes

    private def rebuildGeometry(chunks: Array[Chunk], geometries: mutable.HashMap[(Int, Int), ChunkGeometry]): Unit =
      val byKey = chunks.iterator.map(c => (c.cx, c.cz) -> c).toMap
      val live = mutable.HashSet.empty[(Int, Int)]
      var i = 0
      while i < chunks.length do
        val chunk = chunks(i)
        val key = (chunk.cx, chunk.cz)
        live += key
        val border = borderStamp(chunk, byKey)
        val existing = geometries.get(key)
        if existing.forall(g => g.revision != chunk.geometryRevision || g.border != border) then
          existing.foreach(destroyGeometry)
          val built = buildChunk(chunk, byKey, border)
          if built != null then geometries(key) = built else geometries.remove(key)
        i += 1
      val stale = geometries.keys.filterNot(live.contains).toArray
      stale.foreach { key =>
        geometries.remove(key).foreach(destroyGeometry)
      }

    private def borderStamp(chunk: Chunk, byKey: Map[(Int, Int), Chunk]): Int =
      var stamp = 1
      var dz = -1
      while dz <= 1 do
        var dx = -1
        while dx <= 1 do
          if dx != 0 || dz != 0 then byKey.get((chunk.cx + dx, chunk.cz + dz)).foreach(n => stamp = stamp * 31 + n.geometryRevision)
          dx += 1
        dz += 1
      stamp

    private var proved = false
    private def proveBuilder(): Unit =
      if proved then return
      proved = true
      val verts = Array(0f, 0f, 0f, 1f, 0f, 0f, 0f, 1f, 0f)
      val idx = Array(0, 1, 2)
      val vb = createBuffer(verts.length * 4, VK_BUFFER_USAGE_ACCELERATION_STRUCTURE_BUILD_INPUT_READ_ONLY_BIT_KHR | VK_BUFFER_USAGE_SHADER_DEVICE_ADDRESS_BIT, hostFlags, map = true)
      val ib = createBuffer(idx.length * 4, VK_BUFFER_USAGE_ACCELERATION_STRUCTURE_BUILD_INPUT_READ_ONLY_BIT_KHR | VK_BUFFER_USAGE_SHADER_DEVICE_ADDRESS_BIT, hostFlags, map = true)
      MemoryUtil.memByteBuffer(vb._3, verts.length * 4).order(java.nio.ByteOrder.LITTLE_ENDIAN).asFloatBuffer.put(verts)
      MemoryUtil.memByteBuffer(ib._3, idx.length * 4).order(java.nio.ByteOrder.LITTLE_ENDIAN).asIntBuffer.put(idx)
      val stack = MemoryStack.stackPush()
      try
        val geometry = triangleGeometry(stack, bufferAddress(vb._1), bufferAddress(ib._1), 3, 1)
        val sizes = blasSizes(stack, geometry, 1)
        ensureScratch(sizes.buildScratchSize)
        val storage = createBuffer(sizes.accelerationStructureSize, VK_BUFFER_USAGE_ACCELERATION_STRUCTURE_STORAGE_BIT_KHR | VK_BUFFER_USAGE_SHADER_DEVICE_ADDRESS_BIT, VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT, map = false)
        val info = VkAccelerationStructureCreateInfoKHR.calloc(stack).sType(VK_STRUCTURE_TYPE_ACCELERATION_STRUCTURE_CREATE_INFO_KHR).buffer(storage._1).size(sizes.accelerationStructureSize).`type`(VK_ACCELERATION_STRUCTURE_TYPE_BOTTOM_LEVEL_KHR)
        val out = stack.mallocLong(1)
        check(vkCreateAccelerationStructureKHR(device, info, null, out), "probe as")
        check(vkResetCommandBuffer(cmd, 0), "probe reset")
        check(vkBeginCommandBuffer(cmd, VkCommandBufferBeginInfo.calloc(stack).sType(VK_STRUCTURE_TYPE_COMMAND_BUFFER_BEGIN_INFO).flags(VK_COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT)), "probe begin")
        val build = VkAccelerationStructureBuildGeometryInfoKHR.calloc(1, stack)
        build.get(0).sType(VK_STRUCTURE_TYPE_ACCELERATION_STRUCTURE_BUILD_GEOMETRY_INFO_KHR).`type`(VK_ACCELERATION_STRUCTURE_TYPE_BOTTOM_LEVEL_KHR).flags(VK_BUILD_ACCELERATION_STRUCTURE_PREFER_FAST_TRACE_BIT_KHR).mode(VK_BUILD_ACCELERATION_STRUCTURE_MODE_BUILD_KHR).dstAccelerationStructure(out.get(0)).geometryCount(1).pGeometries(geometry).scratchData().deviceAddress(scratchAddress)
        val range = VkAccelerationStructureBuildRangeInfoKHR.calloc(1, stack)
        range.get(0).primitiveCount(1)
        val pp = stack.mallocPointer(1)
        pp.put(range.address()).flip()
        vkCmdBuildAccelerationStructuresKHR(cmd, build, pp)
        check(vkEndCommandBuffer(cmd), "probe end")
        val commands = stack.mallocPointer(1)
        commands.put(cmd).flip()
        check(vkQueueSubmit(queue, VkSubmitInfo.calloc(stack).sType(VK_STRUCTURE_TYPE_SUBMIT_INFO).pCommandBuffers(commands), 0L), "probe submit")
        check(vkQueueWaitIdle(queue), "probe idle")
        vkDestroyAccelerationStructureKHR(device, out.get(0), null)
        vkDestroyBuffer(device, storage._1, null)
        vkFreeMemory(device, storage._2, null)
        ()
      finally
        vkUnmapMemory(device, vb._2); vkDestroyBuffer(device, vb._1, null); vkFreeMemory(device, vb._2, null)
        vkUnmapMemory(device, ib._2); vkDestroyBuffer(device, ib._1, null); vkFreeMemory(device, ib._2, null)
        stack.close()

    private def buildChunk(chunk: Chunk, byKey: Map[(Int, Int), Chunk], border: Int): ChunkGeometry | Null =
      proveBuilder()
      val extracted = extract(chunk, byKey)
      if extracted == null then return null
      // Builds past about 4096 triangles reset this GPU, so keep the top of the
      // column (the scan is bottom-up) where the camera actually looks.
      val total = extracted._3
      val triangles = math.min(total, 3500)
      val start = total - triangles
      val vertices = extracted._1.slice(start * 9, total * 9)
      val faces = extracted._2.slice(start * 8, total * 8)
      val vertexCount = triangles * 3
      if vertices.length != vertexCount * 3 || faces.length != triangles * 8 then
        throw IllegalStateException(s"geometry size mismatch: ${vertices.length} floats, ${faces.length} face floats, $triangles triangles")
      var finite = 0
      while finite < vertices.length do
        if !vertices(finite).isFinite then throw IllegalStateException("non-finite ray-trace vertex")
        finite += 1
      val indices = new Array[Int](vertexCount)
      var n = 0
      while n < vertexCount do
        indices(n) = n
        n += 1
      val vertexBytes = vertices.length * 4
      val indexBytes = indices.length * 4
      val faceBytes = faces.length * 4
      val inputUsage = VK_BUFFER_USAGE_ACCELERATION_STRUCTURE_BUILD_INPUT_READ_ONLY_BIT_KHR | VK_BUFFER_USAGE_SHADER_DEVICE_ADDRESS_BIT
      val vertexBuf = createBuffer(vertexBytes, inputUsage, hostFlags, map = true)
      val indexBuf = createBuffer(indexBytes, inputUsage, hostFlags, map = true)
      val faceBuf = createBuffer(faceBytes, VK_BUFFER_USAGE_STORAGE_BUFFER_BIT | VK_BUFFER_USAGE_SHADER_DEVICE_ADDRESS_BIT, hostFlags, map = true)
      MemoryUtil.memByteBuffer(vertexBuf._3, vertexBytes).order(java.nio.ByteOrder.LITTLE_ENDIAN).asFloatBuffer.put(vertices)
      MemoryUtil.memByteBuffer(indexBuf._3, indexBytes).order(java.nio.ByteOrder.LITTLE_ENDIAN).asIntBuffer.put(indices)
      MemoryUtil.memByteBuffer(faceBuf._3, faceBytes).order(java.nio.ByteOrder.LITTLE_ENDIAN).asFloatBuffer.put(faces)
      val vertexAddress = bufferAddress(vertexBuf._1)
      val indexAddress = bufferAddress(indexBuf._1)
      val faceAddress = bufferAddress(faceBuf._1)
      if vertexAddress == 0L || indexAddress == 0L || faceAddress == 0L then throw IllegalStateException("buffer device address is unavailable")
      val stack = MemoryStack.stackPush()
      try
        val geometry = triangleGeometry(stack, vertexAddress, indexAddress, vertexCount, triangles)
        val sizes = blasSizes(stack, geometry, triangles)
        if sizes.accelerationStructureSize <= 0 then throw IllegalStateException(s"acceleration structure size for $triangles triangles is 0")
        ensureScratch(sizes.buildScratchSize)
        val storage = createBuffer(sizes.accelerationStructureSize, VK_BUFFER_USAGE_ACCELERATION_STRUCTURE_STORAGE_BIT_KHR | VK_BUFFER_USAGE_SHADER_DEVICE_ADDRESS_BIT, VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT, map = false)
        val info = VkAccelerationStructureCreateInfoKHR.calloc(stack).sType(VK_STRUCTURE_TYPE_ACCELERATION_STRUCTURE_CREATE_INFO_KHR).buffer(storage._1).size(sizes.accelerationStructureSize).`type`(VK_ACCELERATION_STRUCTURE_TYPE_BOTTOM_LEVEL_KHR)
        val out = stack.mallocLong(1)
        check(vkCreateAccelerationStructureKHR(device, info, null, out), "bottom-level acceleration structure")
        val as = out.get(0)
        val buildCmd = allocateCommand()
        val begin = VkCommandBufferBeginInfo.calloc(stack).sType(VK_STRUCTURE_TYPE_COMMAND_BUFFER_BEGIN_INFO).flags(VK_COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT)
        check(vkBeginCommandBuffer(buildCmd, begin), "begin blas")
        val build = VkAccelerationStructureBuildGeometryInfoKHR.calloc(1, stack)
        build.get(0).sType(VK_STRUCTURE_TYPE_ACCELERATION_STRUCTURE_BUILD_GEOMETRY_INFO_KHR).`type`(VK_ACCELERATION_STRUCTURE_TYPE_BOTTOM_LEVEL_KHR).flags(VK_BUILD_ACCELERATION_STRUCTURE_PREFER_FAST_TRACE_BIT_KHR).mode(VK_BUILD_ACCELERATION_STRUCTURE_MODE_BUILD_KHR).dstAccelerationStructure(as).geometryCount(1).pGeometries(geometry).scratchData().deviceAddress(scratchAddress)
        val range = VkAccelerationStructureBuildRangeInfoKHR.calloc(1, stack)
        range.get(0).primitiveCount(triangles).primitiveOffset(0).firstVertex(0).transformOffset(0)
        val pp = stack.mallocPointer(1)
        pp.put(range.address()).flip()
        vkCmdBuildAccelerationStructuresKHR(buildCmd, build, pp)
        check(vkEndCommandBuffer(buildCmd), "end blas")
        check(vkQueueSubmit(queue, VkSubmitInfo.calloc(stack).sType(VK_STRUCTURE_TYPE_SUBMIT_INFO).pCommandBuffers({
          val p = stack.mallocPointer(1); p.put(buildCmd).flip(); p
        }), 0L), "submit blas")
        check(vkQueueWaitIdle(queue), s"blas idle ($triangles triangles)")
        vkUnmapMemory(device, indexBuf._2)
        vkDestroyBuffer(device, indexBuf._1, null)
        vkFreeMemory(device, indexBuf._2, null)
        // The build copied the vertices into the acceleration structure.
        vkUnmapMemory(device, vertexBuf._2)
        vkDestroyBuffer(device, vertexBuf._1, null)
        vkFreeMemory(device, vertexBuf._2, null)
        new ChunkGeometry((chunk.cx, chunk.cz), chunk.geometryRevision, border, triangles, as, storage._1, storage._2, 0L, 0L, faceBuf._1, faceBuf._2, faceAddress)
      finally stack.close()

    private def emptyInts: java.nio.IntBuffer =
      val buffers = BufferUtils.createIntBuffer(1)
      buffers.limit(0)
      buffers

    private def copyBuffer(stack: MemoryStack, src: Long, dst: Long, offset: Long, size: Long): Unit =
      val region = VkBufferCopy.calloc(1, stack)
      region.srcOffset(offset).dstOffset(0).size(size)
      vkCmdCopyBuffer(cmd, src, dst, region)

    private def triangleGeometry(stack: MemoryStack, vertexAddress: Long, indexAddress: Long, vertexCount: Int, triangles: Int): VkAccelerationStructureGeometryKHR.Buffer =
      val geometry = VkAccelerationStructureGeometryKHR.calloc(1, stack)
      geometry.get(0).sType(VK_STRUCTURE_TYPE_ACCELERATION_STRUCTURE_GEOMETRY_KHR).geometryType(VK_GEOMETRY_TYPE_TRIANGLES_KHR).flags(VK_GEOMETRY_OPAQUE_BIT_KHR)
      val tris = geometry.get(0).geometry().triangles()
      tris.sType(VK_STRUCTURE_TYPE_ACCELERATION_STRUCTURE_GEOMETRY_TRIANGLES_DATA_KHR)
      tris.vertexFormat(VK_FORMAT_R32G32B32_SFLOAT)
      tris.vertexData().deviceAddress(vertexAddress)
      tris.vertexStride(12)
      tris.maxVertex(vertexCount - 1)
      tris.indexType(VK_INDEX_TYPE_UINT32)
      tris.indexData().deviceAddress(indexAddress)
      geometry

    private def blasSizes(stack: MemoryStack, geometry: VkAccelerationStructureGeometryKHR.Buffer, triangles: Int): VkAccelerationStructureBuildSizesInfoKHR =
      val build = VkAccelerationStructureBuildGeometryInfoKHR.calloc(stack).sType(VK_STRUCTURE_TYPE_ACCELERATION_STRUCTURE_BUILD_GEOMETRY_INFO_KHR).`type`(VK_ACCELERATION_STRUCTURE_TYPE_BOTTOM_LEVEL_KHR).pGeometries(geometry).geometryCount(1)
      val sizes = VkAccelerationStructureBuildSizesInfoKHR.calloc(stack).sType(VK_STRUCTURE_TYPE_ACCELERATION_STRUCTURE_BUILD_SIZES_INFO_KHR)
      vkGetAccelerationStructureBuildSizesKHR(device, VK_ACCELERATION_STRUCTURE_BUILD_TYPE_DEVICE_KHR, build, stack.ints(triangles), sizes)
      sizes

    private def extract(chunk: Chunk, byKey: Map[(Int, Int), Chunk]): (Array[Float], Array[Float], Int) | Null =
      val cs = Terrain.chunkSize
      val height = Terrain.worldHeight
      val blocks = chunk.blocks
      if blocks == null then return null
      val verts = new Grow(4096)
      val faces = new Grow(4096)
      var triangles = 0
      def worldBlock(x: Int, y: Int, z: Int): Block =
        if y < 0 || y >= height then Block.Air
        else
          val cx = Math.floorDiv(x, cs)
          val cz = Math.floorDiv(z, cs)
          byKey.get((cx, cz)) match
            case Some(c) =>
              val data = c.blocks
              if data == null then Block.Air
              else Block.fromId(data((y * cs + Math.floorMod(z, cs)) * cs + Math.floorMod(x, cs)))
            case None => Block.Air
      def occludes(b: Block): Boolean = b.solid && !b.translucent && !b.liquid
      val dirs = Array((0, 1, 0), (0, -1, 0), (1, 0, 0), (-1, 0, 0), (0, 0, 1), (0, 0, -1))
      val corners = Array(
        Array((0, 1, 0), (1, 1, 0), (1, 1, 1), (0, 1, 1)),
        Array((0, 0, 1), (1, 0, 1), (1, 0, 0), (0, 0, 0)),
        Array((1, 0, 1), (1, 1, 1), (1, 1, 0), (1, 0, 0)),
        Array((0, 0, 0), (0, 1, 0), (0, 1, 1), (0, 0, 1)),
        Array((1, 0, 1), (0, 0, 1), (0, 1, 1), (1, 1, 1)),
        Array((0, 0, 0), (1, 0, 0), (1, 1, 0), (0, 1, 0))
      )
      var lx = 0
      while lx < cs && triangles < 60000 do
        var lz = 0
        while lz < cs && triangles < 60000 do
          var y = 0
          while y < height && triangles < 60000 do
            val id = blocks((y * cs + lz) * cs + lx)
            if id != 0 then
              val block = Block.fromId(id)
              if occludes(block) then
                val wx = chunk.baseX + lx
                val wz = chunk.baseZ + lz
                var face = 0
                while face < 6 do
                  val d = dirs(face)
                  if !occludes(worldBlock(wx + d._1, y + d._2, wz + d._3)) then
                    val (ar, ag, ab) = albedo(block, d._2)
                    val emit = if block == Block.FurnaceLit then 6f else 0f
                    val c = corners(face)
                    // Chunk-local positions. The instance matrix carries the chunk origin, which
                    // keeps the acceleration structure in a range the builder handles well.
                    emitQuad(verts, faces, lx, y, lz, d._1, d._2, d._3, ar, ag, ab, emit, c)
                    triangles += 2
                  face += 1
            y += 1
          lz += 1
        lx += 1
      if triangles == 0 then null else (verts.toArray, faces.toArray, triangles)

    private def emitQuad(verts: Grow, faces: Grow, wx: Int, y: Int, wz: Int, nx: Int, ny: Int, nz: Int, ar: Float, ag: Float, ab: Float, emit: Float, corners: Array[(Int, Int, Int)]): Unit =
      val order = Array(0, 1, 2, 0, 2, 3)
      var t = 0
      while t < 2 do
        var k = 0
        while k < 3 do
          val p = corners(order(t * 3 + k))
          verts.add(wx + p._1).add(y + p._2).add(wz + p._3)
          k += 1
        faces.add(nx).add(ny).add(nz).add(emit).add(ar).add(ag).add(ab).add(0f)
        t += 1

    private def albedo(block: Block, ny: Int): (Float, Float, Float) =
      if block == Block.Grass && ny > 0 then (0.30f, 0.62f, 0.18f)
      else if block == Block.Grass && ny == 0 then (0.45f, 0.36f, 0.18f)
      else if block == Block.Grass then (0.36f, 0.28f, 0.14f)
      else block.rgb

    private def ensureScratch(needed: Long): Unit =
      val align = math.max(scratchAlign, 1)
      val size = 16L * 1024L * 1024L
      if needed > size then throw IllegalStateException(s"scratch request $needed exceeds the fixed buffer")
      if scratch != 0L then return
      val created = createBuffer(size, VK_BUFFER_USAGE_STORAGE_BUFFER_BIT | VK_BUFFER_USAGE_SHADER_DEVICE_ADDRESS_BIT, VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT, map = false)
      scratch = created._1
      scratchMemory = created._2
      scratchSize = size
      val base = bufferAddress(scratch)
      scratchAddress = (base + align - 1L) / align * align
      if scratchAddress + needed > base + size then throw IllegalStateException("aligned scratch does not fit")

    private def updateDescriptors(stack: MemoryStack, instanceCount: Int): Unit =
      val writes = VkWriteDescriptorSet.calloc(12, stack)
      def image(binding: Int, view: Long, sampled: Boolean): Unit =
        val info = VkDescriptorImageInfo.calloc(1, stack)
        info.get(0).imageView(view).imageLayout(VK_IMAGE_LAYOUT_GENERAL)
        if sampled then info.get(0).sampler(sampler)
        val w = writes.get(binding)
        w.sType(VK_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET).dstSet(descriptorSet).dstBinding(binding).descriptorCount(1).pImageInfo(info)
        w.descriptorType(if sampled then VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER else VK_DESCRIPTOR_TYPE_STORAGE_IMAGE)
      val uboInfo = VkDescriptorBufferInfo.calloc(1, stack)
      uboInfo.get(0).buffer(ubo).offset(0).range(512)
      writes.get(0).sType(VK_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET).dstSet(descriptorSet).dstBinding(0).descriptorCount(1).descriptorType(VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER).pBufferInfo(uboInfo)
      val asInfo = VkWriteDescriptorSetAccelerationStructureKHR.calloc(stack).sType(VK_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET_ACCELERATION_STRUCTURE_KHR).pAccelerationStructures(stack.longs(tlas))
      writes.get(1).sType(VK_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET).pNext(asInfo.address()).dstSet(descriptorSet).dstBinding(1).descriptorCount(1).descriptorType(VK_DESCRIPTOR_TYPE_ACCELERATION_STRUCTURE_KHR)
      image(2, gbuffer.view, sampled = true)
      image(3, water.view, sampled = true)
      image(4, lighting.view, sampled = false)
      image(5, reflection.view, sampled = false)
      image(6, volume.view, sampled = false)
      val infoInfo = VkDescriptorBufferInfo.calloc(1, stack)
      infoInfo.get(0).buffer(infoBuffer).offset(0).range(math.max(instanceCount, 1) * 16)
      writes.get(7).sType(VK_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET).dstSet(descriptorSet).dstBinding(7).descriptorCount(1).descriptorType(VK_DESCRIPTOR_TYPE_STORAGE_BUFFER).pBufferInfo(infoInfo)
      val read = historyRead
      val write = read ^ 1
      image(8, historyColor(read).view, sampled = false)
      image(9, historyDepth(read).view, sampled = false)
      image(10, historyColor(write).view, sampled = false)
      image(11, historyDepth(write).view, sampled = false)
      vkUpdateDescriptorSets(device, writes, null)

    private def barrier(stack: MemoryStack, srcStage: Int, dstStage: Int, srcAccess: Int, dstAccess: Int): Unit =
      val memory = VkMemoryBarrier.calloc(1, stack)
      memory.get(0).sType(VK_STRUCTURE_TYPE_MEMORY_BARRIER).srcAccessMask(srcAccess).dstAccessMask(dstAccess)
      vkCmdPipelineBarrier(cmd, srcStage, dstStage, 0, memory, null, null)

    def destroyGeometry(g: ChunkGeometry): Unit =
      if g.as != 0L then vkDestroyAccelerationStructureKHR(device, g.as, null)
      if g.asBuffer != 0L then vkDestroyBuffer(device, g.asBuffer, null)
      if g.asMemory != 0L then vkFreeMemory(device, g.asMemory, null)
      if g.vertices != 0L then vkDestroyBuffer(device, g.vertices, null)
      if g.vertexMemory != 0L then vkFreeMemory(device, g.vertexMemory, null)
      if g.faces != 0L then vkDestroyBuffer(device, g.faces, null)
      if g.faceMemory != 0L then vkFreeMemory(device, g.faceMemory, null)

    def destroy(geometries: Iterable[ChunkGeometry]): Unit =
      val idle=vkDeviceWaitIdle(device)
      if idle!=VK_ERROR_DEVICE_LOST then check(idle,"idle ray device for teardown")
      rayLoop.closeAfterDeviceIdle()
      glFinish()
      geometries.foreach(destroyGeometry)
      destroyTargets()
      if pipeline != 0L then vkDestroyPipeline(device, pipeline, null)
      if pipelineLayout != 0L then vkDestroyPipelineLayout(device, pipelineLayout, null)
      if descriptorPool != 0L then vkDestroyDescriptorPool(device, descriptorPool, null)
      if descriptorLayout != 0L then vkDestroyDescriptorSetLayout(device, descriptorLayout, null)
      if sampler != 0L then vkDestroySampler(device, sampler, null)
      if tlas != 0L then vkDestroyAccelerationStructureKHR(device, tlas, null)
      if tlasBuffer != 0L then vkDestroyBuffer(device, tlasBuffer, null)
      if tlasMemory != 0L then vkFreeMemory(device, tlasMemory, null)
      if scratch != 0L then vkDestroyBuffer(device, scratch, null)
      if scratchMemory != 0L then vkFreeMemory(device, scratchMemory, null)
      destroyMapped(ubo, uboMemory)
      destroyMapped(infoBuffer, infoMemory)
      destroyMapped(instanceBuffer, instanceMemory)
      if glSignal != 0 then GLSem.glDeleteSemaphoresEXT(glSignal)
      if glWait != 0 then GLSem.glDeleteSemaphoresEXT(glWait)
      if glToVk != 0L then vkDestroySemaphore(device, glToVk, null)
      if vkToGl != 0L then vkDestroySemaphore(device, vkToGl, null)
      if copyProgram != 0 then glDeleteProgram(copyProgram)
      if copyFbo != 0 then glDeleteFramebuffers(copyFbo)
      vkDestroyCommandPool(device, pool, null)
      vkDestroyDevice(device, null)
      vkDestroyInstance(instance, null)
      memProps.free()

    private def destroyTargets(): Unit =
      Array(gbuffer, water, lighting, reflection, volume).foreach(freeShared)
      historyColor.foreach(freeShared)
      historyDepth.foreach(freeShared)
      gbuffer = null; water = null; lighting = null; reflection = null; volume = null
      historyColor = Array.empty; historyDepth = Array.empty

    private def freeShared(image: SharedImage | Null): Unit =
      if image == null then return
      if image.glTex != 0 then glDeleteTextures(image.glTex)
      if image.glMemory != 0 then GLMem.glDeleteMemoryObjectsEXT(image.glMemory)
      if image.view != 0L then vkDestroyImageView(device, image.view, null)
      if image.image != 0L then vkDestroyImage(device, image.image, null)
      if image.memory != 0L then vkFreeMemory(device, image.memory, null)

    private def destroyMapped(buffer: Long, memory: Long): Unit =
      if buffer != 0L then vkDestroyBuffer(device, buffer, null)
      if memory != 0L then
        vkUnmapMemory(device, memory)
        vkFreeMemory(device, memory, null)

    private def hostFlags: Int =
      VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT | VK_MEMORY_PROPERTY_HOST_COHERENT_BIT

    private def createBuffer(size: Long, usage: Int, properties: Int, map: Boolean): (Long, Long, Long) =
      val stack = MemoryStack.stackPush()
      try
        val info = VkBufferCreateInfo.calloc(stack).sType(VK_STRUCTURE_TYPE_BUFFER_CREATE_INFO).size(math.max(size, 256L)).usage(usage).sharingMode(VK_SHARING_MODE_EXCLUSIVE)
        val out = stack.mallocLong(1)
        check(vkCreateBuffer(device, info, null, out), "buffer")
        val buffer = out.get(0)
        val req = VkMemoryRequirements.calloc(stack)
        vkGetBufferMemoryRequirements(device, buffer, req)
        val flags = VkMemoryAllocateFlagsInfo.calloc(stack).sType(VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_FLAGS_INFO)
        if (usage & VK_BUFFER_USAGE_SHADER_DEVICE_ADDRESS_BIT) != 0 then flags.flags(VK_MEMORY_ALLOCATE_DEVICE_ADDRESS_BIT)
        val alloc = VkMemoryAllocateInfo.calloc(stack).sType(VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_INFO).pNext(flags.address()).allocationSize(req.size).memoryTypeIndex(memoryType(req.memoryTypeBits, properties))
        val mem = stack.mallocLong(1)
        check(vkAllocateMemory(device, alloc, null, mem), "buffer memory")
        check(vkBindBufferMemory(device, buffer, mem.get(0), 0), "bind buffer")
        val mapped = if map then
          val pp = stack.mallocPointer(1)
          check(vkMapMemory(device, mem.get(0), 0, req.size, 0, pp), "map buffer")
          pp.get(0)
        else 0L
        (buffer, mem.get(0), mapped)
      finally stack.close()

    private def createShared(width: Int, height: Int, linear: Boolean, r32: Boolean = false, interop: Boolean = true, rgba32: Boolean = false): SharedImage =
      val stack = MemoryStack.stackPush()
      try
        val external = VkExternalMemoryImageCreateInfo.calloc(stack).sType(VK_STRUCTURE_TYPE_EXTERNAL_MEMORY_IMAGE_CREATE_INFO).handleTypes(VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_FD_BIT)
        val format = if r32 then VK_FORMAT_R32_SFLOAT else if rgba32 then VK_FORMAT_R32G32B32A32_SFLOAT else VK_FORMAT_R16G16B16A16_SFLOAT
        val info = VkImageCreateInfo.calloc(stack).sType(VK_STRUCTURE_TYPE_IMAGE_CREATE_INFO).pNext(external.address()).imageType(VK_IMAGE_TYPE_2D).format(format).mipLevels(1).arrayLayers(1).samples(VK_SAMPLE_COUNT_1_BIT).tiling(VK_IMAGE_TILING_OPTIMAL).usage(VK_IMAGE_USAGE_SAMPLED_BIT | VK_IMAGE_USAGE_STORAGE_BIT | VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT | VK_IMAGE_USAGE_TRANSFER_SRC_BIT | VK_IMAGE_USAGE_TRANSFER_DST_BIT).sharingMode(VK_SHARING_MODE_EXCLUSIVE).initialLayout(VK_IMAGE_LAYOUT_UNDEFINED)
        if !interop then info.pNext(0L)
        info.extent().width(width).height(height).depth(1)
        val out = stack.mallocLong(1)
        check(vkCreateImage(device, info, null, out), "shared image")
        val image = out.get(0)
        val req = VkMemoryRequirements.calloc(stack)
        vkGetImageMemoryRequirements(device, image, req)
        val dedicated = VkMemoryDedicatedAllocateInfo.calloc(stack).sType(VK_STRUCTURE_TYPE_MEMORY_DEDICATED_ALLOCATE_INFO).image(image)
        val exported = VkExportMemoryAllocateInfo.calloc(stack).sType(VK_STRUCTURE_TYPE_EXPORT_MEMORY_ALLOCATE_INFO).pNext(dedicated.address()).handleTypes(VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_FD_BIT)
        val alloc = VkMemoryAllocateInfo.calloc(stack).sType(VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_INFO).pNext(exported.address()).allocationSize(req.size).memoryTypeIndex(memoryType(req.memoryTypeBits, VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT))
        if !interop then alloc.pNext(dedicated.address())
        val mem = stack.mallocLong(1)
        check(vkAllocateMemory(device, alloc, null, mem), "shared image memory")
        check(vkBindImageMemory(device, image, mem.get(0), 0), "bind shared image")
        transitionImage(stack, image, interop)
        var glMemory=0
        var tex=0
        if interop then
          val fdInfo = VkMemoryGetFdInfoKHR.calloc(stack).sType(VK_STRUCTURE_TYPE_MEMORY_GET_FD_INFO_KHR).memory(mem.get(0)).handleType(VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_FD_BIT)
          val fds = stack.mallocInt(1)
          check(vkGetMemoryFdKHR(device, fdInfo, fds), "export image fd")
          glMemory = GLMem.glCreateMemoryObjectsEXT()
          GLMem.glMemoryObjectParameteriEXT(glMemory, GLMem.GL_DEDICATED_MEMORY_OBJECT_EXT, GL_TRUE)
          GLMemFd.glImportMemoryFdEXT(glMemory, req.size, GLMemFd.GL_HANDLE_TYPE_OPAQUE_FD_EXT, fds.get(0))
          tex = glGenTextures()
          glBindTexture(GL_TEXTURE_2D, tex)
          glTexParameteri(GL_TEXTURE_2D, GLMem.GL_TEXTURE_TILING_EXT, GLMem.GL_OPTIMAL_TILING_EXT)
          val glFormat = if r32 then GL_R32F else if rgba32 then GL_RGBA32F else GL_RGBA16F
          GLMem.glTexStorageMem2DEXT(GL_TEXTURE_2D, 1, glFormat, width, height, glMemory, 0L)
          val filter = if linear then GL_LINEAR else GL_NEAREST
          glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, filter)
          glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, filter)
          glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL12.GL_CLAMP_TO_EDGE)
          glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL12.GL_CLAMP_TO_EDGE)
          glTexParameteri(GL_TEXTURE_2D, GL12.GL_TEXTURE_MAX_LEVEL, 0)
          glBindTexture(GL_TEXTURE_2D, 0)
        val viewInfo = VkImageViewCreateInfo.calloc(stack).sType(VK_STRUCTURE_TYPE_IMAGE_VIEW_CREATE_INFO).image(image).viewType(VK_IMAGE_VIEW_TYPE_2D).format(format)
        viewInfo.subresourceRange().aspectMask(VK_IMAGE_ASPECT_COLOR_BIT).levelCount(1).layerCount(1)
        val viewOut = stack.mallocLong(1)
        check(vkCreateImageView(device, viewInfo, null, viewOut), "image view")
        new SharedImage(image, mem.get(0), viewOut.get(0), glMemory, tex, width, height, req.size)
      finally stack.close()

    private def transitionImage(stack: MemoryStack, image: Long, interop: Boolean): Unit =
      check(vkResetCommandBuffer(cmd, 0), "reset transition")
      val begin = VkCommandBufferBeginInfo.calloc(stack).sType(VK_STRUCTURE_TYPE_COMMAND_BUFFER_BEGIN_INFO).flags(VK_COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT)
      check(vkBeginCommandBuffer(cmd, begin), "begin transition")
      val barrier = VkImageMemoryBarrier.calloc(1, stack)
      barrier.get(0).sType(VK_STRUCTURE_TYPE_IMAGE_MEMORY_BARRIER).oldLayout(VK_IMAGE_LAYOUT_UNDEFINED).newLayout(VK_IMAGE_LAYOUT_GENERAL).srcQueueFamilyIndex(VK_QUEUE_FAMILY_IGNORED).dstQueueFamilyIndex(VK_QUEUE_FAMILY_IGNORED).image(image).srcAccessMask(0).dstAccessMask(VK_ACCESS_SHADER_READ_BIT | VK_ACCESS_SHADER_WRITE_BIT)
      if interop then
        barrier.get(0).srcQueueFamilyIndex(family).dstQueueFamilyIndex(VK_QUEUE_FAMILY_EXTERNAL).dstAccessMask(0)
      barrier.get(0).subresourceRange().aspectMask(VK_IMAGE_ASPECT_COLOR_BIT).levelCount(1).layerCount(1)
      vkCmdPipelineBarrier(cmd, VK_PIPELINE_STAGE_TOP_OF_PIPE_BIT, if interop then VK_PIPELINE_STAGE_BOTTOM_OF_PIPE_BIT else VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT, 0, null, null, barrier)
      check(vkEndCommandBuffer(cmd), "end transition")
      val commands = stack.mallocPointer(1)
      commands.put(cmd).flip()
      check(vkQueueSubmit(queue, VkSubmitInfo.calloc(stack).sType(VK_STRUCTURE_TYPE_SUBMIT_INFO).pCommandBuffers(commands), 0L), "submit transition")
      check(vkQueueWaitIdle(queue), "transition idle")

    private def createSemaphorePair(): (Long, Int) =
      val stack = MemoryStack.stackPush()
      try
        val exp = VkExportSemaphoreCreateInfo.calloc(stack).sType(VK_STRUCTURE_TYPE_EXPORT_SEMAPHORE_CREATE_INFO).handleTypes(VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_OPAQUE_FD_BIT)
        val info = VkSemaphoreCreateInfo.calloc(stack).sType(VK_STRUCTURE_TYPE_SEMAPHORE_CREATE_INFO).pNext(exp.address())
        val out = stack.mallocLong(1)
        check(vkCreateSemaphore(device, info, null, out), "semaphore")
        val fdInfo = VkSemaphoreGetFdInfoKHR.calloc(stack).sType(VK_STRUCTURE_TYPE_SEMAPHORE_GET_FD_INFO_KHR).semaphore(out.get(0)).handleType(VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_OPAQUE_FD_BIT)
        val fds = stack.mallocInt(1)
        check(vkGetSemaphoreFdKHR(device, fdInfo, fds), "export semaphore")
        val glSem = GLSem.glGenSemaphoresEXT()
        GLSemFd.glImportSemaphoreFdEXT(glSem, GLSemFd.GL_HANDLE_TYPE_OPAQUE_FD_EXT, fds.get(0))
        (out.get(0), glSem)
      finally stack.close()

    private def createSampler(stack: MemoryStack): Long =
      val info = VkSamplerCreateInfo.calloc(stack).sType(VK_STRUCTURE_TYPE_SAMPLER_CREATE_INFO).magFilter(VK_FILTER_NEAREST).minFilter(VK_FILTER_NEAREST).mipmapMode(VK_SAMPLER_MIPMAP_MODE_NEAREST).addressModeU(VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE).addressModeV(VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE).addressModeW(VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE).maxLod(0f)
      val out = stack.mallocLong(1)
      check(vkCreateSampler(device, info, null, out), "sampler")
      out.get(0)

    private def createLayout(stack: MemoryStack): Long =
      val bindings = VkDescriptorSetLayoutBinding.calloc(12, stack)
      def bind(i: Int, kind: Int): Unit =
        bindings.get(i).binding(i).descriptorType(kind).descriptorCount(1).stageFlags(VK_SHADER_STAGE_COMPUTE_BIT)
      bind(0, VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER)
      bind(1, VK_DESCRIPTOR_TYPE_ACCELERATION_STRUCTURE_KHR)
      bind(2, VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER)
      bind(3, VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER)
      bind(4, VK_DESCRIPTOR_TYPE_STORAGE_IMAGE)
      bind(5, VK_DESCRIPTOR_TYPE_STORAGE_IMAGE)
      bind(6, VK_DESCRIPTOR_TYPE_STORAGE_IMAGE)
      bind(7, VK_DESCRIPTOR_TYPE_STORAGE_BUFFER)
      bind(8, VK_DESCRIPTOR_TYPE_STORAGE_IMAGE)
      bind(9, VK_DESCRIPTOR_TYPE_STORAGE_IMAGE)
      bind(10, VK_DESCRIPTOR_TYPE_STORAGE_IMAGE)
      bind(11, VK_DESCRIPTOR_TYPE_STORAGE_IMAGE)
      val info = VkDescriptorSetLayoutCreateInfo.calloc(stack).sType(VK_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_CREATE_INFO).pBindings(bindings)
      val out = stack.mallocLong(1)
      check(vkCreateDescriptorSetLayout(device, info, null, out), "descriptor layout")
      out.get(0)

    private def createPipelineLayout(stack: MemoryStack, layout: Long): Long =
      val info = VkPipelineLayoutCreateInfo.calloc(stack).sType(VK_STRUCTURE_TYPE_PIPELINE_LAYOUT_CREATE_INFO).pSetLayouts(stack.longs(layout))
      val out = stack.mallocLong(1)
      check(vkCreatePipelineLayout(device, info, null, out), "pipeline layout")
      out.get(0)

    private def createPool(stack: MemoryStack): Long =
      val sizes = VkDescriptorPoolSize.calloc(5, stack)
      sizes.get(0).`type`(VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER).descriptorCount(2)
      sizes.get(1).`type`(VK_DESCRIPTOR_TYPE_ACCELERATION_STRUCTURE_KHR).descriptorCount(2)
      sizes.get(2).`type`(VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER).descriptorCount(4)
      sizes.get(3).`type`(VK_DESCRIPTOR_TYPE_STORAGE_IMAGE).descriptorCount(16)
      sizes.get(4).`type`(VK_DESCRIPTOR_TYPE_STORAGE_BUFFER).descriptorCount(4)
      val info = VkDescriptorPoolCreateInfo.calloc(stack).sType(VK_STRUCTURE_TYPE_DESCRIPTOR_POOL_CREATE_INFO).flags(VK_DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT).maxSets(4).pPoolSizes(sizes)
      val out = stack.mallocLong(1)
      check(vkCreateDescriptorPool(device, info, null, out), "descriptor pool")
      out.get(0)

    private def allocateSet(stack: MemoryStack, pool: Long, layout: Long): Long =
      val info = VkDescriptorSetAllocateInfo.calloc(stack).sType(VK_STRUCTURE_TYPE_DESCRIPTOR_SET_ALLOCATE_INFO).descriptorPool(pool).pSetLayouts(stack.longs(layout))
      val out = stack.mallocLong(1)
      check(vkAllocateDescriptorSets(device, info, out), "descriptor set")
      out.get(0)

    private def createCompute(stack: MemoryStack, layout: Long, spirv: ByteBuffer): Long =
      val moduleInfo = VkShaderModuleCreateInfo.calloc(stack).sType(VK_STRUCTURE_TYPE_SHADER_MODULE_CREATE_INFO).pCode(spirv)
      val moduleOut = stack.mallocLong(1)
      check(vkCreateShaderModule(device, moduleInfo, null, moduleOut), "shader module")
      val stage = VkPipelineShaderStageCreateInfo.calloc(stack).sType(VK_STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_CREATE_INFO).stage(VK_SHADER_STAGE_COMPUTE_BIT).module(moduleOut.get(0)).pName(stack.UTF8("main"))
      val info = VkComputePipelineCreateInfo.calloc(1, stack)
      info.get(0).sType(VK_STRUCTURE_TYPE_COMPUTE_PIPELINE_CREATE_INFO).stage(stage).layout(layout)
      val out = stack.mallocLong(1)
      val result = vkCreateComputePipelines(device, 0L, info, null, out)
      vkDestroyShaderModule(device, moduleOut.get(0), null)
      check(result, "compute pipeline")
      out.get(0)

    private def readSpirv(file: File): ByteBuffer =
      if !file.isFile then throw IllegalStateException(s"missing ${file.getPath}")
      val bytes = java.nio.file.Files.readAllBytes(file.toPath)
      val buffer = MemoryUtil.memAlloc(bytes.length)
      buffer.put(bytes).flip()
      buffer

    private def bufferAddress(buffer: Long): Long =
      val stack = MemoryStack.stackPush()
      try
        val info = VkBufferDeviceAddressInfo.calloc(stack).sType(VK_STRUCTURE_TYPE_BUFFER_DEVICE_ADDRESS_INFO).buffer(buffer)
        vkGetBufferDeviceAddress(device, info)
      finally stack.close()

    private def deviceAddressOfAcceleration(as: Long): Long =
      val stack = MemoryStack.stackPush()
      try
        val info = VkAccelerationStructureDeviceAddressInfoKHR.calloc(stack).sType(VK_STRUCTURE_TYPE_ACCELERATION_STRUCTURE_DEVICE_ADDRESS_INFO_KHR).accelerationStructure(as)
        vkGetAccelerationStructureDeviceAddressKHR(device, info)
      finally stack.close()

    private def memoryType(mask: Int, properties: Int): Int =
      val found = findMemory(mask, properties)
      if found < 0 then throw IllegalStateException(s"no Vulkan memory type for properties $properties")
      found

    private def findMemory(mask: Int, properties: Int): Int =
      var i = 0
      while i < memProps.memoryTypeCount do
        val ok = (mask & (1 << i)) != 0
        val flags = memProps.memoryTypes(i).propertyFlags
        if ok && (flags & properties) == properties then return i
        i += 1
      -1

    private def roundUp(value: Long, align: Int): Long =
      val a = align.toLong
      (value + a - 1L) / a * a

    private def buildCopyProgram(): Int =
      val vs = glCreateShader(GL_VERTEX_SHADER)
      glShaderSource(vs, "#version 120\nvarying vec2 vUv;\nvoid main(){gl_Position=gl_Vertex;vUv=gl_MultiTexCoord0.xy;}\n")
      glCompileShader(vs)
      val fs = glCreateShader(GL_FRAGMENT_SHADER)
      glShaderSource(fs, """#version 120
        |uniform sampler2D depthTex;
        |uniform sampler2D normalTex;
        |uniform sampler2D waterNTex;
        |uniform sampler2D waterDTex;
        |uniform int hasWater;
        |varying vec2 vUv;
        |void main(){
        |  vec3 n = texture2D(normalTex, vUv).rgb;
        |  float d = texture2D(depthTex, vUv).r;
        |  gl_FragData[0] = vec4(n, d);
        |  if (hasWater == 0) { gl_FragData[1] = vec4(0.0); return; }
        |  vec4 water = texture2D(waterNTex, vUv);
        |  float wd = texture2D(waterDTex, vUv).r;
        |  gl_FragData[1] = vec4(water.rgb, water.a > 0.5 ? wd : 0.0);
        |}
        |""".stripMargin)
      glCompileShader(fs)
      if glGetShaderi(fs, GL_COMPILE_STATUS) == 0 then throw IllegalStateException("ray-tracing copy shader: " + glGetShaderInfoLog(fs, 4096))
      val program = glCreateProgram()
      glAttachShader(program, vs); glAttachShader(program, fs); glLinkProgram(program)
      if glGetProgrami(program, GL_LINK_STATUS) == 0 then throw IllegalStateException("ray-tracing copy link: " + glGetProgramInfoLog(program, 4096))
      glDeleteShader(vs); glDeleteShader(fs)
      program

  private def check(result: Int, what: String): Unit =
    if result != VK_SUCCESS then throw IllegalStateException(s"$what failed (Vulkan $result)")

  private final class Grow(initial: Int):
    private var values = new Array[Float](initial)
    private var used = 0
    def add(v: Float): Grow =
      if used == values.length then
        val next = new Array[Float](values.length * 2)
        System.arraycopy(values, 0, next, 0, used)
        values = next
      values(used) = v
      used += 1
      this
    def add(v: Int): Grow = add(v.toFloat)
    def toArray: Array[Float] =
      val out = new Array[Float](used)
      System.arraycopy(values, 0, out, 0, used)
      out

  private def openDevice(): Device =
    val caps = org.lwjgl.opengl.GL.getCapabilities
    if !caps.GL_EXT_memory_object_fd || !caps.GL_EXT_semaphore_fd then throw IllegalStateException("this OpenGL context has no memory/semaphore interop")
    val glUuid = BufferUtils.createByteBuffer(16)
    GLMem.glGetUnsignedBytevEXT(GLMem.GL_DEVICE_UUID_EXT, glUuid)
    val stack = MemoryStack.stackPush()
    try
      val app = VkApplicationInfo.calloc(stack).sType(VK_STRUCTURE_TYPE_APPLICATION_INFO).apiVersion(VK_API_VERSION_1_2)
      val instanceInfo = VkInstanceCreateInfo.calloc(stack).sType(VK_STRUCTURE_TYPE_INSTANCE_CREATE_INFO).pApplicationInfo(app)
      val instanceOut = stack.mallocPointer(1)
      check(vkCreateInstance(instanceInfo, null, instanceOut), "instance")
      val instance = new VkInstance(instanceOut.get(0), instanceInfo)
      val devices = stack.mallocPointer(16)
      val count = stack.mallocInt(1)
      check(vkEnumeratePhysicalDevices(instance, count, null), "device count")
      val n = math.min(count.get(0), 16)
      check(vkEnumeratePhysicalDevices(instance, count, devices), "devices")
      var chosen: VkPhysicalDevice | Null = null
      var name = ""
      var i = 0
      while i < n && chosen == null do
        val physical = new VkPhysicalDevice(devices.get(i), instance)
        val id = VkPhysicalDeviceIDProperties.calloc(stack).sType(VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_ID_PROPERTIES)
        val props2 = VkPhysicalDeviceProperties2.calloc(stack).sType(VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PROPERTIES_2).pNext(id.address())
        vkGetPhysicalDeviceProperties2(physical, props2)
        val uuid = id.deviceUUID()
        var same = true
        var b = 0
        while b < 16 && same do
          if uuid.get(b) != glUuid.get(b) then same = false
          b += 1
        if same then
          chosen = physical
          name = props2.properties.deviceNameString
        i += 1
      if chosen == null then
        vkDestroyInstance(instance, null)
        throw IllegalStateException("no Vulkan device matches this OpenGL context")
      val ray = VkPhysicalDeviceRayQueryFeaturesKHR.calloc(stack).sType(VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_RAY_QUERY_FEATURES_KHR)
      val accel = VkPhysicalDeviceAccelerationStructureFeaturesKHR.calloc(stack).sType(VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_ACCELERATION_STRUCTURE_FEATURES_KHR).pNext(ray.address())
      val address = VkPhysicalDeviceBufferDeviceAddressFeatures.calloc(stack).sType(VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_BUFFER_DEVICE_ADDRESS_FEATURES).pNext(accel.address())
      val features = VkPhysicalDeviceFeatures2.calloc(stack).sType(VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FEATURES_2).pNext(address.address())
      vkGetPhysicalDeviceFeatures2(chosen, features)
      if !ray.rayQuery || !accel.accelerationStructure || !address.bufferDeviceAddress || !features.features.shaderInt64 then
        vkDestroyInstance(instance, null)
        throw IllegalStateException(s"$name does not expose ray queries")
      var family = -1
      val qCount = stack.mallocInt(1)
      vkGetPhysicalDeviceQueueFamilyProperties(chosen, qCount, null)
      val queues = VkQueueFamilyProperties.calloc(qCount.get(0), stack)
      vkGetPhysicalDeviceQueueFamilyProperties(chosen, qCount, queues)
      var q = 0
      while q < qCount.get(0) && family < 0 do
        val flags = queues.get(q).queueFlags
        if (flags & VK_QUEUE_GRAPHICS_BIT) != 0 && (flags & VK_QUEUE_COMPUTE_BIT) != 0 then family = q
        q += 1
      if family < 0 then
        vkDestroyInstance(instance, null)
        throw IllegalStateException("no graphics+compute queue")
      ray.rayQuery(true); accel.accelerationStructure(true); address.bufferDeviceAddress(true); features.features.shaderInt64(true)
      val queueInfo = VkDeviceQueueCreateInfo.calloc(1, stack)
      queueInfo.get(0).sType(VK_STRUCTURE_TYPE_DEVICE_QUEUE_CREATE_INFO).queueFamilyIndex(family).pQueuePriorities(stack.floats(1f))
      val extensions = stack.mallocPointer(5)
      extensions.put(stack.UTF8(VK_KHR_RAY_QUERY_EXTENSION_NAME))
      extensions.put(stack.UTF8(VK_KHR_ACCELERATION_STRUCTURE_EXTENSION_NAME))
      extensions.put(stack.UTF8(VK_KHR_DEFERRED_HOST_OPERATIONS_EXTENSION_NAME))
      extensions.put(stack.UTF8(VK_KHR_EXTERNAL_MEMORY_FD_EXTENSION_NAME))
      extensions.put(stack.UTF8(VK_KHR_EXTERNAL_SEMAPHORE_FD_EXTENSION_NAME))
      extensions.flip()
      val deviceInfo = VkDeviceCreateInfo.calloc(stack).sType(VK_STRUCTURE_TYPE_DEVICE_CREATE_INFO).pNext(features.address()).pQueueCreateInfos(queueInfo).ppEnabledExtensionNames(extensions)
      val deviceOut = stack.mallocPointer(1)
      check(vkCreateDevice(chosen, deviceInfo, null, deviceOut), "device")
      val device = new VkDevice(deviceOut.get(0), chosen, deviceInfo)
      val queueOut = stack.mallocPointer(1)
      vkGetDeviceQueue(device, family, 0, queueOut)
      new Device(instance, chosen, device, new VkQueue(queueOut.get(0), device), family, s"$name, hardware ray queries")
    finally stack.close()

  // Device.open is the public constructor path; keep Vulkan types out of the caller's class init.
  private object Device:
    def open(): Device = openDevice()
