package blockbox;

import blockbox.render.RayFrameLayout;
import org.lwjgl.system.MemoryStack;
import org.lwjgl.vulkan.*;
import static org.lwjgl.vulkan.VK10.*;
import static org.lwjgl.vulkan.KHRAccelerationStructure.*;

/**
 * Direct LWJGL Vulkan frame loop. Scene ownership, BLAS/TLAS and shading stay in
 * Scala/GLSL. This mixed-source helper compiles with the engine's LWJGL classpath.
 */
public final class HardwareRayLoop implements AutoCloseable {
  private final VkDevice device;
  private final VkQueue queue;
  private final VkCommandBuffer commands;
  private final int family;
  private final long fence, owner = Thread.currentThread().threadId();
  private long transfer;
  private boolean pending, recording, closed;
  private long frames;

  public HardwareRayLoop(VkDevice device, VkQueue queue, VkCommandBuffer commands, int family) {
    this.device = java.util.Objects.requireNonNull(device);
    this.queue = java.util.Objects.requireNonNull(queue);
    this.commands = java.util.Objects.requireNonNull(commands);
    if (family < 0)
      throw new IllegalArgumentException("invalid ray queue family");
    this.family = family;
    try (MemoryStack stack = MemoryStack.stackPush()) {
      var info = VkFenceCreateInfo.calloc(stack).sType(VK_STRUCTURE_TYPE_FENCE_CREATE_INFO);
      var out = stack.mallocLong(1);
      check(vkCreateFence(device, info, null, out), "create ray frame fence");
      fence = out.get(0);
    }
  }
  private void owned() {
    if (closed || owner != Thread.currentThread().threadId())
      throw new IllegalStateException("ray loop used outside its render thread/lifetime");
  }
  /** Must precede mapped-buffer writes, geometry changes, or command-buffer reuse. */
  public void awaitFrame() {
    owned();
    if (pending) {
      check(vkWaitForFences(device, fence, true, 5_000_000_000L), "wait for ray frame");
      pending = false;
    }
  }
  public void begin(
      long gbuffer, long water, long transferBuffer, long inputStride, int width, int height) {
    owned();
    if (pending || recording)
      throw new IllegalStateException("ray command buffer is busy");
    if (gbuffer == 0 || water == 0 || transferBuffer == 0)
      throw new IllegalArgumentException("missing shared ray images");
    transfer = transferBuffer;
    if (inputStride < (long) width * height * 16 || (inputStride & 255) != 0)
      throw new IllegalArgumentException("invalid ray input transfer");
    check(vkResetCommandBuffer(commands, 0), "reset ray commands");
    try (MemoryStack stack = MemoryStack.stackPush()) {
      var begin = VkCommandBufferBeginInfo.calloc(stack)
                      .sType(VK_STRUCTURE_TYPE_COMMAND_BUFFER_BEGIN_INFO)
                      .flags(VK_COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT);
      check(vkBeginCommandBuffer(commands, begin), "begin ray commands");
      barrier(stack, VK_PIPELINE_STAGE_HOST_BIT, VK_PIPELINE_STAGE_TRANSFER_BIT,
          VK_ACCESS_HOST_WRITE_BIT, VK_ACCESS_TRANSFER_READ_BIT);
      copyInput(stack, gbuffer, 0, width, height);
      copyInput(stack, water, inputStride, width, height);
      barrier(stack, VK_PIPELINE_STAGE_TRANSFER_BIT, VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT,
          VK_ACCESS_TRANSFER_WRITE_BIT, VK_ACCESS_SHADER_READ_BIT);
      barrier(stack, VK_PIPELINE_STAGE_HOST_BIT,
          VK_PIPELINE_STAGE_ACCELERATION_STRUCTURE_BUILD_BIT_KHR
              | VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT,
          VK_ACCESS_HOST_WRITE_BIT,
          VK_ACCESS_ACCELERATION_STRUCTURE_READ_BIT_KHR | VK_ACCESS_UNIFORM_READ_BIT
              | VK_ACCESS_SHADER_READ_BIT);
    }
    recording = true;
  }
  private void barrier(
      MemoryStack stack, int source, int target, int sourceAccess, int targetAccess) {
    var memory = VkMemoryBarrier.calloc(1, stack);
    memory.get(0)
        .sType(VK_STRUCTURE_TYPE_MEMORY_BARRIER)
        .srcAccessMask(sourceAccess)
        .dstAccessMask(targetAccess);
    vkCmdPipelineBarrier(commands, source, target, 0, memory, null, null);
  }
  private void copyInput(MemoryStack stack, long image, long offset, int width, int height) {
    var region = VkBufferImageCopy.calloc(1, stack);
    region.get(0).bufferOffset(offset);
    region.get(0).imageSubresource().aspectMask(VK_IMAGE_ASPECT_COLOR_BIT).layerCount(1);
    region.get(0).imageExtent().set(width, height, 1);
    vkCmdCopyBufferToImage(commands, transfer, image, VK_IMAGE_LAYOUT_GENERAL, region);
  }
  private void copyOutput(MemoryStack stack, long image, long offset, int width, int height) {
    var region = VkBufferImageCopy.calloc(1, stack);
    region.get(0).bufferOffset(offset);
    region.get(0).imageSubresource().aspectMask(VK_IMAGE_ASPECT_COLOR_BIT).layerCount(1);
    region.get(0).imageExtent().set(width, height, 1);
    vkCmdCopyImageToBuffer(commands, image, VK_IMAGE_LAYOUT_GENERAL, transfer, region);
  }
  public void dispatch(long pipeline, long layout, long descriptors, int width, int height,
      long stride, long outputOffset, long lighting, long reflection, long volume) {
    owned();
    if (!recording || pipeline == 0 || layout == 0 || descriptors == 0)
      throw new IllegalStateException("incomplete hardware ray frame");
    int gx = RayFrameLayout.groups(width), gy = RayFrameLayout.groups(height);
    if (lighting == 0 || reflection == 0 || volume == 0 || stride < (long) width * height * 8
        || (stride & 255) != 0)
      throw new IllegalArgumentException("invalid ray output transfer");
    try (MemoryStack stack = MemoryStack.stackPush()) {
      barrier(stack, VK_PIPELINE_STAGE_ACCELERATION_STRUCTURE_BUILD_BIT_KHR,
          VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT, VK_ACCESS_ACCELERATION_STRUCTURE_WRITE_BIT_KHR,
          VK_ACCESS_ACCELERATION_STRUCTURE_READ_BIT_KHR);
      vkCmdBindPipeline(commands, VK_PIPELINE_BIND_POINT_COMPUTE, pipeline);
      vkCmdBindDescriptorSets(
          commands, VK_PIPELINE_BIND_POINT_COMPUTE, layout, 0, stack.longs(descriptors), null);
      vkCmdDispatch(commands, gx, gy, 1);
      barrier(stack, VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT,
          VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT | VK_PIPELINE_STAGE_TRANSFER_BIT,
          VK_ACCESS_SHADER_WRITE_BIT, VK_ACCESS_SHADER_READ_BIT | VK_ACCESS_TRANSFER_READ_BIT);
      copyOutput(stack, lighting, outputOffset, width, height);
      copyOutput(stack, reflection, outputOffset + stride, width, height);
      copyOutput(stack, volume, outputOffset + stride * 2, width, height);
      barrier(stack, VK_PIPELINE_STAGE_TRANSFER_BIT, VK_PIPELINE_STAGE_HOST_BIT,
          VK_ACCESS_TRANSFER_WRITE_BIT, VK_ACCESS_HOST_READ_BIT);
      check(vkEndCommandBuffer(commands), "end ray commands");
      recording = false;
      check(vkResetFences(device, fence), "reset ray frame fence");
      var submit = VkSubmitInfo.calloc(stack)
                       .sType(VK_STRUCTURE_TYPE_SUBMIT_INFO)
                       .pCommandBuffers(stack.pointers(commands.address()));
      check(vkQueueSubmit(queue, submit, fence), "submit hardware ray frame");
      pending = true;
    }
    // Completion plus the TRANSFER -> HOST barrier makes the mapped output
    // safe for GL's client-memory upload. No cross-API semaphore can leak state.
    awaitFrame();
    frames++;
  }
  public long submittedFrames() {
    return frames;
  }
  @Override
  public void close() {
    if (closed)
      return;
    awaitFrame();
    closeAfterDeviceIdle();
  }
  /**
   * Teardown after Scala has idled the device (or received DEVICE_LOST).
   * A lost device cannot signal the normal frame fence again.
   */
  public void closeAfterDeviceIdle() {
    if (closed)
      return;
    owned();
    vkDestroyFence(device, fence, null);
    pending = false;
    recording = false;
    closed = true;
  }
  private static void check(int result, String action) {
    if (result != VK_SUCCESS)
      throw new IllegalStateException(action + " failed (Vulkan " + result + ")");
  }
}
