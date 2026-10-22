package blockbox;

import blockbox.render.RayFrameLayout;
import org.lwjgl.BufferUtils;
import org.lwjgl.opengl.EXTSemaphore;
import org.lwjgl.system.MemoryStack;
import org.lwjgl.vulkan.*;
import java.nio.IntBuffer;
import static org.lwjgl.vulkan.VK10.*;
import static org.lwjgl.vulkan.VK11.VK_QUEUE_FAMILY_EXTERNAL;
import static org.lwjgl.vulkan.KHRAccelerationStructure.*;

/** Direct LWJGL Vulkan frame loop. Scene ownership, BLAS/TLAS and shading stay in
 * Scala/GLSL. This mixed-source helper compiles with the engine's LWJGL classpath. */
public final class HardwareRayLoop implements AutoCloseable {
  private final VkDevice device;
  private final VkQueue queue;
  private final VkCommandBuffer commands;
  private final int family;
  private final long[] sharedImages=new long[5];
  private final long fence, owner=Thread.currentThread().threadId();
  private final IntBuffer outputs=BufferUtils.createIntBuffer(5);
  private final IntBuffer layouts=BufferUtils.createIntBuffer(5);
  private final IntBuffer noBuffers=BufferUtils.createIntBuffer(0);
  private boolean pending, recording, closed;
  private long frames;

  public HardwareRayLoop(VkDevice device,VkQueue queue,VkCommandBuffer commands,int family) {
    this.device=java.util.Objects.requireNonNull(device);
    this.queue=java.util.Objects.requireNonNull(queue);
    this.commands=java.util.Objects.requireNonNull(commands);
    if (family<0) throw new IllegalArgumentException("invalid ray queue family");
    this.family=family;
    try (MemoryStack stack=MemoryStack.stackPush()) {
      var info=VkFenceCreateInfo.calloc(stack).sType(VK_STRUCTURE_TYPE_FENCE_CREATE_INFO);
      var out=stack.mallocLong(1);
      check(vkCreateFence(device,info,null,out),"create ray frame fence"); fence=out.get(0);
    }
    for (int i=0;i<5;i++) layouts.put(i,EXTSemaphore.GL_LAYOUT_GENERAL_EXT);
  }
  private void owned() {
    if (closed || owner!=Thread.currentThread().threadId()) throw new IllegalStateException("ray loop used outside its render thread/lifetime");
  }
  /** Must precede mapped-buffer writes, geometry changes, or command-buffer reuse. */
  public void awaitFrame() {
    owned();
    if (pending) {
      check(vkWaitForFences(device,fence,true,5_000_000_000L),"wait for ray frame"); pending=false;
    }
  }
  public void begin(long gbuffer,long water,long lighting,long reflection,long volume) {
    owned();
    if (pending || recording) throw new IllegalStateException("ray command buffer is busy");
    if (gbuffer==0 || water==0 || lighting==0 || reflection==0 || volume==0) throw new IllegalArgumentException("missing shared ray images");
    sharedImages[0]=gbuffer; sharedImages[1]=water; sharedImages[2]=lighting; sharedImages[3]=reflection; sharedImages[4]=volume;
    check(vkResetCommandBuffer(commands,0),"reset ray commands");
    try (MemoryStack stack=MemoryStack.stackPush()) {
      var begin=VkCommandBufferBeginInfo.calloc(stack).sType(VK_STRUCTURE_TYPE_COMMAND_BUFFER_BEGIN_INFO).flags(VK_COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT);
      check(vkBeginCommandBuffer(commands,begin),"begin ray commands");
      transferImages(stack,true);
      barrier(stack,VK_PIPELINE_STAGE_HOST_BIT,VK_PIPELINE_STAGE_ACCELERATION_STRUCTURE_BUILD_BIT_KHR|VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT,
        VK_ACCESS_HOST_WRITE_BIT,VK_ACCESS_ACCELERATION_STRUCTURE_READ_BIT_KHR|VK_ACCESS_UNIFORM_READ_BIT|VK_ACCESS_SHADER_READ_BIT);
    }
    recording=true;
  }
  private void barrier(MemoryStack stack,int source,int target,int sourceAccess,int targetAccess) {
    var memory=VkMemoryBarrier.calloc(1,stack);
    memory.get(0).sType(VK_STRUCTURE_TYPE_MEMORY_BARRIER).srcAccessMask(sourceAccess).dstAccessMask(targetAccess);
    vkCmdPipelineBarrier(commands,source,target,0,memory,null,null);
  }
  private void transferImages(MemoryStack stack,boolean acquire) {
    var images=VkImageMemoryBarrier.calloc(5,stack);
    for (int i=0;i<5;i++) {
      var image=images.get(i);
      image.sType(VK_STRUCTURE_TYPE_IMAGE_MEMORY_BARRIER).image(sharedImages[i])
        .oldLayout(VK_IMAGE_LAYOUT_GENERAL).newLayout(VK_IMAGE_LAYOUT_GENERAL)
        .srcQueueFamilyIndex(acquire ? VK_QUEUE_FAMILY_EXTERNAL : family)
        .dstQueueFamilyIndex(acquire ? family : VK_QUEUE_FAMILY_EXTERNAL)
        .srcAccessMask(acquire ? 0 : VK_ACCESS_SHADER_READ_BIT|VK_ACCESS_SHADER_WRITE_BIT)
        .dstAccessMask(acquire ? VK_ACCESS_SHADER_READ_BIT|VK_ACCESS_SHADER_WRITE_BIT : 0);
      image.subresourceRange().aspectMask(VK_IMAGE_ASPECT_COLOR_BIT).levelCount(1).layerCount(1);
    }
    vkCmdPipelineBarrier(commands,acquire ? VK_PIPELINE_STAGE_TOP_OF_PIPE_BIT : VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT,
      acquire ? VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT : VK_PIPELINE_STAGE_BOTTOM_OF_PIPE_BIT,0,null,null,images);
  }
  public void dispatch(long pipeline,long layout,long descriptors,long fromGl,long toGl,int width,int height,
                       int glSemaphore,int gbuffer,int water,int lighting,int reflection,int volume) {
    owned();
    if (!recording || pipeline==0 || layout==0 || descriptors==0 || fromGl==0 || toGl==0 || glSemaphore==0)
      throw new IllegalStateException("incomplete hardware ray frame");
    int gx=RayFrameLayout.groups(width),gy=RayFrameLayout.groups(height);
    try (MemoryStack stack=MemoryStack.stackPush()) {
      barrier(stack,VK_PIPELINE_STAGE_ACCELERATION_STRUCTURE_BUILD_BIT_KHR,VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT,
        VK_ACCESS_ACCELERATION_STRUCTURE_WRITE_BIT_KHR,VK_ACCESS_ACCELERATION_STRUCTURE_READ_BIT_KHR);
      vkCmdBindPipeline(commands,VK_PIPELINE_BIND_POINT_COMPUTE,pipeline);
      vkCmdBindDescriptorSets(commands,VK_PIPELINE_BIND_POINT_COMPUTE,layout,0,stack.longs(descriptors),null);
      vkCmdDispatch(commands,gx,gy,1);
      barrier(stack,VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT,VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT,
        VK_ACCESS_SHADER_WRITE_BIT,VK_ACCESS_SHADER_READ_BIT);
      transferImages(stack,false);
      check(vkEndCommandBuffer(commands),"end ray commands"); recording=false;
      check(vkResetFences(device,fence),"reset ray frame fence");
      var submit=VkSubmitInfo.calloc(stack).sType(VK_STRUCTURE_TYPE_SUBMIT_INFO)
        .pWaitSemaphores(stack.longs(fromGl)).pWaitDstStageMask(stack.ints(VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT|VK_PIPELINE_STAGE_ACCELERATION_STRUCTURE_BUILD_BIT_KHR))
        .pCommandBuffers(stack.pointers(commands.address())).pSignalSemaphores(stack.longs(toGl));
      check(vkQueueSubmit(queue,submit,fence),"submit hardware ray frame"); pending=true;
    }
    outputs.put(0,gbuffer).put(1,water).put(2,lighting).put(3,reflection).put(4,volume);
    EXTSemaphore.glWaitSemaphoreEXT(glSemaphore,noBuffers,outputs,layouts);
    frames++;
  }
  public long submittedFrames() { return frames; }
  @Override public void close() {
    if (closed) return;
    awaitFrame(); closeAfterDeviceIdle();
  }
  /** Teardown after Scala has idled the device (or received DEVICE_LOST).
   * A lost device cannot signal the normal frame fence again. */
  public void closeAfterDeviceIdle() {
    if (closed) return;
    owned(); vkDestroyFence(device,fence,null);
    pending=false; recording=false; closed=true;
  }
  private static void check(int result,String action) {
    if (result!=VK_SUCCESS) throw new IllegalStateException(action+" failed (Vulkan "+result+")");
  }
}
