package blockbox.render;

import java.nio.ByteBuffer;
import java.nio.ByteOrder;

/** Validated, allocation-free packing shared by the Scala RT engine and GPU loop. */
public final class RayFrameLayout {
  public static final int FRAME_BYTES = 336;
  private RayFrameLayout() {}
  public static int groups(int extent) {
    if (extent < 1 || extent > 32768)
      throw new IllegalArgumentException("invalid ray target extent");
    return (extent + 7) >>> 3;
  }
  public static void matrix(ByteBuffer dst, float[] matrix) {
    if (matrix == null || matrix.length != 16 || dst.remaining() < 64)
      throw new IllegalArgumentException("invalid ray matrix");
    for (float value : matrix)
      if (!Float.isFinite(value))
        throw new IllegalArgumentException("non-finite ray matrix");
    dst.order(ByteOrder.nativeOrder());
    for (float value : matrix) dst.putFloat(value);
  }
  public static void vector(ByteBuffer dst, float x, float y, float z, float w) {
    if (dst.remaining() < 16 || !Float.isFinite(x) || !Float.isFinite(y) || !Float.isFinite(z)
        || !Float.isFinite(w))
      throw new IllegalArgumentException("invalid ray frame vector");
    dst.order(ByteOrder.nativeOrder()).putFloat(x).putFloat(y).putFloat(z).putFloat(w);
  }
}
