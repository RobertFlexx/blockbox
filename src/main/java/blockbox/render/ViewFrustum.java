package blockbox.render;

/** Six clip planes extracted from OpenGL column-major matrices, reused each frame. */
public final class ViewFrustum {
  private final float[] clip = new float[16];
  private final float[] planes = new float[24];

  public void update(float[] projection, float[] view) {
    if (projection == null || view == null || projection.length != 16 || view.length != 16)
      throw new IllegalArgumentException("frustum requires two 4x4 matrices");
    for (int i = 0; i < 16; i++)
      if (!Float.isFinite(projection[i]) || !Float.isFinite(view[i]))
        throw new IllegalArgumentException("non-finite frustum matrix");
    for (int column = 0; column < 4; column++) {
      for (int row = 0; row < 4; row++) {
        float value = 0;
        for (int k = 0; k < 4; k++) value += projection[k * 4 + row] * view[column * 4 + k];
        if (!Float.isFinite(value))
          throw new IllegalArgumentException("frustum matrix overflow");
        clip[column * 4 + row] = value;
      }
    }
    for (int axis = 0; axis < 3; axis++) {
      for (int side = 0; side < 2; side++) {
        int offset = (axis * 2 + side) * 4;
        float sign = side == 0 ? 1 : -1;
        for (int column = 0; column < 4; column++) {
          planes[offset + column] = clip[column * 4 + 3] + sign * clip[column * 4 + axis];
        }
      }
    }
  }

  public boolean intersects(
      float minX, float minY, float minZ, float maxX, float maxY, float maxZ) {
    if (!Float.isFinite(minX) || !Float.isFinite(minY) || !Float.isFinite(minZ)
        || !Float.isFinite(maxX) || !Float.isFinite(maxY) || !Float.isFinite(maxZ) || minX > maxX
        || minY > maxY || minZ > maxZ)
      throw new IllegalArgumentException("invalid frustum bounds");
    for (int p = 0; p < planes.length; p += 4) {
      float x = planes[p] >= 0 ? maxX : minX;
      float y = planes[p + 1] >= 0 ? maxY : minY;
      float z = planes[p + 2] >= 0 ? maxZ : minZ;
      if (planes[p] * x + planes[p + 1] * y + planes[p + 2] * z + planes[p + 3] < -0.01f)
        return false;
    }
    return true;
  }
}
