package blockbox.render;

/** Six clip planes extracted from OpenGL column-major matrices, reused each frame. */
public final class ViewFrustum {
  private final float[] clip = new float[16];
  private final float[] planes = new float[24];

  public void update(float[] projection, float[] view) {
    for (int column = 0; column < 4; column++) {
      for (int row = 0; row < 4; row++) {
        float value = 0;
        for (int k = 0; k < 4; k++) value += projection[k * 4 + row] * view[column * 4 + k];
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

  public boolean intersects(float minX, float minY, float minZ, float maxX, float maxY, float maxZ) {
    for (int p = 0; p < planes.length; p += 4) {
      float x = planes[p] >= 0 ? maxX : minX;
      float y = planes[p + 1] >= 0 ? maxY : minY;
      float z = planes[p + 2] >= 0 ? maxZ : minZ;
      if (planes[p] * x + planes[p + 1] * y + planes[p + 2] * z + planes[p + 3] < -0.01f) return false;
    }
    return true;
  }
}
