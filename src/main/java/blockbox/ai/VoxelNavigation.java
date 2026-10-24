package blockbox.ai;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.PriorityQueue;

/** Bounded A* for a two-block-tall actor. Queries never mutate the world. */
public final class VoxelNavigation {
  public static final int SOLID = 1, WATER = 2, HAZARD = 4;
  @FunctionalInterface
  public interface Cells {
    int at(int x, int y, int z);
  }
  public record Step(int x, int y, int z) {}
  private static final class Node {
    final Step cell;
    final Node parent;
    final float g, f;
    Node(Step cell, Node parent, float g, float f) {
      this.cell = cell;
      this.parent = parent;
      this.g = g;
      this.f = f;
    }
  }
  private VoxelNavigation() {}

  private static boolean clear(Cells cells, int x, int y, int z) {
    return ((cells.at(x, y, z) | cells.at(x, y + 1, z)) & (SOLID | HAZARD)) == 0;
  }
  /** Feet in water can rise one block into more water, or breach one block of air. */
  private static boolean canSwimUp(Cells cells, int x, int y, int z) {
    int above = y + 1;
    if (!clear(cells, x, above, z))
      return false;
    int flags = cells.at(x, above, z);
    if ((flags & HAZARD) != 0)
      return false;
    return (flags & WATER) != 0 || (flags & SOLID) == 0;
  }
  private static boolean supported(Cells cells, int x, int y, int z) {
    int below = cells.at(x, y - 1, z), here = cells.at(x, y, z);
    return (below & HAZARD) == 0 && ((below & SOLID) != 0 || (here & WATER) != 0);
  }
  private static int landing(Cells cells, int x, int y, int z) {
    if (clear(cells, x, y, z) && supported(cells, x, y, z))
      return y;
    // A step up requires clearance over the actor before the jump.
    if ((cells.at(x, y, z) & SOLID) != 0 && clear(cells, x, y + 1, z)
        && supported(cells, x, y + 1, z))
      return y + 1;
    for (int drop = 1; drop <= 3; drop++) {
      if (!clear(cells, x, y - drop, z))
        break;
      if (supported(cells, x, y - drop, z))
        return y - drop;
    }
    return Integer.MIN_VALUE;
  }
  private static float heuristic(Step p, int tx, int ty, int tz) {
    return Math.max(Math.abs(p.x - tx), Math.abs(p.z - tz)) + Math.abs(p.y - ty) * 0.3f;
  }

  /**
   * Returns a route excluding the start, or a safe partial route when the budget
   * is exhausted. Radius and expansion limits bound work even for unreachable goals.
   */
  public static List<Step> find(
      Cells cells, int sx, int sy, int sz, int tx, int ty, int tz, int radius, int maxNodes) {
    if (cells == null || radius < 1 || radius > 48 || maxNodes < 1 || maxNodes > 4096)
      throw new IllegalArgumentException("invalid navigation bounds");
    if (Math.abs((long) sx) > 30_000_000 || Math.abs((long) sz) > 30_000_000
        || Math.abs((long) tx) > 30_000_000 || Math.abs((long) tz) > 30_000_000 || sy < 0
        || sy > 510 || ty < 0 || ty > 510)
      return List.of();
    Step start = new Step(sx, sy, sz);
    PriorityQueue<Node> open = new PriorityQueue<>(
        Comparator.comparingDouble((Node n) -> n.f).thenComparingDouble(n -> n.g));
    HashMap<Step, Float> scores = new HashMap<>();
    Node first = new Node(start, null, 0, heuristic(start, tx, ty, tz)), best = first;
    open.add(first);
    scores.put(start, 0f);
    int expanded = 0;
    while (!open.isEmpty() && expanded++ < maxNodes) {
      Node current = open.remove();
      Step c = current.cell;
      if (current.g > scores.getOrDefault(c, Float.POSITIVE_INFINITY))
        continue;
      if (heuristic(c, tx, ty, tz) < heuristic(best.cell, tx, ty, tz))
        best = current;
      if (c.x == tx && c.z == tz && Math.abs(c.y - ty) <= 1) {
        best = current;
        break;
      }
      // A stroke straight up through water, including one block of air above the
      // surface. Without this, a bank higher than the water column is unreachable
      // and the mob treads water against the wall.
      if ((cells.at(c.x, c.y, c.z) & WATER) != 0) {
        int up = c.y + 1;
        if (up <= 509 && Math.abs(up - sy) <= 12 && canSwimUp(cells, c.x, c.y, c.z)) {
          Step next = new Step(c.x, up, c.z);
          float cost = 1.2f + ((cells.at(c.x, up, c.z) & WATER) != 0 ? 3f : 0f);
          float g = current.g + cost;
          if (g < scores.getOrDefault(next, Float.POSITIVE_INFINITY)) {
            scores.put(next, g);
            open.add(new Node(next, current, g, g + heuristic(next, tx, ty, tz)));
          }
        }
      }
      for (int dx = -1; dx <= 1; dx++)
        for (int dz = -1; dz <= 1; dz++) {
          if (dx == 0 && dz == 0)
            continue;
          int x = c.x + dx, z = c.z + dz;
          if (Math.abs(x - sx) > radius || Math.abs(z - sz) > radius)
            continue;
          int y = landing(cells, x, c.y, z);
          if (y == Integer.MIN_VALUE || y < 1 || y > 509 || Math.abs(y - sy) > 12)
            continue;
          if (y > c.y && !clear(cells, c.x, c.y + 1, c.z))
            continue;
          // Diagonal moves must fit the whole body through both corner cells.
          if (dx != 0 && dz != 0
              && (!clear(cells, c.x + dx, Math.max(y, c.y), c.z)
                  || !clear(cells, c.x, Math.max(y, c.y), c.z + dz)))
            continue;
          Step next = new Step(x, y, z);
          float cost = (dx != 0 && dz != 0 ? 1.414214f : 1f) + Math.abs(y - c.y) * 0.35f
              + ((cells.at(x, y, z) & WATER) != 0 ? 3f : 0f);
          float g = current.g + cost;
          if (g >= scores.getOrDefault(next, Float.POSITIVE_INFINITY))
            continue;
          scores.put(next, g);
          open.add(new Node(next, current, g, g + heuristic(next, tx, ty, tz)));
        }
    }
    ArrayList<Step> result = new ArrayList<>();
    for (Node n = best; n.parent != null; n = n.parent) result.add(n.cell);
    java.util.Collections.reverse(result);
    return result;
  }
}
