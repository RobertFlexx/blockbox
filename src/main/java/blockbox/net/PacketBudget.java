package blockbox.net;

/** Per-connection token bucket. Call from the connection's reader thread. */
public final class PacketBudget {
  private final double rate;
  private final double capacity;
  private double tokens;
  private long lastNanos;

  public PacketBudget(double rate, double capacity, long nowNanos) {
    if (!(rate > 0) || !(capacity >= 1)) throw new IllegalArgumentException("invalid packet budget");
    this.rate = rate;
    this.capacity = capacity;
    this.tokens = capacity;
    this.lastNanos = nowNanos;
  }

  public boolean allow(long nowNanos) {
    double elapsed = Math.max(0, (nowNanos - lastNanos) / 1_000_000_000.0);
    lastNanos = Math.max(lastNanos, nowNanos);
    tokens = Math.min(capacity, tokens + elapsed * rate);
    if (tokens < 1) return false;
    tokens -= 1;
    return true;
  }
}
