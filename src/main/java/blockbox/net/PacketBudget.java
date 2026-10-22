package blockbox.net;

/** Per-connection token bucket. Call from the connection's reader thread. */
public final class PacketBudget {
  private final double rate;
  private final double capacity;
  private double tokens;
  private long lastNanos;

  public PacketBudget(double rate, double capacity, long nowNanos) {
    if (!(rate > 0) || !(capacity >= 1) || !Double.isFinite(rate) || !Double.isFinite(capacity)) throw new IllegalArgumentException("invalid packet budget");
    this.rate = rate;
    this.capacity = capacity;
    this.tokens = capacity;
    this.lastNanos = nowNanos;
  }

  public boolean allow(long nowNanos) {
    // nanoTime's signed value may wrap; subtraction remains valid across wrap.
    long delta=nowNanos-lastNanos;
    double elapsed=delta>0 ? delta/1_000_000_000.0 : 0;
    if (delta>0) lastNanos=nowNanos;
    tokens = Math.min(capacity, tokens + elapsed * rate);
    if (tokens < 1) return false;
    tokens -= 1;
    return true;
  }
}
