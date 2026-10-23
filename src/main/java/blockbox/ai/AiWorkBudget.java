package blockbox.ai;

/**
 * Render-thread token bucket for expensive mob planning. Movement and collision
 * never wait for tokens; a mob follows its previous safe route until it can plan.
 */
public final class AiWorkBudget {
  private final double rate;
  private final int burst;
  private double tokens;

  public AiWorkBudget(double searchesPerSecond, int maximumBurst) {
    if (!Double.isFinite(searchesPerSecond) || searchesPerSecond <= 0 || maximumBurst < 1
        || maximumBurst > 32)
      throw new IllegalArgumentException("invalid AI work budget");
    rate = searchesPerSecond;
    burst = maximumBurst;
    tokens = maximumBurst;
  }

  public void advance(float seconds) {
    if (!Float.isFinite(seconds) || seconds < 0)
      throw new IllegalArgumentException("invalid AI time step");
    tokens = Math.min(burst, tokens + seconds * rate);
  }

  public boolean tryAcquire() {
    if (tokens < 1)
      return false;
    tokens -= 1;
    return true;
  }
}
