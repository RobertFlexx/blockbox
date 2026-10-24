package blockbox

/** Shader and compatibility draws must share a forward, nonzero linear fog interval. */
object WorldFog:
  final case class Range(start: Float, end: Float)

  def range(distance: Float, density: Float, rain: Float, underwater: Boolean): Range =
    if underwater then Range(0.6f, 25f)
    else
      val end = distance * (0.95f - rain.max(0).min(1) * 0.22f)
      val wantedStart = distance * (0.65f * 1.6f / density.max(0.6f)).max(0.25f).min(0.85f)
      // Rain shortens visibility. Keep a gradual fade even at the lightest fog setting;
      // a reversed interval makes fixed-function entities turn solid fog grey nearby.
      Range(wantedStart.min(end - distance * 0.16f), end)
