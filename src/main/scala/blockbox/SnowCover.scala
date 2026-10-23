package blockbox

object SnowCover:
  def advance(current: Float, precipitation: Float, dt: Float): Float =
    val fall = precipitation.max(0).min(1)
    val rate = if fall > 0.12f then fall / 80f else -1f / 600f
    (current + rate * dt.max(0).min(1)).max(0).min(1)
  def level(amount: Float): Int = math.round(amount.max(0).min(1) * 8)
  def cold(biome: TerrainBiome): Boolean =
    biome == TerrainBiome.SnowyTaiga || biome == TerrainBiome.SnowyTundra ||
      biome == TerrainBiome.JaggedPeaks || biome == TerrainBiome.Alpine
