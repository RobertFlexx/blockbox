package blockbox

import java.util.concurrent.ConcurrentHashMap
import scala.collection.mutable.ArrayBuffer
import scala.math.*

object Terrain:
  val seaLevel = 32
  val worldHeight = 128
  val chunkSize = 16

enum TerrainBiome:
  case Ocean, Beach, Wetlands, Plains, Prairie, Meadow, Forest, OldGrowthForest, BirchForest,
    Savanna, Desert, Badlands, Taiga, SnowyTundra, Alpine, StonyHighlands, JaggedPeaks, RockyCoast,
    DuneDesert, SnowyTaiga, RedwoodForest
  case WillowMarsh, AutumnGrove

enum CaveBiome:
  case Normal, Limestone, Lush, Crystal, Rooted, DeepStone

final case class CaveEntrance(
    x: Float,
    y: Float,
    z: Float,
    dx: Float,
    dz: Float,
    length: Float,
    radius: Float,
    bend: Float
)

final class TerrainGenerator(
    val seed: Long,
    decorate: (Int, Int, Array[Byte]) => Unit = (_, _, _) => ()
):
  private val seedHash = (seed ^ (seed >>> 32)).toInt
  private val maxTerrainCacheEntries = 262144
  private val heightCache = new ConcurrentHashMap[Long, java.lang.Integer]()
  private val entranceCache = new ConcurrentHashMap[Long, Option[CaveEntrance]]()
  private final case class TintColumn(height: Int, color: (Float, Float, Float))
  private final case class BiomeColumn(height: Int, biome: TerrainBiome)
  private val grassTintCache = new ConcurrentHashMap[Long, TintColumn]()
  private val foliageTintCache = new ConcurrentHashMap[Long, (Float, Float, Float)]()
  private val biomeCache = new ConcurrentHashMap[Long, BiomeColumn]()

  private def columnKey(x: Int, z: Int): Long =
    (x.toLong << 32) ^ (z.toLong & 0xffffffffL)

  private def hash(x: Int, y: Int, z: Int, base: Int): Float =
    var n = x * 374761393 + y * 668265263 + z * 1442695041 + base * 1274126177 + seedHash * 60493
    n = (n ^ (n >>> 13)) * 1274126177
    ((n ^ (n >>> 16)) & 0x7fffffff).toFloat / 2147483647f

  private def valueNoise2D(x: Double, z: Double, base: Int): Float =
    val xi = floor(x).toInt; val zi = floor(z).toInt
    val xf = x - xi.toDouble; val zf = z - zi.toDouble
    val u = xf * xf * (3.0 - 2.0 * xf)
    val v = zf * zf * (3.0 - 2.0 * zf)
    val a = hash(xi, zi, 0, base).toDouble * 2.0 - 1.0
    val b = hash(xi + 1, zi, 0, base).toDouble * 2.0 - 1.0
    val c = hash(xi, zi + 1, 0, base).toDouble * 2.0 - 1.0
    val d = hash(xi + 1, zi + 1, 0, base).toDouble * 2.0 - 1.0
    val x1 = a + (b - a) * u
    val x2 = c + (d - c) * u
    (x1 + (x2 - x1) * v).toFloat

  private def fbm2D(x: Double, z: Double, octaves: Int, base: Int): Float =
    var total = 0.0; var amp = 1.0; var freq = 1.0; var norm = 0.0
    for _ <- 0 until octaves do
      total += amp * valueNoise2D(x * freq, z * freq, base).toDouble
      norm += amp; amp *= 0.52; freq *= 2.05
    (total / norm).toFloat

  private def valueNoise3D(x: Double, y: Double, z: Double, base: Int): Float =
    val xi = floor(x).toInt; val yi = floor(y).toInt; val zi = floor(z).toInt
    val xf = x - xi.toDouble; val yf = y - yi.toDouble; val zf = z - zi.toDouble
    val u = xf * xf * (3.0 - 2.0 * xf)
    val v = yf * yf * (3.0 - 2.0 * yf)
    val w = zf * zf * (3.0 - 2.0 * zf)
    val aaa = hash(xi, yi, zi, base).toDouble * 2.0 - 1.0;
    val baa = hash(xi + 1, yi, zi, base).toDouble * 2.0 - 1.0
    val aba = hash(xi, yi + 1, zi, base).toDouble * 2.0 - 1.0;
    val bba = hash(xi + 1, yi + 1, zi, base).toDouble * 2.0 - 1.0
    val aab = hash(xi, yi, zi + 1, base).toDouble * 2.0 - 1.0;
    val bab = hash(xi + 1, yi, zi + 1, base).toDouble * 2.0 - 1.0
    val abb = hash(xi, yi + 1, zi + 1, base).toDouble * 2.0 - 1.0;
    val bbb = hash(xi + 1, yi + 1, zi + 1, base).toDouble * 2.0 - 1.0
    val x11 = aaa + (baa - aaa) * u; val x12 = aba + (bba - aba) * u
    val x21 = aab + (bab - aab) * u; val x22 = abb + (bbb - abb) * u
    val y1 = x11 + (x12 - x11) * v; val y2 = x21 + (x22 - x21) * v
    (y1 + (y2 - y1) * w).toFloat

  private def ridgedNoise(x: Double, z: Double, base: Int, octaves: Int): Float =
    var total = 0.0; var amp = 1.0; var freq = 1.0; var norm = 0.0
    for _ <- 0 until octaves do
      val n = 1.0 - abs(valueNoise2D(x * freq, z * freq, base).toDouble)
      total += amp * n * n
      norm += amp; amp *= 0.5; freq *= 2.2
    (total / norm).toFloat

  private def moistureAt(x: Int, z: Int): Float =
    val fx = x.toDouble; val fz = z.toDouble
    // Broad, slow climate cells keep biomes coherent. A small local term breaks
    // hard borders without making snow/desert confetti.
    val broad = fbm2D((fx + 500.0) * 0.00165, (fz - 300.0) * 0.00165, 5, 51)
    val regional = fbm2D((fx - 930.0) * 0.0038, (fz + 760.0) * 0.0038, 4, 53) * 0.26f
    val local = fbm2D((fx - 180.0) * 0.0080, (fz + 90.0) * 0.0080, 3, 52) * 0.12f
    (broad + regional + local).max(-1f).min(1f)

  private def temperatureAt(x: Int, z: Int, h: Int): Float =
    val fx = x.toDouble; val fz = z.toDouble
    // Broad temperature bands make deserts and snow fields read as actual regions.
    // Height cools the value, so mountains can snow without covering normal plains.
    val climateMacro = fbm2D((fx + 50.0) * 0.00095, (fz + 50.0) * 0.00095, 5, 301)
    val climateRegional = fbm2D((fx - 620.0) * 0.0028, (fz + 240.0) * 0.0028, 4, 302) * 0.24f
    climateMacro + climateRegional - ((h - Terrain.seaLevel).toFloat / 158f)

  private def beachAt(h: Int): Boolean = h <= Terrain.seaLevel + 2

  private def coldRegionAt(x: Int, z: Int): Float =
    val broad = fbm2D((x - 2100.0) * 0.00078, (z + 900.0) * 0.00078, 5, 721)
    val regional = fbm2D((x + 740.0) * 0.0018, (z - 1200.0) * 0.0018, 4, 722) * 0.18f
    broad + regional

  private def desertRegionAt(x: Int, z: Int): Float =
    // Broad desert continents first, with only a small detail term. This prevents the old
    // sand-in-the-middle-of-plains look and makes deserts read as their own land.
    val broad = fbm2D((x + 1400.0) * 0.00058, (z - 900.0) * 0.00058, 5, 701)
    val regional = fbm2D((x - 400.0) * 0.00135, (z + 330.0) * 0.00135, 4, 702) * 0.18f
    val edge = fbm2D((x + 90.0) * 0.0032, (z - 260.0) * 0.0032, 3, 703) * 0.08f
    broad + regional + edge

  private def forestRegionAt(x: Int, z: Int): Float =
    val broad = fbm2D((x + 840.0) * 0.0026, (z - 310.0) * 0.0026, 4, 612)
    val groves = fbm2D((x - 340.0) * 0.0085, (z + 190.0) * 0.0085, 3, 613) * 0.16f
    broad + groves

  private def badlandsRegionAt(x: Int, z: Int): Float =
    val broad = fbm2D((x - 3180.0) * 0.00105, (z + 1760.0) * 0.00105, 5, 1801)
    val broken = fbm2D((x + 540.0) * 0.0026, (z - 940.0) * 0.0026, 3, 1802) * 0.22f
    broad + broken

  private def wetlandRegionAt(x: Int, z: Int): Float =
    val broad = fbm2D((x + 2410.0) * 0.00125, (z + 1150.0) * 0.00125, 5, 1811)
    val pools = fbm2D((x - 730.0) * 0.0031, (z + 420.0) * 0.0031, 3, 1812) * 0.20f
    broad + pools

  private def oldGrowthRegionAt(x: Int, z: Int): Float =
    val broad = fbm2D((x - 4200.0) * 0.00115, (z + 2700.0) * 0.00115, 5, 1861)
    val groves = fbm2D((x + 610.0) * 0.0027, (z - 880.0) * 0.0027, 3, 1862) * 0.22f
    broad + groves

  private def prairieRegionAt(x: Int, z: Int): Float =
    val broad = fbm2D((x + 3300.0) * 0.00105, (z - 2400.0) * 0.00105, 5, 1871)
    val sweep = fbm2D((x - 310.0) * 0.0021, (z + 570.0) * 0.0021, 3, 1872) * 0.16f
    broad + sweep

  private def stonyHighlandRegionAt(x: Int, z: Int): Float =
    val broad = fbm2D((x - 1900.0) * 0.00105, (z + 3600.0) * 0.00105, 4, 1881)
    val broken = ridgedNoise((x + 940.0) * 0.0042, (z - 670.0) * 0.0042, 1882, 3) * 0.28f
    broad + broken

  private def mountainBiomeStrengthAt(x: Int, z: Int): Float =
    val chains = fbm2D((x - 1500.0) * 0.00078, (z + 700.0) * 0.00078, 5, 202)
    val ridges = ridgedNoise((x + 410.0) * 0.0028, (z - 70.0) * 0.0028, 201, 4)
    (smooth01((chains + 0.34f) / 0.80f) * (0.58f + ridges * 0.42f)).max(0f).min(1f)

  private def badlandsAt(x: Int, z: Int, h: Int): Boolean =
    h > Terrain.seaLevel + 4 && h < Terrain.seaLevel + 58 && badlandsRegionAt(
      x,
      z
    ) > 0.30f && moistureAt(x, z) < 0.03f && temperatureAt(x, z, h) > 0.02f

  private def wetlandAt(x: Int, z: Int, h: Int): Boolean =
    h >= Terrain.seaLevel - 3 && h <= Terrain.seaLevel + 12 && wetlandRegionAt(
      x,
      z
    ) > 0.28f && moistureAt(x, z) > 0.12f && desertRegionAt(x, z) < 0.20f

  private def taigaAt(x: Int, z: Int, h: Int): Boolean =
    val t = temperatureAt(x, z, h)
    val m = moistureAt(x, z)
    t < -0.20f && t > -0.78f && m > -0.22f && forestRegionAt(x, z) > -0.28f

  private def riverStrengthAt(x: Int, z: Int): Float =
    val fx = x.toDouble; val fz = z.toDouble
    val channelA = abs(fbm2D((fx + 260.0) * 0.0019, (fz - 530.0) * 0.0019, 5, 2401))
    val channelB = abs(fbm2D((fx - 930.0) * 0.0032, (fz + 760.0) * 0.0032, 4, 2402))
    val broadGate = smooth01(
      ((fbm2D((fx + 80.0) * 0.00075, (fz - 110.0) * 0.00075, 4, 2403) + 0.42f) / 0.92f)
    )
    val main = smooth01(((0.090f - channelA) / 0.090f))
    val branch = smooth01(((0.052f - channelB) / 0.052f)) * 0.62f
    ((main + branch) * broadGate).max(0f).min(1f)

  private def lakeStrengthAt(x: Int, z: Int): Float =
    val fx = x.toDouble; val fz = z.toDouble
    val basin = fbm2D((fx - 1700.0) * 0.00105, (fz + 980.0) * 0.00105, 5, 2501)
    val shape = fbm2D((fx + 310.0) * 0.0044, (fz - 630.0) * 0.0044, 3, 2502) * 0.22f
    smooth01(((basin + shape - 0.38f) / 0.34f))

  private def freshwaterAt(x: Int, z: Int, h: Int): Boolean =
    h <= Terrain.seaLevel + 7 && (riverStrengthAt(x, z) > 0.62f || lakeStrengthAt(x, z) > 0.58f)

  private def freshwaterShoreAt(x: Int, z: Int, h: Int): Boolean =
    h <= Terrain.seaLevel + 9 && (riverStrengthAt(x, z) > 0.40f || lakeStrengthAt(x, z) > 0.44f)

  private def waterSurfaceAt(x: Int, z: Int, h: Int): Int =
    // Keep every generated natural water body on one stable surface plane.
    // The old freshwater +1 level made rivers/lakes meet oceans at different
    // heights, which rendered as huge transparent sheets and stair-stepped water.
    Terrain.seaLevel

  private def coldAt(x: Int, z: Int, h: Int): Boolean =
    val t = temperatureAt(x, z, h)
    val coldRegion = coldRegionAt(x, z)
    // Snow should read as a real biome or alpine cap, not random high-elevation paint.
    val alpine = h > Terrain.seaLevel + 76 && t < -0.08f && coldRegion < 0.42f
    val snowField = t < -0.62f && coldRegion < 0.05f
    val deepSnow = t < -0.78f && coldRegion < 0.25f
    alpine || snowField || deepSnow

  private def desertAt(x: Int, z: Int, h: Int): Boolean =
    val m = moistureAt(x, z)
    val t = temperatureAt(x, z, h)
    val score = desertRegionAt(x, z)
    val coherentDesert = score > 0.16f && m < 0.08f && t > -0.06f
    val hotCore = score > 0.28f && m < 0.20f && t > 0.03f
    !beachAt(h) && !freshwaterAt(
      x,
      z,
      h
    ) && h < Terrain.seaLevel + 52 && (coherentDesert || hotCore)

  private def savannaAt(x: Int, z: Int, h: Int): Boolean =
    !desertAt(x, z, h) && h > Terrain.seaLevel + 3 && h < Terrain.seaLevel + 58 && temperatureAt(
      x,
      z,
      h
    ) > 0.06f && moistureAt(x, z) < 0.05f && desertRegionAt(x, z) > -0.12f

  private def birchGroveAt(x: Int, z: Int, h: Int): Boolean =
    !coldAt(x, z, h) && moistureAt(x, z) > 0.12f && forestRegionAt(x, z) > 0.18f && hash(
      Math.floorDiv(x, 22),
      0,
      Math.floorDiv(z, 22),
      6200
    ) > 0.44f

  def savannaBlendAt(x: Int, z: Int, h: Int): Float =
    val t = temperatureAt(x, z, h)
    val m = moistureAt(x, z)
    val edgeNoise = fbm2D((x + 410.0) * 0.0022, (z - 260.0) * 0.0022, 3, 3301) * 0.09f
    val dry = smooth01((0.22f - m + edgeNoise) / 0.50f)
    val warm = smooth01((t + 0.03f + edgeNoise * 0.35f) / 0.38f)
    val region = smooth01((desertRegionAt(x, z) + 0.18f + edgeNoise) / 0.62f)
    if desertAt(x, z, h) || h <= Terrain.seaLevel + 2 then 0f
    else (dry * warm * region).max(0f).min(1f)

  def birchBlendAt(x: Int, z: Int, h: Int): Float =
    val m = moistureAt(x, z)
    val forest = forestRegionAt(x, z)
    val edgeNoise = fbm2D((x - 720.0) * 0.0020, (z + 540.0) * 0.0020, 3, 3302) * 0.10f
    val wet = smooth01((m - 0.02f + edgeNoise) / 0.48f)
    val grove = smooth01((forest - 0.06f + edgeNoise) / 0.46f)
    val coolEnough = 1f - smooth01((temperatureAt(x, z, h) - 0.30f + edgeNoise * 0.30f) / 0.34f)
    if coldAt(x, z, h) || desertAt(x, z, h) || h <= Terrain.seaLevel + 2 then 0f
    else (wet * grove * coolEnough).max(0f).min(1f)

  def grassTintAt(x: Int, z: Int, h: Int): (Float, Float, Float) =
    val key = columnKey(x, z)
    val cached = grassTintCache.get(key)
    if cached != null && cached.height == h then cached.color
    else
      val color = computeGrassTintAt(x, z, h)
      if grassTintCache.size() < maxTerrainCacheEntries then
        grassTintCache.putIfAbsent(key, TintColumn(h, color))
      color

  /** Tree colour follows the ground climate, not the height of each leaf layer. */
  def foliageTintAt(x: Int, z: Int): (Float, Float, Float) =
    val key = columnKey(x, z); val cached = foliageTintCache.get(key)
    if cached != null then cached
    else
      val h = heightAt(x, z)
      val color = biomeAt(x, z, h) match
        case TerrainBiome.AutumnGrove =>
          val blend = (valueNoise2D(x * 0.019, z * 0.019, 1829) + 1f) * 0.5f
          (0.92f + blend * 0.08f, 0.27f + blend * 0.38f, 0.08f + blend * 0.10f)
        case TerrainBiome.WillowMarsh => (0.42f, 0.68f, 0.31f)
        case _                        => grassTintAt(x, z, h)
      if foliageTintCache.size() < maxTerrainCacheEntries then
        foliageTintCache.putIfAbsent(key, color)
      color

  private def computeGrassTintAt(x: Int, z: Int, h: Int): (Float, Float, Float) =
    val savanna = savannaBlendAt(x, z, h).max(0f).min(1f)
    val birch = (birchBlendAt(x, z, h) * (1f - savanna * 0.82f)).max(0f).min(1f)
    val plains = (1f - savanna * 0.86f - birch * 0.82f).max(0.18f)
    val cold = if coldAt(x, z, h) then 0.45f else 0f
    val total = (plains + savanna + birch).max(0.001f)
    // Stronger biome color anchors keep the grass blocks visibly different after
    // texture modulation and lighting: plains is rich green, birch is pale/lush,
    // and savanna is dry olive.
    var r = (plains * 0.52f + birch * 0.68f + savanna * 0.63f) / total
    var g = (plains * 0.76f + birch * 0.83f + savanna * 0.62f) / total
    var b = (plains * 0.34f + birch * 0.44f + savanna * 0.28f) / total
    if cold > 0f then
      val w = (cold * 0.82f).max(0f).min(1f)
      r = r * (1f - w) + 0.50f * w
      g = g * (1f - w) + 0.82f * w
      b = b * (1f - w) + 0.92f * w
    (r.max(0.30f).min(0.82f), g.max(0.44f).min(1.00f), b.max(0.10f).min(0.52f))

  private def rockyAt(x: Int, z: Int, h: Int): Boolean =
    if h < Terrain.seaLevel + 42 then false
    else
      val ridge = ridgedNoise((x + 310.0) * 0.0075, (z - 520.0) * 0.0075, 931, 3)
      val exposure = fbm2D((x - 120.0) * 0.014, (z + 270.0) * 0.014, 3, 932)
      h > Terrain.seaLevel + 90 || (ridge > 0.84f && exposure > 0.20f)

  private def localSlopeAt(x: Int, z: Int, h: Int): Int =
    val h1 = heightAt(x + 1, z)
    val h2 = heightAt(x - 1, z)
    val h3 = heightAt(x, z + 1)
    val h4 = heightAt(x, z - 1)
    max(max(abs(h - h1), abs(h - h2)), max(abs(h - h3), abs(h - h4)))

  private def cliffAt(x: Int, z: Int, h: Int): Boolean =
    h > Terrain.seaLevel + 34 && localSlopeAt(x, z, h) >= 5

  private def spruceAt(x: Int, z: Int, h: Int): Boolean =
    (coldAt(x, z, h) && moistureAt(x, z) > -0.28f) || taigaAt(x, z, h)

  private def caveMouthDepthAt(x: Int, z: Int, h: Int): Int =
    if h <= Terrain.seaLevel + 8 || h >= Terrain.worldHeight - 16 then 0
    else
      // A second entrance family creates rare irregular sinkholes on otherwise broad
      // uplands. They are cell-anchored and seam-stable, and immediately transition
      // into the same cave network rather than being decorative dead-end pits.
      val sinkCell = 92
      val scx = Math.floorDiv(x, sinkCell); val scz = Math.floorDiv(z, sinkCell)
      if hash(scx, 0, scz, 8650) > 0.935f && h > Terrain.seaLevel + 16 then
        val sx = scx * sinkCell + 14 + (hash(scx, 1, scz, 8651) * (sinkCell - 28)).toInt
        val sz = scz * sinkCell + 14 + (hash(scx, 2, scz, 8652) * (sinkCell - 28)).toInt
        val dx0 = (x - sx).toFloat; val dz0 = (z - sz).toFloat
        val radius = 2.2f + hash(scx, 3, scz, 8653) * 2.9f
        val ragged = valueNoise2D((x + 91.0) * 0.17, (z - 43.0) * 0.17, 8654) * 0.55f
        val d = sqrt(dx0 * dx0 + dz0 * dz0).toFloat
        if d < radius + ragged then
          val core = 1f - (d / radius.max(0.2f)).min(1f)
          return (6f + core * 15f + hash(scx, 4, scz, 8655) * 6f).toInt.max(4).min(27)
      0

  /** Sparse, seeded tunnel anchors. Neighboring chunks ask for the same world-space paths, so an
    * entrance can cross any chunk/cell boundary without being clipped.
    */
  private[blockbox] def caveEntranceForCell(cx: Int, cz: Int): Option[CaveEntrance] =
    val key = columnKey(cx, cz)
    val cached = entranceCache.get(key)
    if cached != null then cached
    else
      val result =
        if hash(cx, 0, cz, 8670) < 0.74f then None
        else
          val x = cx * 80 + 16 + (hash(cx, 1, cz, 8671) * 48f).toInt
          val z = cz * 80 + 16 + (hash(cx, 2, cz, 8672) * 48f).toInt
          val h = heightAt(x, z)
          if h <= Terrain.seaLevel + 8 || h >= Terrain.worldHeight - 16 then None
          else
            val initialAngle = hash(cx, 3, cz, 8673) * Pi.toFloat * 2f
            var angle = initialAngle
            var bestHeight = Int.MinValue
            var i = 0
            while i < 4 do
              val a = initialAngle + i * Pi.toFloat * 0.5f
              val ahead = heightAt(x + (cos(a) * 12).toInt, z + (sin(a) * 12).toInt)
              if ahead > bestHeight then
                bestHeight = ahead; angle = a
              i += 1
            Some(
              CaveEntrance(
                x,
                h + 0.5f,
                z,
                cos(angle).toFloat,
                sin(angle).toFloat,
                46f + hash(cx, 4, cz, 8674) * 28f,
                2.3f + hash(cx, 5, cz, 8675) * 1.2f,
                (hash(cx, 6, cz, 8676) - 0.5f) * 18f
              )
            )
      if entranceCache.size() > 8192 then entranceCache.clear()
      entranceCache.put(key, result)
      result

  private def carveEntrances(blocks: Array[Byte], baseX: Int, baseZ: Int): Unit =
    def carve(a: Vec3, b: Vec3, startRadius: Float, endRadius: Float, verticalScale: Float): Unit =
      val extent = startRadius.max(endRadius) + 1f
      val minX = floor(a.x.min(b.x) - extent).toInt.max(baseX)
      val maxX = ceil(a.x.max(b.x) + extent).toInt.min(baseX + Terrain.chunkSize - 1)
      val minZ = floor(a.z.min(b.z) - extent).toInt.max(baseZ)
      val maxZ = ceil(a.z.max(b.z) + extent).toInt.min(baseZ + Terrain.chunkSize - 1)
      if minX > maxX || minZ > maxZ then return
      val minY = floor(a.y.min(b.y) - extent * verticalScale).toInt.max(5)
      val maxY = ceil(a.y.max(b.y) + extent * verticalScale).toInt.min(Terrain.worldHeight - 2)
      val dx = b.x - a.x; val dz = b.z - a.z
      val lengthSq = (dx * dx + dz * dz).max(0.001f)
      var x = minX
      while x <= maxX do
        var z = minZ
        while z <= maxZ do
          val t = (((x + 0.5f - a.x) * dx + (z + 0.5f - a.z) * dz) / lengthSq).max(0f).min(1f)
          val px = a.x + dx * t; val pz = a.z + dz * t; val py = a.y + (b.y - a.y) * t
          val radius = startRadius + (endRadius - startRadius) * t
          val horizontal =
            ((x + 0.5f - px) * (x + 0.5f - px) + (z + 0.5f - pz) * (z + 0.5f - pz)) / (radius * radius)
          if horizontal < 1.1f then
            var y = minY
            while y <= maxY do
              val dy = (y + 0.5f - py) / (radius * verticalScale)
              val shape = horizontal + dy * dy
              val edge = if shape > 0.72f then
                valueNoise3D(x * 0.19, y * 0.17, z * 0.19, 8677) * 0.12f
              else 0f
              if shape < 1f + edge then
                val index = (y * Terrain.chunkSize + z - baseZ) * Terrain.chunkSize + x - baseX
                if blocks(index) != Block.Water.id && blocks(index) != Block.Bedrock.id then
                  blocks(index) = Block.Air.id
              y += 1
          z += 1
        x += 1
    var cx = Math.floorDiv(baseX - 110, 80)
    val endCX = Math.floorDiv(baseX + Terrain.chunkSize + 110, 80)
    val endCZ = Math.floorDiv(baseZ + Terrain.chunkSize + 110, 80)
    while cx <= endCX do
      var cz = Math.floorDiv(baseZ - 110, 80)
      while cz <= endCZ do
        caveEntranceForCell(cx, cz).foreach { e =>
          val sideX = -e.dz; val sideZ = e.dx
          val a = Vec3(e.x, e.y, e.z)
          val b = Vec3(e.x + e.dx * 12f, e.y - 5f, e.z + e.dz * 12f)
          val c = Vec3(
            e.x + e.dx * e.length * 0.55f + sideX * e.bend,
            e.y - 16f,
            e.z + e.dz * e.length * 0.55f + sideZ * e.bend
          )
          val d = Vec3(e.x + e.dx * e.length, e.y - 31f, e.z + e.dz * e.length)
          carve(a, b, e.radius * 1.15f, e.radius, 0.90f)
          carve(b, c, e.radius, e.radius * 1.25f, 0.90f)
          carve(c, d, e.radius * 1.25f, e.radius * 1.7f, 0.90f)
          // The descending entrance opens into a chamber and two traversable
          // branches, joining nearby noise caves instead of ending as a blind pit.
          carve(d, d, e.radius * 2.4f, e.radius * 2.4f, 0.68f)
          var side = -1
          while side <= 1 do
            val branch = Vec3(
              d.x + e.dx * 16f + sideX * side * 20f,
              (d.y - 6f).max(10f),
              d.z + e.dz * 16f + sideZ * side * 20f
            )
            carve(d, branch, e.radius, e.radius * 0.9f, 0.95f)
            side += 2
        }
        cz += 1
      cx += 1

  private def caveWaterSourceAt(x: Int, y: Int, z: Int): Boolean =
    if y <= 8 || y >= Terrain.seaLevel - 6 then false
    else
      val cell = 11
      val cx = Math.floorDiv(x, cell)
      val cy = Math.floorDiv(y, 5)
      val cz = Math.floorDiv(z, cell)
      hash(cx, cy, cz, 8700) > 0.976f && valueNoise3D(
        (x + 500.0) * 0.026,
        (y - 40.0) * 0.022,
        (z - 300.0) * 0.026,
        8701
      ) > 0.14f

  def caveBiomeAt(x: Int, y: Int, z: Int, h: Int): CaveBiome =
    val depth = h - y
    if depth < 16 then CaveBiome.Normal
    else
      // Cave materials are queried for huge numbers of underground voxels. Use
      // stable coarse 3-D region hashes here instead of several interpolated noise
      // octaves per block; cave *shape* remains noise-driven, while biome regions
      // become effectively free to classify on worker threads.
      val cx = Math.floorDiv(x, 44)
      val cy = Math.floorDiv(y, 22)
      val cz = Math.floorDiv(z, 44)
      val region = hash(cx, cy, cz, 8751)
      val detail =
        hash(Math.floorDiv(x + 19, 28), Math.floorDiv(y - 7, 18), Math.floorDiv(z - 13, 28), 8752)
      if y < 22 && region < 0.18f then CaveBiome.DeepStone
      else if depth > 34 && region > 0.72f && detail > 0.56f then CaveBiome.Crystal
      else if detail > 0.70f && y > 18 then CaveBiome.Lush
      else if region > 0.52f && region < 0.72f then CaveBiome.Limestone
      else if detail < 0.20f && depth > 26 then CaveBiome.Rooted
      else CaveBiome.Normal

  private def caveWallBlock(x: Int, y: Int, z: Int, h: Int): Block =
    caveBiomeAt(x, y, z, h) match
      case CaveBiome.Limestone => if hash(x, y, z, 8761) > 0.30f then Block.Clay else Block.Stone
      case CaveBiome.Lush      => if hash(x, y, z, 8762) > 0.24f then Block.Dirt else Block.Clay
      case CaveBiome.Crystal   =>
        val crystal = hash(x, y, z, 8763)
        if crystal > 0.992f && y < 28 then Block.Diamond
        else if crystal > 0.955f then Block.Copper
        else Block.Stone
      case CaveBiome.Rooted    => if hash(x, y, z, 8764) > 0.74f then Block.Dirt else Block.Stone
      case CaveBiome.DeepStone => if hash(x, y, z, 8765) > 0.88f then Block.Coal else Block.Stone
      case CaveBiome.Normal    => Block.Stone

  private def caveNoise(x: Int, y: Int, z: Int, h: Int): Boolean =
    caveNoise(x, y, z, h, caveMouthDepthAt(x, z, h))

  private def caveNoise(x: Int, y: Int, z: Int, h: Int, mouthDepth: Int): Boolean =
    if y <= 4 || y >= Terrain.worldHeight - 4 then return false
    val depth = (h - y).max(0).toFloat
    val mouth = mouthDepth > 0 && depth >= 0f && depth <= mouthDepth.toFloat
    if mouth then
      val throat = valueNoise3D(x.toDouble * 0.024, y.toDouble * 0.030, z.toDouble * 0.024, 8610)
      val ceilingBreak = valueNoise3D(
        (x + 90).toDouble * 0.055,
        (y - 20).toDouble * 0.045,
        (z + 40).toDouble * 0.055,
        8612
      )
      return depth < 4f || throat > -0.52f || ceilingBreak > 0.38f || depth < mouthDepth.toFloat * 0.52f
    if y >= h - 10 then return false
    val lowEnough = y > 7 && depth > 13f
    val dx = x.toDouble; val dy = y.toDouble; val dz = z.toDouble

    // 3D domain warping prevents the old cave tubes from looking like vertical
    // cookie-cutter cylinders. Two cheap warp samples bend every cave family in
    // all three dimensions while remaining deterministic across chunk seams.
    val warpX = valueNoise3D(dx * 0.0032, dy * 0.0030, dz * 0.0032, 9001).toDouble * 14.0
    val warpZ = valueNoise3D(
      (dx + 430.0) * 0.0032,
      (dy - 170.0) * 0.0030,
      (dz - 290.0) * 0.0032,
      9002
    ).toDouble * 14.0
    val wx = dx + warpX
    val wz = dz + warpZ

    val region = valueNoise3D(wx * 0.0037, dy * 0.0026, wz * 0.0037, 77)
    if region < -0.66f then return false

    // "Cheese" caverns: large connected rooms with depth-dependent thresholds.
    val cheeseA = valueNoise3D(wx * 0.0082, dy * 0.0055, wz * 0.0082, 99)
    val cheeseB = valueNoise3D((wx + 700.0) * 0.0110, dy * 0.0077, (wz - 500.0) * 0.0110, 199)
    val cheese = cheeseA * 0.68f + cheeseB * 0.32f
    val cheeseThreshold = 0.29f - smooth01((depth - 24f) / 54f) * 0.10f
    val cavern = lowEnough && depth > 21f && region > -0.30f && cheese > cheeseThreshold
    val megaRoom =
      lowEnough && depth > 44f && region > -0.03f && cheeseA > 0.34f && cheeseB > -0.16f

    // True 3D spaghetti/noodle caves. Intersecting fields creates long winding
    // passages that rise, fall and merge instead of following only X/Z contours.
    val spaghettiA = abs(
      valueNoise3D((wx + 120.0) * 0.0145, dy * 0.0180, (wz - 90.0) * 0.0145, 333)
    )
    val spaghettiB = abs(
      valueNoise3D((wx - 360.0) * 0.0125, (dy + 80.0) * 0.0150, (wz + 210.0) * 0.0125, 444)
    )
    val spaghetti =
      lowEnough && y < Terrain.seaLevel + 70 && ((spaghettiA < 0.070f && spaghettiB < 0.38f) || (spaghettiB < 0.060f && spaghettiA < 0.42f))

    val ravinePath = abs(valueNoise2D((wx + 150.0) * 0.0038, (wz - 470.0) * 0.0038, 445))
    val ravineShape = abs(valueNoise3D(wx * 0.0062, dy * 0.0125, wz * 0.0062, 555))
    val ravine = depth > 18f && depth < 78f && ravinePath < 0.030f && ravineShape < 0.56f

    cavern || megaRoom || spaghetti || ravine

  def heightAt(x: Int, z: Int): Int =
    val key = columnKey(x, z)
    val cached = heightCache.get(key)
    if cached != null then cached.intValue()
    else
      val computed = computeHeightAt(x, z)
      if heightCache.size() < maxTerrainCacheEntries then heightCache.putIfAbsent(key, computed)
      computed

  private def computeHeightAt(x: Int, z: Int): Int =
    val fx = x.toDouble; val fz = z.toDouble

    // Minecraft-like terrain needs multiple scales working together: broad continents,
    // rolling local relief, occasional hills, rarer mountain chains, and carved low divots.
    // Keep all coordinate math in Double so far-from-spawn terrain stays stable.
    val warpX = fbm2D((fx + 810.0) * 0.00070, (fz - 410.0) * 0.00070, 4, 1201).toDouble * 96.0
    val warpZ = fbm2D((fx - 260.0) * 0.00070, (fz + 680.0) * 0.00070, 4, 1202).toDouble * 96.0
    val wx = fx + warpX
    val wz = fz + warpZ

    val continent = fbm2D(wx * 0.00092, wz * 0.00092, 6, 1).toDouble
    val regional = fbm2D((wx + 300.0) * 0.0022, (wz - 250.0) * 0.0022, 5, 151).toDouble * 12.0
    val localRoll = fbm2D((wx - 670.0) * 0.0088, (wz + 430.0) * 0.0088, 3, 153).toDouble * 5.2
    val detail = fbm2D((wx + 120.0) * 0.024, (wz - 210.0) * 0.024, 2, 154).toDouble * 2.4

    val hillMask = smooth01(
      ((fbm2D((wx + 900.0) * 0.00165, (wz - 400.0) * 0.00165, 4, 101) + 0.48f) / 0.90f)
    )
    val highlandMask = smooth01(
      ((fbm2D((wx - 540.0) * 0.00130, (wz + 220.0) * 0.00130, 5, 1001) + 0.30f) / 0.82f)
    )
    val mountainRegion = smooth01(
      ((fbm2D((wx - 1500.0) * 0.00078, (wz + 700.0) * 0.00078, 5, 202) + 0.34f) / 0.80f)
    )
    val mountainMask = mountainRegion * smooth01(((continent + 0.66) / 1.08).toFloat)

    val rolling = (ridgedNoise(
      (wx - 110.0) * 0.0038,
      (wz + 210.0) * 0.0038,
      171,
      4
    ).toDouble * 28.0 + localRoll) * hillMask.toDouble
    val highlands = ridgedNoise(
      (wx + 130.0) * 0.0025,
      (wz - 710.0) * 0.0025,
      1002,
      5
    ).toDouble * 27.0 * highlandMask.toDouble
    val mountainCore = ridgedNoise((wx + 410.0) * 0.0028, (wz - 70.0) * 0.0028, 201, 5).toDouble
    val mountainDetail =
      ridgedNoise((wx - 240.0) * 0.0072, (wz + 520.0) * 0.0072, 301, 3).toDouble * 12.0
    val erosion = fbm2D((wx + 910.0) * 0.00175, (wz - 810.0) * 0.00175, 4, 205).toDouble
    val peakShape = pow(mountainCore.max(0.001), 1.55)
    val mountains = (peakShape * 62.0 + mountainDetail) * mountainMask.toDouble
    val lowErosion = smooth01(((0.28 - erosion) / 0.76).toFloat).toDouble
    val jaggedMask =
      mountainMask.toDouble * lowErosion * smooth01(((mountainCore - 0.50) / 0.42).toFloat).toDouble
    val jaggedPeaks = pow(
      ridgedNoise((wx + 1260.0) * 0.0105, (wz - 940.0) * 0.0105, 206, 3).toDouble,
      2.15
    ) * 20.0 * jaggedMask

    // Divots and soft valleys break up the flat green-sheet look without creating ugly holes.
    val valley = abs(fbm2D((wx - 90.0) * 0.0027, (wz + 120.0) * 0.0027, 5, 251).toDouble)
    val riverish = smooth01((max(0.0, 0.132 - valley) / 0.132).toFloat).toDouble
    val riverCarve = riverStrengthAt(x, z).toDouble
    val lakeCarve = lakeStrengthAt(x, z).toDouble
    val valleyCut =
      riverish * (7.5 + 10.0 * mountainMask + 4.0 * highlandMask) + riverCarve * (5.0 + 7.0 * highlandMask) + lakeCarve * 4.5
    val dimple = smooth01(
      (max(
        0.0,
        0.095 - abs(fbm2D((wx + 70.0) * 0.0068, (wz - 190.0) * 0.0068, 4, 252).toDouble)
      ) / 0.095).toFloat
    ).toDouble
    val divots = dimple * (2.0 + 4.0 * hillMask)

    // Reuse climate samples through the whole height calculation. These fields are
    // multi-octave noise and used to be recomputed several times per column.
    val temp0 = temperatureAt(x, z, Terrain.seaLevel + 10)
    val moist0 = moistureAt(x, z)
    val desertScore0 = desertRegionAt(x, z)
    val desertish = desertScore0 > 0.10f && moist0 < 0.12f && temp0 > -0.02f
    val dryRelief =
      if desertish then
        val dune = fbm2D((wx + 220.0) * 0.010, (wz - 180.0) * 0.010, 3, 811).toDouble * 4.2
        val mesaMask = smooth01(
          ((fbm2D((wx - 600.0) * 0.00155, (wz + 330.0) * 0.00155, 4, 813) + 0.20f) / 0.70f)
        ).toDouble
        val mesa = ridgedNoise(
          (wx - 500.0) * 0.0048,
          (wz + 260.0) * 0.0048,
          812,
          3
        ).toDouble * 12.0 * mesaMask
        val badlandsMask = smooth01((badlandsRegionAt(x, z) - 0.20f) / 0.42f).toDouble
        val terraceRaw =
          ridgedNoise((wx + 840.0) * 0.0037, (wz - 1140.0) * 0.0037, 814, 4).toDouble * 18.0
        val terrace = (floor(terraceRaw / 3.5) * 3.5) * badlandsMask
        val duneBlend =
          smooth01((desertScore0 - 0.18f) / 0.24f) * smooth01((-moist0 + 0.02f) / 0.24f)
        val windRidge = pow(
          (sin(
            (wx + wz * 0.38) * 0.075 + valueNoise2D(wx * 0.012, wz * 0.012, 815) * 2.8
          ) + 1) * 0.5,
          1.6
        ) * 7.0
        dune * (1 - duneBlend) + windRidge * duneBlend + mesa * (1 - duneBlend * 0.6) + terrace
      else 0.0

    // Biome-specific relief, blended softly so there are no hard biome cliffs.
    val savannaReliefBlend = smooth01((temp0 + 0.02f) / 0.32f).toDouble * smooth01(
      (0.18f - moist0) / 0.42f
    ).toDouble * smooth01((desertScore0 + 0.18f) / 0.62f).toDouble * (if desertish then 0.15
                                                                      else 1.0)
    val birchReliefBlend = smooth01((moist0 - 0.04f) / 0.46f).toDouble * smooth01(
      (forestRegionAt(x, z) - 0.04f) / 0.48f
    ).toDouble * (1.0 - savannaReliefBlend.min(1.0) * 0.75)
    val savannaRoll = (ridgedNoise(
      (wx + 320.0) * 0.0045,
      (wz - 160.0) * 0.0045,
      824,
      3
    ).toDouble * 7.5 - 2.0) * savannaReliefBlend
    val birchRoll =
      fbm2D((wx - 130.0) * 0.0064, (wz + 360.0) * 0.0064, 3, 825).toDouble * 3.4 * birchReliefBlend

    val preBase =
      Terrain.seaLevel + 8.0 +
        continent * 22.0 +
        regional +
        rolling +
        highlands +
        mountains +
        jaggedPeaks +
        dryRelief +
        savannaRoll +
        birchRoll +
        detail -
        valleyCut -
        divots

    // Wetland macro cells flatten gently toward the water table, producing broad
    // marshy lowlands instead of isolated one-block puddles.
    val wetMask = smooth01((wetlandRegionAt(x, z) - 0.28f) / 0.16f).toDouble * smooth01(
      (moist0 - 0.12f) / 0.16f
    ).toDouble
    // Warped continuous channels wind between low islands. The same sea-level
    // water table fills them, avoiding disconnected raised sheets at region borders.
    val channelWarp = valueNoise2D(wx * 0.006, wz * 0.006, 1830) * 2.4
    val channelField = abs(valueNoise2D(wx * 0.023 + channelWarp, wz * 0.023, 1831))
    val channel = smooth01((0.24f - channelField) / 0.22f)
    val wetTarget =
      Terrain.seaLevel + 1.6 + valueNoise2D(wx * 0.06, wz * 0.06, 1813) * 0.7 - channel * 3.6
    val wetBlend =
      (wetMask * smooth01(((Terrain.seaLevel + 35.0 - preBase) / 24.0).toFloat)).min(0.98)
    val base = preBase * (1.0 - wetBlend) + wetTarget * wetBlend

    base.round.toInt.max(1).min(Terrain.worldHeight - 8)

  def biomeAt(x: Int, z: Int, h: Int): TerrainBiome =
    val key = columnKey(x, z)
    val cached = biomeCache.get(key)
    if cached != null && cached.height == h then cached.biome
    else
      val result = computeBiomeAt(x, z, h)
      if biomeCache.size() < maxTerrainCacheEntries then
        biomeCache.putIfAbsent(key, BiomeColumn(h, result))
      result

  private def computeBiomeAt(x: Int, z: Int, h: Int): TerrainBiome =
    if wetlandAt(x, z, h) then
      if wetlandRegionAt(x, z) > 0.38f then TerrainBiome.WillowMarsh else TerrainBiome.Wetlands
    else if h < Terrain.seaLevel - 1 then TerrainBiome.Ocean
    else if beachAt(h) then
      if stonyHighlandRegionAt(x, z) > 0.12f || temperatureAt(x, z, h) < -0.24f then
        TerrainBiome.RockyCoast
      else TerrainBiome.Beach
    else
      // Classify from one shared climate sample set. The earlier version called the
      // helper predicates in sequence, causing temperature/moisture FBM to be sampled
      // over and over for the same column.
      val t = temperatureAt(x, z, h)
      val m = moistureAt(x, z)
      val mountain = if h > Terrain.seaLevel + 48 then mountainBiomeStrengthAt(x, z) else 0f
      if h > Terrain.seaLevel + 72 && mountain > 0.50f then TerrainBiome.JaggedPeaks
      else if h > Terrain.seaLevel + 52 && mountain > 0.34f then TerrainBiome.Alpine
      else
        val badlands =
          h > Terrain.seaLevel + 4 && h < Terrain.seaLevel + 58 && m < 0.03f && t > 0.02f && badlandsRegionAt(
            x,
            z
          ) > 0.30f
        if badlands then TerrainBiome.Badlands
        else
          val desertScore = desertRegionAt(x, z)
          val freshwater = h <= Terrain.seaLevel + 7 && (riverStrengthAt(
            x,
            z
          ) > 0.62f || lakeStrengthAt(x, z) > 0.58f)
          val desert =
            h < Terrain.seaLevel + 52 && !freshwater && ((desertScore > 0.16f && m < 0.08f && t > -0.06f) || (desertScore > 0.28f && m < 0.20f && t > 0.03f))
          if desert then
            if desertScore > 0.24f && m < -0.04f then TerrainBiome.DuneDesert
            else TerrainBiome.Desert
          else
            val wetland = h <= Terrain.seaLevel + 12 && wetlandRegionAt(
              x,
              z
            ) > 0.28f && m > 0.12f && desertScore < 0.20f
            if wetland then
              if wetlandRegionAt(x, z) > 0.38f then TerrainBiome.WillowMarsh
              else TerrainBiome.Wetlands
            else
              val coldRegion = coldRegionAt(x, z)
              val cold =
                (h > Terrain.seaLevel + 76 && t < -0.08f && coldRegion < 0.42f) ||
                  (t < -0.62f && coldRegion < 0.05f) ||
                  (t < -0.78f && coldRegion < 0.25f)
              if cold && m < 0.10f then TerrainBiome.SnowyTundra
              else
                val forest = forestRegionAt(x, z)
                val taiga = t < -0.20f && t > -0.78f && m > -0.22f && forest > -0.28f
                if taiga then
                  if t < -0.38f || cold then TerrainBiome.SnowyTaiga else TerrainBiome.Taiga
                else if h > Terrain.seaLevel + 38 && mountain > 0.20f && mountain < 0.50f && stonyHighlandRegionAt(
                    x,
                    z
                  ) > 0.26f
                then TerrainBiome.StonyHighlands
                else
                  val savanna =
                    h > Terrain.seaLevel + 3 && h < Terrain.seaLevel + 58 && t > 0.06f && m < 0.05f && desertScore > -0.12f
                  if savanna then TerrainBiome.Savanna
                  else
                    val oldGrowth =
                      forest > 0.30f && m > 0.08f && t > -0.22f && t < 0.48f && oldGrowthRegionAt(
                        x,
                        z
                      ) > 0.18f
                    val birch = !cold && m > 0.12f && forest > 0.18f && hash(
                      Math.floorDiv(x, 22),
                      0,
                      Math.floorDiv(z, 22),
                      6200
                    ) > 0.44f
                    val prairie =
                      forest < 0.02f && m > -0.30f && m < 0.20f && t > -0.16f && t < 0.48f && prairieRegionAt(
                        x,
                        z
                      ) > 0.18f
                    if oldGrowth && m > 0.20f && h < Terrain.seaLevel + 42 then
                      TerrainBiome.RedwoodForest
                    else if oldGrowth then TerrainBiome.OldGrowthForest
                    else if birch then TerrainBiome.BirchForest
                    else if forest > 0.12f && m > -0.20f && t > -0.20f && t < 0.22f && valueNoise2D(
                        x * 0.0018,
                        z * 0.0018,
                        1828
                      ) > -0.08f
                    then TerrainBiome.AutumnGrove
                    else if forest > 0.12f && m > -0.28f then TerrainBiome.Forest
                    else if prairie then TerrainBiome.Prairie
                    else if h > Terrain.seaLevel + 24 && m > -0.18f && t < 0.34f then
                      TerrainBiome.Meadow
                    else TerrainBiome.Plains

  def surfaceBlock(x: Int, y: Int, z: Int, h: Int): Block =
    val biome = biomeAt(x, z, h)
    val shore = freshwaterShoreAt(x, z, h)
    if biome == TerrainBiome.WillowMarsh || biome == TerrainBiome.Wetlands then
      if h <= Terrain.seaLevel || valueNoise2D(x * 0.08, z * 0.08, 1833) > 0.12f then Block.Clay
      else Block.Grass
    else if biome == TerrainBiome.AutumnGrove then
      if valueNoise2D(x * 0.075, z * 0.075, 1832) > 0.18f then Block.Dirt else Block.Grass
    else if shore && h <= Terrain.seaLevel + 8 then Block.Sand
    else
      biome match
        case TerrainBiome.Ocean | TerrainBiome.Beach | TerrainBiome.Desert |
            TerrainBiome.DuneDesert =>
          Block.Sand
        case TerrainBiome.Badlands                              => Block.Clay
        case TerrainBiome.SnowyTundra | TerrainBiome.SnowyTaiga => Block.Snow
        case TerrainBiome.RockyCoast                            => Block.Stone
        case TerrainBiome.JaggedPeaks                           =>
          if h > Terrain.seaLevel + 88 || hash(x, h, z, 735) > 0.46f then Block.Snow
          else Block.Stone
        case TerrainBiome.StonyHighlands =>
          if hash(x, h, z, 739) > 0.28f then Block.Stone else Block.Grass
        case TerrainBiome.Alpine =>
          if coldAt(x, z, h) then Block.Snow
          else if cliffAt(x, z, h) || rockyAt(x, z, h) then Block.Stone
          else Block.Grass
        case _ =>
          if cliffAt(x, z, h) then Block.Stone
          else if rockyAt(x, z, h) && hash(x, h, z, 733) > 0.82f then Block.Stone
          else Block.Grass

  def fillBlock(x: Int, y: Int, z: Int, h: Int): Block =
    val biome = biomeAt(x, z, h)
    val beach = beachAt(h)
    val freshwaterShore = freshwaterShoreAt(x, z, h)
    val sandy =
      beach || freshwaterShore || (biome == TerrainBiome.Desert || biome == TerrainBiome.DuneDesert)
    val badlands = biome == TerrainBiome.Badlands
    val surface = surfaceBlock(x, y, z, h)
    if y == h then surface
    else if badlands && y > h - 13 then
      val band = Math.floorMod(y + (valueNoise2D(x * 0.018, z * 0.018, 1820) * 4f).toInt, 11)
      if band <= 1 then Block.Sand else Block.Clay
    else if y > h - 3 && sandy then Block.Sand
    else if surface == Block.Stone && y > h - (if cliffAt(x, z, h) then 7 else 4) then Block.Stone
    else if y > h - 6 && cliffAt(x, z, h) && hash(x, y, z, 734) > 0.20f then Block.Stone
    else if y > h - 18 then Block.Dirt
    else if y <= 4 then Block.Bedrock
    else if y < h - 28 && y < 12 && hash(x, y, z, 17) > 0.92f then Block.Clay
    else
      OreDistribution
        .at(seed, x, y, z, h)
        .getOrElse(if y < h - 16 then caveWallBlock(x, y, z, h) else Block.Stone)

  def furnaceAt(x: Int, z: Int, h: Int, surface: Block): Option[(Int, Int, Int)] =
    // Furnaces should be player-made utility blocks, not random world decoration.
    None

  def treeAt(x: Int, z: Int, h: Int, surface: Block): Option[(Int, Int, Int, Int)] =
    if h <= Terrain.seaLevel || surface == Block.Sand || surface == Block.Stone || caveMouthDepthAt(
        x,
        z,
        h
      ) > 0
    then return None
    val forestNoise = forestRegionAt(x, z)
    val biome = biomeAt(x, z, h)
    if biome == TerrainBiome.Badlands || biome == TerrainBiome.Desert || biome == TerrainBiome.DuneDesert
    then return None
    val willow = biome == TerrainBiome.WillowMarsh
    val autumn = biome == TerrainBiome.AutumnGrove
    // Coherent glades leave room to walk and frame the surrounding crowns.
    if autumn && valueNoise2D((x + 53.0) * 0.022, (z - 71.0) * 0.022, 1829) > 0.16f then return None
    if !willow && h <= Terrain.seaLevel + 2 then return None
    val redwood = biome == TerrainBiome.RedwoodForest
    val oldGrowth = biome == TerrainBiome.OldGrowthForest || redwood
    val spruce = spruceAt(x, z, h) || biome == TerrainBiome.SnowyTaiga || redwood
    val savanna = savannaAt(x, z, h)
    val birch = birchGroveAt(x, z, h)
    val spacingCell = if willow then 9
    else if autumn then 8
    else if oldGrowth then 7
    else if spruce then 7
    else if savanna then 10
    else if birch then 8
    else if forestNoise > 0.34f then 8
    else 11
    val cellX = Math.floorDiv(x, spacingCell)
    val cellZ = Math.floorDiv(z, spacingCell)
    val cellGate = hash(cellX, cellZ, 0, 913)
    val treeChance = hash(x, z, 0, 19)
    val dense = forestNoise > 0.22f && moistureAt(x, z) > -0.42f
    val threshold =
      if willow then 0.974f
      else if autumn then 0.970f
      else if oldGrowth then 0.958f
      else if spruce then 0.972f
      else if savanna then 0.976f
      else if birch then 0.974f
      else if dense then 0.978f
      else if forestNoise > -0.08f then 0.991f
      else 0.9970f
    val height =
      if willow then 6 + (hash(x, z, 0, 47) * 3).toInt
      else if autumn then 7 + (hash(x, z, 0, 47) * 4).toInt
      else if redwood then 18 + (hash(x, z, 0, 47) * 10).toInt
      else if oldGrowth then 9 + (hash(x, z, 0, 47) * 7).toInt
      else if spruce then 8 + (hash(x, z, 0, 42) * 5).toInt
      else if savanna then 5 + (hash(x, z, 0, 45) * 4).toInt
      else if birch then 6 + (hash(x, z, 0, 46) * 3).toInt
      else if hash(x, z, 0, 43) > 0.78f then 7 + (hash(x, z, 0, 44) * 4).toInt
      else 4 + (hash(x, z, 0, 41) * 4).toInt
    if cellGate > 0.44f && treeChance > threshold then Some((x, h + 1, z, height))
    else None

  def cactusAt(x: Int, z: Int, h: Int, surface: Block): Option[(Int, Int, Int, Int)] =
    val biome = biomeAt(x, z, h)
    val desert =
      biome == TerrainBiome.Badlands || biome == TerrainBiome.Desert || biome == TerrainBiome.DuneDesert
    if (surface != Block.Sand && surface != Block.Clay) || !desert || h <= Terrain.seaLevel + 2 || caveMouthDepthAt(
        x,
        z,
        h
      ) > 0
    then None
    else
      val cellOk = hash(Math.floorDiv(x, 8), Math.floorDiv(z, 8), 0, 3033) > 0.66f
      val chance = hash(x, z, 0, 3034)
      if cellOk && chance > 0.982f then Some((x, h + 1, z, 2 + (hash(x, z, 0, 3035) * 3).toInt))
      else None

  def bushAt(x: Int, z: Int, h: Int, surface: Block): Option[(Int, Int, Int, Int)] =
    if surface != Block.Grass || h <= Terrain.seaLevel + 2 || coldAt(x, z, h) || cliffAt(
        x,
        z,
        h
      ) || caveMouthDepthAt(x, z, h) > 0
    then None
    else
      val grove = forestRegionAt(x, z)
      val cellOk = hash(Math.floorDiv(x, 9), Math.floorDiv(z, 9), 0, 4040) > 0.88f
      val chance = hash(x, z, 0, 4041)
      if grove > 0.02f && cellOk && chance > 0.994f then Some((x, h + 1, z, 1))
      else None

  def boulderAt(x: Int, z: Int, h: Int, surface: Block): Option[(Int, Int, Int, Int)] =
    if h <= Terrain.seaLevel + 4 || surface == Block.Sand || surface == Block.Snow || caveMouthDepthAt(
        x,
        z,
        h
      ) > 0
    then None
    else
      val rockyPatch =
        rockyAt(x, z, h) || fbm2D(
          (x + 40.0) * 0.006,
          (z - 90.0) * 0.006,
          3,
          5050
        ) > (if biomeAt(x, z, h) == TerrainBiome.AutumnGrove then 0.05f else 0.42f)
      val cellOk = hash(
        Math.floorDiv(x, 13),
        Math.floorDiv(z, 13),
        0,
        5051
      ) > (if biomeAt(x, z, h) == TerrainBiome.AutumnGrove then 0.55f else 0.86f)
      val chance = hash(x, z, 0, 5052)
      if rockyPatch && cellOk && chance > 0.991f then
        Some((x, h + 1, z, if hash(x, z, 0, 5053) > 0.78f then 2 else 1))
      else None

  /** Sparse, deterministic fallen trunks make forest floors feel lived-in without turning
    * decoration into per-block random clutter. Anchors are chosen per large cell so the exact same
    * feature is reconstructed by every neighboring chunk.
    */
  def fallenLogAt(x: Int, z: Int, h: Int, surface: Block): Option[(Int, Int, Int, Int, Boolean)] =
    val spruce = spruceAt(x, z, h)
    val autumn = biomeAt(x, z, h) == TerrainBiome.AutumnGrove
    val forested = forestRegionAt(x, z) > 0.02f || birchGroveAt(x, z, h) || spruce
    if !forested || h <= Terrain.seaLevel + 2 || (surface != Block.Grass && surface != Block.Snow && !(autumn && surface == Block.Dirt)) || cliffAt(
        x,
        z,
        h
      ) || caveMouthDepthAt(x, z, h) > 0
    then None
    else
      val cell = if autumn then 16 else 22
      val cellX = Math.floorDiv(x, cell)
      val cellZ = Math.floorDiv(z, cell)
      val ax = cellX * cell + 3 + (hash(cellX, 0, cellZ, 7201) * (cell - 6)).toInt.min(cell - 7)
      val az = cellZ * cell + 3 + (hash(cellX, 0, cellZ, 7202) * (cell - 6)).toInt.min(cell - 7)
      if x != ax || z != az || hash(cellX, 0, cellZ, 7203) < (if autumn then 0.35f else 0.70f) then
        None
      else
        val length = 3 + (hash(cellX, 0, cellZ, 7204) * 3f).toInt
        val alongX = hash(cellX, 0, cellZ, 7205) > 0.5f
        val ground = (0 until length).map(i =>
          heightAt(x + (if alongX then i else 0), z + (if alongX then 0 else i))
        )
        if ground.max - ground.min > 1 then None else Some((x, ground.max + 1, z, length, alongX))

  /** Tiny broken structures serve as navigation landmarks. They deliberately use existing
    * Stone/Brick ids so adding them cannot invalidate older save formats.
    */
  def ruinAt(x: Int, z: Int, h: Int, surface: Block): Option[(Int, Int, Int)] =
    if h <= Terrain.seaLevel + 5 || surface == Block.Sand || surface == Block.Snow || cliffAt(
        x,
        z,
        h
      ) || caveMouthDepthAt(x, z, h) > 0
    then None
    else
      val biome = biomeAt(x, z, h)
      if biome == TerrainBiome.Ocean || biome == TerrainBiome.Beach || biome == TerrainBiome.Wetlands || (biome == TerrainBiome.Desert || biome == TerrainBiome.DuneDesert)
      then None
      else
        val cell = 58
        val cellX = Math.floorDiv(x, cell)
        val cellZ = Math.floorDiv(z, cell)
        val ax = cellX * cell + 7 + (hash(cellX, 0, cellZ, 7301) * (cell - 14)).toInt.min(cell - 15)
        val az = cellZ * cell + 7 + (hash(cellX, 0, cellZ, 7302) * (cell - 14)).toInt.min(cell - 15)
        if x != ax || z != az || hash(cellX, 0, cellZ, 7303) < 0.84f then None
        else
          val h1 = heightAt(x + 3, z); val h2 = heightAt(x - 3, z)
          val h3 = heightAt(x, z + 3); val h4 = heightAt(x, z - 3)
          if Seq(h1, h2, h3, h4).exists(v => abs(v - h) > 1) then None else Some((x, h + 1, z))

  private def smooth01(v: Float): Float =
    val t = v.max(0f).min(1f); t * t * (3f - 2f * t)

  /** Generate a chunk's block data and its sparse log-orientation map (non-Y axes only). Keeping
    * both results together avoids computing the same chunk twice.
    */
  def generateChunk(cx: Int, cz: Int): (Array[Byte], scala.collection.immutable.Map[Int, LogAxis]) =
    val baseX = cx * Terrain.chunkSize
    val baseZ = cz * Terrain.chunkSize
    val endX = baseX + Terrain.chunkSize - 1
    val endZ = baseZ + Terrain.chunkSize - 1
    val blocks = new Array[Byte](Terrain.chunkSize * Terrain.worldHeight * Terrain.chunkSize)
    java.util.Arrays.fill(blocks, 0, Terrain.chunkSize * Terrain.chunkSize, Block.Bedrock.id)
    val logAxes = scala.collection.mutable.HashMap.empty[Int, LogAxis]
    def idx(lx: Int, y: Int, lz: Int): Int = (y * Terrain.chunkSize + lz) * Terrain.chunkSize + lx

    // Cache a slightly expanded height field so cliff/surface decoration does not call
    // heightAt repeatedly. Complex terrain keeps quality, but chunk creation hitches less.
    val columnCount = Terrain.chunkSize * Terrain.chunkSize
    val heights = new Array[Int](columnCount)
    val surfaces = new Array[Block](columnCount)
    val biomes = new Array[TerrainBiome](columnCount)
    val desertFlags = new Array[Boolean](columnCount)
    val beachFlags = new Array[Boolean](columnCount)
    val shoreFlags = new Array[Boolean](columnCount)
    val cliffFlags = new Array[Boolean](columnCount)
    val waterLines = new Array[Int](columnCount)
    val mouthDepths = new Array[Int](columnCount)
    def cidx(lx: Int, lz: Int): Int = lz * Terrain.chunkSize + lx

    val extPad = 4
    val extSize = Terrain.chunkSize + extPad * 2 + 1
    val extHeights = new Array[Int](extSize * extSize)
    def eidx(ex: Int, ez: Int): Int = ez * extSize + ex
    var ex = 0
    while ex < extSize do
      var ez = 0
      while ez < extSize do
        val wx = baseX + ex - extPad
        val wz = baseZ + ez - extPad
        extHeights(eidx(ex, ez)) = heightAt(wx, wz)
        ez += 1
      ex += 1
    def hLocal(lx: Int, lz: Int): Int = extHeights(eidx(lx + extPad, lz + extPad))
    def cachedSlope(lx: Int, lz: Int, h: Int): Int =
      val h1 = hLocal(lx + 1, lz)
      val h2 = hLocal(lx - 1, lz)
      val h3 = hLocal(lx, lz + 1)
      val h4 = hLocal(lx, lz - 1)
      max(max(abs(h - h1), abs(h - h2)), max(abs(h - h3), abs(h - h4)))
    def cachedCliff(lx: Int, lz: Int, h: Int): Boolean =
      h > Terrain.seaLevel + 34 && cachedSlope(lx, lz, h) >= 5

    var lx = 0
    while lx < Terrain.chunkSize do
      val wx = baseX + lx
      var lz = 0
      while lz < Terrain.chunkSize do
        val wz = baseZ + lz
        val ci = cidx(lx, lz)
        val h = hLocal(lx, lz)
        val waterLine = waterSurfaceAt(wx, wz, h)
        val beach = beachAt(h)
        val shore = freshwaterShoreAt(wx, wz, h)
        val biome = biomeAt(wx, wz, h)
        val desert = (biome == TerrainBiome.Desert || biome == TerrainBiome.DuneDesert)
        val rocky = rockyAt(wx, wz, h)
        val cliff = cachedCliff(lx, lz, h)
        val surface =
          if biome == TerrainBiome.WillowMarsh || biome == TerrainBiome.Wetlands || biome == TerrainBiome.AutumnGrove
          then surfaceBlock(wx, h, wz, h)
          else if shore && h <= Terrain.seaLevel + 8 then Block.Sand
          else
            biome match
              case TerrainBiome.Ocean | TerrainBiome.Beach | TerrainBiome.Desert |
                  TerrainBiome.DuneDesert =>
                Block.Sand
              case TerrainBiome.Badlands                              => Block.Clay
              case TerrainBiome.SnowyTundra | TerrainBiome.SnowyTaiga => Block.Snow
              case TerrainBiome.RockyCoast                            => Block.Stone
              case TerrainBiome.JaggedPeaks                           =>
                if h > Terrain.seaLevel + 88 || hash(wx, h, wz, 735) > 0.46f then Block.Snow
                else Block.Stone
              case TerrainBiome.StonyHighlands =>
                if cliff || hash(wx, h, wz, 739) > 0.28f then Block.Stone else Block.Grass
              case TerrainBiome.Alpine =>
                if coldAt(wx, wz, h) then Block.Snow
                else if cliff || rocky then Block.Stone
                else Block.Grass
              case _ =>
                if cliff then Block.Stone
                else if rocky && hash(wx, h, wz, 733) > 0.82f then Block.Stone
                else Block.Grass
        heights(ci) = h
        surfaces(ci) = surface
        biomes(ci) = biome
        desertFlags(ci) = desert
        beachFlags(ci) = beach
        shoreFlags(ci) = shore
        cliffFlags(ci) = cliff
        waterLines(ci) = waterLine
        mouthDepths(ci) = caveMouthDepthAt(wx, wz, h)
        lz += 1
      lx += 1

    lx = 0
    while lx < Terrain.chunkSize do
      val wx = baseX + lx
      var lz = 0
      while lz < Terrain.chunkSize do
        val wz = baseZ + lz
        val ci = cidx(lx, lz)
        val h = heights(ci)
        val surface = surfaces(ci)
        val biome = biomes(ci)
        val desert = desertFlags(ci)
        val badlands = biome == TerrainBiome.Badlands
        val beach = beachFlags(ci)
        val shore = shoreFlags(ci)
        val cliff = cliffFlags(ci)
        val mouthDepth = mouthDepths(ci)
        val waterLine = waterLines(ci)
        // Only visit columns up to real terrain/water height. This is the big cheap win:
        // do not loop through 256 air cells just to decide they are empty.
        val maxY = max(h, waterLine).min(Terrain.worldHeight - 1)
        var y = 1
        while y <= maxY do
          if y <= h then
            val block =
              if y == h then surface
              else if badlands && y > h - 13 then
                val band =
                  Math.floorMod(y + (valueNoise2D(wx * 0.018, wz * 0.018, 1820) * 4f).toInt, 11)
                if band <= 1 then Block.Sand else Block.Clay
              else if y > h - (if desert then 2 else 3) && (beach || shore || desert) then
                Block.Sand
              else if surface == Block.Stone && y > h - (if cliff then 7 else 4) then Block.Stone
              else if y > h - 6 && cliff && hash(wx, y, wz, 734) > 0.20f then Block.Stone
              else if y > h - 18 then Block.Dirt
              else if y <= 4 then Block.Bedrock
              else if y < h - 28 && y < 12 && hash(wx, y, wz, 17) > 0.92f then Block.Clay
              else
                OreDistribution
                  .at(seed, wx, y, wz, h)
                  .getOrElse(if y < h - 16 then caveWallBlock(wx, y, wz, h) else Block.Stone)
            val canCave = y > 4 && (y < h - 12 || (mouthDepth > 0 && y >= h - mouthDepth))
            val cave = canCave && caveNoise(wx, y, wz, h, mouthDepth)
            if !cave then blocks(idx(lx, y, lz)) = block.id
          else if y <= waterLine && y > h then blocks(idx(lx, y, lz)) = Block.Water.id
          y += 1
        lz += 1
      lx += 1

    carveEntrances(blocks, baseX, baseZ)

    // Fill vertical air pockets that are directly connected to natural surface water.
    // This fixes lakes/oceans looking like they have missing chunks or hollow blue panes
    // when caves carve into shallow lakebeds. It is a cheap column pass, not a runtime scan.
    lx = 0
    while lx < Terrain.chunkSize do
      var lz = 0
      while lz < Terrain.chunkSize do
        val ci = cidx(lx, lz)
        var y = waterLines(ci).min(Terrain.worldHeight - 2)
        var waterAbove = false
        while y >= 1 do
          val i = idx(lx, y, lz)
          val block = blocks(i)
          if block == Block.Water.id then waterAbove = true
          else if block == Block.Air.id && waterAbove then blocks(i) = Block.Water.id
          else if block != Block.Air.id then waterAbove = false
          y -= 1
        lz += 1
      lx += 1

    // Small dormant cave pools. Step by 2 vertically and use deterministic widening so it
    // looks natural without scanning every cave air cell at full resolution.
    lx = 0
    while lx < Terrain.chunkSize do
      val wx = baseX + lx
      var lz = 0
      while lz < Terrain.chunkSize do
        val wz = baseZ + lz
        val maxY = (Terrain.seaLevel - 6).min(Terrain.worldHeight - 3)
        var y = 8
        while y <= maxY do
          val i = idx(lx, y, lz)
          val below = idx(lx, y - 1, lz)
          if blocks(i) == Block.Air.id && blocks(below) != Block.Air.id && blocks(
              below
            ) != Block.Water.id && caveWaterSourceAt(wx, y, wz)
          then
            blocks(i) = Block.Water.id
            if lx + 1 < Terrain.chunkSize && blocks(idx(lx + 1, y, lz)) == Block.Air.id then
              blocks(idx(lx + 1, y, lz)) = Block.Water.id
            if lx > 0 && blocks(idx(lx - 1, y, lz)) == Block.Air.id && hash(wx, y, wz, 8702) > 0.42f
            then blocks(idx(lx - 1, y, lz)) = Block.Water.id
            if lz + 1 < Terrain.chunkSize && blocks(idx(lx, y, lz + 1)) == Block.Air.id then
              blocks(idx(lx, y, lz + 1)) = Block.Water.id
            if lz > 0 && blocks(idx(lx, y, lz - 1)) == Block.Air.id && hash(wx, y, wz, 8703) > 0.42f
            then blocks(idx(lx, y, lz - 1)) = Block.Water.id
          y += 2
        lz += 1
      lx += 1

    def cachedChunkHeight(wx: Int, wz: Int): Int =
      val lx0 = wx - baseX
      val lz0 = wz - baseZ
      if lx0 >= 0 && lx0 < Terrain.chunkSize && lz0 >= 0 && lz0 < Terrain.chunkSize then
        heights(cidx(lx0, lz0))
      else
        val ex0 = wx - baseX + extPad
        val ez0 = wz - baseZ + extPad
        if ex0 >= 0 && ex0 < extSize && ez0 >= 0 && ez0 < extSize then extHeights(eidx(ex0, ez0))
        else heightAt(wx, wz)
    def cachedChunkSurface(wx: Int, wz: Int, h: Int): Block =
      val lx0 = wx - baseX
      val lz0 = wz - baseZ
      if lx0 >= 0 && lx0 < Terrain.chunkSize && lz0 >= 0 && lz0 < Terrain.chunkSize then
        surfaces(cidx(lx0, lz0))
      else surfaceBlock(wx, h, wz, h)
    def columnIsDecoratable(wx: Int, wz: Int, h: Int): Boolean =
      if caveMouthDepthAt(wx, wz, h) > 0 then false
      else
        val lx0 = wx - baseX
        val lz0 = wz - baseZ
        if lx0 >= 0 && lx0 < Terrain.chunkSize && lz0 >= 0 && lz0 < Terrain.chunkSize && h >= 0 && h < Terrain.worldHeight - 2
        then
          val here = blocks(idx(lx0, h, lz0))
          val above = blocks(idx(lx0, h + 1, lz0))
          here != Block.Air.id && here != Block.Water.id && above == Block.Air.id
        else true

    for wx <- baseX - 4 to endX + 4; wz <- baseZ - 4 to endZ + 4 do
      val h = cachedChunkHeight(wx, wz)
      val surface = cachedChunkSurface(wx, wz, h)
      if columnIsDecoratable(wx, wz, h) then
        treeAt(wx, wz, h, surface).foreach { case (tx, ty, tz, th) =>
          placeTreeWorld(
            blocks,
            baseX,
            baseZ,
            tx,
            ty,
            tz,
            th,
            spruceAt(wx, wz, h) || biomeAt(wx, wz, h) == TerrainBiome.SnowyTaiga || biomeAt(
              wx,
              wz,
              h
            ) == TerrainBiome.RedwoodForest
          )
        }
        cactusAt(wx, wz, h, surface).foreach { case (cx0, cy0, cz0, ch) =>
          placeCactusWorld(blocks, baseX, baseZ, cx0, cy0, cz0, ch)
        }
        bushAt(wx, wz, h, surface).foreach { case (bx0, by0, bz0, br) =>
          placeBushWorld(blocks, baseX, baseZ, bx0, by0, bz0, br)
        }
        boulderAt(wx, wz, h, surface).foreach { case (bx0, by0, bz0, br) =>
          placeBoulderWorld(blocks, baseX, baseZ, bx0, by0, bz0, br)
        }
        fallenLogAt(wx, wz, h, surface).foreach { case (lx0, ly0, lz0, length, alongX) =>
          placeFallenLogWorld(blocks, baseX, baseZ, lx0, ly0, lz0, length, alongX, logAxes)
        }
        ruinAt(wx, wz, h, surface).foreach { case (rx0, ry0, rz0) =>
          placeRuinWorld(blocks, baseX, baseZ, rx0, ry0, rz0)
        }
    // Redwood skirts reach further than any other decoration, so look a little wider
    // for them only; otherwise canopies would be sliced flat at chunk borders.
    for wx <- baseX - 8 to endX + 8; wz <- baseZ - 8 to endZ + 8 do
      if wx < baseX - 4 || wx > endX + 4 || wz < baseZ - 4 || wz > endZ + 4 then
        val h = cachedChunkHeight(wx, wz)
        if biomeAt(wx, wz, h) == TerrainBiome.RedwoodForest && columnIsDecoratable(wx, wz, h) then
          treeAt(wx, wz, h, cachedChunkSurface(wx, wz, h)).foreach { case (tx, ty, tz, th) =>
            placeTreeWorld(blocks, baseX, baseZ, tx, ty, tz, th, spruce = true)
          }
    // Ground cover is a final column pass: it cannot bury trunks, grow in caves,
    // replace water, or depend on which neighboring chunk happened to load first.
    for lx <- 0 until Terrain.chunkSize; lz <- 0 until Terrain.chunkSize do
      val ci = cidx(lx, lz); val h = heights(ci)
      val wx = baseX + lx; val wz = baseZ + lz; val biome = biomes(ci)
      def placeCover(y: Int, block: Block): Unit =
        if y > 0 && y < Terrain.worldHeight && blocks(idx(lx, y, lz)) == Block.Air.id then
          blocks(idx(lx, y, lz)) = block.id
      if biome == TerrainBiome.WillowMarsh || biome == TerrainBiome.Wetlands then
        val bed = valueNoise2D(wx * 0.13, wz * 0.13, 1834)
        if h < Terrain.seaLevel && h >= Terrain.seaLevel - 3 && bed > -0.12f && hash(
            wx,
            0,
            wz,
            1835
          ) > 0.72f && blocks(idx(lx, Terrain.seaLevel, lz)) == Block.Water.id
        then placeCover(Terrain.seaLevel + 1, Block.LilyPad)
        else if h >= Terrain.seaLevel && h <= Terrain.seaLevel + 2 && bed > -0.18f && hash(
            wx,
            0,
            wz,
            1836
          ) > 0.34f && blocks(idx(lx, h, lz)) == surfaces(ci).id
        then
          val bank = Vector((1, 0), (-1, 0), (0, 1), (0, -1)).exists { case (dx, dz) =>
            cachedChunkHeight(wx + dx, wz + dz) < Terrain.seaLevel
          }
          if bank then placeCover(h + 1, Block.Reeds)
      else if biome == TerrainBiome.AutumnGrove && h > Terrain.seaLevel + 2 && blocks(
          idx(lx, h, lz)
        ) == surfaces(ci).id && (surfaces(ci) == Block.Grass || surfaces(ci) == Block.Dirt) && hash(
          wx,
          0,
          wz,
          1837
        ) > 0.24f
      then placeCover(h + 1, Block.LeafLitter)
    decorate(cx, cz, blocks)
    (blocks, logAxes.toMap)

  def fillChunkBlocks(cx: Int, cz: Int): Array[Byte] = generateChunk(cx, cz)._1

  def fillChunkLogAxes(cx: Int, cz: Int): scala.collection.immutable.Map[Int, LogAxis] =
    generateChunk(cx, cz)._2

  private def placeTreeWorld(
      blocks: Array[Byte],
      baseX: Int,
      baseZ: Int,
      x: Int,
      y: Int,
      z: Int,
      height: Int,
      spruce: Boolean
  ): Unit =
    val biome = biomeAt(x, z, y - 1)
    val redwood = biome == TerrainBiome.RedwoodForest
    val willow = biome == TerrainBiome.WillowMarsh
    val autumn = biome == TerrainBiome.AutumnGrove
    val dry = !willow && !autumn && savannaAt(x, z, y - 1)
    val birch = !spruce && !dry && !willow && !autumn && birchGroveAt(x, z, y - 1)
    val wood = if spruce then Block.PineWood
    else if dry then Block.AcaciaWood
    else if birch then Block.BirchWood
    else Block.Wood
    val leaves = if spruce then Block.PineLeaves
    else if dry then Block.AcaciaLeaves
    else if birch then Block.BirchLeaves
    else Block.Leaves
    def set(wx: Int, wy: Int, wz: Int, block: Block): Unit =
      val lx = wx - baseX; val lz = wz - baseZ
      if lx >= 0 && lx < Terrain.chunkSize && lz >= 0 && lz < Terrain.chunkSize && wy >= 0 && wy < Terrain.worldHeight
      then
        val i = idx(lx, wy, lz)
        if block != leaves || blocks(i) == Block.Air.id then blocks(i) = block.id
    val leanX = if willow then 0
    else if dry then (if hash(x, z, 0, 6005) > 0.5f then 1 else -1)
    else if !spruce && hash(x, z, 0, 6001) > 0.88f then
      (if hash(x, z, 0, 6002) > 0.5f then 1 else -1
    )
    else 0
    val leanZ = if willow then 0
    else if dry then (if hash(x, z, 0, 6006) > 0.5f then 1 else -1)
    else if !spruce && hash(x, z, 0, 6003) > 0.88f then
      (if hash(x, z, 0, 6004) > 0.5f then 1 else -1
    )
    else 0
    for dy <- 0 until height do
      val sx = x + (if dy > height / 2 then leanX else 0)
      val sz = z + (if dy > height / 2 then leanZ else 0)
      set(sx, y + dy, sz, wood)
      if redwood && dy < height - 4 then
        set(sx + 1, y + dy, sz, wood); set(sx, y + dy, sz + 1, wood);
        set(sx + 1, y + dy, sz + 1, wood)
    val topX = x + leanX
    val topZ = z + leanZ
    if willow then
      // Four short boughs support a rounded crown and uneven hanging curtains.
      // Everything stays inside the existing four-block decoration halo.
      for (dx, dz) <- Seq((1, 0), (-1, 0), (0, 1), (0, -1)); step <- 1 to 2 do
        set(x + dx * step, y + height - 3 + step / 2, z + dz * step, wood)
      for dx <- -4 to 4; dz <- -4 to 4; dy <- -2 to 1 do
        val shape = dx * dx / 17f + dz * dz / 15f + dy * dy / 5f
        if shape < 1.15f && hash(x + dx, y + height + dy, z + dz, 7410) > 0.07f then
          set(x + dx, y + height + dy, z + dz, leaves)
      for
        dx <- -4 to 4; dz <- -4 to 4 if dx * dx + dz * dz >= 9 && dx * dx + dz * dz <= 18 && hash(
          x + dx,
          y,
          z + dz,
          7411
        ) > 0.22f
      do
        val length = 2 + (hash(x + dx, y, z + dz, 7412) * 4).toInt
        for drop <- 1 to length do set(x + dx, y + height - 1 - drop, z + dz, leaves)
    else if redwood then
      // Giant redwood: a long bare 2x2 trunk, then round tiers that widen downward like
      // stacked skirts, centred on the trunk and with ragged edges.
      val start = (height * 0.42f).toInt
      val top = height + 3
      val span = (top - start).max(1).toFloat
      for dy <- start to top do
        val t = (top - dy) / span
        val tier = (top - dy) % 3
        val radius = (1.1f + t * 4.4f - tier * 0.95f).max(0.75f)
        val reach = ceil(radius).toInt + 1
        for bx <- topX - reach to topX + 1 + reach; bz <- topZ - reach to topZ + 1 + reach do
          val ddx = bx - topX - 0.5f; val ddz = bz - topZ - 0.5f
          if ddx * ddx + ddz * ddz <= radius * radius + hash(bx, y + dy, bz, 7401) * 1.4f then
            set(bx, y + dy, bz, leaves)
      set(topX, y + top + 1, topZ, leaves)
    else if spruce then
      // Minecraft's spruce silhouette: from the tip down, each tier widens one block per
      // layer and then restarts narrow, so the tree reads as stacked, drooping skirts.
      val top = height + 3
      val widest = if height >= 11 then 4 else 3
      val bottom = (height / 4).max(2)
      var radius = 0; var tierMax = 1
      var dy = top
      while dy >= bottom do
        for dx <- -radius to radius; dz <- -radius to radius do
          val corner = radius > 1 && abs(dx) == radius && abs(dz) == radius
          val rim = abs(dx) + abs(dz) >= 2 * radius - (if radius > 1 then 1 else 0)
          val trimmed = radius > 0 && rim && hash(topX + dx, y + dy, topZ + dz, 7311) > 0.60f
          if !corner && !trimmed && !(dx == 0 && dz == 0 && dy < height) then
            set(topX + dx, y + dy, topZ + dz, leaves)
        if radius >= tierMax then
          radius = if tierMax >= 2 then 1 else 0
          tierMax = (tierMax + 1).min(widest)
        else radius += 1
        dy -= 1
      set(topX, y + top + 1, topZ, leaves)
    else if dry then
      // Sparse acacia-ish flat canopy.
      for dx <- -3 to 3; dz <- -3 to 3; dy <- height - 1 to height + 1 do
        val dist = abs(dx) + abs(dz)
        if dist <= 4 && hash(topX + dx, y + dy, topZ + dz, 6110) > 0.18f then
          set(topX + dx, y + dy, topZ + dz, leaves)
    else
      if autumn then
        for (dx, dz) <- Seq((1, 0), (-1, 0), (0, 1), (0, -1)); step <- 1 to 2 do
          set(topX + dx * step, y + height - 3 + step, topZ + dz * step, wood)
      val wide = hash(x, z, 0, 6101) > 0.68f
      val leafStart = if wide then height - 3 else height - 2
      val leafSpread = if wide then 4 else if height >= 7 then 4 else 3
      for
        dx <- -leafSpread to leafSpread; dz <- -leafSpread to leafSpread;
        dy <- leafStart to height + 2
      do
        val dist = abs(dx) + abs(dz) + max(0, dy - height)
        val edgeNoise = hash(topX + dx, y + dy, topZ + dz, 6102)
        if dist <= leafSpread + (if edgeNoise > 0.62f then 1
                                 else 0) && !(dx == 0 && dz == 0 && dy > height - 1)
        then set(topX + dx, y + dy, topZ + dz, leaves)

  private def placeCactusWorld(
      blocks: Array[Byte],
      baseX: Int,
      baseZ: Int,
      x: Int,
      y: Int,
      z: Int,
      height: Int
  ): Unit =
    def set(wx: Int, wy: Int, wz: Int, block: Block): Unit =
      val lx = wx - baseX; val lz = wz - baseZ
      if lx >= 0 && lx < Terrain.chunkSize && lz >= 0 && lz < Terrain.chunkSize && wy >= 0 && wy < Terrain.worldHeight
      then blocks(idx(lx, wy, lz)) = block.id
    for dy <- 0 until height do set(x, y + dy, z, Block.Cactus)

  private def placeBushWorld(
      blocks: Array[Byte],
      baseX: Int,
      baseZ: Int,
      x: Int,
      y: Int,
      z: Int,
      radius: Int
  ): Unit =
    def set(wx: Int, wy: Int, wz: Int, block: Block): Unit =
      val lx = wx - baseX; val lz = wz - baseZ
      if lx >= 0 && lx < Terrain.chunkSize && lz >= 0 && lz < Terrain.chunkSize && wy >= 0 && wy < Terrain.worldHeight
      then
        val i = idx(lx, wy, lz)
        if blocks(i) == Block.Air.id then blocks(i) = block.id
    // Small hand-placed shrub shape. This prevents the old dense leaf blanket look.
    set(x, y, z, Block.Leaves)
    if hash(x, z, 0, 4044) > 0.35f then set(x + 1, y, z, Block.Leaves)
    if hash(x, z, 0, 4045) > 0.35f then set(x - 1, y, z, Block.Leaves)
    if hash(x, z, 0, 4046) > 0.35f then set(x, y, z + 1, Block.Leaves)
    if hash(x, z, 0, 4047) > 0.35f then set(x, y, z - 1, Block.Leaves)
    if hash(x, z, 0, 4048) > 0.72f then set(x, y + 1, z, Block.Leaves)

  private def placeBoulderWorld(
      blocks: Array[Byte],
      baseX: Int,
      baseZ: Int,
      x: Int,
      y: Int,
      z: Int,
      radius: Int
  ): Unit =
    def set(wx: Int, wy: Int, wz: Int, block: Block): Unit =
      val lx = wx - baseX; val lz = wz - baseZ
      if lx >= 0 && lx < Terrain.chunkSize && lz >= 0 && lz < Terrain.chunkSize && wy >= 0 && wy < Terrain.worldHeight
      then blocks(idx(lx, wy, lz)) = block.id
    for dx <- -radius to radius; dz <- -radius to radius; dy <- 0 to radius do
      val ell = dx * dx + dz * dz + dy * dy
      if ell <= radius * radius + 1 && hash(x + dx, y + dy, z + dz, 5054) > 0.16f then
        set(
          x + dx,
          y + dy,
          z + dz,
          if hash(x + dx, y + dy, z + dz, 5055) > 0.74f then Block.Clay else Block.Stone
        )

  private def placeFallenLogWorld(
      blocks: Array[Byte],
      baseX: Int,
      baseZ: Int,
      x: Int,
      y: Int,
      z: Int,
      length: Int,
      alongX: Boolean,
      logAxes: scala.collection.mutable.HashMap[Int, LogAxis]
  ): Unit =
    val spruce = spruceAt(x, z, y - 1)
    val dry = savannaAt(x, z, y - 1)
    val birch = !spruce && !dry && birchGroveAt(x, z, y - 1)
    val wood = if spruce then Block.PineWood
    else if dry then Block.AcaciaWood
    else if birch then Block.BirchWood
    else Block.Wood
    val leaves = if spruce then Block.PineLeaves
    else if dry then Block.AcaciaLeaves
    else if birch then Block.BirchLeaves
    else Block.Leaves
    val axis = if alongX then LogAxis.X else LogAxis.Z
    def setIfOpen(wx: Int, wy: Int, wz: Int, block: Block): Unit =
      val lx = wx - baseX; val lz = wz - baseZ
      if lx >= 0 && lx < Terrain.chunkSize && lz >= 0 && lz < Terrain.chunkSize && wy >= 0 && wy < Terrain.worldHeight
      then
        val i = idx(lx, wy, lz)
        val existing = Block.fromId(blocks(i))
        if existing == Block.Air || existing.cutout then blocks(i) = block.id
    // Record the log's orientation only where the wood cell is actually placed,
    // so overlapping standing-tree trunks stay vertical.
    def setWoodIfOpen(wx: Int, wy: Int, wz: Int): Unit =
      val lx = wx - baseX; val lz = wz - baseZ
      if lx >= 0 && lx < Terrain.chunkSize && lz >= 0 && lz < Terrain.chunkSize && wy >= 0 && wy < Terrain.worldHeight
      then
        val i = idx(lx, wy, lz)
        val existing = Block.fromId(blocks(i))
        if existing == Block.Air || existing.cutout then
          blocks(i) = wood.id
          logAxes(i) = axis
    var i = 0
    while i < length do
      val wx = x + (if alongX then i else 0)
      val wz = z + (if alongX then 0 else i)
      setWoodIfOpen(wx, y, wz)
      if i > 0 && i < length - 1 && hash(wx, y, wz, 7211) > 0.78f then
        setIfOpen(wx, y + 1, wz, leaves)
      i += 1
    // A couple of low leaf remnants at an end keep these from reading as player-placed beams.
    if hash(x, y, z, 7212) > 0.46f then
      val ex = x + (if alongX then length - 1 else 0)
      val ez = z + (if alongX then 0 else length - 1)
      setIfOpen(ex + (if alongX then 0 else 1), y, ez + (if alongX then 1 else 0), leaves)

  private def placeRuinWorld(
      blocks: Array[Byte],
      baseX: Int,
      baseZ: Int,
      x: Int,
      y: Int,
      z: Int
  ): Unit =
    def setIfOpen(wx: Int, wy: Int, wz: Int, block: Block): Unit =
      val lx = wx - baseX; val lz = wz - baseZ
      if lx >= 0 && lx < Terrain.chunkSize && lz >= 0 && lz < Terrain.chunkSize && wy >= 0 && wy < Terrain.worldHeight
      then
        val i = idx(lx, wy, lz)
        val existing = Block.fromId(blocks(i))
        if existing == Block.Air || existing.cutout then blocks(i) = block.id
    def masonry(wx: Int, wy: Int, wz: Int): Block =
      if hash(wx, wy, wz, 7310) > 0.30f then Block.Brick else Block.Stone

    // Broken 5x5 perimeter with deterministic gaps and a doorway. Uneven column
    // heights make silhouettes readable from a distance without spawning a full house.
    for dx <- -2 to 2; dz <- -2 to 2 do
      val edge = abs(dx) == 2 || abs(dz) == 2
      val doorway = dz == 2 && dx == 0
      if edge && !doorway && hash(x + dx, y, z + dz, 7311) > 0.20f then
        val height = 1 + (hash(x + dx, y, z + dz, 7312) * 3f).toInt
        var dy = 0
        while dy < height do
          if hash(x + dx, y + dy, z + dz, 7313) > 0.12f then
            setIfOpen(x + dx, y + dy, z + dz, masonry(x + dx, y + dy, z + dz))
          dy += 1
    // Foundation/rubble details, intentionally incomplete.
    for dx <- -1 to 1; dz <- -1 to 1 do
      if hash(x + dx, y, z + dz, 7314) > 0.62f then setIfOpen(x + dx, y, z + dz, Block.Stone)
    val rubble = Array((-3, 0), (3, 1), (-2, 3), (2, -3), (0, -3), (3, -2))
    rubble.foreach { case (dx, dz) =>
      if hash(x + dx, y, z + dz, 7315) > 0.52f then
        setIfOpen(x + dx, y, z + dz, masonry(x + dx, y, z + dz))
    }

  private def placeTree(blocks: Array[Byte], lx: Int, y: Int, lz: Int, height: Int): Unit =
    val baseX = 0; val baseZ = 0
    for dy <- 0 until height do
      val ly = y + dy
      if ly < Terrain.worldHeight then blocks(idx(lx, ly, lz)) = Block.Wood.id
    val leafStart = height - 2
    val leafSpread = if height >= 6 then 4 else 3
    for
      dx <- -leafSpread to leafSpread; dz <- -leafSpread to leafSpread;
      dy <- leafStart to height + 1
    do
      val ax = lx + dx; val ay = y + dy; val az = lz + dz
      if ax >= 0 && ax < Terrain.chunkSize && az >= 0 && az < Terrain.chunkSize && ay >= 0 && ay < Terrain.worldHeight
      then
        val dist = abs(dx) + abs(dz) + max(0, dy - height)
        if dist <= leafSpread + 1 && !(dx == 0 && dz == 0 && dy > height - 1) then
          blocks(idx(ax, ay, az)) = Block.Leaves.id

  private def idx(lx: Int, y: Int, lz: Int): Int =
    (y * Terrain.chunkSize + lz) * Terrain.chunkSize + lx
