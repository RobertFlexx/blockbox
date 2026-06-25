package blockbox

import scala.collection.mutable.ArrayBuffer
import scala.math.*

object Terrain:
  val seaLevel = 32
  val worldHeight = 128
  val chunkSize = 16

final class TerrainGenerator(val seed: Long):
  private val seedHash = (seed ^ (seed >>> 32)).toInt

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
    val aaa = hash(xi, yi, zi, base).toDouble * 2.0 - 1.0; val baa = hash(xi+1, yi, zi, base).toDouble * 2.0 - 1.0
    val aba = hash(xi, yi+1, zi, base).toDouble * 2.0 - 1.0; val bba = hash(xi+1, yi+1, zi, base).toDouble * 2.0 - 1.0
    val aab = hash(xi, yi, zi+1, base).toDouble * 2.0 - 1.0; val bab = hash(xi+1, yi, zi+1, base).toDouble * 2.0 - 1.0
    val abb = hash(xi, yi+1, zi+1, base).toDouble * 2.0 - 1.0; val bbb = hash(xi+1, yi+1, zi+1, base).toDouble * 2.0 - 1.0
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

  private def riverStrengthAt(x: Int, z: Int): Float =
    val fx = x.toDouble; val fz = z.toDouble
    val channelA = abs(fbm2D((fx + 260.0) * 0.0019, (fz - 530.0) * 0.0019, 5, 2401))
    val channelB = abs(fbm2D((fx - 930.0) * 0.0032, (fz + 760.0) * 0.0032, 4, 2402))
    val broadGate = smooth01(((fbm2D((fx + 80.0) * 0.00075, (fz - 110.0) * 0.00075, 4, 2403) + 0.42f) / 0.92f))
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
    !beachAt(h) && !freshwaterAt(x, z, h) && h < Terrain.seaLevel + 52 && (coherentDesert || hotCore)

  private def savannaAt(x: Int, z: Int, h: Int): Boolean =
    !desertAt(x, z, h) && h > Terrain.seaLevel + 3 && h < Terrain.seaLevel + 58 && temperatureAt(x, z, h) > 0.06f && moistureAt(x, z) < 0.05f && desertRegionAt(x, z) > -0.12f

  private def birchGroveAt(x: Int, z: Int, h: Int): Boolean =
    !coldAt(x, z, h) && moistureAt(x, z) > 0.12f && forestRegionAt(x, z) > 0.18f && hash(Math.floorDiv(x, 22), 0, Math.floorDiv(z, 22), 6200) > 0.44f

  def savannaBlendAt(x: Int, z: Int, h: Int): Float =
    val t = temperatureAt(x, z, h)
    val m = moistureAt(x, z)
    val edgeNoise = fbm2D((x + 410.0) * 0.0022, (z - 260.0) * 0.0022, 3, 3301) * 0.09f
    val dry = smooth01((0.22f - m + edgeNoise) / 0.50f)
    val warm = smooth01((t + 0.03f + edgeNoise * 0.35f) / 0.38f)
    val region = smooth01((desertRegionAt(x, z) + 0.18f + edgeNoise) / 0.62f)
    if desertAt(x, z, h) || h <= Terrain.seaLevel + 2 then 0f else (dry * warm * region).max(0f).min(1f)

  def birchBlendAt(x: Int, z: Int, h: Int): Float =
    val m = moistureAt(x, z)
    val forest = forestRegionAt(x, z)
    val edgeNoise = fbm2D((x - 720.0) * 0.0020, (z + 540.0) * 0.0020, 3, 3302) * 0.10f
    val wet = smooth01((m - 0.02f + edgeNoise) / 0.48f)
    val grove = smooth01((forest - 0.06f + edgeNoise) / 0.46f)
    val coolEnough = 1f - smooth01((temperatureAt(x, z, h) - 0.30f + edgeNoise * 0.30f) / 0.34f)
    if coldAt(x, z, h) || desertAt(x, z, h) || h <= Terrain.seaLevel + 2 then 0f else (wet * grove * coolEnough).max(0f).min(1f)

  def grassTintAt(x: Int, z: Int, h: Int): (Float, Float, Float) =
    val savanna = savannaBlendAt(x, z, h).max(0f).min(1f)
    val birch = (birchBlendAt(x, z, h) * (1f - savanna * 0.82f)).max(0f).min(1f)
    val plains = (1f - savanna * 0.86f - birch * 0.82f).max(0.18f)
    val cold = if coldAt(x, z, h) then 0.45f else 0f
    val total = (plains + savanna + birch).max(0.001f)
    // Stronger biome color anchors keep the grass blocks visibly different after
    // texture modulation and lighting: plains is rich green, birch is pale/lush,
    // and savanna is dry olive.
    var r = (plains * 0.42f + birch * 0.66f + savanna * 0.54f) / total
    var g = (plains * 0.82f + birch * 0.94f + savanna * 0.58f) / total
    var b = (plains * 0.25f + birch * 0.36f + savanna * 0.13f) / total
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
    coldAt(x, z, h) && moistureAt(x, z) > -0.28f

  private def caveMouthDepthAt(x: Int, z: Int, h: Int): Int =
    if h <= Terrain.seaLevel + 8 || h >= Terrain.worldHeight - 16 then 0
    else
      // Sparse side-of-hill entrances. Wider irregular throats make caves read like
      // natural Minecraft-style hillside mouths instead of circular top-down gashes.
      val cellSize = 60
      val cellX = Math.floorDiv(x, cellSize)
      val cellZ = Math.floorDiv(z, cellSize)
      val gate = hash(cellX, cellZ, 0, 8600)
        if gate < 0.840f then 0
      else
        val anchorX = cellX * cellSize + 12 + (hash(cellX, 0, cellZ, 8601) * (cellSize - 24)).toInt
        val anchorZ = cellZ * cellSize + 12 + (hash(cellX, 1, cellZ, 8602) * (cellSize - 24)).toInt
        val dx = (x - anchorX).toFloat
        val dz = (z - anchorZ).toFloat
        val angle = hash(cellX, 4, cellZ, 8606) * Pi.toFloat * 2f
        val dirX = cos(angle).toFloat
        val dirZ = sin(angle).toFloat
        val forward = dx * dirX + dz * dirZ
        val side = abs(-dx * dirZ + dz * dirX)
        val length = 24f + hash(cellX, 5, cellZ, 8607) * 38f
        val throatRadius = 1.75f + hash(cellX, 6, cellZ, 8608) * 1.85f
        val taper = 1.58f - forward / (length * 1.75f)
        val slopeSignal = ridgedNoise((x + 120.0) * 0.006, (z - 280.0) * 0.006, 8604, 3)
        val raggedEdge = valueNoise2D((x + 300.0) * 0.12, (z - 170.0) * 0.12, 8611) * 0.38f
        val terrainOk = h > Terrain.seaLevel + 14 && (slopeSignal > 0.38f || h > Terrain.seaLevel + 38)
        if terrainOk && forward >= -2f && forward <= length && side <= throatRadius * taper.max(0.46f) + raggedEdge then
          val sideEdge = 1f - (side / throatRadius.max(0.001f)).min(1f)
          val entrance = 3.0f + sideEdge * 3.4f
          val slopedDown = forward.max(0f) * (0.28f + hash(cellX, 7, cellZ, 8609) * 0.20f)
          (entrance + slopedDown).toInt.max(2).min(32)
        else 0

  private def caveWaterSourceAt(x: Int, y: Int, z: Int): Boolean =
    if y <= 8 || y >= Terrain.seaLevel - 6 then false
    else
      val cell = 11
      val cx = Math.floorDiv(x, cell)
      val cy = Math.floorDiv(y, 5)
      val cz = Math.floorDiv(z, cell)
      hash(cx, cy, cz, 8700) > 0.976f && valueNoise3D((x + 500.0) * 0.026, (y - 40.0) * 0.022, (z - 300.0) * 0.026, 8701) > 0.14f

  private def caveNoise(x: Int, y: Int, z: Int, h: Int): Boolean =
    caveNoise(x, y, z, h, caveMouthDepthAt(x, z, h))

  private def caveNoise(x: Int, y: Int, z: Int, h: Int, mouthDepth: Int): Boolean =
    if y <= 4 || y >= Terrain.worldHeight - 4 then return false
    val depth = (h - y).max(0).toFloat
    val mouth = mouthDepth > 0 && depth >= 0f && depth <= mouthDepth.toFloat
    if mouth then
      val throat = valueNoise3D(x.toDouble * 0.024, y.toDouble * 0.030, z.toDouble * 0.024, 8610)
      val ceilingBreak = valueNoise3D((x + 90).toDouble * 0.055, (y - 20).toDouble * 0.045, (z + 40).toDouble * 0.055, 8612)
      return depth < 4f || throat > -0.52f || ceilingBreak > 0.38f || depth < mouthDepth.toFloat * 0.52f
    if y >= h - 10 then return false
    val lowEnough = y > 7 && depth > 13f
    val dx = x.toDouble; val dy = y.toDouble; val dz = z.toDouble
    val region = valueNoise3D(dx * 0.0037, dy * 0.0026, dz * 0.0037, 77)
    if region < -0.68f then return false
    val roomA = valueNoise3D(dx * 0.0080, dy * 0.0054, dz * 0.0080, 99)
    val roomB = valueNoise3D((dx + 700.0) * 0.0108, dy * 0.0079, (dz - 500.0) * 0.0108, 199)
    val roomC = valueNoise3D((dx - 230.0) * 0.0047, dy * 0.0035, (dz + 830.0) * 0.0047, 777)
    val cavern = lowEnough && depth > 20f && region > -0.26f && roomA > 0.20f && roomB > -0.34f && roomC > -0.38f
    val bigRoom = lowEnough && depth > 34f && region > -0.04f && roomA > 0.32f && roomB > -0.22f
    val giantPocket = lowEnough && depth > 46f && roomC > 0.28f && region > -0.06f && roomA > 0.00f
    val tubeA = abs(valueNoise2D(dx * 0.0055, dz * 0.0055, 333))
    val tubeB = abs(valueNoise2D((dx + 120.0) * 0.0064, (dz - 90.0) * 0.0064, 444))
    val verticalBand = valueNoise3D(dx * 0.0047, dy * 0.0104, dz * 0.0047, 555)
    val tubeC = abs(valueNoise2D((dx - 360.0) * 0.0044, (dz + 210.0) * 0.0044, 445))
    val tunnel = lowEnough && depth > 13f && y < Terrain.seaLevel + 70 && ((tubeA < 0.037f && verticalBand > -0.52f) || (tubeB < 0.033f && verticalBand > -0.45f) || (tubeC < 0.024f && verticalBand > -0.30f))
    cavern || bigRoom || giantPocket || tunnel

  def heightAt(x: Int, z: Int): Int =
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

    val hillMask = smooth01(((fbm2D((wx + 900.0) * 0.00165, (wz - 400.0) * 0.00165, 4, 101) + 0.48f) / 0.90f))
    val highlandMask = smooth01(((fbm2D((wx - 540.0) * 0.00130, (wz + 220.0) * 0.00130, 5, 1001) + 0.30f) / 0.82f))
    val mountainRegion = smooth01(((fbm2D((wx - 1500.0) * 0.00078, (wz + 700.0) * 0.00078, 5, 202) + 0.34f) / 0.80f))
    val mountainMask = mountainRegion * smooth01(((continent + 0.66) / 1.08).toFloat)

    val rolling = (ridgedNoise((wx - 110.0) * 0.0038, (wz + 210.0) * 0.0038, 171, 4).toDouble * 28.0 + localRoll) * hillMask.toDouble
    val highlands = ridgedNoise((wx + 130.0) * 0.0025, (wz - 710.0) * 0.0025, 1002, 5).toDouble * 27.0 * highlandMask.toDouble
    val mountainCore = ridgedNoise((wx + 410.0) * 0.0028, (wz - 70.0) * 0.0028, 201, 5).toDouble
    val mountainDetail = ridgedNoise((wx - 240.0) * 0.0072, (wz + 520.0) * 0.0072, 301, 3).toDouble * 12.0
    val peakShape = pow(mountainCore.max(0.001), 1.55)
    val mountains = (peakShape * 62.0 + mountainDetail) * mountainMask.toDouble

    // Divots and soft valleys break up the flat green-sheet look without creating ugly holes.
    val valley = abs(fbm2D((wx - 90.0) * 0.0027, (wz + 120.0) * 0.0027, 5, 251).toDouble)
    val riverish = smooth01((max(0.0, 0.132 - valley) / 0.132).toFloat).toDouble
    val riverCarve = riverStrengthAt(x, z).toDouble
    val lakeCarve = lakeStrengthAt(x, z).toDouble
    val valleyCut = riverish * (7.5 + 10.0 * mountainMask + 4.0 * highlandMask) + riverCarve * (5.0 + 7.0 * highlandMask) + lakeCarve * 4.5
    val dimple = smooth01((max(0.0, 0.095 - abs(fbm2D((wx + 70.0) * 0.0068, (wz - 190.0) * 0.0068, 4, 252).toDouble)) / 0.095).toFloat).toDouble
    val divots = dimple * (2.0 + 4.0 * hillMask)

    val desertish = desertRegionAt(x, z) > 0.10f && moistureAt(x, z) < 0.12f && temperatureAt(x, z, Terrain.seaLevel + 8) > -0.02f
    val dryRelief =
      if desertish then
        val dune = fbm2D((wx + 220.0) * 0.010, (wz - 180.0) * 0.010, 3, 811).toDouble * 4.2
        val mesaMask = smooth01(((fbm2D((wx - 600.0) * 0.00155, (wz + 330.0) * 0.00155, 4, 813) + 0.20f) / 0.70f)).toDouble
        val mesa = ridgedNoise((wx - 500.0) * 0.0048, (wz + 260.0) * 0.0048, 812, 3).toDouble * 12.0 * mesaMask
        dune + mesa
      else 0.0

    // Biome-specific relief, blended softly so there are no hard biome cliffs.
    val temp0 = temperatureAt(x, z, Terrain.seaLevel + 10)
    val moist0 = moistureAt(x, z)
    val savannaReliefBlend = smooth01((temp0 + 0.02f) / 0.32f).toDouble * smooth01((0.18f - moist0) / 0.42f).toDouble * smooth01((desertRegionAt(x, z) + 0.18f) / 0.62f).toDouble * (if desertish then 0.15 else 1.0)
    val birchReliefBlend = smooth01((moist0 - 0.04f) / 0.46f).toDouble * smooth01((forestRegionAt(x, z) - 0.04f) / 0.48f).toDouble * (1.0 - savannaReliefBlend.min(1.0) * 0.75)
    val savannaRoll = (ridgedNoise((wx + 320.0) * 0.0045, (wz - 160.0) * 0.0045, 824, 3).toDouble * 7.5 - 2.0) * savannaReliefBlend
    val birchRoll = fbm2D((wx - 130.0) * 0.0064, (wz + 360.0) * 0.0064, 3, 825).toDouble * 3.4 * birchReliefBlend

    val base =
      Terrain.seaLevel + 8.0 +
      continent * 22.0 +
      regional +
      rolling +
      highlands +
      mountains +
      dryRelief +
      savannaRoll +
      birchRoll +
      detail -
      valleyCut -
      divots

    base.round.toInt.max(1).min(Terrain.worldHeight - 8)

  def surfaceBlock(x: Int, y: Int, z: Int, h: Int): Block =
    val beach = beachAt(h)
    val freshwaterShore = freshwaterShoreAt(x, z, h)
    if beach || freshwaterShore || desertAt(x, z, h) then Block.Sand
    else if coldAt(x, z, h) then Block.Snow
    else if cliffAt(x, z, h) then Block.Stone
    else if rockyAt(x, z, h) && hash(x, h, z, 733) > 0.82f then Block.Stone
    else Block.Grass

  def fillBlock(x: Int, y: Int, z: Int, h: Int): Block =
    val beach = beachAt(h)
    val freshwaterShore = freshwaterShoreAt(x, z, h)
    val desert = desertAt(x, z, h)
    val surface = surfaceBlock(x, y, z, h)
    if y == h then surface
    else if y > h - (if desert then 2 else 3) && (beach || freshwaterShore || desert) then Block.Sand
    else if surface == Block.Stone && y > h - (if cliffAt(x, z, h) then 7 else 4) then Block.Stone
    else if y > h - 6 && cliffAt(x, z, h) && hash(x, y, z, 734) > 0.20f then Block.Stone
    else if y > h - 18 then Block.Dirt
    else if y < 6 && h > Terrain.seaLevel then Block.Stone
    else if y < h - 28 && y < 12 && hash(x, y, z, 17) > 0.92f then Block.Clay
    else if y < h - 28 && y < 60 && hash(x, y, z, 37) > 0.88f then Block.Coal
    else if y < h - 28 && y < 48 && hash(x, y, z, 71) > 0.91f then Block.Copper
    else if y < h - 28 && y < 40 && hash(x, y, z, 81) > 0.955f then Block.IronOre
    else if y < h - 28 && y < 30 && hash(x, y, z, 91) > 0.97f then Block.GoldOre
    else if y < h - 28 && y < 16 && hash(x, y, z, 101) > 0.985f then Block.Diamond
    else if y <= 4 then Block.Bedrock
    else Block.Stone

  def furnaceAt(x: Int, z: Int, h: Int, surface: Block): Option[(Int, Int, Int)] =
    // Furnaces should be player-made utility blocks, not random world decoration.
    None

  def treeAt(x: Int, z: Int, h: Int, surface: Block): Option[(Int, Int, Int, Int)] =
    if h <= Terrain.seaLevel + 2 || surface == Block.Sand || surface == Block.Stone || caveMouthDepthAt(x, z, h) > 0 then return None
    val forestNoise = forestRegionAt(x, z)
    val spruce = spruceAt(x, z, h)
    val savanna = savannaAt(x, z, h)
    val birch = birchGroveAt(x, z, h)
    val spacingCell = if spruce then 7 else if savanna then 10 else if birch then 8 else if forestNoise > 0.34f then 8 else 11
    val cellX = Math.floorDiv(x, spacingCell)
    val cellZ = Math.floorDiv(z, spacingCell)
    val cellGate = hash(cellX, cellZ, 0, 913)
    val treeChance = hash(x, z, 0, 19)
    val dense = forestNoise > 0.22f && moistureAt(x, z) > -0.42f
    val threshold =
      if spruce then 0.972f
      else if savanna then 0.976f
      else if birch then 0.974f
      else if dense then 0.978f
      else if forestNoise > -0.08f then 0.991f
      else 0.9970f
    val height =
      if spruce then 8 + (hash(x, z, 0, 42) * 5).toInt
      else if savanna then 5 + (hash(x, z, 0, 45) * 4).toInt
      else if birch then 6 + (hash(x, z, 0, 46) * 3).toInt
      else if hash(x, z, 0, 43) > 0.78f then 7 + (hash(x, z, 0, 44) * 4).toInt
      else 4 + (hash(x, z, 0, 41) * 4).toInt
    if cellGate > 0.44f && treeChance > threshold then Some((x, h + 1, z, height))
    else None

  def cactusAt(x: Int, z: Int, h: Int, surface: Block): Option[(Int, Int, Int, Int)] =
    if surface != Block.Sand || !desertAt(x, z, h) || h <= Terrain.seaLevel + 2 || caveMouthDepthAt(x, z, h) > 0 then None
    else
      val cellOk = hash(Math.floorDiv(x, 8), Math.floorDiv(z, 8), 0, 3033) > 0.66f
      val chance = hash(x, z, 0, 3034)
      if cellOk && chance > 0.982f then Some((x, h + 1, z, 2 + (hash(x, z, 0, 3035) * 3).toInt))
      else None

  def bushAt(x: Int, z: Int, h: Int, surface: Block): Option[(Int, Int, Int, Int)] =
    if surface != Block.Grass || h <= Terrain.seaLevel + 2 || coldAt(x, z, h) || cliffAt(x, z, h) || caveMouthDepthAt(x, z, h) > 0 then None
    else
      val grove = forestRegionAt(x, z)
      val cellOk = hash(Math.floorDiv(x, 9), Math.floorDiv(z, 9), 0, 4040) > 0.88f
      val chance = hash(x, z, 0, 4041)
      if grove > 0.02f && cellOk && chance > 0.994f then Some((x, h + 1, z, 1))
      else None

  def boulderAt(x: Int, z: Int, h: Int, surface: Block): Option[(Int, Int, Int, Int)] =
    if h <= Terrain.seaLevel + 4 || surface == Block.Sand || surface == Block.Snow || caveMouthDepthAt(x, z, h) > 0 then None
    else
      val rockyPatch = rockyAt(x, z, h) || fbm2D((x + 40.0) * 0.006, (z - 90.0) * 0.006, 3, 5050) > 0.42f
      val cellOk = hash(Math.floorDiv(x, 13), Math.floorDiv(z, 13), 0, 5051) > 0.86f
      val chance = hash(x, z, 0, 5052)
      if rockyPatch && cellOk && chance > 0.991f then Some((x, h + 1, z, if hash(x, z, 0, 5053) > 0.78f then 2 else 1))
      else None

  private def smooth01(v: Float): Float =
    val t = v.max(0f).min(1f); t * t * (3f - 2f * t)

  def fillChunkBlocks(cx: Int, cz: Int): Array[Byte] =
    val baseX = cx * Terrain.chunkSize
    val baseZ = cz * Terrain.chunkSize
    val endX = baseX + Terrain.chunkSize - 1
    val endZ = baseZ + Terrain.chunkSize - 1
    val blocks = new Array[Byte](Terrain.chunkSize * Terrain.worldHeight * Terrain.chunkSize)
    def idx(lx: Int, y: Int, lz: Int): Int = (y * Terrain.chunkSize + lz) * Terrain.chunkSize + lx

    // Cache a slightly expanded height field so cliff/surface decoration does not call
    // heightAt repeatedly. Complex terrain keeps quality, but chunk creation hitches less.
    val columnCount = Terrain.chunkSize * Terrain.chunkSize
    val heights = new Array[Int](columnCount)
    val surfaces = new Array[Block](columnCount)
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
    def cachedCliff(lx: Int, lz: Int, h: Int): Boolean = h > Terrain.seaLevel + 34 && cachedSlope(lx, lz, h) >= 5

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
        val desert = desertAt(wx, wz, h)
        val cold = coldAt(wx, wz, h)
        val rocky = rockyAt(wx, wz, h)
        val cliff = cachedCliff(lx, lz, h)
        val surface =
          if beach || shore || desert then Block.Sand
          else if cold then Block.Snow
          else if cliff then Block.Stone
          else if rocky && hash(wx, h, wz, 733) > 0.82f then Block.Stone
          else Block.Grass
        heights(ci) = h
        surfaces(ci) = surface
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
        val desert = desertFlags(ci)
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
              else if y > h - (if desert then 2 else 3) && (beach || shore || desert) then Block.Sand
              else if surface == Block.Stone && y > h - (if cliff then 7 else 4) then Block.Stone
              else if y > h - 6 && cliff && hash(wx, y, wz, 734) > 0.20f then Block.Stone
              else if y > h - 18 then Block.Dirt
              else if y < 6 && h > Terrain.seaLevel then Block.Stone
              else if y < h - 28 && y < 12 && hash(wx, y, wz, 17) > 0.92f then Block.Clay
              else if y < h - 28 && y < 60 && hash(wx, y, wz, 37) > 0.88f then Block.Coal
              else if y < h - 28 && y < 48 && hash(wx, y, wz, 71) > 0.91f then Block.Copper
              else if y < h - 28 && y < 40 && hash(wx, y, wz, 81) > 0.955f then Block.IronOre
              else if y < h - 28 && y < 30 && hash(wx, y, wz, 91) > 0.97f then Block.GoldOre
              else if y < h - 28 && y < 16 && hash(wx, y, wz, 101) > 0.985f then Block.Diamond
              else if y <= 4 then Block.Bedrock
              else Block.Stone
            val canCave = y > 4 && (y < h - 12 || (mouthDepth > 0 && y >= h - mouthDepth))
            val cave = canCave && caveNoise(wx, y, wz, h, mouthDepth)
            if !cave then blocks(idx(lx, y, lz)) = block.id
          else if y <= waterLine && y > h then
            blocks(idx(lx, y, lz)) = Block.Water.id
          y += 1
        lz += 1
      lx += 1

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
          if blocks(i) == Block.Air.id && blocks(below) != Block.Air.id && blocks(below) != Block.Water.id && caveWaterSourceAt(wx, y, wz) then
            blocks(i) = Block.Water.id
            if lx + 1 < Terrain.chunkSize && blocks(idx(lx + 1, y, lz)) == Block.Air.id then blocks(idx(lx + 1, y, lz)) = Block.Water.id
            if lx > 0 && blocks(idx(lx - 1, y, lz)) == Block.Air.id && hash(wx, y, wz, 8702) > 0.42f then blocks(idx(lx - 1, y, lz)) = Block.Water.id
            if lz + 1 < Terrain.chunkSize && blocks(idx(lx, y, lz + 1)) == Block.Air.id then blocks(idx(lx, y, lz + 1)) = Block.Water.id
            if lz > 0 && blocks(idx(lx, y, lz - 1)) == Block.Air.id && hash(wx, y, wz, 8703) > 0.42f then blocks(idx(lx, y, lz - 1)) = Block.Water.id
          y += 2
        lz += 1
      lx += 1

    def cachedChunkHeight(wx: Int, wz: Int): Int =
      val lx0 = wx - baseX
      val lz0 = wz - baseZ
      if lx0 >= 0 && lx0 < Terrain.chunkSize && lz0 >= 0 && lz0 < Terrain.chunkSize then heights(cidx(lx0, lz0))
      else
        val ex0 = wx - baseX + extPad
        val ez0 = wz - baseZ + extPad
        if ex0 >= 0 && ex0 < extSize && ez0 >= 0 && ez0 < extSize then extHeights(eidx(ex0, ez0)) else heightAt(wx, wz)
    def cachedChunkSurface(wx: Int, wz: Int, h: Int): Block =
      val lx0 = wx - baseX
      val lz0 = wz - baseZ
      if lx0 >= 0 && lx0 < Terrain.chunkSize && lz0 >= 0 && lz0 < Terrain.chunkSize then surfaces(cidx(lx0, lz0)) else surfaceBlock(wx, h, wz, h)
    def columnIsDecoratable(wx: Int, wz: Int, h: Int): Boolean =
      if caveMouthDepthAt(wx, wz, h) > 0 then false
      else
        val lx0 = wx - baseX
        val lz0 = wz - baseZ
        if lx0 >= 0 && lx0 < Terrain.chunkSize && lz0 >= 0 && lz0 < Terrain.chunkSize && h >= 0 && h < Terrain.worldHeight - 2 then
          val here = blocks(idx(lx0, h, lz0))
          val above = blocks(idx(lx0, h + 1, lz0))
          here != Block.Air.id && here != Block.Water.id && above == Block.Air.id
        else true

    for wx <- baseX - 4 to endX + 4; wz <- baseZ - 4 to endZ + 4 do
      val h = cachedChunkHeight(wx, wz)
      val surface = cachedChunkSurface(wx, wz, h)
      if columnIsDecoratable(wx, wz, h) then
        treeAt(wx, wz, h, surface).foreach { case (tx, ty, tz, th) =>
          placeTreeWorld(blocks, baseX, baseZ, tx, ty, tz, th, spruceAt(wx, wz, h))
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
    blocks

  private def placeTreeWorld(blocks: Array[Byte], baseX: Int, baseZ: Int, x: Int, y: Int, z: Int, height: Int, spruce: Boolean): Unit =
    val dry = savannaAt(x, z, y - 1)
    val birch = !spruce && !dry && birchGroveAt(x, z, y - 1)
    val wood = if spruce then Block.PineWood else if dry then Block.AcaciaWood else if birch then Block.BirchWood else Block.Wood
    val leaves = if spruce then Block.PineLeaves else if dry then Block.AcaciaLeaves else if birch then Block.BirchLeaves else Block.Leaves
    def set(wx: Int, wy: Int, wz: Int, block: Block): Unit =
      val lx = wx - baseX; val lz = wz - baseZ
      if lx >= 0 && lx < Terrain.chunkSize && lz >= 0 && lz < Terrain.chunkSize && wy >= 0 && wy < Terrain.worldHeight then
        val i = idx(lx, wy, lz)
        if block != leaves || blocks(i) == Block.Air.id then blocks(i) = block.id
    val leanX = if dry then (if hash(x, z, 0, 6005) > 0.5f then 1 else -1) else if !spruce && hash(x, z, 0, 6001) > 0.88f then (if hash(x, z, 0, 6002) > 0.5f then 1 else -1) else 0
    val leanZ = if dry then (if hash(x, z, 0, 6006) > 0.5f then 1 else -1) else if !spruce && hash(x, z, 0, 6003) > 0.88f then (if hash(x, z, 0, 6004) > 0.5f then 1 else -1) else 0
    for dy <- 0 until height do
      val sx = x + (if dy > height / 2 then leanX else 0)
      val sz = z + (if dy > height / 2 then leanZ else 0)
      set(sx, y + dy, sz, wood)
    val topX = x + leanX
    val topZ = z + leanZ
    if spruce then
      for dy <- 2 to height + 4 do
        val layer = dy - 2
        val radius = (4 - layer / 2).max(1)
        for dx <- -radius to radius; dz <- -radius to radius do
          val dist = abs(dx) + abs(dz)
          if dist <= radius + 1 && !(dx == 0 && dz == 0 && dy < height) then
            set(topX + dx, y + dy, topZ + dz, leaves)
      set(topX, y + height + 4, topZ, leaves)
    else if dry then
      // Sparse acacia-ish flat canopy.
      for dx <- -3 to 3; dz <- -3 to 3; dy <- height - 1 to height + 1 do
        val dist = abs(dx) + abs(dz)
        if dist <= 4 && hash(topX + dx, y + dy, topZ + dz, 6110) > 0.18f then set(topX + dx, y + dy, topZ + dz, leaves)
    else
      val wide = hash(x, z, 0, 6101) > 0.68f
      val leafStart = if wide then height - 3 else height - 2
      val leafSpread = if wide then 4 else if height >= 7 then 4 else 3
      for dx <- -leafSpread to leafSpread; dz <- -leafSpread to leafSpread; dy <- leafStart to height + 2 do
        val dist = abs(dx) + abs(dz) + max(0, dy - height)
        val edgeNoise = hash(topX + dx, y + dy, topZ + dz, 6102)
        if dist <= leafSpread + (if edgeNoise > 0.62f then 1 else 0) && !(dx == 0 && dz == 0 && dy > height - 1) then
          set(topX + dx, y + dy, topZ + dz, leaves)

  private def placeCactusWorld(blocks: Array[Byte], baseX: Int, baseZ: Int, x: Int, y: Int, z: Int, height: Int): Unit =
    def set(wx: Int, wy: Int, wz: Int, block: Block): Unit =
      val lx = wx - baseX; val lz = wz - baseZ
      if lx >= 0 && lx < Terrain.chunkSize && lz >= 0 && lz < Terrain.chunkSize && wy >= 0 && wy < Terrain.worldHeight then
        blocks(idx(lx, wy, lz)) = block.id
    for dy <- 0 until height do set(x, y + dy, z, Block.Cactus)

  private def placeBushWorld(blocks: Array[Byte], baseX: Int, baseZ: Int, x: Int, y: Int, z: Int, radius: Int): Unit =
    def set(wx: Int, wy: Int, wz: Int, block: Block): Unit =
      val lx = wx - baseX; val lz = wz - baseZ
      if lx >= 0 && lx < Terrain.chunkSize && lz >= 0 && lz < Terrain.chunkSize && wy >= 0 && wy < Terrain.worldHeight then
        val i = idx(lx, wy, lz)
        if blocks(i) == Block.Air.id then blocks(i) = block.id
    // Small hand-placed shrub shape. This prevents the old dense leaf blanket look.
    set(x, y, z, Block.Leaves)
    if hash(x, z, 0, 4044) > 0.35f then set(x + 1, y, z, Block.Leaves)
    if hash(x, z, 0, 4045) > 0.35f then set(x - 1, y, z, Block.Leaves)
    if hash(x, z, 0, 4046) > 0.35f then set(x, y, z + 1, Block.Leaves)
    if hash(x, z, 0, 4047) > 0.35f then set(x, y, z - 1, Block.Leaves)
    if hash(x, z, 0, 4048) > 0.72f then set(x, y + 1, z, Block.Leaves)

  private def placeBoulderWorld(blocks: Array[Byte], baseX: Int, baseZ: Int, x: Int, y: Int, z: Int, radius: Int): Unit =
    def set(wx: Int, wy: Int, wz: Int, block: Block): Unit =
      val lx = wx - baseX; val lz = wz - baseZ
      if lx >= 0 && lx < Terrain.chunkSize && lz >= 0 && lz < Terrain.chunkSize && wy >= 0 && wy < Terrain.worldHeight then
        blocks(idx(lx, wy, lz)) = block.id
    for dx <- -radius to radius; dz <- -radius to radius; dy <- 0 to radius do
      val ell = dx * dx + dz * dz + dy * dy
      if ell <= radius * radius + 1 && hash(x + dx, y + dy, z + dz, 5054) > 0.16f then
        set(x + dx, y + dy, z + dz, if hash(x + dx, y + dy, z + dz, 5055) > 0.74f then Block.Clay else Block.Stone)

  private def placeTree(blocks: Array[Byte], lx: Int, y: Int, lz: Int, height: Int): Unit =
    val baseX = 0; val baseZ = 0
    for dy <- 0 until height do
      val ly = y + dy
      if ly < Terrain.worldHeight then blocks(idx(lx, ly, lz)) = Block.Wood.id
    val leafStart = height - 2
    val leafSpread = if height >= 6 then 4 else 3
    for dx <- -leafSpread to leafSpread; dz <- -leafSpread to leafSpread; dy <- leafStart to height + 1 do
      val ax = lx + dx; val ay = y + dy; val az = lz + dz
      if ax >= 0 && ax < Terrain.chunkSize && az >= 0 && az < Terrain.chunkSize && ay >= 0 && ay < Terrain.worldHeight then
        val dist = abs(dx) + abs(dz) + max(0, dy - height)
        if dist <= leafSpread + 1 && !(dx == 0 && dz == 0 && dy > height - 1) then
          blocks(idx(ax, ay, az)) = Block.Leaves.id

  private def idx(lx: Int, y: Int, lz: Int): Int =
    (y * Terrain.chunkSize + lz) * Terrain.chunkSize + lx

