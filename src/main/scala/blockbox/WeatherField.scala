package blockbox

import scala.math.*

/** Seed/time driven weather cells: every peer samples the same moving storm fronts. */
object WeatherField:
  final case class Cell(x: Float, z: Float, radius: Float, strength: Float, id: Long)
  final case class Sample(rain: Float, cloud: Float)
  final case class Strike(x: Float, z: Float, age: Float, strength: Float, id: Long)
  private val spacing = 560f
  // Clouds and precipitation share this world-space wind, including with a
  // fixed celestial time. Rendering never derives it from camera movement.
  val windX = 0.72f
  val windZ = 0.21f
  def cloudOffset(time: Float): (Float, Float) = (time * windX, time * windZ)
  private def hash(value: Long): Float =
    var h = value
    h = (h ^ (h >>> 30)) * 0xbf58476d1ce4e5b9L
    h = (h ^ (h >>> 27)) * 0x94d049bb133111ebL
    ((h ^ (h >>> 31)) >>> 40).toFloat / 16777215f

  def cells(
      seed: Long,
      time: Float,
      x: Float,
      z: Float,
      activity: Float,
      distance: Float = 1100f
  ): Vector[Cell] =
    val (driftX, driftZ) = cloudOffset(time)
    val gx = floor((x - driftX) / spacing).toInt; val gz = floor((z - driftZ) / spacing).toInt
    val radius = ceil(distance / spacing).toInt.min(5).max(1)
    (for ix <- gx - radius to gx + radius; iz <- gz - radius to gz + radius yield
      // Pack both signed coordinates before mixing: multiplying then XORing
      // creates mirrored cell-ID collisions, including duplicate thunder IDs.
      val id = seed ^ (ix.toLong << 32) ^ (iz.toLong & 0xffffffffL)
      val strength = BlockboxSky.smooth01((activity - (0.08f + hash(id + 41) * 0.36f)) / 0.48f)
      Cell(
        (ix + 0.20f + hash(id) * 0.60f) * spacing + driftX,
        (iz + 0.20f + hash(id + 7) * 0.60f) * spacing + driftZ,
        175f + hash(id + 19) * 115f,
        strength,
        id
      )
    ).filter(_.strength > 0.001f).toVector

  def sample(cells: Seq[Cell], x: Float, z: Float): Sample =
    var rain = 0f; var cloud = 0f
    cells.foreach { c =>
      val dx = x - c.x; val dz = z - c.z
      val d = sqrt(dx * dx + dz * dz).toFloat / c.radius
      rain = rain.max((1f - BlockboxSky.smooth01((d - 0.40f) / 0.60f)) * c.strength)
      cloud = cloud.max((1f - BlockboxSky.smooth01((d - 0.40f) / 0.95f)) * c.strength)
    }
    Sample(rain, cloud)

  def strikes(cells: Seq[Cell], time: Float): Vector[Strike] =
    val epoch = floor(time / 12f).toLong
    (for c <- cells if c.strength > 0.65f; e <- epoch - 1 to epoch yield
      val id = c.id ^ (e * 83492791L)
      val occurred = e * 12.0 + 2.0 + hash(id + 131) * 7.0
      Strike(
        c.x + (hash(id + 39) - 0.5f) * c.radius,
        c.z + (hash(id + 81) - 0.5f) * c.radius,
        (time - occurred).toFloat,
        c.strength,
        id
      )
    ).filter(s => s.age >= 0f && s.age < 7f && hash(s.id + 3) > 0.38f).toVector

  def flash(strikes: Seq[Strike], x: Float, z: Float): Float =
    strikes.foldLeft(0f) { (brightness, s) =>
      val d = sqrt(pow(x - s.x, 2) + pow(z - s.z, 2)).toFloat
      val pulse = exp(-s.age * 13).toFloat * (0.65f + 0.35f * cos(s.age * 65).toFloat)
      brightness.max(pulse * s.strength / (1f + d / 260f))
    }
