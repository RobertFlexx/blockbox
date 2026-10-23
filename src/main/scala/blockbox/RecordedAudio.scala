package blockbox

import java.nio.file.{Files, Path}
import java.util.concurrent.ConcurrentHashMap
import org.lwjgl.BufferUtils
import org.lwjgl.stb.STBVorbis
import org.lwjgl.system.MemoryUtil
import scala.math.*

/** Bounded, worker-thread Vorbis decoding of the bundled CC0 recordings. */
object RecordedAudio:
  private val cache = ConcurrentHashMap[String, Option[Array[Float]]]()
  private val digging = ConcurrentHashMap[(BlockboxSounds.Material, Int), Array[Float]]()
  private val root = Path
    .of(Option(System.getProperty("blockbox.audioRoot")).getOrElse("assets/audio"))
    .toAbsolutePath
    .normalize()

  /** Decode during the title screen, rather than behind the first block impact. */
  def preload(): Unit =
    if Files.isDirectory(root) then
      val files = Files.walk(root, 3)
      try
        import scala.jdk.CollectionConverters.*
        files.iterator().asScala.filter(p => p.toString.endsWith(".ogg")).take(128).foreach { p =>
          val name = root.relativize(p).toString.replace('\\', '/')
          load(name, loop = name.contains("rain-loop") || name.contains("furnace_loop"))
        }
      finally files.close()

  /** Keep two milliseconds before the first audible sample; recordings often include a quiet
    * lead-in that otherwise makes a correctly timed event lag.
    */
  private[blockbox] def trimLeadingSilence(samples: Array[Float]): Array[Float] =
    if samples.isEmpty then samples
    else
      val peak = samples.iterator.map(abs).max
      if peak < 0.00001f then samples
      else
        val onset = samples.indexWhere(v => abs(v) >= peak * 0.02f)
        val start = (onset - 88).max(0)
        if start == 0 then samples else java.util.Arrays.copyOfRange(samples, start, samples.length)

  def load(name: String, loop: Boolean = false): Option[Array[Float]] =
    if !name.matches("[a-zA-Z0-9_/-]+\\.ogg") || name.contains("..") then return None
    cache.computeIfAbsent(
      name + (if loop then "#loop" else "#shot"),
      _ => decode(root.resolve(name).normalize(), loop)
    )

  private def decode(path: Path, loop: Boolean): Option[Array[Float]] =
    if !path.startsWith(root) || !Files.isRegularFile(path) || Files.size(path) > 2 * 1024 * 1024
    then return None
    var encoded: java.nio.ByteBuffer = null
    var decoded: java.nio.ShortBuffer = null
    try
      val bytes = Files.readAllBytes(path)
      encoded = MemoryUtil.memAlloc(bytes.length); encoded.put(bytes).flip()
      val channels = BufferUtils.createIntBuffer(1); val rate = BufferUtils.createIntBuffer(1)
      decoded = STBVorbis.stb_vorbis_decode_memory(encoded, channels, rate)
      if decoded == null || channels.get(0) < 1 || channels.get(0) > 2 || rate.get(0) < 8000 || rate
          .get(0) > 96000
      then return None
      val count = decoded.remaining() / channels.get(0)
      if count < 2 || count.toLong > rate.get(0).toLong * 60 then return None
      val frames = (count.toLong * BlockboxSounds.sampleRate / rate.get(0)).toInt
      if frames < 2 then return None
      val result = new Array[Float](frames)
      var peak = 0f; var sum = 0.0; var i = 0
      def mono(frame: Int): Float =
        var value = 0f; var c = 0
        while c < channels.get(0) do
          value += decoded.get(frame * channels.get(0) + c) / 32768f
          c += 1
        value / channels.get(0)
      while i < frames do
        val position = i.toDouble * rate.get(0) / BlockboxSounds.sampleRate
        val a = position.toInt.min(count - 2); val fraction = (position - a).toFloat
        val value = mono(a) * (1f - fraction) + mono(a + 1) * fraction
        result(i) = value; sum += value; i += 1
      val mean = (sum / frames).toFloat
      i = 0
      while i < frames do
        result(i) -= mean; peak = peak.max(abs(result(i))); i += 1
      val gain = if peak > 0 then (if loop then 0.55f else 0.65f) / peak else 1f
      val clip = if loop then result else trimLeadingSilence(result)
      i = 0
      while i < clip.length do
        val fade = if loop then 1f else (i / 80f).min(1f) * ((clip.length - 1 - i) / 441f).min(1f)
        clip(i) *= gain * fade; i += 1
      // Source rain was crossfaded by its distributor. A short wrap blend also
      // removes the tiny residual Vorbis boundary discontinuity after decoding.
      if loop then
        val blend = (BlockboxSounds.sampleRate * 0.12f).toInt.min(frames / 4)
        val joined = new Array[Float](frames - blend)
        Array.copy(result, blend, joined, 0, joined.length)
        i = 0
        while i < blend do
          val t = i.toFloat / (blend - 1)
          joined(joined.length - blend + i) = result(frames - blend + i) * (1f - t) + result(i) * t
          i += 1
        val correction = joined.head - joined.last
        val tail = 441.min(joined.length / 8)
        i = 0
        while i < tail do
          val t = i.toFloat / (tail - 1)
          joined(joined.length - tail + i) += correction * t * t * (3f - 2f * t)
          i += 1
        Some(joined)
      else Some(clip)
    catch
      case e: Exception =>
        System.err.println(s"Recording unavailable: ${path.getFileName}: ${e.getMessage}"); None
    finally
      if decoded != null then MemoryUtil.memFree(decoded)
      if encoded != null then MemoryUtil.memFree(encoded)

  def effect(
      action: BlockboxSounds.Action,
      material: BlockboxSounds.Material,
      variant: Int
  ): Option[Array[Float]] =
    import BlockboxSounds.{Action, Material}
    if action == Action.Dig then
      return Some(
        digging.computeIfAbsent(
          (material, Math.floorMod(variant, BlockboxSounds.variants)),
          key =>
            val source = effect(Action.Place, key._1, key._2).getOrElse(
              BlockboxSounds.clip(Action.Place, key._1, key._2)
            )
            val count = source.length.min((BlockboxSounds.sampleRate * 0.12f).toInt)
            val result = new Array[Float](count)
            val coefficient =
              (1 - math.exp(-2 * math.Pi * 1400 / BlockboxSounds.sampleRate)).toFloat
            var filtered = 0f
            for i <- result.indices do
              filtered += (source(i) - filtered) * coefficient
              val fade = (i / 176f).min(1f) * ((count - i - 1) / 1323f).max(0).min(1f)
              result(i) = filtered * fade * 0.7f
            result
        )
      )
    val step = action == Action.Step || action == Action.Land
    val bank = if step then
      material match
        case Material.Grass | Material.Foliage                => Some("footstep_grass")
        case Material.Wood                                    => Some("footstep_wood")
        case Material.Snow                                    => Some("footstep_snow")
        case Material.Stone | Material.Metal | Material.Glass => Some("footstep_concrete")
        case _                                                => None
    else if action == Action.Break || action == Action.Place then
      material match
        case Material.Glass => Some("impactGlass_heavy")
        case Material.Wood  => Some("impactWood_medium")
        case Material.Metal => Some("impactMetal_light")
        case Material.Stone => Some("impactMining")
        case _              => Some("impactSoft_medium")
    else None
    if step && (material == Material.Grass || material == Material.Foliage) then
      load(f"foley/grass_step_${Math.floorMod(variant, 3)}%03d.ogg")
        .orElse(load(f"kenney/footstep_grass_${Math.floorMod(variant, 5)}%03d.ogg"))
    else if action == Action.Splash then
      load(f"foley/water_splash_${Math.floorMod(variant, 2)}%03d.ogg")
    else if step && material == Material.Sand then
      val kind = if Math.floorMod(variant, 2) == 0 then "place" else "break"
      load(f"foley/sand_${kind}_${Math.floorMod(variant / 2, 3)}%03d.ogg")
    else if step && material == Material.Snow then
      load(f"foley/snow_step_${Math.floorMod(variant, 4)}%03d.ogg")
    else if material == Material.Stone && action == Action.Break then load("foley/stone_break.ogg")
    else if material == Material.Stone && action == Action.Place then load("foley/stone_chip.ogg")
    else if material == Material.Sand && (action == Action.Break || action == Action.Place) then
      val kind = if action == Action.Break then "break" else "place"
      load(f"foley/sand_${kind}_${Math.floorMod(variant, 3)}%03d.ogg")
    else if action == Action.Thunder then load("weather/thunder-distant.ogg")
    else
      bank.flatMap(name =>
        load(f"kenney/${name}_${Math.floorMod(variant, if step then 5 else 3)}%03d.ogg")
      )
