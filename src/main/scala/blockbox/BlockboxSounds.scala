package blockbox

import scala.math.*
import java.util.concurrent.ConcurrentHashMap

/** Original material foley: filtered impacts, friction, debris and short resonances. Clips are
  * immutable, deterministic variants cached on the synthesis worker.
  */
object BlockboxSounds:
  val sampleRate = 44100
  val variants = 6
  enum Material:
    case Stone, Wood, Dirt, Grass, Sand, Snow, Glass, Foliage, Metal
  enum Action:
    case Step, Place, Break, Land, Splash, Thunder, Hurt, Ui
  import Material.*
  import Action.*
  private final case class Key(action: Action, material: Material, variant: Int)
  private final case class Profile(
      body: Float,
      cutoff: Float,
      grit: Float,
      resonance: Float,
      decay: Float
  )
  private val cache = ConcurrentHashMap[Key, Array[Float]]()

  def material(block: Block): Material = block match
    case Block.Wood | Block.BirchWood | Block.PineWood | Block.AcaciaWood | Block.Planks |
        Block.BirchPlanks | Block.PinePlanks | Block.AcaciaPlanks | Block.Torch |
        Block.SupplyCrate =>
      Wood
    case Block.Grass             => Grass
    case Block.Dirt | Block.Clay => Dirt
    case Block.Sand              => Sand
    case Block.Snow              => Snow
    case Block.Glass             => Glass
    case Block.Leaves | Block.BirchLeaves | Block.PineLeaves | Block.AcaciaLeaves | Block.Cactus |
        Block.Reeds | Block.LilyPad | Block.LeafLitter =>
      Foliage
    case Block.Copper | Block.IronIngot | Block.GoldIngot => Metal
    case _                                                => Stone

  def clip(action: Action, material: Material, variant: Int): Array[Float] =
    val key = Key(action, material, Math.floorMod(variant, variants))
    cache.computeIfAbsent(key, k => synthesize(k.action, k.material, k.variant))

  private def profile(material: Material): Profile = material match
    case Stone   => Profile(185, 2800, 0.72f, 0.09f, 0.030f)
    case Wood    => Profile(125, 1900, 0.34f, 0.24f, 0.045f)
    case Dirt    => Profile(90, 1050, 0.56f, 0.015f, 0.039f)
    case Grass   => Profile(105, 2600, 0.66f, 0.025f, 0.040f)
    case Sand    => Profile(85, 3800, 0.79f, 0.010f, 0.049f)
    case Snow    => Profile(95, 2200, 0.68f, 0.015f, 0.045f)
    case Glass   => Profile(1900, 7600, 0.27f, 0.18f, 0.031f)
    case Foliage => Profile(140, 4800, 0.80f, 0.010f, 0.043f)
    case Metal   => Profile(430, 5700, 0.40f, 0.18f, 0.046f)

  private final class Noise(seed: Int):
    private var state = if seed == 0 then 1 else seed
    def next(): Float =
      state ^= state << 13; state ^= state >>> 17; state ^= state << 5
      ((state >>> 8).toFloat / 8388608f) - 1f
    def unit(): Float = (next() + 1f) * 0.5f

  private final class LowPass(frequency: Float):
    private val coefficient = (1.0 - exp(-2 * Pi * frequency / sampleRate)).toFloat
    private var last = 0f
    def apply(value: Float): Float =
      last += (value - last) * coefficient
      last

  private def pulse(t: Float, start: Float, attack: Float, decay: Float): Float =
    val x = t - start
    if x <= 0 then 0f else ((1 - exp(-x / attack)) * exp(-x / decay)).toFloat

  def synthesize(action: Action, material: Material, variant: Int): Array[Float] =
    val rng = Noise(
      0x61c88647 ^ material.ordinal * 19349663 ^ action.ordinal * 73856093 ^ variant * 83492791
    )
    val p = profile(material)
    val duration = action match
      case Step    => 0.19f
      case Place   => 0.16f
      case Break   => if material == Glass then 0.43f else 0.34f
      case Land    => 0.25f
      case Splash  => 0.55f
      case Thunder => 2.20f
      case Hurt    => 0.22f
      case Ui      => 0.055f
    val result = new Array[Float]((duration * sampleRate).toInt)
    val low = LowPass(if action == Thunder then 150 else p.cutoff)
    val bass = LowPass(if action == Thunder then 48 else 260)
    val hiss = LowPass(6200)
    val pitch = 0.94f + rng.unit() * 0.12f
    val fragments =
      if action == Break then 11 else if action == Step || action == Land then 4 else 2
    val starts = Array.tabulate(fragments)(j =>
      if j == 0 then 0f
      else if action == Break then 0.012f + j * 0.014f + rng.unit() * 0.025f
      else 0.018f + j * 0.009f + rng.unit() * 0.008f
    )
    val weights = Array.tabulate(fragments)(j => (0.40f + rng.unit() * 0.60f) / (1f + j * 0.42f))
    val modal = material match
      case Glass => Array(1f, 1.73f, 2.41f, 3.08f, 3.77f)
      case Metal => Array(1f, 2.76f, 4.13f, 6.27f)
      case Wood  => Array(1f, 2.19f, 3.72f)
      case _     => Array(1f, 1.51f)
    var previousInput = 0f; var dc = 0f; var sumSquares = 0.0; var peak = 0f
    var i = 0
    while i < result.length do
      val t = i.toFloat / sampleRate
      val white = rng.next()
      val filtered = low(white); val lowBody = bass(white); val fine = white - hiss(white)
      var body = 0f; var grit = 0f
      var j = 0
      while j < fragments do
        val decay =
          p.decay * (if action == Break then 0.68f else if action == Place then 0.70f else 1f)
        body += pulse(t, starts(j), 0.0015f, decay) * weights(j)
        grit += pulse(
          t,
          starts(j) + 0.004f,
          if material == Sand || material == Foliage then 0.008f else 0.0025f,
          decay * 1.2f
        ) * weights(j)
        j += 1
      var value =
        filtered * grit * p.grit + lowBody * body * (if material == Glass then 0.12f else 1.8f)
      if material == Grass || material == Snow || material == Foliage then
        value += fine * grit * (if material == Snow then 0.09f else 0.14f)
      var mode = 0
      while mode < modal.length do
        val modeDecay = if action == Break && (material == Glass || material == Metal) then 0.10f
        else p.decay * 0.7f
        val envelope = pulse(
          t,
          if mode == 0 then 0f else starts((mode + variant) % starts.length),
          0.001f,
          modeDecay / (1 + mode * 0.27f)
        )
        value += sin(
          t * 2 * Pi * p.body * modal(mode) * pitch
        ).toFloat * envelope * p.resonance / (1 + mode * 0.8f)
        mode += 1
      action match
        case Splash =>
          val spray = pulse(t, 0, 0.004f, 0.09f) + pulse(t, 0.045f, 0.018f, 0.14f) * 0.6f
          val bubbles = sin(2 * Pi * (260f * t - 200f * t * t)).toFloat * pulse(
            t,
            0.055f,
            0.006f,
            0.045f
          ) * 0.13f
          value = filtered * spray * 0.8f + fine * spray * 0.09f + lowBody * spray * 2 + bubbles
        case Thunder =>
          val rumble = pulse(t, 0, 0.025f, 0.65f) + pulse(t, 0.20f, 0.06f, 0.5f) * 0.7f + pulse(
            t,
            0.55f,
            0.12f,
            0.35f
          ) * 0.5f
          val crack = pulse(t, 0.025f, 0.001f, 0.018f)
          value = (filtered * 0.9f + lowBody * 1.6f) * rumble + white * crack * 0.09f
        case Hurt =>
          value = (filtered * 0.55f + lowBody * 3f) * pulse(t, 0, 0.002f, 0.035f) + fine * pulse(
            t,
            0.025f,
            0.008f,
            0.035f
          ) * 0.03f
        case Ui   => value = (filtered * 0.7f + lowBody * 0.4f) * pulse(t, 0, 0.0006f, 0.007f)
        case Land => value += lowBody * 2f * pulse(t, 0.008f, 0.003f, 0.045f)
        case _    => ()
      // DC blocking and short fades prevent boundary clicks, including cached tails.
      val highPassed = value - previousInput + 0.995f * dc
      previousInput = value; dc = highPassed
      val fade = (t / 0.0018f).min(1f) * ((duration - t) / 0.012f).min(1f)
      val sample = highPassed * fade
      result(i) = sample
      peak = peak.max(abs(sample)); sumSquares += sample.toDouble * sample
      i += 1
    val target = action match
      case Ui      => 0.024f
      case Step    => 0.070f
      case Place   => 0.075f
      case Break   => 0.105f
      case Thunder => 0.11f
      case _       => 0.095f
    val rms = sqrt(sumSquares / result.length).toFloat.max(0.00001f)
    val gain = (target / rms).min(0.76f / peak.max(0.00001f))
    i = 0
    while i < result.length do
      result(i) *= gain; i += 1
    result(0) = 0f; result(result.length - 1) = 0f
    result
