package blockbox

import org.lwjgl.glfw.GLFW.*
import javax.sound.sampled.AudioFormat
import javax.sound.sampled.AudioSystem
import scala.math.*

object BlockboxPlatform:
  def libdecorCheck(): Boolean =
    try
      val paths = Seq(
        "/usr/lib64/libdecor-0.so",
        "/usr/lib/libdecor-0.so",
        "/usr/lib/x86_64-linux-gnu/libdecor-0.so",
        "/usr/local/lib/libdecor-0.so",
        "/usr/lib/aarch64-linux-gnu/libdecor-0.so"
      )
      paths.exists(p => new java.io.File(p).exists) ||
      sys.env.contains("LD_LIBRARY_PATH") && sys.env("LD_LIBRARY_PATH").split(':').exists { dir =>
        new java.io.File(dir, "libdecor-0.so").exists
      }
    catch case _: Exception => false

  def configureGlfwPlatform(): Unit =
    val xdg = sys.env.getOrElse("XDG_SESSION_TYPE", "unknown")
    val hasWaylandDisplay = sys.env.contains("WAYLAND_DISPLAY")
    val hasDisplay = sys.env.contains("DISPLAY")
    sys.env.get("GLFW_PLATFORM").map(_.toLowerCase) match
      case Some("x11") =>
        System.err.println(
          s"Blockbox: forced X11 platform on $xdg session (DISPLAY=${sys.env.getOrElse("DISPLAY", "unset")})"
        )
        glfwInitHint(GLFW_PLATFORM, GLFW_PLATFORM_X11)
      case Some("wayland") =>
        System.err.println(s"Blockbox: forced Wayland platform on $xdg session")
        glfwInitHint(GLFW_PLATFORM, GLFW_PLATFORM_WAYLAND)
        if !libdecorCheck() then
          System.err.println(
            "Blockbox: libdecor not detected. Install libdecor for ghost-free Wayland rendering (apt install libdecor / dnf install libdecor)."
          )
      case Some("null") => glfwInitHint(GLFW_PLATFORM, GLFW_PLATFORM_NULL)
      case Some(other)  =>
        System.err.println(s"Blockbox: unknown GLFW_PLATFORM '$other', using GLFW default")
      case None =>
        System.err.println(
          s"Blockbox: no GLFW_PLATFORM set, GLFW will auto-detect on $xdg session (WAYLAND_DISPLAY=$hasWaylandDisplay DISPLAY=$hasDisplay)"
        )

private object BlockboxProceduralAudioEngine:
  private final case class Request(
      samples: () => Array[Float],
      gain: Float,
      pitch: Float,
      pan: Float,
      createdAt: Long = System.nanoTime()
  )
  private final case class Playback(
      samples: Array[Float],
      gain: Float,
      pitch: Float,
      pan: Float,
      createdAt: Long
  )
  private final class Voice(val clip: Playback):
    var cursor = 0f
    val left = clip.gain * sqrt((1f - clip.pan) * 0.5f).toFloat
    val right = clip.gain * sqrt((1f + clip.pan) * 0.5f).toFloat
  private val rate = BlockboxSounds.sampleRate.toFloat
  private val requests = new java.util.concurrent.ArrayBlockingQueue[Request](64)
  private val pending = new java.util.concurrent.ArrayBlockingQueue[Playback](64)
  private val mixerStarted = java.util.concurrent.atomic.AtomicBoolean(false)
  private val synthStarted = java.util.concurrent.atomic.AtomicBoolean(false)
  private val retryAfter = java.util.concurrent.atomic.AtomicLong(0)
  @volatile private var running = true
  @volatile private var synthThread: Thread = null
  @volatile private var mixerThread: Thread = null
  @volatile private var currentSink: AudioSink = null
  @volatile private var rainSamples: Array[Float] = Array.emptyFloatArray
  @volatile private var rainTarget = 0f
  private val rainRequested = java.util.concurrent.atomic.AtomicBoolean(false)
  @volatile private var furnaceSamples: Array[Float] = Array.emptyFloatArray
  @volatile private var furnaceTarget = 0f
  @volatile private var furnacePan = 0f
  private val furnaceRequested = java.util.concurrent.atomic.AtomicBoolean(false)
  private val prepared = java.util.concurrent.atomic.AtomicBoolean(false)

  def prepare(): Unit =
    if prepared.compareAndSet(false, true) then
      enqueue(Request(() => { RecordedAudio.preload(); Array.emptyFloatArray }, 0f, 1f, 0f))

  private trait AudioSink:
    def backendName: String
    def write(bytes: Array[Byte], length: Int): Unit
    def close(): Unit

  private final class JavaSoundSink(line: javax.sound.sampled.SourceDataLine) extends AudioSink:
    def backendName: String = "Java Sound"
    def write(bytes: Array[Byte], length: Int): Unit =
      var offset = 0
      while offset < length do
        val written = line.write(bytes, offset, length - offset)
        if written <= 0 then throw new java.io.IOException("Java Sound stopped accepting audio")
        offset += written
    def close(): Unit =
      try
        line.stop(); line.flush()
      catch case _: Throwable => ()
      try line.close()
      catch case _: Throwable => ()

  private final class ProcessSink(process: Process, val backendName: String) extends AudioSink:
    private val out = process.getOutputStream
    def write(bytes: Array[Byte], length: Int): Unit =
      if !process.isAlive then
        throw new java.io.IOException(
          s"$backendName audio process exited with ${process.exitValue()}"
        )
      out.write(bytes, 0, length)
      // Process.getOutputStream is buffered. Flush each audio quantum instead of
      // waiting for an 8 KiB pipe buffer (46 ms of stereo PCM) to fill.
      out.flush()
    def close(): Unit =
      try process.destroy()
      catch case _: Throwable => ()
      try out.close()
      catch case _: Throwable => ()

  private def executableOnPath(name: String): Option[String] =
    val path = sys.env.getOrElse("PATH", "")
    path
      .split(java.util.regex.Pattern.quote(java.io.File.pathSeparator))
      .iterator
      .filter(_.nonEmpty)
      .map(dir => new java.io.File(dir, name))
      .find(f => f.isFile && f.canExecute)
      .map(_.getAbsolutePath)

  private def processSink(command: Seq[String], name: String): Option[AudioSink] =
    try
      val pb = new ProcessBuilder(command*)
      pb.redirectError(ProcessBuilder.Redirect.INHERIT)
      val process = pb.start()
      // Argument/connection failures usually exit immediately. Probe once when the
      // backend opens rather than silently dropping the first several footsteps.
      Thread.sleep(12L)
      if process.isAlive then Some(new ProcessSink(process, name))
      else None
    catch case _: Throwable => None

  private def openAudioSink(format: AudioFormat): AudioSink =
    var failures = List.empty[String]

    // Prefer PipeWire on modern desktops. Java Sound can hand back a raw ALSA device
    // (or nothing) while the user's real output route lives in the PipeWire session.
    // The binaries expect RAW little-endian s16 PCM, so --raw is mandatory: without it
    // they try to parse media headers from the sample stream and exit immediately.
    val pipewireBin = executableOnPath("pw-cat").orElse(executableOnPath("pw-play"))
    if pipewireBin.nonEmpty then
      val exe = pipewireBin.get
      val isPwCat = exe.endsWith("pw-cat")
      val name = if isPwCat then "PipeWire pw-cat" else "PipeWire pw-play"
      val base = Seq(
        exe,
        "--playback",
        "--raw",
        "--rate=44100",
        "--channels=2",
        "--channel-map=FL,FR",
        "--latency=16ms",
        "--format=s16",
        "-"
      )
      processSink(base, name) match
        case Some(sink) => return sink
        case None       => failures ::= name + " failed to start"

    try
      val line = AudioSystem.getSourceDataLine(format)
      line.open(format, 4096)
      line.start()
      return new JavaSoundSink(line)
    catch case e: Throwable => failures ::= Option(e.getMessage).getOrElse(e.getClass.getSimpleName)

    val alsa = executableOnPath("aplay").flatMap { exe =>
      processSink(
        Seq(exe, "-q", "-t", "raw", "-f", "S16_LE", "-r", "44100", "-c", "2"),
        "ALSA aplay"
      )
    }
    alsa match
      case Some(sink) => return sink
      case None       => failures ::= "aplay unavailable"

    throw new IllegalStateException(
      s"no usable audio output backend (${failures.reverse.mkString("; ")})"
    )

  def play(
      action: BlockboxSounds.Action,
      material: BlockboxSounds.Material,
      variant: Int,
      gain: Float,
      pitch: Float,
      pan: Float
  ): Unit =
    enqueue(
      Request(
        () =>
          RecordedAudio
            .effect(action, material, variant)
            .getOrElse(BlockboxSounds.clip(action, material, variant)),
        gain.max(0f).min(2f),
        pitch.max(0.7f).min(1.4f),
        pan.max(-1f).min(1f)
      )
    )

  def rain(intensity: Float): Unit =
    rainTarget = intensity.max(0f).min(1f) * 0.72f
    if rainTarget > 0.001f && rainRequested.compareAndSet(false, true) then
      enqueue(
        Request(
          () =>
            rainSamples = RecordedAudio
              .load("weather/rain-loop.ogg", loop = true)
              .getOrElse(Array.emptyFloatArray)
            Array.emptyFloatArray
          ,
          0f,
          1f,
          0f
        )
      )

  def recording(name: String, gain: Float, pan: Float): Unit =
    enqueue(
      Request(
        () => RecordedAudio.load(name).getOrElse(Array.emptyFloatArray),
        gain.max(0f).min(1.5f),
        1f,
        pan.max(-1f).min(1f)
      )
    )

  def furnace(intensity: Float, pan: Float): Unit =
    furnaceTarget = intensity.max(0f).min(1f) * 0.28f
    furnacePan = pan.max(-1f).min(1f)
    if furnaceTarget > 0.001f && furnaceRequested.compareAndSet(false, true) then
      enqueue(
        Request(
          () =>
            furnaceSamples = RecordedAudio
              .load("foley/furnace_loop.ogg", loop = true)
              .getOrElse(Array.emptyFloatArray)
            Array.emptyFloatArray
          ,
          0f,
          1f,
          0f
        )
      )

  def submit(seconds: Float)(sample: (Int, Float) => Float): Unit =
    enqueue(
      Request(
        () =>
          val count = (rate * seconds.max(0.005f).min(2f)).toInt
          val values = new Array[Float](count)
          var i = 0
          while i < count do
            val value = sample(i, rate)
            val fade = (i.toFloat / 80).min(1f) * ((count - 1 - i).toFloat / 160).min(1f)
            values(i) = (if value.isFinite then value.max(-1f).min(1f) * fade else 0f)
            i += 1
          values
        ,
        1f,
        1f,
        0f
      )
    )

  private def enqueue(request: Request): Unit =
    running = true
    if requests.offer(request) then
      if synthStarted.compareAndSet(false, true) then
        synthThread = new Thread(() => synthesisLoop(), "blockbox-audio-synth")
        synthThread.setDaemon(true); synthThread.start()
      if System.nanoTime() >= retryAfter.get() && mixerStarted.compareAndSet(false, true) then
        mixerThread = new Thread(() => audioLoop(), "blockbox-audio-mixer")
        mixerThread.setDaemon(true); mixerThread.start()

  private def synthesisLoop(): Unit =
    try
      while running do
        val request = requests.take()
        if request.gain == 0f || System.nanoTime() - request.createdAt < 150_000_000L then
          val samples = request.samples()
          if samples.nonEmpty && request.gain > 0f then
            pending.offer(
              Playback(samples, request.gain, request.pitch, request.pan, request.createdAt)
            )
    catch
      case _: InterruptedException => Thread.currentThread().interrupt()
      case e: Exception            => System.err.println("Sound synthesis failed: " + e.getMessage)
    finally synthStarted.set(false)

  private def audioLoop(): Unit =
    var sink: AudioSink = null
    try
      sink = openAudioSink(new AudioFormat(rate, 16, 2, true, false))
      currentSink = sink
      System.err.println(s"Blockbox audio: ${sink.backendName}, stereo 44.1 kHz")
      val active = scala.collection.mutable.ArrayBuffer.empty[Voice]
      val frames = 256
      val bytes = new Array[Byte](frames * 4)
      val frameNanos = (frames * 1_000_000_000.0 / rate).toLong
      var deadline = System.nanoTime()
      var rainCursor = 0
      var rainVolume = 0f
      var furnaceCursor = 0
      var furnaceVolume = 0f
      var firePan = 0f
      while running do
        var collecting = active.length < 32
        while collecting do
          val next = pending.poll()
          if next == null then collecting = false
          else if System.nanoTime() - next.createdAt < 150_000_000L then
            active += Voice(next)
            if active.length >= 32 then collecting = false
        var frame = 0
        while frame < frames do
          var left = 0f; var right = 0f; var v = 0
          rainVolume += (rainTarget - rainVolume) * 0.00012f
          val ambience = rainSamples
          if ambience.nonEmpty then
            if rainCursor >= ambience.length then rainCursor = 0
            left += ambience(rainCursor) * rainVolume
            right += ambience((rainCursor + 397) % ambience.length) * rainVolume
            rainCursor += 1
          furnaceVolume += (furnaceTarget - furnaceVolume) * 0.00018f
          firePan += (furnacePan - firePan) * 0.00018f
          val fire = furnaceSamples
          if fire.nonEmpty then
            if furnaceCursor >= fire.length then furnaceCursor = 0
            val sample = fire(furnaceCursor) * furnaceVolume
            left += sample * sqrt((1f - firePan) * 0.5f).toFloat
            right += sample * sqrt((1f + firePan) * 0.5f).toFloat
            furnaceCursor += 1
          while v < active.length do
            val voice = active(v); val clip = voice.clip
            val index = voice.cursor.toInt
            if index < clip.samples.length - 1 then
              val fraction = voice.cursor - index
              val sample =
                clip.samples(index) + (clip.samples(index + 1) - clip.samples(index)) * fraction
              left += sample * voice.left; right += sample * voice.right
              voice.cursor += clip.pitch
            else voice.cursor = clip.samples.length
            v += 1
          def pcm(value: Float): Int = (tanh(value.toDouble) * 0.95 * Short.MaxValue).toInt
          val l = pcm(left); val r = pcm(right); val offset = frame * 4
          bytes(offset) = l.toByte; bytes(offset + 1) = (l >>> 8).toByte
          bytes(offset + 2) = r.toByte; bytes(offset + 3) = (r >>> 8).toByte
          frame += 1
        var v = active.length - 1
        while v >= 0 do
          if active(v).cursor >= active(v).clip.samples.length - 1 then active.remove(v)
          v -= 1
        // Clock silence too: a running desktop stream avoids device underruns and
        // the first-impact delay. Pacing prevents filling an OS pipe far in advance.
        sink.write(bytes, bytes.length)
        deadline += frameNanos
        val now = System.nanoTime()
        if deadline > now then java.util.concurrent.locks.LockSupport.parkNanos(deadline - now)
        else if now - deadline > 50_000_000L then deadline = now
    catch
      case _: InterruptedException => Thread.currentThread().interrupt()
      case e: Exception            =>
        if running then System.err.println("Audio output failed; will retry: " + e.getMessage)
        requests.clear(); pending.clear()
        retryAfter.set(System.nanoTime() + 1_000_000_000L)
    finally
      if sink != null then sink.close()
      currentSink = null; mixerStarted.set(false)

  def shutdown(): Unit =
    running = false
    if synthThread != null then synthThread.interrupt()
    if mixerThread != null then mixerThread.interrupt()
    if currentSink != null then currentSink.close()
    requests.clear(); pending.clear()
    rainTarget = 0f; rainRequested.set(false)
    furnaceTarget = 0f; furnaceRequested.set(false)

trait BlockboxAudio:
  protected def prepareAudio(): Unit = BlockboxProceduralAudioEngine.prepare()
  protected def soundEnabled: Boolean
  private val soundSequence = java.util.concurrent.atomic.AtomicInteger(0)
  private var leftFoot = false

  private def effect(
      action: BlockboxSounds.Action,
      block: Block,
      gain: Float = 1f,
      speed: Float = 1f,
      pan: Float = 0f
  ): Unit =
    if soundEnabled then
      val sequence = soundSequence.getAndIncrement()
      val pitch = speed * (0.97f + Math.floorMod(sequence * 13, 11) * 0.006f)
      BlockboxProceduralAudioEngine.play(
        action,
        BlockboxSounds.material(block),
        sequence,
        gain,
        pitch,
        pan
      )

  protected def playPlaceSound(): Unit = effect(BlockboxSounds.Action.Ui, Block.Wood, 0.75f)
  protected def playPlaceSound(block: Block): Unit =
    effect(BlockboxSounds.Action.Place, block, 0.92f)
  protected def playBreakSound(): Unit = playBreakSound(Block.Stone)
  protected def playBreakSound(block: Block): Unit = effect(BlockboxSounds.Action.Break, block)
  protected def playFootstepSound(block: Block, sprinting: Boolean): Unit =
    leftFoot = !leftFoot
    effect(
      BlockboxSounds.Action.Step,
      block,
      if sprinting then 1f else 0.82f,
      if sprinting then 1.08f else 1f,
      if leftFoot then -0.16f else 0.16f
    )
  protected def playLandSound(block: Block, intensity: Float): Unit =
    effect(BlockboxSounds.Action.Land, block, intensity.max(0.3f).min(1.25f), 0.96f)
  protected def playSplashSound(): Unit = effect(BlockboxSounds.Action.Splash, Block.Water)
  protected def playThunderSound(intensity: Float, pan: Float = 0f): Unit =
    effect(BlockboxSounds.Action.Thunder, Block.Stone, intensity.max(0.02f).min(1.2f), 0.92f, pan)
  protected def updateRainAmbience(intensity: Float): Unit =
    BlockboxProceduralAudioEngine.rain(if soundEnabled then intensity else 0f)
  protected def updateFurnaceAmbience(intensity: Float, pan: Float = 0f): Unit =
    BlockboxProceduralAudioEngine.furnace(if soundEnabled then intensity else 0f, pan)
  protected def playFurnaceIgnition(gain: Float, pan: Float): Unit =
    if soundEnabled && gain > 0.005f then
      BlockboxProceduralAudioEngine.recording("foley/furnace_ignite.ogg", gain * 0.32f, pan)
  protected def playHurtSound(): Unit = effect(BlockboxSounds.Action.Hurt, Block.Wood, 0.90f)
  protected def playMonsterSound(event: Int, variant: Int, gain: Float, pan: Float): Unit =
    if soundEnabled && gain > 0.005f then
      val bank = Vector("idle", "hurt", "attack", "death")(event.max(0).min(3))
      val count = if event <= 1 then 3 else 2
      BlockboxProceduralAudioEngine.recording(
        s"zombie/${bank}_${Math.floorMod(variant, count)}.ogg",
        gain,
        pan
      )
  protected def shutdownAudio(): Unit = BlockboxProceduralAudioEngine.shutdown()

  protected def playTone(freq: Float, seconds: Float, volume: Float, decay: Float): Unit =
    if soundEnabled then
      BlockboxProceduralAudioEngine.submit(seconds) { (i, rate) =>
        val t = i.toFloat / rate
        sin(2 * Pi * freq * t).toFloat * volume * pow(
          (1f - t / seconds).max(0f),
          decay.toDouble
        ).toFloat
      }
  protected def playNoise(seconds: Float, volume: Float): Unit =
    if soundEnabled then
      val noise = new scala.util.Random()
      BlockboxProceduralAudioEngine.submit(seconds) { (i, rate) =>
        (noise.nextFloat() * 2 - 1) * volume * (1f - i / (rate * seconds)).max(0f)
      }
