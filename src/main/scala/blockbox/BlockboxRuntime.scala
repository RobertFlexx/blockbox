package blockbox

import org.lwjgl.glfw.GLFW.*
import javax.sound.sampled.AudioFormat
import javax.sound.sampled.AudioSystem
import scala.math.*

object BlockboxPlatform:
  def libdecorCheck(): Boolean =
    try
      val paths = Seq(
        "/usr/lib64/libdecor-0.so", "/usr/lib/libdecor-0.so",
        "/usr/lib/x86_64-linux-gnu/libdecor-0.so", "/usr/local/lib/libdecor-0.so",
        "/usr/lib/aarch64-linux-gnu/libdecor-0.so"
      )
      paths.exists(p => java.io.File(p).exists) ||
      sys.env.contains("LD_LIBRARY_PATH") && sys.env("LD_LIBRARY_PATH").split(':').exists { dir =>
        java.io.File(dir, "libdecor-0.so").exists
      }
    catch case _: Exception => false

  def configureGlfwPlatform(): Unit =
    val xdg = sys.env.getOrElse("XDG_SESSION_TYPE", "unknown")
    val hasWaylandDisplay = sys.env.contains("WAYLAND_DISPLAY")
    val hasDisplay = sys.env.contains("DISPLAY")
    sys.env.get("GLFW_PLATFORM").map(_.toLowerCase) match
      case Some("x11") =>
        System.err.println(s"Blockbox: forced X11 platform on $xdg session (DISPLAY=${sys.env.getOrElse("DISPLAY","unset")})")
        glfwInitHint(GLFW_PLATFORM, GLFW_PLATFORM_X11)
      case Some("wayland") =>
        System.err.println(s"Blockbox: forced Wayland platform on $xdg session")
        glfwInitHint(GLFW_PLATFORM, GLFW_PLATFORM_WAYLAND)
        if !libdecorCheck() then System.err.println("Blockbox: libdecor not detected. Install libdecor for ghost-free Wayland rendering (apt install libdecor / dnf install libdecor).")
      case Some("null") => glfwInitHint(GLFW_PLATFORM, GLFW_PLATFORM_NULL)
      case Some(other) => System.err.println(s"Blockbox: unknown GLFW_PLATFORM '$other', using GLFW default")
      case None =>
        System.err.println(s"Blockbox: no GLFW_PLATFORM set, GLFW will auto-detect on $xdg session (WAYLAND_DISPLAY=$hasWaylandDisplay DISPLAY=$hasDisplay)")

trait BlockboxAudio:
  protected def soundEnabled: Boolean

  protected def playPlaceSound(): Unit = playTone(140f, 0.045f, 0.28f, 0.35f)
  protected def playBreakSound(): Unit = playNoise(0.075f, 0.26f)

  protected def playTone(freq: Float, seconds: Float, volume: Float, decay: Float): Unit =
    if soundEnabled then playSound(seconds) { (i, rate) =>
      val t = i.toFloat / rate
      val env = pow((1f - t / seconds).max(0f), decay.toDouble).toFloat
      sin(2.0 * Pi * freq * t).toFloat * volume * env
    }

  protected def playNoise(seconds: Float, volume: Float): Unit =
    if soundEnabled then playSound(seconds) { (i, _) =>
      var n = i * 1103515245 + 12345
      n = (n ^ (n >>> 16)) * 2246822519L.toInt
      val env = (1f - i.toFloat / (44100f * seconds)).max(0f)
      (((n & 0xffff).toFloat / 32768f) - 1f) * volume * env
    }

  private def playSound(seconds: Float)(sample: (Int, Float) => Float): Unit =
    Thread(() =>
      try
        val rate = 44100f
        val frames = (rate * seconds).toInt.max(1)
        val bytes = new Array[Byte](frames * 2)
        var i = 0
        while i < frames do
          val s = (sample(i, rate).max(-1f).min(1f) * Short.MaxValue).toInt
          bytes(i * 2) = (s & 0xff).toByte
          bytes(i * 2 + 1) = ((s >>> 8) & 0xff).toByte
          i += 1
        val format = AudioFormat(rate, 16, 1, true, false)
        val line = AudioSystem.getSourceDataLine(format)
        line.open(format, bytes.length)
        line.start()
        line.write(bytes, 0, bytes.length)
        line.drain()
        line.close()
      catch case _: Throwable => ()
    ).start()
