package blockbox

import org.lwjgl.BufferUtils
import org.lwjgl.opengl.{GL, GL20}
import org.lwjgl.opengl.GL11.*
import org.lwjgl.opengl.GL15.*
import scala.collection.mutable.ArrayBuffer
import scala.math.*

/** Cached, camera-relative cloud geometry. Noise and face extraction run only after crossing a
  * cache region or changing the weather's coverage band.
  */
object BlockboxClouds:
  private val cell = 12f
  private val altitude = 164f
  private var vbo = 0
  private var originX = Int.MinValue
  private var originZ = Int.MinValue
  private var cachedRadius = 0
  private var cachedCover = -1
  private var cachedSeed = 0L
  private val starts = new Array[Int](12)
  private val counts = new Array[Int](12)

  private def noise(x: Int, z: Int, seed: Long): Float =
    var n = x * 374761393 + z * 668265263 + (seed ^ (seed >>> 32)).toInt
    n = (n ^ (n >>> 13)) * 1274126177
    ((n ^ (n >>> 16)) & 0x7fffffff).toFloat / 2147483647f

  private def field(x: Float, z: Float, seed: Long): Float =
    val ix = floor(x).toInt; val iz = floor(z).toInt
    val fx = x - ix; val fz = z - iz
    val u = fx * fx * (3f - 2f * fx); val v = fz * fz * (3f - 2f * fz)
    val a = noise(ix, iz, seed); val b = noise(ix + 1, iz, seed)
    val c = noise(ix, iz + 1, seed); val d = noise(ix + 1, iz + 1, seed)
    (a + (b - a) * u) * (1f - v) + (c + (d - c) * u) * v

  private def rebuild(gx: Int, gz: Int, radius: Int, cover: Int, seed: Long, time: Float): Unit =
    originX = gx; originZ = gz; cachedRadius = radius; cachedCover = cover; cachedSeed = seed
    val span = radius * 2 + 3
    val heights = new Array[Float](span * span)
    val stormy = new Array[Boolean](span * span)
    val (driftX, driftZ) = WeatherField.cloudOffset(time)
    val storms = WeatherField.cells(
      seed,
      time,
      gx * cell + driftX,
      gz * cell + driftZ,
      cover / 5f,
      radius * cell
    )
    var z = 0
    while z < span do
      var x = 0
      while x < span do
        val wx = gx + x - radius - 1; val wz = gz + z - radius - 1
        val broad = field(wx * 0.052f, wz * 0.052f, seed)
        val medium = field(wx * 0.118f + 19.3f, wz * 0.118f - 11.7f, seed)
        val detail = field(wx * 0.235f - 6.8f, wz * 0.235f + 4.1f, seed)
        val shape = broad * 0.46f + medium * 0.39f + detail * 0.15f
        val wet = WeatherField.sample(storms, wx * cell + driftX, wz * cell + driftZ).cloud
        val threshold = 0.60f - wet * 0.28f
        stormy(z * span + x) = wet > 0.32f
        if shape > threshold then
          heights(z * span + x) =
            4f + floor((shape - threshold) * 20f).toFloat.min(8f) * 2f + floor(
              wet * 5f
            ).toFloat * 4f
        x += 1
      z += 1
    val groups = Array.fill(12)(ArrayBuffer.empty[Float])
    def quad(
        group: Int,
        x0: Float,
        y0: Float,
        z0: Float,
        x1: Float,
        y1: Float,
        z1: Float,
        x2: Float,
        y2: Float,
        z2: Float,
        x3: Float,
        y3: Float,
        z3: Float
    ): Unit =
      groups(group) ++= Array(x0, y0, z0, x1, y1, z1, x2, y2, z2, x3, y3, z3)
    z = 1
    while z < span - 1 do
      var x = 1
      while x < span - 1 do
        val h = heights(z * span + x)
        if h > 0f then
          val bank = if stormy(z * span + x) then 6 else 0
          val x0 = (x - radius - 1) * cell; val x1 = x0 + cell
          val z0 = (z - radius - 1) * cell; val z1 = z0 + cell
          quad(0 + bank, x0, h, z0, x1, h, z0, x1, h, z1, x0, h, z1)
          quad(1 + bank, x0, 0, z1, x1, 0, z1, x1, 0, z0, x0, 0, z0)
          val west = heights(z * span + x - 1); val east = heights(z * span + x + 1)
          val north = heights((z - 1) * span + x); val south = heights((z + 1) * span + x)
          if west < h then quad(2 + bank, x0, west, z0, x0, h, z0, x0, h, z1, x0, west, z1)
          if east < h then quad(3 + bank, x1, east, z1, x1, h, z1, x1, h, z0, x1, east, z0)
          if north < h then quad(4 + bank, x1, north, z0, x1, h, z0, x0, h, z0, x0, north, z0)
          if south < h then quad(5 + bank, x0, south, z1, x0, h, z1, x1, h, z1, x1, south, z1)
        x += 1
      z += 1
    val data = BufferUtils.createFloatBuffer(groups.iterator.map(_.length).sum.max(1))
    var i = 0
    while i < 12 do
      starts(i) = data.position() / 3
      counts(i) = groups(i).length / 3
      groups(i).foreach(v => data.put(v))
      i += 1
    data.flip()
    if vbo == 0 then vbo = glGenBuffers()
    glBindBuffer(GL_ARRAY_BUFFER, vbo); glBufferData(GL_ARRAY_BUFFER, data, GL_STATIC_DRAW)
    glBindBuffer(GL_ARRAY_BUFFER, 0)

  def render(
      camera: Vec3,
      time: Float,
      distance: Int,
      daylight: Float,
      warm: Float,
      storm: Float,
      lightning: Float,
      seed: Long,
      palette: BlockboxAtmosphere.Palette
  ): Unit =
    val (driftX, driftZ) = WeatherField.cloudOffset(time)
    val gx = floor((camera.x - driftX) / cell).toInt
    val gz = floor((camera.z - driftZ) / cell).toInt
    val radius = ceil(distance / cell).toInt + 7
    val cover = (storm.max(0f).min(1f) * 5f).toInt
    if vbo == 0 || abs(gx.toLong - originX) > 4 || abs(
        gz.toLong - originZ
      ) > 4 || cachedRadius != radius || cachedCover != cover || cachedSeed != seed
    then rebuild(gx, gz, radius, cover, seed, time)
    BlockboxRender2D.resetGlArraysAndBuffers()
    glDisable(GL_TEXTURE_2D); glDisable(GL_BLEND); glDisable(GL_CULL_FACE)
    glEnable(GL_DEPTH_TEST); glDepthMask(true)
    val ox = originX * cell + driftX - camera.x
    val oz = originZ * cell + driftZ - camera.z
    glPushMatrix()
    glTranslatef(ox, altitude, oz)
    glBindBuffer(GL_ARRAY_BUFFER, vbo)
    glEnableClientState(GL_VERTEX_ARRAY); glVertexPointer(3, GL_FLOAT, 12, 0L)
    val light = 0.30f + daylight * 0.70f
    val r = light + warm * 0.16f + lightning * 0.3f
    val g = light + warm * 0.01f + lightning * 0.32f
    val b = light - warm * 0.14f + lightning * 0.36f
    val previous = glGetInteger(GL20.GL_CURRENT_PROGRAM)
    val shaded = ensureProgram()
    if shaded then
      GL20.glUseProgram(program)
      BlockboxAtmosphere.setPaletteUniforms(program, palette)
      def u3(name: String, x: Float, y: Float, z: Float): Unit =
        val loc = GL20.glGetUniformLocation(program, name)
        if loc >= 0 then GL20.glUniform3f(loc, x, y, z)
      def u1(name: String, v: Float): Unit =
        val loc = GL20.glGetUniformLocation(program, name)
        if loc >= 0 then GL20.glUniform1f(loc, v)
      def u2(name: String, x: Float, y: Float): Unit =
        val loc = GL20.glGetUniformLocation(program, name)
        if loc >= 0 then GL20.glUniform2f(loc, x, y)
      u3("offset", ox, altitude - camera.y, oz)
      u3("lit", r, g, b)
      u3(
        "shadow",
        r * 0.56f + palette.top.x * 0.22f,
        g * 0.56f + palette.top.y * 0.22f,
        b * 0.56f + palette.top.z * 0.22f
      )
      u1("rim", 0.05f + warm * 0.24f)
      u2("fade", distance * 0.35f, distance.toFloat)
    else
      glEnable(GL_FOG); glFogi(GL_FOG_MODE, GL_LINEAR)
      glFogf(GL_FOG_START, distance * 0.66f); glFogf(GL_FOG_END, distance.toFloat)
    val densityLocation = if shaded then GL20.glGetUniformLocation(program, "density") else -1
    var group = 0
    while group < 12 do
      val density = if group >= 6 then 0.42f else 1f
      if shaded then
        if densityLocation >= 0 then GL20.glUniform1f(densityLocation, density)
        (group % 6) match
          case 0 => glNormal3f(0f, 1f, 0f)
          case 1 => glNormal3f(0f, -1f, 0f)
          case 2 => glNormal3f(-1f, 0f, 0f)
          case 3 => glNormal3f(1f, 0f, 0f)
          case 4 => glNormal3f(0f, 0f, -1f)
          case _ => glNormal3f(0f, 0f, 1f)
      else
        val shade = (group % 6) match
          case 0 => 1f
          case 1 => 0.65f
          case 2 => 0.78f
          case 3 => 0.87f
          case _ => 0.81f
        glColor4f(
          (r * shade * density).min(1.1f),
          (g * shade * density).min(1.1f),
          (b * shade * density).min(1.1f),
          1f
        )
      glDrawArrays(GL_QUADS, starts(group), counts(group))
      group += 1
    if shaded then
      GL20.glUseProgram(previous)
      glNormal3f(0f, 0f, 1f)
    glDisableClientState(GL_VERTEX_ARRAY); glBindBuffer(GL_ARRAY_BUFFER, 0)
    glPopMatrix()
    glColor4f(1f, 1f, 1f, 1f)

  private var program = 0
  private var programFailed = false

  private def ensureProgram(): Boolean =
    if program == 0 && !programFailed && GL.getCapabilities.OpenGL20 then
      try program = BlockboxAtmosphere.buildProgram(vertexSource, fragmentSource)
      catch
        case e: Exception =>
          programFailed = true
          System.err.println("Cloud shader unavailable; using fogged flat clouds: " + e.getMessage)
    program != 0

  private val vertexSource = """#version 120
    uniform vec3 offset;
    varying vec3 view;
    varying vec3 normal;
    varying float height;
    void main() {
      gl_Position = gl_ModelViewProjectionMatrix * gl_Vertex;
      view = gl_Vertex.xyz + offset;
      normal = gl_Normal;
      height = gl_Vertex.y;
    }
  """

  // Tops take full sun, undersides sit in sky-tinted shadow, and sides darken toward their
  // base. Sun-facing sides catch a warm rim at dusk, and distance dissolves everything into
  // the exact sky color behind it, so there is no fog-colored band at the horizon.
  private val fragmentSource = """#version 120
    uniform vec3 lit, shadow;
    uniform float density, rim;
    uniform vec2 fade;
    varying vec3 view;
    varying vec3 normal;
    varying float height;
  """ + BlockboxAtmosphere.skyFunction + """
    void main() {
      vec3 n = length(normal) > 0.001 ? normalize(normal) : vec3(0.0, 1.0, 0.0);
      float side = 1.0 - abs(n.y);
      float shade = n.y > 0.5 ? 1.0 : (n.y < -0.5 ? 0.0 : mix(0.30, 0.78, clamp(height / 20.0, 0.0, 1.0)));
      vec3 color = mix(shadow, lit, shade);
      vec3 toward = normalize(vec3(sun.x + 0.000001, 0.0, sun.z + 0.000001));
      color = (color + lit * side * max(dot(n, toward), 0.0) * rim) * density;
      float away = smoothstep(fade.x, fade.y, length(view.xz));
      vec3 skyDir = length(view) > 0.001 ? normalize(view) : vec3(0.0, 1.0, 0.0);
      gl_FragColor = vec4(mix(color, skyColor(skyDir), away), 1.0);
    }
  """

  def destroy(): Unit =
    if vbo != 0 then glDeleteBuffers(vbo)
    vbo = 0
    if program != 0 then GL20.glDeleteProgram(program)
    program = 0; programFailed = false
