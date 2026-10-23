package blockbox

import java.nio.file.{Files, Path}
import java.io.ByteArrayInputStream
import javax.sound.sampled.{AudioFormat, AudioInputStream, AudioFileFormat, AudioSystem}
import scala.collection.mutable.ArrayBuffer
import scala.math.*

object PresentationTest:
  def run(): Unit =
    handAttachments(); atmosphere(); audio(); weatherAndBiomes()

  def weatherAndBiomes(): Unit =
    val storms = WeatherField.cells(8765, 400, 0, 0, 1f)
    assert(
      storms.nonEmpty && storms == WeatherField.cells(8765, 400, 0, 0, 1f),
      "storm cells agree between peers"
    )
    assert(WeatherField.cells(8765, 400, 0, 0, 0).isEmpty)
    val later = WeatherField.cells(8765, 401, 0, 0, 1f).map(c => c.id -> c).toMap
    storms.foreach { cell =>
      later.get(cell.id).foreach { moved =>
        assert(
          abs(moved.x - cell.x - WeatherField.windX) < 0.001f && abs(
            moved.z - cell.z - WeatherField.windZ
          ) < 0.001f,
          "rain fronts advance continuously in the same world-space wind as the clouds"
        )
      }
    }
    val (ax, az) = WeatherField.cloudOffset(100f)
    val (bx, bz) = WeatherField.cloudOffset(100f + 1f / 60f)
    assert(bx > ax && bz > az && bx - ax < 0.02f, "cloud movement is smooth at frame intervals")
    val storm = storms.minBy(c => c.x * c.x + c.z * c.z)
    assert(WeatherField.sample(storms, storm.x, storm.z).rain > 0.9f)
    val transect = (-12 to 12).map(i => WeatherField.sample(storms, storm.x + i * 40, storm.z).rain)
    assert(
      transect.min < 0.05f && transect.max > 0.9f,
      "rain is localized with clear ground outside"
    )
    val edge =
      (0 to 1000).map(i => WeatherField.sample(Seq(storm), storm.x + i * 0.5f, storm.z).rain)
    assert(edge.sliding(2).forall(p => abs(p(0) - p(1)) < 0.01f), "no hard storm boundary")
    val strikes = (0 to 80).flatMap(t => WeatherField.strikes(storms, t.toFloat))
    assert(strikes.nonEmpty && strikes.forall(s => s.age >= 0 && s.age < 7))
    val gen = TerrainGenerator(8675309)
    val required = Set(
      TerrainBiome.RockyCoast,
      TerrainBiome.DuneDesert,
      TerrainBiome.SnowyTaiga,
      TerrainBiome.RedwoodForest,
      TerrainBiome.WillowMarsh,
      TerrainBiome.AutumnGrove,
      TerrainBiome.Badlands
    )
    val found = scala.collection.mutable.Map.empty[TerrainBiome, (Int, Int, Int)]
    for x <- -8192 to 8192 by 128; z <- -8192 to 8192 by 128 do
      val h = gen.heightAt(x, z); val biome = gen.biomeAt(x, z, h)
      if required(biome) && !found.contains(biome) then found(biome) = (x, h, z)
    assert(
      required.subsetOf(found.keySet.toSet),
      s"new biomes reachable from real terrain: ${found.keys.mkString(",")}"
    )
    val (badlandsX, _, badlandsZ) = found(TerrainBiome.Badlands)
    var desertSamples = 0; var cacti = 0
    for dx <- -48 to 48; dz <- -48 to 48 do
      val x = badlandsX + dx; val z = badlandsZ + dz; val h = gen.heightAt(x, z)
      if gen.biomeAt(x, z, h) == TerrainBiome.Badlands then
        val surface = gen.surfaceBlock(x, h, z, h)
        assert(gen.treeAt(x, z, h, surface).isEmpty, "red deserts never spawn tree anchors")
        desertSamples += 1
        if gen.cactusAt(x, z, h, surface).nonEmpty then cacti += 1
    assert(desertSamples > 100 && cacti > 0, "cacti grow naturally on red-desert clay")
    val localBiomes = (for x <- -1024 to 1024 by 64; z <- -1024 to 1024 by 64
    yield gen.biomeAt(x, z, gen.heightAt(x, z))).toSet
    assert(
      localBiomes.size >= 7,
      s"several biome types are reachable within an ordinary exploration trip: $localBiomes"
    )
    biomeEcology(gen, found.toMap)
    sys.env.get("BLOCKBOX_RENDER_ARTIFACTS").foreach { directory =>
      Files.writeString(
        Path.of(directory).resolve("biome-locations.csv"),
        "seed,biome,x,y,z\n" + found.toVector
          .sortBy(_._1.ordinal)
          .map { case (biome, (x, y, z)) => s"8675309,$biome,$x,$y,$z" }
          .mkString("\n") + "\n"
      )
    }
    println(
      "[weather/terrain] localized storms, seven biome regions and biome ecology passed"
    )

  private def biomeEcology(
      gen: TerrainGenerator,
      locations: Map[TerrainBiome, (Int, Int, Int)]
  ): Unit =
    for block <- FloraModel.blocks do
      assert(
        !block.solid && block.cutout && !Block.isItem(block),
        "flora is scenery, not a collision cube or tool"
      )
      assert(Block.fromId(block.id) == block, "new palette ids round trip")
      val mesh = FloraModel.vertices(block)
      val triangles = mesh
        .grouped(3)
        .map(t => t.map(v => (v.x, v.y, v.z)).sortBy(p => (p._1, p._2, p._3)))
        .toVector
      assert(
        triangles.distinct.length == triangles.length,
        "two-sided cutouts have no duplicate coplanar triangles"
      )
      assert(
        mesh.nonEmpty && mesh.length <= 24 && mesh.length % 3 == 0,
        "bounded cached scenery mesh"
      )
      assert(
        mesh.forall(v =>
          v.x >= 0 && v.x <= 1 && v.y >= WaterSurface.fullHeight - 1f && v.y <= 1 && v.z >= 0 && v.z <= 1
        )
      )
      assert(mesh.forall(v => abs(v.nx * v.nx + v.ny * v.ny + v.nz * v.nz - 1) < 0.001f))
      val alpha = for x <- 0 until 16; y <- 0 until 16
      yield BlockboxTextures.pixel(block, FaceKind.North, x, y).get._4
      assert(alpha.contains(0) && alpha.contains(255), "authored cutout art has a real silhouette")
      val floor = if block == Block.LilyPad then Block.Water else Block.Clay
      assert(FloraModel.supported(block, 0, 1, 0, (_, y, _) => if y == 0 then floor else Block.Air))
      assert(!FloraModel.supported(block, 0, 1, 0, (_, _, _) => Block.Air))
      val hit = BlockboxRaycast.raycast(
        Vec3(0.5f, 2f, 0.5f),
        Vec3(0, -1, 0),
        4,
        (x, y, z) => if x == 0 && y == 0 && z == 0 then block else Block.Air
      )
      assert(hit.exists(_.block == (0, 0, 0)), "plants can be selected and harvested")
    var reeds = 0; var lilies = 0; var litter = 0; var wet = 0; var dry = 0; var clay = 0
    for biome <- Vector(TerrainBiome.WillowMarsh, TerrainBiome.AutumnGrove) do
      val (x, _, z) = locations(biome)
      val cx = Math.floorDiv(x, 16); val cz = Math.floorDiv(z, 16)
      for dx <- -3 to 3; dz <- -3 to 3 do
        val blocks = gen.fillChunkBlocks(cx + dx, cz + dz)
        assert(
          java.util.Arrays.equals(blocks, gen.fillChunkBlocks(cx + dx, cz + dz)),
          "ecology is independent of generation order"
        )
        def at(lx: Int, y: Int, lz: Int): Block =
          if y < 0 || y >= Terrain.worldHeight then Block.Air
          else Block.fromId(blocks((y * 16 + lz) * 16 + lx))
        for lx <- 0 until 16; lz <- 0 until 16 do
          val wx = (cx + dx) * 16 + lx; val wz = (cz + dz) * 16 + lz
          val h = gen.heightAt(wx, wz)
          if gen.biomeAt(wx, wz, h) == TerrainBiome.WillowMarsh then
            if at(lx, Terrain.seaLevel, lz) == Block.Water then wet += 1 else dry += 1
            if at(lx, h, lz) == Block.Clay then clay += 1
          for y <- h + 1 to (h + 5).min(Terrain.worldHeight - 1) do
            val b = at(lx, y, lz)
            if FloraModel.contains(b) then
              assert(
                FloraModel.supported(b, lx, y, lz, at),
                "generated flora has the correct real support"
              )
              if b == Block.Reeds then reeds += 1
              else if b == Block.LilyPad then lilies += 1
              else litter += 1
    assert(
      wet > 50 && dry > 50 && clay > 50,
      s"marsh channels, islands and mud banks: water=$wet land=$dry mud=$clay"
    )
    assert(
      reeds > 0 && lilies > 0 && litter > 100,
      s"distinct rooted vegetation: reeds=$reeds lilies=$lilies litter=$litter"
    )

  private def rotate(v: Vec3, a: Vec3): Vec3 =
    val z = toRadians(a.z); val y = toRadians(a.y); val x = toRadians(a.x)
    val rz = Vec3((v.x * cos(z) - v.y * sin(z)).toFloat, (v.x * sin(z) + v.y * cos(z)).toFloat, v.z)
    val ry =
      Vec3((rz.x * cos(y) + rz.z * sin(y)).toFloat, rz.y, (-rz.x * sin(y) + rz.z * cos(y)).toFloat)
    Vec3(ry.x, (ry.y * cos(x) - ry.z * sin(x)).toFloat, (ry.y * sin(x) + ry.z * cos(x)).toFloat)

  def handAttachments(): Unit =
    val zombieSkins = (0 until 64).map(BlockboxSkin.zombiePixels(_))
    assert(
      zombieSkins.map(java.util.Arrays.hashCode(_)).distinct.length == 64,
      "zombie appearances have distinct clothes, wear and skin detail"
    )
    assert(
      java.util.Arrays.equals(BlockboxSkin.zombiePixels(7), BlockboxSkin.zombiePixels(71)),
      "replicated IDs choose from a bounded deterministic skin bank"
    )
    for item <- Seq(Block.IronSword, Block.IronPickaxe, Block.IronShovel, Block.IronAxe) do
      val grip = FirstPersonPose.zombieAttachment(item)
      val shaft = rotate(rotate(Vec3(0.707f, 0.707f, 0), grip.rotation), Vec3(90, 0, 0))
      assert(
        shaft.y > 0.85f && shaft.z < 0.1f,
        "equipped zombie tools rise upright from an extended fist"
      )
    val cube = FirstPersonPose.attachment(Block.Stone)
    val half = cube.size / 2
    val corners = for x <- Seq(-half, half); y <- Seq(-half, half); z <- Seq(-half, half)
    yield rotate(Vec3(x, y, z), cube.rotation) + cube.position
    assert(corners.map(_.z).max < -0.112f, "held cube sits in front of the avatar's fist")
    val torch = FirstPersonPose.attachment(Block.Torch)
    def held(x: Float, y: Float, z: Float): Vec3 =
      rotate(
        Vec3(x - 0.5f, y - FirstPersonPose.torchGripHeight, z - 0.5f) * torch.size,
        torch.rotation
      ) + torch.position
    def insideFist(p: Vec3): Boolean =
      math.abs(p.x) < 0.1125f && math.abs(p.z) < 0.1125f && p.y > -0.36f && p.y < -0.25f
    val (fx, fy, fz) = TorchModel.flameCenter(TorchMount.Floor)
    assert(
      insideFist(held(0.5f, FirstPersonPose.torchGripHeight, 0.5f)),
      "the fist closes around the torch handle"
    )
    assert(!insideFist(held(fx, fy, fz)), "the burning end clears the hand")
    for item <- Seq(Block.IronSword, Block.IronPickaxe, Block.StoneAxe, Block.IronShovel) do
      val attachment = FirstPersonPose.attachment(item)
      val shaft = rotate(rotate(Vec3(0.7f, 0.7f, 0), attachment.rotation), Vec3(12, 0, 0))
      assert(shaft.z < -0.10f, "third-person tools extend forward from the wrist")
      assert(shaft.x > 0.20f, "the tool projects ahead and outside the fist, clearing the torso")
      assert(
        shaft.y > 0.80f,
        "tools rise from the fist in a readable Minecraft-style grip"
      )
      val normal = rotate(rotate(Vec3(0, 0, 1), attachment.rotation), Vec3(12, 0, 0))
      assert(abs(normal.y) < 0.16f, "the tool's broad face stays vertical in the resting hand")
    val pickPose = FirstPersonPose.itemPose(Block.DiamondPickaxe, 0, 0, 1)
    val swordPose = FirstPersonPose.itemPose(Block.DiamondSword, 0, 0, 1)
    assert(swordPose == pickPose, "all tools share the same hand/display basis")
    assert(
      pickPose.position == Vec3(0.56f, -0.52f, -0.72f) && pickPose.attack == Vec3(0, 45, 0),
      "vanilla item arm at rest"
    )
    assert(
      FirstPersonPose.itemPose(Block.Torch, 0, 0, 1).display == FirstPersonPose.generatedDisplay
    )
    assert(FirstPersonPose.itemPose(Block.Stone, 0, 0, 1).display == FirstPersonPose.blockDisplay)
    // At rest the attack transform cancels, so the sprite sits at position + display.
    def onSprite(x: Float, y: Float): Vec3 =
      val d = pickPose.display
      pickPose.position + d.position + rotate(Vec3(x - 0.5f, y - 0.5f, 0) * d.size, d.rotation)
    val handleEnd = onSprite(0.1f, 0.1f); val head = onSprite(0.9f, 0.9f)
    assert(head.y > handleEnd.y + 0.5f, "the tool rises from the bottom of the screen")
    assert(
      head.x / -head.z > handleEnd.x / -handleEnd.z,
      "it leans outward as it rises, as in Minecraft"
    )
    val hand = FirstPersonPose.handPose(0, 0, 1)
    assert(
      hand.position == Vec3(0.64f, -0.6f, -0.72f) && hand.yaw == 45f && hand.roll == 0f,
      "vanilla bare arm at rest"
    )
    val lowered = FirstPersonPose.itemPose(Block.IronPickaxe, 0, 0, 0)
    assert(
      lowered.position.y < pickPose.position.y - 0.5f,
      "equip animation lowers the whole item out of view"
    )
    assert(
      FirstPersonPose.handPose(0, 0, 0).position.y < hand.position.y - 0.5f,
      "and the bare arm"
    )
    for
      block <- Seq(Block.Stone, Block.Torch, Block.IronPickaxe); kick <- Seq(0f, 0.25f, 0.6f, 1f);
      blocked <- Seq(0f, 0.5f, 1f)
    do
      val pose = FirstPersonPose.itemPose(block, kick, blocked, 1f)
      val arm = FirstPersonPose.handPose(kick, blocked, 1f)
      assert(
        pose.position.z < -0.55f && arm.position.z < -0.55f && pose.attack.x.isFinite && arm.yaw.isFinite
      )
    val open = FirstPersonPose.obstruction(Vec3(0.5f, 41.6f, 2.5f), 0, 0, (_, _, _) => Block.Air)
    val wall = FirstPersonPose.obstruction(
      Vec3(0.5f, 41.6f, 2.5f),
      0,
      0,
      (_, _, z) => if z == 1 then Block.Stone else Block.Air
    )
    val side = FirstPersonPose.obstruction(
      Vec3(0.5f, 41.6f, 2.5f),
      0,
      0,
      (x, _, _) => if x == 1 then Block.Stone else Block.Air
    )
    assert(open == 0 && wall > 0.9f && side > 0.9f, "front and side walls retract the hand")
    val free = FirstPersonPose.itemPose(Block.Stone, 0, 0, 1);
    val close = FirstPersonPose.itemPose(Block.Stone, 0, 1, 1)
    assert(
      close.position.z > free.position.z && close.position.y < free.position.y,
      "obstruction pulls the complete rig toward the camera and down"
    )

  def atmosphere(): Unit =
    for time <- Seq(0f, 150f, 580f, 820f, 1070f); storm <- Seq(0f, 0.7f, 1f) do
      val phase = BlockboxSky.dayPhase(time)
      val palette =
        BlockboxAtmosphere.palette(phase, BlockboxSky.daylightFactor(phase), storm, storm, 0)
      val samples =
        (-1000 to 1000).map(i => BlockboxAtmosphere.color(Vec3(0, i / 1000f, -1), palette))
      assert(samples.forall(c => Seq(c.x, c.y, c.z).forall(v => v.isFinite && v >= 0 && v <= 1)))
      val steps = samples.sliding(2).map(pair => (pair(1) - pair(0)).lengthSquared).max
      assert(steps < 0.0001f, "continuous atmosphere has no horizon or ring color jump")
      val fog =
        BlockboxSky.clearColor(false, phase, BlockboxSky.daylightFactor(phase), storm, storm, 0)
      assert(abs(fog._1 - palette.horizon.x) < 0.00001f, "fog and sky use the same horizon palette")

  def audio(): Unit =
    val delayed = Array.fill(4410)(0f) ++ Array.fill(440)(0.25f)
    val trimmed = RecordedAudio.trimLeadingSilence(delayed)
    assert(
      trimmed.indexWhere(_ > 0.1f) <= 88 && trimmed.last == delayed.last,
      "recorded impacts retain a short attack, without silent lead-in"
    )
    val immediate = Array(0f, 0.1f, -0.2f, 0f)
    assert(RecordedAudio.trimLeadingSilence(immediate) eq immediate)
    assert(RecordedAudio.trimLeadingSilence(Array.emptyFloatArray).isEmpty)
    import BlockboxSounds.*
    val metrics = ArrayBuffer("material,action,variant,rms,peak,mean,high_frequency_energy")
    def rms(values: Array[Float]): Float = sqrt(
      values.iterator.map(v => v.toDouble * v).sum / values.length
    ).toFloat
    def treble(values: Array[Float]): Float =
      sqrt(
        (1 until values.length).iterator
          .map(i => pow(values(i) - values(i - 1), 2))
          .sum / values.length
      ).toFloat / rms(values)
    for
      material <- Material.values;
      action <- Seq(Action.Step, Action.Place, Action.Break, Action.Land);
      variant <- 0 until variants
    do
      val values = clip(action, material, variant)
      val peak = values.iterator.map(abs).max;
      val mean = values.iterator.map(_.toDouble).sum / values.length
      assert(
        values.forall(_.isFinite) && peak < 0.77f && rms(values) > 0.008f,
        "sound is audible and leaves mixer headroom"
      )
      assert(
        abs(mean) < 0.007f && values.head == 0 && values.last == 0,
        "sound has no DC offset or boundary click"
      )
      assert(
        clip(action, material, variant + variants) eq values,
        "finite clip variants are reused"
      )
      metrics += s"$material,$action,$variant,${rms(values)},$peak,$mean,${treble(values)}"
    val glass = clip(Action.Break, Material.Glass, 0);
    val dirt = clip(Action.Break, Material.Dirt, 0)
    assert(
      treble(glass) > treble(dirt) * 1.4f,
      "glass fragments and soft earth have distinct spectra"
    )
    assert(
      !java.util.Arrays
        .equals(clip(Action.Step, Material.Stone, 0), clip(Action.Step, Material.Stone, 1)),
      "successive footsteps have distinct transients"
    )
    for action <- Seq(Action.Ui, Action.Hurt, Action.Splash, Action.Thunder) do
      val values = clip(action, Material.Stone, 0)
      assert(
        values.forall(_.isFinite) && values.head == 0 && values.last == 0 && rms(values) > 0.005f
      )
    sys.env.get("BLOCKBOX_RENDER_ARTIFACTS").foreach { directory =>
      val output = Path.of(directory); Files.createDirectories(output)
      Files.writeString(output.resolve("audio-metrics.csv"), metrics.mkString("\n") + "\n")
      val montage = ArrayBuffer.empty[Float]
      val timeline = ArrayBuffer("seconds,material,action")
      for
        material <- Seq(
          Material.Stone,
          Material.Wood,
          Material.Grass,
          Material.Sand,
          Material.Snow,
          Material.Glass
        );
        action <- Seq(Action.Step, Action.Place, Action.Break)
      do
        timeline += f"${montage.length.toDouble / sampleRate}%.3f,$material,$action"
        montage ++= clip(action, material, 0);
        montage ++= Array.fill[Float]((sampleRate * 0.22f).toInt)(0)
      writeWav(output.resolve("audio-materials.wav"), montage.toArray)
      Files.writeString(output.resolve("audio-timeline.csv"), timeline.mkString("\n") + "\n")
    }
    println(
      "[presentation] shared hand attachments, continuous sky and 216 material sound variants passed"
    )

  private[blockbox] def writeWav(path: Path, values: Array[Float]): Unit =
    val bytes = new Array[Byte](values.length * 2)
    var i = 0
    while i < values.length do
      val sample = (values(i) * Short.MaxValue).toInt
      bytes(i * 2) = sample.toByte; bytes(i * 2 + 1) = (sample >>> 8).toByte
      i += 1
    val stream = AudioInputStream(
      ByteArrayInputStream(bytes),
      AudioFormat(BlockboxSounds.sampleRate.toFloat, 16, 1, true, false),
      values.length
    )
    try AudioSystem.write(stream, AudioFileFormat.Type.WAVE, path.toFile)
    finally stream.close()
