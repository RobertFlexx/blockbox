package blockbox

import scala.collection.mutable

object WeatherPolishTest:
  import InteractionSystemsTest.{call, field, set}

  def run(): Unit =
    fog(); walking(); menus(); mutedFootsteps()
    println(
      "[weather-polish] valid storm fog, synchronized foot contacts, menu pause policy and passive physics passed"
    )

  private def fog(): Unit =
    for
      distance <- Seq(32f, 64f, 128f, 1024f); density <- Seq(0.6f, 1f, 1.6f, 3f)
      rain <- 0 to 20
    do
      val range = WorldFog.range(distance, density, rain / 20f, false)
      assert(range.start >= 0 && range.end > range.start)
      assert(range.end - range.start >= distance * 0.159f, "a storm keeps a gradual fog transition")
      val nearVisibility = ((range.end - 2) / (range.end - range.start)).max(0).min(1)
      assert(nearVisibility == 1f, "nearby skin and item colors cannot collapse to the fog color")
    assert(WorldFog.range(32, 0.6f, 1, true) == WorldFog.Range(0.6f, 25f))

  private def walking(): Unit =
    for step <- Seq(1.3f, 1.58f, 1.9f) do
      val results = Seq(24, 60, 144).map { fps =>
        val gait = WalkingGait()
        val contacts = (0 until fps * 4).count(_ => gait.advance(10f / (fps * 4), step))
        (contacts, gait.phase)
      }
      assert(
        results.map(_._1).distinct.size == 1,
        "contact timing depends on distance, not frame rate"
      )
      assert(results.map(_._2).max - results.map(_._2).min < 0.00001f)
    val gait = WalkingGait()
    assert(!gait.advance(0, 1.58f))
    assert(
      !gait.advance(0.3f, 1.58f) && gait.advance(0.3f, 1.58f),
      "the first footfall needs only a short weight shift"
    )
    assert(
      math.abs(math.sin(gait.phase)) < 0.15,
      "sound coincides with the bob's foot-contact trough"
    )
    val stoppedPhase = gait.phase
    assert(
      !gait.advance(0, 1.58f),
      "stopping, opening a menu and standing still cannot emit footsteps"
    )
    assert(gait.phase == stoppedPhase, "brief pauses preserve animation phase")
    gait.reset(); assert(gait.phase == 0)

  private def menus(): Unit =
    val game = Blockbox(); set(game, "soundEnabled", false); set(game, "activeWorldSession", true)
    set(game, "worldClock", 150.0); set(game, "furnaceProgress", 1.25f)
    val screens = Seq(
      Screen.Paused,
      Screen.Inventory,
      Screen.Catalog,
      Screen.FurnaceUI,
      Screen.Journal,
      Screen.Settings,
      Screen.Resources
    )
    set(game, "settingsReturnTo", Screen.Paused); set(game, "resourcesReturnTo", Screen.Settings)
    for screen <- screens do
      set(game, "screen", screen)
      assert(call(game, "worldSimulationRunning") == false)
      call(game, "updateWorldSimulation", Float.box(0.05f), Double.box(20.0))
      assert(
        field[Double](game, "worldClock") == 150.0 && field[Float](
          game,
          "furnaceProgress"
        ) == 1.25f,
        "singleplayer menus freeze the complete world, including the furnace"
      )
      set(game, "multiplayerMode", true)
      assert(
        call(game, "worldSimulationRunning") == true,
        "multiplayer inventory and pause menus are local overlays"
      )
      set(game, "multiplayerMode", false)
    set(game, "multiplayerMode", true); set(game, "screen", Screen.MainMenu)
    assert(call(game, "worldSimulationRunning") == false)
    val data = new Array[Byte](16 * 16 * Terrain.worldHeight)
    for x <- 0 until 16; z <- 0 until 16 do data((39 * 16 + z) * 16 + x) = Block.Stone.id
    val chunk = Chunk(0, 0, null, TerrainGenerator(42), data)
    field[mutable.AnyRefMap[(Int, Int), Chunk]](game, "chunks")((0, 0)) = chunk
    for mode <- GameMode.values do
      set(game, "gameMode", mode); set(game, "flyEnabled", false)
      set(game, "camera", Vec3(8.5f, 45, 8.5f)); set(game, "velocity", Vec3(0, 2f, 0))
      set(game, "onGround", false); set(game, "fallPeakY", 45f)
      for _ <- 0 until 150 do call(game, "updatePassivePlayerPhysics", Float.box(0.02f))
      val pos = field[Vec3](game, "camera")
      assert(
        field[Boolean](game, "onGround") && pos.y >= 41.62f && pos.y < 41.65f,
        s"$mode falls and lands safely while a multiplayer menu is open: $pos"
      )
    set(game, "flyEnabled", true); set(game, "camera", Vec3(8.5f, 50, 8.5f))
    call(game, "updatePassivePlayerPhysics", Float.box(0.1f))
    assert(field[Vec3](game, "camera").y == 50f, "creative flight intentionally hovers")
    set(game, "flyEnabled", false)
    for x <- 7 to 9; z <- 7 to 9; y <- 40 to 43 do chunk.setBlock(x, y, z, Block.Water)
    set(game, "camera", Vec3(8.5f, 42.2f, 8.5f)); set(game, "velocity", Vec3(0, -1, 0))
    for _ <- 0 until 100 do call(game, "updatePassivePlayerPhysics", Float.box(0.025f))
    val floating = field[Vec3](game, "camera")
    assert(
      floating.y > 43 && floating.y < 45,
      s"menu-open players retain water buoyancy: $floating"
    )
    assert(BlockTints.item(Block.GrassTuft, FaceKind.North) == BlockTints.plainsGrass)
    assert(BlockTints.item(Block.Fern, FaceKind.North) == BlockTints.plainsGrass)

  private def mutedFootsteps(): Unit =
    val events = mutable.ArrayBuffer.empty[Int]
    class CapturingAudio extends BlockboxAudio:
      protected def soundEnabled: Boolean = false
      override protected def soundEvent(
          action: BlockboxSounds.Action,
          block: Block,
          variant: Int
      ): Unit =
        assert(action == BlockboxSounds.Action.Step && block == Block.Grass)
        events += variant
      def step(): Unit = playFootstepSound(Block.Grass, false)
    val audio = CapturingAudio()
    for _ <- 0 until 80 do audio.step()
    assert(
      events.toVector == Vector.tabulate(80)(_ % BlockboxSounds.variants),
      "muting local audio still publishes each contact with the same variant used for local playback"
    )
