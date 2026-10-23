package blockbox

import scala.math.*

import java.io.*
import scala.collection.mutable

object SurvivalTest:
  def run(): Unit =
    tools(); breathing(); ores(); zombies(); combat(); furnace(); quickMove(); saves();
    adventures(); movement()
    println(
      "[survival] tool progression, recipes, breathing, ore veins, furnace, shift-click, zombies, PvP and save compatibility passed"
    )

  private def grid(rows: Seq[Seq[Block]]): Array[Block] =
    val result = Array.fill(9)(Block.Air)
    rows.zipWithIndex.foreach((row, y) =>
      row.zipWithIndex.foreach((block, x) => result(y * 3 + x) = block)
    )
    result

  def tools(): Unit =
    import Block.*
    assert(
      Block.Custom00.ordinal == 35 && Block.Custom31.ordinal == 66 && Stick.ordinal == 67,
      "legacy IDs are stable"
    )
    for item <- ToolSystem.materials.flatMap(_._2) do
      assert(
        CustomBlocks.registered(item) && !CustomBlocks.isSlot(item) && Block.isItem(
          item
        ) && !item.solid
      )
      assert(Block.fromId(item.id) == item && Block.find(item.toString).contains(item))
      val mesh = ItemModel.mesh(item)
      assert(mesh.nonEmpty && mesh.length % 4 == 0)
      assert(mesh.map(_.z).min < 0 && mesh.map(_.z).max > 0, "tools have real thickness")
    val plank = BirchPlanks; val a = Air; val s = Stick
    assert(
      InventoryModel
        .craftingResult(grid(Seq(Seq(plank), Seq(plank))))
        .exists(r => r.output == Stick && r.outputCount == 4)
    )
    for (material, items) <- ToolSystem.materials do
      assert(
        InventoryModel
          .craftingResult(grid(Seq(Seq(material, material, material), Seq(a, s, a), Seq(a, s, a))))
          .exists(_.output == items(0))
      )
      assert(
        InventoryModel
          .craftingResult(grid(Seq(Seq(material, material), Seq(s, material), Seq(s, a))))
          .exists(_.output == items(1)),
        "axes also match mirrored recipes"
      )
    assert(
      InventoryModel.craftingResult(grid(Seq(Seq(IronOre, Coal)))).isEmpty,
      "iron requires the furnace"
    )
    assert(
      !InventoryModel
        .craftingResult(grid(Seq(Seq(plank, plank), Seq(plank, plank))))
        .exists(_.output == Furnace)
    )
    assert(!ToolSystem.canHarvest(Stone, Air) && ToolSystem.canHarvest(Stone, WoodenPickaxe))
    assert(
      !ToolSystem.canHarvest(IronOre, WoodenPickaxe) && ToolSystem.canHarvest(IronOre, StonePickaxe)
    )
    assert(
      !ToolSystem.canHarvest(Diamond, StonePickaxe) && ToolSystem.canHarvest(Diamond, IronPickaxe)
    )
    assert(ToolSystem.miningSpeed(Wood, IronAxe) > ToolSystem.miningSpeed(Wood, IronPickaxe) * 3)
    assert(
      ToolSystem.miningSpeed(Sand, DiamondShovel) > ToolSystem.miningSpeed(Sand, DiamondSword) * 3
    )
    assert(ToolSystem.drop(Diamond) == (DiamondGem, 1) && ToolSystem.drop(Coal) == (CoalLump, 1))
    val (r, g, b) = WaterAppearance.tint((1f, 1f, 1f), 3f)
    assert(r < g && g < b && b < 1f, "water absorbs red before green and blue")

  def breathing(): Unit =
    val air = AirSupply()
    assert(air.update(14, true, true) == 0 && air.bubbles == 1)
    assert(air.update(1, true, true) == 0 && air.bubbles == 0)
    assert(air.update(0.9f, true, true) == 0 && air.update(0.1f, true, true) == 2)
    assert(air.update(3, true, true) == 6, "drowning deals two health every second")
    air.update(1, false, true)
    assert(air.remaining == 5 && air.bubbles == 4)
    air.update(0, true, false)
    assert(air.remaining == air.capacity, "creative has unlimited breath")
    for step <- Seq(0.05f, 0.1f, 0.25f) do
      air.reset()
      val damage = (0 until math.round(18f / step)).map(_ => air.update(step, true, true)).sum
      assert(damage >= 4 && damage <= 6, "breathing is stable across frame rates")

  def ores(): Unit =
    val counts = mutable.Map.empty[Block, Int].withDefaultValue(0)
    var neighbors = 0; var samples = 0
    for x <- -64 until 64; z <- -64 until 64; y <- 5 until 64 do
      OreDistribution.at(8675309L, x, y, z, 90).foreach { ore =>
        counts(ore) += 1; samples += 1
        if OreDistribution
            .at(8675309L, x + 1, y, z, 90)
            .contains(ore) || OreDistribution.at(8675309L, x, y, z + 1, 90).contains(ore)
        then neighbors += 1
        if ore == Block.Diamond then assert(y <= 20)
        if ore == Block.GoldOre then assert(y <= 34)
      }
    assert(
      counts(Block.Coal) > counts(Block.IronOre) && counts(Block.IronOre) > counts(
        Block.GoldOre
      ) && counts(Block.GoldOre) > counts(Block.Diamond),
      counts.toString
    )
    assert(
      counts(Block.Diamond) > 0 && counts(Block.Diamond) < 1200,
      s"diamonds are discoverable but rare: $counts"
    )
    assert(neighbors > samples * 0.65f, "ores occur in mineable clusters")
    for x <- -17 to 17; y <- 0 to 4 do assert(OreDistribution.at(0, x, y, -16, 80).isEmpty)

  def zombies(): Unit =
    def floor(x: Int, y: Int, z: Int): Block = if y <= 0 then Block.Stone else Block.Air
    assert(ZombieSystem.canSpawn(Vec3(0.5f, 1, 0.5f), 0.18f, 15, 0, floor))
    assert(!ZombieSystem.canSpawn(Vec3(0.5f, 1, 0.5f), 1, 15, 0, floor))
    assert(
      ZombieSystem.canSpawn(Vec3(0.5f, 1, 0.5f), 1, 0, 0, floor),
      "caves can spawn zombies in daylight"
    )
    assert(
      !ZombieSystem.canSpawn(Vec3(0.5f, 1, 0.5f), 0.18f, 0, 12, floor),
      "torches protect a room"
    )
    assert(
      !ZombieSystem.canSpawn(Vec3(0.5f, 1, 0.5f), 0.18f, 0, 1, floor),
      "even weak block light prevents spawning"
    )
    val origin = ZombieSystem.Target("Origin", Vec3(0, 2.62f, 0))
    assert(!ZombieSystem.inSpawnRange(Vec3(0, 1, 10), Seq(origin)))
    assert(ZombieSystem.inSpawnRange(Vec3(0, 1, 40), Seq(origin)))
    assert(!ZombieSystem.inSpawnRange(Vec3(0, 1, 90), Seq(origin)))
    assert(
      !ZombieSystem.inSpawnRange(Vec3(0, 1, 40), Seq(origin, origin.copy(eye = Vec3(0, 2.62f, 41))))
    )
    assert(
      ZombieSystem.canSpawn(
        Vec3(0.5f, 1, 0.5f),
        0.18f,
        0,
        0,
        (x, y, z) => if y == 1 then Block.LeafLitter else floor(x, y, z)
      ),
      "forest floor cover does not suppress night spawning"
    )
    val mob = ZombieSystem.Zombie(1, Vec3(0.5f, 1, 0.5f))
    val player = ZombieSystem.Target("Player", Vec3(0.5f, 2.62f, -9))
    for _ <- 0 until 50 do ZombieSystem.update(mob, 0.05f, Seq(player), false, false, floor)
    assert(
      mob.pos.z < -2 && mob.pos.y >= 0.99f && mob.health == 20,
      "zombies walk toward the player and stay above the floor"
    )
    for _ <- 0 until 20 do ZombieSystem.update(mob, 0.05f, Seq.empty, true, false, floor)
    assert(mob.health < 18 && mob.burning > 0, "direct sun ignites zombies")
    ZombieSystem.update(mob, 0.05f, Seq.empty, true, true, floor)
    assert(mob.burning == 0, "rain extinguishes daylight fire")
    val snapshot = ZombieSystem.parseSnapshot(ZombieSystem.snapshot(Seq(mob)))
    assert(snapshot.head._1 == mob.id && snapshot.head._2 == mob.pos)
    assert(ZombieSystem.parseSnapshot("MOBS").isEmpty)
    assert(
      ZombieSystem.rayDistance(Vec3(mob.pos.x, 2, mob.pos.z + 3), Vec3(0, 0, -1), mob, 4).nonEmpty
    )
    val wall = (x: Int, y: Int, z: Int) => if z == -2 then Block.Stone else floor(x, y, z)
    assert(!ZombieSystem.visible(Vec3(0, 2, 0), Vec3(0, 2, -4), wall))
    val hidden = ZombieSystem.Target("Hidden", Vec3(0.5f, 2.62f, -4))
    val unaware = ZombieSystem.Zombie(60, Vec3(0.5f, 1, 0.5f)); unaware.roamTimer = 100f
    for _ <- 0 until 20 do ZombieSystem.update(unaware, 0.05f, Seq(hidden), false, false, wall)
    assert(
      unaware.memory == 0 && unaware.lastKnown == Vec3(0.5f, 1, 0.5f),
      "nearby players behind walls remain undetected"
    )
    val distant = ZombieSystem.Zombie(61, Vec3(0.5f, 1, 0.5f)); distant.roamTimer = 100f
    ZombieSystem.update(
      distant,
      0.05f,
      Seq(hidden.copy(eye = Vec3(0.5f, 2.62f, -35))),
      false,
      false,
      floor
    )
    assert(distant.memory == 0, "clear sight beyond the detection radius cannot acquire a player")
    val tracker = ZombieSystem.Zombie(62, Vec3(0.5f, 1, 0.5f)); tracker.roamTimer = 100f
    ZombieSystem.update(tracker, 0.05f, Seq(hidden), false, false, floor)
    val seen = tracker.lastKnown
    for _ <- 0 until 20 do
      ZombieSystem.update(
        tracker,
        0.05f,
        Seq(hidden.copy(eye = Vec3(9, 2.62f, -8))),
        false,
        false,
        wall
      )
    assert(
      tracker.memory > 0 && tracker.lastKnown == seen,
      "pursuit uses only the last visible position, never hidden live coordinates"
    )
    for _ <- 0 until 130 do ZombieSystem.update(tracker, 0.05f, Seq(hidden), false, false, wall)
    assert(tracker.memory == 0, "unrefreshed visual memory expires")
    val barricade = (x: Int, y: Int, z: Int) =>
      if z == -3 && math.abs(x) <= 2 && y >= 1 && y <= 3 then Block.Stone else floor(x, y, z)
    val hunter = ZombieSystem.Zombie(3, Vec3(0.5f, 1, 0.5f))
    val quarry = ZombieSystem.Target("Quarry", Vec3(0.5f, 2.62f, -7))
    ZombieSystem.update(hunter, 0.05f, Seq(quarry), false, false, floor)
    for _ <- 0 until 340 do ZombieSystem.update(hunter, 0.05f, Seq(quarry), false, false, barricade)
    assert(
      hunter.pos.z < -5.5f && !ZombieSystem.collides(hunter.pos, barricade),
      s"zombies navigate around a tall barricade: ${hunter.pos}"
    )
    // A moving target changes heading without instantaneous grid-aligned turns.
    val smooth = ZombieSystem.Zombie(40, Vec3(0.5f, 1, 0.5f))
    var previousVelocity = Vec3(0, 0, 0)
    for i <- 0 until 180 do
      val before = smooth.pos; val yaw = smooth.yaw
      val target = ZombieSystem.Target("Moving", Vec3(5f + sin(i * 0.015).toFloat * 2f, 2.62f, -8))
      ZombieSystem.update(smooth, 1f / 60f, Seq(target), false, false, floor)
      val velocity = (smooth.pos - before) * 60f
      assert(
        sqrt((velocity - previousVelocity).lengthSquared) < 0.4f,
        "steering accelerates continuously"
      )
      assert(
        abs(smooth.yaw - yaw) <= 3.01f,
        "heading changes are bounded independently of waypoints"
      )
      previousVelocity = velocity
    // A deep lake has no nearby solid support. Zombies must rise, settle with
    // their heads above water, and continue pursuing across its surface.
    def lake(x: Int, y: Int, z: Int): Block =
      if y <= 0 then Block.Stone else if y <= 4 then Block.Water else Block.Air
    for step <- Seq(1f / 30f, 1f / 60f, 1f / 120f) do
      val swimmer = ZombieSystem.Zombie(41, Vec3(0.5f, 1, 0.5f))
      val target = ZombieSystem.Target("Swimmer", Vec3(0.5f, 5.37f, -15))
      var low = Float.MaxValue; var high = Float.MinValue
      for i <- 0 until (9f / step).toInt do
        val before = swimmer.pos
        ZombieSystem.update(swimmer, step, Seq(target), true, false, lake)
        assert(!ZombieSystem.collides(swimmer.pos, lake), "swimming respects body collisions")
        assert((swimmer.pos - before).lengthSquared < 0.025f, "buoyancy never teleports a swimmer")
        if i * step > 6f then { low = low.min(swimmer.pos.y); high = high.max(swimmer.pos.y) }
      assert(
        abs(swimmer.pos.y - (4f + WaterSurface.fullHeight - 1.25f)) < 0.025f && high - low < 0.035f,
        s"stable waterline at $step: $low..$high"
      )
      assert(
        swimmer.pos.z < -8f && swimmer.swimming > 0.98f && swimmer.burning == 0,
        "surface pursuit and swimming animation"
      )
    val flowing = ZombieSystem.waterSurface(Vec3(0.5f, 3.5f, 0.5f), lake, (_, _, _) => 2).get
    assert(
      abs(flowing - (4f + WaterSurface.height(2))) < 0.001f,
      "buoyancy uses flowing-water surface heights"
    )
    def shore(x: Int, y: Int, z: Int): Block =
      if y <= 0 || z < -4 && y <= 4 then Block.Stone else if y <= 4 then Block.Water else Block.Air
    val climber = ZombieSystem.Zombie(42, Vec3(0.5f, 3.75f, 0.5f))
    val bank = ZombieSystem.Target("Bank", Vec3(0.5f, 6.62f, -9))
    for _ <- 0 until 500 do ZombieSystem.update(climber, 0.02f, Seq(bank), false, false, shore)
    assert(
      climber.pos.z < -6f && climber.pos.y >= 4.99f,
      s"swimmers climb a one-block shore: ${climber.pos}"
    )
    def roof(x: Int, y: Int, z: Int): Block = if y == 4 then Block.Stone else lake(x, y, z)
    val enclosed = ZombieSystem.Zombie(43, Vec3(0.5f, 1, 0.5f))
    for _ <- 0 until 300 do
      ZombieSystem.update(enclosed, 0.02f, Seq.empty, false, false, roof)
      assert(
        !ZombieSystem.collides(enclosed.pos, roof),
        "buoyancy cannot push a zombie through a ceiling"
      )
    val crowd = (0 until 20).map(i => ZombieSystem.Zombie(100 + i, Vec3(0.5f, 1, 0.5f)))
    ZombieSystem.separate(crowd, floor, 1f / 60f)
    assert(
      crowd.forall(z => (z.pos - Vec3(0.5f, 1, 0.5f)).lengthSquared <= 0.00014f),
      "crowd separation is bounded per frame, including dense groups"
    )
    hunter.heldItem = Block.DiamondSword
    assert(
      ZombieSystem.parseSnapshot(ZombieSystem.snapshot(Seq(hunter))).head._7 == Block.DiamondSword,
      "equipment survives mob synchronization and persistence"
    )
    assert(
      ZombieSystem.parseSnapshot("MOBS|1,0,1,0,0,20,0,0").head._7 == Block.Air,
      "older mob saves remain readable"
    )
    val rng = scala.util.Random(123)
    val equipment =
      (0 until 10000).map(_ => ZombieSystem.spawnEquipment(rng)).filter(_ != Block.Air)
    assert(
      equipment.length > 650 && equipment.length < 950 && equipment.distinct.length == 16,
      "rare equipped spawns can use every tool and weapon tier"
    )
    val wanderer = ZombieSystem.Zombie(53, Vec3(0.5f, 1, 0.5f))
    for _ <- 0 until 240 do ZombieSystem.update(wanderer, 0.05f, Seq.empty, false, false, floor)
    assert(
      (wanderer.pos - Vec3(0.5f, 1, 0.5f)).lengthSquared > 2f,
      "idle zombies explore without a player target"
    )
    val intruder = ZombieSystem.Target("Intruder", wanderer.pos + Vec3(0, 1.62f, -5))
    for _ <- 0 until 6 do ZombieSystem.update(wanderer, 0.05f, Seq(intruder), false, false, floor)
    assert(wanderer.memory > 5f && !wanderer.roaming, "perception promptly interrupts wandering")
    val sentinel = ZombieSystem.Zombie(54, Vec3(0.5f, 1, 0.5f))
    val visitor = ZombieSystem.Target("Creative", Vec3(0.5f, 2.62f, -3), vulnerable = false)
    for _ <- 0 until 20 do ZombieSystem.update(sentinel, 0.05f, Seq(visitor), false, false, floor)
    assert(sentinel.memory == 0f, "creative players never trigger pursuit")
    val attacker = ZombieSystem.Zombie(55, Vec3(0.5f, 1, 0.5f))
    val close = ZombieSystem.Target("Close", Vec3(0.5f, 2.62f, -0.5f))
    ZombieSystem.update(attacker, 0.05f, Seq(close), false, false, floor)
    attacker.attackCooldown = 0f
    def closedDoor(x: Int, y: Int, z: Int): Block =
      if z == -1 && y < 4 then Block.Stone else floor(x, y, z)
    assert(
      ZombieSystem.update(attacker, 0.01f, Seq(close), false, false, closedDoor).isEmpty,
      "an attack rechecks a newly closed obstruction"
    )

  private def method(game: Blockbox, name: String, args: AnyRef*): Any =
    val m = game.getClass.getDeclaredMethods
      .find(m =>
        (m.getName == name || m.getName.endsWith("$$" + name)) && m.getParameterCount == args.length
      )
      .get
    m.setAccessible(true); m.invoke(game, args*)
  private def set(game: Blockbox, name: String, value: Any): Unit =
    val f = game.getClass.getDeclaredFields
      .find(f => f.getName == name || f.getName.endsWith("$$" + name))
      .get
    f.setAccessible(true); f.set(game, value)
  private def get[A](game: Blockbox, name: String): A =
    val f = game.getClass.getDeclaredFields
      .find(f => f.getName == name || f.getName.endsWith("$$" + name))
      .get
    f.setAccessible(true); f.get(game).asInstanceOf[A]

  def combat(): Unit =
    val eye = Vec3(0.5f, 242.62f, 0.5f); val target = eye + Vec3(0, 0, -3)
    val air = (_: Int, _: Int, _: Int) => Block.Air
    assert(CombatSystem.canHit(eye, 0, 0, target, air))
    assert(
      !CombatSystem.canHit(eye, 180, 0, target, air),
      "looking away cannot hit a nearby player"
    )
    assert(!CombatSystem.canHit(eye, 0, 0, eye + Vec3(0, 0, -5), air), "melee reach is bounded")
    assert(
      !CombatSystem
        .canHit(eye, 0, 0, target, (_, _, z) => if z == -1 then Block.Stone else Block.Air),
      "solid walls block melee"
    )
    val game = Blockbox(); set(game, "soundEnabled", false); set(game, "camera", eye)
    val remotes = get[mutable.HashMap[String, RemotePlayer]](game, "remotePlayers")
    val attacker = RemotePlayer("Attacker", target, target, 180, 180, 0, 0, 0, 0, Block.IronSword)
    remotes(attacker.name) = attacker
    method(game, "hitPlayer", attacker.name, "Player")
    assert(
      get[Float](game, "playerHealth") == 14f && get[Vec3](
        game,
        "playerKnockback"
      ).lengthSquared > 1,
      "swords hurt and knock back the victim"
    )
    method(game, "hitPlayer", attacker.name, "Player")
    assert(get[Float](game, "playerHealth") == 14f, "the host enforces an attacker cooldown")
    get[mutable.HashMap[String, Double]](game, "lastCombatHit").clear()
    set(game, "gameMode", GameMode.Creative)
    method(game, "hitPlayer", attacker.name, "Player")
    assert(get[Float](game, "playerHealth") == 14f, "creative players are immune")

  def furnace(): Unit =
    val game = Blockbox()
    val hotbar = get[Array[Block]](game, "hotbarBlocks");
    val counts = get[Array[Int]](game, "hotbarCounts")
    hotbar(0) = Block.IronOre; counts(0) = 3; hotbar(1) = Block.CoalLump; counts(1) = 1
    set(game, "soundEnabled", false); set(game, "furnaceInput", Block.IronOre)
    method(game, "smeltFurnace")
    method(game, "updateFurnace", Float.box(3f))
    assert(get[Int](game, "furnaceOutputCount") == 0 && counts(0) == 3, "smelting takes real time")
    method(game, "updateFurnace", Float.box(1f))
    assert(
      get[Block](game, "furnaceOutput") == Block.IronIngot && get[Int](
        game,
        "furnaceOutputCount"
      ) == 1
    )
    assert(
      counts(0) == 2 && counts(1) == 0 && get[Float](game, "furnaceFuelRemaining") == 7,
      "one ore and one coal are spent exactly once"
    )
    method(game, "takeFurnaceOutput")
    assert(method(game, "totalItemCount", Block.IronIngot) == Int.box(1))
    set(game, "framebufferWidth", 800); set(game, "framebufferHeight", 600)
    hotbar(2) = Block.Sand; counts(2) = 8; hotbar(3) = Block.CoalLump; counts(3) = 2
    val scale = method(game, "uiScale").asInstanceOf[java.lang.Float].floatValue()
    val layout = BlockboxUiLayout.furnace(800, 600, scale)
    def click(r: UiRect, shift: Boolean = false): Unit =
      method(
        game,
        "handleFurnaceClick",
        Float.box(r.x + r.w / 2),
        Float.box(r.y + r.h / 2),
        Boolean.box(shift)
      )
    click(layout.hotbar(2), true)
    assert(
      get[Block](game, "furnaceInput") == Block.Sand && get[Int](game, "furnaceRequested") == 8,
      "shift-click selects and queues the visible stack using the drawn layout"
    )
    method(game, "updateFurnace", Float.box(2f))
    click(layout.hotbar(2))
    assert(
      get[Float](game, "furnaceProgress") == 2f,
      "clicking the selected stack does not reset a running batch"
    )
    for _ <- 0 until 15 do method(game, "updateFurnace", Float.box(2f))
    assert(get[Int](game, "furnaceOutputCount") == 8 && counts(2) == 0)
    click(layout.output)
    assert(method(game, "totalItemCount", Block.Glass) == Int.box(8))
    assert(
      get[FieldJournal](game, "fieldJournal").entries(12).completed,
      "actual furnace production advances the glass objective"
    )
    counts(2) = 3; hotbar(2) = Block.Sand
    click(layout.hotbar(2), true); click(layout.stop)
    assert(
      get[Int](game, "furnaceRequested") == 0 && counts(2) == 3,
      "stop leaves unsmelted items in inventory"
    )

  def adventures(): Unit =
    val journal = FieldJournal()
    assert(journal.entries.length == 19 && journal.pageCount == 4)
    journal.track(15)
    assert(journal.currentObjective.exists(_.title == "Across the water"))
    journal.travel(Float.NaN, true); journal.travel(Float.PositiveInfinity, false);
    journal.travel(10000, false)
    assert(
      journal.entries(14).progress == 0 && journal.entries(15).progress == 0,
      "teleports and invalid travel cannot complete goals"
    )
    journal.collected(Block.Wood, 8)
    assert(
      journal.claim(0, _ => 0).isEmpty && !journal.rewardClaimed(0),
      "full inventory never loses a reward"
    )
    assert(journal.claim(0, _ => 64).contains((Block.Planks, 16)))
    assert(journal.claim(0, _ => 64).isEmpty, "each objective reward can be collected only once")
    assert(journal.claim(19, _ => 64).isEmpty && journal.claim(-1, _ => 64).isEmpty)
    assert(journal.claim(1, _ => 64).isEmpty, "unfinished objectives cannot grant rewards")
    for _ <- 0 until 12 do journal.placedBlock(Block.Torch)
    journal.smelted(Block.Glass, 8)
    for _ <- 0 until 125 do journal.travel(8, false)
    for _ <- 0 until 19 do journal.travel(8, true)
    for _ <- 0 until 20 do journal.defeatedZombie()
    journal.collected(Block.DiamondSword, 1)
    for _ <- 0 until 3 do journal.salvagedCache()
    assert(
      journal.entries.drop(12).forall(_.completed),
      "building, crafting, exploration, swimming and combat advance the new objectives"
    )
    val raw = ByteArrayOutputStream(); val out = DataOutputStream(raw)
    journal.write(out); journal.writeProgress(out); journal.writeAdventure(out);
    journal.writeDiscoveries(out)
    val restored = FieldJournal(); val in = DataInputStream(ByteArrayInputStream(raw.toByteArray))
    restored.read(in); restored.readProgress(in); restored.readAdventure(in);
    restored.readDiscoveries(in)
    assert(restored.entries == journal.entries && restored.rewardClaimed(0))
    assert(
      restored.claim(0, _ => 64).isEmpty,
      "saving and reloading cannot duplicate claimed rewards"
    )
    assert(
      restored.currentObjective == journal.currentObjective,
      "tracked progress survives reload"
    )
    val game = Blockbox(); set(game, "soundEnabled", false)
    set(game, "framebufferWidth", 640); set(game, "framebufferHeight", 360)
    val goals = get[FieldJournal](game, "fieldJournal"); goals.collected(Block.DiamondSword, 1)
    val scale = method(game, "uiScale").asInstanceOf[Float]
    val layout = BlockboxUiLayout.journal(640, 360, scale)
    assert(layout.rows.length == 3)
    set(game, "journalPage", 5)
    val action = method(game, "journalAction", layout.rows(2)).asInstanceOf[UiRect]
    method(
      game,
      "handleJournalClick",
      Float.box(action.x + action.w / 2),
      Float.box(action.y + action.h / 2)
    )
    assert(
      goals.rewardClaimed(17) && method(game, "totalItemCount", Block.RainbowBlock) == Int.box(16),
      "the last objective remains reachable and claimable in the compact journal"
    )
    method(
      game,
      "handleJournalClick",
      Float.box(action.x + action.w / 2),
      Float.box(action.y + action.h / 2)
    )
    assert(
      method(game, "totalItemCount", Block.RainbowBlock) == Int.box(16),
      "repeat clicks do not duplicate rewards"
    )

  def movement(): Unit =
    val game = Blockbox(); set(game, "soundEnabled", false);
    set(game, "gameMode", GameMode.Creative)
    val walk = MovementSpeeds.ground(false, false, false, false)
    set(game, "smoothedHorizontalSpeed", 16f); set(game, "viewFovOffset", 7.5f)
    method(game, "toggleFlyForPlayer", "Player")
    assert(get[Boolean](game, "flyEnabled"))
    method(game, "toggleFlyForPlayer", "Player")
    assert(
      !get[Boolean](game, "flyEnabled") && get[Float](game, "viewFovOffset") == 0f && get[Float](
        game,
        "smoothedHorizontalSpeed"
      ) == 0f,
      "leaving flight clears flight-era FOV and gait state"
    )
    assert(
      MovementSpeeds.ground(false, false, false, false) == walk && walk == 5.5f,
      "ground speed is independent from flight and landing history"
    )
    assert(
      MovementSpeeds.ground(false, true, false, false) > walk && MovementSpeeds.ground(
        false,
        false,
        true,
        false
      ) < walk
    )
    assert(MovementSpeeds.ground(false, false, false, true) < MovementSpeeds.flight(true))
    for fov <- Seq(45f, 70f, 100f, 110f); aspect <- Seq(0.5, 1.0, 1.78, 2.4, 4.0) do
      val near = CameraProjection.nearPlane(fov, aspect)
      val t = math.tan(math.toRadians(fov) * 0.5)
      assert(
        near > 0.01 && near * math.sqrt(1 + t * t * (1 + aspect * aspect)) <= 0.12001,
        "near-plane corners stay inside wall/ceiling clearance at every FOV and aspect"
      )
    for remaining <- (1 to 50).map(_ / 50f) do
      assert(
        method(game, "damageFlash", Float.box(remaining)) == Boolean.box(true),
        "hurt colour stays steady through its lifetime"
      )
    assert(method(game, "damageFlash", Float.box(0)) == Boolean.box(false))

  def quickMove(): Unit =
    val game = Blockbox(); set(game, "soundEnabled", false)
    val hotbar = get[Array[Block]](game, "hotbarBlocks");
    val hotCounts = get[Array[Int]](game, "hotbarCounts")
    val pack = get[Array[Block]](game, "backpackBlocks");
    val packCounts = get[Array[Int]](game, "backpackCounts")
    for i <- hotbar.indices do { hotbar(i) = Block.Air; hotCounts(i) = 0 }
    for i <- pack.indices do { pack(i) = Block.Air; packCounts(i) = 0 }
    hotbar(2) = Block.Stone; hotCounts(2) = 40; pack(5) = Block.Stone; packCounts(5) = 60
    method(game, "quickMoveHotbar", Int.box(2))
    assert(
      packCounts(5) == 64 && pack(0) == Block.Stone && packCounts(0) == 36 && hotbar(
        2
      ) == Block.Air,
      "shift-click tops up a matching stack before filling the first empty slot"
    )
    method(game, "quickMoveBackpack", Int.box(0))
    assert(
      hotbar(0) == Block.Stone && hotCounts(0) == 36 && pack(0) == Block.Air,
      "backpack stacks move to the hotbar"
    )
    val cells = get[Array[Block]](game, "craftGridBlocks");
    val cellCounts = get[Array[Int]](game, "craftGridCounts")
    cells(0) = Block.BirchPlanks; cellCounts(0) = 3; cells(3) = Block.BirchPlanks; cellCounts(3) = 3
    method(game, "tryCraftGrid", java.lang.Boolean.TRUE)
    assert(
      hotbar(8) == Block.Stick && hotCounts(8) == 12 && cells(0) == Block.Air && cells(
        3
      ) == Block.Air,
      "shift-crafting spends every ingredient and fills the hotbar from the right"
    )
    set(game, "gameMode", GameMode.Creative)
    method(game, "creativeQuickGive", Block.Torch)
    assert(
      hotbar(1) == Block.Torch && hotCounts(1) == 64,
      "creative shift-click gives a full stack in the first free hotbar slot"
    )

  def saves(): Unit =
    val game = Blockbox()
    val journal = get[FieldJournal](game, "fieldJournal")
    journal.collected(Block.IronPickaxe, 1); journal.collected(Block.DiamondGem, 3);
    journal.defeatedZombie()
    val raw = ByteArrayOutputStream(); val out = DataOutputStream(raw)
    journal.write(out); journal.writeProgress(out)
    val read = FieldJournal(); val in = DataInputStream(ByteArrayInputStream(raw.toByteArray))
    read.read(in); read.readProgress(in)
    assert(read.entries == journal.entries, "new survival goals survive the optional save tail")
    val hotbar = get[Array[Block]](game, "hotbarBlocks");
    val counts = get[Array[Int]](game, "hotbarCounts")
    hotbar(0) = Block.DiamondPickaxe; counts(0) = 1
    val bytes = ByteArrayOutputStream()
    method(game, "writeWorldExtras", DataOutputStream(bytes))
    val loaded = Blockbox()
    method(loaded, "readWorldExtras", DataInputStream(ByteArrayInputStream(bytes.toByteArray)))
    assert(get[Array[Block]](loaded, "hotbarBlocks")(0) == Block.DiamondPickaxe)
