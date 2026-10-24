package blockbox

import scala.math.*
import scala.collection.mutable

object InteractionSystemsTest:
  def run(): Unit =
    items(); presentation(); interactions()
    println(
      "[interactions] atomic dropped items, buoyancy, crouch physics, billboards, tiled grass, directional sound and salvage passed"
    )

  def field[A](game: Blockbox, name: String): A =
    val f = game.getClass.getDeclaredFields
      .find(f => f.getName == name || f.getName.endsWith("$$" + name))
      .get
    f.setAccessible(true); f.get(game).asInstanceOf[A]
  def set(game: Blockbox, name: String, value: Any): Unit =
    val f = game.getClass.getDeclaredFields
      .find(f => f.getName == name || f.getName.endsWith("$$" + name))
      .get
    f.setAccessible(true); f.set(game, value)
  def call(game: Blockbox, name: String, args: AnyRef*): Any =
    val m = game.getClass.getDeclaredMethods
      .find(m =>
        (m.getName == name || m.getName.endsWith("$$" + name)) && m.getParameterCount == args.length
      )
      .get
    m.setAccessible(true); m.invoke(game, args*)

  def items(): Unit =
    val world = (x: Int, y: Int, z: Int) => if y <= 0 then Block.Stone else Block.Air
    val drops = DroppedItems()
    val e = drops.spawn(Block.IronSword, 1, Vec3(0.5f, 3, 0.5f)).get
    assert(
      drops.reserve("Alice", e.id, 1, Vec3(0.5f, 3, 0.5f)).isEmpty,
      "fresh drops have pickup grace"
    )
    for _ <- 0 until 100 do drops.update(0.025f, world, _ => true)
    assert(e.pos.y >= 1.12f && e.pos.y < 1.2f && e.grounded, s"item settles above ground: ${e.pos}")
    val offer = drops.reserve("Alice", e.id, 1, Vec3(0.5f, 1, 0.5f)).get
    assert(
      drops.reserve("Bob", e.id, 1, Vec3(0.5f, 1, 0.5f)).isEmpty,
      "two players cannot receive one item"
    )
    drops.resolve("Bob", offer.id, accepted = true)
    assert(drops.forPlayer("Alice").size == 1, "only the reserved recipient can acknowledge")
    val saved = DroppedItems(); saved.receive(drops.saveSnapshot, interpolate = false)
    assert(
      saved.entities.values.map(_.count).sum == 1,
      "pending delivery remains in the world save"
    )
    drops.resolve("alice", offer.id, accepted = false)
    assert(
      drops.entities.values.map(_.count).sum == 1,
      "a full inventory returns the reserved stack"
    )
    for _ <- 0 until 30 do drops.update(0.025f, world, _ => true)
    val returned = drops.entities.values.head
    val second = drops.reserve("Alice", returned.id, 1, returned.pos).get
    drops.resolve("Alice", second.id, accepted = true);
    drops.resolve("Alice", second.id, accepted = true)
    assert(
      drops.entities.isEmpty && drops.forPlayer("Alice").isEmpty,
      "repeat acknowledgements cannot award twice"
    )
    val merged = drops.spawn(Block.Stone, 7, Vec3(2, 2, 2)).get
    assert(drops.spawn(Block.Stone, 8, Vec3(2.1f, 2, 2)).get.id == merged.id && merged.count == 15)
    val before = drops.snapshot
    try { drops.receive("ITEMS|1,3,-2,0,0,0,0,0", false); assert(false) }
    catch case _: IllegalArgumentException => ()
    assert(drops.snapshot == before, "bad snapshots cannot partially replace the item state")
    val lake = (x: Int, y: Int, z: Int) =>
      if y <= 0 then Block.Stone else if y <= 4 then Block.Water else Block.Air
    val floating = DroppedItems();
    val buoy = floating.spawn(Block.DiamondGem, 1, Vec3(0.5f, 1.5f, 0.5f)).get
    for _ <- 0 until 500 do floating.update(0.025f, lake, _ => true)
    assert(
      abs(buoy.pos.y - (4 + WaterSurface.fullHeight - 0.04f)) < 0.08f,
      s"items float at the connected water surface: ${buoy.pos}"
    )
    val serialized = DroppedItems.messages(drops.snapshot, 3)
    val aging = DroppedItems()
    val sitting = aging.spawn(Block.Dirt, 1, Vec3(0.5f, 1.2f, 0.5f)).get
    aging.update(299f, world, _ => true)
    assert(aging.entities.contains(sitting.id), "an unpicked item lasts until five minutes")
    aging.update(1f, world, _ => true)
    assert(aging.entities.isEmpty, "an unpicked item despawns at five minutes")
    val pile = DroppedItems()
    val older = pile.spawn(Block.Sand, 1, Vec3(0.5f, 1.2f, 0.5f)).get
    val newer = pile.spawn(Block.Sand, 1, Vec3(6f, 1.2f, 6f)).get
    pile.update(40f, world, _ => true)
    older.pos = Vec3(2f, 1f, 2f); newer.pos = Vec3(2.4f, 1f, 2f); newer.age = 4f
    pile.update(0.05f, world, _ => true)
    assert(
      pile.entities.size == 1 && pile.entities.values.head.count == 2 && pile.entities.values.head.age < 10f,
      "nearby stacks merge and keep the younger despawn timer"
    )
    assert(serialized.head.startsWith("ITEMBEGIN|3|") && serialized.last == "ITEMEND|3")
    assert(serialized.forall(_.length < blockbox.net.BlockboxNet.MAX_LINE_LENGTH))

  def presentation(): Unit =
    val standing = Vec3(0, 2.62f, 0)
    val low = PlayerStance.eye(standing, 1)
    assert(abs(standing.y - low.y - 0.30f) < 0.00001f)
    var crouch = 0f
    for _ <- 0 until 30 do crouch = PlayerStance.approach(crouch, true, 1f / 60f)
    assert(crouch > 0.99f)
    for yaw <- Vector(0f, 90f, 173f); pitch <- Vector(-80f, 0f, 75f) do
      val (right, up) = ParticleBillboard.axes(yaw, pitch)
      val forward = BlockboxRaycast.viewDirection(yaw, pitch)
      def dot(a: Vec3, b: Vec3) = a.x * b.x + a.y * b.y + a.z * b.z
      assert(
        abs(dot(right, forward)) < 0.0001f && abs(dot(up, forward)) < 0.0001f && abs(
          dot(right, up)
        ) < 0.0001f
      )
    val art = Array.tabulate(16, 16)((y, x) => (x * 10 + y, x * 10 + y, x * 10 + y, 255))
    val inside = art(8)(8); TerrainUv.tileGrass(art)
    assert(art(8)(8) == inside, "grass interiors retain the selected texture pack")
    for i <- 0 until 16 do
      assert(
        art(0)(i) == art(15)(i) && art(i)(0) == art(i)(15) && art(0)(i) == art(i)(0) && art(0)(
          i
        ) == art(0)(15 - i),
        "all grass borders match after quarter turns"
      )
    assert((for x <- -16 to 16; z <- -16 to 16 yield TerrainUv.rotation(x, z)).toSet.size == 4)
    assert(SpatialAudio.mix(Vec3(0, 0, 0), 0, Vec3(-4, 0, 0), 24, false, false).pan < -0.9f)
    val open = SpatialAudio.mix(Vec3(0, 0, 0), 0, Vec3(4, 0, 0), 24, false, false)
    val covered = SpatialAudio.mix(Vec3(0, 0, 0), 0, Vec3(4, 0, 0), 24, true, false)
    assert(open.pan > 0.9f && covered.gain < open.gain * 0.4f)
    assert(SpatialAudio.mix(Vec3(0, 0, 0), 0, Vec3(30, 0, 0), 24, false, false).gain == 0)
    for fov <- Vector(120f, 127.5f); aspect <- Vector(16.0 / 9, 32.0 / 9) do
      val near = CameraProjection.nearPlane(fov, aspect)
      val tangent = tan(toRadians(fov) / 2)
      assert(near * sqrt(1 + tangent * tangent * (1 + aspect * aspect)) <= 0.12001)
    val cache = SupplyCaches.contents(42L, -10, 40, 11)
    assert(
      cache == SupplyCaches.contents(42L, -10, 40, 11) && cache.exists(_._1 == Block.GoldIngot)
    )
    val view = Blockbox()
    set(view, "camera", standing); set(view, "crouchAmount", 1f)
    set(view, "yaw", 0f); set(view, "pitch", 0f)
    set(view, "framebufferWidth", 960); set(view, "framebufferHeight", 640)
    val projected = call(view, "projectWorldToScreen", low + Vec3(0, 0, -4))
      .asInstanceOf[Option[(Float, Float, Float)]]
      .get
    assert(
      abs(projected._1 - 480) < 0.01f && abs(projected._2 - 320) < 0.01f,
      "name tags project from the lowered crouching eye"
    )

  def interactions(): Unit =
    val edits = new ChunkBlockMap[Block]
    edits((-1, 30, -1)) = Block.Stone; edits((0, 30, 0)) = Block.Dirt
    assert(edits.positionsInChunk(-1, -1) == Vector((-1, 30, -1)))
    edits((-1, 30, -1)) = Block.Sand
    assert(edits.positionsInChunk(-1, -1).size == 1 && edits((-1, 30, -1)) == Block.Sand)
    edits.remove((-1, 30, -1)); assert(edits.positionsInChunk(-1, -1).isEmpty)
    edits.clear(); assert(edits.isEmpty && edits.positionsInChunk(0, 0).isEmpty)
    val game = Blockbox(); set(game, "soundEnabled", false)
    set(game, "camera", Vec3(2.5f, 2.97f, 2.5f))
    val chunks = field[mutable.AnyRefMap[(Int, Int), Chunk]](game, "chunks")
    val c = Chunk(0, 0, null, TerrainGenerator(42), new Array[Byte](16 * 16 * Terrain.worldHeight))
    chunks((0, 0)) = c
    c.setBlock(2, 3, 2, Block.Stone)
    assert(
      call(game, "collidesPlayer", field[Vec3](game, "camera"), java.lang.Boolean.FALSE) == true
    )
    assert(
      call(game, "collidesPlayer", field[Vec3](game, "camera"), java.lang.Boolean.TRUE) == false,
      "crouching shrinks the collision height without moving the feet"
    )
    val hot = field[Array[Block]](game, "hotbarBlocks");
    val counts = field[Array[Int]](game, "hotbarCounts")
    hot(0) = Block.Stone; counts(0) = 4; set(game, "selectedBlock", 0)
    call(game, "dropHeldStack", java.lang.Boolean.FALSE)
    val drops = field[DroppedItems](game, "droppedItems")
    assert(
      counts(0) == 3 && drops.entities.values.map(_.count).sum == 1,
      "Q creates an entity and consumes one held item"
    )
    call(game, "dropHeldStack", java.lang.Boolean.TRUE)
    assert(
      counts(0) == 0 && drops.entities.values.map(_.count).sum == 4,
      "Ctrl+Q drops the rest of the stack"
    )
    set(game, "camera", Vec3(2.5f, 2.62f, 3.5f)); set(game, "yaw", 0f); set(game, "pitch", 0f)
    c.setBlock(2, 2, 1, Block.Stone)
    hot(0) = Block.WoodenPickaxe; counts(0) = 1
    call(game, "breakTargetBlock", java.lang.Boolean.TRUE)
    assert(
      c.getBlock(2, 2, 1) == Block.Air && drops.entities.values
        .filter(_.block == Block.Stone)
        .map(_.count)
        .sum == 5
    )
    assert(
      call(game, "totalItemCount", Block.Stone) == 0,
      "mined loot is physical, not teleported into inventory"
    )
    c.setBlock(2, 2, 1, Block.SupplyCrate)
    call(game, "breakTargetBlock", java.lang.Boolean.TRUE)
    assert(
      drops.entities.values.exists(_.block == Block.GoldIngot),
      "real supply crates release their deterministic salvage"
    )
    assert(field[FieldJournal](game, "fieldJournal").entries(18).progress == 1)
    val knock = ZombieSystem.Zombie(90, Vec3(0.5f, 1, 0.5f))
    knock.velocity = Vec3(4f, 2f, 0)
    ZombieSystem.update(
      knock,
      0.05f,
      Seq.empty,
      false,
      false,
      (_, y, _) => if y <= 0 then Block.Stone else Block.Air
    )
    assert(
      knock.pos.x > 0.65f && knock.velocity.x < 4f,
      "zombie knockback actually moves the body and decays"
    )
    assert(CombatSystem.rayPlayer(Vec3(0, 2.72f, 3), Vec3(0, 0, -1), Vec3(0, 2.62f, 0)).nonEmpty)
    assert(
      CombatSystem
        .rayPlayer(Vec3(0, 2.72f, 3), Vec3(0, 0, -1), Vec3(0, 2.62f, 0), crouch = 1f)
        .isEmpty,
      "crouched combat bounds follow the shorter body"
    )
    c.dispose()

  def multiplayer(): Unit =
    import blockbox.net.BlockboxNet
    val host = Blockbox(); set(host, "soundEnabled", false);
    set(host, "gameMode", GameMode.Creative)
    set(host, "playerName", "Host"); set(host, "worldSeed", 42L)
    val spawn = Vec3(3.5f, 65.62f, 3.5f); set(host, "camera", spawn)
    val server = GameServer(
      0,
      42L,
      spawn,
      "Host",
      0,
      () => Seq.empty,
      () => Seq("Host"),
      () => spawn,
      () => 0f,
      () => 0f,
      ModRuntime.VanillaHash,
      "",
      () => "ENV|150|-1|0|0|0|0|0"
    )
    set(host, "gameServer", server); set(host, "multiplayerMode", true)
    val a = GameClient("127.0.0.1", server.localPort);
    val b = GameClient("127.0.0.1", server.localPort)
    val alice = Blockbox(); val bob = Blockbox()
    for (game, name, client) <- Vector((alice, "Alice", a), (bob, "Bob", b)) do
      set(game, "soundEnabled", false); set(game, "playerName", name)
      set(game, "gameClient", client); set(game, "multiplayerMode", true)
    var returned: GameClient = null
    def pump(label: String)(condition: => Boolean): Unit =
      val deadline = System.nanoTime() + 5_000_000_000L
      while !condition && System.nanoTime() < deadline do
        call(host, "processNetworkMessages"); call(alice, "processNetworkMessages");
        call(bob, "processNetworkMessages")
        Thread.sleep(2)
      assert(condition, label)
    try
      assert(a.connect("Alice", ModRuntime.VanillaHash));
      assert(b.connect("Bob", ModRuntime.VanillaHash))
      a.send(BlockboxNet.pos("Alice", spawn.x, spawn.y, spawn.z, 0, 0, 1, 64, Block.Stone.ordinal))
      b.send(BlockboxNet.pos("Bob", spawn.x, spawn.y, spawn.z, 0, 0, 2, 64, Block.Stone.ordinal))
      pump("host receives both guest bodies") {
        field[mutable.HashMap[String, RemotePlayer]](host, "remotePlayers").size == 2
      }
      // Drain WORLD/RESUME and the initial empty item snapshots before offering loot.
      for _ <- 0 until 10 do
        call(alice, "processNetworkMessages"); call(bob, "processNetworkMessages"); Thread.sleep(2)
      val items = field[DroppedItems](host, "droppedItems")
      val e = items.spawn(Block.Stone, 1, Vec3(3.5f, 64.2f, 3.5f)).get; e.delay = 0
      a.send(s"PICK|${e.id}|1"); b.send(s"PICK|${e.id}|1")
      pump("one physical item is awarded exactly once") {
        call(alice, "totalItemCount", Block.Stone)
          .asInstanceOf[Int] + call(bob, "totalItemCount", Block.Stone).asInstanceOf[Int] == 1 &&
        items.forPlayer("Alice").isEmpty && items.forPlayer("Bob").isEmpty
      }
      val (owner, ownerClient, receiver, receiverClient, ownerName, receiverName) =
        if call(alice, "totalItemCount", Block.Stone) == 1 then (alice, a, bob, b, "Alice", "Bob")
        else (bob, b, alice, a, "Bob", "Alice")
      call(owner, "dropHeldStack", java.lang.Boolean.TRUE)
      pump("Q creates a host-owned throw and confirms the inventory debit") {
        items.entities.nonEmpty && field[mutable.HashMap[Long, (Block, Int)]](
          owner,
          "pendingTosses"
        ).isEmpty
      }
      assert(call(owner, "totalItemCount", Block.Stone) == 0)
      val thrown = items.entities.values.head; thrown.delay = 0
      receiverClient.send(s"PICK|${thrown.id}|1")
      pump("another guest can collect the thrown item") {
        call(receiver, "totalItemCount", Block.Stone) == 1 && items
          .forPlayer(receiverName)
          .isEmpty && items.entities.isEmpty
      }
      val receipt = field[mutable.HashSet[Long]](receiver, "appliedItemOffers").max
      call(receiver, "stopNetworking")
      pump("guest leaves the host") {
        !field[mutable.HashMap[String, RemotePlayer]](host, "remotePlayers").contains(receiverName)
      }
      call(receiver, "resetInventory")
      returned = GameClient("127.0.0.1", server.localPort)
      assert(returned.connect(receiverName, ModRuntime.VanillaHash))
      set(receiver, "gameClient", returned); set(receiver, "multiplayerMode", true)
      pump("same-session inventory and pickup receipt resume") {
        call(receiver, "totalItemCount", Block.Stone) == 1
      }
      server.sendTo(receiverName, s"ITEMOFFER|$receipt|${Block.Stone.ordinal}|1")
      for _ <- 0 until 20 do
        call(receiver, "processNetworkMessages"); call(host, "processNetworkMessages");
        Thread.sleep(2)
      assert(
        call(receiver, "totalItemCount", Block.Stone) == 1,
        "replayed pickup after reconnect cannot duplicate items"
      )
      println(
        "[multiplayer-items] two simultaneous pickups, guest handoff and reconnect receipt replay passed"
      )
    finally
      a.disconnect(); b.disconnect(); if returned != null then returned.disconnect()
      server.stop()
