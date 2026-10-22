package blockbox

import blockbox.net.{BlockboxNet,BlockboxSockets}
import java.io.*
import java.util.concurrent.atomic.AtomicReference
import scala.collection.mutable.ArrayBuffer

@main def runGameSystemsTests(): Unit =
  GameSystemsTest.layouts()
  GameSystemsTest.journal()
  GameSystemsTest.terrain()
  GameSystemsTest.appearance()
  GameSystemsTest.torches()
  GameSystemsTest.lighting()
  PresentationTest.run()
  ModdingTest.run()
  if sys.env.get("BLOCKBOX_SKIP_SOCKET_TESTS").contains("1") then println("Live multiplayer tests skipped: BLOCKBOX_SKIP_SOCKET_TESTS=1")
  else GameSystemsTest.network()
  println("GameSystemsTest: all enabled tests passed")

object GameSystemsTest:
  def appearance(): Unit =
    var used = Set.empty[Int]
    for _ <- 0 until BlockboxNet.MAX_PLAYERS do
      val color = PlayerAppearance.nextFreeColor(used).get
      assert(!used.contains(color))
      used += color
    assert(PlayerAppearance.nextFreeColor(used).isEmpty,"a full session never reuses a shirt")
    assert(PlayerAppearance.shirts.distinct.length==BlockboxNet.MAX_PLAYERS)
    val released = 17
    assert(PlayerAppearance.nextFreeColor(used-released).contains(released),"departed shirt becomes available")

  def torches(): Unit =
    val gen = TerrainGenerator(42)
    val chunk = Chunk(0,0,null,gen,new Array[Byte](16*16*Terrain.worldHeight))
    val directory = java.nio.file.Files.createTempDirectory("blockbox-torch-save")
    try
      for mount <- TorchMount.values do
        val (sx,sy,sz)=mount.support
        assert(TorchMount.fromNormal((-sx,-sy,-sz)).contains(mount))
        val x=2+mount.id*2; val y=40; val z=8
        chunk.setBlock(x+sx,y+sy,z+sz,Block.Stone)
        chunk.setBlock(x,y,z,Block.Torch,LogAxis.Y,mount)
        assert(mount.supported(x,y,z,chunk.getBlock))
        chunk.setBlock(x,y-1,z,Block.Stone)
        assert(chunk.getTorchMount(x,y,z)==mount,"adding a floor cannot rotate a wall torch")
        val mesh=TorchModel.vertices(mount)
        assert(mesh.nonEmpty && mesh.length%3==0)
        assert(mesh.forall(v => v.x>=0 && v.x<=1 && v.y>=0 && v.y<=1 && v.z>=0 && v.z<=1),"torch stays inside its voxel")
        assert(mesh.forall(v => math.abs(v.nx*v.nx+v.ny*v.ny+v.nz*v.nz-1)<0.001f),"torch normals match its faces")
        val packet=BlockboxNet.parseBlock(BlockboxNet.packet(BlockboxNet.block(x,y,z,Block.Torch.ordinal,0,0,mount.id)),Terrain.worldHeight)
        assert(packet.torchMount==mount.id,"wall attachment survives a network round trip")
      assert(TorchMount.fromNormal((0,-1,0)).isEmpty,"no ceiling attachment")
      assert(!TorchMount.canSupport(Block.Leaves) && !TorchMount.canSupport(Block.Water) && TorchMount.canSupport(Block.Glass))
      chunk.save(directory.toFile)
      val restored=Chunk(0,0,null,gen,new Array[Byte](16*16*Terrain.worldHeight))
      restored.load(directory.toFile)
      for mount <- TorchMount.values do
        val x=2+mount.id*2
        assert(restored.getTorchMount(x,40,8)==mount,"saved torch keeps its clicked support")
        val (sx,sy,sz)=mount.support
        restored.setBlock(x+sx,40+sy,8+sz,Block.Air)
        assert(!mount.supported(x,40,8,restored.getBlock),"only the saved support keeps a torch attached")
      // A legacy wall torch on a seam must wait for its supporting chunk, and
      // saving before that chunk arrives must preserve the pending migration.
      val edge=Chunk(0,0,null,gen,new Array[Byte](16*16*Terrain.worldHeight))
      edge.setBlock(15,40,8,Block.Torch,LogAxis.Y,TorchMount.EastWall)
      val old=edge.captureSaveSnapshot().copy(torchMounts=Nil).toBytes().dropRight(8)
      blockbox.io.BlockboxSaveFiles.writeChunkBytesAtomic(directory,0,0,old,true)
      val legacy=Chunk(0,0,null,gen,new Array[Byte](16*16*Terrain.worldHeight)); legacy.load(directory.toFile)
      assert(!legacy.torchAttachmentResolved(15,40,8),"unloaded support does not destroy a legacy torch")
      legacy.save(directory.toFile)
      val pending=Chunk(0,0,null,gen,new Array[Byte](16*16*Terrain.worldHeight)); pending.load(directory.toFile)
      assert(!pending.torchAttachmentResolved(15,40,8),"unresolved legacy attachment survives another save")
      pending.migrateLegacyTorches((x,y,z) => Some(if (x,y,z)==(16,40,8) then Block.Stone else Block.Air))
      assert(pending.torchAttachmentResolved(15,40,8) && pending.getTorchMount(15,40,8)==TorchMount.EastWall)
      def at(x: Int,y: Int,z: Int): Block = if (x,y,z)==(0,0,0) then Block.Torch else Block.Air
      assert(BlockboxRaycast.raycast(Vec3(0.5f,0.3f,2),Vec3(0,0,-1),4,at).exists(_.block==(0,0,0)),"torch shaft can be mined")
      assert(BlockboxRaycast.raycast(Vec3(0.1f,0.3f,2),Vec3(0,0,-1),4,at).isEmpty,"ray passes through empty part of a torch voxel")
    finally blockbox.io.BlockboxFiles.deleteRecursive(directory)

  def lighting(): Unit =
    val source=BlockLight.Source(19,30,8,15)
    val open=BlockLight.build(Seq(source),16,Terrain.worldHeight,(_,_,_) => Block.Air)
    assert(open.level(15,30,8)==11 && open.level(9,30,8)==5,"torch light crosses the neighboring chunk's interior")
    val diagonal=BlockLight.build(Seq(BlockLight.Source(19,30,19,15)),16,Terrain.worldHeight,(_,_,_) => Block.Air)
    assert(diagonal.level(15,30,15)==7,"light crosses a diagonal chunk boundary")
    val wall=BlockLight.build(Seq(source),16,Terrain.worldHeight,(x,_,_) => if x==16 then Block.Stone else Block.Air)
    assert(wall.level(15,30,8)==0,"solid walls block local light without leaks")
    val glass=BlockLight.build(Seq(source),16,Terrain.worldHeight,(x,_,_) => if x==16 then Block.Glass else Block.Air)
    assert(glass.level(15,30,8)==11,"glass transmits torch light")
    val water=BlockLight.build(Seq(source),16,Terrain.worldHeight,(x,_,_) => if x==16 then Block.Water else Block.Air)
    assert(water.level(15,30,8)==10,"water attenuates light")
    val dark=BlockLight.build(Seq.empty,16,Terrain.worldHeight,(_,_,_) => Block.Air)
    assert(dark.level(15,30,8)==0 && dark.maxY<dark.minY,"unlit chunks allocate no lighting volume")

  def layouts(): Unit =
    for (w,h) <- Seq((640f,360f),(1280f,720f),(1920f,1080f),(3440f,1440f),(3840f,2160f)); requested <- Seq(0.6f,1f,1.5f,3f) do
      val s = requested.min((w/960f).min(h/600f)).max(0.45f)
      val menu = BlockboxUiLayout.mainMenu(w,h,s)
      assert(menu.buttons.length==8)
      for (a,i) <- menu.buttons.zipWithIndex do
        assert(a.x>=0 && a.y>=0 && a.x+a.w<=w && a.y+a.h<=h,s"button outside viewport $w x $h")
        assert(a.contains(a.x+a.w/2,a.y+a.h/2))
        for b <- menu.buttons.drop(i+1) do
          assert(a.x+a.w<=b.x || b.x+b.w<=a.x || a.y+a.h<=b.y || b.y+b.h<=a.y,"menu buttons overlap")
      val options = BlockboxUiLayout.settings(w,h,s)
      assert(options.content.h>0 && options.content.w>0)
      assert(options.content.y+options.content.h<options.done.y)
      assert(options.tabs.forall(t => t.y+t.h<options.content.y))
      assert(options.done.y+options.done.h<=h)

  def journal(): Unit =
    val journal = FieldJournal()
    for i <- 0 until 6 do journal.observe(i,25,true,b => if b==Block.Wood then 8 else if b==Block.Furnace || b==Block.IronIngot then 1 else 0)
    for _ <- 0 until 24 do journal.placedBlock()
    assert(journal.entries.forall(_.completed))
    val announcements = (0 until 6).flatMap(_ => journal.nextAnnouncement())
    assert(announcements.distinct.length==6 && journal.nextAnnouncement().isEmpty)
    val bytes = ByteArrayOutputStream()
    journal.write(DataOutputStream(bytes))
    val restored = FieldJournal()
    restored.read(DataInputStream(ByteArrayInputStream(bytes.toByteArray)))
    assert(restored.entries==journal.entries)
    assert(restored.nextAnnouncement().isEmpty,"completed journal notifications persisted")
    val collected = FieldJournal()
    collected.collected(Block.Wood,4); collected.collected(Block.BirchWood,4)
    collected.observe(0,0,true,_ => 0)
    assert(collected.entries.head.completed,"collected logs remain recorded after crafting")
    val underground = FieldJournal()
    underground.observe(-1,30,false,_ => 64)
    assert(underground.entries.last.progress==0 && underground.entries.head.progress==0,"underground/creative do not fabricate discovery or resource goals")

  def terrain(): Unit =
    val seed = 8675309L
    val first = TerrainGenerator(seed); val second = TerrainGenerator(seed)
    for (cx,cz) <- Seq((0,0),(-1,0),(2,-3)) do
      val a = first.generateChunk(cx,cz)._1; val b = second.generateChunk(cx,cz)._1
      assert(java.util.Arrays.equals(a,b),"terrain deterministic across generators")
      assert(a.take(256).forall(_==Block.Bedrock.id),"solid bedrock floor")
    val safe = BlockboxWorld.findSafeSpawn(first,(x,y,z) => Some(if x==0 && z==0 && y<=60 then Block.Wood else if y<=40 then Block.Stone else Block.Air))
    assert(safe.x==2.5f && math.abs(safe.y-42.7f)<0.01f,"spawn avoids tree trunks and has head room")
    val entrances = (for cx <- -7 to 7; cz <- -7 to 7; e <- first.caveEntranceForCell(cx,cz) yield e).take(3)
    assert(entrances.nonEmpty,"sample region contains natural entrances")
    val chunks = scala.collection.mutable.Map.empty[(Int,Int),Array[Byte]]
    def at(x: Int,y: Int,z: Int): Block =
      val cx = Math.floorDiv(x,16); val cz = Math.floorDiv(z,16)
      val blocks = chunks.getOrElseUpdate((cx,cz),first.generateChunk(cx,cz)._1)
      Block.fromId(blocks((y*16+Math.floorMod(z,16))*16+Math.floorMod(x,16)))
    for e <- entrances do
      val start = Vec3(e.x+e.dx*12,e.y-5,e.z+e.dz*12)
      val end = Vec3(e.x+e.dx*e.length*0.55f-e.dz*e.bend,e.y-16,e.z+e.dz*e.length*0.55f+e.dx*e.bend)
      for i <- 1 to 8 do
        val p = start+(end-start)*(i/8f)
        assert(at(math.floor(p.x).toInt,math.floor(p.y).toInt,math.floor(p.z).toInt)==Block.Air,"entrance remains connected across chunks")
      val roofY = math.ceil(start.y+e.radius+2).toInt
      assert(at(math.floor(start.x).toInt,roofY,math.floor(start.z).toInt)!=Block.Air,"entrance descends under a natural roof")

  private def awaitValue[A](label: String)(read: => Option[A]): A =
    val deadline = System.nanoTime()+10_000_000_000L
    var result = read
    while result.isEmpty && System.nanoTime()<deadline do
      Thread.sleep(5)
      result = read
    assert(result.nonEmpty,s"Timed out waiting for $label")
    result.get

  private def clientMessage(client: GameClient,prefix: String): String =
    awaitValue(prefix) {
      var found: Option[String] = None
      var message = client.pollMessage()
      while message.nonEmpty && found.isEmpty do
        if message.get.startsWith(prefix) then found = message
        else message = client.pollMessage()
      found
    }

  def network(): Unit =
    val spawn = Vec3(0.5f,65f,0.5f)
    val snapshot = Vector.tabulate(50000)(i => (i%400+400,40,i/400+400,if i==0 then Block.Torch.id else Block.Stone.id,0,0,if i==0 then 4 else 0))
    val server = GameServer(0,42L,spawn,"Host",0,() => snapshot,() => Seq("Host"),() => spawn,() => 0f,() => 0f,ModRuntime.VanillaHash,"",() => "ENV|200|-1|0.6|0.7|0.8|0.1|0")
    val alice = GameClient("127.0.0.1",server.localPort)
    val bob = GameClient("127.0.0.1",server.localPort)
    try
      val joined = AtomicReference[Option[Boolean]](None)
      BlockboxSockets.daemonThread("test-join",() => joined.set(Some(alice.connect("Alice",ModRuntime.VanillaHash)))).start()
      awaitValue("Alice registration") { server.pollMessage().filter(_.startsWith("JOIN|Alice|")) }
      // Mutate the world while the snapshot is being sent. Live BLOC packets must
      // follow SNAPEND so they cannot invalidate the initial snapshot checksum.
      for i <- 0 until 100 do server.broadcastBlockChange(1000+i,60,0,Block.Planks.id)
      assert(awaitValue("snapshot join")(joined.get()),alice.errorMessage)
      assert(alice.isConnected)
      val environment = clientMessage(alice,"ENV|")
      assert(BlockboxNet.parseEnvironment(BlockboxNet.packet(environment)).rain()==0.6f)
      val syncedTorch=BlockboxNet.parseBlock(BlockboxNet.packet(clientMessage(alice,"BLOC|400|40|400|")),Terrain.worldHeight)
      assert(syncedTorch.torchMount==4,"joining preserves wall torch attachments in the checksummed snapshot")
      assert(bob.connect("Bob",ModRuntime.VanillaHash),bob.errorMessage)
      while bob.pollMessage().nonEmpty do ()
      alice.send("POS|Impersonated|0.5|65|0.5|90|25|9")
      val position = clientMessage(bob,"POS|Alice|")
      val parsed = BlockboxNet.parsePosition(BlockboxNet.packet(position),BlockboxNet.MAX_PLAYERS)
      assert(parsed.name=="Alice" && parsed.yaw==90f && parsed.color!=9,"server owns sender identity/color")
      alice.send("SWING|Host")
      assert(clientMessage(bob,"SWING|")=="SWING|Alice","mining gestures use authenticated connection identity")
      alice.send(BlockboxNet.block(2,64,0,Block.Stone.ordinal))
      val proposed = awaitValue("world edit proposal"){ server.pollMessage().filter(_.startsWith("EDIT|Alice|")) }
      assert(proposed.startsWith("EDIT|Alice|2|64|0|3"))
      server.broadcastBlockChange(2,64,0,Block.Stone.id)
      assert(clientMessage(bob,"BLOC|").startsWith("BLOC|2|64|0|3"),"nearby block edit synchronized")
      alice.send(BlockboxNet.block(3,64,0,Block.Torch.ordinal,0,0,2))
      val torchProposal=awaitValue("wall torch proposal"){ server.pollMessage().filter(_.startsWith("EDIT|Alice|3|64|0|")) }
      val proposedTorch=BlockboxNet.parseBlock(BlockboxNet.packet("BLOC|"+torchProposal.split("\\|",-1).drop(2).mkString("|")),Terrain.worldHeight)
      assert(proposedTorch.torchMount==2,"the server preserves the clicked attachment when forwarding a placement proposal")
      while server.pollMessage().nonEmpty do ()
      alice.send(BlockboxNet.block(10000,64,0,Block.Stone.ordinal))
      alice.send(BlockboxNet.block(3,64,0,255))
      alice.send("TIME|night")
      alice.send("POS|Alice|0.5|65|0.5|91|25|0")
      clientMessage(bob,"POS|Alice|") // Barrier: preceding packets were handled.
      val events = ArrayBuffer.empty[String]
      var event = server.pollMessage()
      while event.nonEmpty do
        events += event.get
        event = server.pollMessage()
      assert(!events.exists(e => e.startsWith("BLOC|") || e.startsWith("EDIT|") || e.startsWith("TIME|")),"far/unknown block edits and unauthorized environment packets rejected")
      // A duplicate name receives a clean error rather than a partial handshake.
      val duplicate = GameClient("127.0.0.1",server.localPort)
      assert(!duplicate.connect("Alice",ModRuntime.VanillaHash))
      assert(duplicate.errorMessage.contains("already"))
      duplicate.disconnect()
    finally
      alice.disconnect(); bob.disconnect(); server.stop()
