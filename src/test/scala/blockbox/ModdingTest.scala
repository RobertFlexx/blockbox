package blockbox

import java.nio.file.{Files,Path}
import java.util.function.Consumer
import scala.collection.mutable.ArrayBuffer

object ModdingTest:
  private def rejects(action: => Unit): Unit=
    try
      action
      throw AssertionError("Expected a rejected value")
    catch case _: IllegalArgumentException => ()

  def run(): Unit=
    val bus=ModEventBus()
    val order=ArrayBuffer.empty[Int]
    val low=bus.listen("damage",-10,new Consumer[Object]:
      def accept(e: Object): Unit=order+=3)
    bus.listen("damage",10,new Consumer[Object]:
      def accept(e: Object): Unit={order+=1; low.close()})
    bus.listen("damage",0,new Consumer[Object]:
      def accept(e: Object): Unit=order+=2)
    bus.fireDamage(ModDamageEvent(null,"player","test",3))
    assert(order.toVector==Vector(1,2),"priority ordering and safe unsubscription")
    var failures=0
    bus.onDamage(new Consumer[ModDamageEvent]:
      def accept(e: ModDamageEvent): Unit={failures+=1; throw IllegalStateException("expected test fault")})
    for _ <- 0 until 12 do bus.fireDamage(ModDamageEvent(null,"player","test",3))
    assert(failures==8,"failing listeners stop without disabling healthy listeners")
    val rules=ModRulesApi(); rules.set("gravity",20)
    val peer=ModRulesApi(); peer.receive(rules.snapshot)
    assert(peer.get("gravity")==20)
    rejects(rules.set("gravity",Float.NaN)); rejects(rules.set("reach",999))
    val original=peer.snapshot
    rejects(peer.receive("gravity=25,jumpSpeed=NaN"))
    assert(peer.snapshot==original,"invalid rule packets have no partial effect")
    val scheduler=ModScheduler()
    var first=0; var second=0
    scheduler.runLater(0,new Runnable:
      def run(): Unit={first+=1; scheduler.runLater(0,new Runnable { def run(): Unit={second+=1} })})
    scheduler.tick(0.1f); assert(first==1 && second==0)
    scheduler.tick(0.1f); assert(second==1,"jobs scheduled from callbacks run on the next tick")
    val worldgen=ModWorldgenApi()
    worldgen.onChunk(new Consumer[ModChunkEvent]:
      def accept(c: ModChunkEvent): Unit={c.set(1,12,1,Block.Glass); throw IllegalStateException("expected rollback")})
    val data=new Array[Byte](16*16*128)
    worldgen.decorate(1,0,0,data)
    assert(data.forall(_==0),"failed world generators cannot leave partial chunk edits")
    val temporary=Files.createTempDirectory("blockbox-mod-integration")
    blockbox.io.BlockboxFiles.copyRecursive(Path.of("examples/mods/skybound"),temporary.resolve("skybound"))
    val game=Blockbox()
    val manager=ModManager(game,temporary.toFile)
    try
      manager.loadAll()
      assert(manager.loadedMods.count(_.loaded)==1,manager.loadedMods.map(_.status).mkString(","))
      val moon=manager.api.block("skybound:moonstone")
      assert(moon!=Block.Air && moon!=Block.Stone && moon.solid && CustomBlocks.definition(moon).get.emission==11)
      assert(Block.find("skybound:moonstone").contains(moon) && Block.fromId(moon.id)==moon)
      assert(!Block.all.contains(Block.Custom00),"unregistered voxel slots stay out of the inventory catalog")
      val grid=Array.fill(9)(Block.Air); grid(0)=Block.Coal; grid(1)=Block.Glass
      assert(manager.recipes.result(grid).exists(r => r.output==moon && r.outputCount==2),"Groovy recipe participates in crafting")
      val damage=manager.events.fireDamage(ModDamageEvent(manager.api,"Player","falling",4))
      assert(damage.amount==2,"Groovy event changes actual damage")
      assert(manager.rules.get("gravity")==27)
      val gen=TerrainGenerator(2468,(cx,cz,blocks) => manager.worldgen.decorate(2468,cx,cz,blocks))
      val chunk=gen.fillChunkBlocks(0,0)
      assert(chunk.contains(moon.id),"registered Groovy generator writes custom saved voxel IDs")
      assert(java.util.Arrays.equals(chunk,gen.fillChunkBlocks(0,0)),"custom generation is deterministic")
      val packed=BlockboxNetRoundtrip(moon)
      assert(packed==moon,"custom voxel survives network serialization")
    finally
      manager.shutdown(); blockbox.io.BlockboxFiles.deleteRecursive(temporary)
    println("[modding] Groovy conversion, custom voxels, crafting, generation, rules and event isolation passed")

  private def BlockboxNetRoundtrip(block: Block): Block=
    val packet=blockbox.net.BlockboxNet.block(2,14,8,block.ordinal,0,0,0)
    Block.fromId(blockbox.net.BlockboxNet.parseBlock(blockbox.net.BlockboxNet.packet(packet),128).blockId.toByte)
