package blockbox

import java.io.*
import scala.collection.mutable

object SurvivalTest:
  def run(): Unit =
    tools(); breathing(); ores(); zombies(); combat(); furnace(); saves()
    println("[survival] tool progression, recipes, breathing, ore veins, furnace, zombies, PvP and save compatibility passed")

  private def grid(rows: Seq[Seq[Block]]): Array[Block] =
    val result=Array.fill(9)(Block.Air)
    rows.zipWithIndex.foreach((row,y) => row.zipWithIndex.foreach((block,x) => result(y*3+x)=block))
    result

  def tools(): Unit =
    import Block.*
    assert(Block.Custom00.ordinal==35 && Block.Custom31.ordinal==66 && Stick.ordinal==67,"legacy IDs are stable")
    for item <- ToolSystem.materials.flatMap(_._2) do
      assert(CustomBlocks.registered(item) && !CustomBlocks.isSlot(item) && Block.isItem(item) && !item.solid)
      assert(Block.fromId(item.id)==item && Block.find(item.toString).contains(item))
      val mesh=ItemModel.mesh(item)
      assert(mesh.nonEmpty && mesh.length%4==0)
      assert(mesh.map(_.z).min<0 && mesh.map(_.z).max>0,"tools have real thickness")
    val plank=BirchPlanks; val a=Air; val s=Stick
    assert(InventoryModel.craftingResult(grid(Seq(Seq(plank),Seq(plank)))).exists(r => r.output==Stick && r.outputCount==4))
    for (material,items) <- ToolSystem.materials do
      assert(InventoryModel.craftingResult(grid(Seq(Seq(material,material,material),Seq(a,s,a),Seq(a,s,a)))).exists(_.output==items(0)))
      assert(InventoryModel.craftingResult(grid(Seq(Seq(material,material),Seq(s,material),Seq(s,a)))).exists(_.output==items(1)),"axes also match mirrored recipes")
    assert(InventoryModel.craftingResult(grid(Seq(Seq(IronOre,Coal)))).isEmpty,"iron requires the furnace")
    assert(!InventoryModel.craftingResult(grid(Seq(Seq(plank,plank),Seq(plank,plank)))).exists(_.output==Furnace))
    assert(!ToolSystem.canHarvest(Stone,Air) && ToolSystem.canHarvest(Stone,WoodenPickaxe))
    assert(!ToolSystem.canHarvest(IronOre,WoodenPickaxe) && ToolSystem.canHarvest(IronOre,StonePickaxe))
    assert(!ToolSystem.canHarvest(Diamond,StonePickaxe) && ToolSystem.canHarvest(Diamond,IronPickaxe))
    assert(ToolSystem.miningSpeed(Wood,IronAxe)>ToolSystem.miningSpeed(Wood,IronPickaxe)*3)
    assert(ToolSystem.miningSpeed(Sand,DiamondShovel)>ToolSystem.miningSpeed(Sand,DiamondSword)*3)
    assert(ToolSystem.drop(Diamond)==(DiamondGem,1) && ToolSystem.drop(Coal)==(CoalLump,1))
    val (r,g,b)=WaterAppearance.tint((1f,1f,1f),3f)
    assert(r<g && g<b && b<1f,"water absorbs red before green and blue")

  def breathing(): Unit =
    val air=AirSupply()
    assert(air.update(14,true,true)==0 && air.bubbles==1)
    assert(air.update(1,true,true)==0 && air.bubbles==0)
    assert(air.update(0.9f,true,true)==0 && air.update(0.1f,true,true)==2)
    assert(air.update(3,true,true)==6,"drowning deals two health every second")
    air.update(1,false,true)
    assert(air.remaining==5 && air.bubbles==4)
    air.update(0,true,false)
    assert(air.remaining==air.capacity,"creative has unlimited breath")
    for step <- Seq(0.05f,0.1f,0.25f) do
      air.reset()
      val damage=(0 until math.round(18f/step)).map(_ => air.update(step,true,true)).sum
      assert(damage>=4 && damage<=6,"breathing is stable across frame rates")

  def ores(): Unit =
    val counts=mutable.Map.empty[Block,Int].withDefaultValue(0)
    var neighbors=0; var samples=0
    for x <- -64 until 64; z <- -64 until 64; y <- 5 until 64 do
      OreDistribution.at(8675309L,x,y,z,90).foreach { ore =>
        counts(ore)+=1; samples+=1
        if OreDistribution.at(8675309L,x+1,y,z,90).contains(ore) || OreDistribution.at(8675309L,x,y,z+1,90).contains(ore) then neighbors+=1
        if ore==Block.Diamond then assert(y<=20)
        if ore==Block.GoldOre then assert(y<=34)
      }
    assert(counts(Block.Coal)>counts(Block.IronOre) && counts(Block.IronOre)>counts(Block.GoldOre) && counts(Block.GoldOre)>counts(Block.Diamond),counts.toString)
    assert(counts(Block.Diamond)>0 && counts(Block.Diamond)<1200,s"diamonds are discoverable but rare: $counts")
    assert(neighbors>samples*0.65f,"ores occur in mineable clusters")
    for x <- -17 to 17; y <- 0 to 4 do assert(OreDistribution.at(0,x,y,-16,80).isEmpty)

  def zombies(): Unit =
    def floor(x: Int,y: Int,z: Int): Block = if y<=0 then Block.Stone else Block.Air
    assert(ZombieSystem.canSpawn(Vec3(0.5f,1,0.5f),0.18f,15,0,floor))
    assert(!ZombieSystem.canSpawn(Vec3(0.5f,1,0.5f),1,15,0,floor))
    assert(ZombieSystem.canSpawn(Vec3(0.5f,1,0.5f),1,0,0,floor),"caves can spawn zombies in daylight")
    assert(!ZombieSystem.canSpawn(Vec3(0.5f,1,0.5f),0.18f,0,12,floor),"torches protect a room")
    val mob=ZombieSystem.Zombie(1,Vec3(0.5f,1,0.5f))
    val player=ZombieSystem.Target("Player",Vec3(0.5f,2.62f,-9))
    for _ <- 0 until 50 do ZombieSystem.update(mob,0.05f,Seq(player),false,false,floor)
    assert(mob.pos.z< -2 && mob.pos.y>=0.99f && mob.health==20,"zombies walk toward the player and stay above the floor")
    for _ <- 0 until 20 do ZombieSystem.update(mob,0.05f,Seq.empty,true,false,floor)
    assert(mob.health<18 && mob.burning>0,"direct sun ignites zombies")
    ZombieSystem.update(mob,0.05f,Seq.empty,true,true,floor)
    assert(mob.burning==0,"rain extinguishes daylight fire")
    val snapshot=ZombieSystem.parseSnapshot(ZombieSystem.snapshot(Seq(mob)))
    assert(snapshot.head._1==mob.id && snapshot.head._2==mob.pos)
    assert(ZombieSystem.parseSnapshot("MOBS").isEmpty)
    assert(ZombieSystem.rayDistance(Vec3(mob.pos.x,2,mob.pos.z+3),Vec3(0,0,-1),mob,4).nonEmpty)
    val wall=(x: Int,y: Int,z: Int) => if z== -2 then Block.Stone else floor(x,y,z)
    assert(!ZombieSystem.visible(Vec3(0,2,0),Vec3(0,2,-4),wall))

  private def method(game: Blockbox,name: String,args: AnyRef*): Any =
    val m=game.getClass.getDeclaredMethods.find(m => m.getName==name && m.getParameterCount==args.length).get
    m.setAccessible(true); m.invoke(game,args*)
  private def set(game: Blockbox,name: String,value: Any): Unit =
    val f=game.getClass.getDeclaredField(name); f.setAccessible(true); f.set(game,value)
  private def get[A](game: Blockbox,name: String): A =
    val f=game.getClass.getDeclaredField(name); f.setAccessible(true); f.get(game).asInstanceOf[A]

  def combat(): Unit =
    val eye=Vec3(0.5f,242.62f,0.5f); val target=eye+Vec3(0,0,-3)
    val air=(_: Int,_: Int,_: Int) => Block.Air
    assert(CombatSystem.canHit(eye,0,0,target,air))
    assert(!CombatSystem.canHit(eye,180,0,target,air),"looking away cannot hit a nearby player")
    assert(!CombatSystem.canHit(eye,0,0,eye+Vec3(0,0,-5),air),"melee reach is bounded")
    assert(!CombatSystem.canHit(eye,0,0,target,(_,_,z) => if z== -1 then Block.Stone else Block.Air),"solid walls block melee")
    val game=Blockbox(); set(game,"soundEnabled",false); set(game,"camera",eye)
    val remotes=get[mutable.HashMap[String,RemotePlayer]](game,"remotePlayers")
    val attacker=RemotePlayer("Attacker",target,target,180,180,0,0,0,0,Block.IronSword)
    remotes(attacker.name)=attacker
    method(game,"hitPlayer",attacker.name,"Player")
    assert(get[Float](game,"playerHealth")==14f && get[Vec3](game,"playerKnockback").lengthSquared>1,"swords hurt and knock back the victim")
    method(game,"hitPlayer",attacker.name,"Player")
    assert(get[Float](game,"playerHealth")==14f,"the host enforces an attacker cooldown")
    get[mutable.HashMap[String,Double]](game,"lastCombatHit").clear()
    set(game,"gameMode",GameMode.Creative)
    method(game,"hitPlayer",attacker.name,"Player")
    assert(get[Float](game,"playerHealth")==14f,"creative players are immune")

  def furnace(): Unit =
    val game=Blockbox()
    val hotbar=get[Array[Block]](game,"hotbarBlocks"); val counts=get[Array[Int]](game,"hotbarCounts")
    hotbar(0)=Block.IronOre; counts(0)=3; hotbar(1)=Block.CoalLump; counts(1)=1
    set(game,"soundEnabled",false); set(game,"furnaceInput",Block.IronOre)
    method(game,"smeltFurnace")
    method(game,"updateFurnace",Float.box(3f))
    assert(get[Int](game,"furnaceOutputCount")==0 && counts(0)==3,"smelting takes real time")
    method(game,"updateFurnace",Float.box(1f))
    assert(get[Block](game,"furnaceOutput")==Block.IronIngot && get[Int](game,"furnaceOutputCount")==1)
    assert(counts(0)==2 && counts(1)==0 && get[Float](game,"furnaceFuelRemaining")==7,"one ore and one coal are spent exactly once")
    method(game,"takeFurnaceOutput")
    assert(method(game,"totalItemCount",Block.IronIngot)==Int.box(1))

  def saves(): Unit =
    val game=Blockbox()
    val journal=get[FieldJournal](game,"fieldJournal")
    journal.collected(Block.IronPickaxe,1); journal.collected(Block.DiamondGem,3); journal.defeatedZombie()
    val raw=ByteArrayOutputStream(); val out=DataOutputStream(raw)
    journal.write(out); journal.writeProgress(out)
    val read=FieldJournal(); val in=DataInputStream(ByteArrayInputStream(raw.toByteArray))
    read.read(in); read.readProgress(in)
    assert(read.entries==journal.entries,"new survival goals survive the optional save tail")
    val hotbar=get[Array[Block]](game,"hotbarBlocks"); val counts=get[Array[Int]](game,"hotbarCounts")
    hotbar(0)=Block.DiamondPickaxe; counts(0)=1
    val bytes=ByteArrayOutputStream()
    method(game,"writeWorldExtras",DataOutputStream(bytes))
    val loaded=Blockbox()
    method(loaded,"readWorldExtras",DataInputStream(ByteArrayInputStream(bytes.toByteArray)))
    assert(get[Array[Block]](loaded,"hotbarBlocks")(0)==Block.DiamondPickaxe)
