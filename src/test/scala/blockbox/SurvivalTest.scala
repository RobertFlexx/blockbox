package blockbox

import scala.math.*

import java.io.*
import scala.collection.mutable

object SurvivalTest:
  def run(): Unit =
    tools(); breathing(); ores(); zombies(); combat(); furnace(); quickMove(); saves()
    println("[survival] tool progression, recipes, breathing, ore veins, furnace, shift-click, zombies, PvP and save compatibility passed")

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
    val barricade=(x: Int,y: Int,z: Int) => if z== -3 && math.abs(x)<=2 && y>=1 && y<=3 then Block.Stone else floor(x,y,z)
    val hunter=ZombieSystem.Zombie(3,Vec3(0.5f,1,0.5f))
    val quarry=ZombieSystem.Target("Quarry",Vec3(0.5f,2.62f,-7))
    for _ <- 0 until 340 do ZombieSystem.update(hunter,0.05f,Seq(quarry),false,false,barricade)
    assert(hunter.pos.z< -5.5f && !ZombieSystem.collides(hunter.pos,barricade),s"zombies navigate around a tall barricade: ${hunter.pos}")
    // A moving target changes heading without instantaneous grid-aligned turns.
    val smooth=ZombieSystem.Zombie(40,Vec3(0.5f,1,0.5f))
    var previousVelocity=Vec3(0,0,0)
    for i <- 0 until 180 do
      val before=smooth.pos; val yaw=smooth.yaw
      val target=ZombieSystem.Target("Moving",Vec3(5f+sin(i*0.015).toFloat*2f,2.62f,-8))
      ZombieSystem.update(smooth,1f/60f,Seq(target),false,false,floor)
      val velocity=(smooth.pos-before)*60f
      assert(sqrt((velocity-previousVelocity).lengthSquared)<0.4f,"steering accelerates continuously")
      assert(abs(smooth.yaw-yaw)<=3.01f,"heading changes are bounded independently of waypoints")
      previousVelocity=velocity
    // A deep lake has no nearby solid support. Zombies must rise, settle with
    // their heads above water, and continue pursuing across its surface.
    def lake(x: Int,y: Int,z: Int): Block = if y<=0 then Block.Stone else if y<=4 then Block.Water else Block.Air
    for step <- Seq(1f/30f,1f/60f,1f/120f) do
      val swimmer=ZombieSystem.Zombie(41,Vec3(0.5f,1,0.5f))
      val target=ZombieSystem.Target("Swimmer",Vec3(0.5f,5.37f,-15))
      var low=Float.MaxValue; var high=Float.MinValue
      for i <- 0 until (9f/step).toInt do
        val before=swimmer.pos
        ZombieSystem.update(swimmer,step,Seq(target),true,false,lake)
        assert(!ZombieSystem.collides(swimmer.pos,lake),"swimming respects body collisions")
        assert((swimmer.pos-before).lengthSquared<0.025f,"buoyancy never teleports a swimmer")
        if i*step>6f then {low=low.min(swimmer.pos.y);high=high.max(swimmer.pos.y)}
      assert(abs(swimmer.pos.y-3.75f)<0.025f && high-low<0.035f,s"stable waterline at $step: $low..$high")
      assert(swimmer.pos.z< -8f && swimmer.swimming>0.98f && swimmer.burning==0,"surface pursuit and swimming animation")
    val flowing=ZombieSystem.waterSurface(Vec3(0.5f,3.5f,0.5f),lake,(_,_,_) => 2).get
    assert(abs(flowing-(4f+WaterSurface.height(2)))<0.001f,"buoyancy uses flowing-water surface heights")
    def shore(x: Int,y: Int,z: Int): Block =
      if y<=0 || z< -4 && y<=4 then Block.Stone else if y<=4 then Block.Water else Block.Air
    val climber=ZombieSystem.Zombie(42,Vec3(0.5f,3.75f,0.5f))
    val bank=ZombieSystem.Target("Bank",Vec3(0.5f,6.62f,-9))
    for _ <- 0 until 500 do ZombieSystem.update(climber,0.02f,Seq(bank),false,false,shore)
    assert(climber.pos.z< -6f && climber.pos.y>=4.99f,s"swimmers climb a one-block shore: ${climber.pos}")
    def roof(x: Int,y: Int,z: Int): Block = if y==4 then Block.Stone else lake(x,y,z)
    val enclosed=ZombieSystem.Zombie(43,Vec3(0.5f,1,0.5f))
    for _ <- 0 until 300 do
      ZombieSystem.update(enclosed,0.02f,Seq.empty,false,false,roof)
      assert(!ZombieSystem.collides(enclosed.pos,roof),"buoyancy cannot push a zombie through a ceiling")
    val crowd=(0 until 20).map(i => ZombieSystem.Zombie(100+i,Vec3(0.5f,1,0.5f)))
    ZombieSystem.separate(crowd,floor,1f/60f)
    assert(crowd.forall(z => (z.pos-Vec3(0.5f,1,0.5f)).lengthSquared<=0.00014f),"crowd separation is bounded per frame, including dense groups")
    hunter.heldItem=Block.DiamondSword
    assert(ZombieSystem.parseSnapshot(ZombieSystem.snapshot(Seq(hunter))).head._7==Block.DiamondSword,"equipment survives mob synchronization and persistence")
    assert(ZombieSystem.parseSnapshot("MOBS|1,0,1,0,0,20,0,0").head._7==Block.Air,"older mob saves remain readable")
    val rng=scala.util.Random(123)
    val equipment=(0 until 10000).map(_ => ZombieSystem.spawnEquipment(rng)).filter(_!=Block.Air)
    assert(equipment.length>650 && equipment.length<950 && equipment.distinct.length==16,"rare equipped spawns can use every tool and weapon tier")

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

  def quickMove(): Unit =
    val game=Blockbox(); set(game,"soundEnabled",false)
    val hotbar=get[Array[Block]](game,"hotbarBlocks"); val hotCounts=get[Array[Int]](game,"hotbarCounts")
    val pack=get[Array[Block]](game,"backpackBlocks"); val packCounts=get[Array[Int]](game,"backpackCounts")
    for i <- hotbar.indices do { hotbar(i)=Block.Air; hotCounts(i)=0 }
    for i <- pack.indices do { pack(i)=Block.Air; packCounts(i)=0 }
    hotbar(2)=Block.Stone; hotCounts(2)=40; pack(5)=Block.Stone; packCounts(5)=60
    method(game,"quickMoveHotbar",Int.box(2))
    assert(packCounts(5)==64 && pack(0)==Block.Stone && packCounts(0)==36 && hotbar(2)==Block.Air,
      "shift-click tops up a matching stack before filling the first empty slot")
    method(game,"quickMoveBackpack",Int.box(0))
    assert(hotbar(0)==Block.Stone && hotCounts(0)==36 && pack(0)==Block.Air,"backpack stacks move to the hotbar")
    val cells=get[Array[Block]](game,"craftGridBlocks"); val cellCounts=get[Array[Int]](game,"craftGridCounts")
    cells(0)=Block.BirchPlanks; cellCounts(0)=3; cells(3)=Block.BirchPlanks; cellCounts(3)=3
    method(game,"tryCraftGrid",java.lang.Boolean.TRUE)
    assert(hotbar(8)==Block.Stick && hotCounts(8)==12 && cells(0)==Block.Air && cells(3)==Block.Air,
      "shift-crafting spends every ingredient and fills the hotbar from the right")
    set(game,"gameMode",GameMode.Creative)
    method(game,"creativeQuickGive",Block.Torch)
    assert(hotbar(1)==Block.Torch && hotCounts(1)==64,"creative shift-click gives a full stack in the first free hotbar slot")

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
