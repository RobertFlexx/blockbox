package blockbox

import scala.collection.mutable
import java.io.*

object WorldPolishTest:
  import InteractionSystemsTest.{call, field, set}
  def run(): Unit =
    leaves(); inventories(); grassAndSnow()
    println(
      "[world-polish] leaf support/decay/save boundaries, real furnace stacks, creative pickup, grass spread and snow accumulation passed"
    )

  private def leaves(): Unit =
    val blocks = mutable.HashMap.empty[(Int, Int, Int), Block]
    def at(x: Int, y: Int, z: Int): Block = blocks.getOrElse((x, y, z), Block.Air)
    val decay = LeafDecay()
    blocks((0, 5, 0)) = Block.Wood
    for x <- 1 to 10 do blocks((x, 5, 0)) = Block.Leaves
    assert(decay.supported(5, 5, 0, at, (_, _) => true))
    assert(
      !decay.supported(10, 5, 0, at, (_, _) => true),
      "a remote log cannot support an entire floating forest"
    )
    assert(
      decay.supported(10, 5, 0, at, (x, _) => x < 11),
      "unloaded neighbors cannot destroy a canopy"
    )
    blocks.remove((0, 5, 0)); decay.disturb(0, 5, 0, at)
    val bytes = ByteArrayOutputStream(); decay.write(DataOutputStream(bytes))
    val restored = LeafDecay();
    restored.read(DataInputStream(ByteArrayInputStream(bytes.toByteArray)))
    for _ <- 0 until 100 do
      restored.update(
        0.25f,
        at,
        (_, _) => true,
        (x, _, _) => x == 4,
        (x, y, z) => blocks.remove((x, y, z))
      )
    assert(
      blocks.size == 1 && blocks((4, 5, 0)) == Block.Leaves,
      "queued decay survives reload, reaches the far canopy and keeps placed leaves"
    )
    for block <- FloraModel.blocks; variant <- 0 until 8 do
      assert(
        FloraModel
          .vertices(block, variant)
          .forall(v => v.x >= 0 && v.x <= 1 && v.z >= 0 && v.z <= 1 && v.y >= -0.04f && v.y <= 1),
        "varied plants stay within their voxel rather than clipping through nearby walls"
      )

  private def inventories(): Unit =
    val game = Blockbox(); set(game, "soundEnabled", false)
    set(game, "gameMode", GameMode.Creative)
    val hot = field[Array[Block]](game, "hotbarBlocks")
    val counts = field[Array[Int]](game, "hotbarCounts")
    hot(0) = Block.Dirt; counts(0) = 7
    call(game, "takeHotbarSlot", Int.box(0), Boolean.box(true))
    assert(counts(0) == 3 && field[Int](game, "heldInventoryCount") == 4)
    call(game, "handleBackpackSlotClick", Int.box(1), Boolean.box(false))
    assert(
      field[Array[Int]](game, "backpackCounts")(1) == 4 && field[Int](
        game,
        "heldInventoryCount"
      ) == 0,
      "creative inventory moves physical stacks rather than copying or deleting them"
    )
    val drops = field[DroppedItems](game, "droppedItems")
    val feet = field[Vec3](game, "camera") - Vec3(0, 1, 0)
    val chunk = Chunk(
      Math.floorDiv(feet.x.toInt, 16),
      Math.floorDiv(feet.z.toInt, 16),
      null,
      TerrainGenerator(42),
      new Array[Byte](16 * 16 * Terrain.worldHeight)
    )
    field[mutable.AnyRefMap[(Int, Int), Chunk]](game, "chunks")((chunk.cx, chunk.cz)) = chunk
    val item = drops.spawn(Block.Sand, 5, feet).get; item.delay = 0
    call(game, "updateDroppedItems", Float.box(0f))
    assert(
      drops.entities.isEmpty && hot.zip(counts).exists((b, n) => b == Block.Sand && n == 5),
      "creative players actually collect physical drops into inventory"
    )
    set(game, "furnaceSlots", true); set(game, "furnaceInput", Block.IronOre)
    set(game, "furnaceInputCount", 9); set(game, "furnaceFuel", Block.CoalLump)
    set(game, "furnaceFuelCount", 3); set(game, "snowCover", 0.7f)
    call(game, "setHeldItem", Block.Glass, Int.box(6), Boolean.box(true))
    val bytes = ByteArrayOutputStream()
    call(game, "writeWorldExtras", DataOutputStream(bytes))
    val saved = Blockbox()
    call(saved, "readWorldExtras", DataInputStream(ByteArrayInputStream(bytes.toByteArray)))
    assert(
      field[Int](saved, "furnaceInputCount") == 9 && field[Int](saved, "furnaceFuelCount") == 3
    )
    assert(
      field[Block](saved, "heldInventoryBlock") == Block.Glass && field[Int](
        saved,
        "heldInventoryCount"
      ) == 6,
      "autosave includes a detached cursor stack and both furnace buffers"
    )
    assert(math.abs(field[Float](saved, "snowCover") - 0.7f) < 0.001f)
    call(saved, "clearHeldItem")
    call(saved, "handleFurnaceSlot", Boolean.box(false), Boolean.box(true), Boolean.box(false))
    assert(
      field[Int](saved, "heldInventoryCount") == 5 && field[Int](saved, "furnaceInputCount") == 4,
      "right-click splits a furnace input like an ordinary inventory slot"
    )

  private def grassAndSnow(): Unit =
    assert(ToolSystem.drop(Block.Grass) == (Block.Dirt, 1), "grass blocks drop dirt")
    val game = Blockbox(); set(game, "soundEnabled", false)
    val chunk =
      Chunk(0, 0, null, TerrainGenerator(42), new Array[Byte](16 * 16 * Terrain.worldHeight))
    field[mutable.AnyRefMap[(Int, Int), Chunk]](game, "chunks")((0, 0)) = chunk
    chunk.setBlock(5, 30, 5, Block.Dirt); chunk.setBlock(6, 30, 5, Block.Grass)
    assert(call(game, "canGrowGrassAt", Int.box(5), Int.box(30), Int.box(5)) == true)
    chunk.setBlock(5, 31, 5, Block.Water)
    assert(call(game, "canGrowGrassAt", Int.box(5), Int.box(30), Int.box(5)) == false)
    chunk.setBlock(5, 31, 5, Block.Stone)
    assert(call(game, "canGrowGrassAt", Int.box(5), Int.box(30), Int.box(5)) == false)
    var cover = 0f
    for _ <- 0 until 400 do cover = SnowCover.advance(cover, 1f, 0.25f)
    assert(cover > 0.99f && SnowCover.level(cover) == 8, "sustained snowfall settles gradually")
    val after = SnowCover.advance(cover, 0, 1f)
    assert(after < cover && after > 0.99f, "snow thaws slowly instead of flashing away")
    assert(SnowCover.cold(TerrainBiome.SnowyTaiga) && !SnowCover.cold(TerrainBiome.Badlands))
