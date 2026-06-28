package blockbox

import scala.collection.mutable.ArrayBuffer

final case class CraftShape(width: Int, height: Int, cells: Vector[Block])
final case class CraftRecipe(width: Int, height: Int, cells: Vector[Block], output: Block, outputCount: Int, label: String)

object InventoryModel:
  val smeltableInputs: Array[Block] = Array(Block.Sand, Block.Clay, Block.Stone, Block.Wood, Block.BirchWood, Block.PineWood, Block.AcaciaWood, Block.IronOre, Block.GoldOre)
  val fuelBlocks: Array[Block] = Array(Block.Coal, Block.Wood, Block.BirchWood, Block.PineWood, Block.AcaciaWood, Block.Planks, Block.BirchPlanks, Block.PinePlanks, Block.AcaciaPlanks)

  private def craftRecipe(rows: Seq[Seq[Block]], output: Block, outputCount: Int, label: String): CraftRecipe =
    val h = rows.length.max(1)
    val w = rows.map(_.length).max.max(1)
    val cells = (0 until h).flatMap { y =>
      val row = rows(y)
      (0 until w).map(x => if x < row.length then row(x) else Block.Air)
    }.toVector
    CraftRecipe(w, h, cells, output, outputCount, label)

  private def row(blocks: Block*): Seq[Block] = blocks

  val craftingRecipes: Array[CraftRecipe] = Array(
    craftRecipe(Seq(row(Block.Wood)), Block.Planks, 4, "oak wood -> planks"),
    craftRecipe(Seq(row(Block.BirchWood)), Block.BirchPlanks, 4, "birch wood -> planks"),
    craftRecipe(Seq(row(Block.PineWood)), Block.PinePlanks, 4, "pine wood -> planks"),
    craftRecipe(Seq(row(Block.AcaciaWood)), Block.AcaciaPlanks, 4, "acacia wood -> planks"),
    craftRecipe(Seq(row(Block.Wood, Block.Wood), row(Block.Wood, Block.Wood)), Block.Planks, 18, "oak log pack -> planks"),
    craftRecipe(Seq(row(Block.BirchWood, Block.BirchWood), row(Block.BirchWood, Block.BirchWood)), Block.BirchPlanks, 18, "birch log pack -> planks"),
    craftRecipe(Seq(row(Block.PineWood, Block.PineWood), row(Block.PineWood, Block.PineWood)), Block.PinePlanks, 18, "pine log pack -> planks"),
    craftRecipe(Seq(row(Block.AcaciaWood, Block.AcaciaWood), row(Block.AcaciaWood, Block.AcaciaWood)), Block.AcaciaPlanks, 18, "acacia log pack -> planks"),
    craftRecipe(Seq(row(Block.Stone, Block.Stone), row(Block.Stone, Block.Stone)), Block.Brick, 8, "stone square -> brick"),
    craftRecipe(Seq(row(Block.Clay, Block.Clay), row(Block.Clay, Block.Clay)), Block.Brick, 6, "clay square -> brick"),
    craftRecipe(Seq(row(Block.Sand, Block.Clay), row(Block.Clay, Block.Sand)), Block.Brick, 4, "sand clay mix -> brick"),
    craftRecipe(Seq(row(Block.Sand, Block.Sand), row(Block.Sand, Block.Sand)), Block.Glass, 4, "sand square -> glass"),
    craftRecipe(Seq(row(Block.Glass, Block.Glass), row(Block.Glass, Block.Glass)), Block.Glass, 4, "glass polish"),
    craftRecipe(Seq(row(Block.Sand, Block.Sand, Block.Sand), row(Block.Sand, Block.Clay, Block.Sand), row(Block.Sand, Block.Sand, Block.Sand)), Block.Glass, 12, "clear glass batch"),
    craftRecipe(Seq(row(Block.Stone, Block.Stone, Block.Stone), row(Block.Stone, Block.Air, Block.Stone), row(Block.Stone, Block.Stone, Block.Stone)), Block.Furnace, 1, "stone furnace"),
    craftRecipe(Seq(row(Block.Planks, Block.Planks), row(Block.Planks, Block.Planks)), Block.Furnace, 1, "plank crate -> furnace"),
    craftRecipe(Seq(row(Block.BirchPlanks, Block.BirchPlanks), row(Block.BirchPlanks, Block.BirchPlanks)), Block.Furnace, 1, "birch crate -> furnace"),
    craftRecipe(Seq(row(Block.PinePlanks, Block.PinePlanks), row(Block.PinePlanks, Block.PinePlanks)), Block.Furnace, 1, "pine crate -> furnace"),
    craftRecipe(Seq(row(Block.AcaciaPlanks, Block.AcaciaPlanks), row(Block.AcaciaPlanks, Block.AcaciaPlanks)), Block.Furnace, 1, "acacia crate -> furnace"),
    craftRecipe(Seq(row(Block.Coal), row(Block.Planks)), Block.Torch, 4, "coal over oak plank -> torches"),
    craftRecipe(Seq(row(Block.Coal), row(Block.BirchPlanks)), Block.Torch, 4, "coal over birch plank -> torches"),
    craftRecipe(Seq(row(Block.Coal), row(Block.PinePlanks)), Block.Torch, 4, "coal over pine plank -> torches"),
    craftRecipe(Seq(row(Block.Coal), row(Block.AcaciaPlanks)), Block.Torch, 4, "coal over acacia plank -> torches"),
    craftRecipe(Seq(row(Block.Dirt, Block.Dirt), row(Block.Dirt, Block.Dirt)), Block.Grass, 1, "dirt patch -> grass"),
    craftRecipe(Seq(row(Block.Grass), row(Block.Dirt)), Block.Dirt, 2, "turn grass to dirt"),
    craftRecipe(Seq(row(Block.Snow, Block.Snow), row(Block.Snow, Block.Snow)), Block.Snow, 4, "packed snow"),
    craftRecipe(Seq(row(Block.Cactus, Block.Cactus), row(Block.Cactus, Block.Cactus)), Block.Cactus, 4, "cactus bundle"),
    craftRecipe(Seq(row(Block.Coal, Block.Coal), row(Block.Coal, Block.Coal)), Block.Coal, 4, "coal bundle"),
    craftRecipe(Seq(row(Block.Copper, Block.Copper), row(Block.Copper, Block.Copper)), Block.Copper, 4, "copper pack"),
    craftRecipe(Seq(row(Block.IronOre, Block.IronOre), row(Block.IronOre, Block.IronOre)), Block.IronOre, 4, "iron ore pack"),
    craftRecipe(Seq(row(Block.GoldOre, Block.GoldOre), row(Block.GoldOre, Block.GoldOre)), Block.GoldOre, 4, "gold ore pack"),
    craftRecipe(Seq(row(Block.Diamond, Block.Diamond), row(Block.Diamond, Block.Diamond)), Block.Diamond, 4, "diamond cluster"),
    craftRecipe(Seq(row(Block.IronOre, Block.Coal)), Block.IronIngot, 1, "quick iron bloom"),
    craftRecipe(Seq(row(Block.GoldOre, Block.Coal)), Block.GoldIngot, 1, "quick gold bloom"),
    craftRecipe(Seq(row(Block.IronIngot, Block.IronIngot), row(Block.IronIngot, Block.IronIngot)), Block.IronOre, 1, "iron blocky ore pack"),
    craftRecipe(Seq(row(Block.GoldIngot, Block.GoldIngot), row(Block.GoldIngot, Block.GoldIngot)), Block.GoldOre, 1, "gold blocky ore pack"),
    craftRecipe(Seq(row(Block.Brick, Block.Brick), row(Block.Brick, Block.Brick)), Block.Brick, 4, "brick stack"),
    craftRecipe(Seq(row(Block.RainbowBlock, Block.Glass), row(Block.Glass, Block.RainbowBlock)), Block.RainbowBlock, 4, "rainbow glass mix"),
    craftRecipe(Seq(row(Block.Leaves, Block.Leaves), row(Block.Leaves, Block.Leaves)), Block.Leaves, 4, "leaf bundle"),
    craftRecipe(Seq(row(Block.BirchLeaves, Block.BirchLeaves), row(Block.BirchLeaves, Block.BirchLeaves)), Block.BirchLeaves, 4, "birch leaf bundle"),
    craftRecipe(Seq(row(Block.PineLeaves, Block.PineLeaves), row(Block.PineLeaves, Block.PineLeaves)), Block.PineLeaves, 4, "pine leaf bundle"),
    craftRecipe(Seq(row(Block.AcaciaLeaves, Block.AcaciaLeaves), row(Block.AcaciaLeaves, Block.AcaciaLeaves)), Block.AcaciaLeaves, 4, "acacia leaf bundle")
  )

  def normalizeCraftGrid(cells: Array[Block]): Option[CraftShape] =
    var minX = 3; var minY = 3; var maxX = -1; var maxY = -1
    var i = 0
    while i < cells.length do
      if cells(i) != Block.Air then
        val x = i % 3; val y = i / 3
        minX = minX.min(x); minY = minY.min(y); maxX = maxX.max(x); maxY = maxY.max(y)
      i += 1
    if maxX < 0 then None
    else
      val w = maxX - minX + 1
      val h = maxY - minY + 1
      val out = ArrayBuffer.empty[Block]
      var y = minY
      while y <= maxY do
        var x = minX
        while x <= maxX do
          out += cells(y * 3 + x)
          x += 1
        y += 1
      Some(CraftShape(w, h, out.toVector))

  def craftingResult(cells: Array[Block]): Option[CraftRecipe] =
    normalizeCraftGrid(cells).flatMap { shape =>
      craftingRecipes.find(r => r.width == shape.width && r.height == shape.height && r.cells == shape.cells)
    }

  def smeltResult(input: Block): Option[(Block, Int)] = input match
    case Block.Sand => Some((Block.Glass, 1))
    case Block.Clay => Some((Block.Brick, 1))
    case Block.Stone => Some((Block.Brick, 2))
    case Block.Wood | Block.BirchWood | Block.PineWood | Block.AcaciaWood => Some((Block.Coal, 1))
    case Block.IronOre => Some((Block.IronIngot, 1))
    case Block.GoldOre => Some((Block.GoldIngot, 1))
    case _ => None

  def fuelBurnTime(fuel: Block): Float =
    if fuel == Block.Planks || fuel == Block.BirchPlanks || fuel == Block.PinePlanks || fuel == Block.AcaciaPlanks then 3f else 4f
