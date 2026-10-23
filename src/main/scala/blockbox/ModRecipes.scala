package blockbox

/** Per-game recipe overrides, evaluated before the vanilla recipes. */
final class ModRecipes:
  private val entries = scala.collection.mutable.LinkedHashMap.empty[String, CraftRecipe]
  private val removed = scala.collection.mutable.HashSet.empty[String]
  private[blockbox] def checkpoint(): () => Unit =
    val saved = entries.toVector; val hidden = removed.toSet
    () => { entries.clear(); entries ++= saved; removed.clear(); removed ++= hidden }
  def register(id: String, rows: Vector[Vector[Block]], output: Block, count: Int): Unit =
    require(id != null && id.contains(":"), "Recipe ID must be namespaced")
    require(
      rows.nonEmpty && rows.length <= 3 && rows.forall(r =>
        r.nonEmpty && r.length == rows.head.length && r.length <= 3
      ),
      "Recipes must be rectangular and at most 3 x 3"
    )
    require(
      output != null && output != Block.Air && count >= 1 && count <= 64,
      "Invalid recipe output"
    )
    require(rows.flatten.exists(_ != Block.Air), "Recipe needs an ingredient")
    require(entries.size < 512 || entries.contains(id), "Custom recipe limit is 512")
    entries(id) = CraftRecipe(rows.head.length, rows.length, rows.flatten, output, count, id)
    removed.remove(id)
  def remove(id: String): Unit =
    entries.remove(id); removed.add(id)
  def result(grid: Array[Block]): Option[CraftRecipe] =
    InventoryModel.normalizeCraftGrid(grid).flatMap { shape =>
      entries.valuesIterator
        .find(r => r.width == shape.width && r.height == shape.height && r.cells == shape.cells)
        .orElse(InventoryModel.craftingResult(grid).filterNot(r => removed.contains(r.label)))
    }
