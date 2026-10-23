package blockbox

/** Sealed salvage in generated ruins gives exploration a material reward beyond sightseeing. */
object SupplyCaches:
  def contents(seed: Long, x: Int, y: Int, z: Int): Vector[(Block, Int)] =
    val random = new scala.util.Random(
      seed ^ (x.toLong * 341873128712L) ^ (z.toLong * 132897987541L) ^ y.toLong
    )
    val useful = Vector(Block.Torch, Block.CoalLump, Block.IronIngot, Block.Glass)
    val first = useful(random.nextInt(useful.length)) -> (3 + random.nextInt(5))
    val second = Block.GoldIngot -> (1 + random.nextInt(3))
    val rare =
      if random.nextInt(8) == 0 then Vector(Block.DiamondGem -> 1) else Vector(Block.Stick -> 6)
    Vector(first, second) ++ rare
