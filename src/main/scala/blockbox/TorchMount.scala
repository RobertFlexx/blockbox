package blockbox

/** The supporting block is part of a torch's saved state, never guessed by meshing. */
enum TorchMount(val id: Int, val support: (Int, Int, Int)):
  case Floor extends TorchMount(0, (0, -1, 0))
  case EastWall extends TorchMount(1, (1, 0, 0))
  case WestWall extends TorchMount(2, (-1, 0, 0))
  case SouthWall extends TorchMount(3, (0, 0, 1))
  case NorthWall extends TorchMount(4, (0, 0, -1))

  def supported(x: Int, y: Int, z: Int, at: (Int, Int, Int) => Block): Boolean =
    TorchMount.canSupport(at(x + support._1, y + support._2, z + support._3))

object TorchMount:
  private val all = values
  def fromId(id: Int): TorchMount = if id>=0 && id<all.length then all(id) else TorchMount.Floor

  def fromNormal(normal: (Int, Int, Int)): Option[TorchMount] = normal match
    case (0, 1, 0) => Some(TorchMount.Floor)
    case (-1, 0, 0) => Some(TorchMount.EastWall)
    case (1, 0, 0) => Some(TorchMount.WestWall)
    case (0, 0, -1) => Some(TorchMount.SouthWall)
    case (0, 0, 1) => Some(TorchMount.NorthWall)
    case _ => None

  def canSupport(block: Block): Boolean = block.solid && !block.cutout && block != Block.Cactus

