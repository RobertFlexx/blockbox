package blockbox

/** Bounded voxel lighting with a full propagation halo across all eight neighbors. */
object BlockLight:
  final case class Source(x: Int, y: Int, z: Int, level: Int)
  def emission(block: Block): Int = block match
    case Block.Torch      => 15
    case Block.FurnaceLit => 13
    case _                => CustomBlocks.definition(block).map(_.emission).getOrElse(0)

  final class Field(
      private val light: Array[Byte],
      val minY: Int,
      val maxY: Int,
      private val size: Int,
      private val min: Int
  ):
    def isEmpty: Boolean = light.isEmpty
    def level(x: Int, y: Int, z: Int): Int =
      if light.isEmpty || x < min || z < min || x >= min + size || z >= min + size || y < minY || y > maxY
      then 0
      else light(((y - minY) * size + z - min) * size + x - min).toInt & 0xff

  def build(
      sources: Seq[Source],
      chunkSize: Int,
      height: Int,
      at: (Int, Int, Int) => Block
  ): Field =
    val min = -15; val max = chunkSize + 14; val size = max - min + 1
    def axisDistance(v: Int): Int =
      if v < -1 then -1 - v else if v > chunkSize then v - chunkSize else 0
    val relevant = sources.filter(s =>
      s.level > 0 && s.y >= 0 && s.y < height && axisDistance(s.x) + axisDistance(s.z) < s.level
    )
    if relevant.isEmpty then return Field(Array.emptyByteArray, 0, -1, size, min)
    val minY = (relevant.map(s => s.y - s.level + 1).min).max(0)
    val maxY = (relevant.map(s => s.y + s.level - 1).max).min(height - 1)
    val light = new Array[Byte](size * size * (maxY - minY + 1))
    var queue = new Array[Int](light.length.min(8192).max(256))
    var head = 0; var tail = 0
    def enqueue(x: Int, y: Int, z: Int, level: Int): Unit =
      if x >= min && x <= max && z >= min && z <= max && y >= minY && y <= maxY then
        val i = ((y - minY) * size + z - min) * size + x - min
        if level > (light(i).toInt & 0xff) then
          light(i) = level.toByte
          if tail == queue.length then queue = java.util.Arrays.copyOf(queue, queue.length * 2)
          queue(tail) = i; tail += 1
    relevant.sortBy(-_.level).foreach(s => enqueue(s.x, s.y, s.z, s.level.min(15)))
    while head < tail do
      val i = queue(head); head += 1
      val x = i % size + min; val z = (i / size) % size + min; val y = i / (size * size) + minY
      val next = (light(i).toInt & 0xff) - 1
      if next > 0 then
        def spread(nx: Int, ny: Int, nz: Int): Unit =
          if nx >= min && nx <= max && nz >= min && nz <= max && ny >= minY && ny <= maxY then
            val block = at(nx, ny, nz)
            if !block.solid || block.translucent || block.cutout then
              enqueue(nx, ny, nz, if block == Block.Water || block.cutout then next - 1 else next)
        spread(x - 1, y, z); spread(x + 1, y, z); spread(x, y - 1, z); spread(x, y + 1, z);
        spread(x, y, z - 1); spread(x, y, z + 1)
    Field(light, minY, maxY, size, min)
