package blockbox

/** Worker-side sky light for chunk meshing.
  *
  * The mesher copies a chunk and its eight neighbours into one dense array (x and z in -16..31
  * local coordinates). Sky light starts at 15 above every column, falls straight down without loss,
  * is dimmed one level per foliage or water block, and then floods sideways and downward one level
  * per step, so overhangs, cave mouths and tree canopies get soft Minecraft-style shadow instead of
  * a binary "is the column open" test.
  */
private[blockbox] object ChunkLighting:
  val span: Int = Terrain.chunkSize * 3

  final class Scratch:
    val blocks = new Array[Byte](span * span * Terrain.worldHeight)
    val sky = new Array[Byte](span * span * Terrain.worldHeight)
    val shadowTop = new Array[Int](span * span)
    val litBottom = new Array[Int](span * span)
    var queue = new Array[Int](16384)

  val scratch: ThreadLocal[Scratch] = ThreadLocal.withInitial(() => Scratch())

  /** Fills `s.sky` from `s.blocks`. `cost(id)` is 0 for clear blocks, 15 for opaque ones and the
    * per-block dimming for everything in between.
    */
  def floodSky(s: Scratch, cost: Array[Int], height: Int): Unit =
    val region = s.blocks
    val sky = s.sky
    val layer = span * span
    java.util.Arrays.fill(sky, 0.toByte)
    var column = 0
    while column < layer do
      var level = 15
      var top = -1
      var bottom = height
      var y = height - 1
      var i = y * layer + column
      while y >= 0 && level > 0 do
        val c = cost(region(i) & 0xff)
        level = if c >= 15 then 0 else math.max(0, level - c)
        if level < 15 && top < 0 then top = y
        sky(i) = level.toByte
        if level > 1 then bottom = y
        y -= 1
        i -= layer
      s.shadowTop(column) = top
      s.litBottom(column) = bottom
      column += 1

    // Only cells that are brighter than the same height in a neighbouring column can
    // push light anywhere, so seeding is limited to that band of each column.
    var queue = s.queue
    var tail = 0
    column = 0
    while column < layer do
      val bottom = s.litBottom(column)
      if bottom < height then
        val x = column % span
        val z = column / span
        var reach = -1
        if x > 0 then reach = math.max(reach, s.shadowTop(column - 1))
        if x < span - 1 then reach = math.max(reach, s.shadowTop(column + 1))
        if z > 0 then reach = math.max(reach, s.shadowTop(column - span))
        if z < span - 1 then reach = math.max(reach, s.shadowTop(column + span))
        var y = bottom
        while y <= reach do
          val i = y * layer + column
          if sky(i) > 1 then
            if tail == queue.length then queue = java.util.Arrays.copyOf(queue, queue.length * 2)
            queue(tail) = i
            tail += 1
          y += 1
      column += 1

    var head = 0
    while head < tail do
      val i = queue(head)
      head += 1
      val level = sky(i).toInt
      if level > 1 then
        val column = i % layer
        val y = i / layer
        val x = column % span
        val z = column / span
        var direction = 0
        while direction < 6 do
          val j = direction match
            case 0 => if x > 0 then i - 1 else -1
            case 1 => if x < span - 1 then i + 1 else -1
            case 2 => if z > 0 then i - span else -1
            case 3 => if z < span - 1 then i + span else -1
            case 4 => if y > 0 then i - layer else -1
            case _ => if y < height - 1 then i + layer else -1
          if j >= 0 then
            val c = cost(region(j) & 0xff)
            if c < 15 then
              val next = level - math.max(1, c)
              if next > sky(j) then
                sky(j) = next.toByte
                if tail == queue.length then
                  queue = java.util.Arrays.copyOf(queue, queue.length * 2)
                queue(tail) = j
                tail += 1
          direction += 1
    s.queue = queue
