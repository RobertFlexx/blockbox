package blockbox

object TerrainUv:
  /** Match all four borders under quarter turns while retaining the pack's interior art. */
  def tileGrass(art: Array[Array[(Int, Int, Int, Int)]]): Unit =
    val n = art.length
    if n < 4 || art.exists(_.length != n) then return
    val edges = Array.tabulate(n) { i =>
      val j = n - 1 - i
      val samples = Vector(
        art(0)(i),
        art(0)(j),
        art(n - 1)(i),
        art(n - 1)(j),
        art(i)(0),
        art(j)(0),
        art(i)(n - 1),
        art(j)(n - 1)
      )
      (
        samples.map(_._1).sum / 8,
        samples.map(_._2).sum / 8,
        samples.map(_._3).sum / 8,
        samples.map(_._4).sum / 8
      )
    }
    for y <- 0 until n; x <- 0 until n do
      val distance = math.min(math.min(x, y), math.min(n - 1 - x, n - 1 - y))
      if distance <= 1 then
        val along = if y == distance || y == n - 1 - distance then x else y
        val edge = edges(along)
        val old = art(y)(x); val weight = if distance == 0 then 1f else 0.25f
        def blend(a: Int, b: Int): Int = math.round(a * (1 - weight) + b * weight)
        art(y)(x) = (blend(old._1, edge._1), blend(old._2, edge._2), blend(old._3, edge._3), old._4)
  def rotation(x: Int, z: Int): Int =
    var h = x * 374761393 + z * 668265263
    h = (h ^ (h >>> 13)) * 1274126177
    (h ^ (h >>> 16)) & 3
  def rotate(u: Float, v: Float, turns: Int): (Float, Float) = (turns & 3) match
    case 1 => (v, 1f - u)
    case 2 => (1f - u, 1f - v)
    case 3 => (1f - v, u)
    case _ => (u, v)
