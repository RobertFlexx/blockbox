package blockbox

import scala.collection.mutable

/** Main-thread block overrides indexed by chunk. Streaming one chunk never scans all world edits.
  */
final class ChunkBlockMap[V] extends mutable.AbstractMap[(Int, Int, Int), V]:
  private val valuesByPosition = mutable.HashMap.empty[(Int, Int, Int), V]
  private val byChunk = mutable.HashMap.empty[(Int, Int), mutable.HashSet[(Int, Int, Int)]]
  private def chunk(p: (Int, Int, Int)): (Int, Int) =
    (Math.floorDiv(p._1, Terrain.chunkSize), Math.floorDiv(p._3, Terrain.chunkSize))
  def get(p: (Int, Int, Int)): Option[V] = valuesByPosition.get(p)
  def iterator: Iterator[((Int, Int, Int), V)] = valuesByPosition.iterator
  override def size: Int = valuesByPosition.size
  def addOne(entry: ((Int, Int, Int), V)): this.type =
    valuesByPosition.addOne(entry)
    byChunk.getOrElseUpdate(chunk(entry._1), mutable.HashSet.empty).add(entry._1)
    this
  def subtractOne(p: (Int, Int, Int)): this.type =
    valuesByPosition.remove(p)
    val key = chunk(p)
    byChunk.get(key).foreach { bucket =>
      bucket.remove(p)
      if bucket.isEmpty then byChunk.remove(key)
    }
    this
  override def clear(): Unit = { valuesByPosition.clear(); byChunk.clear() }
  def positionsInChunk(cx: Int, cz: Int): Vector[(Int, Int, Int)] =
    byChunk.get((cx, cz)).fold(Vector.empty[(Int, Int, Int)])(_.toVector)
