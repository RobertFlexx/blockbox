package blockbox

/** Sparse clustered deposits. Cell coordinates use floor division on both sides of zero
  * and never depend on chunk boundaries or generation order. */
object OreDistribution:
  private def hash(x: Int,y: Int,z: Int,seed: Long,salt: Int): Int =
    var h=x*374761393 ^ y*668265263 ^ z*1442695041 ^ seed.toInt ^ (seed>>>32).toInt ^ salt*982451653
    h=(h^(h>>>13))*1274126177
    (h^(h>>>16)) & 0x7fffffff
  private def vein(x: Int,y: Int,z: Int,seed: Long,salt: Int,size: Int,chance: Int,radius: Float): Boolean =
    val cx=Math.floorDiv(x,size); val cy=Math.floorDiv(y,size); val cz=Math.floorDiv(z,size)
    val h=hash(cx,cy,cz,seed,salt)
    if h%100>=chance then false
    else
      val margin=math.ceil(radius).toInt
      val span=(size-margin*2).max(1)
      val dx=x-(cx*size+margin+(h>>>7)%span)
      val dy=y-(cy*size+margin+(h>>>13)%span)
      val dz=z-(cz*size+margin+(h>>>19)%span)
      dx*dx+dy*dy*1.45f+dz*dz<=radius*radius && hash(x,y,z,seed,salt+17)%100<88

  def at(seed: Long,x: Int,y: Int,z: Int,surface: Int): Option[Block] =
    if y<=4 || y>surface-5 then None
    else if y<=20 && vein(x,y,z,seed,101,10,17,1.65f) then Some(Block.Diamond)
    else if y<=34 && vein(x,y,z,seed,91,10,29,1.9f) then Some(Block.GoldOre)
    else if y<=72 && vein(x,y,z,seed,81,9,55,2.2f) then Some(Block.IronOre)
    else if y<=88 && vein(x,y,z,seed,71,10,64,2.5f) then Some(Block.Copper)
    else if y<=128 && vein(x,y,z,seed,37,9,80,2.7f) then Some(Block.Coal)
    else None
