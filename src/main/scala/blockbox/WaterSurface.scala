package blockbox

/** Shared render/immersion heights. Wave displacement is visual and pins at every
  * land or open-water edge, so a surface cannot lift away from its enclosing volume.
  */
object WaterSurface:
  def height(level: Int): Float =
    if level<=0 || level>=8 then 1f else 0.36f+0.52f*level.max(1)/7f

  def waveWeight(x: Int,y: Int,z: Int,water: (Int,Int,Int) => Boolean): Float =
    // Four cells share a corner. A missing cell pins that corner to the bank.
    var dx= -1
    while dx<=0 do
      var dz= -1
      while dz<=0 do
        if !water(x+dx,y,z+dz) then return 0f
        dz+=1
      dx+=1
    dx= -2
    while dx<=1 do
      var dz= -2
      while dz<=1 do
        if !water(x+dx,y,z+dz) then return 0.45f
        dz+=1
      dx+=1
    1f
