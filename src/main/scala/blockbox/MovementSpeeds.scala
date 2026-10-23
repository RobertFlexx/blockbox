package blockbox

/** Ground movement has one profile in either mode; flight never changes it. */
object MovementSpeeds:
  def ground(swimming: Boolean, sprinting: Boolean, crouching: Boolean, fast: Boolean): Float =
    if crouching && !swimming then 3.2f
    else if swimming then if fast then 6.5f else if sprinting then 3.8f else 2.8f
    else if fast then 13f
    else if sprinting then 7.2f
    else 5.5f
  def flight(fast: Boolean): Float = if fast then 16f else 9f
