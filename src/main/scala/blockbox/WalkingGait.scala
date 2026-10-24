package blockbox

/** One distance-driven phase for foot contacts, walking sway and local footstep audio. */
final class WalkingGait:
  private var contact = 0.0
  private var started = false
  def phase: Float = (contact * math.Pi).toFloat

  def advance(distance: Float, stepLength: Float): Boolean =
    if distance <= 0 || stepLength <= 0 then false
    else
      // The first contact follows a short weight shift, rather than a full stride of silence.
      if !started then { contact = 0.65; started = true }
      val before = contact.toInt
      contact += distance.toDouble / stepLength
      val landed = contact.toInt > before
      contact %= 2.0
      landed

  // Preserve phase while stopped so a brief pause cannot snap the walking pose.
  def reset(): Unit = { started = false; contact = 0.0 }
