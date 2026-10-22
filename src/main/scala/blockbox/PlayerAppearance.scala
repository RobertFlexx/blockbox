package blockbox

import blockbox.net.BlockboxNet

object PlayerAppearance:
  // Stable IDs: retain the original ten shirts and give every available session slot
  // its own color. Additional hues use distinct brightness/saturation combinations.
  val shirts: Vector[(Float,Float,Float)] = Vector(
    (0.22f,0.46f,0.95f),(0.95f,0.28f,0.22f),(0.20f,0.78f,0.35f),(0.95f,0.76f,0.20f),
    (0.62f,0.36f,0.94f),(0.95f,0.44f,0.82f),(0.12f,0.80f,0.80f),(1.00f,0.50f,0.18f),
    (0.80f,0.82f,0.88f),(0.52f,0.88f,0.18f),(0.14f,0.24f,0.48f),(0.54f,0.14f,0.22f),
    (0.10f,0.42f,0.30f),(0.66f,0.48f,0.13f),(0.35f,0.20f,0.54f),(0.62f,0.26f,0.46f),
    (0.10f,0.47f,0.57f),(0.64f,0.30f,0.12f),(0.32f,0.37f,0.43f),(0.37f,0.49f,0.13f),
    (0.50f,0.71f,1.00f),(1.00f,0.62f,0.55f),(0.53f,0.91f,0.70f),(1.00f,0.91f,0.54f),
    (0.78f,0.65f,1.00f),(1.00f,0.71f,0.87f),(0.51f,0.94f,0.92f),(1.00f,0.76f,0.49f),
    (0.95f,0.94f,0.87f),(0.76f,0.92f,0.53f),(0.31f,0.29f,0.27f),(0.57f,0.43f,0.34f)
  )
  require(shirts.length == BlockboxNet.MAX_PLAYERS && shirts.distinct.length == shirts.length)

  def nextFreeColor(used: Set[Int]): Option[Int] = shirts.indices.find(i => !used.contains(i))
