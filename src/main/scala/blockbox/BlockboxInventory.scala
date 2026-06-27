package blockbox

import scala.math.*

trait BlockboxInventory:
  protected val placeableBlocks: Array[Block]
  protected val inventory: Array[Int]
  protected val hotbarBlocks: Array[Block]
  protected val hotbarCounts: Array[Int]
  protected val maxStackSize: Int
  protected var selectedBlock: Int
  protected var heldInventoryBlock: Block
  protected var heldInventoryCount: Int
  protected var heldFromHotbar: Boolean
  protected val craftGridBlocks: Array[Block]
  protected var furnaceInput: Block
  protected var furnaceFuel: Block
  protected var furnaceProgress: Float
  protected var furnaceFuelRemaining: Float
  protected var furnaceOutput: Block
  protected var furnaceOutputCount: Int
  protected def gameMode: GameMode
  protected def gameMode_=(mode: GameMode): Unit
  protected def addChatMessage(msg: String): Unit
  protected def playPlaceSound(): Unit
  protected def blockName(block: Block): String
  protected def totalItemCount(block: Block): Int
  protected def consumeInventory(block: Block, amount: Int): Boolean
  protected def gainItem(block: Block, amount: Int): Unit

  protected def smeltableInputs: Array[Block] = InventoryModel.smeltableInputs
  protected def fuelBlocks: Array[Block] = InventoryModel.fuelBlocks

  protected def resetHotbarDefaults(): Unit =
    var i = 0
    while i < hotbarBlocks.length do
      hotbarBlocks(i) = Block.Air
      hotbarCounts(i) = 0
      i += 1
    selectedBlock = selectedBlock.max(0).min(hotbarBlocks.length - 1)
    clearHeldItem()

  protected def resetInventory(): Unit =
    java.util.Arrays.fill(inventory, 0)
    resetHotbarDefaults()
    if gameMode == GameMode.Creative then
      for b <- placeableBlocks do inventory(b.ordinal) = maxStackSize
      inventory(Block.Coal.ordinal) = inventory(Block.Coal.ordinal).max(maxStackSize)
    furnaceInput = Block.Air
    furnaceFuel = Block.Air
    furnaceProgress = 0f
    furnaceFuelRemaining = 0f
    furnaceOutput = Block.Air
    furnaceOutputCount = 0
    var cg = 0
    while cg < craftGridBlocks.length do
      craftGridBlocks(cg) = Block.Air
      cg += 1

  protected def validHotbarBlock(block: Block): Boolean =
    block != Block.Air && block != Block.Water && block != Block.Bedrock && block != Block.FurnaceLit

  protected def clearHeldItem(): Unit =
    heldInventoryBlock = Block.Air
    heldInventoryCount = 0
    heldFromHotbar = false

  protected def releaseHeldItem(): Unit =
    if gameMode == GameMode.Survival && heldFromHotbar && heldInventoryBlock != Block.Air then addBackpackItem(heldInventoryBlock, heldInventoryCount.max(1))
    clearHeldItem()

  protected def setHeldItem(block: Block, count: Int, fromHotbar: Boolean = false): Unit =
    if validHotbarBlock(block) && count > 0 then
      heldInventoryBlock = block
      heldInventoryCount = count.min(maxStackSize)
      heldFromHotbar = fromHotbar
    else clearHeldItem()

  protected def addBackpackItem(block: Block, amount: Int): Int =
    if !validHotbarBlock(block) || amount <= 0 then 0
    else
      val room = 999 - inventory(block.ordinal)
      val moved = amount.min(room.max(0))
      if moved > 0 then inventory(block.ordinal) += moved
      moved

  protected def hotbarItemCount(block: Block): Int =
    if !validHotbarBlock(block) then 0
    else
      var total = 0
      var i = 0
      while i < hotbarBlocks.length do
        if hotbarBlocks(i) == block then total += hotbarCounts(i).max(0)
        i += 1
      total

  protected def compactHotbarAssignments(): Unit =
    // The hotbar is real stack storage now, separate from the backpack inventory.
    // It should never show a stale ghost item or duplicate shortcut clone.
    val seen = scala.collection.mutable.HashSet.empty[Block]
    var i = 0
    while i < hotbarBlocks.length do
      val b = hotbarBlocks(i)
      if !validHotbarBlock(b) then
        hotbarBlocks(i) = Block.Air
        hotbarCounts(i) = 0
      else if gameMode == GameMode.Creative then
        if seen.contains(b) then
          hotbarBlocks(i) = Block.Air
          hotbarCounts(i) = 0
        else
          seen += b
          hotbarCounts(i) = maxStackSize
      else if hotbarCounts(i) <= 0 then
        hotbarBlocks(i) = Block.Air
        hotbarCounts(i) = 0
      else if seen.contains(b) then
        addBackpackItem(b, hotbarCounts(i))
        hotbarBlocks(i) = Block.Air
        hotbarCounts(i) = 0
      else
        seen += b
        hotbarCounts(i) = hotbarCounts(i).min(maxStackSize)
      i += 1
    if gameMode == GameMode.Survival && heldInventoryBlock != Block.Air && heldInventoryCount <= 0 then clearHeldItem()

  protected def assignBlockToHotbar(block: Block, preferredSlot: Int): Unit =
    if validHotbarBlock(block) then
      val slot = preferredSlot.max(0).min(hotbarBlocks.length - 1)
      val oldBlock = hotbarBlocks(slot)
      val oldCount = hotbarCounts(slot)
      if gameMode == GameMode.Survival && validHotbarBlock(oldBlock) && oldCount > 0 then addBackpackItem(oldBlock, oldCount)
      var i = 0
      while i < hotbarBlocks.length do
        if i != slot && hotbarBlocks(i) == block then
          if gameMode == GameMode.Survival && hotbarCounts(i) > 0 then addBackpackItem(block, hotbarCounts(i))
          hotbarBlocks(i) = Block.Air
          hotbarCounts(i) = 0
        i += 1
      if gameMode == GameMode.Creative then
        hotbarBlocks(slot) = block
        hotbarCounts(slot) = maxStackSize
        selectedBlock = slot
      else
        val moved = inventory(block.ordinal).max(0).min(maxStackSize)
        if moved > 0 then
          inventory(block.ordinal) -= moved
          hotbarBlocks(slot) = block
          hotbarCounts(slot) = moved
          selectedBlock = slot
        else
          hotbarBlocks(slot) = Block.Air
          hotbarCounts(slot) = 0
      compactHotbarAssignments()

  protected def consumeSelectedHotbar(amount: Int): Boolean =
    if amount <= 0 then true
    else if gameMode == GameMode.Creative then true
    else
      val slot = selectedBlock.max(0).min(hotbarBlocks.length - 1)
      if hotbarBlocks(slot) == Block.Air || hotbarCounts(slot) < amount then false
      else
        hotbarCounts(slot) -= amount
        compactHotbarAssignments()
        true

  protected def takeHotbarSlot(slot0: Int): Unit =
    val slot = slot0.max(0).min(hotbarBlocks.length - 1)
    val block = hotbarBlocks(slot)
    val count = if gameMode == GameMode.Creative then maxStackSize else hotbarCounts(slot)
    if validHotbarBlock(block) && count > 0 then
      setHeldItem(block, count, fromHotbar = gameMode == GameMode.Survival)
      if gameMode == GameMode.Survival then
        hotbarBlocks(slot) = Block.Air
        hotbarCounts(slot) = 0
        compactHotbarAssignments()

  protected def placeHeldItemInHotbar(slot0: Int): Unit =
    if heldInventoryBlock == Block.Air then return
    val slot = slot0.max(0).min(hotbarBlocks.length - 1)
    val oldBlock = hotbarBlocks(slot)
    val oldCount = hotbarCounts(slot)
    if gameMode == GameMode.Creative then
      hotbarBlocks(slot) = heldInventoryBlock
      hotbarCounts(slot) = maxStackSize
      selectedBlock = slot
      clearHeldItem()
    else
      if validHotbarBlock(oldBlock) && oldCount > 0 then addBackpackItem(oldBlock, oldCount)
      val moveCount = heldInventoryCount.max(1).min(maxStackSize)
      val canMove = heldFromHotbar || consumeInventory(heldInventoryBlock, moveCount)
      if canMove then
        hotbarBlocks(slot) = heldInventoryBlock
        hotbarCounts(slot) = moveCount
        selectedBlock = slot
        clearHeldItem()
        compactHotbarAssignments()
      else addChatMessage("Missing item stack")

  protected def currentCraftingResult: Option[CraftRecipe] =
    InventoryModel.craftingResult(craftGridBlocks)

  protected def craftInputCounts: Map[Block, Int] =
    val counts = scala.collection.mutable.HashMap.empty[Block, Int]
    var i = 0
    while i < craftGridBlocks.length do
      val b = craftGridBlocks(i)
      if b != Block.Air then counts(b) = counts.getOrElse(b, 0) + 1
      i += 1
    counts.toMap

  protected def canCraftCurrent(recipe: CraftRecipe): Boolean =
    gameMode == GameMode.Creative || craftInputCounts.forall { case (b, n) => totalItemCount(b) >= n }

  protected def tryCraftGrid(): Unit =
    currentCraftingResult match
      case Some(recipe) if canCraftCurrent(recipe) =>
        val counts = craftInputCounts
        var ok = true
        counts.foreach { case (b, n) => if !consumeInventory(b, n) then ok = false }
        if ok then
          gainItem(recipe.output, recipe.outputCount)
          playPlaceSound()
        else addChatMessage("Missing crafting ingredients")
      case Some(_) => addChatMessage("Missing crafting ingredients")
      case None => addChatMessage("No matching recipe")

  protected def smeltResult(input: Block): Option[(Block, Int)] =
    InventoryModel.smeltResult(input)

  protected def canConsumeFuelFor(input: Block): Boolean =
    totalItemCount(Block.Coal) > 0 || fuelBlocks.exists(b => b != Block.Coal && totalItemCount(b) > (if b == input then 1 else 0))

  protected def consumeFuelFor(input: Block): Boolean =
    if furnaceFuelRemaining > 0f then true
    else if totalItemCount(Block.Coal) > 0 then
      consumeInventory(Block.Coal, 1)
      furnaceFuel = Block.Coal
      furnaceFuelRemaining = 8f
      true
    else
      val picked = fuelBlocks.find(b => b != Block.Coal && totalItemCount(b) > (if b == input then 1 else 0))
      picked match
        case Some(fuel) =>
          consumeInventory(fuel, 1)
          furnaceFuel = fuel
          furnaceFuelRemaining = InventoryModel.fuelBurnTime(fuel)
          true
        case None => false

  protected def takeFurnaceOutput(): Unit =
    if furnaceOutput != Block.Air && furnaceOutputCount > 0 then
      val room = (999 - totalItemCount(furnaceOutput)).max(0)
      val moved = math.min(room, furnaceOutputCount)
      if moved > 0 then
        gainItem(furnaceOutput, moved)
        furnaceOutputCount -= moved
        playPlaceSound()
      if furnaceOutputCount <= 0 then
        furnaceOutput = Block.Air
        furnaceOutputCount = 0
      else addChatMessage("Inventory stack is full")

  protected def smeltOneInternal(showMessages: Boolean): Boolean =
    if furnaceInput == Block.Air then
      if showMessages then addChatMessage("Choose an input from the furnace list first")
      return false
    if totalItemCount(furnaceInput) <= 0 then
      if showMessages then addChatMessage(s"No ${blockName(furnaceInput)} left")
      furnaceInput = Block.Air
      furnaceProgress = 0f
      return false
    val result = smeltResult(furnaceInput)
    if result.isEmpty then
      if showMessages then addChatMessage("That item cannot be smelted")
      return false
    val (out, amount) = result.get
    if furnaceOutput != Block.Air && furnaceOutput != out then
      if showMessages then addChatMessage("Take the current output first")
      return false
    if furnaceOutputCount + amount > 999 then
      if showMessages then addChatMessage("Furnace output is full")
      return false
    if !consumeFuelFor(furnaceInput) then
      if showMessages then addChatMessage("Need fuel: Coal or Wood")
      return false
    consumeInventory(furnaceInput, 1)
    furnaceFuelRemaining -= 1f
    furnaceProgress = 4f
    furnaceOutput = out
    furnaceOutputCount += amount
    if totalItemCount(furnaceInput) <= 0 then furnaceInput = Block.Air
    if furnaceFuelRemaining <= 0f then
      furnaceFuelRemaining = 0f
      furnaceFuel = Block.Air
    playPlaceSound()
    true

  protected def smeltFurnace(): Unit =
    if smeltOneInternal(showMessages = true) then furnaceProgress = 0f

  protected def smeltAllFurnace(): Unit =
    var made = 0
    var keepGoing = true
    while keepGoing && made < 256 do
      keepGoing = smeltOneInternal(showMessages = false)
      if keepGoing then made += 1
    furnaceProgress = 0f
    if made == 0 then smeltOneInternal(showMessages = true)
    else addChatMessage(s"Smelted $made item${if made == 1 then "" else "s"}")
