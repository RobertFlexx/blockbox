package blockbox

import scala.math.*

trait BlockboxInventory:
  protected val placeableBlocks: Array[Block]
  // Aggregate totals stay as a backward-compatible save/migration surface.
  // Live survival behavior is driven by the physical slot arrays below.
  protected val inventory: Array[Int]
  protected val backpackBlocks: Array[Block]
  protected val backpackCounts: Array[Int]
  protected val hotbarBlocks: Array[Block]
  protected val hotbarCounts: Array[Int]
  protected val maxStackSize: Int
  protected var selectedBlock: Int
  protected var heldInventoryBlock: Block
  protected var heldInventoryCount: Int
  protected var heldFromHotbar: Boolean
  protected val craftGridBlocks: Array[Block]
  protected val craftGridCounts: Array[Int]
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

  protected def validHotbarBlock(block: Block): Boolean =
    block != null && CustomBlocks.registered(block) && block != Block.Air && block != Block.Water && block != Block.Bedrock && block != Block.FurnaceLit

  protected def syncInventoryTotalsFromBackpack(): Unit =
    java.util.Arrays.fill(inventory, 0)
    var i = 0
    while i < backpackBlocks.length do
      val b = backpackBlocks(i)
      val n = backpackCounts(i).max(0).min(maxStackSize)
      if validHotbarBlock(b) && n > 0 then inventory(b.ordinal) += n
      i += 1

  protected def rebuildBackpackFromInventoryTotals(): Unit =
    var i = 0
    while i < backpackBlocks.length do
      backpackBlocks(i) = Block.Air; backpackCounts(i) = 0; i += 1
    var slot = 0; var bi = 0
    while bi < inventory.length && slot < backpackBlocks.length do
      val block = if bi < Block.values.length then Block.values(bi) else Block.Air
      var remaining = inventory(bi).max(0)
      if validHotbarBlock(block) then
        while remaining > 0 && slot < backpackBlocks.length do
          val moved = remaining.min(maxStackSize)
          backpackBlocks(slot) = block; backpackCounts(slot) = moved
          remaining -= moved; slot += 1
      bi += 1
    syncInventoryTotalsFromBackpack()

  protected def resetHotbarDefaults(): Unit =
    var i = 0
    while i < hotbarBlocks.length do
      hotbarBlocks(i) = Block.Air; hotbarCounts(i) = 0; i += 1
    selectedBlock = selectedBlock.max(0).min(hotbarBlocks.length - 1)
    clearHeldItem()

  protected def resetInventory(): Unit =
    java.util.Arrays.fill(inventory, 0)
    var i = 0
    while i < backpackBlocks.length do
      backpackBlocks(i) = Block.Air; backpackCounts(i) = 0; i += 1
    resetHotbarDefaults()
    if gameMode == GameMode.Creative then
      for b <- placeableBlocks do inventory(b.ordinal) = maxStackSize
      inventory(Block.Coal.ordinal) = inventory(Block.Coal.ordinal).max(maxStackSize)
    furnaceInput = Block.Air; furnaceFuel = Block.Air; furnaceProgress = 0f; furnaceFuelRemaining = 0f
    furnaceOutput = Block.Air; furnaceOutputCount = 0
    i = 0
    while i < craftGridBlocks.length do
      craftGridBlocks(i) = Block.Air; craftGridCounts(i) = 0; i += 1

  protected def clearHeldItem(): Unit =
    heldInventoryBlock = Block.Air; heldInventoryCount = 0; heldFromHotbar = false

  /** Return a detached cursor stack without ever silently deleting it. */
  protected def releaseHeldItem(): Boolean =
    if gameMode == GameMode.Survival && heldInventoryBlock != Block.Air && heldInventoryCount > 0 then
      val returned = addBackpackItem(heldInventoryBlock, heldInventoryCount)
      var remaining = heldInventoryCount - returned
      var i = 0
      while i < hotbarBlocks.length && remaining > 0 do
        if hotbarBlocks(i) == heldInventoryBlock && hotbarCounts(i) < maxStackSize then
          val moved = remaining.min(maxStackSize - hotbarCounts(i))
          hotbarCounts(i) += moved; remaining -= moved
        i += 1
      i = 0
      while i < hotbarBlocks.length && remaining > 0 do
        if hotbarBlocks(i) == Block.Air || hotbarCounts(i) <= 0 then
          val moved = remaining.min(maxStackSize)
          hotbarBlocks(i) = heldInventoryBlock; hotbarCounts(i) = moved; remaining -= moved
        i += 1
      if remaining > 0 then
        heldInventoryCount = remaining
        addChatMessage(s"Inventory full: make room before closing ($remaining ${blockName(heldInventoryBlock)})")
        return false
    clearHeldItem(); true

  protected def setHeldItem(block: Block, count: Int, fromHotbar: Boolean = false): Unit =
    if validHotbarBlock(block) && count > 0 then
      heldInventoryBlock = block; heldInventoryCount = count.min(maxStackSize); heldFromHotbar = fromHotbar
    else clearHeldItem()

  protected def backpackSlotCapacity: Int = backpackBlocks.length
  protected def backpackSlotsUsed: Int =
    var used = 0; var i = 0
    while i < backpackBlocks.length do
      if validHotbarBlock(backpackBlocks(i)) && backpackCounts(i) > 0 then used += 1
      i += 1
    used

  protected def inventoryRoomFor(block: Block): Int =
    if !validHotbarBlock(block) then 0
    else if gameMode == GameMode.Creative then Int.MaxValue / 4
    else
      var room = 0; var i = 0
      while i < hotbarBlocks.length do
        if hotbarBlocks(i) == block then room += maxStackSize - hotbarCounts(i).max(0).min(maxStackSize)
        else if hotbarBlocks(i) == Block.Air || hotbarCounts(i) <= 0 then room += maxStackSize
        i += 1
      i = 0
      while i < backpackBlocks.length do
        if backpackBlocks(i) == block then room += maxStackSize - backpackCounts(i).max(0).min(maxStackSize)
        else if backpackBlocks(i) == Block.Air || backpackCounts(i) <= 0 then room += maxStackSize
        i += 1
      room

  protected def backpackItemCount(block: Block): Int =
    var total = 0; var i = 0
    while i < backpackBlocks.length do
      if backpackBlocks(i) == block then total += backpackCounts(i).max(0)
      i += 1
    total

  protected def addBackpackItem(block: Block, amount: Int): Int =
    if !validHotbarBlock(block) || amount <= 0 then 0
    else if gameMode == GameMode.Creative then
      inventory(block.ordinal) = inventory(block.ordinal).max(maxStackSize); amount
    else
      var remaining = amount; var i = 0
      while i < backpackBlocks.length && remaining > 0 do
        if backpackBlocks(i) == block && backpackCounts(i) > 0 && backpackCounts(i) < maxStackSize then
          val moved = remaining.min(maxStackSize - backpackCounts(i)); backpackCounts(i) += moved; remaining -= moved
        i += 1
      i = 0
      while i < backpackBlocks.length && remaining > 0 do
        if backpackBlocks(i) == Block.Air || backpackCounts(i) <= 0 then
          val moved = remaining.min(maxStackSize); backpackBlocks(i) = block; backpackCounts(i) = moved; remaining -= moved
        i += 1
      val moved = amount - remaining
      if moved > 0 then syncInventoryTotalsFromBackpack()
      moved

  protected def removeBackpackItem(block: Block, amount: Int): Int =
    if amount <= 0 || !validHotbarBlock(block) then 0
    else
      var remaining = amount; var i = backpackBlocks.length - 1
      while i >= 0 && remaining > 0 do
        if backpackBlocks(i) == block && backpackCounts(i) > 0 then
          val used = remaining.min(backpackCounts(i)); backpackCounts(i) -= used; remaining -= used
          if backpackCounts(i) <= 0 then
            backpackBlocks(i) = Block.Air
            backpackCounts(i) = 0
        i -= 1
      val removed = amount - remaining
      if removed > 0 then syncInventoryTotalsFromBackpack()
      removed

  protected def handleBackpackSlotClick(slot: Int, rightClick: Boolean): Unit =
    if slot < 0 || slot >= backpackBlocks.length then return
    val block = backpackBlocks(slot); val count = backpackCounts(slot).max(0)
    if gameMode == GameMode.Creative then
      if validHotbarBlock(block) then setHeldItem(block, maxStackSize)
      return
    if heldInventoryBlock == Block.Air then
      if validHotbarBlock(block) && count > 0 then
        val take = if rightClick then (count + 1) / 2 else count
        setHeldItem(block, take, fromHotbar = true); backpackCounts(slot) -= take
        if backpackCounts(slot) <= 0 then
          backpackBlocks(slot) = Block.Air
          backpackCounts(slot) = 0
        syncInventoryTotalsFromBackpack()
    else if !validHotbarBlock(block) || count <= 0 then
      val put = if rightClick then 1 else heldInventoryCount
      backpackBlocks(slot) = heldInventoryBlock; backpackCounts(slot) = put; heldInventoryCount -= put
      if heldInventoryCount <= 0 then clearHeldItem()
      syncInventoryTotalsFromBackpack()
    else if block == heldInventoryBlock && count < maxStackSize then
      val put = if rightClick then 1 else (maxStackSize - count).min(heldInventoryCount)
      backpackCounts(slot) += put; heldInventoryCount -= put
      if heldInventoryCount <= 0 then clearHeldItem()
      syncInventoryTotalsFromBackpack()
    else if !rightClick then
      val oldBlock = block; val oldCount = count
      backpackBlocks(slot) = heldInventoryBlock; backpackCounts(slot) = heldInventoryCount
      setHeldItem(oldBlock, oldCount, fromHotbar = true); syncInventoryTotalsFromBackpack()

  protected def hotbarItemCount(block: Block): Int =
    if !validHotbarBlock(block) then 0
    else
      var total = 0; var i = 0
      while i < hotbarBlocks.length do
        if hotbarBlocks(i) == block then total += hotbarCounts(i).max(0)
        i += 1
      total

  protected def compactHotbarAssignments(): Unit =
    var i = 0
    while i < hotbarBlocks.length do
      val b = hotbarBlocks(i)
      if !validHotbarBlock(b) then
        hotbarBlocks(i) = Block.Air
        hotbarCounts(i) = 0
      else if gameMode == GameMode.Creative then hotbarCounts(i) = maxStackSize
      else if hotbarCounts(i) <= 0 then
        hotbarBlocks(i) = Block.Air
        hotbarCounts(i) = 0
      else hotbarCounts(i) = hotbarCounts(i).min(maxStackSize)
      i += 1
    if gameMode == GameMode.Survival && heldInventoryBlock != Block.Air && heldInventoryCount <= 0 then clearHeldItem()

  protected def assignBlockToHotbar(block: Block, preferredSlot: Int): Unit =
    if validHotbarBlock(block) then
      val slot = preferredSlot.max(0).min(hotbarBlocks.length - 1)
      val oldBlock = hotbarBlocks(slot); val oldCount = hotbarCounts(slot)
      if gameMode == GameMode.Survival && validHotbarBlock(oldBlock) && oldCount > 0 then addBackpackItem(oldBlock, oldCount)
      if gameMode == GameMode.Creative then
        hotbarBlocks(slot) = block
        hotbarCounts(slot) = maxStackSize
        selectedBlock = slot
      else
        val moved = removeBackpackItem(block, maxStackSize)
        if moved > 0 then
          hotbarBlocks(slot) = block
          hotbarCounts(slot) = moved
          selectedBlock = slot
        else hotbarBlocks(slot) = Block.Air; hotbarCounts(slot) = 0
      compactHotbarAssignments()

  protected def consumeSelectedHotbar(amount: Int): Boolean =
    if amount <= 0 || gameMode == GameMode.Creative then true
    else
      val slot = selectedBlock.max(0).min(hotbarBlocks.length - 1)
      if hotbarBlocks(slot) == Block.Air || hotbarCounts(slot) < amount then false
      else hotbarCounts(slot) -= amount; compactHotbarAssignments(); true

  protected def takeHotbarSlot(slot0: Int, rightClick: Boolean = false): Unit =
    val slot = slot0.max(0).min(hotbarBlocks.length - 1); val block = hotbarBlocks(slot)
    val count = if gameMode == GameMode.Creative then maxStackSize else hotbarCounts(slot)
    if validHotbarBlock(block) && count > 0 then
      val take = if rightClick && gameMode == GameMode.Survival then (count + 1) / 2 else count
      setHeldItem(block, take, fromHotbar = gameMode == GameMode.Survival)
      if gameMode == GameMode.Survival then
        hotbarCounts(slot) -= take
        compactHotbarAssignments()

  protected def placeHeldItemInHotbar(slot0: Int, rightClick: Boolean = false): Unit =
    if heldInventoryBlock == Block.Air then return
    val slot = slot0.max(0).min(hotbarBlocks.length - 1); val oldBlock = hotbarBlocks(slot); val oldCount = hotbarCounts(slot).max(0)
    if gameMode == GameMode.Creative then
      hotbarBlocks(slot) = heldInventoryBlock
      hotbarCounts(slot) = maxStackSize
      selectedBlock = slot
      clearHeldItem()
    else if !validHotbarBlock(oldBlock) || oldCount <= 0 then
      val put = if rightClick then 1 else heldInventoryCount
      hotbarBlocks(slot) = heldInventoryBlock; hotbarCounts(slot) = put; selectedBlock = slot; heldInventoryCount -= put
      if heldInventoryCount <= 0 then clearHeldItem()
    else if oldBlock == heldInventoryBlock && oldCount < maxStackSize then
      val put = if rightClick then 1 else (maxStackSize - oldCount).min(heldInventoryCount)
      hotbarCounts(slot) += put; selectedBlock = slot; heldInventoryCount -= put
      if heldInventoryCount <= 0 then clearHeldItem()
    else if !rightClick then
      hotbarBlocks(slot) = heldInventoryBlock; hotbarCounts(slot) = heldInventoryCount; selectedBlock = slot
      setHeldItem(oldBlock, oldCount, fromHotbar = true)
    compactHotbarAssignments()

  protected def handleCraftSlotClick(slot: Int, rightClick: Boolean): Unit =
    if slot < 0 || slot >= craftGridBlocks.length then return
    val block = craftGridBlocks(slot); val count = craftGridCounts(slot).max(0)
    if heldInventoryBlock == Block.Air then
      if validHotbarBlock(block) && count > 0 then
        val take = if rightClick then (count + 1) / 2 else count
        setHeldItem(block, take, fromHotbar = true); craftGridCounts(slot) -= take
        if craftGridCounts(slot) <= 0 then
          craftGridBlocks(slot) = Block.Air
          craftGridCounts(slot) = 0
    else if !validHotbarBlock(block) || count <= 0 then
      val put = if rightClick then 1 else heldInventoryCount
      craftGridBlocks(slot) = heldInventoryBlock; craftGridCounts(slot) = put; heldInventoryCount -= put
      if heldInventoryCount <= 0 then clearHeldItem()
    else if block == heldInventoryBlock && count < maxStackSize then
      val put = if rightClick then 1 else (maxStackSize - count).min(heldInventoryCount)
      craftGridCounts(slot) += put; heldInventoryCount -= put
      if heldInventoryCount <= 0 then clearHeldItem()
    else if !rightClick then
      val oldBlock = block; val oldCount = count
      craftGridBlocks(slot) = heldInventoryBlock; craftGridCounts(slot) = heldInventoryCount
      setHeldItem(oldBlock, oldCount, fromHotbar = true)

  protected def resolveCraftingRecipe(cells: Array[Block]): Option[CraftRecipe] = InventoryModel.craftingResult(cells)
  protected def currentCraftingResult: Option[CraftRecipe] = resolveCraftingRecipe(craftGridBlocks)

  protected def canCraftCurrent(recipe: CraftRecipe): Boolean =
    gameMode == GameMode.Creative || {
      var ok = inventoryRoomFor(recipe.output) >= recipe.outputCount
      var i = 0
      while i < craftGridBlocks.length && ok do
        if craftGridBlocks(i) != Block.Air && craftGridCounts(i) <= 0 then ok = false
        i += 1
      ok
    }

  protected def tryCraftGrid(): Unit =
    currentCraftingResult match
      case Some(recipe) if canCraftCurrent(recipe) =>
        if gameMode == GameMode.Survival then
          var i = 0
          while i < craftGridBlocks.length do
            if craftGridBlocks(i) != Block.Air then
              craftGridCounts(i) -= 1
              if craftGridCounts(i) <= 0 then
                craftGridBlocks(i) = Block.Air
                craftGridCounts(i) = 0
            i += 1
        gainItem(recipe.output, recipe.outputCount); playPlaceSound()
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
      val room = inventoryRoomFor(furnaceOutput).max(0)
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
