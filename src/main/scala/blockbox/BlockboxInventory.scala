package blockbox

import scala.math.*

trait BlockboxInventory:
  protected var furnaceRequested = 0
  protected var furnaceSlots = false
  protected var furnaceInputCount = 0
  protected var furnaceFuelCount = 0
  protected var furnacePaused = false
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

  /** Progress tracking for items that reach the inventory without gainItem. */
  protected def noteCollected(block: Block, amount: Int): Unit
  protected def noteSmelted(block: Block, amount: Int): Unit = ()
  protected def furnaceAvailable: Boolean = true
  protected def noteFurnaceBurning(burning: Boolean): Unit = ()

  protected def smeltableInputs: Array[Block] = InventoryModel.smeltableInputs
  protected def fuelBlocks: Array[Block] = InventoryModel.fuelBlocks

  protected def validHotbarBlock(block: Block): Boolean =
    block != null && CustomBlocks.registered(
      block
    ) && block != Block.Air && block != Block.Water && block != Block.Bedrock && block != Block.FurnaceLit

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
    furnaceInput = Block.Air; furnaceFuel = Block.Air; furnaceProgress = 0f;
    furnaceFuelRemaining = 0f
    furnaceOutput = Block.Air; furnaceOutputCount = 0
    furnaceRequested = 0
    furnaceSlots = false; furnaceInputCount = 0; furnaceFuelCount = 0; furnacePaused = false
    i = 0
    while i < craftGridBlocks.length do
      craftGridBlocks(i) = Block.Air; craftGridCounts(i) = 0; i += 1

  protected def clearHeldItem(): Unit =
    heldInventoryBlock = Block.Air; heldInventoryCount = 0; heldFromHotbar = false

  /** Return a detached cursor stack without ever silently deleting it. */
  protected def releaseHeldItem(): Boolean =
    if (gameMode == GameMode.Survival || heldFromHotbar) && heldInventoryBlock != Block.Air && heldInventoryCount > 0
    then
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
        addChatMessage(
          s"Inventory full: make room before closing ($remaining ${blockName(heldInventoryBlock)})"
        )
        return false
    clearHeldItem(); true

  protected def setHeldItem(block: Block, count: Int, fromHotbar: Boolean = false): Unit =
    if validHotbarBlock(block) && count > 0 then
      heldInventoryBlock = block; heldInventoryCount = count.min(maxStackSize);
      heldFromHotbar = fromHotbar
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
    else
      var room = 0; var i = 0
      while i < hotbarBlocks.length do
        if hotbarBlocks(i) == block then
          room += maxStackSize - hotbarCounts(i).max(0).min(maxStackSize)
        else if hotbarBlocks(i) == Block.Air || hotbarCounts(i) <= 0 then room += maxStackSize
        i += 1
      i = 0
      while i < backpackBlocks.length do
        if backpackBlocks(i) == block then
          room += maxStackSize - backpackCounts(i).max(0).min(maxStackSize)
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
        if backpackBlocks(i) == block && backpackCounts(i) > 0 && backpackCounts(i) < maxStackSize
        then
          val moved = remaining.min(maxStackSize - backpackCounts(i)); backpackCounts(i) += moved;
          remaining -= moved
        i += 1
      i = 0
      while i < backpackBlocks.length && remaining > 0 do
        if backpackBlocks(i) == Block.Air || backpackCounts(i) <= 0 then
          val moved = remaining.min(maxStackSize); backpackBlocks(i) = block;
          backpackCounts(i) = moved; remaining -= moved
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
      backpackBlocks(slot) = heldInventoryBlock; backpackCounts(slot) = put;
      heldInventoryCount -= put
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

  private type SlotRef = (Array[Block], Array[Int], Int)
  private def slotRefs(blocks: Array[Block], counts: Array[Int]): IndexedSeq[SlotRef] =
    blocks.indices.map(i => (blocks, counts, i))
  private def hotbarSlots = slotRefs(hotbarBlocks, hotbarCounts)
  private def backpackSlots = slotRefs(backpackBlocks, backpackCounts)

  /** Minecraft's moveItemStackTo: top up matching stacks, then fill empty slots, in order. */
  private def moveInto(block: Block, amount: Int, targets: Seq[SlotRef]): Int =
    var remaining = amount
    for (blocks, counts, i) <- targets do
      if remaining > 0 && blocks(i) == block && counts(i) > 0 && counts(i) < maxStackSize then
        val moved = remaining.min(maxStackSize - counts(i)); counts(i) += moved; remaining -= moved
    for (blocks, counts, i) <- targets do
      if remaining > 0 && (blocks(i) == Block.Air || counts(i) <= 0) then
        val moved = remaining.min(maxStackSize); blocks(i) = block; counts(i) = moved;
        remaining -= moved
    amount - remaining

  /** Shift-click (survival): the slot's whole stack moves to the other section, as far as it fits;
    * the cursor stack is left alone.
    */
  private def quickMove(
      blocks: Array[Block],
      counts: Array[Int],
      slot: Int,
      targets: Seq[SlotRef]
  ): Unit =
    val block = blocks(slot); val count = counts(slot)
    if validHotbarBlock(block) && count > 0 then
      val moved = moveInto(block, count, targets)
      counts(slot) -= moved
      if counts(slot) <= 0 then
        blocks(slot) = Block.Air; counts(slot) = 0
      if moved > 0 then
        syncInventoryTotalsFromBackpack(); compactHotbarAssignments()

  protected def quickMoveHotbar(slot: Int): Unit =
    quickMove(hotbarBlocks, hotbarCounts, slot, backpackSlots)
  protected def quickMoveBackpack(slot: Int): Unit =
    quickMove(backpackBlocks, backpackCounts, slot, hotbarSlots)
  protected def quickMoveCraft(slot: Int): Unit =
    quickMove(craftGridBlocks, craftGridCounts, slot, backpackSlots ++ hotbarSlots)

  /** Creative shift-click: a full stack into the first free hotbar slot, as from Minecraft's
    * creative tabs.
    */
  protected def creativeQuickGive(block: Block): Unit =
    val slot = hotbarBlocks.indexWhere(b => !validHotbarBlock(b))
    if validHotbarBlock(block) && !hotbarBlocks.contains(block) && slot >= 0 then
      hotbarBlocks(slot) = block; hotbarCounts(slot) = maxStackSize

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
      else if hotbarCounts(i) <= 0 then
        hotbarBlocks(i) = Block.Air
        hotbarCounts(i) = 0
      else hotbarCounts(i) = hotbarCounts(i).min(maxStackSize)
      i += 1
    if gameMode == GameMode.Survival && heldInventoryBlock != Block.Air && heldInventoryCount <= 0
    then clearHeldItem()

  protected def assignBlockToHotbar(block: Block, preferredSlot: Int): Unit =
    if validHotbarBlock(block) then
      val slot = preferredSlot.max(0).min(hotbarBlocks.length - 1)
      val oldBlock = hotbarBlocks(slot); val oldCount = hotbarCounts(slot)
      if gameMode == GameMode.Survival && validHotbarBlock(oldBlock) && oldCount > 0 then
        addBackpackItem(oldBlock, oldCount)
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
        else hotbarBlocks(slot) = Block.Air;
        hotbarCounts(slot) = 0
      compactHotbarAssignments()

  protected def consumeSelectedHotbar(amount: Int): Boolean =
    if amount <= 0 || gameMode == GameMode.Creative then true
    else
      val slot = selectedBlock.max(0).min(hotbarBlocks.length - 1)
      if hotbarBlocks(slot) == Block.Air || hotbarCounts(slot) < amount then false
      else hotbarCounts(slot) -= amount;
      compactHotbarAssignments(); true

  protected def takeHotbarSlot(slot0: Int, rightClick: Boolean = false): Unit =
    val slot = slot0.max(0).min(hotbarBlocks.length - 1); val block = hotbarBlocks(slot)
    val count = hotbarCounts(slot)
    if validHotbarBlock(block) && count > 0 then
      val take = if rightClick then (count + 1) / 2 else count
      setHeldItem(block, take, fromHotbar = true)
      hotbarCounts(slot) -= take
      compactHotbarAssignments()

  protected def placeHeldItemInHotbar(slot0: Int, rightClick: Boolean = false): Unit =
    if heldInventoryBlock == Block.Air then return
    val slot = slot0.max(0).min(hotbarBlocks.length - 1); val oldBlock = hotbarBlocks(slot);
    val oldCount = hotbarCounts(slot).max(0)
    if !validHotbarBlock(oldBlock) || oldCount <= 0 then
      val put = if rightClick then 1 else heldInventoryCount
      hotbarBlocks(slot) = heldInventoryBlock; hotbarCounts(slot) = put; selectedBlock = slot;
      heldInventoryCount -= put
      if heldInventoryCount <= 0 then clearHeldItem()
    else if oldBlock == heldInventoryBlock && oldCount < maxStackSize then
      val put = if rightClick then 1 else (maxStackSize - oldCount).min(heldInventoryCount)
      hotbarCounts(slot) += put; selectedBlock = slot; heldInventoryCount -= put
      if heldInventoryCount <= 0 then clearHeldItem()
    else if !rightClick then
      hotbarBlocks(slot) = heldInventoryBlock; hotbarCounts(slot) = heldInventoryCount;
      selectedBlock = slot
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
      craftGridBlocks(slot) = heldInventoryBlock; craftGridCounts(slot) = put;
      heldInventoryCount -= put
      if heldInventoryCount <= 0 then clearHeldItem()
    else if block == heldInventoryBlock && count < maxStackSize then
      val put = if rightClick then 1 else (maxStackSize - count).min(heldInventoryCount)
      craftGridCounts(slot) += put; heldInventoryCount -= put
      if heldInventoryCount <= 0 then clearHeldItem()
    else if !rightClick then
      val oldBlock = block; val oldCount = count
      craftGridBlocks(slot) = heldInventoryBlock; craftGridCounts(slot) = heldInventoryCount
      setHeldItem(oldBlock, oldCount, fromHotbar = true)

  protected def resolveCraftingRecipe(cells: Array[Block]): Option[CraftRecipe] =
    InventoryModel.craftingResult(cells)
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

  /** @param all
    *   shift-click: craft until an ingredient or inventory room runs out. Like Minecraft, those
    *   results fill the hotbar from the right, then the backpack from the end.
    */
  protected def tryCraftGrid(all: Boolean = false): Unit =
    val quick = all && gameMode == GameMode.Survival
    currentCraftingResult match
      case Some(recipe) if canCraftCurrent(recipe) =>
        var crafting = true
        while crafting do
          if gameMode == GameMode.Survival then
            var i = 0
            while i < craftGridBlocks.length do
              if craftGridBlocks(i) != Block.Air then
                craftGridCounts(i) -= 1
                if craftGridCounts(i) <= 0 then
                  craftGridBlocks(i) = Block.Air
                  craftGridCounts(i) = 0
              i += 1
          if quick then
            moveInto(recipe.output, recipe.outputCount, (backpackSlots ++ hotbarSlots).reverse)
            noteCollected(recipe.output, recipe.outputCount)
          else gainItem(recipe.output, recipe.outputCount)
          crafting = quick && currentCraftingResult.contains(recipe) && canCraftCurrent(recipe)
        if quick then
          syncInventoryTotalsFromBackpack(); compactHotbarAssignments()
        playPlaceSound()
      case Some(_) => addChatMessage("Missing crafting ingredients")
      case None    => addChatMessage("No matching recipe")

  protected def smeltResult(input: Block): Option[(Block, Int)] =
    InventoryModel.smeltResult(input)

  /** Migrate the old input selector once, without duplicating items already in the backpack. */
  protected def enableFurnaceSlots(): Unit =
    if !furnaceSlots then
      val amount =
        if furnaceInput != Block.Air then totalItemCount(furnaceInput).min(maxStackSize) else 0
      if amount > 0 && consumeInventory(furnaceInput, amount) then furnaceInputCount = amount
      else furnaceInput = Block.Air
      furnaceFuel = Block.Air; furnaceFuelCount = 0; furnaceSlots = true

  protected def furnaceInputAvailable: Int =
    if furnaceSlots then furnaceInputCount else totalItemCount(furnaceInput)

  /** Input and fuel slots use the same split/merge/swap rules as ordinary inventory slots. */
  protected def handleFurnaceSlot(fuel: Boolean, rightClick: Boolean, quick: Boolean): Unit =
    enableFurnaceSlots()
    val blocks = Array(if fuel then furnaceFuel else furnaceInput)
    val counts = Array(if fuel then furnaceFuelCount else furnaceInputCount)
    val before = blocks(0)
    if quick then quickMove(blocks, counts, 0, backpackSlots ++ hotbarSlots)
    else if heldInventoryBlock == Block.Air then
      if counts(0) > 0 then
        val take = if rightClick then (counts(0) + 1) / 2 else counts(0)
        setHeldItem(blocks(0), take, fromHotbar = true); counts(0) -= take
    else if (
        if fuel then InventoryModel.fuelBurnTime(heldInventoryBlock) > 0
        else smeltResult(heldInventoryBlock).nonEmpty
      )
    then
      if counts(0) <= 0 || blocks(0) == heldInventoryBlock then
        val amount = (if rightClick then 1 else heldInventoryCount).min(maxStackSize - counts(0))
        blocks(0) = heldInventoryBlock; counts(0) += amount; heldInventoryCount -= amount
        if heldInventoryCount <= 0 then clearHeldItem()
      else if !rightClick then
        val oldBlock = blocks(0); val oldCount = counts(0)
        blocks(0) = heldInventoryBlock; counts(0) = heldInventoryCount
        setHeldItem(oldBlock, oldCount, fromHotbar = true)
    if counts(0) <= 0 then blocks(0) = Block.Air
    if fuel then { furnaceFuel = blocks(0); furnaceFuelCount = counts(0) }
    else
      furnaceInput = blocks(0); furnaceInputCount = counts(0)
      if before != furnaceInput then { furnaceRequested = 0; furnaceProgress = 0 }
    furnacePaused = false

  protected def quickFeedFurnace(blocks: Array[Block], counts: Array[Int], slot: Int): Unit =
    enableFurnaceSlots()
    val block = blocks(slot)
    val fuel = smeltResult(block).isEmpty && InventoryModel.fuelBurnTime(block) > 0
    if smeltResult(block).nonEmpty || fuel then
      val target = if fuel then furnaceFuel else furnaceInput
      val existing = if fuel then furnaceFuelCount else furnaceInputCount
      if target == block || existing == 0 then
        val moved = counts(slot).min(maxStackSize - existing)
        if moved > 0 then
          counts(slot) -= moved
          if counts(slot) == 0 then blocks(slot) = Block.Air
          if fuel then { furnaceFuel = block; furnaceFuelCount += moved }
          else { furnaceInput = block; furnaceInputCount += moved }
          syncInventoryTotalsFromBackpack(); compactHotbarAssignments()

  protected def clickFurnaceOutput(rightClick: Boolean, quickMove: Boolean): Unit =
    if quickMove then takeFurnaceOutput()
    else if furnaceOutputCount > 0 && (heldInventoryBlock == Block.Air || heldInventoryBlock == furnaceOutput)
    then
      val amount = (if rightClick then (furnaceOutputCount + 1) / 2 else furnaceOutputCount)
        .min(maxStackSize - heldInventoryCount)
      if amount > 0 then
        val previous = heldInventoryCount
        setHeldItem(furnaceOutput, previous + amount, fromHotbar = true)
        furnaceOutputCount -= amount
        if furnaceOutputCount == 0 then furnaceOutput = Block.Air

  protected def canConsumeFuelFor(input: Block): Boolean =
    if furnaceSlots then
      furnaceFuelRemaining + furnaceFuelCount * InventoryModel.fuelBurnTime(furnaceFuel) >= 1f
    else
      furnaceFuelRemaining + fuelBlocks
        .map(b =>
          (totalItemCount(b) - (if b == input then 1 else 0)).max(0) * InventoryModel.fuelBurnTime(
            b
          )
        )
        .sum >= 1f

  protected def consumeFuelFor(input: Block): Boolean =
    if furnaceSlots then
      while furnaceFuelRemaining < 1f && furnaceFuelCount > 0 do
        furnaceFuelCount -= 1
        furnaceFuelRemaining += InventoryModel.fuelBurnTime(furnaceFuel)
        if furnaceFuelCount == 0 then furnaceFuel = Block.Air
      return furnaceFuelRemaining >= 1f
    while furnaceFuelRemaining < 1f do
      val picked = fuelBlocks.find(b => totalItemCount(b) > (if b == input then 1 else 0))
      picked match
        case Some(fuel) =>
          if !consumeInventory(fuel, 1) then return false
          furnaceFuel = fuel
          furnaceFuelRemaining += InventoryModel.fuelBurnTime(fuel)
        case None => return false
    true

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
      if showMessages then addChatMessage("Choose an input stack below")
      return false
    if furnaceInputAvailable <= 0 then
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
    if furnaceOutputCount + amount > maxStackSize then
      if showMessages then addChatMessage("Furnace output is full")
      return false
    if !consumeFuelFor(furnaceInput) then
      if showMessages then addChatMessage("Need fuel: Coal or Wood")
      return false
    if furnaceSlots then furnaceInputCount -= 1 else consumeInventory(furnaceInput, 1)
    furnaceFuelRemaining = (furnaceFuelRemaining - 1f).max(0f)
    furnaceOutput = out
    furnaceOutputCount += amount
    noteSmelted(out, amount)
    if furnaceInputAvailable <= 0 then furnaceInput = Block.Air
    if furnaceFuelRemaining <= 0f then
      furnaceFuelRemaining = 0f
      if !furnaceSlots then furnaceFuel = Block.Air
    playPlaceSound()
    true

  protected def smeltFurnace(): Unit =
    requestSmelting(1)

  protected def smeltAllFurnace(): Unit =
    requestSmelting(furnaceInputAvailable.min(maxStackSize))

  private def requestSmelting(count: Int): Unit =
    if !furnaceAvailable then addChatMessage("Open a placed furnace first")
    else if furnaceInput == Block.Air || furnaceInputAvailable <= 0 then
      addChatMessage("Choose an input first")
    else if !canConsumeFuelFor(furnaceInput) then
      addChatMessage("Need fuel: coal, logs, planks or sticks")
    else if smeltResult(furnaceInput).exists((out, _) =>
        furnaceOutput != Block.Air && furnaceOutput != out
      )
    then addChatMessage("Take the current output first")
    else if smeltResult(furnaceInput).exists((_, amount) =>
        furnaceOutputCount + amount > maxStackSize
      )
    then addChatMessage("Take the output to make room")
    else { furnacePaused = false; furnaceRequested = count.max(1).min(maxStackSize) }

  protected def updateFurnace(dt: Float): Unit =
    if furnaceSlots && !furnacePaused && furnaceRequested <= 0 && furnaceAvailable && furnaceInputAvailable > 0 && canConsumeFuelFor(
        furnaceInput
      ) && smeltResult(furnaceInput).exists((out, n) =>
        (furnaceOutput == Block.Air || furnaceOutput == out) && furnaceOutputCount + n <= maxStackSize
      )
    then furnaceRequested = furnaceInputAvailable
    if furnaceRequested > 0 then
      if !furnaceAvailable || furnaceInputAvailable <= 0 || (!canConsumeFuelFor(
          furnaceInput
        ))
      then
        furnaceRequested = 0; furnaceProgress = 0f
      else
        furnaceProgress += dt
        if furnaceProgress >= 4f then
          furnaceProgress -= 4f
          if smeltOneInternal(showMessages = true) then furnaceRequested -= 1
          else furnaceRequested = 0
          if furnaceRequested <= 0 then furnaceProgress = 0f
    noteFurnaceBurning(furnaceRequested > 0)
