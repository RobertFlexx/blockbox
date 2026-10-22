# Survival in blockbox

Press **J** for the two-page field journal. It records your progress, shows recipes,
and saves completed goals with the world. Press **E** for the 3×3 crafting grid.
Right click an inventory stack to split it; right click a crafting slot to place one item.

## Getting established

1. Break logs by hand and craft each log into four matching planks.
2. Stack two planks vertically to make four sticks. Any wood species works for tools.
3. Make a wooden pickaxe. Mine stone, then make stone tools and a furnace.
4. Put eight stone around an empty center to craft the furnace. Place it and right click it.
5. Mine iron with a stone pickaxe. Select iron ore in the furnace, supply coal or wood,
   and choose **Smelt One** or **Smelt All**. Each item takes four seconds. Take the output.
6. Make an iron pickaxe, explore below Y 20, and look for small diamond deposits.
   Diamond gems make the fastest tools.

Furnaces draw input and fuel from your inventory. Coal smelts eight items, a log two,
a plank one and a half, and two sticks one. Smelt a log to make charcoal (the coal item).
The input list scrolls when more than six materials are available.

## Tool recipes

`M` is the head material, `S` is a stick, and `.` is an empty slot. The head material
can be planks, stone, iron ingots, or diamond gems. Axes also accept the mirrored shape.

| Pickaxe | Axe | Shovel | Sword |
| --- | --- | --- | --- |
| `MMM` | `MM.` | `.M.` | `.M.` |
| `.S.` | `MS.` | `.S.` | `.M.` |
| `.S.` | `.S.` | `.S.` | `.S.` |

Pickaxes mine rock and ore, axes chop wood, and shovels dig earth, sand, snow and clay.
Swords cut leaves and deal more damage in combat. Using the right tool is faster;
using an insufficient pickaxe breaks the ore without yielding its resource.

| Resource | Minimum pickaxe | Where to look |
| --- | --- | --- |
| Stone and coal | Wood | Common, including shallow hillsides |
| Copper and iron | Stone | Copper below Y 88; iron below Y 72 |
| Gold | Iron | Smaller, less common deposits below Y 34 |
| Diamond | Iron | Rare small deposits below Y 20 |

Deposits occur in clusters, with coal and copper more common than precious ores.
New ore placement applies to newly generated chunks. Previously saved chunks keep their blocks.
Iron, copper and gold ore smelt into ingots. Coal ore yields coal, and diamond ore yields gems.

## Water, weather and the night

You have fifteen seconds of air. Blue bubbles above the hearts show your remaining
breath. Once they disappear, drowning deals two health per second. Surface to refill
your air; creative mode has unlimited breath.

Rain falls in moving storm regions. Clouds, fog, light, surface splashes and water
ripples follow the weather. Roofs and tree canopies stop the local rain. Underwater
vision has distance absorption, subtle refraction and moving light on the bottom;
the submerged portion of a character takes the water's color.

Zombies spawn away from players in unlit caves and outside at night. Place torches
to protect your base and mine. A coal item above a stick makes four torches. Zombies
approach with their arms out, route around walls, jump up a block, avoid dangerous
drops and cactus, and attack at close range. They remember the last place they saw
you for six seconds and can hear nearby players behind cover. They spread out when
crowded and search again when a route is blocked.
Attack with left click; swords deal the most damage. Defeated zombies drop coal, with
a small chance of an iron ingot. Exposed daylight ignites them; roofs, rain and water
protect them from the sun. Damage briefly flashes their skin red. Their groans,
attacks, pain and deaths use recorded monster voice performances, with distance,
stereo positioning, and muffling through walls or water.

Eight percent spawn carrying a tool or weapon. All sixteen tools are possible;
wood and stone are more common than iron and diamond. Armed zombies hit harder and
have a fifteen percent chance to drop their equipment when defeated. Their held
items, damage flashes, movement and voices are synchronized in multiplayer.

Players can fight in multiplayer with left click. Aim within three and a half blocks;
solid walls block attacks, and hits have a half-second cooldown. Swords do more damage
and hits push players back. Creative players are immune. The host validates the target,
reach, view direction, obstruction and weapon damage.

The inventory has a live character preview, nine-column storage, the crafting grid
and the hotbar. The creative catalog has category tabs and a search field; click an
item, then a hotbar slot to equip its stack. Right click a catalog hotbar slot to clear it.

Multiplayer uses protocol **8**. All players need this version. The host controls
zombie movement, spawning, attacks and drops; held tools and blocks are synchronized.
