# Survival in blockbox

Press **J** for the field journal (four pages at normal sizes, with larger readable rows on compact screens). It records nineteen survival, building, exploration and combat objectives, shows recipes,
and saves progress with the world. Track a goal to pin it to the HUD; completed goals
have a one-time reward. A full inventory leaves the reward available to collect later. Press **E** for the 3×3 crafting grid.
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
The furnace uses the same nine-column inventory slots as the backpack. Click a smeltable
stack to select it; shift-click to queue the whole stack. Output is collected by clicking
the output slot. Stop cancels the pending batch without spending unprocessed input.
A running furnace changes its placed texture, emits warm light and small flame/smoke
particles, and plays a quiet positional fire loop. It keeps working when you walk away;
its chunk stays loaded until it finishes. Breaking it stops the batch. Multiplayer
ignition/extinguishing is validated and relayed by the host, and disconnects release
the player's running fire state.

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

Zombies spawn in small groups 24–80 blocks from players, in unlit caves and outside
at night. Every member checks ground support, available space, zero block light,
and distance from every player. Place torches
to protect your base and mine. A coal item above a stick makes four torches. Zombies
approach with their arms out, route around walls, jump up a block, avoid dangerous
drops and cactus, and attack at close range. They remember the last place they saw
you for six seconds. They acquire players only with unobstructed sight within 32
blocks, and cannot track a player's changing position through walls. They spread out when
crowded and search again when a route is blocked.
Without a target they pause, look for safe nearby destinations and wander at a
slower pace. They float with their heads above water, keep pursuing while swimming,
and climb accessible banks. Perception runs at staggered intervals; path searches
share a bounded work budget while collision and smooth steering update every tick.
This is deterministic perception and navigation, rather than a neural network.
Attack with left click; swords deal the most damage. Defeated zombies drop coal, with
a small chance of an iron ingot. Exposed daylight ignites them; roofs, rain and water
protect them from the sun. Damage briefly flashes their skin red. Their groans,
attacks, pain and deaths use recorded monster voice performances, with distance,
stereo positioning, and muffling through walls or water.

Eight percent spawn carrying a tool or weapon. All sixteen tools are possible;
wood and stone are more common than iron and diamond. Armed zombies hit harder and
have a fifteen percent chance to drop their equipment when defeated. Their held
items, damage flashes, movement and voices are synchronized in multiplayer.
Sixty-four cached appearances vary skin, clothes, scars, hair and tears. The mob ID
selects the same appearance on every peer and after loading. Equipped zombies hold
their weapons upright in their extended hands.

Snow steps use four separate crisp recordings by qubodup. Stone breaking and
placing use Spring Spring's break and chip recordings. Sand walking alternates six
Fantozzi performances; sand placing and breaking have gentler granular variations.
Water entry uses two Peludo splash recordings, and PagDev's fireplace recording
provides the quiet furnace ambience. These are CC0 public-domain
dedications; source links and file hashes are in `assets/audio/CREDITS.md` and
`assets/audio/manifest.csv`.

Players can fight in multiplayer with left click. Aim within three and a half blocks;
solid walls block attacks, and hits have a half-second cooldown. Swords do more damage
and hits push players back. Creative players are immune. The host validates the target,
reach, view direction, obstruction and weapon damage.

The inventory has a live character preview, nine-column storage, the crafting grid
and the hotbar. The creative catalog has category tabs and a search field; click an
item, then a hotbar slot to equip its stack. Right click a catalog hotbar slot to clear it.

Multiplayer uses protocol **9**. All players need this version. The host controls
zombie movement, spawning, attacks and drops; held tools and blocks are synchronized.

## Places to explore

Willow marshes have winding shallow channels, muddy banks and low grassy islands.
Reed beds follow the banks and lily pads grow in patches on shallow water, beneath
forked willows with hanging leaf curtains. Autumn groves have amber and golden
crowns around open glades, patchy earth, scattered leaf litter, fallen trunks and
rocky outcrops. Ground cover has small cutout meshes, can be harvested and placed,
and disappears when its support is removed. Plants do not block movement.

Rocky coasts expose stone shores; dune deserts have wind-shaped ridges; badlands
have layered mesas; snowy taiga and giant redwoods give forests different scales.
Smaller coherent regional masks make these variations easier to encounter
on an ordinary exploration trip. Red-desert badlands grow cacti on clay and do not
spawn tree anchors. Existing saved blocks stay intact; new terrain uses the updated
generator. Biome enum IDs are appended, preserving older discovery records.

Ground movement uses the same walking/sprinting profile in both game modes. Flight
collides with walls, and leaving it resets view FOV and gait state. The world camera
stays steady while the hand/tool bobs. The projection near plane fits inside the eye's
collision clearance, including at wide FOVs and ultrawide aspect ratios. Hurt colour
holds a single restrained red overlay for half a second, with no repeated strobe.

Short recordings trim quiet lead-in and preload during the title screen. The mixer
uses 256-frame quanta, flushes every pipe write and drops stale queued impacts;
block actions trigger audio before mesh work. Output devices add their own latency.

See `FORMATTING.md` for the repeatable formatting command and release checks.

## Dropped items and shared worlds

Breaking a block releases a physical item. Walk close to collect it; a full inventory
leaves it in the world. Press **Q** to throw one held item, or **Ctrl+Q** to throw the
whole stack. Other players can collect it. Items spin, bob, merge into stacks and
float in water; unattended drops disappear after five minutes. The host validates
and reserves multiplayer pickups so two people cannot collect the same stack.

Guests returning under the same player name resume their last position and facing
for that hosted world and session. The same running client also retains inventory,
journal progress and unfinished item transfers for that exact session. A new host
session starts fresh guest session state.

Nearby footsteps, landings, splashes and accepted block actions are audible in
multiplayer, with stereo direction, distance falloff and muffling through cover.
Hold **Shift** to crouch: the camera lowers, the body bends and other players see
the pose. Crouching reduces collision height and prevents standing into a low roof.
Combat knockback applies in PvP and against zombies, with a short recovery indicator
under the crosshair. The field-of-view slider supports 50–120 degrees.

Ruins in newly generated terrain can contain sealed **Supply Crates**. Break a crate
to recover planks, useful supplies, gold and occasional diamonds. The journal's
“Salvage the old world” goal rewards finding three caches. Each generated cache can
be salvaged once; the broken block is saved like other world edits.
