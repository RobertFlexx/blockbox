# Recorded sound credits

All recordings in this directory are available under **CC0 1.0 Universal**:
<https://creativecommons.org/publicdomain/zero/1.0/>. They are separate assets
from the game's source-code license. No Minecraft audio is included.

| Files | Creator / original collection | Source |
| --- | --- | --- |
| `kenney/*.ogg` (35 files: grass, concrete, snow and wood footsteps; mining, wood, glass, metal and soft impacts) | Kenney, **Impact Sounds**, 2019 | https://kenney.nl/assets/impact-sounds |
| `weather/rain-loop.ogg` | Kresiek The Furry, **AMB Rain Loop 2**, 2025 | https://opengameart.org/content/amb-rain-loop-2 |
| `weather/thunder-distant.ogg` | rubberduck, **100 CC0 SFX #2**, 2018, `sfx100v2_thunder_01` | https://opengameart.org/content/100-cc0-sfx-2 |
| `foley/snow_step_000.ogg`–`003.ogg` | Iwan "qubodup" Gabovitch, **4 dry snow steps**, 2009 | https://opengameart.org/content/4-dry-snow-steps |
| `foley/stone_break.ogg`, `foley/stone_chip.ogg` | Julie Damsgaard / Spring Spring, **Various Sound Effects**, 2024 (`break_stone.wav`, `small_rock_impact.wav`) | https://opengameart.org/content/various-sound-effects-0 |

The Kenney files were retrieved unchanged from the `sample/assets/audio/impact`
directory in https://github.com/iree-gd/iree.gd. The weather files were retrieved
from https://github.com/Julian-adv/OpenMMO/tree/master/client/public/sounds,
whose provenance is documented in `doc/assets/sfx.md`. That distributor's rain
edit uses seconds 505–545 of the original recording with a three-second
self-crossfade, producing a 37-second loop. The thunder recording is unchanged.
Only the two explicitly CC0 weather recordings were imported from that repository.

`manifest.csv` records each included file's SHA-256, original filename and download
location. The runtime decoder resamples to 44.1 kHz, removes DC, applies headroom
and short one-shot fades, and smooths the rain loop boundary. Playback uses small
pitch variations and stereo positioning. Procedural foley supplements materials
for which this set has no recording.

The six `foley` recordings were downloaded directly from OpenGameArt. Their
source pages explicitly grant CC0. They replace the snow walking and stone
breaking/placing selections. The sources were downmixed to mono 44.1 kHz Vorbis
with short boundary fades; the stone break's silent tail was trimmed. The snow
clips preserve four separate performances, rather than pitches of one step.

## Zombie voices

`zombie/*.ogg` contains ten voice performances by **artisticdude** from the
[Zombies Sound Pack](https://opengameart.org/content/zombies-sound-pack), created
for the free and open-source RPG Summoning Wars (2011). The original pack is
released under [CC0 1.0](https://creativecommons.org/publicdomain/zero/1.0/).
Source archive: <https://opengameart.org/sites/default/files/zombies.zip>.

The source WAVs were converted to mono 44.1 kHz Vorbis. These are recorded monster
voices, with separate ambient, hurt, attack and death variations; their original
pitch is preserved. The runtime applies positional attenuation and stereo pan.
The original numbered WAV is recorded in each manifest entry.

## Sand and furnace

`foley/sand_place_000.ogg`–`002.ogg` and `sand_break_000.ogg`–`002.ogg`
use Fantozzi's six sand performances, sliced by qubodup in
[Fantozzi's Footsteps (Grass/Sand & Stone)](https://opengameart.org/content/fantozzis-footsteps-grasssand-stone)
(2014, CC0). Placement uses the L1–L3 takes with a gentler low-pass; breaking
uses R1–R3. Mono 44.1 kHz conversion and short fades avoid sharp boundary clicks.

`foley/furnace_loop.ogg` and `furnace_ignite.ogg` use PagDev's
[Fireplace Sound loop](https://opengameart.org/content/fireplace-sound-loop)
(2021, CC0), `fire.wav`. The loop uses seconds 8–16 and the ignition uses
seconds 4–4.5. Both are filtered to remove rumble and harsh high frequencies;
the loop has gentle compression and runtime wrap crossfading. Furnace ambience
is quiet, fades in/out, and attenuates with distance; it does not stack without limit.

`foley/water_splash_000.ogg` and `001.ogg` use Peludo's `splash1.wav` and
`splash2.wav` from [Water Splash and sand footsteps](https://opengameart.org/content/water-splash-and-sand-footsteps)
(2021, CC0). Creator's project: [RNAn on itch.io](https://rnan.itch.io/),
referenced at the creator's request. Mono conversion, gentle compression,
low/high-pass filtering and boundary fades keep water entry soft and natural.
Sand walking now alternates the six recorded Fantozzi takes listed above.
