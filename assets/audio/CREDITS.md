# Recorded sound credits

All recordings in this directory are available under **CC0 1.0 Universal**:
<https://creativecommons.org/publicdomain/zero/1.0/>. They are separate assets
from the game's source-code license. No Minecraft audio is included.

| Files | Creator / original collection | Source |
| --- | --- | --- |
| `kenney/*.ogg` (35 files: grass, concrete, snow and wood footsteps; mining, wood, glass, metal and soft impacts) | Kenney, **Impact Sounds**, 2019 | https://kenney.nl/assets/impact-sounds |
| `weather/rain-loop.ogg` | Kresiek The Furry, **AMB Rain Loop 2**, 2025 | https://opengameart.org/content/amb-rain-loop-2 |
| `weather/thunder-distant.ogg` | rubberduck, **100 CC0 SFX #2**, 2018, `sfx100v2_thunder_01` | https://opengameart.org/content/100-cc0-sfx-2 |

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
