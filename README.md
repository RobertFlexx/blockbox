# blockbox

blockbox is a free and open source voxel sandbox made in scala 3 with lwjgl.
blockbox *launcher* is **[here](https://github.com/RobertFlexx/blockbox-launcher)**
this is not some giant studio project. it is made by one guy, robertflexx. i work very hard on it and i am building it piece by piece because i like blocky games, weird game engines, linux, jvm stuff, and making things from scratch.

the goal is to have a familiar blocky sandbox feeling, but not just be a cheap copy of anything. blockbox has its own codebase, its own messy little history, and its own direction.

it is still early, still rough, and still changing a lot, but it is already playable.
(again, like i said, its VERY early, like early early beta and i have a lot done already. im not very good at UI, please dont bash me for it. expect tons of bugs, but im working on it)

## what it is right now

blockbox is a first playable slice of a voxel sandbox.

right now it has:

* a main menu
* a new world screen
* a load world screen
* settings
* pause menu
* in-game hud
* survival and creative modes
* mouse look
* block breaking
* block placing
* hotbar
* inventory work
* world saving and loading
* procedural terrain
* chunks
* caves
* beaches
* mountains
* snow
* forests
* shrubs
* ores
* water
* simple structures
* simple mobs
* health and food ui
* sprinting
* swimming
* buoyancy
* underwater fog
* crosshair based block picking
* generated pixel style block textures
* transparent glass, water, and leaves
* render distance settings
* basic multiplayer experiments
* chat
* commands
* no proprietary assets

it is not finished, but it is real and it runs.

## why i am making it

i wanted to make a block game that feels handmade.

not a polished corporate thing. not a template. not a fake project that only has a screenshot and nothing else. and no paid bullshit, fully free and fucking open source and auditable. if you dont like somethin, patch it. im all open.

i wanted something that actually has code, terrain, saving, ui, bugs, fixes, multiplayer attempts, weird experiments, and all the stuff that happens when one person is trying to build a game for real.

blockbox is me learning, building, fixing, breaking things, and making it better over time.

## requirements

you need:

* java 21 or newer
* scala-cli
* opengl support
* a desktop that can run lwjgl

linux is the main setup right now, but windows launch scripts are included in newer builds.

## running on linux

from the project folder:

```bash
bash scripts/run.sh
```

the first run will download scala and lwjgl dependencies through scala-cli.

if you are on wayland and the window does not show up, try forcing x11:

```bash
glfw_platform=x11 bash scripts/run.sh
```

## running on windows

newer builds include:

```text
run-blockbox-windows.bat
```

or:

```text
scripts\run-blockbox-windows.bat
```

the windows script tries to set up what it needs and loads the lwjgl windows native jars.

## controls

basic controls:

* `w/a/s/d` moves
* mouse looks around
* left click breaks a block
* right click places a block
* `space` jumps in survival
* `space` moves up in creative
* `left control` sprints
* `left shift` swims down in water or moves down in creative
* `1` to `9` and `0` select hotbar slots
* `e` opens inventory or creative catalog
* `esc` pauses
* `t` opens chat
* `/` opens chat with command mode
* `tab` can autocomplete commands
* up and down can move through command suggestions
* `f3` toggles debug info
* `f4` toggles wireframe
* `f5` toggles chunk borders

settings controls:

* `f` toggles fast movement
* `v` toggles vsync
* left and right, or `-` and `+`, adjust render distance
* `p` changes how escape behaves from the pause menu

## commands

some commands exist for testing and messing with worlds.

examples:

```text
/help
/enablecheats
/gamemode creative
/gamemode survival
/timeset day
/timeset night
/fly
/tp playername
/tp player1 player2
/tp 0 90 0
/tp playername 0 90 0
```

cheat commands need cheats enabled. in multiplayer, clients should not be able to just change their own gamemode from settings.

## world saves

blockbox saves worlds into:

```text
worlds/
```

each world gets its own folder.

the newer save system tries to be safer by writing temporary files first, then replacing the real save files after the write finishes. corrupt chunks should be skipped instead of killing the whole world load.

it is still early, but the goal is simple:

when you leave a world and come back, it should look like how you left it.

## multiplayer

multiplayer is experimental.

there is lan hosting and joining, player names, chat, player positions, block updates, and some server side checks.

it is not perfect yet. chunk sync and usernames are still being improved. the goal is to make it feel more like a real little server instead of just a quick hacked together connection.

## current problems

blockbox is still a prototype, so there are bugs.

things that still need work:

* better multiplayer chunk sync
* better player identity handling on servers
* better inventory dragging
* better mobs
* better animations
* better world generation
* better sound
* better crafting
* better ui polish
* better performance
* better save management
* better structure generation

i am not pretending it is done. i am working on it.

## direction

i want blockbox to become a fun handmade voxel sandbox with:

* better survival
* better creative building
* better terrain
* better caves
* better mobs
* better structures
* better multiplayer
* better ui
* better world saves
* real crafting
* more blocks
* more items
* more reasons to explore

it is made by one guy, robertflexx, and i work very hard on it.

that is basically the whole spirit of the project.
