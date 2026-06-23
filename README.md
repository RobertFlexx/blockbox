# Blockbox

Blockbox is a free and open source Scala voxel sandbox prototype. It aims for familiar blocky worlds while building its own identity, mechanics, and codebase.

## Current State

This is a first playable slice:

- Main menu, settings screen, pause flow, and in-game HUD text.
- Procedural blocky terrain with grass, dirt, stone, sand, water, wood, planks, glass, and leafy blocks.
- Chunk-style mesh building with hidden faces skipped before upload.
- Survival and creative modes with mouse look, block breaking, and block placement.
- Framebuffer-aware rendering for resized/HiDPI windows.
- Larger Minecraft-like worlds with beaches, mountains, snow, caves, ores, forests, shrubs, water, and simple generated structures.
- Render-distance settings with dynamic mesh rebuilds around the player.
- Gravity, jumping, basic player collision, hotbar inventory, and simple wandering mobs.
- Generated pixel-art block textures, translucent water/glass/leaves, smoother biome terrain, cave carving, improved mobs, health bars, and damage feedback.
- Accurate crosshair-based voxel picking for block breaking and placement.
- Sprinting, swimming, buoyancy, underwater fog, and a subtle underwater overlay.
- Scala 3 + LWJGL, no proprietary assets.

## Requirements

- Java 21 or newer.
- `scala-cli`.
- Linux desktop with OpenGL support.

## Run

```bash
bash scripts/run.sh
```

The first run downloads LWJGL dependencies and Linux native libraries.

On Linux, X11/XWayland is the supported launch path right now. If your desktop session is Wayland and the window does not appear, run:

```bash
GLFW_PLATFORM=x11 bash scripts/run.sh
```

## Controls

- `Enter`: Start game from the main menu.
- `S`: Settings from the main menu.
- `Esc`: Pause in game, return/back in menus.
- `W/A/S/D`: Move.
- `Space`: Jump in survival, move up in creative.
- `Left Control`: Sprint in survival.
- `Left Shift`: Swim down in survival water, move down in creative.
- Mouse: Look around in game.
- Left mouse: Break targeted block.
- Right mouse: Place selected block.
- `1`-`9`, `0`: Select build block.
- `R`: Regenerate the world.
- `M`: Toggle survival/creative mode.
- `F`: Toggle fast movement in settings.
- `V`: Toggle VSync in settings.
- `Left`/`Right` or `-`/`+`: Adjust render distance in settings.

## Direction

Next milestones should add saved worlds, richer terrain biomes, inventory/crafting, audio, configurable controls, better mob AI, and multiplayer experiments.
