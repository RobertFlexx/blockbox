# blockbox modding

blockbox has early modding support.

it is not trying to be a locked down tiny scripting toy. the goal is real modding. mods can add commands, draw gui, listen to game events, touch world state, give items, change blocks, react to players, and eventually add bigger systems.

groovy is the main scripting language for blockbox mods because blockbox already runs on the jvm. that means groovy can feel like a script, but still talk directly to the game.

jar mods can also be used, so java, kotlin, scala, clojure, groovy, and other jvm languages can work too.

the basic idea is:

```text
engine: scala 3
primary mod scripts: groovy
compiled mods: jar files
mod folder: mods/
```

blockbox mods are trusted code, like minecraft java mods. do not install random mods from people you do not trust.

## current modding status

modding is still early, but it is real.

right now blockbox has:

* a `mods/` folder
* groovy script loading
* groovy scripts inside jar files
* jar mod metadata
* mod descriptions
* mod side detection
* client mods
* server mods
* both-side mods
* server-interactive modpack checks
* mod command registration
* mod command autocomplete
* mod gui drawing
* hud render hooks
* world tick hooks
* block break hooks
* block place hooks
* chat hooks
* player api
* world api
* gui api
* scheduler api
* config/data folder api
* basic custom block registration
* loaded mods screen
* safer error handling for broken mod callbacks

it is not forge. it is not fabric. it is blockbox doing its own simple jvm modding system.

it will grow over time.

## mods folder

mods go in:

```text
mods/
```

a mod can be either a folder:

```text
mods/
  my_mod/
    blockbox.mod.json
    main.groovy
```

or a jar:

```text
mods/
  my_mod.jar
```

jar mods should have `blockbox.mod.json` at the root of the jar.

groovy jar mods can also include `main.groovy` at the root of the jar.

## blockbox.mod.json

every proper mod should have a `blockbox.mod.json`.

example:

```json
{
  "id": "rainbow_control",
  "name": "Rainbow Control",
  "version": "1.0.0",
  "description": "adds a rainbow command and a small collapsible hud panel",
  "side": "both",
  "serverInteractive": true,
  "main": "main.groovy"
}
```

fields:

* `id` is the stable mod id
* `name` is the display name
* `version` is the mod version
* `description` shows in the mods screen
* `side` says where the mod runs
* `serverInteractive` says whether the mod affects server/game state
* `main` points to the groovy script or main class

## mod sides

blockbox supports three basic sides.

```text
client
server
both
```

a client mod is for local ui, visuals, hud changes, client helpers, or other stuff that does not change the shared game state.

a server mod is for world behavior, commands, server checks, block changes, gameplay rules, and multiplayer state.

a both-side mod has client and server behavior.

examples:

```json
{
  "side": "client",
  "serverInteractive": false
}
```

good for:

* huds
* gui helpers
* visual overlays
* local settings panels
* cosmetic client features

```json
{
  "side": "server",
  "serverInteractive": true
}
```

good for:

* commands that change the world
* gameplay rules
* block behavior
* survival systems
* server side events

```json
{
  "side": "both",
  "serverInteractive": true
}
```

good for:

* a mod that has a client gui and server gameplay behavior
* custom blocks
* custom commands
* tools that affect the world

## multiplayer mod checks

server-interactive mods become part of the multiplayer modpack hash.

that means if a host has a server-side gameplay mod, clients need the same mod installed before joining.

client-only mods do not block joining.

this is important because blockbox does not want fake multiplayer where the host has different gameplay rules than the client. server-side mods need to match so the game does not desync or lie.

basic rule:

```text
client-only mod = does not need to match
server or both interactive mod = needs to match
```

example:

```json
{
  "side": "both",
  "serverInteractive": true
}
```

this will be checked when joining a host game.

example:

```json
{
  "side": "client",
  "serverInteractive": false
}
```

this should not block joining.

## basic groovy mod

a simple groovy mod can look like this:

```groovy
def init(api) {
  api.log.info("hello from my mod")

  api.commands.register("hello") { ctx ->
    ctx.reply("hello from groovy")
  }
}
```

put it here:

```text
mods/
  hello_mod/
    blockbox.mod.json
    main.groovy
```

## commands

mods can register commands.

example:

```groovy
def init(api) {
  api.commands.register("heal") { ctx ->
    ctx.player.health = 20
    ctx.player.food = 20
    ctx.reply("healed")
  }
}
```

a command can also be more descriptive:

```groovy
def init(api) {
  api.commands.register(
    "rainbow",
    "gives rainbow blocks",
    "/rainbow [amount]",
    true
  ) { ctx ->
    def amount = ctx.argInt(0, 1)
    ctx.player.give("RainbowBlock", amount)
    ctx.reply("gave rainbow block x" + amount)
  }
}
```

command context usually gives you:

* the command name
* arguments
* player name
* player reference
* reply helper
* argument helpers

## events

events are how mods hook into the game.

instead of editing the engine directly, a mod listens for something and reacts to it.

example:

```groovy
def init(api) {
  api.events.onBlockBreak { e ->
    api.log.info(e.playerName + " broke " + e.block)
  }
}
```

example cancelling behavior:

```groovy
def init(api) {
  api.events.onBlockBreak { e ->
    if (e.block.toString() == "Diamond") {
      e.cancelled = true
      e.player.sendMessage("no breaking diamonds")
    }
  }
}
```

common event types:

* block break
* block place
* hud render
* world tick
* world loaded
* chat message

events are meant to be the clean way to make gameplay mods.

## world api

mods can read and change blocks through the world api.

example:

```groovy
def init(api) {
  api.commands.register("stonehere") { ctx ->
    def p = ctx.player
    api.world.setBlock(p.blockX, p.blockY - 1, p.blockZ, "Stone")
    ctx.reply("placed stone under you")
  }
}
```

common world api ideas:

```groovy
api.world.getBlock(x, y, z)
api.world.setBlock(x, y, z, "Stone")
api.world.fill(x1, y1, z1, x2, y2, z2, "Glass")
api.world.seed
api.world.name
```

mods should use the world api instead of trying to directly mutate chunk arrays.

the world api is supposed to handle the real engine work:

* changing the block
* marking chunks dirty
* rebuilding meshes
* syncing multiplayer
* keeping the save correct

## player api

mods can use player references.

example:

```groovy
def init(api) {
  api.commands.register("whereami") { ctx ->
    def p = ctx.player
    ctx.reply("you are at " + p.x + ", " + p.y + ", " + p.z)
  }
}
```

useful player fields and helpers:

```groovy
ctx.player.name
ctx.player.x
ctx.player.y
ctx.player.z
ctx.player.blockX
ctx.player.blockY
ctx.player.blockZ
ctx.player.give("Stone", 64)
ctx.player.teleport(0, 90, 0)
ctx.player.sendMessage("hello")
ctx.player.health = 20
ctx.player.food = 20
ctx.player.op
```

server-side mods should prefer player api calls instead of touching internal player state directly.

## gui api

client mods can draw gui.

example:

```groovy
def init(api) {
  def open = true

  api.events.onHudRender { e ->
    def gui = e.gui

    if (!gui.isPlaying) {
      return
    }

    if (gui.buttonClicked(24, 80, 180, 28, open ? "hide panel" : "show panel")) {
      open = !open
    }

    if (open) {
      gui.panel(24, 116, 260, 120, "my mod")
      gui.textShadow(40, 150, "hello from a mod", 1, 1, 1, 1)
    }
  }
}
```

gui helpers include:

```groovy
gui.width
gui.height
gui.scale
gui.mouseX
gui.mouseY
gui.leftDown
gui.leftClicked
gui.cursorMode
gui.setCursorMode(true)
gui.toggleCursorMode()
gui.isPlaying
gui.rect(x, y, w, h, r, g, b, a)
gui.text(x, y, text, r, g, b, scale)
gui.textShadow(x, y, text, r, g, b, scale)
gui.button(x, y, w, h, label)
gui.buttonClicked(x, y, w, h, label)
gui.inRect(mx, my, x, y, w, h)
gui.panel(x, y, w, h, title)
```

for clickable gui mods, press the mod cursor key in-game.

the current test key is:

```text
f8
```

that unlocks the cursor so mod gui buttons can be clicked.

## content api

blockbox has early content registration.

example:

```groovy
def init(api) {
  api.content.registerBlock(
    "RainbowBlock",
    "Rainbow Block",
    true,
    2.0f,
    8
  )
}
```

this area is still early.

the long term goal is real registry based content:

* custom blocks
* custom items
* custom recipes
* custom textures
* custom sounds
* custom mobs
* custom structures

right now the built-in game still has a lot of enum-based block code because blockbox started simple. the modding api is the path toward a better registry system over time.

## scheduler api

mods can schedule delayed or repeating work.

example:

```groovy
def init(api) {
  api.scheduler.runLater(2.0) {
    api.say("two seconds passed")
  }

  api.scheduler.every(10.0) {
    api.say("ten second loop")
  }
}
```

use the scheduler for delayed effects instead of creating random threads inside mods.

## files api

mods get their own config and data folders.

example:

```groovy
def init(api) {
  def configDir = api.files.configDir("my_mod")
  def dataDir = api.files.dataDir("my_mod")

  api.log.info("config: " + configDir)
  api.log.info("data: " + dataDir)
}
```

mods should save their own stuff in their own folders instead of dumping files into the main game folder.

## logging

mods should use the log api.

example:

```groovy
def init(api) {
  api.log.info("loaded")
  api.log.warn("something weird happened")
  api.log.error("something failed")
}
```

this keeps mod output readable.

## error handling

blockbox tries to not let one broken mod destroy the whole game.

if a mod event keeps throwing errors every frame, blockbox can disable that broken handler for the session.

this is important for gui mods. a broken hud render callback could otherwise spam errors forever.

still, mods are trusted code. a bad mod can crash the game if it does something really bad.

## jar mods

jar mods are for people who want compiled jvm mods.

a jar mod can be written in:

* java
* groovy
* kotlin
* scala
* clojure
* any other jvm language

the jar should include:

```text
blockbox.mod.json
```

at the root.

for a groovy script jar, include:

```text
main.groovy
```

at the root too.

example jar layout:

```text
rainbow-control-mod-1.0.2.jar
  blockbox.mod.json
  main.groovy
  assets/
```

## groovy jar script mods

a groovy script jar is basically a mod folder packed into a jar.

that makes it easy to distribute.

example:

```text
my_mod/
  blockbox.mod.json
  main.groovy
```

zip those files into a jar:

```bash
python3 - <<'PY'
from pathlib import Path
import zipfile
out = Path("my-mod-1.0.0.jar")
with zipfile.ZipFile(out, "w", zipfile.ZIP_DEFLATED) as z:
    for p in Path(".").rglob("*"):
        if p.is_file() and not str(p).startswith("build/"):
            z.write(p, p.as_posix())
print(out)
PY
```

then copy it to:

```text
mods/
```

## preferred mod style

the preferred blockbox mod style is simple groovy.

example:

```groovy
def init(api) {
  api.log.info("loading my mod")

  api.commands.register("test") { ctx ->
    ctx.reply("test command works")
  }

  api.events.onBlockBreak { e ->
    e.player.sendMessage("you broke " + e.block)
  }

  api.events.onHudRender { e ->
    def gui = e.gui
    if (gui.isPlaying) {
      gui.textShadow(20, 40, "my mod loaded", 1, 1, 1, 1)
    }
  }
}
```

try to keep mods readable.

a good blockbox mod should usually have:

```text
blockbox.mod.json
main.groovy
README.md
assets/
config/
```

## example complete mod

folder:

```text
mods/
  example_mod/
    blockbox.mod.json
    main.groovy
```

`blockbox.mod.json`:

```json
{
  "id": "example_mod",
  "name": "Example Mod",
  "version": "1.0.0",
  "description": "a tiny example blockbox groovy mod",
  "side": "both",
  "serverInteractive": true,
  "main": "main.groovy"
}
```

`main.groovy`:

```groovy
def init(api) {
  api.log.info("example mod loaded")

  api.commands.register("example") { ctx ->
    ctx.reply("example command works")
  }

  api.events.onBlockBreak { e ->
    e.player.sendMessage("block broken: " + e.block)
  }

  api.events.onHudRender { e ->
    def gui = e.gui
    if (!gui.isPlaying) return

    gui.textShadow(20, 38, "example mod active", 1, 1, 1, 1)
  }
}
```

## making server mods

server mods should change real game behavior.

good examples:

* new commands
* block rules
* world events
* survival changes
* teleport systems
* anti-cheat checks
* gameplay mechanics

server mods should be marked like this:

```json
{
  "side": "server",
  "serverInteractive": true
}
```

or:

```json
{
  "side": "both",
  "serverInteractive": true
}
```

## making client mods

client mods should not change the shared world.

good examples:

* hud overlays
* fps displays
* local ui helpers
* visual options
* client panels
* cosmetic-only helpers

client mods should be marked like this:

```json
{
  "side": "client",
  "serverInteractive": false
}
```

client mods should not give items, place blocks, or change survival state.

## making both-side mods

both-side mods are for things that need ui and server logic.

example:

* a gui button that sends a request to give a custom item
* a custom block with client visuals and server behavior
* a server command with a client panel
* a gameplay feature with hud info

mark them like this:

```json
{
  "side": "both",
  "serverInteractive": true
}
```

players joining a host will need the same server-interactive modpack.

## common mistakes

### my mod shows up but does nothing

check that `blockbox.mod.json` has the right `main`.

```json
{
  "main": "main.groovy"
}
```

also make sure `main.groovy` has:

```groovy
def init(api) {
}
```

### groovy says method not found

groovy numbers are often `Double` by default.

newer blockbox mod api methods try to accept normal groovy numbers, but if something still breaks, cast it:

```groovy
gui.rect(x as float, y as float, w as float, h as float, 0f, 0f, 0f, 0.5f)
```

### multiplayer blocks joining

that usually means the server has a server-interactive mod that the client does not have.

make sure both sides have the same jar in:

```text
mods/
```

### command works in singleplayer but not multiplayer

check the mod side.

gameplay commands should be server-interactive.

```json
{
  "side": "both",
  "serverInteractive": true
}
```

### gui button does not click

make sure the mod cursor is unlocked.

press:

```text
f8
```

## security

blockbox mods are trusted code.

a mod can potentially:

* read files
* write files
* open network connections
* crash the game
* use reflection
* access jvm stuff

this is similar to minecraft java mods.

only install mods from people you trust.

blockbox may add more safety tools later, but full jvm modding means full responsibility.

## future modding goals (open to contributions)

the modding system should become much bigger.

planned ideas:

* better registry based custom blocks
* real custom item registry
* custom textures from mod assets
* custom recipes
* custom mobs
* custom structures
* mod config screens
* client-server mod channels
* better mod dependency checks
* version ranges
* server modpack export
* modpack folder support
* better mod error screens
* generated api docs
* example mods
* official groovy template

the goal is to make blockbox easy to hack on.

not locked down. not fake. not a weird closed system.

a player should be able to make a tiny groovy script in five minutes, or a huge jar mod with serious jvm code.

that is the spirit of blockbox modding.
