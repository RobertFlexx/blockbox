blockbox mods folder

jar mods:
  put coolmod.jar in this folder.
  jar should contain blockbox.mod.json:
  {
    "id": "coolmod",
    "name": "Cool Mod",
    "version": "1.0.0",
    "description": "what this mod does",
    "side": "both",
    "serverInteractive": true,
    "main": "com.example.CoolMod"
  }

groovy script mods:
  create a folder like mods/coolmod/
  put blockbox.mod.json and main.groovy inside it.

simple main.groovy:
  def init(api) {
    api.commands.register("hello") { ctx -> ctx.reply("hello from groovy") }
    api.events.onBlockBreak { e -> e.playerName }
    api.events.onHudRender { e -> e.gui.textShadow(20, 20, "mod hud", 1, 1, 1, 1) }
  }

client gui mods can draw with api.gui during onHudRender. Press F8 in-game to unlock the mouse cursor for clickable mod UI.

side values:
  client = gui/visual/client-only, does not block joining
  server = server gameplay mod, must match host
  both = client and server mod

serverInteractive true means players must have the same modpack hash to join a host.
mods are trusted full-access jvm code, like minecraft java mods. only install mods you trust.
