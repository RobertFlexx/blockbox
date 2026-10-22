import blockbox.*

class SkyboundWorkshop implements BlockboxMod {
    Block moonstone
    Block stone
    void onInit(ModApi api) {
        stone = api.block("Stone")
        moonstone = api.content.registerCube('skybound:moonstone', 'Moonstone', [
            slot: 6, backing: 'Stone', red: 0.68, green: 0.84, blue: 1.0,
            hardness: 0.7, light: 11
        ])
        api.content.registerRecipe('skybound:moonstone', [['Coal', 'Glass']], 'skybound:moonstone', 2)
        api.content.setDrop('skybound:moonstone', 'skybound:moonstone', 1)
        api.rules.set('gravity', 27)
        api.rules.set('jumpSpeed', 11)
        api.rules.set('fallDamage', 0.25)
        api.worldgen.onChunk { chunk ->
            // Only the chunk transaction is used on generation workers.
            for (int x = 0; x < 16; x++) for (int z = 0; z < 16; z++) {
                int y = 12 + chunk.random.nextInt(26)
                if (chunk.get(x, y, z) == stone && chunk.random.nextFloat() < 0.045)
                    chunk.set(x, y, z, moonstone)
            }
        }
        api.events.onBlockInteract { event ->
            if (event.block == moonstone) {
                event.handled = true
                api.player(event.playerName).teleport(event.x + 0.5, event.y + 3.7, event.z + 0.5)
            }
        }
        api.events.onDamage { event ->
            if (event.reason == 'falling') event.amount = event.amount * 0.5
        }
        api.events.onHudRender { event ->
            event.gui.textShadow(18, 40, 'SKYBOUND  /  Moonstone lifts you', 0.72, 0.86, 1.0, 0.75)
        }
        api.commands.register('skybound', true, '/skybound', 'Give a moonstone starter kit') { command ->
            if (command.requireOp()) command.player.give(moonstone, 8)
        }
    }
    void onWorldLoaded(ModApi api) { api.say('Skybound: craft moonstone from coal + glass. Right-click it to rise.') }
}
