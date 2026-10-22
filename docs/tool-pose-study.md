# Held-tool coordinate study

References inspected for the held-item correction:

- [Minecraft handheld model display data](https://github.com/misode/mcmeta/blob/assets/assets/minecraft/models/item/handheld.json): separate first/third person transforms; first-person display rotation `(0,-90,25)`, translation `(1.13,3.2,1.13)/16`, uniform scale `0.68`.
- [Open-source Minecraft Renderer](https://github.com/zardoy/minecraft-renderer/blob/main/src/three/holdingBlock.ts): camera-space hand motion, followed by item display translation, XYZ rotation and scale. Swing rotations compose around the hand before the display transform.
- [Luanti's camera](https://github.com/luanti-org/luanti/blob/master/src/client/camera.cpp) and [extruded wield mesh](https://github.com/luanti-org/luanti/blob/master/src/client/wieldmesh.cpp): a separate perspective view for held items, solid pixel extrusion, consistent item coordinates and view-relative motion.
- [VoxeLibre wieldview](https://github.com/VoxeLibre/VoxeLibre/blob/master/mods/PLAYER/mcl_wieldview/init.lua): third-person items attach to the animated hand bone.

Blockbox's source art is an original 16×16 silhouette, with positive X toward the tool head. Its mesh origin is the grip at sprite `(4.5,11.5)`, not sprite center `(8,8)`. Applying display angles directly to that grip, with large pitch/roll adjustments, produced the sideways, inverted-looking head.

First person uses Minecraft's chains unchanged, under a fixed 70° perspective with a 0.05 near plane. Held things use `applyItemArmTransform` `(0.56,-0.52,-0.72)` and the attack swing. Tools, items and the torch then take `item/generated`'s display `(0,-90,25)`. Blocks take `block/block`'s `(0,45,0)` at scale `0.4`. No arm is drawn while holding something. A local translation `(-3.5,-3.5,0)/16` recenters the mesh on the sprite center; the art and extrusion share Minecraft's sprite basis, so no basis change is needed. The tool rises from the bottom of the screen and leans outward, matching vanilla screenshots. The empty hand is `renderPlayerArm`'s chain applied to one straight 4×12×4 skin box with the right-arm pivot `(-5,2,0)/16`. A Z 180° turn maps the Y-up skin box into Minecraft's Y-down model space. Swing, equip, bobbing and wall withdrawal act on the complete assembly.

Third person keeps the mesh origin at the physical grip. The resting forearm rotation is accounted for once; the tool face stays vertical, and the shaft extends forward, outward and slightly downward. The item inherits the arm's movement and attack animation. Inventory art is unchanged.

No reference code or Minecraft textures are distributed with this implementation. The references were used to study coordinate conventions and transformation order. Engine-rendered first/third person fixtures cover all four tool shapes, mining and equip motion, and the UI uses the same solid models/art.
