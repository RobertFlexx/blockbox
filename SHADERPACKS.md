# shader packs

blockbox can load glsl shader packs. they are not a full iris/optifine clone. if a pack needs iris to rewrite the shader source, or some minecraft-only attribute, it will probably not just work. the stuff below is what the game actually gives you.

three packs ship with the game. `default` is the quiet one. `ultra` is the high-quality raster pack (soft shadows, volumetric clouds, reflective water, god rays). `ray-traced` is that same grade plus hardware ray-traced sunlight, occlusion, one bounce and reflections on a GPU that can do them; it falls back to `ultra`'s shadow maps otherwise.

Choose `ultra` in the Resources screen to use the renderer shown in the quality
previews. The selection is stored in `shaderpacks/active.txt`; each installed pack
can be selected there. A running game needs restarting to load updated game code.

Ultra and Ray Traced share linear HDR surface lighting and a single filmic display conversion. Clear days use neutral sunlight and blue skylight; low sun brings amber light and distance haze; nights use blue moonlight, stars and warm torches. Both draw eroded cumulus volumes with self-shadowing, darken in storms, and respond to lightning. Water combines depth absorption, refraction, fine ripples, Fresnel reflections and a broken sun or moon highlight. Level water captures the mirrored terrain and characters at half resolution, including objects outside the main view. Mipmapped colour and cutout coverage filter distant leaves without filling their holes. Flowing water uses refined screen reflections with a cloudy sky fallback.

`blockbox.linearLighting=true` puts characters and held tools through the terrain's linear surface lighting and deferred shadows. Animated players and zombies also cast into the shared shadow map. Inventory previews and the first-person layer retain their own display lighting.

Terrain and compatibility entities share one validated linear fog interval. Heavy rain
shortens both fog endpoints while retaining a gradual transition, including at the
lightest horizon-fog setting. Rain uses roof-occluded textured streaks with transparent
edges and height fades; broad untextured sheets beneath clouds have been removed.
Third-person tools follow the vanilla `item/handheld` right-hand display basis
(`0, -90, 55` before the hand layer), anchored at the handle and scaled to the avatar.
See the [vanilla handheld model](https://assets.mcasset.cloud/1.21.1/assets/minecraft/models/item/handheld.json)
and [Mojang's player holding animation](https://github.com/Mojang/bedrock-samples/blob/main/resource_pack/animations/player.animation.json).

Underwater absorption and scattering cover the wet part of the view ray. Airborne scenery stays visible through a gently distorted surface without a hard critical-angle mask. A near-plane crossing is reconstructed when the camera sits just below the surface; it cannot turn the sky into deep-water fog. Shore vertices stay pinned to their banks, exposed sides and bottoms close the water volume, and a separate water depth target selects the nearest interface while preserving opaque depth for refraction.

Water depth uses R32F rather than half precision. Optical waves fade before their
crests become smaller than a few pixels. The underside uses absorbed reflection
instead of mirroring sharp floor texels overhead. Shadow filtering keeps full
penumbra samples at soft edges and skips them for lit or contact-shadow pixels;
volumetric shadow rays skip directions with no visible shaft contribution.
The engine also caches unchanged scalar and sampler uniform values per linked
program. This reduces driver calls without reducing resolution or shader samples;
the gameplay render test compares the cached and uncached output pixel for pixel.

Terrain supplies its compatibility face shade separately in `gl_MultiTexCoord1.z`; HDR packs remove it before lighting, retaining the material's albedo and biome tint. Water uses that coordinate for its shore wave weight instead. Leaf canopies retain one interior layer behind their cutouts; fully buried faces stay culled. This gives ordinary generated trees depth in direct views and reflections rather than rendering a hollow shell. [Luanti's node drawtypes](https://api.luanti.org/nodes/#node-drawtypes) describe the related distinction between external-only and interior-visible foliage.

Plant meshes encode `-(rootY + 1)` in `gl_MultiTexCoord1.z`. Ultra and Ray Traced
use that marker to anchor grass and fern roots while their tips move. Wind phases
use world coordinates, so matching leaf edges stay together across chunk borders.
Canopy exposure treats opaque neighbors as occluders: fully enclosed core faces
are removed while exposed cutouts and their shallow inner layer remain visible.
Block debris uses opaque alpha-tested billboards with depth writes and the same
linear lighting as entities, avoiding translucent holes over the scene.

Visual tests include the normal gameplay frame, with HUD and first-person rendering. For a read-only GPU review of a saved position with the current user preferences, set `BLOCKBOX_REVIEW_WORLD` to the saved world directory when running `blockbox.runShaderPipelineTests`. It renders both packs at 1920×1080 and writes previews to the supplied artifact directory. Shader selections are isolated in a temporary directory. Constructed lake scenes remain lighting regression fixtures; their water depth, terrain and field of view differ from generated worlds.

## drop a pack in

```text
shaderpacks/
  my-pack/
    pack.properties
    shaders/
      gbuffers_terrain.vsh
      gbuffers_terrain.fsh
      gbuffers_textured.vsh
      gbuffers_textured.fsh
      gbuffers_translucent.vsh
      gbuffers_translucent.fsh
      gbuffers_water.vsh
      gbuffers_water.fsh
      deferred.fsh
      composite.fsh
      final.fsh
      lib/
        common.glsl
```

`.vert` and `.frag` work too, if you dont want the minecraft names.

pick a pack from resources, or set `BLOCKBOX_SHADERPACK`, or write the folder name into `shaderpacks/active.txt`. if you save a broken shader while the game is open, it keeps the last good version and prints the compiler log. it does not take the renderer down with it.

## what gets drawn

the chunk pass is split like minecraft:

* `gbuffers_terrain` opaque blocks
* `gbuffers_textured` cutout stuff (leaves, torches, glass edges)
* `gbuffers_translucent` the see-through bits, color only
* `gbuffers_water` water

then, if you have them:

* `shadow`, `shadow_solid`, `shadow_cutout`
* `deferred`, `deferred1` ... `deferred99`
* `composite`, `composite1` ... `composite99`
* `final`

only the files that exist get run. post passes are fullscreen. they do not rebuild chunks.

`shadowMapResolution`, `shadowDistance` and `noiseTextureResolution` in the pack are actually read, within sane limits. if your pack never samples a shadow map, the shadow pass is skipped.

## lighting, read this or your pack will be black

this is the part people get wrong.

vertex color is only the face shade, the ambient occlusion, and the biome tint. it is not the torch light and it is not the time of day. `gl_Color` in the vertex shader is unclamped, so a flame vertex comes through above 1.75 (the flame itself is about `(2.0, 1.65, 0.80)`). test that in the vertex shader and pass a flag. the fragment stage clamps it.

block light and sky light are in `gl_MultiTexCoord1`, on minecraft's 0..240 scale. x is block light (torches). y is sky light.

do not multiply that by `gl_TextureMatrix[1]`. that matrix gets stomped by ui and the hand, and then every face samples the unlit corner of the lightmap. that is the "shaders are all black" bug. map it yourself:

```glsl
vec2 lm = gl_MultiTexCoord1.xy;
vLightUv = clamp(lm, vec2(0.0), vec2(240.0)) * (1.0 / 256.0) + vec2(0.03125);
```

`lightmap` is a 16x16 texture. block light goes across, sky light goes down. the game rebuilds it when the time, the weather, night shading, or the torch flicker changes. dusk, moonlight, dark nights and warm torches are already in there. dont darken the night a second time.

```glsl
vec3 light = texture2D(lightmap, vLightUv).rgb;
gl_FragData[0] = vec4(albedo.rgb * min(vColor.rgb, vec3(1.0)) * light, albedo.a);
```

`uNightDarkness` preserves the bright-night setting in Ultra and Ray Traced; packs using the compatibility lightmap already receive that setting through the lightmap.

the same lightmap is what you get with shaders off, through fixed-function multitexture. if shaders and no-shaders look like different games, the pack is doing something extra.

## uniforms you will probably want

the usual minecraft-ish names are there, next to the blockbox ones:

```glsl
uniform sampler2D uAtlas;
uniform sampler2D texture;
uniform sampler2D lightmap;
uniform sampler2D depthtex0;
uniform sampler2D colortex0;
uniform sampler2D noisetex;
uniform sampler2D shadowtex0;
uniform vec3 fogColor;
uniform float rainStrength;
uniform float wetness;
uniform int isEyeInWater;
uniform float frameTimeCounter;
uniform vec3 cameraPosition;
uniform vec3 sunPosition;
uniform vec3 moonPosition;
uniform float near;
uniform float far;
uniform mat4 gbufferProjection;
uniform mat4 gbufferProjectionInverse;
uniform mat4 gbufferModelView;
uniform mat4 gbufferModelViewInverse;
```

not every stage gets every uniform. if you ask for something the engine does not have, you get nothing. it will not invent fake iris data.

## includes

```glsl
#include "lib/common.glsl"
```

quoted or angle brackets, both fine. includes stay inside the pack's `shaders/` folder. `..` does not get you out, and include loops get rejected.

## what this is not

no compute, no tessellation, no ssbo pack systems, no iris source patcher. if your pack needs those, it needs a port.

`ray-traced` is not a compute shader you can copy. the pack sets `blockbox.raytracing=true` and the engine traces the loaded chunks on the GPU's ray-tracing hardware, then the pack's deferred pass reads `rtLighting`, `rtReflection` and `rtVolume`. `blockbox.volumetricClouds=true` tells the game to skip its own block clouds so the pack can draw the sky. both keys live in `pack.properties`.

The ray frame loop is `src/main/scala/blockbox/HardwareRayLoop.java`, compiled alongside Scala with LWJGL. Vulkan owns its ray inputs, outputs and history images. OpenGL packs the reduced-resolution G-buffer, reads it into a persistently mapped, coherent **host-cached** transfer buffer, and uploads the completed ray outputs from that same buffer. Vulkan records the buffer/image copies and compute dispatch; host/transfer/compute barriers and a reusable fence protect every handoff. This explicit staging path includes readback/upload traffic. It avoids external-image and external-semaphore interoperability that produced intermittent blank gameplay frames on the tested NVIDIA driver. There is no CPU pixel tracing or per-frame pixel-processing loop. Host caching matters: uncached transfer memory made the replay several times slower.

`BlockboxRayTrace.scala` retains device selection, chunk geometry, acceleration structures, descriptors, history and resource ownership. Scene acceleration structures are reused until terrain changes; at most two chunk builds run per frame, nearest first. Empty chunks are cached too. Build-size queries use the same flags as the actual acceleration builds. The GPU executes `assets/shaders/rt/trace.comp` through Vulkan ray queries. Pure Java helpers provide bounded voxel navigation and AI planning budgets, validated frame packing, packet budgets and frustum checks.

Native tracing requires Vulkan ray-query/acceleration-structure support and matching OpenGL/Vulkan device UUIDs (`GL_EXT_memory_object` provides the GL identifier). Unsupported systems use the raster fallback and report why. Opaque-FD image/semaphore extensions are no longer required. To require a real hardware run during validation, set `BLOCKBOX_REQUIRE_HARDWARE_RT=1` together with `BLOCKBOX_RUN_ENGINE_RENDER_TESTS=1`; fallback rendering alone will then fail the hardware check. Software EGL validation exercises the pack pipeline, rather than proving hardware execution or frame rate.

After changing the native kernel, rebuild the shipped binary with `glslangValidator -V --target-env vulkan1.2 assets/shaders/rt/trace.comp -o assets/shaders/rt/trace.spv` and validate it with `spirv-val --target-env vulkan1.2 assets/shaders/rt/trace.spv`. Release validation also compiles and validates these kernels when both tools are available.

post passes should stay small. the world is already in the gbuffer. dont go sampling half the screen to guess something that is already in the normal or the depth.

## Lighting, water and weather quality

Both HDR packs filter shadow **comparisons** bilinearly, then use a stable soft PCSS kernel whose penumbra grows with caster distance. Filtering the comparisons preserves leaf coverage; filtering raw depths would invent blockers in transparent gaps. The reconstruction footprint removes shadow-map stair steps at contact. Screen-space contact visibility fades over a depth interval, and nearby ambient visibility gives voxel corners and feet depth. Albedo and illumination are combined in linear HDR, with one display conversion at the end. The antialiasing setting controls the final shader FXAA resolve; the HUD is drawn afterwards.

Water uses band-limited crossing gravity/capillary waves, full-precision interface depth, depth absorption, checked refraction and GGX highlights. Shore vertices are pinned; isolated water volumes include their sides and bottom. The half-resolution planar mirror stores premultiplied cutout coverage and filtered mipmaps. Distant sky reflections use a smoother optical normal to suppress bright cloud striping while terrain reflections keep their ripple detail. Underwater views preserve nearby character/foliage silhouettes and use absorption, suspended particles and light shafts.

Cloud banks and precipitation share a continuous world-space wind and the same storm cells. The nearest four cells are exposed as `bbStormCells[4]` (`x,z,radius,strength`) and `bbStormCellCount`. These uniforms are additive; older packs can ignore them. Dense banks darken their underside and the ground underneath them. Wet surfaces retain moisture after the front passes and reflect view-dependent highlights. A fixed sun/time-of-day override does not freeze cloud advection. The night sky includes stable constellation patterns, filtered stellar cores/halos and a subdued galactic band.

For a full gameplay replay, set `BLOCKBOX_REVIEW_WORLD` to a saved-world directory when running `blockbox.runShaderPipelineTests`. This uses its real texture pack, FOV, chunks, water and UI, warms the bounded ray upload queue, rejects blank frames and captures the saved time, day and sunset for both HDR packs. It reads the save without modifying it.
