# shader packs

blockbox can load glsl shader packs. they are not a full iris/optifine clone. if a pack needs iris to rewrite the shader source, or some minecraft-only attribute, it will probably not just work. the stuff below is what the game actually gives you.

three packs ship with the game. `default` is the quiet one. `ultra` is the high-quality raster pack (soft shadows, volumetric clouds, reflective water, god rays). `ray-traced` is that same grade plus hardware ray-traced sunlight, occlusion, one bounce and reflections on a GPU that can do them; it falls back to `ultra`'s shadow maps otherwise.

Ultra and Ray Traced share linear HDR surface lighting and a single filmic display conversion. Clear days use neutral sunlight and blue skylight; low sun brings amber light and distance haze; nights use blue moonlight, stars and warm torches. Both draw eroded cumulus volumes with self-shadowing, darken in storms, and respond to lightning. Water combines depth absorption, refraction, fine ripples, Fresnel reflections and a broken sun or moon highlight. Level water captures the mirrored terrain and characters at half resolution, including objects outside the main view. Mipmapped colour and cutout coverage filter distant leaves without filling their holes. Flowing water uses refined screen reflections with a cloudy sky fallback.

`blockbox.linearLighting=true` puts characters and held tools through the terrain's linear surface lighting and deferred shadows. Animated players and zombies also cast into the shared shadow map. Inventory previews and the first-person layer retain their own display lighting.

Underwater absorption and scattering cover the wet part of the view ray. Airborne scenery stays visible through a gently distorted surface without a hard critical-angle mask. A near-plane crossing is reconstructed when the camera sits just below the surface; it cannot turn the sky into deep-water fog. Shore vertices stay pinned to their banks, exposed sides and bottoms close the water volume, and a separate water depth target selects the nearest interface while preserving opaque depth for refraction.

Terrain supplies its compatibility face shade separately in `gl_MultiTexCoord1.z`; HDR packs remove it before lighting, retaining the material's albedo and biome tint. Water uses that coordinate for its shore wave weight instead. Leaf canopies retain one interior layer behind their cutouts; fully buried faces stay culled. This gives ordinary generated trees depth in direct views and reflections rather than rendering a hollow shell. [Luanti's node drawtypes](https://api.luanti.org/nodes/#node-drawtypes) describe the related distinction between external-only and interior-visible foliage.

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

The ray frame loop is `src/main/scala/blockbox/HardwareRayLoop.java`, compiled alongside Scala with LWJGL. It acquires the five shared images, records the compute dispatch and memory barriers, releases image ownership, submits GPU work and coordinates OpenGL/Vulkan semaphores. A reusable fence protects mapped writes and command-buffer reuse. `BlockboxRayTrace.scala` retains device selection, chunk geometry, acceleration structures, descriptors, history and resource ownership. The GPU executes `assets/shaders/rt/trace.comp` through Vulkan ray queries; Java does not trace pixels on the CPU. Pure Java helpers provide bounded voxel navigation, validated frame packing, packet budgets and frustum checks.

Native tracing currently requires Linux opaque-FD interop: `GL_EXT_memory_object_fd`, `GL_EXT_semaphore_fd`, Vulkan ray-query/acceleration-structure support, and matching OpenGL/Vulkan device UUIDs. Unsupported systems use the raster fallback and report why. To require a real hardware run during validation, set `BLOCKBOX_REQUIRE_HARDWARE_RT=1` together with `BLOCKBOX_RUN_ENGINE_RENDER_TESTS=1`; fallback rendering alone will then fail the hardware check. Software EGL validation exercises the pack pipeline, rather than proving hardware execution or frame rate.

After changing the native kernel, rebuild the shipped binary with `glslangValidator -V --target-env vulkan1.2 assets/shaders/rt/trace.comp -o assets/shaders/rt/trace.spv` and validate it with `spirv-val --target-env vulkan1.2 assets/shaders/rt/trace.spv`. Release validation also compiles and validates these kernels when both tools are available.

post passes should stay small. the world is already in the gbuffer. dont go sampling half the screen to guess something that is already in the normal or the depth.
