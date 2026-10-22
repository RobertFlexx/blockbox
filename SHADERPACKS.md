# shader packs

blockbox can load glsl shader packs. they are not a full iris/optifine clone. if a pack needs iris to rewrite the shader source, or some minecraft-only attribute, it will probably not just work. the stuff below is what the game actually gives you.

a few packs already sit in `shaderpacks/`. `default` is the quiet one. `minecraft-plus` is the one i treat as the reference.

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

`uNightDarkness` still exists so old packs dont break. none of the packs that ship with the game use it. ignore it.

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

post passes should stay small. the world is already in the gbuffer. dont go sampling half the screen to guess something that is already in the normal or the depth.
