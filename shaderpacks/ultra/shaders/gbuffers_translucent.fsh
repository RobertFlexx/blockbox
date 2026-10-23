#version 120
#include "lib/common.glsl"
#include "lib/surface.glsl"
uniform sampler2D uAtlas;
uniform sampler2D lightmap;
uniform float uDaylight;
uniform float rainStrength;
uniform float uTime;
uniform vec3 uMoonDir;
uniform vec3 uSunDir;
varying vec4 vColor;
varying vec2 vLightUv;
varying float vBlockLight;
varying float vSkyLight;
varying vec2 vUv;
varying vec3 vNormalWorld;
varying float vFogDepth;
void main() {
  vec4 texel = texture2D(uAtlas, vUv);
  if (texel.a <= 0.01) discard;
  vec3 albedo = linearAlbedo(texel.rgb, vColor.rgb);
  vec3 ambient, direct;
  surfaceLight(albedo, normalize(vNormalWorld), normalize(uSunDir), normalize(uMoonDir), vSkyLight,
               vBlockLight, sat(rainStrength), uTime, ambient, direct);
  albedo = ambient + direct;
  gl_FragData[0] = vec4(albedo, texel.a * vColor.a);
}
