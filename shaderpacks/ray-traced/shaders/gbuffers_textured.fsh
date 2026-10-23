#version 120
#include "lib/common.glsl"
#include "lib/surface.glsl"
/* RENDERTARGETS: 0,1,2,6,7 */
uniform sampler2D uAtlas;
uniform sampler2D lightmap;
uniform float uDaylight;
uniform float uTime;
uniform float wetness;
uniform float rainStrength;
uniform int renderStage;
uniform int isEyeInWater;
uniform vec3 uSunDir;
uniform vec3 uMoonDir;
varying vec4 vColor;
varying vec2 vLightUv;
varying float vBlockLight;
varying float vSkyLight;
varying float vEmission;
varying vec2 vUv;
varying vec3 vNormalWorld;
varying vec3 vNormalView;
varying float vFogDepth;
varying vec3 vWorldPos;
varying vec3 vViewPos;

void main() {
  vec4 texel = texture2D(uAtlas, vUv);
  if (texel.a <= 0.45 || (dot(vNormalView, vViewPos) > 0.0 && vEmission < 0.5)) discard;
  vec3 albedo = linearAlbedo(texel.rgb, vColor.rgb);
  vec3 n = normalize(vNormalWorld);
  vec3 sun = normalize(uSunDir);
  float sky = sat(vSkyLight);
  float block = sat(vBlockLight);
  vec3 ambient, direct;
  surfaceLight(albedo, n, sun, normalize(uMoonDir), sky, block, sat(rainStrength), uTime, ambient,
               direct);
  vec3 color = ambient + direct;
  color = mix(color, pow(texel.rgb, vec3(2.2)) * vec3(4.0, 2.2, 0.75), vEmission);
  direct *= 1.0 - vEmission;
  if (bbPlanarPass != 0)
    color = mix(reflectedSurface(ambient, direct, vWorldPos, n, sun, uTime, rainStrength), color,
                vEmission);
  gl_FragData[0] = vec4(color, 1.0);
  gl_FragData[1] = vec4(normalize(vNormalView) * 0.5 + 0.5, 1.0);
  gl_FragData[2] = vec4(float(renderStage) / 255.0, sky, 0.0, max(block, vEmission));
  gl_FragData[3] = vec4(direct, 1.0);
  gl_FragData[4] = vec4(albedo, 1.0 - vEmission);
}
