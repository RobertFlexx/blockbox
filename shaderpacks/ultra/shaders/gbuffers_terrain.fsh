#version 120
#ifndef RGBA16F
#define RGBA16F 34842
#endif
#ifndef RGBA8
#define RGBA8 32856
#endif
#include "lib/common.glsl"
#include "lib/surface.glsl"
/* RENDERTARGETS: 0,1,2,6,7 */
const int colortex6Format = RGBA16F;
const int colortex7Format = RGBA8;

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

void main() {
  vec4 texel = texture2D(uAtlas, vUv);
  if (texel.a <= 0.01) discard;
  vec3 albedo = linearAlbedo(texel.rgb, vColor.rgb);
  vec3 n = normalize(vNormalWorld);
  vec3 sun = normalize(uSunDir);
  vec3 moon = normalize(uMoonDir);
  float sky = sat(vSkyLight);
  float block = sat(vBlockLight);
  float wet = sat(wetness) * sky * (1.0 - vEmission);
  albedo = mix(albedo, albedo * albedo * vec3(0.62, 0.72, 0.78), wet * 0.72);

  vec3 ambient, direct;
  surfaceLight(albedo, n, sun, normalize(uMoonDir), sky, block, sat(rainStrength), uTime, ambient,
               direct);
  vec3 color = ambient + direct;
  vec3 toEye = normalize(cameraPosition - vWorldPos);
  vec3 H = normalize(sun + toEye + vec3(0.0, 0.00001, 0.0));
  float nv = sat(dot(n, toEye)), nl = sat(dot(n, sun));
  float wetFresnel = 0.025 + 0.975 * pow(1.0 - nv, 5.0);
  float sheen = pow(sat(dot(n, H)), 96.0) * nl * wet * wetFresnel * 6.0 *
                smoothstep(0.0, 0.15, sun.y) * (1.0 - rainStrength * 0.8);
  // Rain films reflect the sky/light colour rather than the diffuse pigment.
  direct += sunlightColor(sun) * sheen;
  color += sunlightColor(sun) * sheen;
  float flicker = 0.975 + 0.025 * sin(uTime * 9.0 + vUv.x * 41.0);
  color = mix(color, pow(texel.rgb, vec3(2.2)) * vec3(4.0, 2.2, 0.75) * flicker, vEmission);
  direct *= 1.0 - vEmission;
  if (bbPlanarPass != 0)
    color = mix(reflectedSurface(ambient, direct, vWorldPos, n, sun, uTime, rainStrength), color,
                vEmission);
  if (isEyeInWater != 0) {
    float waves = sin((vWorldPos.x + vWorldPos.z) * 1.57 + uTime * 0.8) +
                  sin((vWorldPos.x - vWorldPos.z) * 0.78 - uTime * 0.65);
    float caustic = pow(max(1.0 - abs(waves) * 2.4, 0.0), 6.0);
    color += albedo * vec3(0.18, 0.3, 0.28) * caustic * max(n.y, 0.0) * sky * daylightAmount(sun);
  }
  gl_FragData[0] = vec4(color, 1.0);
  gl_FragData[1] = vec4(normalize(vNormalView) * 0.5 + 0.5, 1.0);
  gl_FragData[2] = vec4(float(renderStage) / 255.0, sky, wet, max(block, vEmission));
  gl_FragData[3] = vec4(direct, 1.0);
  gl_FragData[4] = vec4(albedo, 1.0 - vEmission);
}
