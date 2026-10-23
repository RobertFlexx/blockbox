#version 120
#include "lib/common.glsl"
uniform sampler2D uAtlas;
uniform sampler2D lightmap;
uniform vec3 uFogColor;
uniform float uFogStart;
uniform float uFogEnd;
uniform float uDaylight;
uniform float uTime;
uniform int renderStage;
uniform vec3 uSunDir;
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
  // Chunk meshes use local coordinates. Keep every spatial frequency periodic over
  // Blockbox's 16-block chunk span so adjacent chunks meet at the same wave phase.
  const float CHUNK_WAVE = 0.3926990817;  // 2*pi / 16
  vec2 wave = vec2(sin(vWorldPos.z * (CHUNK_WAVE * 2.0) + uTime * 1.25) +
                       sin((vWorldPos.x + vWorldPos.z) * CHUNK_WAVE - uTime * 0.82),
                   cos(vWorldPos.x * (CHUNK_WAVE * 2.0) - uTime * 1.08) +
                       cos((vWorldPos.x - vWorldPos.z) * CHUNK_WAVE + uTime * 0.71)) *
              0.0018;
  vec4 base = texture2D(uAtlas, vUv + wave) * vColor;
  vec3 light = bbBlockLight(lightmap, vLightUv, vSkyLight, vBlockLight);
  float exposure = vSkyLight * vSkyLight * vSkyLight;
  float fresnel = pow(1.0 - max(normalize(vNormalWorld).y, 0.0), 2.0);
  float sparkle = pow(max(dot(normalize(vNormalWorld + vec3(wave.x * 15.0, 0.0, wave.y * 15.0)),
                              normalize(uSunDir)),
                          0.0),
                      28.0);
  base.rgb = mix(base.rgb, vec3(0.055, 0.25, 0.48), 0.10 + fresnel * 0.12);
  base.rgb *= light;
  base.rgb += sparkle * vec3(1.0, 0.88, 0.62) * uDaylight * 0.32 * exposure;
  float fog = bb_saturate((uFogEnd - vFogDepth) / max(uFogEnd - uFogStart, 0.001));
  base.rgb = mix(uFogColor, base.rgb, fog);
  gl_FragData[0] = base;
  gl_FragData[1] = vec4(normalize(vNormalView) * 0.5 + 0.5, 1.0);
  gl_FragData[2] = vec4(float(renderStage) / 255.0, vSkyLight, 0.0, vBlockLight);
}
