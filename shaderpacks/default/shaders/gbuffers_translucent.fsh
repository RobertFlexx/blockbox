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
  vec4 base = texel * vec4(min(vColor.rgb, vec3(1.0)), vColor.a);
  if (base.a <= 0.01) discard;
  vec3 n = normalize(vNormalWorld);
  float sun = max(dot(n, normalize(uSunDir)), 0.0);
  float moon = max(dot(n, normalize(uMoonDir)), 0.0);
  float night = 1.0 - bb_saturate((uDaylight - 0.15) / 0.85);
  float exposure = vSkyLight * vSkyLight * vSkyLight;
  vec3 light = bbBlockLight(lightmap, vLightUv, vSkyLight, vBlockLight) *
               mix(1.0, 0.88, vSkyLight * uDaylight);
  light += (vec3(1.0, 0.90, 0.74) * sun * uDaylight * 0.14 +
            vec3(0.44, 0.56, 0.86) * moon * night * 0.05) *
           exposure;
  base.rgb *= light;
  float fog = bb_saturate((uFogEnd - vFogDepth) / max(uFogEnd - uFogStart, 0.001));
  base.rgb = mix(uFogColor, base.rgb, fog);
  gl_FragData[0] = base;
  gl_FragData[1] = vec4(normalize(vNormalView) * 0.5 + 0.5, 1.0);
  gl_FragData[2] = vec4(float(renderStage) / 255.0, vSkyLight, 0.0, vBlockLight);
}
