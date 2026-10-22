#version 120
#include "lib/common.glsl"
uniform sampler2D uAtlas;
uniform sampler2D lightmap;
uniform float uTime;
uniform vec3 uFogColor;
uniform float uFogStart;
uniform float uFogEnd;
uniform float rainStrength;
varying vec4 vColor;
varying vec2 vLightUv;
varying float vBlockLight;
varying float vSkyLight;
varying float vEmission;
varying vec2 vUv;
varying float vFogDepth;
void main() {
    vec4 base = texture2D(uAtlas, vUv) * vec4(min(vColor.rgb, vec3(1.0)), vColor.a);
    if (base.a <= 0.01) discard;
    base.rgb *= bbBlockLight(lightmap, vLightUv, vSkyLight, vBlockLight);
    base.rgb *= 1.0 - bb_saturate(rainStrength) * 0.045;
    float fog = bb_saturate((uFogEnd - vFogDepth) / max(uFogEnd - uFogStart, 0.001));
    base.rgb = mix(uFogColor, base.rgb, fog);
    gl_FragColor = base;
}
