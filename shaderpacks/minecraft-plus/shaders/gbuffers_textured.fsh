#version 120
#include "lib/common.glsl"
uniform sampler2D uAtlas;
uniform sampler2D lightmap;
uniform float uTime;
uniform vec3 uFogColor;
uniform float uFogStart;
uniform float uFogEnd;
uniform float uDaylight;
uniform float rainStrength;
uniform float wetness;
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
    float daylight = bb_saturate((uDaylight - 0.12) / 0.88);
    float night = 1.0 - daylight;
    float sun = max(dot(n, normalize(uSunDir)), 0.0);
    float moon = max(dot(n, normalize(uMoonDir)), 0.0);
    float exposure = vSkyLight * vSkyLight * vSkyLight;
    // The lightmap carries sky, torch light, dusk tint and weather. Only add a
    // restrained directional response so vanilla's palette stays familiar.
    vec3 light = bbBlockLight(lightmap, vLightUv, vSkyLight, vBlockLight) * mix(1.0, 0.88, vSkyLight * daylight);
    light += (vec3(1.0, 0.92, 0.78) * sun * daylight * 0.13 + vec3(0.32, 0.42, 0.68) * moon * night * 0.045) * exposure;
    base.rgb *= light;
    // Upward faces open to the rain darken and pick up a restrained cool sheen.
    float wet = bb_saturate(wetness) * max(n.y, 0.0) * exposure;
    base.rgb *= 1.0 - wet * 0.115;
    base.rgb += vec3(0.035, 0.050, 0.070) * wet * (0.35 + 0.65 * daylight) * dot(light, vec3(0.333));
    base.rgb = mix(base.rgb, vec3(dot(base.rgb, vec3(0.299, 0.587, 0.114))), bb_saturate(rainStrength) * 0.025);
    float flicker = 0.975 + 0.025 * sin(uTime * 9.0 + vUv.x * 41.0) * sin(uTime * 6.3);
    base.rgb = mix(base.rgb, texel.rgb * vec3(1.20, 1.03, 0.68) * flicker, vEmission);
    float fog = bb_saturate((uFogEnd - vFogDepth) / max(uFogEnd - uFogStart, 0.001));
    base.rgb = mix(uFogColor, base.rgb, fog);
    gl_FragData[0] = base;
    gl_FragData[1] = vec4(normalize(vNormalView) * 0.5 + 0.5, 1.0);
    gl_FragData[2] = vec4(float(renderStage) / 255.0, vSkyLight, 0.0, max(vBlockLight, vEmission));
}
