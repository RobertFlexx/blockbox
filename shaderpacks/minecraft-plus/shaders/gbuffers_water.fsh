#version 120
#include "lib/common.glsl"
uniform sampler2D uAtlas;
uniform sampler2D lightmap;
uniform vec3 uFogColor;
uniform float uFogStart;
uniform float uFogEnd;
uniform float uDaylight;
uniform float uTime;
uniform float rainStrength;
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
    // Chunk meshes use local coordinates. Quantized frequencies make the wave field
    // exactly repeat every 16 blocks, avoiding visible phase jumps at chunk borders.
    const float CHUNK_WAVE = 0.3926990817; // 2*pi / 16
    vec2 wave = vec2(
        sin(vWorldPos.z * (CHUNK_WAVE * 2.0) + uTime * 1.10) + sin((vWorldPos.x + vWorldPos.z) * CHUNK_WAVE - uTime * 0.73),
        cos(vWorldPos.x * (CHUNK_WAVE * 2.0) - uTime * 0.96) + cos((vWorldPos.x - vWorldPos.z) * CHUNK_WAVE + uTime * 0.66)
    ) * (0.00135 * mix(1.0, 1.38, bb_saturate(rainStrength)));
    vec4 base = texture2D(uAtlas, vUv + wave) * vColor;
    vec3 n = normalize(vNormalWorld + vec3(wave.x * 10.0, 0.0, wave.y * 10.0));
    float daylight = bb_saturate((uDaylight - 0.12) / 0.88);
    float night = 1.0 - daylight;
    float exposure = vSkyLight * vSkyLight * vSkyLight;
    float sunSpec = pow(max(dot(n, normalize(uSunDir)), 0.0), 42.0) * daylight;
    float moonSpec = pow(max(dot(n, normalize(uMoonDir)), 0.0), 54.0) * night;
    base.rgb = mix(base.rgb, vec3(0.07, 0.27, 0.47), 0.075 + bb_saturate(rainStrength) * 0.025);
    base.rgb *= bbBlockLight(lightmap, vLightUv, vSkyLight, vBlockLight);
    base.rgb += (vec3(1.0, 0.86, 0.58) * sunSpec * 0.18 + vec3(0.44, 0.58, 0.92) * moonSpec * 0.08) * exposure;
    float fog = bb_saturate((uFogEnd - vFogDepth) / max(uFogEnd - uFogStart, 0.001));
    base.rgb = mix(uFogColor, base.rgb, fog);
    gl_FragData[0] = base;
    gl_FragData[1] = vec4(normalize(vNormalView) * 0.5 + 0.5, 1.0);
    gl_FragData[2] = vec4(float(renderStage) / 255.0, vSkyLight, 0.0, vBlockLight);
}
