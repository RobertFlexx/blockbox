#version 120
#include "lib/common.glsl"
uniform sampler2D uAtlas;
uniform sampler2D lightmap;
uniform float uDaylight;
uniform float uTime;
uniform vec3 uSunDir;
uniform vec3 uMoonDir;
uniform int renderStage;
uniform float rainStrength;
uniform vec3 uFogColor;
uniform int isEyeInWater;
varying vec4 vColor;
varying vec2 vLightUv;
varying float vBlockLight;
varying float vSkyLight;
varying vec2 vUv;
varying vec3 vNormalView;
varying vec3 vNormalWorld;
varying vec3 vViewPos;
varying vec3 vSunView;
varying vec3 vMoonView;
varying float vFogDepth;
void main() {
    float t = uTime;
    float w1 = sin(vUv.x * 36.0 + vUv.y * 19.0 + t * 1.85);
    float w2 = sin(vUv.y * 44.0 - vUv.x * 13.0 - t * 1.31);
    float w3 = sin((vUv.x + vUv.y) * 29.0 + t * 0.77);
    vec2 distortion = vec2(w1 + w3 * 0.55, w2 - w3 * 0.45) * 0.00135;
    vec4 texel = texture2D(uAtlas, vUv + distortion) * vColor;
    vec3 ambient = bbBlockLight(lightmap, vLightUv, vSkyLight, vBlockLight);
    float exposure = vSkyLight * vSkyLight * vSkyLight;

    vec3 n = normalize(vNormalView + vec3((w1 - w3) * 0.045, 0.0, (w2 + w3) * 0.045));
    vec3 viewDir = normalize(-vViewPos);
    float ndv = sat(dot(n, viewDir));
    float fresnel = 0.035 + 0.965 * pow(1.0 - ndv, 5.0);
    float sunSpec = pow(max(dot(reflect(-normalize(vSunView), n), viewDir), 0.0), 72.0) * uDaylight;
    float moonSpec = pow(max(dot(reflect(-normalize(vMoonView), n), viewDir), 0.0), 54.0) * (1.0 - uDaylight);

    vec3 deepDay = vec3(0.035, 0.20, 0.31);
    vec3 deepNight = vec3(0.018, 0.055, 0.12);
    vec3 reflectionDay = mix(vec3(0.39, 0.63, 0.82),uFogColor,rainStrength*0.90);
    vec3 reflectionNight = vec3(0.12, 0.19, 0.34);
    vec3 deep = mix(deepNight, deepDay, uDaylight);
    vec3 reflection = mix(reflectionNight, reflectionDay, uDaylight);

    // Water under a roof or in a cave reflects its surroundings, not the sky.
    vec3 c = mix(texel.rgb, deep, 0.22) * ambient;
    c = mix(c, reflection, fresnel * 0.34 * exposure);
    c += (vec3(1.00, 0.84, 0.58) * sunSpec * (1.0-rainStrength*0.9) * 0.95 + vec3(0.56, 0.68, 1.00) * moonSpec * 0.38) * exposure;
    c += vec3(0.025, 0.055, 0.070) * (w1 + w2 + w3) * 0.12 * ambient;

    gl_FragData[0] = vec4(c, max(texel.a, 0.50+fresnel*0.22));
    gl_FragData[1] = vec4(n * 0.5 + 0.5, 1.0);
    gl_FragData[2] = vec4(float(renderStage) / 255.0, fresnel, 1.0, 1.0);
}
