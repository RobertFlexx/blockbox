#version 120
#include "lib/common.glsl"
uniform sampler2D uAtlas;
uniform sampler2D lightmap;
uniform float uDaylight;
uniform float uTime;
uniform int renderStage;
uniform float rainStrength;
uniform vec3 uFogColor;
uniform int isEyeInWater;
varying vec4 vColor;
varying vec2 vLightUv;
varying float vBlockLight;
varying float vSkyLight;
varying float vEmission;
varying vec2 vUv;
varying vec3 vNormalView;
varying vec3 vNormalWorld;
varying vec3 vViewPos;
varying vec3 vSunView;
varying vec3 vMoonView;
varying float vFogDepth;
void main() {
    float a = sin(vUv.x * 40.0 + uTime * 1.45);
    float b = cos(vUv.y * 35.0 - uTime * 1.10);
    vec2 shift = vec2(a, b) * 0.0010;
    vec4 base = texture2D(uAtlas, vUv + shift) * vColor;
    vec3 n = normalize(vNormalView + vec3(a * 0.030, 0.0, b * 0.030));
    vec3 viewDir = normalize(-vViewPos);
    float fresnel = pow(1.0 - sat(dot(n, viewDir)), 4.0);
    vec3 waterTint = mix(vec3(0.035,0.10,0.20), vec3(0.045,0.30,0.48), uDaylight);
    base.rgb = mix(base.rgb, waterTint, 0.16 + fresnel * 0.18 * vSkyLight);
    base.rgb *= bbBlockLight(lightmap, vLightUv, vSkyLight, vBlockLight);
    float sunSpec = pow(max(dot(reflect(-normalize(vSunView), n), viewDir), 0.0), 64.0) * uDaylight;
    base.rgb += vec3(1.0, 0.90, 0.70) * sunSpec * (1.0-rainStrength*0.9) * 0.30 * vSkyLight * vSkyLight * vSkyLight;
    gl_FragData[0] = vec4(base.rgb, max(base.a, 0.50+fresnel*0.22));
    gl_FragData[1] = vec4(n * 0.5 + 0.5, 1.0);
    gl_FragData[2] = vec4(float(renderStage) / 255.0, fresnel, 1.0, clamp(vBlockLight, 0.0, 1.0));
}
