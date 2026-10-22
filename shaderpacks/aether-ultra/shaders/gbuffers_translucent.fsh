#version 120
#include "lib/common.glsl"
uniform sampler2D uAtlas;
uniform sampler2D lightmap;
uniform float uTime;
uniform float uDaylight;
uniform vec3 uSunDir;
uniform vec3 uMoonDir;
uniform int renderStage;
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
    vec4 base = texture2D(uAtlas, vUv) * vec4(min(vColor.rgb, vec3(1.0)), vColor.a);
    if (base.a <= 0.015) discard;
    vec3 n = normalize(vNormalWorld);
    float sun = max(dot(n, normalize(uSunDir)), 0.0);
    float moon = max(dot(n, normalize(uMoonDir)), 0.0);
    float night = 1.0 - sat((uDaylight - 0.10) / 0.90);
    float exposure = vSkyLight * vSkyLight * vSkyLight;
    vec3 light = bbBlockLight(lightmap, vLightUv, vSkyLight, vBlockLight);
    light += (vec3(1.0, 0.91, 0.74) * sun * uDaylight * 0.22 + vec3(0.36, 0.49, 0.76) * moon * night * 0.10) * exposure;
    base.rgb *= light;
    gl_FragData[0] = base;
    gl_FragData[1] = vec4(normalize(vNormalView) * 0.5 + 0.5, 1.0);
    gl_FragData[2] = vec4(float(renderStage) / 255.0, base.a, 0.0, clamp(vBlockLight, 0.0, 1.0));
}
