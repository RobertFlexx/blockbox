#version 120
#include "lib/common.glsl"
uniform sampler2D uAtlas;
uniform sampler2D lightmap;
uniform float uDaylight;
uniform float uTime;
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
    float a = sin(vUv.x * 33.0 + vUv.y * 17.0 + uTime * 1.22);
    float b = cos(vUv.y * 41.0 - vUv.x * 11.0 - uTime * 0.93);
    vec2 shift = vec2(a,b) * 0.0014;
    vec4 base = texture2D(uAtlas, vUv + shift) * vColor;
    vec3 n = normalize(vNormalView + vec3(a*0.045,0.0,b*0.045));
    vec3 viewDir = normalize(-vViewPos);
    float fresnel = pow(1.0 - sat(dot(n,viewDir)), 5.0);
    float exposure = vSkyLight * vSkyLight * vSkyLight;
    float moonSpec = pow(max(dot(reflect(-normalize(vMoonView), n), viewDir),0.0), 64.0) * (1.0-uDaylight);
    vec3 nightWater = vec3(0.012,0.045,0.11);
    vec3 dayWater = vec3(0.035,0.22,0.34);
    base.rgb = mix(base.rgb, mix(nightWater,dayWater,uDaylight), 0.26 + fresnel*0.18*vSkyLight);
    base.rgb *= bbBlockLight(lightmap, vLightUv, vSkyLight, vBlockLight);
    base.rgb += vec3(0.54,0.68,1.0) * moonSpec * 0.72 * exposure;
    gl_FragData[0] = vec4(base.rgb,max(base.a,0.90));
    gl_FragData[1] = vec4(n*0.5+0.5,1.0);
    gl_FragData[2] = vec4(float(renderStage)/255.0,fresnel,1.0,clamp(vBlockLight,0.0,1.0));
}
