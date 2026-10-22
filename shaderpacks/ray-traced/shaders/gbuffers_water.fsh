#version 120
#ifndef RGBA16F
#define RGBA16F 34842
#endif
/* RENDERTARGETS: 3,4 */
const int colortex3Format = RGBA16F;
const int colortex4Format = RGBA16F;
uniform float uTime;
uniform mat4 gbufferModelView;
varying vec3 vNormalWorld;
varying vec3 vWorldPos;
varying float vWaveWeight;
varying float vWaveX;
varying float vWaveZ;
void main() {
    // Blend is on, so alpha must be 1 or the cleared aux buffer shows through.
    const float K = 0.3926990817;
    float dhdx = cos(vWorldPos.x * K * 2.0 + uTime * 1.35) * 0.028 * K * 2.0
               + cos(vWorldPos.x * K - uTime * 0.64) * 0.020 * K;
    float dhdz = cos(vWorldPos.z * K + uTime * 0.72) * 0.025 * K
               - sin(vWorldPos.z * K * 2.0 - uTime * 1.05) * 0.028 * K * 2.0;
    // A finer ripple on top of the swell, for the broken mirror in the references.
    dhdx += cos(vWorldPos.x*4.7+vWorldPos.z*2.3+uTime*2.1)*0.022 + cos(vWorldPos.x*8.3-vWorldPos.z*3.7-uTime*1.8)*0.008;
    dhdz += sin(vWorldPos.z*4.1-vWorldPos.x*1.9-uTime*1.7)*0.022 + sin(vWorldPos.z*7.7+vWorldPos.x*4.3+uTime*2.6)*0.008;
    vec3 worldN = normalize(vNormalWorld + vec3(-dhdx,0.0,-dhdz)*abs(vNormalWorld.y)*vWaveWeight);
    vec3 viewN = normalize(mat3(gbufferModelView) * worldN);
    gl_FragData[0] = vec4(viewN * 0.5 + 0.5, 1.0);
    gl_FragData[1] = vec4(gl_FragCoord.z, worldN.x * 0.5 + 0.5, worldN.z * 0.5 + 0.5, 1.0);
}
