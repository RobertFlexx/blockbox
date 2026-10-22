#version 120
#include "lib/waves.glsl"
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
void main() {
    vec3 wave=waterWaves(vWorldPos.xz,uTime);
    vec3 worldN=normalize(vNormalWorld+vec3(-wave.y,0.0,-wave.z)*abs(vNormalWorld.y)*vWaveWeight);
    vec3 viewN = normalize(mat3(gbufferModelView) * worldN);
    gl_FragData[0] = vec4(viewN * 0.5 + 0.5, 1.0);
    gl_FragData[1] = vec4(gl_FragCoord.z, worldN.x * 0.5 + 0.5, worldN.z * 0.5 + 0.5, 1.0);
}
