#version 120
#include "lib/waves.glsl"
#ifndef RGBA16F
#define RGBA16F 34842
#endif
#ifndef R32F
#define R32F 33326
#endif
/* RENDERTARGETS: 3,4 */
const int colortex3Format = RGBA16F;
// Projected depth approaches 1 very quickly. Half precision quantizes it into
// metre-wide bands, corrupting refraction thickness and ray reconstruction.
const int colortex4Format = R32F;
uniform float uTime;
uniform mat4 gbufferModelView;
varying vec3 vNormalWorld;
varying vec3 vWorldPos;
varying float vWaveWeight;
void main() {
  float footprint = max(length(dFdx(vWorldPos.xz)), length(dFdy(vWorldPos.xz)));
  vec3 wave = waterOpticalWaves(vWorldPos.xz, uTime, footprint);
  vec3 worldN =
      normalize(vNormalWorld + vec3(-wave.y, 0.0, -wave.z) * abs(vNormalWorld.y) * vWaveWeight);
  vec3 viewN = normalize(mat3(gbufferModelView) * worldN);
  gl_FragData[0] = vec4(viewN * 0.5 + 0.5, 1.0);
  gl_FragData[1] = vec4(gl_FragCoord.z, worldN.x * 0.5 + 0.5, worldN.z * 0.5 + 0.5, 1.0);
}
