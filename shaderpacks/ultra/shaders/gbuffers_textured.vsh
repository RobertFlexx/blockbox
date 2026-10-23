#version 120
uniform float uTime;
uniform int bbEntityPass;
uniform mat4 gbufferModelViewInverse;
uniform vec3 cameraPosition;
uniform int isEyeInWater;
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
varying vec3 vViewPos;
varying float vPlant;
void main() {
  vec4 vertex = gl_Vertex;
  // Leaves and other cutout geometry get a small wind so canopies aren't frozen.
  vec3 restWorld = (gbufferModelViewInverse * (gl_ModelViewMatrix * vertex)).xyz +
                   vec3(cameraPosition.x, 0.0, cameraPosition.z);
  float bend =
      gl_MultiTexCoord1.z < 0.0 ? clamp(vertex.y + gl_MultiTexCoord1.z + 1.0, 0.0, 1.0) : 1.0;
  float wind = sin(uTime * 1.35 + restWorld.x * 0.71 + restWorld.z * 0.53);
  if (bbEntityPass == 0) vertex.x += wind * 0.032 * bend;
  if (bbEntityPass == 0)
    vertex.z += cos(uTime * 1.05 + restWorld.z * 0.64 + restWorld.y * 0.4) * 0.028 * bend;
  vec4 eye = gl_ModelViewMatrix * vertex;
  gl_Position = gl_ProjectionMatrix * eye;
  gl_ClipVertex = eye;
  vViewPos = eye.xyz;
  vPlant = gl_MultiTexCoord1.z < 0.0 ? 1.0 : 0.0;
  vColor = gl_Color;
  if (bbEntityPass == 0 && gl_MultiTexCoord1.z > 0.0) vColor.rgb /= max(gl_MultiTexCoord1.z, 0.1);
  vec2 lm = gl_MultiTexCoord1.xy;
  vLightUv = max(lm.x, lm.y) > 1.5
                 ? clamp(lm, vec2(0.0), vec2(240.0)) * (1.0 / 256.0) + vec2(0.03125)
                 : clamp(lm, vec2(0.0), vec2(1.0));
  vBlockLight = clamp(gl_MultiTexCoord1.x / 240.0, 0.0, 1.0);
  vSkyLight = clamp(gl_MultiTexCoord1.y / 240.0, 0.0, 1.0);
  vEmission = step(1.75, max(gl_Color.r, max(gl_Color.g, gl_Color.b)));
  vUv = gl_MultiTexCoord0.xy;
  vNormalWorld =
      bbEntityPass != 0
          ? normalize(mat3(gbufferModelViewInverse) * normalize(gl_NormalMatrix * gl_Normal))
          : normalize(gl_Normal);
  vNormalView = normalize(gl_NormalMatrix * gl_Normal);
  vFogDepth = isEyeInWater != 0 ? length(eye.xyz) : length((gbufferModelViewInverse * eye).xz);
  vWorldPos = (gbufferModelViewInverse * eye).xyz + vec3(cameraPosition.x, 0.0, cameraPosition.z);
}
