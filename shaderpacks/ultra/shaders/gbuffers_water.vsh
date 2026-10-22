#version 120
uniform float uTime;
uniform mat4 gbufferModelViewInverse;
uniform vec3 cameraPosition;
uniform int isEyeInWater;
varying vec4 vColor;
varying vec2 vLightUv;
varying float vBlockLight;
varying float vSkyLight;
varying vec2 vUv;
varying vec3 vNormalWorld;
varying vec3 vNormalView;
varying vec3 vWorldPos;
varying float vWaveWeight;
varying float vWaveX;
varying float vWaveZ;
void main() {
    vec4 vertex = gl_Vertex;
    // Chunk-periodic waves so neighbouring chunks meet at the same height.
    const float K = 0.3926990817; // 2*pi / 16
    float wx = sin(vertex.x * K * 2.0 + uTime * 1.35) * 0.028 + sin(vertex.z * K + uTime * 0.72) * 0.025;
    float wz = cos(vertex.z * K * 2.0 - uTime * 1.05) * 0.028 + cos(vertex.x * K - uTime * 0.64) * 0.020;
    vertex.y += (wx+wz)*clamp(gl_MultiTexCoord1.z,0.0,1.0);
    vec4 eye = gl_ModelViewMatrix * vertex;
    gl_Position = gl_ProjectionMatrix * eye;
    vColor = gl_Color;
    vec2 lm = gl_MultiTexCoord1.xy;
    vLightUv = max(lm.x, lm.y) > 1.5 ? clamp(lm, vec2(0.0), vec2(240.0)) * (1.0 / 256.0) + vec2(0.03125) : clamp(lm, vec2(0.0), vec2(1.0));
    vBlockLight = clamp(gl_MultiTexCoord1.x / 240.0, 0.0, 1.0);
    vSkyLight = clamp(gl_MultiTexCoord1.y / 240.0, 0.0, 1.0);
    vUv = gl_MultiTexCoord0.xy;
    vNormalWorld = normalize(gl_Normal);
    vNormalView = normalize(gl_NormalMatrix * gl_Normal);
    vWorldPos = (gbufferModelViewInverse * eye).xyz + vec3(cameraPosition.x,0.0,cameraPosition.z);
    vWaveWeight=clamp(gl_MultiTexCoord1.z,0.0,1.0);
    vWaveX = wx;
    vWaveZ = wz;
}
