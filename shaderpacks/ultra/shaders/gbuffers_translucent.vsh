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
varying float vFogDepth;
void main() {
    vec4 eye = gl_ModelViewMatrix * gl_Vertex;
    gl_Position = gl_ProjectionMatrix * eye;
    vColor = gl_Color;
    if(gl_MultiTexCoord1.z>0.0) vColor.rgb/=max(gl_MultiTexCoord1.z,0.1);
    vec2 lm = gl_MultiTexCoord1.xy;
    vLightUv = max(lm.x, lm.y) > 1.5 ? clamp(lm, vec2(0.0), vec2(240.0)) * (1.0 / 256.0) + vec2(0.03125) : clamp(lm, vec2(0.0), vec2(1.0));
    vBlockLight = clamp(gl_MultiTexCoord1.x / 240.0, 0.0, 1.0);
    vSkyLight = clamp(gl_MultiTexCoord1.y / 240.0, 0.0, 1.0);
    vUv = gl_MultiTexCoord0.xy;
    vNormalWorld = normalize(gl_Normal);
    vFogDepth = isEyeInWater != 0 ? length(eye.xyz) : length((gbufferModelViewInverse * eye).xz);
}
