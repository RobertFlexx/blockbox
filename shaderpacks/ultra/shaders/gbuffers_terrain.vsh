#version 120
uniform float uTime;
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
void main() {
    vec4 vertex = gl_Vertex;
    // Flames and other emissive vertices shimmer. Everything else stays put.
    if (max(gl_Color.r, max(gl_Color.g, gl_Color.b)) >= 1.75) {
        float phase = uTime * 7.3 + gl_Vertex.x * 1.7 + gl_Vertex.z * 2.9;
        vertex.x += sin(phase) * 0.009;
        vertex.z += cos(phase * 0.83) * 0.009;
        vertex.y += sin(phase * 1.31) * 0.012;
    }
    vec4 eye = gl_ModelViewMatrix * vertex;
    gl_Position = gl_ProjectionMatrix * eye;
    gl_ClipVertex=eye;
    vColor = gl_Color;
    // Vertex shade exists for the compatibility renderer. HDR lighting already
    // shades these normals, so retain biome tint/AO without darkening faces twice.
    if(gl_MultiTexCoord1.z>0.0) vColor.rgb/=max(gl_MultiTexCoord1.z,0.1);
    vec2 lm = gl_MultiTexCoord1.xy;
    vLightUv = max(lm.x, lm.y) > 1.5 ? clamp(lm, vec2(0.0), vec2(240.0)) * (1.0 / 256.0) + vec2(0.03125) : clamp(lm, vec2(0.0), vec2(1.0));
    vBlockLight = clamp(gl_MultiTexCoord1.x / 240.0, 0.0, 1.0);
    vSkyLight = clamp(gl_MultiTexCoord1.y / 240.0, 0.0, 1.0);
    vEmission = step(1.75, max(gl_Color.r, max(gl_Color.g, gl_Color.b)));
    vUv = gl_MultiTexCoord0.xy;
    vNormalWorld = normalize(gl_Normal);
    vNormalView = normalize(gl_NormalMatrix * gl_Normal);
    vFogDepth = isEyeInWater != 0 ? length(eye.xyz) : length((gbufferModelViewInverse * eye).xz);
    vWorldPos = (gbufferModelViewInverse * eye).xyz + vec3(cameraPosition.x,0.0,cameraPosition.z);
}
