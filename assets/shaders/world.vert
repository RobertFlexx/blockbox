#version 120
uniform mat4 gbufferModelViewInverse;
uniform int isEyeInWater;

varying vec4 vColor;
varying vec2 vLightUv;
varying float vBlockLight;
varying float vSkyLight;
varying float vEmission;
varying vec2 vUv;
varying vec3 vNormal;
varying float vFogDepth;
varying float vWorldY;
varying vec2 vLocalXZ;

void main() {
    vec4 eyePosition = gl_ModelViewMatrix * gl_Vertex;
    gl_Position = gl_ProjectionMatrix * eyePosition;
    vColor = gl_Color;
    // UV1 carries block/sky light on Minecraft's 0..240 scale; texture matrix 1 maps it
    // onto the lightmap's texel centres.
    vec2 lm = gl_MultiTexCoord1.xy;
    vLightUv = max(lm.x, lm.y) > 1.5 ? clamp(lm, vec2(0.0), vec2(240.0)) * (1.0 / 256.0) + vec2(0.03125) : clamp(lm, vec2(0.0), vec2(1.0));
    vBlockLight = clamp(gl_MultiTexCoord1.x / 240.0, 0.0, 1.0);
    vSkyLight = clamp(gl_MultiTexCoord1.y / 240.0, 0.0, 1.0);
    vEmission = step(1.75, max(gl_Color.r, max(gl_Color.g, gl_Color.b)));
    vUv = gl_MultiTexCoord0.xy;
    // Chunk normals are emitted in world/object space. Keeping them there makes
    // the sun/moon direction independent from camera yaw and pitch.
    vNormal = normalize(gl_Normal);
    vFogDepth = isEyeInWater != 0 ? length(eyePosition.xyz) : length((gbufferModelViewInverse * eyePosition).xz);
    vWorldY = gl_Vertex.y;
    vLocalXZ = gl_Vertex.xz;
}
