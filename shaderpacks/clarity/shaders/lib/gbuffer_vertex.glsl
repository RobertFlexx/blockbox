uniform float uTime;
uniform mat4 gbufferModelViewInverse;
uniform int isEyeInWater;
uniform vec3 uSunDir;
uniform vec3 uMoonDir;
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
    vec4 vertex=gl_Vertex;
    if(max(gl_Color.r,max(gl_Color.g,gl_Color.b))>=1.75) {
        float phase=uTime*7.3+gl_Vertex.x*1.7+gl_Vertex.z*2.9;
        vertex.x+=sin(phase)*0.009;
        vertex.z+=cos(phase*0.83)*0.009;
        vertex.y+=sin(phase*1.31)*0.012;
    }
    vec4 eye = gl_ModelViewMatrix * vertex;
    gl_Position = gl_ProjectionMatrix * eye;
    // Vertex colour is face shade, ambient occlusion and biome tint only. Sky and
    // torch light arrive as (block, sky) * 240 and are resolved by `lightmap`.
    vColor = gl_Color;
    vec2 lm = gl_MultiTexCoord1.xy;
    vLightUv = max(lm.x, lm.y) > 1.5 ? clamp(lm, vec2(0.0), vec2(240.0)) * (1.0 / 256.0) + vec2(0.03125) : clamp(lm, vec2(0.0), vec2(1.0));
    vBlockLight = clamp(gl_MultiTexCoord1.x / 240.0, 0.0, 1.0);
    vSkyLight = clamp(gl_MultiTexCoord1.y / 240.0, 0.0, 1.0);
    vEmission = step(1.75, max(gl_Color.r, max(gl_Color.g, gl_Color.b)));
    vUv = gl_MultiTexCoord0.xy;
    vNormalView = normalize(gl_NormalMatrix * gl_Normal);
    vNormalWorld = normalize(gl_Normal);
    vViewPos = eye.xyz;
    vSunView = normalize(gl_NormalMatrix * uSunDir);
    vMoonView = normalize(gl_NormalMatrix * uMoonDir);
    vFogDepth = isEyeInWater != 0 ? length(eye.xyz) : length((gbufferModelViewInverse * eye).xz);
}
