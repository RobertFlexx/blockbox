#version 120
#include "lib/common.glsl"
uniform sampler2D colortex0;
uniform sampler2D depthtex0;
uniform sampler2D normaltex0;
uniform sampler2D materialtex0;
uniform sampler2D shadowtex0;
uniform vec2 viewSize;
uniform mat4 gbufferProjectionInverse;
uniform mat4 gbufferModelViewInverse;
uniform mat4 shadowProjection;
uniform mat4 shadowModelView;
const int shadowMapResolution = 2048;
const float shadowDistance = 192.0;
const int noiseTextureResolution = 256;
uniform float sunBrightness;
uniform float rainStrength;
varying vec2 vUv;

float bbShadow(vec3 playerPos, vec3 normalView) {
    vec4 shadowClip = shadowProjection * shadowModelView * vec4(playerPos, 1.0);
    vec3 shadowNdc = shadowClip.xyz / max(abs(shadowClip.w), 1e-6);
    vec3 suv = shadowNdc * 0.5 + 0.5;
    if (suv.x <= 0.002 || suv.x >= 0.998 || suv.y <= 0.002 || suv.y >= 0.998 || suv.z <= 0.0 || suv.z >= 1.0) return 1.0;
    float texel = 1.0 / max(float(shadowMapResolution), 1.0);
    // Bias grows slightly on grazing surfaces to suppress acne without making
    // contact shadows detach from blocks.
    float slope = 1.0 - clamp(abs(normalView.z), 0.0, 1.0);
    float compareDepth = suv.z - (0.00055 + slope * 0.00075);
    float lit = 0.0;
    for (int y = -1; y <= 1; ++y) {
        for (int x = -1; x <= 1; ++x) {
            float stored = texture2D(shadowtex0, suv.xy + vec2(float(x), float(y)) * texel).r;
            lit += compareDepth <= stored ? 1.0 : 0.0;
        }
    }
    return lit / 9.0;
}

void main() {
    vec2 px = 1.0 / max(viewSize, vec2(1.0));
    vec3 c = texture2D(colortex0, vUv).rgb;
    float d = texture2D(depthtex0, vUv).r;
    vec3 n = normalize(texture2D(normaltex0, vUv).rgb * 2.0 - 1.0);

    if (d < 0.999999) {
        vec4 clip = vec4(vUv * 2.0 - 1.0, d * 2.0 - 1.0, 1.0);
        vec4 view = gbufferProjectionInverse * clip;
        view /= max(abs(view.w), 1e-6);
        vec3 playerPos = (gbufferModelViewInverse * view).xyz;
        float shadow = bbShadow(playerPos, n);
        // Keep Minecraft's readable palette: shadows add depth instead of crushing
        // caves/faces into black. Twilight is intentionally gentler.
        float strength = mix(0.16, 0.34, clamp((sunBrightness - 0.18) / 0.82, 0.0, 1.0));
        strength *= mix(1.0, 0.62, clamp(rainStrength, 0.0, 1.0));
        vec4 material = texture2D(materialtex0, vUv);
        // Sun shadows only matter where the sky reaches; caves are already dark.
        strength *= material.g * material.g;
        strength *= 1.0 - material.a * material.a;
        c *= mix(1.0 - strength, 1.0, shadow);
    }

    float d1 = texture2D(depthtex0, vUv + vec2(px.x, 0.0)).r;
    float d2 = texture2D(depthtex0, vUv + vec2(-px.x, 0.0)).r;
    float d3 = texture2D(depthtex0, vUv + vec2(0.0, px.y)).r;
    float d4 = texture2D(depthtex0, vUv + vec2(0.0,-px.y)).r;
    float edge = clamp((abs(d-d1)+abs(d-d2)+abs(d-d3)+abs(d-d4))*18.0, 0.0, 1.0);
    float normalFacing = clamp(n.z * 0.5 + 0.5, 0.0, 1.0);
    c *= 1.0 - edge * (0.022 + (1.0-normalFacing)*0.015);
    // Cloudy weather slightly compresses the brightest values without turning the
    // world gray; terrain shaders already handle the material wetness response.
    c *= 1.0 - clamp(rainStrength, 0.0, 1.0) * 0.025;
    gl_FragColor = vec4(c, 1.0);
}
