#version 120
#ifndef RGBA16F
#define RGBA16F 34842
#endif
#include "lib/common.glsl"
/* RENDERTARGETS: 0 */
const int colortex5Format = RGBA16F;

uniform sampler2D colortex0;
uniform sampler2D colortex1;
uniform sampler2D colortex6;
uniform sampler2D colortex7;
uniform sampler2D colortex2;
uniform sampler2D depthtex0;
uniform sampler2D shadowtex0;
uniform sampler2D rtLighting;
uniform vec2 rtSize;
uniform int rtActive;
uniform vec2 viewSize;
uniform float near;
uniform float far;
uniform vec3 cameraPosition;
uniform vec3 sunPosition;
uniform mat4 gbufferProjectionInverse;
uniform mat4 gbufferModelViewInverse;
uniform mat4 shadowProjection;
uniform mat4 shadowModelView;
varying vec2 vUv;

void main() {
    vec3 color = texture2D(colortex0, vUv).rgb;
    float depth = texture2D(depthtex0, vUv).r;
    if (depth > 0.99995) {
        gl_FragColor = vec4(color, 1.0);
        return;
    }
    vec4 material = texture2D(colortex2, vUv);
    float sky = sat(material.g);
    vec3 direct = texture2D(colortex6, vUv).rgb;
    vec4 albedoA = texture2D(colortex7, vUv);
    vec3 albedo = albedoA.rgb;
    float keepLit = albedoA.a;
    vec3 view = reconstructView(vUv, depth, gbufferProjectionInverse);
    vec3 world = worldFromView(view, gbufferModelViewInverse, cameraPosition);
    vec3 n = decodeNormal(texture2D(colortex1, vUv).rgb);
    // View normal was stored; shadow wants a world normal. The 3x3 of the inverse
    // modelview rotates view space back to world for this camera-relative matrix.
    vec3 worldN = normalize(mat3(gbufferModelViewInverse) * n);
    float dither = hash12(gl_FragCoord.xy + cameraPosition.xz);
    float sh = pcssShadow(shadowtex0,world,worldN,sunPosition.y> -0.02 ? sunPosition : -sunPosition, cameraPosition, shadowModelView, shadowProjection, dither);
    vec3 gi = vec3(0.0);
    if (rtActive == 1) {
        vec4 rt = upsampleRT(rtLighting, vUv, rtSize, depthtex0, depth);
        // Rays replace the shadow map, but the raster colour stays. Replacing it
        // with one-bounce noise turned leaves black and snow into a checkerboard.
        sh=min(sh,sat(rt.a));
        gi = rt.rgb;
    }
    vec3 shadowed = (color - direct) * mix(0.82, 1.0, sh) + direct * sh;
    shadowed += albedo * gi * 0.22 * sky;
    color = mix(color, max(shadowed, vec3(0.0)), keepLit * mix(1.0, sky, 0.35));
    gl_FragColor = vec4(max(color, vec3(0.0)), 1.0);
}
