#version 120
uniform int uAntialiasing;
uniform vec2 viewSize;
#include "lib/common.glsl"
uniform sampler2D colortex0;
varying vec2 vUv;
void main() {
    vec3 c = max(bbResolveAA(colortex0, vUv, viewSize, uAntialiasing), vec3(0.0));
    // Preserve the texture palette; only compress runaway HDR highlights and add a
    // tiny display-space contrast lift.
    c = c / (vec3(1.0) + max(c - vec3(1.0), vec3(0.0)) * 0.45);
    c = (c - 0.5) * 1.025 + 0.5;
    gl_FragColor = vec4(bbDisplayDither(clamp(c, 0.0, 1.0),gl_FragCoord.xy),1.0);
}
