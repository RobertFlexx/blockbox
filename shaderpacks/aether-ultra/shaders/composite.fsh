#version 120
#include "lib/common.glsl"
uniform sampler2D colortex0;
uniform sampler2D depthtex0;
uniform vec2 viewSize;
varying vec2 vUv;

vec3 sampleBloom(vec2 uv, vec2 o) {
    vec3 c = texture2D(colortex0, sat2(uv + o)).rgb;
    float y = luma(c);
    return c * smoothstep(1.10, 2.50, y);
}

void main() {
    vec2 px = 1.0 / max(viewSize, vec2(1.0));
    vec3 c = texture2D(colortex0, vUv).rgb;
    vec3 b = vec3(0.0);
    b += sampleBloom(vUv, vec2( 3.0, 0.0) * px);
    b += sampleBloom(vUv, vec2(-3.0, 0.0) * px);
    b += sampleBloom(vUv, vec2(0.0,  3.0) * px);
    b += sampleBloom(vUv, vec2(0.0, -3.0) * px);
    b += sampleBloom(vUv, vec2( 2.15, 2.15) * px);
    b += sampleBloom(vUv, vec2(-2.15, 2.15) * px);
    b += sampleBloom(vUv, vec2( 2.15,-2.15) * px);
    b += sampleBloom(vUv, vec2(-2.15,-2.15) * px);
    b *= 0.125;
    c += b * 0.065;
    gl_FragColor = vec4(c, 1.0);
}
