#version 120
uniform int uAntialiasing;
uniform vec2 viewSize;
uniform float sunBrightness;
uniform sampler2D colortex0;
#include "lib/common.glsl"
varying vec2 vUv;
void main() {
    vec3 c=bbResolveAA(colortex0,vUv,viewSize,uAntialiasing);
    c *= mix(vec3(0.98,0.99,1.015),vec3(1.008,1.002,0.992),sunBrightness);
    c=bbNaturalTone(c);
    gl_FragColor=vec4(bbDisplayDither(c,gl_FragCoord.xy),1.0);
}
