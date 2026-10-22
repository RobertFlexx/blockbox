#version 120
#include "lib/common.glsl"
uniform sampler2D colortex0;
uniform vec2 viewSize;
uniform float sunBrightness;
varying vec2 vUv;
void main() {
    vec2 px=1.0/max(viewSize,vec2(1.0));
    vec3 c=texture2D(colortex0,vUv).rgb;
    vec3 glow=vec3(0.0);
    glow+=texture2D(colortex0,vUv+vec2(2.0*px.x,0.0)).rgb;
    glow+=texture2D(colortex0,vUv-vec2(2.0*px.x,0.0)).rgb;
    glow+=texture2D(colortex0,vUv+vec2(0.0,2.0*px.y)).rgb;
    glow+=texture2D(colortex0,vUv-vec2(0.0,2.0*px.y)).rgb;
    glow*=0.25;
    glow*=smoothstep(1.05,2.50,luma(glow));
    c+=glow*mix(0.065,0.045,sunBrightness);
    gl_FragColor=vec4(c,1.0);
}
