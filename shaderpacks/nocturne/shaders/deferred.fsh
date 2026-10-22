#version 120
#include "lib/common.glsl"
uniform sampler2D colortex0;
uniform sampler2D depthtex0;
uniform sampler2D normaltex0;
uniform vec2 viewSize;
uniform float near;
uniform float far;
uniform float sunBrightness;
uniform vec3 fogColor;
varying vec2 vUv;
void main() {
    vec3 c = texture2D(colortex0,vUv).rgb;
    float d = texture2D(depthtex0,vUv).r;
    if (d < 0.99998) {
        float z = linearDepth(d,near,far);
        vec3 n = decodeNormal(texture2D(normaltex0,vUv).rgb);
        vec2 px = 1.0/max(viewSize,vec2(1.0));
        float dz1 = abs(linearDepth(texture2D(depthtex0,sat2(vUv+vec2(px.x,0.0))).r,near,far)-z);
        float dz2 = abs(linearDepth(texture2D(depthtex0,sat2(vUv+vec2(0.0,px.y))).r,near,far)-z);
        float contact = smoothstep(0.03,0.75,min(dz1,dz2)) * (1.0-smoothstep(1.0,5.0,min(dz1,dz2)));
        c *= 1.0-contact*0.08;
        float mist = smoothstep(far*0.48,far*0.97,z);
        vec3 nightFog = fogColor*vec3(0.42,0.56,0.88);
        vec3 dayFog = fogColor*vec3(0.90,0.94,1.02);
        c = mix(c,mix(nightFog,dayFog,sunBrightness),mist*0.70);
    }
    gl_FragColor=vec4(c,1.0);
}
