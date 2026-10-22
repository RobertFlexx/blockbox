#version 120
/* RENDERTARGETS: 5 */
uniform sampler2D colortex0;
uniform float sunBrightness;
uniform float rainStrength;
varying vec2 vUv;
void main() {
    vec3 c = max(texture2D(colortex0, vUv).rgb, vec3(0.0));
    float l = dot(c, vec3(0.2126, 0.7152, 0.0722));
    float knee=mix(0.45,1.25,clamp((sunBrightness-0.18)/0.82,0.0,1.0));
    float soft=max(l-knee+0.25,0.0);
    soft=soft*soft/(soft+0.5);
    vec3 bright=c*(soft/max(l,0.0001));
    bright*=1.0-rainStrength*0.35;
    gl_FragColor = vec4(bright, 1.0);
}
