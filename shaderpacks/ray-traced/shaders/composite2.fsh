#version 120
/* RENDERTARGETS: 5 */
uniform sampler2D colortex6;
uniform vec2 viewSize;
varying vec2 vUv;
void main() {
    vec2 px = vec2(0.0, 1.0 / max(viewSize.y, 1.0));
    vec3 c = texture2D(colortex6, vUv).rgb * 0.22;
    c += texture2D(colortex6, vUv + px * 2.0).rgb * 0.16;
    c += texture2D(colortex6, vUv - px * 2.0).rgb * 0.16;
    c += texture2D(colortex6, vUv + px * 5.0).rgb * 0.12;
    c += texture2D(colortex6, vUv - px * 5.0).rgb * 0.12;
    c += texture2D(colortex6, vUv + px * 9.0).rgb * 0.08;
    c += texture2D(colortex6, vUv - px * 9.0).rgb * 0.08;
    c += texture2D(colortex6, vUv + px * 16.0).rgb * 0.03;
    c += texture2D(colortex6, vUv - px * 16.0).rgb * 0.03;
    gl_FragColor = vec4(c, 1.0);
}
