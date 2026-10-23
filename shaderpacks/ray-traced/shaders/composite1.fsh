#version 120
/* RENDERTARGETS: 6 */
uniform sampler2D colortex5;
uniform vec2 viewSize;
varying vec2 vUv;
void main() {
  vec2 px = vec2(1.0 / max(viewSize.x, 1.0), 0.0);
  vec3 c = texture2D(colortex5, vUv).rgb * 0.22;
  c += texture2D(colortex5, vUv + px * 2.0).rgb * 0.16;
  c += texture2D(colortex5, vUv - px * 2.0).rgb * 0.16;
  c += texture2D(colortex5, vUv + px * 5.0).rgb * 0.12;
  c += texture2D(colortex5, vUv - px * 5.0).rgb * 0.12;
  c += texture2D(colortex5, vUv + px * 9.0).rgb * 0.08;
  c += texture2D(colortex5, vUv - px * 9.0).rgb * 0.08;
  c += texture2D(colortex5, vUv + px * 16.0).rgb * 0.03;
  c += texture2D(colortex5, vUv - px * 16.0).rgb * 0.03;
  gl_FragColor = vec4(c, 1.0);
}
