#version 120
const int shadowMapResolution = 4096;
const float shadowDistance = 160.0;
uniform sampler2D gtexture;
varying vec2 vUv;
varying vec4 vColor;
void main() {
  vec4 albedo = texture2D(gtexture, vUv) * vColor;
  if (max(vColor.r, max(vColor.g, vColor.b)) >= 1.75) discard;
  if (albedo.a < 0.45) discard;
  gl_FragColor = vec4(albedo.rgb, 1.0);
}
