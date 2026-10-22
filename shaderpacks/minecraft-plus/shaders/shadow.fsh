#version 120
uniform sampler2D gtexture;
varying vec2 vUv;
varying vec4 vColor;
void main() {
    if (max(vColor.r,max(vColor.g,vColor.b))>=1.75) discard;
    vec4 albedo = texture2D(gtexture, vUv) * vColor;
    if (albedo.a < 0.35) discard;
    gl_FragColor = vec4(1.0);
}
