#version 120
varying vec2 vUv;
varying vec4 vColor;
varying float vBlockLight;
varying float vEmission;
void main() {
    gl_Position = ftransform();
    vUv = (gl_TextureMatrix[0] * gl_MultiTexCoord0).xy;
    vColor = gl_Color;
    vBlockLight = clamp(gl_MultiTexCoord1.x / 240.0, 0.0, 1.0);
    vEmission = step(1.75, max(gl_Color.r, max(gl_Color.g, gl_Color.b)));
}
