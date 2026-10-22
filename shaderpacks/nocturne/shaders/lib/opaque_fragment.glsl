uniform sampler2D uAtlas;
uniform sampler2D lightmap;
uniform float uTime;
uniform float uDaylight;
uniform vec3 uSunDir;
uniform vec3 uMoonDir;
uniform int renderStage;
varying vec4 vColor;
varying vec2 vLightUv;
varying float vBlockLight;
varying float vSkyLight;
varying float vEmission;
varying vec2 vUv;
varying vec3 vNormalView;
varying vec3 vNormalWorld;
varying vec3 vViewPos;
varying vec3 vSunView;
varying vec3 vMoonView;
varying float vFogDepth;
void main() {
    vec4 texel = texture2D(uAtlas, vUv);
    vec4 base = texel * vec4(min(vColor.rgb, vec3(1.0)), vColor.a);
    if (base.a <= 0.01) discard;
    vec3 n = normalize(vNormalWorld);
    float sun = max(dot(n, normalize(uSunDir)), 0.0);
    float moon = max(dot(n, normalize(uMoonDir)), 0.0);
    float night = 1.0 - sat((uDaylight - 0.10) / 0.90);
    float exposure = vSkyLight * vSkyLight * vSkyLight;
    vec3 light = bbBlockLight(lightmap, vLightUv, vSkyLight, vBlockLight) * mix(1.0, 0.86, vSkyLight * uDaylight);
    // Moonlight is the star of this pack: a strong cool key on moon-facing faces.
    light += (vec3(0.90, 0.91, 0.86) * sun * uDaylight * 0.20 + vec3(0.46, 0.58, 0.94) * moon * night * 0.24) * exposure;
    base.rgb *= light;
    // Cool the night grade everywhere torches do not reach.
    float torch = vBlockLight * vBlockLight;
    base.rgb = mix(base.rgb, base.rgb * vec3(0.82, 0.92, 1.14), night * 0.16 * (1.0 - torch));
    float flicker = 0.975 + 0.025 * sin(uTime * 9.0 + vUv.x * 41.0) * sin(uTime * 6.3);
    base.rgb = mix(base.rgb, texel.rgb * vec3(1.24, 1.02, 0.62) * flicker, vEmission);
    gl_FragData[0] = base;
    gl_FragData[1] = vec4(normalize(vNormalView) * 0.5 + 0.5, 1.0);
    gl_FragData[2] = vec4(float(renderStage) / 255.0, base.a, 0.0, max(vBlockLight, vEmission));
}
