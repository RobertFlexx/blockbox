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
    vec3 sdir = normalize(uSunDir);
    vec3 mdir = normalize(uMoonDir);
    float sun = max(dot(n, sdir), 0.0);
    float moon = max(dot(n, mdir), 0.0);
    float up = sat(n.y * 0.5 + 0.5);
    float night = 1.0 - sat((uDaylight - 0.10) / 0.90);
    float sunrise = (1.0 - smoothstep(0.10, 0.48, abs(sdir.y))) * uDaylight;
    float exposure = vSkyLight * vSkyLight * vSkyLight;

    // Ambient comes from the engine lightmap (sky, torches, dusk, weather). Open
    // sky adds a soft blue fill from above and a cooler bounce on the undersides.
    vec3 ambient = bbBlockLight(lightmap, vLightUv, vSkyLight, vBlockLight);
    vec3 skyFill = mix(vec3(0.66, 0.70, 0.77), vec3(0.80, 0.81, 0.84), up);
    vec3 light = ambient * mix(vec3(1.0), skyFill, vSkyLight * uDaylight);

    vec3 sunColor = mix(vec3(1.00, 0.95, 0.84), vec3(1.00, 0.57, 0.30), sunrise * 0.85);
    vec3 moonColor = vec3(0.34, 0.47, 0.76);
    float wrapSun = sat((dot(n, sdir) + 0.18) / 1.18);
    light += (sunColor * wrapSun * uDaylight * 0.25 + moonColor * moon * night * 0.12) * exposure;
    base.rgb *= light;

    // Very subtle sun-facing micro contrast, intentionally not plastic/specular.
    float faceLift = pow(sun, 2.0) * uDaylight * exposure;
    base.rgb *= 1.0 + faceLift * 0.055;

    float flicker = 0.975 + 0.025 * sin(uTime * 9.0 + vUv.x * 41.0) * sin(uTime * 6.3);
    base.rgb = mix(base.rgb, texel.rgb * vec3(1.30, 1.06, 0.66) * flicker, vEmission);
    gl_FragData[0] = base;
    gl_FragData[1] = vec4(normalize(vNormalView) * 0.5 + 0.5, 1.0);
    gl_FragData[2] = vec4(float(renderStage) / 255.0, texel.a, 0.0, max(vBlockLight, vEmission));
}
