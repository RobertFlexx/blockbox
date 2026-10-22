#version 120

uniform sampler2D uAtlas;
uniform sampler2D lightmap;
uniform vec3 uFogColor;
uniform float uFogStart;
uniform float uFogEnd;
uniform float uDaylight;
uniform float uTime;
uniform int uPass;
uniform vec3 uSunDir;
uniform vec3 uMoonDir;

varying vec4 vColor;
varying vec2 vLightUv;
varying float vBlockLight;
varying float vSkyLight;
varying float vEmission;
varying vec2 vUv;
varying vec3 vNormal;
varying float vFogDepth;
varying float vWorldY;
varying vec2 vLocalXZ;

float saturate(float v) { return clamp(v, 0.0, 1.0); }

vec3 bbBlockLight(sampler2D lightmap, vec2 uv, float sky, float block) {
    vec3 light = texture2D(lightmap, uv).rgb;
    float known = max(sky, block);
    if (max(light.r, max(light.g, light.b)) < 0.08 && known > 0.35)
        light = max(light, vec3(0.62, 0.72, 0.84) * sky + vec3(1.0, 0.78, 0.46) * block);
    return light;
}

void main() {
    vec4 texel = texture2D(uAtlas, vUv);
    // Vertex colour is face shade x ambient occlusion x biome tint; flames exceed 1.75.
    vec4 albedo = texel * vec4(min(vColor.rgb, vec3(1.0)), vColor.a);

    // The lightmap already resolves sky light (time of day, weather) and warm torch
    // light. Direct sun and moon add shape only where the sky actually reaches.
    vec3 ambient = bbBlockLight(lightmap, vLightUv, vSkyLight, vBlockLight);
    vec3 n = normalize(vNormal);
    float sunFacing = max(dot(n, normalize(uSunDir)), 0.0);
    float moonFacing = max(dot(n, normalize(uMoonDir)), 0.0);
    float night = 1.0 - saturate((uDaylight - 0.18) / 0.82);
    float horizonWarmth = saturate(1.0 - max(uSunDir.y, 0.0) * 2.3);
    vec3 sunColor = mix(vec3(1.10, 0.72, 0.46), vec3(1.04, 0.99, 0.90), 1.0 - horizonWarmth);
    float exposure = vSkyLight * vSkyLight * vSkyLight;
    vec3 direct = sunColor * sunFacing * uDaylight * 0.18 + vec3(0.55, 0.70, 1.0) * moonFacing * night * 0.05;
    // Open daylight trades a little sky fill for the sun term so sunlit tops peak
    // near 1.0 and faces turned away from the sun keep their shape.
    vec3 color = albedo.rgb * (ambient * mix(1.0, 0.86, vSkyLight * uDaylight) + direct * exposure);

    // A tiny time-varying water grade gives oceans/lakes life without moving the
    // actual mesh or breaking block collision/flow behavior.
    if (uPass == 3) {
        // Frequencies are chosen so the 16-block chunk-local coordinate reset is
        // periodic, keeping the effect seamless without a per-chunk world-offset uniform.
        float shimmerA = sin((vLocalXZ.x + vLocalXZ.y) * 0.7853982 + uTime * 1.35) * 0.5 + 0.5;
        float shimmerB = sin((vLocalXZ.x - vLocalXZ.y) * 0.3926991 - uTime * 0.91) * 0.5 + 0.5;
        float shimmer = shimmerA * 0.65 + shimmerB * 0.35;
        vec3 waterTint = vec3(0.10, 0.38, 0.68) * ambient;
        color = mix(color, waterTint, 0.045 + shimmer * 0.035);
    }

    float flicker = 0.975 + 0.025 * sin(uTime * 9.0 + vUv.x * 41.0) * sin(uTime * 6.3);
    color = mix(color, texel.rgb * vec3(1.22, 1.04, 0.66) * flicker, vEmission);

    // Gentle shoulder so sunlit highlights and flames roll off instead of clipping.
    color = color / (vec3(0.94) + color * 0.06);
    float fogRange = max(uFogEnd - uFogStart, 0.001);
    float fog = saturate((uFogEnd - vFogDepth) / fogRange);
    color = mix(uFogColor, color, fog);
    gl_FragColor = vec4(color, albedo.a);
}
