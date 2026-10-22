uniform sampler2D uAtlas;
uniform sampler2D lightmap;
uniform float uTime;
uniform float uDaylight;
uniform vec3 uSunDir;
uniform vec3 uMoonDir;
uniform int renderStage;
uniform int isEyeInWater;
uniform float rainStrength;
uniform vec3 uFogColor;
uniform mat4 gbufferModelViewInverse;
uniform vec3 cameraPosition;
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
    float up = sat(n.y * 0.5 + 0.5);
    float night = 1.0 - sat((uDaylight - 0.12) / 0.88);
    float exposure = vSkyLight * vSkyLight * vSkyLight;
    // Clean daylight: the lightmap's sky term gets a slightly cool fill from above,
    // then a crisp warm key from the sun on faces that can see it.
    vec3 light = bbBlockLight(lightmap, vLightUv, vSkyLight, vBlockLight);
    light *= mix(vec3(1.0), mix(vec3(0.78, 0.80, 0.83), vec3(0.86, 0.87, 0.89), up), vSkyLight * uDaylight);
    light += (vec3(1.00, 0.94, 0.80) * sun * uDaylight * 0.22 + vec3(0.44, 0.56, 0.84) * moon * night * 0.08) * exposure;
    base.rgb *= light * (1.0 - rainStrength * exposure * 0.18);
    float flicker = 0.975 + 0.025 * sin(uTime * 9.0 + vUv.x * 41.0) * sin(uTime * 6.3);
    base.rgb = mix(base.rgb, texel.rgb * vec3(1.18, 1.03, 0.72) * flicker, vEmission);
    if (isEyeInWater != 0) {
        vec3 world = (gbufferModelViewInverse * vec4(vViewPos,1.0)).xyz;
        world.xz += cameraPosition.xz;
        float waves = sin((world.x+world.z)*1.5707963+uTime*0.8)
                    + sin((world.x-world.z)*0.7853982-uTime*0.65);
        float caustic = pow(max(1.0-abs(waves)*2.4,0.0),6.0);
        base.rgb += vec3(0.10,0.18,0.16)*caustic*max(n.y,0.0)*vSkyLight*uDaylight;
        base.rgb *= exp(-vec3(0.022,0.007,0.003)*vFogDepth);
        float waterFog = 1.0-exp(-max(vFogDepth-0.6,0.0)*0.075);
        base.rgb = mix(base.rgb,uFogColor,waterFog);
    }
    gl_FragData[0] = base;
    gl_FragData[1] = vec4(normalize(vNormalView) * 0.5 + 0.5, 1.0);
    gl_FragData[2] = vec4(float(renderStage) / 255.0, base.a, 0.0, max(vBlockLight, vEmission));
}
