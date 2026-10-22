#version 120
#include "lib/common.glsl"
uniform sampler2D colortex0;
uniform sampler2D depthtex0;
uniform sampler2D normaltex0;
uniform sampler2D materialtex0;
uniform vec2 viewSize;
uniform float near;
uniform float far;
uniform float sunBrightness;
uniform vec3 fogColor;
uniform mat4 gbufferProjectionInverse;
uniform mat4 gbufferModelViewInverse;
uniform vec3 cameraPosition;
varying vec2 vUv;

float aoTap(vec2 offset, float centerDepth, vec3 centerNormal, vec2 texel) {
    vec2 uv = sat2(vUv + offset * texel);
    float raw = texture2D(depthtex0, uv).r;
    if (raw >= 0.99998) return 0.0;
    float z = linearDepth(raw, near, far);
    vec3 n = decodeNormal(texture2D(normaltex0, uv).rgb);
    float dz = centerDepth - z;
    float depthOcc = smoothstep(0.03, 0.85, dz) * (1.0 - smoothstep(1.1, 5.5, abs(dz)));
    float normalOcc = pow(1.0 - sat(dot(centerNormal, n)), 1.35) * 0.16;
    return max(depthOcc, normalOcc);
}

void main() {
    vec3 color = texture2D(colortex0, vUv).rgb;
    float d = texture2D(depthtex0, vUv).r;
    if (d >= 0.99998) {
        gl_FragColor = vec4(color, 1.0);
        return;
    }

    float z = linearDepth(d, near, far);
    vec3 n = decodeNormal(texture2D(normaltex0, vUv).rgb);
    float stage = stageFromMaterial(texture2D(materialtex0, vUv));
    vec2 texel = 1.0 / max(viewSize, vec2(1.0));

    // Eight low-radius depth/normal taps. Water/translucency gets much less AO.
    float radius = mix(1.35, 2.65, sat(z / 72.0));
    float occ = 0.0;
    occ += aoTap(vec2( 1.0, 0.0) * radius, z, n, texel);
    occ += aoTap(vec2(-1.0, 0.0) * radius, z, n, texel);
    occ += aoTap(vec2( 0.0, 1.0) * radius, z, n, texel);
    occ += aoTap(vec2( 0.0,-1.0) * radius, z, n, texel);
    occ += aoTap(vec2( 0.72, 0.72) * radius, z, n, texel);
    occ += aoTap(vec2(-0.72, 0.72) * radius, z, n, texel);
    occ += aoTap(vec2( 0.72,-0.72) * radius, z, n, texel);
    occ += aoTap(vec2(-0.72,-0.72) * radius, z, n, texel);
    occ *= 0.125;
    float aoStrength = (stage > 1.5) ? 0.10 : 0.34;
    aoStrength *= 1.0 - texture2D(materialtex0,vUv).a * 0.8;
    color *= 1.0 - occ * aoStrength;

    // Reconstruct camera-relative world position for height-aware atmosphere.
    vec3 viewPos = reconstructView(vUv, d, gbufferProjectionInverse);
    vec4 relWorld4 = gbufferModelViewInverse * vec4(viewPos, 1.0);
    vec3 worldPos = relWorld4.xyz;
    worldPos.x += cameraPosition.x;
    worldPos.z += cameraPosition.z;

    float distFog = smoothstep(far * 0.52, far * 0.96, z);
    float lowMist = exp(-max(worldPos.y - 50.0, 0.0) * 0.030) * smoothstep(22.0, 150.0, z) * 0.12;
    float fogAmount = sat(distFog * 0.72 + lowMist);
    vec3 atmosphere = mix(fogColor * vec3(0.72, 0.81, 0.96), fogColor * vec3(1.10, 1.02, 0.91), sunBrightness);
    color = mix(color, atmosphere, fogAmount);

    gl_FragColor = vec4(color, 1.0);
}
