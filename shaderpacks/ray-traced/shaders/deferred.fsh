#version 120
#ifndef RGBA16F
#define RGBA16F 34842
#endif
#include "lib/common.glsl"
/* RENDERTARGETS: 0 */
const int colortex5Format = RGBA16F;

uniform sampler2D colortex0;
uniform sampler2D colortex1;
uniform sampler2D colortex6;
uniform sampler2D colortex7;
uniform sampler2D colortex2;
uniform sampler2D depthtex0;
uniform sampler2D shadowtex0;
uniform sampler2D rtLighting;
uniform vec2 rtSize;
uniform int rtActive;
uniform vec2 viewSize;
uniform float near;
uniform float uTime;
uniform float rainStrength;
uniform float far;
uniform vec3 cameraPosition;
uniform vec3 sunPosition;
uniform mat4 gbufferProjectionInverse;
uniform mat4 gbufferProjection;
uniform mat4 gbufferModelViewInverse;
uniform mat4 shadowProjection;
uniform mat4 shadowModelView;
varying vec2 vUv;

// Short-range visibility supplements the world shadow map at feet, voxel edges
// and overhangs. All comparisons are in view-space metres, never raw depth.
float contactShadow(vec3 view, vec3 n, vec3 light) {
  if (dot(n, light) < 0.02 || -view.z > 85.0) return 1.0;
  vec3 start = view + n * 0.045;
  float visibility = 1.0;
  for (int i = 0; i < 12; i++) {
    float t = 0.08 + float(i) * 0.16;
    vec3 p = start + light * t;
    vec4 clip = gbufferProjection * vec4(p, 1.0);
    vec2 uv = clip.xy / max(clip.w, 0.0001) * 0.5 + 0.5;
    if (clip.w < 0.05 || min(uv.x, uv.y) <= 0.0 || max(uv.x, uv.y) >= 1.0) break;
    float d = texture2D(depthtex0, uv).r;
    float dz = -p.z - linearDepth(d, near, far);
    float thickness = 0.12 + t * 0.025;
    if (d < 0.99995) {
      float hit = smoothstep(0.015, 0.07, dz) * (1.0 - smoothstep(thickness * 0.7, thickness, dz));
      visibility = min(visibility, 1.0 - hit);
    }
  }
  return visibility;
}

float ambientVisibility(vec3 view, vec3 n) {
  if (-view.z > 100.0) return 1.0;
  float radius = 1.15, occlusion = 0.0;
  vec2 scale =
      vec2(gbufferProjection[0][0], gbufferProjection[1][1]) * radius / max(-view.z, 0.1) * 0.5;
  for (int i = 0; i < 8; i++) {
    float angle = float(i) * 0.785398;
    vec2 direction = vec2(cos(angle), sin(angle));
    float horizon = 0.0;
    for (int j = 0; j < 3; j++) {
      vec2 uv = vUv + direction * scale * (0.22 + float(j) * 0.34);
      if (min(uv.x, uv.y) <= 0.0 || max(uv.x, uv.y) >= 1.0) continue;
      float d = texture2D(depthtex0, uv).r;
      if (d > 0.99995) continue;
      vec3 delta = reconstructView(uv, d, gbufferProjectionInverse) - view;
      float distance = length(delta);
      float facing = max(dot(n, delta / max(distance, 0.001)) - 0.10, 0.0);
      horizon = max(horizon, facing * (1.0 - smoothstep(radius * 0.2, radius, distance)));
    }
    occlusion += horizon;
  }
  return 1.0 - occlusion * 0.12 * (1.0 - smoothstep(60.0, 100.0, -view.z));
}

void main() {
  vec3 color = texture2D(colortex0, vUv).rgb;
  float depth = texture2D(depthtex0, vUv).r;
  if (depth > 0.99995) {
    gl_FragColor = vec4(color, 1.0);
    return;
  }
  vec4 material = texture2D(colortex2, vUv);
  float sky = sat(material.g);
  vec3 direct = texture2D(colortex6, vUv).rgb;
  vec4 albedoA = texture2D(colortex7, vUv);
  vec3 albedo = albedoA.rgb;
  float keepLit = albedoA.a;
  vec3 view = reconstructView(vUv, depth, gbufferProjectionInverse);
  vec3 world = worldFromView(view, gbufferModelViewInverse, cameraPosition);
  vec3 n = decodeNormal(texture2D(colortex1, vUv).rgb);
  // View normal was stored; shadow wants a world normal. The 3x3 of the inverse
  // modelview rotates view space back to world for this camera-relative matrix.
  vec3 worldN = normalize(mat3(gbufferModelViewInverse) * n);
  float sh = 1.0;
  if (max(direct.r, max(direct.g, direct.b)) > 0.0001) {
    float dither = 0.37;  // fixed filter orientation avoids a stippled penumbra
    vec3 lightWorld = normalize(sunPosition.y > -0.02 ? sunPosition : -sunPosition);
    sh = pcssShadow(shadowtex0, world, worldN, lightWorld, cameraPosition, shadowModelView,
                    shadowProjection, dither);
    vec3 lightView = vec3(dot(gbufferModelViewInverse[0].xyz, lightWorld),
                          dot(gbufferModelViewInverse[1].xyz, lightWorld),
                          dot(gbufferModelViewInverse[2].xyz, lightWorld));
    if (sh > 0.001) sh = min(sh, contactShadow(view, n, lightView));
    if (sh > 0.001)
      sh *= cloudTransmittance(world, normalize(sunPosition), uTime, sat(rainStrength));
  }
  float ao = ambientVisibility(view, n);
  vec3 gi = vec3(0.0);
  if (rtActive == 1) {
    vec4 rt = upsampleRT(rtLighting, vUv, rtSize, depthtex0, depth);
    // Rays replace the shadow map, but the raster colour stays. Replacing it
    // with one-bounce noise turned leaves black and snow into a checkerboard.
    sh = min(sh, sat(rt.a));
    gi = rt.rgb;
  }
  vec3 shadowed = max(color - direct, vec3(0.0)) * mix(ao, 1.0, sat(material.a)) + direct * sh;
  shadowed += albedo * gi * 0.22 * sky;
  color = mix(color, max(shadowed, vec3(0.0)), keepLit * mix(1.0, sky, 0.35));
  gl_FragColor = vec4(max(color, vec3(0.0)), 1.0);
}
