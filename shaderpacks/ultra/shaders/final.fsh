#version 120
#include "lib/common.glsl"
uniform sampler2D colortex0;
uniform sampler2D colortex5;
uniform sampler2D depthtex0;
uniform vec2 viewSize;
uniform int uAntialiasing;
uniform float sunBrightness;
uniform float rainStrength;
uniform vec3 sunPosition;
uniform mat4 gbufferModelView;
uniform mat4 gbufferProjection;
uniform int isEyeInWater;
varying vec2 vUv;

void main() {
  vec3 color = max(resolveAA(colortex0, vUv, viewSize, uAntialiasing), vec3(0.0));
  vec3 bloom = max(texture2D(colortex5, vUv).rgb, vec3(0.0));
  color += bloom * 0.22;

  // Sun in view space, then on the screen, for the flare in the reference shots.
  vec3 sunView = mat3(gbufferModelView) * normalize(sunPosition);
  vec4 sunClip = gbufferProjection * vec4(sunView, 1.0);
  vec2 sunUv = sunClip.xy / max(abs(sunClip.w), 0.0001) * 0.5 + 0.5;
  float inFront = sunView.z < 0.0 ? 1.0 : 0.0;
  float onScreen = sunUv.x > -0.2 && sunUv.x < 1.2 && sunUv.y > -0.2 && sunUv.y < 1.2 ? 1.0 : 0.0;
  float visible =
      inFront * onScreen * (1.0 - rainStrength * 0.9) * (isEyeInWater == 0 ? 1.0 : 0.15);
  // The sun is hidden when the pixel under it is nearby terrain.
  float unoccluded = 0.0;
  for (int i = 0; i < 5; i++) {
    float a = float(i) * 2.39996;
    vec2 offset = vec2(cos(a), sin(a)) * 0.006;
    unoccluded += step(0.99995, texture2D(depthtex0, sat2(sunUv + offset)).r);
  }
  visible *= unoccluded / 5.0 * smoothstep(-0.05, 0.05, sunPosition.y);
  vec2 toSun = sunUv - vUv;
  float streak = pow(clamp(1.0 - abs(toSun.x) * viewSize.x * 0.55, 0.0, 1.0), 2.0);
  streak *= pow(clamp(1.0 - abs(toSun.y) * 1.6, 0.0, 1.0), 8.0);
  float beam = pow(clamp(1.0 - abs(toSun.y) * viewSize.y * 0.35, 0.0, 1.0), 2.0);
  beam *= pow(clamp(1.0 - abs(toSun.x) * 2.2, 0.0, 1.0), 6.0);
  vec3 flare = vec3(1.0, 0.92, 0.75) * (streak * 0.09 + beam * 0.14);
  vec2 ghostUv = vec2(0.5) - toSun * 0.45;
  float g1 =
      pow(clamp(1.0 - length((vUv - ghostUv) * vec2(viewSize.x / viewSize.y, 1.0)) * 7.0, 0.0, 1.0),
          3.0);
  vec2 ghostUv2 = vec2(0.5) - toSun * 0.9;
  float g2 = pow(
      clamp(1.0 - length((vUv - ghostUv2) * vec2(viewSize.x / viewSize.y, 1.0)) * 10.0, 0.0, 1.0),
      3.0);
  flare += vec3(0.45, 0.85, 0.55) * g1 * 0.07;
  flare += vec3(0.75, 0.45, 0.95) * g2 * 0.05;
  float dist = length(toSun * vec2(viewSize.x / max(viewSize.y, 1.0), 1.0));
  flare += vec3(1.0, 0.85, 0.55) * pow(clamp(1.0 - dist * 1.4, 0.0, 1.0), 3.0) * 0.06;
  color += flare * visible * sunBrightness;

  color = grade(color);
  gl_FragColor = vec4(displayDither(color, gl_FragCoord.xy), 1.0);
}
