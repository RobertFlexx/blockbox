// Small gravity waves: independent directions and wavelengths avoid a grid of
// axis-aligned sine bands. xyz = height, d(height)/dx, d(height)/dz.
vec3 waterWave(vec2 p, float time, vec2 direction, float wavelength, float amplitude) {
  float k = 6.2831853 / wavelength;
  vec2 across = vec2(-direction.y, direction.x);
  float bend = dot(p, across) * k * 0.43 + time * 0.19;
  float phase = dot(p, direction) * k - sqrt(9.81 * k) * time * 0.65 + sin(bend) * 0.55;
  // Curved crests avoid ruler-straight reflection bands. Include the phase
  // derivative so the optical normal still matches the displaced surface.
  vec2 gradient = k * (direction + across * cos(bend) * 0.2365);
  return vec3(sin(phase) * amplitude, cos(phase) * amplitude * gradient);
}
vec3 waterWaves(vec2 p, float time) {
  return waterWave(p, time, vec2(0.9397, 0.3420), 12.0, 0.035) +
         waterWave(p, time, vec2(-0.5299, 0.8480), 6.0, 0.020) +
         waterWave(p, time, vec2(0.6508, -0.7593), 2.8, 0.008);
}
// Band-limit optical slopes to at least three pixels per crest. Multiple
// crossing directions per scale keep distant water from forming scan-line bands.
vec3 waterFilteredWave(vec2 p, float time, vec2 dir, float wavelength, float amplitude,
                       float footprint) {
  return waterWave(p, time, dir, wavelength, amplitude) *
         (1.0 - smoothstep(wavelength * 0.12, wavelength * 0.32, footprint));
}
vec3 waterOpticalWaves(vec2 p, float time, float footprint) {
  return waterFilteredWave(p, time, vec2(0.9397, 0.3420), 12.0, 0.035, footprint) +
         waterFilteredWave(p, time, vec2(-0.5299, 0.8480), 6.0, 0.020, footprint) +
         waterFilteredWave(p, time, vec2(0.6508, -0.7593), 2.8, 0.008, footprint) +
         waterFilteredWave(p, time, vec2(-0.8192, -0.5735), 1.35, 0.0035, footprint) +
         waterFilteredWave(p, time, vec2(0.3420, 0.9397), 0.86, 0.0025, footprint) +
         waterFilteredWave(p, time, vec2(0.9659, -0.2588), 0.64, 0.0013, footprint) +
         waterFilteredWave(p, time, vec2(-0.6428, 0.7660), 0.47, 0.0009, footprint) +
         waterFilteredWave(p, time, vec2(0.1219, -0.9925), 0.34, 0.00055, footprint) +
         waterFilteredWave(p, time, vec2(-0.9703, 0.2419), 0.26, 0.00035, footprint);
}
