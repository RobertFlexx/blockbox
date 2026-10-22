// Small gravity waves: independent directions and wavelengths avoid a grid of
// axis-aligned sine bands. xyz = height, d(height)/dx, d(height)/dz.
vec3 waterWave(vec2 p,float time,vec2 direction,float wavelength,float amplitude) {
    float k=6.2831853/wavelength;
    vec2 across=vec2(-direction.y,direction.x);
    float bend=dot(p,across)*k*0.43+time*0.19;
    float phase=dot(p,direction)*k-sqrt(9.81*k)*time*0.65+sin(bend)*0.55;
    // Curved crests avoid ruler-straight reflection bands. Include the phase
    // derivative so the optical normal still matches the displaced surface.
    vec2 gradient=k*(direction+across*cos(bend)*0.2365);
    return vec3(sin(phase)*amplitude,cos(phase)*amplitude*gradient);
}
vec3 waterWaves(vec2 p,float time) {
    return waterWave(p,time,vec2(0.9397,0.3420),12.0,0.035)
         + waterWave(p,time,vec2(-0.5299,0.8480),6.0,0.020)
         + waterWave(p,time,vec2(0.6508,-0.7593),2.8,0.008);
}
// Sub-metre waves affect the optical normal, not a mesh sampled every metre.
// Fade unresolved frequencies before they can shimmer in distant reflections.
vec3 waterRipples(vec2 p,float time,float footprint) {
    return waterWave(p,time,vec2(-0.8192,-0.5735),0.90,0.0030)*(1.0-smoothstep(0.12,0.45,footprint))
         + waterWave(p,time,vec2(0.3420,0.9397),0.43,0.0012)*(1.0-smoothstep(0.055,0.21,footprint))
         + waterWave(p,time,vec2(0.9659,-0.2588),0.23,0.00055)*(1.0-smoothstep(0.03,0.11,footprint));
}
