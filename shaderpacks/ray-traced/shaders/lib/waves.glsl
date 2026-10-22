// Small gravity waves: independent directions and wavelengths avoid a grid of
// axis-aligned sine bands. xyz = height, d(height)/dx, d(height)/dz.
vec3 waterWave(vec2 p,float time,vec2 direction,float wavelength,float amplitude) {
    float k=6.2831853/wavelength;
    float phase=dot(p,direction)*k-sqrt(9.81*k)*time*0.65;
    return vec3(sin(phase)*amplitude,cos(phase)*amplitude*k*direction);
}
vec3 waterWaves(vec2 p,float time) {
    return waterWave(p,time,vec2(0.9397,0.3420),12.0,0.035)
         + waterWave(p,time,vec2(-0.5299,0.8480),6.0,0.020)
         + waterWave(p,time,vec2(0.6508,-0.7593),2.8,0.008)
         + waterWave(p,time,vec2(-0.8192,-0.5735),0.9,0.0025);
}
