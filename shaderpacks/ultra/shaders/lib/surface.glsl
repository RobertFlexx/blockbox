uniform float uNightDarkness;
uniform int bbPlanarPass;
uniform sampler2D shadowtex0;
uniform mat4 shadowProjection;
uniform mat4 shadowModelView;
uniform vec3 cameraPosition;

vec3 reflectedSurface(vec3 ambient,vec3 direct,vec3 world,vec3 normal,vec3 sun,float time,float rain) {
    float sh=pcssShadow(shadowtex0,world,normal,sun.y> -0.02 ? sun : -sun,cameraPosition,
                        shadowModelView,shadowProjection,0.5);
    sh*=cloudTransmittance(world,normalize(sun),time,sat(rain));
    return ambient+direct*sh;
}
// Sun, moon, skylight and torches in linear radiance. Vertex AO and biome colour
// already enter albedo; directional light is kept separate for deferred shadows.
void surfaceLight(vec3 albedo,vec3 n,vec3 sun,vec3 moon,float sky,float block,
                  float rain,float time,out vec3 ambient,out vec3 direct) {
    float day=daylightAmount(sun),night=nightAmount(sun);
    float open=pow(sat(sky),1.6);
    float hemi=0.42+0.58*sat(n.y*0.5+0.5);
    vec3 skyColor=mix(vec3(0.004,0.012,0.026),vec3(0.14,0.23,0.36),day);
    skyColor=mix(skyColor,vec3(0.27,0.15,0.13),goldenHour(sun)*0.4);
    skyColor*=1.0-rain*0.64;
    // Diffuse ground bounce keeps downward faces warm while open shade stays blue.
    vec3 ground=vec3(0.075,0.062,0.043)*day*(1.0-rain*0.6)*(1.0-max(n.y,0.0));
    float flicker=0.98+0.02*sin(time*7.0)+0.008*sin(time*13.7);
    vec3 torch=vec3(2.4,0.88,0.19)*pow(sat(block),2.2)*flicker;
    skyColor+=vec3(0.14,0.18,0.25)*night*(1.0-step(0.001,uNightDarkness));
    skyColor+=vec3(0.45,0.55,0.72)*sat(bbLightning);
    ambient=albedo*((skyColor*hemi+ground)*open+torch+vec3(0.0008,0.001,0.0015));
    float sunUp=smoothstep(-0.055,0.10,sun.y);
    direct=albedo*sunlightColor(sun)*max(dot(n,sun),0.0)*sunUp*open*(1.0-rain*0.88);
    direct+=albedo*vec3(0.026,0.065,0.12)*max(dot(n,moon),0.0)*night*open*(1.0-rain*0.8);
}
