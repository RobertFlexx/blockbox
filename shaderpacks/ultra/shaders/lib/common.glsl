// Shared shading for the Ultra and Ray Traced packs.
// GLSL 1.20: no textureSize, no bitwise ops, no inverse().

uniform float bbLightning;

float sat(float x) { return clamp(x, 0.0, 1.0); }
vec2 sat2(vec2 x) { return clamp(x, vec2(0.0), vec2(1.0)); }
vec3 sat3(vec3 x) { return clamp(x, vec3(0.0), vec3(1.0)); }
float luma(vec3 c) { return dot(c, vec3(0.2126, 0.7152, 0.0722)); }

vec3 bbBlockLight(sampler2D lightmap, vec2 uv, float sky, float block) {
    vec3 light = texture2D(lightmap, uv).rgb;
    float known = max(sky, block);
    if (max(light.r, max(light.g, light.b)) < 0.08 && known > 0.35)
        light = max(light, vec3(0.62, 0.72, 0.84) * sky + vec3(1.0, 0.78, 0.46) * block);
    return light;
}

float linearDepth(float d, float n, float f) {
    float z = d * 2.0 - 1.0;
    return (2.0 * n * f) / max(f + n - z * (f - n), 0.00001);
}

vec3 reconstructView(vec2 uv, float depth, mat4 invProjection) {
    vec4 clip = vec4(uv * 2.0 - 1.0, depth * 2.0 - 1.0, 1.0);
    vec4 view = invProjection * clip;
    return view.xyz / max(abs(view.w), 0.00001);
}

// Modelview maps (world.x - cam.x, world.y, world.z - cam.z). Y stays absolute.
vec3 worldFromView(vec3 viewPos, mat4 invModelView, vec3 camera) {
    vec4 rel = invModelView * vec4(viewPos, 1.0);
    return vec3(rel.x + camera.x, rel.y, rel.z + camera.z);
}

vec3 decodeNormal(vec3 n) {
    vec3 v = n * 2.0 - 1.0;
    float m = dot(v, v);
    return m > 0.04 ? normalize(v) : vec3(0.0, 1.0, 0.0);
}

float hash12(vec2 p) {
    vec3 p3 = fract(vec3(p.xyx) * 0.1031);
    p3 += dot(p3, p3.yzx + 33.33);
    return fract((p3.x + p3.y) * p3.z);
}

float hash13(vec3 p) {
    p = fract(p * 0.1031);
    p += dot(p, p.zyx + 31.32);
    return fract((p.x + p.y) * p.z);
}

float valueNoise(vec2 p) {
    vec2 i = floor(p);
    vec2 f = fract(p);
    f = f * f * (3.0 - 2.0 * f);
    float a = hash12(i);
    float b = hash12(i + vec2(1.0, 0.0));
    float c = hash12(i + vec2(0.0, 1.0));
    float d = hash12(i + vec2(1.0, 1.0));
    return mix(mix(a, b, f.x), mix(c, d, f.x), f.y);
}

float valueNoise3(vec3 p) {
    vec3 i = floor(p);
    vec3 f = fract(p);
    f = f * f * (3.0 - 2.0 * f);
    float n000 = hash13(i);
    float n100 = hash13(i + vec3(1.0, 0.0, 0.0));
    float n010 = hash13(i + vec3(0.0, 1.0, 0.0));
    float n110 = hash13(i + vec3(1.0, 1.0, 0.0));
    float n001 = hash13(i + vec3(0.0, 0.0, 1.0));
    float n101 = hash13(i + vec3(1.0, 0.0, 1.0));
    float n011 = hash13(i + vec3(0.0, 1.0, 1.0));
    float n111 = hash13(i + vec3(1.0, 1.0, 1.0));
    float nx00 = mix(n000, n100, f.x);
    float nx10 = mix(n010, n110, f.x);
    float nx01 = mix(n001, n101, f.x);
    float nx11 = mix(n011, n111, f.x);
    return mix(mix(nx00, nx10, f.y), mix(nx01, nx11, f.y), f.z);
}

float fbm(vec3 p) {
    float a = 0.5;
    float s = 0.0;
    s += a * valueNoise3(p); p = p * 2.03 + vec3(19.1, 7.7, 3.3); a *= 0.5;
    s += a * valueNoise3(p); p = p * 2.02 + vec3(4.2, 11.4, 8.1); a *= 0.5;
    s += a * valueNoise3(p);
    return s;
}

// Shadow map is camera-centered. Vertices arrive as (world - camera.xz, world.y - camera.y).
float shadowCompare(sampler2D shadowMap, vec3 world, vec3 camera, mat4 shadowModel, mat4 shadowProj, float bias) {
    vec3 rel = vec3(world.x - camera.x, world.y, world.z - camera.z);
    vec4 sc = shadowProj * shadowModel * vec4(rel, 1.0);
    vec3 ndc = sc.xyz / max(abs(sc.w), 0.00001);
    vec2 uv = ndc.xy * 0.5 + 0.5;
    if (uv.x < 0.0 || uv.y < 0.0 || uv.x > 1.0 || uv.y > 1.0) return 1.0;
    float z = ndc.z * 0.5 + 0.5;
    if (z < 0.0 || z > 1.0) return 1.0;
    float occluder = texture2D(shadowMap, uv).r;
    return z - bias <= occluder ? 1.0 : 0.0;
}

float pcssShadow(sampler2D shadowMap, vec3 world, vec3 normal, vec3 sun, vec3 camera, mat4 shadowModel, mat4 shadowProj, float dither) {
    float ndotl = sat(dot(normal, normalize(sun)));
    float bias=0.00012+(1.0-ndotl)*0.0004;
    vec2 rot = vec2(cos(dither * 6.28318), sin(dither * 6.28318));
    vec2 perp = vec2(-rot.y, rot.x);
    // A short blocker search, then a wider penumbra. Reads as a large soft sun.
    float blocker = 0.0;
    float blockers = 0.0;
    vec2 texel = vec2(1.0 / 2048.0);
    for (int i = 0; i < 8; i++) {
        float a = float(i) * 2.39996 + dither * 6.28318;
        vec2 o = vec2(cos(a), sin(a)) * (1.5 + float(i) * 0.55) * texel;
        vec3 rel = vec3(world.x - camera.x, world.y, world.z - camera.z);
        vec4 sc = shadowProj * shadowModel * vec4(rel, 1.0);
        vec3 ndc = sc.xyz / max(abs(sc.w), 0.00001);
        vec2 uv = ndc.xy * 0.5 + 0.5 + o;
        float z = ndc.z * 0.5 + 0.5;
        if (uv.x > 0.0 && uv.y > 0.0 && uv.x < 1.0 && uv.y < 1.0) {
            float d = texture2D(shadowMap, uv).r;
            if (d < z - bias) { blocker += d; blockers += 1.0; }
        }
    }
    float spread = 2.2;
    if (blockers > 0.0) {
        float avg = blocker / blockers;
        vec3 rel = vec3(world.x - camera.x, world.y, world.z - camera.z);
        vec4 sc = shadowProj * shadowModel * vec4(rel, 1.0);
        float receiver = (sc.z / max(abs(sc.w), 0.00001)) * 0.5 + 0.5;
        spread = clamp((receiver - avg) / max(avg, 0.05) * 18.0, 1.4, 7.5);
    }
    float sum = 0.0;
    for (int i = 0; i < 12; i++) {
        float a = float(i) * 2.39996 + dither * 6.28318;
        float r = sqrt((float(i) + 0.5) / 12.0) * spread;
        vec2 o = vec2(cos(a), sin(a)) * r * texel * 3.2;
        vec3 rel = vec3(world.x - camera.x, world.y, world.z - camera.z);
        vec4 sc = shadowProj * shadowModel * vec4(rel, 1.0);
        vec3 ndc = sc.xyz / max(abs(sc.w), 0.00001);
        vec2 uv = ndc.xy * 0.5 + 0.5 + vec2(dot(o, rot), dot(o, perp));
        float z = ndc.z * 0.5 + 0.5;
        float lit = 1.0;
        if (uv.x > 0.0 && uv.y > 0.0 && uv.x < 1.0 && uv.y < 1.0 && z > 0.0 && z < 1.0)
            lit = z - bias <= texture2D(shadowMap, uv).r ? 1.0 : 0.0;
        sum += lit;
    }
    return sum / 12.0;
}

vec4 rtTap(sampler2D tex, vec2 uv) { return texture2D(tex, uv); }

// Depth-aware upsample from the half-resolution ray tracing targets.
vec4 upsampleRT(sampler2D tex, vec2 uv, vec2 rtSize, sampler2D depthTex, float centerDepth) {
    vec2 texel = 1.0 / max(rtSize, vec2(1.0));
    vec4 sum = vec4(0.0);
    float wsum = 0.0;
    for (int y = 0; y < 2; y++) {
        for (int x = 0; x < 2; x++) {
            vec2 o = (vec2(float(x), float(y)) - 0.5) * texel;
            vec2 suv = sat2(uv + o);
            float d = texture2D(depthTex, suv).r;
            float w = exp(-abs(d - centerDepth) / max(1.0-centerDepth,0.00005) * 50.0);
            sum += rtTap(tex, suv) * w;
            wsum += w;
        }
    }
    if (wsum < 0.0001) return rtTap(tex, uv);
    return sum / wsum;
}

// All pack lighting is linear HDR. Conversion to display space happens once in final.
float daylightAmount(vec3 sun) { return smoothstep(-0.15, 0.12, sun.y); }
float nightAmount(vec3 sun) { return 1.0-smoothstep(-0.16, 0.01, sun.y); }
float goldenHour(vec3 sun) { return exp(-pow((sun.y-0.035)*4.4,2.0)); }
vec3 sunlightColor(vec3 sun) {
    return mix(vec3(1.65,1.68,1.72),vec3(1.65,0.59,0.18),goldenHour(sun));
}
vec3 linearAlbedo(vec3 texel,vec3 tint) {
    return pow(max(texel*min(tint,vec3(1.0)),vec3(0.0)),vec3(2.2));
}

vec3 sunDiscColor(vec3 rd, vec3 sun, float daylight, float storm) {
    float d=sat(dot(rd,normalize(sun)));
    float disc=smoothstep(0.99935,0.99965,d);
    float glow=pow(d,24.0)*0.12+pow(d,256.0)*0.35+pow(d,1024.0)*0.6;
    return sunlightColor(sun)*(disc*8.0+glow)*smoothstep(-0.055,0.02,sun.y)*(1.0-storm*0.96);
}

// Continuous blue-hour, daytime and sunset scattering, shared by fog and reflections.
// The sun disk is separate so aerial perspective never paints a disk over terrain.
vec3 skyScattering(vec3 dir,vec3 sun,float storm) {
    float day=daylightAmount(sun);
    float gold=goldenHour(sun)*(1.0-storm);
    float elev=sat(dir.y);
    float horizonBand=pow(1.0-elev,3.0);
    float towardSun=pow(sat(dot(dir,normalize(sun))),3.0);
    vec3 zenith=mix(vec3(0.0015,0.006,0.018),vec3(0.075,0.23,0.57),day);
    zenith=mix(zenith,vec3(0.16,0.12,0.28),gold*0.55);
    vec3 horizon=mix(vec3(0.012,0.037,0.068),vec3(0.42,0.65,0.86),day);
    vec3 sunset=mix(vec3(0.48,0.24,0.30),vec3(1.15,0.50,0.115),towardSun);
    horizon=mix(horizon,sunset,gold*0.92);
    vec3 sky=mix(zenith,horizon,horizonBand);
    sky+=vec3(0.9,0.38,0.085)*towardSun*gold*pow(1.0-elev,1.6)*0.36;
    vec3 stormSky=mix(vec3(0.003,0.008,0.016),vec3(0.085,0.115,0.15),day);
    stormSky*=mix(0.65,1.25,horizonBand);
    sky=mix(sky,stormSky,storm*0.93);
    vec3 moon=-normalize(sun);
    float md=sat(dot(dir,moon));
    sky+=vec3(0.008,0.026,0.046)*(pow(md,8.0)+pow(md,96.0)*1.8)*nightAmount(sun)*(1.0-storm);
    return mix(sky,horizon*vec3(0.22,0.27,0.32),sat(-dir.y*3.0));
}

vec3 atmosphere(vec3 rd,vec3 sun,float daylight,float storm,float time) {
    vec3 dir=normalize(rd);
    vec3 sky=skyScattering(dir,sun,storm)+sunDiscColor(dir,sun,daylight,storm);
    sky+=vec3(0.32,0.43,0.62)*sat(bbLightning);
    float night=nightAmount(sun)*(1.0-storm);
    if (night>0.001) {
        vec2 angular=vec2(atan(dir.z,dir.x)/6.2831853+0.5,asin(clamp(dir.y,-1.0,1.0))/3.1415927+0.5);
        vec2 field=angular*vec2(760.0,380.0);
        vec2 cell=floor(field);
        float seed=hash12(cell);
        vec2 center=vec2(hash12(cell+17.1),hash12(cell+39.7))*0.6+0.2;
        float star=pow(sat(1.0-length(fract(field)-center)*3.5),2.0)*step(0.986,seed);
        vec3 starColor=mix(vec3(0.52,0.73,1.0),vec3(1.0,0.82,0.65),hash12(cell+5.3));
        sky+=starColor*star*night*smoothstep(-0.02,0.25,dir.y)*mix(0.3,1.4,hash12(cell+73.0));
        float md=sat(dot(dir,-normalize(sun)));
        sky+=vec3(0.65,0.85,1.0)*smoothstep(0.99945,0.9997,md)*night*3.0;
    }
    return sky;
}

// Cumulus billows with a soft eroded edge. Only occupied cells sample 3D detail.
float cloudDensity(vec3 p,float time,float cover) {
    vec2 flow=(p.xz+vec2(time*0.65,-time*0.22))/156.0;
    vec2 grid=floor(flow-0.5);
    float field=-1.0;
    for (int x=0;x<2;x++) for (int z=0;z<2;z++) {
        vec2 cell=grid+vec2(float(x),float(z));
        float seed=hash12(cell+vec2(13.7,29.1));
        if (seed>mix(0.69,1.0,sat((cover-0.55)/0.4))) continue;
        float r=45.0+seed*25.0+sat(cover-0.55)*35.0;
        vec2 center=cell+0.5+vec2(fract(seed*7.37)-0.5,fract(seed*13.13)-0.5)*0.16;
        vec3 q=vec3((flow.x-center.x)*156.0,p.y-171.0-seed*8.0,(flow.y-center.y)*156.0);
        vec3 main=q/vec3(r,r*0.45,r*0.8);
        vec3 a=(q-vec3(-r*0.46,r*0.24,r*0.12))/vec3(r*0.64,r*0.52,r*0.62);
        vec3 b=(q-vec3(r*0.37,r*0.28,-r*0.10))/vec3(r*0.65,r*0.57,r*0.61);
        float d=max(1.0-dot(main,main),max(1.0-dot(a,a),1.0-dot(b,b)));
        field=max(field,d);
    }
    if (field< -0.22) return 0.0;
    vec3 detail=p+vec3(time*0.65,0.0,-time*0.22);
    field+=(valueNoise3(detail*0.11)-0.5)*0.38;
    field+=(valueNoise3(detail*0.27)-0.5)*0.13;
    return smoothstep(-0.035,0.58,field)*smoothstep(144.0,154.0,p.y);
}

vec3 marchClouds(vec3 color,vec3 ro,vec3 rd,float tmax,vec3 sun,float daylight,float storm,float time) {
    if (abs(rd.y)<0.0001) return color;
    float t0=(144.0-ro.y)/rd.y,t1=(242.0-ro.y)/rd.y;
    float ta=max(min(t0,t1),0.0),tb=min(max(t0,t1),tmax);
    if (tb<=ta || tmax<8.0) return color;
    const int STEPS=48;
    float dt=(tb-ta)/float(STEPS),transmit=1.0;
    vec3 scatter=vec3(0.0);
    float day=daylightAmount(sun),night=nightAmount(sun);
    vec3 lightDir=normalize(sun.y> -0.02 ? sun : -sun);
    vec3 direct=mix(sunlightColor(sun)*0.75,vec3(0.026,0.065,0.105),night)*(1.0-storm*0.8);
    vec3 ambient=mix(vec3(0.005,0.016,0.032),vec3(0.18,0.25,0.37),day);
    ambient=mix(ambient,ambient*0.42,storm)+vec3(0.3,0.4,0.55)*sat(bbLightning);
    for (int i=0;i<STEPS;i++) {
        float jitter=0.35+hash12(gl_FragCoord.xy)*0.3;
        float t=ta+(float(i)+jitter)*dt;
        vec3 p=ro+rd*t;
        float d=cloudDensity(p,time,0.55+storm*0.4);
        if (d>0.002) {
            float light=0.0;
            for (int s=0;s<3;s++)
                light+=cloudDensity(p+lightDir*(6.0+float(s)*10.0),time,0.55+storm*0.4);
            float beer=exp(-light*1.15);
            float silver=pow(sat(dot(rd,lightDir)),12.0)*0.7;
            float heightLight=smoothstep(150.0,205.0,p.y);
            vec3 lit=direct*(0.12+beer*(0.95+silver))+ambient*mix(0.6,1.25,heightLight);
            float absorb=exp(-d*dt*0.075);
            scatter+=transmit*(1.0-absorb)*lit;
            transmit*=absorb;
            if (transmit<0.025) break;
        }
    }
    return color*transmit+scatter;
}

// Dielectric Fresnel for either side of an air/water interface. Above the
// critical angle water-to-air transmission becomes total internal reflection.
// Sparse world-anchored suspended particles, bounded by the water interface or
// the opaque scene. Soft analytic discs avoid a noisy full-screen sparkle layer.
vec3 waterMotes(vec3 ro,vec3 rd,float distance,float time) {
    float endT=min(distance,28.0),brightness=0.0;
    vec3 glow=vec3(0.0);
    for(int i=0;i<20;i++) {
        float t=endT*(float(i)+0.5)/20.0;
        vec3 cell=floor((ro+rd*t)/2.2);
        float seed=hash13(cell+13.7);
        vec3 jitter=vec3(hash13(cell+3.1),hash13(cell+7.9),hash13(cell+17.3));
        vec3 point=(cell+0.15+jitter*0.7)*2.2;
        point.y+=sin(time*0.35+seed*19.0)*0.09;
        float along=dot(point-ro,rd);
        if(along<=0.25 || along>=distance) continue;
        float radius=0.012+along*0.00065;
        float separation=length(point-ro-rd*along);
        float mote=(1.0-smoothstep(radius*0.1,radius,separation))*(0.25+seed*0.75);
        if(mote>brightness) {
            brightness=mote;
            glow=vec3(0.20,0.48,0.75)*mote*exp(-vec3(0.14,0.05,0.025)*along);
        }
    }
    return glow;
}

float dielectricFresnel(float cosine,float eta) {
    float c=clamp(cosine,0.0001,1.0);
    float sinT2=eta*eta*(1.0-c*c);
    if (sinT2>=1.0) return 1.0;
    float ct=sqrt(max(1.0-sinT2,0.0));
    float rs=(eta*c-ct)/max(eta*c+ct,0.0001);
    float rp=(c-eta*ct)/max(c+eta*ct,0.0001);
    return (rs*rs+rp*rp)*0.5;
}

float fresnelSchlick(float ndotv) {
    return mix(0.025,1.0,pow(sat(1.0-ndotv),5.0));
}

vec3 grade(vec3 c) {
    c=max(c,vec3(0.0))*0.9;
    vec3 mapped=sat3((c*(2.51*c+0.03))/(c*(2.43*c+0.59)+0.14));
    mapped=mix(vec3(luma(mapped)),mapped,1.045);
    return pow(sat3(mapped),vec3(1.0/2.2));
}

vec3 displayDither(vec3 color, vec2 pixel) {
    float a = fract(52.9829189 * fract(dot(pixel, vec2(0.06711056, 0.00583715))));
    float b = fract(52.9829189 * fract(dot(pixel + vec2(37.0, 17.0), vec2(0.06711056, 0.00583715))));
    return sat3(color + (a + b - 1.0) * (0.65 / 255.0));
}
