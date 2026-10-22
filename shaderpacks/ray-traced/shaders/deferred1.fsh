#version 120
#include "lib/common.glsl"
/* RENDERTARGETS: 0 */

uniform sampler2D colortex0;
uniform sampler2D colortex3;
uniform sampler2D colortex4;
uniform sampler2D depthtex0;
uniform sampler2D shadowtex0;
uniform sampler2D rtLighting;
uniform sampler2D rtReflection;
uniform sampler2D rtVolume;
uniform vec2 rtSize;
uniform int rtActive;
uniform sampler2D planarReflection;
uniform int planarActive;
uniform float planarHeight;
uniform vec2 planarSize;
uniform vec2 viewSize;
uniform float near;
uniform float far;
uniform float sunBrightness;
uniform float rainStrength;
uniform float uTime;
uniform float frameTimeCounter;
uniform int isEyeInWater;
uniform float uWaterSurfaceY;
uniform vec3 cameraPosition;
uniform vec3 sunPosition;
uniform mat4 gbufferProjection;
uniform mat4 gbufferProjectionInverse;
uniform mat4 gbufferModelView;
uniform mat4 gbufferModelViewInverse;
uniform mat4 shadowProjection;
uniform mat4 shadowModelView;
varying vec2 vUv;

vec3 rayDir(float depth) {
    vec3 view = reconstructView(vUv, depth, gbufferProjectionInverse);
    return normalize(mat3(gbufferModelViewInverse)*view);
}

float sceneShadow(vec3 world) {
    return shadowCompare(shadowtex0, world, cameraPosition, shadowModelView, shadowProjection, 0.025/(7.0*max(shadowDistance,32.0)));
}

// rgb is the reflection, a is how much of it came from an actual on-screen hit.
vec2 screenHit(vec3 viewPos,vec3 R) {
    vec3 pos=viewPos;
    float stepLen=0.18;
    vec2 hitUv=vec2(-1.0);
    for (int i=0;i<32;i++) {
        vec3 previous=pos;
        pos+=R*stepLen;
        stepLen*=1.16;
        if (pos.z> -0.05) break;
        vec4 clip=gbufferProjection*vec4(pos,1.0);
        if (clip.w<0.05) break;
        vec2 uv=clip.xy/clip.w*0.5+0.5;
        if (uv.x<0.0 || uv.y<0.0 || uv.x>1.0 || uv.y>1.0) break;
        float sd=texture2D(depthtex0,uv).r;
        if (sd>0.99995) continue;
        float sceneZ=linearDepth(sd,near,far);
        float delta= -pos.z-sceneZ;
        if (delta>0.0 && delta<max(0.3,stepLen*1.6)) {
            vec3 lo=previous,hi=pos;
            vec2 refined=uv;
            for (int j=0;j<5;j++) {
                vec3 mid=(lo+hi)*0.5;
                vec4 projected=gbufferProjection*vec4(mid,1.0);
                vec2 sampleUv=projected.xy/projected.w*0.5+0.5;
                float dz= -mid.z-linearDepth(texture2D(depthtex0,sat2(sampleUv)).r,near,far);
                if (dz>0.0) { hi=mid; refined=sampleUv; } else lo=mid;
            }
            float hitDepth=texture2D(depthtex0,sat2(refined)).r;
            float error=abs(-hi.z-linearDepth(hitDepth,near,far));
            if (hitDepth<0.99995 && error<max(0.28,-hi.z*0.012)) {hitUv=refined; break;}
        }
    }
    return hitUv;
}

vec4 screenReflect(vec3 viewPos,vec3 viewN,vec3 rd) {
    vec2 hitUv=screenHit(viewPos+viewN*0.035,reflect(normalize(viewPos),viewN));
    vec3 skyCol = atmosphere(rd, sunPosition, sunBrightness, rainStrength, uTime);
    skyCol = marchClouds(skyCol,cameraPosition,rd,800.0,normalize(sunPosition),sunBrightness,rainStrength,uTime);
    if (hitUv.x < 0.0) return vec4(skyCol, 0.0);
    float hd = texture2D(depthtex0, hitUv).r;
    if (hd > 0.99995) return vec4(skyCol, 0.0);
    vec2 edge = sat2(min(hitUv, 1.0 - hitUv) * 8.0);
    float edgeFade = edge.x * edge.y;
    vec3 hit=texture2D(colortex0,hitUv).rgb;
    if (isEyeInWater!=0) {
        float reflectedDistance=length(reconstructView(hitUv,hd,gbufferProjectionInverse)-viewPos);
        vec3 absorption=exp(-vec3(0.25,0.085,0.04)*min(reflectedDistance,80.0));
        vec3 deep=vec3(0.003,0.035,0.055)*(0.08+0.92*daylightAmount(normalize(sunPosition)));
        hit=hit*absorption+deep*(1.0-absorption);
        skyCol=deep;
    }
    return vec4(mix(skyCol, hit, edgeFade), edgeFade);
}

vec4 planarReflect(vec3 world,vec3 worldN,vec3 reflectedDir) {
    vec3 flatView=normalize(mat3(gbufferModelView)*vec3(0.0,1.0,0.0));
    vec3 waveView=normalize(mat3(gbufferModelView)*worldN);
    vec2 uv=vUv+(waveView-flatView).xy*0.085;
    vec2 edge=smoothstep(vec2(0.0),vec2(0.018),min(uv,1.0-uv));
    float confidence=edge.x*edge.y;
    // Mipmapped, premultiplied coverage keeps cutout canopies stable in the mirror.
    vec2 px=0.65/planarSize;
    float mip=mix(0.0,0.7,smoothstep(15.0,100.0,length(world-cameraPosition)));
    vec4 p=texture2D(planarReflection,sat2(uv),mip)*0.56;
    p+=texture2D(planarReflection,sat2(uv+vec2(px.x,0.0)),mip)*0.11;
    p+=texture2D(planarReflection,sat2(uv-vec2(px.x,0.0)),mip)*0.11;
    p+=texture2D(planarReflection,sat2(uv+vec2(0.0,px.y)),mip)*0.11;
    p+=texture2D(planarReflection,sat2(uv-vec2(0.0,px.y)),mip)*0.11;
    vec3 skyCol=vec3(0.0);
    if(p.a<0.999) {
        skyCol=atmosphere(reflectedDir,sunPosition,sunBrightness,rainStrength,uTime);
        skyCol=marchClouds(skyCol,world,reflectedDir,800.0,normalize(sunPosition),sunBrightness,rainStrength,uTime);
    }
    return vec4(p.rgb+skyCol*(1.0-sat(p.a)),confidence);
}

void main() {
    float depth = texture2D(depthtex0, vUv).r;
    bool sky = depth > 0.99995;
    float storm = sat(rainStrength);
    float day = sat(sunBrightness);
    vec3 sun = normalize(sunPosition);
    vec3 rd = rayDir(sky ? 0.85 : depth);
    vec3 color = sky ? atmosphere(rd, sun, day, storm, uTime) : texture2D(colortex0, vUv).rgb;

    vec4 waterTex = texture2D(colortex3, vUv);
    bool water = waterTex.a > 0.5;
    float surfaceDist = sky ? far : length(reconstructView(vUv,depth,gbufferProjectionInverse));
    float opaqueZ = sky ? far : linearDepth(depth,near,far);
    float opaqueDist = surfaceDist;
    float interfaceY=uWaterSurfaceY;
    // An interface closer than the camera's near plane has no rasterized water
    // fragment. Recover that crossing rather than fogging all the air beyond it.
    float nearSurface=-1.0;
    if(!water && isEyeInWater!=0 && rd.y>0.0001) {
        float t=(uWaterSurfaceY-cameraPosition.y)/rd.y;
        if(t>0.0 && t<surfaceDist) {water=true; nearSurface=t;}
    }
    if (water) {
        float waterDepth = texture2D(colortex4, vUv).r;
        vec3 waterView = reconstructView(vUv, waterDepth, gbufferProjectionInverse);
        if(nearSurface>0.0) {
            waterView=mat3(gbufferModelView)*rd*nearSurface;
            vec4 clip=gbufferProjection*vec4(waterView,1.0);
            waterDepth=clip.z/clip.w*0.5+0.5;
        }
        vec3 waterWorld = worldFromView(waterView, gbufferModelViewInverse, cameraPosition);
        interfaceY=waterWorld.y;
        vec3 viewN=decodeNormal(waterTex.rgb);
        if(nearSurface>0.0) viewN=normalize(mat3(gbufferModelView)*vec3(0.0,-1.0,0.0));
        bool immersed=isEyeInWater!=0;
        vec3 worldN=normalize(mat3(gbufferModelViewInverse)*viewN);
        // Orient the interface normal toward the incident medium, including vertical water.
        if (dot(worldN,rd)>0.0) {worldN= -worldN; viewN= -viewN;}
        float waterZ=linearDepth(waterDepth,near,far);
        float thickness=clamp((opaqueZ-waterZ)*length(waterView)/max(-waterView.z,0.1),0.0,64.0);
        surfaceDist=length(waterView);
        float cosine=sat(dot(viewN,normalize(-waterView)));
        float eta=immersed ? 1.33333 : 0.75000;
        float fres=dielectricFresnel(cosine,eta);
        if(immersed) fres=min(fres,0.18); // rough underside scatters the sharp critical-angle transition
        vec3 reflDir=reflect(rd,worldN);
        bool planar=planarActive!=0 && !immersed && abs(waterWorld.y-planarHeight)<0.25 && worldN.y>0.7;
        vec4 ssr=planar ? planarReflect(waterWorld,worldN,reflDir) : screenReflect(waterView,viewN,reflDir);
        vec3 refl=ssr.rgb;
        if (immersed) {
            vec3 deep=vec3(0.003,0.035,0.055)*(0.08+0.92*daylightAmount(sun));
            // A hazy underside, rather than a bright duplicate of the bottom.
            refl=mix(deep,ssr.rgb*vec3(0.55,0.8,1.0),ssr.a*0.35);
        }
        if (rtActive==1 && ssr.a<0.95) {
            vec4 rt=upsampleRT(rtReflection,vUv,rtSize,colortex4,waterDepth);
            vec3 nativeReflection=rt.rgb;
            if(immersed) nativeReflection=mix(vec3(0.003,0.035,0.055)*(0.08+0.92*daylightAmount(sun)),rt.rgb*vec3(0.55,0.8,1.0),0.35);
            refl=mix(refl,nativeReflection,sat(rt.a)*(1.0-ssr.a));
        }
        vec3 transmitted=vec3(0.0);
        if (fres<0.9999) {
            // Preserve the full field of view underwater. Waves bend the air view
            // smoothly, without remapping geometry into an abrupt Snell window.
            vec3 flatN=abs(worldN.y)>0.7 ? vec3(0.0,sign(worldN.y),0.0) : worldN;
            vec3 transmittedDir=immersed ? normalize(rd+(worldN-flatN)*0.32) : refract(rd,worldN,eta);
            vec3 transmittedView=normalize(mat3(gbufferModelView)*transmittedDir);
            vec2 hit=immersed ? vec2(-1.0) : screenHit(waterView+transmittedView*0.04,transmittedView);
            if (immersed) {
                // Only the path from the eye to this interface is underwater.
                // Airborne canopies retain their cutout alpha and their daylight colour.
                transmitted=atmosphere(transmittedDir,sun,day,storm,uTime);
                transmitted=marchClouds(transmitted,waterWorld,transmittedDir,800.0,sun,day,storm,uTime);
                vec4 airClip=gbufferProjection*vec4(transmittedView,0.0);
                vec2 airUv=sat2(airClip.xy/max(airClip.w,0.001)*0.5+0.5);
                float airDepth=texture2D(depthtex0,airUv).r;
                if(airDepth<0.99995) transmitted=texture2D(colortex0,airUv).rgb;
            } else {
                vec2 fallbackUv=sat2(vUv+viewN.xy*min(thickness,6.0)*0.0025);
                if (texture2D(depthtex0,fallbackUv).r<waterDepth) fallbackUv=vUv;
                vec2 sampleUv=hit.x>=0.0 ? sat2(hit) : fallbackUv;
                if(hit.x>=0.0) {
                    float hitZ=linearDepth(texture2D(depthtex0,sampleUv).r,near,far);
                    // A coarse screen-space ray can step past a thin swimmer and
                    // hit the bottom. Preserve the nearest opaque silhouette at
                    // such depth discontinuities instead of erasing its body.
                    if(abs(hitZ-opaqueZ)>max(0.4,thickness*0.15)) sampleUv=fallbackUv;
                }
                transmitted=texture2D(colortex0,sampleUv).rgb;
                vec3 absorb=exp(-vec3(0.20,0.055,0.026)*thickness);
                transmitted=transmitted*absorb+vec3(0.002,0.026,0.038)*(0.035+0.965*daylightAmount(sun))*(1.0-absorb);
                float ripple=sin(waterWorld.x*1.7+uTime*1.4)*sin(waterWorld.z*1.35-uTime*0.9);
                float sh=sceneShadow(waterWorld);
                if (rtActive==1) sh=sat(texture2D(rtVolume,vUv).a);
                transmitted+=vec3(0.16,0.25,0.22)*pow(sat(ripple),3.0)*sh*daylightAmount(sun)*exp(-thickness*0.45)*(1.0-storm);
            }
        }
        color=mix(transmitted,refl,fres);
        if (!immersed) {
            vec3 toEye=normalize(-waterView);
            vec3 sunView=normalize(mat3(gbufferModelView)*sun);
            float sunUp=smoothstep(-0.04,0.08,sun.y);
            float sh=sceneShadow(waterWorld);
            if (rtActive==1) sh=sat(texture2D(rtVolume,vUv).a);
            // GGX with derivative-based slope variance prevents subpixel glints
            // from turning into fireflies as the camera moves across the lake.
            sh*=cloudTransmittance(waterWorld,sun,uTime,storm);
            vec3 H=normalize(sunView+toEye+vec3(0.0,0.00001,0.0));
            float nv=max(dot(viewN,toEye),0.001),nl=max(dot(viewN,sunView),0.0);
            float nh=sat(dot(viewN,H)),vh=sat(dot(toEye,H));
            float slope=length(fwidth(viewN));
            float alpha=clamp(sqrt(0.000064+slope*slope*0.25),0.008,0.18);
            float a2=alpha*alpha,denom=nh*nh*(a2-1.0)+1.0;
            float D=a2/(3.14159265*denom*denom);
            float gv=2.0*nv/(nv+sqrt(a2+(1.0-a2)*nv*nv));
            float gl=2.0*nl/max(nl+sqrt(a2+(1.0-a2)*nl*nl),0.00001);
            float F=0.02037+0.97963*pow(1.0-vh,5.0);
            float spec=min(D*gv*gl*F/(4.0*nv),32.0)*sunUp*sh*(1.0-storm*0.88);
            color+=sunlightColor(sun)*spec;
            vec3 moonH=normalize(-sunView+toEye+vec3(0.0,0.00001,0.0));
            color+=vec3(0.10,0.23,0.36)*pow(sat(dot(viewN,moonH)),600.0)*nightAmount(sun)*(1.0-storm);

        }
    }

    if (isEyeInWater==0 && (!sky || water)) {
        float dist=min(surfaceDist,600.0);
        float low=goldenHour(sun);
        float density=mix(0.00075,0.0032,low)+storm*0.005;
        float heightFog=exp(-max(cameraPosition.y-45.0,0.0)*0.012);
        float haze=1.0-exp(-dist*density*heightFog);
        vec3 aerial=skyScattering(rd,sun,storm);
        color=mix(color,aerial,sat(haze));

    }

    float cloudFar = sky && !water ? 800.0 : surfaceDist;
    if (isEyeInWater==0) color=marchClouds(color,cameraPosition,rd,cloudFar,sun,day,storm,uTime);

    // Volumetric shafts. Ray tracing writes its own; otherwise march the shadow map.
    vec3 shafts = vec3(0.0);
    if (rtActive==1 && isEyeInWater==0) {
        shafts = upsampleRT(rtVolume, vUv, rtSize, depthtex0, sky ? 0.999 : depth).rgb;
    } else if (isEyeInWater==0) {
        float endT=min(sky && !water ? 120.0 : surfaceDist,120.0);
        float acc=0.0;
        for (int i=0;i<12;i++) {
            float t=endT*(float(i)+0.5)/12.0;
            vec3 p=cameraPosition+rd*t;
            float vis=sceneShadow(p);
            acc+=vis*exp(-max(p.y-46.0,0.0)*0.018);
        }
        float low=goldenHour(sun);
        float night=nightAmount(sun);
        vec3 lightDir=normalize(sun.y> -0.02 ? sun : -sun);
        float heading=pow(sat(dot(rd,lightDir)),3.0);
        vec3 shaftCol=mix(sunlightColor(sun),vec3(0.018,0.065,0.11),night);
        shafts=shaftCol*acc*mix(0.002,0.023,low)*heading*(endT/120.0)*(1.0-storm*0.85);
    }
    color += shafts;

    if (isEyeInWater!=0) {
        float wetDistance=min(water ? surfaceDist : opaqueDist,80.0);
        vec3 absorption=exp(-vec3(0.16,0.055,0.032)*wetDistance);
        vec3 deep=vec3(0.003,0.035,0.055)*(0.08+0.92*daylightAmount(sun));
        color=color*absorption+deep*(1.0-absorption);
        // Single scattering through the wet segment only. Surface focusing and
        // real shadow casters break it into shafts rather than a screen-space veil.
        float endT=min(wetDistance,36.0),scatter=0.0;
        for(int i=0;i<12;i++) {
            float t=endT*(float(i)+0.5)/12.0;
            vec3 p=cameraPosition+rd*t;
            vec2 surface=p.xz+sun.xz*(interfaceY-p.y)/max(sun.y,0.18);
            float focus=0.28+0.72*pow(valueNoise(surface*0.55+vec2(uTime*0.08,-uTime*0.06)),2.0);
            scatter+=sceneShadow(p)*focus*exp(-t*0.035);
        }
        vec3 submergedSun= -refract(-sun,vec3(0.0,1.0,0.0),0.75);
        float phase=0.18+0.82*pow(sat(dot(rd,submergedSun)),4.0);
        color+=vec3(0.035,0.20,0.34)*(scatter/12.0)*(1.0-exp(-endT*0.085))*phase*daylightAmount(sun)*(1.0-storm*0.85);
        color+=waterMotes(cameraPosition,rd,wetDistance,uTime)*(0.04+daylightAmount(sun)*0.96)*exp(-max(interfaceY-cameraPosition.y,0.0)*0.035);
    }
    gl_FragColor = vec4(max(color, vec3(0.0)), 1.0);
}
