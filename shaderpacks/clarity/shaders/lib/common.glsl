float sat(float x) { return clamp(x, 0.0, 1.0); }

// If the lightmap lookup misses (bad uv, unbound texture) a sunlit face would
// multiply by the unlit corner and the whole shader world goes black. Fall back
// to the sky/torch levels that are already on the vertex.
vec3 bbBlockLight(sampler2D lightmap, vec2 uv, float sky, float block) {
    vec3 light = texture2D(lightmap, uv).rgb;
    float known = max(sky, block);
    if (max(light.r, max(light.g, light.b)) < 0.08 && known > 0.35)
        light = max(light, vec3(0.62, 0.72, 0.84) * sky + vec3(1.0, 0.78, 0.46) * block);
    return light;
}
vec2 sat2(vec2 x) { return clamp(x, vec2(0.0), vec2(1.0)); }
vec3 sat3(vec3 x) { return clamp(x, vec3(0.0), vec3(1.0)); }
float luma(vec3 c) { return dot(c, vec3(0.2126, 0.7152, 0.0722)); }

float linearDepth(float d, float n, float f) {
    float z = d * 2.0 - 1.0;
    return (2.0 * n * f) / max(f + n - z * (f - n), 0.00001);
}

vec3 reconstructView(vec2 uv, float depth, mat4 invProjection) {
    vec4 clip = vec4(uv * 2.0 - 1.0, depth * 2.0 - 1.0, 1.0);
    vec4 view = invProjection * clip;
    return view.xyz / max(abs(view.w), 0.00001);
}

vec3 acesApprox(vec3 x) {
    x = max(x, vec3(0.0));
    return sat3((x * (2.51 * x + 0.03)) / (x * (2.43 * x + 0.59) + 0.14));
}

vec3 saturation(vec3 c, float amount) {
    return mix(vec3(luma(c)), c, amount);
}

float hash12(vec2 p) {
    vec3 p3 = fract(vec3(p.xyx) * 0.1031);
    p3 += dot(p3, p3.yzx + 33.33);
    return fract((p3.x + p3.y) * p3.z);
}

vec3 decodeNormal(vec3 n) {
    vec3 v = n * 2.0 - 1.0;
    float m = dot(v, v);
    return m > 0.04 ? normalize(v) : vec3(0.0, 1.0, 0.0);
}

float stageFromMaterial(vec4 m) {
    return floor(m.r * 255.0 + 0.5);
}

// Edge-aware final resolve. Straight texture coordinates keep block picking exact.
vec3 bbResolveAA(sampler2D source, vec2 uv, vec2 resolution, int antialiasing) {
    vec3 center = texture2D(source, uv).rgb;
    if (antialiasing == 0) return center;
    vec2 px = 1.0 / max(resolution, vec2(1.0));
    vec3 nw = texture2D(source, uv + vec2(-1.0, 1.0) * px).rgb;
    vec3 ne = texture2D(source, uv + vec2( 1.0, 1.0) * px).rgb;
    vec3 sw = texture2D(source, uv + vec2(-1.0,-1.0) * px).rgb;
    vec3 se = texture2D(source, uv + vec2( 1.0,-1.0) * px).rgb;
    vec3 luma = vec3(0.299, 0.587, 0.114);
    float c = dot(center, luma);
    float a = dot(nw, luma), b = dot(ne, luma), d = dot(sw, luma), e = dot(se, luma);
    float lo = min(c, min(min(a,b),min(d,e)));
    float hi = max(c, max(max(a,b),max(d,e)));
    if (hi-lo < max(0.025, hi*0.12)) return center;
    vec2 direction = vec2(-((a+b)-(d+e)), (a+d)-(b+e));
    float reduce = max((a+b+d+e)*0.03125, 0.0078125);
    direction = clamp(direction/(min(abs(direction.x),abs(direction.y))+reduce),vec2(-3.0),vec2(3.0))*px;
    vec3 inner = 0.5*(texture2D(source,uv+direction*(-1.0/6.0)).rgb+texture2D(source,uv+direction*(1.0/6.0)).rgb);
    vec3 outer = inner*0.5 + 0.25*(texture2D(source,uv-direction*0.5).rgb+texture2D(source,uv+direction*0.5).rgb);
    float resolved = dot(outer,luma);
    return mix(center, resolved < lo || resolved > hi ? inner : outer, 0.48);
}

// Stable display-space dither breaks up 8-bit gradient bands without temporal grain.
vec3 bbDisplayDither(vec3 color, vec2 pixel) {
    float a=fract(52.9829189*fract(dot(pixel,vec2(0.06711056,0.00583715))));
    float b=fract(52.9829189*fract(dot(pixel+vec2(37.0,17.0),vec2(0.06711056,0.00583715))));
    return clamp(color+(a+b-1.0)*(0.65/255.0),0.0,1.0);
}

// A luminance-preserving shoulder keeps texture hues and neutral whites intact.
vec3 bbNaturalTone(vec3 c) {
    c=max(c,vec3(0.0));
    float peak=max(max(c.r,c.g),c.b);
    float shoulder=0.82+0.18*(1.0-exp(-max(peak-0.82,0.0)/0.18));
    return c*(peak>0.82?shoulder/max(peak,0.00001):1.0);
}
