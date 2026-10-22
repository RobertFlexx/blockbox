float bb_saturate(float x) { return clamp(x, 0.0, 1.0); }

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
vec3 bb_tonemap(vec3 c) { return c / (1.0 + c * 0.12); }

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
