// Fixed celestial directions keep recognizable patterns stable while looking
// around. Analytic cores/halos are filtered over the pixel's angular footprint.
float constellationStar(vec3 d, vec3 at, float pixel) {
  float angle2 = max(2.0 * (1.0 - dot(d, at)), 0.0);
  if (angle2 > 0.00012) return 0.0;
  float core = max(pixel * 0.58, 0.00065);
  return exp(-angle2 / (core * core)) * 0.95 + exp(-angle2 / 0.000006) * 0.10;
}
float constellationArc(vec3 d, vec3 a, vec3 b, float pixel) {
  vec3 ab = b - a;
  float t = clamp(dot(d - a, ab) / dot(ab, ab), 0.0, 1.0);
  vec3 nearest = normalize(a + t * ab);
  float angle2 = max(2.0 * (1.0 - dot(d, nearest)), 0.0);
  float width = max(pixel * 0.5, 0.00045);
  return (1.0 - smoothstep(width * width, width * width * 4.0, angle2)) * 0.018;
}
vec3 constellationSky(vec3 d, float time) {
  float pixel = clamp(max(length(dFdx(d)), length(dFdy(d))), 0.00035, 0.004);
  float stars = 0.0, links = 0.0;
  stars += constellationStar(d, vec3(-0.4575256, 0.6691306, -0.5856061), pixel);
  stars += constellationStar(d, vec3(-0.2773937, 0.7313537, -0.6230365), pixel);
  stars += constellationStar(d, vec3(-0.1144089, 0.6819984, -0.7223495), pixel);
  stars += constellationStar(d, vec3(0.0783361, 0.7660444, -0.6379964), pixel);
  stars += constellationStar(d, vec3(0.2714246, 0.7193398, -0.6394364), pixel);
  stars += constellationStar(d, vec3(0.3705143, 0.7986355, -0.4742367), pixel);
  stars += constellationStar(d, vec3(0.4959112, 0.7771460, -0.3874483), pixel);
  links += constellationArc(d, vec3(-0.4575256, 0.6691306, -0.5856061),
                            vec3(-0.2773937, 0.7313537, -0.6230365), pixel);
  links += constellationArc(d, vec3(-0.2773937, 0.7313537, -0.6230365),
                            vec3(-0.1144089, 0.6819984, -0.7223495), pixel);
  links += constellationArc(d, vec3(-0.1144089, 0.6819984, -0.7223495),
                            vec3(0.0783361, 0.7660444, -0.6379964), pixel);
  links += constellationArc(d, vec3(0.0783361, 0.7660444, -0.6379964),
                            vec3(0.2714246, 0.7193398, -0.6394364), pixel);
  links += constellationArc(d, vec3(0.2714246, 0.7193398, -0.6394364),
                            vec3(0.3705143, 0.7986355, -0.4742367), pixel);
  links += constellationArc(d, vec3(0.3705143, 0.7986355, -0.4742367),
                            vec3(0.4959112, 0.7771460, -0.3874483), pixel);
  stars += constellationStar(d, vec3(0.8029648, 0.5000000, 0.3244188), pixel);
  stars += constellationStar(d, vec3(0.7880925, 0.3420201, 0.5117933), pixel);
  stars += constellationStar(d, vec3(0.7242362, 0.1391731, 0.6753612), pixel);
  stars += constellationStar(d, vec3(0.6544609, -0.0697565, 0.7528711), pixel);
  stars += constellationStar(d, vec3(0.5208409, -0.2923717, 0.8020247), pixel);
  stars += constellationStar(d, vec3(0.6984011, -0.1564345, 0.6984011), pixel);
  stars += constellationStar(d, vec3(0.8090170, 0.0000000, 0.5877853), pixel);
  links += constellationArc(d, vec3(0.8029648, 0.5000000, 0.3244188),
                            vec3(0.7880925, 0.3420201, 0.5117933), pixel);
  links += constellationArc(d, vec3(0.7880925, 0.3420201, 0.5117933),
                            vec3(0.7242362, 0.1391731, 0.6753612), pixel);
  links += constellationArc(d, vec3(0.7242362, 0.1391731, 0.6753612),
                            vec3(0.6544609, -0.0697565, 0.7528711), pixel);
  links += constellationArc(d, vec3(0.6544609, -0.0697565, 0.7528711),
                            vec3(0.5208409, -0.2923717, 0.8020247), pixel);
  links += constellationArc(d, vec3(0.5208409, -0.2923717, 0.8020247),
                            vec3(0.6984011, -0.1564345, 0.6984011), pixel);
  links += constellationArc(d, vec3(0.6984011, -0.1564345, 0.6984011),
                            vec3(0.8090170, 0.0000000, 0.5877853), pixel);
  links += constellationArc(d, vec3(0.8090170, 0.0000000, 0.5877853),
                            vec3(0.7880925, 0.3420201, 0.5117933), pixel);
  stars += constellationStar(d, vec3(-0.1301314, 0.7313537, 0.6694681), pixel);
  stars += constellationStar(d, vec3(-0.2055514, 0.8191520, 0.5354797), pixel);
  stars += constellationStar(d, vec3(-0.3545852, 0.7431448, 0.5674549), pixel);
  stars += constellationStar(d, vec3(-0.3783381, 0.8386706, 0.3917805), pixel);
  stars += constellationStar(d, vec3(-0.5502174, 0.7547096, 0.3573154), pixel);
  links += constellationArc(d, vec3(-0.1301314, 0.7313537, 0.6694681),
                            vec3(-0.2055514, 0.8191520, 0.5354797), pixel);
  links += constellationArc(d, vec3(-0.2055514, 0.8191520, 0.5354797),
                            vec3(-0.3545852, 0.7431448, 0.5674549), pixel);
  links += constellationArc(d, vec3(-0.3545852, 0.7431448, 0.5674549),
                            vec3(-0.3783381, 0.8386706, 0.3917805), pixel);
  links += constellationArc(d, vec3(-0.3783381, 0.8386706, 0.3917805),
                            vec3(-0.5502174, 0.7547096, 0.3573154), pixel);
  stars += constellationStar(d, vec3(0.8757580, -0.4383711, -0.2021847), pixel);
  stars += constellationStar(d, vec3(0.9653374, -0.2588190, -0.0337103), pixel);
  stars += constellationStar(d, vec3(0.8996666, -0.4067366, 0.1586355), pixel);
  stars += constellationStar(d, vec3(0.9191581, -0.2079117, 0.3345462), pixel);
  stars += constellationStar(d, vec3(0.8002346, -0.3583679, 0.4808295), pixel);
  links += constellationArc(d, vec3(0.8757580, -0.4383711, -0.2021847),
                            vec3(0.9653374, -0.2588190, -0.0337103), pixel);
  links += constellationArc(d, vec3(0.9653374, -0.2588190, -0.0337103),
                            vec3(0.8996666, -0.4067366, 0.1586355), pixel);
  links += constellationArc(d, vec3(0.8996666, -0.4067366, 0.1586355),
                            vec3(0.9191581, -0.2079117, 0.3345462), pixel);
  links += constellationArc(d, vec3(0.9191581, -0.2079117, 0.3345462),
                            vec3(0.8002346, -0.3583679, 0.4808295), pixel);
  stars += constellationStar(d, vec3(-0.9237195, 0.2923717, 0.2475099), pixel);
  stars += constellationStar(d, vec3(-0.8550793, 0.5150381, 0.0597930), pixel);
  stars += constellationStar(d, vec3(-0.8974877, 0.4226183, -0.1261337), pixel);
  stars += constellationStar(d, vec3(-0.7302783, 0.6293204, -0.2657996), pixel);
  stars += constellationStar(d, vec3(-0.7344312, 0.5000000, -0.4589235), pixel);
  links += constellationArc(d, vec3(-0.9237195, 0.2923717, 0.2475099),
                            vec3(-0.8550793, 0.5150381, 0.0597930), pixel);
  links += constellationArc(d, vec3(-0.8550793, 0.5150381, 0.0597930),
                            vec3(-0.8974877, 0.4226183, -0.1261337), pixel);
  links += constellationArc(d, vec3(-0.8974877, 0.4226183, -0.1261337),
                            vec3(-0.7302783, 0.6293204, -0.2657996), pixel);
  links += constellationArc(d, vec3(-0.7302783, 0.6293204, -0.2657996),
                            vec3(-0.7344312, 0.5000000, -0.4589235), pixel);
  float twinkle = 0.97 + 0.03 * sin(time * 0.6 + d.x * 31.0 + d.z * 19.0);
  return vec3(0.78, 0.87, 1.0) * stars * twinkle + vec3(0.35, 0.48, 0.72) * links;
}
