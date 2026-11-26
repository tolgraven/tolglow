(ns tolglow.video.shader-examples
  "Example shader cues (#673): a 2D plasma for a projector, the same plasma
  on a 200 × 1 strip target, and a ray-marched 3D scene whose camera and
  viewport are cue parameters. Studio offers them as starting points; the
  measurement harness and tests use them as fixed workloads. Shared with
  Studio, which publishes them through the ordinary definition command.")

(def plasma-glsl
  "void main() {
  vec2 uv = (gl_FragCoord.xy - u_origin) / u_resolution;
  float t = u_beats * u_speed * 0.5;
  float v = sin(uv.x * 10.0 * u_scale + t)
          + sin((uv.y * 10.0 * u_scale + t) * 0.5)
          + sin((uv.x * 10.0 * u_scale + uv.y * 10.0 * u_scale + t) * 0.5);
  vec2 c = uv * 10.0 * u_scale - 5.0;
  v += sin(sqrt(c.x * c.x + c.y * c.y + 1.0) + t);
  vec3 col = 0.5 + 0.5 * cos(6.28318 * (v * 0.25 + u_hue + vec3(0.0, 0.33, 0.67)));
  fragColor = vec4(col * u_level, 1.0);
}")

(def scene-glsl
  "float sdBox(vec3 p, vec3 b) { vec3 q = abs(p) - b; return length(max(q, 0.0)) + min(max(q.x, max(q.y, q.z)), 0.0); }
float map(vec3 p) {
  float floorPlane = p.y + 1.0;
  vec3 q = p; q.xz = mod(q.xz + 2.0, 4.0) - 2.0;
  float pillars = sdBox(q, vec3(0.35, 1.0 + 0.5 * sin(u_beats * 1.5707963), 0.35));
  float ball = length(p - vec3(0.0, 0.2 + 0.4 * sin(u_beats * 3.14159), 0.0)) - 0.6;
  return min(floorPlane, min(pillars, ball));
}
vec3 normalAt(vec3 p) {
  vec2 e = vec2(0.001, 0.0);
  return normalize(vec3(map(p + e.xyy) - map(p - e.xyy), map(p + e.yxy) - map(p - e.yxy), map(p + e.yyx) - map(p - e.yyx)));
}
void main() {
  vec2 uv = ((gl_FragCoord.xy - u_origin) / u_resolution) * 2.0 - 1.0;
  uv.x *= u_resolution.x / u_resolution.y;
  vec3 eye = vec3(sin(u_orbit) * u_distance, u_height, cos(u_orbit) * u_distance);
  vec3 forward = normalize(-eye);
  vec3 right = normalize(cross(forward, vec3(0.0, 1.0, 0.0)));
  vec3 up = cross(right, forward);
  vec3 dir = normalize(forward / tan(radians(u_fov) * 0.5) + uv.x * right + uv.y * up);
  float t = 0.0; vec3 col = vec3(0.0);
  for (int i = 0; i < 96; i++) {
    vec3 p = eye + dir * t;
    float d = map(p);
    if (d < 0.001) {
      vec3 n = normalAt(p);
      float light = max(dot(n, normalize(vec3(0.6, 0.8, 0.3))), 0.0);
      col = u_tint * (0.15 + 0.85 * light) * exp(-0.04 * t * t);
      break;
    }
    t += d;
    if (t > 40.0) break;
  }
  fragColor = vec4(col * u_level, 1.0);
}")

(def ^:private level {:type :number :default 1.0 :bounds [0 1]})

(defn plasma
  "A 2D plasma; :size defaults to a 640 × 360 projector feed."
  ([] (plasma {}))
  ([{:keys [id size fps] :or {id :shader-plasma size [640 360] fps 30}}]
   {:id id :version 1 :name "Shader plasma" :capabilities [:emission]
    :params {:level level
             :speed {:type :number :default 1.0 :bounds [0 4]}
             :scale {:type :number :default 1.0 :bounds [0.1 4]}
             :hue {:type :number :default 0.0 :bounds [0 1]}}
    :shader {:glsl plasma-glsl :size size :fps fps :kind :2d
             :uniforms {:u_level {:type :float :value {:op :param :key :level}}
                        :u_speed {:type :float :value {:op :param :key :speed}}
                        :u_scale {:type :float :value {:op :param :key :scale}}
                        :u_hue {:type :float :value {:op :param :key :hue}}}}}))

(defn plasma-strip
  "The plasma rendered at 200 × 1 and sampled pixel-for-pixel onto the strip
  that `strip-mapping` names."
  ([] (plasma-strip {}))
  ([{:keys [id mapping fps] :or {id :shader-plasma-strip mapping :shader-strip fps 30}}]
   (assoc (plasma {:id id :size [200 1] :fps fps})
          :name "Shader plasma strip"
          :outputs {:color {:op :video :mapping mapping}})))

(defn strip-mapping
  "A mapping that samples a 200 × 1 feed pixel-for-pixel onto a 200-pixel layout."
  [{:keys [id source layout] :or {id :shader-strip}}]
  {:id id :source source
   :strips [{:id :strip :layout layout :points [[0.0 0.5] [1.0 0.5]] :height 1.0 :filter :nearest}]})

(defn scene
  "A ray-marched 3D scene. The camera orbit, height, distance and field of
  view are parameters, and :viewport places the picture inside the target."
  ([] (scene {}))
  ([{:keys [id size viewport fps] :or {id :shader-scene size [640 360] fps 30}}]
   {:id id :version 1 :name "Shader 3D scene" :capabilities [:emission]
    :params {:level level
             :orbit {:type :number :default 0.6 :bounds [-3.2 3.2]}
             :height {:type :number :default 1.2 :bounds [-0.5 5]}
             :distance {:type :number :default 6.0 :bounds [2 20]}
             :fov {:type :number :default 60.0 :bounds [20 120]}
             :tint {:type :color :default [0.4 0.7 1.0]}}
    :shader (cond-> {:glsl scene-glsl :size size :fps fps :kind :3d
                     :uniforms {:u_level {:type :float :value {:op :param :key :level}}
                                :u_orbit {:type :float :value {:op :param :key :orbit}}
                                :u_height {:type :float :value {:op :param :key :height}}
                                :u_distance {:type :float :value {:op :param :key :distance}}
                                :u_fov {:type :float :value {:op :param :key :fov}}
                                :u_tint {:type :vec3 :value {:op :param :key :tint}}}}
              viewport (assoc :viewport viewport))}))

(defn templates
  "Starting points Studio offers, by id."
  []
  {:plasma (plasma) :scene (scene) :plasma-strip (plasma-strip)})
