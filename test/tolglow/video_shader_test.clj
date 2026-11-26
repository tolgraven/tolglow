(ns tolglow.video-shader-test
  "Shader cues (#673): the :shader effect block, the independently scheduled
  producer and its feed references, and a 200 × 1 render target sampled
  pixel-for-pixel onto a strip through a real show runtime. The GPU is the
  external boundary: these tests inject a deterministic renderer that runs a
  Clojure function per known source and refuses any other source with a
  compile log, as the driver does. Afterglow's JOGL renderer has its own
  native tests, and `tolglow.bench-shader-cues` measures it."
  (:require [clojure.test :refer [deftest is testing]]
            [tolglow.compiler :as compiler]
            [tolglow.data :as data]
            [tolglow.demo :as demo]
            [tolglow.persistence :as persistence]
            [tolglow.rig.pixel-layout :as layout]
            [tolglow.show :as show]
            [tolglow.studio :as studio]
            [tolglow.transit :as transit]
            [tolglow.video.hub :as video-hub]
            [tolglow.video.shader :as shader]
            [tolglow.video.shader-producer :as producer]
            [tolglow.video.source :as source])
  (:import [java.io ByteArrayInputStream]
           [java.util.concurrent Callable Executors ExecutorService]))

;; A deterministic renderer -------------------------------------------------

(defn- bgra [w h color-at]
  (let [px (byte-array (* 4 w h))]
    (dotimes [y h]
      (dotimes [x w]
        (let [[r g b] (color-at x y) i (* 4 (+ x (* y w)))]
          (aset px i (unchecked-byte b)) (aset px (+ i 1) (unchecked-byte g))
          (aset px (+ i 2) (unchecked-byte r)) (aset px (+ i 3) (unchecked-byte 255)))))
    px))

(defn test-renderer
  "programs: GLSL source -> (fn [uniforms x y w h] [r g b]). :render-ms
  delays each frame (a slow GPU)."
  [programs & {:keys [render-ms]}]
  (let [^ExecutorService worker (Executors/newSingleThreadExecutor)
        log (atom {:compiles [] :renders [] :released []})
        task (fn [f] (.submit worker ^Callable f))]
    (with-meta
      (reify producer/ShaderRenderer
        (compile-shader [_ s]
          (task #(if (get programs (:source s))
                   (do (swap! log update :compiles conj (:source s)) {:compiled? true})
                   (throw (ex-info "GLSL compilation failed"
                                   {:type :afterglow.render.gpu.native-context/invalid-program :stage :compile
                                    :log "ERROR: 0:1: Use of undeclared identifier 'oops'\n"})))))
        (render-frame [_ s {:keys [width height uniforms]}]
          (task #(let [f (or (get programs (:source s))
                             (throw (ex-info "GLSL compilation failed" {:stage :compile :log "not resident"})))]
                   (when render-ms (Thread/sleep (long render-ms)))
                   (swap! log update :renders conj [(:source s) uniforms])
                   {:pixels (bgra width height (fn [x y] (f uniforms x y width height)))
                    :width width :height height :draw-ms 0.1 :readback-ms 0.05})))
        (release-shader [_ s] (task #(do (swap! log update :released conj (:source s)) true)))
        (describe-renderer [_] {:backend :test :renders (count (:renders @log))})
        (close-renderer [_] (.shutdownNow worker)))
      {:log log})))

(defn- log-of [renderer] @(:log (meta renderer)))

(defn- eventually [pred & [timeout-ms]]
  (let [deadline (+ (System/currentTimeMillis) (or timeout-ms 3000))]
    (loop []
      (or (pred)
          (when (< (System/currentTimeMillis) deadline)
            (Thread/sleep 5)
            (recur))))))

;; Definitions --------------------------------------------------------------

(def index-glsl "void main() { fragColor = vec4(floor(gl_FragCoord.x) / 255.0, u_level, 0.0, 1.0); }")

(def gradient-glsl "void main() { fragColor = vec4(gl_FragCoord.xy / u_resolution, u_level, 1.0); }")

(def programs
  {index-glsl (fn [{:strs [u_level]} x _ _ _] [x (Math/round (* 255.0 (double u_level))) 0])
   gradient-glsl (fn [{:strs [u_level]} x y w h] [(quot (* 255 x) (max 1 (dec w))) (quot (* 255 y) (max 1 (dec h)))
                                                  (Math/round (* 255.0 (double u_level)))])})

(def strip-effect
  {:id :index-strip :version 1
   :params {:level {:type :number :default 0.5 :bounds [0 1]}}
   :shader {:glsl index-glsl :size [200 1] :fps 30
            :uniforms {:u_level {:type :float :value {:op :param :key :level}}}}
   :outputs {:color {:op :video :mapping :strip-200}}})

(deftest shader-blocks-validate-with-paths-and-type-their-uniforms
  (is (= [] (compiler/definition-errors :effects strip-effect)))
  (is (= [] (compiler/definition-errors :effects (dissoc strip-effect :outputs)))
      "A shader cue may drive only a direct output")
  (doseq [[path definition]
          [[[:shader :glsl] (assoc-in strip-effect [:shader :glsl] "")]
           [[:shader :size] (assoc-in strip-effect [:shader :size] [0 1])]
           [[:shader :size] (assoc-in strip-effect [:shader :size] [4096 4096])]
           [[:shader :viewport] (assoc-in strip-effect [:shader :viewport] [0 0 300 1])]
           [[:shader :fps] (assoc-in strip-effect [:shader :fps] 120)]
           [[:shader :uniforms :u_time] (assoc-in strip-effect [:shader :uniforms :u_time] {:type :float :value 1})]
           [[:shader :uniforms :u_resolution] (assoc-in strip-effect [:shader :uniforms :u_resolution] {:type :vec2 :value [1 1]})]
           [[:shader :uniforms :u_level :value] (assoc-in strip-effect [:shader :uniforms :u_level :type] :vec3)]
           [[:shader :uniforms :u_level :value :key] (assoc-in strip-effect [:shader :uniforms :u_level :value :key] :gone)]
           [[:shader :uniforms :u_level :value] (assoc-in strip-effect [:shader :uniforms :u_level :value] {:op :position :axis :x})]
           [[:shader] (assoc-in strip-effect [:shader :colour] 1)]]]
    (is (= path (:path (first (compiler/definition-errors :effects definition)))) (pr-str path)))
  (testing "the step keeps evaluated uniforms in the instance state"
    (let [step (compiler/compile-effect (assoc-in strip-effect [:shader :uniforms :u_steps]
                                                  {:type :int :value {:op :multiply :args [{:op :param :key :level} 5]}}))
          result (step {:frame {:time-ms 1500 :fixtures []} :instance {:started-at-ms 500 :params {:level 0.25}}
                        :state nil :assignments [] :intents [] :done? false})]
      (is (= {"u_time" 1.0 "u_beats" 0.0 "u_level" 0.25 "u_steps" 1} (shader/uniforms-of result)))))
  (is (false? (compiler/predictable-outputs? strip-effect)) "Shader uniforms are never forecast"))

;; Producer -----------------------------------------------------------------

(defn- request [overrides]
  (merge {:feed :shader/index-strip :effect [:index-strip 1] :order 1
          :shader (shader/afterglow-shader (:shader strip-effect))
          :width 200 :height 1 :fps 30
          :uniforms {"u_time" 0.0 "u_beats" 0.0 "u_level" 0.5}}
         overrides))

(defn- red-at [frame x] (first (source/rgb-at frame x 0)))

(deftest the-producer-publishes-the-last-completed-frame-per-instance-and-feed
  (let [r (test-renderer programs)
        p (producer/create {:renderer r})]
    (try
      (producer/sync! p {"a" (request {})})
      (is (eventually #(get-in (producer/frames p) [:frames :shader/index-strip])))
      (let [f (get-in (producer/frames p) [:frames :shader/index-strip])]
        (is (= [200 1] [(:width f) (:height f)]))
        (is (= (range 200) (map #(red-at f %) (range 200))) "Each target pixel carries its own index")
        (is (= f (get-in (producer/frames p) [:instance-frames "a" :shader/index-strip]))))
      (testing "unchanged uniforms re-use the published frame instead of rendering"
        (let [renders (count (:renders (log-of r)))]
          (Thread/sleep 150)
          (is (= renders (count (:renders (log-of r)))))
          (is (pos? (get-in (producer/status p) [:instances 0 :unchanged])))))
      (testing "new committed uniforms reach the next slot"
        (producer/sync! p {"a" (request {:uniforms {"u_time" 0.1 "u_beats" 0.0 "u_level" 1.0}})})
        (is (eventually #(= 255 (second (source/rgb-at (get-in (producer/frames p) [:frames :shader/index-strip]) 0 0))))))
      (testing "a feed resolves to its newest-launched instance; each instance keeps its own frame"
        (producer/sync! p {"a" (request {:uniforms {"u_time" 0.1 "u_beats" 0.0 "u_level" 1.0}})
                           "b" (request {:order 2 :uniforms {"u_time" 0.0 "u_beats" 0.0 "u_level" 0.0}})})
        (is (eventually #(get-in (producer/frames p) [:instance-frames "b"])))
        (let [{:keys [frames instance-frames]} (producer/frames p)]
          (is (= 0 (second (source/rgb-at (get frames :shader/index-strip) 0 0))))
          (is (= 255 (second (source/rgb-at (get-in instance-frames ["a" :shader/index-strip]) 0 0))))))
      (testing "an ended instance drops its frame, and its program once nothing renders it"
        (producer/sync! p {"b" (request {:order 2 :uniforms {"u_time" 0.0 "u_beats" 0.0 "u_level" 0.0}})})
        (is (= #{"b"} (set (keys (:instance-frames (producer/frames p))))))
        (is (empty? (:released (log-of r))) "b still renders the same program")
        (producer/sync! p {})
        (is (= {:frames {} :instance-frames {}} (producer/frames p)))
        (is (eventually #(= [index-glsl] (:released (log-of r)))))
        (is (= [] (:instances (producer/status p)))))
      (finally (producer/stop! p) (producer/close-renderer r)))))

(deftest a-failed-compile-keeps-rendering-the-last-working-program
  (let [r (test-renderer programs)
        p (producer/create {:renderer r})
        broken (assoc (request {}) :shader {:source "void main() { fragColor = vec4(oops); }"
                                            :uniforms {"u_time" :float "u_beats" :float "u_level" :float}})]
    (try
      (producer/sync! p {"a" (request {})})
      (is (eventually #(get-in (producer/frames p) [:frames :shader/index-strip])))
      (producer/sync! p {"a" (assoc broken :uniforms {"u_time" 0.0 "u_beats" 0.0 "u_level" 1.0})})
      (is (eventually #(get-in (producer/status p) [:instances 0 :error])))
      (let [row (get-in (producer/status p) [:instances 0])]
        (is (= :shader/compile-failed (get-in row [:error :code])))
        (is (re-find #"oops" (get-in row [:error :log])))
        (is (:last-working? row)))
      (testing "the last working program renders the new values of the uniforms it shares"
        (is (eventually #(= 255 (second (source/rgb-at (get-in (producer/frames p) [:frames :shader/index-strip]) 0 0)))))
        (is (= index-glsl (first (last (:renders (log-of r)))))))
      (is (= 1 (count (filter #{index-glsl} (:compiles (log-of r))))) "A failed version is not recompiled every slot")
      (finally (producer/stop! p) (producer/close-renderer r)))))

(deftest slow-renders-drop-slots-instead-of-catching-up
  (let [r (test-renderer programs :render-ms 70)
        p (producer/create {:renderer r})]
    (try
      ;; Every slot has new uniforms, so every slot needs a render.
      (let [stop (promise)
            feeder (future (loop [t 0.0]
                             (producer/sync! p {"a" (request {:uniforms {"u_time" t "u_beats" 0.0 "u_level" 0.5}})})
                             (when (= ::go (deref stop 5 ::go)) (recur (+ t 0.01)))))]
        (Thread/sleep 1000)
        (deliver stop :stop)
        @feeder)
      (let [{:keys [slots rendered dropped]} (get-in (producer/status p) [:instances 0])]
        ;; 30 fps for one second is about 30 slots; a 70 ms render fits about 14.
        (is (<= 10 rendered 16) (pr-str [slots rendered dropped]))
        (is (<= 12 dropped 22) (pr-str [slots rendered dropped]))
        (is (<= 26 (+ slots dropped) 34) "Each slot is either rendered or dropped, never queued"))
      (finally (producer/stop! p) (producer/close-renderer r)))))

(deftest instances-beyond-the-limit-wait-in-launch-order
  (let [r (test-renderer programs)
        p (producer/create {:renderer r})]
    (try
      (producer/sync! p (into {} (for [i (range 10)] [(str "i" i) (request {:order i})])))
      (is (= (set (map #(str "i" %) (range producer/max-instances)))
             (set (map :instance-id (:instances (producer/status p))))))
      (is (= ["i8" "i9"] (:refused (producer/status p))))
      (finally (producer/stop! p) (producer/close-renderer r)))))

(deftest a-missing-gpu-renderer-is-reported-not-thrown
  (let [p (producer/create {:renderer #(throw (ex-info "Shader cues need Afterglow's native GPU renderer" {:code :shader/no-gpu-renderer}))})]
    (try
      (producer/sync! p {"a" (request {})})
      (is (eventually #(= :shader/no-gpu-renderer (get-in (producer/status p) [:error :code]))))
      (is (= {:compiled? false :message "Shader cues need Afterglow's native GPU renderer" :code :shader/no-gpu-renderer}
             (producer/audition! p {:source "void main(){}"})))
      (finally (producer/stop! p)))))

;; Through a show runtime ---------------------------------------------------

(def strip-layout {:id :strip-200 :shape :path :color-layout :rgb :universe 1 :address 1 :pixels 200
                   :points [[0 0 0] [4 0 0]]})

(def strip-mapping {:id :strip-200 :source :shader/index-strip
                    :strips [{:id :s :layout :strip-200 :points [[0.0 0.5] [1.0 0.5]] :height 1.0 :filter :nearest}]})

(defn- start-show [renderer]
  (show/create-runtime {:model (-> (demo/model)
                                   (assoc :rig (layout/materialize {:fixtures [] :pixel-layouts [strip-layout]}))
                                   (assoc-in [:settings :video :mappings] {:strip-200 strip-mapping}))
                        :workspace-options nil
                        :shader-options {:renderer renderer}}))

(defn- pixel-dmx
  "RGB bytes of strip pixel n, which may straddle universes 1 and 2."
  [runtime n]
  (let [universes (get-in @(:state runtime) [:output :universes])]
    (mapv (fn [k] (let [channel (+ 512 (* 3 n) k)
                        ^bytes u (get universes (quot channel 512))]
                    (bit-and 0xff (aget u (rem channel 512)))))
          (range 3))))

(defn- linear-byte [srgb]
  (let [c (/ srgb 255.0)]
    (Math/round (* 255.0 (if (<= c 0.04045) (/ c 12.92) (Math/pow (/ (+ c 0.055) 1.055) 2.4))))))

(defn- accepted! [reply] (is (= :accepted (:status @reply)) (pr-str @reply)) @reply)

(defn- publish! [runtime definition t]
  (let [effect (show/submit! runtime :definition {:kind :effects :definition definition})
        cue (show/submit! runtime :definition {:kind :cues :definition {:id (:id definition) :effect-id (:id definition) :priority 10}})]
    (show/tick! runtime t)
    (run! accepted! [effect cue])))

(defn- launch! [runtime cue t]
  (let [reply (show/submit! runtime {:op :cue/launch :cue-id cue})]
    (show/tick! runtime t)
    (:instance-id (accepted! reply))))

(defn- command! [runtime payload t]
  (let [reply (show/submit! runtime payload)]
    (show/tick! runtime t)
    (accepted! reply)))

(defn- tick-until [runtime t pred]
  (loop [t' t]
    (show/tick! runtime t')
    (cond (pred) t'
          (> t' (+ t 10000)) (throw (ex-info "Condition never held" {}))
          :else (do (Thread/sleep 10) (recur (+ t' 25))))))

(deftest a-200-by-1-shader-target-drives-a-strip-pixel-for-pixel
  (let [r (test-renderer programs)
        runtime (start-show r)]
    (try
      (publish! runtime strip-effect 0)
      (let [id (launch! runtime :index-strip 25)
            t (tick-until runtime 50 #(get-in @(:state runtime) [:video-latch :instance-frames id :shader/index-strip]))]
        (show/tick! runtime (+ t 25))
        (is (= (mapv (fn [n] [(linear-byte n) (linear-byte 128) 0]) (range 200))
               (mapv #(pixel-dmx runtime %) (range 200)))
            "Pixel n of the strip is texel n of the render target, in strip order")
        (testing "the cue's control drives the uniform through the committed frame"
          (command! runtime {:op :param/set :instance-id id :param :level :value 1.0} (+ t 50))
          (let [t (tick-until runtime (+ t 75) #(= 255 (second (pixel-dmx runtime 0))))]
            (is (= [(linear-byte 199) 255 0] (pixel-dmx runtime 199)))
            (testing "the Studio projection lists the feed and its producer"
              (let [video (:video (show/snapshot runtime))]
                (is (= [:shader/index-strip] (mapv :id (filter #(= :shader (:kind %)) (:sources video)))))
                (is (= [id] (mapv :instance-id (get-in video [:shaders :instances]))))
                (is (= [200 1] (get-in video [:mappings :strip-200 :size])))))
            (testing "stopping the cue stops its cadence and clears the strip"
              (command! runtime {:op :instance/end :instance-id id :force? true} (+ t 25))
              (show/tick! runtime (+ t 50))
              (show/tick! runtime (+ t 75))
              (is (= [] (:instances (producer/status (:shader-video runtime)))))
              (is (nil? (get-in @(:state runtime) [:video-latch :frames :shader/index-strip])))
              (is (= [0 0 0] (pixel-dmx runtime 0)))))))
      (finally (show/stop! runtime)))))

(deftest a-paused-instance-and-a-stopped-transport-freeze-the-shader
  (let [r (test-renderer programs)
        runtime (start-show r)]
    (try
      (publish! runtime strip-effect 0)
      (let [id (launch! runtime :index-strip 25)
            t (tick-until runtime 50 #(get-in @(:state runtime) [:video-latch :instance-frames id]))
            uniforms #(shader/uniforms-of (get-in @(:state runtime) [:model :instances id]))]
        (show/tick! runtime (+ t 100))
        (let [before (uniforms)]
          (command! runtime {:op :transport/set :transport {:playing? false}} (+ t 125))
          (let [held (uniforms)]
            (show/tick! runtime (+ t 500))
            (is (= held (uniforms)) "A stopped transport holds the shader clock")
            (is (< (get held "u_time") (+ (get before "u_time") 0.05)) (pr-str before held))))
        (Thread/sleep 100)
        (let [renders (count (:renders (log-of r)))]
          (Thread/sleep 150)
          (is (= renders (count (:renders (log-of r)))) "Frozen uniforms render nothing new")))
      (finally (show/stop! runtime)))))

(deftest a-running-shader-cue-saves-and-reloads
  (let [r (test-renderer programs)
        runtime (start-show r)]
    (try
      (publish! runtime strip-effect 0)
      (let [id (launch! runtime :index-strip 25)]
        (command! runtime {:op :param/set :instance-id id :param :level :value 0.75} 50)
        (show/tick! runtime 75)
        (is (= 1 (count (get-in @(:state runtime) [:model :instances]))))
        (let [saved (persistence/read-string (show/export-show-source runtime))
              thawed (persistence/thaw (demo/model) saved)
              r2 (test-renderer programs)
              restored (show/create-runtime {:model thawed :workspace-options nil :shader-options {:renderer r2}})]
          (try
            (is (= (:shader strip-effect) (get-in thawed [:registry :effects :index-strip :shader])))
            ;; A restore rekeys instance ids.
            (let [[restored-id instance] (first (:instances thawed))]
              (is (= 0.75 (get-in instance [:params :level])))
              (is (= 0.75 (get (shader/uniforms-of instance) "u_level"))
                  "The committed uniforms are portable instance state")
              (tick-until restored 100 #(get-in @(:state restored) [:video-latch :instance-frames restored-id])))
            (is (= 191 (second (source/rgb-at (get-in @(:state restored) [:video-latch :frames :shader/index-strip]) 0 0))))
            (finally (show/stop! restored)))))
      (finally (show/stop! runtime)))))

(deftest studio-routes-pump-projector-frames-and-audition-compiles
  (let [r (test-renderer programs)
        runtime (start-show r)
        handler (studio/handler runtime)
        transit-headers {"host" "localhost" "content-type" "application/transit+json" "accept" "application/transit+json"}
        audition (fn [shader]
                   (let [response (handler {:request-method :post :uri "/api/video/shader/audition" :headers transit-headers
                                            :body (ByteArrayInputStream. (.getBytes (transit/encode {:shader shader :params (:params strip-effect)}) "UTF-8"))})]
                     [(:status response) (transit/decode (:body response))]))]
    (try
      (publish! runtime strip-effect 0)
      (let [id (launch! runtime :index-strip 25)
            t (tick-until runtime 50 #(get-in @(:state runtime) [:video-latch :frames :shader/index-strip]))
            next! #(handler {:request-method :get :uri "/api/video/next" :headers {"host" "localhost"}
                             :query-string (str "source=:shader/index-strip&wait=50&after=" %)})
            frame (next! -1)
            sequence (parse-long (get-in frame [:headers "X-Frame-Sequence"]))
            bytes ^bytes (:body frame)]
        (is (= 200 (:status frame)))
        (is (= ["200" "1"] (mapv #(get-in frame [:headers %]) ["X-Frame-Width" "X-Frame-Height"])))
        (is (= 800 (alength bytes)))
        (is (= [0 128 199] (mapv #(bit-and 0xff (aget bytes %)) [2 1 (+ 2 (* 4 199))]))
            "BGRA: red of pixel n is n, green the level")
        (testing "nothing newer within the wait answers 204, so the page holds its picture"
          (is (= 204 (:status (next! sequence)))))
        (testing "a new committed value arrives as the next frame"
          (command! runtime {:op :param/set :instance-id id :param :level :value 1.0} (+ t 25))
          (tick-until runtime (+ t 50) #(= 255 (second (source/rgb-at (get-in @(:state runtime) [:video-latch :frames :shader/index-strip]) 0 0))))
          (let [frame (next! sequence) bytes ^bytes (:body frame)]
            (is (= 200 (:status frame)))
            (is (= 255 (bit-and 0xff (aget bytes 1)))))))
      (is (= 204 (:status (handler {:request-method :get :uri "/api/video/next" :headers {"host" "localhost"}
                                    :query-string "source=:shader/none&wait=10"}))))
      (testing "audition compiles on the renderer and publishes nothing"
        (let [revision (get-in @(:state runtime) [:model :registry :revision])]
          (is (= [200 {:compiled? true}] (audition (:shader strip-effect))))
          (let [[status body] (audition (assoc (:shader strip-effect) :glsl "void main() { fragColor = vec4(oops); }"))]
            (is (= 200 status))
            (is (false? (:compiled? body)))
            (is (re-find #"oops" (:log body))))
          (let [[_ body] (audition (assoc-in (:shader strip-effect) [:uniforms :u_level :value :key] :gone))]
            (is (= [:shader :uniforms :u_level :value :key] (get-in body [:errors 0 :path]))
                "Uniform expressions are checked against the cue's parameters first"))
          (is (= revision (get-in @(:state runtime) [:model :registry :revision])))
          (testing "Studio auditions a registered cue's edit by id, typed by its own parameters"
            (is (= {:compiled? true} (show/video-action! runtime :audition {:effect-id "index-strip" :glsl gradient-glsl}))))))
      (testing "saving GLSL publishes the next version through the definition command"
        (let [save! #(let [request (future (handler {:request-method :post :uri "/api/video/shader/save" :headers transit-headers
                                                     :body (ByteArrayInputStream. (.getBytes (transit/encode %) "UTF-8"))}))
                           ;; The route waits for the commit; the show commits on its next tick.
                           response (loop [t 100000]
                                      (if (realized? request) @request
                                          (do (show/tick! runtime t) (Thread/sleep 2) (recur (+ t 25)))))]
                       [(:status response) (transit/decode (:body response))])
              [status body] (save! {:effect-id :index-strip :glsl gradient-glsl :expected-version 1})]
          (is (= [200 :accepted] [status (:status body)]) (pr-str body))
          (is (= [2 gradient-glsl] ((juxt :version (comp :glsl :shader)) (get-in @(:state runtime) [:model :registry :effects :index-strip]))))
          (is (= 422 (first (save! {:effect-id :index-strip :glsl index-glsl :expected-version 1}))) "A stale version is refused")
          (is (= 422 (first (save! {:effect-id :solid-red :glsl index-glsl}))) "Only a shader effect takes GLSL")))
      (testing "the projector page is a static Studio asset"
        (let [page (handler {:request-method :get :uri "/studio/video-output.html" :headers {"host" "localhost"}})]
          (is (= 200 (:status page)))
          (is (re-find #"api/video/next" (slurp (:body page))))))
      (finally (show/stop! runtime)))))

(deftest an-agent-proposed-shader-cue-uses-the-same-validated-command
  ;; The agent tool `publish_effect` sends JSON-shaped data through the
  ;; ordinary :definition command; enum strings normalize as for any effect.
  (let [proposal (data/normalize-json
                  {:id "agent-glow" :version 1 :name "Agent glow"
                   :params {:level {:type "number" :default 0.4 :bounds [0 1]}}
                   :shader {:glsl index-glsl :size [200 1] :fps 25 :kind "2d"
                            :uniforms {:u_level {:type "float" :value {:op "param" :key "level"}}}}
                   :outputs {:color {:op "video" :mapping "strip-200"}}})
        r (test-renderer programs)
        runtime (start-show r)]
    (try
      (is (= [] (compiler/definition-errors :effects (update-in proposal [:outputs :color :mapping] keyword))))
      (let [reply (show/submit! runtime :definition {:kind :effects :definition (update-in proposal [:outputs :color :mapping] keyword)
                                                     :source {:adapter :agent :session-id "run-1"}})]
        (show/tick! runtime 0)
        (accepted! reply)
        (is (= :shader/agent-glow (shader/feed-id (get-in @(:state runtime) [:model :registry :effects :agent-glow])))))
      (finally (show/stop! runtime)))))

(deftest a-saved-version-reloads-the-running-cue-and-a-broken-one-keeps-the-last-working
  (let [r (test-renderer programs)
        runtime (start-show r)
        save! (fn [glsl t]
                (let [reply (show/video-action! runtime :save-glsl {:effect-id :index-strip :glsl glsl})]
                  (show/tick! runtime t)
                  (accepted! reply)))
        row #(first (:instances (producer/status (:shader-video runtime))))]
    (try
      (let [effect (show/submit! runtime :definition {:kind :effects :definition strip-effect})
            cue (show/submit! runtime :definition {:kind :cues :definition {:id :index-strip :effect-id :index-strip :priority 10
                                                                            :reload-policy :preserve-state}})]
        (show/tick! runtime 0)
        (run! accepted! [effect cue]))
      (let [id (launch! runtime :index-strip 25)
            t (tick-until runtime 50 #(get-in @(:state runtime) [:video-latch :instance-frames id]))]
        (save! "void main() { fragColor = vec4(oops); }" (+ t 25))
        (let [t (tick-until runtime (+ t 50) #(get-in (row) [:error :log]))]
          (is (:last-working? (row)))
          (is (= 2 (get-in @(:state runtime) [:model :instances id :effect-version])) "The instance runs version 2")
          (let [rendered (:rendered (row))]
            (tick-until runtime (+ t 25) #(> (:rendered (row)) (+ rendered 3)))
            (is (= index-glsl (first (last (:renders (log-of r))))) "Frames keep coming from the last working program"))
          (save! gradient-glsl (+ t 1000))
          (tick-until runtime (+ t 1025) #(and (nil? (:error (row))) (= gradient-glsl (first (last (:renders (log-of r)))))))
          (is (= 3 (get-in @(:state runtime) [:model :instances id :effect-version])))
          (is (false? (:last-working? (row))))))
      (finally (show/stop! runtime)))))
