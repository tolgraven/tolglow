(ns tolglow.studio.video-strips-host-test
  "Video strip mapping through a real show runtime (#672): captured DMX for a
  labelled grid on two strips, composition with other cues and opacity,
  transport stop, cue-driven placement, save/reload and the Studio routes."
  (:require [clojure.test :refer [deftest is testing]]
            [tolglow.demo :as demo]
            [tolglow.persistence :as persistence]
            [tolglow.rig.pixel-layout :as layout]
            [tolglow.show :as show]
            [tolglow.studio :as studio]
            [tolglow.transit :as transit]
            [tolglow.video.hub :as video-hub]
            [tolglow.video.source :as source])
  (:import [java.io ByteArrayInputStream File]
           [javax.imageio ImageIO]))

(def tubes
  [{:id :tube-a :shape :path :color-layout :rgb :universe 1 :address 1 :pixels 8 :points [[0 0 0] [2 0 0]]}
   {:id :tube-b :shape :path :color-layout :rgb :universe 1 :address 101 :pixels 8 :points [[0 1 0] [2 1 0]]}])

(defn- video-settings [source]
  {:sources {:grid source}
   :mappings {:wall {:id :wall :source :grid
                     :strips [{:id :a :layout :tube-a :points [[0 0.125] [1 0.125]] :height 0.05}
                              {:id :b :layout :tube-b :points [[0 0.625] [1 0.625]] :height 0.05}]}}})

(defn- model [source]
  (-> (demo/model)
      (assoc :rig (layout/materialize {:fixtures [] :pixel-layouts tubes}))
      (assoc-in [:settings :video] (video-settings source))))

(defn- dmx
  "RGB bytes of the n-th pixel of a tube (0-based) in the committed frame."
  [runtime address n]
  (let [^bytes u (get-in @(:state runtime) [:output :universes 1])
        base (+ (dec address) (* 3 n))]
    (mapv #(bit-and 0xff (aget u (+ base %))) (range 3))))

(defn- expected-dmx
  "Pixels without a dimmer receive the linear colour scaled to a byte."
  ([column row] (expected-dmx column row 1.0))
  ([column row gain]
   (mapv (fn [b] (let [c (/ b 255.0)
                       linear (if (<= c 0.04045) (/ c 12.92) (Math/pow (/ (+ c 0.055) 1.055) 2.4))]
                   (* gain 255.0 linear)))
         (source/grid-color column row))))

(defn- near? [expected actual] (every? #(<= (Math/abs (double %)) 1.01) (map - expected actual)))

(defn- start [source]
  (let [runtime (show/create-runtime {:model (model source) :workspace-options nil})]
    ;; Sources normally open on the hub's own thread after a commit; open them
    ;; here directly so the first frame is deterministic.
    (video-hub/reconcile! (:video runtime) {:grid source})
    runtime))

(defn- accepted! [reply] (is (= :accepted (:status @reply)) (pr-str @reply)) @reply)

(defn- publish! [runtime t]
  (let [effect (show/submit! runtime :definition
                             {:kind :effects
                              :definition {:id :video-wall :version 1
                                           :params {:drift {:type :number :default 0.0 :bounds [-1 1]}}
                                           :outputs {:color {:op :video :mapping :wall
                                                             :placement {:x {:op :param :key :drift}}}}}})
        cue (show/submit! runtime :definition {:kind :cues :definition {:id :video-wall :effect-id :video-wall :priority 10}})
        red-effect (show/submit! runtime :definition
                                 {:kind :effects :definition {:id :red-a :version 1 :targets (mapv #(keyword (str "tube-a-head-" %)) (range 1 9))
                                                              :outputs {:color [1.0 0.0 0.0]}}})
        red-cue (show/submit! runtime :definition {:kind :cues :definition {:id :red-a :effect-id :red-a :priority 20}})]
    (show/tick! runtime t)
    (run! accepted! [effect cue red-effect red-cue])))

(defn- launch! [runtime cue t]
  (let [reply (show/submit! runtime {:op :cue/launch :cue-id cue})]
    (show/tick! runtime t)
    (:instance-id (accepted! reply))))

(defn- command! [runtime payload t]
  (let [reply (show/submit! runtime payload)]
    (show/tick! runtime t)
    (accepted! reply)))

(deftest a-video-cue-writes-each-strip-from-its-own-rows-in-endpoint-order
  (let [runtime (start {:kind :test-pattern :width 640 :height 360})]
    (try
      (publish! runtime 0)
      (let [id (launch! runtime :video-wall 25)]
        (show/tick! runtime 50)
        (doseq [n (range 8)]
          (is (near? (expected-dmx n 0) (dmx runtime 1 n)) (str "tube A pixel " n))
          (is (near? (expected-dmx n 2) (dmx runtime 101 n)) (str "tube B pixel " n)))
        (testing "a cue parameter drives source placement"
          (command! runtime {:op :param/set :instance-id id :param :drift :value 0.125} 75)
          (show/tick! runtime 100)
          (is (near? (expected-dmx 1 0) (dmx runtime 1 0)))
          (is (near? [0 0 0] (dmx runtime 1 7)) "the last pixel now reads past the frame edge"))
        (testing "instance opacity scales the video layer"
          (command! runtime {:op :param/set :instance-id id :param :drift :value 0.0} 125)
          (command! runtime {:op :instance/opacity-set :instance-id id :value 0.5} 150)
          (show/tick! runtime 175)
          (is (near? (expected-dmx 7 0 0.5) (dmx runtime 1 7))))
        (testing "a higher-priority cue composes over one strip and leaves the other"
          (command! runtime {:op :instance/opacity-set :instance-id id :value 1.0} 200)
          (launch! runtime :red-a 225)
          (show/tick! runtime 250)
          (is (= [255 0 0] (dmx runtime 1 0)))
          (is (near? (expected-dmx 3 2) (dmx runtime 101 3)))))
      (testing "the Studio projection carries source status and sampled strip colours"
        (let [video (:video (show/snapshot runtime))]
          (is (= [:grid] (mapv :id (:sources video))))
          (is (false? (get-in video [:sources 0 :stale?])))
          (is (= [640 360] (get-in video [:mappings :wall :size])))
          (is (= "#0000ff" (get-in video [:mappings :wall :strips 0 :colors 0])))
          (is (= "#ff0000" (get-in video [:mappings :wall :strips 0 :colors 7])))))
      (finally (show/stop! runtime)))))

(deftest stopping-the-transport-freezes-video-and-play-resumes-on-the-current-frame
  (let [file (File/createTempFile "tolglow-video" ".tgvf")
        _ (source/write-feed! file (source/test-pattern-frame {:width 64 :height 36}) {:frames 1})
        runtime (start {:kind :feed :file (str file)})]
    (try
      (publish! runtime 0)
      (launch! runtime :video-wall 25)
      (show/tick! runtime 50)
      (is (near? (expected-dmx 0 0) (dmx runtime 1 0)))
      (command! runtime {:op :transport/set :transport {:playing? false}} 75)
      (source/write-feed! file (source/test-pattern-frame {:width 64 :height 36 :shift 3}) {:frames 2})
      (show/tick! runtime 100)
      (show/tick! runtime 125)
      (is (near? (expected-dmx 0 0) (dmx runtime 1 0)) "stopped: the strip keeps the frame it stopped on")
      (is (true? (get-in (show/snapshot runtime) [:video :frozen?])))
      (command! runtime {:op :transport/set :transport {:playing? true}} 150)
      (show/tick! runtime 175)
      (is (near? (expected-dmx 3 0) (dmx runtime 1 0)) "playing again: the newest frame, no catch-up")
      (finally (show/stop! runtime) (.delete file)))))

(deftest mappings-save-reload-and-invalid-edits-are-refused
  (let [runtime (start {:kind :test-pattern})]
    (try
      (let [moved (show/submit! runtime :video-mapping
                                {:op :put-strip :mapping-id :wall
                                 :strip {:id :a :layout :tube-a :points [[1 0.125] [0 0.125]] :height 0.1 :filter :nearest}})
            invalid (show/submit! runtime :video-mapping {:op :put-strip :mapping-id :wall :strip {:id :c :layout :x :points []}})]
        (show/tick! runtime 0)
        (accepted! moved)
        (is (= :rejected (:status @invalid))))
      (let [saved (persistence/read-string (show/export-show-source runtime))
            thawed (persistence/thaw (demo/model) saved)]
        (is (= (get-in @(:state runtime) [:model :settings :video]) (get-in thawed [:settings :video])))
        (is (= [[1 0.125] [0 0.125]] (get-in thawed [:settings :video :mappings :wall :strips 0 :points])))
        (is (thrown? clojure.lang.ExceptionInfo
                     (persistence/thaw (demo/model) (assoc-in saved [:model :settings :video :mappings :wall :strips 0 :points] [[0 0]])))))
      (finally (show/stop! runtime)))))

(defn- while-ticking [runtime f]
  (let [operation (future (f))
        deadline (+ (System/nanoTime) 6000000000)]
    (try (loop [t 0]
           (cond (realized? operation) @operation
                 (> (System/nanoTime) deadline) (throw (ex-info "Owner command did not settle" {}))
                 :else (do (show/tick! runtime t) (Thread/sleep 2) (recur (+ t 5)))))
         (finally (future-cancel operation)))))

(deftest studio-routes-edit-mappings-and-serve-the-latched-frame
  (let [runtime (start {:kind :test-pattern :width 320 :height 180})
        handler (studio/handler runtime)
        transit-headers {"host" "localhost" "content-type" "application/transit+json" "accept" "application/transit+json"}]
    (try
      (show/tick! runtime 0)
      (let [response (while-ticking runtime
                       #(handler {:request-method :post :uri "/api/video-mapping" :headers transit-headers
                                  :body (ByteArrayInputStream. (.getBytes (transit/encode {:op :put-strip :mapping-id :wall
                                                                                          :strip {:id :b :layout :tube-b :points [[0 0.875] [1 0.875]]}})
                                                                          "UTF-8"))}))]
        (is (= 200 (:status response)) (pr-str (transit/decode (:body response))))
        (is (= [[0 0.875] [1 0.875]] (get-in @(:state runtime) [:model :settings :video :mappings :wall :strips 1 :points]))))
      (let [status (handler {:request-method :get :uri "/api/video" :headers transit-headers})
            body (transit/decode (:body status))]
        (is (= 200 (:status status)))
        (is (= :grid (get-in body [:sources 0 :id]))))
      (let [frame (handler {:request-method :get :uri "/api/video/frame" :query-string "source=:grid&width=160"
                            :headers {"host" "localhost"}})
            image (ImageIO/read ^java.io.InputStream (:body frame))]
        (is (= 200 (:status frame)))
        (is (= "image/jpeg" (get-in frame [:headers "Content-Type"])))
        (is (= [160 90] [(.getWidth image) (.getHeight image)])))
      (is (= 404 (:status (handler {:request-method :get :uri "/api/video/frame" :query-string "source=:none"
                                    :headers {"host" "localhost"}}))))
      (testing "without the native bridge, Syphon discovery reports why"
        (let [body (transit/decode (:body (handler {:request-method :get :uri "/api/video/syphon" :headers transit-headers})))]
          (when-not (System/getenv "TOLGLOW_VIDEO_BRIDGE")
            (is (= [] (:servers body)))
            (is (string? (:error body))))))
      (finally (show/stop! runtime)))))
