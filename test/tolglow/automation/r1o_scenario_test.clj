(ns tolglow.automation.r1o-scenario-test
  "R1o as deterministic seeded data (#217). On the 2023 RGBW gig rig, the real
  show owner runs auto mode for 32 bars at 120 BPM: predominantly red looks with
  occasional muted purple, beat one-shots, a quiet darkness/one-spotlight
  section and abrupt strobe contrast, while a performer plays from Push. The
  checks read captured DMX and committed show state, never planner internals.
  Artistic quality is for the operator to judge in the Room and on fixtures."
  (:require [afterglow.render.frame :as render]
            [clojure.test :refer [deftest is testing]]
            [tolglow.lighting-capacity :as capacity]
            [tolglow.registry :as registry]
            [tolglow.automation.red-show :refer [red deep-red muted-purple movers model auto-config]]
            [tolglow.show :as show]
            [tolglow.venues.gig-2023 :as gig]))

(def frame-ms 125)

(def end-ms 64000)

(def push {:adapter :push :session-id "push-3"})

(def push-spot-ms 20000)

(def stop-ms 36000)

(def play-ms 40000)

(defn- meter []
  (let [patch (render/compile-patch (gig/fixtures))]
    (capacity/prepare patch)))

(defn- lit [meter universes]
  (if-not (seq universes) #{}
    (let [^doubles levels (capacity/local-brightnesses meter universes)]
      (into #{} (keep (fn [[id [index _]]] (when (> (aget levels (int index)) 1e-3) id))) (:writers-by-head meter)))))

(defn- strobing-washes [universes]
  ;; Thunder Wash channel 2: 11-250 strobes, 0-5 and 251-255 are open.
  (let [^bytes dmx (get universes 1)]
    (into #{} (keep (fn [{:keys [id address fixture-type-id]}]
                      (when (#{:cameo-thunder-wash-600-rgbw :cameo-thunder-wash-600-rgb} fixture-type-id)
                        (when (<= 11 (bit-and 0xff (aget dmx (+ address 0))) 250) id))))
          (gig/fixtures))))

(defn run
  "Tick the real show owner and return one record per frame."
  [seed]
  (let [captured (atom nil)
        sink {:send! (fn [universes _] (reset! captured (update-vals universes aclone)))}
        rt (show/create-runtime {:model (model) :sink sink :workspace-options nil})
        meter (meter)
        submit! (fn [t type payload]
                  (let [reply (show/submit! rt type payload)]
                    (show/tick! rt t)
                    (deref reply 2000 ::missing)))]
    (try
      (show/tick! rt 0)
      (is (= :accepted (:status (submit! 0 :auto (auto-config seed)))))
      (loop [t frame-ms frames [] spot nil]
        (if (> t end-ms) frames
          (let [spot (cond
                       (= t push-spot-ms)
                       (:instance-id (submit! t :command {:op :cue/launch :cue-id :spot :source push
                                                          :launch {:mode :persistent}}))
                       (= t stop-ms) (do (submit! t :command {:op :transport/set :source push :transport {:playing? false}}) spot)
                       (= t play-ms) (do (submit! t :command {:op :transport/set :source push :transport {:playing? true}}) spot)
                       :else (do (show/tick! rt t) spot))
                m (:model @(:state rt))
                universes @captured]
            (is (nil? (get-in @(:state rt) [:status :error])) (pr-str (:status @(:state rt))))
            (recur (+ t frame-ms)
                   (conj frames {:t t :beat (get-in m [:transport :beat]) :playing? (get-in m [:transport :playing?])
                                 :section (get-in m [:auto :state :section :id])
                                 :tick (get-in m [:auto :state :tick])
                                 :instances (into (sorted-map)
                                                  (map (fn [[id i]] [id (-> (select-keys i [:cue-id :params :phase :launch-mode :duration :duration-ms :started-at-beat])
                                                                           ;; Session IDs are fresh UUIDs per start.
                                                                           (assoc :adapter (get-in i [:source :adapter])))]))
                                                  (:instances m))
                                 :spot spot
                                 :lit (lit meter universes)
                                 :strobing (strobing-washes universes)
                                 :dmx (vec (get universes 1))})
                   spot))))
      (finally (show/stop! rt)))))

(defn- auto? [instance] (= :auto (:adapter instance)))

(defn- launches
  "Every auto-launched instance, once, with the section it started in."
  [frames]
  (vals (reduce (fn [seen {:keys [instances section]}]
                  (reduce-kv (fn [seen id i] (if (or (contains? seen id) (not (auto? i))) seen
                                               (assoc seen id (assoc i :id id :section section))))
                             seen instances))
                (sorted-map) frames)))

(defn- section-runs [frames]
  (map (fn [run] {:section (:section (first run)) :frames run})
       (partition-by :section (filter :section frames))))

(deftest r1o-prompt-to-show-scenario-on-the-rgbw-rig
  (let [frames (run 217)
        all (launches frames)
        sections (map :section (section-runs frames))
        colored (filter #(#{:wash-color :head-glow :accent} (:cue-id %)) all)
        colors (frequencies (map #(get-in % [:params :color]) colored))]
    (testing "seeded replay: the same seed gives the same decisions, sections and DMX"
      (is (true? (= frames (run 217))))
      (is (true? (not= (map :section frames) (map :section (run 218))))))

    (testing "every section of the plan plays, most often red"
      (is (= #{:red :purple :quiet :strobe} (set sections)) (pr-str sections))
      (is (= :red (key (apply max-key val (frequencies (map :section (filter :section frames))))))))

    (testing "predominantly red with occasional muted purple, whole palette colors only"
      (is (every? #{red deep-red muted-purple} (keys colors)) (pr-str colors))
      (is (> (+ (colors red 0) (colors deep-red 0)) (* 2 (colors muted-purple 0))) (pr-str colors))
      (is (pos? (colors muted-purple 0)) (pr-str colors)))

    (testing "auto launches are musical one-shots or timed cues, never millisecond timers"
      (is (seq all))
      (is (every? #(and (#{:one-shot :timed} (:launch-mode %)) (map? (:duration %)) (nil? (:duration-ms %))) all))
      (is (every? #(#{{:beats 1} {:beats 2} {:bars 1}} (:duration %)) all)))

    (testing "beat one-shots land on the beat"
      (let [starts (map :started-at-beat all)]
        (is (every? #(== % (Math/rint %)) starts) (pr-str (take 10 starts)))))

    (testing "movement in a lit section brings its own light; strobe stays out of calm sections"
      (doseq [[_ group] (group-by (juxt :section :beat-of)
                                  (map #(assoc % :beat-of (:started-at-beat %)) all))
              :when (#{:red :purple} (:section (first group)))
              :when (some #(= :sweep (:cue-id %)) group)]
        (is (some #(= :head-glow (:cue-id %)) group) (pr-str (map :cue-id group))))
      (is (not-any? #(and (= :strobe-hit (:cue-id %)) (not= :strobe (:section %))) all)))

    (testing "quiet sections stay dark apart from at most one spotlight"
      (let [quiet (mapcat :frames (filter #(= :quiet (:section %)) (section-runs frames)))]
        (is (seq quiet))
        (is (every? #(<= (count (:lit %)) 1) quiet) (pr-str (distinct (map :lit quiet))))
        (is (some #(= :sweep (:cue-id %)) (filter #(= :quiet (:section %)) all)) "heads still move in the dark")))

    (testing "strobe contrast is abrupt: entering cuts every auto look and the washes strobe"
      (doseq [[before after] (partition 2 1 (section-runs frames))
              :when (#{:strobe :quiet} (:section after))
              :let [entry (first (:frames after))
                    prior (into #{} (keep (fn [[id i]] (when (auto? i) id))) (:instances (last (:frames before))))]]
        (is (not-any? prior (keys (:instances entry))) (str "cut into " (:section after))))
      (is (some #(seq (:strobing %)) (filter #(= :strobe (:section %)) frames)))
      (is (not-any? #(seq (:strobing %)) (filter #(#{:red :purple :quiet} (:section %)) frames))))

    (testing "Push performance stays available: the performer's spotlight survives every cut"
      (let [spot (:spot (last frames))
            held (filter #(>= (:t %) push-spot-ms) frames)]
        (is (string? spot))
        (is (every? #(= :running (get-in % [:instances spot :phase])) held))
        (is (not-any? #(and (= :spot (:cue-id %)) (some (fn [f] (and (>= (:t f) push-spot-ms) (contains? (:instances f) (:id %)))) frames))
                      all)
            "auto does not stack a second spotlight over the performer's")
        (is (every? #(contains? (:lit %) :moving-2) (filter #(>= (:t %) (+ push-spot-ms frame-ms)) frames)))))

    (testing "a stopped transport holds the planner and resumes without catching up"
      (let [held (filter #(< stop-ms (:t %) play-ms) frames)
            at-stop (some #(when (= stop-ms (:t %)) %) frames)
            resumed (filter #(< play-ms (:t %) (+ play-ms 500)) frames)]
        (is (every? #(and (= (:tick at-stop) (:tick %)) (= (:section at-stop) (:section %))) held))
        (is (<= (- (:tick (last resumed)) (:tick at-stop)) 1) "at most one decision in the first resumed beat")))))

(deftest an-unlaunchable-cue-is-dropped-instead-of-stalling-the-show
  ;; Before #217 a refusal met while predicting emission for movement escaped
  ;; the planner and failed every following show tick.
  (let [bad {:id :closed-shutters :version 1 :targets movers :outputs {:shutter 1.0 :dimmer 1.0 :color red}}
        rt (show/create-runtime {:model (-> (model) (update :registry registry/publish :effects bad)
                                            (update :registry registry/publish :cues {:id :bad :effect-id :closed-shutters}))
                                 :workspace-options nil})]
    (try
      (show/tick! rt 0)
      (let [reply (show/submit! rt :auto {:enabled? true :seed 3 :scope {:cue-ids [:sweep :bad :head-glow]}})]
        (show/tick! rt 0)
        (is (= :accepted (:status (deref reply 2000 nil)))))
      (let [seen (atom #{})
            refusals (into #{} (mapcat (fn [t]
                                         (show/tick! rt t)
                                         (swap! seen into (map :cue-id) (vals (get-in @(:state rt) [:model :instances])))
                                         (map #(select-keys % [:cue-id :code])
                                              (mapcat :refused (get-in @(:state rt) [:model :auto :reasons])))))
                           (range 125 4000 125))
            m (:model @(:state rt))]
        (is (= #{{:cue-id :bad :code :incapable-target}} refusals) "the refusal is reported, not thrown")
        (is (nil? (get-in @(:state rt) [:status :error])))
        (is (true? (get-in m [:auto :enabled?])))
        (is (= #{:sweep :head-glow} @seen) "movement still gets its light from the launchable companion"))
      (finally (show/stop! rt)))))
