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
            [tolglow.automation.policy :as policy]
            [tolglow.registry :as registry]
            [tolglow.automation.red-show :as red-show :refer [red deep-red muted-purple movers model auto-config]]
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

(defn- pan-bytes
  "Coarse pan bytes of the sweeping heads."
  [universes]
  (let [^bytes dmx (get universes 1)]
    (mapv (fn [{:keys [address channels]}]
            (bit-and 0xff (aget dmx (+ address (dec (first (:pan channels))) -1))))
          (filter #(some #{(:id %)} movers) (gig/fixtures)))))

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
                                 :pan (pan-bytes universes)
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
        (is (some #(= :sweep (:cue-id %)) (filter #(= :quiet (:section %)) all)) "heads still move in the dark")
        (let [swept (filter (fn [f] (some #(and (= :sweep (:cue-id %)) (= :running (:phase %))) (vals (:instances f)))) quiet)]
          (is (seq swept))
          (is (< 3 (count (distinct (map :pan swept)))) "pan DMX of the dark heads keeps changing"))))

    (testing "strobe contrast is abrupt: entering cuts every auto look and the washes strobe"
      (doseq [[before after] (partition 2 1 (section-runs frames))
              :when (#{:strobe :quiet} (:section after))
              :let [entry (first (:frames after))
                    prior (into #{} (keep (fn [[id i]] (when (auto? i) id))) (:instances (last (:frames before))))]]
        (is (not-any? prior (keys (:instances entry))) (str "cut into " (:section after))))
      (is (every? #(seq (:strobing %)) (filter #(= :strobe (:section %)) frames)) "every strobe-section frame strobes")
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

(defn- runtime [model] (show/create-runtime {:model model :workspace-options nil}))

(defn- submit!! [rt t type payload]
  (let [reply (show/submit! rt type payload)]
    (show/tick! rt t)
    (deref reply 2000 ::missing)))

(defn- auto-of [rt] (get-in @(:state rt) [:model :auto]))

(def bad-ids (mapv #(keyword (str "bad-" %)) (range 10)))

(defn- with-bad-cues
  "Ten cues that write a shutter to heads without one: the show refuses each."
  [model]
  (reduce (fn [m id]
            (-> m (update :registry registry/publish :effects {:id id :version 1 :targets movers
                                                              :outputs {:shutter 1.0 :dimmer 1.0 :color red}})
                (update :registry registry/publish :cues {:id id :effect-id id})))
          model bad-ids))

(deftest unlaunchable-cues-are-dropped-instead-of-stalling-the-show
  ;; Before #217 a refusal met while predicting emission for movement escaped
  ;; the planner and failed every following show tick; a first fix still gave
  ;; up once half the pool was refused. Here ten of twelve cues are refused.
  (let [met (atom [])]
   (doseq [seed [3 5 11 17 23]]
    (let [rt (runtime (with-bad-cues (model)))]
      (try
        (show/tick! rt 0)
        (is (= :accepted (:status (submit!! rt 0 :auto {:enabled? true :seed seed
                                                        :scope {:cue-ids (into [:sweep :head-glow] bad-ids)}}))))
        (let [seen (atom #{})
              ;; The first decision runs on the configure tick.
              refusals (atom (mapv :cue-id (mapcat :refused (:reasons (auto-of rt)))))]
          (doseq [t (range 125 8000 125)]
            (let [tick (get-in (auto-of rt) [:state :tick])]
              (show/tick! rt t)
              (swap! seen into (map :cue-id) (vals (get-in @(:state rt) [:model :instances])))
              ;; Reasons stay visible between decisions; read them once per decision.
              (when (not= tick (get-in (auto-of rt) [:state :tick]))
                (swap! refusals into (map :cue-id) (mapcat :refused (:reasons (auto-of rt)))))))
          (is (nil? (get-in @(:state rt) [:status :error])))
          (is (true? (:enabled? (auto-of rt))) (pr-str (:reasons (auto-of rt))))
          (is (seq @seen))
          (is (every? #{:sweep :head-glow} @seen) "only launchable cues run")
          (swap! met conj (count @refusals))
          (is (every? (set bad-ids) @refusals))
          (is (= (count @refusals) (count (distinct @refusals))) "each refusal is met once, then remembered")
          (is (= (set @refusals) (set (keys (get-in (auto-of rt) [:state :refusals]))))))
        (finally (show/stop! rt)))))
   (is (some #(>= % 6) @met) (str "most of the pool was refused in some run " @met))))

(deftest a-performer-takes-over-an-auto-look-through-the-show-owner
  (let [rt (runtime (model))]
    (try
      (show/tick! rt 0)
      (submit!! rt 0 :auto {:enabled? true :seed 1 :scope {:cue-ids [:wash-color]} :interval {:bars 1}
                            :launch {:mode :persistent}})
      (doseq [t (range 125 2000 125)] (show/tick! rt t))
      (let [owned (get-in (auto-of rt) [:state :owned-instance-ids])
            id (first owned)]
        (is (= 1 (count owned)))
        (is (= :accepted (:status (submit!! rt 2000 :command {:op :param/set :instance-id id :param :level :value 0.3
                                                             :source {:adapter :push :session-id "push-3"}}))))
        (doseq [t (range 2125 4500 125)] (show/tick! rt t))
        (is (not-any? #{id} (get-in (auto-of rt) [:state :owned-instance-ids])))
        (let [others (get-in (auto-of rt) [:state :owned-instance-ids])]
          (is (seq others) "auto keeps launching its own looks")
          (is (= :accepted (:status (submit!! rt 4500 :auto {:enabled? false}))))
          (let [instances (get-in @(:state rt) [:model :instances])]
            (is (= :running (get-in instances [id :phase])) "Stop auto leaves the performer's look")
            (is (= 0.3 (get-in instances [id :params :level])))
            (is (every? #(not= :running (get-in instances [% :phase])) others) "auto's own looks end"))))
      (finally (show/stop! rt)))))

(deftest studio-stop-and-start-keep-the-repl-palette
  (let [rt (runtime (model))]
    (try
      (show/tick! rt 0)
      (submit!! rt 0 :auto (auto-config 217))
      ;; Studio's Stop, then Start with Vary ticked; Studio has no palette editor.
      (submit!! rt 125 :auto {:enabled? false :keep-plan? true})
      (is (= red-show/red-palette (get-in (auto-of rt) [:config :randomize :palette])))
      (submit!! rt 250 :auto {:enabled? true :seed 42 :interval {:beats 8} :count 1 :scope {:page-id red-show/page-id}
                              :launch {:mode :timed :duration {:beats 4}} :randomize {:params "marked"} :keep-plan? true})
      (let [config (:config (auto-of rt))]
        (is (true? (:enabled? (auto-of rt))))
        (is (= [:red :purple :quiet :strobe] (mapv :id (:sections config))))
        (is (= {:params :marked :palette red-show/red-palette} (:randomize config))))
      ;; Start with Vary unticked keeps the palette for later, varying nothing.
      (submit!! rt 375 :auto {:enabled? true :seed 42 :scope :all :keep-plan? true})
      (is (= {:params :none :palette red-show/red-palette} (get-in (auto-of rt) [:config :randomize])))
      (finally (show/stop! rt)))))

(deftest section-scopes-are-refused-when-configured
  (let [rt (runtime (model))]
    (try
      (show/tick! rt 0)
      (doseq [[scope message] [[:nope "Scope is :all, {:page-id id} or {:cue-ids [ids]}"]
                               [{:page-id :gone} "No saved page :gone"]]]
        (let [ack (submit!! rt 0 :auto {:enabled? true :seed 1 :launch {:mode :persistent}
                                        :sections [{:id :a :length {:bars 1} :config {:scope {:cue-ids [:wash-color]}}}
                                                   {:id :b :weight 3 :length {:bars 1} :config {:scope scope}}]})]
          (is (= :rejected (:status ack)))
          (is (re-find (re-pattern (java.util.regex.Pattern/quote message)) (str (:message ack))) (:message ack))
          (is (re-find #"\[:sections 1 :config :scope\]" (str (:message ack))))))
      (is (nil? (:auto (:model @(:state rt)))) "nothing was committed")
      (finally (show/stop! rt)))))

(deftest a-planner-failure-ends-auto-looks-and-keeps-the-show-running
  (let [rt (runtime (model))]
    (try
      (show/tick! rt 0)
      (submit!! rt 0 :auto {:enabled? true :seed 1 :scope {:cue-ids [:wash-color]} :interval {:beats 1}
                            :launch {:mode :persistent}})
      (doseq [t (range 125 2000 125)] (show/tick! rt t))
      (let [owned (get-in (auto-of rt) [:state :owned-instance-ids])
            advance policy/advance]
        (is (<= 3 (count owned)))
        ;; Fault injection at the planner boundary: a non-ExceptionInfo error.
        (with-redefs [policy/advance (fn [& args] (if (>= (nth args 3) 2500) (throw (NullPointerException.)) (apply advance args)))]
          (doseq [t (range 2125 3500 125)] (show/tick! rt t)))
        (let [m (:model @(:state rt)) auto (:auto m)]
          (is (nil? (get-in @(:state rt) [:status :error])))
          (is (false? (:enabled? auto)))
          (is (= :planner-failed (get-in auto [:reasons 0 :reason])))
          (is (<= (count owned) (get-in auto [:reasons 0 :released])))
          (is (not-any? #(and (= :auto (get-in % [:source :adapter])) (= :running (:phase %))) (vals (:instances m)))
              "no auto look keeps running while the panel reads off")
          (is (> (:time-ms (:transport m)) 3000) "the show kept ticking")))
      (finally (show/stop! rt)))))

(deftest a-pan-wrap-refusal-is-retried-once-the-head-moves-back
  ;; :start-wrap-mismatch depends on the head's current pan, not on the cue, so
  ;; auto reports it for that decision only and launches the path once the
  ;; head is back on the published wrap.
  (let [head {:id :far-travel :universe 1 :address 1 :position [0 0 0]
              :channels {:pan [1 3] :tilt [2 4]}
              :calibration {:motor-rate-deg-per-sec {:pan 90 :tilt 120} :pan {:min-deg -270 :max-deg 270}}}
        rt (show/create-runtime {:workspace-options nil :sink {:send! (fn [_ _])}
                                 :model (afterglow.control.model/new-show {:rig {:fixtures [head]}
                                                                           :transport {:playing? true :beat 0.0 :bpm 120.0}})})
        point (fn [yaw] (let [r (Math/toRadians yaw)] [(Math/sin r) 0 (Math/cos r)]))
        path! (fn [t spec] (is (= :accepted (:status (submit!! rt t :aim-path spec))) (pr-str spec)))]
    (try
      (show/tick! rt 0)
      ;; Published from the neutral pose, the start resolves to the -170 degree wrap.
      (path! 0 {:id :wrapped :targets [:far-travel] :points [(point -170) (point -150)] :sync-arrival? true})
      (path! 0 {:id :out :targets [:far-travel] :mode :once :duration {:ms 1000} :points (mapv point [-90 0 90 150 190])})
      (path! 0 {:id :back :targets [:far-travel] :mode :once :duration {:ms 1000} :points (mapv point [190 150 90 0 -90 -160])})
      (let [out (:instance-id (submit!! rt 0 :command {:op :cue/launch :cue-id :out :launch {:mode :persistent}}))]
        (doseq [t (range 125 1300 125)] (show/tick! rt t))
        (is (= :accepted (:status (submit!! rt 1300 :auto {:enabled? true :seed 1 :intent :darkness :scope {:cue-ids [:wrapped]}
                                                           :interval {:beats 2} :launch {:mode :timed :duration {:beats 2}}}))))
        (let [refused (mapcat :refused (:reasons (auto-of rt)))]
          (is (= [:start-wrap-mismatch] (mapv :code refused)) (pr-str (:reasons (auto-of rt))))
          (is (empty? (get-in (auto-of rt) [:state :refusals])) "a pose-dependent refusal is not remembered"))
        ;; The performer brings the head back to the published wrap.
        (submit!! rt 1400 :command {:op :instance/end :instance-id out :force? true})
        (submit!! rt 1400 :command {:op :cue/launch :cue-id :back :launch {:mode :persistent}})
        (doseq [t (range 1500 4500 125)] (show/tick! rt t))
        (is (some #(and (= :wrapped (:cue-id %)) (= :auto (get-in % [:source :adapter])))
                  (vals (get-in @(:state rt) [:model :instances])))
            (pr-str (:reasons (auto-of rt)))))
      (finally (show/stop! rt)))))

(deftest deleting-a-scoped-cue-never-blocks-a-backup
  (let [rt (runtime (model))]
    (try
      (show/tick! rt 0)
      (submit!! rt 0 :auto (auto-config 217))
      (is (= :accepted (:status (submit!! rt 125 :command {:op :cue/delete :cue-id :accent}))))
      (doseq [t (range 250 20000 125)] (show/tick! rt t))
      (is (true? (:enabled? (auto-of rt))))
      (is (nil? (get-in @(:state rt) [:status :error])))
      (let [document (show/export-show rt)]
        (is (map? document) "the backup is written")
        (is (some #(= :accent (get-in % [:config :scope :cue-ids 3])) (get-in document [:model :auto :config :sections]))
            "the plan is saved as configured"))
      (finally (show/stop! rt)))))

(deftest a-show-restored-with-auto-off-can-still-stop-its-looks
  (let [rt (runtime (model))]
    (try
      (show/tick! rt 0)
      (submit!! rt 0 :auto {:enabled? true :seed 2 :scope {:cue-ids [:wash-color :accent]} :interval {:beats 1}
                            :launch {:mode :persistent}})
      (doseq [t (range 125 2500 125)] (show/tick! rt t))
      (let [document (-> (show/export-show rt)
                         ;; As if saved by a build whose intent this planner does not have.
                         (assoc-in [:model :auto :config :intent] :bright))
            _ (is (= :accepted (:status (submit!! rt 2500 :import {:source (pr-str document)}))))
            restored (auto-of rt)
            auto-looks #(filterv (fn [i] (and (= :auto (get-in i [:source :adapter])) (= :running (:phase i))))
                                 (vals (get-in @(:state rt) [:model :instances])))]
        (is (false? (:enabled? restored)))
        (is (some #(= :restored-disabled (:reason %)) (:reasons restored)))
        (is (seq (auto-looks)) "the looks came back, still owned by auto")
        (is (= (set (map :id (auto-looks))) (set (get-in restored [:state :owned-instance-ids]))))
        (is (= :accepted (:status (submit!! rt 2625 :auto {:enabled? false}))))
        (is (empty? (auto-looks)) "Stop ends them"))
      (finally (show/stop! rt)))))

(deftest a-sections-bad-launch-params-do-not-suppress-the-cue-elsewhere
  ;; :bad launches :wash-color with a level outside its bounds; the show refuses
  ;; that launch. The refusal belongs to the section's params, so :good still
  ;; launches the cue.
  (let [rt (runtime (model))
        sections (atom [])]
    (try
      (show/tick! rt 0)
      (is (= :accepted (:status (submit!! rt 0 :auto {:enabled? true :seed 4 :scope {:cue-ids [:wash-color]} :interval {:beats 1}
                                                     :sections [{:id :bad :length {:bars 1} :max-run 1 :transition :cut
                                                                 :config {:launch {:mode :timed :duration {:beats 1} :params {:level 2.0}}}}
                                                                {:id :good :length {:bars 1} :max-run 1 :transition :cut
                                                                 :config {:launch {:mode :timed :duration {:beats 1}}}}]}))))
      (let [launched-in (atom #{}) refused-in (atom #{})]
        (doseq [t (range 0 24000 125)]
          (let [tick (get-in (auto-of rt) [:state :tick])]
            (show/tick! rt t)
            (let [auto (auto-of rt) section (get-in auto [:state :section :id])]
              (swap! sections conj section)
              (when (not= tick (get-in auto [:state :tick]))
                (when (some #(= :launch-refused (:reason %)) (:reasons auto)) (swap! refused-in conj section)))
              (when (some #(and (= :wash-color (:cue-id %)) (= :auto (get-in % [:source :adapter])))
                          (vals (get-in @(:state rt) [:model :instances])))
                (swap! launched-in conj section)))))
        (is (= #{:bad :good} (set @sections)))
        (is (= #{:bad} @refused-in) "the bad section's launches are refused")
        (is (contains? @launched-in :good) "the good section still launches the cue")
        (is (empty? (get-in (auto-of rt) [:state :refusals])) "nothing is remembered against the cue"))
      (finally (show/stop! rt)))))

(deftest settings-from-another-build-survive-a-studio-stop
  (let [rt (runtime (model))]
    (try
      (show/tick! rt 0)
      (submit!! rt 0 :auto (auto-config 217))
      (doseq [t (range 125 1000 125)] (show/tick! rt t))
      (let [document (assoc-in (show/export-show rt) [:model :auto :config :newer-setting] {:instance-ids ["0:1" "zz"]})]
        (is (= :accepted (:status (submit!! rt 1000 :import {:source (pr-str document)}))))
        (is (= {:config {:newer-setting {:instance-ids ["0:1" "zz"]}}} (:unrecognized (auto-of rt))))
        ;; Studio's Stop keeps the plan, and with it the set-aside settings.
        (is (= :accepted (:status (submit!! rt 1125 :auto {:enabled? false :keep-plan? true}))))
        (is (= {:instance-ids ["0:1" "zz"]} (get-in (show/export-show rt) [:model :auto :config :newer-setting]))))
      (finally (show/stop! rt)))))
