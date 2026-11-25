(ns tolglow.qa.mixed-stage
  "Repeatable, disconnected fixture and cue load for GPU qualification.
  Capture one frame or pass a frame count for a consecutive 25 ms replay."
  (:require [afterglow.control.model :as control-model]
            [clojure.data.json :as json]
            [tolglow.application :as app]
            [tolglow.demo :as demo]
            [tolglow.rig.pixel-layout :as pixels]
            [tolglow.show :as show]
            [tolglow.venue-fog :as venue-fog]))

(def ^:private movers
  (mapv #(assoc % :groups [:beams] :max-watts 65 :beam-angle 15 :lens-radius-m 0.093)
        (demo/fixtures)))

(def ^:private washes
  (mapv (fn [i position]
          (assoc (nth movers i) :id (keyword (str "wash-" (inc i))) :name (str "Wash " (inc i))
                 :position position :groups [:washes] :fixture-type :wash
                 :max-watts 105 :beam-angle 64 :lens-radius-m 0.16 :address (+ 97 (* i 12))))
        (range) [[-4.35 3.0 -1.7] [4.35 3.0 -1.7] [-3.6 3.6 1.9]
                 [3.6 3.6 1.9] [-1.75 0.6 -3.05] [1.75 0.6 -3.05]]))

(def ^:private tube-layouts
  (mapv (fn [i x]
          {:id (keyword (str "tube-" (inc i))) :shape :path :group :tubes
           :universe 2 :address (+ 1 (* i 40)) :pixels 10 :color-layout :rgbw
           :diffuser-radius-m 0.034 :points [[x 0.18 0.50] [(+ x 1.5) 0.18 0.50]]})
        (range) [-3.6 -1.7 0.2 2.1]))

(defn- control [default lo hi]
  {:type :number :default default :bounds [lo hi] :ui/widget :fader})

(defn- expr [operation & args] {:op operation :args (vec args)})

(defn- looks [strip-fixtures]
  (let [c demo/constant
        p demo/param
        x {:op :position :axis :x}
        z {:op :position :axis :z}
        tick (expr :floor (expr :multiply (c 4) {:op :beat}))
        alternate (expr :floor (expr :multiply (c 2) (expr :fract (expr :multiply (c 0.5) tick))))
        chase-phase (expr :add (expr :floor (expr :multiply x (c 6)))
                           (expr :floor (expr :multiply tick (p :rate))))
        chase (expr :floor (expr :multiply (c 2) (expr :fract (expr :multiply (c 0.23) chase-phase))))]
    [{:id :beam-color :name "Violet shafts" :targets (mapv :id movers)
      :params {:level (control 0.84 0 1)}
      :outputs {:color [0.20 0.014 0.83] :dimmer (p :level) :focus 0.42}}
     {:id :beam-movement :name "Crossing sweep" :targets (mapv :id movers)
      :params {:width (control 1.5 0 3) :height (control 1.4 0.1 3.5)}
      :outputs {:aim (expr :vec3
                           (expr :add (expr :multiply x (c -0.35))
                                 (expr :multiply (p :width) (expr :sin (expr :add x {:op :beat}))))
                           (p :height) (c 0.9))}}
     {:id :wash-color :name "Copper wash" :targets (mapv :id washes)
      :params {:level (control 0.46 0 1) :tint (control 0.30 0 1)}
      ;; Keep every wash aimed onto the deck or back wall. The low fixtures
      ;; light the floor and haze upstage instead of shining into the audience.
      :outputs {:color (expr :vec3 (c 0.70)
                           (expr :add (c 0.15) (expr :multiply (c 0.52) (p :tint)))
                           (c 0.10))
                :dimmer (p :level)
                :aim (expr :vec3 (expr :multiply x (c 0.74)) (c 0.06)
                           (expr :add (c -2.60) (expr :multiply z (c 0.64))))}}
     {:id :wash-steps :name "Wash steps" :targets (mapv :id (take 2 washes))
      :priority 20 :params {:depth (control 0.62 0 1)}
      :outputs {:dimmer (expr :add (c 0.12) (expr :multiply (p :depth) alternate))}}
     {:id :tube-chase :name "Tube chase" :targets (mapv :id strip-fixtures)
      :params {:level (control 0.69 0 1) :rate (control 0.9 0.15 2)}
      :outputs {:color (expr :multiply (c [0.07 0.48 0.98]) (p :level)
                             (expr :add (c 0.20) (expr :multiply (c 0.80) chase)))}}
     {:id :stage-haze :name "Stage haze" :targets [:stage-hazer] :params {} :outputs {:fog 0.72}}
     {:id :stage-energy :name "Stage energy" :show-control? true
      :params {:brightness (control 0.46 0 1) :activity (control 0.39 0 1)
               :cap (control 0.46 0 1)}
      :outputs {}}]))

(defn- register-look [model {:keys [id name priority show-control?] :as effect}]
  (-> model
      (assoc-in [:registry :effects id] (assoc (dissoc effect :priority) :version 1))
      (assoc-in [:registry :cues id]
                (cond-> {:id id :version 1 :name name :effect-id id :priority (or priority 0)}
                  show-control? (assoc :show-control? true)))))

(defn- energy [param fallback] {:cue-id :stage-energy :param param :default fallback})

(defn model []
  (let [strip-fixtures (into [] (mapcat pixels/fixtures) tube-layouts)
        hazer {:id :stage-hazer :name "Upstage haze" :position [-3.4 0.32 -2.65]
               :address 180 :universe 1 :channels {:fog 1}
               :fogger {:flow-m3-min 78 :mixing-seconds 3.5}}
        effects (looks strip-fixtures)
        page {:id :stage :name "Mixed stage"
              :cells (into {} (map-indexed (fn [i {:keys [id]}] [[i 0] id])) effects)}
        rig {:room {:width 12 :height 5 :depth 10}
             :trusses [{:id :back-truss :position [0 4.15 -1.5] :length 9}]
             :fixtures (into (conj (into movers washes) hazer) strip-fixtures)
             :pixel-layouts tube-layouts}]
    (reduce register-look
            (assoc-in (control-model/new-show
                        {:show-id :mixed-stage :pages {:stage page} :rig rig
                         :macros {} :sequences {}
                         :settings {:autopilot {:enable-cue :stage-energy :rotate? false :seed 41
                                               :targets {:brightness (energy :brightness 0.46)
                                                         :activity (energy :activity 0.39)}}
                                    :lighting-budget {:enabled? true :global {:limit (energy :cap 1.0)
                                                                              :standby-level 0.015}}}})
                      ;; Repeat the same starting haze without waiting through a
                      ;; minute of physical pre-roll or changing emitter output.
                      venue-fog/saved-path {:mixed-m3 38.0 :sources []})
            effects)))

(def cue-ids [:beam-color :beam-movement :wash-color :wash-steps :tube-chase :stage-haze :stage-energy])

(def ^:private lfos
  [[:beam-color :level :sine 4 0.53 0.87]
   [:beam-movement :width :triangle 8 0.85 2.35]
   [:beam-movement :height :sine 5 0.75 2.05]
   [:wash-color :tint :sine 6 0.20 0.43]
   [:tube-chase :level :triangle 3 0.48 0.89]])

(defn- accepted! [receipt]
  (let [ack (deref receipt 5000 {:status :timeout})]
    (when-not (= :accepted (:status ack))
      (throw (ex-info "Mixed-stage command failed" {:ack ack})))
    ack))

(defn start-look!
  "Launch through the shared command queue. Supply a clock tick for a manual host."
  [runtime advance!]
  (let [receipts (into {} (map (fn [id] [id (show/submit! runtime {:op :cue/launch :cue-id id})])) cue-ids)
        _ (advance!)
        ids (update-vals receipts (comp :instance-id accepted!))
        bindings (mapv (fn [[cue param waveform beats from to]]
                         (show/submit! runtime :modulation-bind
                           {:instance-id (ids cue) :param param
                            :binding {:type :lfo :waveform waveform :period {:beats beats}
                                      :phase-reference :transport :from from :to to}}))
                       lfos)]
    (advance!)
    (run! accepted! bindings)
    ids))

(defn- preview-frame [snapshot]
  (-> (select-keys snapshot [:show-id :epoch :time-ms :frame-number :revision])
      (assoc :output (select-keys (:output snapshot) [:atmosphere :values]))))

(defn- capture! [file frame-count]
  (let [runtime (show/create-runtime {:model (model) :workspace-options nil})
        t (atom -25)
        tick #(show/tick! runtime (swap! t + 25))]
    (try
      (start-look! runtime tick)
      (while (< @t 8000) (tick))
      (let [snapshot (show/snapshot runtime)
            first-frame (preview-frame snapshot)
            capture (if frame-count
                      {:rig (:rig snapshot)
                       :frames (into [first-frame]
                                     (repeatedly (dec frame-count) #(do (tick) (preview-frame (show/snapshot runtime)))))}
                      (assoc first-frame :rig (:rig snapshot)))]
        (spit file (json/write-str capture)))
      (finally (show/stop! runtime) (shutdown-agents)))))

(defn- serve! []
  (let [host (app/start! {:init {:components [:show] :modules [:web-server]}
                          :show {:model (model) :workspace-options nil}
                          :web-server {:host "127.0.0.1" :port 0}})
        port (get-in host [:application :modules :web-server :port])]
    (try
      (start-look! host (constantly nil))
      (println (str "Mixed-stage Studio listening on http://127.0.0.1:" port))
      (flush)
      (let [finished (promise)
            stop (Thread. ^Runnable (fn [] (app/stop! host) (deliver finished true)))]
        (.addShutdownHook (Runtime/getRuntime) stop)
        @finished)
      (catch Exception error
        (app/stop! host)
        (throw error)))))

(defn -main [argument & [count-text]]
  (if (= "studio" argument)
    (serve!)
    (let [frame-count (some-> count-text parse-long)]
      (when (and count-text (not (pos-int? frame-count)))
        (throw (ex-info "Frame count must be a positive integer" {:frame-count count-text})))
      (capture! argument frame-count))))
