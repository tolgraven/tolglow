(ns tolglow.legacy-frame-oracle.capture
  "Capture one pass of the historical show frame loop in an isolated JVM."
  (:require [afterglow.effects :as effects]
            [afterglow.effects.channel :as channel]
            [afterglow.effects.color :as color]
            [afterglow.effects.movement :as movement]
            [afterglow.rhythm :as rhythm]
            [afterglow.show :as show]
            [afterglow.show-context :refer [with-show]]
            [ola-clojure.ola-service :as ola]
            [overtone.at-at :as clock]
            [thi.ng.color.core :as color-value]
            [tolglow.color :as author-color]
            [tolglow.fixtures :as fixture]
            [tolglow.legacy-frame-oracle.scenario :as scenario])
  (:import [com.google.protobuf ByteString]
           [java.util IdentityHashMap Random]
           [javax.vecmath Point3d]))

(defn- forbidden-start [& _]
  (throw (ex-info "The offline capture attempted to start a show" {})))

(defn- authored-colors []
  (let [rng (Random. scenario/seed)
        random (fn
                 ([] (.nextDouble rng))
                 ([limit] (* limit (.nextDouble rng))))]
    (with-redefs [clojure.core/rand random]
      (into (sorted-map)
            (for [[id {:keys [color hue-spread]}] (sort-by key scenario/palette)]
              [id (author-color/like (name color) :h-spread hue-spread
                                     :s-spread 0.0 :l-spread 0.0)])))))

(defn- offline-show [metronome]
  (let [{:keys [id universe address position]} scenario/fixture
        [x y z] position
        running-show (show/show :universes [universe] :base-metronome metronome)]
    (with-show running-show
      (show/patch-fixture! id (fixture/rgbw-60-moving-beam) universe address :x x :y y :z z))
    (reset! (:pool running-show) :serial)
    running-show))

(defn- effect [id colors]
  (let [{:keys [kind attribute dmx aim blend percent]} (scenario/cues id)
        fixtures (show/all-fixtures)
        label (name id)]
    (case kind
      :color (color/color-effect label (colors id) fixtures :htp? (= blend :max))
      :aim (movement/aim-effect label (apply #(Point3d. %1 %2 %3) aim) fixtures)
      :channel (channel/channel-effect label dmx
                                       (filter #(= attribute (:color %)) (:channels (first fixtures))))
      :function (channel/function-effect label
                                         ({:dimmer :dimmer :shutter :shutter-open} attribute)
                                         percent fixtures))))

(defn- logical-target [{:keys [kind target-id]}]
  (case kind
    :channel target-id
    :function [(:id scenario/fixture) (second target-id)]
    (:id scenario/fixture)))

(defn- portable-assignment-value [{:keys [kind value]}]
  (case kind
    :color (mapv double (take 3 @(color-value/as-rgba value)))
    :aim (let [^Point3d point value] [(.-x point) (.-y point) (.-z point)])
    value))

(defn- trace-generation [effect label ^IdentityHashMap sources]
  (update effect :gen-fn
          (fn [generate]
            (fn [show snapshot]
              (let [assigners (vec (generate show snapshot))]
                (doseq [assigner assigners] (.put sources assigner label))
                assigners)))))

(defn- ordering [running-show]
  (mapv :key (:meta @(:active-effects running-show))))

(defn- capture []
  (let [now (atom 0)
        universes (atom nil)
        reductions (atom nil)
        resolved (atom nil)
        sources (IdentityHashMap.)
        run-assigners effects/run-assigners
        resolve-assignment effects/resolve-assignment
        send-frame (ns-resolve 'afterglow.show 'send-frame)
        create-buffers (ns-resolve 'afterglow.show 'create-buffers)]
    (with-redefs [clock/now (fn [] @now)
                  show/start! forbidden-start
                  ola/UpdateDmxData (fn [{:keys [universe ^ByteString data]} _]
                                      (when (contains? @universes universe)
                                        (throw (ex-info "Duplicate output for a universe" {:universe universe})))
                                      (swap! universes assoc universe (scenario/unsigned-bytes (.toByteArray data))))
                  effects/run-assigners (fn [show snapshot assigners before]
                                          (let [assignment (run-assigners show snapshot assigners before)
                                                labels (mapv #(.get sources %) assigners)]
                                            (when (some nil? labels)
                                              (throw (ex-info "Unattributed legacy assigner" {:kind (:kind assignment)})))
                                            (swap! reductions conj {:kind (:kind assignment)
                                                                    :target (logical-target assignment)
                                                                    :sources labels
                                                                    :value (portable-assignment-value assignment)})
                                            assignment))
                  effects/resolve-assignment (fn [assignment show snapshot buffers]
                                               (swap! resolved conj [(:kind assignment) (logical-target assignment)])
                                               (resolve-assignment assignment show snapshot buffers))]
      (let [metronome (rhythm/metronome 120 :bpb 4 :bpp 4)
            colors (authored-colors)
            initial (offline-show metronome)]
        (loop [operations scenario/operations, running-show initial, buffers (create-buffers initial)
               epoch 0, launches {}, first-launches {}, history [], frames []]
          (if-let [[op time-ms label :as input] (first operations)]
            (do
              (reset! now time-ms)
              (case op
                :frame
                (let [snapshot (rhythm/metro-snapshot metronome time-ms)]
                  (.clear sources)
                  (reset! reductions [])
                  (reset! resolved [])
                  (reset! universes (sorted-map))
                  (with-show running-show (send-frame running-show buffers snapshot))
                  (when-not (= #{(:universe scenario/fixture)} (set (keys @universes)))
                    (throw (ex-info "Missing captured universe" {:universes (keys @universes)})))
                  (recur (next operations) running-show buffers epoch launches first-launches history
                         (conj frames {:label label :epoch epoch :time-ms time-ms
                                       :snapshot (select-keys snapshot [:instant :beat :beat-phase :bar :bar-phase])
                                       :active (ordering running-show)
                                       :reductions @reductions :resolution-order @resolved :universes @universes})))
                :launch
                (let [instance (with-show running-show
                                 (show/add-effect! label (trace-generation (effect label colors) label sources)
                                                   :priority (:priority (scenario/cues label))))]
                  (recur (next operations) running-show buffers epoch (assoc launches label instance)
                         (if (contains? first-launches label) first-launches (assoc first-launches label instance))
                         (conj history {:input input :epoch epoch :instance instance :active (ordering running-show)}) frames))
                (:end :end-stale)
                (let [instance ((if (= op :end-stale) first-launches launches) label)
                      before (ordering running-show)]
                  (with-show running-show (show/end-effect! label :force true :when-id instance))
                  (recur (next operations) running-show buffers epoch launches first-launches
                         (conj history {:input input :epoch epoch :instance instance
                                        :outcome (if (= before (ordering running-show)) :ignored :ended)
                                        :active (ordering running-show)}) frames))
                :restart
                (let [fresh (offline-show metronome)]
                  (with-show running-show (show/clear-effects!))
                  (recur (next operations) fresh (create-buffers fresh) (inc epoch) {} first-launches
                         (conj history {:input input :epoch (inc epoch) :active []}) frames))))
            {:source scenario/legacy-source :seed scenario/seed :fixture scenario/fixture
             :authored-rgb (into (sorted-map)
                                 (for [[id value] colors]
                                   [id (mapv double (take 3 @(color-value/as-rgba value)))]))
             :history history :frames frames}))))))

(defn -main [& [output]]
  (when-not output (throw (ex-info "Supply the capture output path" {})))
  (try (scenario/write-edn! output (capture))
       (finally (shutdown-agents))))
