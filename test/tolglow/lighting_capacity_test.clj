(ns tolglow.lighting-capacity-test
  (:require [afterglow.render.frame :as frame]
            [clojure.test :refer [deftest is testing]]
            [tolglow.demo :as demo]
            [tolglow.compiler :as compiler]
            [tolglow.output.pause-look :as pause]
            [tolglow.registry :as registry]
            [tolglow.integrations.analysis :as analysis]
            [tolglow.lighting-capacity :as capacity]
            [tolglow.persistence :as persistence]
            [tolglow.show :as show]))

(def rgb {:id :wash :universe 1 :address 511 :channels {:red 1 :green 2 :blue 3}
          :max-watts 120 :measured-relative-brightness 2.0})
(def lamp {:id :lamp :universe 2 :address 2 :channels {:dimmer {:offset 1 :fine-offset 2 :inverted-from 0}}})
(def fog {:id :fogger :universe 2 :address 4 :channels {:fog 1 :fan 2 :dimmer 3} :max-watts 1200})

(defn near? [expected actual] (and (number? actual) (< (abs (- expected actual)) 0.006)))

(deftest measure-the-completed-universes
  (let [patch (frame/compile-patch [rgb lamp fog])
        plan (capacity/prepare patch)
        measure (fn [color d master]
                  (let [frame (frame/resolve-frame patch [{:target-id :wash :attribute :color :value color}
                                                          {:target-id :lamp :attribute :dimmer :value d}
                                                          {:target-id :fogger :attribute :fog :value 1}]
                                                   {:master master :compact-values? true :include-values? false})]
                    (capacity/measure plan (:universes frame))))
        white (measure [1 1 1] 1 1) red (measure [1 0 0] 0 1)]
    (is (= 2 (:heads-count white)))
    (is (= 1 (:rated-heads white)))
    (is (near? 3 (:relative-capacity white)))
    (is (near? 120 (:rated-watts white)))
    (is (near? 120 (:used-rated-watts white)))
    (is (near? 1 (:utilization white)))
    (is (near? 0 (:headroom white)))
    (is (= [1.0 1.0 1.0] (:color white)))
    (is (near? (/ (* 2 0.321) (+ 0.321 0.793 0.074)) (:relative-output red)))
    (is (near? 40 (:used-rated-watts red)))
    (is (= [1.0 0.0 0.0] (:color red)))
    (is (near? 0.501 (:utilization (measure [1 1 1] 1 0.501))))
    (is (near? 0 (:utilization (measure [1 1 1] 1 0))))
    (is (near? 1 (:headroom (capacity/measure (capacity/prepare (frame/compile-patch [])) {}))))))

(defn signal [amplitude]
  (:features (analysis/analyze (analysis/new-analyzer {})
              {:samples (double-array (map #(* amplitude (Math/sin (* % (/ (* 2 Math/PI 100) 48000)))) (range 4800)))
               :sample-rate 48000.0 :started-at-ms 0.0 :received-at-ms 100.0})))

(defn white-look []
  (let [m (demo/model)]
    (-> m (assoc-in [:rig :fixtures] [rgb])
        (assoc-in [:registry :effects :ember :outputs] {:color (demo/constant [1 1 1])})
        (update-in [:registry :effects :ember] dissoc :envelope))))

(deftest audio-limits-an-authored-look-on-the-normal-frame-path
  (doseq [batch? [true false]]
    (let [rt (show/create-runtime {:model (white-look) :batch? batch?})
          now (atom 0)
          tick (fn [] (show/tick! rt (swap! now + 25)) (get-in (show/snapshot rt) [:output :lighting-capacity]))
          issue (fn [type payload]
                  (let [ack (show/submit! rt type payload)] (tick) (deref ack)))
          features! (:features! (show/audio-callbacks rt))
          feed (fn [a] (features! (signal a)))
          data (signal 0.25)]
      (try
        (is (nil? (tick)))
        (is (= :rejected (:status (issue :settings {:scope :lighting-capacity :values {:minimum 0.8 :maximum 0.2}}))))
        (is (= :accepted (:status (issue :settings {:scope :lighting-capacity
                       :values {:enabled? true :follow-energy? true :minimum 0.0 :maximum 1.0
                                :reference-rms (* 2 (:audio/energy data)) :signal-timeout-ms 500}}))))
        (is (= :no-signal (:mode (tick))))
        (is (true? (get-in (show/export-show rt) [:model :settings :lighting-capacity :follow-energy?])))
        (feed 0.25)
        (is (zero? (:utilization (tick))) "Audio may scale a look, never invent lit heads")
        (is (= :accepted (:status (issue :command {:op :cue/launch :cue-id :ember}))))
        (let [series (repeatedly 6 tick)]
          (doseq [summary series]
            (is (= :following (:mode summary)))
            (is (near? 0.5 (:target summary)))
            (is (near? 0.5 (:utilization summary)))
            (is (near? 0.5 (:gain summary)))
            (is (= [:ember] (mapv :effect-id (:writers summary))))))
        (feed 0.0)
        (is (zero? (:utilization (tick))) "A fresh silent block is valid low energy")
        (is (= :accepted (:status (issue :command {:op :blackout/set :value true}))))
        (feed 0.25)
        (is (= :bypassed (:mode (tick))))
        (is (zero? (:utilization (tick))))
        (issue :command {:op :blackout/set :value false})
        (issue :command {:op :master/set :value 0.2})
        (is (near? 0.2 (:utilization (tick))))
        (is (= 0.2 (:master (show/snapshot rt))))
        (swap! now + 510)
        (is (= :no-signal (:mode (tick))))
        (issue :settings {:scope :lighting-capacity :values {:follow-energy? false}})
        (feed 0.0)
        (is (= :manual (:mode (tick))))
        (is (near? 0.2 (:utilization (tick))))
        (finally (show/stop! rt))))))

(deftest foreground-chroma-and-muted-writers
  (let [patch (frame/compile-patch [rgb])
        plan (capacity/prepare patch)
        block (fn [id color opacity] {:kind :assignments :source {:effect-id id}
                                       :assignments [{:target-id :wash :attribute :color
                                                       :value color :opacity opacity}]})
        blocks [(block :white [1 1 1] 1) (block :red [1 0 0] 1) (block :muted [0 0 1] 0)]
        frame (frame/resolve-contributions patch blocks {:master 0.4})]
    (is (= [1.0 0.0 0.0] (:color (capacity/measure plan (:universes frame)))))
    (is (= [:red] (mapv :effect-id (capacity/writers plan blocks))))))

(deftest fog-demand-is-selected-separately-from-light-energy
  (let [rt (show/create-runtime {:model (assoc-in (white-look) [:rig :fixtures] [rgb fog])})
        now (atom 0)
        tick (fn [t] (reset! now t) (show/tick! rt t)
               (bit-and 255 (aget ^bytes (get-in @(:state rt) [:output :universes 2]) 3)))
        configure (fn [values] (let [ack (show/submit! rt :settings {:scope :lighting-capacity :values values})]
                                 (tick (inc @now)) (is (= :accepted (:status @ack)))))
        feed (:features! (show/audio-callbacks rt))]
    (try
      (configure {:enabled? true :follow-energy? true :reference-rms 0.2 :fog-fixture-ids [:fogger]})
      (feed (signal 0.6))
      (is (zero? (tick 25)))
      (configure {:fog? true :fog-fixture-ids []})
      (is (zero? (tick 50)))
      (configure {:fog-fixture-ids [:fogger]})
      (is (= 128 (tick 75)))
      (is (zero? (tick 490)) "Pulse ends even under a sustained peak")
      (feed (signal 0))
      (is (zero? (tick 500)))
      (feed (signal 0.6))
      (is (zero? (tick 525)) "A second peak inside the duty rest cannot trigger")
      (is (zero? (tick 7000)) "Stale audio never starts fog")
      (feed (signal 0.6))
      (is (= 128 (tick 7025)))
      (configure {:fog? false})
      (is (zero? (tick 7050)) "Disarming cancels a pending demand on the next commit")
      (finally (show/stop! rt)))))


(deftest modifier-only-native-light-writers
  (let [fixture (assoc rgb :channels {:red 1 :green 2 :blue 3 :pan 4 :tilt 5})
        patch (frame/compile-patch [fixture])
        plan (capacity/prepare patch)
        source {:kind :assignments :assignments [{:target-id :wash :attribute :color :value [0.8 0.2 0.1]}]
                :source {:effect-id :base} :native-layer {:id :look :member 0}}
        modifier (fn [id member attr expr opacity]
                   {:source {:effect-id id} :kind :assignments :assignments []
                    :native-layer {:id :look :member member :targets :previous :member-opacity opacity
                                   :transforms [{:attribute attr :value expr}]}})
        fade {:op :multiply :args [{:op :previous :attribute :color} 0.5]}
        blocks [source (modifier :fade 1 :color fade 0.75)
                (modifier :muted 2 :color fade 0.0)
                (modifier :aim 3 :pan 0.5 1.0)]
        output (frame/resolve-contributions patch blocks {})]
    (is (= [:base :fade] (mapv :effect-id (capacity/writers plan blocks))))
    (is (near? 0.5 (/ (bit-and 255 (aget ^bytes (get-in output [:universes 1]) 510)) 255.0)))
    (is (near? 0.25 (second (:color (capacity/measure plan (:universes output))))))))

(deftest warm-cold-white-and-ultraviolet
  (let [fixtures [{:id :white :universe 1 :address 1 :channels {:dimmer 1 :warm-white 2 :cold-white 3}
                   :measured-relative-brightness 1 :max-watts 100}
                  {:id :uv :universe 1 :address 4 :channels {:uv 1} :max-watts 50}]
        patch (frame/compile-patch fixtures)
        output (frame/resolve-frame patch [{:target-id :white :attribute :dimmer :value 0.5}
                                           {:target-id :white :attribute :cold-white :value 1}
                                           {:target-id :uv :attribute :uv :value 1}] {})
        meter (capacity/measure (capacity/prepare patch) (:universes output))]
    (is (near? 0.25 (:utilization meter)))
    (is (< 74.9 (:used-rated-watts meter) 75.2))
    (is (> (last (:color meter)) (first (:color meter))))
    (is (thrown? clojure.lang.ExceptionInfo
                 (persistence/validate-model (assoc-in (white-look) [:settings :lighting-capacity] {:reference-rms 0}))))))

(deftest catalog-and-ambient-look-use-the-renderer-emitter-spectrum
  (let [fixture {:id :extra :universe 1 :address 1 :max-watts 120
                 :channels {:cool-white 1 :cyan 2 :lime 3 :dimmer 4}}
        patch (frame/compile-patch [fixture])
        emit (fn [field] {:target-id :extra :attribute field :value 1.0})
        all {:kind :assignments :assignments (mapv emit (keys (:channels fixture)))}
        frame (frame/resolve-contributions patch [all] {})
        look (pause/ambient-layer [fixture] {:mode :ambient :color [0.42 0.42 0.42] :level 0.1})
        paused (frame/resolve-contributions patch [all look] {})
        plan (capacity/prepare patch)]
    (is (near? 1.0 (:utilization (capacity/measure plan (:universes frame)))))
    (is (< (:utilization (capacity/measure plan (:universes paused))) 0.1))
    (doseq [field [:cool-white :lime :cyan]]
      (is (empty? (registry/validate-definition :effects {:id field :version 1 :params {} :outputs {field (demo/constant 0.5)}})))
      (is (ifn? (compiler/compile-effect {:id field :version 1 :params {} :outputs {field (demo/constant 1.0)}
                                         :envelope {:attack {:beats 1} :attributes [field]}}))))))

(deftest compare-source-output-on-a-shared-white-reference
  (let [weak {:id :weak :universe 1 :address 1 :channels {:dimmer 1}
              :measured-relative-brightness 1}
        strong {:id :strong :universe 1 :address 2 :channels {:dimmer 1}
                :measured-relative-brightness 4 :max-watts 100}
        uv {:id :uv :universe 1 :address 3 :channels {:uv 1} :max-watts 70}
        patch (frame/compile-patch [weak strong uv])
        plan (capacity/prepare patch)
        frame (frame/resolve-frame patch [{:target-id :weak :attribute :dimmer :value 0.8}
                                          {:target-id :strong :attribute :dimmer :value 0.2}
                                          {:target-id :uv :attribute :uv :value 0.5}])
        samples (capacity/measure-sources plan (:universes frame) [:weak :strong :uv])]
    (is (= (get-in samples [:strong :global-brightness]) (get-in samples [:weak :global-brightness])))
    (is (= 0.25 (get-in samples [:weak :global-maximum])))
    (is (near? 0.8 (get-in samples [:weak :local-brightness])))
    (is (near? 0.2 (get-in samples [:strong :local-brightness])))
    (is (nil? (get-in samples [:weak :used-rated-watts])))
    (is (near? 0.8 (get-in samples [:weak :emitter-drive])))
    (is (near? 20 (get-in samples [:strong :used-rated-watts])))
    (is (near? 0.5 (get-in samples [:uv :emitter-drive])))
    (is (zero? (get-in samples [:uv :global-brightness])))
    (is (= {:strong {:local-drive 0.5 :achievable 0.5 :limited? false}
            :weak {:local-drive 1.0 :achievable 0.25 :limited? true}}
           (capacity/match-brightness plan [:strong :weak] 0.5)))))
