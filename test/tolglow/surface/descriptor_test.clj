(ns tolglow.surface.descriptor-test
  "Surface contract: descriptors, normalized events and the replay harness
  over the real Afterglow codecs (#706 slice 0)."
  (:require [afterglow.control.codecs.push-2 :as push-2]
            [afterglow.control.codecs.push-3 :as push-3]
            [afterglow.control.codecs.x-touch :as mcu]
            [clojure.test :refer [deftest is testing]]
            [tolglow.surface.descriptor :as descriptor]
            [tolglow.surface.link :as link]
            [tolglow.surface.virtual :as virtual]))

;; The same synthetic capture form as afterglow.control.codecs.push-3-test:
;; real addresses come only from a capture of the device in User Mode.
(def ^:private captured-layout
  {:pads (into {} (for [x (range 8) y (range 8)] [[x y] {:channel 0 :note (+ 36 x (* 8 y))}]))
   :encoder-touches {:e1 {:channel 0 :note 0} :e2 {:channel 0 :note 1}}
   :encoders {:e1 {:channel 0 :controller 71} :e2 {:channel 0 :controller 72}}
   :buttons {:shift {:channel 0 :controller 49}}})

(deftest every-descriptor-is-plain-valid-data
  (doseq [d [descriptor/x-touch descriptor/x-touch-ext descriptor/push-2 descriptor/studio-grid
             (descriptor/push-3 captured-layout)]]
    (is (descriptor/valid? d) (pr-str (:id d))))
  (is (= {:strips [8 1]} (:viewports descriptor/x-touch) (:viewports descriptor/x-touch-ext)))
  (is (not (contains? (:groups descriptor/x-touch-ext) :master-fader)) "The extender has no master fader")
  (is (= {:grid [8 8] :encoders [2 1]} (:viewports (descriptor/push-3 captured-layout))))
  (is (not (descriptor/valid? (assoc-in descriptor/x-touch [:groups :faders :kind] :sliders))))
  (is (not (descriptor/valid? (assoc descriptor/studio-grid :viewport-groups {:grid #{:missing}})))))

(deftest the-mcu-table-round-trips-through-the-afterglow-codec
  (let [controls (concat
                  (for [x (range 8)] {:group :faders :at [x 0] :type :move :value (/ x 7.0)})
                  (for [x (range 8) type [:touch :release-touch]] {:group :faders :at [x 0] :type type})
                  (for [x (range 8) delta [1 -3]] {:group :encoders :at [x 0] :type :turn :delta delta})
                  (for [x (range 8)] {:group :encoders :at [x 0] :type :press})
                  (for [x (range 8) y (range 4) type [:press :release]] {:group :strip-buttons :at [x y] :type type})
                  (for [name (get-in descriptor/x-touch [:groups :buttons :capabilities :names])]
                    {:group :buttons :at [name 0] :type :press})
                  [{:group :master-fader :at [0 0] :type :move :value 1.0}])]
    (doseq [control controls
            :let [bytes (descriptor/x-touch-input-bytes control)
                  event (descriptor/x-touch-event "xt" (mcu/decode bytes))]]
      (is (= (assoc control :surface "xt")
             (cond-> (dissoc event :identity) (:value control) (update :value #(/ (Math/round (* 7.0 %)) 7.0))))
          (pr-str control bytes))
      (is (descriptor/valid-event? descriptor/x-touch event) (pr-str event)))
    (testing "extender strip controls normalize the same way; it has no named buttons"
      (is (= [:strip-buttons [3 1]] ((juxt :group :at) (descriptor/x-touch-event "ext" (mcu/decode [0x90 11 127])))))
      (is (not (descriptor/valid-event? descriptor/x-touch-ext
                                        (descriptor/x-touch-event "ext" (mcu/decode [0x90 46 127]))))))
    (is (nil? (descriptor/x-touch-event "xt" (mcu/decode [0x90 100 127]))) "Unmapped notes stay unmapped")))

(deftest the-captured-push-3-layout-round-trips-and-keeps-pad-identity
  (let [compiled (push-3/compile-layout captured-layout)
        d (descriptor/push-3 captured-layout)
        normalize descriptor/push-3-event
        messages (concat (for [[[x y] {:keys [channel note]}] (:pads captured-layout)] [(+ 0x90 channel) note 100])
                         [[0xb0 71 1] [0xb0 72 127] [0x90 0 127] [0xb0 49 127]])
        events (virtual/replay #(push-3/decode compiled %) normalize "push" messages)]
    (is (= (set (keys (:pads captured-layout)))
           (set (map :at (filter #(= :pads (:group %)) events)))))
    (is (every? #(descriptor/valid-event? d %) events))
    (is (= [[:encoders [:e1 0] :turn 1] [:encoders [:e2 0] :turn -1]]
           (mapv (juxt :group :at :type :delta) (filter #(= :turn (:type %)) events))))
    (is (= [:pad 0 36] (:identity (first (filter #(= [0 0] (:at %)) events)))) "Identity is the member's channel and note")
    (is (= [:buttons [:shift 0] :press] ((juxt :group :at :type) (peek events))))))

(deftest push-2-pads-normalize-from-the-source-derived-codec
  (let [events (virtual/replay identity descriptor/push-2-pad-event "p2"
                               [[0x90 (push-2/pad-note 0 0) 90] [0x80 (push-2/pad-note 7 7) 0] [0x90 10 90]])]
    (is (= [[[0 0] :press] [[7 7] :release]] (mapv (juxt :at :type) events)))
    (is (every? #(descriptor/valid-event? descriptor/push-2 %) events))))

(deftest a-virtual-surface-records-input-and-latest-feedback
  (let [surface (virtual/create descriptor/x-touch-ext "ext")]
    (is (= 3 (count (virtual/replay-into! surface mcu/decode descriptor/x-touch-event
                                          [[0x90 104 127] [0xe0 0 64] [0x90 46 127] [0xb0 16 1]])))
        "The extender has no bank buttons, so that press is refused before it is recorded")
    (is (thrown? clojure.lang.ExceptionInfo
                 (virtual/input! surface {:surface "ext" :group :master-fader :at [0 0] :type :move :value 1})))
    (virtual/present! surface [{:group :scribble :at [0 0] :state {:text "Ember" :color :red}}
                               {:group :faders :at [0 0] :state {:value 0.5}}])
    (virtual/present! surface [{:group :scribble :at [0 0] :state {:text "Ember" :color :cyan}}])
    (is (= {[:scribble [0 0]] {:text "Ember" :color :cyan} [:faders [0 0]] {:value 0.5}} (virtual/outputs surface)))
    (is (thrown? clojure.lang.ExceptionInfo (virtual/present! surface [{:group :pads :at [0 0] :state {}}])))))

(deftest linking-remaps-coordinates-but-keeps-member-identity
  (let [attach (fn [m id d]
                 (:model (link/apply-operation m {:op :control-surface/attach :surface-id id :descriptor d :role :unit
                                                  :viewports {:strips {:type :mixer-strips}}})))
        model (-> {} (attach "main" :behringer/x-touch) (attach "ext" :behringer/x-touch-ext)
                  (as-> m (:model (link/apply-operation m {:op :control-surface/link :link-id "desk"
                                                           :mode :extend :members [{:surface-id "main"} {:surface-id "ext"}]}))))
        touches (map #(descriptor/x-touch-event % (mcu/decode [0x90 104 127])) ["main" "ext"])]
    (is (= [[0 0] [8 0]] (mapv #(:at (link/logical-position model (:surface %) :strips (:at %))) touches)))
    (is (= [[:touch 0] [:touch 0]] (mapv :identity touches)))
    (is (= ["main" "ext"] (mapv :surface touches)) "Two members touching strip 1 stay two gestures")))
