(ns tolglow.typed-layers-host-test
  "Colour modifiers through `show/tick!` on Afterglow's typed colour layer path
  send the same DMX as its general path on every frame (#908)."
  (:require [afterglow.control.model :as control]
            [afterglow.output.artnet :as artnet]
            ;; Fails to load against an Afterglow pin without the typed path.
            [afterglow.render.typed-layers :as typed-layers]
            [clojure.test :refer [deftest is testing]]
            [tolglow.effects :as fx]
            [tolglow.show :as show])
  (:import [java.util Arrays]))

(def ^:private pixel-count 192)

(def ^:private fixtures
  (into (mapv (fn [i]
                (let [offset (* 3 i)]
                  {:id (str "pixel-" i) :universe (inc (quot offset 510)) :address (inc (mod offset 510))
                   :position [(- (* 0.25 (mod i 32)) 4.0) (* 0.5 (quot i 32)) 0.0]
                   :channels {:red 1 :green 2 :blue 3}}))
              (range pixel-count))
        ;; A white-only head inside the modifiers' selection keeps its light.
        [{:id "white" :universe 9 :address 1 :position [0.0 1.0 0.0] :channels {:white 1}}]))

(def ^:private pixel-ids (mapv :id (butlast fixtures)))

(def ^:private x (fx/position :x))

(def ^:private y (fx/position :y))

(def ^:private beat {:op :beat :clock :absolute})

(defn- op [operator & args] {:op operator :args (mapv fx/expression args)})

(defn- wave [phase] (fx/add 0.5 (fx/multiply 0.5 (op :sin phase))))

(def ^:private previous-color {:op :previous :attribute :color :default [0.0 0.0 0.0]})

(def ^:private incoming {:op :incoming :attribute :color})

(defn- modifier [id transform outputs]
  (cond-> {:id id :version 1 :targets pixel-ids :priority 50 :params {}
           :layer {:transforms [{:attribute :color :value transform}]}}
    outputs (assoc :outputs {:color outputs})))

(def ^:private effects
  {:wash {:id :wash :version 1 :targets pixel-ids :priority 10 :params {}
          :outputs {:color (fx/clamp (fx/rgb (wave (fx/add (fx/multiply x 0.9) (fx/divide beat 2.0)))
                                             (wave (fx/subtract (fx/multiply y 1.3) beat))
                                             (fx/add 0.2 (fx/multiply 0.5 (wave (fx/add x y))))))
                    :white 0.4}}
   :keyhole (modifier :keyhole {:op :multiply :args [previous-color incoming]}
                      (let [m (fx/clamp (fx/add 0.15 (wave (fx/add (fx/multiply x 1.7) beat))))] (fx/rgb m m m)))
   :hue (modifier :hue {:op :rotate-hue :args [previous-color 1/6]} nil)
   :shade (modifier :shade {:op :color-grade :args [previous-color incoming]}
                    (fx/rgb (fx/multiply 0.7 (wave (fx/add x beat))) 0.4 (op :fract (fx/multiply 0.3 y))))
   :guard (modifier :guard {:op :color-guard :args [previous-color incoming]} (fx/rgb 0.55 0.6 0.08))
   :sparkle (assoc (modifier :sparkle {:op :color-grade :args [previous-color incoming]}
                             (fx/rgb (fx/multiply 0.85 (op :floor (fx/clamp (fx/add (wave (fx/multiply 37.0 (fx/add x (fx/multiply 3.1 y) beat))) 0.2) 0.0 1.0)))
                                     0.0 0.0))
                   :priority 60)})

(def ^:private cases
  "Each case's cues, launch options and whether HDR is on."
  {:keyhole {:cues [:wash :keyhole]}
   :hue {:cues [:wash :hue]}
   :shade {:cues [:wash :shade]}
   :guard {:cues [:wash :guard]}
   :guard-sparkle {:cues [:wash :guard :sparkle]}
   ;; Instance opacity becomes member opacity: the modifier mixes over the wash.
   :keyhole-translucent {:cues [:wash :keyhole] :launch {:keyhole {:system {:opacity 0.6}}}}
   :guard-hdr {:cues [:wash :guard] :hdr? true}})

(defn- capture-sink []
  (let [latest (volatile! [])]
    {:latest latest :description {:type :artdmx-capture :network false}
     :send! (fn [universes _] (vreset! latest (mapv (fn [[u b]] (artnet/artdmx-packet u 1 b)) (sort-by key universes))))
     :close! (fn [])}))

(defn- runtime [{:keys [cues launch hdr?] :or {launch {}}} batch? typed?]
  (let [model (reduce (fn [model id]
                        (let [{:keys [model ack]} (control/apply-command model (cond-> {:op :cue/launch :cue-id id :command-id (name id)
                                                                                        :source :test :time-ms 0
                                                                                        :show-id (:show-id model) :epoch (:epoch model)}
                                                                                 (launch id) (assoc :launch (launch id))))]
                          (is (= :accepted (:status ack)))
                          model))
                      (reduce (fn [model {:keys [id priority] :as effect}]
                                (-> model
                                    (assoc-in [:registry :effects id] (dissoc effect :priority))
                                    (assoc-in [:registry :cues id] {:id id :effect-id id :priority priority :version 1})))
                              (cond-> (control/new-show {:show-id "typed-layers" :rig {:fixtures fixtures}
                                                         :transport {:beat 0 :bpm 120 :playing? true}})
                                hdr? (assoc-in [:settings :hdr] {:enabled? true}))
                              (vals effects))
                      cues)]
    (show/create-runtime {:model model :workers 2 :batch? batch? :typed-layers? typed? :sink (capture-sink)})))

(defn- packets [rt] @(get-in rt [:sink :latest]))

(defn- same? [a b]
  (and (seq a) (= (count a) (count b)) (every? true? (map #(Arrays/equals ^bytes %1 ^bytes %2) a b))))

(defn- composition [rt] (select-keys (meta (get-in @(:state rt) [:output])) [::typed-layers/typed ::typed-layers/fallback]))

(deftest colour-modifiers-send-the-general-path-dmx-every-frame
  (doseq [[case-id spec] cases
          batch? [true false]]
    (testing (str case-id (if batch? " compact" " rich"))
      (let [typed (runtime spec batch? true)
            general (runtime spec batch? false)
            wash (runtime (assoc spec :cues [:wash]) batch? true)]
        (try
          (let [differs (volatile! 0)]
            (dotimes [i 40]
              (let [t (* 25.0 (inc i))]
                (doseq [rt [typed general wash]] (show/tick! rt t))
                (doseq [rt [typed general]]
                  (let [{:keys [status model]} @(:state rt)]
                    (is (nil? (:error status)))
                    (is (not-any? :fault (vals (:instances model))))))
                (is (same? (packets general) (packets typed)) (str "frame " i))
                ;; A typed batch takes the typed path; a rich runtime emits
                ;; assignment rows, which only a transform-only modifier avoids.
                (is (= (if (or batch? (= :hue case-id)) {::typed-layers/typed true} {::typed-layers/fallback :member-writes})
                       (composition typed)))
                (is (= {} (composition general)))
                (when-not (same? (packets wash) (packets typed)) (vswap! differs inc))))
            (is (= 40 @differs) "The modifier changes the wash on every frame")
            (testing "the white-only head keeps the wash's white"
              (is (= (get-in @(:state wash) [:output :universes 9 0])
                     (get-in @(:state typed) [:output :universes 9 0])))))
          (finally (run! show/stop! [typed general wash])))))))

(deftest typed-layers-option-is-a-boolean
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"Typed layers must be true or false"
                        (show/create-runtime {:typed-layers? :sometimes}))))
