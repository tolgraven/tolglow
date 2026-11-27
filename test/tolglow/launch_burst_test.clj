(ns tolglow.launch-burst-test
  "A beat that starts many launches at once (#798). Each launch is admitted
  without a pass over the whole rig, and a burst of grid launches renders
  exactly as the same launches made directly on that beat. Assertions count
  work and compare captured output; none reads a clock."
  (:require [clojure.test :refer [deftest is testing]]
            [tolglow.aim-paths :as aim-paths]
            [tolglow.compiler.capability :as capability]
            [tolglow.demo :as demo]
            [tolglow.registry :as registry]
            [tolglow.show :as show]))

(defn- head [i] (keyword (str "head-" (inc i))))

(def ^:private fan
  {:id :fan :version 1 :name "Fan" :targets (mapv head (range 8))
   :params {:level {:type :number :default 0.5 :bounds [0.0 1.0]}}
   :outputs {:dimmer {:op :param :key :level} :color [0.2 0.4 0.6]}})

(defn- show-model []
  (-> (demo/model)
      (assoc-in [:registry :effects :fan] fan)
      (assoc-in [:registry :cues :fan] {:id :fan :version 1 :effect-id :fan :priority 20})))

(defn- runtime [output]
  (show/create-runtime {:model (show-model) :workspace-options nil :effect-log! (fn [_]) :workers 2
                        :sink {:send! (fn [universes _] (swap! output conj (mapv #(bit-and 255 %) (get universes 1))))}}))

(defn- launches
  "One launch per head, in head order, with the command IDs `prefix`-i."
  [prefix extra]
  (mapv (fn [i] (merge {:op :cue/launch :cue-id :fan :command-id (str prefix "-" i)
                        :source {:adapter :repl :session-id "burst"}
                        :launch {:mode :one-shot :duration {:beats 1} :targets [(head i)]}}
                       extra))
        (range 8)))

(defn- fan-instances [rt]
  (->> (vals (get-in @(:state rt) [:model :instances]))
       (filter #(= :fan (:effect-id %)))
       (sort-by :order)
       (mapv (juxt :targets :effect-version #(some? (get-in % [:system :last-frame-number]))))))

(deftest a-burst-of-grid-launches-renders-as-the-same-launches-made-on-the-beat
  ;; 120 BPM: the grid boundary at beat 1 is t = 500. One show schedules the
  ;; eight launches for that beat; its twin makes them directly at t = 500.
  (let [grid-output (atom []) direct-output (atom [])
        grid (runtime grid-output) direct (runtime direct-output)
        frames (fn [rt t-range] (mapv (fn [t] (show/tick! rt t) t) t-range))]
    (try
      (show/tick! grid 0)
      (show/tick! direct 0)
      (doseq [command (launches "grid" {:grid {:beats 1}})] (show/submit! grid command))
      (frames grid (range 25 476 25))
      (frames direct (range 25 476 25))
      (is (= [] (fan-instances grid)) "Nothing launches before the beat")
      (doseq [command (launches "direct" {})] (show/submit! direct command))
      (show/tick! grid 500)
      (show/tick! direct 500)
      (is (= (mapv (fn [i] [[(head i)] 1 true]) (range 8)) (fan-instances grid))
          "All eight start on the beat, in launch order, and render on that frame")
      (is (= (fan-instances direct) (fan-instances grid)))
      (is (= (peek @direct-output) (peek @grid-output)) "The beat frame's output is the same")
      (frames grid (range 525 701 25))
      (frames direct (range 525 701 25))
      (is (= (take-last 8 @direct-output) (take-last 8 @grid-output)) "And so is every frame after it")
      (finally (show/stop! grid) (show/stop! direct)))))

(deftest admitting-launches-indexes-the-rig-only-for-a-synchronized-aim-path
  ;; Before #798 every launch indexed the whole rig by wire ID to check
  ;; synchronized aim paths, which this effect is not.
  (let [output (atom []) rt (runtime output) wire-ids (atom 0) wire-id @#'aim-paths/wire-id]
    (with-redefs-fn {#'aim-paths/wire-id (fn [id] (swap! wire-ids inc) (wire-id id))}
      (fn []
        (try
          (show/tick! rt 0)
          (doseq [command (launches "now" {})] (show/submit! rt command))
          (show/tick! rt 25)
          (is (= 8 (count (fan-instances rt))))
          (is (= 0 @wire-ids))
          (finally (show/stop! rt)))))))

(defn- reference-skipped
  "Heads in rig order that the definition addresses, written attributes they
  cannot implement, as the capability report states it."
  [definition fixtures]
  (let [heads (if (contains? definition :targets)
                (filter #(some #{(:id %)} (:targets definition)) fixtures)
                fixtures)]
    (for [attribute (distinct (map :attribute (registry/output-entries (:outputs definition))))
          :let [skipped (mapv :id (remove #(capability/capable? % attribute) heads))]
          :when (seq skipped)]
      {:attribute attribute :heads skipped :requires (capability/requires attribute)})))

(deftest a-capability-report-looks-heads-up-in-rig-order
  (let [fixtures (conj (vec (demo/fixtures))
                       {:id :hazer :universe 2 :address 1 :channels {:fog 1}}
                       {:id :white-par :universe 2 :address 5 :channels {:dimmer 1 :white 2}})
        color {:id :c :version 1 :outputs {:color [1.0 0.0 0.0]}}]
    (doseq [[label definition] [["no targets" color]
                                ["targets out of rig order" (assoc color :targets [:white-par :head-3 :hazer :head-1])]
                                ["duplicates and unknown IDs" (assoc color :targets [:hazer :nope :hazer :head-2])]
                                ["empty targets" (assoc color :targets [])]
                                ["nil targets" (assoc color :targets nil)]]]
      (testing label
        (is (= (vec (reference-skipped definition fixtures)) (capability/skipped definition fixtures)))))
    (testing "a second rig is indexed afresh"
      (let [other (vec (reverse fixtures))
            definition (assoc color :targets [:white-par :hazer])]
        (capability/skipped definition fixtures)
        (is (= (vec (reference-skipped definition other)) (capability/skipped definition other)))))))
