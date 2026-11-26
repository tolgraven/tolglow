(ns tolglow.position-hold-test
  "A moving head holds its last committed pose when the last effect writing its
  pan/tilt ends (#690). Every assertion reads captured DMX packets from the
  real frame owner."
  (:require [clojure.test :refer [deftest is testing]]
            [afterglow.control.model :as control]
            [tolglow.demo :as demo]
            [tolglow.show :as show]
            [tolglow.show.position-hold :as position-hold]))

(def ^:private heads
  "Two 16-bit heads (one crossing a universe boundary) and one 8-bit head."
  [{:id :wide :universe 1 :address 511 :position [0 0 -1] :channels {:pan [1 3] :tilt [2 4]}}
   {:id :narrow :universe 2 :address 5 :position [0 0 -2] :rotation-deg [0 20 0]
    :channels {:pan [1 2] :tilt [3 4]}}
   {:id :eight :universe 3 :address 1 :position [1 0 -1] :channels {:pan 1 :tilt 2 :dimmer 3}}
   {:id :wash :universe 3 :address 10 :channels {:red 1 :green 2 :blue 3}}])

(def ^:private slots
  "Captured [universe index] slots of every position channel."
  {[:wide :pan] [[1 510] [2 0]] [:wide :tilt] [[1 511] [2 1]]
   [:narrow :pan] [[2 4] [2 5]] [:narrow :tilt] [[2 6] [2 7]]
   [:eight :pan] [[3 0]] [:eight :tilt] [[3 1]]})

(defn- position [packet]
  (into {} (for [[k addresses] slots]
             [k (mapv (fn [[universe index]] (get-in packet [universe index])) addresses)])))

(defn- cue [id outputs]
  {:effect {:id id :version 1 :targets [:wide :narrow :eight] :outputs outputs}
   :cue {:id id :version 1 :effect-id id}})

(def ^:private cues
  {:aim-left (cue :aim-left {:aim [-1.5 0.5 1.0]})
   :aim-right (cue :aim-right {:aim [2.0 1.0 0.5]})
   :direct (cue :direct {:pan 0.3 :tilt 0.7})
   :wash (cue :wash {:dimmer 1})
   ;; A standalone modifier over every head lower-priority lighting touched.
   :halve {:effect {:id :halve :version 1 :outputs {}
                    :layer {:targets :previous
                            :transforms [{:attribute :dimmer
                                          :value {:op :multiply
                                                  :args [{:op :previous :attribute :dimmer :default 0} 0.5]}}]}}
           :cue {:id :halve :version 1 :effect-id :halve}}})

(defn- runtime [captured]
  (let [model (reduce-kv (fn [model id {:keys [effect cue]}]
                           (-> model
                               (assoc-in [:registry :effects id] effect)
                               (assoc-in [:registry :cues id] cue)))
                         (control/new-show {:rig {:fixtures heads}}) cues)]
    (show/create-runtime
     {:workspace-options nil :model model
      :sink {:send! (fn [universes _]
                      (reset! captured (update-vals universes #(mapv (partial bit-and 255) %))))}})))

(defn- submit-at!
  ([runtime payload t] (submit-at! runtime :command payload t))
  ([runtime type payload t]
   (let [reply (show/submit! runtime type payload)]
     (show/tick! runtime t)
     (deref reply 1000 ::missing))))

(defn- frames! [runtime captured times]
  (mapv (fn [t] (show/tick! runtime t) (position @captured)) times))

(def ^:private zero-pose (into {} (map (fn [[k addresses]] [k (vec (repeat (count addresses) 0))]) slots)))

(deftest one-shot-aim-holds-its-pose-after-it-ends
  (let [captured (atom nil) rt (runtime captured)]
    (try
      (show/tick! rt 0)
      (is (= zero-pose (position @captured)) "Unwritten heads start at their packed default")
      (let [launch (submit-at! rt {:op :cue/launch :cue-id :aim-left :launch {:mode :one-shot :duration {:ms 200}}} 0)
            aimed (position @captured)]
        (is (= :accepted (:status launch)))
        (is (every? #(some pos? (get aimed %)) (keys slots)) "The aim drives every pan/tilt slot away from zero")
        (let [running (frames! rt captured (range 25 200 25))
              ended (frames! rt captured (range 200 1001 25))]
          (is (every? #(= aimed %) running))
          (is (empty? (get-in @(:state rt) [:model :instances])) "The one-shot instance has ended")
          (is (every? #(= aimed %) ended) "16-bit and 8-bit heads hold the pose on every following frame")
          (testing "the committed preview agrees with the wire"
            (let [values (get-in (show/snapshot rt) [:output :values :eight])]
              (is (= (get-in aimed [[:eight :pan] 0]) (Math/round (* 255.0 (:pan values)))))))))
      (finally (show/stop! rt)))))

(deftest a-later-cue-moves-a-held-head
  (let [captured (atom nil) rt (runtime captured)]
    (try
      (submit-at! rt {:op :cue/launch :cue-id :aim-left :launch {:mode :one-shot :duration {:ms 100}}} 0)
      (let [left (position @captured)
            held (frames! rt captured (range 100 301 50))]
        (is (every? #(= left %) held))
        (submit-at! rt {:op :cue/launch :cue-id :aim-right :launch {:mode :one-shot :duration {:ms 100}}} 300)
        (let [right (position @captured)]
          (is (every? #(not= (get left %) (get right %)) (keys slots)) "The second aim moves every axis")
          (is (every? #(= right %) (frames! rt captured (range 400 601 50))) "The head then holds the new pose"))
        (submit-at! rt {:op :cue/launch :cue-id :direct :launch {:mode :one-shot :duration {:ms 100}}} 600)
        (let [direct (position @captured)]
          (is (= [77] (get direct [:eight :pan])) "A direct pan/tilt write replaces the held pose")
          (is (= [179] (get direct [:eight :tilt])))
          (is (every? #(= direct %) (frames! rt captured (range 700 901 50))) "Direct writes are held too")))
      (finally (show/stop! rt)))))

(deftest non-position-channels-keep-their-release
  (let [captured (atom nil) rt (runtime captured)]
    (try
      (submit-at! rt {:op :cue/launch :cue-id :aim-left :launch {:mode :one-shot :duration {:ms 100}}} 0)
      (submit-at! rt {:op :cue/launch :cue-id :wash :launch {:mode :one-shot :duration {:ms 100}}} 0)
      (is (= 255 (get-in @captured [3 2])) "The dimmer is driven while its cue runs")
      (let [aimed (position @captured)]
        (show/tick! rt 200)
        (is (= aimed (position @captured)))
        (is (= 0 (get-in @captured [3 2])) "The dimmer still returns to its idle value"))
      (finally (show/stop! rt)))))

(deftest blackout-and-stop-are-unchanged
  (let [captured (atom nil) rt (runtime captured)]
    (submit-at! rt {:op :cue/launch :cue-id :aim-left :launch {:mode :one-shot :duration {:ms 100}}} 0)
    (submit-at! rt {:op :cue/launch :cue-id :wash :launch {:mode :persistent}} 0)
    (let [aimed (position @captured)]
      (show/tick! rt 200)
      (is (= aimed (position @captured)))
      (is (= :accepted (:status (submit-at! rt {:op :blackout/set :value true} 250))))
      (is (= 0 (get-in @captured [3 2])) "Blackout still darkens the dimmer")
      (is (= aimed (position @captured)) "Blackout is an emission control; it does not move heads")
      (show/stop! rt)
      (is (= zero-pose (position @captured)) "Stop still sends the idle shutdown packet"))))

(deftest removing-a-held-head-retires-its-slots
  (let [captured (atom nil) rt (runtime captured)
        kept [[:wide :pan] [:wide :tilt] [:narrow :pan] [:narrow :tilt]]]
    (try
      (submit-at! rt {:op :cue/launch :cue-id :aim-left :launch {:mode :one-shot :duration {:ms 100}}} 0)
      (show/tick! rt 200)
      (let [aimed (position @captured)
            rig (get-in @(:state rt) [:model :rig])
            smaller (update rig :fixtures #(filterv (fn [f] (not= :eight (:id f))) %))]
        (is (= :accepted (:status (submit-at! rt :rig smaller 250))))
        (show/tick! rt 300)
        (is (= (select-keys aimed kept) (select-keys (position @captured) kept)) "Heads still patched keep holding")
        (is (not (contains? (get-in (show/snapshot rt) [:output :values]) :eight)) "The removed head is no longer rendered")
        (is (= [[0] [0]] [(get (position @captured) [:eight :pan]) (get (position @captured) [:eight :tilt])])
            "Retired slots are released as before"))
      (finally (show/stop! rt)))))

(defn- record-show!
  "Run one scripted show and return every captured packet and the values row."
  [model options script]
  (let [captured (atom nil)
        rt (show/create-runtime
            (merge {:workspace-options nil :model model
                    :sink {:send! (fn [universes _]
                                    (reset! captured (update-vals universes #(mapv (partial bit-and 255) %))))}}
                   options))
        ids (atom {})]
    (try
      (into [] (for [t (range 0 3001 25)]
                 (do (doseq [[cue-id launch] (get-in script [:launch t])]
                       (swap! ids assoc cue-id (:instance-id (let [reply (show/submit! rt {:op :cue/launch :cue-id cue-id :launch launch})]
                                                 (show/tick! rt t)
                                                 (deref reply 1000 nil)))))
                     (doseq [[op cue-id value] (get-in script [:ops t])]
                       (show/submit! rt (cond-> {:op op :instance-id (get @ids cue-id)} value (assoc :value value))))
                     (show/tick! rt t)
                     [@captured (get-in (show/snapshot rt) [:output :values])])))
      (finally (show/stop! rt)))))

(defn- master-like [model options script]
  (with-redefs [position-hold/with-hold (fn [_ _ blocks _] blocks)]
    (record-show! model options script)))

(def ^:private unreleased-script
  "Position writers that keep running, at full and partial opacity, beside
  emission cues that start and end."
  {:launch {0 [[:sweep {:mode :persistent}] [:ember {:mode :persistent}]]
            100 [[:flash {:mode :one-shot :duration {:beats 1/4}}]]
            400 [[:breathe {:mode :timed :duration {:ms 600}}]]
            1500 [[:spot {:mode :one-shot :duration {:ms 300}}]]}
   :ops {700 [[:instance/opacity-set :sweep 0.5]]
         1200 [[:instance/opacity-set :sweep 0.25]]
         1800 [[:instance/opacity-set :sweep 1.0]]
         2000 [[:instance/end :ember nil]]}})

(deftest shows-without-a-released-position-writer-are-byte-identical
  (doseq [options [{} {:batch? true}]]
    (testing (pr-str options)
      (let [held (record-show! (demo/model) options unreleased-script)
            master (master-like (demo/model) options unreleased-script)]
        (is (= 121 (count held)))
        (is (some #(pos? (get-in (first %) [1 5])) held) "The sweep positions the demo heads")
        (is (= (mapv first master) (mapv first held)) "Every captured packet matches the unheld renderer")
        (is (= (mapv second master) (mapv second held)) "Committed values match too")))))

(deftest a-released-writer-is-the-only-difference
  (let [script (assoc-in unreleased-script [:ops 2500] [[:instance/end :sweep nil]])
        held (mapv first (record-show! (demo/model) {} script))
        master (mapv first (master-like (demo/model) {} script))
        first-diff (first (keep-indexed (fn [i [a b]] (when (not= a b) i)) (map vector held master)))]
    (is (some? first-diff))
    (is (< (* 25 first-diff) 3001))
    (is (> (* 25 first-diff) 2500) "Packets match until the sweep's release has ended")
    (is (every? #(= [0 0] [(get-in % [1 5]) (get-in % [1 6])]) (subvec master first-diff)) "Unheld, pan falls to zero")
    (is (= 1 (count (distinct (map #(subvec (get % 1) 5 9) (subvec held first-diff)))))
        "Held, pan and tilt stay at one pose")
    (is (pos? (get-in (nth held first-diff) [1 5])))))

(deftest compact-batch-frames-hold-too
  (let [captured (atom nil)
        model (reduce-kv (fn [model id {:keys [effect cue]}]
                           (-> model (assoc-in [:registry :effects id] effect) (assoc-in [:registry :cues id] cue)))
                         (control/new-show {:rig {:fixtures heads}}) cues)
        rt (show/create-runtime {:workspace-options nil :model model :batch? true
                                 :sink {:send! (fn [universes _] (reset! captured (update-vals universes #(mapv (partial bit-and 255) %))))}})]
    (try
      (submit-at! rt {:op :cue/launch :cue-id :aim-left :launch {:mode :one-shot :duration {:ms 100}}} 0)
      (let [aimed (position @captured)]
        (is (nil? (get-in @(:state rt) [:output :values])) "Batch frames carry compact history only")
        (is (every? #(= aimed %) (frames! rt captured (range 100 501 50)))))
      (finally (show/stop! rt)))))

(defn- rig-model []
  (reduce-kv (fn [model id {:keys [effect cue]}]
               (-> model (assoc-in [:registry :effects id] effect) (assoc-in [:registry :cues id] cue)))
             (control/new-show {:rig {:fixtures heads}}) cues))

(deftest a-standalone-modifier-leaves-held-heads-alone
  (doseq [[label script] {"modifier launched while held"
                          {:launch {0 [[:aim-left {:mode :one-shot :duration {:ms 100}}] [:wash {:mode :persistent}]]
                                    300 [[:halve {:mode :persistent}]]}
                           :ops {600 [[:instance/end :halve nil]]}}
                          "modifier already running at release"
                          {:launch {0 [[:wash {:mode :persistent}]]
                                    25 [[:halve {:mode :persistent}]]
                                    50 [[:aim-left {:mode :one-shot :duration {:ms 100}}]]}}}]
    (testing label
      (let [held (mapv first (record-show! (rig-model) {} script))
            master (mapv first (master-like (rig-model) {} script))
            aimed (position (nth held 2))
            unheld #(reduce (fn [packet [universe index]] (assoc-in packet [universe index] 0)) % (apply concat (vals slots)))]
        (is (every? #(some pos? (get aimed %)) (keys slots)))
        (is (every? #(= aimed (position %)) (drop 8 held)) "Every head holds its pose before, during and after the modifier")
        (is (= 128 (get-in (nth held 20) [3 2])) "The modifier still halves the lit head's dimmer")
        (is (= (mapv unheld master) (mapv unheld held)) "Only the held position slots differ from the unheld renderer")))))

(deftest zero-opacity-and-restart-hold-the-pose
  (let [captured (atom nil) rt (runtime captured)]
    (try
      (let [launch (submit-at! rt {:op :cue/launch :cue-id :aim-left :launch {:mode :persistent}} 0)
            aimed (position @captured)]
        (is (= :accepted (:status (submit-at! rt {:op :instance/opacity-set :instance-id (:instance-id launch) :value 0} 50))))
        (is (= aimed (position @captured)) "A running writer at zero opacity writes nothing, so the pose holds")
        (is (= :accepted (:status (submit-at! rt {:op :show/restart} 100))))
        (is (empty? (get-in @(:state rt) [:model :instances])))
        (is (every? #(= aimed %) (frames! rt captured (range 125 301 25)))
            "A restart clears cues but does not command the heads to move"))
      (finally (show/stop! rt)))))
