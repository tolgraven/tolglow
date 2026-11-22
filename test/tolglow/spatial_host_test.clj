(ns tolglow.spatial-host-test
  (:require [afterglow.control.model :as control]
            [afterglow.reactive :as graph]
            [clojure.test :refer [deftest is]]
            [tolglow.show :as show]))

(def fixtures
  [{:id :left :position [0 1 0] :universe 1 :address 1 :channels {:red 1 :green 2 :blue 3}}
   {:id :near :position [0.5 1 0] :universe 1 :address 4 :channels {:red 1 :green 2 :blue 3}}
   {:id :far :position [3 1 0] :universe 1 :address 7 :channels {:red 1 :green 2 :blue 3}}])

(defn sphere [x] {:shape :sphere :position [x 1 0] :radius 0.1})

(defn runtime [batch?]
  (let [captured (atom nil)
        capture! (fn [universes _frame-id]
                   (reset! captured (mapv #(bit-and 255 %) (take 9 (get universes 1)))))
        effects {:red {:id :red :version 1 :params {} :outputs {:red 1.0}}
                 :hot {:id :hot :version 1 :params {} :targets [:left] :outputs {:color [1.4 0 0]}}
                 :blue {:id :blue :version 1 :params {} :outputs {:blue 1.0}}}
        cues {:red {:id :red :version 1 :effect-id :red :selector {:volume-id :solo}}
              :blue {:id :blue :version 1 :effect-id :blue}}]
    (assoc (show/create-runtime {:model (assoc (control/new-show {:registry {:effects effects :cues cues}})
                                               :rig {:fixtures fixtures})
                                 :batch? batch? :sink {:send! capture!}})
           ::captured-dmx captured)))

(defn tick [rt now]
  (show/tick! rt now)
  (is (nil? (get-in @(:state rt) [:status :error])) (pr-str (:status @(:state rt)))))

(defn submit [rt now op payload]
  (let [receipt (show/submit! rt op payload)]
    (tick rt now)
    (let [ack (deref receipt 1000 {:status :timeout})]
      (is (= :accepted (:status ack)) (pr-str ack))
      ack)))

(defn settings [rt now settings]
  (let [receipt (submit rt now :settings {:scope :spatial :values settings})]
    (is (pos-int? (:revision receipt)))
    (is (<= (:revision receipt) (:revision (show/snapshot rt))))
    receipt))

(defn output [rt]
  @(::captured-dmx rt))

(deftest sleeping-volumes-support-scoped-launch-and-stay-asleep
  (doseq [batch? [true false]]
    (let [rt (runtime batch?)]
      (try
        (settings rt 0 {:enabled? true :volumes {:solo (sphere 0)}})
        (is (true? (get-in @(:state rt) [:spatial-cache :suspended?])))
        (submit rt 25 :command {:op :cue/launch :cue-id :red :command-id "chosen"})
        (is (= [255 0 0 0 0 0 0 0 0] (output rt)))
        (settings rt 50 {:volumes {:solo (sphere 3)}})
        (submit rt 75 :command {:op :cue/launch :cue-id :red :command-id "chosen"})
        (is (= 1 (count (:instances (show/snapshot rt)))))
        (submit rt 100 :command {:op :effect/launch :effect-id :blue :launch {:selector {:volume-id :solo}}})
        (is (= [255 0 0 0 0 0 0 0 255] (output rt)))
        (is (true? (get-in @(:state rt) [:spatial-cache :suspended?])))
        (finally (show/stop! rt))))))

(deftest physics-stops-at-last-release-and-resumes-from-its-last-position
  (let [rt (runtime false) close (atom nil) observed (atom [])]
    (try
      (settings rt 0 {:enabled? true :gravity [0 0 0] :volumes
                      {:moving {:shape :ball :radius 0.1 :position [0 1 0] :velocity [1 0 0]}}})
      (let [lease #(graph/acquire! (show/reactions rt) [:spatial/body :moving] (fn [v] (swap! observed conj v)))
            pos #(get-in (show/snapshot rt) [:spatial :signals :moving :position])
            x #(first (pos))]
        (reset! close (:close! (lease)))
        (tick rt 10)
        (tick rt 110)
        (is (< 0.099 (x) 0.101))
        (@close)
        (let [held (pos) notifications (count @observed)]
          (tick rt 2000)
          (is (= held (pos)))
          (is (= notifications (count @observed)))
          (is (true? (get-in @(:state rt) [:spatial-cache :suspended?])))
          (reset! close (:close! (lease)))
          (tick rt 3000)
          (is (= held (pos)))
          (tick rt 3100)
          (is (< 0.199 (x) 0.201))
          (is (= (pos) (:position (last @observed))))))
      (finally (when-let [f @close] (f)) (show/stop! rt)))))

(deftest contact-leases-survive-transit-and-leave-the-performers-cues-alone
  (let [rt (runtime true) restored (runtime true)]
    (try
      (settings rt 0 {:enabled? true :volumes {:solo (sphere 0)}
                      :bindings {:enter-blue {:volume-id :solo :action {:cue-id :blue} :force? true}}})
      (submit rt 25 :command {:op :cue/launch :cue-id :red :launch {:targets [:far]}})
      (is (= [0 0 255 0 0 0 255 0 0] (output rt)))
      (let [saved (show/export-show-source rt)]
        (submit restored 50 :import {:source saved})
        (tick restored 75)
        (is (= [0 0 255 0 0 0 255 0 0] (output restored)))
        (is (= 2 (count (:instances (show/snapshot restored)))))
        (settings restored 100 {:volumes {:solo (sphere 0.5)}})
        (is (= [0 0 0 0 0 255 255 0 0] (output restored)))
        (is (= 2 (count (:instances (show/snapshot restored)))))
        (settings restored 125 {:enabled? false})
        (is (= [0 0 0 0 0 0 255 0 0] (output restored))))
      (finally (show/stop! restored) (show/stop! rt)))))

(deftest overdrive-detects-neighbours-before-color-spill-and-releases-on-blackout
  (doseq [batch? [true false]]
    (let [rt (runtime batch?)]
      (try
        (settings rt 0 {:enabled? true :volumes {:excess {:shape :hdr :source-id :left :threshold 0.15}}
                        :bindings {:highlight {:volume-id :excess :action {:effect-id :blue} :force? true}}})
        (submit rt 25 :command {:op :effect/launch :effect-id :hot})
        (is (= [255 0 0 0 0 0 0 0 0] (output rt)))
        (is (< 0.19 (get-in @(:state rt) [:output :hdr-contacts :left :near] 0) 0.21))
        (tick rt 50)
        (is (= #{:near} (get-in (show/snapshot rt) [:spatial :members :excess])))
        (is (= [255 0 0 0 0 255 0 0 0] (output rt)))
        (submit rt 75 :command {:op :blackout/set :value true})
        (tick rt 100)
        (is (empty? (get-in (show/snapshot rt) [:spatial :members :excess])))
        (is (= 1 (count (:instances (show/snapshot rt)))))
        (is (every? zero? (output rt)))
        (finally (show/stop! rt))))))

(deftest subscribing-to-contact-events-starts-the-world-and-launches-only-on-a-matching-contact
  (let [rt (runtime false)]
    (try
      (settings rt 0 {:enabled? true :volumes {:solo (sphere 0.5)}})
      (submit rt 25 :event-subscriber
              {:op :save :subscriber {:id :on-near :match {:type :spatial/enter :volume-id :solo :fixture-id :near}
                                      :actions [{:op :cue/launch :select {:ids [:red] :pick :first}}]}})
      (tick rt 50)
      (is (= [0 0 0 255 0 0 0 0 0] (output rt)))
      (settings rt 75 {:volumes {:solo (sphere 3)}})
      (tick rt 100)
      (is (= 1 (count (:instances (show/snapshot rt)))))
      (submit rt 125 :event-subscriber {:op :remove :id :on-near})
      (is (true? (get-in @(:state rt) [:spatial-cache :suspended?])))
      (finally (show/stop! rt)))))

(deftest a-timed-contact-action-fires-when-a-ball-crosses-between-two-dmx-frames
  (let [rt (runtime true)]
    (try
      (settings rt 0 {:enabled? true :gravity [0 0 0]
                      :volumes {:throw {:shape :ball :radius 0.025 :position [-0.25 1 0] :velocity [20 0 0]}}
                      :bindings {:hit {:volume-id :throw :selector {:fixture-ids [:near]}
                                       :action {:cue-id :blue} :duration {:beats 1/4}}}})
      (tick rt 50)
      (is (empty? (get-in (show/snapshot rt) [:spatial :members :throw])))
      (is (= [0 0 0 0 0 255 0 0 0] (output rt)))
      (tick rt 200)
      (is (every? zero? (output rt)))
      (finally (show/stop! rt)))))

(deftest a-missing-target-rejects-only-its-command
  (let [rt (runtime false)]
    (try
      (settings rt 0 {:enabled? true :volumes {:solo (sphere 0)}})
      (let [bad (show/submit! rt :command {:op :cue/launch :cue-id :red :launch {:selector {:volume-id :removed}}})
            good (show/submit! rt :command {:op :effect/launch :effect-id :blue :launch {:targets [:near]}})]
        (tick rt 25)
        (is (= [:rejected :invalid-target] ((juxt :status :code) @bad)))
        (is (= :accepted (:status @good)))
        (is (= [0 0 0 0 0 255 0 0 0] (output rt))))
      (finally (show/stop! rt)))))
