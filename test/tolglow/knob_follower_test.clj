(ns tolglow.knob-follower-test
  "A continuous linked-knob envelope that follows with bounded acceleration
  (#914), through the real show owner: a reversal keeps position and velocity,
  the move settles exactly on the plain value, replays at different frame
  rates commit the same values at the same beats, and the move freezes with
  the transport, keeps its musical time across a rebase and survives a backup."
  (:require [afterglow.control.model :as control]
            [clojure.test :refer [deftest is testing]]
            [tolglow.clock :as show-clock]
            [tolglow.knobs :as knobs]
            [tolglow.persistence :as files]
            [tolglow.show :as show]))

(def ^:private effect
  {:id :wash :version 1
   :params {:level {:type :number :default 0.0 :bounds [0.0 1.0]}
            :shape {:type :enum :values [:dot :bar :ring] :default :dot}}
   :outputs {:dimmer {:op :param :key :level}}})

(defn- base-model []
  (control/new-show {:show-id :follow :transport {:playing? true :beat 0.0 :bpm 120.0}
                     :registry {:effects {:wash effect} :cues {:wash {:id :wash :effect-id :wash :version 1}}}
                     :rig {:fixtures [{:id :head :universe 1 :address 1 :channels {:dimmer 1}}]}}))

(defn- running-model []
  (let [{:keys [model ack]} (control/apply-command (base-model)
                                                   {:op :cue/launch :cue-id :wash :launch {:mode :persistent}
                                                    :command-id "launch" :show-id :follow :epoch 0
                                                    :source {:adapter :test} :time-ms 0})]
    (assert (= :accepted (:status ack)) (pr-str ack))
    model))

(def ^:private studio {:adapter :studio :session-id "follow-test"})

(def ^:private envelope
  "The whole range in 1 beat either way, full speed reached in half a beat:
  an acceleration limit of 2 ranges per beat²."
  {:attack {:beats 1} :release {:beats 1} :accelerate {:beats 1/2}})

(def ^:private accel 2.0)

(defn- link [envelope]
  {:id "level" :destination {:kind :param :cue-id :wash :param :level} :range [0.0 1.0] :envelope envelope})

(defn- runtime [] (show/create-runtime {:model (running-model) :workspace-options nil}))

(defn- model-of [rt] (:model @(:state rt)))

(defn- submit! [rt time payload]
  (let [reply (show/submit! rt :command (merge {:source studio} payload))]
    (show/tick! rt time)
    (let [ack (deref reply 5000 ::missing)]
      (assert (= :accepted (:status ack)) (pr-str ack))
      ack)))

(defn- level [rt]
  (get-in (some #(when (= :wash (:cue-id %)) %) (vals (:instances (model-of rt)))) [:params :level]))

(defn- beat [rt] (get-in (model-of rt) [:transport :beat]))

(defn- turn! [rt t value] (submit! rt t {:op :knob/set :knob-id "k" :value (double value)}))

(defn- frame! [rt t] (show/tick! rt t) [t (beat rt) (level rt)])

(defn- reversal
  "Create the knob at 0, turn it to 1 at 100 ms and back to 0.1 at reverse-ms,
  ticking every period ms until 3000 ms. Returns [t beat committed-level] rows."
  [envelope period reverse-ms]
  (let [rt (runtime)]
    (try
      (submit! rt 0 {:op :knob/create :knob-id "k" :name "K" :links [(link envelope)]})
      (turn! rt 20 0.0)
      (let [ticks (fn [from to] (mapv #(frame! rt %) (range from to period)))
            a (ticks 40 100)
            turned [(do (turn! rt 100 1.0) 100) (beat rt) (level rt)]
            b (ticks (+ 100 period) reverse-ms)
            _ (turn! rt reverse-ms 0.1)
            c [[reverse-ms (beat rt) (level rt)]]
            d (ticks (+ reverse-ms period) 3000)]
        (vec (concat a [turned] b c d)))
      (finally (show/stop! rt)))))

(defn- second-differences [rows]
  (let [xs (mapv #(nth % 2) rows)]
    (mapv (fn [a b c] (- (+ a c) (* 2.0 b))) xs (rest xs) (drop 2 xs))))

(deftest a-reversed-continuous-link-keeps-its-velocity
  (let [rows (reversal envelope 20 500)
        ;; 20 ms frames at 120 BPM are 0.04 beats.
        bound (+ (* accel 0.04 0.04) 1.0e-9)
        moving (drop-while #(< (first %) 100) rows)]
    (is (every? #(<= (Math/abs %) bound) (second-differences moving))
        (str "no first-difference jump beyond the acceleration limit: " (apply max (map #(Math/abs %) (second-differences moving)))))
    (is (> (apply max (map #(nth % 2) rows)) (nth (some #(when (= 500 (first %)) %) rows) 2))
        "turned back while rising, it carries on up a little before it turns")
    (is (= 0.1 (nth (peek rows) 2)) "it settles exactly on the plain value at the knob position")
    (is (every? #(<= 0.0 (nth % 2) 1.0) rows))
    (testing "the linear slew of the same envelope without :accelerate kinks at the reversal"
      (let [linear (reversal (dissoc envelope :accelerate) 20 500)]
        (is (> (apply max (map #(Math/abs %) (second-differences (drop-while #(< (first %) 100) linear)))) 0.07))))))

(deftest replays-at-25-and-40-ms-frames-commit-the-same-values-at-the-same-beats
  (let [at-25 (into {} (map (fn [[t b x]] [t [b x]])) (reversal envelope 25 500))
        at-40 (into {} (map (fn [[t b x]] [t [b x]])) (reversal envelope 40 500))
        shared (sort (filter #(contains? at-40 %) (keys at-25)))]
    (is (< 10 (count shared)))
    (doseq [t shared
            :let [[b1 x1] (at-25 t) [b2 x2] (at-40 t)]]
      (is (< (Math/abs (- b1 b2)) 1.0e-9))
      (is (< (Math/abs (- x1 x2)) 1.0e-9) (pr-str {:t t :at-25 x1 :at-40 x2})))
    (is (= 0.1 (second (at-25 2900)) (second (at-40 2900))))))

(deftest a-follower-freezes-on-stop-and-keeps-its-musical-time-across-a-rebase
  (let [rt (runtime)]
    (try
      (submit! rt 0 {:op :knob/create :knob-id "k" :name "K" :links [(link envelope)]})
      (turn! rt 20 0.0)
      (turn! rt 100 1.0)
      (let [before (mapv #(frame! rt %) (range 120 400 20))
            step (- (nth (peek before) 2) (nth (peek (pop before)) 2))]
        (submit! rt 400 {:op :transport/set :transport {:playing? false}})
        (let [held (level rt) entry (get-in (model-of rt) [:knobs :knobs "k" :motion "level"])]
          (is (= #{:velocity :accel} (into #{} (filter #{:velocity :accel}) (keys entry))) "the move is a follower")
          (is (every? #(= held (nth % 2)) (mapv #(frame! rt %) (range 420 900 20))) "it holds while stopped")
          (submit! rt 900 {:op :transport/set :transport {:playing? true}})
          (let [[_ _ resumed] (frame! rt 920)
                resumed-step (- resumed held)]
            ;; The stop landed mid-frame, so the first step after it may be part of a frame.
            (is (<= 0.0 resumed-step (+ step (* accel 0.04 0.04) 1.0e-9))
                "it carries on without catching up")))
        (let [m (model-of rt) at (level rt)]
          (swap! (:state rt) update :model show-clock/rebase {:beat (+ (beat rt) 8.0) :time-ms (:time-ms m)})
          (is (< (Math/abs (- at (knobs/output-level (model-of rt) (get-in (model-of rt) [:knobs :knobs "k"])
                                                    (link envelope))))
                 1.0e-9)
              "a seek leaves the follower where it was")))
      (finally (show/stop! rt)))))

(deftest a-follower-move-survives-a-backup-and-malformed-moves-are-dropped
  (let [rt (runtime)]
    (try
      (submit! rt 0 {:op :knob/create :knob-id "k" :name "K" :links [(link envelope)]})
      (turn! rt 20 0.0)
      (turn! rt 100 1.0)
      (mapv #(frame! rt %) (range 120 300 20))
      (turn! rt 300 0.2)
      (let [model (model-of rt)
            entry (get-in model [:knobs :knobs "k" :motion "level"])
            restored (files/thaw (base-model) (files/freeze model))
            again (get-in restored [:knobs :knobs "k" :motion "level"])]
        (is (pos? (:velocity entry)) "the reversal took over a rising velocity")
        (is (= (select-keys entry [:from :beats :velocity :accel :start]) (select-keys again [:from :beats :velocity :accel :start])))
        (doseq [bad [{:accel -1.0} {:accel ##Inf} {:velocity "fast"} {:accel 1.0 :beats 0.0}]]
          (let [state (assoc-in (:knobs model) [:knobs "k" :motion "level"] (merge entry bad))]
            (is (nil? (get-in (knobs/restore state {} 1 (beat rt)) [:knobs "k" :motion "level"])) (pr-str bad)))))
      (finally (show/stop! rt)))))

(deftest acceleration-is-refused-on-a-stepped-link
  (let [rt (runtime)]
    (try
      (let [reply (show/submit! rt :command {:source studio :op :knob/create :knob-id "k" :name "K"
                                             :links [{:id "shape" :destination {:kind :param :cue-id :wash :param :shape}
                                                      :range [:dot :ring] :envelope envelope}]})]
        (show/tick! rt 0)
        (let [ack (deref reply 5000 ::missing)]
          (is (= :rejected (:status ack)))
          (is (re-find #"(?i)acceleration" (str (:message ack))))))
      (finally (show/stop! rt)))))
