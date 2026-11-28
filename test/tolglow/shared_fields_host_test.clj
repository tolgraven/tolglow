(ns tolglow.shared-fields-host-test
  "Typed batch fields are retained and shared read-only by their pure binding.
  Two instances of one compiled effect, scaled by different envelopes, run
  through the host tick on a stopped transport against the scalar runtime."
  (:require [afterglow.control.model :as control]
            [afterglow.render.ir :as ir]
            [clojure.test :refer [deftest is testing]]
            [tolglow.bench-show :as bench]
            [tolglow.show :as show]))

(def ^:private rig
  (vec (concat
        (for [i (range 8)] {:id (str "pixel-" i) :universe 1 :address (inc (* 4 i)) :position [(double i) 0.0 0.0]
                            :channels {:red 1 :green 2 :blue 3 :white 4}})
        (for [i (range 4)] {:id (str "par-" i) :universe 1 :address (+ 33 (* 4 i)) :position [(double i) 2.0 0.0]
                            :channels {:red 1 :green 2 :blue 3 :dimmer 4}}))))

(def ^:private fade
  ;; Pure fields read only the parameter and position, so a stopped transport
  ;; hits the binding on every frame. The envelope scales dimmer and colour.
  {:id :fade :version 1 :params {:level {:type :number :default 0.3 :bounds [0 1]}}
   :envelope {:attack {:beats 4} :release {:beats 1}}
   :outputs [{:attribute :color :blend :max
              :value {:op :vec3 :args [{:op :param :key :level}
                                       {:op :multiply :args [{:op :position :axis :x} 0.1]} 0.2]}}
             {:attribute :dimmer :blend :add :value {:op :param :key :level}}]})

(defn- model []
  (reduce (fn [m [id priority]]
            (bench/command m [id :cue] {:op :cue/put :cue {:id id :effect-id :fade :priority priority}}))
          (bench/command (control/new-show {:show-id "shared-fields" :rig {:fixtures rig}})
                         :publish {:op :registry/publish :kind :effects :definition fade})
          [[:fade-a 0] [:fade-b 1]]))

(defn- run-show
  "Return captured packets per frame. Cue A launches at 0 ms, cue B at 500 ms,
  the transport stops at 1000 ms and resumes after the stopped frames."
  [overrides]
  (let [clock (volatile! 0.0)
        sink (bench/capture-sink)
        runtime (show/create-runtime (merge {:model (model) :workspace-options nil :sink sink
                                             :physical-now-ms #(deref clock)}
                                            overrides))
        frame! (fn [time-ms & [command]]
                 (vreset! clock (double time-ms))
                 (vreset! (:latest sink) [])
                 (let [reply (when command (show/submit! runtime :command command))]
                   (show/tick! runtime (double time-ms))
                   (when reply
                     (is (= :accepted (:status (deref reply 2000 {:status :timeout}))) (pr-str command)))
                   (mapv vec @(:latest sink))))]
    (try
      {:playing (vec (for [t (range 25 1000 25)]
                       (frame! t (case t 25 {:op :cue/launch :cue-id :fade-a}
                                         500 {:op :cue/launch :cue-id :fade-b} nil))))
       :stopped (vec (cons (frame! 1000 {:op :transport/set :transport {:playing? false}})
                           (for [t (range 1025 1200 25)] (frame! t))))
       :instances (count (:instances (:model @(:state runtime))))
       :resumed (vec (cons (frame! 1200 {:op :transport/set :transport {:playing? true}})
                           (for [t (range 1225 4000 25)] (frame! t))))}
      (finally (show/stop! runtime)))))

(deftest stopped-transport-keeps-shared-batch-fields-byte-stable
  (let [typed (run-show {:workers 2})
        scalar (run-show {:workers 1 :batch? false})
        stopped (:stopped typed)]
    (is (= 2 (:instances typed)) "two instances of one compiled effect")
    (testing "every stopped frame repeats the first one exactly"
      (is (every? seq stopped))
      (is (apply = (rest stopped)))
      (is (= (second stopped) (last stopped))))
    (testing "the typed host matches the scalar runtime frame for frame"
      (is (= (:playing scalar) (:playing typed)))
      (is (= (:stopped scalar) (:stopped typed)))
      (is (= (:resumed scalar) (:resumed typed))))
    (testing "the envelopes were mid-attack while stopped"
      (is (not= (last stopped) (last (:resumed typed)))))))

(def ^:private large-pixels
  ;; Above the 16,384 colour-and-white pixels where shared fields reached
  ;; 65,536 elements and stopped being retained (#875).
  20000)

(def ^:private large-rig
  (vec (for [i (range large-pixels)]
         {:id (str "pixel-" i) :universe (inc (quot i 128)) :address (inc (* 4 (mod i 128)))
          :position [(double (mod i 200)) 0.0 (double (quot i 200))]
          :channels {:red 1 :green 2 :blue 3 :white 4}})))

(def ^:private wave
  ;; One group, a colour and a white field on every pixel. It reads the beat,
  ;; so it evaluates while the transport plays and only then.
  {:id :wave :version 1 :params {:level {:type :number :default 0.6 :bounds [0 1]}}
   :outputs [{:attribute :color
              :value {:op :vec3 :args [{:op :multiply :args [{:op :param :key :level}
                                                            {:op :add :args [0.5 {:op :multiply :args [0.5 {:op :sin :args [{:op :add :args [{:op :multiply :args [{:op :position :axis :x} 0.05]} {:op :beat}]}]}]}]}]}
                                       0.1 {:op :multiply :args [{:op :position :axis :z} 0.01]}]}}
             {:attribute :white :value {:op :multiply :args [{:op :param :key :level} 0.5]}}]})

(defn- run-large-show
  "Count evaluations of the large group per frame while playing, then stopped."
  []
  (let [calls (atom 0)
        evaluate-batch ir/evaluate-batch
        clock (volatile! 0.0)
        sink (bench/capture-sink)
        model (-> (control/new-show {:show-id "shared-fields-large" :rig {:fixtures large-rig}})
                  (bench/command :publish {:op :registry/publish :kind :effects :definition wave})
                  (bench/command [:wave :cue] {:op :cue/put :cue {:id :wave :effect-id :wave :priority 0}}))]
    (with-redefs [ir/evaluate-batch (fn [plan env positions & more]
                                      (when (< 16384 (if (vector? positions) (count positions)
                                                         (quot (alength ^doubles positions) 3)))
                                        (swap! calls inc))
                                      (apply evaluate-batch plan env positions more))]
      (let [runtime (show/create-runtime {:model model :workspace-options nil :sink sink :workers 2
                                          :physical-now-ms #(deref clock)})
            frame! (fn [time-ms & [command]]
                     (vreset! clock (double time-ms))
                     (vreset! (:latest sink) [])
                     (reset! calls 0)
                     (let [reply (when command (show/submit! runtime :command command))]
                       (show/tick! runtime (double time-ms))
                       (when reply
                         (is (= :accepted (:status (deref reply 5000 {:status :timeout}))) (pr-str command)))
                       {:evaluations @calls :packets (mapv vec @(:latest sink))}))]
        (try
          {:playing (vec (for [t (range 25 150 25)]
                           (frame! t (when (= t 25) {:op :cue/launch :cue-id :wave}))))
           :stopped (vec (cons (frame! 150 {:op :transport/set :transport {:playing? false}})
                               (for [t (range 175 300 25)] (frame! t))))}
          (finally (show/stop! runtime)))))))

(deftest stopped-transport-retains-shared-fields-above-sixteen-thousand-pixels
  (let [{:keys [playing stopped]} (run-large-show)
        later (rest stopped)]
    (testing "the group evaluates on every playing frame after launch"
      (is (= [1 1 1 1] (mapv :evaluations (rest playing)))))
    (testing "a stopped transport reuses the retained 80,000-element fields"
      (is (<= (:evaluations (first stopped)) 1))
      (is (= [0 0 0 0 0] (mapv :evaluations later))))
    (testing "and every stopped frame repeats the same packets"
      (is (< 150 (count (:packets (last stopped)))) "157 universes of pixels")
      (is (apply = (map :packets later))))))
