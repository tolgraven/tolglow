(ns tolglow.shared-fields-host-test
  "Typed batch fields are retained and shared read-only by their pure binding.
  Two instances of one compiled effect, scaled by different envelopes, run
  through the host tick on a stopped transport against the scalar runtime."
  (:require [afterglow.control.model :as control]
            [clojure.test :refer [deftest is testing]]
            [tolglow.bench-show :as bench]
            [tolglow.compiler.batch :as batch]
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

(def ^:private matrix-rig
  ;; Eight columns of six pixels: the colour field reads only x, so its batch
  ;; evaluates one sample per column (#874).
  (vec (concat
        (for [i (range 48)] {:id (str "pixel-" i) :address (inc (mod (* 4 i) 512)) :universe 1
                             :position [(double (mod i 8)) 0.0 (double (quot i 8))]
                             :channels {:red 1 :green 2 :blue 3 :white 4}})
        (for [i (range 4)] {:id (str "par-" i) :universe 1 :address (+ 193 (* 4 i)) :position [(double i) 2.0 0.0]
                            :channels {:red 1 :green 2 :blue 3 :dimmer 4}}))))

(defn- model [rig]
  (reduce (fn [m [id priority]]
            (bench/command m [id :cue] {:op :cue/put :cue {:id id :effect-id :fade :priority priority}}))
          (bench/command (control/new-show {:show-id "shared-fields" :rig {:fixtures rig}})
                         :publish {:op :registry/publish :kind :effects :definition fade})
          [[:fade-a 0] [:fade-b 1]]))

(defn- run-show
  "Return captured packets per frame. Cue A launches at 0 ms, cue B at 500 ms,
  the transport stops at 1000 ms and resumes after the stopped frames."
  ([overrides] (run-show rig overrides))
  ([rig overrides]
   (let [clock (volatile! 0.0)
         sink (bench/capture-sink)
         runtime (show/create-runtime (merge {:model (model rig) :workspace-options nil :sink sink
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
       (finally (show/stop! runtime))))))

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

(deftest stopped-transport-keeps-coordinate-sampled-fields-byte-stable
  (is (= [8 nil] (:coordinate-samples (meta (batch/compile-effect fade matrix-rig))))
      "the colour batch samples one pixel per column; the par dimmer reads no coordinate")
  (let [typed (run-show matrix-rig {:workers 2})
        scalar (run-show matrix-rig {:workers 1 :batch? false})
        stopped (:stopped typed)]
    (is (= 2 (:instances typed)))
    (testing "every stopped frame repeats the first one exactly"
      (is (every? seq stopped))
      (is (apply = (rest stopped))))
    (testing "captured ArtDMX matches the scalar runtime frame for frame"
      (is (= (:playing scalar) (:playing typed)))
      (is (= (:stopped scalar) (:stopped typed)))
      (is (= (:resumed scalar) (:resumed typed))))))
