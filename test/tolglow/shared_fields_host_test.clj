(ns tolglow.shared-fields-host-test
  "Typed batch fields are retained and shared read-only by their pure binding.
  Two instances of one compiled effect, scaled by different envelopes, run
  through the host tick on a stopped transport against the scalar runtime."
  (:require [afterglow.control.model :as control]
            [afterglow.render.ir :as ir]
            [afterglow.runtime.cadence :as cadence]
            [afterglow.runtime.keyframes :as keyframes]
            [afterglow.runtime.reuse :as reuse]
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
          (let [playing (vec (for [t (range 25 150 25)]
                               (frame! t (when (= t 25) {:op :cue/launch :cue-id :wave}))))
                stopped (vec (cons (frame! 150 {:op :transport/set :transport {:playing? false}})
                                   (for [t (range 175 300 25)] (frame! t))))
                held (show/retained-pure-bindings runtime)
                instance-id (first (keys (:instances (:model @(:state runtime)))))
                released (vec (cons (frame! 300 {:op :instance/end :instance-id instance-id})
                                    (for [t (range 325 400 25)] (frame! t))))]
            {:playing playing :stopped stopped :held held :released released
             :instances (count (:instances (:model @(:state runtime))))
             :retained-after-release (show/retained-pure-bindings runtime)})
          (finally (show/stop! runtime)))))))

(defn- lit? [packets]
  ;; ArtDMX payload starts after the 18-byte header.
  (boolean (some (fn [packet] (some #(not (zero? %)) (drop 18 packet))) packets)))

(deftest stopped-transport-retains-shared-fields-above-sixteen-thousand-pixels
  (let [{:keys [playing stopped held released instances retained-after-release]} (run-large-show)
        later (rest stopped)]
    (testing "the group evaluates on every playing frame after launch"
      (is (= [1 1 1 1] (mapv :evaluations (rest playing)))))
    (testing "a stopped transport reuses the retained 80,000-element fields"
      (is (<= (:evaluations (first stopped)) 1))
      (is (= [0 0 0 0 0] (mapv :evaluations later))))
    (testing "every later stopped frame repeats the stop frame's lit packets"
      (is (< 150 (count (:packets (first stopped)))) "157 universes of pixels")
      (is (lit? (:packets (first stopped))))
      (is (every? #(= (:packets (first stopped)) (:packets %)) later)))
    (testing "a released cue retains nothing"
      (is (seq held) "the stopped cue's program retained its fields")
      (is (zero? instances))
      (is (every? #(zero? (:evaluations %)) released))
      (is (not-any? reuse/retains? held))
      (is (empty? retained-after-release)))))

(deftest deferred-keyframe-fallback-records-the-pure-bindings-it-creates
  ;; A GPU-deferred step builds its CPU fallback on first use, outside the
  ;; compile. Its bindings must still reach the step's ::pure-bindings so the
  ;; show can clear them when the program stops running.
  (let [definition (-> wave (assoc :id :deferred-wave) (update :outputs #(subvec % 0 1)))
        compile (#'show/recording-bindings
                 (batch/compiler-for-rig rig {:bind-pure #'show/bind-pure :prepare-group (constantly {})}))
        step (compile definition)
        recorded #(vec (::show/pure-bindings (meta step)))
        {:keys [samples measure-effect]} (cadence/observer)
        input {:effect-id "deferred"
               :frame {:time-ms 0 :beat 0.0 :bpm 120.0 :fixtures rig :transport {:beat 0.0 :bpm 120}
                       :measure-effect measure-effect :keyframe-intervals {"deferred" {:ms 100}}}
               :instance {:id "deferred" :definition definition :effect-id :deferred-wave :params {}
                          :started-at-ms 0 :started-at-beat 0 :phase :running
                          :timing-meter {:beats-per-bar 4 :bars-per-phrase 4}}
               :state {} :assignments [] :contributions [] :intents [] :done? false}]
    (is (= :deferred (:backend (meta step))))
    (let [at-compile (recorded)]
      (is (not-any? reuse/retains? at-compile))
      (keyframes/evaluate step input)
      (is (pos? (get-in @samples ["deferred" :calls] 0)) "endpoints were sampled on the CPU fallback")
      (let [added (subvec (recorded) (count at-compile))]
        (is (seq added) "the fallback's bindings join the step's record when it is built")
        (is (some reuse/retains? added) "its CPU group retained the sampled endpoint field")
        (run! reuse/clear! (recorded))
        (is (not-any? reuse/retains? (recorded)) "clearing the record releases it")))))

(def ^:private sampled-wave
  ;; The large wave reading only x: 200 columns, so its group evaluates 200
  ;; samples and expands them to 80,000 elements per frame (#874).
  (assoc wave :id :sampled-wave
         :outputs [{:attribute :color
                    :value {:op :vec3 :args [{:op :multiply :args [{:op :param :key :level}
                                                                  {:op :add :args [0.5 {:op :multiply :args [0.5 {:op :sin :args [{:op :add :args [{:op :multiply :args [{:op :position :axis :x} 0.05]} {:op :beat}]}]}]}]}]}
                                             0.1 0.2]}}
                   {:attribute :white :value {:op :multiply :args [{:op :param :key :level} 0.5]}}]))

(deftest stopped-transport-retains-coordinate-sampled-fields-above-sixteen-thousand-pixels
  (let [calls (atom 0)
        bind-pure (fn [spec calculate] (reuse/evaluator spec (fn [inputs] (swap! calls inc) (calculate inputs))))
        step (batch/compile-effect sampled-wave large-rig {:bind-pure bind-pure})
        plain (batch/compile-effect sampled-wave large-rig {:coordinate-classes? false})
        frame (fn [t beat]
                {:frame {:time-ms t :fixtures large-rig :transport {:beat beat :bpm 120}}
                 :instance {:id "large" :definition sampled-wave :effect-id :sampled-wave :params {}
                            :started-at-ms 0 :started-at-beat 0 :phase :running
                            :timing-meter {:beats-per-bar 4 :bars-per-phrase 4}}
                 :state {} :assignments [] :contributions [] :intents [] :done? false})
        stopped (mapv #(step (frame % 1.0)) [500 525 550 575])
        color #(get-in % [:contributions 0 :batch :outputs :color :values])]
    (is (= [200] (:coordinate-samples (meta step))))
    (is (= 60000 (alength ^doubles (color (first stopped)))) "three colour components per pixel")
    (is (= 1 @calls) "four stopped frames evaluate once")
    (is (every? #(identical? (color (first stopped)) (color %)) stopped) "the expanded array is retained by reference")
    (is (java.util.Arrays/equals ^doubles (color (plain (frame 500 1.0))) ^doubles (color (first stopped))))
    (step (frame 600 1.25))
    (is (= 2 @calls) "a moving transport evaluates again")))
