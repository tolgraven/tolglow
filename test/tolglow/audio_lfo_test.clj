(ns tolglow.audio-lfo-test
  "Audio-shaped LFOs (#676): morph, silence policy, live registered waveform,
  parameter LFO and linked-knob use, persistence and clock rebase, offline
  replay of deterministic passages at two tempos, and the real owner and
  capture worker on injected PCM."
  (:require [afterglow.control.model :as control]
            [afterglow.render.ir :as ir]
            [clojure.test :refer [deftest is testing]]
            [tolglow.audio-lfo :as lfo]
            [tolglow.audio.fixtures :as fixtures]
            [tolglow.audio.shape :as shape]
            [tolglow.audio.shape-worker :as shape-worker]
            [tolglow.clock :as clock]
            [tolglow.connections :as connections]
            [tolglow.effects.parameter-sources :as sources]
            [tolglow.knobs :as knobs]
            [tolglow.modulation-test :as fixture]
            [tolglow.show :as show]
            [tolglow.signals :as signals]))

(def ^:private settings {:points 8 :morph {:beats 4} :curve :linear})

(defn- configured
  ([] (configured settings))
  ([config] (lfo/configure (assoc-in (fixture/model) [:transport :beat] 0.0) {:lfo-id :bass :config config})))

(defn- extraction [lfo-model shape seq & [extra]]
  (merge {:status :ok :shape shape :seq seq :run-id "run-1" :window {:end-beat 9.9}
          :spec-digest (lfo/spec-digest (get-in lfo-model [:audio-lfos :lfos :bass :config]))}
         extra))

(defn- at-beat [model beat] (assoc-in model [:transport :beat] beat))

(defn- state [model] (get-in model [:audio-lfos :lfos :bass]))

(def ^:private ramp [0.0 0.25 0.5 0.75 1.0 0.75 0.5 0.25])

(def ^:private square [1.0 1.0 1.0 1.0 0.0 0.0 0.0 0.0])

(defn- close? [tolerance a b] (and (= (count a) (count b)) (every? true? (map #(<= (Math/abs (- (double %1) (double %2))) tolerance) a b))))

(deftest configuration-is-validated-and-publishes-a-live-waveform
  (let [model (configured)
        registry (:waveforms model)
        waveform (get-in registry [:waveforms :audio/bass])]
    (is (= (signals/validate-registry registry) registry))
    (is (= {:live? true :category :audio :params {} :range [0.0 1.0] :name "Audio · bass"}
           (select-keys waveform [:live? :category :params :range :name])))
    (is (= {:type :audio-lfo :lfo-id :bass :band :bass :extract :envelope} (:source waveform)))
    (is (= (vec (repeat 49 0.5)) (signals/glyph registry :audio/bass)) "the mid rest shape before any audio")
    (is (= :waiting (:status (state model))))
    (doseq [[config message] [[{:band :treble} #"Band"] [{:points 100} #"points"] [{:morph {:ms 300}} #"Morph"]
                              [{:smoothing 1.0} #"Smoothing"] [{:on-silence :fade} #"Silence"] [{:speed 2} #"Unknown"]]]
      (let [error (try (lfo/apply-operation (fixture/model) {:op :audio-lfo/configure :lfo-id :bass :config config})
                       nil (catch clojure.lang.ExceptionInfo e e))]
        (is (= :invalid-audio-lfo (:control/error (ex-data error))) (pr-str config))
        (is (re-find message (ex-message error)))))
    (testing "the waveform ID cannot take over another waveform"
      (let [other (assoc (first (signals/builtin-waveforms)) :id :audio/bass)
            taken (assoc (fixture/model) :waveforms (signals/publish (signals/new-registry) other))]
        (is (thrown-with-msg? clojure.lang.ExceptionInfo #"already uses"
                              (lfo/configure taken {:lfo-id :bass :config settings})))))))

(deftest the-tail-guard-is-an-optional-extractor-setting
  (let [model (configured)
        guarded (lfo/configure model {:lfo-id :bass :config {:extract :cycle :tail-guard {:beats 0.0625}}})]
    (is (nil? (get-in model [:audio-lfos :lfos :bass :config :tail-guard])) "the default guard is not stored")
    (is (= {:beats 0.0625} (:tail-guard (get (lfo/extractor-specs guarded) :bass))))
    (is (not= (lfo/spec-digest (get-in model [:audio-lfos :lfos :bass :config]))
              (lfo/spec-digest (get-in guarded [:audio-lfos :lfos :bass :config])))
        "a new guard waits for the next window")
    (is (= (lfo/spec-digest (assoc settings :name "bass" :extract :cycle))
           (lfo/spec-digest (get-in (lfo/configure guarded {:lfo-id :bass :config {:tail-guard nil}})
                                    [:audio-lfos :lfos :bass :config])))
        "nil returns to the default")
    (is (= (:audio-lfos guarded) (lfo/validate-state (:audio-lfos guarded))))
    (is (= {:beats 0.0625}
           (get-in (lfo/configure guarded {:lfo-id :bass :config {:band :bass :extract :cycle :window {:bars 1} :morph {:bars 1}
                                                                  :smoothing 0.25 :on-silence :hold}})
                   [:audio-lfos :lfos :bass :config :tail-guard]))
        "Studio's Apply, which sends no guard, keeps a guard set from the REPL")
    (let [error (try (lfo/apply-operation model {:op :audio-lfo/configure :lfo-id :bass :config {:tail-guard {:beats 0.5}}})
                     nil (catch clojure.lang.ExceptionInfo e e))]
      (is (= :invalid-audio-lfo (:control/error (ex-data error))))
      (is (re-find #"Tail guard" (ex-message error))))
    (testing "the analysed span reaches the projection"
      (let [admitted (lfo/admit (at-beat model 10.0) {:bass (extraction model ramp 1 {:analysed-until-beat 9.87
                                                                                        :tail-guard {:beats 0.03 :ms 15.0}})})]
        (is (= {:analysed-until-beat 9.87 :tail-guard {:beats 0.03 :ms 15.0}}
               (select-keys (:last (lfo/lfo-view admitted :bass)) [:analysed-until-beat :tail-guard])))))))

(deftest a-show-has-a-bounded-number-of-audio-lfos
  (let [full (reduce (fn [m i] (lfo/configure m {:lfo-id (keyword (str "lfo-" i)) :config {}}))
                     (fixture/model) (range lfo/max-lfos))
        refused (try (lfo/apply-operation full {:op :audio-lfo/configure :lfo-id :one-more :config {}})
                     nil (catch clojure.lang.ExceptionInfo e e))]
    (is (= lfo/max-lfos (count (get-in full [:audio-lfos :lfos]))))
    (is (= :invalid-audio-lfo (:control/error (ex-data refused))))
    (is (re-find #"at most 8 audio LFOs" (ex-message refused)))
    (is (= :mid (get-in (lfo/configure full {:lfo-id :lfo-0 :config {:band :mid}}) [:audio-lfos :lfos :lfo-0 :config :band]))
        "an existing LFO can still be changed")))

(deftest the-shape-worker-finishes-jobs-off-the-calling-thread
  (let [samples (fixtures/passage :dry-sine {:bpm 120.0 :bars 2})
        specs {:a {:band :bass :extract :cycle :digest 11} :b {:band :sub :extract :cycle :digest 12}}
        jobs (loop [start 0 bank (shape/new-bank 48000.0) jobs []]
               (if (>= start (alength samples)) jobs
                   (let [end (min (alength samples) (+ start 480))
                         r (shape/advance-bank bank specs {:samples (java.util.Arrays/copyOfRange samples (int start) (int end))
                                                           :sample-rate 48000.0 :started-at-ms (* 1000.0 (/ start 48000.0))
                                                           :clock {:time-ms 0.0 :beat 0.0 :bpm 120.0 :playing? true}})]
                     (recur end (:bank r) (into jobs (:jobs r))))))
        published (atom [])
        threads (atom #{})
        worker (shape-worker/start! (fn [result] (swap! threads conj (.getName (Thread/currentThread)))
                                      (swap! published conj result)))]
    (try
      (is (= [[:a 1] [:b 1]] (sort (map (juxt :id :seq) jobs))))
      ;; Two rounds of the same LFOs in one hand-off: only the newest per LFO is kept.
      ((:submit! worker) (concat (map #(assoc % :seq 0) jobs) jobs))
      (is (true? ((:stop! worker) true 5000)) "a drain finishes what was handed over")
      (is (= #{"tolglow-audio-shapes"} @threads))
      (is (= [[:a 1] [:b 1]] (sort (for [result @published [id extraction] result] [id (:seq extraction)])))
          "one result per LFO, from its newest job")
      (is (every? #(= :ok (:status %)) (mapcat vals @published)))
      (is (= 110 (Math/round (double (some #(get-in % [:a :frequency-hz]) @published)))))
      (is (= #{11 12} (set (map :spec-digest (mapcat vals @published)))))
      (testing "a cancelled worker drops pending work and accepts no more"
        (let [late (atom 0) worker (shape-worker/start! (fn [_] (swap! late inc)))]
          ((:stop! worker) false 1000)
          ((:submit! worker) jobs)
          (is (zero? ((:pending worker))))
          (is (zero? @late))))
      (finally ((:stop! worker) false 1000)))))

(deftest the-template-is-the-same-table-the-owner-samples
  (let [random (java.util.Random. 676)]
    (doseq [points [8 32 63]]
      (let [table (vec (repeatedly points #(/ (Math/round (* 10000.0 (.nextDouble random))) 10000.0)))
            evaluate (ir/compile-expression (lfo/table-expression table))]
        (doseq [phase (concat [0.0 0.5 0.999999 1.0 -0.25 3.125] (repeatedly 40 #(* 4.0 (.nextDouble random))))]
          (is (< (Math/abs (- (evaluate {:params {signals/phase-input phase}}) (shape/table-value table phase))) 1.0e-12)
              (pr-str points phase)))))))

(deftest a-new-shape-morphs-from-what-is-showing
  (let [model (configured)
        first-shape (lfo/admit (at-beat model 10.0) {:bass (extraction model ramp 1)})
        lfo (state first-shape)]
    (is (= 1 (:commits lfo)))
    (is (= ramp (:to lfo)))
    (is (= 10.0 (:morph-start-beat lfo)))
    (is (= 4.0 (:morph-beats lfo)))
    (is (= (vec (repeat 8 0.5)) (lfo/shape-at lfo 10.0)) "the morph starts at the shape that was showing")
    (is (close? 1.0e-12 (mapv #(+ 0.5 (* 0.5 (- % 0.5))) ramp) (lfo/shape-at lfo 12.0)) "halfway, linear")
    (is (= ramp (lfo/shape-at lfo 14.0)))
    (is (= ramp (lfo/shape-at lfo 99.0)))
    (is (< (Math/abs (- 0.1 (:latency-beats (:last lfo)))) 1.0e-9) "window ended at 9.9, committed at 10")
    (testing "a shape that arrives mid-morph continues from the blend without a jump"
      (let [second-shape (lfo/admit (at-beat first-shape 12.0) {:bass (extraction model square 2)})
            next-lfo (state second-shape)]
        (is (= (lfo/shape-at lfo 12.0) (lfo/shape-at next-lfo 12.0)))
        (is (= square (lfo/shape-at next-lfo 16.0)))
        (is (= [(vec (repeat 8 0.5)) ramp] (:history next-lfo)) "the replaced targets, oldest first")))
    (testing "the smoothstep curve eases both ends"
      (let [eased (state (lfo/admit (at-beat (configured (assoc settings :curve :smoothstep)) 0.0)
                                    {:bass (extraction model ramp 1)}))]
        (is (= 0.5 (lfo/progress eased 2.0)))
        (is (< (lfo/progress eased 0.4) 0.03))))
    (testing "a transport that jumps back without a rebase shows the target"
      (is (= ramp (lfo/shape-at lfo 3.0))))))

(deftest silence-and-low-confidence-follow-the-policy
  (let [model (lfo/admit (at-beat (configured) 0.0) {:bass (extraction (configured) ramp 1)})
        quiet (fn [m status seq] (lfo/admit (at-beat m 8.0) {:bass (extraction (configured) nil seq {:status status})}))]
    (testing "hold keeps the target and reports the state"
      (let [held (state (quiet model :silent 2))]
        (is (= ramp (:to held)))
        (is (= :silent (:status held)))
        (is (= 1 (:commits held)))))
    (testing "low confidence always holds"
      (let [released (lfo/configure model {:lfo-id :bass :config {:on-silence :release}})
            unsure (state (quiet released :low-confidence 2))]
        (is (= ramp (:to unsure)))
        (is (= :low-confidence (:status unsure)))))
    (testing "release morphs to the rest shape"
      (let [released (lfo/configure model {:lfo-id :bass :config {:on-silence :release :rest-shape :low}})
            rested (state (quiet released :silent 2))]
        (is (= (vec (repeat 8 0.0)) (:to rested)))
        (is (= ramp (:from rested)))
        (is (= 8.0 (:morph-start-beat rested)))))))

(deftest smoothing-keeps-part-of-the-previous-target
  (let [model (configured (assoc settings :smoothing 0.5))
        lfo (state (lfo/admit (at-beat model 0.0) {:bass (extraction model square 1)}))]
    (is (= [0.75 0.75 0.75 0.75 0.25 0.25 0.25 0.25] (:to lfo)) "half of the mid rest shape, half the new square")))

(deftest only-fresh-extractions-for-the-current-spec-are-admitted
  (let [model (lfo/admit (at-beat (configured) 0.0) {:bass (extraction (configured) ramp 5)})
        same (fn [m] (= (state m) (state model)))]
    (is (same (lfo/admit model {:bass (extraction (configured) square 5)})) "a sequence already committed")
    (is (same (lfo/admit model {:bass (extraction (configured) square 4)})) "an older sequence")
    (is (same (lfo/admit model {:bass (extraction (configured) square 6 {:spec-digest 1})})) "an older band or window")
    (is (same (lfo/admit model {:treble (extraction (configured) square 6)})) "an LFO that does not exist")
    (is (= square (:to (state (lfo/admit model {:bass (extraction (configured) square 1 {:run-id "run-2"})}))))
        "a new capture run starts its own sequence")
    (testing "reconfiguring the extractor waits for the next window"
      (let [rebanded (lfo/configure model {:lfo-id :bass :config {:band :mid}})]
        (is (= :waiting (:status (state rebanded))))
        (is (= (state rebanded) (state (lfo/admit rebanded {:bass (extraction (configured) square 6)})))
            "measured under the old band")
        (is (= square (:to (state (lfo/admit rebanded {:bass (extraction rebanded square 6)})))))))))

(deftest a-shape-every-bar-keeps-one-registry-version
  (let [model (reduce (fn [m i]
                        (lfo/admit (at-beat m (* 4.0 i))
                                   {:bass (extraction m (assoc ramp (mod i 8) (/ i 40.0)) i)}))
                      (configured) (range 1 31))
        registry (:waveforms model)]
    (is (= [[:audio/bass 31]] (filterv #(= :audio/bass (first %)) (keys (:versions registry)))))
    (is (= 31 (get-in registry [:waveforms :audio/bass :version])))
    (is (= registry (signals/validate-registry registry)))
    (is (= (:to (state model)) (vec (butlast (signals/glyph registry :audio/bass {:samples 9}))))
        "the picker glyph samples the published target")
    (is (thrown? clojure.lang.ExceptionInfo (signals/publish-live registry (assoc (first (signals/builtin-waveforms)) :live? true)))
        "a live waveform never replaces a versioned one")
    (testing "a selector choice recorded at an older version follows the current one"
      (let [choices (:choices (signals/selector-schema (:waveforms (configured)) [:sine :audio/bass]))]
        (is (= 1 (:version (second choices))))
        (is (= (signals/sample registry :audio/bass {}) (signals/sample registry :audio/bass {:version 1})))
        (is (map? (signals/select-waveform registry {:op :const :value 1.0} (mapv #(select-keys % [:id :version]) choices) {})))
        (is (thrown? clojure.lang.ExceptionInfo (signals/sample registry :sine {:version 2}))
            "a versioned waveform still refuses an unknown version")))))

(def ^:private audio-binding
  {:type :lfo :waveform :audio/bass :period {:beats 1} :from 0.0 :to 1.0 :phase-reference :transport})

(deftest a-parameter-lfo-follows-the-morph-and-holds-while-stopped
  (let [model (-> (lfo/admit (at-beat (configured) 10.0) {:bass (extraction (configured) ramp 1)})
                  (sources/bind {:instance-id "0:1" :param :level :binding audio-binding}))
        lfo (state model)
        level (fn [frame] (get-in (sources/sample model frame {}) [:overrides "0:1" :level]))
        expected (fn [beat] (shape/table-value (lfo/shape-at lfo beat) beat))]
    (doseq [beat [10.0 10.375 11.625 12.25 13.5 14.0 15.125]]
      (is (< (Math/abs (- (expected beat) (level {:beat beat :time-ms 0.0}))) 1.0e-12) (str "beat " beat)))
    (is (not= (level {:beat 12.375 :time-ms 0.0})
              (shape/table-value ramp 12.375))
        "mid-morph the LFO is not the published target")
    (testing "a stopped transport holds the beat, so the morph and the LFO stand still"
      (let [stopped {:beat 12.25 :time-ms 90000.0 :playing? false}]
        (is (= (level {:beat 12.25 :time-ms 0.0}) (level stopped)))
        (is (= (expected 12.3) (level {:beat 12.3 :time-ms 95000.0 :playing? true}))
            "on play it continues from the held beat, with no catch-up")))
    (testing "a saved binding survives the live waveform moving on"
      (let [later (lfo/admit (at-beat model 20.0) {:bass (extraction (configured) square 2)})]
        (is (> (get-in later [:waveforms :waveforms :audio/bass :version])
               (get-in later [:instances "0:1" :parameter-bindings :level :waveform-version])))
        (is (nil? (sources/validate-saved! later (get-in later [:instances "0:1"]))))))
    (testing "after the LFO is removed its waveform plays the frozen target"
      (let [removed (:model (lfo/apply-operation model {:op :audio-lfo/remove :lfo-id :bass}))]
        (is (= (shape/table-value ramp 12.25) (get-in (sources/sample removed {:beat 12.25 :time-ms 0.0} {})
                                                      [:overrides "0:1" :level])))))))

(deftest a-linked-knob-scans-the-audio-lfo-period
  (let [model (sources/bind (configured) {:instance-id "0:1" :param :level :binding audio-binding})
        created (knobs/apply-operation model {:op :knob/create :knob-id :scan :name "Scan"
                                              :links [{:id "rate" :destination {:kind :lfo :instance-id "0:1" :param :level :field :period}
                                                       :range [4 1/2] :quantize {:ratios :preferred}}]})
        turned (knobs/apply-operation (:model created) {:op :knob/set :knob-id :scan :value 1.0})]
    (is (= [:applied] (mapv :status (get-in turned [:result :links]))))
    (is (= {:beats 1/2} (get-in turned [:model :instances "0:1" :parameter-bindings :level :binding :period])))))

(deftest state-survives-save-restore-and-clock-rebase
  (let [model (lfo/admit (at-beat (configured) 10.0) {:bass (extraction (configured) ramp 1)})
        saved (:audio-lfos model)]
    (is (= saved (lfo/validate-state saved)))
    (is (thrown? clojure.lang.ExceptionInfo (lfo/validate-state (assoc-in saved [:lfos :bass :to] [2.0]))))
    (let [restored (get-in (lfo/restore saved) [:lfos :bass])]
      (is (= ramp (:from restored) (:to restored)) "a restored LFO resumes at its saved target")
      (is (= :waiting (:status restored))))
    (is (= 18.0 (get-in (lfo/rebase-clock saved 8.0) [:lfos :bass :morph-start-beat])))
    (testing "a seek through the show clock keeps morph progress"
      (let [before (lfo/shape-at (state model) 12.0)
            rebased (clock/rebase (at-beat model 12.0) {:beat 100.0})]
        (is (= 98.0 (:morph-start-beat (state rebased))))
        (is (= before (lfo/shape-at (state rebased) 100.0)))))))

(deftest replayed-passages-at-two-tempos
  (doseq [bpm [84.0 172.0]]
    (testing (str bpm " BPM")
      (let [run (fn [kind config] (lfo/replay (fixtures/passage kind {:bpm bpm :bars 8}) {:bpm bpm :config config}))
            frame-ms (/ 1000.0 60.0)
            sine-cycle (run :dry-sine {:extract :cycle})
            synth-cycle (run :delay-synth {:extract :cycle})
            guitar (run :guitar-phrase {:extract :envelope})]
        (doseq [{:keys [commits]} [sine-cycle synth-cycle guitar]]
          (is (= 7 (count commits)) "one extraction per whole bar after the first")
          (is (every? #(<= 0.0 (:latency-ms %) (+ 10.0 frame-ms)) commits) "within one block and one frame of the bar line")
          (is (every? #(< (:table-jump %) 1.0e-9) commits) "no table discontinuity at a window change"))
        (testing "a dry sine gives a steady sine cycle"
          (is (every? #(and (= :ok (:status %)) (< (Math/abs (- 110.0 (:frequency-hz %))) 0.5)) (:commits sine-cycle))))
        (testing "the delayed synth alternates saw and square bars, so the LFO keeps morphing"
          (is (every? #(= :ok (:status %)) (:commits synth-cycle)))
          (is (every? :committed? (:commits synth-cycle))))
        (testing "the plucked phrase alternates a shaped bar and a silent one; hold keeps the last shape"
          (is (= [:ok :silent :ok :silent :ok :silent :ok] (mapv :status (:commits guitar))))
          (is (= [true false true false true false true] (mapv :committed? (:commits guitar)))))
        (is (< (:max-on-commit (:steps synth-cycle)) (+ 0.05 (:max (:steps synth-cycle))))
            "commit frames move no more than the LFO's ordinary motion")))))

(defn- pcm-source [^bytes bytes chunk closed]
  (let [position (atom 0)]
    {:started-at-ms 0.0 :description {:name "Injected passage"}
     :read! (fn [out offset length]
              (let [n (min length chunk (- (alength bytes) @position))]
                (if (zero? n) -1 (do (System/arraycopy bytes @position out offset n) (swap! position + n) n))))
     :close! #(swap! closed inc)}))

(def ^:private wash
  {:id :wash :version 1 :category :light
   :params {:level {:type :number :default 0.0 :bounds [0.0 1.0]}}
   :outputs {:dimmer {:op :param :key :level}}})

(deftest the-owner-commits-shapes-from-real-capture
  (let [closed (atom 0)
        bpm 120.0
        bytes (fixtures/pcm16-bytes (fixtures/passage :delay-synth {:bpm bpm :bars 4 :sample-rate 8000.0}))
        ;; Capture timestamps start at 0 ms; the transport reports beat 16 at 8000 ms.
        model (control/new-show {:show-id :audio-lfo-test :time-ms 8000.0
                                 :transport {:playing? true :beat 16.0 :bpm bpm :time-ms 8000.0}
                                 :registry {:effects {:wash wash} :cues {:wash {:id :wash :effect-id :wash :version 1}}}
                                 :rig {:fixtures [{:id :left :universe 1 :address 1 :channels {:dimmer 1}}]}})
        rt (show/create-runtime {:model model :workspace-options nil
                                 :audio-options {:open-source! (fn [_] (pcm-source bytes 640 closed))}})]
    (try
      (show/tick! rt 8000)
      (let [at! (fn [time kind payload]
                  (let [reply (show/submit! rt kind payload)]
                    (show/tick! rt time)
                    (deref reply 5000 {:status :timeout})))
            configure (at! 8001 :command {:op :audio-lfo/configure :lfo-id :bass
                                          :config {:extract :cycle :points 16 :morph {:bars 1}}})
            launch (at! 8002 :command {:op :cue/launch :cue-id :wash})
            bound (at! 8003 :modulation-bind {:instance-id (:instance-id launch) :param :level
                                              :binding (assoc audio-binding :period {:beats 2})})
            handle (connections/start-audio! (:connections rt) {:default? true :sample-rate 8000.0 :block-frames 400}
                                             (show/audio-callbacks rt))]
        (is (= :accepted (:status configure)) (pr-str configure))
        (is (= :accepted (:status bound)) (pr-str bound))
        (is (= :completed (:status (deref (:result handle) 5000 ::timeout))))
        (show/tick! rt 8004)
        (let [snapshot (show/snapshot rt)
              [view] (:audio-lfos snapshot)
              lfo (get-in @(:state rt) [:model :audio-lfos :lfos :bass])]
          (testing "the worker extracted whole bars on the transport's beat grid and the owner committed them"
            (is (= :ok (:status view)))
            (is (= {:index 2 :start-beat 8.0 :end-beat 12.0} (select-keys (get-in view [:last :window]) [:index :start-beat :end-beat]))
                "the last whole bar the four-bar passage closed; the newest extraction wins the mailbox")
            (is (< 50.0 (get-in view [:last :frequency-hz]) 90.0) "the 55 and 82.5 Hz bass notes")
            (is (= {:beats 0.03125 :ms 15.625} (get-in view [:last :tail-guard])) "1/32 beat at 120 BPM (#912)")
            (is (< (Math/abs (- (- 12.0 0.03125) (get-in view [:last :analysed-until-beat]))) (/ 3.0 8000.0))
                "the REPL and Studio read the beat the cycle was analysed up to")
            (is (= 16 (count (:to lfo))))
            (is (= :pcm (get-in view [:input :kind])))
            (is (= "bass 60 Hz–250 Hz" (:band-label view))))
          (testing "the live waveform and the bound parameter follow it"
            (is (= (:to lfo) (mapv #(/ (Math/round (* 10000.0 %)) 10000.0) (:to view))))
            (is (pos? (get-in @(:state rt) [:model :waveforms :waveforms :audio/bass :version])))
            (is (some #(= :audio/bass (:id %)) (:parameter-waveforms snapshot)))
            (is (= :lfo (get-in (first (:instances snapshot)) [:parameter-sources :level :sources 0 :type])))))
        (testing "removing the LFO is an owner command too"
          (is (= :accepted (:status (at! 8005 :command {:op :audio-lfo/remove :lfo-id :bass}))))
          (show/tick! rt 8006)
          (is (= [] (:audio-lfos (show/snapshot rt))))))
      (finally (show/stop! rt)))))
