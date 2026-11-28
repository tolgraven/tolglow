(ns tolglow.audio-lfo-knobs-test
  "Linked knobs on an audio-shaped LFO's band and morph time (#676): validated
  links, retimed morphs without a jump, a stopped transport, the shared
  projection, and the real owner and capture worker on injected PCM."
  (:require [afterglow.control.model :as control]
            [clojure.test :refer [deftest is testing]]
            [tolglow.audio-lfo :as lfo]
            [tolglow.audio.fixtures :as fixtures]
            [tolglow.connections :as connections]
            [tolglow.knobs :as knobs]
            [tolglow.modulation-test :as fixture]
            [tolglow.show :as show]))

(def ^:private settings {:points 8 :morph {:beats 4} :curve :linear})

(def ^:private ramp [0.0 0.25 0.5 0.75 1.0 0.75 0.5 0.25])

(defn- configured []
  (lfo/configure (assoc-in (fixture/model) [:transport :beat] 0.0) {:lfo-id :bass :config settings}))

(defn- extraction [model shape seq]
  {:status :ok :shape shape :seq seq :run-id "run-1" :window {:end-beat 9.9}
   :spec-digest (lfo/spec-digest (get-in model [:audio-lfos :lfos :bass :config]))})

(defn- at-beat [model beat] (assoc-in model [:transport :beat] beat))

(defn- state [model] (get-in model [:audio-lfos :lfos :bass]))

(def ^:private band-link {:id "band" :destination {:kind :audio-lfo :lfo-id :bass :field :band} :range [:sub :full]})

(def ^:private morph-link {:id "morph" :destination {:kind :audio-lfo :lfo-id :bass :field :morph}
                           :range [1 8] :quantize {:ratios :preferred}})

(defn- refusal [model command]
  (try (knobs/apply-operation model command) nil
       (catch clojure.lang.ExceptionInfo error [(:control/error (ex-data error)) (ex-message error)])))

(defn- turn [model knob-id value]
  (knobs/apply-operation model {:op :knob/set :knob-id knob-id :value value}))

(deftest a-knob-steps-the-band-and-scales-the-morph-time
  (let [created (knobs/apply-operation (configured) {:op :knob/create :knob-id :sweep :name "Sweep"
                                                     :links [band-link morph-link]})
        model (:model created)]
    (doseq [[value band morph] [[0.0 :sub 1] [0.2 :bass 4/3] [0.5 :mid 8/3] [0.99 :full 8] [1.0 :full 8]]]
      (let [turned (turn model :sweep value)
            config (:config (state (:model turned)))]
        (is (= [:applied :applied] (mapv :status (get-in turned [:result :links]))) (str value))
        (is (= band (:band config)) (str "band at " value))
        (is (= {:beats morph} (:morph config)) (str "morph at " value))))
    (testing "the knob projection shows each link resolved against the LFO"
      (let [turned (:model (turn model :sweep 0.2))
            [band morph] (:links (knobs/knob-view turned :sweep))]
        (is (= [:ok :ok] [(:status band) (:status morph)]))
        (is (= "audio LFO bass · band" (:label band)))
        (is (= "bass" (get-in band [:resolved :text])))
        (is (= "4/3 beats" (get-in morph [:resolved :text])))
        (is (= [6 1] [(:steps band) (:step band)]) "six named bands; 0.2 selects the second")
        (is (not (:diverged? band)))))
    (testing "the audio LFO projection names the knobs that drive it"
      (is (= [{:knob-id :sweep :knob-name "Sweep" :value 0.0 :link-id "band" :field :band :range [:sub :full]}
              {:knob-id :sweep :knob-name "Sweep" :value 0.0 :link-id "morph" :field :morph :range [1 8]}]
             (:knobs (lfo/lfo-view model :bass)))))
    (testing "an edit from the REPL or Studio shows as a divergence until the knob moves"
      (let [edited (:model (lfo/apply-operation model {:op :audio-lfo/configure :lfo-id :bass :config {:band :high}}))
            band (first (:links (knobs/knob-view edited :sweep)))]
        (is (true? (:diverged? band)))
        (is (= "high" (get-in band [:current :text])))))))

(deftest knob-links-on-an-audio-lfo-are-validated
  (let [model (:model (knobs/apply-operation (configured) {:op :knob/create :knob-id :k :name "K"}))
        link (fn [destination range & [extra]]
               (refusal model {:op :knob/link :knob-id :k :link (merge {:id "l" :destination destination :range range} extra)}))]
    (is (= [:invalid-knob "No audio LFO :treble"] (link {:kind :audio-lfo :lfo-id :treble :field :band} [:sub :mid])))
    (is (= [:invalid-knob "An audio LFO :field is :band or :morph"] (link {:kind :audio-lfo :lfo-id :bass :field :points} [8 16])))
    (is (= :invalid-knob (first (link {:kind :audio-lfo :lfo-id "bass" :field :band} [:sub :mid]))))
    (is (re-find #"declared values|satisfy" (second (link {:kind :audio-lfo :lfo-id :bass :field :band} [:sub :treble]))))
    (is (re-find #"satisfy" (second (link {:kind :audio-lfo :lfo-id :bass :field :morph} [0 4]))) "a morph needs positive beats")
    (is (re-find #"Unknown destination field" (second (link {:kind :audio-lfo :lfo-id :bass :field :band :param :level} [:sub :mid]))))
    (testing "removing the LFO leaves the link visible as dangling and skipped"
      (let [linked (:model (knobs/apply-operation model {:op :knob/link :knob-id :k :link band-link}))
            removed (:model (lfo/apply-operation linked {:op :audio-lfo/remove :lfo-id :bass}))
            turned (turn removed :k 0.5)]
        (is (= {:status :dangling :reason :lfo-missing}
               (select-keys (first (:links (knobs/knob-view removed :k))) [:status :reason])))
        (is (= [{:link-id "band" :status :skipped :reason :lfo-missing}] (get-in turned [:result :links])))))))

(deftest without-audio-a-knob-change-keeps-the-documented-rest-shape
  (let [model (:model (knobs/apply-operation (configured) {:op :knob/create :knob-id :sweep :name "Sweep" :links [band-link]}))
        turned (:model (turn model :sweep 0.6))
        rest (vec (repeat 8 0.5))]
    (is (= :mid (get-in (state turned) [:config :band])))
    (is (= :waiting (:status (state turned))))
    (is (= rest (:to (state turned)) (lfo/shape-at (state turned) 50.0)) "the :mid rest shape until a whole window is shaped")
    (is (= turned (:model (turn model :sweep 0.6))) "the same turn on the same model gives the same model")
    (testing "a shape measured under the old band is not admitted after the knob moved"
      (is (= (state turned) (state (lfo/admit (at-beat turned 10.0) {:bass (extraction model ramp 1)})))))))

(deftest a-new-morph-time-retimes-a-morph-under-way-without-a-jump
  (let [base (:model (knobs/apply-operation (configured) {:op :knob/create :knob-id :morph :name "Morph"
                                                          :links [morph-link]}))
        shaped (lfo/admit (at-beat base 10.0) {:bass (extraction base ramp 1)})
        before (state shaped)
        mid (at-beat shaped 11.0)                               ; a quarter through a 4-beat morph
        retimed (:model (turn mid :morph 1.0))                  ; 8 beats
        after (state retimed)]
    (is (= {:beats 8} (get-in after [:config :morph])))
    (is (= 8.0 (:morph-beats after)))
    (is (= (lfo/shape-at before 11.0) (lfo/shape-at after 11.0)) "the shape showing does not move")
    (is (== 0.25 (lfo/progress after 11.0)))
    (is (== 0.5 (lfo/progress after 13.0)) "the remaining three quarters take six beats")
    (is (= ramp (lfo/shape-at after 17.0)))
    (testing "a stopped transport holds its beat: the retime takes effect from there with no catch-up"
      (let [stopped (-> mid (assoc-in [:transport :playing?] false))
            held (state (:model (turn stopped :morph 1.0)))]
        (is (= (lfo/shape-at before 11.0) (lfo/shape-at held 11.0)))
        (is (== 0.25 (lfo/progress held 11.0)) "no progress while the beat stands still")
        (is (== (lfo/progress after 12.0) (lfo/progress held 12.0)) "on play it continues from the held beat")))
    (testing "a settled LFO keeps its finished morph and uses the new time from the next shape"
      (let [settled (state (:model (turn (at-beat shaped 30.0) :morph 1.0)))]
        (is (= (select-keys before [:morph-start-beat :morph-beats]) (select-keys settled [:morph-start-beat :morph-beats])))
        (is (= 8.0 (:morph-beats (state (lfo/admit (at-beat (:model (turn (at-beat shaped 30.0) :morph 1.0)) 31.0)
                                                    {:bass (extraction base [1.0 1.0 1.0 1.0 0.0 0.0 0.0 0.0] 2)})))))))))

;; Owner and capture --------------------------------------------------------------

(defn- pcm-source [^bytes bytes chunk]
  (let [position (atom 0)]
    {:started-at-ms 0.0 :description {:name "Injected passage"}
     :read! (fn [out offset length]
              (let [n (min length chunk (- (alength bytes) @position))]
                (if (zero? n) -1 (do (System/arraycopy bytes @position out offset n) (swap! position + n) n))))
     :close! (fn [])}))

(deftest the-owner-shapes-the-band-a-knob-chose-from-real-capture
  (let [bpm 120.0
        bytes (fixtures/pcm16-bytes (fixtures/passage :delay-synth {:bpm bpm :bars 4 :sample-rate 8000.0}))
        model (control/new-show {:show-id :audio-lfo-knobs-test :time-ms 8000.0
                                 :transport {:playing? true :beat 16.0 :bpm bpm :time-ms 8000.0}
                                 :rig {:fixtures [{:id :left :universe 1 :address 1 :channels {:dimmer 1}}]}})
        rt (show/create-runtime {:model model :workspace-options nil
                                 :audio-options {:open-source! (fn [_] (pcm-source bytes 640))}})]
    (try
      (show/tick! rt 8000)
      (let [at! (fn [time kind payload]
                  (let [reply (show/submit! rt kind payload)]
                    (show/tick! rt time)
                    (deref reply 5000 {:status :timeout})))
            configured (at! 8001 :command {:op :audio-lfo/configure :lfo-id :bass
                                           :config {:band :full :extract :envelope :points 16 :morph {:bars 1}}})
            created (at! 8002 :command {:op :knob/create :knob-id :sweep :name "Sweep" :links [band-link morph-link]})
            turned (at! 8003 :command {:op :knob/set :knob-id :sweep :value 0.2 :command-id "turn-1"})
            replayed (at! 8004 :command {:op :knob/set :knob-id :sweep :value 0.2 :command-id "turn-1"})
            nudged (at! 8005 :command (assoc (knobs/encoder-command :sweep 5 false) :command-id "nudge-1"))]
        (is (= [:accepted :accepted :accepted :accepted :accepted] (mapv :status [configured created turned replayed nudged])))
        (is (= [:applied :applied] (mapv :status (:links turned))))
        (is (= (:revision turned) (:revision replayed)) "replaying the turn's command ID changes nothing")
        (testing "an encoder turn is the same knob command; five coarse detents stay on the bass step and move the morph one ratio"
          (is (= 0.25 (get-in @(:state rt) [:model :knobs :knobs :sweep :value])))
          (is (= {:band :bass :morph {:beats 3/2}} (select-keys (get-in @(:state rt) [:model :audio-lfos :lfos :bass :config]) [:band :morph]))))
        (let [handle (connections/start-audio! (:connections rt) {:default? true :sample-rate 8000.0 :block-frames 400}
                                               (show/audio-callbacks rt))]
          (is (= :completed (:status (deref (:result handle) 5000 ::timeout)))))
        (show/tick! rt 8006)
        (let [snapshot (show/snapshot rt)
              [view] (:audio-lfos snapshot)]
          (testing "the capture worker shaped whole bars on the knob's band and the owner committed them"
            (is (= :ok (:status view)))
            (is (= "bass 60 Hz–250 Hz" (:band-label view)))
            (is (pos? (:commits view)))
            (is (= {:index 2 :start-beat 8.0 :end-beat 12.0} (select-keys (get-in view [:last :window]) [:index :start-beat :end-beat]))))
          (testing "Studio and the REPL read the same knob and LFO projections"
            (is (= [:sweep :sweep] (mapv :knob-id (:knobs view))))
            (is (= ["bass" "3/2 beats"] (mapv #(get-in % [:resolved :text]) (:links (first (get-in snapshot [:knobs :knobs])))))))
          (testing "turning to another band waits for the next whole window under it"
            (let [shaped (get-in @(:state rt) [:model :audio-lfos :lfos :bass :to])
                  moved (at! 8007 :command {:op :knob/set :knob-id :sweep :value 0.5})
                  lfo (get-in @(:state rt) [:model :audio-lfos :lfos :bass])]
              (is (= :accepted (:status moved)))
              (is (= [:mid :waiting] [(get-in lfo [:config :band]) (:status lfo)]))
              (is (= shaped (:to lfo)) "the shape stays until the next window")))))
      (finally (show/stop! rt)))))
