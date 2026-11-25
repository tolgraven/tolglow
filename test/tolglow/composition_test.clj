(ns tolglow.composition-test
  (:require [clojure.test :refer [deftest is]]
            [afterglow.control.model :as control]
            [afterglow.runtime.engine :as engine]
            [tolglow.compiler :as compiler]
            [clojure.data.json :as json]
            [tolglow.composition :as composition]
            [tolglow.composition.sequence-edit :as edit]
            [tolglow.data :as data]
            [tolglow.studio.web :as web]))

(defn command [model id op time & fields]
  (let [result (control/apply-command model
                 (merge {:show-id (:show-id model) :epoch (:epoch model)
                         :command-id id :source :composition-test :op op :time-ms time}
                        (apply hash-map fields)))]
    (when-not (= :accepted (get-in result [:ack :status]))
      (throw (ex-info "Test command rejected" result)))
    result))

(defn show []
  (let [effect {:id :level :version 1
                :params {:level {:type :number :default 0.5 :bounds [0 1]}}
                :envelope {:release-ms 50} :outputs {:dimmer {:op :param :key :level}}}
        initial (control/new-show {:show-id "composition" :transport {:beat 0 :bpm 120 :playing? true}})]
    (reduce (fn [model cue]
              (:model (command model (str "cue-" (name cue)) :cue/put 0
                               :cue {:id cue :effect-id :level})))
            (:model (command initial :effect :registry/publish 0 :kind :effects :definition effect))
            [:red :blue :spot])))

(def ms-sequence {:id :sequence :mode :sequence :loop :once
                  :slots [{:id :first :cue-id :red :duration-ms 100}
                          {:id :second :cue-id :blue :duration-ms 100}]})

(def beat-sequence {:id :beats :mode :sequence :timebase :beats :loop :loop
                    :slots [{:id :first :cue-id :red :duration-beats 1}
                            {:id :second :cue-id :blue :duration-beats 2}]})

(defn running [model]
  (set (map :cue-id (filter #(= :running (:phase %)) (vals (:instances model))))))

(deftest saved-empty-and-muted-steps-keep-time-and-release-only-owned-cues
  (let [outside (command (show) "outside" :cue/launch 0 :cue-id :blue)
        base (:model outside)
        outside-id (get-in outside [:ack :instance-id])
        program (-> {:id :chase :mode :sequence :slots []}
                    (edit/add-step nil {:id :first :cue-id :red})
                    (edit/add-empty-step :first :rest)
                    (edit/add-step :rest {:id :muted :cue-id :blue})
                    (edit/toggle-muted :muted)
                    (edit/add-step :muted {:id :last :cue-id :spot}))
        saved (composition/save base program)
        started (-> saved (composition/start :chase 0) (composition/advance 0))
        red-id (first (vals (:composition-tokens started)))
        sample (fn [model beat]
                 (-> model (assoc-in [:transport :beat] beat) (composition/advance (* 500 beat))))
        empty-step (sample started 1)
        muted-step (sample empty-step 2)
        last-step (sample muted-step 3)
        ended (sample last-step 4)
        playback #(get-in % [:composition-runs :chase :playback])]
    (is (= (:instances base) (:instances saved)))
    (is (empty? (:composition-runs saved)))
    (is (= :releasing (get-in empty-step [:instances red-id :phase])))
    (is (= :rest (:slot-id (playback empty-step))))
    (is (= :empty (:step-kind (playback empty-step))))
    (is (= 1.0 (:remaining-beats (playback empty-step))))
    (is (nil? (:cue-id (playback empty-step))))
    (is (= :muted (:slot-id (playback muted-step))))
    (is (true? (:muted? (playback muted-step))))
    (is (every? (comp empty? :composition-tokens) [empty-step muted-step ended]))
    (is (= #{:blue} (running muted-step)))
    (is (= #{:blue :spot} (running last-step)))
    (is (empty? (:composition-runs ended)))
    (is (every? #(= (get-in base [:instances outside-id]) (get-in % [:instances outside-id]))
                [started empty-step muted-step last-step ended]))
    (is (thrown? clojure.lang.ExceptionInfo (composition/save base (assoc program :mode :macro))))
    (is (thrown? clojure.lang.ExceptionInfo
                 (composition/save base (assoc-in program [:slots 2 :cue-id] :unregistered))))))

(deftest unidentified-slots-receive-free-ids-after-an-earlier-deletion
  (let [saved (composition/save (show) {:id :c :mode :sequence
                                        :slots [{:cue-id :red} {:cue-id :blue}]})
        ;; A client removes slot-0 and appends a new cue without an ID.
        edited (update-in saved [:compositions :c :slots] #(conj (subvec % 1) {:cue-id :spot}))
        resaved (composition/save saved (get-in edited [:compositions :c]))]
    (is (= ["slot-0" "slot-1"] (mapv :id (get-in saved [:compositions :c :slots]))))
    (is (= ["slot-1" "slot-2"] (mapv :id (get-in resaved [:compositions :c :slots]))))
    (is (= [:blue :spot] (mapv :cue-id (get-in resaved [:compositions :c :slots]))))))

;; Studio sends the projected program back as JSON; textual IDs become keywords.
(defn- through-studio-wire [definition]
  (data/normalize-json (json/read-str (json/write-str (web/json-data definition)) :key-fn keyword)))

(deftest studio-wire-saves-keep-slot-ids-unique-by-name
  (let [saved (composition/save (show) (through-studio-wire {:id :c :mode :sequence
                                                             :slots [{:cue-id :red} {:cue-id :blue}]}))
        reopened (through-studio-wire (get-in saved [:compositions :c]))
        edited (update reopened :slots #(conj (subvec % 1) {:cue-id :spot}))
        resaved (composition/save saved (through-studio-wire edited))
        again (composition/save resaved (through-studio-wire (get-in resaved [:compositions :c])))
        ids #(mapv :id (get-in % [:compositions :c :slots]))]
    (is (= [:slot-1] (mapv :id (butlast (:slots edited)))))
    (is (= [:slot-1 "slot-2"] (ids resaved)))
    (is (= [:slot-1 :slot-2] (ids again)))
    (is (= [:blue :spot] (mapv :cue-id (get-in again [:compositions :c :slots]))))
    (is (thrown? clojure.lang.ExceptionInfo
                 (composition/save (show) {:id :d :mode :sequence
                                           :slots [{:id :a :cue-id :red} {:id "a" :cue-id :blue}]})))
    (is (thrown? clojure.lang.ExceptionInfo
                 (composition/save (show) {:id :d :mode :macro
                                           :slots [{:id :a :cue-id :red} {:id "a" :cue-id :blue}]})))))

(deftest unmuted-flags-do-not-block-macros
  (let [program {:id :m :mode :macro :slots [{:id :a :cue-id :red :muted? false}]}]
    (is (= [:a] (mapv :id (get-in (composition/save (show) program) [:compositions :m :slots]))))
    (is (thrown? clojure.lang.ExceptionInfo
                 (composition/save (show) (assoc-in program [:slots 0 :muted?] true))))))

(deftest canonical-musical-slots-save-and-play-with-captured-meter
  (let [definition {:id :bars :mode :sequence
                    :slots [{:cue-id :red :duration {:bars 1}}
                            {:cue-id :blue :duration {:beats 1/2}}]}
        start (-> (show) (assoc-in [:transport :meter] {:beats-per-bar 3})
                  (composition/save definition) (composition/start :bars 0) (composition/advance 0))
        mid (-> start (assoc-in [:transport :beat] 1.5)
                (assoc-in [:transport :bpm] 240) (composition/advance 200))
        next (-> mid (assoc-in [:transport :beat] 3)
                 (assoc-in [:transport :meter :beats-per-bar] 7) (composition/advance 500))
        end (-> next (assoc-in [:transport :beat] 3.5) (composition/advance 600))]
    (is (= #{:red} (running mid)))
    (is (= 0.5 (get-in mid [:composition-runs :bars :playback :phase])))
    (is (= #{:blue} (running next)))
    (is (= 0.5 (get-in next [:composition-runs :bars :playback :remaining-beats])))
    (is (empty? (:composition-runs end)))
    (is (empty? (:composition-tokens end)))))

(deftest save-normalizes-the-selected-timebase-without-giving-macros-a-clock
  (let [ms (composition/save (show) {:id :ms :mode :sequence :timebase :ms :slots [{:cue-id :red}]})
        beats (composition/save (show) {:id :beats :mode :sequence :timebase :beats :slots [{:cue-id :red}]})
        default (composition/save (show) {:id :default :mode :sequence :slots [{:cue-id :red}]})
        macro (composition/save (show) {:id :macro :mode :macro :slots [{:cue-id :red}]})]
    (is (= {:ms 1000} (get-in ms [:compositions :ms :slots 0 :duration])))
    (is (= {:beats 1} (get-in beats [:compositions :beats :slots 0 :duration])))
    (is (= :beats (get-in default [:compositions :default :timebase])))
    (is (= {:beats 1} (get-in default [:compositions :default :slots 0 :duration])))
    (is (nil? (get-in beats [:compositions :beats :slots 0 :duration-ms])))
    (is (nil? (get-in macro [:compositions :macro :slots 0 :duration-ms])))))

(deftest save-rejects-unknown-cues-invalid-parameters-and-nonfinite-durations
  (doseq [definition [(assoc-in ms-sequence [:slots 0 :cue-id] :missing)
                      (assoc-in ms-sequence [:slots 0 :duration-ms] 0)
                      (assoc-in ms-sequence [:slots 0 :duration-ms] ##NaN)
                      (assoc-in ms-sequence [:slots 0 :duration-ms] ##Inf)
                      (assoc-in beat-sequence [:slots 0 :duration-beats] -1)
                      (assoc-in beat-sequence [:slots 0 :duration-ms] 100)
                      (assoc-in ms-sequence [:slots 0 :params] {:level 2})
                      (assoc-in ms-sequence [:slots 0 :launch] {:mode :one-shot})
                      (assoc-in ms-sequence [:slots 1 :id] :first)
                      (assoc ms-sequence :slots [])]]
    (is (thrown? clojure.lang.ExceptionInfo (composition/save (show) definition)))))

(deftest composition-uses-cue-presets-before-explicit-slot-and-launch-values
  (let [model (assoc-in (show) [:registry :cues :red :saved-params] {:level 0.7})
        definition {:id :preset-macro :mode :macro :slots [{:cue-id :red}]}]
    (doseq [[slot expected] [[{} 0.7] [{:params {:level 0.2}} 0.2]
                             [{:params {:level 0.2} :launch {:params {:level 0.4}}} 0.4]]]
      (let [started (-> model
                        (composition/save (update-in definition [:slots 0] merge slot))
                        (composition/start :preset-macro 0))]
        (is (= [expected] (mapv #(get-in % [:params :level]) (vals (:instances started)))))))
    (is (thrown? clojure.lang.ExceptionInfo
                 (composition/save (assoc-in model [:registry :cues :red :saved-params] {:level 2}) definition)))))

(deftest appending-a-cue-uses-the-saved-macro-after-an-intervening-edit
  (let [selected-id :opening-look
        macro {:id selected-id :name "Opening look" :mode :macro
               :slots [{:id :first :cue-id :red :params {:level 1/3}}]}
        selected (-> (show)
                     (composition/save ms-sequence)
                     (composition/save macro)
                     (composition/start selected-id 0))
        ;; A REPL save lands after the operator selects the macro in Studio.
        repl-edit (-> macro
                      (assoc :name "Revised opening look")
                      (update :slots conj {:id "slot-2" :cue-id :blue :params {:level 1/2}}))
        committed (composition/save selected repl-edit)
        appended (composition/append-macro-cue committed selected-id :spot)
        saved (get-in committed [:compositions selected-id])
        updated (get-in appended [:compositions selected-id])
        slots (:slots updated)
        playback-keys [:instances :composition-runs :composition-tokens]]
    (is (= (dissoc saved :slots) (dissoc updated :slots)))
    (is (= (:slots saved) (vec (butlast slots))))
    (is (= {:cue-id :spot :params {}} (dissoc (last slots) :id)))
    (is (= 3 (count (distinct (map :id slots)))))
    (is (= (inc (:revision committed)) (:revision appended)))
    (is (= (get-in committed [:compositions :sequence])
           (get-in appended [:compositions :sequence])))
    (is (= (select-keys committed playback-keys)
           (select-keys appended playback-keys)))))

(deftest appending-a-cue-requires-an-existing-macro-and-registered-cue
  (let [model (-> (show)
                  (composition/save ms-sequence)
                  (composition/save {:id :macro :mode :macro :slots [{:cue-id :red}]}))]
    (doseq [[macro-id cue-id path] [[:missing :red [:macro-id]]
                                   [:sequence :red [:macro-id]]
                                   [:macro :missing [:slots 1]]]]
      (let [errors (try
                     (composition/append-macro-cue model macro-id cue-id)
                     nil
                     (catch clojure.lang.ExceptionInfo error (:errors (ex-data error))))]
        (is (= [path] (mapv :path errors)))
        (is (string? (:message (first errors))))))))

(deftest milliseconds-continue-to-drive-the-saved-sequence-with-owned-release
  (let [saved (composition/save (show) ms-sequence)
        start (-> saved (composition/start :sequence 0) (composition/advance 0))
        next (composition/advance start 100)
        first-id (first (keys (:instances start)))
        ended (composition/advance next 200)]
    (is (= #{:red} (running start)))
    (is (= #{:blue} (running next)))
    (is (= :releasing (get-in next [:instances first-id :phase])))
    (is (= :second (get-in next [:composition-runs :sequence :playback :slot-id])))
    (is (= 0.0 (get-in next [:composition-runs :sequence :playback :phase])))
    (is (empty? (:composition-runs ended)))
    (is (empty? (:composition-tokens ended)))
    (is (= #{} (running ended)))))

(deftest millisecond-pause-freezes-and-resume-does-not-catch-up-wall-time
  (let [started (-> (show) (composition/save ms-sequence)
                    (composition/start :sequence 0) (composition/advance 0))
        paused (-> started (assoc-in [:transport :playing?] false) (composition/advance 50))
        waiting (composition/advance paused 5000)
        resumed (-> waiting (assoc-in [:transport :playing?] true) (composition/advance 5000))
        next (composition/advance resumed 5050)]
    (is (= 0.5 (get-in paused [:composition-runs :sequence :playback :phase])))
    (is (= 0.5 (get-in waiting [:composition-runs :sequence :playback :phase])))
    (is (= 0.5 (get-in resumed [:composition-runs :sequence :playback :phase])))
    (is (nil? (get-in waiting [:composition-runs :sequence :playback :remaining-ms])))
    (is (= #{:red} (running waiting)))
    (is (= #{:blue} (running next)))
    (is (= 2 (count (:instances next))))))

(deftest beat-sequences-use-host-phase-and-tempo-without-reconstructing-history
  (let [started (-> (show) (composition/save beat-sequence)
                    (composition/start :beats 0) (composition/advance 0))
        halfway (-> started (assoc-in [:transport :beat] 0.5)
                     (assoc-in [:transport :bpm] 60) (composition/advance 250))
        next (-> halfway (assoc-in [:transport :beat] 1) (composition/advance 750))
        paused (-> next (assoc-in [:transport :playing?] false) (composition/advance 750))
        waiting (composition/advance paused 5000)
        resumed (-> waiting (assoc-in [:transport :playing?] true) (composition/advance 5000))
        looped (-> resumed (assoc-in [:transport :beat] 3) (composition/advance 7000))]
    (is (= #{:red} (running halfway)))
    (is (= 0.5 (get-in halfway [:composition-runs :beats :playback :phase])))
    (is (= 500.0 (get-in halfway [:composition-runs :beats :playback :remaining-ms])))
    (is (= #{:blue} (running next)))
    (is (= 2.0 (get-in next [:composition-runs :beats :playback :remaining-beats])))
    (is (= (get-in paused [:composition-runs :beats :state :active-token])
           (get-in resumed [:composition-runs :beats :state :active-token])))
    (is (= #{:red} (running looped)))
    (is (= 3 (count (:instances looped))))))

(deftest starting-sequences-paused-waits-but-macros-remain-immediate
  (doseq [[definition id] [[ms-sequence :sequence] [beat-sequence :beats]]]
    (let [paused (-> (show) (assoc-in [:transport :playing?] false)
                     (composition/save definition) (composition/start id 0)
                     (composition/advance 5000))
          resumed (-> paused (assoc-in [:transport :playing?] true) (composition/advance 5000))]
      (is (empty? (:instances paused)))
      (is (false? (get-in paused [:composition-runs id :playback :started?])))
      (is (= #{:red} (running resumed)))))
  (let [macro (-> (show) (assoc-in [:transport :playing?] false)
                   (composition/save {:id :macro :mode :macro :slots [{:cue-id :red}]})
                   (composition/start :macro 0))]
    (is (= #{:red} (running macro)))))

(deftest stopping-macro-releases-exact-owned-ids-and-preserves-human-held-cues
  (let [human (command (show) :human :cue/launch 0 :cue-id :red :launch {:mode :held})
        human-id (get-in human [:ack :instance-id])
        started (-> (:model human)
                     (composition/save {:id :macro :mode :macro
                                        :slots [{:id :a :cue-id :red} {:id :b :cue-id :blue}]})
                     (composition/start :macro 0))
        owned (set (vals (:composition-tokens started)))
        later (command started :later :cue/launch 10 :cue-id :red)
        later-id (get-in later [:ack :instance-id])
        stopped (composition/stop (:model later) :macro 20)
        rendered (:model (engine/advance stopped {:time-ms 70 :fixtures []} compiler/compile-effect))]
    (is (= 2 (count owned)))
    (is (every? #(= :releasing (get-in stopped [:instances % :phase])) owned))
    (is (= :running (get-in stopped [:instances human-id :phase])))
    (is (= :running (get-in stopped [:instances later-id :phase])))
    (is (= #{human-id later-id} (set (keys (:instances rendered)))))
    (is (= stopped (composition/stop stopped :macro 20)))
    (is (empty? (:composition-tokens stopped)))))

(deftest restart-discards-stale-runs-without-releasing-a-new-show-instance
  (let [started (-> (show) (composition/save ms-sequence)
                    (composition/start :sequence 0) (composition/advance 0))
        restarted (:model (command started :restart :show/restart 10))
        human (command restarted :human-new :cue/launch 10 :cue-id :spot)
        advanced (composition/advance (:model human) 20)]
    (is (= #{:spot} (running advanced)))
    (is (empty? (:composition-runs advanced)))
    (is (empty? (:composition-tokens advanced)))
    (is (= #{:spot} (running (composition/stop (:model human) :sequence 20))))))

(deftest editing-a-running-definition-requires-explicit-restart-to-change-playback
  (let [started (-> (show) (composition/save ms-sequence)
                    (composition/start :sequence 0) (composition/advance 0))
        edited (composition/save started (assoc-in ms-sequence [:slots 1 :cue-id] :spot))
        next (composition/advance edited 100)
        old-owner (get-in next [:composition-runs :sequence :owner-id])
        restarted (-> next (composition/start :sequence 100) (composition/advance 100))]
    (is (= #{:blue} (running next)))
    (is (not= old-owner (get-in restarted [:composition-runs :sequence :owner-id])))
    (is (= #{:red} (running restarted)))
    (is (= #{:spot} (running (composition/advance restarted 200))))))
