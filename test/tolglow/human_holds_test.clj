(ns tolglow.human-holds-test
  "A performer's hold keeps agents off a control they touched, and agent
  writes on one line resolve by admission order (#854). Every scenario goes
  through the real owner command path and observes captured ArtDmx packets."
  (:require [afterglow.control.model :as control]
            [afterglow.output.artnet :as artnet]
            [clojure.test :refer [deftest is testing]]
            [tolglow.event-subscribers :as subscribers]
            [tolglow.persistence :as files]
            [tolglow.show :as show]))

(def ^:private effect
  {:id :wash :version 1 :category :light
   :params {:level {:type :number :default 0.0 :bounds [0.0 1.0]}}
   :outputs {:dimmer {:op :param :key :level}}})

(defn- live-model []
  (control/new-show
   {:show-id :hold-test :transport {:playing? true :beat 0.0 :bpm 120.0}
    :registry {:effects {:wash effect}
               :cues {:wash {:id :wash :effect-id :wash :version 1}}}
    :rig {:fixtures [{:id :left :universe 1 :address 1 :channels {:dimmer 1}}]}}))

(def ^:private agent-a {:adapter :agent :session-id "conversation-a"})

(def ^:private agent-b {:adapter :agent :session-id "conversation-b"})

(def ^:private push {:adapter :push-3 :session-id "push"})

(def ^:private x-touch {:adapter :x-touch :session-id "x-touch"})

(def ^:private repl {:adapter :repl :session-id "repl"})

(def ^:private operator {:operator-id :front-of-house})

(defn- harness []
  (let [packets (atom [])
        sink {:send! (fn [universes _]
                       (swap! packets conj (vec (artnet/artdmx-packet 1 1 (get universes 1)))))}]
    {:rt (show/create-runtime {:model (live-model) :sink sink :workspace-options nil})
     :packets packets}))

(defn- dimmer [packets] (bit-and 255 (nth (peek @packets) 18)))

(defn- at!
  "Submit at time ms (120 BPM from beat 0 at 0 ms: beat = ms / 500), tick and
  return the receipt."
  ([rt time payload] (at! rt time payload nil))
  ([rt time payload authority]
   (let [reply (show/submit! rt :command payload authority)]
     (show/tick! rt time)
     (deref reply 1000 ::missing))))

(defn- tool!
  "Call an agent tool through show/dispatch-tool!, ticking the owner one
  millisecond at a time until it answers."
  [rt tool arguments metadata]
  (let [result (future (show/dispatch-tool! rt tool arguments metadata))]
    (loop [n 0]
      (when (and (not (realized? result)) (< n 400))
        (show/tick! rt (inc (get-in @(:state rt) [:model :time-ms])))
        (Thread/sleep 2)
        (recur (inc n))))
    (deref result 1000 ::timeout)))

(defn- beat [rt] (get-in @(:state rt) [:model :transport :beat]))

(defn- recent [rt] (:recent (show/schedule rt)))

(defn- outcome [rt event-id]
  (select-keys (last (filter #(= event-id (:event-id %)) (recent rt))) [:event-id :status :reason :by :held-by]))

(defn- launch! [rt]
  (:instance-id (at! rt 0 {:op :cue/launch :cue-id :wash :launch {:mode :persistent} :source repl})))

(defn- ramp [id event-id timing to over source]
  (merge {:op :param/ramp :event-id event-id :instance-id id :param :level :to to :over over :source source}
         timing))

(deftest a-push-write-holds-its-parameter-against-agent-ramps-until-the-bar-line
  (let [{:keys [rt packets]} (harness)]
    (try
      (let [id (launch! rt)
            destination {:target :param :instance-id id :param :level}
            ;; Posted before the hold, due inside it.
            before (at! rt 250 (ramp id "before" {:at {:beat 6}} 1.0 {:beats 1} agent-a))
            ;; Beat 2: the grace ends on the first bar line at least one bar later, beat 8.
            pushed (at! rt 1000 {:op :param/set :instance-id id :param :level :value 0.4 :source push})
            held (show/schedule rt)
            snapshot-holds (get-in (show/snapshot rt) [:schedule :holds])
            compact (show/dispatch-tool! rt "inspect_schedule" {} {:run-id "run-a" :conversation-id "conversation-a"})
            inside (at! rt 1500 (ramp id "inside" {:in {:beats 1}} 1.0 {:beats 1} agent-a))
            after (at! rt 1500 (ramp id "after" {:at {:bar 2}} 1.0 {:beats 2} agent-a))
            frames (into (sorted-map) (map (fn [t] (show/tick! rt t) [(beat rt) (dimmer packets)]))
                         (range 2000 5250 250))]
        (is (= :accepted (:status before)))
        (is (= :accepted (:status pushed)))
        (testing "the hold is in the shared schedule projection, the Studio snapshot and the agent's compact view"
          (is (= [{:destination destination :touched? false :touched-beat 2.0 :until-beat 8.0 :source push}]
                 (:holds held)))
          (is (= (:holds held) snapshot-holds))
          (is (= [[(str id "/level") 8.0]] (:holds compact))))
        (testing "an agent ramp posted inside the hold is refused with the hold named"
          (is (= :rejected (:status inside)))
          (is (= :human-held (:code inside)))
          (is (= {:reason :human-held :destination destination :held-by push :until-beat 8.0}
                 (select-keys (:details inside) [:reason :destination :held-by :until-beat]))))
        (is (= :accepted (:status after)) "one due on the bar line that ends the grace is accepted")
        (testing "the ramp posted before the hold is cancelled when due"
          (is (= {:event-id "before" :status :cancelled :reason :human-held :held-by push} (outcome rt "before"))))
        (testing "the performer's value stays on the wire until the bar line, then the agent ramp starts there"
          (is (every? #(= 102 %) (vals (subseq frames < 8.0))) (pr-str frames))
          (is (= 102 (get frames 8.0)) "the ramp starts from the performer's value on beat 8")
          (is (<= 178 (get frames 9.0) 179))
          (is (= 255 (get frames 10.0)) (pr-str frames))
          (is (= {:event-id "after" :status :completed} (select-keys (last (recent rt)) [:event-id :status]))))
        (testing "the hold has ended in every view"
          (is (empty? (:holds (show/schedule rt))))
          (is (empty? (get-in (show/snapshot rt) [:schedule :holds])))))
      (finally (show/stop! rt)))))

(deftest direct-agent-tool-writes-to-a-held-control-are-refused
  (let [{:keys [rt packets]} (harness)]
    (try
      (let [id (launch! rt)
            meta (fn [command-id] {:run-id "run-a" :conversation-id "conversation-a" :command-id command-id})
            _ (at! rt 1000 {:op :param/set :instance-id id :param :level :value 0.4 :source push})
            set-tool (tool! rt "set_parameter" {:instance-id id :param "level" :value 0.9} (meta "tool-set"))
            stop-tool (tool! rt "stop_instance" {:instance-id id} (meta "tool-stop"))
            ramp-tool (tool! rt "schedule_events" {:events [{:id "soon" :op "param/ramp" :instance-id id :param "level"
                                                             :to 1.0 :over {:beats 1} :in {:beats 1}}]}
                             (meta "tool-ramp"))]
        (is (= [:rejected :human-held] ((juxt :status :code) set-tool)))
        (is (= [:rejected :human-held] ((juxt :status :code) stop-tool)) "a hold on a parameter covers ending its instance")
        (is (= [:rejected :human-held] ((juxt :status :code) ramp-tool)))
        (is (some? (get-in @(:state rt) [:model :instances id])))
        (show/tick! rt 3000)
        (is (= 102 (dimmer packets)))
        (show/tick! rt 4100)
        (testing "after the grace the same tool writes again"
          (is (= :accepted (:status (tool! rt "set_parameter" {:instance-id id :param "level" :value 0.9} (meta "tool-set-2")))))
          (show/tick! rt 4500)
          (is (= 230 (dimmer packets)))))
      (finally (show/stop! rt)))))

(deftest an-encoder-held-for-eight-bars-keeps-the-hold-and-a-stop-does-not-shorten-it
  (let [{:keys [rt packets]} (harness)]
    (try
      (let [id (launch! rt)
            gesture {:instance-id id :param :level :control-id :encoder-1 :gesture-id :g1 :source x-touch}
            begun (at! rt 500 (assoc gesture :op :gesture/begin) operator)
            ;; Touched at beat 3: the earliest end is beat 8, the hold's end is unknown.
            inside (at! rt 1500 (ramp id "inside" {:in {:beats 1}} 1.0 {:beats 1} agent-a))
            later (at! rt 1500 {:op :param/set :event-id "bar-4" :instance-id id :param :level :value 1.0
                                :at {:bar 4} :source agent-a})
            touched (vec (for [k (range 1 9)]
                           (let [t (+ 500 (* 2000 k))
                                 sampled (at! rt t (assoc gesture :op :gesture/sample :sequence k :value (* 0.1 k)) operator)]
                             [(beat rt) (:status sampled) (mapv :touched? (:holds (show/schedule rt))) (dimmer packets)])))
            during (outcome rt "bar-4")
            ended (at! rt 16750 (assoc gesture :op :gesture/end :last-sequence 8) operator)
            released (:holds (show/schedule rt))
            stopped (at! rt 17000 {:op :transport/set :transport {:playing? false} :source repl})
            _ (doseq [t (range 17500 27500 1000)] (show/tick! rt t))
            stopped-beat (beat rt)
            stopped-holds (:holds (show/schedule rt))
            early (at! rt 27250 {:op :param/set :event-id "early" :instance-id id :param :level :value 0.1
                                 :at {:beat 39} :source agent-a})
            back (at! rt 27250 {:op :param/set :event-id "back" :instance-id id :param :level :value 0.1
                                :at {:bar 10} :source agent-a})
            resumed (at! rt 27500 {:op :transport/set :transport {:playing? true} :source repl})
            ;; Beat 34 at 27500 ms; beat 40 at 30500 ms.
            frames (into (sorted-map) (map (fn [t] (show/tick! rt t) [(beat rt) (dimmer packets)]))
                         (range 27750 31000 250))]
        (is (= :accepted (:status begun)))
        (is (= [:rejected :human-held] ((juxt :status :code) inside)))
        (is (= :accepted (:status later)) "due past the earliest end, it is checked again when due")
        (testing "every bar of the eight the encoder is held keeps the hold"
          (is (= (mapv (fn [k] (+ 1.0 (* 4 k))) (range 1 9)) (mapv first touched)))
          (is (every? (fn [[_ status holds]] (and (= :accepted status) (= [true] holds))) touched) (pr-str touched))
          (is (= (mapv #(Math/round (* 25.5 %)) (range 1 9)) (mapv peek touched))
              "the performer's samples reach the wire"))
        (is (= {:event-id "bar-4" :status :cancelled :reason :human-held :held-by x-touch} during)
            "an agent write due while the encoder is held is cancelled")
        (is (= :accepted (:status ended)))
        (testing "release at beat 33.5 starts the grace: it ends on bar line 40"
          (is (= [{:touched? false :released-beat 33.5 :until-beat 40.0}]
                 (mapv #(select-keys % [:touched? :released-beat :until-beat]) released))))
        (testing "a transport stop holds the hold's position"
          (is (= :accepted (:status stopped)))
          (is (= 34.0 stopped-beat))
          (is (= released stopped-holds) "ten stopped seconds do not shorten it"))
        (is (= [:rejected :human-held] ((juxt :status :code) early)))
        (is (= :accepted (:status back)))
        (is (= :accepted (:status resumed)))
        (testing "after release the agent writes again from the bar line that ends the grace"
          (is (every? #(= 204 %) (vals (subseq frames < 40.0))) (pr-str frames))
          (is (every? #(= 26 %) (vals (subseq frames >= 40.0))) (pr-str frames))
          (is (= {:event-id "back" :status :fired} (select-keys (outcome rt "back") [:event-id :status])))
          (is (empty? (:holds (show/schedule rt))))))
      (finally (show/stop! rt)))))

(defn- line-scenario
  "Post param sets due on beat 4 (bar 1) in order, one post each, then play
  past it. posts are [source event-id value]."
  [posts]
  (let [{:keys [rt packets]} (harness)]
    (try
      (let [id (launch! rt)
            receipts (vec (map-indexed (fn [i [source event-id value]]
                                         (at! rt (+ 250 (* 50 i))
                                              {:op :param/set :event-id event-id :instance-id id :param :level
                                               :value value :at {:bar 1} :source source}))
                                       posts))]
        (doseq [t (range 1000 2750 250)] (show/tick! rt t))
        {:receipts (mapv :status receipts)
         :level (get-in @(:state rt) [:model :instances id :params :level])
         :outcomes (into {} (map (fn [[_ event-id]] [event-id (outcome rt event-id)])) posts)
         :recent (mapv #(dissoc % :time-ms) (recent rt))
         :packets @packets})
      (finally (show/stop! rt)))))

(deftest two-agent-sessions-on-one-line-resolve-by-admission-order
  (let [a-first (line-scenario [[agent-a "a-set" 0.2] [agent-b "b-set" 0.6]])
        b-first (line-scenario [[agent-b "b-set" 0.6] [agent-a "a-set" 0.2]])]
    (is (= [:accepted :accepted] (:receipts a-first)))
    (testing "the first admitted stands; the later records :superseded with the winner's event ID"
      (is (== 0.2 (:level a-first)))
      (is (= {:event-id "a-set" :status :fired} (get-in a-first [:outcomes "a-set"])))
      (is (= {:event-id "b-set" :status :cancelled :reason :superseded :by "a-set"} (get-in a-first [:outcomes "b-set"])))
      (is (= 51 (bit-and 255 (nth (peek (:packets a-first)) 18)))))
    (testing "reversing the admission order reverses the winner"
      (is (== 0.6 (:level b-first)))
      (is (= {:event-id "a-set" :status :cancelled :reason :superseded :by "b-set"} (get-in b-first [:outcomes "a-set"]))))
    (testing "replaying the same command log gives the same outcomes and output"
      (let [again (line-scenario [[agent-a "a-set" 0.2] [agent-b "b-set" 0.6]])]
        (is (= (:recent a-first) (:recent again)))
        (is (= (:packets a-first) (:packets again)))))
    (testing "one session's own later event on the line still supersedes its earlier one"
      (let [own (line-scenario [[agent-a "a-1" 0.2] [agent-a "a-2" 0.6]])]
        (is (== 0.6 (:level own)))
        (is (= :fired (get-in own [:outcomes "a-1" :status])))
        (is (= :fired (get-in own [:outcomes "a-2" :status])))))
    (testing "the scheduler carries no rank an agent could raise its write with"
      (let [{:keys [rt]} (harness)]
        (try
          (let [id (launch! rt)
                ranked (at! rt 250 {:op :schedule/post :source agent-b
                                    :events [{:id "ranked" :op :param/set :instance-id id :param :level :value 0.6
                                              :at {:bar 1} :rank 99}]})]
            (is (= [:rejected :invalid-schedule] ((juxt :status :code) ranked))))
          (finally (show/stop! rt)))))))

(deftest a-humans-scheduled-event-on-the-line-stands-over-both-agents
  (doseq [order [[[agent-a "a-set" 0.2] [repl "human" 0.9] [agent-b "b-set" 0.6]]
                 [[repl "human" 0.9] [agent-a "a-set" 0.2] [agent-b "b-set" 0.6]]
                 [[agent-a "a-set" 0.2] [agent-b "b-set" 0.6] [repl "human" 0.9]]]]
    (let [{:keys [level outcomes packets]} (line-scenario order)
          position (into {} (map-indexed (fn [i [_ event-id]] [event-id i])) order)]
      (testing (pr-str (mapv second order))
        (is (== 0.9 level))
        (is (= 230 (bit-and 255 (nth (peek packets) 18))))
        (is (= :fired (get-in outcomes ["human" :status])))
        (doseq [event-id ["a-set" "b-set"]
                :when (> (position event-id) (position "human"))]
          (is (= {:event-id event-id :status :cancelled :reason :superseded :by "human"} (get outcomes event-id))))))))

(deftest concurrent-human-and-agent-writes-never-let-the-agent-write-inside-the-hold
  (dotimes [_ 12]
    (let [{:keys [rt packets]} (harness)]
      (try
        (let [id (launch! rt)
              start (java.util.concurrent.CountDownLatch. 1)
              send! (fn [payload] (future (.await start) (show/submit! rt :command payload)))
              ;; Beat 3 is inside the hold a push write at beat 0 starts (to bar line 4).
              agents [(send! {:op :param/set :event-id "a-set" :instance-id id :param :level :value 0.2
                              :at {:beat 3} :source agent-a})
                      (send! {:op :param/set :event-id "b-set" :instance-id id :param :level :value 0.6
                              :at {:beat 3} :source agent-b})]
              human (send! {:op :param/set :instance-id id :param :level :value 0.4 :source push})
              _ (.countDown start)
              replies (mapv deref (conj agents human))
              _ (show/tick! rt 250)
              [a b h] (mapv #(deref % 1000 ::missing) replies)]
          (doseq [t (range 500 3000 250)] (show/tick! rt t))
          (is (= :accepted (:status h)))
          (doseq [[event-id receipt] [["a-set" a] ["b-set" b]]]
            (if (< (:revision receipt (Long/MAX_VALUE)) (:revision h))
              (is (= {:event-id event-id :status :cancelled :reason :human-held :held-by push} (outcome rt event-id))
                  "admitted before the push write, it is cancelled when due inside the hold")
              (is (= [:rejected :human-held] ((juxt :status :code) receipt))
                  "admitted after the push write, it is refused when posted")))
          (is (== 0.4 (get-in @(:state rt) [:model :instances id :params :level])))
          (is (= 102 (dimmer packets))))
        (finally (show/stop! rt))))))

(deftest concurrent-agent-sessions-resolve-by-the-order-the-owner-admitted-them
  (dotimes [_ 12]
    (let [{:keys [rt]} (harness)]
      (try
        (let [id (launch! rt)
              start (java.util.concurrent.CountDownLatch. 1)
              send! (fn [payload] (future (.await start) (show/submit! rt :command payload)))
              replies [(send! {:op :param/set :event-id "a-set" :instance-id id :param :level :value 0.2
                               :at {:bar 1} :source agent-a})
                       (send! {:op :param/set :event-id "b-set" :instance-id id :param :level :value 0.6
                               :at {:bar 1} :source agent-b})]
              _ (.countDown start)
              promises (mapv deref replies)
              _ (show/tick! rt 250)
              [a b] (mapv #(deref % 1000 ::missing) promises)
              [winner loser value] (if (< (:revision a) (:revision b)) ["a-set" "b-set" 0.2] ["b-set" "a-set" 0.6])]
          (doseq [t (range 500 2750 250)] (show/tick! rt t))
          (is (= [:accepted :accepted] [(:status a) (:status b)]))
          (is (== value (get-in @(:state rt) [:model :instances id :params :level])))
          (is (= {:event-id loser :status :cancelled :reason :superseded :by winner} (outcome rt loser))))
        (finally (show/stop! rt))))))

(deftest the-grace-is-an-operator-setting
  (let [{:keys [rt]} (harness)]
    (try
      (let [id (launch! rt)
            settings! (fn [values source]
                        (let [reply (show/submit! rt :settings {:scope :agent-holds :values values :source source})]
                          (show/tick! rt (inc (get-in @(:state rt) [:model :time-ms])))
                          (deref reply 1000 ::missing)))
            until (fn [t] (at! rt t {:op :param/set :instance-id id :param :level :value 0.5 :source push})
                    (:until-beat (first (:holds (show/schedule rt)))))]
        (is (= 8.0 (until 1000)) "by default the grace ends on the first bar line at least one bar after beat 2")
        (is (= :accepted (:status (settings! {:grace-bars 2} repl))))
        (is (= 16.0 (until 3000)) "two bars from beat 6 ends on bar line 16")
        (is (= :rejected (:status (settings! {:grace-bars 17} repl))))
        (is (= :rejected (:status (settings! {:grace-bars 0} agent-a))) "an agent cannot shorten its own exclusion")
        (is (= :accepted (:status (settings! {:grace-bars 0} repl))))
        (is (nil? (until 3500)) "with a zero grace an ungestured write holds nothing past itself"))
      (finally (show/stop! rt)))))

(deftest holds-start-only-from-human-writes-and-never-survive-a-restore
  (let [{:keys [rt]} (harness)]
    (try
      (let [id (launch! rt)]
        (at! rt 250 {:op :param/set :instance-id id :param :level :value 0.3 :source agent-a})
        (at! rt 300 {:op :param/set :instance-id id :param :level :value 0.3 :source {:adapter :composition :session-id "m"}})
        (is (empty? (:holds (show/schedule rt))) "an agent or a macro write holds nothing")
        (at! rt 500 {:op :variable/set :variable :section :value 2 :source repl})
        (at! rt 500 {:op :instance/opacity-set :instance-id id :value 0.5 :source x-touch})
        (is (= [{:target :opacity :instance-id id} {:target :variable :variable :section}]
               (mapv :destination (:holds (show/schedule rt))))
            "an opacity write and a REPL variable write hold their destinations")
        (is (= [:rejected :human-held]
               ((juxt :status :code) (at! rt 750 {:op :variable/set :variable :section :value 3 :source agent-a}))))
        (let [model (:model @(:state rt))
              restored (files/thaw (live-model) (files/freeze model))]
          (is (seq (get-in model [:event-subscribers :holds])))
          (is (nil? (get-in restored [:event-subscribers :holds])) "a restored show starts untouched")
          (is (empty? (:holds (subscribers/schedule-view restored))))))
      (finally (show/stop! rt)))))
