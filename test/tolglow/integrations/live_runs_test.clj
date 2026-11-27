(ns tolglow.integrations.live-runs-test
  "What actually ran becomes a run record on a real show runtime: one channel
  per launched cue, one take per activation, lanes for overlapping runs and
  the manual effect-group tree (#683). The record is then mirrored into a
  simulated Live Set (tolglow.integrations.fake-live) and run clips are played
  back into the show. No Live, Link, Max or MIDI device is used."
  (:require [afterglow.control.model :as control]
            [clojure.data.json :as json]
            [clojure.test :refer [deftest is testing]]
            [tolglow.integrations.fake-live :as fake]
            [tolglow.integrations.live-connection :as live]
            [tolglow.integrations.live-runs :as runs]
            [tolglow.show :as show]
            [tolglow.studio :as studio])
  (:import [java.io ByteArrayInputStream]))

(defn- effect [id]
  {:id id :version 1 :category :light
   :params {:level {:type :number :default 0.0 :bounds [0.0 1.0]}}
   :outputs {:dimmer {:op :param :key :level}}})

(def page-cues
  "A 64-cue page: every cue is a definition, and only the launched ones get
  a channel."
  (into {} (for [i (range 64) :let [id (keyword (str "c" i))]] [id {:id id :effect-id :wash :version 1}])))

(def registry
  {:effects {:wash (effect :wash)}
   :cues (merge page-cues
                {:a {:id :a :effect-id :wash :version 1 :name "Amber sweep"}
                 :b {:id :b :effect-id :wash :version 1 :name "Blue wash"}})})

(def groups
  {:manual [{:id :stage :name "Stage"
             :groups [{:id :warm :name "Warm" :cue-ids [:a]}]
             :cue-ids [:b]}]})

(defn- runtime
  ([] (runtime {:live-runs false}))
  ([options]
  (show/create-runtime
   {:model (control/new-show {:show-id :live-runs :transport {:playing? true :beat 0.0 :bpm 120.0}
                              :registry registry
                              :pages {:page-1 {:id :page-1 :cells (into {} (map-indexed (fn [i id] [[(mod i 8) (quot i 8)] id]) (keys page-cues)))}}
                              :rig {:fixtures [{:id :left :universe 1 :address 1 :channels {:dimmer 1}}]}})
    :sink {:send! (fn [_ _])} :workspace-options nil :phrase-capture false :live-runs (:live-runs options)})))

(def push {:adapter :push :session-id "pads"})

(def studio {:adapter :studio :session-id "browser"})

(defn- harness []
  (let [rt (runtime)
        record (atom (runs/new-record {:record-id "r1"}))
        tick! (fn [beat]
                ;; 120 BPM: a beat is 500 ms.
                (show/tick! rt (long (* 500 beat)))
                (swap! record runs/observe-model (:model @(:state rt))))
        submit! (fn [beat payload]
                  (let [reply (show/submit! rt :command payload)]
                    (tick! beat)
                    (deref reply 1000 ::missing)))]
    {:rt rt :record record :tick! tick! :submit! submit!}))

(defn- launch! [{:keys [submit!]} beat cue source & [params]]
  (:instance-id (submit! beat {:op :cue/launch :cue-id cue :source source
                               :launch (cond-> {:mode :persistent} params (assoc :params params))})))

(defn- release! [{:keys [submit!]} beat id source]
  (submit! beat {:op :instance/end :instance-id id :source source}))

(defn- perform!
  "Launch A, then B while A is held, release A, launch A again, release
  both. Then two overlapping runs of A. B comes from Studio, outside Live."
  [{:keys [tick! submit!] :as h}]
  (submit! 0 {:op :effect-groups/configure :config groups :source studio})
  (tick! 0)
  (let [a1 (launch! h 1 :a push {:level 0.5})
        b (launch! h 2 :b studio {:level 0.25})
        _ (submit! 3 {:op :param/set :instance-id a1 :param :level :value 0.75 :source push})
        _ (release! h 4 a1 push)
        a2 (launch! h 6 :a push)
        _ (release! h 7 b studio)
        _ (release! h 10 a2 push)
        a3 (launch! h 12 :a push {:level 1.0})
        a4 (launch! h 13 :a push {:level 0.1})]
    (submit! 14 {:op :param/set :instance-id a4 :param :level :value 0.2 :source push})
    (release! h 15 a3 push)
    (release! h 16 a4 push)
    (tick! 17)
    {:a1 a1 :a2 a2 :a3 a3 :a4 a4 :b b}))

(defn- timing [take]
  ((juxt :cue-id :lane :start-beat :release-beat) take))

(deftest each-activation-is-one-take-on-its-cues-channel
  (let [{:keys [rt record] :as h} (harness)]
    (try
      (let [ids (perform! h)
            r @record]
        (testing "only launched cues get channels, in first-launch order"
          (is (= [:a :b] (:channel-order r)))
          (is (= 66 (count (get-in (:model @(:state rt)) [:registry :cues]))) "the page still has all its definitions"))
        (testing "A then B while A is held, release A, relaunch A: two A takes on lane 0, one B take"
          (is (= [[:a 0 1 4] [:b 0 2 7] [:a 0 6 10] [:a 0 12 15] [:a 1 13 16]]
                 (mapv timing (runs/takes r)))))
        (testing "a take is a recorded run, distinct from the instance and the cue definition"
          (let [[t1 tb t2] (runs/takes r)]
            (is (= [(:a1 ids) (:b ids) (:a2 ids)] (mapv :instance-id [t1 tb t2])))
            (is (= ["r1.1" "r1.2" "r1.3"] (mapv :take-id [t1 tb t2])))
            (is (= {:level 0.5} (:params t1)) "launch values")
            (is (= [{:beat 3 :param :level :value 0.75 :at 2}] (:moves t1)) "parameter performance, with its offset into the run")
            (is (= [1 2] (mapv :run [t1 t2])) "successive runs of one cue are numbered")
            (is (= {:adapter :studio :session-id "browser"} (:source tb)) "B was launched from outside Live")
            (is (every? #(= :ended (:phase %)) (runs/takes r)) "takes outlive their instances")
            (is (empty? (:instances (:model @(:state rt)))))))
        (testing "overlapping runs of A hold separate lanes with their own identities and moves"
          (let [[_ _ _ a3 a4] (runs/takes r)]
            (is (= [0 1] (mapv :lane [a3 a4])))
            (is (= [(:a3 ids) (:a4 ids)] (mapv :instance-id [a3 a4])))
            (is (empty? (:moves a3)))
            (is (= [{:beat 14 :param :level :value 0.2 :at 1}] (:moves a4)))
            (is (= 2 (get-in r [:channels :a :lanes])))))
        (testing "channels follow the manual effect-group tree"
          (is (= [:stage :warm] (get-in r [:channels :a :group-path])))
          (is (= [:stage] (get-in r [:channels :b :group-path])))
          (is (= [{:id :stage :name "Stage" :groups [{:id :warm :name "Warm" :groups []}]}] (:groups r)))))
      (finally (show/stop! rt)))))

(defn- performed []
  (let [{:keys [rt record] :as h} (harness)]
    (try (let [ids (perform! h)] (assoc h :ids ids :record-value @record))
         (finally (show/stop! rt)))))

(defn- track-row [t] ((juxt :name :kind :group-track-id) t))

(defn- clips-on [set-snapshot name]
  (mapv (juxt :name :length-beats :slot-index)
        (:clips (first (filter #(= name (:name %)) (:tracks set-snapshot))))))

(deftest the-record-mirrors-into-run-tracks-clips-and-group-tracks
  (let [{r :record-value} (performed)
        ls (fake/create {:tracks [{:name "Bass" :arm? true :slots {0 {:clip-id "clip:1" :name "Bassline" :clip {:length-beats 8}}}}]
                          :scenes 2 :companion? false})
        stage (fake/add-group-track! ls "Tolglow group stage")
        warm (fake/add-group-track! ls "Tolglow group warm" stage)
        conn (live/open (fake/transport ls {:json? true}) {:session-id "mirror"})
        result (runs/sync! conn r)
        snapshot (:set result)]
    (testing "one track per channel lane, inside the Group Tracks of its effect group"
      (is (:complete? result) (pr-str (:problems result) (:applied result)))
      (is (empty? (:problems result)))
      (is (= [["Bass" :midi nil]
              ["Tolglow group stage" :group nil]
              ["Tolglow run b" :midi stage]
              ["Tolglow group warm" :group stage]
              ["Tolglow run a" :midi warm]
              ["Tolglow run a · 2" :midi warm]]
             (mapv track-row (:tracks snapshot)))))
    (testing "one clip per take with the held length; successive runs reuse A's track"
      (is (= [["Amber sweep run 1 [r1.1]" 3 0] ["Amber sweep run 2 [r1.3]" 4 1] ["Amber sweep run 3 [r1.4]" 3 2]]
             (clips-on snapshot "Tolglow run a")))
      (is (= [["Amber sweep run 4 [r1.5]" 3 0]] (clips-on snapshot "Tolglow run a · 2")))
      (is (= [["Blue wash run 1 [r1.2]" 5 0]] (clips-on snapshot "Tolglow run b")))
      (is (= 3 (:scenes @ls)) "a scene was added when A's track ran out of slots")
      (is (= [{:pitch runs/run-pitch :start 0 :duration 3 :velocity 127}]
             (get-in (:result (live/request! conn :read-clip {:clip-id (get-in result [:clips "r1.1" :clip-id])})) [:clip :notes]))))
    (testing "the performer's own track is untouched"
      (is (= [["Bassline" 8 0]] (clips-on snapshot "Bass")))
      (is (= {:port "Keyboard" :channel 1} (:input (first (:tracks snapshot))))))
    (testing "a rerun, or a reopened Set on a new connection, reconnects by name and writes nothing"
      (let [again (runs/sync! (live/open (fake/transport ls) {:session-id "reopened"}) r)]
        (is (:complete? again))
        (is (empty? (:applied again)))
        (is (= (:clips result) (:clips again)))
        (is (= (:tracks result) (:tracks again)))))
    (testing "the projection binds each take to its clip"
      (let [p (runs/projection r result)
            [a b] (:channels p)]
        (is (= ["Amber sweep" "Blue wash"] (mapv :name [a b])))
        (is (= [["Tolglow run a" 3] ["Tolglow run a · 2" 1]] (mapv (juxt :track-name (comp count :takes)) (:lanes a))))
        (is (= {:take-id "r1.1" :start-beat 1.0 :release-beat 4.0 :length-beats 3.0 :params {:level 0.5} :moves 1}
               (select-keys (first (:takes (first (:lanes a)))) [:take-id :start-beat :release-beat :length-beats :params :moves])))
        (is (every? #(= :in-live (get-in % [:clip :status])) (mapcat :takes (mapcat :lanes (:channels p)))))
        (is (= [:a] (get-in p [:groups 0 :groups 0 :channels])))
        (is (= [:b] (get-in p [:groups 0 :channels])))
        (is (= warm (get-in p [:groups 0 :groups 0 :track-id])))))))

(deftest group-tracks-that-cannot-be-created-leave-the-channels-ungrouped-and-reported
  (let [{r :record-value} (performed)
        ls (fake/create {:companion? false})
        result (runs/sync! (live/open (fake/transport ls) {}) r)]
    (is (not (:complete? result)) "the groups stay pending")
    (is (= [:group-creation-unavailable] (distinct (map :code (filter #(= :ensure-group (:op %)) (:problems result))))))
    (is (= [["Tolglow run a" :midi nil] ["Tolglow run a · 2" :midi nil] ["Tolglow run b" :midi nil]]
           (mapv track-row (:tracks (:set result)))))
    (is (= 5 (count (:clips result))) "every take still became a clip")
    (is (every? :ungrouped? (filter #(= :ensure-track (:op %)) (:applied result))))
    (testing "once a template places the groups, the existing tracks are reported outside them"
      (let [stage (fake/add-group-track! ls "Tolglow group stage")]
        (fake/add-group-track! ls "Tolglow group warm" stage)
        (let [again (runs/sync! (live/open (fake/transport ls) {}) r)]
          (is (empty? (:applied again)) "nothing is moved or recreated")
          (is (= #{["Tolglow run a" :not-in-group] ["Tolglow run a · 2" :not-in-group] ["Tolglow run b" :not-in-group]}
                 (set (map (juxt :name :code) (:problems again))))))))))

(deftest an-uncertain-clip-write-is-found-again-instead-of-written-twice
  (let [{r :record-value} (performed)
        ls (fake/create {:companion? false})
        conn (live/open (fake/transport ls) {})]
    (fake/inject-fault! ls :write-clip :ok :lose-ack)
    (let [first-run (runs/sync! conn r {:groups? false})]
      (is (not (:complete? first-run)))
      (is (= [:executed :indeterminate] (mapv :status (filter #(= :write-clip (:op %)) (:applied first-run))))
          "the run stops at the uncertain receipt")
      (is (= 2 (count (:clips first-run))) "the read-back finds the clip whose receipt was lost"))
    (let [second-run (runs/sync! conn r {:groups? false})]
      (is (:complete? second-run))
      (is (= 3 (count (filter #(= :write-clip (:op %)) (:applied second-run)))) "only the three missing clips")
      (is (= 5 (count (mapcat :clips (:tracks (:set second-run))))) "no clip was written twice"))
    (testing "a clip the performer deletes is written again on the next sync; one moved elsewhere is kept"
      (let [synced (runs/sync! conn r {:groups? false})
            c1 (get-in synced [:clips "r1.1" :clip-id])]
        (fake/delete-clip! ls c1)
        (let [after (runs/sync! conn r {:groups? false})]
          (is (= [{:op :write-clip :status :executed :take-id "r1.1"}] (map #(select-keys % [:op :status :take-id]) (:applied after)))))))))

(deftest a-running-take-is-recorded-but-not-written-until-released
  (let [{:keys [rt record tick!] :as h} (harness)]
    (try
      (tick! 0)
      (let [id (launch! h 1 :a push {:level 0.5})
            ls (fake/create {:companion? false})
            conn (live/open (fake/transport ls) {})
            mirror (runs/sync! conn @record {:groups? false})]
        (is (= [["Tolglow run a" :midi nil]] (mapv track-row (:tracks (:set mirror)))) "the channel exists from the first launch")
        (is (empty? (:clips mirror)))
        (is (= {:status :recording} (get-in (runs/projection @record mirror) [:channels 0 :lanes 0 :takes 0 :clip])))
        (release! h 3 id push)
        (let [mirror (runs/sync! conn @record {:groups? false :arrangement-offset-beats 16})]
          (is (= [["Amber sweep run 1 [r1.1]" 2 0]] (clips-on (:set mirror) "Tolglow run a")))
          (is (= [{:start 17 :length 2}] (map #(select-keys % [:start :length]) (get-in @ls [:tracks 0 :arrangement])))
              "placed in the Arrangement at its start beat plus the offset")))
      (finally (show/stop! rt)))))

(deftest playing-a-run-clip-steers-its-cue-once-and-records-nothing
  (let [{:keys [rt record tick! submit!] :as h} (harness)]
    (try
      (perform! h)
      (let [ls (fake/create {:companion? false})
            conn (live/open (fake/transport ls) {})
            mirror (runs/sync! conn @record {:groups? false})
            takes-before (count (:takes @record))
            n (atom 0)
            envelope (fn [] {:command-id (str "playback-" (swap! n inc))})
            player (atom (runs/player mirror {:bridge-id "fake"}))
            run! (fn [beat result]
                   (reset! player (:player result))
                   (doseq [c (:commands result)]
                     (let [ack (submit! beat c)]
                       (when (= :cue/launch (:op c))
                         (let [bound (runs/acknowledge @player ack envelope)]
                           (reset! player (:player bound))
                           (doseq [c (:commands bound)] (submit! beat c)))))))
            clip-id (get-in mirror [:clips "r1.1" :clip-id])
            track-id (get-in mirror [:tracks [:lane :a 0]])
            started {:type :clip-started :track-id track-id :clip-id clip-id :serial 7 :beat 20}]
        (run! 20 (runs/play @player @record started envelope))
        (run! 20 (runs/play @player @record started envelope))
        (let [running (vals (:instances (:model @(:state rt))))]
          (is (= 1 (count running)) "a repeated report of the same clip start launches once")
          (is (= {:level 0.5} (:params (first running))) "the take's launch values")
          (is (= [{:take-id "r1.1" :cue-id :a}] (mapv #(select-keys % [:take-id :cue-id]) (:playback (runs/projection @record))))))
        (run! 22 (runs/advance @player 22 envelope))
        (is (= 0.75 (get-in (first (vals (:instances (:model @(:state rt))))) [:params :level])) "the recorded move, two beats in")
        (run! 23 (runs/advance @player 23 envelope))
        (tick! 24)
        (is (empty? (:instances (:model @(:state rt)))) "released after the held three beats")
        (is (= takes-before (count (:takes @record))) "playback made no take")
        (testing "stopping a clip before its launch is acknowledged still ends that launch"
          (let [started (assoc started :serial 8 :beat 30)
                launched (runs/play @player @record started envelope)
                stopped (runs/play (:player launched) @record {:type :clip-stopped :track-id track-id :serial 9} envelope)
                ack (submit! 30 (first (:commands launched)))
                bound (runs/acknowledge (:player stopped) ack envelope)]
            (is (empty? (:commands stopped)))
            (is (= [:instance/end] (mapv :op (:commands bound))))
            (submit! 30 (first (:commands bound)))
            (tick! 31)
            (is (empty? (:instances (:model @(:state rt)))))
            (is (= takes-before (count (:takes @record)))))))
      (finally (show/stop! rt)))))

(defn- request [app uri]
  (app {:request-method :get :uri uri :headers {"host" "localhost"} :body (ByteArrayInputStream. (.getBytes "" "UTF-8"))}))

(deftest the-runtime-owns-the-record-and-studio-reads-the-same-projection
  (let [rt (runtime {:live-runs {:record-id "show1"}})
        app (studio/handler rt)
        h {:rt rt
           :tick! (fn [beat] (show/tick! rt (long (* 500 beat))))
           :submit! (fn [beat payload]
                      (let [reply (show/submit! rt :command payload)]
                        (show/tick! rt (long (* 500 beat)))
                        (deref reply 1000 ::missing)))}]
    (try
      (perform! h)
      (let [repl (show/live-runs rt)
            response (request app "/api/live-runs")
            ring (json/read-str (:body response) :key-fn keyword)]
        (is (= 200 (:status response)) (pr-str response))
        (is (= [[:a 0 1 4] [:b 0 2 7] [:a 0 6 10] [:a 0 12 15] [:a 1 13 16]]
               (mapv timing (runs/takes (show/live-runs-record rt))))
            "the runtime folds every owner cycle into the record")
        (testing "Studio's route serves the REPL's projection"
          (is (= 5 (:takes repl) (:takes ring)))
          (is (= ["a" "b"] (mapv :cue-id (:channels ring))))
          (is (= (mapv (comp count :lanes) (:channels repl)) (mapv (comp count :lanes) (:channels ring))))
          (is (= (mapv :take-id (mapcat :takes (mapcat :lanes (:channels repl))))
                 (mapv :take-id (mapcat :takes (mapcat :lanes (:channels ring))))))
          (is (= ["stage" "warm"] (:group-path (first (:channels ring)))))
          (is (= "show1.1" (get-in ring [:channels 0 :lanes 0 :takes 0 :take-id])))
          (is (= "not-in-live" (get-in ring [:channels 0 :lanes 0 :takes 0 :clip :status])))
          (is (pos? (get-in ring [:cost :cycles]))))
        (testing "a sync from the REPL shows in Studio: each take bound to its clip"
          (let [ls (fake/create {:companion? false})
                stage (fake/add-group-track! ls "Tolglow group stage")
                _ (fake/add-group-track! ls "Tolglow group warm" stage)
                result (show/sync-live-runs! rt (live/open (fake/transport ls) {}))
                ring (json/read-str (:body (request app "/api/live-runs")) :key-fn keyword)]
            (is (:complete? result))
            (is (= {:complete? true :pending 0 :problems []} (:mirror ring)))
            (is (every? #(= "in-live" (get-in % [:clip :status])) (mapcat :takes (mapcat :lanes (:channels ring)))))
            (is (= (get-in result [:clips "show1.1" :clip-id]) (get-in ring [:channels 0 :lanes 0 :takes 0 :clip :clip-id]))))))
      (finally (show/stop! rt)))))

(deftest the-run-record-can-be-disabled
  (let [rt (runtime {:live-runs false})
        app (studio/handler rt)]
    (try
      (show/tick! rt 0)
      (is (thrown? clojure.lang.ExceptionInfo (show/live-runs rt)))
      (is (= 400 (:status (request app "/api/live-runs"))))
      (finally (show/stop! rt)))))

;; ---------------------------------------------------------------------------
;; Review follow-ups (#750)

(defn- fold-models
  "Fold plain show models, one per step: {:beat b :epoch e :playing? bool
  :commands [[command ack] ..] :instances {id phase}}."
  [record steps]
  (reduce (fn [record [i {:keys [beat epoch commands instances playing?] :or {epoch 0 playing? true}}]]
            (let [ids (map-indexed (fn [j _] (str "s" i "-" j)) commands)]
              (runs/observe-model record
                                  {:epoch epoch :time-ms (* 500 beat)
                                   :transport {:beat (double beat) :bpm 120.0 :playing? playing? :time-ms (* 500 beat)}
                                   :registry {:cues {:a {:name "A" :version 3}}}
                                   :instances (into {} (for [[id phase] instances] [id {:id id :phase phase :effect-version 7}]))
                                   :control {:history (into {} (map (fn [id [c a]] [id {:command (assoc c :command-id id :time-ms (* 500 beat)
                                                                                                          :source (or (:source c) {:adapter :push}))
                                                                                        :ack (assoc a :status :accepted)}])
                                                                    ids commands))
                                             :history-order (vec ids)}})))
          record (map-indexed vector steps)))

(def ^:private launch-a [{:op :cue/launch :cue-id :a :launch {:mode :persistent}} {:instance-id "0:1"}])

(deftest a-seek-inside-a-run-does-not-change-its-hold
  (let [r (fold-models (runs/new-record) [{:beat 8 :commands [launch-a] :instances {"0:1" :running}}
                                         {:beat 10 :instances {"0:1" :running}}
                                         {:beat 1 :instances {"0:1" :running}}
                                         {:beat 2 :commands [[{:op :instance/end :instance-id "0:1"} {}]]}])
        [t] (runs/takes r)]
    (is (= [8 2] [(:start-beat t) (:release-beat t)]))
    (is (= 3 (runs/held-beats t)) "2 beats before the seek and 1 after")
    (is (= 3 (runs/clip-beats t)))))

(deftest a-show-restart-ends-open-takes-at-the-restart
  (let [r (fold-models (runs/new-record) [{:beat 1 :commands [launch-a] :instances {"0:1" :running}}
                                         {:beat 4 :instances {"0:1" :running}}
                                         {:beat 5 :epoch 1}])
        [t] (runs/takes r)]
    (is (= [5 :epoch] [(:end-beat t) (:interrupted t)]) "the new epoch's first beat, not the last folded one")))

(deftest parameter-moves-are-bounded-per-take-under-a-burst
  (let [moves (for [i (range 2000)] [{:op :param/set :instance-id "0:1" :param (if (even? i) :level :hue) :value (/ i 2000.0)} {}])
        record (runs/new-record {:max-moves 64})
        ;; 2,000 moves over 100 owner cycles of 20 moves, two parameters.
        steps (cons {:beat 0 :commands [launch-a] :instances {"0:1" :running}}
                    (for [[i chunk] (map-indexed vector (partition 20 moves))]
                      {:beat (/ (inc i) 4) :commands chunk :instances {"0:1" :running}}))
        start (System/nanoTime)
        r (fold-models record steps)
        elapsed-ms (/ (- (System/nanoTime) start) 1e6)
        [t] (runs/takes r)]
    (is (= 64 (count (:moves t))) "moves of one parameter on one beat coalesce, then the cap holds")
    (is (:truncated? t))
    (is (= 1360 (:moves-dropped t)) "32 cycles fill the 64 kept moves; the other 1,360 writes are counted")
    (is (= [0.0095 0.009] (mapv :value (take 2 (sort-by :param (filter #(= 1/4 (:beat %)) (:moves t))))))
        "the last value of each parameter in a cycle wins")
    (is (= 1360 (:moves-dropped r)))
    (is (= 1360 (get-in (runs/projection r) [:channels 0 :lanes 0 :takes 0 :moves-dropped])))
    (println (format "live-runs burst fold: 2,000 moves over 101 cycles in %.1f ms" elapsed-ms))))

(deftest a-take-keeps-its-launch-and-definition-version-and-replays-the-launch
  (let [launch {:mode :timed :duration-ms 750 :targets [:left] :params {:level 0.4}}
        r (fold-models (runs/new-record) [{:beat 0 :commands [[{:op :cue/launch :cue-id :a :launch launch} {:instance-id "0:1"}]]
                                           :instances {"0:1" :running}}
                                          {:beat 2}])
        [t] (runs/takes r)]
    (is (= launch (:launch t)))
    (is (= [7 3] [(:effect-version t) (:cue-version t)]))
    (is (= launch (runs/replay-launch t)) "mode, targets and duration replay as recorded")
    (is (= :persistent (:mode (runs/replay-launch (assoc-in t [:launch :mode] :held)))) "a held pad's release is the clip's")
    (let [p (runs/player {:clips {(:take-id t) {:clip-id "clip:1"}}})
          {:keys [commands player]} (runs/play p r {:type :clip-started :track-id "track:1" :clip-id "clip:1" :serial 1 :beat 0}
                                               (fn [] {:command-id "c1"}))]
      (is (= [:cue/launch] (mapv :op commands)))
      (is (= 7 (get-in commands [0 :source :live/effect-version])))
      (is (= [[2 {:op :instance/end}]] (get-in player [:playing "track:1" :due]))
          "the replay ends after the take's actual length, even for a timed launch"))))

(deftest a-reloaded-companion-counting-from-one-again-is-not-a-repeat
  (let [r (fold-models (runs/new-record) [{:beat 0 :commands [launch-a] :instances {"0:1" :running}}
                                         {:beat 2 :commands [[{:op :instance/end :instance-id "0:1"} {}]]}])
        take-id (:take-id (first (runs/takes r)))
        n (atom 0)
        env (fn [] {:command-id (str "c" (swap! n inc))})
        play (fn [p e] (runs/play p r e env))
        s1 (play (runs/player {:clips {take-id {:clip-id "clip:1"}}}) {:type :clip-started :track-id "t" :clip-id "clip:1" :watch-session "w1" :serial 1 :beat 0})
        s2 (play (:player s1) {:type :clip-stopped :track-id "t" :watch-session "w1" :serial 2 :beat 1})
        again (play (:player s2) {:type :clip-started :track-id "t" :clip-id "clip:1" :watch-session "w1" :serial 1 :beat 2})
        reloaded (play (:player s2) {:type :clip-started :track-id "t" :clip-id "clip:1" :watch-session "w2" :serial 1 :beat 2})]
    (is (empty? (:commands again)) "the same session and serial is a repeat")
    (is (= [:cue/launch] (mapv :op (:commands reloaded))) "a new watch session's serial 1 is a new start")
    (testing "unknown clips are counted, and only the last few kept"
      (let [p (reduce (fn [p i] (:player (play p {:type :clip-started :track-id "t" :clip-id (str "clip:x" i) :watch-session "w3" :serial i})))
                      (runs/player {}) (range 100))]
        (is (= 100 (:ignored-count p)))
        (is (= 16 (count (:ignored p))))))))

(deftest live-input-makes-takes-and-clip-playback-does-not
  (let [r (fold-models (runs/new-record)
                       [{:beat 0 :commands [[{:op :cue/launch :cue-id :a :launch {:mode :persistent} :source {:type :ableton :origin :live-input}} {:instance-id "0:1"}]
                                            [{:op :cue/launch :cue-id :a :launch {:mode :persistent} :source {:type :ableton :origin :live-playback}} {:instance-id "0:2"}]]
                         :instances {"0:1" :running "0:2" :running}}])]
    (is (= ["0:1"] (mapv :instance-id (runs/takes r))) "a looping launcher or Capture clip does not record a take each pass")))

(deftest arrangement-placement-is-its-own-step-and-an-overlap-holds-back-nothing
  (let [;; Two takes of one cue whose beats overlap after a transport loop.
        r (fold-models (runs/new-record {:record-id "x"})
                       [{:beat 4 :commands [launch-a] :instances {"0:1" :running}}
                        {:beat 8 :commands [[{:op :instance/end :instance-id "0:1"} {}]]}
                        {:beat 5 :commands [[{:op :cue/launch :cue-id :a :launch {:mode :persistent}} {:instance-id "0:2"}]] :instances {"0:2" :running}}
                        {:beat 7 :commands [[{:op :instance/end :instance-id "0:2"} {}]]}])
        ls (fake/create {:companion? false})
        conn (live/open (fake/transport ls) {})
        options {:groups? false :arrangement-offset-beats 0}]
    (fake/inject-fault! ls :place-clip :refuse)
    (let [first-sync (runs/sync! conn r options)]
      (is (= 2 (count (:clips first-sync))) "both takes have Session clips")
      (is (every? #(false? (get-in @ls [:tracks 0 :slots % :looping?])) [0 1]) "run clips are written without looping")
      (is (= [{:op :place-clip :status :rejected :take-id "x.1"}]
             (mapv #(select-keys % [:op :status :take-id]) (filter #(= :place-clip (:op %)) (:applied first-sync))))
          "the first placement failed; the overlapping one was never attempted")
      (is (= #{[:placement-overlap "x.2"] [:injected "x.1"]} (set (map (juxt :code :take-id) (filter :take-id (:problems first-sync))))))
      (is (not (:complete? first-sync)) "x.1 still waits for its Arrangement copy"))
    (let [repair (runs/sync! conn r options)
          p (runs/projection r repair)]
      (is (= [:place-clip] (mapv :op (:applied repair))) "the failed placement is repaired on the next sync")
      (is (:complete? repair) "the overlap is reported, not retried forever")
      (is (= [[:placement-overlap "x.2"]] (mapv (juxt :code :take-id) (:problems repair))))
      (is (= {:status :placed :start-beat 4.0} (get-in p [:channels 0 :lanes 0 :takes 0 :arrangement])))
      (is (= {:status :placement-overlap} (get-in p [:channels 0 :lanes 0 :takes 1 :arrangement])) "x.2 reused lane 0 after x.1's release"))
    (is (empty? (:applied (runs/sync! conn r options))) "a third sync writes nothing")))

(deftest the-runtime-plays-run-clips-reported-by-the-companion
  (let [rt (runtime {:live-runs {:record-id "rt"}})
        h {:tick! (fn [beat] (show/tick! rt (long (* 500 beat))))
           :submit! (fn [beat payload]
                      (let [reply (show/submit! rt :command payload)]
                        (show/tick! rt (long (* 500 beat)))
                        (deref reply 1000 ::missing)))}
        tick! (:tick! h)
        instances #(vals (:instances (:model @(:state rt))))]
    (try
      (perform! h)
      (is (= :not-synced (:reason (show/live-run-clip! rt {:type "clip-started"}))))
      (let [ls (fake/create {:companion? false})
            mirror (show/sync-live-runs! rt (live/open (fake/transport ls) {}) {:groups? false})
            clip-id (get-in mirror [:clips "rt.1" :clip-id])
            track-id (get-in mirror [:tracks [:lane :a 0]])
            takes (count (:takes (show/live-runs-record rt)))
            ;; Exactly what the companion emits: Live song time, no show beat.
            event {:type "clip-started" :track-id track-id :clip-id clip-id :watch-session "w1" :serial 1 :song-beat 512}]
        (tick! 20)
        (is (= {:status :queued} (show/live-run-clip! rt event)) "the callback only queues")
        (is (= {:status :queued} (show/live-run-clip! rt event)))
        (is (empty? (instances)) "nothing happens until the owner runs")
        ;; The owner decodes and submits in one cycle; the launch applies in the next.
        (tick! 20) (tick! 20)
        (is (= [{:level 0.5}] (mapv :params (instances))) "one launch with the take's values; the repeat launched nothing")
        ;; A move the owner reaches in one cycle is applied in the next.
        (tick! 22.25) (tick! 22.5)
        (is (= [0.75] (mapv #(get-in % [:params :level]) (instances))) "the owner replays the move two beats in")
        (doseq [b [23 23.25 23.5]] (tick! b))
        (is (empty? (instances)) "and releases after the held three beats")
        (is (= takes (count (:takes (show/live-runs-record rt)))) "playback made no take"))
      (finally (show/stop! rt)))))

(deftest the-projection-lists-only-the-newest-takes
  (let [steps (mapcat (fn [i] [{:beat (* 2 i) :commands [[{:op :cue/launch :cue-id :a :launch {:mode :persistent}} {:instance-id (str "0:" i)}]]
                                :instances {(str "0:" i) :running}}
                               {:beat (inc (* 2 i)) :commands [[{:op :instance/end :instance-id (str "0:" i)} {}]]}])
                      (range 10))
        r (fold-models (runs/new-record {:record-id "p"}) steps)
        p (runs/projection r nil {:limit 3})]
    (is (= [10 3 true] ((juxt :takes :shown :truncated?) p)))
    (is (= ["p.8" "p.9" "p.10"] (mapv :take-id (mapcat :takes (mapcat :lanes (:channels p))))))))

(defn- live-runs-pending [state]
  (runs/pending-launches (:player state)))

(defn- synced-runtime
  "A runtime that performed, synced to the simulated Set; returns it with the
  run-clip event for take rt.1."
  []
  (let [rt (runtime {:live-runs {:record-id "rt"}})
        h {:tick! (fn [beat] (show/tick! rt (long (* 500 beat))))
           :submit! (fn [beat payload]
                      (let [reply (show/submit! rt :command payload)]
                        (show/tick! rt (long (* 500 beat)))
                        (deref reply 1000 ::missing)))}]
    (perform! h)
    (let [mirror (show/sync-live-runs! rt (live/open (fake/transport (fake/create {:companion? false})) {}) {:groups? false})]
      {:rt rt :track-id (get-in mirror [:tracks [:lane :a 0]])
       :event {:type "clip-started" :track-id (get-in mirror [:tracks [:lane :a 0]])
               :clip-id (get-in mirror [:clips "rt.1" :clip-id]) :watch-session "w1"}})))

(deftest run-clip-events-from-another-thread-never-deadlock-the-owner
  (let [{:keys [rt event track-id]} (synced-runtime)
        running (atom true)
        owner (future (loop [beat 20.0] (when @running (show/tick! rt (long (* 500 beat))) (recur (+ beat 0.05)))))
        callback (future (doseq [i (range 200)]
                           (show/live-run-clip! rt (assoc event :serial (* 2 i) :type (if (even? i) "clip-started" "clip-stopped")))
                           (Thread/sleep 1)))]
    (try
      (is (not= ::timeout (deref callback 10000 ::timeout)) "the callback thread finished")
      (reset! running false)
      (is (not= ::timeout (deref owner 10000 ::timeout)) "the owner thread kept cycling")
      (is (nil? (.findDeadlockedThreads (java.lang.management.ManagementFactory/getThreadMXBean))))
      (finally (reset! running false) (show/stop! rt)))))

(deftest a-rejected-run-clip-launch-is-not-left-pending
  (let [{:keys [rt event]} (synced-runtime)]
    (try
      (show/tick! rt 10000)
      ;; The cue is gone, so the launch is refused.
      (deref (show/submit! rt :command {:op :cue/delete :cue-id :a :source studio}) 0 nil)
      (show/tick! rt 10010)
      (show/live-run-clip! rt (assoc event :serial 1))
      (doseq [t [10020 10030 10040 10050]] (show/tick! rt t))
      (let [state @(:live-runs rt)]
        (is (empty? (live-runs-pending state)) "the refusal was read from the reply")
        (is (empty? (:replies state))))
      (finally (show/stop! rt)))))

(deftest a-stopped-clip-ends-its-replay-even-when-nothing-else-is-due
  (let [r (fold-models (runs/new-record) [{:beat 0 :commands [[{:op :cue/launch :cue-id :a :launch {:mode :timed :duration {:beats 8}}} {:instance-id "0:1"}]]
                                           :instances {"0:1" :running}}
                                          {:beat 2 :instances {}}])
        [t] (runs/takes r)
        env (let [n (atom 0)] (fn [] {:command-id (str "c" (swap! n inc))}))
        p (runs/player {:clips {(:take-id t) {:clip-id "clip:1"}}})
        started (runs/play p r {:type :clip-started :track-id "t" :clip-id "clip:1" :serial 1 :beat 100} env)
        bound (runs/acknowledge (:player started) {:command-id "c1" :status :accepted :instance-id "9:1"} env)
        later (runs/advance (:player bound) 101 env #{"9:1"})
        stopped (runs/play (:player later) r {:type :clip-stopped :track-id "t" :serial 2 :beat 101} env)]
    (is (= 2 (runs/held-beats t)) "the timed run ended after 2 of its 8 beats")
    (is (= [[2 {:op :instance/end}]] (get-in later [:player :playing "t" :due])) "its replay ends after 2 beats")
    (is (= [[:instance/end "9:1"]] (mapv (juxt :op :instance-id) (:commands stopped))) "an early clip stop ends it")
    (is (empty? (:playing (:player (runs/advance (:player bound) 101 env #{})))) "an instance that already ended is dropped")))

(deftest a-move-after-a-seek-replays-at-its-real-offset
  (let [r (fold-models (runs/new-record) [{:beat 8 :commands [launch-a] :instances {"0:1" :running}}
                                         {:beat 10 :instances {"0:1" :running}}
                                         {:beat 1 :instances {"0:1" :running}}
                                         {:beat 2 :commands [[{:op :param/set :instance-id "0:1" :param :level :value 0.3} {}]]
                                          :instances {"0:1" :running}}
                                         {:beat 3 :commands [[{:op :instance/end :instance-id "0:1"} {}]]}])
        [t] (runs/takes r)
        {:keys [player]} (runs/play (runs/player {:clips {(:take-id t) {:clip-id "clip:1"}}}) r
                                    {:type :clip-started :track-id "t" :clip-id "clip:1" :serial 1 :beat 0} (fn [] {:command-id "c1"}))]
    (is (= 3 (:at (first (:moves t)))) "two beats before the seek and one after")
    (is (= [[3 :param/set] [4 :instance/end]] (mapv (fn [[at c]] [at (:op c)]) (get-in player [:playing "t" :due]))))))

(deftest a-replay-end-refused-by-a-full-owner-queue-is-sent-again
  (let [{:keys [rt event]} (synced-runtime)
        state (:live-runs rt)
        instances #(vals (:instances (:model @(:state rt))))]
    (try
      (show/tick! rt 10000)
      (show/live-run-clip! rt (assoc event :serial 1))
      (doseq [t [10010 10020 10030]] (show/tick! rt t))
      (is (= 1 (count (instances))) "the replay runs")
      ;; Fill the owner queue with 256 commands nobody has drained yet.
      (dotimes [_ 256] (show/submit! rt :command {:op :master/set :value 1.0 :source studio}))
      ;; This test thread stands in for the owner, which reaches the replay's
      ;; end (three held beats in) while the queue is full.
      (#'show/advance-run-player! rt state (assoc-in (:model @(:state rt)) [:transport :beat] 1000.0))
      (is (= [:rejected] (mapv (comp :status deref :reply) (vals (:end-replies @state)))) "the end was refused")
      (is (empty? (:playing (:player @state))) "the player has let the entry go")
      (show/tick! rt 10040)
      (is (= 1 (count (instances))) "the backlog drained; the instance still runs, so the owner sent the end again")
      (show/tick! rt 10050)
      (is (empty? (instances)) "the resent end ended the replay")
      (is (empty? (filter (comp #(and (realized? %) (= :rejected (:status @%))) :reply) (vals (:end-replies @state)))))
      (finally (show/stop! rt)))))

(deftest dropped-playback-events-are-visible-to-the-repl-and-studio
  (let [{:keys [rt event]} (synced-runtime)
        app (studio/handler rt)]
    (try
      (dotimes [i 300] (show/live-run-clip! rt (assoc event :serial i :type "clip-stopped")))
      (is (= {:queued 256 :dropped 44} (:playback-events (show/live-runs rt))))
      (is (= {:queued 256 :dropped 44} (:playback-events (json/read-str (:body (request app "/api/live-runs")) :key-fn keyword))))
      (finally (show/stop! rt)))))
