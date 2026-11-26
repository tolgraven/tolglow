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
            (is (= [{:beat 3 :param :level :value 0.75}] (:moves t1)) "parameter performance")
            (is (= [1 2] (mapv :run [t1 t2])) "successive runs of one cue are numbered")
            (is (= {:adapter :studio :session-id "browser"} (:source tb)) "B was launched from outside Live")
            (is (every? #(= :ended (:phase %)) (runs/takes r)) "takes outlive their instances")
            (is (empty? (:instances (:model @(:state rt)))))))
        (testing "overlapping runs of A hold separate lanes with their own identities and moves"
          (let [[_ _ _ a3 a4] (runs/takes r)]
            (is (= [0 1] (mapv :lane [a3 a4])))
            (is (= [(:a3 ids) (:a4 ids)] (mapv :instance-id [a3 a4])))
            (is (empty? (:moves a3)))
            (is (= [{:beat 14 :param :level :value 0.2}] (:moves a4)))
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
          (is (= [{:op :write-clip :status :executed :take-id "r1.1"}] (:applied after))))))))

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
