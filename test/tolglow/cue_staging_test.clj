(ns tolglow.cue-staging-test
  (:require [clojure.data.json :as json]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [tolglow.show :as show]
            [tolglow.studio :as studio]
            [tolglow.studio.access :as access]
            [tolglow.transit :as transit])
  (:import [java.io ByteArrayInputStream]))

(def ^:private operator {:operator-id "repl/stage-test"})

(defn- apply-at!
  ([runtime payload time] (apply-at! runtime payload time nil))
  ([runtime payload time authority]
   (let [receipt (show/submit! runtime :command payload authority)]
     (show/tick! runtime time)
     (deref receipt 1000 ::missing))))

(defn- model [runtime] (:model @(:state runtime)))

(defn- staged [runtime stage-id]
  (some #(when (= stage-id (:stage-id %)) %) (show/staged-cues runtime)))

(defn- history [runtime] (show/operator-history runtime))

(defn- undo! [runtime time]
  (let [{:keys [current-id position]} (history runtime)]
    (apply-at! runtime {:op :history/undo :expected-cursor current-id :expected-history-position position}
               time operator)))

(defn- redo! [runtime node-id time]
  (let [{:keys [current-id position]} (history runtime)]
    (apply-at! runtime {:op :history/redo :expected-cursor current-id :expected-history-position position
                        :node-id node-id} time operator)))

(defn- level [runtime instance-id] (get-in (model runtime) [:instances instance-id :params :level]))

(deftest staged-edits-share-history-and-launch-with-their-values-on-the-grid
  (let [runtime (show/create-runtime {:workers 1 :workspace-options nil})]
    (try
      (is (= {:status :accepted :stage-id "s1"}
             (select-keys (apply-at! runtime {:op :cue-stage/open :cue-id :ember :stage-id "s1"} 0)
                          [:status :stage-id])))
      (let [stage (staged runtime "s1")]
        (is (= [:staged :ember] ((juxt :status :cue-id) stage)))
        (is (not (contains? stage :instance-id)) "a stage names no instance")
        (is (number? (get-in stage [:params :level])))
        (is (= :number (get-in stage [:schema :level :type]))))
      (is (empty? (:instances (model runtime))) "a stage renders nothing")
      (is (empty? (:instances (show/snapshot runtime))))

      (testing "an atomic edit is one history step on the stage"
        (let [ack (apply-at! runtime {:op :cue-stage/edit :stage-id "s1" :param :level :value 0.3
                                      :command-id "edit-1"} 10 operator)]
          (is (= [:accepted 1] ((juxt :status :history-cursor) ack)))
          (is (= 0.3 (get-in (staged runtime "s1") [:params :level])))
          (is (= [{:id 1 :stage-id "s1" :parameter :level :after 0.3 :cue-id :ember}]
                 (mapv #(select-keys % [:id :stage-id :parameter :after :cue-id])
                       (rest (:nodes (history runtime))))))
          (is (= ack (apply-at! runtime {:op :cue-stage/edit :stage-id "s1" :param :level :value 0.3
                                         :command-id "edit-1"} 11 operator))
              "a retry returns the original receipt")))

      (testing "a grouped gesture addressed to the stage makes one step"
        (let [gesture {:stage-id "s1" :param :level :control-id "encoder-1" :gesture-id "turn-1"}]
          (is (= :accepted (:status (apply-at! runtime (assoc gesture :op :gesture/begin) 20 operator))))
          (doseq [[sequence value] [[1 0.4] [2 0.5] [3 0.6]]]
            (is (= :accepted (:status (apply-at! runtime (assoc gesture :op :gesture/sample :sequence sequence :value value)
                                                 (+ 20 sequence) operator))))
            (is (= value (get-in (staged runtime "s1") [:params :level]))))
          (is (= 2 (:history-cursor (apply-at! runtime (assoc gesture :op :gesture/end :last-sequence 3) 30 operator))))
          (is (= [1 2] (mapv :id (rest (:nodes (history runtime))))))))

      (testing "undo and redo travel the shared history on the stage"
        (is (= [:accepted 1] ((juxt :status :history-cursor) (undo! runtime 40))))
        (is (= 0.3 (get-in (staged runtime "s1") [:params :level])))
        (is (= [:accepted 2] ((juxt :status :history-cursor) (redo! runtime 2 41))))
        (is (= 0.6 (get-in (staged runtime "s1") [:params :level])))
        (is (empty? (:instances (model runtime)))))

      (testing "a grid launch freezes the values and fires at the boundary"
        (let [ack (apply-at! runtime {:op :cue-stage/launch :stage-id "s1" :grid {:bars 1} :command-id "go-1"} 100)]
          (is (= [:pending 4] ((juxt :status :due-beat) ack))))
        (is (= :scheduled (:status (staged runtime "s1"))))
        (is (= :stage-unavailable
               (:code (apply-at! runtime {:op :cue-stage/edit :stage-id "s1" :param :level :value 0.9
                                          :command-id "late-edit"} 110 operator))))
        (is (every? #(nil? (:target-unavailable-reason %)) (:nodes (history runtime)))
            "a waiting stage keeps its history steps travellable")
        (show/tick! runtime 1900)
        (is (empty? (:instances (model runtime))))
        (show/tick! runtime 2000)
        (let [[instance] (vals (:instances (model runtime)))
              id (:id instance)]
          (is (= [:ember 0.6 2000] ((juxt :cue-id (comp :level :params) :started-at-ms) instance)))
          (is (nil? (staged runtime "s1")))
          (is (= {:status :fired :instance-id id} (select-keys (show/quantized-cue-status runtime "go-1")
                                                              [:status :instance-id])))
          (testing "the staged edits stay undoable on the running instance"
            (is (every? #(nil? (:stage-id %)) (:nodes (history runtime))))
            (is (every? #(nil? (:unavailable-reason %)) (:nodes (history runtime))))
            (is (= 1 (:history-cursor (undo! runtime 2010))))
            (is (= 0.3 (level runtime id)))
            (is (= 2 (:history-cursor (redo! runtime 2 2020))))
            (is (= 0.6 (level runtime id))))))
      (finally (show/stop! runtime)))))

(deftest cancelling-a-stage-launches-nothing-and-returns-the-cursor
  (let [runtime (show/create-runtime {:workers 1 :workspace-options nil})]
    (try
      (let [id (:instance-id (apply-at! runtime {:op :cue/launch :cue-id :ember :launch {:params {:level 0.2}}} 0))]
        (is (= :accepted (:status (apply-at! runtime {:op :gesture/begin :instance-id id :param :level
                                                       :control-id "c" :gesture-id "g"} 2 operator))))
        (apply-at! runtime {:op :gesture/sample :instance-id id :param :level :control-id "c" :gesture-id "g"
                            :sequence 1 :value 0.4} 3 operator)
        (is (= 1 (:history-cursor (apply-at! runtime {:op :gesture/end :instance-id id :param :level :control-id "c"
                                                       :gesture-id "g" :last-sequence 1} 4 operator))))
        (apply-at! runtime {:op :cue-stage/open :cue-id :ice :stage-id "s2"} 10)
        (apply-at! runtime {:op :cue-stage/edit :stage-id "s2" :param :level :value 0.9 :command-id "e2"} 11 operator)
        (let [{:keys [current-id position]} (history runtime)]
          (is (= 2 current-id))
          (is (= :accepted (:status (apply-at! runtime {:op :cue-stage/cancel :stage-id "s2"} 12))))
          (is (nil? (staged runtime "s2")))
          (is (= [nil 0] (mapv :parent-id (:nodes (history runtime)))))
          (is (= [1 (inc position)] ((juxt :current-id :position) (history runtime))))
          (is (= [id] (keys (:instances (model runtime))))))
        (testing "undo continues with the edit before the stage"
          (is (= 0 (:history-cursor (undo! runtime 20))))
          (is (= 0.2 (level runtime id))))
        (testing "cancelling a scheduled stage withdraws its launch"
          (apply-at! runtime {:op :cue-stage/open :cue-id :ice :stage-id "s3"} 30)
          (is (= :pending (:status (apply-at! runtime {:op :cue-stage/launch :stage-id "s3" :grid {:bars 1}
                                                       :command-id "go-3"} 40))))
          (is (= :accepted (:status (apply-at! runtime {:op :cue-stage/cancel :stage-id "s3"} 50))))
          (is (= :cancelled (:status (show/quantized-cue-status runtime "go-3"))))
          (show/tick! runtime 4000)
          (is (= [id] (keys (:instances (model runtime)))))
          (is (empty? (show/staged-cues runtime)))))
      (finally (show/stop! runtime)))))

(deftest staged-launches-keep-intake-order-and-settle-open-gestures
  (let [runtime (show/create-runtime {:workers 1 :workspace-options nil})]
    (try
      (doseq [[stage cue] [["a" :ember] ["c" :ice]]]
        (apply-at! runtime {:op :cue-stage/open :cue-id cue :stage-id stage} 0))
      (testing "an open gesture commits under its owner when the cue launches"
        (let [gesture {:stage-id "c" :param :level :control-id "encoder" :gesture-id "held-turn"}]
          (apply-at! runtime (assoc gesture :op :gesture/begin) 5 operator)
          (apply-at! runtime (assoc gesture :op :gesture/sample :sequence 1 :value 0.4) 6 operator)))
      (is (= :pending (:status (apply-at! runtime {:op :cue-stage/launch :stage-id "a" :grid {:beats 1}
                                                   :command-id "go-a"} 10))))
      (is (= :pending (:status (let [reply (show/submit! runtime :cue-schedule
                                                         {:command {:op :cue/launch :command-id "plain" :cue-id :ember
                                                                    :source {:adapter :repl :session-id "t"}}
                                                          :grid {:beats 1}})]
                                 (show/tick! runtime 15)
                                 (deref reply 1000 ::missing)))))
      (is (= :pending (:status (apply-at! runtime {:op :cue-stage/launch :stage-id "c" :grid {:beats 1}
                                                   :command-id "go-c"} 20))))
      (is (= {} (get-in (model runtime) [:control :edit-history :open])))
      (is (= [{:actor "REPL" :parameter :level :after 0.4 :stage-id "c"}]
             (mapv #(select-keys % [:actor :parameter :after :stage-id]) (rest (:nodes (history runtime))))))
      (show/tick! runtime 500)
      (let [instances (sort-by :order (vals (:instances (model runtime))))]
        (is (= [:ember :ember :ice] (mapv :cue-id instances)))
        (is (= (mapv #(:instance-id (show/quantized-cue-status runtime %)) ["go-a" "plain" "go-c"])
               (mapv :id instances)))
        (is (= 0.4 (get-in (last instances) [:params :level]))))
      (testing "an immediate launch reports its instance and stage"
        (apply-at! runtime {:op :cue-stage/open :cue-id :ice :stage-id "now" :params {:level 0.25}} 510)
        (let [ack (apply-at! runtime {:op :cue-stage/launch :stage-id "now" :command-id "go-now"} 520)]
          (is (= [:accepted "now"] ((juxt :status :stage-id) ack)))
          (is (= 0.25 (level runtime (:instance-id ack))))
          (is (= ack (apply-at! runtime {:op :cue-stage/launch :stage-id "now" :command-id "go-now"} 530))
              "a retried launch replays its receipt")
          (is (= 4 (count (:instances (model runtime)))))))
      (finally (show/stop! runtime)))))

(deftest staged-commands-refuse-without-changing-the-show
  (let [runtime (show/create-runtime {:workers 1 :workspace-options nil})]
    (try
      (apply-at! runtime {:op :cue-stage/open :cue-id :ember :stage-id "s"} 0)
      (let [before (model runtime)
            refusal #(select-keys (apply-at! runtime %1 %2 %3) [:status :code])]
        (is (= {:status :rejected :code :operator-required}
               (refusal {:op :cue-stage/edit :stage-id "s" :param :level :value 0.5 :command-id "a"} 1 nil)))
        (is (= {:status :rejected :code :invalid-value}
               (refusal {:op :cue-stage/edit :stage-id "s" :param :level :value "loud" :command-id "b"} 2 operator)))
        (is (= {:status :rejected :code :invalid-params}
               (refusal {:op :cue-stage/edit :stage-id "s" :param :nope :value 1 :command-id "c"} 3 operator)))
        (is (= {:status :rejected :code :stage-unavailable}
               (refusal {:op :param/set :stage-id "gone" :param :level :value 0.5} 4 nil)))
        (is (= {:status :rejected :code :invalid-command}
               (refusal {:op :param/set :stage-id "s" :instance-id "0:1" :param :level :value 0.5} 5 nil)))
        (is (= {:status :rejected :code :stage-exists}
               (refusal {:op :cue-stage/open :cue-id :ember :stage-id "s"} 6 nil)))
        (is (= {:status :rejected :code :unknown-cue}
               (refusal {:op :cue-stage/open :cue-id :missing :stage-id "t"} 7 nil)))
        (is (= {:status :rejected :code :invalid-launch}
               (refusal {:op :cue-stage/open :cue-id :ember :stage-id "t" :launch {:mode :held}} 8 nil)))
        (is (= {:status :rejected :code :unknown-stage}
               (refusal {:op :cue-stage/launch :stage-id "t"} 9 nil)))
        (is (= {:status :rejected :code :unknown-stage}
               (refusal {:op :cue-stage/cancel :stage-id "t"} 10 nil)))
        (is (= (select-keys before [:revision :instances :control :registry])
               (select-keys (model runtime) [:revision :instances :control :registry])))
        (is (empty? (rest (:nodes (history runtime))))))
      (testing "a direct parameter set on the stage is an external write"
        (is (= :accepted (:status (apply-at! runtime {:op :param/set :stage-id "s" :param :level :value 0.45} 20))))
        (is (= 0.45 (get-in (staged runtime "s") [:params :level])))
        (is (empty? (:instances (model runtime)))))
      (testing "a restart ends every stage"
        (is (= :accepted (:status (apply-at! runtime {:op :show/restart} 30))))
        (is (empty? (show/staged-cues runtime))))
      (finally (show/stop! runtime)))))

(deftest saving-drops-stages-and-their-history-steps
  (let [runtime (show/create-runtime {:workers 1 :workspace-options nil})]
    (try
      (apply-at! runtime {:op :cue-stage/open :cue-id :ember :stage-id "s"} 0)
      (apply-at! runtime {:op :cue-stage/edit :stage-id "s" :param :level :value 0.3 :command-id "e"} 1 operator)
      (let [saved (show/export-show runtime)]
        (is (nil? (get-in saved [:model :control :tolglow.cue-staging/stages])))
        (is (= :accepted (:status (let [reply (show/submit! runtime :import {:source (show/export-show-source runtime)})]
                                    (show/tick! runtime 20)
                                    (deref reply 1000 ::missing)))))
        (is (empty? (show/staged-cues runtime)))
        (is (empty? (rest (:nodes (history runtime)))) "staged steps are not saved")
        (testing "a reused stage ID starts with no history"
          (apply-at! runtime {:op :cue-stage/open :cue-id :ember :stage-id "s" :params {:level 0.3}} 30)
          (is (empty? (rest (:nodes (history runtime)))))
          (is (= :at-history-root (:code (undo! runtime 40))))))
      (finally (show/stop! runtime)))))

(defn- studio-command [app command]
  (let [response (app {:request-method :post :uri "/api/command"
                       :headers {"host" "localhost" "content-type" "application/json"
                                 "x-tolglow-form" "studio-v1" "accept" "application/transit+json"}
                       :body (ByteArrayInputStream. (.getBytes ^String (json/write-str command) "UTF-8"))})]
    [(:status response) (transit/decode (:body response))]))

(defn- post! [runtime app command time]
  (let [reply (future (studio-command app command))
        deadline (+ (System/nanoTime) 3000000000)]
    (try
      (loop []
        (cond
          (realized? reply) @reply
          (< (System/nanoTime) deadline) (do (show/tick! runtime time) (Thread/sleep 5) (recur))
          :else (throw (ex-info "Studio command timed out" {:command command}))))
      (finally (future-cancel reply)))))

(deftest studio-sends-the-same-staging-commands-with-listener-authority
  (let [runtime (show/create-runtime {:workers 1 :workspace-options nil})
        app (studio/handler runtime)
        local (access/listener-handler app {})
        source {:adapter "web" :session-id "studio-stage"}
        envelope {:epoch 0 :runtime-id (:id runtime) :source source}]
    (try
      (is (= [200 "s"] (let [[status body] (post! runtime local (merge envelope {:op "cue-stage/open" :cue-id "ember"
                                                                                  :stage-id "s" :command-id "open"}) 0)]
                         [status (:stage-id body)])))
      (testing "a bare handler has no operator authority for history"
        (is (= :operator-required
               (:code (second (post! runtime app (merge envelope {:op "cue-stage/edit" :stage-id "s" :param "level"
                                                                  :value 0.35 :command-id "no-op"}) 5))))))
      (let [[status body] (post! runtime local (merge envelope {:op "cue-stage/edit" :stage-id "s" :param "level"
                                                                :value 0.35 :command-id "edit"}) 10)]
        (is (= [200 :accepted 1] [status (:status body) (:history-cursor body)])))
      (is (= 0.35 (get-in (staged runtime "s") [:params :level])))
      (let [entry (get-in (model runtime) [:control :history "edit"])]
        (is (str/starts-with? (:operator-id entry) "studio/"))
        (is (= "Studio" (:actor (last (:nodes (history runtime)))))))
      (let [[status body] (post! runtime local (merge envelope {:op "cue-stage/launch" :stage-id "s" :command-id "go"}) 20)]
        (is (= [200 :accepted "s"] [status (:status body) (:stage-id body)]))
        (is (= 0.35 (level runtime (:instance-id body)))))
      (finally (show/stop! runtime)))))
