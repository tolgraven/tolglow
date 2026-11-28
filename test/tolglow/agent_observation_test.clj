(ns tolglow.agent-observation-test
  "tolglow-agent-obs/1 (#849) through the real show owner: the R1o red show on
  the 2023 gig rig, ticked by `show/tick!` with a capture sink. Observations
  are read with `show/agent-observation` on the test thread from committed
  state, encoded to wire JSON and parsed back before they are checked."
  (:require [clojure.data.json :as json]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [tolglow.agent-observation :as ao]
            [tolglow.agents :as agents]
            [tolglow.automation.red-show :as red-show]
            [tolglow.json-schema :as schema]
            [tolglow.show :as show]
            [tolglow.studio :as studio])
  (:import [java.util.concurrent CountDownLatch TimeUnit]))

(def ^:private frame-ms 125.0)

(def ^:private push {:adapter :push :session-id "push-3"})

(def ^:private observation-schema (delay (schema/load-schema "schemas/agent-observation-1.schema.json")))

(defn- wire
  "What a consumer receives: the envelope encoded and parsed back."
  [envelope]
  (json/read-str (ao/encode envelope)))

(defn- schema-errors [envelope]
  (schema/errors @observation-schema (wire envelope)))

(defn- runtime
  ([] (runtime {}))
  ([options]
   (show/create-runtime (merge {:model (red-show/model) :workspace-options nil
                                :sink {:send! (fn [_ _])}}
                               options))))

(defn- submit! [rt t type payload]
  (let [reply (show/submit! rt type payload)]
    (show/tick! rt t)
    (deref reply 2000 ::missing)))

(defn- start-auto! [rt]
  (show/tick! rt 0.0)
  (is (= :accepted (:status (submit! rt frame-ms :auto (red-show/auto-config 217))))))

(defn- ticks! [rt from n]
  (doseq [i (range from (+ from n))] (show/tick! rt (* i frame-ms))))

(deftest snapshot-then-deltas-reconstruct-every-committed-state
  (let [rt (runtime) frame (atom 31)]
    (try
      (start-auto! rt)
      (ticks! rt 2 30)
      (doseq [profile [:compact :tick :full]]
        (testing (str profile)
          (let [first-snap (show/agent-observation rt {:profile profile})]
            (is (= "snapshot" (get first-snap "kind")))
            (is (= ["start"] (get first-snap "wake")))
            (is (empty? (schema-errors first-snap)) (pr-str (take 5 (schema-errors first-snap))))
            (loop [i 0 held (ao/sections-of (wire first-snap)) token (get first-snap "token") wakes #{}]
              (if (= i 24)
                (testing "a bar crossing woke at least one delta"
                  (is (contains? wakes "grid:bar")))
                (do
                  (show/tick! rt (* (swap! frame inc) frame-ms))
                  (let [delta (wire (show/agent-observation rt {:profile profile :base token}))
                        rebuilt (ao/apply-delta held delta)
                        fresh (wire (show/agent-observation rt {:profile profile}))]
                    (is (= "delta" (get delta "kind")))
                    (is (= token (get delta "base")))
                    (is (empty? (schema/errors @observation-schema delta)) (pr-str (take 5 (schema/errors @observation-schema delta))))
                    (testing "the reconstruction equals a snapshot of the same committed state"
                      (is (= (ao/sections-of fresh) rebuilt))
                      (is (= (get fresh "digest") (get delta "digest")))
                      (when-not (= :tick profile)
                        (is (= (get delta "digest") (ao/content-digest rebuilt)))))
                    (recur (inc i) rebuilt (get delta "token") (into wakes (get delta "wake"))))))))))
      (finally (show/stop! rt)))))

(deftest snapshots-and-deltas-stay-within-the-profile-caps
  (let [rt (runtime)]
    (try
      (start-auto! rt)
      (ticks! rt 2 40)
      (doseq [profile [:compact :full]]
        (let [text (ao/encode (show/agent-observation rt {:profile profile}))]
          (is (<= (ao/estimate-tokens text) (ao/caps profile)) (str profile " " (ao/estimate-tokens text)))))
      (let [base (show/agent-observation rt {:profile :tick})
            deltas (for [i (range 42 74)]
                     (do (show/tick! rt (* i frame-ms))
                         (show/agent-observation rt {:profile :tick :base (get base "token")})))]
        (doseq [d deltas :when (= "delta" (get d "kind"))]
          (is (<= (ao/estimate-tokens (ao/encode d)) (ao/caps :tick)))))
      (testing "a held block is sent as its digest alone"
        (let [full (show/agent-observation rt {:profile :compact})
              lean (show/agent-observation rt {:profile :compact :have (vals (get full "blocks"))})]
          (is (every? #(contains? full %) ["rig" "catalog" "goals"]))
          (is (not-any? #(contains? lean %) ["rig" "catalog" "goals"]))
          (is (= (get full "blocks") (get lean "blocks")))
          (is (empty? (schema-errors lean)))))
      (finally (show/stop! rt)))))

(defn- digests
  "The observation digest at every frame of the seeded take."
  [options]
  (let [rt (runtime options)]
    (try
      (start-auto! rt)
      (vec (for [i (range 2 66)]
             (do (show/tick! rt (* i frame-ms))
                 (get (show/agent-observation rt {:profile :full}) "digest"))))
      (finally (show/stop! rt)))))

(deftest the-same-take-gives-the-same-digests
  (let [one (digests {:workers 1})]
    (is (= 64 (count (distinct one))) "the observed state changes on every frame")
    (is (= one (digests {:workers 1})) "replayed")
    (is (= one (digests {:workers 2})) "two effect workers")))

(deftest a-parameter-edit-addresses-one-cell
  (let [rt (runtime)]
    (try
      (show/tick! rt 0.0)
      (submit! rt frame-ms :command {:op :transport/set :source push :transport {:playing? false}})
      (let [launched (submit! rt (* 2 frame-ms) :command {:op :cue/launch :cue-id :wash-color :source push
                                                         :launch {:mode :persistent}})
            id (:instance-id launched)
            _ (ticks! rt 3 4)
            before (show/agent-observation rt {:profile :compact})
            _ (submit! rt (* 8 frame-ms) :command {:op :param/set :source push :instance-id id :param :level :value 0.45})
            delta (show/agent-observation rt {:profile :compact :base (get before "token")})
            run-ops (filter #(= "run" (first (second %))) (get delta "changes"))]
        (is (string? id))
        (is (= "delta" (get delta "kind")))
        (is (= [["set" ["run" "rows" id 9 "level"] 0.45]] run-ops))
        (testing "the human surface is named without its session"
          (is (= "push" (get-in before ["run" "rows" id 8])))))
      (finally (show/stop! rt)))))

(deftest unknown-evicted-or-foreign-bases-yield-snapshots
  (let [rt (runtime)]
    (try
      (start-auto! rt)
      (ticks! rt 2 4)
      (let [snap (show/agent-observation rt {})]
        (is (= ["resync"] (get (show/agent-observation rt {:base "o-unknown"}) "wake")))
        (is (= ["profile"] (get (show/agent-observation rt {:profile :full :base (get snap "token")}) "wake")))
        (testing "a slow consumer's delta comes from the newest committed state"
          (ticks! rt 6 12)
          (let [delta (show/agent-observation rt {:base (get snap "token")})]
            (is (= "delta" (get delta "kind")))
            (is (= (get-in @(:state rt) [:model :frame-number]) (get-in delta ["id" "frame"]))))))
      (let [store (ao/create-store {:max-entries 2})
            input {:runtime-id "r" :frame-rate 40 :state @(:state rt)}
            first-snap (ao/observe store input {})
            later #(assoc-in input [:state :model :frame-number] %)]
        (ao/observe store (later 1001) {})
        (ao/observe store (later 1002) {})
        (is (= ["resync"] (get (ao/observe store (later 1003) {:base (get first-snap "token")}) "wake")) "evicted")
        (let [current (ao/observe store (later 1004) {})]
          (is (= ["epoch"] (get (ao/observe store (assoc-in (later 1005) [:state :model :epoch] 99)
                                            {:base (get current "token")}) "wake")))))
      (finally (show/stop! rt)))))

(deftest projection-runs-off-the-frame-owner
  (let [rt (runtime)
        entered (CountDownLatch. 1)
        release (CountDownLatch. 1)
        clock ao/clock-section]
    (try
      (start-auto! rt)
      (ticks! rt 2 4)
      (let [frame-before (get-in @(:state rt) [:model :frame-number])
            observation (with-redefs [ao/clock-section (fn [model]
                                                         (.countDown entered)
                                                         (.await release 10 TimeUnit/SECONDS)
                                                         (clock model))]
                          (let [reader (future (show/agent-observation rt {:profile :tick}))]
                            (is (.await entered 10 TimeUnit/SECONDS))
                            (testing "the owner keeps committing while the reader is blocked"
                              (ticks! rt 6 20)
                              (is (= (+ frame-before 20) (get-in @(:state rt) [:model :frame-number]))))
                            (.countDown release)
                            (deref reader 10000 ::timeout)))]
        (testing "the reader saw the one committed state it captured, unchanged by it"
          (is (= frame-before (get-in observation ["id" "frame"])))
          (is (= (+ frame-before 20) (get-in @(:state rt) [:model :frame-number])))))
      (finally (.countDown release) (show/stop! rt)))))

(deftest observing-never-changes-show-state
  (let [rt (runtime)]
    (try
      (start-auto! rt)
      (ticks! rt 2 10)
      (let [before @(:state rt)]
        (dotimes [_ 3]
          (doseq [p [:tick :compact :full]] (show/agent-observation rt {:profile p})))
        (show/agent-observation-page rt ["rig" "fixtures"] {:offset 0 :limit 4})
        (is (identical? before @(:state rt))))
      (finally (show/stop! rt)))))

(def ^:private secret "sk-test-SENTINEL-4f1a9c")

(deftest secrets-never-reach-an-observation
  (let [model (-> (red-show/model)
                  (assoc-in [:settings :integrations :ableset] {:api-key secret :token secret})
                  (assoc-in [:settings :agent] {:credential secret})
                  (update-in [:rig :fixtures 0] assoc :name (str "Mover " secret) :api-key secret)
                  (update-in [:registry :effects :wash-color] assoc :name (str "Wash " secret) :notes secret))
        rt (runtime {:model model})]
    (try
      (agents/configure! (show/agent-manager rt) {:provider :openai :model "chosen-model" :credential secret})
      (start-auto! rt)
      (ticks! rt 2 12)
      (let [texts (concat (for [p [:tick :compact :full]] (ao/encode (show/agent-observation rt {:profile p})))
                          [(ao/canonical-json (show/agent-observation-page rt ["rig" "fixtures"] {}))
                           (ao/canonical-json (show/agent-observation-page rt ["catalog" "effects"] {}))])]
        (is (= 5 (count texts)))
        (doseq [text texts]
          (is (not (str/includes? text secret)))
          (is (not (str/includes? text "SENTINEL")))
          (is (not (re-find #"(?i)\"[^\"]*(credential|api[-_]?key|secret|password)[^\"]*\":" text)))))
      (finally (show/stop! rt)))))

(deftest paged-tables-say-so-and-inspect-reads-the-rest
  (let [rows (into (sorted-map) (map (fn [i] [(format "f-%04d" i) ["pixel" [] [0.0 0.0 0.0] [0.0 0.0 0.0]]])) (range 400))
        rig {:fixtures (mapv (fn [i] {:id (format "f-%04d" i) :position [(double (mod i 20)) 0.0 (double (quot i 20))]
                                      :channels {:red 1 :green 2 :blue 3}}) (range 400))}
        input {:runtime-id "r" :frame-rate 40
               :state {:model (assoc (red-show/model) :rig rig) :output {}}}
        doc (ao/document (atom {}) input :compact)
        table (get-in doc [:slow "rig" "fixtures"])]
    (is (= 400 (count rows)))
    (is (true? (get table "more")))
    (is (= 400 (get table "total")))
    (is (< 0 (count (get table "rows")) 400))
    (is (<= (ao/estimate-tokens (get-in doc [:slow "rig"])) (ao/section-budget :compact "rig")))
    (let [page (ao/inspect input ["rig" "fixtures"] {:offset 390 :limit 20})]
      (is (= 400 (get page "total")))
      (is (= (map #(format "f-%04d" %) (range 390 400)) (keys (get page "rows")))))))

(deftest the-validator-rejects-what-the-schema-forbids
  (let [snap (wire (let [rt (runtime)]
                     (try (start-auto! rt) (ticks! rt 2 3) (show/agent-observation rt {})
                          (finally (show/stop! rt)))))]
    (is (empty? (schema/errors @observation-schema snap)))
    (is (seq (schema/errors @observation-schema (dissoc snap "clock"))))
    (is (seq (schema/errors @observation-schema (assoc snap "kind" "full"))))
    (is (seq (schema/errors @observation-schema (assoc-in snap ["look" "meters" "lit"] -1))))
    (is (seq (schema/errors @observation-schema (assoc snap "kind" "delta"))) "a delta needs base and changes")))

(deftest studio-serves-the-same-projection-as-the-repl
  (let [rt (runtime)]
    (try
      (start-auto! rt)
      (ticks! rt 2 10)
      (let [app (studio/handler rt)
            get! (fn [uri query] (app {:request-method :get :uri uri :query-string query
                                       :headers {"host" "localhost" "accept" "application/json"}}))
            response (get! "/api/agent/observation" "profile=compact")
            body (json/read-str (:body response))
            repl (wire (show/agent-observation rt {:profile :compact}))]
        (is (= 200 (:status response)))
        (is (str/starts-with? (get-in response [:headers "Content-Type"]) "application/json"))
        (is (empty? (schema/errors @observation-schema body)))
        (testing "one committed state, one observation, whichever side reads it"
          (is (= (get repl "digest") (get body "digest")))
          (is (= (ao/sections-of repl) (ao/sections-of body))))
        (show/tick! rt (* 12 frame-ms))
        (let [delta (json/read-str (:body (get! "/api/agent/observation" (str "profile=compact&base=" (get repl "token")))))]
          (is (= "delta" (get delta "kind")) "a REPL token is a base for Studio")
          (is (= (ao/sections-of (wire (show/agent-observation rt {:profile :compact})))
                 (ao/apply-delta (ao/sections-of repl) delta)))
          (is (= (get (show/agent-observation rt {:profile :compact}) "digest") (get delta "digest"))))
        (let [have (str/join "," (vals (get body "blocks")))
              lean (json/read-str (:body (get! "/api/agent/observation" (str "profile=compact&have=" have))))]
          (is (not-any? #(contains? lean %) ["rig" "catalog" "goals"])))
        (let [page (json/read-str (:body (get! "/api/agent/observation/page" "path=rig.fixtures&offset=2&limit=3")))]
          (is (= (get (show/agent-observation-page rt ["rig" "fixtures"] {:offset 2 :limit 3}) "rows") (get page "rows")))
          (is (= 3 (count (get page "rows")))))
        (testing "malformed parameters are refused"
          (is (= 400 (:status (get! "/api/agent/observation" "profile=verbose"))))
          (is (= 400 (:status (get! "/api/agent/observation/page" "path=status"))))
          (is (thrown? clojure.lang.ExceptionInfo (show/agent-observation rt {:profile :verbose})))
          (is (thrown? clojure.lang.ExceptionInfo (show/agent-observation-page rt ["status"]))))
        (is (= 405 (:status (app {:request-method :post :uri "/api/agent/observation"
                                  :headers {"host" "localhost" "origin" "http://localhost"}})))
            "read-only"))
      (finally (show/stop! rt)))))
