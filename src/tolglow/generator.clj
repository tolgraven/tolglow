(ns tolglow.generator
  "Tier 2 effect authoring (#855): run agent-written generator source in a
  separate worker JVM whose only product is effect DSL data, then send that
  data through the ordinary registry validation and compiler.

  The evaluator never runs in this process. `open` starts a worker
  (`tolglow.generator.worker`) with its own bounded heap; `generate!` checks
  the session's capability and origin, sends one request, waits at most the
  deadline, kills and replaces the worker when it does not answer in time, and
  validates whatever comes back. Nothing is published: the caller publishes a
  :valid result through the registry like any other definition. Call it from
  an agent or command thread, never from the lighting frame path.

      (def sandbox (generator/open {}))
      (generator/generate! sandbox {:scopes #{:author-code} :origin :local-session}
                           {:language :clojure :seed 7
                            :source \"(fx/wash :agent/ember {:color [1 0.3 0]})\"})
      ;; => {:status :valid :definition {...} :digest \"…\" :provenance {...}}
      (generator/close! sandbox)"
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [tolglow.compiler :as compiler]
            [tolglow.generator.limits :as limits]
            [tolglow.registry :as registry])
  (:import [java.io BufferedReader InputStreamReader OutputStreamWriter Writer]
           [java.nio.charset StandardCharsets]
           [java.util.concurrent LinkedBlockingQueue TimeUnit]))

(def code-origins
  "Session origins whose generator source may reach the evaluator once the
  operator has granted :author-code. Imported shows, shared effects and
  downloaded or followed profiles never may (interface.md section 4)."
  #{:local-session :remote-backend})

(def languages #{:clojure})

(def default-startup-ms
  "How long `generate!` waits for a fresh worker to load before refusing. The
  worker compiles the effect builders from source on a development classpath."
  120000)

(defn- refused [stage errors & {:as extra}]
  (merge {:status :refused :stage stage :errors (vec errors)} extra))

(defn- error [path message] {:path path :message message})

(defn worker-command
  "The argv that starts a worker on `classpath` (default: this JVM's)."
  [{:keys [heap-mb classpath java]}]
  [(or java (str (io/file (System/getProperty "java.home") "bin" "java")))
   (str "-Xmx" (or heap-mb (:heap-mb limits/defaults)) "m")
   "-Xss4m" "-XX:+UseSerialGC" "-XX:TieredStopAtLevel=1" "-Djava.awt.headless=true"
   "-cp" (or classpath (System/getProperty "java.class.path"))
   "clojure.main" "-m" "tolglow.generator.worker"])

(defn- daemon! [name f]
  (doto (Thread. ^Runnable f ^String name) (.setDaemon true) (.start)))

(defn- spawn [opts]
  (let [process (.start (ProcessBuilder. ^java.util.List (worker-command opts)))
        lines (LinkedBlockingQueue.)
        stderr (StringBuilder.)]
    (daemon! "tolglow-generator-stdout"
             #(with-open [in (BufferedReader. (InputStreamReader. (.getInputStream process) StandardCharsets/UTF_8))]
                (try
                  (loop []
                    (when-let [line (.readLine in)] (.put lines line) (recur)))
                  (catch java.io.IOException _)
                  (finally (.put lines ::eof)))))
    (daemon! "tolglow-generator-stderr"
             #(with-open [in (BufferedReader. (InputStreamReader. (.getErrorStream process) StandardCharsets/UTF_8))]
                (try
                  (loop []
                    (when-let [line (.readLine in)]
                      (locking stderr
                        (.append stderr line) (.append stderr "\n")
                        (when (> (.length stderr) 4000) (.delete stderr 0 (- (.length stderr) 4000))))
                      (recur)))
                  (catch java.io.IOException _))))
    {:process process :lines lines :stderr stderr :ready? false
     :writer (OutputStreamWriter. (.getOutputStream process) StandardCharsets/UTF_8)}))

(defn- stop-worker! [{:keys [^Process process]}]
  (when process
    (.destroyForcibly process)
    (.waitFor process 5 TimeUnit/SECONDS)))

(defn open
  "Start a generator sandbox. Options: :limits (see tolglow.generator.limits),
  :startup-ms, and :classpath/:java for the worker command. The worker starts
  loading at once so the first request finds it warm."
  ([] (open {}))
  ([{:keys [limits startup-ms] :as opts}]
   (let [limits (limits/limits (or limits {}))
         opts (assoc opts :limits limits :heap-mb (:heap-mb limits) :startup-ms (or startup-ms default-startup-ms))]
     {:opts opts :lock (Object.) :worker (atom (spawn opts)) :requests (atom 0)})))

(defn close!
  "Stop the worker process. A closed sandbox refuses further requests."
  [{:keys [worker lock]}]
  (locking lock
    (stop-worker! @worker)
    (reset! worker nil)))

(defn requests
  "How many requests reached a worker process."
  [sandbox]
  @(:requests sandbox))

(defn worker-pid
  "Process ID of the current worker, or nil."
  [{:keys [worker]}]
  (some-> @worker ^Process (:process) .pid))

(defn- replace-worker! [{:keys [worker opts]}]
  (stop-worker! @worker)
  (reset! worker (spawn opts)))

(defn- await-ready! [{:keys [worker opts] :as sandbox}]
  (let [{:keys [^Process process ^LinkedBlockingQueue lines ready?] :as w} @worker]
    (cond
      ready? nil
      :else
      (let [line (.poll lines (:startup-ms opts) TimeUnit/MILLISECONDS)]
        (if (= line (pr-str {:ready true}))
          (do (swap! worker assoc :ready? true) nil)
          (let [diagnostic (locking (:stderr w) (str (:stderr w)))]
            (replace-worker! sandbox)
            (refused :worker [(error [] (str "Generator worker did not start"
                                             (when-not (.isAlive process) (str " (exit " (.exitValue process) ")"))
                                             (when (seq diagnostic) (str ": " (str/trim diagnostic)))))])))))))

(defn- exchange!
  "Send one request to a ready worker and wait at most the deadline."
  [{:keys [worker opts requests] :as sandbox} request]
  (let [{:keys [^Writer writer ^LinkedBlockingQueue lines ^Process process]} @worker
        deadline-ms (get-in request [:limits :deadline-ms] (get-in opts [:limits :deadline-ms]))
        start (System/nanoTime)]
    (swap! requests inc)
    (try
      (.write writer (str (pr-str request) "\n"))
      (.flush writer)
      (catch java.io.IOException _))
    (let [line (.poll lines deadline-ms TimeUnit/MILLISECONDS)
          elapsed-ms (/ (- (System/nanoTime) start) 1e6)]
      (cond
        (nil? line)
        (do (replace-worker! sandbox)
            (refused :sandbox [(error [] (str "Generator did not finish within " deadline-ms
                                              " ms; its worker process was stopped"))]
                     :limit :deadline :elapsed-ms elapsed-ms))
        (= ::eof line)
        (do (.waitFor process 5 TimeUnit/SECONDS)
            (replace-worker! sandbox)
            (refused :sandbox [(error [] (str "Generator worker exited"
                                              (when-not (.isAlive process) (str " with code " (.exitValue process)))))]
                     :limit :crash :elapsed-ms elapsed-ms))
        :else
        (let [answer (edn/read-string line)]
          (when (:fatal answer) (replace-worker! sandbox))
          (assoc answer :elapsed-ms elapsed-ms))))))

(defn- validate [text limits]
  (let [value (try (edn/read-string {:default (fn [tag _] (throw (ex-info (str "Unknown tag #" tag) {})))} text)
                   (catch Exception e (ex-info (str "Unreadable generator result: " (ex-message e)) {})))]
    (cond
      (instance? Exception value) (refused :result [(error [] (ex-message value))])
      (not (map? value)) (refused :result [(error [] "A generator returns one effect definition map")])
      :else
      (if-let [problems (limits/portable-errors value limits)]
        (refused :result problems)
        (let [problems (registry/validate-definition :effects value)]
          (if (seq problems)
            (refused :registry problems)
            (try
              (compiler/compile-plan value)
              {:status :valid :definition value}
              (catch clojure.lang.ExceptionInfo e
                (refused :compile (or (seq (:errors (ex-data e))) [(error [] (ex-message e))]))))))))))

(defn- gate [{:keys [scopes origin]} {:keys [language source seed]} limits]
  (cond
    (not (contains? (set scopes) :author-code))
    (refused :capability [(error [:scopes] "This session does not hold the author-code capability")])
    (not (code-origins origin))
    (refused :capability [(error [:origin] (str "Generator source from " (pr-str origin)
                                                " never runs; only an operator-enabled local session or remote backend may"))])
    (not (languages language))
    (refused :request [(error [:language] (str "Generator language " (pr-str language)
                                               " is not available; supported: " (pr-str (sort languages))))])
    (not (string? source)) (refused :request [(error [:source] "Generator source must be a string")])
    (> (count source) (:source-chars limits))
    (refused :request [(error [:source] (str "Generator source is limited to " (:source-chars limits) " characters"))])
    (not (int? seed)) (refused :request [(error [:seed] "A generator needs an explicit integer seed")])))

(defn generate!
  "Run one generator request {:language :clojure :source s :seed n :limits {}}
  for `session` {:scopes #{...} :origin kw}. Returns {:status :valid
  :definition d :digest hex :provenance {...}} or {:status :refused :stage s
  :errors [{:path :message}]}; :stage is :capability, :request, :worker,
  :sandbox (with :limit when a bound stopped it), :result, :registry or
  :compile. Requests are serialized on the sandbox's single worker."
  [{:keys [opts lock worker] :as sandbox} session request]
  (let [limits (try (limits/limits (merge (:limits opts) (:limits request)))
                    (catch clojure.lang.ExceptionInfo e e))]
    (if (instance? Exception limits)
      (refused :request (:errors (ex-data limits)))
      (or (gate session request limits)
          (locking lock
            (if (nil? @worker)
              (refused :worker [(error [] "Generator sandbox is closed")])
              (or (await-ready! sandbox)
                  (let [answer (exchange! sandbox {:id (inc (requests sandbox)) :source (:source request)
                                                   :seed (:seed request) :limits limits})
                        elapsed (select-keys answer [:elapsed-ms])]
                    (if (= :ok (:status answer))
                      (let [result (validate (:value answer) limits)]
                        (if (= :valid (:status result))
                          (assoc result
                                 :digest (limits/digest (:definition result))
                                 :provenance (merge {:tier :generator :language (:language request)
                                                     :seed (:seed request) :source-digest (limits/digest (:source request))}
                                                    elapsed))
                          (merge result elapsed)))
                      (dissoc answer :id :value :fatal))))))))))
