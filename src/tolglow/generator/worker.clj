(ns tolglow.generator.worker
  "Entry point of the generator worker process (#855). Reads one EDN request
  per line on stdin, evaluates it with tolglow.generator.sandbox and writes one
  EDN answer per line on stdout. The host (tolglow.generator) owns the
  process: it bounds the heap, kills the process at the deadline and starts a
  replacement. Nothing here touches show state, devices or the network."
  (:require [clojure.edn :as edn]
            [tolglow.generator.limits :as limits]
            [tolglow.generator.sandbox :as sandbox])
  (:import [java.io BufferedReader InputStreamReader OutputStreamWriter PrintStream PrintWriter OutputStream]
           [java.nio.charset StandardCharsets]))

(defn- soft-deadline
  "The sandbox stops itself at four fifths of the host deadline, so a clean
  answer normally arrives before the host has to kill the process."
  [deadline-ms]
  (max 1 (quot (* 4 deadline-ms) 5)))

(defn- answer [{:keys [id source seed limits]}]
  (let [limits (limits/limits (or limits {}))
        result (sandbox/evaluate source seed (assoc limits :deadline-ms (soft-deadline (:deadline-ms limits))))]
    (if (= :ok (:status result))
      (let [text (binding [*print-length* nil *print-level* nil *print-meta* false
                           *print-namespace-maps* false *print-dup* false]
                   (pr-str (:value result)))]
        (if (> (count text) (:result-chars limits))
          {:id id :status :refused :stage :result
           :errors [{:path [] :message (str "Printed result exceeds " (:result-chars limits) " characters")}]}
          {:id id :status :ok :value text}))
      (assoc result :id id))))

(defn- handle [line]
  (let [request (try (edn/read-string line) (catch Exception e e))]
    (if (instance? Exception request)
      {:status :refused :stage :request :errors [{:path [] :message (str "Malformed worker request: " (ex-message request))}]}
      (try
        (answer request)
        (catch OutOfMemoryError _
          {:id (:id request) :status :refused :stage :sandbox :limit :memory :fatal true
           :errors [{:path [] :message "Generator exhausted the worker heap"}]})
        (catch Exception e
          {:id (:id request) :status :refused :stage :request :errors [{:path [] :message (ex-message e)}]})))))

(defn -main [& _]
  (let [protocol (PrintWriter. (OutputStreamWriter. System/out StandardCharsets/UTF_8) true)
        input (BufferedReader. (InputStreamReader. System/in StandardCharsets/UTF_8))
        send! (fn [message]
                (.println protocol (binding [*print-length* nil *print-level* nil] (pr-str message)))
                (.flush protocol))]
    ;; Anything else that prints must not corrupt the line protocol.
    (System/setOut (PrintStream. (OutputStream/nullOutputStream)))
    (send! {:ready true})
    (loop []
      (when-let [line (.readLine input)]
        (let [message (handle line)]
          (send! message)
          (if (:fatal message)
            (System/exit 3)
            (recur)))))
    (System/exit 0)))
