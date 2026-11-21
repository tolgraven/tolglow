(ns tolglow.integrations.ableton-link
  "Receive tempo, beat and optional shared transport from a local Carabiner daemon."
  (:require [clojure.edn :as edn]
            [clojure.string :as str])
  (:import [java.io BufferedInputStream ByteArrayOutputStream]
           [java.net InetSocketAddress Socket SocketTimeoutException]
           [java.nio.charset StandardCharsets]))

(defn- receive-status! [line config now! emit!]
  (when (str/starts-with? line "status ")
    (let [{:keys [beat bpm peers playing]} (edn/read-string (subs line 7))
          follow? (:follow-transport? config)
          source (:source-id config)
          at (now!)]
      (when (and (integer? peers) (not (neg? peers)))
        (cond
          (zero? peers) (emit! {:type :link/lost :source-id source :time-ms at})
          (and (number? beat) (Double/isFinite (double beat)) (<= 0 beat)
               (number? bpm) (< 0 bpm 1000) (or (not follow?) (boolean? playing)))
          (emit! {:type :link/status :source-id source :time-ms at :beat beat :bpm bpm
                  :peers peers :playing? (if follow? playing true)}))))))

(defn transport [config now!]
  {:open!
   (fn [emit!]
     (let [socket (Socket.)
           closed? (atom false)
           send! (fn [command]
                   (let [out (.getOutputStream socket)]
                     (.write out (.getBytes (str command "\n") StandardCharsets/US_ASCII))
                     (.flush out)))]
       (try
         (.connect socket (InetSocketAddress. "127.0.0.1" (int (:port config))) 2000)
         (.setSoTimeout socket 250)
         (when (:follow-transport? config) (send! "enable-start-stop-sync"))
         (send! "status")
         (let [worker (Thread.
                       (fn []
                         (try
                           (with-open [in (BufferedInputStream. (.getInputStream socket))]
                             (let [line (ByteArrayOutputStream.)]
                               (loop [last-probe (System/nanoTime)]
                                 (when-not @closed?
                                   (let [b (try (.read in) (catch SocketTimeoutException _ ::idle))
                                         now (System/nanoTime)
                                         probe? (> (- now last-probe) 500000000)]
                                     (cond
                                       (= b -1) (throw (java.io.EOFException. "Link connection ended"))
                                       (= b ::idle) nil
                                       (= b 10) (do (receive-status! (str/trim (.toString line StandardCharsets/UTF_8))
                                                                     config now! emit!)
                                                    (.reset line))
                                       (< (.size line) 8192) (.write line (int b))
                                       :else (throw (ex-info "Oversize Link response" {})))
                                     ;; Carabiner pushes transport edges; occasional status keeps the
                                     ;; selected clock's freshness and phase current at steady tempo.
                                     (when probe? (send! "status"))
                                     (recur (if probe? now last-probe)))))))
                           (catch Exception _
                             (when-not @closed? (emit! {:type :link/lost :source-id (:source-id config) :time-ms (now!)})))
                           (finally (.close socket)))) "tolglow-ableton-link")]
           (.setDaemon worker true)
           (.start worker)
           {:socket socket :worker worker :closed? closed?})
         (catch Exception e (.close socket) (throw e)))))
   :close! (fn [{:keys [^Socket socket ^Thread worker closed?]}]
             (reset! closed? true) (.close socket) (.join worker 1000))})
