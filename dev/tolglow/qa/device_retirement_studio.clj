(ns tolglow.qa.device-retirement-studio
  "Captured-output show for the device retirement browser check (#905, #907).
  Lighting packets go to an in-memory capture sink. Two device sessions use
  injected transports and never touch hardware: a Push 2 session
  \"qa-push\" whose display write blocks until released, and a MIDI session
  \"qa-midi\". Only the loopback web server is opened.

  $W/bin/repl run tolglow.qa.device-retirement-studio <port> <work-dir>

  Control files in the work directory, written by
  dev/studio/device-retirement-qa.mjs:
    import     import the show's own source, which retires both sessions
    release    let the blocked display write return
    reconnect  connect qa-push again from the REPL side
    stop       stop the show
  The harness writes state.json after each step: the retirement status, the
  REPL's refusal for connect-device! of qa-push while it is closing, and the
  captured packet count, which keeps rising while the write is blocked."
  (:require [clojure.data.json :as json]
            [tolglow.demo :as demo]
            [tolglow.show :as show])
  (:import [java.io File]))

(def ^:private capture-midi {:open! (fn [_] :midi) :send! (fn [_ _]) :close! (fn [_])})

(defn- block-until!
  "Hold the writer until release, ignoring interrupts as a stuck USB write would."
  [release]
  (loop [interrupted? false]
    (if (realized? release)
      (when interrupted? (.interrupt (Thread/currentThread)))
      (recur (try (deref release 20 nil) interrupted? (catch InterruptedException _ true))))))

(defn- wait-for [directory name]
  (let [file (File. ^File directory ^String name)]
    (loop []
      (when-not (.exists file)
        (Thread/sleep 50)
        (recur)))))

(defn- retirement [runtime]
  (get-in (show/snapshot runtime) [:status :device-retirement]))

(defn- write-state! [directory step runtime packets extra]
  (spit (File. ^File directory "state.json")
        (json/write-str (merge {:step step :packets @packets
                                :retirement (update (retirement runtime) :closing sort)}
                               extra)
                        :value-fn (fn [_ v] (cond (keyword? v) (subs (str v) 1) (set? v) (vec (sort v)) :else v))))
  (println "device-retirement-qa" step) (flush))

(defn -main [& [port work-dir]]
  (let [directory (doto (File. (str work-dir)) (.mkdirs))
        packets (atom 0)
        block? (atom false) entered (promise) release (promise)
        push-options {:session-id "qa-push" :midi capture-midi
                      :display {:open! (constantly :display) :close! (fn [_])
                                :render! (fn [_ _] (when @block? (deliver entered true) (block-until! release)))}}
        ;; The host-supplied sink opener of show/start!: an in-memory capture.
        runtime (show/start! {:model (demo/model) :workspace-options nil :recovery false}
                             (constantly {:send! (fn [_ _] (swap! packets inc)) :description {:type :capture}}))
        web ((requiring-resolve 'tolglow.studio/listen!) runtime {:host "127.0.0.1" :port (Long/parseLong (str port))})]
    (try
      (show/connect-device! runtime :push-2 push-options)
      (show/connect-device! runtime :midi
                            {:session-id "qa-midi" :transport {:open! (constantly :virtual-midi) :send! (fn [& _]) :close! (fn [_])}})
      ;; A running cue, so every frame has light for the capture sink.
      (deref (show/submit! runtime {:op :cue/launch :cue-id :ember :launch {:mode :persistent}
                                    :source {:adapter :repl :session-id "qa-operator"}})
             5000 nil)
      (reset! block? true)
      ;; A changing grand master changes the Push display view, so its next write blocks.
      (loop [level 0.5]
        (when-not (deref entered 100 nil)
          (show/submit! runtime {:op :master/set :value level})
          (recur (if (> level 0.9) 0.5 (+ level 0.05)))))
      (write-state! directory "blocked" runtime packets {})
      (wait-for directory "import")
      (let [before @packets
            receipt (show/submit! runtime :import {:source (show/export-show-source runtime)})]
        (println "device-retirement-qa import" (:status (deref receipt 5000 {:status :pending})))
        ;; The import retires both sessions in the tick that commits it.
        (loop [] (when (seq @(:devices runtime)) (Thread/sleep 20) (recur)))
        (Thread/sleep 500)
        (let [refusal (try (show/connect-device! runtime :push-2 push-options) nil
                           (catch clojure.lang.ExceptionInfo error {:message (ex-message error) :data (ex-data error)}))]
          (write-state! directory "imported" runtime packets {:repl-refusal refusal :packets-before-import before})))
      (wait-for directory "release")
      (let [at-release @packets]
        (deliver release true)
        (loop [] (when (seq (:closing (retirement runtime))) (Thread/sleep 20) (recur)))
        (write-state! directory "released" runtime packets {:packets-at-release at-release}))
      (wait-for directory "reconnect")
      (reset! block? false)
      (show/connect-device! runtime :push-2 push-options)
      (write-state! directory "reconnected" runtime packets {})
      (wait-for directory "stop")
      (write-state! directory "stopping" runtime packets {:connected (vec (sort (keys @(:devices runtime))))})
      (finally
        (deliver release true)
        ((:stop! web))
        (show/stop! runtime)))))
