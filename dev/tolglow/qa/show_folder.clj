(ns tolglow.qa.show-folder
  "Run the optimized Studio against three Mac-style show folders and check the
  folder identity note and the checkpoint from before the update (#742) in a
  real browser. Folders A and B share a temporary recovery base holding an
  older shared checkpoint (blackout on); C has its own base. Each show starts
  through `tolglow.desktop.launch` as a Mac window would, binds to 127.0.0.1
  and has no output sink. Nothing outside the temporary directories is read or
  written."
  (:require [clojure.java.io :as io]
            [tolglow.application :as app]
            [tolglow.desktop.launch :as launch]
            [tolglow.show :as show]
            [tolglow.show.recovery :as recovery])
  (:import [java.lang ProcessHandle]
           [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]
           [java.util.concurrent TimeUnit]))

(defn- temp-dir [prefix]
  (.toFile (Files/createTempDirectory prefix (make-array FileAttribute 0))))

(defn- write-legacy!
  "Write a real show's checkpoint, with blackout on, where desktop shows
  autosaved before per-folder recovery."
  [base]
  (let [runtime (show/start! {:recovery false})]
    (try
      (when-not (= :accepted (:status @(show/submit! runtime {:op :blackout/set :value true})))
        (throw (ex-info "Could not prepare the older checkpoint" {})))
      (recovery/write-source! (io/file (recovery/show-directory base :main) "recovery.transit")
                              (show/export-show-source runtime))
      (finally (show/stop! runtime)))))

(defn- folder-show
  "Start a show for a new folder under `base` as the Mac app would."
  [base port]
  (let [folder (temp-dir "tolglow-qa-show-folder")
        claim (launch/claim-folder-recovery! folder base)
        cfg (launch/desktop-config {:init {:components [:show] :modules [:web-server]}
                                    :show {:recovery {:interval-ms 1000} :workspace-options nil}
                                    :web-server {:host "127.0.0.1" :port port}}
                                   (merge {:devices? true} (launch/claim-options claim)))]
    {:claim claim :runtime (app/start! cfg)}))

(defn- check-browser! [origins output]
  (let [process (-> (ProcessBuilder. (into ["node" "dev/studio/show-folder-qa.mjs" output] origins))
                    (.inheritIO) (.start))]
    (try
      (when-not (.waitFor process 240 TimeUnit/SECONDS)
        (throw (ex-info "Show folder checks timed out" {})))
      (when-not (zero? (.exitValue process))
        (throw (ex-info "Show folder checks failed" {:exit (.exitValue process)})))
      (finally
        (when (.isAlive process)
          (with-open [children (.descendants process)]
            (doseq [child (.toArray children)] (.destroyForcibly ^ProcessHandle child)))
          (.destroyForcibly process))))))

(defn -main
  "Arguments: an output directory for screenshots (default target/show-folder)
  and an optional first loopback port; the three shows use it and the next two
  (default: any free ports)."
  [& [output port]]
  (let [shared (doto (temp-dir "tolglow-qa-recovery") write-legacy!)
        separate (doto (temp-dir "tolglow-qa-recovery") write-legacy!)
        first-port (if port (parse-long port) 0)
        shows (atom [])]
    (try
      (doseq [[base offset] [[shared 0] [shared 1] [separate 2]]]
        (swap! shows conj (folder-show base (if (pos? first-port) (+ first-port offset) 0))))
      (check-browser! (mapv #(str "http://127.0.0.1:" (get-in (:runtime %) [:application :modules :web-server :port])) @shows)
                      (or output "target/show-folder"))
      (finally
        (doseq [{:keys [runtime claim]} @shows]
          (try (app/stop! runtime) (finally ((:release! claim)))))
        (shutdown-agents)))))
