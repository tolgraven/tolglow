(ns tolglow.qa.template-groups-browser
  "Run the optimized Studio against the preview-only demo show and exercise
  template groups (#812) in headless Chrome. Phase studio: launch a template
  group from the Template groups row, size a placement's card in Arrange and
  save a running macro as a template group. Phase repl: the REPL launches a
  second placement and resizes a card, and the open board follows; Studio
  then stops both placements. Phase changed: a REPL edit marks running
  placements Changed, and Studio relaunches one. No device output. Usage:
  lein with-profile +dev run -m tolglow.qa.template-groups-browser [directory] [port]"
  (:require [clojure.java.io :as io]
            [clojure.java.shell :as shell]
            [tolglow.application :as app]
            [tolglow.demo :as demo]
            [tolglow.show :as show]
            [tolglow.template-groups :as template-groups]))

(defn- accepted! [ack what]
  (when-not (= :accepted (:status ack)) (throw (ex-info (str "Could not " what) {:ack ack})))
  ack)

(defn- node! [& args]
  (let [{:keys [exit out err]} (apply shell/sh "node" "dev/studio/template-groups-browser-qa.mjs" args)]
    (print out) (flush)
    (when-not (zero? exit)
      (binding [*out* *err*] (print err) (flush))
      (throw (ex-info "Template groups browser check failed" {:phase (last args) :error err})))))

(def verse-look
  {:id :verse-look :name "Verse look"
   :roles [{:id :wash :name "Washes" :selector {:fixture-ids [:head-1 :head-2]}}
           {:id :beams :name "Beams" :selector {:fixture-ids [:head-3]}}]
   :members [{:id :fill :cue-id :ember :role :wash}
             {:id :pulse :cue-id :ice :role :beams}
             {:id :spot :cue-id :spot}]})

(defn -main [& [directory port]]
  (let [port (Long/parseLong (or port "19840"))
        host (app/start! {:init {:components [:show] :modules [:web-server]}
                          :show {:model (demo/model) :workspace-options nil :recovery false}
                          :web-server {:host "127.0.0.1" :port port}})
        origin (str "http://127.0.0.1:" port)
        directory (or directory "target/template-groups-browser")
        repl! (fn [type payload] (accepted! @(show/submit! host type payload) (pr-str [type payload])))
        group #(template-groups/lookup (:model @(:state host)) %)]
    (try
      (.mkdirs (io/file directory))
      (repl! :template-group verse-look)
      (repl! :composition {:id :opening :mode :macro :name "Opening"
                           :slots [{:id :a :cue-id :moon} {:id :b :cue-id :sage}]})
      (repl! :composition-run {:id :opening})
      (node! origin directory "studio")
      (println "Studio saved cards:" (:cards (group :verse-look)) "and template" (select-keys (group :opening) [:id :members]))
      ;; REPL -> Studio: a second placement on other fixtures and a card size.
      (repl! :template-group-launch {:id :verse-look :placement :left
                                     :bindings {:wash {:fixture-ids [:head-5]} :beams {:fixture-ids [:head-6]}}})
      (repl! :template-group-cards {:id :verse-look :resize {:slot-id :spot :size :medium}})
      (node! origin directory "repl")
      (println "After Studio stop:" (mapv :placement (:placements (first (filter #(= :verse-look (:id %))
                                                                                  (:template-groups (show/snapshot host)))))))
      ;; REPL edit -> Studio: both placements show Changed; Studio relaunches one.
      (repl! :template-group-launch {:id :verse-look})
      (repl! :template-group-launch {:id :verse-look :placement :left
                                     :bindings {:wash {:fixture-ids [:head-5]} :beams {:fixture-ids [:head-6]}}})
      (repl! :template-group (update verse-look :members pop))
      (node! origin directory "changed")
      (finally (app/stop! host) (shutdown-agents)))))
