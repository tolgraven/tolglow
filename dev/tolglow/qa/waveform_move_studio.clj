(ns tolglow.qa.waveform-move-studio
  "Preview-only show for the waveform move browser check (#468). Two
  activations of one wash run with a sine LFO on the first one's Level. The
  browser harness drives Studio's Destination select; this namespace plays the
  REPL side: on request it attaches a waveform to the destination Studio has
  chosen, so Studio's move is refused, and afterwards it reads the refusal from
  the same projection Studio used. Only the loopback web server is opened, and
  output stays in memory.

  lein run -m tolglow.qa.waveform-move-studio target/waveform-move-qa 19941

  The port may also come from TOLGLOW_QA_PORT."
  (:require [clojure.java.io :as io]
            [clojure.java.shell :as shell]
            [clojure.pprint :as pprint]
            [tolglow.application :as app]
            [tolglow.demo :as demo]
            [tolglow.show :as show]))

(def wash
  {:id :drift-wash :name "Drift wash" :version 1 :categories [:color]
   :outputs {:dimmer {:op :param :key :level}}
   :params {:level {:type :number :name "Level" :default 0.25 :bounds [0 1] :ui/order 0}
            :hue {:type :number :name "Hue" :default 60 :bounds [0 360] :ui/order 1}}})

(defn- send! [host op payload]
  (let [ack (deref (show/submit! host op payload) 10000 nil)]
    (when-not (= :accepted (:status ack)) (throw (ex-info "QA command failed" {:op op :ack ack})))
    ack))

(defn- launch! [host]
  (:instance-id (send! host :command {:op :cue/launch :cue-id :drift-wash :launch {:mode :persistent}})))

(defn- await-file [^java.io.File file]
  (loop [waited 0]
    (cond (.exists file) true
          (> waited 60000) false
          :else (do (Thread/sleep 50) (recur (+ waited 50))))))

(defn -main [& [directory port]]
  (let [dir (io/file (or directory "target/waveform-move-qa"))
        port (parse-long (or port (System/getenv "TOLGLOW_QA_PORT") "0"))
        model (-> (demo/model)
                  (assoc-in [:registry :effects :drift-wash] wash)
                  (assoc-in [:registry :cues :drift-wash] {:id :drift-wash :name "Drift wash" :effect-id :drift-wash
                                                          :version 1 :swatch "#5f9fd6"})
                  (assoc-in [:pages :main :cells [0 2]] :drift-wash))
        host (app/start! {:init {:components [:show] :modules [:web-server]}
                          :show {:model model :workspace-options nil :recovery false}
                          :web-server {:host "127.0.0.1" :port port}})]
    (try
      (.mkdirs dir)
      (doseq [name ["occupy" "occupied"]] (.delete (io/file dir name)))
      (let [first-id (launch! host)
            second-id (launch! host)
            origin (str "http://127.0.0.1:" (get-in host [:application :modules :web-server :port]))
            ;; The REPL side: attach a waveform to the destination Studio has chosen.
            repl (future
                   (when (await-file (io/file dir "occupy"))
                     (send! host :modulation-bind {:instance-id first-id :param :hue
                                                   :binding {:type :lfo :waveform :triangle :period {:beats 2}
                                                             :from 90 :to 270}})
                     (spit (io/file dir "occupied") "")))]
        (send! host :modulation-bind {:instance-id first-id :param :level
                                      :binding {:type :lfo :waveform :sine :period {:bars 1}
                                                :phase-reference :launch :from 0.1 :to 0.9}})
        (Thread/sleep 400)
        (let [{:keys [out err exit]} (shell/sh "node" "dev/studio/waveform-move-qa.mjs" origin first-id second-id (str dir))]
          (print out) (flush)
          (future-cancel repl)
          (when-not (zero? exit)
            (binding [*out* *err*] (print err) (flush))
            (throw (ex-info "Waveform move browser check failed" {:exit exit}))))
        ;; The REPL reads the refusal Studio showed from the same projection.
        (let [{:keys [parameter-source-outcomes instances]} (show/snapshot host)
              refused (filter #(= :rejected (:status %)) parameter-source-outcomes)
              moved (some #(when (= second-id (:id %)) (get-in % [:parameter-sources :level :sources 0])) instances)
              report {:refused (mapv #(select-keys % [:request-id :status :reason :message]) refused)
                      :accepted (count (filter #(= :accepted (:status %)) parameter-source-outcomes))
                      :second-level-source (select-keys moved [:type :binding])
                      :first-sources (some #(when (= first-id (:id %)) (keys (:parameter-sources %))) instances)}]
          (spit (io/file dir "repl-outcomes.edn") (with-out-str (pprint/pprint report)))
          (pprint/pprint report)
          (when-not (and (= [:occupied-destination] (mapv :reason refused))
                         (= :lfo (:type moved)) (= :sine (get-in moved [:binding :waveform])))
            (throw (ex-info "The REPL projection disagrees with what Studio showed" report)))
          (println "REPL projection carries the refusal reason and the moved waveform.")))
      (finally (app/stop! host) (shutdown-agents)))))
