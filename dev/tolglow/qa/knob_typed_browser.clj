(ns tolglow.qa.knob-typed-browser
  "Run the optimized Studio on a show whose cue has a parameter of every type
  and exercise typed knob destinations (#667) in headless Chrome: generate a
  candidate restricted to choices, switches and numbers, add a choice from the
  picker to the candidate and sweep it in the Room, then link a switch to a
  kept knob and turn it. A REPL knob with a vector link then reaches the open
  page. Usage:
  lein with-profile +dev run -m tolglow.qa.knob-typed-browser [directory] [port]"
  (:require [afterglow.control.model :as control]
            [clojure.java.io :as io]
            [clojure.java.shell :as shell]
            [tolglow.application :as app]
            [tolglow.show :as show]))

(def ^:private effect
  {:id :typed :version 1 :name "Typed"
   :params {:level {:type :number :default 0.5 :bounds [0.0 1.0]}
            :bands {:type :integer :default 3 :bounds [1 8]}
            :zoom {:type :number :default 1.0 :bounds [0.0 4.0] :step 0.25}
            :tint {:type :color :default [0.2 0.4 1.0]}
            :shape {:type :enum :values [:dot :bar :ring :star :wave] :default :bar}
            :strobe {:type :boolean :default false}
            :centre {:type :vector :size 3 :default [0.0 1.0 2.0] :bounds [-4.0 4.0]
                     :semantic/type :position :unit :m}
            :beats {:type :number :default 1 :bounds [1/8 8] :unit :beats :semantic/type :beat-ratio}}
   :outputs {:dimmer {:op :param :key :level} :color {:op :param :key :tint :type :vec3}}})

(defn- model []
  (control/new-show {:show-id :typed :transport {:playing? true :beat 0.0 :bpm 120.0}
                     :registry {:effects {:typed effect} :cues {:typed {:id :typed :effect-id :typed :version 1}}}
                     :rig {:fixtures [{:id :wash :universe 1 :address 1
                                       :channels {:dimmer 1 :red 2 :green 3 :blue 4}}]}}))

(defn- accepted! [ack what]
  (when-not (= :accepted (:status ack)) (throw (ex-info (str "Could not " what) ack)))
  ack)

(defn- browser! [origin directory phase]
  (let [{:keys [exit out err]} (shell/sh "node" "dev/studio/knob-typed-browser-qa.mjs" origin directory phase)]
    (print out) (flush)
    (when-not (zero? exit)
      (binding [*out* *err*] (print err) (flush))
      (throw (ex-info (str "Typed knob browser check failed (" phase ")") {:error err})))))

(defn -main [& [directory port]]
  (let [port (Long/parseLong (or port "19701"))
        host (app/start! {:init {:components [:show] :modules [:web-server]}
                          :show {:model (model) :workspace-options nil :recovery false}
                          :web-server {:host "127.0.0.1" :port port}})
        origin (str "http://127.0.0.1:" port)
        directory (or directory "target/knob-typed-browser")
        repl! #(accepted! @(show/submit! host :command (merge {:source {:adapter :repl :session-id "qa"}} %)) (pr-str %))]
    (try
      (.mkdirs (io/file directory))
      (repl! {:op :cue/launch :cue-id :typed :launch {:mode :persistent}})
      (browser! origin directory "browser")
      ;; REPL -> Studio: a knob with a vector link, turned from the REPL.
      (repl! {:op :knob/create :knob-id "aim" :name "Aim"
              :links [{:id "centre" :destination {:kind :param :cue-id :typed :param :centre}
                       :range [[-2.0 0.0 1.0] [2.0 2.0 3.0]] :curve :ease-in-out}]})
      (repl! {:op :knob/set :knob-id "aim" :value 0.5})
      (println "REPL knob:" (get-in (show/snapshot host) [:knobs :knobs]))
      (browser! origin directory "repl")
      (finally (app/stop! host) (shutdown-agents)))))
