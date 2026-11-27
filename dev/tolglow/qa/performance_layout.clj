(ns tolglow.qa.performance-layout
  "Run the optimized Studio against a preview-only scene with differently
  configured concurrent effects. Pass a directory for screenshots and optionally
  authoring for the Compose and Rig checks. The default scenario also runs the
  #655 widget study and a macro with custom card sizes, so the slot-grid checks
  (#795) see every widget type and a custom arrangement. A scenario naming a
  .mjs file runs that browser script against the same scene, for captures."
  (:require [clojure.java.io :as io]
            [clojure.java.shell :as shell]
            [clojure.string :as str]
            [tolglow.application :as app]
            [tolglow.qa.compound-widgets :as compound-widgets]
            [tolglow.demo :as demo]
            [tolglow.qa.push-parameters :as parameters]
            [tolglow.show :as show]
            [tolglow.signals :as signals]))

(def slot-study
  "Controls the widget study lacks, so the slot-grid checks (#795) also see
  wide cards (a range, component readouts and shape glyphs) and compact
  toggles and menus."
  {:id :slot-study :name "Slot study" :version 1 :categories [:intensity]
   :outputs {:dimmer {:op :param :key :level}}
   :params {:level {:type :number :name "Level" :default 0.5 :bounds [0 1] :ui/order 0}
            :span {:type :range :name "Span" :default [0.2 0.8] :bounds [0 1] :ui/order 1}
            :drift {:type :vector :name "Drift" :size 3 :default [0.0 0.0 0.0] :ui/order 2}
            :on {:type :boolean :name "Gate" :default true :ui/order 3}
            :mode {:type :enum :name "Mode" :values [:soft :hard] :default :soft :ui/order 4}
            :shape {:type :enum :name "Shape" :values [:sine :triangle] :default :sine :ui/order 5}}})

(defn -main [& [directory scenario]]
  (let [custom? (some-> scenario (str/ends-with? ".mjs"))
        script (case (if custom? :custom scenario)
                 :custom scenario
                 nil "dev/studio/performance-layout-qa.mjs"
                 "gestures" "dev/studio/cue-gesture-qa.mjs"
                 "cue-actions" "dev/studio/cue-action-qa.mjs"
                 "authoring" "dev/studio/compose-rig-layout-qa.mjs"
                 "shell-alignment" "dev/studio/shell-alignment-qa.mjs"
                 (throw (ex-info "Unknown Studio browser scenario" {:scenario scenario})))
        waves (let [registry (signals/new-registry)
                    sine (get-in registry [:waveforms :sine])]
                (signals/publish registry (assoc sine :id "custom/sine" :name "Studio test sine")))
        model (-> (demo/model)
                  (assoc :waveforms waves)
                  (assoc-in [:registry :effects :selector] {:id :selector :name "Wave selector" :version 1
                                                            :targets [:head-6 :head-8]
                                                            :params {:wave (signals/selector-schema waves [:sine :triangle])
                                                                     :sample {:name "Lamp sample" :type :number :default 0.5 :bounds [0 1]}}
                                                            :prefabs {:rig/level {:mode :replace :glsl "0.5 + 0.1 * world.x"}}
                                                            :blocks {[:params :sample] {:stages [{:block :rig/level}]}}
                                                            :outputs {:dimmer {:op :param :key :sample}}})
                  (assoc-in [:registry :cues :selector] {:id :selector :name "Wave selector" :effect-id :selector :version 1})
                  (assoc-in [:pages :main :cells [1 2]] :selector)
                  (assoc-in [:registry :effects :orbital] parameters/orbital-effect)
                  (assoc-in [:registry :cues :orbital] {:id :orbital :version 1 :name "Orbital wash"
                                                        :effect-id :orbital :swatch "#b6df91"})
                  (assoc-in [:pages :main :cells [0 2]] :orbital)
                  (assoc-in [:registry :effects :widget-study] compound-widgets/widget-effect)
                  (assoc-in [:registry :cues :widget-study] {:id :widget-study :name "Widget study" :effect-id :widget-study
                                                            :version 1 :swatch "#c45a3c"})
                  (assoc-in [:pages :main :cells [2 2]] :widget-study)
                  (assoc-in [:registry :effects :slot-study] slot-study)
                  (assoc-in [:registry :cues :slot-study] {:id :slot-study :name "Slot study" :effect-id :slot-study
                                                          :version 1 :swatch "#8fb6d8"})
                  (assoc-in [:pages :main :cells [3 2]] :slot-study)
                  ;; The card-set macro's members, named apart from the standalone cards.
                  (assoc-in [:registry :cues :study-set] {:id :study-set :name "Study set" :effect-id :widget-study
                                                         :version 1 :swatch "#d08a5c"})
                  (assoc-in [:registry :cues :orbit-set] {:id :orbit-set :name "Orbit set" :effect-id :orbital
                                                         :version 1 :swatch "#9fd0a0"}))
        host (app/start! {:init {:components [:show] :modules [:web-server]}
                          :show {:model model :workspace-options nil :recovery false}
                          :web-server {:host "127.0.0.1" :port 0}})
        origin (str "http://127.0.0.1:" (get-in host [:application :modules :web-server :port]))
        directory (or directory "target/performance-layout")]
    (try
      (.mkdirs (io/file directory))
      (doseq [cue [:ember :breathe :sweep :orbital]]
        (let [ack @(show/submit! host :command {:op :cue/launch :cue-id cue :launch {:mode :persistent}})]
          (when-not (= :accepted (:status ack)) (throw (ex-info "Could not launch performance fixture" ack)))))
      (when (or (nil? scenario) custom?)
        (doseq [cue [:widget-study :slot-study]]
          (let [ack @(show/submit! host :command {:op :cue/launch :cue-id cue :launch {:mode :persistent}})]
            (when-not (= :accepted (:status ack)) (throw (ex-info "Could not launch a slot-grid study" ack)))))
        ;; A macro is the one place cards take custom sizes (#795).
        (doseq [[op payload] [[:composition {:id :qa/card-set :name "Card set" :mode :macro
                                             :slots [{:id :orbit :cue-id :orbit-set} {:id :study :cue-id :study-set}
                                                     {:id :solitary :cue-id :spot}]}]
                              [:composition-cards {:id :qa/card-set :cards [{:slot-id :study :size :medium}
                                                                            {:slot-id :orbit :size :large}]}]
                              [:composition-run {:id :qa/card-set}]]]
          (let [ack @(show/submit! host op payload)]
            (when-not (= :accepted (:status ack)) (throw (ex-info "Could not prepare the card-set macro" ack))))))
      (when (= "cue-actions" scenario)
        (doseq [definition [{:id :qa/night-set :name "Night set" :mode :macro :slots [{:cue-id :breathe}]}
                            {:id "qa/early-set" :name "Early set" :mode :macro :slots [{:cue-id :sweep}]}
                            {:id :qa/steps :name "Timed steps" :mode :sequence :slots [{:cue-id :ice}]}]]
          (let [ack @(show/submit! host :composition definition)]
            (when-not (= :accepted (:status ack)) (throw (ex-info "Could not save composition fixture" ack))))))
      (let [{:keys [exit out err]} (shell/sh "node" script origin directory)]
        (print out) (flush)
        (when-not (zero? exit)
          (binding [*out* *err*] (print err) (flush))
          (throw (ex-info "Studio layout browser check failed" {:scenario script :error err}))))
      (finally (app/stop! host) (shutdown-agents)))))
