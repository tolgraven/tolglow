(ns tolglow.qa.parameter-widgets
  "Render the shared parameter widgets from a preview-only show: the real Push
  projection through the capture worker, and the Studio controls in Chromium.
  The effect carries the parameter types of the Push layout fixture views."
  (:require [afterglow.control.devices.push-2 :as push]
            [clojure.data.json :as json]
            [clojure.java.io :as io]
            [clojure.java.shell :as shell]
            [tolglow.application :as app]
            [tolglow.demo :as demo]
            [tolglow.effects :as fx]
            [tolglow.effects.parameter-sources :as parameter-sources]
            [tolglow.show :as show]
            [tolglow.signals :as signals]
            [tolglow.studio.display :as display]
            [tolglow.waveforms :as waveforms]))

(def ember-effect
  {:id :ember-wash :name "Ember wash" :version 1 :categories [:color :movement]
   :outputs {:color {:op :param :key :tint} :dimmer {:op :param :key :level} :aim {:op :param :key :position}}
   :params {:level {:type :number :name "Level" :default 0.62 :bounds [0 1] :ui/order 0}
            :speed {:type :number :name "Speed" :default 1.5 :bounds [0 4] :unit :beats :ui/order 1}
            :position {:type :vector :name "Position" :size 3 :bounds [-1 1] :default [0.123456 -0.5 0.8] :ui/order 2}
            :tint {:type :color :name "Tint" :default [1.0 0.42 0.12] :ui/order 3}
            :shape {:type :enum :name "Shape" :values [:sine :saw :square] :default :saw :ui/order 4}}})

(defn- selector-effect [waves]
  (let [schema (signals/selector-schema waves [:sine :triangle :saw :pulse])]
    (assoc (fx/effect :selector {:params {:wave schema :level (fx/parameter 0.8 {:bounds [0 1]})}}
                      (fx/intensity (fx/multiply (fx/param :level)
                                                 (signals/select-waveform waves (fx/param :wave) (:choices schema)
                                                                          {:phase (fx/cycles {:bars 1})}))))
           :name "Wave selector")))

(defn- send! [host op payload]
  (let [ack (deref (show/submit! host op payload) 10000 nil)]
    (when-not (= :accepted (:status ack)) (throw (ex-info "QA command failed" {:op op :ack ack})))
    ack))

(defn- launch! [host cue]
  (:instance-id (send! host :command {:op :cue/launch :cue-id cue :launch {:mode :persistent}})))

(defn- bind! [host id param binding]
  (send! host :modulation-bind {:instance-id id :param param :binding (assoc binding :type :lfo)}))

(defn- record!
  "Write one projected Push view per scenario. Touches are display state, so the
  scenario supplies them the way the control surface would."
  [host id file]
  (let [cache (waveforms/create-cache)
        selection {:selected-instance id :selected-cue :ember-wash :ready? true :page-id :main :origin [0 0]}
        touch (fn [param] {:touched-parameter {:instance-id id :param param :component 0}})
        scenarios (concat [["live-auto" {}] ["live-touch-tint" (touch :tint)] ["live-touch-position" (touch :position)]]
                          ;; History needs consecutive observations on the running show clock.
                          (repeat 40 ["live-history" {:parameter-view :history :history-beats 4}]))]
    (with-open [out (io/writer file)]
      (doseq [[index [label extra]] (map-indexed vector scenarios)]
        (when (= "live-history" label) (Thread/sleep 110))
        (let [snapshot @(:state host)
              view (merge (push/display-view selection (:model snapshot)) extra)]
          (.write out (display/frame-request (str index ":" label) (waveforms/project-display cache snapshot view nil)))
          (.write out "\n"))))))

(defn write-fixture-glyphs
  "Write the registered waveform glyphs the offline Push fixture views use.
  `waveform-shapes-test` fails when this file and the registry disagree."
  []
  (spit "dev/push-display/waveform-glyphs.json"
        (str (json/write-str (mapv #(select-keys % [:id :name :glyph]) (parameter-sources/waveform-options {}))) "\n"))
  (shutdown-agents))

(defn -main [& [directory]]
  (let [dir (io/file (or directory "target/parameter-widgets-qa"))
        port (parse-long (or (System/getenv "TOLGLOW_QA_PORT") "0"))
        waves (signals/new-registry)
        model (-> (demo/model)
                  (assoc :waveforms waves)
                  (assoc-in [:registry :effects :ember-wash] ember-effect)
                  (assoc-in [:registry :cues :ember-wash] {:id :ember-wash :name "Ember wash" :effect-id :ember-wash
                                                          :version 1 :swatch "#f45232"})
                  (assoc-in [:pages :main :cells [0 2]] :ember-wash)
                  (assoc-in [:registry :effects :selector] (selector-effect waves))
                  (assoc-in [:registry :cues :selector] {:id :selector :name "Wave selector" :effect-id :selector
                                                        :version 1 :swatch "#89b9e0"})
                  (assoc-in [:pages :main :cells [1 2]] :selector))
        host (app/start! {:init {:components [:show] :modules [:web-server]}
                          :show {:model model :workspace-options nil}
                          :web-server {:host "127.0.0.1" :port port}})]
    (try
      (.mkdirs dir)
      (let [id (launch! host :ember-wash)
            selector (launch! host :selector)
            origin (str "http://127.0.0.1:" (get-in host [:application :modules :web-server :port]))]
        (bind! host id :level {:waveform :sine :period {:bars 1} :from 0.1 :to 0.9})
        (bind! host id :speed {:waveform :saw :period {:beats 1} :from 0.0 :to 4.0})
        (bind! host id :position {:waveform :triangle :period {:bars 2} :from [-0.9 -0.6 -0.9] :to [0.9 0.8 0.9]})
        (bind! host id :tint {:waveform :sine :period {:bars 1} :from [1.0 0.42 0.12] :to [0.1 0.3 1.0]})
        (Thread/sleep 400)
        (record! host id (io/file dir "requests.ndjson"))
        (let [{:keys [out err exit]} (shell/sh "node" "dev/studio/parameter-widgets-qa.mjs" origin id selector (str dir))]
          (print out) (flush)
          (when-not (zero? exit)
            (binding [*out* *err*] (print err) (flush))
            (throw (ex-info "Parameter widget browser check failed" {:exit exit})))))
      (finally (app/stop! host) (shutdown-agents)))))
