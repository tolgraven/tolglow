(ns tolglow.fog-calibration-test
  (:require [afterglow.control.model :as control]
            [afterglow.render.atmosphere :as fog]
            [afterglow.render.frame :as frame]
            [clojure.data.json :as json]
            [clojure.test :refer [deftest is]]
            [tolglow.persistence :as files]
            [tolglow.rig.fog-calibration :as scan]
            [tolglow.show :as show]
            [tolglow.studio :as studio]))

(def nozzle {:id :jet :universe 0 :address 1 :channels {:fog 1} :fogger {:flow-m3-min 60}})

(def room {:width 2 :height 2 :depth 5 :air-changes-per-hour 12})

(def model (assoc (control/new-show {:registry {:effects {:jet {:id :jet :name "Fog" :version 1
                                                               :targets [:jet] :params {} :outputs {:fog 1}}}
                                               :cues {:go {:id :go :name "Burst" :version 1 :effect-id :jet}}}})
                  :rig {:fixtures [nozzle] :room room}))

(defn- camera-readings []
  (let [fixture (assoc-in nozzle [:fogger :mixing-seconds] 3)
        plan (fog/plan (assoc room :measured-fog {:reference-path-m 3 :extinction-per-m 2.1
                                                  :clearance-half-life-s 18}) (frame/compile-patch [fixture]))
        times (range 0 85000 1000)
        history (rest (reductions #(fog/present plan %1 %2 {0 [(if (<= 5000 %2 12000) 255 0)]})
                                  (fog/initial plan 0 0) times))]
    (mapv (fn [t s] {:elapsed-ms t :bright (+ 0.05 (* 0.75 (- 1 (:fog-level s)))) :dark 0.05
                     :phase (cond (< t 5000) "baseline" (< t 13000) "active" :else "clearing")}) times history)))

(deftest fit-a-stationary-phone-to-the-actual-cue-burst
  (let [now (atom 0) out (atom [])
        rt (show/create-runtime {:model model :physical-now-ms #(deref now)
                                 :sink {:send! (fn [packet _] (swap! out conj (bit-and 255 (first (get packet 0)))))}})
        common {:expected-epoch (:epoch model) :runtime-id (:id rt) :session-id "phone"}
        issue (fn [type data] (let [p (show/submit! rt type data)] (show/tick! rt @now) @p))
        scan! #(issue :fog-calibration (merge common %))
        doc #(files/read-string (show/export-show-source rt))]
    (try
      (let [response ((studio/handler rt) {:request-method :get :uri "/api/fog-calibration" :headers {}})
            ids (mapv #(get % "id") (get (json/read-str (:body response)) "fixtures"))]
        (is (= 200 (:status response))) (is (= ["jet"] ids)))
      (is (= :accepted (:status (scan! {:op :start :fixture-id "jet" :path-m 3}))))
      (is (zero? (last @out)))
      (is (nil? (scan/current (:model (doc)))))
      (reset! now 5000)
      (let [id (:instance-id (issue :command {:op :cue/launch :cue-id :go}))]
        (is (= 255 (last @out)))
        (reset! now 13000) (issue :command {:op :instance/remove :instance-id id})
        (is (zero? (last @out))))
      (reset! now 85000)
      (let [samples (camera-readings) group {:op :samples :samples samples}]
        (is (= :accepted (:status (scan! {:op :samples :samples (subvec samples 0 10)}))))
        (is (pos? (:fog/observed-change-per-s (show/variables rt))))
        (is (= :accepted (:status (scan! group))))
        (is (= :accepted (:status (scan! group))))
        (let [m (:model @(:state rt))]
          (is (< 0.0 (get-in m [:variables :fog/observed-level]) 1.0))
          (is (neg? (get-in m [:variables :fog/observed-change-per-s])))
          (is (= {:sample-time-ms 84000 :phase :clearing :path-m 3 :session-id "phone" :epoch (:epoch model)}
                 (dissoc (get-in m [:variables :fog/observation]) :level :level-change-per-s))))
        (is (= :rejected (:status (scan! (assoc-in group [:samples 0 :bright] 0.7)))))
        (let [reply (scan! {:op :fit :fit {:clearance-half-life-s 999}})
              fit (:fit reply)]
          (is (= :accepted (:status reply)) (str reply))
          (is (= :metered-burst (:method fit)))
          (is (< (abs (- (:clearance-half-life-s fit) 18)) 0.4))
          (is (= :accepted (:status (scan! {:op :apply}))))
          (let [saved (:model (doc)) applied (get-in saved [:rig :room :measured-fog])]
            (is (= (:extinction-per-m fit) (:extinction-per-m applied)))
            (is (= (:mixing-seconds fit) (get-in saved [:rig :fixtures 0 :fogger :mixing-seconds])))
            (is (nil? (scan/current saved)))
            (is (= (:extinction-per-m fit) (get-in (show/snapshot rt) [:output :atmosphere :extinction-per-m]))))))
      (finally (show/stop! rt)))))
