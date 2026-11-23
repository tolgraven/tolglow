(ns tolglow.push-scene-test
  (:require [afterglow.render.frame :as render]
            [clojure.test :refer [deftest is]]
            [clojure.data.json :as json]
            [tolglow.demo :as demo]
            [tolglow.show :as show]
            [tolglow.show.push-scene :as preview]))

(deftest capture-uses-a-real-committed-frame-and-keeps-ordinary-views-cheap
  (let [runtime (show/create-runtime {:model (demo/model)})
        cache (preview/create-cache) ordinary {:display-mode :parameters}]
    (try
      (let [receipt (show/submit! runtime {:op :cue/launch :cue-id :ember})]
        (show/tick! runtime 100.0)
        (is (= :accepted (:status @receipt))))
      (show/tick! runtime 200.0)
      (let [state @(:state runtime)
            _ (is (identical? ordinary (preview/project-display cache state ordinary)))
            _ (is (nil? @cache))
            sim (:scene (preview/project-display cache state {:display-mode :simulator}))
            rig (:scene (preview/project-display cache state {:display-mode :rig :camera :panorama}))]
        (is (= 8 (:fixture-count sim) (count (get-in sim [:rig :fixtures]))))
        (is (identical? (:rig sim) (:rig rig)))
        (is (= :panorama (:camera rig)))
        (is (nil? (:editor sim)))
        (is (= "0" (get-in rig [:editor :selection :id])))
        (is (= (:emitted-color (first (render/project-compact (get-in state [:output :compact]) [0]))) (get-in sim [:output :values "0" :emitted-color])))
        (is (pos? (first (get-in sim [:output :values "0" :emitted-color])))))
      (finally (show/stop! runtime)))))

(deftest a-dense-rig-is-sampled-across-its-extent-with-bounded-device-traffic
  (let [fixture (first (demo/fixtures))
        fixtures (mapv (fn [i] (assoc fixture :id (str "rig/long-identifier-" i) :position [(/ i 1000.0) 2 0])) (range 20000))
        state {:model {:rig {:fixtures fixtures}} :output {}}
        projected (preview/project-display (preview/create-cache) state {:display-mode :rig})
        scene (:scene projected)]
    (is (= 20000 (:fixture-count scene)))
    (is (= 32 (count (get-in scene [:rig :fixtures]))))
    (is (= "0" (get-in scene [:rig :fixtures 0 :id])))
    (is (= "19999" (:id (peek (get-in scene [:rig :fixtures])))))
    (let [focused (:scene (preview/project-display (preview/create-cache)
                                                  (assoc-in state [:model :settings :controllers :push-display :selected-fixture]
                                                            "rig/long-identifier-10")
                                                  {:display-mode :rig :fixture-offset -3}))]
      (is (= "7" (get-in focused [:editor :selection :id])))
      (is (some #(= "7" (:id %)) (get-in focused [:rig :fixtures]))))
    (is (= [19.999 2 0] (get-in scene [:bounds :max])))
    (is (< (count (.getBytes (json/write-str projected) "UTF-8")) 65536))))
