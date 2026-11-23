(ns tolglow.pixel-layout-host-test
  (:require [clojure.test :refer [deftest is]]
            [tolglow.application :as app]
            [tolglow.demo :as demo]
            [tolglow.persistence :as persistence]
            [tolglow.pixel-layout-test :refer [ribbon]]
            [tolglow.rig.pixel-layout :as layout]
            [tolglow.rig.pixel-storage :as storage]
            [tolglow.show :as show]
            [tolglow.studio.web :as web]
            [tolglow.transit :as wire]))

(defn- rig [runtime] (get-in @(:state runtime) [:model :rig]))

(deftest compact-rig-edits-commit-once-and-reopen-in-wire-order
  (let [runtime (show/create-runtime {:workspace-options nil})
        initial {:fixtures [] :pixel-layouts [ribbon]}]
    (try
      (let [reply (show/submit! runtime :rig initial)
            racing-edit (show/submit! runtime :rig (assoc initial :pixel-layouts []))]
        (show/tick! runtime 0)
        (is (= :accepted (:status @reply)))
        (is (= :rejected (:status @racing-edit))))
      (let [committed (rig runtime)
            conflict (assoc initial :fixtures [{:id "blocked" :channels {:red 1} :universe 2 :address 1}])
            rejected (show/submit! runtime :rig conflict)
            source (show/export-show-source runtime)
            file (persistence/read-string source)]
        (show/tick! runtime 25)
        (is (= :rejected (:status @rejected)))
        (is (= committed (rig runtime)))
        (is (= [1 2] (sort (keys (get-in file [:frame-output :universes])))))
        (is (= (mapv :id (:fixtures committed)) (get-in file [:frame-output :target-order])))
        (is (= [] (get-in (wire/decode source) [:model :rig :fixtures])))
        (is (= committed (:rig (persistence/thaw (demo/model) file)))))
      (finally (show/stop! runtime)))))

(deftest file-keeps-independent-calibration-and-nonstandard-target-order
  (let [rig (-> (layout/materialize {:fixtures [] :pixel-layouts [ribbon]})
                (assoc-in [:fixtures 2 :calibration] {:observed-gain 0.8}))]
    (is (= rig (storage/expand (storage/compact rig))))
    (is (empty? (:fixtures (storage/compact rig))))
    (doseq [custom [(update rig :fixtures (comp vec reverse))
                    (assoc-in rig [:fixtures 2 :position] [7 8 9])]]
      (is (= custom (storage/expand (storage/compact custom)))))))

(deftest fixture-module-mixes-existing-patches-and-sculpted-layouts
  (let [manual {:id :desk :position [0 0 0] :address 1 :universe 20 :channels {:dimmer 1}}
        runtime (app/start! {:init {:components [:show] :modules [:fixtures] :post []}
                             :show {:workers 1 :workspace-options nil}
                             :fixtures {:patches [manual] :pixel-layouts [ribbon]}})]
    (try
      (is (= (into [:desk] (map :id (layout/fixtures ribbon))) (mapv :id (:fixtures (rig runtime)))))
      (is (= [ribbon] (:pixel-layouts (rig runtime))))
      (finally (app/stop! runtime)))))

(deftest restore-rejects-incomplete-or-malformed-layouts
  (let [rig (layout/materialize {:fixtures [] :pixel-layouts [ribbon]})
        document (persistence/freeze (assoc (demo/model) :rig rig))]
    (doseq [invalid [(assoc-in document [:model :rig :pixel-layouts 0 :points] [])
                     (update-in document [:model :rig :fixtures] pop)
                     (assoc-in document [:model :rig :fixtures 0 :pixel-index] 17)]]
      (is (thrown? clojure.lang.ExceptionInfo (persistence/thaw (demo/model) invalid))))))

(deftest studio-form-retains-new-string-layout-ids-and-wire-index-keys
  (let [spec (assoc ribbon :id "venue/ribbon" :group "front" :position-overrides {2 [4 5 6]})
        handler (web/app {:heartbeat! (constantly nil)
                          :submit! (fn [_ payload] (doto (promise) (deliver {:status :accepted :received payload})))})
        response (handler {:request-method :post :uri "/api/rig"
                           :headers {"host" "localhost" "accept" "application/transit+json"
                                     "content-type" "application/transit+json" "x-tolglow-form" "studio-v1"}
                           :body (java.io.ByteArrayInputStream.
                                  (.getBytes (wire/encode {:fixtures [] :pixel-layouts [spec]}) "UTF-8"))})]
    (is (= 200 (:status response)))
    (is (= spec (get-in (wire/decode (:body response)) [:received :pixel-layouts 0])))))
