(ns tolglow.fixture-photometry-test
  (:require [afterglow.output.scheduler :as scheduler]
            [clojure.data.json :as json]
            [clojure.test :refer [deftest is]]
            [tolglow.demo :as demo]
            [tolglow.rig.photometry :as photo]
            [tolglow.show :as show]
            [tolglow.studio :as studio]))

(def rig {:fixtures
          (mapv (fn [id type address] {:id id :fixture-type-id type :universe 1 :address address
                                       :channels {:dimmer 1 :red 2 :green 3 :blue 4 :white 5 :amber 6 :uv 7 :cold-white 8}
                                       :rgbw-policy :extract-min})
                [:ref :spot :other-spot] [:wash :beam :beam] [1 11 21])})

(defn- readings [] [{:fixture-id "ref" :dark 0.05 :white 0.25}
                     {:fixture-id "spot" :dark 0.10 :white 0.50}])

(deftest relative-photometry-uses-real-publisher-and-releases-the-rig
  (let [now (atom 0) capture (atom nil)
        model (-> (demo/model) (assoc :rig rig)
                  (assoc-in [:registry :fixtures :beam] {:id :beam :version 1 :name "Beam"}))
        rt (show/create-runtime {:model model :independent-output? true :physical-now-ms #(deref now)
                                 :sink {:send! (fn [out _] (reset! capture (get out 1)))}})
        payload {:session-id "test-camera" :runtime-id (:id rt) :expected-epoch (:epoch model)}
        issue (fn [p] (let [reply (show/submit! rt :photometry (merge payload p))]
                        (show/tick! rt (swap! now + 25)) @reply))
        start {:op :start :armed? true :fixture-ids ["ref" "spot"]}
        step (fn [id phase]
               (let [ack (issue {:op :step :fixture-id id :phase phase})]
                 (is (= :accepted (:status ack)) (str ack))
                 (is (false? (:presented? ack)))
                 (scheduler/publish! (:publisher rt))
                 (is (:presented? (issue {:op :heartbeat})))
                 (mapv #(bit-and 255 %) @capture)))]
    (try
      (let [response ((studio/handler rt) {:request-method :get :uri "/api/photometry" :headers {}})]
        (is (= 200 (:status response)))
        (is (= ["wash" "beam" "beam"] (mapv #(get % "type") (get (json/read-str (:body response)) "fixtures")))))
      (is (= :rejected (:status (issue (assoc start :armed? false)))))
      (is (= :rejected (:status (issue (assoc start :fixture-ids ["spot" "other-spot"])))))
      (is (= :accepted (:status (issue start))))
      (is (= :rejected (:status (issue {:op :apply :distance-m 1 :readings (readings)}))))
      (is (every? zero? (take 8 (step "ref" "dark"))))
      (let [bytes (step "ref" "white")]
        (is (= [255 255 255 255 255 255 0 255] (subvec bytes 0 8)))
        (is (every? zero? (subvec bytes 10 18))))
      (step "spot" "dark")
      (let [bytes (step "spot" "white")]
        (is (every? zero? (subvec bytes 0 8)))
        (is (= [255 255 255 255 255 255 0 255] (subvec bytes 10 18))))
      (is (= :rejected (:status (issue {:op :apply :distance-m 1 :readings (assoc-in (readings) [1 :white] 1)}))))
      (is (= :accepted (:status (issue {:op :apply :distance-m 1 :readings (readings)}))))
      (let [m (:model @(:state rt))]
        (is (nil? (photo/current m)))
        (is (= [1.0 2.0 2.0] (mapv :measured-relative-brightness (get-in m [:rig :fixtures]))))
        (is (= 2 (get-in m [:registry :fixtures :beam :version])))
        (is (= 2.0 (get-in m [:registry :versions [:fixtures :beam 2] :measured-relative-brightness]))))
      (is (= :accepted (:status (issue start))))
      (step "ref" "white")
      (swap! now + 16000)
      (is (= :rejected (:status (issue {:op :heartbeat}))))
      (is (nil? (photo/current (:model @(:state rt)))))
      (finally (show/stop! rt)))))

(deftest uv-only-fixtures-are-dark-throughout-photometry
  (let [model (assoc (demo/model) :rig (update rig :fixtures conj {:id :uv-only :universe 1 :address 40 :channels {:uv 1}}))
        session {:session-id "uv-blackout"}
        start (:model (photo/apply-operation model (merge session {:op :start :armed? true :fixture-ids [:ref]}) 0 true))]
    (is (not-any? #(= :uv-only (:id %)) (photo/fixtures model)))
    (doseq [phase [:dark :white]]
      (let [op (merge session {:op :step :fixture-id :ref :phase phase})
            output (photo/current (:model (photo/apply-operation start op 1 true)))]
        (is (some #{:uv-only} (map :target-id (get-in output [:block :assignments]))))
        (is (= 0 (get-in output [:expected-bytes [1 39]])))))))
