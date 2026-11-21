(ns tolglow.studio.expressive-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.data.json :as json]
            [afterglow.control.devices.expressive :as mapping]
            [tolglow.studio :as studio]
            [tolglow.studio.demo :as demo]
            [tolglow.studio.expressive :as expressive]
            [tolglow.studio.persistence :as files]
            [tolglow.studio.web :as web])
  (:import [java.io ByteArrayInputStream]))

(defn request [uri data]
  {:request-method :post :uri uri :headers {"host" "localhost"}
   :body (ByteArrayInputStream. (.getBytes (json/write-str (web/json-data data)) "UTF-8"))})
(defn result [response] (json/read-str (:body response) :key-fn keyword))
(def config
  {:devices {"push-main" {:enabled? true :auto? true :disabled-axes #{:glide}
                         :params {:level {:axis :pressure :range [0.1 0.8]
                                          :input-range [0.2 0.9] :curve :smoothstep :inverted? true}}
                         :cues {:ember {:params {:level {:axis :slide :range [0.2 0.6]}}}
                                :ice {:enabled? false}}}}})

(deftest preview-uses-actual-mapper-with-pinned-and-active-schemas
  (let [show (demo/model)
        show (assoc-in show [:settings :expressive] config)
        preview (expressive/preview show {:device-id "push-main" :cue-id :ember :device-kind :push-3})]
    (is (= #{:strike :pressure :glide :slide} (set (keys (:axes preview)))))
    (is (= :slide (get-in preview [:mappings 0 :axis])))
    (is (= [0.2 0.6] (get-in preview [:mappings 0 :range])))
    (is (= 0.2 (get-in preview [:samples 0 :values :level])))
    (is (= 0.6 (get-in preview [:samples 4 :values :level])))
    (is (= :unchanged (:note-lifecycle preview)))
    (is (false? (:hardware-verified? preview)))
    (let [draft (expressive/preview show {:device-id "push-main" :cue-id :ember :policy {:enabled? false}})]
      (is (empty? (:mappings draft)))
      (is (= [{:param :level :code :policy-disabled}] (:disabled-reasons draft))))
    (is (= config (get-in show [:settings :expressive])) "Preview never publishes its draft")
    (let [v1 (get-in show [:registry :effects :ember])
          v2 (-> v1 (assoc :version 2) (assoc-in [:params :level :bounds] [0 10]))
          pinned (-> show (assoc-in [:registry :effects :ember] v2)
                     (assoc-in [:registry :versions [:effects :ember 1]] v1)
                     (assoc-in [:registry :cues :ember :effect-version] 1))]
      (is (= 1 (:effect-version (expressive/preview pinned {:device-id "push-main" :cue-id :ember}))))
      (let [active (assoc-in pinned [:instances "held"] {:cue-id :ember :definition v2 :params {:level 5}})
            p (expressive/preview active {:device-id "push-main" :cue-id :ember :instance-id "held"})]
        (is (= 2 (:effect-version p)))
        (is (= [0 10] (get-in p [:schemas :level :bounds])))))))

(deftest ring-preview-normalizes-policy-and-rejects-unknown-inputs
  (let [runtime (studio/create-runtime {:workspace-options nil})]
    (try
      (let [app (studio/handler runtime)
            response (app (request "/api/expressive/preview"
                                   {:device-id "push-main" :device-kind "push-3" :cue-id "ember"
                                    :policy {:disabled-axes ["pressure"]}}))]
        (is (= 200 (:status response)))
        (is (= [] (:mappings (result response))))
        (is (= [{:param "level" :axis "pressure" :code "axis-disabled"}]
               (:disabled-reasons (result response))))
        (doseq [payload [{:device-id "p" :cue-id :missing}
                         {:device-id "p" :cue-id :ember :device-kind :unknown}
                         {:device-id "p" :cue-id :ember :device-kind :mpe :axes [:x]}
                         {:device-id "p" :cue-id :ember :instance-id "missing"}
                         {:device-id "p" :cue-id :ember :policy {:params {:level {:axis :pressure :curve :wrong}}}}]]
          (is (= 400 (:status (app (request "/api/expressive/preview" payload))))))
        (is (empty? @(:devices runtime)) "Preview never opens hardware"))
      (finally (studio/stop! runtime)))))

(deftest json-settings-save-and-full-show-roundtrip-restore-policy-only
  (let [runtime (studio/create-runtime {:workspace-options nil})]
    (try
      (let [app (web/app {:heartbeat! (fn [_])
                          :submit! (fn [type payload]
                                     (let [receipt (studio/submit! runtime type payload)]
                                       (studio/tick! runtime 0) receipt))})
            saved (app (request "/api/settings" {:scope :expressive :values config}))
            stored (get-in @(:state runtime) [:model :settings :expressive])]
        (is (= 200 (:status saved)))
        (is (= :slide (get-in stored [:devices :push-main :cues :ember :params :level :axis])))
        (is (= #{:glide} (get-in stored [:devices :push-main :disabled-axes])))
        (is (= :smoothstep (get-in stored [:devices :push-main :params :level :curve])))
        (let [document (files/read-string (pr-str (studio/export-show runtime)))
              restored (files/thaw (demo/model) document)
              receipt (studio/submit! runtime :import {:source (pr-str document)})]
          (studio/tick! runtime 1)
          (is (= :accepted (:status @receipt)))
          (is (= stored (get-in restored [:settings :expressive])))
          (is (= stored (get-in @(:state runtime) [:model :settings :expressive])))
          (is (empty? @(:devices runtime)))
          (is (= (:mappings (expressive/preview restored {:device-id "push-main" :cue-id :ember}))
                 (:mappings (expressive/preview (:model @(:state runtime)) {:device-id "push-main" :cue-id :ember})))))
        (is (= 422 (:status (app (request "/api/settings" {:scope :expressive
                                                           :values {:devices {:bad {:enabled? "false"}}}})))))
        (is (= stored (get-in @(:state runtime) [:model :settings :expressive]))))
      (finally (studio/stop! runtime)))))

(deftest invalid-imported-policy-is-rejected-before-restoring-state
  (let [valid (files/freeze (assoc-in (demo/model) [:settings :expressive] config))
        invalid (assoc-in valid [:model :settings :expressive :devices "push-main" :disabled-axes] #{:x})
        data (try (files/thaw (demo/model) invalid) nil
                  (catch clojure.lang.ExceptionInfo error (ex-data error)))]
    (is (= :invalid-show (:persistence/error data)))
    (is (= [:settings :expressive :devices "push-main" :disabled-axes]
           (get-in data [:errors 0 :path])))))
