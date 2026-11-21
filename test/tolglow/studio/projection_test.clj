(ns tolglow.studio.projection-test
  (:require [afterglow.render.frame :as render]
            [clojure.test :refer [deftest is testing]]
            [clojure.data.json :as json]
            [tolglow.show :as show]
            [tolglow.studio :as studio]
            [tolglow.studio.projection :as projection]
            [tolglow.studio.web :as web]))

(def view
  {"runtime-id" "runtime" "show-id" "main" "epoch" 0 "revision" 2 "frame-number" 10
   "library" (vec (repeat 100 {"name" "Unchanged cue" "params" {"level" 0.5}}))
   "instances" [{"id" "0:1" "params" {"rig/aim" [0 1 2] "level" 0.5}}]})

(defn wire [value] (json/read-str (json/write-str (web/json-data value))))

(deftest patches-distinguish-removal-nil-and-vector-identity
  (let [before {"drop" true "nullable" 3 "params" {"rig/aim" [0 1 2]}}
        after {"nullable" nil "new" false "params" {"rig/aim" [0 4 2]}}]
    (is (= [["remove" ["drop"]] ["set" ["new"] false]
            ["set" ["nullable"] nil] ["set" ["params" "rig/aim" 1] 4]]
           (projection/changes before after)))
    (is (nil? (projection/changes before after 2))))
  (let [before [{"id" "a" "value" 1} {"id" "b" "value" 2}]
        after (vec (reverse before))]
    (is (= [["set" ["instances"] after]]
           (projection/changes {"instances" before} {"instances" after})))
    (is (= [["set" ["instances"] [(first before)]]]
           (projection/changes {"instances" before} {"instances" [(first before)]})))))

(deftest full-initial-frame-and-exact-base-json-delta
  (let [cache (projection/create)
        full (projection/project! cache view nil)
        next (-> view (update "frame-number" inc) (assoc-in ["instances" 0 "params" "level"] 0.8))
        patch (projection/project! cache next (:token full))]
    (is (= "full" (:kind full)))
    (is (= view (:view full)))
    (is (= "patch" (:kind patch)))
    (is (= (:token full) (:base-token patch)))
    (is (= [["set" ["frame-number"] 11] ["set" ["instances" 0 "params" "level"] 0.8]]
           (get (wire patch) "changes")))
    (is (= "tolglow-state/1" (get (wire patch) "protocol")))
    (is (= 2 (:entries (projection/status cache))))
    (is (= [] (:changes (projection/project! cache next (:token patch)))))
    (is (= (:token patch) (:token (projection/project! cache next (:token patch)))))))

(deftest stale-missing-evicted-or-replaced-show-bases-receive-full-state
  (let [cache (projection/create {:max-entries 2})
        first (projection/project! cache view nil)
        second (projection/project! cache (update view "frame-number" inc) (:token first))
        third (projection/project! cache (update view "frame-number" + 2) (:token second))]
    (is (= "full" (:kind (projection/project! cache view "unknown-token"))))
    (is (= "full" (:kind (projection/project! cache view (:token first)))))
    (is (= "full" (:kind (projection/project! cache (assoc view "epoch" 1) (:token third)))))
    (is (= "full" (:kind (projection/project! cache (assoc view "runtime-id" "replacement") (:token third)))))
    (is (= "full" (:kind (projection/project! cache (assoc view "show-id" "new-show") (:token third)))))
    (is (= 2 (:entries (projection/status cache))))))

(deftest cache-byte-and-patch-bounds-never-create-a-partial-result
  (let [cache (projection/create {:max-bytes 100})
        full (projection/project! cache view nil)]
    (is (= "full" (:kind (projection/project! cache view (:token full)))))
    (is (= 0 (:entries (projection/status cache)))))
  (let [cache (projection/create {:max-operations 1})
        full (projection/project! cache view nil)]
    (is (= "full" (:kind (projection/project! cache (assoc view "revision" 3 "frame-number" 11) (:token full))))))
  (let [cache (projection/create)
        small (select-keys view ["runtime-id" "show-id" "epoch" "revision" "frame-number"])
        first (projection/project! cache small nil)
        expanded (into small (map (fn [i] [(str "field-" i) i]) (range 30)))]
    (is (= "full" (:kind (projection/project! cache expanded (:token first)))))
    (projection/clear! cache)
    (is (= {:entries 0 :bytes 0 :closed? true} (projection/status cache)))
    (is (thrown? clojure.lang.ExceptionInfo (projection/project! cache small nil)))))

(deftest concurrent-http-clients-keep-their-own-exact-base
  (let [cache (projection/create {:max-entries 16})
        first (projection/project! cache view nil)
        jobs (mapv (fn [index]
                     (future (projection/project! cache (assoc view "frame-number" (+ 11 index)) (:token first))))
                   (range 10))
        responses (mapv deref jobs)]
    (is (every? #(= "patch" (:kind %)) responses))
    (is (every? #(= (:token first) (:base-token %)) responses))
    (is (= 10 (count (distinct (map :token responses)))))
    (is (= 11 (:entries (projection/status cache))))))

(deftest incremental-normalization-and-byte-accounting-match-actual-json
  (let [cache (projection/create {:max-entries 1})
        identity {:runtime-id "runtime" :show-id :main :epoch 0 :revision 1}
        sources [{:payload {:rig/aim [0 1 2] :nil nil :set #{:color :dimmer}}}
                 {:payload {:rig/aim [0 1.0 2] :text "åäö \"\n\\"}}
                 {:payload {:rig/aim [0 1.0] :text "åäö \"\n\\" :nil nil}}
                 {:payload {}}
                 {:payload []}
                 {:payload (list :a :b)}
                 {:payload {:a 1 "a" 2}}
                 {:payload {:a 2 "a" 3}}
                 {}]]
    (doseq [source sources]
      (let [source (merge identity source)
            full (projection/project! cache source nil)]
        (is (= (wire source) (get (wire full) "view")))
        (is (= (alength (.getBytes ^String (json/write-str (:view full)) "UTF-8"))
               (:bytes (projection/status cache)))))))
  (let [cache (projection/create)
        first (projection/project! cache view nil)
        second (projection/project! cache (assoc view "frame-number" 11) nil)]
    (is (identical? (get-in first [:view "library"]) (get-in second [:view "library"])))
    (is (thrown? clojure.lang.ExceptionInfo (projection/project! cache nil nil)))))

(deftest actual-ring-state-deltas-follow-commands-and-show-restarts
  (let [runtime (show/create-runtime {:workers 1 :workspace-options nil})
        app (studio/handler runtime)
        projections (get-in @(:observers runtime) [:studio :browser-projections])
        request (fn [query]
                  (let [response (app {:request-method :get :uri "/api/state" :query-string query})]
                    (is (= 200 (:status response)))
                    (json/read-str (:body response))))]
    (try
      (let [full (request "projection=delta-v1")
            token (get full "token")
            reply (show/submit! runtime {:op :master/set :value 0.4})]
        (show/tick! runtime 25)
        (is (= :accepted (:status @reply)))
        (let [delta (request (str "projection=delta-v1&base=" token))
              plain (request "")]
          (is (= "patch" (get delta "kind")))
          (is (= token (get delta "base-token")))
          (is (some #{["set" ["master"] 0.4]} (get delta "changes")))
          (is (= 0.4 (get plain "master")))
          (is (nil? (get plain "protocol")))
          (show/submit! runtime {:op :show/restart})
          (show/tick! runtime 50)
          (let [reset (request (str "projection=delta-v1&base=" (get delta "token")))]
            (is (= "full" (get reset "kind")))
            (is (= 1 (get-in reset ["view" "epoch"]))))))
      (finally (show/stop! runtime)))
    (is (= {:entries 0 :bytes 0 :closed? true} (projection/status projections)))))

(deftest snapshot-projects-committed-poses-for-the-selected-heads
  (let [runtime (show/create-runtime)
        state @(:state runtime)
        head {:id :hung :universe 1 :address 1 :rotation-deg [0 0 180]
              :channels {:pan 1 :tilt 2}
              :calibration {:pan {:min-deg -30 :max-deg 30} :tilt {:min-deg -20 :max-deg 20}}}
        pixels (mapv (fn [i] {:id i :universe 2 :address (inc i) :channels {:red 1}}) (range 17))
        fixtures (into [head] pixels)
        move [{:target-id :hung :attribute :direction :value [-1 -1 0]}]]
    (try
      (doseq [options [{} {:include-values? false :compact-values? true}]]
        (let [frame (render/resolve-frame fixtures move options)
              projected (show/project-snapshot runtime (-> state (assoc :output frame)
                                                           (assoc-in [:model :rig :fixtures] fixtures)))
              values (get-in projected [:output :values])
              pose (get-in values [:hung :pose])]
          (is (= 16 (count values)))
          (is (not (contains? (values 0) :pose)))
          (is (= {:pan-deg 30.0 :tilt-deg 20.0} (select-keys pose [:pan-deg :tilt-deg])))
          (is (not= (:direction pose) (get-in values [:hung :direction])))))
      (finally (show/stop! runtime)))))
