;; Run from the project root with the normal deps.edn classpath:
;; clojure -M docs/benchmarks/state-delta-bench.clj > /tmp/state-delta-report.edn
(require '[clojure.data.json :as json]
         '[tolglow.show :as show]
            [tolglow.studio :as studio]
         '[tolglow.demo :as demo])

(defn large-model []
  (let [model (demo/model)
        cue (get-in model [:registry :cues :ember])]
    (-> model
        (assoc-in [:rig :fixtures]
                  (mapv (fn [index]
                          {:id (keyword (str "head-" (inc index)))
                           :name (str "RGBW pixel " (inc index))
                           :position [(double (mod index 40)) 3.0 (double (quot index 40))]
                           :universe (inc (quot index 100)) :address (inc (* 5 (mod index 100)))
                           :channels {:dimmer 1 :red 2 :green 3 :blue 4 :white 5}
                           :capabilities #{:color :dimmer} :rgbw-policy :extract-min})
                        (range 1000)))
        (update-in [:registry :cues] into
                   (map (fn [index]
                          (let [id (keyword (str "library-cue-" index))]
                            [id (assoc cue :id id :name (str "Library cue " index)
                                       :params {:level (/ (mod index 10) 10.0)})]))
                        (range 1000))))))

(defn request [app query]
  (let [start (System/nanoTime)
        response (app {:request-method :get :uri "/api/state" :query-string query})
        elapsed (/ (- (System/nanoTime) start) 1e6)
        body (:body response)]
    (when-not (= 200 (:status response)) (throw (ex-info "State request failed" response)))
    {:bytes (alength (.getBytes ^String body "UTF-8")) :ms elapsed
     :body (json/read-str body)}))

(defn summary [values]
  (let [values (vec (sort values)) n (count values)]
    {:samples n :min (first values) :p50 (nth values (quot n 2))
     :p95 (nth values (min (dec n) (int (* n 0.95)))) :max (peek values)}))

(let [runtime (show/create-runtime {:model (large-model) :workers 2 :workspace-options nil})
      app (studio/handler runtime)]
  (try
    (let [launch (show/submit! runtime {:op :cue/launch :cue-id :ember})
          _ (show/tick! runtime 0)
          id (:instance-id @launch)
          first (request app "projection=delta-v1")
          measurements
          (loop [index 0 token (get-in first [:body "token"]) previous (:body (request app "")) result []]
            (if (= index 40) result
              (let [editing? (odd? index)
                    command (when editing? (show/submit! runtime {:op :param/set :instance-id id :param :level
                                                                    :value (/ (mod index 10) 10.0)}))
                    _ (show/tick! runtime (* 25 (inc index)))
                    _ (when command (when-not (= :accepted (:status @command)) (throw (ex-info "Edit rejected" @command))))
                    full (request app "")
                    coarse (request app (str "runtime-id=" (get previous "runtime-id") "&epoch=" (get previous "epoch")
                                             "&revision=" (get previous "revision")))
                    delta (request app (str "projection=delta-v1&base=" token))]
                (recur (inc index) (get-in delta [:body "token"]) (:body full)
                       (conj result {:editing? editing? :full-bytes (:bytes full) :coarse-bytes (:bytes coarse)
                                     :delta-bytes (:bytes delta) :delta-kind (get-in delta [:body "kind"])
                                     :delta-request-ms (:ms delta)})))))]
      (prn {:workload {:fixtures 1000 :cues 1012 :active-effects 1 :samples 40
                      :preview-fixtures 16 :jvm (System/getProperty "java.version")}
            :first-full-envelope-bytes (:bytes first)
            :units {:bytes "UTF-8 JSON body bytes before optional HTTP compression"
                    :ms "Ring projection, diff and JSON encoding; excludes frame rendering, network and browser"}
            :full-bytes (summary (map :full-bytes measurements))
            :coarse-idle-bytes (summary (map :coarse-bytes (remove :editing? measurements)))
            :coarse-edit-bytes (summary (map :coarse-bytes (filter :editing? measurements)))
            :delta-bytes (summary (map :delta-bytes measurements))
            :delta-request-ms (summary (map :delta-request-ms (drop 5 measurements)))
            :all-deltas? (every? #(= "patch" (:delta-kind %)) measurements)
            :measurements measurements}))
    (finally (show/stop! runtime) (shutdown-agents))))
