(ns tolglow.integrations.live-parameters
  "Durable Live parameter identities and value observations for the Studio patchbay.")

(def max-parameters 64)

(defn wire-id [id] (if (keyword? id) (subs (str id) 1) id))

(defn valid-id? [s] (and (string? s) (re-matches #"[A-Za-z0-9][A-Za-z0-9_.-]{0,63}" s)))

(defn- named? [s] (and (string? s) (<= 1 (count s) 512)))
(defn- finite? [x] (and (number? x) (Double/isFinite (double x))))

(defn descriptor [data]
  (when-not (and (map? data)
                 (every? #{:path :name :track-name :device-name :range :quantized?} (keys data))
                 (named? (:path data))
                 (re-matches #"live_set tracks [0-9]+ (?:devices [0-9]+ (?:chains|return_chains) [0-9]+ )*(?:devices [0-9]+ parameters [0-9]+|mixer_device (?:volume|panning|sends [0-9]+))"
                             (:path data))
                 (every? named? ((juxt :name :track-name :device-name) data))
                 (boolean? (:quantized? data))
                 (vector? (:range data)) (= 2 (count (:range data)))
                 (every? finite? (:range data)) (apply < (:range data)))
    (throw (ex-info "Select a device or mixer parameter on a Live track" {:sync/error :invalid-live-parameter})))
  data)

(defn sources [table]
  (when-not (and (map? table) (<= (count table) max-parameters))
    (throw (ex-info "A Live connection supports 64 observed parameters" {:sync/error :invalid-live-parameters})))
  (into {} (map (fn [[id data]]
                  (let [id (wire-id id)]
                    (when-not (valid-id? id)
                      (throw (ex-info "Use a short unique parameter source name" {:sync/error :invalid-live-parameter-id})))
                    [id (descriptor data)])) table)))

(defn observation
  "Admit the current declared address and range only. Receipt time belongs to
  the Studio's monotonic clock. Live supplies the parameter's native units."
  [live event-id observed-at {:keys [id path value automation-state]}]
  (when-let [spec (get-in live [:observed-parameters (wire-id id)])]
    (let [[low high] (:range spec)]
      (when (and (= :lighting (:mode live)) (= path (:path spec))
                 (finite? value) (<= low value high)
                 (integer? automation-state) (<= 0 automation-state 2))
        {:type :sync/observation
         :observation {:source-id (str "live/" (wire-id (:source-id live)) "/parameter/" (wire-id id))
                       :event-id event-id :observed-at-ms observed-at :kind :automation
                       :value value :gate? (> value low) :confidence 1.0 :origin :live-playback
                       :port (merge {:transport :live :signal :device-parameter
                                     :automation-state (nth [:none :playing :overridden] automation-state)}
                                    (select-keys spec [:path :name :device-name :track-name :range :quantized?]))}}))))
