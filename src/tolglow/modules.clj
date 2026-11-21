(ns tolglow.modules
  "Ordered Tolglow component, module and post-start lifecycles.
  A setup function receives the current context and its own options. It returns
  a map with optional :value and :stop! entries. Later modules can use earlier
  values through [:modules id]. A registry is ordinary Clojure data.")

(defn register
  "Add a setup function to a registry. Duplicate IDs are rejected."
  [registry id setup]
  (when-not (and (map? registry) (keyword? id) (fn? setup) (not (contains? registry id)))
    (throw (ex-info "Module needs a unique keyword ID and a setup function" {:module id})))
  (assoc registry id setup))

(defn settings [config id]
  (let [v (get config id {})]
    (cond
      (boolean? v) {:enabled v}
      (and (map? v) (boolean? (get v :enabled true))) v
      :else (throw (ex-info "Module settings must be a boolean or an options map" {:module id})))))

(defn- find-setup [registry namespaces id]
  (or (get registry id)
      (some (fn [namespace]
              (require namespace)
              (get (ns-publics namespace) (symbol (name id))))
            namespaces)
      (throw (ex-info "No setup function registered for enabled module" {:module id}))))

(defn plan
  "Resolve enabled modules before acquiring resources. Required components run
  even when a show config disables a module with the same kind of settings."
  [registry {:keys [init] :as config}]
  (let [namespaces (:search-in-namespaces init [])
        stages [:components :modules :post]]
    (when-not (and (map? init) (sequential? namespaces) (every? symbol? namespaces)
                   (every? #(sequential? (get init % [])) stages))
      (throw (ex-info "Expected ordered :init stages and namespace symbols" {})))
    (let [entries (for [stage stages id (get init stage)] [stage id])
          ids (map second entries)]
      (when-not (and (every? keyword? ids) (= (count ids) (count (set ids))))
        (throw (ex-info "Startup IDs must be unique keywords across the three stages" {:ids (vec ids)})))
      (mapv (fn [[stage id]]
              (let [opts (settings config id)
                    enabled? (or (= :components stage) (:enabled opts true))]
                (cond-> {:stage stage :id id :enabled? enabled? :options (dissoc opts :enabled)}
                  enabled? (assoc :setup (find-setup registry namespaces id)))))
            entries))))

(defn- unwind! [cleanups]
  (reduce (fn [errors stop!]
            (try (stop!) errors (catch Throwable e (conj errors e))))
          [] (rseq (vec cleanups))))

(defn start!
  "Start one independent application. Registration and config carry no
  process-global state. A failed start cleans up every completed acquisition."
  [registry config]
  (let [plan (plan registry config)
        cleanups (atom [])]
    (try
      (let [context
            (reduce (fn [ctx {:keys [id enabled? options setup]}]
                      (if-not enabled? ctx
                        (let [{:keys [value stop!] :as result} (setup ctx options)]
                          (when-not (and (map? result) (or (nil? stop!) (ifn? stop!)))
                            (throw (ex-info "Setup must return a map with optional :value and :stop!" {:module id})))
                          (when stop! (swap! cleanups conj stop!))
                          (-> ctx (assoc-in [:modules id] value) (update :loaded-components conj id)))))
                    {:config config :modules {} :loaded-components []} plan)]
        (assoc context :stop! (fn []
                                (let [[stops _] (swap-vals! cleanups (constantly []))
                                      errors (unwind! stops)]
                                  (when (seq errors)
                                    (let [cause (ex-info "Module shutdown failed" {:failures (count errors)})]
                                      (doseq [e errors] (.addSuppressed cause e))
                                      (throw cause)))))))
      (catch Throwable failure
        (doseq [error (unwind! @cleanups)] (.addSuppressed failure error))
        (throw failure)))))
