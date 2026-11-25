(ns tolglow.observations
  "Consumer-owned browser observations for the show reaction graph."
  (:require [afterglow.reactive :as graph])
  (:import [java.util UUID]))

(def demand-query :studio/observation-demand)

(defn- finite? [x] (and (number? x) (Double/isFinite (double x))))

(defn- rgb? [x]
  (and (vector? x) (= 3 (count x)) (every? #(and (integer? %) (<= 0 % 255)) x)))

(def defaults
  {:preview.rig/center-rgb {:owner :rig :sampler :center-rgb :interval-ms 100 :valid? rgb?}
   :preview.perform/center-rgb {:owner :perform :sampler :center-rgb :interval-ms 100 :valid? rgb?}})

(defn- watched-input [lock latest on-demand! on-release!]
  ;; Afterglow installs one watch for all direct and transitive consumers and
  ;; removes it when the last one leaves. The producer follows that boundary.
  (reify clojure.lang.IRef
    (deref [_] @latest)
    (getWatches [_] (.getWatches ^clojure.lang.IRef latest))
    (getValidator [_] (get-validator latest))
    (setValidator [_ f] (set-validator! latest f))
    (addWatch [this k f]
      (locking lock
        (let [idle? (empty? (.getWatches ^clojure.lang.IRef latest))]
          (add-watch latest k f)
          (when idle? (on-demand!))))
      this)
    (removeWatch [this k]
      (locking lock
        (remove-watch latest k)
        (when (empty? (.getWatches ^clojure.lang.IRef latest)) (on-release!)))
      this)))

(defn- source-entry [{:keys [lock state version]} [id {:keys [owner sampler interval-ms valid?] :as spec}]]
  (when-not (and (keyword? id) (#{:rig :perform} owner) (keyword? sampler)
                 (integer? interval-ms) (<= 25 interval-ms 60000) (fn? valid?))
    (throw (ex-info "Invalid browser observation source" {:id id})))
  (let [latest (atom nil)
        changed! #(swap! version inc)
        release! #(do (swap! state update :grants dissoc id) (reset! latest nil) (changed!))]
    [id (assoc spec :latest latest :ref (watched-input lock latest changed! release!))]))

(defn create
  "Each configured source has a preview owner, a browser sampler, a minimum
  sampling interval and a pure validator. Source IDs are show coeffect IDs."
  ([] (create defaults))
  ([descriptors]
   (when-not (and (map? descriptors) (<= (count descriptors) 8))
     (throw (ex-info "Expected at most eight browser observation sources" {})))
   (let [manager {:lock (Object.) :state (atom {:providers {} :grants {} :serial 0}) :version (atom 0)}]
     (assoc manager :sources (into {} (map (partial source-entry manager)) descriptors)))))

(defn- identity-of [model] (select-keys model [:show-id :epoch]))

(defn latest-observation [context [_ id]]
  (let [sample ((:cofx context) id)]
    (when (and sample (= (:show sample) (identity-of @(:db context)))) sample)))

(defn register! [system manager]
  (doseq [[id {:keys [ref]}] (:sources manager)] (graph/reg-cofx! system id {:ref ref}))
  (graph/reg-sub! system :browser/observation latest-observation
    {:kind :sub :id :browser/observation
     :implementation {:type :var :symbol (str (symbol #'latest-observation))}})
  system)

(defn scope-query
  "Give each mounted producer a fresh, server-owned identity. The socket hub
  keeps this private key and exposes only the original query to the client."
  [[op owner :as query]]
  (when-not (and (= op demand-query) (= 2 (count query)) (#{:rig :perform} owner))
    (throw (ex-info "Invalid browser producer query" {})))
  (conj query (str (UUID/randomUUID))))

(defn- active? [source] (seq (.getWatches ^clojure.lang.IRef (:ref source))))

(defn- revoke! [manager id]
  (swap! (:state manager) update :grants dissoc id)
  (reset! (get-in manager [:sources id :latest]) nil))

(defn drop-provider! [{:keys [lock state version] :as manager} provider]
  (locking lock
    (when (contains? (:providers @state) provider)
      (swap! state update :providers dissoc provider)
      (doseq [[id grant] (:grants @state) :when (= provider (:provider grant))] (revoke! manager id))
      (swap! version inc))))

(defn- first-provider [providers owner]
  (->> providers (filter (fn [[query _]] (= owner (second query)))) (sort-by val) ffirst))

(defn- grant! [{:keys [state] :as manager} provider show id]
  (let [previous (get-in @state [:grants id])]
    (if (and (= provider (:provider previous)) (= show (:show previous)))
      previous
      (let [grant {:token (str (UUID/randomUUID)) :provider provider :show show :sequence -1 :frame-number -1 :completed-ms -1}]
        (revoke! manager id)
        (swap! state assoc-in [:grants id] grant)
        grant))))

(defn- instructions! [{:keys [sources state] :as manager} provider show]
  (let [owner (second provider)]
    (if (= provider (first-provider (:providers @state) owner))
      (into {} (for [[id source] sources :when (and (= owner (:owner source)) (active? source))]
                 [id (merge (select-keys source [:sampler :interval-ms])
                            (select-keys (grant! manager provider show id) [:token :show]))]))
      {})))

(defn provider-lease [{:keys [state lock version] :as manager} current-model provider]
  (locking lock
    (let [n (inc (:serial @state))]
      (swap! state #(-> % (assoc :serial n) (assoc-in [:providers provider] n)))))
  {:source! #(deref version)
   :observe! (fn [changed!]
               (add-watch version provider (fn [_ _ _ _] (changed!)))
               #(remove-watch version provider))
   :sample! (fn [_] (locking lock (instructions! manager provider (identity-of (current-model)))))
   :close! #(drop-provider! manager provider)})

(defn accept!
  "The stream supplies a private producer identity for this socket. Keep host
  receipt time and the producer's two monotonic timestamps in separate domains."
  [{:keys [lock state sources]} current provider {:keys [source token show sequence frame-number sampled-ms completed-ms value]}]
  (locking lock
    (let [grant (get-in @state [:grants source])
          spec (get sources source)]
      (when (and grant (active? spec) (= provider (:provider grant))
                 (= token (:token grant)) (= show (:show grant) (identity-of current))
                 (integer? sequence) (<= 0 sequence 9007199254740991) (> sequence (:sequence grant))
                 (integer? frame-number) (<= 0 frame-number (:frame-number current 0)) (<= (:frame-number grant) frame-number)
                 (finite? sampled-ms) (finite? completed-ms) (<= 0 sampled-ms completed-ms)
                 (<= (:completed-ms grant) completed-ms) (some? value) ((:valid? spec) value))
        (swap! state update-in [:grants source] assoc :sequence sequence :frame-number frame-number :completed-ms completed-ms)
        (let [latest (:latest spec)]
          (when-not (= value (:value @latest))
            (reset! latest {:source source :producer (peek provider) :show show :sequence sequence
                            :frame-number frame-number :sampled-ms sampled-ms :completed-ms completed-ms
                            :received-ns (System/nanoTime) :value value})))
        true))))
