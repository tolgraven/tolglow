(ns tolglow.studio.reactions
  "Live subscription and event boundaries for Studio. External resources exist
  only while the corresponding re-frame query has consumers."
  (:require [cljs.spec.alpha :as s]
            [re-frame.core :as rf]
            [reagent.core :as r]
            [reagent.ratom :as ratom]))

(defonce ^:private recent (atom []))

(defonce ^:private db-schema (atom map?))

(defn validate-app-db!
  "Install a predicate or registered cljs.spec schema at the event commit boundary."
  [schema]
  (reset! db-schema schema))

(defn- ensure-valid [schema value]
  (when (and schema (not (if (fn? schema) (schema value) (s/valid? schema value))))
    (throw (ex-info "Value failed its registered schema" {})))
  value)

(defn- report! [kind id args error]
  (let [owner (some #(when (and (map? %) (or (:instance-id %) (:cue-id %)))
                       (select-keys % [:instance-id :cue-id])) args)
        wire-id (cond (keyword? id) id (nil? id) "unknown" :else (.slice (pr-str id) 0 128))
        fault (merge owner {:kind kind :id wire-id :at (.now js/Date)
                            :message (.slice (str (or (.-message error) error)) 0 1024)
                            :stack (some-> (.-stack error) (.slice 0 4096))})]
    ;; Reporting itself updates app-db. Deduplicate repeated failures so a
    ;; subscription to the whole db cannot generate a diagnostic feedback loop.
    (let [key [kind id owner (:message fault)]
          now (:at fault)]
      (when-not (some (fn [[k t]] (and (= k key) (< (- now t) 5000))) @recent)
        (swap! recent #(vec (take-last 64 (conj % [key now]))))
        (.error js/console "Studio handler failed" (pr-str [kind id]) error)
        (when-not (= id ::fault) (rf/dispatch [::fault fault]))))))

(rf/reg-event-fx ::fault
  (fn [{:keys [db]} [_ fault]]
    {:db (update-in db [:ui :reactive-faults]
                    (fn [old] (vec (take-last 64 (conj (vec old) fault)))))
     :studio/report-client-fault fault}))

(rf/reg-global-interceptor
  (rf/->interceptor :id ::validate-db
    :after (fn [context]
             (when (contains? (:effects context) :db)
               (ensure-valid @db-schema (get-in context [:effects :db])))
             context)))

(rf/reg-event-error-handler
  (fn [error wrapped]
    (let [event (:event-v (ex-data wrapped))]
      (report! :event (first event) (rest event) error))))

(defn- bucket [db period]
  (let [view (get-in db [:server-projection :view])
        transport (:transport view)
        meter (:meter transport)
        beats (:beats-per-bar meter 4)
        [unit span] (first period)
        position (case unit
                   :frames (:frame-number view)
                   :beats (:beat transport)
                   :bars (some-> (:beat transport) (/ beats))
                   :phrases (some-> (:beat transport) (/ (* beats (:bars-per-phrase meter 4)))))]
    (when (number? position)
      [(:runtime-id view) (:epoch view) (js/Math.floor (/ position span))])))

(defn- pace! [db period tick!]
  (if-let [ms (or (:ms period) (some->> (:fps period) (/ 1000)))]
    (let [timer (js/setInterval tick! (max 1 ms))]
      #(js/clearInterval timer))
    (let [key (random-uuid)
          last (volatile! (bucket @db period))]
      (add-watch db key (fn [_ _ _ next-db]
                          (let [current (bucket next-db period)]
                            (when (not= current @last) (vreset! last current) (tick!)))))
      #(remove-watch db key))))

(defn- read-cofx [db query injections]
  (reduce (fn [ctx spec]
            (let [interceptor (apply rf/inject-cofx (if (vector? spec) spec [spec]))]
              (:coeffects ((:before interceptor) {:coeffects ctx}))))
          {:db db :query query} injections))

(defn reg-sub!
  "Register (fn [context query] value); :db in context is a derefable.
  :cofx accepts the same IDs or [ID argument] as inject-cofx. :sources maps
  keys to (fn [query] watchable) factories for CLJS atoms, cursors or ratoms.
  :sample is an optional async (fn [query AbortSignal]) accessed as :sampled,
  with at most one request in flight. :period is {:fps n}, {:ms n}, {:beats n},
  {:bars n}, {:phrases n} or {:frames n}. With no sample, a tick invalidates
  the calculation to refresh synchronous coeffects. :validate accepts a
  predicate or cljs.spec; :invalid defaults to :retain, or choose :fallback."
  [id {:keys [cofx sources sample period validate invalid fallback]} compute]
  (when (and period (not (and (map? period) (= 1 (count period))
                              (#{:fps :ms :beats :bars :phrases :frames} (ffirst period))
                              (let [n (val (first period))] (and (number? n) (js/Number.isFinite n) (pos? n))))))
    (throw (ex-info "Invalid subscription cadence" {:id id :period period})))
  (when (and invalid (not (#{:retain :fallback} invalid)))
    (throw (ex-info "Unknown validation policy" {:id id :policy invalid})))
  (when (some? fallback) (ensure-valid validate fallback))
  (rf/reg-sub-raw id
    (fn [db query]
      (let [pulse (r/atom 0) sampled (r/atom nil)
            held (volatile! fallback) active? (volatile! false) busy? (volatile! false)
            cancel (volatile! nil) stop-clock (volatile! nil) inputs (volatile! {})
            watch-key (random-uuid)
            tick! (fn []
                    (when @active?
                      (if-not sample (swap! pulse inc)
                        (when-not @busy?
                          (vreset! busy? true)
                          (let [controller (js/AbortController.)]
                            (vreset! cancel controller)
                            (-> (js/Promise.resolve nil)
                                (.then (fn [_] (when @active? (sample query (.-signal controller)))))
                                (.then (fn [v] (when @active? (reset! sampled v))))
                                (.catch (fn [e] (when @active? (report! :source id query e))))
                                (.finally (fn [] (vreset! busy? false)))))))))
            start! (fn []
                     (vreset! active? true)
                     (doseq [[key factory] sources]
                       (let [source (factory query)]
                         (add-watch source watch-key (fn [_ _ old v] (when (not= old v) (swap! pulse inc))))
                         (vswap! inputs assoc key source)))
                     (when period (vreset! stop-clock (pace! db period tick!)))
                     (when sample (tick!)))]
        (ratom/make-reaction
          (fn []
            (try
              (when-not @active? (start!))
              @pulse
              (let [ctx (assoc (read-cofx db query cofx) :sources (into {} (map (fn [[k v]] [k @v])) @inputs)
                               :sampled @sampled)
                    value (ensure-valid validate (compute ctx query))]
                (vreset! held value))
              (catch :default e
                (report! :subscription id query e)
                (if (= invalid :fallback) fallback @held))))
          :on-dispose (fn [_]
                        (vreset! active? false)
                        (when @stop-clock (@stop-clock))
                        (when @cancel (.abort @cancel))
                        (doseq [[_ source] @inputs] (remove-watch source watch-key))))))))
