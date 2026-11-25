(ns tolglow.autopilot
  "Run the musical feedback loop at the show boundary. Only ordinary data in
  the run extension is saved. Compiled fixture plans and prior blocks belong
  to the frame owner."
  (:require [afterglow.time :as time]
            [tolglow.autopilot.actions :as actions]
            [tolglow.autopilot.config :as config]
            [tolglow.autopilot.controller :as controller]
            [tolglow.autopilot.statistics :as statistics]
            [tolglow.autopilot.target-response :as target-response]
            [tolglow.autopilot.signal :as signal]
            [tolglow.autopilot.scope :as scope]
            [tolglow.lighting-activity :as activity]
            [tolglow.lighting-capacity :as capacity]))

(def run-path [:extensions ::run])

(defn- run [model] (get-in model run-path))

(defn- live-scope [model scope]
  ;; A saved page or a group can disappear while a show is playing.
  (try (controller/scope-ids model scope)
       (catch clojure.lang.ExceptionInfo _ [])))

(defn- members [previous model patch scope]
  (let [page-id (:page-id scope)
        page (when page-id (get-in model [:pages page-id]))
        registry (when page-id (:registry model))
        groups (when (:group-id scope)
                 (get-in model [:extensions :tolglow.lighting-scenes/control :groups]))]
    (if (and (identical? patch (:patch previous)) (= scope (:scope previous))
             (identical? page (:page previous)) (identical? registry (:registry previous))
             (identical? groups (:groups previous)))
      previous
      {:patch patch :scope scope :page page :registry registry :groups groups
       :ids (live-scope model scope)
       :cue-ids (when page (vec (scope/page-cue-ids model page-id)))})))

(defn standalone-scope [previous model patch]
  (let [{:keys [enabled? scope] :or {scope :global}} (get-in model [:settings :hdr])]
    (when (and enabled? (not= :global scope))
      (members previous model patch scope))))

(defn- scope-plan [previous model patch meter scope]
  (let [selection (members (:selection previous) model patch scope)]
    (if (and (identical? selection (:selection previous)) (identical? meter (:meter previous)))
      previous
      (let [capacities (:source-capacities meter)
            ids (filterv #(pos? (get-in capacities [% :relative-capacity] 0)) (:ids selection))]
        {:patch patch :meter meter :scope scope :selection selection
         :ids ids :cue-ids (:cue-ids selection)
         :source-rows (mapv (fn [id] {:id id :capacity (get-in capacities [id :relative-capacity])}) ids)
         :source-meter (assoc meter :meter-heads
                              (object-array (map #(aget ^objects (:meter-heads meter)
                                                       (int (first (get (:writers-by-head meter) %)))) ids)))
         :scoped-meter (if (= scope :global) meter (assoc meter :source-capacities (select-keys capacities ids)))}))))

(defn- metered-frame? [meter frame]
  (and (:universes frame) (every? (partial contains? (:universes frame)) (:universes meter))))

(defn- read-sources [{:keys [source-meter source-rows]} frame]
  (let [^doubles levels (if (metered-frame? source-meter frame)
                          (capacity/local-brightnesses source-meter (:universes frame))
                          (double-array (count source-rows)))]
    (reduce-kv (fn [rows i source] (conj rows (assoc source :level (aget levels (int i)))))
               [] source-rows)))

(defn- perform [model state decisions time-ms command]
  (reduce (fn [{:keys [model state author-request]} decision]
            (let [result (actions/apply-decision model state decision time-ms command)]
              (if-let [pending (get-in result [:state :pending])]
                (cond-> result (= author-request pending) (assoc :author-request pending))
                (dissoc result :author-request))))
          (actions/cleanup model state time-ms command) decisions))

(defn- stop-run [model prior time-ms command]
  (if prior
    (-> (:model (actions/stop model (:actions prior) time-ms command))
        (update :extensions dissoc ::run)
        (update :variables dissoc :lighting/autopilot))
    model))

(defn clear-session [model time-ms command]
  (stop-run model (run model) time-ms command))

(defn prepare
  "Decide and edit the model before effects run. raw-frame is delayed: a vetoed
  or non-due decision never renders a forecast. A cue launched by this decision
  participates in this tick's ordinary composition."
  [model previous-cache patch meter time-ms suspended? raw-frame command modulation-view last-frame]
  (let [prior (run model)]
    (if-not (config/enabled? model)
      {:model (stop-run model prior time-ms command)}
      (let [opts (config/with-defaults (get-in model [:settings :autopilot]))
            scope (:scope opts)
            plan (scope-plan (:plan previous-cache) model patch meter scope)
            changed? (and prior (or (not= scope (:scope prior))
                                    (and (:plan previous-cache)
                                         (or (not (identical? patch (get-in previous-cache [:plan :patch])))
                                             (not= (:ids plan) (get-in previous-cache [:plan :ids]))))))
            model (if changed? (stop-run model prior time-ms command) model)
            prior (when-not changed? prior)
            clock (assoc (time/frame-clock (assoc model :time-ms time-ms)) :epoch (:epoch model))
            context {:start-clock (or (:start-clock prior) clock) :modulation-view modulation-view
                     :prepared (signal/prepare (:signals previous-cache) (:waveforms model))}
            model (assoc model :time-ms time-ms)
            requested (config/targets model opts context)
            knobs (config/policy-values model opts context)
            previous-state (:controller prior)
            due? (and (not suspended?) (controller/due? previous-state clock (:epoch model) knobs))
            previous-report (or (:statistics prior)
                                (when (metered-frame? meter last-frame)
                                  (let [baseline (get-in (activity/advance-scene-meter nil patch (:scoped-meter plan)
                                                                 last-frame [] time-ms) [:report :scene])]
                                    {:window (:window opts)
                                     :brightness {:mean (:brightness baseline) :instant (:brightness baseline)}
                                     :activity {:mean (:score baseline) :instant (:score baseline)}})))
            initial (when previous-report
                      {:brightness (get-in previous-report [:brightness :instant])
                       :activity (get-in previous-report [:activity :instant])})
            response (target-response/advance (:target-response prior)
                       (cond-> clock suspended? (assoc :playing? false)) requested initial (:target-response opts))
            targets (:targets response)
            sources (when due? (read-sources plan (force raw-frame)))
            result (when due?
                     (controller/advance previous-state
                       {:clock clock :epoch (:epoch model) :sources sources :targets targets
                        :statistics previous-report :cue-ids (:cue-ids plan) :options knobs}))
            action-state (or (:actions prior) (actions/initial-state model scope))
            performed (perform model action-state (:actions result) time-ms command)
            next-run (cond-> (assoc prior :scope scope :start-clock (:start-clock context) :requested-targets requested :targets targets
                                    :target-response (:state response) :actions (:state performed))
                       due? (assoc :controller (:state result) :gains (:gains result) :decision (:report result))
                       (seq (:actions result)) (assoc :last-action (:result performed)))]
        {:model (assoc-in (:model performed) run-path next-run)
         :plan plan :clock clock :context context :options opts :tolerance (:tolerance knobs)
         :author-request (:author-request performed)}))))

(defn- multiply-gains [base more]
  ;; The controller holds one gain per head. Add the usually sparse artistic
  ;; overrides to that persistent map, retaining it between decision ticks.
  (if (seq base) (merge-with * more base) more))

(defn start-hdr-clock [model time-ms]
  (if (get-in model [:settings :hdr :enabled?])
    (if (get-in model [:extensions ::hdr-clock]) model
      (assoc-in model [:extensions ::hdr-clock] (time/frame-clock (assoc model :time-ms time-ms))))
    (if (get-in model [:extensions ::hdr-clock]) (update model :extensions dissoc ::hdr-clock) model)))

(defn render-options [model prepared blocked? modulation-view standalone-scope]
  (let [standalone (get-in model [:settings :hdr])
        managed? (and (:plan prepared) (not blocked?))
        independent? (and (:enabled? standalone) (not blocked?))]
    (when (or managed? independent?)
      (let [current (run model)
            opts (if independent? standalone (get-in prepared [:options :hdr]))
            opts (merge config/hdr-defaults opts)
            context (if independent? {:start-clock (get-in model [:extensions ::hdr-clock])
                                      :modulation-view modulation-view}
                               (:context prepared))
            fields (config/hdr-values model opts context)
            scope (if independent? (:scope opts :global) (:scope current))
            ids (when-not (= :global scope)
                  (if independent? (:ids standalone-scope) (get-in prepared [:plan :ids])))]
        (cond-> fields
          ids (assoc :fixture-ids (vec ids))
          (seq (:gains current)) (update :source-gains multiply-gains (:gains current))
          (seq (get-in current [:actions :instance-gains])) (update :instance-gains multiply-gains (get-in current [:actions :instance-gains])))))))

(defn observe [model prepared previous-cache frame blocks {:keys [suspended? calibrating?]} raw-options raw-blocks]
  (if-let [plan (:plan prepared)]
    (let [prior (run model)
          clock (:clock prepared)
          global-report (when (= :global (:scope plan)) (:lighting-activity frame))
          measured (when-not global-report
                     (activity/advance-scene-meter (:activity previous-cache) (:patch plan) (:scoped-meter plan)
                                                   frame blocks (:time-ms clock)))
          scene (:scene (or global-report (:report measured)))
          levels {:brightness (:brightness scene 0.0) :activity (:score scene 0.0)}
          history (statistics/advance (:history prior) (cond-> clock calibrating? (assoc :playing? false))
                                    (get-in prepared [:options :window]) levels (:targets prior) (:tolerance prepared))
          view {:enabled? true :scope (:scope prior) :requested-targets (:requested-targets prior) :targets (:targets prior) :statistics (:report history)
                :decision (:decision prior) :last-action (:last-action prior)}
          model (-> model (assoc-in (conj run-path :history) (:state history))
                    (assoc-in (conj run-path :statistics) (:report history))
                    (assoc-in [:variables :lighting/autopilot] view))]
      {:model model :cache {:plan plan :activity (:state measured) :signals (get-in prepared [:context :prepared])
                            :raw-options raw-options :blocks (when-not suspended? raw-blocks)}})
    {:model model}))

(defn accept-authored [model {:keys [request definition error]} time-ms command]
  (let [current (run model)]
    (if (and current (config/enabled? model))
      (let [opts (get-in model [:settings :autopilot])
            error (or error (when (or (not= :author (:preference opts))
                                      (not= (:scope current) (:scope opts :global))) :scope-changed))
            result (if error (actions/reject-authored model (:actions current) request error)
                     (actions/accept-authored model (:actions current) request definition time-ms command))]
        {:model (-> (:model result) (assoc-in (conj run-path :actions) (:state result))
                    (assoc-in (conj run-path :last-action) (:result result)))
         :ack {:status :accepted :result (:result result)}})
      {:model model :ack {:status :rejected :reason :disabled}})))
