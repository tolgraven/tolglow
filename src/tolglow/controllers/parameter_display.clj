(ns tolglow.controllers.parameter-display
  "Project committed source ownership and fixture-scoped samples for the visible encoder bank."
  (:require [afterglow.runtime :as runtime]
            [tolglow.effects.parameter-sources :as sources]))

(defn- choose-fixture [fixtures instance preferred]
  (let [{:keys [targets outputs]} (:definition instance)
        output-groups (when (vector? outputs) (map :targets outputs))
        outputs (when (and (seq output-groups) (every? some? output-groups)) (into #{} cat output-groups))
        authored (when targets (set targets))
        ordered (if (contains? instance :targets)
                  (keep (into {} (map (juxt :id identity)) fixtures) (:targets instance))
                  fixtures)
        candidates (filter (fn [{:keys [id]}] (and (or (nil? authored) (authored id))
                                                   (or (nil? outputs) (outputs id)))) ordered)]
    (or (when preferred (some #(when (= preferred (:id %)) %) candidates))
        (first candidates))))

(defn- fixture-for [cache fixtures instance preferred]
  (let [definition (:definition instance)
        launch (:targets instance)
        previous (::fixture @cache)]
    (if (and previous (identical? fixtures (:rig previous))
             (identical? launch (:launch previous)) (identical? definition (:definition previous))
             (= preferred (:preferred previous)))
      (:fixture previous)
      (let [chosen (choose-fixture fixtures instance preferred)]
        (swap! cache assoc ::fixture {:rig fixtures :launch launch :definition definition
                                      :preferred preferred :fixture chosen})
        chosen))))

(defn- driving? [{:keys [held? pending? error]}] (not (or held? pending? error)))

(defn- disposition [{:keys [sources error]}]
  (cond error :error
        (some driving? sources) :active
        (some :error sources) :error
        (some :held? sources) :held
        (seq sources) :pending))

(defn readings
  "Resolve the selected instance once. Per-fixture block reads happen only for
  encoders in this bank, and always address a real eligible head."
  [cache {:keys [model parameter-samples modulation-view]} instance view resolve-inputs]
  (let [writers (sources/source-index model (assoc parameter-samples :diagnostics (:diagnostics modulation-view)))
        fields (sources/project instance (get writers (:id instance)) resolve-inputs
                                (some #(not= false (:enabled? %)) (:responses model)))
        fixture (delay (fixture-for cache (get-in model [:rig :fixtures]) instance
                                   (or (:target-id view) (get-in model [:settings :controllers :push-display :target-id]))))]
    (fn [param]
      (when-let [{:keys [sources error effective-value] :as field} (get fields param)]
        (let [state (disposition field)
              block (when (= :active state) (some #(when (and (driving? %) (= :blocks (:type %))) %) sources))
              target (when block @fixture)
              sample (when target (runtime/observe instance [(:path block) (:id target)]))
              state (if (and block (nil? (:result sample))) :pending state)
              diagnostic (or error (some :error sources))]
          (cond-> {:source-state state
                   :source-types (mapv :type sources)
                   :effective-value (if block (:result sample) effective-value)}
            diagnostic (assoc :source-error diagnostic)
            target (assoc :sample-fixture (select-keys target [:id :name]))))))))
