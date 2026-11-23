(ns tolglow.effects.block-trace
  "Inspect the compiled step that supplied an instance's committed observation."
  (:require [afterglow.reactive :as graph]
            [afterglow.render.blocks :as blocks]
            [afterglow.runtime :as runtime]
            [tolglow.effects.variable-blocks :as variable-blocks]
            [tolglow.effects.prefabs :as prefabs]))

(defn source [{:keys [model variable-observation]}]
  (if variable-observation
    (vary-meta (variable-blocks/resolved model variable-observation) assoc ::variables variable-observation)
    model))

(defn inspect [context [_ instance-id path target]]
  (let [model @(:db context)]
    (if (nil? instance-id)
      (variable-blocks/inspect (::variables (meta model)) path)
      (runtime/observe (get-in model [:instances instance-id]) [path target]))))

(defn- summaries [source materials]
  (update-vals materials #(assoc (select-keys % [:name :mode :inputs]) :source source)))

(def ^:private builtin-summaries (summaries :builtin blocks/library))

(defn material-sources [{:keys [db]} [_ effect-id]]
  (let [model @db
        {:keys [imported-prefabs prefabs]} (get-in model [:registry :effects effect-id])]
    [imported-prefabs (prefabs/catalog model) prefabs]))

(defn library [{:keys [subscribe]} [_ effect-id]]
  (let [[imported shared local] @(subscribe [::material-sources effect-id])]
    (if (or (seq imported) (seq shared) (seq local))
      (merge builtin-summaries (summaries :captured imported) (summaries :show shared)
             (summaries :local local))
      builtin-summaries)))

(defn register! [system]
  (graph/reg-sub! system ::material-sources material-sources
                  {:id ::material-sources :kind :sub
                   :implementation {:type :var :symbol "tolglow.effects.block-trace/material-sources"}})
  (graph/reg-sub! system :runtime/block-library library
                  {:id :runtime/block-library :kind :sub
                   :implementation {:type :var :symbol "tolglow.effects.block-trace/library"}})
  (graph/reg-sub! system :runtime/block-trace inspect
                  {:id :runtime/block-trace :kind :sub
                   :implementation {:type :var :symbol "tolglow.effects.block-trace/inspect"}})
  system)

(defn project-display [runtime-id model view]
  (if (= :block-trace (:display-mode view))
    (let [instance (get-in model [:instances (:instance-id view)])
          settings (get-in model [:settings :controllers :push-display])
          path (or (:binding-path view) (:binding-path settings)
                   (first (sort-by pr-str (keys (get-in instance [:definition :blocks])))))
          target (or (:target-id view) (:target-id settings))
          current? (and instance (= (:epoch view) (:epoch model))
                        (= (:definition-version view) (:effect-version instance)))]
      (assoc view :runtime-id runtime-id
             :block-trace (merge (select-keys settings [:stage-index :component-index])
                                 {:binding-path path :target-id target
                                  :sample (when (and current? path) (runtime/observe instance [path target]))})))
    view))
