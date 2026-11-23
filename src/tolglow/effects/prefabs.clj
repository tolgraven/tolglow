(ns tolglow.effects.prefabs
  "Reusable material definitions and the snapshots owned by published effects."
  (:require [clojure.string :as str]
            [tolglow.compiler.blocks :as blocks]
            [tolglow.registry :as registry]))

(defn catalog [model]
  (get-in model [:settings :prefabs :definitions] {}))

(defn draft-plan
  "Prepare an isolated stage with the same typed inputs as a published effect.
  A cue parameter can be type-checked here before its value is available."
  [id material dry mode]
  (let [chain {:base dry :stages [{:block id :mode mode}]}
        definition {:blocks {[:varyings :sample] chain} :prefabs {id material}}
        parameter (fn [node _ _] (assoc node :type (:type node :number)))]
    (blocks/prepare definition [] parameter)))

(defn- validate-material! [id material]
  (when-not (and (map? material) (string? (:name material))
                 (not (str/blank? (:name material)))
                 (map? (:inputs material)) (every? keyword? (keys (:inputs material)))
                 (#{:replace :add :multiply} (:mode material)) (registry/data? material))
    (throw (ex-info "A material needs a name, input defaults and a stage mode"
                    {:path [:prefabs :definitions id]})))
  (draft-plan id material 0.0 :replace))

(defn validate! [settings]
  (when (some? settings)
    (let [entries (:definitions settings {})]
      (when-not (and (map? settings) (every? #{:definitions} (keys settings))
                     (map? entries) (<= (count entries) 64) (every? keyword? (keys entries)))
        (throw (ex-info "Provide up to 64 named material definitions"
                        {:path [:prefabs :definitions]})))
      (doseq [[id material] entries]
        (validate-material! id material)))))

(defn shared-ids [bindings local]
  (into #{} (comp (mapcat :stages) (map :block) (remove #(contains? local %)))
        (vals bindings)))

(defn capture
  "Snapshot shared stages used by this revision. Local definitions take
  precedence. An imported effect can supply materials absent from the show."
  [entries {:keys [blocks prefabs imported-prefabs] :as definition}]
  (if (seq blocks)
    (let [wanted (shared-ids blocks prefabs)]
      ;; An empty snapshot also matters: an intervening library edit must not
      ;; replace a published built-in when this revision is later restored.
      (assoc definition :imported-prefabs
             (merge {} (select-keys imported-prefabs wanted) (select-keys entries wanted))))
    (dissoc definition :imported-prefabs)))

(defn prepare-command [model command]
  (if (and (= :registry/publish (:op command)) (= :effects (:kind command)))
    (let [previous (get-in model [:control :history (:command-id command) :command])
          entries (if previous (get-in previous [:definition :imported-prefabs]) (catalog model))]
      (update command :definition (partial capture entries)))
    command))

(defn startup [model]
  (let [entries (catalog model)
        freeze (fn [effect]
                 (if (contains? effect :imported-prefabs) effect (capture entries effect)))
        archive (fn [versions [kind :as ref] definition]
                  (assoc versions ref (if (= kind :effects) (freeze definition) definition)))]
    (validate! (get-in model [:settings :prefabs]))
    (-> model
        (update-in [:registry :effects] update-vals freeze)
        (update-in [:registry :versions] #(reduce-kv archive {} %))
        (update :instances update-vals #(update % :definition freeze)))))
