(ns tolglow.effects.variable-edit
  "Find numeric show variables and change a single binding on the show owner."
  (:require [clojure.string :as str]
            [tolglow.effects.variable-blocks :as blocks]
            [tolglow.registry :as registry]))

(defn- signal-type [value]
  (cond
    (registry/finite-number? value) :number
    (and (vector? value) (<= 2 (count value) 4) (every? registry/finite-number? value))
    (case (count value) 2 :vec2 3 :vec3 4 :vec4)))

(defn- children [[path value]]
  (when (and (map? value) (< (count path) 16))
    (keep (fn [[k v]] (when (keyword? k) [(conj path k) v])) value)))

(defn sources
  "Return matching numeric paths, on request, without sending their source values.
  Large variable trees can always be addressed by their exact keyword path."
  [variables query]
  (when-not (or (nil? query) (and (string? query) (<= (count query) 256)))
    (throw (ex-info "Enter up to 256 characters to search variable paths." {})))
  (let [query (str/lower-case (str/trim (or query "")))
        tree (tree-seq #(map? (second %)) children [[:variables] variables])
        matches (keep (fn [[path value]]
                        (when-let [kind (signal-type value)]
                          (when (str/includes? (str/lower-case (pr-str path)) query)
                            {:path path :type kind}))) (take 2048 tree))]
    {:sources (vec (sort-by (comp pr-str :path) (take 48 matches)))}))

(defn apply-edit [model {:keys [op path expected binding] :as edit}]
  (when-not (and (#{:put :remove} op) (contains? edit :expected)
                 (vector? path) (<= 2 (count path)) (= :variables (first path)) (every? keyword? path)
                 (or (= :remove op) (and (map? binding) (registry/data? binding))))
    (throw (ex-info "Choose a show variable and supply its previous binding." {})))
  (let [settings (get-in model [:settings :variable-blocks] {})
        bindings (get settings :bindings {})
        current (get bindings path)
        next-value (when (= :put op) binding)]
    (cond
      (= next-value current) {:model model :ack {:status :accepted :revision (:revision model)}}
      (not= expected current)
      {:model model :ack {:status :rejected :reason :stale-binding
                          :message "This binding changed in another session. Reload the current binding before saving."}}
      :else
      (let [updated (assoc settings :bindings (if (= :put op) (assoc bindings path binding) (dissoc bindings path)))
            _ (blocks/compile-settings updated)
            next-model (-> model (assoc-in [:settings :variable-blocks] updated) (update :revision inc))]
        {:model next-model :ack {:status :accepted :revision (:revision next-model)}}))))
