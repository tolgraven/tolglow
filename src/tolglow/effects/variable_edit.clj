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

(defn- child-paths [readable? [path value]]
  (when (< (count path) 16)
    (cond
      (map? value)
      (keep (fn [[k v]]
              (when (or (keyword? k) (and readable? (or (string? k) (nat-int? k))))
                [(conj path k) v]))
            value)

      (and readable? (vector? value))
      (map-indexed (fn [i v] [(conj path i) v]) value))))

(defn sources
  "Find numeric signal paths on demand. Writable binding paths use keyword
  keys. Readable signal paths can also include string keys and vector indices."
  ([variables query] (sources variables query {}))
  ([variables query {:keys [readable? type]}]
   (when-not (or (nil? query) (and (string? query) (<= (count query) 256)))
     (throw (ex-info "Enter up to 256 characters to search variable paths." {})))
   (when-not (or (nil? type) (#{:number :vec2 :vec3 :vec4} type))
     (throw (ex-info "Choose a numeric signal type." {})))
   (let [needle (str/lower-case (str/trim (or query "")))
         nodes (tree-seq (fn [[_ value]] (or (map? value) (and readable? (vector? value))))
                         (partial child-paths readable?) [[:variables] variables])
         match (fn [[path value]]
                 (when-let [kind (signal-type value)]
                   (when (and (or (nil? type) (= type kind))
                              (str/includes? (str/lower-case (pr-str path)) needle))
                     {:path path :type kind})))]
     {:sources (->> nodes (take 2048) (keep match) (take 48) (sort-by (comp pr-str :path)) vec)})))

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
            next-model (-> model (assoc-in [:settings :variable-blocks] updated) (update :revision inc))]
        (blocks/compile-model next-model)
        {:model next-model :ack {:status :accepted :revision (:revision next-model)}}))))
