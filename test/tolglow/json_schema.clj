(ns tolglow.json-schema
  "A small JSON Schema (Draft 2020-12) validator for the keywords Tolglow's
  agent schemas use. `check-schema` walks every subschema and refuses any
  other keyword, so a schema edit cannot pass a check this validator does not
  perform. Validate parsed JSON (string keys), not Clojure data with keywords."
  (:require [clojure.data.json :as json]
            [clojure.java.io :as io]
            [clojure.string :as str]))

(def ^:private annotations #{"$schema" "$id" "title" "description" "$defs"})

(def ^:private supported
  (into annotations ["type" "required" "properties" "additionalProperties" "const" "enum"
                     "minLength" "maxLength" "minimum" "maximum" "exclusiveMinimum" "pattern"
                     "items" "prefixItems" "minItems" "maxItems" "$ref" "allOf" "if" "then" "else"]))

(defn check-schema
  "Throw when any subschema of `schema` uses a keyword this validator does not
  implement. Returns the schema."
  ([schema] (check-schema schema []) schema)
  ([schema path]
   (cond
     (boolean? schema) nil
     (not (map? schema)) (throw (ex-info "A schema is an object or a boolean" {:path path}))
     :else
     (let [unknown (remove supported (keys schema))]
       (when (seq unknown) (throw (ex-info "Unsupported schema keyword" {:keywords unknown :path path})))
       (doseq [k ["properties" "$defs"] [name sub] (get schema k)] (check-schema sub (conj path k name)))
       (doseq [k ["additionalProperties" "items" "if" "then" "else"] :when (contains? schema k)]
         (check-schema (get schema k) (conj path k)))
       (doseq [k ["prefixItems" "allOf"] [i sub] (map-indexed vector (get schema k))]
         (check-schema sub (conj path k i)))))))

(defn load-schema [resource]
  (doto (json/read-str (slurp (io/resource resource))) check-schema))

(defn json-equal?
  "JSON equality: numbers compare by value (1 equals 1.0), objects by keys."
  [a b]
  (cond
    (and (number? a) (number? b)) (== a b)
    (and (map? a) (map? b)) (and (= (set (keys a)) (set (keys b))) (every? #(json-equal? (get a %) (get b %)) (keys a)))
    (and (sequential? a) (sequential? b)) (and (= (count a) (count b)) (every? true? (map json-equal? a b)))
    :else (= a b)))

(defn- json-type? [t v]
  (case t
    "null" (nil? v)
    "boolean" (boolean? v)
    "string" (string? v)
    "object" (map? v)
    "array" (sequential? v)
    "number" (number? v)
    "integer" (or (integer? v) (and (number? v) (not (Double/isInfinite (double v)))
                                    (== (Math/floor (double v)) (double v))))))

(defn- resolve-ref [root ref]
  (when-not (str/starts-with? ref "#/") (throw (ex-info "Only local $ref is supported" {:ref ref})))
  (or (get-in root (str/split (subs ref 2) #"/"))
      (throw (ex-info "Unresolved $ref" {:ref ref}))))

(declare errors)

(defn- check [root schema v path]
  (let [{:strs [type required properties additionalProperties const enum minLength maxLength
                minimum maximum exclusiveMinimum pattern items prefixItems minItems maxItems
                allOf] ref "$ref" if-schema "if" then-schema "then" else-schema "else"} schema
        fail (fn [message] [{:path path :message message :value (if (coll? v) (str (clojure.core/type v)) v)}])]
    (concat
     (when ref (errors root (resolve-ref root ref) v path))
     (when (and type (not (some #(json-type? % v) (if (string? type) [type] type))))
       (fail (str "expected type " type)))
     (when (and (contains? schema "const") (not (json-equal? const v))) (fail (str "expected const " const)))
     (when (and enum (not (some #(json-equal? % v) enum))) (fail (str "expected one of " enum)))
     (when (string? v)
       (concat (when (and minLength (< (count v) minLength)) (fail "string too short"))
               (when (and maxLength (> (count v) maxLength)) (fail "string too long"))
               (when (and pattern (not (re-find (re-pattern pattern) v))) (fail (str "does not match " pattern)))))
     (when (number? v)
       (concat (when (and minimum (< v minimum)) (fail (str "below minimum " minimum)))
               (when (and maximum (> v maximum)) (fail (str "above maximum " maximum)))
               (when (and exclusiveMinimum (<= v exclusiveMinimum)) (fail (str "not above " exclusiveMinimum)))))
     (when (map? v)
       (concat (keep (fn [k] (when-not (contains? v k) {:path (conj path k) :message "required"})) required)
               (mapcat (fn [[k child]]
                         (cond
                           (contains? properties k) (errors root (get properties k) child (conj path k))
                           (contains? schema "additionalProperties")
                           (errors root additionalProperties child (conj path k))))
                       v)))
     (when (sequential? v)
       (let [v (vec v) prefix (count prefixItems)]
         (concat (when (and minItems (< (count v) minItems)) (fail "too few items"))
                 (when (and maxItems (> (count v) maxItems)) (fail "too many items"))
                 (mapcat (fn [i s] (when (< i (count v)) (errors root s (nth v i) (conj path i)))) (range) prefixItems)
                 (when (contains? schema "items")
                   (mapcat #(errors root items (nth v %) (conj path %)) (range prefix (count v)))))))
     (mapcat #(errors root % v path) allOf)
     (when if-schema
       (if (empty? (errors root if-schema v path))
         (when then-schema (errors root then-schema v path))
         (when else-schema (errors root else-schema v path)))))))

(defn errors
  "Validation errors of parsed JSON `v` against `schema` (within `root`). The
  schema must pass `check-schema`; `load-schema` checks it once."
  ([schema v] (check-schema schema) (errors schema schema v []))
  ([root schema v path]
   (cond (true? schema) []
         (false? schema) [{:path path :message "schema is false"}]
         :else (vec (check root schema v path)))))
