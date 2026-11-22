(ns tolglow.studio.projection
  "Versioned browser-state deltas. Pure patches operate on JSON-shaped values;
  an inert, bounded cache is owned by the HTTP runtime, never the frame owner."
  (:require [clojure.data.json :as json])
  (:import [java.nio.charset StandardCharsets]
           [java.util UUID]))

(def protocol "tolglow-state/1")

(defn identity-of [view]
  (select-keys view ["runtime-id" "show-id" "epoch"]))

(defn- byte-count [value]
  (alength (.getBytes ^String (json/write-str value) StandardCharsets/UTF_8)))

(declare wire-value)

(defn- wire-key [key]
  (let [key (wire-value key nil nil)]
    (if (string? key) key (str key))))

(defn- wire-value [value before previous-wire]
  ;; Snapshot library/rig subtrees are persistent data. Reuse their normalized
  ;; wire form rather than walking every fixture and cue on every HTTP poll.
  (if (= value before) previous-wire
    (cond
      (keyword? value) (subs (str value) 1)
      (map? value)
      (let [keys (mapv wire-key (keys value))
            unique? (= (count keys) (count (set keys)))]
        (into {} (map (fn [[key child] wire-key]
                        [wire-key (wire-value child (when unique? (get before key))
                                              (when unique? (get previous-wire wire-key)))]) value keys)))
      ;; Match the public JSON conversion: postwalk first rebuilds the set
      ;; with normalized members, then exposes that set's iteration as an array.
      (set? value) (vec (into #{} (map #(wire-value % nil nil)) value))
      (sequential? value)
      (let [before (when (sequential? before) (vec before))]
        (mapv (fn [index child]
                (wire-value child (get before index)
                            (when (vector? previous-wire) (get previous-wire index)))) (range) value))
      :else value)))

(defn- json-size-change [before after]
  ;; Exact UTF-8 JSON size difference. Unchanged subtrees cost one equality
  ;; check; a scalar edit does not serialize the entire rig/library for sizing.
  (cond
    (= before after) 0
    (and (map? before) (map? after))
    (+ (- (max 0 (dec (count after))) (max 0 (dec (count before))))
       (reduce (fn [total key]
                 (+ total
                    (cond
                      (and (contains? before key) (contains? after key))
                      (json-size-change (get before key) (get after key))
                      (contains? after key) (+ (byte-count key) 1 (byte-count (get after key)))
                      :else (- (+ (byte-count key) 1 (byte-count (get before key)))))))
               0 (into (set (keys before)) (keys after))))
    (and (vector? before) (vector? after) (= (count before) (count after)))
    (reduce + 0 (map json-size-change before after))
    :else (- (byte-count after) (byte-count before))))

(defn- collect-changes
  [before after max-operations set-op remove-op id-key ordered-keys]
  (let [result (volatile! [])
        emit! (fn [operation]
                (when (>= (count @result) max-operations)
                  (throw (ex-info "Projection patch exceeds operation bound" {::limit true})))
                (vswap! result conj operation))]
    (try
      (letfn [(visit [path old next]
                (when-not (= old next)
                  (cond
                    ;; Transit records, including colours, are tagged values.
                    (and (map? old) (map? next) (not (record? old)) (not (record? next)))
                    (do
                      (doseq [key (ordered-keys (remove #(contains? next %) (keys old)))]
                        (emit! [remove-op (conj path key)]))
                      (doseq [key (ordered-keys (keys next))]
                        (if (contains? old key)
                          (visit (conj path key) (get old key) (get next key))
                          (emit! [set-op (conj path key) (get next key)]))))
                    (and (vector? old) (vector? next) (= (count old) (count next))
                         (every? true? (map #(= (get %1 id-key) (get %2 id-key)) old next)))
                    (dotimes [i (count next)]
                      (visit (conj path i) (nth old i) (nth next i)))
                    :else (emit! [set-op path next]))))]
        (visit [] before after)
        @result)
      (catch clojure.lang.ExceptionInfo error
        (if (::limit (ex-data error)) nil (throw error))))))

(defn changes
  "Return ordered JSON set/remove operations or nil if the patch is too large."
  ([before after] (changes before after 4096))
  ([before after max-operations]
   (collect-changes before after max-operations "set" "remove" "id" sort)))

(defn typed-changes
  "Return Transit operations with the original map keys and atomic tagged values.
  Sibling map edits commute, so heterogeneous keys need no comparison or coercion."
  [before after]
  (collect-changes before after 4096 :set :remove :id identity))

(defn create
  "Create an inert HTTP-owned cache. Serialized byte accounting bounds retained
  wire snapshots, not the JVM object overhead. Oversized snapshots remain full."
  ([] (create {}))
  ([{:keys [max-entries max-bytes max-operations]
     :or {max-entries 8 max-bytes (* 16 1024 1024) max-operations 4096}}]
   (when-not (and (integer? max-entries) (<= 1 max-entries 128)
                  (integer? max-bytes) (pos? max-bytes)
                  (integer? max-operations) (<= 1 max-operations 4096))
     (throw (ex-info "Invalid projection cache bounds" {})))
   {:lock (Object.) :state (atom {:entries {} :order [] :bytes 0 :closed? false})
    :max-entries max-entries :max-bytes max-bytes :max-operations max-operations}))

(defn clear! [store]
  (locking (:lock store)
    (reset! (:state store) {:entries {} :order [] :bytes 0 :closed? true})))

(defn status [store]
  (let [state @(:state store)]
    {:entries (count (:entries state)) :bytes (:bytes state) :closed? (:closed? state)}))

(defn- retain [store state token source view size]
  (if (or (contains? (:entries state) token) (> size (:max-bytes store))) state
    (loop [state (-> state (assoc-in [:entries token] {:source source :view view :bytes size})
                    (update :order conj token) (update :bytes + size))]
      (if (or (> (count (:order state)) (:max-entries store)) (> (:bytes state) (:max-bytes store)))
        (let [old (first (:order state))]
          (recur (-> state (update :bytes - (get-in state [:entries old :bytes]))
                     (update :entries dissoc old) (update :order #(into [] (next %))))))
        state))))

(defn project!
  "Build a full or exact-base patch envelope from a portable Studio view.
  Missing/evicted bases and changed show identities produce full snapshots.
  Every result is based on its request token; no client session or queue is kept.
  Call only at an HTTP/output boundary, outside the lighting owner's lock."
  [store source requested-base]
  (when-not (map? source) (throw (ex-info "Projection source must be a map" {})))
  (let [requested-base (when (and (string? requested-base) (<= 1 (count requested-base) 128)) requested-base)]
    (locking (:lock store)
      (let [state @(:state store)
            _ (when (:closed? state) (throw (ex-info "Projection cache is closed" {})))
            latest (peek (:order state))
            previous (get-in state [:entries latest])
            view (wire-value source (:source previous) (:view previous))
            token (if (= view (:view previous)) latest (str (UUID/randomUUID)))
            base (get-in state [:entries requested-base :view])
            full {:protocol protocol :kind "full" :base-token requested-base :token token
                  :identity (identity-of view) :view view}
            view-size (if previous (+ (:bytes previous) (json-size-change (:view previous) view)) (byte-count view))
            ;; JSON object order cannot change this length. Avoid serializing
            ;; the entire large view twice just to compare envelope sizes.
            full-size (+ view-size (- (byte-count (assoc full :view nil)) 4))
            operations (when (and base (= (identity-of base) (identity-of view)))
                         (changes base view (:max-operations store)))
            patch (when operations
                    {:protocol protocol :kind "patch" :base-token requested-base :token token
                     :identity (identity-of view) :changes operations})
            result (if (and patch (< (byte-count patch) full-size)) patch full)]
        (reset! (:state store) (retain store state token source view view-size))
        result))))
