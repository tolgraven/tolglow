(ns tolglow.studio.projection
  "Apply exact-base transport deltas to an authoritative server projection.
  UI drafts belong above this layer and never become a patch base."
  (:require [tolglow.studio.transit :as transit]))

(def protocol "tolglow-state/2")

(defn identity-of [view] (select-keys view [:runtime-id :show-id :epoch]))
(defn- order-of [view] (mapv view [:epoch :revision :frame-number]))
(defn- token? [value] (and (string? value) (<= 1 (count value) 128)))
(defn- fail! [] (throw (ex-info "Invalid projection delta" {})))

(defn- alter-path [value path operation replacement]
  (if (empty? path)
    (if (= operation :set) replacement (fail!))
    (let [segment (first path) tail (subvec path 1)
          key (cond
                (and (map? value) (not (record? value))) segment
                (and (vector? value) (integer? segment) (<= 0 segment) (< segment (count value))) segment
                :else (fail!))]
      (if (seq tail)
        (if (contains? value key)
          (assoc value key (alter-path (get value key) tail operation replacement))
          (fail!))
        (case operation
          :set (assoc value key replacement)
          :remove (if (and (map? value) (contains? value key)) (dissoc value key) (fail!))
          (fail!))))))

(defn- apply-changes [view operations]
  (when-not (and (vector? operations) (<= (count operations) 4096)) (fail!))
  (reduce
   (fn [view [operation path replacement :as change]]
     (when-not (and (vector? change) (vector? path) (<= (count path) 128)
                    (or (and (= operation :set) (= (count change) 3))
                        (and (= operation :remove) (= (count change) 2)))) (fail!))
     (alter-path view path operation replacement))
   view operations))

(defn- valid-view? [view identity]
  (and (map? view) (= identity (identity-of view))
       (string? (:runtime-id view)) (some? (:show-id view))
       (every? #(and (integer? %) (<= 0 %)) (order-of view))))

(defn receive
  "Return :accepted, :duplicate or :resync without ever mutating the base.
  The socket owner checks connection identity before admitting each message."
  [current {:keys [kind token base-token identity view changes] :as envelope}]
  (let [reject {:status :resync :state current}]
    (try
      (when-not (and (= protocol (:protocol envelope)) (token? token)) (fail!))
      (if (and (= token (:token current)) (= identity (identity-of (:view current))))
        (if (or (= kind :patch) (and (= kind :full) (= view (:view current))))
          {:status :duplicate :state current}
          reject)
        (let [next-view
              (case kind
                :full view
                :patch (if (and current (= base-token (:token current))
                                 (= identity (identity-of (:view current))))
                          (apply-changes (:view current) changes) (fail!))
                (fail!))]
          (if (and (valid-view? next-view identity)
                   (not (and (= (:runtime-id next-view) (get-in current [:view :runtime-id]))
                             (neg? (compare (order-of next-view) (order-of (:view current)))))))
            {:status :accepted :state {:token token :view next-view}}
            reject)))
      (catch :default _ reject))))

(defn- ui-key [m k]
  ;; Many existing form controls use (keyword id) to look up a value by name.
  ;; Keep both entries addressable when the source map has both key types.
  (if (and (string? k) (not (contains? m (keyword k)))) (keyword k) k))

(defn presentation
  "Adapt a typed value for Studio controls. The exact decoded data stays in
  :server-projection. Reuse presentation subtrees shared by successive patches."
  ([value] (presentation value nil nil))
  ([value previous rendered]
   (cond
     (identical? value previous) rendered
     (transit/fraction? value) (transit/fraction-value value)
     (record? value) value
     (keyword? value) (subs (str value) 1)
     (map? value)
     (reduce-kv (fn [out k child]
                  (assoc out (ui-key value k)
                         (presentation child (get previous k)
                                       (get rendered (ui-key previous k)))))
                {} value)
     (sequential? value)
     (mapv (fn [i child] (presentation child (get previous i) (get rendered i)))
           (range) value)
     (set? value) (mapv presentation value)
     :else value)))
