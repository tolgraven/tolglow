(ns tolglow.effects.state-migration
  "Portable effect-state migrations for the :migrate hot-reload policy.

  An effect definition declares data, never a function. :from maps each
  previous version to the steps that bring its state to the next listed
  version (or to this definition's version for the highest key):

    {:version 4
     :state-migration {:from {1 [{:op :rename :from [:count] :to [:frames]}]
                              2 []
                              3 [{:op :default :path [:phase] :value 0}
                                 {:op :scale :path [:frames] :by 2}]}}}

  An instance on version 1 runs the lists for 1, 2 and 3 in order; one on 3
  runs only 3's. A version with no key has no migration and faults; list it
  with [] when its state already fits. Keys must be below the definition's
  own version. Steps are :assoc, :default, :dissoc, :rename and :scale.

  `compile` returns Afterglow's pure (old-state old-version new-version
  context) -> new-state function. Every runtime failure names its step, such
  as [:state-migration :from 3 1 :path]."
  (:refer-clojure :exclude [compile])
  (:require [afterglow.control.model :as control]
            [afterglow.runtime.engine :as engine]))

(def ops #{:assoc :default :dissoc :rename :scale})

(def ^:private step-limit 256)

(defn- error [path message] {:path path :message message})

(defn- finite-number? [x] (and (number? x) (Double/isFinite (double x))))

(defn- state-path? [path]
  (and (vector? path) (<= 1 (count path) 8)
       (every? #(or (keyword? %) (and (string? %) (seq %)) (and (integer? %) (not (neg? %)))) path)))

(defn- portable? [value]
  (cond
    (or (nil? value) (boolean? value) (string? value) (keyword? value) (symbol? value)) true
    (number? value) (finite-number? value)
    (record? value) false
    (map? value) (every? (fn [[k v]] (and (portable? k) (portable? v))) value)
    (or (vector? value) (set? value) (list? value)) (every? portable? value)
    :else false))

(defn- prefix? [a b] (and (<= (count a) (count b)) (= a (subvec b 0 (count a)))))

(defn- step-errors [path {:keys [op] :as step}]
  (let [need-path (fn [k] (when-not (state-path? (get step k))
                            [(error (conj path k) "Use a vector of up to 8 keyword, string or index keys")]))]
    (if-not (and (map? step) (ops op))
      [(error (conj path :op) "Use :assoc, :default, :dissoc, :rename or :scale")]
      (concat
       (case op
         :rename (let [errors (concat (need-path :from) (need-path :to))]
                   (if (seq errors) errors
                       (when (or (prefix? (:from step) (:to step)) (prefix? (:to step) (:from step)))
                         [(error (conj path :to) "Rename to a path that neither contains nor lies inside :from")])))
         (need-path :path))
       (when (and (#{:assoc :default} op)
                  (not (and (contains? step :value) (portable? (:value step)))))
         [(error (conj path :value) "Use a portable data value")])
       (when (and (= :scale op) (not (finite-number? (:by step))))
         [(error (conj path :by) "Scale by a finite number")])))))

(defn errors
  "Validate a :state-migration declaration for a definition at version."
  [migration version]
  (let [from (when (map? migration) (:from migration))]
    (cond
      (not (map? migration)) [(error [:state-migration] "A state migration must be a map")]
      (seq (remove #{:from} (keys migration)))
      [(error [:state-migration (first (remove #{:from} (keys migration)))] "Use only :from")]
      (not (and (map? from) (seq from)))
      [(error [:state-migration :from] "Map each previous version to a vector of steps")]
      :else
      (concat
       (mapcat (fn [[source steps]]
                 (let [path [:state-migration :from source]]
                   (cond
                     (not (and (integer? source) (pos? source)))
                     [(error path "Previous versions are positive integers")]
                     (not (and (integer? version) (< source version)))
                     [(error path (str "Migrate only from versions below this definition's version " version))]
                     (not (vector? steps)) [(error path "Use a vector of steps")]
                     :else (mapcat #(step-errors (conj path %1) %2) (range) steps))))
               (sort-by (comp str key) from))
       (when (> (reduce + (map #(if (vector? %) (count %) 0) (vals from))) step-limit)
         [(error [:state-migration :from] (str "Use at most " step-limit " steps in all"))])))))

(defn reload-policy-errors
  "Validate an authored cue or launch reload policy against the engine's set.
  nil means the default :pinned."
  [policy]
  (when-not (or (nil? policy) (control/reload-policies policy))
    [(error [:reload-policy] "Use :pinned, :preserve-state, :restart or :migrate")]))

(defn- fail! [path message data]
  (throw (ex-info message (assoc data :path path ::authored true))))

(defn- remove-path
  "Remove a map key. Absent or non-map parents leave the state unchanged."
  [state path]
  (let [parent (pop path)]
    (if (empty? parent)
      (dissoc state (peek path))
      (let [container (get-in state parent)]
        (if (and (map? container) (contains? container (peek path)))
          (assoc-in state parent (dissoc container (peek path)))
          state)))))

(defn- whole? [x]
  (or (instance? Long x) (instance? Integer x) (instance? Short x) (instance? Byte x)))

(defn- scale [current by at path]
  (when-not (finite-number? current)
    (fail! (conj at :path) "Scaled state must be a finite number"
           {:state-path path :found (if (nil? current) :absent (.getName (class current)))}))
  (if (and (whole? current) (whole? by))
    (try (Math/multiplyExact (long current) (long by))
         (catch ArithmeticException _
           (fail! (conj at :by) "Scaled integer state exceeds a 64-bit integer" {:state-path path})))
    ;; Ratios, big numbers and doubles become a double; the result must stay finite.
    (let [scaled (* (double current) (double by))]
      (when-not (Double/isFinite scaled)
        (fail! (conj at :by) "Scaled state must stay finite" {:state-path path}))
      scaled)))

(defn- apply-step [state at {:keys [op path value by] :as step}]
  (case op
    :assoc (assoc-in state path value)
    :default (if (some? (get-in state path)) state (assoc-in state path value))
    :dissoc (remove-path state path)
    :rename (let [found (get-in state (:from step) ::absent)]
              (cond
                (= ::absent found) state
                (not= ::absent (get-in state (:to step) ::absent))
                (fail! (conj at :to) "Rename target already holds a value" {:state-path (:to step)})
                :else (assoc-in (remove-path state (:from step)) (:to step) found)))
    :scale (assoc-in state path (scale (get-in state path) by at path))))

(defn- run-step [state at step]
  (try
    (apply-step state at step)
    (catch Exception error
      (if (::authored (ex-data error))
        (throw error)
        ;; assoc-in through a number/string or past a vector's end, for example.
        (throw (ex-info (str "State migration step " (name (:op step)) " failed: " (.getMessage error))
                        {:path at :op (:op step)} error))))))

(defn compile
  "Return the engine migration function for a validated declaration."
  [migration version]
  (let [problems (errors migration version)]
    (when (seq problems)
      (throw (ex-info "Invalid state migration" {:errors (vec problems)}))))
  (let [chain (vec (sort-by key (:from migration)))]
    (fn migrate-state [state old-version _new-version _context]
      (when-not (contains? (:from migration) old-version)
        (fail! [:state-migration :from] "No state migration from this version" {:from-version old-version}))
      (when-not (or (map? state) (nil? state))
        (fail! [:state-migration] "Effect state must be a map to migrate" {}))
      (reduce (fn [state [source steps]]
                (if (< source old-version) state
                    (reduce-kv (fn [state i step] (run-step state [:state-migration :from source i] step))
                               state steps)))
              (or state {}) chain))))

(defn attach
  "Attach the definition's compiled migration to step, once per declaration.
  cache is an optional owned atom; definitions without a migration pass through."
  [cache definition step]
  (if-let [migration (:state-migration definition)]
    (let [key [migration (:version definition)]
          migrate (or (some-> cache deref (get key))
                      (let [compiled (compile migration (:version definition))]
                        (when cache (swap! cache #(assoc (if (>= (count %) 256) {} %) key compiled)))
                        compiled))]
      (engine/with-state-migration step migrate))
    step))
