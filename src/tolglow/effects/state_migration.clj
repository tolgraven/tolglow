(ns tolglow.effects.state-migration
  "Portable effect-state migrations for the :migrate hot-reload policy.

  An effect definition declares data, never a function:

    :state-migration {:from  [1 2]   ; optional accepted previous versions
                      :steps [{:op :rename  :from [:count] :to [:frames]}
                              {:op :assoc   :path [:mode] :value :smooth}
                              {:op :default :path [:phase] :value 0}
                              {:op :dissoc  :path [:legacy]}
                              {:op :scale   :path [:frames] :by 2}]}

  `compile` turns it into Afterglow's pure (old-state old-version new-version
  context) -> new-state function. Steps apply in order. Runtime failures carry
  the authored path, such as [:state-migration :steps 4 :path]."
  (:refer-clojure :exclude [compile])
  (:require [afterglow.control.model :as control]))

(def ops #{:assoc :default :dissoc :rename :scale})

(defn- error [path message] {:path path :message message})

(defn- finite-number? [x] (and (number? x) (Double/isFinite (double x))))

(defn- state-path? [path]
  (and (vector? path) (<= 1 (count path) 8)
       (every? #(or (keyword? %) (and (string? %) (seq %)) (and (integer? %) (not (neg? %)))) path)))

(defn- portable? [value]
  (cond
    (or (nil? value) (boolean? value) (string? value) (keyword? value) (symbol? value)) true
    (number? value) (finite-number? value)
    (map? value) (every? (fn [[k v]] (and (portable? k) (portable? v))) value)
    (coll? value) (every? portable? value)
    :else false))

(defn- step-errors [i {:keys [op] :as step}]
  (let [path [:state-migration :steps i]
        need-path (fn [k] (when-not (state-path? (get step k))
                            [(error (conj path k) "Use a vector of up to 8 keyword, string or index keys")]))]
    (if-not (and (map? step) (ops op))
      [(error (conj path :op) "Use :assoc, :default, :dissoc, :rename or :scale")]
      (concat
       (case op
         :rename (concat (need-path :from) (need-path :to))
         (need-path :path))
       (when (and (#{:assoc :default} op)
                  (not (and (contains? step :value) (portable? (:value step)))))
         [(error (conj path :value) "Use a portable data value")])
       (when (and (= :scale op) (not (finite-number? (:by step))))
         [(error (conj path :by) "Scale by a finite number")])))))

(defn errors
  "Validate a :state-migration declaration as portable data."
  [migration]
  (cond
    (not (map? migration)) [(error [:state-migration] "A state migration must be a map")]
    :else
    (concat
     (when-let [extra (seq (remove #{:from :steps} (keys migration)))]
       [(error [:state-migration (first extra)] "Use only :from and :steps")])
     (when (and (contains? migration :from)
                (not (and (vector? (:from migration)) (seq (:from migration))
                          (every? #(and (integer? %) (pos? %)) (:from migration)))))
       [(error [:state-migration :from] "List the accepted previous versions as positive integers")])
     (if-not (and (vector? (:steps migration)) (<= (count (:steps migration)) 64))
       [(error [:state-migration :steps] "Use a vector of at most 64 steps")]
       (mapcat step-errors (range) (:steps migration))))))

(defn reload-policy-errors
  "Validate an authored cue or launch reload policy against the engine's set."
  [policy]
  (when-not (control/reload-policies policy)
    [(error [:reload-policy] "Use :pinned, :preserve-state, :restart or :migrate")]))

(defn- fail! [path message data]
  (throw (ex-info message (assoc data :path path))))

(defn- apply-step [state i {:keys [op path value by] :as step}]
  (let [at [:state-migration :steps i]]
    (case op
      :assoc (assoc-in state path value)
      :default (if (some? (get-in state path)) state (assoc-in state path value))
      :dissoc (if (= 1 (count path))
                (dissoc state (first path))
                (update-in state (pop path) #(if (map? %) (dissoc % (peek path)) %)))
      :rename (let [found (get-in state (:from step) ::absent)]
                (if (= ::absent found) state
                    (-> (apply-step state i {:op :dissoc :path (:from step)})
                        (assoc-in (:to step) found))))
      :scale (let [current (get-in state path)]
               (when-not (finite-number? current)
                 (fail! (conj at :path) "Scaled state must be a finite number"
                        {:state-path path :found (if (nil? current) :absent (.getName (class current)))}))
               (let [scaled (* current by)]
                 (when-not (finite-number? scaled)
                   (fail! (conj at :by) "Scaled state must stay finite" {:state-path path}))
                 (assoc-in state path scaled))))))

(defn compile
  "Return the engine migration function for a validated declaration."
  [migration]
  (let [problems (errors migration)]
    (when (seq problems)
      (throw (ex-info "Invalid state migration" {:errors (vec problems)}))))
  (let [accepted (some-> (:from migration) set)
        steps (:steps migration)]
    (fn migrate-state [state old-version _new-version _context]
      (when (and accepted (not (accepted old-version)))
        (fail! [:state-migration :from] "No state migration from this version" {:from-version old-version}))
      (when-not (or (map? state) (nil? state))
        (fail! [:state-migration] "Effect state must be a map to migrate" {}))
      (reduce-kv (fn [state i step] (apply-step state i step)) (or state {}) steps))))
