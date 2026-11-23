(ns tolglow.effects.variable-blocks
  "Once-per-frame modulation of shared show variables, using the show clock.
  External sources retain their dry value across frames and save/restore."
  (:require [afterglow.render.ir :as ir]
            [afterglow.runtime.events :as events]
            [afterglow.time :as time]
            [tolglow.compiler.blocks :as blocks]))

(defn compile-settings [{:keys [bindings prefabs] :as settings}]
  (when-not (or (nil? settings)
                (and (map? settings) (every? #{:bindings :prefabs} (keys settings))
                     (or (nil? bindings) (map? bindings))))
    (throw (ex-info "Variable blocks require a settings map with named bindings" {})))
  (when (seq bindings)
    (when-not (every? #(and (vector? %) (= :variables (first %))) (keys bindings))
      (throw (ex-info "Show-wide bindings require paths starting with :variables" {})))
    (doseq [path (keys bindings)
            length (range 2 (count path))]
      (when (contains? bindings (subvec path 0 length))
        (throw (ex-info "One binding would replace the parent of another" {:path path}))))
    (let [no-params (fn [_ _ path] (throw (ex-info "Use show variables as global block inputs" {:path path})))
          plan (blocks/prepare {:blocks bindings :prefabs prefabs} [] no-params)]
      (assoc plan :evaluate (update-vals (:expressions plan) ir/compile-expression)))))

(defn prepare! [cache settings]
  (let [cached @cache]
    (if (= settings (:settings cached))
      (:plan cached)
      (let [plan (compile-settings settings)]
        (vreset! cache {:settings settings :plan plan})
        plan))))

(defn- environment [plan model time-ms]
  (let [{:keys [beat meter]} (:transport model)]
    (blocks/sample-tweens
     plan {:beat beat :elapsed-beats beat :meter (time/normalize-meter meter)
           :time-ms time-ms :elapsed-ms time-ms :position [0.0 0.0 0.0]
           :params (blocks/with-variables plan {} model)})))

(defn sample
  ([plan model time-ms] (sample plan model time-ms nil))
  ([plan model time-ms previous]
   (when plan
     (let [env (environment plan model time-ms)
           same? (and (= (:epoch model) (:epoch previous)) (identical? (:traces plan) (:traces previous)))
           last-values (when same? (:values previous))
           read-path (fn [out path evaluate]
                       (try (assoc-in out [:values path] (evaluate env))
                            (catch Exception error
                              (cond-> (assoc-in out [:errors path] (.getMessage error))
                                (contains? last-values path)
                                (assoc-in [:values path] (get last-values path))))))]
       (reduce-kv read-path {:epoch (:epoch model) :env env :traces (:traces plan)} (:evaluate plan))))))

(defn collect-events [model before after]
  (if (or (:errors before) (:errors after))
    (let [before (when (= (:epoch model) (:epoch before)) (:errors before))
          failed (for [[path message] (:errors after) :when (not= message (get before path))]
                   {:type :variable-block/failed :path path :message message
                    :held? (contains? (:values after) path)})
          recovered (for [path (keys before) :when (and (contains? (:values after) path)
                                                       (not (contains? (:errors after) path)))]
                      {:type :variable-block/recovered :path path})]
      (events/collect model :modulation {:kind :show :id :variable-blocks} (concat failed recovered)))
    model))

(defn resolved [model observation]
  (reduce-kv assoc-in model (:values observation)))

(defn inspect [observation path]
  (when-let [trace (get (:traces observation) path)]
    (let [env (:env observation)]
      (if-let [error (get-in observation [:errors path])]
        (cond-> {:error error :time-ms (:time-ms env)}
          (contains? (:values observation) path) (assoc :held? true :result (get-in observation [:values path])))
        (assoc (trace env) :time-ms (:time-ms env))))))
