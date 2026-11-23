(ns tolglow.effects.variable-blocks
  "Once-per-frame modulation of shared show variables, using the show clock.
  External sources retain their dry value across frames and save/restore."
  (:require [afterglow.render.ir :as ir]
            [afterglow.time :as time]
            [tolglow.compiler.blocks :as blocks]))

(defn compile-settings [{:keys [bindings prefabs]}]
  (when (seq bindings)
    (when-not (every? #(= :variables (first %)) (keys bindings))
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

(defn sample [plan model time-ms]
  (when plan
    (let [{:keys [beat meter]} (:transport model)
          env {:beat beat :elapsed-beats beat :meter (time/normalize-meter meter)
               :time-ms time-ms :elapsed-ms time-ms :position [0.0 0.0 0.0]
               :params (blocks/with-variables plan {} model)}]
      (reduce-kv (fn [out path evaluate]
                   (try (assoc-in out [:values path] (evaluate env))
                        (catch Exception error
                          (assoc-in out [:errors path] (.getMessage error)))))
                 {:env env :traces (:traces plan)} (:evaluate plan)))))

(defn resolved [model observation]
  (reduce-kv assoc-in model (:values observation)))

(defn inspect [observation path]
  (when-let [trace (get (:traces observation) path)]
    (let [env (:env observation)]
      (if-let [error (get-in observation [:errors path])]
        {:error error :time-ms (:time-ms env)}
        (assoc (trace env) :time-ms (:time-ms env))))))
