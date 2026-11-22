(ns tolglow.venue-fog
  "Own one venue estimate independently of any Studio clients or display clock."
  (:require [afterglow.render.atmosphere :as fog]))

(def saved-path [:extensions ::state])

(defn- state-at [plan state now]
  (fog/advance plan state (max now (:sample-time-ms state))))

(defn- saved-state! [s]
  (let [amount? #(and (number? %) (Double/isFinite (double %)) (not (neg? %)))]
    (when-not (and (map? s) (amount? (:mixed-m3 s)) (vector? (:sources s))
                   (every? #(and (some? (:fixture-id %))
                                 (every? amount? (map % [:local-m3 :flow-m3-s :emitted-m3]))) (:sources s))
                   (= (count (:sources s)) (count (set (map :fixture-id (:sources s))))))
      (throw (ex-info "Invalid saved venue fog quantities" {})))
    s))

(defn validate-saved! [model]
  (when-let [s (get-in model saved-path)] (saved-state! s)))

(defn- configure! [owner model compiled-patch now restored?]
  (let [{:keys [rig epoch]} model
        current @owner]
    (when (or restored? (not= epoch (:epoch current)) (not (identical? rig (:rig current))))
      (locking owner
        (let [{old-plan :plan old :state} @owner
              saved (get-in model saved-path)
              prior (if restored? saved (if old (state-at old-plan old now) saved))
              enabled? (or prior (some :fogger (:fixtures rig)))
              plan (when enabled? (fog/plan (:room rig) compiled-patch))
              state (when plan (if prior (fog/reconfigure plan prior epoch now
                                                         (or restored? (not= (:epoch prior) epoch)))
                                 (fog/initial plan epoch now)))]
          (reset! owner {:rig rig :epoch epoch :plan plan :state state}))))))

(defn presented! [owner now universes]
  (when (:plan @owner)
    (locking owner
      (let [{:keys [plan state] :as current} @owner]
        (when plan (reset! owner (assoc current :state (fog/present plan state
                             (max now (:sample-time-ms state)) universes))))))))

(defn sample [owner now]
  (let [{:keys [plan state]} @owner]
    (when plan (state-at plan state now))))

(defn commit!
  "A physical show integrates the last successfully delivered bytes. A preview
  show integrates its completed DMX buffer on the injected show clock."
  [runtime next-state compiled-patch restored?]
  (let [{:keys [model output]} next-state physical? (some? (get-in runtime [:sink :send!]))
        now (if physical? ((get-in runtime [:publisher :now-ms])) (:time-ms model))
        owner (:venue-fog runtime)]
    (configure! owner model compiled-patch now restored?)
    (if (:plan @owner)
      (do
        (when (and (not physical?) (not restored?)) (presented! owner now (:universes output)))
        (assoc-in next-state [:output :atmosphere]
                  (assoc (sample owner now) :basis (if physical? :sent-dmx :rendered-dmx))))
      next-state)))
