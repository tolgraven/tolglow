(ns tolglow.output-response
  "Group and global attack/release response of composed light, before budget
  allocation and metering. The show owner supplies time, previous history and
  the rendered frame; no clocks, devices or effects are invoked here."
  (:require [afterglow.render.emission :as emission]
            [afterglow.render.frame :as frame]
            [afterglow.time :as time]
            [tolglow.output-response.scopes :as scopes]))

(def defaults {:enabled? false :global {} :groups {} :fixtures {} :cues {} :compositions {}})
(defn- id? [x] (or (string? x) (keyword? x)))
(defn- magnitude? [x] (and (time/finite-number? x) (not (neg? x))))

(defn- duration-binding? [binding]
  (and (map? binding) (magnitude? (:default binding 0))
       (or (and (every? #{:variable :default} (keys binding))
                (vector? (:variable binding)) (seq (:variable binding)) (every? id? (:variable binding)))
           (and (every? #{:cue-id :param :default} (keys binding))
                (id? (:cue-id binding)) (id? (:param binding))))))

(defn- stage? [stage]
  (when (map? stage)
    (let [spec (dissoc stage :mode)
          [unit x] (first spec)
          blend? (= :blend unit)]
      (and (= 1 (count spec))
           (if blend? (not (contains? stage :mode))
             (and (time/units unit) (#{:exponential :linear} (:mode stage :exponential))))
           (if (duration-binding? x) (or (not blend?) (<= (:default x 0) 1))
             (and (magnitude? x) (or (not blend?) (<= x 1))))))))

(defn- envelope? [e]
  (and (map? e) (every? #{:attack :release} (keys e)) (every? stage? (vals e))))

(defn settings [opts]
  (let [{:keys [enabled? global] :as s} (merge defaults opts)]
    (when-not (and (map? opts) (every? (set (keys defaults)) (keys opts))
                   (boolean? enabled?) (envelope? global)
                   (every? (fn [key]
                             (let [entries (get s key)]
                               (and (map? entries) (every? id? (keys entries))
                                    (every? envelope? (vals entries))))) scopes/scopes))
      (throw (ex-info "Invalid output response scope or attack/release stage" {:values opts})))
    s))

(defn- magnitude [model binding]
  (if (number? binding) binding
    (let [v (if-let [path (:variable binding)]
              (get-in (:variables model) path)
              (when-let [instance (last (sort-by :order
                                                 (filter #(and (= (:cue-id binding) (:cue-id %))
                                                               (= :running (:phase %))) (vals (:instances model)))))]
                (get (:params instance) (:param binding)
                     (get-in instance [:definition :params (:param binding) :default]))))]
      (if (magnitude? v) v (:default binding 0)))))

(defn- stage-rate [model previous clock stage]
  (let [[unit source] (first (dissoc stage :mode))
        value (magnitude model source)
        linear? (= :linear (:mode stage))]
    [(cond
       (= unit :blend) (if (or (nil? previous) (> (:time-ms clock) (:time-ms previous))) (min 1.0 value) 0.0)
       (zero? value) 1.0
       :else (let [{:keys [unit value]} (time/normalize-duration {unit value} (:meter clock))
                   elapsed (max 0.0 (- (time/coordinate clock unit) (time/coordinate (or previous clock) unit)))
                   step (/ elapsed value)]
               (if linear? (min 1.0 step) (- (Math/expm1 (- step))))))
     linear?]))

(defn- scope-rates [configured model previous clock]
  (let [rates #(update-vals % (partial stage-rate model previous clock))]
    (reduce (fn [m kind]
              (reduce-kv #(assoc %1 [kind %2] (rates %3)) m (get configured kind)))
            {:global (rates (:global configured))} scopes/scopes)))

(defn- slower [[a linear-a :as left] [b _ :as right]]
  (cond (< a b) left (> a b) right linear-a right :else left))

(defn- stage-pair [rates [current tails]]
  (let [local (reduce #(merge-with slower %1 (get rates %2)) {} current)
        local (reduce #(merge-with slower %1 (select-keys (get rates %2) [:release])) local tails)
        stages (merge (:global rates) local)
        [a linear-a] (get stages :attack [(if (seq stages) 1.0 -1.0) false])
        [r linear-r] (get stages :release [(if (seq stages) 1.0 -1.0) false])]
    [a r linear-a linear-r]))

(defn- stage-coefficients [rates layout]
  (let [indices (:indices layout)]
    (if (<= (count indices) 1)
      (stage-pair rates (ffirst indices))
      (let [n (:count layout)
            attack (double-array n) release (double-array n)
            linear? (some (fn [entries] (some second (vals entries))) (vals rates))
            linear-a (when linear? (boolean-array n))
            linear-r (when linear? (boolean-array n))]
        (doseq [[ids ^ints rows] indices]
          (let [[rise fall exact-a exact-r] (stage-pair rates ids)]
            (dotimes [j (alength rows)]
              (let [i (aget rows j)]
                (aset-double attack i (double rise))
                (aset-double release i (double fall))
                (when linear?
                  (aset-boolean linear-a i exact-a)
                  (aset-boolean linear-r i exact-r))))))
        [attack release linear-a linear-r]))))

(defn- immediate-coefficients [coefficient size rows]
  (let [^doubles values (if (number? coefficient) (double-array size coefficient)
                          (aclone ^doubles coefficient))]
    (doseq [i rows] (aset-double values (int i) 1.0))
    values))

(defn advance
  "Follow physical emitter output at the supplied show time. prior-frame is
  the previous committed output from the same patch when enabling a new scope
  mid-show. Hard blackout and the effective master take precedence over tails.
  A rejected tracking follower goes dark immediately and resumes its attack
  from zero when it can track again. Retain the returned runtime history only
  for the same rig and epoch."
  ([previous model patch frame bypass?]
   (advance previous model patch frame bypass? (:master model 1.0) nil))
  ([previous model patch frame bypass? ceiling prior-frame]
   (advance previous model patch frame bypass? ceiling prior-frame nil))
  ([previous model patch frame bypass? ceiling prior-frame blocks]
   (let [opts (get-in model [:settings :output-response])]
     (if (or (not (:enabled? opts)) (not-any? #(seq (get opts %)) (cons :global scopes/scopes))
             bypass? (:blackout? model) (zero? ceiling))
       {:state nil :frame frame}
       (let [clock (time/frame-clock model)
             reset? (and previous
                         (or (not= (:epoch previous) (:epoch model))
                             (< (:time-ms clock) (get-in previous [:clock :time-ms]))
                             (< (:beat clock) (get-in previous [:clock :beat]))))
             prior (when-not reset? previous)
             rates (scope-rates opts model (:clock prior) clock)
             seed (when (and (not reset?) prior-frame (= (:target-order patch) (:target-order prior-frame)))
                    prior-frame)
             scoped (scopes/resolve-scopes model patch opts blocks (or seed (:scope-frame prior)) frame (:scoped prior))
             layout (:layout scoped)
             coefficients (if (and (identical? layout (:layout prior)) (= rates (:rates prior)))
                            (:coefficients prior)
                            (stage-coefficients rates layout))
             [attack release exact-a exact-r] coefficients
             ramps (when (or exact-a exact-r) {:attack exact-a :release exact-r :previous (:ramps prior)})
             suppressed (when-let [reports (:edge-tracking frame)]
                          (not-empty (into [] (keep (fn [{:keys [follower reason]}]
                                                     (when reason (get-in patch [:by-id follower ::frame/index]))))
                                           reports)))
             ;; Immediate coefficients record actual zero in the history even
             ;; for rows outside every configured scope. Keep cached rates intact.
             size (count (:target-order patch))
             result (emission/follow-frame patch frame (:levels prior) seed
                      (if suppressed (immediate-coefficients attack size suppressed) attack)
                      (if suppressed (immediate-coefficients release size suppressed) release)
                      ceiling ramps)]
         {:frame (:frame result)
          :state {:epoch (:epoch model) :clock clock :levels (:levels result)
                  :layout layout :scoped scoped :scope-frame (when (:writers scoped) (:frame result))
                  :rates rates :coefficients coefficients :ramps (:ramps result)}})))))
