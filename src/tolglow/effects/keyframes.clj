(ns tolglow.effects.keyframes
  "Lifecycle and on-demand inspection of the host's committed predictive windows."
  (:require [afterglow.reactive :as graph]
            [afterglow.runtime.keyframes :as keyframes]
            [afterglow.runtime.keyframes.storage :as storage]))

(defn for-save [model windows]
  (reduce-kv (fn [out id window]
               (if (contains? (:instances out) id)
                 (assoc-in out [:instances id ::saved] (storage/freeze window)) out))
             model windows))

(defn restored [model]
  (let [rig (get-in model [:rig :fixtures])]
    (into {} (keep (fn [[id instance]]
                     (when-let [saved (::saved instance)]
                       [id (storage/restore saved rig (:epoch model))]))) (:instances model))))

(defn without-checkpoints [model]
  (if (some ::saved (vals (:instances model)))
    (update model :instances update-vals #(dissoc % ::saved))
    model))

(defn source [model windows]
  (if (seq windows) (vary-meta model assoc ::windows windows) model))

(defn inspect [context [_ id target attribute]]
  (let [model @(:db context)]
    (keyframes/inspect (get (::windows (meta model)) id) target attribute)))

(defn register! [system]
  (graph/reg-sub! system :runtime/keyframe inspect
                  {:id :runtime/keyframe :kind :sub
                   :implementation {:type :var :symbol "tolglow.effects.keyframes/inspect"}})
  system)
