(ns tolglow.development.observation
  "Demand-driven readers of immutable development service snapshots.")

(def unavailable {:result (delay {:error :unavailable})})

(defn selected [state select project]
  (let [token (Object.)]
    {:source! #(select @state)
     :sample! project
     :observe! (fn [wake!]
                 (add-watch state token
                            (fn [_ _ before after]
                              (when-not (identical? (select before) (select after)) (wake!))))
                 #(remove-watch state token))}))
