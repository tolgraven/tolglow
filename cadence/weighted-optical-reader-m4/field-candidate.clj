(in-ns 'afterglow.spatial.field)

(defn- sample-volume-candidate [dispatch-reader volume rig {:keys [values] :as observations} emissions]
  (let [sampling (prepare volume)
        initial (position-weights sampling rig)
        optical (disj (:selection sampling) :position)]
    (if (empty? optical)
      {:weights initial :aims {}}
      (let [prepared (or (::optics observations) (::optics (prepare-observations observations)))
            {:keys [fixtures origins]} @(::optical-rig rig)
            opts (beam/volume-options prepared optical sampling origins)
            read-row (or values {})
            receive (if emissions
                      (dispatch-reader volume sampling opts)
                      #(beam/contribution % (read-row (:id %)) volume sampling opts))
            aims (volatile! (transient {}))
            add-beam (fn [weights source]
                       (if-let [{:keys [weight aim]} (receive source)]
                         (let [id (:id source)]
                           (when aim (vswap! aims assoc! id aim))
                           (assoc! weights id (max weight (get weights id 0.0))))
                         weights))]
        {:weights (persistent! (reduce add-beam (transient initial) (if emissions @emissions fixtures)))
         :aims (persistent! @aims)}))))
