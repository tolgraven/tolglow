(ns tolglow.perf.independent-effects-test
  (:require [clojure.test :refer [deftest is]]
            [tolglow.perf.independent-effects :as bench]))

(defn- job [id] {:program {:id id :version 1} :instance {:id id}})

(defn- runs [jobs footprints]
  (mapv #(mapv (comp :id :program) %) (bench/phases jobs footprints)))

(deftest authored-dependencies-and-dynamic-programs-bound-parallel-batches
  (let [clock [:transport :beat] red [:head :color] dimmer [:head :dimmer] speed [:variables :rate]
        footprint (fn [reads writes] {:reads (set reads) :writes (set writes)})
        known {[:wash 1] (footprint [clock] [red])
               [:intensity 1] (footprint [clock] [dimmer])
               [:second-wash 1] (footprint [clock] [red])
               [:publish 1] (footprint [] [speed])
               [:follow 1] (footprint [speed] [])}]
    (is (= [[:wash :intensity] [:second-wash]]
           (runs (mapv job [:wash :intensity :second-wash]) known)))
    (is (= [[:publish] [:follow]] (runs (mapv job [:publish :follow]) known)))
    (is (= [[:follow] [:publish]] (runs (mapv job [:follow :publish]) known)))
    (doseq [opaque [(job :unknown)
                    (assoc-in (job :wash) [:program :version] 2)
                    (assoc (job :wash) :candidate? true)
                    (assoc-in (job :wash) [:instance :targets] [:head-2])
                    (assoc-in (job :wash) [:instance :source :native-layer] {:id :composite})]]
      (is (= [[:intensity] [(get-in opaque [:program :id])] [:intensity]]
             (runs [(job :intensity) opaque (job :intensity)] known))))))
