(ns tolglow.perf.weighted-live.scene
  "The mixed material, eight washes and four moving volumes of the Mac venue replay."
  (:require [tolglow.bench-show :as bench]
            [tolglow.perf.weighted-venue :as weighted]
            [tolglow.show :as show]))

(def selection #{:position :beam})

(defn- launch [model effect cue]
  (let [id (:id effect)]
    (-> model
        (bench/command [id :publish] {:op :registry/publish :kind :effects :definition effect})
        (bench/command [id :cue] {:op :cue/put :cue (assoc cue :effect-id id)})
        (bench/command [id :run] {:op :cue/launch :cue-id (:id cue)}))))

(defn- mix-chart [model]
  (let [place (fn [i fixture]
                (cond-> fixture (even? i)
                  (assoc :surface-uv [(/ (mod i 113) 112.0) (/ (mod i 17) 16.0)])))
        chart {:id :projected-white :version 1 :params {} :outputs {:white 0.0}
               :prefabs {:rig/chart {:mode :replace :glsl "0.5 + 0.1 * sin(world.z) + 0.2 * sin(uv.x) + 0.15 * sin(uv.y)"}}
               :blocks {[:outputs :white] {:stages [{:block :rig/chart}]}}}]
    (-> model (update-in [:rig :fixtures] #(mapv place (range) %))
        (launch chart {:id :chart}))))

(defn- wash [model i]
  (let [id (keyword (str "wash-" i))
        effect {:id id :version 1 :outputs {:color [(/ (inc i) 21.0) 0.18 0.37] :white (/ (inc i) 20.0)}}]
    (launch model effect {:id id :priority (+ 20 i)})))

(defn build [pixels]
  (let [base (:model (weighted/venue pixels))
        model (weighted/seed-model (reduce wash (mix-chart base) (range 8))
                                   (weighted/volumes 4 selection 0.0))
        by-cue (into {} (map (juxt :cue-id :id)) (vals (:instances model)))
        ids (mapv (comp by-cue weighted/field-id) (range 4))]
    (when-not (every? some? ids) (throw (ex-info "Four launched volume cues are required" {:ids ids})))
    {:model model :volume-ids ids}))

(defn move!
  "One control sample submits at most four absolute positions. Later samples
  overwrite elapsed motion by computing from current beat, without a catch-up queue."
  [host ids beat]
  (mapv (fn [id volume]
          (show/submit! host {:op :param/set :instance-id id :param :position :value (:position volume)}))
        ids (weighted/volumes 4 selection beat)))
