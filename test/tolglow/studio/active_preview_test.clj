(ns tolglow.studio.active-preview-test
  (:require [afterglow.render.frame :as render]
            [clojure.test :refer [deftest is]]
            [tolglow.show.preview :as preview]
            [tolglow.show :as show]
            [tolglow.studio :as studio]))

(defn- movers [n]
  (mapv (fn [i] {:id (keyword (str "mover-" i)) :universe 1 :address (inc (* 4 i))
                 :channels {:red 1 :blue 2 :pan 3 :tilt 4}}) (range n)))

(defn- pixels [n]
  (mapv (fn [i] {:id (keyword (str "pixel-" i)) :universe (+ 2 (quot i 512))
                 :address (inc (mod i 512)) :channels {:green 1}}) (range n)))

(defn- project-both [fixtures assignments]
  (let [patch (render/compile-patch fixtures)]
    (mapv (fn [options]
            (let [frame (render/resolve-frame patch assignments options)
                  bytes (update-vals (:universes frame) vec)
                  values (preview/values frame fixtures)]
              (is (= bytes (update-vals (:universes frame) vec)))
              {:values values :bytes bytes}))
          [{} {:include-values? false :compact-values? true}])))

(deftest sparse-head-and-multi-universe-strip
  (doseq [n [64 128]]
    (let [heads (movers n) last-id (:id (peek heads))
          fixtures (into heads (pixels 513))
          work [{:target-id last-id :attribute :red :value 1.0}
                {:target-id last-id :attribute :pan :value 0.75}
                {:target-id :pixel-512 :attribute :green :value 1.0}]
          [rich compact] (project-both fixtures work)]
      (is (= (:bytes rich) (:bytes compact)))
      (doseq [{:keys [values bytes]} [rich compact]]
        (is (= 16 (count values)))
        (is (= [1.0 0.0 0.0] (get-in values [last-id :emitted-color])))
        (is (= 90.0 (get-in values [last-id :pose :pan-deg])))
        (is (= [0.0 1.0 0.0] (get-in values [:pixel-512 :emitted-color])))
        (is (= -1 (first (get bytes 3))))))))

(deftest dense-strip-projects-bounded-stable-emissions-with-room-for-movers
  (let [strip (pixels 1024) heads (movers 64) head (:id (peek heads))
        fixtures (into strip heads)
        work (into [{:target-id head :attribute :red :value 0.25}]
                   (map #(hash-map :target-id (:id %) :attribute :green :value 1.0) strip))
        [rich compact] (project-both fixtures work)]
    (is (= (set (keys (:values rich))) (set (keys (:values compact)))))
    (let [ids (set (keys (:values rich)))]
      (is (= 128 (count ids)))
      (is (contains? ids head))
      (is (= #{1 2 3} (into #{} (comp (filter #(ids (:id %))) (map :universe)) fixtures)))
      (is (= ids (set (keys (preview/values (render/resolve-frame (vec (reverse fixtures)) work)
                                            fixtures))))))))

(deftest missing-or-explicitly-dark-output-does-not-produce-an-emitter
  (let [fixtures (movers 64)
        output {:target-order (mapv :id fixtures) :values {:mover-62 {:intensity 1.0 :color [1 1 1]}}}]
    (is (empty? (preview/values output fixtures)))))

(deftest ring-inspection-and-websocket-projection-share-the-same-emitted-targets
  (let [runtime (show/create-runtime)
        fixtures (movers 64)
        id (:id (peek fixtures))
        frame (render/resolve-frame fixtures [{:target-id id :attribute :blue :value 1.0}])]
    (try
      (swap! (:state runtime) #(-> % (assoc-in [:model :rig :fixtures] fixtures) (assoc :output frame)))
      (let [expected (get-in (show/project-snapshot runtime @(:state runtime)) [:output :values])
            view (studio/browser-snapshot runtime {})
            query {"runtime-id" (:runtime-id view) "revision" (str (:revision view)) "epoch" (str (:epoch view))}]
        (is (contains? expected id))
        (is (= expected (get-in view [:output :values]) (get-in (studio/browser-snapshot runtime query) [:output :values]))))
      (finally (show/stop! runtime)))))

(deftest fixed-emitters-retain-coverage-in-a-saturated-moving-rig
  (let [fixtures (into (movers 128) (pixels 50))
        work (mapv (fn [{:keys [id channels]}]
                     {:target-id id :attribute (if (:pan channels) :red :green) :value 1.0}) fixtures)]
    (doseq [{:keys [values]} (project-both fixtures work)]
      (is (= 128 (count values)))
      (is (= 16 (count (filter #(pos? (second (:emitted-color %))) (vals values))))))))
