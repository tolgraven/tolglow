(ns tolglow.studio.volume-optics-test
  (:require [cljs.test :refer-macros [deftest is]]
            [tolglow.studio.volume-optics :as optics]))

(def ring {:shape :difference
           :parts [{:shape :rounded-box :position [0 0 0] :size [2.6 2.2 1.5] :rounding 0.84}
                   {:shape :sphere :position [0 0 0] :radius 0.55 :scale [1 1 4]}]})
(defn ball [position] {:volume {:shape :ball :position position :radius 1 :geometry ring}
                       :paint {:color [0.1 0.6 0.9]} :position position :deformation 0.1})

(deftest motion-tint-and-strength-reuse-sampled-compound-geometry
  (let [initial (optics/prepare nil {"a" (ball [0 1 0])})
        moved (optics/prepare initial {"a" (assoc (ball [2 2 0]) :paint {:color [1 0 0]})
                                     "b" (ball [1 4 -2])})
        done (optics/prepare moved {})]
    (is (identical? (:meshes initial) (:meshes moved)))
    (is (= 1 (count (:meshes moved))))
    (doseq [[position object] (map vector [[2 2 0] [1 4 -2]] (:objects moved))]
      (is (every? #(< (abs %) 1e-9) (map - position (:position object)))))
    (is (= [1 0 0] (get-in moved [:objects 0 :color])))
    (is (empty? (:meshes done)))
    (is (empty? (:objects done)))))

(deftest zero-and-nonspatial-signals-do-not-allocate-optical-meshes
  (let [result (optics/prepare nil {:unrelated {:speed 2}})]
    (is (empty? (:meshes result)))
    (is (empty? (:objects result)))))

(deftest restarted-sampling-does-not-reuse-an-old-geometry-id
  (let [initial (optics/prepare nil {:body (ball [0 0 0])})
        paused (optics/prepare initial nil)
        resumed (optics/prepare paused {:body {:volume {:shape :sphere :radius 1 :position [0 0 0]}}})]
    (is (not= (get-in initial [:objects 0 :mesh]) (get-in resumed [:objects 0 :mesh])))))

(deftest crowded-scenes-display-their-cap-without-truncating-the-show
  (let [signal {:volume {:shape :sphere :radius 1 :position [0 0 0]}}
        prepared #(optics/prepare nil (zipmap (range %) (repeat signal)))]
    (is (false? (:limited? (prepared optics/max-visible))))
    (let [{:keys [limited? objects meshes]} (prepared (inc optics/max-visible))]
      (is limited?)
      (is (= optics/max-visible (count objects)))
      (is (= 1 (count meshes))))))
