(ns tolglow.studio.rig-test
  (:require [clojure.test :refer [deftest is]]
            [tolglow.studio.rig :as rig]
            [tolglow.studio.persistence :as persistence]
            [tolglow.studio.demo :as demo]))

(def scene
  {:fixtures [{:id "par" :position [0 3 0] :universe 1 :address 510
               :channels {:red 1 :green 2 :blue 3 :white 4} :footprint 5}]
   :room {:width 12 :height 5 :depth 10 :name "Small club" :surface {:finish :concrete}}
   :trusses [{:id :back :position [0 3.5 -2] :length 8 :rotation-deg [0 30 0]
              :source {:manufacturer "User library" :tags #{:square :aluminium}}}]
   :survey {:units :metres :references '("scan-a" "scan-b") :confidence 3/4}})

(deftest valid-rig-keeps-imported-metadata-and-optional-headless-scene
  (is (empty? (rig/errors scene)))
  (is (identical? scene (rig/validate-rig scene)))
  (is (= {:fixtures []} (rig/validate-rig {:fixtures []})))
  (is (= {:fixtures [] :trusses []} (rig/validate-rig {:fixtures [] :trusses []})))
  (is (= (get-in scene [:trusses 0 :source])
         (get-in (rig/validate-rig scene) [:trusses 0 :source]))))

(deftest geometry-errors-identify-the-field-without-filling-defaults
  (doseq [[path invalid] [[[:room :width] 0] [[:room :height] -1] [[:room :depth] Double/POSITIVE_INFINITY]
                          [[:room] {:width 12}] [[:room] nil] [[:trusses] nil]
                          [[:trusses 0 :length] 0] [[:trusses 0 :length] "8"]
                          [[:trusses 0 :position] [0 1]] [[:trusses 0 :position] [0 Double/NaN 0]]
                          [[:trusses 0 :rotation-deg] [0 30]] [[:trusses 0 :rotation-deg] [0 "30" 0]]
                          [[:trusses 0 :id] ""]]]
    (let [invalid-scene (assoc-in scene path invalid)]
      (is (seq (rig/errors invalid-scene)) (pr-str path))
      (is (thrown? clojure.lang.ExceptionInfo (rig/validate-rig invalid-scene)) (pr-str path))))
  (is (= [:trusses 0 :length] (:path (first (rig/errors (assoc-in scene [:trusses 0 :length] -1))))))
  (is (empty? (rig/errors (update-in scene [:trusses 0] dissoc :rotation-deg))))
  (is (some #(= :duplicate-truss-id (:code %))
            (rig/errors (update scene :trusses conj (first (:trusses scene)))))))

(deftest fixture-validation-uses-the-actual-complete-render-patch
  (let [overlap (update scene :fixtures conj {:id :other :position [0 0 0]
                                             :universe 2 :address 1 :channels {:dimmer 1}})]
    (is (some #(= :invalid-patch (:code %)) (rig/errors overlap)))
    (is (thrown? clojure.lang.ExceptionInfo (rig/validate-rig overlap))))
  (is (some #(= :invalid-fixture (:code %)) (rig/errors {:fixtures [12]})))
  (is (some #(= :invalid-fixtures (:code %)) (rig/errors {:fixtures {}})))
  (is (some #(= :invalid-patch (:code %))
            (rig/errors (assoc-in scene [:fixtures 0 :channels :red] [1 1])))))

(deftest portable-metadata-is-required-at-live-rig-admission
  (doseq [value [(atom {}) (fn [] :x) (Object.) (byte-array 2) 'unresolved-symbol]]
    (is (some #(= :nonportable (:code %)) (rig/errors (assoc scene :imported value)))))
  (is (some #(= :nonfinite (:code %)) (rig/errors (assoc scene :survey-value Double/NaN)))))

(deftest full-show-restoration-reuses-rig-validation-and-keeps-scene-data
  (let [model (assoc (demo/model) :rig scene)
        document (persistence/freeze model)
        restored (persistence/thaw (demo/model) document)]
    (is (= scene (:rig restored)))
    (doseq [[path value] [[[:room :height] 0] [[:trusses 0 :position] [1 2]]
                          [[:trusses 0 :length] -1]]]
      (let [broken (assoc-in document (into [:model :rig] path) value)
            problem (try (persistence/thaw (demo/model) broken) nil
                         (catch clojure.lang.ExceptionInfo error (ex-data error)))]
        (is (= :invalid-show (:persistence/error problem)))
        (is (some #(= (into [:rig] path) (:path %)) (:errors problem)))))))
