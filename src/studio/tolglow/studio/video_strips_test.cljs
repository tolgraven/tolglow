(ns tolglow.studio.video-strips-test
  (:require [cljs.test :refer-macros [deftest is testing]]
            [tolglow.studio.video-strips :as video]
            [tolglow.video.mapping :as mapping]))

(defn- close? [a b] (every? #(< (js/Math.abs %) 1e-9) (map - (flatten a) (flatten b))))

(deftest editor-edits-use-the-shared-mapping-geometry
  (let [strip {:id :tube-a :layout :tube-a :points [[0.25 0.5] [0.75 0.5]] :height 0.04}
        aspect (/ 16 9)]
    (is (close? [[0.35 0.4] [0.85 0.4]] (:points (video/edited-strip strip [:move [0.1 -0.1]] aspect))))
    (is (close? [[0.25 0.5] [0.6 0.7]] (:points (video/edited-strip strip [:point 1 [0.6 0.7]] aspect))))
    (is (= (mapping/rotate (:points strip) 15 aspect) (:points (video/edited-strip strip [:rotate 15] aspect))))
    (is (close? [[0.3 0.5] [0.7 0.5]] (:points (video/edited-strip strip [:length 0.8] aspect))))
    (is (= 0.1 (:height (video/edited-strip strip [:height 0.1] aspect))))
    (is (true? (:reverse? (video/edited-strip strip [:reverse true] aspect))))
    (testing "every edited strip still validates"
      (doseq [op [[:move [0.1 0]] [:rotate 90] [:length 2] [:filter :nearest] [:reverse false]]]
        (is (= [] (mapping/strip-errors (video/edited-strip strip op aspect) [])) (pr-str op))))))

(deftest new-strips-name-their-layout-and-the-frame-route-keeps-keyword-ids
  (let [strip (video/new-strip {:strips []} :tube-b)]
    (is (= :tube-b (:id strip) (:layout strip)))
    (is (= [] (mapping/strip-errors strip []))))
  (is (= ":test-grid" (video/source-param :test-grid)))
  (is (= ":syphon/arena" (video/source-param :syphon/arena)))
  (is (= [{:id :tube-a :pixels 2 :label "tube-a"}]
         (video/layout-choices {:pixel-layouts [{:id :tube-a}]
                                :fixtures [{:pixel-layout-id :tube-a} {:pixel-layout-id :tube-a} {:id :wash}]}))))

(deftest footprints-match-the-jvm-sampler-layout
  ;; Same expectations as tolglow.video-strips-test: the overlay draws what
  ;; the sampler reads.
  (let [prints (mapping/footprints [[0.25 0.5] [0.75 0.5]] 4 0.1 640 360)]
    (is (close? [200 180] (:center (first prints))))
    (is (close? [440 180] (:center (last prints))))
    (is (close? [80 36] [(:along (first prints)) (:across (first prints))]))))

(deftest edits-build-on-the-unsettled-draft-of-the-same-mapping
  (let [committed {:id :tube-2 :layout :tube-2 :points [[0 0.5] [1 0.5]] :height 0.08}
        db {:server-projection {:view {:runtime-id "r" :show-id :main :epoch 0
                                       :settings {:video {:mappings {:m {:id :m :source :s :strips [committed]}}}}}}}
        identity (get-in (video/ui-state db) [:identity])
        rotated (assoc committed :points [[0.5 0] [0.5 1]])]
    (is (= committed (video/current-strip db :m :tube-2)))
    (is (= rotated (video/current-strip (assoc db :video-strips-ui {:identity identity :draft rotated :draft-mapping :m}) :m :tube-2)))
    (is (= committed (video/current-strip (assoc db :video-strips-ui {:identity identity :draft rotated :draft-mapping :other}) :m :tube-2))
        "a draft from another mapping never applies")))

(deftest one-edit-is-in-flight-and-only-the-newest-waits
  (let [db {:server-projection {:view {:runtime-id "r" :show-id :main :epoch 0}}}
        first-edit (video/edit-fx db {:op :put-strip :mapping-id :m :strip {:id :a :height 0.1}})
        db1 (:db first-edit)
        second-edit (video/edit-fx db1 {:op :put-strip :mapping-id :m :strip {:id :a :height 0.2}})
        third-edit (video/edit-fx (:db second-edit) {:op :put-strip :mapping-id :m :strip {:id :a :height 0.3}})]
    (is (some? (:studio/http first-edit)))
    (is (nil? (:studio/http second-edit)))
    (is (= 0.3 (get-in third-edit [:db :video-strips-ui :queued :strip :height])))
    (is (= 0.3 (get-in third-edit [:db :video-strips-ui :draft :height])))))

(deftest reversed-strips-fill-footprints-in-projected-order
  ;; The projection lists colours by footprint along the points; reversal only
  ;; changes which target reads each footprint, so the overlay must not flip them.
  (let [strip {:id :a :layout :a :points [[0.1 0.5] [0.9 0.5]] :height 0.04 :reverse? true}
        overlay (#'video/strip-overlay strip ["#0000ff" "#ff0000"] 640 360 false (fn [_] nil))
        fills (keep #(when (and (vector? %) (= :polygon (first %))) (:fill (second %))) (tree-seq coll? seq overlay))]
    (is (= ["#0000ff" "#ff0000"] fills))))
