(ns tolglow.studio.volume-sculpt-test
  (:require [cljs.test :refer-macros [deftest is]]
            [afterglow.spatial.shape :as shape]
            [re-frame.core :as rf]
            [re-frame.db :as rf-db]
            [tolglow.studio.spatial-drafts :as drafts]
            [tolglow.studio.volume-geometry :as geo]
            [tolglow.studio.volume-sculpt :as sculpt]))

(def db {:connection {:connected? true} :ui {:page-id "main"}
         :server-projection {:view {:runtime-id "runtime" :epoch 3 :show-id :stage :pages [{:id :main}]}}})

(deftest preview-samples-the-actual-rounded-and-combined-distance-field
  (let [cube (assoc (geo/primitive :rounded-box) :position [0 1 0] :rounding 0.8)
        union (geo/combine (assoc cube :selection #{:beam} :strength 0.45) :intersection :sphere)
        sculpted (assoc union :scale [2 1 1] :translation [1 0 0] :falloff {:outer-m 0.2})
        {:keys [distance]} (shape/prepare sculpted)
        guide (geo/guides sculpted)]
    (is (= #{:beam} (:selection union)))
    (is (= 0.45 (:strength union)))
    (is (seq (:surface guide)))
    (is (seq (:outer guide)))
    (is (every? #(< (abs (distance %)) 0.025) (:surface guide)))
    (is (every? #(< (abs (- 0.2 (distance %))) 0.025) (:outer guide)))
    (is (even? (count (:surface guide))))
    (is (nil? (geo/guides (assoc union :parts [cube (assoc (geo/primitive :sphere) :position [50 0 0])]))))))

(deftest bouncing-attached-geometry-preserves-world-pose-and-field-controls
  (let [spec (assoc (geo/primitive :rounded-box) :position [2 3 1] :scale [1 2 3] :strength 0.6)
        ball (geo/moving spec true)
        unwrapped (geo/moving ball false)]
    (is (= :ball (:shape ball)))
    (is (= 0.6 (:strength ball)))
    (is (= (:bounds (shape/prepare spec)) (:bounds (shape/prepare ball)) (:bounds (shape/prepare unwrapped))))
    (is (= [2 3 1] (:position ball)))))

(deftest disabling-motion-retains-the-world-boundary-and-feather-distances
  (let [child {:shape :difference :scale [1 3 1] :translation [0.3 -0.1 0.5]
               :parts [{:shape :rounded-box :position [0.5 0 0] :size [2 1 3] :rounding 0.4}
                       {:shape :sphere :position [0.4 0.1 0] :radius 0.35}]}
        base {:shape :ball :position [2 3 -1] :radius 2 :scale [2 1 1.5]
              :translation [-1 0.5 0.25] :falloff {:outer-m 1.1} :strength 0.8}
        points [[1 3.5 -0.75] [0 0 0] [0.6 2.7 0.1] [2 3.1 1] [2.4 3.5 2.6] [4 5 3]]]
    (doseq [ball [base (assoc base :geometry child)]]
      (let [still (geo/moving ball false)
            before (shape/prepare ball)
            after (shape/prepare still)]
        (is (= (:falloff ball) (:falloff still)))
        (is (= (:strength ball) (:strength still)))
        (doseq [edge [:min :max]]
          (is (every? #(< (abs %) 1e-10) (map - (get-in before [:bounds edge]) (get-in after [:bounds edge])))))
        (doseq [p points]
          (is (= ((:inside? before) p) ((:inside? after) p)))
          (is (< (abs (- ((:distance before) p) ((:distance after) p))) 1e-10)))))))

(deftest combining-the-whole-ball-edits-its-attached-shape
  (let [restore (rf/make-restore-fn)
        ball {:shape :ball :position [2 3 1] :radius 0.9 :velocity [1 2 0] :restitution 0.6
              :scale [2 1 1] :translation [0.5 0 0] :falloff {:outer-m 0.3} :selection #{:position}}
        points [[0.9 3 1] [3.1 3 1] [5.3 3 1] [6.5 3 1]]]
    (try
      (doseq [attached [nil (geo/primitive :rounded-box)]
              [operation included] [[:union [true true true false]]
                                    [:intersection [false true false false]]
                                    [:difference [true false false false]]]]
        (let [volume (cond-> ball attached (assoc :geometry attached))
              draft (assoc sculpt/initial :volume volume)]
          (reset! rf-db/app-db (drafts/update-ui db assoc :sculpt draft))
          (rf/dispatch-sync [::sculpt/combine operation])
          (let [{composed :volume :keys [path]} (sculpt/model @rf-db/app-db)
                {:keys [inside?]} (shape/prepare composed)
                publication (sculpt/publish @rf-db/app-db :cue "combine")]
            (is (= ball (dissoc composed :geometry)))
            (is (= operation (get-in composed [:geometry :shape])))
            (is (= (or attached (assoc (geo/primitive :sphere) :radius (:radius ball)))
                   (get-in composed [:geometry :parts 0])))
            (is (= [:geometry :parts 1] path))
            (is (= included (mapv inside? points)))
            (is (= composed (get-in publication [:studio/http :body :volume]))))
          (rf/dispatch-sync [::sculpt/undo])
          (is (= draft (sculpt/model @rf-db/app-db)))))
      (finally (restore)))))

(deftest intersection-places-the-new-part-next-to-the-translated-attachment
  (let [restore (rf/make-restore-fn)
        volume (merge (:volume sculpt/initial) (geo/primitive :rounded-box)
                      {:position [0 1.5 0] :size [0.5 0.5 0.5]})
        draft (assoc sculpt/initial :volume volume)]
    (try
      (reset! rf-db/app-db (drafts/update-ui db assoc :sculpt draft))
      (rf/dispatch-sync [::sculpt/moving true])
      (doseq [path [[] [:geometry]]]
        (rf/dispatch-sync [::sculpt/set [:path] path])
        (let [{ball :volume :as before} (sculpt/model @rf-db/app-db)]
          (rf/dispatch-sync [::sculpt/combine :intersection])
          (let [{combined :volume selected :path} (sculpt/model @rf-db/app-db)
                [first-part new-part] (get-in combined [:geometry :parts])
                {:keys [inside?]} (shape/prepare combined)]
            (is (= [:geometry :parts 1] selected))
            (is (= (dissoc ball :geometry) (dissoc combined :geometry)))
            (is (= (:geometry ball) first-part))
            (is (= [0.6 0 0] (:position new-part)))
            (is (inside? [0 1.5 0]))
            (is (seq (:surface (geo/guides combined)))))
          (rf/dispatch-sync [::sculpt/undo])
          (is (= before (sculpt/model @rf-db/app-db)))))
      (finally (restore)))))

(deftest drag-and-undo-stage-once-and-cancel-restores-the-original
  (let [start (sculpt/gesture db {:type "sculpt-begin" :token 7 :point [0 1 0]})
        move (sculpt/gesture start {:type "sculpt-move" :token 7 :point [2 1 1]})
        stretch (sculpt/gesture start {:type "sculpt-move" :token 7 :axis 2 :half-extent 1 :delta 0.5})
        finish (sculpt/gesture move {:type "sculpt-end" :token 7})
        cancel (sculpt/gesture move {:type "sculpt-cancel" :token 7})]
    (is (= [2 0 1] (get-in (sculpt/model finish) [:volume :translation])))
    (is (= [1 1 1.5] (get-in (sculpt/model stretch) [:volume :scale])))
    (is (= [(:volume sculpt/initial)] (mapv :volume (:undo (sculpt/model finish)))))
    (is (= sculpt/initial (sculpt/model cancel)))
    (is (= move (sculpt/gesture move {:type "sculpt-cancel" :token 9})))
    (is (= (:server-projection db) (:server-projection move)))))

(deftest spawn-uses-typed-registry-ids-and-fences-repeat-requests
  (let [draft (assoc sculpt/initial :members [{:cue-id :floor/amber} {:effect-id "main/sweep"}]
                     :cue-id "geo" :sources {:position [1 2 3] :color {:hsl [0.5 1 0.4]}})
        staged (drafts/update-ui db assoc :sculpt draft)
        {next :db request :studio/http} (sculpt/publish staged :cue "request")
        body (:body request)]
    (is (= "/api/spatial/cues" (:path request)))
    (is (true? (:typed? request)))
    (is (= [:spawn "geo" :main "runtime" 3] (mapv body [:op :id :page-id :runtime-id :expected-epoch])))
    (is (= (:members draft) (:members body)))
    (is (= (:sources draft) (:sources body)))
    (is (nil? (sculpt/publish next :cue "again")))
    (is (nil? (sculpt/publish (assoc-in db [:connection :connected?] false) :cue "off")))
    (is (= "/api/settings" (get-in (sculpt/publish staged :volume "save") [:studio/http :path])))))
