(ns tolglow.studio.spatial-test
  (:require [cljs.test :refer-macros [deftest is]]
            [tolglow.studio.spatial :as spatial]
            [tolglow.studio.spatial-picker :as picker]))

(deftest author-with-typed-identities-and-scope
  (let [show {:runtime-id "r" :epoch 2 :show-id :s :cues [{:id :page/a} {:id "page/a"} {:id :page/b}]
              :settings {:spatial {:volumes {:orb (spatial/default-spec :ball)}}}}
        db {:server-projection {:view show} :connection {:connected? true}}
        request (:studio/http (spatial/save db {:bindings {:hit {:volume-id :orb :action {:preset :flicker}}}} "saved" nil nil))]
    (is (= :orb (spatial/active-id db)))
    (is (= :page/b (picker/anchor-id db "page/b")))
    (is (= :page/a (picker/anchor-id db :page/a)))
    (is (= "page/a" (picker/anchor-id db "page/a")))
    (is (= 2 (get-in request [:body :expected-epoch])))
    (is (= :orb (get-in request [:body :values :bindings :hit :volume-id])))
    (is (nil? (spatial/save (assoc db :connection {}) {} "" nil nil)))
    (is (= "ball-2" (spatial/fresh-id "ball" ["ball-1"])))
    (is (= {:attributes [:pan] :params {:mode :wash}} (picker/properties "{:attributes [:pan] :params {:mode :wash}}")))
    (is (thrown? js/Error (picker/properties "[:foo]")))
    (let [ball (spatial/thrown (spatial/default-spec :ball) [0 1 2] [1 2 3] "release")]
      (is (= "release" (:throw-id ball)))
      (is (= [1 2 3] (:velocity ball)))
      (is (= :ball (:shape ball))))))
