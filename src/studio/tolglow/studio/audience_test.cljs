(ns tolglow.studio.audience-test
  (:require [cljs.test :refer-macros [deftest is]]
            [tolglow.studio.audience :as audience]))

(deftest ordered-acknowledgement-and-projection
  (let [entry {:included? true :revision 9}
        db {:show {:runtime-id "live" :epoch 1}
            :connections {:generation 2 :audience-operations {:session ["live" 1 2] :entries {"floor" entry}}}
            :server-projection {:view {:revision 8 :connection-status {:audience {}}}}}
        newer (assoc-in db [:server-projection :view :revision] 10)]
    (is (= {"floor" entry} (audience/operations db)))
    (is (= {} (audience/operations newer)))
    (is (= {"floor" entry} (audience/operations (update-in newer [:server-projection :view] dissoc :connection-status))))
    (is (nil? (audience/operations (assoc-in db [:show :epoch] 2))))))
