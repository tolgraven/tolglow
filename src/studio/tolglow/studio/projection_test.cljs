(ns tolglow.studio.projection-test
  (:require [cljs.test :refer-macros [deftest is]]
            [tolglow.studio.projection :as projection]))

(def view {:runtime-id "runtime" :show-id "main" :epoch 0 :revision 2 :frame-number 10
           :nullable 3 :drop true
           :instances [{:id "0:1" :params {:rig/aim [0 1 2] :level 0.5}}]})
(def full {:protocol projection/protocol :kind "full" :base-token nil :token "first"
           :identity (projection/identity-of view) :view view})
(def current (:state (projection/receive nil full)))
(def patch {:protocol projection/protocol :kind "patch" :base-token "first" :token "second"
            :identity (projection/identity-of view)
            :changes [["remove" ["drop"]] ["set" ["nullable"] nil]
                      ["set" ["frame-number"] 11]
                      ["set" ["instances" 0 "params" "rig/aim" 1] 0.8]]})

(deftest wire-roundtrip-keeps-qualified-keys-nil-and-deletions-distinct
  (let [wire #(js->clj (js/JSON.parse (js/JSON.stringify (clj->js % :keyword-fn (fn [key] (subs (str key) 1)))))
                       :keywordize-keys true)
        initial (projection/receive nil (wire full))
        result (projection/receive (:state initial) (wire patch))]
    (is (= :accepted (:status initial)))
    (is (= :accepted (:status result)))
    (is (= (-> view (dissoc :drop) (assoc :nullable nil :frame-number 11)
                (assoc-in [:instances 0 :params :rig/aim] [0 0.8 2]))
           (get-in result [:state :view])))
    (is (= view (:view current)))))

(deftest missing-base-and-invalid-patches-leave-the-last-complete-projection
  (doseq [bad [(assoc patch :base-token "missing")
               (assoc patch :identity {:runtime-id "replacement" :show-id "main" :epoch 0})
               (assoc patch :changes [["set" ["instances" 99 "params"] {}]])
               (assoc patch :changes [["remove" ["instances" 0]]])
               (assoc patch :changes [["remove" ["missing"]]])
               (assoc patch :changes [["set" ["missing" "child"] 1]])
               (assoc patch :changes [["move" ["nullable"] 1]])
               (assoc patch :changes [["set" ["nullable"]]])
               (assoc patch :changes [["set" ["epoch"] 1]])
               (assoc patch :changes [["set" ["frame-number"] 9]])
               (assoc patch :changes [["set" [] nil]])
               (assoc patch :protocol "future/2")]]
    (let [result (projection/receive current bad)]
      (is (= :resync (:status result)))
      (is (= current (:state result)))))
  (is (= :resync (:status (projection/receive nil patch)))))

(deftest repeated-and-out-of-order-envelopes-never-reapply-a-delta
  (let [next (:state (projection/receive current patch))]
    (is (= {:status :duplicate :state next} (projection/receive next patch)))
    (is (= :resync (:status (projection/receive next full))))
    (is (= :resync (:status (projection/receive next (assoc patch :token "third")))))))

(deftest full-snapshot-can-replace-runtime-or-epoch-and-reordered-records
  (let [replacement (assoc view :runtime-id "new-runtime" :epoch 0 :revision 0 :frame-number 0)
        reset (assoc full :token "reset" :view replacement :identity (projection/identity-of replacement))
        reordered [{:id "0:2" :params {}} {:id "0:1" :params {:level 0.5}}]
        result (projection/receive current (assoc patch :changes [["set" ["instances"] reordered]]))]
    (is (= :accepted (:status (projection/receive current reset))))
    (is (= replacement (get-in (projection/receive current reset) [:state :view])))
    (is (= reordered (get-in result [:state :view :instances])))))
