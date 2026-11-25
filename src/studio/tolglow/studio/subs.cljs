(ns tolglow.studio.subs
  (:require [re-frame.core :as rf]
            [tolglow.studio.events :as events]))

(rf/reg-sub :studio/db (fn [db _] (dissoc db :debug)))
(rf/reg-sub :studio/show (fn [db _] (:show db)))
(rf/reg-sub :studio/ui (fn [db _] (:ui db)))
(rf/reg-sub :operator-history/inspector (fn [db _] (events/history-inspector db)))
(rf/reg-sub :studio/numeric-edit (fn [db [_ field-id]] (get-in db [:ui :numeric-edits field-id])))
(rf/reg-sub :studio/current-page
  (fn [db _]
    (some #(when (= (:id %) (get-in db [:ui :page-id])) %)
          (get-in db [:show :pages]))))
(rf/reg-sub :studio/active-instance
  (fn [db _]
    (some #(when (= (:id %) (get-in db [:ui :selected-instance])) %)
          (get-in db [:show :instances]))))
