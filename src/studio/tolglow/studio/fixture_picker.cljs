(ns tolglow.studio.fixture-picker
  (:require [clojure.string :as str]
            [reagent.core :as r]
            [re-frame.core :as rf]))

(def page-size 48)

(defn fixture-options [fixtures search selected]
  (let [query (str/lower-case (str/trim search))
        matches? (fn [{:keys [id name]}]
                   (or (str/blank? query)
                       (str/includes? (str/lower-case (str id)) query)
                       (and name (str/includes? (str/lower-case (str name)) query))))]
    (into (if selected [selected] [])
          (comp (remove #(= (:id %) selected)) (filter matches?) (take page-size) (map :id))
          fixtures)))

(rf/reg-sub ::fixtures (fn [db _] (get-in db [:server-projection :view :rig :fixtures])))
(rf/reg-sub ::count :<- [::fixtures] (fn [fixtures _] (count fixtures)))
(rf/reg-sub ::first-id :<- [::fixtures] (fn [fixtures _] (:id (first fixtures))))
(rf/reg-sub ::ids :<- [::fixtures] (fn [fixtures _] (into #{} (map :id) fixtures)))
(rf/reg-sub ::options :<- [::fixtures]
  (fn [fixtures [_ search selected]] (fixture-options fixtures search selected)))

(defn choose [label empty-label selected on-change]
  (r/with-let [search (r/atom "")]
    (let [options @(rf/subscribe [::options @search selected])]
      [:div.fixture-search-picker
       (when (> @(rf/subscribe [::count]) page-size)
         [:label [:span "Find fixture"]
          [:input {:type "search" :aria-label "Find fixture" :value @search
                   :on-change #(reset! search (.. % -target -value))}]])
       [:label [:span label]
        [:select {:aria-label label :value (if selected "0" "")
                  :on-change #(let [i (parse-long (.. % -target -value))]
                                (on-change (when (some? i) (get options i))))}
         [:option {:value ""} empty-label]
         (for [[i id] (map-indexed vector options)]
           ^{:key [i id]} [:option {:value i} (if (keyword? id) (subs (str id) 1) (str id))])]]])))
