(ns tolglow.studio.page-overview
  (:require [clojure.string :as str]))

(defn- bands [colors]
  (if (= 1 (count colors)) (first colors)
    (let [n (count colors)]
      (str "linear-gradient(135deg,"
           (str/join "," (map-indexed (fn [i c]
                                        (str c " " (/ (* i 100) n) "% " (/ (* (inc i) 100) n) "%"))
                                      colors)) ")"))))

(defn- chip [scheme {:keys [xy cue-color type-colors]}]
  (let [[x y] xy]
    [:i {:style {:grid-column (inc x) :grid-row (- 8 y) :background cue-color}}
     (when (not= :cue scheme)
       [:span {:class (when (= :both scheme) "split") :style {:background (bands type-colors)}}])]))

(defn view [display]
  (let [{:keys [bank banks pages scheme]} (:page-overview display)
        scheme (keyword (or scheme :both))]
    [:section.push-pages {:aria-label "Cue pages"}
     [:div.push-pages-heading
      [:b "Cue pages"]
      [:span (case scheme :cue "Cue colors" :type "Tags & types" "Cue colors + types")]
      [:span (str (inc (or bank 0)) " / " (or banks 1))]]
     (if (empty? pages) [:p.push-pages-empty "No cue pages"]
       [:div.push-pages-grid
        (for [[slot {:keys [id name count cells]}] (map-indexed vector pages)]
          ^{:key (pr-str id)}
          [:div.push-page-tile {:class (when (= id (:page display)) "current") :aria-label (str name ", " count " cues")}
           [:div.push-page-map (for [cell cells] ^{:key (str (:xy cell))} [chip scheme cell])]
           [:div.push-page-label [:b name] [:small (str "↑ " (inc slot) " · " count " cues")]]])])]))
