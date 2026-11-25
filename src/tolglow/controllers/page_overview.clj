(ns tolglow.controllers.page-overview
  "A display-worker projection of cue pages. Miniatures read definitions
  only, so browsing a hundred pages never starts an effect or a visual sampler."
  (:require [afterglow.control.categories :as categories]
            [afterglow.control.display :as display]
            [afterglow.control.pages :as cue-pages]
            [thi.ng.color.core :as colors]))

(def ^:private neutral "#727c78")

(def ^:private tag-palette ["#72b6c6" "#c490d9" "#dfac78" "#8bc69c" "#db889c" "#9aa5df"])

(defn- color [value]
  (cond
    (satisfies? colors/IRGBConvert value)
    (some-> (colors/as-rgba value) ((juxt :r :g :b)) color)

    (and (string? value) (re-matches #"#[\da-fA-F]{3}(?:[\da-fA-F]{3})?" value)) value
    (and (vector? value) (= 3 (count value))
         (every? #(and (number? %) (Double/isFinite (double %)) (<= 0 % 1)) value))
    (apply format "#%02x%02x%02x" (map #(Math/round (* 255.0 %)) value))))

(defn- cue-colors [registry cue-id palette]
  (let [cue (get-in registry [:cues cue-id])
        latest (get-in registry [:effects (:effect-id cue)])
        effect (if-let [revision (:effect-version cue)]
                 (or (get-in registry [:versions [:effects (:effect-id cue) revision]])
                     (when (= revision (:version latest)) latest))
                 latest)
        tags (distinct (concat (categories/definition-categories effect)
                               (sort-by str (concat (:categories cue) (:tags cue) (:tags effect)))))
        tag-colors (mapv #(or (color (get palette %)) (get categories/colors %)
                              (nth tag-palette (mod (hash %) (count tag-palette)))) tags)]
    {:cue-color (or (color (:swatch cue)) (color (:color cue)) (color (:swatch effect))
                    (color (get-in cue [:params :color])) neutral)
     :type-colors (if (seq tag-colors) (vec (take 3 (distinct tag-colors))) [neutral])}))

(defn- thumbnail [registry palette page]
  (let [ordered (sort-by key (:cells page))
        extent (reduce (fn [[w h] [[x y] _]] [(max w (inc x)) (max h (inc y))]) [8 8] ordered)
        position (fn [n size] (min 7 (long (* 8 (/ n size)))))
        tile (fn [tiles [[x y] cue-id]]
               (let [xy [(position x (first extent)) (position y (second extent))]]
                 (if (contains? tiles xy) tiles
                   (assoc tiles xy (assoc (cue-colors registry cue-id palette) :xy xy)))))]
    {:id (:id page)
     :name (let [label (str (or (:name page) (:id page)))]
             (subs label 0 (min 64 (count label))))
     :count (count ordered)
     :cells (vec (vals (reduce tile (sorted-map) ordered)))}))

(defn project [model view]
  (let [ids (display/page-ids model)
        {:keys [pages] :as window} (display/page-window ids (:page view) (:page-bank view))
        options (get-in model [:settings :controllers :push-display])]
    (assoc window :scheme (or (:page-colors view) (:page-colors options) :both)
           :pages (mapv #(thumbnail (:registry model) (:tag-colors options) (cue-pages/page model %)) pages))))

(defn project-display [cache model view]
  (if-not (= :pages (:display-mode view))
    (do (when @cache (vreset! cache nil)) view)
    (let [previous @cache
          selector (select-keys view [:page :page-bank :page-colors])
          options (get-in model [:settings :controllers :push-display])
          recent-source (cue-pages/recent-page-source model)
          reused? (and (= selector (:selector previous)) (identical? options (:options previous))
                       (identical? (:pages model) (:pages previous))
                       (or (not (:includes-recent? previous))
                           (identical? recent-source (:recent-source previous)))
                       (identical? (:registry model) (:registry previous)))
          result (if reused? (:result previous) (project model view))]
      (when-not reused?
        (vreset! cache {:pages (:pages model) :registry (:registry model) :options options
                        :recent-source recent-source
                        :includes-recent? (some #(= cue-pages/recent-page-id (:id %)) (:pages result))
                        :selector selector :result result}))
      (assoc view :page-overview result))))
