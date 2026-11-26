(ns tolglow.studio.panel-tiles-test
  (:require [cljs.test :refer-macros [deftest is testing]]
            [clojure.string :as str]
            [re-frame.db :as rf-db]
            [re-frame.interceptor :as interceptor]
            [re-frame.registrar :as registrar]
            [reagent.core :as r]
            ["react-dom/server" :refer [renderToStaticMarkup]]
            [tolglow.studio.panel-tiles :as tiles]
            [tolglow.studio.panel-window :as window]
            [tolglow.studio.parameter-source :as sources]))

(defn- transition [db event]
  (reset! rf-db/app-db db)
  (:effects (interceptor/execute event (remove #(= :do-fx (:id %))
                                               (registrar/get-handler :event (first event))))))

(defn- run [db & events]
  (reduce (fn [db event] (or (:db (transition db event)) db)) db events))

(def show
  {:cues [{:id :ember :name "Ember" :effect-id :glow}
          {:id :moon :name "Moon" :effect-id :sweep}]
   :effects [{:id :glow :params {:level {:type :number :name "Level" :bounds [0 1] :default 0.7}
                                 :mode {:type :enum :values [:soft :hard] :default :soft}}}
             {:id :sweep :params {:speed {:type :number :bounds [0 4] :default 1}}}]
   :instances [{:id "0:1" :cue-id :ember :effect-id :glow :phase "running" :params {:level 0.4}}]})

(deftest choices-list-every-cue-parameter-running-cues-first
  (let [options (tiles/choices (update show :cues reverse))]
    (is (= [["ember" "level"] ["ember" "mode"] ["moon" "speed"]] (map (juxt :cue :param) options)))
    (is (= ["Ember · Level" "Ember · mode" "Moon · speed"] (map :label options)))
    (is (= [true true false] (map :running? options)))))

(deftest a-binding-follows-its-cue-not-an-instance
  (let [{:keys [instance key schema title]} (tiles/resolve-binding show {:cue "ember" :param "level"})]
    (is (= "0:1" (:id instance)))
    (is (= :level key))
    (is (= 0.7 (:default schema)))
    (is (= "Ember · Level" title))
    (is (= 0.4 (tiles/current-value instance key schema))))
  (testing "a later launch of the same cue takes over"
    (let [relaunched (update show :instances conj {:id "0:2" :cue-id :ember :effect-id :glow :phase "running" :params {}})]
      (is (= "0:2" (:id (:instance (tiles/resolve-binding relaunched {:cue "ember" :param "level"})))))
      (is (= 0.7 (let [{:keys [instance key schema]} (tiles/resolve-binding relaunched {:cue "ember" :param "level"})]
                   (tiles/current-value instance key schema)))
          "an unset parameter reads its default")))
  (testing "a cue that is not running still names its parameter"
    (let [{:keys [instance schema title]} (tiles/resolve-binding show {:cue "moon" :param "speed"})]
      (is (nil? instance))
      (is (= 4 (second (:bounds schema))))
      (is (= "Moon · speed" title))))
  (testing "a parameter the show lacks resolves without a schema"
    (is (nil? (:schema (tiles/resolve-binding show {:cue "moon" :param "gone"}))))))

(defn- control [instance key schema & [_ options]]
  [:div.stub-control {:data-param (name key) :data-compact (boolean (:compact? options))
                      :data-mount (:source-mount options)}
   (str (:id instance))])

(defn- tile [entry tier & [s]]
  (renderToStaticMarkup (r/as-element [tiles/param-tile {:show (or s show)} control entry tier])))

(deftest a-parameter-tile-shows-more-as-it-grows
  (let [entry {:id :param-1 :binding {:cue "ember" :param "level"}}
        single (tile entry :single)
        compact (tile entry :compact)
        full (tile entry :full)]
    (testing "1×1: one reading, no control and no menu"
      (is (re-find #"tile-readout\">0.4" single))
      (is (not (re-find #"stub-control|tile-picker" single))))
    (testing "compact: the reading and a compact shared control"
      (is (re-find #"tile-readout\">0.4" compact))
      (is (re-find #"data-compact=\"true\"" compact))
      (is (re-find #"tile-picker" compact)))
    (testing "full: the shared control at full size and its visualization"
      (is (re-find #"data-compact=\"false\"" full))
      (is (re-find #"parameter-visualizer" full)))
    (testing "the menu marks the bound parameter"
      (is (re-find #"<option value=\"0\" selected=\"\">Ember · Level · running" compact))))
  (testing "unbound, not running and missing parameters explain themselves"
    (is (re-find #"Choose a cue parameter" (tile {:id :param-1} :full)))
    (is (re-find #"tile-picker" (tile {:id :param-1} :single)) "a 1×1 tile without a parameter offers the menu")
    (is (re-find #"Moon · speed shows here while the cue runs" (tile {:id :param-1 :binding {:cue "moon" :param "speed"}} :full)))
    (is (re-find #"no gone on moon" (tile {:id :param-1 :binding {:cue "moon" :param "gone"}} :full)))))

(deftest a-tile-has-its-own-source-trigger-and-editor
  (let [entry {:id :param-1 :binding {:cue "ember" :param "level"}}
        trigger #(renderToStaticMarkup (r/as-element [sources/trigger "0:1" :level "Level" nil nil %]))]
    (reset! rf-db/app-db {})
    (testing "the Live card and a tile showing the same parameter have different trigger IDs"
      (is (re-find #"id=\"source-0:1-level\"" (trigger nil)))
      (is (re-find #"id=\"source-tile-param-1-0:1-level\"" (trigger (tiles/source-mount :param-1)))))
    (testing "the tile passes its own scope to the shared control at both sizes"
      (is (re-find #"data-mount=\"tile-param-1\"" (tile entry :compact)))
      (is (re-find #"data-mount=\"tile-param-1\"" (tile entry :full))))
    (testing "the tile's trigger opens the editor in the tile, not the Live card's"
      (is (not (re-find #"live-source-editor" (tile entry :full))))
      (reset! rf-db/app-db {:ui {:parameter-source {"tile-param-1-0:1" :level}}})
      (let [open (tile entry :full)]
        (is (re-find #"<section id=\"source-editor-tile-param-1-0:1-level\"[^>]*class=\"live-source-editor\"" open))
        (is (re-find #"id=\"source-editor-heading-tile-param-1-0:1-level\"" open)))
      (reset! rf-db/app-db {:ui {:parameter-source {"0:1" :level}}})
      (is (not (re-find #"live-source-editor" (tile entry :full))) "the Live card's editor stays in the card")
      (reset! rf-db/app-db {}))
    (testing "focus moves within the document of the control that was used, as in a tile's own window"
      (let [doc #js {:title "tile window"}
            scope (sources/scope "0:1" (tiles/source-mount :param-1))
            opened (transition {} [::sources/toggle scope [:level] :level doc])
            closed (transition (:db opened) [::sources/close scope [:level] doc])]
        (is (= [scope [:level] true doc] (::sources/focus opened)))
        (is (= [scope [:level] false doc] (::sources/focus closed)))
        (is (= {} (get-in (:db closed) [:ui :parameter-source])))))))

(deftest the-visualization-panel-lists-running-parameters
  (let [entries (tiles/viz-entries show)]
    (is (= [["0:1" :level]] (map (juxt (comp :id :instance) :key) entries)) "enums have no plot")
    (let [panel #(renderToStaticMarkup (r/as-element [tiles/viz-panel {:show %1} %2]))]
      (is (re-find #"parameter-visualizer" (panel show :full)))
      (is (not (re-find #"parameter-visualizer" (panel show :compact))) "compact shows readouts only")
      (is (re-find #"Running effects" (panel (assoc show :instances []) :full))))))

(deftest a-detached-panel-has-one-window-at-a-time
  (let [db {:ui {}}
        popping (transition db [:panel-window/pop-out :live "Live" [700 600]])]
    (is (= {:id :live :generation 1 :title "Live" :width 700 :height 600} (:panel-window/open popping)))
    (is (window/detached? (:db popping) :live))
    (is (nil? (transition (:db popping) [:panel-window/pop-out :live "Live"])) "a second pop-out changes nothing")
    (let [popped (run (:db popping) [:panel-window/opened :live 1])]
      (is (= :popped (:phase (window/state popped :live))))
      (is (= [:live] (window/detached-ids popped)))
      (is (= [:live 1] (:panel-window/close (transition popped [:panel-window/redock :live]))))
      (testing "closing the window docks the panel; a stale close changes nothing"
        (let [closed (run popped [:panel-window/closed :live 1])]
          (is (not (window/detached? closed :live)))
          (is (= [] (window/detached-ids closed)))
          (let [again (run closed [:panel-window/pop-out :live "Live"])]
            (is (= 2 (:generation (window/state again :live))))
            (is (= again (run again [:panel-window/closed :live 1])))))))
    (testing "windows of different panels are independent"
      (let [both (run db [:panel-window/pop-out :live "Live"] [:panel-window/pop-out :param-1 "Parameter 1"])]
        (is (= #{:live :param-1} (set (window/detached-ids both))))))
    (testing "a blocked window returns to docked with an explanation"
      (let [failed (transition (:db popping) [:panel-window/failed :live 1 :blocked])]
        (is (not= :popped (:phase (window/state (:db failed) :live))))
        (is (str/includes? (second (:dispatch failed)) "Allow pop-ups"))))))
