(ns tolglow.studio.db-slice-test
  "Panels that subscribe to a slice of app-db render exactly what they render
  from the whole db, and a slice stays equal across changes the panel does not
  show (#790)."
  (:require [cljs.test :refer-macros [deftest is testing use-fixtures]]
            [re-frame.core :as rf]
            [re-frame.db :as rf-db]
            [reagent.core :as r]
            [reagent.ratom :as ratom]
            ["react-dom/server" :refer [renderToStaticMarkup]]
            [tolglow.studio.db-slice :as db-slice]
            [tolglow.studio.events-test :as events-test]
            [tolglow.studio.frame-stream :as frame-stream]
            [tolglow.studio.knob-candidates :as knob-candidates]
            [tolglow.studio.knobs :as knobs]
            [tolglow.studio.panel-layout :as panel-layout]
            [tolglow.studio.performance :as performance]
            [tolglow.studio.room-window :as room-window]
            [tolglow.studio.knob-presets :as knob-presets]
            [tolglow.studio.chat :as chat]
            [tolglow.studio.view-tabs :as view-tabs]
            [tolglow.studio.views :as views]))

(use-fixtures :each {:before frame-stream/reset-all! :after frame-stream/reset-all!})

(def ^:private source {:adapter :web :session-id "browser-session"})

(def ^:private view
  {:runtime-id "host" :show-id :main :epoch 7 :revision 3 :frame-number 12 :master 0.7 :blackout? false
   :transport {:bpm 124 :playing? true :meter {:beats-per-bar 4 :bars-per-phrase 4}}
   :transport-generation 2
   :rig {:name "Test rig" :fixtures [{:id "head-1"} {:id "head-2"}]}
   :status {:output {:type "art-net"}}
   :cues [{:id "ember" :effect-id "ember" :name "Ember" :category :wash :swatch [1 0.4 0.1]}
          {:id "flash" :effect-id "ember" :name "Flash"}]
   :effects [{:id "ember" :version 2 :params {:level {:type :number :bounds [0 1]}}}]
   :pages [{:id "main" :cells [{:x 0 :y 0 :cue-id "ember"} {:x 1 :y 0 :cue-id "flash"}]}]
   :instances [{:id "7:9" :cue-id "ember" :phase "running" :launch-mode "held" :order 1 :params {:level 0.7}}]
   :pending-cue-launches [{:command-id "own" :cue-id "flash" :due-beat 4.5 :mode :held :source source}]
   :schedule {:upcoming [{:event-id "e1" :origin "scheduled" :op "cue/launch" :label "Accent" :due-beat 8 :category :cue}]
              :ramps [{:event-id "r1" :label "Fade" :start 0 :end 8 :to 0.5 :unit :beats :destination [:master]}]}
   :auto {:enabled? true :section :verse :reasons [{:reason :eligible}]
          :config {:activity-target 0.4 :randomize {:params :marked} :sections [{:id :verse}]}
          :pending [{:event-id "a1" :label "Stop auto" :due-beat 9}]}
   :responses [{:id :swell :envelope {:attack {:beats 1} :sustain 0.8} :enabled? true :state {}}]
   :knobs {:knobs [{:id :k1 :name "Width" :value 0.5 :links []}]}
   :knob-presets {:slots [{:slot 0 :state :stored :knobs [{:id :k1 :value 0.5}]} {:slot 1}]}
   :knob-candidates {:status :idle :characters [{:id :calm :name "Calm"}]}
   :compositions [{:id "m" :name "Macro" :mode "macro" :slots [{:cue-id "ember"}]}]
   :settings {:spatial {:fog {:enabled? true}}}
   :scene-draft {:draft {:id "d" :revision 1 :operations [{:op :set}]} :status :auditioning}
   :show-folder {:id "folder" :new? true}
   :output {:atmosphere {:sources [{:flow-m3-s 0.01}]}}
   :receiver-surfaces []
   :fixture-types []})

(def ^:private ui
  ;; Every UI key the sliced panels read, filled.
  {:performance-section "live" :preview-expanded? true :panel-layout {:class :desktop} :panel-windows {}
   :window-tiers {:room :full} :room-window {:phase :docked} :fullscreen-preview-controls? true
   :preview-controls? true :fog 0.4 :page-id "main" :edit? false :full-grid? true :launch-grid "bar"
   :definition {:mode :form :kind "effects" :source "{:id :x}" :status "Ready" :engine :textarea :auto? true}
   :definition-search "em" :builder {:id "built" :name "Built" :level 0.6 :keyframes []}
   :composition {:mode "macro" :slots [{:cue-id "ember" :params {:level 0.3}}]} :slot-cue "ember"
   :fixture-library {:open? false} :performance {:expanded {}}
   :knobs {:drafts {} :forms {} :new-name "New"} :auto-form {:interval-value 4 :count 2 :target-activity? true}
   :response-form {:id "swell" :params "level" :scope "all" :curve "linear" :unit "beats" :attack-value 1}
   :show-folder-notices #{} :chat-draft "hello" :provider-form {}})

(defn- installed []
  (-> (:db (events-test/full-frame (assoc events-test/base :connection {:connected? true}) view))
      (update :ui merge ui)
      (view-tabs/install nil)
      (assoc :chat {:provider {:ready? true} :conversations [{:id "c1" :title "First"}]}
             :scene-draft-request {:id "r" :draft-id "other"})))

(defn- with-document
  "Render with a minimal browser document for panels that listen to it while
  they render (the cue grid follows its window's focus)."
  [f]
  (if (exists? js/document)
    (f)
    (let [noop (fn [& _])
          window #js {:addEventListener noop :removeEventListener noop}
          doc #js {:addEventListener noop :removeEventListener noop :defaultView window
                   :visibilityState "visible" :hasFocus (constantly true)
                   :getElementById (constantly nil) :querySelector (constantly nil)}]
      (set! (.-document js/globalThis) doc)
      (set! (.-requestAnimationFrame js/globalThis) (constantly 0))
      (set! (.-cancelAnimationFrame js/globalThis) noop)
      (try (f) (finally (doseq [k ["document" "requestAnimationFrame" "cancelAnimationFrame"]]
                          (js-delete js/globalThis k)))))))

(defn- html
  "Static markup, with the random IDs widgets generate per mount made equal."
  [component]
  (.replace (renderToStaticMarkup (r/as-element component))
            (js/RegExp. "[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}" "g") "uuid"))

(defn- same-render
  "Render panel from the whole db and from its slice at paths, with app-db
  holding the whole db for any subscription inside it."
  [db paths panel & args]
  (reset! rf-db/app-db db)
  (reset! db-slice/misses #{})
  (with-document
    #(let [whole (html (into [panel db] args))
           sliced (html (into [panel (db-slice/guard (db-slice/slice db paths) paths)] args))]
       (is (seq whole))
       (is (= whole sliced))
       (is (empty? @db-slice/misses) "the panel reads no unlisted path"))))

(deftest a-slice-holds-only-its-paths
  (let [db {:ui {:view :perform :toast {:message "x"}} :show {:cues [1] :instances [{:id 1 :params {:a 1} :phase "running"}]}}]
    (is (= {:ui {:view :perform} :show {:cues [1]}} (db-slice/slice db [[:ui :view] [:show :cues] [:show :absent]])))
    (is (= {:show {:instances [{:id 1 :phase "running"}]}}
           (db-slice/slice db [{:path [:show :instances] :select [:id :phase]}])))
    (is (= (db-slice/slice db [[:ui :view]]) (db-slice/slice (assoc-in db [:ui :toast] nil) [[:ui :view]])))))

(deftest panels-render-the-same-from-their-slice
  (let [db (installed)]
    (testing "shell"
      (same-render db @#'views/sidebar-paths @#'views/sidebar)
      (same-render db @#'views/topbar-paths @#'views/topbar)
      (same-render db @#'views/view-tabs-paths view-tabs/strip)
      (same-render db @#'views/main-paths @#'views/main-area)
      (same-render db @#'views/folder-notice-paths @#'views/folder-notice)
      (same-render db @#'views/chat-paths chat/panel))
    (testing "the cue grid's cells, queued launches and scheduled events read only its slice"
      (let [page (first (get-in db [:server-projection :view :pages]))]
        (doseq [x (range 3)]
          (same-render db @#'views/cue-grid-paths @#'views/cue-cell page x 0 nil)))
      (same-render db @#'views/cue-grid-paths @#'views/cue-grid)
      (same-render db @#'views/cue-grid-paths views/queued-cues)
      (same-render db @#'views/cue-grid-paths views/scheduled-events))
    (testing "Perform tools and knob panels"
      (same-render db @#'views/auto-paths @#'views/auto-panel)
      (same-render db @#'views/response-paths @#'views/response-panel)
      (same-render db @#'views/knob-presets-paths knob-presets/panel)
      (same-render db @#'views/knob-candidates-paths knob-candidates/panel)
      (same-render db @#'views/board-paths performance/board @#'views/parameter-control)
      (same-render db @#'knobs/card-paths @#'knobs/knob-card (first (get-in db [:show :knobs :knobs])) true))
    (testing "Room, definitions, composition and rig"
      (same-render db @#'views/room-paths room-window/room @#'views/preview-panel)
      (same-render db @#'views/definition-paths @#'views/definition-view)
      (same-render (assoc-in db [:ui :view] :compose) @#'views/composition-paths @#'views/composition-view)
      (same-render (assoc-in db [:ui :view] :rig) @#'views/rig-paths @#'views/rig-view))))

(deftest assistant-keystrokes-leave-the-perform-slices-equal
  (let [db (installed)
        typed (assoc-in db [:ui :chat-draft] "hello there")]
    (doseq [paths [@#'views/perform-paths @#'views/room-paths @#'views/composition-paths @#'views/rig-paths
                   @#'views/definition-paths @#'views/cue-grid-paths @#'views/main-paths]]
      (is (= (db-slice/slice db paths) (db-slice/slice typed paths)) (pr-str paths)))))

(deftest the-development-guard-reports-unlisted-reads
  (reset! db-slice/misses #{})
  (let [warn (.-warn js/console)
        _ (set! (.-warn js/console) (fn [& _]))
        paths [[:ui :view] [:show :cues]]
        guarded (db-slice/guard (db-slice/slice {:ui {:view :perform :panel-windows {:x 1}} :show {:cues [1]}} paths) paths)]
    (is (= :perform (get-in guarded [:ui :view])))
    (is (nil? (get-in guarded [:show :absent-but-unlisted])))
    (is (= {:ui {:view :perform} :show {:cues [1]}} guarded) "equality sees the plain slice")
    (is (nil? (get-in guarded [:ui :panel-windows])) "an unlisted path reads as absent")
    (is (= #{[paths [:show :absent-but-unlisted]] [paths [:ui :panel-windows]]} @db-slice/misses)
        "and each unlisted read is reported")
    (reset! db-slice/misses #{})
    (is (nil? (get-in guarded [:ui :view :deeper])) "below a listed path nothing is guarded")
    (is (empty? @db-slice/misses))
    (set! (.-warn js/console) warn)))

(deftest a-parameter-edit-leaves-the-shell-and-cue-grid-slices-equal
  (let [db (installed)
        edited (-> db
                   (assoc-in [:server-projection :view :instances 0 :params :level] 0.2)
                   (assoc-in [:show :instances 0 :params :level] 0.2)
                   (update-in [:show :revision] inc))]
    (doseq [paths [@#'views/sidebar-paths @#'views/topbar-paths @#'views/view-tabs-paths @#'views/cue-grid-paths
                   @#'views/auto-paths @#'views/response-paths @#'views/knob-presets-paths
                   @#'views/knob-candidates-paths @#'views/chat-paths]]
      (is (= (db-slice/slice db paths) (db-slice/slice edited paths)) (pr-str paths)))
    (is (not= (db-slice/slice db @#'views/board-paths) (db-slice/slice edited @#'views/board-paths))
        "the board shows the parameter, so its slice changes")))

(deftest a-slice-subscription-notifies-only-when-its-paths-change
  (let [before @rf-db/app-db
        db (installed)
        renders (atom 0)]
    (try
      (reset! rf-db/app-db db)
      (let [reader (ratom/make-reaction #(do (swap! renders inc) @(rf/subscribe [:studio/db-slice @#'views/topbar-paths]))
                                        :auto-run true)]
        @reader
        (let [start @renders]
          (swap! rf-db/app-db assoc-in [:show :instances 0 :params :level] 0.1)
          (ratom/flush!)
          (is (= start @renders) "an instance parameter is not in the topbar's slice")
          (swap! rf-db/app-db assoc-in [:show :transport :bpm] 90)
          (ratom/flush!)
          (is (= (inc start) @renders) "the tempo is"))
        (ratom/dispose! reader))
      (finally (reset! rf-db/app-db before)))))

(deftest perform-heading-reads-the-running-count-not-the-instances
  (let [before @rf-db/app-db
        runs (atom 0)]
    (try
      (reset! rf-db/app-db (installed))
      (let [perform (ratom/make-reaction #(do (swap! runs inc) @(rf/subscribe [::views/perform @#'views/perform-paths])) :auto-run true)]
        (is (= 1 (::views/running @perform)))
        (let [start @runs]
          (swap! rf-db/app-db assoc-in [:show :instances 0 :params :level] 0.1)
          (ratom/flush!)
          (is (= start @runs) "a parameter edit leaves Perform's heading and layout alone"))
        (ratom/dispose! perform))
      (finally (reset! rf-db/app-db before)))))

(deftest cue-cells-keep-their-pad-state-from-the-reduced-instances
  (is (= [{:id "7:9" :cue-id "ember" :phase "running" :launch-mode "held"}]
         (get-in (db-slice/slice (installed) @#'views/cue-grid-paths) [:server-projection :view :instances]))))

(defn- detach [db view] (assoc-in db [:ui :panel-windows (keyword "view" (name view))] {:phase :popped :generation 1}))

(deftest a-detached-view-renders-only-in-its-window-and-shows-there
  (let [db (-> (installed) (assoc-in [:ui :view] :library) (detach :perform))]
    (reset! rf-db/app-db db)
    (reset! db-slice/misses #{})
    (with-document
      #(let [main (html [@#'views/main-area (db-slice/guard (db-slice/slice db @#'views/main-paths) @#'views/main-paths)])
             window (fn [view] (html (@#'views/view-content (assoc-in db [:ui :view] view) view)))]
         (testing "the main page leaves the detached Perform out and shows Library"
           (is (not (.includes main "id=\"view-perform\"")))
           (is (re-find #"<section id=\"view-library\" class=\"view\">" main)))
         (testing "the windows' content is visible: each is its window's current view"
           (is (re-find #"<section[^>]*id=\"view-perform\"[^>]*>" (window :perform)))
           (is (not (re-find #"<section[^>]*id=\"view-perform\"[^>]*hidden" (window :perform))))
           (is (re-find #"<section id=\"view-library\" class=\"view\">" (window :library))))
         (is (empty? @db-slice/misses) "main area, placeholder and windows read only listed paths")))
    (testing "on the main page, Perform's placeholder stands in while its view is current"
      (reset! rf-db/app-db (assoc-in db [:ui :view] :perform))
      (with-document
        #(is (.includes (html [@#'views/main-area (db-slice/slice (assoc-in db [:ui :view] :perform) @#'views/main-paths)])
                        "data-detached-view=\"perform\""))))))

(deftest a-main-page-view-change-leaves-a-detached-window-alone
  (let [before @rf-db/app-db
        db (detach (installed) :compose)
        window-content (fn [main-view] (@#'views/view-content (assoc-in (assoc-in db [:ui :view] main-view) [:ui :view] :compose) :compose))
        runs (atom 0)]
    (try
      (is (= (window-content :perform) (window-content :rig)) "the window's panel keeps equal arguments")
      (reset! rf-db/app-db (assoc-in db [:ui :view] :perform))
      (let [as {[:ui :view] :compose}
            reader (ratom/make-reaction #(do (swap! runs inc) (db-slice/subscribe as @#'views/composition-paths))
                                        :auto-run true)
            perform (ratom/make-reaction #(do (swap! runs inc)
                                              @(rf/subscribe [::views/perform (db-slice/without-overridden @#'views/perform-paths
                                                                                                          {[:ui :view] :perform})]))
                                         :auto-run true)]
        (is (= :compose (get-in @reader [:ui :view])) "the slice shows the window's view")
        @perform
        (let [start @runs]
          (swap! rf-db/app-db assoc-in [:ui :view] :rig)
          (ratom/flush!)
          (is (= start @runs) "a main-page view change recomputes neither window slice")
          (swap! rf-db/app-db assoc-in [:ui :composition :mode] "sequence")
          (ratom/flush!)
          (is (= (inc start) @runs) "the composition it shows does"))
        (ratom/dispose! reader)
        (ratom/dispose! perform))
      (finally (reset! rf-db/app-db before)))))

(deftest perform-layout-editing-offers-redock-for-a-popped-room
  (let [db (-> (installed)
               (assoc-in [:ui :panel-layout] {:class :desktop :edit {:class :desktop :layout (panel-layout/default-layout :desktop)}})
               (assoc-in [:ui :room-window] {:phase :popped :generation 1}))]
    (reset! rf-db/app-db db)
    (reset! db-slice/misses #{})
    (with-document
      #(let [markup (html (@#'views/view-content db :perform))]
         (is (re-find #"data-panel=\"room\"" markup))
         (is (re-find #"Redock<span class=\"sr-only\"> Room" markup) "the Room tile's chrome offers Redock")
         (is (not (re-find #"Detach<span class=\"sr-only\"> Room" markup)))
         (is (empty? @db-slice/misses) "edit chrome reads only listed paths")))))

(deftest a-hidden-view-ignores-navigation-between-other-views
  (let [db (installed)
        on (fn [current view] (@#'views/view-content (assoc-in db [:ui :view] current) view))]
    (is (= (on :library :perform) (on :assistant :perform) (on :connections :perform)))
    (is (= (on :perform :library) (on :assistant :library)))
    (is (not= (on :perform :perform) (on :library :perform)) "becoming current does re-render it")))
