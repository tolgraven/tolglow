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
   :knob-candidates {:status :idle :characters [{:id :calm :name "Calm"}]}})

(defn- installed []
  (:db (events-test/full-frame (assoc events-test/base :connection {:connected? true}) view)))

(defn- html [component]
  (renderToStaticMarkup (r/as-element component)))

(defn- same-render
  "Render panel from the whole db and from its slice at paths, with app-db
  holding the whole db for any subscription inside it."
  [db paths panel & args]
  (reset! rf-db/app-db db)
  (let [whole (html (into [panel db] args))
        sliced (html (into [panel (db-slice/slice db paths)] args))]
    (is (seq whole))
    (is (= whole sliced))))

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
      (same-render (view-tabs/install db nil) @#'views/view-tabs-paths view-tabs/strip)
      (same-render db @#'views/chat-paths chat/panel))
    (testing "the cue grid's cells, queued launches and scheduled events read only its slice"
      (let [page (first (get-in db [:server-projection :view :pages]))]
        (doseq [x (range 3)]
          (same-render db @#'views/cue-grid-paths @#'views/cue-cell page x 0 nil)))
      (same-render db @#'views/cue-grid-paths views/queued-cues)
      (same-render db @#'views/cue-grid-paths views/scheduled-events))
    (testing "Perform tools and knob panels"
      (same-render db @#'views/auto-paths @#'views/auto-panel)
      (same-render db @#'views/response-paths @#'views/response-panel)
      (same-render db @#'views/knob-presets-paths knob-presets/panel)
      (same-render db @#'views/knob-candidates-paths knob-candidates/panel))))

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
      (let [perform (ratom/make-reaction #(do (swap! runs inc) @(rf/subscribe [::views/perform])) :auto-run true)]
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
