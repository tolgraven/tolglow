(ns tolglow.studio.panel-layout-test
  (:require [cljs.test :refer-macros [deftest is testing]]
            [re-frame.db :as rf-db]
            [re-frame.interceptor :as interceptor]
            [re-frame.registrar :as registrar]
            [reagent.core :as r]
            ["react-dom/server" :refer [renderToStaticMarkup]]
            [tolglow.studio.panel-layout :as layout]))

(defn- transition [db event]
  (reset! rf-db/app-db db)
  (:effects (interceptor/execute event (remove #(= :do-fx (:id %))
                                               (registrar/get-handler :event (first event))))))

(defn- run [db & events]
  (reduce (fn [db event] (or (:db (transition db event)) db)) db events))

(defn- ids [layout] (mapv :id layout))

(def desktop {:ui {:panel-layout {:class :desktop :saved {}}}})

(deftest size-classes-follow-the-built-in-breakpoints
  (is (= :phone (layout/size-class 390)))
  (is (= :phone (layout/size-class 600)))
  (is (= :tablet (layout/size-class 601)))
  (is (= :tablet (layout/size-class 1199)))
  (is (= :desktop (layout/size-class 1200)))
  (is (= :phone (layout/size-class true true)))
  (is (= :desktop (layout/size-class false false))))

(deftest stored-layouts-are-repaired-for-the-current-screen
  (testing "unknown, repeated and malformed entries go; missing panels return with default sizes"
    (let [repaired (layout/normalize :tablet [{:id "room" :w 40 :h 2} {:id "room" :w 1 :h 10}
                                              {:id "mixer" :w 3 :h 10} "junk" {:id :cues :w 2.4 :h "tall"}])]
      (is (= [:room :cues :live :tools] (ids repaired)))
      (is (= {:id :room :w 6 :h layout/min-rows} (first repaired)) "width clamps to six tablet columns")
      (is (= {:id :cues :w 2 :h 28} (second repaired)) "a non-numeric height falls back to the default")
      (is (= (layout/default-entry :tablet :live) (nth repaired 2)))))
  (testing "a layout that is not a list becomes the default"
    (is (= (layout/default-layout :phone) (layout/normalize :phone {:id :room})))))

(def ^:private custom
  [{:id :room :w 8 :h 20} {:id :cues :w 4 :h 30} {:id :param-2 :w 2 :h 6 :binding {:cue "ember" :param "level"}}
   {:id :live :w 12 :h 40} {:id :tools :w 4 :h 12} {:id :viz :w 6 :h 24}])

(deftest storage-round-trips-named-layouts-and-migrates-version-1
  (let [saved {:desktop {:active "Show" :layouts [{:name "Show" :panels custom}
                                                  {:name "Rehearsal" :panels (layout/default-layout :desktop)}]}
               :phone {:active nil :layouts [{:name "Pocket" :panels (layout/default-layout :phone)}]}}]
    (is (= saved (layout/decode (layout/encode saved))))
    (is (= {} (layout/decode "{\"version\":3,\"classes\":{}}")))
    (is (= {} (layout/decode "not json")))
    (is (= {} (layout/decode nil)))
    (testing "an active name that no layout has is dropped; repeated names keep the first"
      (is (= {:tablet {:active nil :layouts [{:name "A" :panels (layout/default-layout :tablet)}]}}
             (layout/decode "{\"version\":2,\"classes\":{\"watch\":{},\"tablet\":{\"active\":\"B\",\"layouts\":[{\"name\":\"A\",\"panels\":[]},{\"name\":\"A\",\"panels\":[{\"id\":\"room\"}]},{\"name\":\"  \"}]}}}"))))
    (testing "version 1 held one layout per screen; it becomes the active Layout 1"
      (is (= {:tablet {:active "Layout 1" :layouts [{:name "Layout 1" :panels (layout/default-layout :tablet)}]}}
             (layout/decode "{\"version\":1,\"layouts\":{\"watch\":[],\"tablet\":[]}}"))))))

(deftest added-panels-tiers-and-bindings
  (testing "parameter tiles and the visualization panel survive repair; bad bindings are dropped"
    (let [repaired (layout/normalize :desktop [{:id "param-3" :w 1 :h 6 :binding {:cue " ember " :param "level"}}
                                               {:id "viz"} {:id "viz"} {:id "param-x"}
                                               {:id "param-4" :binding {:cue "" :param "level"}}])]
      (is (= [:param-3 :viz :param-4 :cues :live :room :tools] (ids repaired)))
      (is (= {:cue "ember" :param "level"} (:binding (first repaired))))
      (is (not (contains? (nth repaired 2) :binding)))))
  (testing "adding numbers tiles after the highest, the visualization panel only once"
    (let [one (layout/add-panel :desktop (layout/default-layout :desktop) :param)
          two (layout/add-panel :desktop (layout/remove-panel (layout/add-panel :desktop one :param) :param-1) :param)]
      (is (= {:id :param-1 :w 2 :h 6} (peek one)))
      (is (= [:cues :live :room :tools :param-2 :param-3] (ids two)))
      (is (= 1 (count (filter #{:viz} (ids (layout/add-panel :desktop (layout/add-panel :desktop one :viz) :viz))))))
      (is (= (layout/default-layout :desktop) (layout/remove-panel (layout/default-layout :desktop) :room))
          "a Perform panel cannot be removed")))
  (testing "binding and unbinding a tile"
    (let [bound (layout/bind custom :param-2 {:cue "moon" :param "speed"})]
      (is (= {:cue "moon" :param "speed"} (:binding (nth bound 2))))
      (is (not (contains? (nth (layout/bind bound :param-2 nil) 2) :binding)))))
  (testing "tiers follow the share of the grid a tile takes"
    (is (= :single (layout/tier :desktop {:w 2 :h 6})))
    (is (= :single (layout/tier :desktop {:w 12 :h 8})) "a short strip shows one reading")
    (is (= :compact (layout/tier :desktop {:w 4 :h 30})))
    (is (= :full (layout/tier :desktop {:w 5 :h 30})))
    (is (= :single (layout/tier :phone {:w 1 :h 20})) "half a phone is a 1×1 tile")
    (is (= :full (layout/tier :phone {:w 2 :h 20})))
    (is (= :single (layout/tier :tablet {:w 1 :h 20})))
    (is (= :compact (layout/tier :tablet {:w 2 :h 20})))))

(deftest moving-reorders-without-losing-panels
  (let [start (layout/default-layout :desktop)]
    (is (= [:live :cues :room :tools] (ids (layout/move start :live -1))))
    (is (= [:cues :live :room :tools] (ids (layout/move start :cues -1))) "the first panel stays first")
    (is (= [:live :room :tools :cues] (ids (layout/move start :cues 99))))
    (testing "a drop lands after a later target and before an earlier one"
      (is (= [:live :room :cues :tools] (ids (layout/move-to start :cues :room))))
      (is (= [:tools :cues :live :room] (ids (layout/move-to start :tools :cues))))
      (is (= start (layout/move-to start :cues :unknown))))))

(deftest resizing-stays-on-the-grid
  (let [start (layout/default-layout :desktop)]
    (is (= {:id :room :w 6 :h 28} (nth (layout/resize :desktop start :room 1 2) 2)))
    (is (= {:id :room :w 12 :h layout/max-rows} (nth (layout/set-size :desktop start :room 30 500) 2)))
    (is (= {:id :room :w 1 :h layout/min-rows} (nth (layout/set-size :desktop start :room 0 1) 2)))))

(deftest measured-boxes-become-grid-spans
  ;; 1200 px over 12 columns with 16 px gaps: 85.33 px columns, 101.33 px steps.
  (is (= [4 15] (layout/span-for :desktop 1200 389.3 300)))
  (is (= [12 layout/max-rows] (layout/span-for :desktop 1200 5000 5000)))
  (testing "a corner drag adds its distance to the starting box"
    (is (= [5 20] (layout/drag-size :desktop {:container-width 1200 :width 389.3 :height 300} 101 100))))
  (testing "the first layout is measured from what is on screen; hidden panels get defaults"
    (let [seeded (layout/seed :phone {:width 366 :regions [{:id :cues :width 366 :height 508}
                                                            {:id :live :width 0 :height 0}
                                                            {:id :room}
                                                            {:id :tools :width 366 :height 0}]})]
      (is (= [{:id :cues :w 2 :h 26} (layout/default-entry :phone :live)
              (layout/default-entry :phone :room) (layout/default-entry :phone :tools)]
             seeded)))))

(deftest dragging-near-an-edge-scrolls-faster-closer-to-it
  (is (= 0 (layout/edge-speed 400 800)))
  (is (= -18 (layout/edge-speed 0 800)))
  (is (= -9 (layout/edge-speed 32 800)))
  (is (= 9 (layout/edge-speed 768 800)))
  (is (= 18 (layout/edge-speed 900 800)) "a pointer past the bottom scrolls at full speed"))

(deftest a-drag-finds-the-panel-under-the-pointer
  (let [rects [{:id :cues :left 0 :top 0 :width 400 :height 600}
               {:id :live :left 416 :top 0 :width 784 :height 1200}]]
    (is (= :live (layout/target-at rects :cues 500 300)))
    (is (nil? (layout/target-at rects :cues 100 100)) "the dragged panel is never its own target")
    (is (nil? (layout/target-at rects :cues 408 100)) "a gap is not a target")))

(defn- saved-of [db] (get-in db [:ui :panel-layout :saved]))

(deftest editing-saves-only-on-done-and-cancel-restores
  (let [editing (run desktop [:panel-layout/began nil])
        moved (run editing [:panel-layout/move :room -2] [:panel-layout/resize :room 1 2])]
    (is (layout/editing? editing))
    (is (= "Layout 1" (get-in editing [:ui :panel-layout :edit :name])) "a new layout gets a free name")
    (is (= (layout/default-layout :desktop) (layout/active-layout editing)))
    (is (= [:room :cues :live :tools] (ids (layout/active-layout moved))))
    (is (re-find #"Room · position 1 of 4 · 6 columns by 28 rows" (get-in moved [:ui :panel-layout :edit :announce])))
    (testing "cancel keeps the built-in layout"
      (is (nil? (layout/active-layout (run moved [:panel-layout/cancel])))))
    (testing "done stores the named layout for this size class only and makes it active"
      (let [effects (transition (run moved [:panel-layout/name "  Show  "]) [:panel-layout/done])
            saved (saved-of (:db effects))]
        (is (= {:active "Show"} (select-keys (:desktop saved) [:active])))
        (is (= [:room :cues :live :tools] (ids (layout/find-panels saved :desktop "Show"))))
        (is (= {:saved saved :notice "Saved “Show” for this desktop screen."} (:panel-layout/store effects))
            "the confirmation travels with the write and shows only if it succeeds")
        (is (nil? (:dispatch effects)))
        (is (nil? (get-in effects [:db :ui :panel-layout :edit])))
        (testing "another size class still uses the built-in layout"
          (is (nil? (layout/active-layout (assoc-in (:db effects) [:ui :panel-layout :class] :phone)))))
        (testing "reset switches to the built-in layout and keeps the saved one in the menu"
          (let [reset (transition (:db effects) [:panel-layout/reset])]
            (is (nil? (get-in (:panel-layout/store reset) [:saved :desktop :active])))
            (is (= ["Show"] (map :name (get-in (:panel-layout/store reset) [:saved :desktop :layouts]))))
            (is (nil? (layout/active-layout (:db reset))))
            (testing "and the menu switches back to it"
              (is (= [:room :cues :live :tools] (ids (layout/active-layout (run (:db reset) [:panel-layout/select "Show"]))))))))
        (testing "editing a saved layout starts from it rather than measuring again"
          (let [begun (transition (:db effects) [:panel-layout/begin])]
            (is (nil? (:panel-layout/measure begun)))
            (is (= {:name "Show" :original "Show"} (select-keys (get-in begun [:db :ui :panel-layout :edit]) [:name :original])))))
        (testing "renaming replaces the layout in place; save as new keeps the original"
          (let [db (:db effects)
                renamed (run db [:panel-layout/begin] [:panel-layout/name "Encore"] [:panel-layout/done])
                copied (run db [:panel-layout/begin] [:panel-layout/move :tools -3] [:panel-layout/save-as])]
            (is (= ["Encore"] (map :name (layout/named (saved-of renamed) :desktop))))
            (is (= ["Show" "Show 2"] (map :name (layout/named (saved-of copied) :desktop))))
            (is (= "Show 2" (layout/active-name (saved-of copied) :desktop)))
            (is (= [:room :cues :live :tools] (ids (layout/find-panels (saved-of copied) :desktop "Show"))))
            (is (= [:tools :room :cues :live] (ids (layout/find-panels (saved-of copied) :desktop "Show 2"))))
            (testing "a name another layout already has gets a number"
              (let [clash (run copied [:panel-layout/begin] [:panel-layout/name "Show"] [:panel-layout/done])
                    fresh (run copied [:panel-layout/reset] [:panel-layout/began nil] [:panel-layout/name "Show"] [:panel-layout/done])]
                (is (= ["Show" "Show 2"] (map :name (layout/named (saved-of clash) :desktop))) "renaming onto a taken name keeps a free one")
                (is (= ["Show" "Show 2" "Show 3"] (map :name (layout/named (saved-of fresh) :desktop))))))
            (testing "delete removes the edited layout and returns to the built-in one"
              (let [deleted (transition (run copied [:panel-layout/begin]) [:panel-layout/delete])]
                (is (= ["Show"] (map :name (get-in (:panel-layout/store deleted) [:saved :desktop :layouts]))))
                (is (nil? (layout/active-layout (:db deleted))))))))))
    (testing "selecting an unknown layout or while editing changes nothing"
      (is (nil? (transition desktop [:panel-layout/select "Nope"])))
      (is (nil? (transition editing [:panel-layout/select nil]))))
    (testing "without a saved layout, begin measures the built-in one"
      (is (true? (:panel-layout/measure (transition desktop [:panel-layout/begin])))))))

(deftest added-panels-are-edited-bound-and-their-windows-follow-the-layout
  (let [editing (run desktop [:panel-layout/began nil] [:panel-layout/add :param] [:panel-layout/add :viz]
                     [:panel-layout/bind :param-1 {:cue "ember" :param "level"}])]
    (is (= [:cues :live :room :tools :param-1 :viz] (ids (layout/active-layout editing))))
    (is (= {:cue "ember" :param "level"} (:binding (nth (layout/active-layout editing) 4))))
    (testing "binding outside edit mode updates and stores the active layout"
      (let [done (run editing [:panel-layout/done])
            effects (transition done [:panel-layout/bind :param-1 {:cue "moon" :param "speed"}])]
        (is (= {:cue "moon" :param "speed"}
               (:binding (nth (layout/find-panels (:saved (:panel-layout/store effects)) :desktop "Layout 1") 4))))
        (testing "switching away from a layout redocks windows of panels it alone holds"
          (let [detached (assoc-in done [:ui :panel-windows] {:param-1 {:phase :popped :generation 1}
                                                              :live {:phase :popped :generation 1}})
                switched (transition detached [:panel-layout/select nil])]
            (is (= [[:panel-window/redock :param-1]] (:dispatch-n switched)))))))
    (testing "removing a tile in edit mode"
      (is (= [:cues :live :room :tools :viz] (ids (layout/active-layout (run editing [:panel-layout/remove :param-1]))))))))

(deftest pointer-gestures-preview-then-commit-or-cancel
  (let [rects [{:id :cues :left 0 :top 0 :width 400 :height 600}
               {:id :live :left 416 :top 0 :width 784 :height 1200}
               {:id :room :left 0 :top 600 :width 400 :height 360}
               {:id :tools :left 0 :top 960 :width 400 :height 240}]
        editing (run desktop [:panel-layout/began nil])
        drag {:kind :move :id :room :pointer-id 7 :x 100 :y 620 :rects rects :container-width 1200 :width 400 :height 360}]
    (testing "a drag over another panel marks it and the drop moves the panel there"
      (let [over (run editing [:panel-layout/gesture-start drag] [:panel-layout/gesture-move 7 600 300])]
        (is (= :live (get-in over [:ui :panel-layout :edit :gesture :target])))
        (is (= [:cues :live :room :tools] (ids (layout/active-layout over))) "nothing moves before the drop")
        (is (= [:cues :room :live :tools] (ids (layout/active-layout (run over [:panel-layout/gesture-end 7])))))))
    (testing "another pointer cannot steal or end the gesture"
      (let [over (run editing [:panel-layout/gesture-start drag] [:panel-layout/gesture-move 8 600 300]
                      [:panel-layout/gesture-end 8])]
        (is (some? (get-in over [:ui :panel-layout :edit :gesture])))
        (is (nil? (get-in over [:ui :panel-layout :edit :gesture :target])))))
    (testing "escape and pointer cancel abandon the drag"
      (doseq [ending [[:panel-layout/gesture-cancel 7] [:panel-layout/gesture-cancel nil]]]
        (let [after (run editing [:panel-layout/gesture-start drag] [:panel-layout/gesture-move 7 600 300] ending)]
          (is (nil? (get-in after [:ui :panel-layout :edit :gesture])))
          (is (= (layout/default-layout :desktop) (layout/active-layout after))))))
    (testing "escape with a gesture cancels it at once, so a later release commits nothing"
      (let [escaped (run editing [:panel-layout/gesture-start drag] [:panel-layout/gesture-move 7 600 300]
                         [:panel-layout/escape] [:panel-layout/gesture-end 7])]
        (is (layout/editing? escaped))
        (is (= (layout/default-layout :desktop) (layout/active-layout escaped)))))
    (testing "escape without a gesture cancels editing"
      (is (not (layout/editing? (run editing [:panel-layout/move :room -1] [:panel-layout/escape])))))
    (testing "a corner drag previews the snapped size and commits it on release"
      (let [corner (assoc drag :kind :resize :pointer-id 9)
            sizing (run editing [:panel-layout/gesture-start corner] [:panel-layout/gesture-move 9 204 720])]
        (is (= [5 23] (get-in sizing [:ui :panel-layout :edit :gesture :size])))
        (is (= {:id :room :w 5 :h 23} (nth (layout/active-layout (run sizing [:panel-layout/gesture-end 9])) 2)))))
    (testing "a press without movement changes nothing"
      (let [tapped (run editing [:panel-layout/gesture-start (assoc drag :kind :resize)] [:panel-layout/gesture-move 7 102 621]
                        [:panel-layout/gesture-end 7])]
        (is (= (layout/default-layout :desktop) (layout/active-layout tapped)))))))

(deftest focus-follows-the-toolbar-in-and-out-of-edit-mode
  (let [editing (run desktop [:panel-layout/began nil])]
    (is (= ".layout-handle" (:panel-layout/focus-control (transition desktop [:panel-layout/began nil])))
        "entering edit mode focuses the first panel's title")
    (doseq [event [[:panel-layout/done] [:panel-layout/cancel] [:panel-layout/reset] [:panel-layout/escape]]]
      (is (= "#edit-layout" (:panel-layout/focus-control (transition editing event)))
          (str (first event) " returns focus to Edit layout")))
    (testing "escape during a drag keeps focus where it is"
      (let [dragging (run editing [:panel-layout/gesture-start {:kind :move :id :room :pointer-id 1 :x 0 :y 0}])]
        (is (nil? (:panel-layout/focus-control (transition dragging [:panel-layout/escape]))))))))

(deftest changing-size-class-ends-editing-without-saving
  (let [editing (run desktop [:panel-layout/began nil] [:panel-layout/move :room -1])
        effects (transition editing [:panel-layout/class :tablet])]
    (is (= :tablet (get-in effects [:db :ui :panel-layout :class])))
    (is (nil? (get-in effects [:db :ui :panel-layout :edit])))
    (is (= {} (get-in effects [:db :ui :panel-layout :saved])))
    (is (= :studio/toast (first (:dispatch effects))))
    (is (nil? (transition editing [:panel-layout/class :desktop])) "the same class changes nothing")))

(defn- markup [db]
  (renderToStaticMarkup
   (r/as-element [layout/arrangement db {:class "preview-expanded"}
                  {:cues [:div.performance-launch "cues"] :live [:div.performance-live "live"]
                   :room [:div.performance-room "room"] :tools [:div.performance-tools "tools"]}])))

(deftest the-arrangement-keeps-the-built-in-layout-until-one-is-saved
  (let [built-in (markup desktop)]
    (is (re-find #"class=\"performance-layout preview-expanded\"" built-in))
    (is (not (re-find #"custom-layout|grid-column|layout-chrome" built-in)))
    (is (= ["cues" "live" "room" "tools"] (map second (re-seq #"data-panel=\"(\w+)\"" built-in)))))
  (let [saved (assoc-in desktop [:ui :panel-layout :saved :desktop]
                        {:active "Show" :layouts [{:name "Show" :panels [{:id :room :w 8 :h 20} {:id :cues :w 4 :h 30}
                                                                        {:id :live :w 12 :h 40} {:id :tools :w 4 :h 12}]}]})
        custom (markup saved)]
    (is (re-find #"custom-layout" custom))
    (is (re-find #"--layout-columns:12" custom))
    (is (= ["room" "cues" "live" "tools"] (map second (re-seq #"data-panel=\"(\w+)\"" custom))))
    (is (re-find #"data-panel=\"room\" style=\"grid-column:span 8;grid-row:span 20\" data-tier=\"full\"" custom))
    (is (re-find #"data-panel=\"cues\" style=\"[^\"]*\" data-tier=\"compact\"" custom))
    (is (not (re-find #"layout-chrome|inert" custom)) "no edit handles or shield outside edit mode")
    (testing "edit mode shields content and labels every handle"
      (let [editing (markup (run saved [:panel-layout/begin]))]
        (is (re-find #"editing-layout" editing))
        (is (= 4 (count (re-seq #"inert=\"\"" editing))))
        (is (re-find #"aria-label=\"Move Room panel, position 1 of 4. Drag, or use the arrow keys.\"" editing))
        (is (re-find #"aria-label=\"Resize Room, 8 of 12 columns by 20 rows" editing))
        (is (= 4 (count (re-seq #">Detach</button>" editing))) "every panel can be detached")
        (is (re-find #"title=\"Open Room in its own window\"" editing))
        (is (re-find #"title=\"Move Room later\" data-step=\"later\" class=\"layout-step\">Later<span class=\"sr-only\"> Room</span>" editing)
            "a step button's accessible name starts with its visible word")
        (is (not (re-find #"aria-pressed" editing)))))))
