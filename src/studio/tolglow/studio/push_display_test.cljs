(ns tolglow.studio.push-display-test
  (:require [thi.ng.color.core :as color]
            [cljs.test :refer-macros [deftest is]]
            [clojure.string :as str]
            ["react-dom/server" :refer [renderToStaticMarkup]]
            [re-frame.db :as db]
            [re-frame.interceptor :as interceptor]
            [re-frame.registrar :as registrar]
            [reagent.core :as r]
            [tolglow.studio.push-display :as display]
            [tolglow.studio.parameter-widgets :as widgets]))

(deftest passive-display-snapshot-is-bounded-and-emits-no-effects
  (reset! db/app-db {:unrelated :preserved})
  (let [view {:parameters (repeat 20 {:name "Level" :value 0.5}) :cells (range 16) :labels (range 10)
              :cue {:name "Ember"} :effect {:envelope {:attack {:beats 0.5}}} :ignored "not display data"}
        result (:effects (interceptor/execute [:push-display/update view]
                           (remove #(= :do-fx (:id %)) (registrar/get-handler :event :push-display/update))))]
    (is (= #{:db} (set (keys result))))
    (is (= :preserved (get-in result [:db :unrelated])))
    (is (= [8 8 8] (mapv #(count (get-in result [:db :push-display %])) [:parameters :cells :labels])))
    (is (nil? (get-in result [:db :push-display :ignored])))
    (is (= {:attack {:beats 0.5}} (get-in result [:db :push-display :effect :envelope])))))

(deftest waveform-glyphs-are-bounded-display-data
  (let [entry {:id "saw" :name "Saw" :glyph (vec (range 200)) :params {:duty 0.5}}
        glyphs (:waveform-glyphs (display/normalize-view {:waveform-glyphs (concat [7] (repeat 40 entry))}))]
    (is (= 31 (count glyphs)) "at most 32 entries are read and non-maps are dropped")
    (is (= #{:id :name :glyph} (set (keys (first glyphs)))))
    (is (= 129 (count (:glyph (first glyphs)))))
    (is (not (contains? (display/normalize-view {}) :waveform-glyphs)))))

(deftest the-active-track-feel-is-a-bounded-picture
  (let [icon "data:image/svg+xml;base64,PHN2Zy8+"
        shown (display/normalize-view {:personality {:id :squircle :name "Corner obsession" :icon icon :palette [[1 0 0]]}})]
    (is (= {:name "Corner obsession" :icon icon} (:personality shown)))
    (is (= 80 (count (get-in (display/normalize-view {:personality {:name (apply str (repeat 200 "x")) :icon icon}})
                             [:personality :name]))))
    (is (nil? (:personality (display/normalize-view {}))))
    (is (nil? (:personality (display/normalize-view {:personality {:name "Remote" :icon "https://example.invalid/a.svg"}})))
        "the passive display loads nothing but the supplied picture")
    (is (nil? (:personality (display/normalize-view
                             {:personality {:name "Large" :icon (apply str "data:image/svg+xml;base64," (repeat 20000 "A"))}}))))))

(deftest typed-values-share-labels-with-web-controls
  (is (= "Triangle" (widgets/value-label {:choices [{:value 1 :label "Triangle"}]} 1)))
  (is (= "On" (widgets/value-label {:type :boolean} true)))
  (is (= "0.5 · -1" (widgets/value-label {:type :vector} [0.5 -1])))
  (is (= "0.5 beats" (widgets/duration-label {:beats 0.5})))
  (is (= "rgb(255,0,128)" (widgets/color-css [2 -1 0.5])))
  (is (= 0.75 (widgets/proportion {:bounds [-1 1]} 0.5)))
  (is (= 1 (widgets/proportion {:bounds [0 1]} 2)))
  (is (nil? (widgets/proportion {:bounds [1 1]} 1))))

(deftest saved-show-catalog-is-a-bounded-passive-display-view
  (let [view {:display-mode "saved-shows"
              :saved-show-catalog {:page 0 :page-count 2 :total 10 :confirmation "private"
                                   :rows (repeat 10 {:name "Tour" :source {:path "private"}})
                                   :preview {:name "Tour" :path "private"
                                             :sources [{:label "Latest named" :path "private"}]}}}
        normalized (display/normalize-view view)]
    (is (= (repeat 8 {:name "Tour"}) (get-in normalized [:saved-show-catalog :rows])))
    (is (= {:name "Tour" :sources [{:label "Latest named"}]}
           (get-in normalized [:saved-show-catalog :preview])))
    (is (= "saved-shows" (:display-mode normalized)))
    (is (not (contains? (:saved-show-catalog normalized) :confirmation)))))

(deftest registered-preview-retains-only-display-metadata
  (let [catalog {:kind "cue" :next-kind? true
                 :rows [{:name "Amber" :version 5 :selected? true :selection {:key [:cue :amber]}}]
                 :preview {:name "Amber" :version 5 :effect-version 3 :effect-id :amber
                           :categories (concat [{:internal :discard}] (repeat 8 "emission"))
                           :usage {:page-count 1 :running-count 0 :internal :discard}}}
        normalized (:saved-show-catalog (display/normalize-view {:saved-show-catalog catalog}))]
    (is (= {:kind "cue" :next-kind? true
            :rows [{:name "Amber" :version 5 :selected? true}]
            :preview {:name "Amber" :version 5 :effect-version 3 :sources []
                      :categories (vec (repeat 6 "emission")) :usage {:page-count 1 :running-count 0}}}
           normalized))))

(deftest web-and-push-widgets-render-the-shared-color-record
  (let [v (color/rgba 1 0 0 0.5)]
    (is (= [1 0 0 0.5] (widgets/color-components v)))
    (is (= "rgba(255,0,0,0.5)" (widgets/color-css v)))
    (is (= "#123456" (widgets/color-css (color/int24 0x123456))))))

(deftest push-navigation-retains-authoritative-banks-and-touch-without-expanding-the-view
  (let [bank {:index 1 :count 2 :total 10 :offset 8}
        touch {:instance-id "effect-a" :param "color" :encoder 1 :component 0}
        parameter {:id "color" :component 0 :component-label "hue"}
        view (display/normalize-view {:parameter-bank bank :touched-parameter touch :fine? true
                                      :active-effect-navigation {:index 9 :count 12 :offset 8}
                                      :active-effects (mapv #(hash-map :id (str %)) (range 20))
                                      :parameters [parameter]})]
    (is (= "Params 9–10 / 10" (display/bank-label (:parameter-bank view))))
    (is (= touch (:touched-parameter view)))
    (is (= parameter (first (:parameters view))))
    (is (= {:index 9 :count 12 :offset 8} (:active-effect-navigation view)))
    (is (:fine? view))
    (is (= 8 (count (:active-effects view))))
    (is (nil? (display/bank-label {:index 0 :count 1 :total 8 :offset 0}))))
  (is (nil? (display/bank-label nil))))

(deftest beat-ratio-overlay-retains-exact-display-labels
  (let [cell {:position [1 6] :label "4/3" :selected? true}
        normalized (:pad-overlay (display/normalize-view
                                   {:pad-overlay {:layout "beat-ratios" :param "period" :current-ratio "4/3"
                                                  :cells (repeat 20 (assoc cell :value (/ 4 3)))}}))]
    (is (= {:layout "beat-ratios" :param "period" :current-ratio "4/3"}
           (dissoc normalized :cells)))
    (is (= (vec (repeat 16 cell)) (:cells normalized)))))

(deftest a-push-2-cue-grid-overlay-keeps-the-parameter-display
  (let [view {:title "Select running cues · page 1" :parameters [{:id :level :name "Level" :value 0.5}]
              :pad-overlay {:layout :cue-grid :mode :select :page 1 :count 3 :cells (range 64)}}
        normalized (display/normalize-view view)
        html (renderToStaticMarkup (r/as-element [display/display normalized]))]
    (is (= {:layout :cue-grid :mode :select :page 1 :count 3} (:pad-overlay normalized)))
    (is (not (str/includes? html "Cue variations")) "Not drawn as the variation overlay")
    (is (str/includes? html "Level") "The encoders still show the selected launch's parameters")))

(deftest explicit-audio-mode-keeps-the-actual-capture-projection
  (let [waveform {:bins [{:min -0.5 :max 0.4 :rms 0.2 :bands {:low 0.8}}] :sample-end 512}
        view (display/normalize-view {:display-mode "audio" :audio-waveform waveform
                                      :parameters [{:id :level :value 0.7}]})]
    (is (= "audio" (:display-mode view)))
    (is (= waveform (:audio-waveform view)))
    (is (= 1 (count (:parameters view))))
    (is (nil? (:audio-waveform (display/normalize-view {}))))))

(deftest preset-projection-distinguishes-edits-saved-values-and-record-actions
  (let [view {:cue {:id "ember"} :instance-id "0:ember"}
        project #(display/preset-model (display/normalize-view (assoc view :preset %)))]
    (is (= :default (:state (project {:saved? false :changed? false}))))
    (is (= :saved (:state (project {:saved? true :changed? false}))))
    (is (= :changed (:state (project {:saved? true :changed? true}))))
    (is (= :save (:action (project {:record? true :action "save"}))))
    (is (= "Clear preset · top button" (:hint (project {:record? true :action :clear}))))
    (is (nil? (:action (project {:record? false :action :clear}))))
    (is (nil? (display/preset-model {:preset {:saved? true}})))
    (is (nil? (:hint (display/preset-model {:cue {:id :idle} :preset {:record? true :action :save}}))))
    (is (= {:saved? true} (:preset (display/normalize-view {:preset {:saved? true :extra :discarded}}))))))

(deftest touched-global-controls-use-real-units-bounds-and-sync-availability
  (let [master (display/global-control-model {:master 0.73 :touched-control {:target :master :editable? true}})
        tempo (display/global-control-model {:bpm 128.5 :touched-control {:target "tempo" :editable? false}})]
    (is (= 73 (:value master)))
    (is (= {:type :number :bounds [0 100] :unit "%"} (:schema master)))
    (is (false? (:locked? master)))
    (is (= 128.5 (:value tempo)))
    (is (= {:type :number :bounds [20 200] :unit "BPM"} (:schema tempo)))
    (is (:locked? tempo))
    (is (= 240 (:value (display/global-control-model {:bpm 240 :touched-control {:target :tempo :editable? false}}))))
    (is (nil? (display/global-control-model {:touched-control {:target :unknown}})))
    (is (nil? (display/global-control-model (display/normalize-view {}))))))

(deftest passive-updates-clear-prior-global-touch-and-preset-feedback
  (reset! db/app-db {:push-display {:preset {:record? true :action :clear}
                                  :touched-control {:target :tempo :editable? false}}})
  (let [result (:effects (interceptor/execute [:push-display/update {:title "Replacement"}]
                         (remove #(= :do-fx (:id %)) (registrar/get-handler :event :push-display/update))))]
    (is (= #{:db} (set (keys result))))
    (is (nil? (get-in result [:db :push-display :preset])))
    (is (nil? (get-in result [:db :push-display :touched-control])))))

(deftest traces-require-real-finite-samples
  (is (nil? (widgets/sample-points nil)))
  (is (nil? (widgets/sample-points [0 js/NaN 1])))
  (is (= "0,16 100,16" (widgets/sample-points [1 1])))
  (is (= "0,28 50,4 100,28" (widgets/sample-points [{:value 0} {:value 1} {:value 0}]))))

(deftest rotary-controls-preserve-range-units-discrete-steps-and-bipolar-zero
  (let [bipolar {:type :number :bounds [-180 180] :unit :degrees}
        model (widgets/dial-model bipolar 90)]
    (is (= 0.75 (:amount model)))
    (is (= 0.5 (:origin model)))
    (is (:bipolar? model))
    (is (= "degrees" (widgets/unit-label bipolar)))
    (is (= [-180 180] (:bounds model)))
    (is (= -180 (widgets/keyboard-value bipolar 90 "Home")))
    (is (= 180 (widgets/keyboard-value bipolar 90 "End")))
    (is (= 180 (widgets/keyboard-value bipolar 179 "ArrowUp")))
    (is (nil? (widgets/keyboard-value bipolar 90 "Enter"))))
  (is (= 3 (widgets/normalized-value {:type :integer :bounds [0 8]} 0.4)))
  (is (= 9 (count (:ticks (widgets/dial-model {:type :integer :bounds [0 8]} 3)))))
  (is (nil? (widgets/dial-model {:bounds [1 1]} 1)))
  (is (nil? (widgets/dial-model {:bounds [0 1]} js/NaN))))

(deftest waveform-choices-take-precedence-over-numeric-storage-type
  (is (= :choice (widgets/parameter-kind {:type :number :choices [{:value 0 :label "Sine"}]})))
  (is (= :integer (widgets/parameter-kind {:type "integer"})))
  (is (= :color (widgets/parameter-kind {:type :color}))))

(deftest captured-audio-is-bounded-and-color-comes-from-real-band-energy
  (let [bin {:min -0.5 :max 0.75 :rms 0.3 :bands {:low 1 :mid 0 :high 0}}]
    (is (= [bin] (widgets/audio-bins {:bins [bin]})))
    (is (= "rgb(68,145,235)" (widgets/audio-bin-color (:bands bin))))
    (is (= "rgb(245,163,70)" (widgets/audio-bin-color {:mid 1}))))
  (is (nil? (widgets/audio-bins {:bins []})))
  (is (nil? (widgets/audio-bins {:bins [{:min 0.8 :max 0.2 :rms 0.5}]})))
  (is (nil? (widgets/audio-bins {:bins [{:min -1 :max js/NaN :rms 0.5}]})))
  (is (nil? (widgets/audio-bins {:bins (vec (repeat 513 {:min 0 :max 0 :rms 0}))})))
  (is (= 1 (count (widgets/audio-bins {:bins [{:min 0 :max 0 :rms 0}]})))))

(deftest target-picker-bounds-visible-rows-and-omits-device-internals
  (let [view (display/normalize-view {:target-picker {:open? true :page 2 :pages 4
                                                     :rows (repeat 30 {:kind :volume :label "Orbit"
                                                                       :selector {:volume-id :orbit}})}})]
    (is (= (vec (repeat 8 {:kind :volume :label "Orbit"})) (get-in view [:target-picker :rows])))
    (is (= 2 (get-in view [:target-picker :page])))))

(deftest operator-readouts-preserve-dmx-resolution
  (let [codes (fn [bits] (let [max-code (dec (bit-shift-left 1 bits))]
                           (map #(widgets/number-label {:dmx-bits bits} (/ % max-code))
                                (range (inc max-code)))))]
    (is (= "0.123" (widgets/number-label 0.123456789)))
    (is (= "0.12346" (widgets/number-label {:dmx-bits 16} 0.123456789)))
    (is (= "114.123" (widgets/number-label {:dmx-bits 16 :bounds [-180 180]} 114.123456789)))
    (is (= "114" (widgets/number-label {:dmx-bits 8 :bounds [0 255]} 114.123456789)))
    (is (= "0" (widgets/number-label {:dmx-bits 8} -0.000000001)))
    (is (= "-1.23e-7" (widgets/number-label -0.0000001234)))
    (is (= "1 · -0.123" (widgets/value-label {:type :vector} [1.0 -0.123456789])))
    (is (= "123456" (widgets/number-label 123456)))
    (is (= "—" (widgets/number-label js/NaN)))
    (is (= 256 (count (distinct (codes 8)))))
    (is (= 65536 (count (distinct (codes 16)))))))

(deftest encoder-touches-match-the-projected-instance-with-or-without-an-encoder
  (let [color {:id "tint" :param "tint" :schema {:type "color"}}
        level {:id "level" :param "level" :schema {:type "number" :bounds [0 1]}}
        view {:instance-id "a" :parameters [(assoc color :component 0) (assoc color :component 1) level]}
        touched (fn [view] (mapv (comp boolean :touched?) (display/encoder-slots view)))]
    ;; Push 3 projects the parameter without an encoder; Push 2 also names one.
    (is (= [true true false false false false false false]
           (touched (assoc view :touched-parameter {:instance-id "a" :param "tint" :component 0}))))
    (is (= [false true false false false false false false]
           (touched (assoc view :touched-parameter {:instance-id "a" :param "tint" :encoder 1}))))
    (is (every? false? (touched (assoc view :touched-parameter {:instance-id "b" :param "tint"}))))
    (is (every? false? (touched view)))
    (is (= [true false true false false false false false]
           (touched (assoc view :touched-parameter {:instance-id "a" :param "tint" :encoder 0}
                                :touched-parameters [{:instance-id "a" :param "level" :encoder 2}]))))
    (is (= 8 (count (:touched-parameters (display/normalize-view {:touched-parameters (repeat 20 {:param "x"})})))))))

(deftest touched-compound-values-extend-only-into-free-adjacent-slots
  (let [color {:id "tint" :param "tint" :schema {:type "color"}}
        level {:id "level" :param "level" :schema {:type "number" :bounds [0 1]}}
        touch {:instance-id "a" :param "tint"}
        view (fn [parameters & [t]] {:instance-id "a" :touched-parameter t :parameters parameters})
        areas (fn [v] (mapv #(select-keys % [:start :width]) (display/compound-areas (display/encoder-slots v))))]
    (is (= [{:start 1 :width 4}] (areas (view [level color color nil nil level] touch))))
    (is (= [] (areas (view [level color color nil nil level]))) "untouched compounds keep their slots")
    (is (= [] (areas (view [color level] touch))) "a single slot followed by another parameter stays in place")
    (is (= [{:start 0 :width 8}] (areas (view [color] touch))))
    (is (= [] (areas (view [level] {:instance-id "a" :param "level"}))) "scalars never expand")))

(deftest a-vector-spread-over-a-full-bank-draws-as-one-ring-over-its-encoders
  ;; The device view of a Push vector takeover (#813, tolglow.surface.vector-encoders):
  ;; slots 2-5 of a full bank carry the ring's components.
  (let [ring {:id "offset" :param "offset" :name "Offset" :value [0.25 0.5 0.75 1]
              :schema {:type "vector" :size 4 :bounds [0 1]}}
        level (fn [id] {:id id :param id :name id :value 0.5 :schema {:type "number" :bounds [0 1]}})
        view {:instance-id "a"
              :parameters (vec (concat [(level "a") (level "b")]
                                       (for [i (range 4)] (assoc ring :component i :component-label (nth ["X" "Y" "Z" "W"] i)))
                                       [(level "f") (level "g")]))
              :touched-parameters [{:instance-id "a" :param "offset" :component 0 :encoder 2}]}
        normal (display/normalize-view view)
        html (renderToStaticMarkup (r/as-element [display/display normal]))]
    (is (= [{:start 2 :width 4}]
           (mapv #(select-keys % [:start :width]) (display/compound-areas (display/encoder-slots normal)))))
    (is (str/includes? html "Offset components"))
    (is (= 4 (count (re-seq #"ring-component ring-" html))))))

(deftest the-push-display-draws-each-colour-layout-in-its-own-space
  ;; A device snapshot carries its schema through JSON, so the layout is text.
  (doseq [[layout value css letters] [["rgb" [1 0.5 0] "rgb(255,128,0)" ["R" "G" "B"]]
                                      ["rgba" [0 0 1 0.5] "rgb(0,0,255)" ["R" "G" "B"]]
                                      ["rgbw" [0.5 0 0 0.5] "rgb(255,128,128)" ["R" "G" "B" "W"]]
                                      ["hsl" [(/ 1 3) 1 0.5] "rgb(0,255,0)" ["H" "S" "L"]]
                                      ["hsla" [(/ 2 3) 1 0.25 0.5] "rgb(0,0,128)" ["H" "S" "L"]]
                                      ["hsv" [0 1 1] "rgb(255,0,0)" ["H" "S" "V"]]]
          touched? [false true]
          :let [tint {:id "tint" :param "tint" :name "Tint" :value value
                      :schema {:type "vector" :size (count value) :semantic/type layout :bounds [0 1]}}
                view (cond-> {:instance-id "a" :parameters [(assoc tint :component 0)]}
                       touched? (assoc :touched-parameter {:instance-id "a" :param "tint" :component 0}))
                html (renderToStaticMarkup (r/as-element [display/display (display/normalize-view view)]))
                where (str layout (when touched? " touched"))]]
    (is (str/includes? html (str "background:" css)) where)
    (is (= letters (mapv second (re-seq #"<span class=\"color-channel-name\">([^<]+)</span>" html)))
        where)
    (is (= (contains? #{"rgba" "hsla"} layout) (str/includes? html "α")) where)))

(deftest a-touched-knob-shows-its-links-in-the-encoder-area
  ;; Transit/JSON delivery turns keywords into strings; both spellings render.
  (let [view {:touched-control {:target "knob" :knob-id "look" :editable? true}
              :knob {:knob-id "look" :name "Look" :value 0.5 :text "0.500"
                     :columns (concat [{:label "tint · saturation" :text "0.600" :status "ok"}
                                       {:label "pulse · level LFO period" :text "1 beats" :ratio [1 1] :status "ok"
                                        :diverged? true}
                                       {:label "gone · level" :status "dangling"}]
                                      (repeat 10 {:label "extra" :text "x" :status "ok"}))
                     :ignored "not display data"}
              :parameters [{:name "Level" :value 0.5}]}
        normal (display/normalize-view view)
        html (renderToStaticMarkup (r/as-element [display/display normal]))]
    (is (= {:target "knob" :editable? true :knob-id "look"} (:touched-control normal)))
    (is (= 8 (count (get-in normal [:knob :columns]))))
    (is (nil? (get-in normal [:knob :ignored])))
    (is (= :knob (:kind (display/knob-model normal))))
    (is (str/includes? html "Knob Look, 0.500"))
    (is (str/includes? html "1 beats"))
    (is (str/includes? html "Edited since the knob last moved"))
    (is (str/includes? html "class=\"dangling\""))
    (is (not (str/includes? html "Eight encoder parameters")) "the knob replaces the parameter row while touched")))

(deftest the-knob-page-lists-its-knobs-and-marks-a-missing-one
  (let [normal (display/normalize-view
                 {:knob-page {:knob-ids ["look" "gone"]
                              :knobs [{:knob-id "look" :name "Look" :value 0.25 :text "0.250" :links 2}
                                      {:knob-id "gone" :missing? true}]}})
        html (renderToStaticMarkup (r/as-element [display/display normal]))]
    (is (= :page (:kind (display/knob-model normal))))
    (is (str/includes? html "Knob page"))
    (is (str/includes? html "2 links"))
    (is (str/includes? html "Missing knob"))
    (is (nil? (display/knob-model (display/normalize-view {:touched-control {:target "master"}}))))))

(deftest knob-looks-show-empty-saved-selected-and-moved-top-row-pads
  (let [cell (fn [slot state & [extra]]
               (merge {:position [slot 7] :slot slot :state state :label (str "Look " (inc slot))} extra))
        knobs [{:name "Look" :knob-id :look :text "0.200"} {:name "Rate" :knob-id :rate :text "0.100" :missing? true}]
        view {:pad-overlay {:layout :knob-presets :title "Knob looks" :ignored true
                            :cells [(cell 0 :selected {:matches? true :knobs knobs :values {:look 0.2}})
                                    (cell 1 :stored {:matches? false :knobs (repeat 9 (first knobs))})
                                    (cell 2 :empty)
                                    (cell 3 "selected" {:matches? false :knobs knobs})]}}
        shown (display/normalize-view view)
        html (renderToStaticMarkup (r/as-element [display/display shown]))]
    (is (= #{:layout :title :cells} (set (keys (:pad-overlay shown)))))
    (is (nil? (get-in shown [:pad-overlay :cells 0 :values])) "stored positions stay out of display data")
    (is (= 4 (count (get-in shown [:pad-overlay :cells 1 :knobs]))))
    (is (str/includes? html "Knob looks"))
    (is (str/includes? html "Look 0.200"))
    (is (str/includes? html "Rate 0.100 ✕"))
    (is (= 1 (count (re-seq #"class=\"selected\"" html))))
    (is (= 1 (count (re-seq #"class=\"moved\"" html))))
    (is (= 1 (count (re-seq #"class=\"available\"" html))))
    (is (= 5 (count (re-seq #"class=\"empty\"" html))) "the empty slot and four uncaptured pads")))

(deftest a-touched-knob-shows-its-value-while-knob-looks-are-open
  (let [view {:pad-overlay {:layout :knob-presets :title "Knob looks"
                            :cells [{:position [0 7] :slot 0 :state :empty :label "Look 1"}]}
              :touched-control {:target "knob" :knob-id "look" :editable? true}
              :knob {:knob-id "look" :name "Look" :value 0.5 :text "0.500"
                     :columns [{:label "tint · saturation" :text "0.600" :status "ok"}]}}
        html (renderToStaticMarkup (r/as-element [display/display (display/normalize-view view)]))]
    (is (str/includes? html "Knob Look, 0.500"))
    (is (not (str/includes? html "Knob looks")) "the looks return when the encoder is released")
    (is (str/includes? (renderToStaticMarkup (r/as-element [display/display (display/normalize-view (dissoc view :touched-control))]))
                       "Knob looks"))))
