(ns tolglow.studio.card-grid-test
  "Parameter cards take standard slots from the shared description, and only
  a macro arranges its member cards (#795). The geometry itself is measured
  in Chromium by dev/studio/performance-layout-qa.mjs."
  (:require [cljs.test :refer-macros [deftest is testing]]
            [clojure.string :as str]
            ["react-dom/server" :refer [renderToStaticMarkup]]
            [re-frame.db :as rf-db]
            [re-frame.interceptor :as interceptor]
            [re-frame.registrar :as registrar]
            [reagent.core :as r]
            [tolglow.studio.events :as events]
            [tolglow.studio.performance :as performance]
            [tolglow.studio.subs]
            [tolglow.studio.views :as views]))

(def schemas
  {:level {:type :number :name "Level" :bounds [0 1]}
   :steps {:type :integer :name "Steps" :ui/widget :fader :bounds [0 8]}
   :on {:type :boolean :name "On"}
   :tint {:type :color :name "Tint"}
   :tone {:type :vector :size 4 :bounds [0 1] :name "Tone"}
   :offset {:type :vector :size 2 :bounds [-1 1] :name "Offset"}
   :aim {:type :vector :size 3 :bounds [0 1] :name "Aim"}
   :span {:type :range :bounds [0 1] :name "Span"}
   :drift {:type :vector :size 3 :name "Drift"}})

(def params {:level 0.2 :steps 3 :on true :tint [1 0 0] :tone [0.1 0.2 0.3 0.4]
             :offset [0 0] :aim [0.1 0.2 0.3] :span [0.2 0.8] :drift [1 2 3]})

(defn- row-tag [markup]
  (re-find #"<div [^>]*class=\"param-row[^\"]*\"[^>]*>" markup))

(defn- attribute [tag name] (second (re-find (re-pattern (str name "=\"([^\"]*)\"")) tag)))

(defn- render-row [parameter options]
  (reset! rf-db/app-db (assoc-in (events/initial-db "card-grid-test") [:show :instances] [{:id "i1" :params params}]))
  (row-tag (renderToStaticMarkup
            (r/as-element [@#'views/parameter-control {:id "i1" :params params} parameter (get schemas parameter) nil options]))))

(deftest every-parameter-card-names-its-standard-slot
  (doseq [[options surface] [[{:compact? true :strip? true} "strip"] [{} "detail"]]]
    (testing surface
      (is (= {:level ["rotary" "compact"] :steps ["fader" "compact"] :on ["boolean" "compact"]
              :tint ["color" "compact"] :tone ["rings" "compact"] :offset ["xy" "large"]
              :aim ["xyz" "large"] :span ["range" "wide"] :drift ["axes" "wide"]}
             (into {} (for [parameter (keys schemas)
                            :let [tag (render-row parameter options)]]
                        [parameter [(attribute tag "data-control") (attribute tag "data-slot")]])))))))

(def macro {:id :opening-look :mode :macro :name "Opening look" :running? true :layer-id "1:4::opening-look"
            :member-slots [:first :second :third]
            :slots [{:id :first :cue-id :ember} {:id :second :cue-id :ice} {:id :third :cue-id :spot}]
            :cards [{:slot-id :third :size :large} {:slot-id :first :size :medium}]})

(defn- member [id index] {:id id :cue-id :ember :source {:native-layer {:id "1:4::opening-look" :member index}}})

(deftest a-running-macro-arranges-its-member-cards
  (let [instances [(member "a" 0) (member "b" 1) (member "c" 2) {:id "d" :cue-id :ember}]
        macros (performance/macro-cards [macro {:id :steps :mode :sequence}] instances)
        cards (:cards (first macros))
        cards-of (fn [m running] (:cards (first (performance/macro-cards [m] running))))]
    (is (= 1 (count macros)) "a sequence has no section")
    (is (= [["c" :large] ["a" :medium] ["b" :small]] (mapv (juxt (comp :id :instance) :size) cards))
        "listed cards first in card order, the rest in slot order at the default size")
    (is (= [] (performance/macro-cards [(dissoc macro :layer-id)] instances)) "an idle macro has no section")
    (is (= [] (performance/macro-cards [(assoc macro :running? false)] instances)) "a stopped macro has no section")
    (testing "positions count the whole arrangement, as a move does"
      (is (= [[0 3] [2 3]] (mapv (juxt :position :total) (cards-of macro (take 2 (rest instances)))))))
    (testing "a member that is not running has no card"
      (is (= ["a" "b"] (mapv (comp :id :instance) (cards-of (dissoc macro :cards) (take 2 instances))))))
    (testing "typed member slots keep their identities"
      (is (= [:first :second :third] (mapv :slot-id (cards-of (dissoc macro :cards) (take 3 instances))))))))

(defn- transition [db event]
  (reset! rf-db/app-db db)
  (:effects (interceptor/execute event (remove #(= :do-fx (:id %)) (registrar/get-handler :event (first event))))))

(deftest the-size-picker-sends-the-shared-card-command
  (let [db (assoc-in (events/initial-db "card-grid-test") [:server-projection :view :compositions] [macro])]
    (testing "a size applies to the arrangement the show holds when it arrives"
      (is (= {:path "/api/composition/cards" :typed? true :raw-response? true
              ;; The session guard ties the command to the show epoch it was sent for.
              :body {:expected-epoch nil :id :opening-look :resize {:slot-id :first :size :small}}
              :on-failure [:studio/failure]}
             (:studio/http (transition db [:performance/macro-card-size :opening-look :first :small])))))
    (testing "a move names its slot and offset"
      (is (= {:slot-id :first :offset -1}
             (get-in (transition db [:performance/macro-card-move :opening-look :first -1]) [:studio/http :body :move]))))
    (is (nil? (transition db [:performance/macro-card-size :unknown :first :large])) "only a saved macro")))

(deftest saving-in-compose-leaves-the-cards-to-the-show
  (let [db (-> (events/initial-db "card-grid-test")
               (assoc-in [:ui :composition] {:id "m" :mode "macro" :cards [{:slot-id "a" :size "large"}]
                                             :slots [{:id "a" :cue-id "ember"}]}))]
    (is (= {:expected-epoch nil :id "m" :mode "macro" :slots [{:id "a" :cue-id "ember"}]}
           (get-in (transition db [:studio/composition-save false]) [:studio/http :body]))
        "the show keeps the saved arrangement, so a stale editor copy cannot revert it")))

(deftest small-macro-cards-show-their-leading-controls
  (let [control (fn [id schema] {:parameter id :schema schema})
        knob {:type :number :bounds [0 1]}
        pad {:type :vector :size 2 :bounds [0 1]}]
    (is (= [:a :b :c :d] (mapv :parameter (performance/small-controls (mapv #(control % knob) [:a :b :c :d :e]) nil))))
    (is (= [:p] (mapv :parameter (performance/small-controls [(control :p pad) (control :a knob)] nil))))
    (is (= [:a] (mapv :parameter (performance/small-controls [(control :a knob) (control :p pad)] nil))))
    (is (= [:a :c :d] (mapv :parameter (performance/small-controls (mapv control [:a :b :c :d] [knob pad knob knob]) nil)))
        "a control that does not fit is skipped and later ones keep filling the cells")
    (is (= [:p :q] (mapv :parameter (take 2 (performance/small-controls [(control :p {:type :range :bounds [0 1]})
                                                                          (control :q {:type :range :bounds [0 1]})] nil)))))))

(deftest pinned-small-cards-show-exactly-their-pins
  (let [control (fn [id schema] {:parameter id :schema schema})
        knob {:type :number :bounds [0 1]}
        pad {:type :vector :size 2 :bounds [0 1]}
        aim {:attribute :aim :components [[:aim-x knob] [:aim-y knob] [:aim-z knob]]}
        bank (conj (mapv #(control % knob) [:a :b :c :d :e]) aim)]
    (is (= [:e :b] (mapv :parameter (performance/small-controls bank nil [:e "b"])))
        "pins in pin order, and nothing unpinned")
    (is (= [:e :d :c :b] (mapv :parameter (performance/small-controls bank nil [:e :d :c :b :a])))
        "as many pins as fit in four cells")
    (is (= [aim] (performance/small-controls bank nil [:aim-z])) "any component pins its grouped control")
    (is (= [:a :b :c :d] (mapv :parameter (performance/small-controls bank nil [:gone])))
        "pins naming no control leave the leading controls")
    (is (= [:p] (mapv :parameter (performance/small-controls [(control :a knob) (control :p pad)] nil [:p]))))))

(deftest the-pin-picker-sends-the-shared-card-command
  (let [db (assoc-in (events/initial-db "card-grid-test") [:server-projection :view :compositions] [macro])
        bodies #(mapv (comp :body second) (:fx (transition db %)))]
    (is (= [{:expected-epoch nil :id :opening-look :pin {:slot-id :first :parameter :level :pinned? true}}]
           (bodies [:performance/macro-card-pin :opening-look :first [:level] true])))
    (is (= [:aim-x :aim-y] (mapv (comp :parameter :pin) (bodies [:performance/macro-card-pin :opening-look :first [:aim-x :aim-y] false])))
        "unpinning a grouped control removes each of its pins")
    (is (nil? (transition db [:performance/macro-card-pin :unknown :first [:level] true])) "only a saved macro")))

(deftest a-pin-press-pins-or-unpins-only-its-own-control
  (let [aim {:attribute :aim :components [[:aim-x {}] [:aim-y {}] [:aim-z {}]]}]
    (is (= [[:level] true] (performance/pin-press {:parameter :level} [:steps :tone])))
    (is (= [[:tone] false] (performance/pin-press {:parameter :tone} [:steps :tone :level]))
        "unpinning one control leaves the other pins alone")
    (is (= [[:aim-x] true] (performance/pin-press aim nil)) "a group pins its first component")
    (is (= [[:aim-z "aim-y"] false] (performance/pin-press aim [:aim-z :level "aim-y"]))
        "a pinned group unpins every pin naming one of its components")))

(deftest pins-that-do-not-fit-are-marked-not-numbered
  (let [control (fn [id schema] {:parameter id :schema schema})
        knob {:type :number :bounds [0 1]}
        pad {:type :vector :size 2 :bounds [0 1]}
        bank [(control :level knob) (control :offset pad) (control :steps knob)]]
    (is (= {:places {"param/offset" 1} :hidden #{"param/level"}}
           (performance/pin-places bank nil [:offset :level])))
    (is (= {:places {"param/steps" 1 "param/level" 2} :hidden #{}}
           (performance/pin-places bank nil [:steps :level])))
    (is (= {:places {} :hidden #{}} (performance/pin-places bank nil nil)))))

(deftest macro-cards-carry-their-pins
  (let [pinned (assoc macro :cards [{:slot-id :third :size :small :pins [:level]}])]
    (is (= [:level] (:pins (first (:cards (first (performance/macro-cards [pinned] [(member "c" 2)])))))))))

(deftest macro-sizes-are-fractions-of-the-board
  (is (= [[2 1] [3 2] [6 3]] (mapv #(performance/macro-span 6 % 5) [:small :medium :large])))
  (is (= [[1 1] [1 2] [2 3]] (mapv #(performance/macro-span 2 % 5) [:small :medium :large])))
  (is (= [[1 1] [1 2] [1 3]] (mapv #(performance/macro-span 1 % 5) [:small :medium :large])))
  (testing "a single-control member is short when large, and offers no medium"
    (is (= [6 2] (performance/macro-span 6 :large 1)))
    (is (= [:small :large] (performance/member-sizes 1)))
    (is (= [:small :medium :large] (performance/member-sizes 3)))))

(deftest a-macro-bank-stretches-its-tracks-to-the-card
  (let [measure (performance/fluid-measure "strip")]
    (is (= 4 (:columns (measure #js {:clientWidth 378}))) "four 84 px tracks fit 378 px")
    (is (= {"--slot-col" "93px" "--slot-row" "145px"} (performance/fluid-style "strip" 4 390)))
    (is (= {"--slot-col" "134px" "--slot-row" "210px"} (performance/fluid-style "strip" 1 390))
        "with many controls a track grows to at most 1.6 times the smallest")
    (is (= {"--slot-col" "215px" "--slot-row" "210px"} (performance/fluid-style "strip" 1 390 1))
        "a lone strip control widens to at most 2.56 times, near twice a strip knob")
    (is (= {"--slot-col" "358px" "--slot-row" "234px"} (performance/fluid-style "detail" 1 400 1))
        "a lone control in its Detail form: a wide slot of the usual height")))

(deftest opened-rings-cover-the-fewest-cards-beside-their-slot
  (testing "a free 2x2 area beside the rings is used before any card"
    (let [cells [{:index 0 :col 0 :row 0 :w 1 :h 1} {:index 1 :col 1 :row 0 :w 1 :h 1}]
          {:keys [covers style]} (performance/overlay-anchor 4 2 cells (first cells))]
      (is (= #{1} covers) "on one row of four, the area right of the rings still holds the next card")
      (is (string? (get style "--overlay-left")))))
  (testing "the widget study's Detail: Tone opens over one card, never over itself"
    (let [cells [{:index 0 :col 0 :row 0 :w 1 :h 1} {:index 3 :col 1 :row 0 :w 1 :h 1} {:index 1 :col 2 :row 0 :w 2 :h 2}
                 {:index 2 :col 0 :row 1 :w 2 :h 2} {:index 4 :col 2 :row 2 :w 1 :h 1}]
          {:keys [covers]} (performance/overlay-anchor 4 3 cells (second cells))]
      (is (= 1 (count covers)))
      (is (not (contains? covers 3)))))
  (testing "an empty area wins"
    (let [cells [{:index 0 :col 0 :row 0 :w 1 :h 1} {:index 1 :col 0 :row 1 :w 1 :h 1}]
          {:keys [covers]} (performance/overlay-anchor 3 2 cells (first cells))]
      (is (= #{} covers)))))
