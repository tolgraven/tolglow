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

(def macro {:id :opening-look :mode :macro :name "Opening look" :layer-id "1:4::opening-look"
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

(deftest saving-a-macro-in-compose-keeps-its-saved-cards
  (let [db (assoc-in (events/initial-db "card-grid-test") [:server-projection :view :compositions] [macro])
        edited {:id "opening-look" :mode "macro" :name "Opening look" :layer-id "x" :member-slots ["first"]
                :cards [{:slot-id "first" :size "large"}]
                :slots [{:id "first" :cue-id "ember"} {:id "second" :cue-id "ice"}]}]
    (testing "the show's cards win over a stale editor copy, pruned to the remaining slots"
      (is (= {:id "opening-look" :mode "macro" :name "Opening look"
              :cards [{:slot-id :first :size :medium}]
              :slots [{:id "first" :cue-id "ember"} {:id "second" :cue-id "ice"}]}
             (events/composition-for-save db edited))))
    (testing "a sequence never carries cards"
      (is (not (contains? (events/composition-for-save db (assoc edited :mode "sequence")) :cards))))
    (is (str/blank? (str (:cards (events/composition-for-save db (assoc edited :slots [{:cue-id "spot"}])))))
        "no remaining slot keeps no card")))
