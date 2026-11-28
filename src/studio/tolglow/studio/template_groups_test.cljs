(ns tolglow.studio.template-groups-test
  "Template group placements (#812) share the macro board and the card
  commands with the REPL."
  (:require [cljs.test :refer-macros [deftest is testing]]
            [re-frame.db :as rf-db]
            [re-frame.interceptor :as interceptor]
            [re-frame.registrar :as registrar]
            [tolglow.studio.events :as events]
            [tolglow.studio.performance :as performance]))

(def group
  {:id :verse-look :name "Verse look" :version 2
   :members [{:id :fill :cue-id :ember :role :wash} {:id :pulse :cue-id :ice}]
   :cards [{:slot-id :pulse :size :large}]
   :placements [{:placement :main :run-id "template-group/verse-look/main" :layer-id "1:9:main"
                 :member-slots [:fill :pulse] :version 2 :stale? false}
                {:placement :left :run-id "template-group/verse-look/left" :layer-id "1:12:left"
                 :member-slots [:fill] :version 1 :stale? true}]})

(defn- member [layer index id] {:id id :cue-id :ember :source {:native-layer {:id layer :member index}}})

(defn- transition [db event]
  (reset! rf-db/app-db db)
  (:effects (interceptor/execute event (remove #(= :do-fx (:id %)) (registrar/get-handler :event (first event))))))

(deftest each-placement-gets-its-own-board-section-with-the-template-cards
  (let [placements (performance/template-placements [group (assoc group :id :idle :placements [])])
        instances [(member "1:9:main" 0 "a") (member "1:9:main" 1 "b") (member "1:12:left" 0 "c")]
        boards (performance/macro-cards placements instances)]
    (is (= ["template-group/verse-look/main" "template-group/verse-look/left"] (mapv :id placements))
        "an idle template has no section")
    (is (= ["Verse look · main" "Verse look · left"] (mapv :name placements)))
    (is (= [false true] (mapv :stale? placements)))
    (testing "the template's arrangement orders and sizes each placement's cards"
      (is (= [[["b" :large] ["a" :small]] [["c" :small]]]
             (mapv (fn [{:keys [cards]}] (mapv (juxt (comp :id :instance) :size) cards)) boards))))
    (testing "a member a stale placement lacks has no card, and moves count only its members"
      (is (= [:fill] (mapv :slot-id (:cards (second boards)))))
      (is (= [[0 1]] (mapv (juxt :position :total) (:cards (second boards))))))
    (testing "an orphan placement keeps its launched members and no arrangement"
      (let [[orphan] (performance/template-placements [{:id :gone :orphan? true :members [{:id :x :cue-id :ember}]
                                                         :placements [{:placement :main :run-id "template-group/gone/main"
                                                                       :layer-id "1:3:main" :member-slots [:x]}]}])]
        (is (= [true nil [{:id :x}]] ((juxt :orphan? :cards :slots) orphan)))))))

(deftest placement-cards-go-to-the-template-and-macro-cards-stay-on-the-macro
  (let [macro {:id :opening-look :mode :macro :running? true :layer-id "1:4::opening-look"
               :member-slots [:a] :slots [{:id :a :cue-id :ember}]}
        db (-> (events/initial-db "template-groups-test")
               (assoc-in [:server-projection :view :compositions] [macro])
               (assoc-in [:server-projection :view :template-groups] [group]))]
    (is (= {:path "/api/template-group/cards" :typed? true :raw-response? true
            :body {:expected-epoch nil :id :verse-look :resize {:slot-id :fill :size :medium}}
            :on-failure [:studio/failure]}
           (:studio/http (transition db [:performance/macro-card-size "template-group/verse-look/left" :fill :medium]))))
    (is (= {:path "/api/composition/cards" :body {:expected-epoch nil :id :opening-look :move {:slot-id :a :offset 1}}}
           (select-keys (:studio/http (transition db [:performance/macro-card-move :opening-look :a 1])) [:path :body])))
    (testing "launch, stop and save send the REPL's messages"
      (is (= ["/api/template-group/launch" {:expected-epoch nil :id :verse-look}]
             ((juxt :path :body) (:studio/http (transition db [:performance/template-group-launch :verse-look])))))
      (is (= ["/api/template-group/stop" {:expected-epoch nil :id :verse-look}]
             ((juxt :path :body) (:studio/http (transition db [:performance/template-group-stop :verse-look])))))
      (is (= ["/api/template-group/stop" {:expected-epoch nil :id :verse-look :placement :left}]
             ((juxt :path :body) (:studio/http (transition db [:performance/template-group-stop :verse-look :left])))))
      (is (= ["/api/template-group/relaunch" {:expected-epoch nil :id :verse-look :placement :left}]
             ((juxt :path :body) (:studio/http (transition db [:performance/template-group-relaunch :verse-look :left])))))
      (is (= ["/api/template-group" {:expected-epoch nil :id :opening-look :expected-version nil :members [{:id :a :cue-id :ember}]}]
             ((juxt :path :body) (:studio/http (transition db [:performance/template-group-from-macro :opening-look])))))
      (is (nil? (transition db [:performance/template-group-from-macro :unknown]))))
    (testing "Save as template never reuses a taken ID"
      (let [db (assoc-in db [:server-projection :view :template-groups] [group {:id :opening-look} {:id "opening-look-2"}])]
        (is (= :opening-look-3 (get-in (transition db [:performance/template-group-from-macro :opening-look]) [:studio/http :body :id])))))))

(deftest a-placement-card-pin-goes-to-its-template
  (let [macro {:id :opening-look :mode :macro :running? true :layer-id "1:4::opening-look"
               :member-slots [:a] :slots [{:id :a :cue-id :ember}]}
        db (-> (events/initial-db "template-groups-test")
               (assoc-in [:server-projection :view :compositions] [macro])
               (assoc-in [:server-projection :view :template-groups] [group]))
        pins (fn [id parameters pinned?]
               (mapv (fn [[_ request]] ((juxt :path :body) request))
                     (:fx (transition db [:performance/macro-card-pin id :fill parameters pinned?]))))]
    (is (= [["/api/template-group/cards" {:expected-epoch nil :id :verse-look :pin {:slot-id :fill :parameter :level :pinned? true}}]
            ["/api/template-group/cards" {:expected-epoch nil :id :verse-look :pin {:slot-id :fill :parameter :tint :pinned? true}}]]
           (pins "template-group/verse-look/main" [:level :tint] true)))
    (is (= [["/api/composition/cards" {:expected-epoch nil :id :opening-look :pin {:slot-id :fill :parameter :level :pinned? false}}]]
           (pins :opening-look [:level] false))
        "a macro card's pin still goes to the macro")))

(deftest a-macro-becomes-a-template-with-its-members-cards-and-opacity
  (is (= {:id :m :name "M" :expected-version nil :opacity 0.5
          :members [{:id :a :cue-id :ember :params {:level 0.5}}] :cards [{:slot-id :a :size :large}]}
         (performance/template-from-macro {:id :m :name "M" :mode :macro :running? true :layer-id "x" :playback {} :opacity 0.5
                                           :slots [{:id :a :cue-id :ember :params {:level 0.5}}]
                                           :cards [{:slot-id :a :size :large}]}
                                          #{})))
  (is (= "deck-hall" (performance/free-template-id "deck/hall" #{})) "a / never reaches a template ID"))
