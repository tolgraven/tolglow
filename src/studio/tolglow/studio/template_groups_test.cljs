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
    (testing "a member a stale placement lacks has no card"
      (is (= [:fill] (mapv :slot-id (:cards (second boards))))))))

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
      (is (= ["/api/template-group" {:expected-epoch nil :id :opening-look :members [{:id :a :cue-id :ember}]}]
             ((juxt :path :body) (:studio/http (transition db [:performance/template-group-from-macro :opening-look])))))
      (is (nil? (transition db [:performance/template-group-from-macro :unknown]))))))

(deftest a-macro-becomes-a-template-with-its-members-and-cards
  (is (= {:id :m :name "M" :members [{:id :a :cue-id :ember :params {:level 0.5}}] :cards [{:slot-id :a :size :large}]}
         (performance/template-from-macro {:id :m :name "M" :mode :macro :running? true :layer-id "x" :playback {}
                                           :slots [{:id :a :cue-id :ember :params {:level 0.5}}]
                                           :cards [{:slot-id :a :size :large}]}))))
