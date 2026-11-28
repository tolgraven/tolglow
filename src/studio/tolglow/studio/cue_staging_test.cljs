(ns tolglow.studio.cue-staging-test
  (:require [cljs.test :refer-macros [deftest is testing]]
            [clojure.string :as str]
            ["react-dom/server" :refer [renderToStaticMarkup]]
            [re-frame.db :as rf-db]
            [re-frame.interceptor :as interceptor]
            [re-frame.registrar :as registrar]
            [reagent.core :as r]
            [tolglow.studio.cue-staging :as staging]))

(def ^:private history
  {:identity ["runtime" "show" 0] :current-id 2 :position 3
   :nodes [{:id 0 :parent-id nil}
           {:id 1 :parent-id 0 :stage-id "s" :parameter :level}
           {:id 2 :parent-id 1 :stage-id "s" :parameter :level}
           {:id 3 :parent-id 2 :stage-id "other" :parameter :level}
           {:id 4 :parent-id 2 :stage-id "s" :parameter :level}
           {:id 5 :parent-id 2 :stage-id "s" :parameter :level}]})

(def ^:private stage
  {:stage-id "s" :cue-id "ember" :cue-label "Ember" :effect-label "Ember" :status "staged"
   :params {:level 0.6 :mode "soft"}
   :schema {:level {:type "number" :bounds [0 1]} :mode {:type "enum"}}})

(defn- effects [event]
  (:effects (interceptor/execute event (remove #(= :do-fx (:id %))
                                               (registrar/get-handler :event (first event))))))

(deftest stage-undo-and-redo-stay-on-the-stage-steps
  (is (= {:history/undo {:identity ["runtime" "show" 0] :expected-cursor 2 :expected-history-position 3 :target-id 1}
          :history/redo {:identity ["runtime" "show" 0] :expected-cursor 2 :expected-history-position 3 :target-id 5}}
         (staging/history-commands history "s")))
  (testing "another stage's step is not this stage's undo"
    (is (= {:history/undo nil :history/redo {:identity ["runtime" "show" 0] :expected-cursor 2
                                             :expected-history-position 3 :target-id 3}}
           (staging/history-commands history "other"))))
  (testing "a step across a dead edge is never offered"
    (is (nil? (:history/redo (staging/history-commands
                              (update history :nodes (fn [nodes] (mapv #(cond-> % (#{4 5} (:id %))
                                                                          (assoc :unavailable-reason "gone"))
                                                                       nodes)))
                              "s"))))))

(deftest numeric-sliders-use-schema-bounds
  (is (= [0 1 (/ 1 200)] (staging/numeric-schema {:type "number" :bounds [0 1]})))
  (is (= [1 8 1] (staging/numeric-schema {:type :integer :bounds [1 8]})))
  (is (nil? (staging/numeric-schema {:type "enum"})))
  (is (nil? (staging/numeric-schema {:type "number"}))))

(deftest the-panel-shows-staged-values-and-sends-owner-commands
  (let [db {:show {:epoch 0 :runtime-id "runtime" :staged-cues [stage]}
            :server-projection {:view {:operator-history history}}
            :connection {:connected? true}
            :ui {:launch-grid "bar"}}
        html (renderToStaticMarkup (r/as-element [staging/panel db]))]
    (is (str/includes? html "data-stage-id=\"s\""))
    (is (str/includes? html "aria-label=\"level for the staged cue\""))
    (is (str/includes? html "Launch on bar"))
    (is (str/includes? html ">soft<") "a non-numeric parameter is shown, not edited")
    (is (= "" (renderToStaticMarkup (r/as-element [staging/panel (assoc-in db [:show :staged-cues] [])]))))
    (reset! rf-db/app-db db)
    (let [[effect request] (first (:fx (effects [::staging/launch "s" {:bars 1}])))]
      (is (= :studio/send-command effect))
      (is (= ["cue-stage/launch" "s" {:bars 1}]
             [(:op request) (get-in request [:fields :stage-id]) (get-in request [:fields :grid])])))
    (let [{:keys [db fx]} (effects [::staging/edit "s" :level 0.25])
          [_ request] (first fx)]
      (is (= {:value 0.25} (get-in db [:ui :cue-staging :drafts ["s" "level"]])))
      (is (= {:stage-id "s" :param "level" :value 0.25}
             (select-keys (:fields request) [:stage-id :param :value])))
      (is (= "cue-stage/edit" (:op request))))
    (let [[_ request] (first (:fx (effects [::staging/history :history/redo "s"])))]
      (is (= [:history/redo 5 2 3]
             [(:op request) (get-in request [:fields :node-id])
              (get-in request [:fields :expected-cursor]) (get-in request [:fields :expected-history-position])])))))

(deftest drafts-hold-until-the-acknowledged-revision-arrives
  (let [db {:show {:revision 4} :ui {:cue-staging {:drafts {["s" "level"] {:value 0.3}}}}}]
    (is (= 0.3 (staging/shown-value db "s" :level 0.7)) "unacknowledged")
    (let [acked (assoc-in db [:ui :cue-staging :drafts ["s" "level"] :revision] 6)]
      (is (= 0.3 (staging/shown-value acked "s" :level 0.7)) "the frame before the acknowledged revision")
      (is (= 0.7 (staging/shown-value (assoc-in acked [:show :revision] 6) "s" :level 0.7)) "the committed value after it"))
    (is (= 0.7 (staging/shown-value {:show {:revision 1}} "s" :level 0.7)))))

(deftest shared-controls-coalesce-into-one-edit-per-flush
  (reset! rf-db/app-db {:show {:epoch 0 :runtime-id "runtime" :revision 1} :connection {:connected? true}})
  (let [{:keys [db dispatch-later]} (effects [::staging/set "s" :color [1 0 0]])]
    (is (= [{:ms 80 :dispatch [::staging/flush "s" :color]}] dispatch-later))
    (reset! rf-db/app-db db)
    (let [second (effects [::staging/set "s" :color [0 1 0]])]
      (is (nil? (:dispatch-later second)) "one flush per queued parameter")
      (reset! rf-db/app-db (:db second))
      (let [[_ request] (first (:fx (effects [::staging/flush "s" :color])))]
        (is (= ["cue-stage/edit" [0 1 0]] [(:op request) (get-in request [:fields :value])]))))))

(deftest a-revision-note-names-old-and-new-values
  (let [[revised launched] (staging/notes {:schema {:level {:type "number" :bounds [0 1]}}
                                           :revised {:effect-version 2 :adjusted {:level [0.6 0.5]}}
                                           :last-launch {:status "cancelled" :reason "stage-revised"}})]
    (is (re-find #"^The effect was revised: level 0\.60* → 0\.50*\. Check it before launching\.$" revised))
    (is (= "The last launch cancelled: stage revised." launched))))
