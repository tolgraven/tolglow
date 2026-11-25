(ns tolglow.studio.operator-history-test
  (:require [cljs.test :refer [deftest is testing]]
            [tolglow.studio.operator-history :as history]))

(def projection
  {:identity ["runtime-1" "show-1" 3]
   :current-id 21
   :nodes [{:id 13 :parent-id 12 :sequence 3 :actor "Studio" :parameter :gain :before 0.25 :after 0.5
            :cue-id :wash :cue-label "Wash" :effect-id :effect-one :effect-label "Dimmer"}
           {:id 0 :parent-id nil :sequence 0}
           {:id 21 :parent-id 12 :sequence 4 :actor "Push" :parameter :gain :before 0.25 :after 0.8
            :cue-id :beam :cue-label "Beam" :effect-id :effect-two :effect-label "Dimmer"}
           {:id 12 :parent-id 0 :sequence 2 :actor "Push" :parameter :gain :before 0 :after 0.25
            :cue-id :wash :cue-label "Wash" :effect-id :effect-one :effect-label "Dimmer"}]})

(defn- lanes [outline]
  (into {} (map (juxt :id :lane)) (:rows outline)))

(deftest retains-rooted-order-and-stable-branch-lanes
  (let [model (history/outline projection nil)
        extended (update projection :nodes conj {:id 42 :parent-id 13 :sequence 5 :parameter :gain})]
    (is (= [0 12 13 21] (:ids model)))
    (is (= #{0 12 21} (:applied model)))
    (is (= 1 (:fork-width (second (:rows model)))))
    (is (= {0 0, 12 0, 13 0, 21 1} (lanes model)))
    (is (= [0 12 13 42 21] (:ids (history/outline extended nil))))
    (is (= (lanes model) (dissoc (lanes (history/outline extended nil)) 42)))
    (is (= (lanes model) (lanes (history/outline (assoc projection :current-id 13) nil))))))

(deftest filtering-preserves-ancestors-and-separates-identical-effect-labels
  (let [cue (history/outline projection [:cue :beam])
        effect (history/outline projection [:effect :effect-one])]
    (is (= [0 12 21] (:ids cue)))
    (is (= #{21} (:matches cue)))
    (is (= {0 0, 12 0, 21 1} (lanes cue)))
    (is (= [0 12 13] (:ids effect)))
    (is (= #{12 13} (:matches effect)))
    (is (= #{[:effect :effect-one] [:effect :effect-two]}
           (into #{} (keep #(when (= "Dimmer" (:label %)) (:key %))) (:filters cue))))
    (is (= [0 12 13 21] (:ids (history/outline projection [:cue :gone]))))))

(deftest keyboard-navigation-moves-focus-without-selecting-a-preview
  (let [model (history/outline projection nil)
        update #(history/update-interaction %1 projection %2)
        focus (update nil [:focus (history/navigation-target model 21 "ArrowUp")])]
    (is (= 13 (:focus-id focus)))
    (is (nil? (:preview focus)))
    (is (= 12 (history/navigation-target model 21 "ArrowLeft")))
    (is (= 13 (history/navigation-target model 12 "ArrowRight")))
    (is (= 0 (history/navigation-target model 21 "Home")))
    (is (= 21 (history/navigation-target model 0 "End")))
    (is (nil? (history/navigation-target model 0 "ArrowUp")))
    (is (nil? (history/navigation-target model 21 "Enter")))
    (is (nil? (history/navigation-target model 21 " ")))))

(deftest explicit-apply-requires-a-retained-preview-and-the-captured-cursor
  (let [state (history/update-interaction nil projection [:preview 13])
        by-id (:by-id (history/outline projection nil))
        request #(history/apply-request %1 %2 %3 by-id)
        cursor-moved (assoc projection :current-id 12)]
    (is (= {:target-id 13 :expected-cursor 21} (request projection state nil)))
    (testing "A move elsewhere disables Apply until explicitly re-previewed"
      (is (nil? (request cursor-moved state nil)))
      (is (= {:target-id 13 :expected-cursor 12}
             (request cursor-moved (history/update-interaction state cursor-moved [:preview 13]) nil))))
    (testing "Requests are blocked for conflicts, pending commands and unavailable steps"
      (is (nil? (request projection state {:conflict "Automation wrote gain."})))
      (is (nil? (request projection state {:pending? true})))
      (is (nil? (history/apply-request projection state nil (dissoc by-id 13))))
      (is (nil? (history/apply-request projection state nil (dissoc by-id 21))))
      (is (nil? (history/apply-request projection state nil
                                      (assoc-in by-id [13 :unavailable-reason] "The effect ended.")))))
    (testing "Viewing the current step is a no-op and step zero can be applied"
      (is (nil? (request projection (history/update-interaction state projection [:preview 21]) nil)))
      (is (= {:target-id 0 :expected-cursor 21}
             (request projection (history/update-interaction state projection [:preview 0]) nil))))
    (testing "Changing the filter, clearing preview and runtime replacement discard the old selection"
      (is (nil? (request projection (history/update-interaction state projection [:filter [:cue :beam]]) nil)))
      (is (nil? (request projection (history/update-interaction state projection [:clear-preview]) nil)))
      (is (nil? (request (assoc projection :identity ["runtime-2" "show-1" 3]) state nil))))))

(deftest redo-at-a-fork-requires-a-fresh-eligible-child-preview
  (let [fork (assoc projection :current-id 12)
        at-start (assoc projection :current-id 0)
        select #(history/update-interaction nil fork [:preview %])
        request (fn [target] {:identity (:identity fork) :expected-cursor 12 :target-id target})]
    (is (= (request 0) (:history/undo (history/commands fork nil nil))))
    (is (= 12 (get-in (history/commands at-start nil nil) [:history/redo :target-id])))
    (is (:choose-redo? (history/commands fork nil nil)))
    (is (nil? (:history/redo (history/commands fork (select 0) nil))))
    (is (= (request 13) (:history/redo (history/commands fork (select 13) nil))))
    (is (= (request 21) (:history/redo (history/commands fork (select 21) nil))))
    (is (nil? (:history/redo (history/commands at-start (select 21) {:pending? true}))))
    (is (nil? (:history/select (history/commands fork (select 13) {:connection-message "Reconnecting"}))))))
