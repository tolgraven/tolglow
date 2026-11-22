(ns tolglow.performance-controls-test
  (:require [afterglow.control.model :as control]
            [clojure.test :refer [deftest is]]
            [tolglow.persistence :as save]
            [tolglow.show :as show]))

(defn- fixture [id address watts]
  {:id id :universe 1 :address address :groups [:stage] :channels {:dimmer 1}
   :max-watts watts :measured-relative-brightness (/ watts 100.0)})

(defn- model [rig looks]
  (let [effects (into {} (for [[id ids] looks]
                           [id {:id id :version 1 :name (name id) :targets ids :params {} :outputs {:dimmer 1}}]))
        cues (into {} (for [[id _] looks] [id {:id id :effect-id id :version 1}]))]
    (assoc (control/new-show {:registry {:effects effects :cues cues}}) :rig {:fixtures rig})))

(defn- issue! [rt clock type cmd]
  (let [ack (show/submit! rt type cmd)] (show/tick! rt (swap! clock + 25)) @ack))

(deftest unlike-fixtures-measure-on-the-same-rig-reference
  (let [rt (show/create-runtime {:model (model [(fixture :wash 1 200) (fixture :spot 2 100)] {})})
        clock (atom 0)]
    (try
      (is (= :accepted (:status (issue! rt clock :lighting-scene
                                 {:group-id :stage :op :apply :target 0.30 :effect-ids [] :brightness-reference :global}))))
      (dotimes [_ 20] (show/tick! rt (swap! clock + 25)))
      (let [{:keys [wash spot]} (show/source-brightness rt [:wash :spot])]
        (is (< (abs (- (:local-brightness wash) 0.30)) 0.015))
        (is (< (abs (- (:local-brightness spot) 0.60)) 0.015))
        (is (< (abs (- (:global-brightness wash) (:global-brightness spot))) 0.01))
        (is (= 0.5 (:global-maximum spot))))
      (finally (show/stop! rt)))))

(deftest independent-launches-share-a-physical-output-budget
  (let [rig (mapv fixture [:a :b :c :d :e :f :g :h :i] (range 1 10) [100 40 20 20 20 20 20 20 20])
        looks {:wide [:a :b] :side [:b :c] :fill [:a :d]}
        output (atom nil)
        rt (show/create-runtime {:model (model rig looks)
                                 :sink {:send! (fn [packet _] (reset! output (mapv #(bit-and 255 %) (take 9 (get packet 1)))))}})
        clock (atom 0)
        issue #(issue! rt clock %1 %2)]
    (try
      (is (= :accepted (:status (issue :settings {:scope :lighting-budget :values {:enabled? true
                         :global {:max-active 2 :limit 0.30 :standby-level 0.04 :strategy :priority :fixture-order [:b :a]}}}))))
      (doseq [look (keys looks)] (is (= :accepted (:status (issue :command {:op :cue/launch :cue-id look})))))
      (let [report (get-in (show/variables rt) [:lighting/budget :global])
            watts (/ (reduce + (map * @output (map :max-watts rig))) 255.0)]
        (is (= 2 (:active-count report)))
        (is (= 2 (:shed-count report)))
        (is (every? pos? (take 4 @output)))
        (is (every? zero? (drop 4 @output)))
        (is (<= watts (:limit-watts report)))
        (is (< (abs (- watts (:used-watts report))) 1e-6)))
      (is (= :accepted (:status (issue :command {:op :blackout/set :value true}))))
      (is (every? zero? @output))
      (finally (show/stop! rt)))))

(deftest independent-group-patches-survive-transit-and-rejected-edits
  (let [rt (show/create-runtime {:model (model [(fixture :a 1 100)] {})})
        clock (atom 0)
        edit #(issue! rt clock :settings {:scope :output-response :values %})]
    (try
      (is (= :accepted (:status (edit {:enabled? true :global {:attack {:beats 1/2}} :groups {:stage {:attack {:beats 1}}}}))))
      (is (= :accepted (:status (edit {:groups {:stage {:release {:bars 1}} :other {:attack {:ms 10}}}}))))
      (is (= :rejected (:status (edit {:groups {:stage {:release {:ms -10}}}}))))
      (let [settings (get-in (:model (save/read-string (show/export-show-source rt))) [:settings :output-response])]
        (is (= {:attack {:beats 1} :release {:bars 1}} (get-in settings [:groups :stage])))
        (is (= {:attack {:ms 10}} (get-in settings [:groups :other])))
        (is (= {:beats 1/2} (get-in settings [:global :attack]))))
      (is (= :accepted (:status (edit {:groups {:other nil}}))))
      (is (= #{:stage} (set (keys (get-in (show/snapshot rt) [:settings :output-response :groups])))))
      (finally (show/stop! rt)))))
