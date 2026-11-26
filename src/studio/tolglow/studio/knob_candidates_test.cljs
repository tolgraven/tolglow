(ns tolglow.studio.knob-candidates-test
  (:require [cljs.test :refer-macros [deftest is testing]]
            [re-frame.db :as rf-db]
            [re-frame.interceptor :as interceptor]
            [re-frame.registrar :as registrar]
            [tolglow.studio.knob-candidates :as candidates]))

(def show
  {:runtime-id "r" :show-id :show :epoch 3 :revision 9
   :instances [{:id "3:1" :cue-id :strobe :order 1 :phase :running}
               {:id "3:0" :cue-id :wash-tide :order 0 :phase :running}
               {:id "3:2" :cue-id :strobe :order 2 :phase :running}
               {:id "3:3" :cue-id :haze :order 3 :phase :releasing}]
   :knob-candidates
   {:status :ready :characters [{:id :energy :name "Energy"}]
    :candidate {:id "candidate-1" :name "Generated Energy" :seed 7 :character :energy :character-name "Energy"
                :score {:total 0.911} :value 0.5
                :links [{:id "strobe-level" :label "strobe · level" :status :ok :role :intensity
                         :range [0.4 1.0] :direction :up :curve :ease-in :input [0.5 1.0]
                         :resolved {:text "0.400"}}
                        {:id "strobe-duty" :label "strobe · duty" :status :ok :role :duty
                         :range [0.1 0.6] :direction :down :curve :linear :input [0.5 1.0]
                         :resolved {:text "0.600"}}
                        {:id "wash-period" :label "wash-tide · level LFO period" :status :ok :role :period
                         :range [0.25 4] :direction :down :curve :linear :input [0 1] :quantize {:ratios :preferred}
                         :resolved {:text "1 beats"}}]
                :explain [{:link-id "strobe-level" :why "Intensity 0.4 → 1.0" :correlation "brighter but shorter pulses"}]}}})

(deftest running-cues-are-listed-once-in-launch-order
  (is (= ["wash-tide" "strobe"] (candidates/running-cues show))))

(deftest generate-fields-send-only-what-the-operator-chose
  (is (= {:fields {}} (candidates/generate-fields {})))
  (is (= {:fields {:seed 42 :character "energy" :cue-ids ["strobe" "wash-tide"]}}
         (candidates/generate-fields {:seed " 42 " :character "energy" :cue-ids #{"wash-tide" "strobe"}})))
  (is (= {:error "The seed is a whole number."} (candidates/generate-fields {:seed "4.5"}))))

(deftest ranges-read-in-sweep-order-with-musical-fractions
  (let [[level duty period] (get-in show [:knob-candidates :candidate :links])]
    (is (= "0.4 → 1" (candidates/range-text level)))
    (is (= "0.6 → 0.1" (candidates/range-text duty)) "a falling link reads from the knob's low end")
    (is (= "4 → 1/4" (candidates/range-text period)))))

(defn- transition [db event]
  (reset! rf-db/app-db db)
  (:effects (interceptor/execute event (remove #(= :do-fx (:id %)) (registrar/get-handler :event (first event))))))

(deftest buttons-send-the-shared-candidate-commands
  (let [db {:show show :session "s" :connection {:connected? true}}
        sent (fn [effects] (select-keys (get-in effects [:fx 0 1]) [:op :fields]))]
    (is (= {:op :knob-candidate/generate :fields {:seed 5 :character "energy"}}
           (sent (transition (assoc-in db [:ui :knob-candidates] {:seed "5" :character "energy"})
                             [::candidates/generate]))))
    (is (= {:op :knob-candidate/reroll :fields {}} (sent (transition db [::candidates/reroll]))))
    (is (= {:op :knob-candidate/unlink :fields {:link-id "strobe-duty"}}
           (sent (transition db [::candidates/unlink "strobe-duty"]))))
    (is (= {:op :knob-candidate/keep :fields {:value 0.8 :name "Lift"}}
           (sent (transition (assoc-in db [:ui :knob-candidates :name] " Lift ")
                             [::candidates/keep (get-in show [:knob-candidates :candidate]) 0.8]))))
    (is (= {:op :knob-candidate/generate :fields {:from-knob "generated-1"}}
           (sent (transition db [::candidates/repeat "generated-1"]))))
    (is (= "The seed is a whole number."
           (get-in (transition (assoc-in db [:ui :knob-candidates :seed] "x") [::candidates/generate])
                   [:db :ui :knob-candidates :error])))))

(deftest sweeping-keeps-one-audition-in-flight-and-ends-on-the-last-position
  (let [db0 {:show show :session "s" :connection {:connected? true}}
        projection (:knob-candidates show)
        sent-value #(get-in % [:fx 0 1 :fields :value])
        first-move (transition db0 [::candidates/audition 0.3])
        waiting (transition (:db first-move) [::candidates/audition 0.6])
        latest (transition (:db waiting) [::candidates/audition 0.9])]
    (is (= :knob-candidate/audition (get-in first-move [:fx 0 1 :op])))
    (is (= 0.3 (sent-value first-move)))
    (is (nil? (:fx waiting)))
    (is (nil? (:fx latest)))
    (is (= 0.9 (candidates/sweep-value (:db latest) projection)))
    (let [acked (transition (:db latest) [::candidates/audition-acked 0.3 {:status :accepted :draft-revision 4}])]
      (is (= 0.9 (sent-value acked)) "the receipt sends only the newest position")
      (let [done (transition (:db acked) [::candidates/audition-acked 0.9 {:status :accepted :draft-revision 5}])]
        (is (nil? (:fx done)))
        (is (= 0.9 (candidates/sweep-value (:db done) (assoc projection :audition {:value 0.3 :draft-revision 4}))))
        (is (= 0.2 (candidates/sweep-value (:db done) (assoc projection :audition {:value 0.2 :draft-revision 6})))
            "a later REPL or agent audition moves the slider")))
    (is (nil? (:fx (transition db0 [::candidates/audition 2]))) "out-of-range positions are not sent")))
