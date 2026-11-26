(ns tolglow.studio.knobs-test
  (:require [cljs.test :refer-macros [deftest is testing]]
            [re-frame.db :as rf-db]
            [re-frame.interceptor :as interceptor]
            [re-frame.registrar :as registrar]
            [tolglow.studio.knobs :as knobs]))

(def show
  {:runtime-id "r" :show-id :show :epoch 3 :revision 9
   :parameter-waveforms [{:id :pulse :params {:duty {:type :number :default 0.5 :bounds [0 1]}}}]
   :instances [{:id "3:1" :cue-id :tint :order 0 :phase :running
                :definition {:params {:saturation {:type :number :default 0 :bounds [0 1]}
                                      :hue {:type :color :default [1 0 0]}}}}
               {:id "3:2" :cue-id :pulse :order 1 :phase :running
                :definition {:params {:level {:type :number :default 0 :bounds [0 1]}}}
                :parameter-sources {:level {:sources [{:type :lfo :binding {:waveform :pulse :from 0 :to 0.5}}]}}}]
   :knobs {:knobs [{:id :look :name "Look" :value 0.5
                    :links [{:id "rate" :label "pulse · level LFO period" :status :ok
                             :destination {:kind :lfo :cue-id :pulse :param :level :field :period}
                             :range [2 0.5] :direction :up :curve :linear :input [0 1] :quantize {:ratios :preferred}
                             :resolved {:value 1 :text "1 beats" :ratio [1 1]}}]}]}})

(deftest destination-choices-list-numeric-parameters-and-waveform-fields
  (let [choices (knobs/destination-choices show)]
    (is (= ["tint · saturation" "pulse · level" "pulse · level LFO low" "pulse · level LFO gain (high)"
            "pulse · level LFO period" "pulse · level LFO duty"]
           (mapv :label choices)))
    (is (= {:kind :lfo :cue-id :pulse :param :level :input :duty} (:destination (last choices))))
    (is (= (count choices) (count (distinct (map :key choices)))))))

(deftest link-payload-builds-the-shared-link-or-explains-the-problem
  (let [choice (nth (knobs/destination-choices show) 4)
        form (knobs/default-form choice)]
    (is (= {:from "2" :to "0.5" :quantize "preferred"} (select-keys form [:from :to :quantize])))
    (is (= {:link {:id "link-2" :destination {:kind :lfo :cue-id :pulse :param :level :field :period}
                   :range [2 0.5] :direction :up :curve :linear :input [0 1] :quantize {:ratios :preferred}}}
           (knobs/link-payload {:choice choice :form form :link-id "link-2"})))
    (is (= 1.5 (get-in (knobs/link-payload {:choice choice :form (assoc form :from "3/2") :link-id "x"})
                       [:link :range 0]))
        "a typed fraction is accepted")
    (is (= {:error "Enter numbers for From and To, such as 0.25 or 1/2."}
           (knobs/link-payload {:choice choice :form (assoc form :to "fast") :link-id "x"})))
    (is (= {:error "Name the show variable."} (knobs/link-payload {:choice "variable" :form form :link-id "x"})))
    (testing "editing a projected link sends only link fields"
      (is (= {:id "rate" :destination {:kind :lfo :cue-id :pulse :param :level :field :period}
              :range [2 0.5] :direction :down :curve :linear :input [0 1] :quantize {:ratios :preferred}}
             (knobs/edited-link (get-in show [:knobs :knobs 0 :links 0]) :direction :down))))))

(defn- transition [db event]
  (reset! rf-db/app-db db)
  (:effects (interceptor/execute event (remove #(= :do-fx (:id %)) (registrar/get-handler :event (first event))))))

(deftest turning-the-knob-sends-the-shared-command
  (let [effects (transition {:show show :session "s" :connection {:connected? true}}
                            [::knobs/set-value :look 0.75])
        [[kind request]] (:fx effects)]
    (is (= :studio/send-command kind))
    (is (= {:op :knob/set :fields {:knob-id :look :value 0.75} :epoch 3 :runtime-id "r"}
           (select-keys request [:op :fields :epoch :runtime-id])))
    (is (= 0.75 (get-in effects [:db :ui :knobs :drafts "look" :value])))
    (is (nil? (:fx (transition {:show show} [::knobs/set-value :look 1.5]))) "out-of-range values are not sent")))

(deftest slider-turns-stay-in-order-and-the-draft-waits-for-its-revision
  (let [db0 {:show show :session "s" :connection {:connected? true}}
        knob (get-in show [:knobs :knobs 0])
        first-turn (transition db0 [::knobs/set-value :look 0.6])
        sent-value #(get-in % [:fx 0 1 :fields :value])
        while-in-flight (transition (:db first-turn) [::knobs/set-value :look 0.7])
        latest (transition (:db while-in-flight) [::knobs/set-value :look 0.8])]
    (is (= 0.6 (sent-value first-turn)))
    (is (nil? (:fx while-in-flight)) "a turn waits while the previous one is in flight")
    (is (nil? (:fx latest)))
    (is (= 0.8 (knobs/draft-value (:db latest) knob)) "the slider shows the newest turn")
    (testing "the first receipt sends only the newest turn"
      (let [acked (transition (:db latest) [::knobs/value-acked :look 0.6 {:status :accepted :revision 10}])]
        (is (= 0.8 (sent-value acked)))
        (is (= 0.8 (knobs/draft-value (:db acked) knob)) "an older receipt never snaps the slider back")
        (let [done (transition (:db acked) [::knobs/value-acked :look 0.8 {:status :accepted :revision 12}])]
          (is (nil? (:fx done)))
          (is (= 0.8 (knobs/draft-value (:db done) knob)) "kept until the snapshot reaches revision 12")
          (is (= 0.8 (knobs/draft-value (assoc-in (:db done) [:show :revision] 11) knob)))
          (is (nil? (knobs/draft-value (assoc-in (:db done) [:show :revision] 12) knob))
              "then the projection, which may hold a REPL or Push value, wins")
          (is (nil? (knobs/draft-value (assoc-in (:db done) [:show :epoch] 4) knob)) "a new epoch drops the draft"))))
    (testing "a refusal is shown and a newer turn is still sent"
      (let [failed (transition (:db latest) [::knobs/value-failed :look 0.6 "Unknown knob"])]
        (is (= 0.8 (sent-value failed)))
        (is (= "Unknown knob" (get-in failed [:db :ui :knobs :error])))))
    (testing "a refusal of the newest turn drops its draft"
      (let [failed (transition (:db first-turn) [::knobs/value-failed :look 0.6 "Unknown knob"])]
        (is (nil? (:fx failed)))
        (is (nil? (knobs/draft-value (:db failed) knob)))
        (is (= "Unknown knob" (get-in failed [:db :ui :knobs :error])))))))
