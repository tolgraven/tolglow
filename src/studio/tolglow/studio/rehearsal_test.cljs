(ns tolglow.studio.rehearsal-test
  (:require [cljs.test :refer-macros [deftest is testing]]
            [clojure.string :as str]
            ["react-dom/server" :refer [renderToStaticMarkup]]
            [re-frame.db :as rf-db]
            [re-frame.interceptor :as interceptor]
            [re-frame.registrar :as registrar]
            [reagent.core :as r]
            [tolglow.studio.rehearsal :as rehearsal]))

(defn- run-event [db event]
  (reset! rf-db/app-db db)
  (:effects (interceptor/execute event (remove #(= :do-fx (:id %)) (registrar/get-handler :event (first event))))))

(deftest the-form-makes-the-session-choice
  (is (= {:mode :club :seed 670 :agent {:latency-beats 0}} (rehearsal/choice rehearsal/default-form)))
  (is (= {:mode :concert :take :b :seed 7 :agent {:latency-beats 1.5 :outage {:from-section 2 :mode :silent}}}
         (rehearsal/choice {:passage "concert-b" :seed "7" :latency-beats "1.5" :outage "silent" :outage-from "2"})))
  (is (thrown-with-msg? js/Error #"seed" (rehearsal/choice (assoc rehearsal/default-form :seed "-1"))))
  (is (thrown-with-msg? js/Error #"reply time" (rehearsal/choice (assoc rehearsal/default-form :latency-beats "99")))))

(deftest arm-asks-to-replace-the-show-and-carries-the-page-identity
  (let [db {:show {:epoch 2 :runtime-id "rt"} :connection {:connected? true}
            :ui {:rehearsal {:form {:passage "concert-a" :replace? true}}}}
        {:keys [studio/http]} (run-event db [::rehearsal/act :arm])]
    (is (= "/api/rehearsal/arm" (:path http)))
    (is (= {:runtime-id "rt" :expected-epoch 2 :mode :concert :take :a :seed 670 :agent {:latency-beats 0} :replace-show? true
            :discard-edits? false}
           (:body http)))
    (is (= {:runtime-id "rt" :expected-epoch 2} (:body (:studio/http (run-event db [::rehearsal/act :stop])))))
    (testing "the receipt becomes the status and the confirmation resets"
      (let [after (run-event db [::rehearsal/acted :arm {:status "armed" :loaded? true}])]
        (is (= "armed" (get-in after [:db :rehearsal :status :status])))
        (is (false? (get-in after [:db :ui :rehearsal :form :replace?])))))
    (testing "polling runs only while the panel is open"
      (is (nil? (:studio/http (run-event db [::rehearsal/poll]))))
      (is (= "/api/rehearsal" (:path (:studio/http (run-event (assoc-in db [:ui :rehearsal :open?] true) [::rehearsal/poll]))))))))

(def status
  {:status "finished" :choice {:mode "club" :seed 670} :beat 193.2 :before-rehearsal {:path "/tmp/before.transit" :show-id "gig-2023"} :bars 48 :playing? false :loaded? true
   :moods [{:section "groove" :kind "groove" :bar 0 :scope ["wash-color" "sweep"] :interval {:beats 1} :launch "one-shot" :plan ["drive" "lift"]}
           {:section "quiet" :kind "quiet" :bar 40 :scope ["spot" "sweep"] :interval {:bars 1} :launch "timed" :max-lit 1 :intent "darkness"}]
   :run {:agent [{:section "groove" :suggested-beat 0 :accepted-beat 0 :status "completed" :wall-ms 3.2
                  :receipts {"configure_auto" {:status "accepted"}}}
                 {:section "breakdown" :suggested-beat 60 :accepted-beat 80.9 :status "cancelled" :timed-out? true :receipts {}}]}
   :review-status "ready" :review-choice {:mode "club" :seed 670}
   :review {:id "club" :flags ["The agent did not answer for breakdown and the host gave up; the previous mood played on."]
            :report "# Club rehearsal"
            :responses [{:section "groove" :position "bar 0" :outcome "applied" :applied-beat 0 :late-beats 0 :carry-over-launches 0 :early-launches 0}
                        {:section "breakdown" :position "bar 16" :outcome "timed-out" :carry-over-launches 0 :early-launches 0}]
            :phrases [{:phrase 0 :first-bar 0 :music ["groove"] :launches 30 :new-cues ["sweep"] :novelty nil :mean-lit 6.5}
                      {:phrase 1 :first-bar 4 :music ["groove"] :launches 31 :new-cues [] :novelty 0.12 :mean-lit 6}]
            :repetition {:threshold 0.2 :mean-novelty 0.12 :repeat-phrases [1] :longest-repeat-run {:length 1}}
            :launch-timing {:launches 61 :measured 59 :due-on-own-grid 59 :max-after-due 0.25}
            :boundaries {:count 5 :with-agent-change 1 :on-bar 4 :on-phrase 2}
            :continuity {:decisions 190 :longest-gap {:beats 1.03} :stalls [] :pending-auto-changes 0 :agent-runs-open-at-end 0}}})

(deftest the-panel-shows-the-run-and-its-review
  (is (= "Finished · club · seed 670 · bar 48 beat 1.20 of 48 (transport stopped)" (rehearsal/status-line status)))
  (is (= "Nothing armed" (rehearsal/status-line nil)))
  (is (= "Arming · club · seed 670" (rehearsal/status-line {:status "arming" :choice {:mode "club" :seed 670}})))
  (let [html (renderToStaticMarkup (r/as-element [rehearsal/panel {:rehearsal {:status status} :connection {:connected? true}
                                                                   :ui {:rehearsal {:open? true}}}]))]
    (is (str/includes? html "REHEARSE WITH THE AGENT"))
    (is (str/includes? html "What the agent will set"))
    (is (str/includes? html "spot, sweep · every 1 bars · timed · at most 1 lit · darkness"))
    (is (str/includes? html "agent timed out") "the review's outcome")
    (is (str/includes? html "timed out") "the run's reply")
    (is (str/includes? html "Mean novelty 0.12"))
    (is (str/includes? html "190 auto decisions; longest gap 1.03 beats; 0 stalls"))
    (is (str/includes? html "the previous mood played on"))
    (is (str/includes? html "seeded replay"))
    (is (not (str/includes? html "Replace the open show")) "this session's own show needs no confirmation")
    (is (str/includes? html "Restore gig-2023 from before the rehearsal") "the replaced show can come back")
    (is (re-find #"<button [^>]*disabled=\"\"[^>]*id=\"rehearsal-start\"" html) "Start waits for Arm")
    (is (not (re-find #"<button [^>]*disabled=\"\"[^>]*id=\"rehearsal-arm\"" html)) "a finished run can be armed again")))


(deftest a-failed-arm-and-unsaved-edits-show-in-the-panel
  (let [render (fn [status] (renderToStaticMarkup (r/as-element [rehearsal/panel {:rehearsal {:status status} :connection {:connected? true}
                                                                                :ui {:rehearsal {:open? true}}}])))
        failed (render {:status "failed" :failure "The show stopped while the rehearsal was arming" :choice {:mode "club" :seed 670}})
        edited (render {:status "stopped" :loaded? true :edited? true :choice {:mode "club" :seed 670}
                        :before-rehearsal {:path "/tmp/b.transit" :show-id "gig-2023"}})
        elsewhere (render {:status "stopped" :loaded? false :before-rehearsal {:path "/tmp/b.transit" :show-id "gig-2023"}})]
    (is (re-find #"id=\"rehearsal-error\"[^>]*>The show stopped while the rehearsal was arming" failed))
    (is (str/includes? failed "Failed · club · seed 670"))
    (is (str/includes? edited "Discard the edits made to the rehearsal show"))
    (is (str/includes? edited "Restore gig-2023 from before the rehearsal"))
    (is (not (str/includes? elsewhere "Restore gig-2023")) "no restore over a show that is not the rehearsal's")))
