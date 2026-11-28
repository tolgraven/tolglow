(ns tolglow.studio.cue-pad-preview-test
  (:require [cljs.test :refer-macros [deftest is testing]]
            [tolglow.studio.cue-pad-preview :as pad-preview]))

(def cues [{:id :ember :name "Ember"} {:id "merlot" :name "Merlot"}])

(deftest a-healthy-page-says-nothing
  (is (= [] (pad-preview/fault-lines {:value {:colors {:ember [1 0 0]}}} cues [])))
  (is (= [] (pad-preview/fault-lines nil cues nil)) "before the first result arrives")
  (is (nil? (pad-preview/fault-notice []))))

(deftest a-failed-query-or-refused-lease-is-said-instead-of-showing-colours
  (is (= ["Pad previews unavailable: the show did not sample this page."]
         (pad-preview/fault-lines {:error :unavailable} cues [])))
  (is (= ["Pad previews unavailable: Visible cue preview limit reached."]
         (pad-preview/fault-lines {:value {:colors {} :fault {:stage :lease :message "Visible cue preview limit reached"}}}
                                  cues []))))

(deftest sampling-that-is-behind-is-said-with-the-frame-shown
  ;; The server's :stale marker (#906), also in tolglow.studio/query-sampling.
  (is (= ["The pad preview for this page is still being sampled; showing frame 12."]
         (pad-preview/fault-lines {:value {:colors {:ember [1 0 0]}} :stale {:reason :late :frame-number 12}} cues [])))
  (is (= ["The pad preview for this page is waiting for a free sampler; nothing sampled yet."]
         (pad-preview/fault-lines {:error :unavailable :stale {:reason :busy}} cues []))
      "an unavailable result that is only behind does not say the show refused the page"))

(deftest faulted-cues-are-named-with-their-stage-and-message
  (testing "typed and text cue IDs both find their names"
    (is (= ["Ember cannot be compiled for its preview head: Invalid effect expression"
            "Merlot preview failed: Computed fixture output must be finite and within [0,1]"]
           (pad-preview/fault-lines {:value {:colors {}
                                             :faults {:ember {:stage :plan :message "Invalid effect expression"}
                                                      "merlot" {:stage "sample" :message "Computed fixture output must be finite and within [0,1]"}}}}
                                    cues []))))
  (testing "a long list names a few and counts the rest"
    (let [faults (into {} (for [i (range 7)] [(keyword (str "cue-" i)) {:stage :sample :message "x"}]))
          lines (pad-preview/fault-lines {:value {:colors {} :faults faults}} cues [])]
      (is (= 5 (count lines)))
      (is (= "3 more pad previews failed." (peek lines))))))

(deftest response-sample-lease-faults-match-the-repl-records
  (is (= ["Parameter sample lease keeps failing: reader failed"
          "Parameter sample lease failed: once"]
         (pad-preview/fault-lines {:value {:colors {}}} cues
                                  [{:message "reader failed" :repeated? true} {:message "once" :repeated? false}]))))

(deftest the-notice-is-a-polite-status-region
  (let [[tag attrs & children] (pad-preview/fault-notice ["one" "two"])]
    (is (= :div.cue-preview-faults tag))
    (is (= {:role "status" :aria-live "polite"} attrs))
    (is (= 2 (count (first children))))))
