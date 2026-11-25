(ns tolglow.composition.sequence-edit-test
  (:require [clojure.test :refer [deftest is]]
            [tolglow.composition.sequence-edit :as edit]))

(def ^:private empty-draft {:id :chase :mode :sequence :slots []})

(deftest insertion-selection-and-copies-use-stable-typed-step-ids
  (let [opened (edit/add-step empty-draft nil {:id :step :cue-id :red :params {:level 0.4}
                                               :duration {:bars 1/2} :launch {:mode :held}})
        copied (edit/copy-step opened :step)
        duplicated (edit/duplicate-step opened :step "copy")
        pasted (edit/paste-step duplicated nil :third copied)
        changed (-> pasted (edit/toggle-muted "copy") (edit/set-duration :third {:beats 1/4}))
        removed (edit/delete-step changed :step)]
    (is (empty? (:slots empty-draft)))
    (is (= [:step "copy" :third] (mapv :id (:slots pasted))))
    (is (= copied (dissoc (edit/select-step changed :step) :id)))
    (is (= copied (dissoc (edit/select-step pasted "copy") :id)))
    (is (= {:bars 1/2} (:duration (edit/select-step changed :copy))))
    (is (= {:beats 1/4} (:duration (edit/select-step changed :third))))
    (is (true? (:muted? (edit/select-step removed "copy"))))
    (is (= copied (dissoc (edit/select-step (edit/toggle-muted removed :copy) "copy") :id)))
    (is (= ["copy" :third] (mapv :id (:slots removed))))
    (is (thrown? clojure.lang.ExceptionInfo (edit/select-step removed :step)))
    (is (thrown? clojure.lang.ExceptionInfo (edit/toggle-muted removed :step)))
    (is (thrown? clojure.lang.ExceptionInfo (edit/paste-step removed :step :fourth copied)))
    (is (thrown? clojure.lang.ExceptionInfo (edit/duplicate-step removed "copy" :third)))
    ;; Studio's JSON boundary keywordizes textual IDs, so names identify steps.
    (is (= :step (:id (edit/select-step opened "step"))))
    (is (thrown? clojure.lang.ExceptionInfo (edit/duplicate-step opened :step "step")))
    (is (thrown? clojure.lang.ExceptionInfo (edit/add-step removed nil {:id :copy :cue-id :red})))))

(deftest empty-steps-use-the-existing-clock-domain
  (let [legacy (assoc empty-draft :slots [{:id :legacy :cue-id :red :duration-ms 400}])
        gap (edit/add-empty-step legacy :legacy :gap)
        changed (edit/set-duration gap :gap {:ms 250})]
    (is (= [{:id :gap :kind :empty :params {} :duration {:beats 1}}]
           (:slots (edit/add-empty-step empty-draft nil :gap))))
    (is (= {:id :gap :kind :empty :params {} :duration {:ms 1000}}
           (edit/select-step gap :gap)))
    (is (= {:ms 250} (:duration (edit/select-step changed :gap))))
    (is (= [] (:slots (edit/delete-step legacy :legacy))))
    (is (thrown? clojure.lang.ExceptionInfo (edit/set-duration legacy :legacy {:beats 1})))
    (is (thrown? clojure.lang.ExceptionInfo
                 (edit/add-step empty-draft nil {:id :bad :kind :empty :cue-id :red})))
    (is (thrown? clojure.lang.ExceptionInfo (edit/paste-step legacy :legacy :new nil)))))

(deftest extending-musical-length-retains-exact-fractions-and-uses-the-supplied-meter
  (let [original (edit/add-step empty-draft nil {:id :a :cue-id :red :duration {:bars 1/2}})
        meter {:beats-per-bar 7/2 :bars-per-phrase 3}
        same-unit (edit/extend-duration original :a {:bars 1/4} meter)
        combined (edit/extend-duration same-unit :a {:phrases 1/2} meter)
        legacy (assoc empty-draft :slots [{:id :a :cue-id :red :duration-beats 1/3}])]
    (is (= {:bars 1/2} (:duration (edit/select-step original :a))))
    (is (= {:bars 3/4} (:duration (edit/select-step same-unit :a))))
    (is (= {:beats 63/8} (:duration (edit/select-step combined :a))))
    (is (= {:id :a :cue-id :red :duration {:beats 1/2}}
           (edit/select-step (edit/extend-duration legacy :a {:beats 1/6} meter) :a)))
    (doseq [increment [{:beats 0} {:beats -1} {:beats ##Inf} {:ms 250} {:beats 1 :bars 1}]]
      (is (thrown? clojure.lang.ExceptionInfo (edit/extend-duration original :a increment meter))))))
