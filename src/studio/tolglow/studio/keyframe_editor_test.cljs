(ns tolglow.studio.keyframe-editor-test
  (:require [cljs.test :refer-macros [deftest is]]
            [tolglow.studio.events :as events]
            [tolglow.studio.effect-inspector :as inspect]
            [tolglow.studio.keyframe-editor :as editor]))

(deftest musical-durations-and-handle-edits
  (is (= 0.125 (editor/ratio-value "1/8")))
  (is (nil? (editor/ratio-value "1/0")))
  (is (= [0.0 1.5 0.75 0] (editor/move-handle [0.1 0.2 0.75 0] 0 -19 -30 [-0.5 1.5]))))

(deftest visual-builder-and-running-inspector-preserve-authored-timing
  (let [spec {:interval {:beats 0.25} :curve {:type :spring :frequency 2 :damping 0.35}
              :progress {:mode :offset :param :level}}
        definition (events/builder-definition (assoc (get-in (events/initial-db "test") [:ui :builder]) :keyframes spec) 1)
        token {:runtime-id "one" :epoch 4}
        instance {:id "4:1" :definition definition}
        db {:show token :server-projection {:view {:instances [instance]}}}
        request #(get-in (inspect/keyframe-request db ["4:1" % :keyframes] token %2) [:studio/http :body])]
    (is (= spec (:keyframes definition)))
    (is (= (dissoc definition :keyframes) (:definition (request 1 nil))))
    (is (thrown? js/Error (request 2 spec)))))
