(ns tolglow.studio.lighting-response-test
  (:require [cljs.test :refer-macros [deftest is]]
            [tolglow.studio.lighting-response :as response]))

(deftest arrival-and-frame-blend-bindings-survive-the-stage-editor
  (doseq [saved [{:ms 300 :mode :linear}
                 {:mode :linear :beats {:cue-id :ride :param :fall :default 0.5}}
                 {:blend 0}
                 {:blend {:variable [:user :persistence] :default 0.125}}]]
    (is (= saved (response/stage-duration (response/stage-form saved)))))
  (is (= {:blend 0.25} (response/stage-duration {:source :fixed :mode :blend :value "1/4"})))
  (is (thrown? js/Error (response/stage-duration {:source :fixed :mode :blend :value "1.1"}))))
