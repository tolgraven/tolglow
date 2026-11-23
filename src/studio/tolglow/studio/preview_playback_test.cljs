(ns tolglow.studio.preview-playback-test
  (:require [cljs.test :refer-macros [deftest is]]
            [tolglow.studio.state-socket :as socket]))

(deftest hidden-studio-keeps-only-the-actual-picture-in-picture-scene-demand
  (let [scene [:spatial/scene] audio [:audio/analysis]
        demand #{scene audio}]
    (is (= demand (socket/visible-queries demand true false)))
    (is (= #{scene} (socket/visible-queries demand false true)))
    (is (empty? (socket/visible-queries #{audio} false true)))
    (is (empty? (socket/visible-queries demand false false)))))
