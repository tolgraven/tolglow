(ns tolglow.studio.shader-cues-test
  (:require [cljs.test :refer [deftest is]]
            [tolglow.studio.shader-cues :as shader-cues]
            [tolglow.video.shader-examples :as examples]))

(deftest feeds-follow-the-server-naming
  ;; Same rule as tolglow.video.shader/feed-id on the JVM.
  (is (= :shader/shader-plasma (shader-cues/feed-id (examples/plasma))))
  (is (= :shader/my.plasma (shader-cues/feed-id {:id :my/plasma :shader {}})))
  (is (= :wall (shader-cues/feed-id {:id :x :shader {:feed :wall}}))))

(deftest only-shader-effects-are-listed-in-id-order
  (is (= [:a :b] (mapv :id (shader-cues/shader-effects [{:id :b :shader {}} {:id :solid} {:id :a :shader {}}])))))

(deftest projector-and-preview-urls-carry-the-keyword-feed
  (is (= "/studio/video-output.html?source=%3Ashader%2Fshader-plasma" (shader-cues/output-url :shader/shader-plasma)))
  (is (= "/api/video/frame?source=%3Ashader%2Fx&width=320&v=7" (shader-cues/frame-url :shader/x 320 7))))

(deftest the-preview-token-advances-at-most-eight-times-a-second
  (is (= {:token 3 :at 1000} (shader-cues/next-shown {:token nil :at 0} 3 1000)))
  (is (= {:token 3 :at 1000} (shader-cues/next-shown {:token 3 :at 1000} 4 1100)) "too soon")
  (is (= {:token 4 :at 1200} (shader-cues/next-shown {:token 3 :at 1000} 4 1200)))
  (is (= {:token 3 :at 1000} (shader-cues/next-shown {:token 3 :at 1000} nil 5000)) "a stopped feed keeps its picture"))
