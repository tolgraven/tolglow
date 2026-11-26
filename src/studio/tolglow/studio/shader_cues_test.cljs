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

(deftest a-saved-edit-is-the-next-version-with-only-the-source-changed
  (let [effect (assoc (examples/scene) :categories #{:emission})
        next (shader-cues/edited-definition effect "void main() { fragColor = vec4(1.0); }")]
    (is (= 2 (:version next)))
    (is (= "void main() { fragColor = vec4(1.0); }" (get-in next [:shader :glsl])))
    (is (= (dissoc (:shader effect) :glsl) (dissoc (:shader next) :glsl)))
    (is (not (contains? next :categories)) "Projection-only keys are not published")))

(deftest projector-and-preview-urls-carry-the-keyword-feed
  (is (= "/studio/video-output.html?source=%3Ashader%2Fshader-plasma" (shader-cues/output-url :shader/shader-plasma)))
  (is (= "/api/video/frame?source=%3Ashader%2Fx&width=320&v=7" (shader-cues/frame-url :shader/x 320 7))))
