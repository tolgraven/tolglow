(ns tolglow.studio.material-designer-test
  (:require [cljs.test :refer-macros [deftest is]]
            [tolglow.studio.material-designer :as designer]))

(deftest shared-material-ids-have-stable-typed-identities
  (is (= :venue/shimmer (designer/material-id "venue/shimmer")))
  (is (= :ripple (designer/material-id "ripple")))
  (doseq [text ["" ":ripple" "venue/" "a/b/c" "bad name" nil]]
    (is (nil? (designer/material-id text)))))

(deftest canvas-pixels-preserve-the-sampled-colour-channels
  (is (= [1 2 3 255 4 5 6 255] (designer/rgba [[1 2 3] [4 5 6]]))))
