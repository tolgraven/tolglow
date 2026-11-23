(ns tolglow.fog-optics-test
  (:require [afterglow.render.fog-optics :as optics]
            [clojure.test :refer [deftest is]]))

(deftest preview-carries-the-current-afterglow-material
  (is (= (optics/browser-module) (slurp "js/preview/fog-optics.generated.js"))
      "Regenerate with lein run -m afterglow.render.fog-optics js/preview/fog-optics.generated.js"))
