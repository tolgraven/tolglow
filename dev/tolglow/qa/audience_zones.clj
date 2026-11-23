(ns tolglow.qa.audience-zones
  "Run the audience operator UI checks against an isolated preview show."
  (:require [tolglow.qa.studio-browser :as browser]))

(defn -main [& [output]]
  (browser/-main "audience-zones" output))
