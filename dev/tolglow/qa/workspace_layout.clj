(ns tolglow.qa.workspace-layout
  "Open real paired Git branches on the preview host for Studio layout checks."
  (:require [tolglow.development.workspace-test :as fixture]
            [tolglow.qa.studio-browser :as browser])
  (:import [java.nio.file Files]))

(defn -main [& [output]]
  (let [root (Files/createTempDirectory "tolglow-workspace-layout-" fixture/attrs)]
    (try
      (let [repos (into {} (for [id [:tolglow :afterglow]]
                             [id (str (fixture/repo! root (name id)))]))]
        (browser/run-browser! "dev/studio/workspace-layout-qa.mjs"
                              (or output "target/workspace-layout")
                              {:workspace-options {:repositories repos :workspace-parent (str (.resolve root "workspaces"))}}))
      (finally (fixture/delete-tree! root)))))
