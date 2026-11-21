(ns tolglow.studio.page-routes
  "Page URLs shared by the Ring and browser routers.")

(def routes
  [["/"             {:name :home :surface :studio}]
   ["/perform"      {:name :perform :surface :studio :view :perform}]
   ["/compose"      {:name :compose :surface :studio :view :compose}]
   ["/rig"          {:name :rig :surface :studio :view :rig}]
   ["/library"      {:name :library :surface :studio :view :library}]
   ["/assistant"    {:name :assistant :surface :studio :view :assistant}]
   ["/connections"  {:name :connections :surface :studio :view :connections}]
   ["/develop"      {:name :develop :surface :studio :view :develop}]
   ["/debug"        {:name :debug :surface :studio :view :debug}]
   ["/repl"         {:name :repl :surface :repl}]
   ["/push-display" {:name :push-display :surface :push-display}]])

(def views (into #{} (keep (comp :view second)) routes))
