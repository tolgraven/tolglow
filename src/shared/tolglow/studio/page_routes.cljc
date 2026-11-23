(ns tolglow.studio.page-routes
  "Page URLs shared by the Ring and browser routers.")

(def routes
  [["/"             {:name :home :surface :studio}]
   ["/perform"      {:name :perform :surface :studio :view :perform
                    :nav {:label "Perform" :icon :layout-grid}}]
   ["/compose"      {:name :compose :surface :studio :view :compose
                    :nav {:label "Compose" :icon :list-music}}]
   ["/rig"          {:name :rig :surface :studio :view :rig
                    :nav {:label "Rig & space" :short-label "Rig" :icon :lamp-ceiling}}]
   ["/library"      {:name :library :surface :studio :view :library
                    :nav {:label "Definitions" :short-label "Defs" :icon :library-big}}]
   ["/assistant"    {:name :assistant :surface :studio :view :assistant
                    :nav {:label "Assistant" :icon :sparkles}}]
   ["/connections"  {:name :connections :surface :studio :view :connections
                    :nav {:label "Connections" :short-label "Connect" :icon :cable}}]
   ["/develop"      {:name :develop :surface :studio :view :develop
                    :nav {:label "Develop" :icon :square-terminal}}]
   ["/debug"        {:name :debug :surface :studio :view :debug
                    :nav {:label "Debug" :icon :activity}}]
   ["/repl"         {:name :repl :surface :repl}]
   ["/push-display" {:name :push-display :surface :push-display}]])

(def views (into #{} (keep (comp :view second)) routes))

(def navigation
  "Studio links in their display order. Standalone windows omit :nav."
  (->> routes
       (keep (fn [[_ {:keys [name nav]}]] (when nav (assoc nav :route name))))
       (map-indexed (fn [index link] (assoc link :shortcut (str (inc index)))))
       vec))

(def shortcut-routes (into {} (map (juxt :shortcut :route)) navigation))
