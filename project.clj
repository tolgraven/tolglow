(defproject afterglow-tol "0.1.0-SNAPSHOT"
  :description "tolgraven afterglow env"
  :url "http://example.com/FIXME"
  :license {:name "Secret"
            :url "none"}
  :dependencies [[org.clojure/clojure "1.8.0" #_"1.9.0"]
                 [afterglow "0.2.4"]
                 ;; [afterglow "0.2.5-SNAPSHOT"]
                 ]
  :repl-options {:host "0.0.0.0" :port 5000}  ;allow remote connections, consistent port
  ;; :plugins [[lein-git-deps "0.0.1-SNAPSHOT"]]
  ;; :git-dependencies [["https://github.com/brunchboy/afterglow.git"]]
  :main ^:skip-aot afterglow-tol.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
