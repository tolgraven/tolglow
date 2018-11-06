(defproject tolglow "0.0.1-SNAPSHOT"
 :description "tolgraven + afterglow "
 :url "http://example.com/FIXME"
 :license {:name "Secret" :url "none"}
 :dependencies [[org.clojure/clojure "1.8.0" #_"1.9.0"]
                [quil "2.7.1"]
                [mvxcvi/puget "1.0.2"]
                [mvxcvi/whidbey "1.3.2"]
                ;; [overtone "0.10.3"] ;in afterglow tho
                [afterglow "0.2.4"]
                [org.clojure/tools.namespace "0.2.7"]
                [com.bhauman/rebel-readline "0.1.4"]]
                ;; [afterglow "0.2.5-SNAPSHOT"]]
;;  :main #_^:skip-aot tolglow.core #_nil
 :main tolglow.core
 #_:repl-options #_{:host "0.0.0.0" ; allow remote connections
                :port 5000 ; consistent port
                :init-ns tolglow.core}
 ;; :plugins [[lein-git-deps "0.0.1-SNAPSHOT"]]
 ;; :git-dependencies [["https://github.com/brunchboy/afterglow.git"]]
;;  :target-path "target/%s"
 :profiles
 {:dev {:plugins [#_[io.aviso/pretty "0.1.20"]]
        :dependencies [[org.clojure/tools.namespace "0.2.7"]
                       [org.clojure/tools.trace "0.7.10"]
                       #_[io.aviso/pretty "0.1.20"]]
        :repl-options {:init-ns tolglow.core
                       :host "0.0.0.0"
                       :port 5000 ; consistent port
                       :welcome (println "tolglow loaded.")}
        :jvm-opts ["-XX:-OmitStackTraceInFastThrow"
                   "-Dapple.awt.UIElement=true"
                   "-Dclojure.compiler.disable-locals-clearing=true"]
        #_:env #_{:dev "true"}}
  :uberjar {:aot :all
            :jvm-opts ["-Dclojure.compiler.direct-linking=true"]
            #_:env #_{:production "true"}}})
