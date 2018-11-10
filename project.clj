(defproject tolglow "0.0.1-SNAPSHOT"
 :description "tolgraven + afterglow "
 :url "http://example.com/FIXME"
 :license {:name "Secret" :url "none"}
 :dependencies
 [[org.clojure/clojure "1.8.0" #_"1.9.0"]
  [afterglow #_"0.2.5-SNAPSHOT" "0.2.4"] ;in checkouts, so keep out!! dep issues now there tho
  [quil "2.7.1"]
  [mvxcvi/puget "1.0.2"]
  [mvxcvi/whidbey "1.3.2"]
  ;; [overtone "0.10.3"] ;in afterglow tho
  [org.clojure/tools.namespace "0.2.7"]
  [ring-middleware-format "0.7.2"
   :exclusions [ring/ring-jetty-adapter cheshire org.clojure/tools.reader
                org.clojure/java.classpath org.clojure/core.memoize com.fasterxml.jackson.core/jackson-core]]
  [com.bhauman/rebel-readline "0.1.4"]]
 :main tolglow.core ;; #_^:skip-aot tolglow.core #_nil
 :jvm-opts ;put in ext map and reference from that?
 ["-Dapple.awt.UIElement=true"] ;background app / no dock icon
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
                   "-Dclojure.compiler.disable-locals-clearing=true"]
        #_:env #_{:dev "true"}}
  :uberjar {:aot :all
            :jvm-opts ["-XX:-OmitStackTraceInFastThrow"
                       "-Dclojure.compiler.direct-linking=true"]
            #_:env #_{:production "true"}}})
