(defproject tolglow "0.0.1-SNAPSHOT"
 :description "tolgraven + afterglow "
 :url "http://example.com/FIXME"
 :license {:name "Secret" :url "none"}
 :dependencies
 [[org.clojure/clojure "1.9.0"]
  [afterglow "0.2.5-SNAPSHOT"] ;like when in checkouts + has been lein install'd which does it go with ;;[overtone "0.10.3"] ;in afterglow tho
  [quil "3.0.0"]
  [incanter "1.9.3"]
  [walmartlabs/datascope "0.1.1"]
  [thi.ng/color "1.3.2-SNAPSHOT"] ;XXX this got overridden by dep in afterglow still on 1.3.0, even tho SNAPSHOT was in my classpath, show source took me to my version etc - that's fucked up? you cant depend on something newer than what you depend on?
  [mvxcvi/puget "1.0.2"]
  ;; [mvxcvi/whidbey "1.3.2"]
  [ring-middleware-format "0.7.2" :exclusions
   [ring/ring-jetty-adapter cheshire org.clojure/tools.reader
    org.clojure/java.classpath org.clojure/core.memoize
    com.fasterxml.jackson.core/jackson-core]]
  [com.bhauman/rebel-readline "0.1.4"] ;should only be used in prod/standalone right
  #_[afterglow-max "something"]]

 :main tolglow.core
 :target-path "target/%s/" ;dont have uberjar compiled files fucking up regular target classpath...
 :jvm-opts ["-Dapple.awt.UIElement=true"] ;background app / no dock icon
 :repl-options {:init-ns tolglow.core
                :host "0.0.0.0" ;allows remote connections
                :port 5000} ; consistent port
;;  :target-path "target/%s"
 :profiles
 {:dev {:plugins [#_[io.aviso/pretty "0.1.20"]]
        :dependencies [[org.clojure/tools.namespace "0.2.7"]
                       [org.clojure/tools.trace "0.7.10"]
                       ;; [org.clojure/test.check "0.9.0"]
                       #_[io.aviso/pretty "0.1.37"]]
        :main ^:skip-aot tolglow.core ;unfuck reloads
        :jvm-opts ["-XX:-OmitStackTraceInFastThrow" ;;disable pre-allocated exceptions optimization
                   ;; "-Dclojure.compiler.disable-locals-clearing=true" ;;worse perf but easier debug
                   ;; "-Xloggc:/tmp/jvm-gc.log", "-XX:+PrintGCDetails", "-XX:+PrintGCTimeStamps"
                   "-XX:+UseG1GC", "-XX:MaxGCPauseMillis=30" ;not guaranteed, only with G1 gc
                   "-Xmx4g"
                   ;; "-XX:+UseConcMarkSweepGC"
                   "-Dcom.sun.management.jmxremote"
                   "-Dcom.sun.management.jmxremote.ssl=false"
                   "-Dcom.sun.management.jmxremote.authenticate=false"
                   "-Dcom.sun.management.jmxremote.port=10001"] ;not sure if this works and how low it can go?
        :repl-options {:welcome (println "tolglow development loaded.")}
        #_:env #_{:dev "true"}}
  :uberjar {:aot :all ;hmm might still be good to have a more hackable uberjar tho, no aot/release stuff...
            :jvm-opts ["-Dclojure.compiler.direct-linking=true"] ;this helps w perf tho.
           :repl-options {:welcome (println "tolglow JAR loaded.")}
            #_:env #_{:production "true"}}
  :uberjar-dev {:repl-options {:welcome (println "tolglow dev-JAR loaded.")}}})
