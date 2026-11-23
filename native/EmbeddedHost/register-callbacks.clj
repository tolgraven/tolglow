(require '[clojure.data.json :as json]
         '[clojure.java.io :as io]
         '[ring.adapter.jetty :as jetty])

;; Jetty builds method handles from Ring's concrete listener. Reify class
;; suffixes are chosen during AOT compilation and differ between releases.
(let [listener ((ns-resolve 'ring.adapter.jetty 'websocket-listener) {})
      entry (fn [klass] {:name (.getName ^Class klass) :allPublicMethods true
                          :allDeclaredMethods true :allDeclaredConstructors true :allDeclaredFields true})
      classes [(class listener)
               com.github.fzakaria.slf4j.timbre.TimbreServiceProvider
               com.github.fzakaria.slf4j.timbre.TimbreLoggerFactory
               com.github.fzakaria.slf4j.timbre.TimbreLoggerAdapter]
      destination (io/file "callbacks/META-INF/native-image/tolglow/callbacks/reflect-config.json")]
  (io/make-parents destination)
  (spit destination (json/write-str (mapv entry classes)))
  (println "Registered Jetty listener" (.getName (class listener))))
