(defproject tolgraven/linux-audience "0.1.0-SNAPSHOT"
  :description "Headless Bluetooth LE dance-floor sensor gateway"
  :dependencies [[org.clojure/clojure "1.12.7"]
                 [org.clojure/data.json "2.5.2"]
                 [com.github.hypfvieh/bluez-dbus "0.3.5"]
                 [com.github.hypfvieh/dbus-java-core "5.2.2"]
                 [com.github.hypfvieh/dbus-java-transport-native-unixsocket "5.2.2"]
                 [com.google.zxing/core "3.5.4"]]
  :source-paths ["src" "../../src"]
  :repositories {"central" "https://repo.maven.apache.org/maven2/"}
  :profiles {:paired {:source-paths [~(str (or (System/getenv "AFTERGLOW_ROOT") "../../../afterglow") "/src")]
                      :test-paths ["paired-test"]}}
  :main tolglow.linux.audience.main
  :jvm-opts ["-Djava.awt.headless=true"])
