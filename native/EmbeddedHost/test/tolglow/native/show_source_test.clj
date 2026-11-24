(ns tolglow.native.show-source-test
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is]]
            [tolglow.modules :as modules]
            [tolglow.fixed-host :as host]
            [tolglow.native.show-source :as source])
  (:import [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]))

(defn- with-show [f]
  (let [folder (.toFile (Files/createTempDirectory "tolglow-user-show" (make-array FileAttribute 0)))]
    (try (f folder) (finally (run! io/delete-file (reverse (file-seq folder)))))))

(deftest interprets-sibling-namespaces-as-ordinary-configured-modules
  (with-show
    (fn [folder]
      (let [lib (io/file folder "wash" "my_pool.cljc")
            source-file (io/file folder "show.clj")]
        (io/make-parents lib)
        (spit lib "(ns wash.my-pool) (defn weight [n] (* n 3))
                   (defn colors [_ opts] {:value (weight (:weight opts))})")
        (spit source-file "(ns show (:require [wash.my-pool :as fx]))
                           (def config {:init {:components [:colors]
                                               :modules [:unused] :search-in-namespaces '[wash.my-pool]}
                                        :unused false :colors {:weight (fx/weight 2)}})")
        (let [{:keys [config registry]} (source/read-show folder {})]
          (is (= 18 (get-in (modules/start! registry config) [:modules :colors])))
          (is (nil? (registry :unused))))
        (spit source-file "(ns show) (def different {})")
        (is (thrown-with-msg? Exception #"show/config" (source/read-show folder {})))
        (spit (io/file folder "tolglow.edn") "{:init {:components [:colors]}}")
        (let [core-setup (fn [_ _] {:value :precompiled})
              {:keys [config registry]} (source/read-show folder {:colors core-setup})]
          (is (= :precompiled (get-in (modules/start! registry config) [:modules :colors]))))))))

(deftest show-sources-stay-within-the-selected-folder
  (with-show
    (fn [folder]
      (with-show
        (fn [outside]
          (let [external (io/file outside "paid.clj")]
            (spit external "(ns paid) (def value 8)")
            (Files/createSymbolicLink (.toPath (io/file folder "paid.clj")) (.toPath external)
                                      (make-array FileAttribute 0))
            (spit (io/file folder "show.clj") "(ns show (:require [paid])) (def config {})")
            (is (thrown-with-msg? Exception #"Could not find namespace: paid"
                                  (source/read-show folder {})))))))))

(deftest credentials-and-state-belong-to-the-configured-show-folder
  (let [config (host/launch-config "/data" "/shows/red"
                                   {:show {:credential-path "auth.edn"}
                                    :show-state {:load-file "last.transit" :save-file "/saved/set.transit"}} identity)]
    (is (= "/shows/red/auth.edn" (get-in config [:show :credential-path])))
    (is (= {:load-file "/shows/red/last.transit" :save-file "/saved/set.transit"} (:show-state config)))
    (is (= "/data/credentials.edn" (get-in (host/launch-config "/data" "" {} identity) [:show :credential-path])))))
