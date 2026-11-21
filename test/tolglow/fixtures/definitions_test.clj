(ns tolglow.fixtures.definitions-test
  (:require [clojure.test :refer [deftest is]] [clojure.data.json :as json]
            [tolglow.fixtures.definitions :as library]
            [tolglow.integrations.fixtures-test :as fixture-data]
            [tolglow.integrations.gdtf-test :as gdtf-data]))

(def manufacturers {"example" {"name" "Example Lighting" "fixtureCount" 2 "color" "#ffffff"}})
(def catalog {"key" "example" "name" "Example Lighting" "fixtures"
              [{"key" "head" "name" "RGBW Head" "categories" ["Moving Head"]}
               {"key" "wash" "name" "Wide Wash" "categories" ["Color Changer"]}]
              "redirects" [{"key" "old-head" "redirectTo" "example/head"}]})

(defn provider [calls]
  (fn [request]
    (swap! calls conj request)
    {:status 200 :body (json/write-str
                        (case (:path request)
                          "/api/v1/manufacturers" manufacturers
                          "/api/v1/manufacturers/example" catalog
                          "/api/v1/get-search-results" [{"key" "example/head" "name" "Example Lighting RGBW Head" "color" "#ffffff"}]
                          "/example/head.json" fixture-data/ofl
                          "/example/wash.json" (assoc fixture-data/ofl :name "Wide Wash")))}))

(deftest explicit-catalog-fetch-enables-deterministic-offline-search
  (let [calls (atom []) fetch (provider calls)
        initial (library/new-library)
        manufacturers (library/refresh-manufacturers initial fetch {:retrieved-at "2026-11-21"})
        refreshed (library/refresh-manufacturer (:library manufacturers) fetch "example")
        found (library/search (:library refreshed) {:query "lighting rgbw" :category "Moving Head"})]
    (is (= 2 (count @calls)))
    (is (= ["example/head"] (mapv :key (:results found))))
    (is (false? (get-in found [:results 0 :cached?])))
    (is (:offline? found))
    (is (= 1 (:total found)))
    (is (= ["example/head" "example/wash"] (mapv :key (:results (library/search (:library refreshed) {})))) )
    (is (= 2 (count @calls)))
    (is (empty? (:entries initial)))
    (is (empty? (:documents (:library refreshed))))
    (is (= "MIT" (get-in found [:results 0 :provenance :license :spdx])))
    (is (= "example/head" (get-in refreshed [:redirects 0 "redirectTo"])))
    (is (not (contains? (:library refreshed) :registry)))
    (is (not (contains? (:library refreshed) :rig)))))

(deftest fetch-cache-and-import-are-independent-of-show-state
  (let [calls (atom []) fetch (provider calls)
        initial (:library (library/refresh-manufacturers (library/new-library) fetch))
        result (library/fetch-fixture initial fetch "example/head" {:retrieved-at "today"})
        cached (library/fetch-fixture (:library result) (fn [_] (throw (ex-info "No network" {}))) "example/head")
        imported (library/import-fixture (:library result) "example/head" {:id :new-head :mode "7ch"})]
    (is (= ["7ch"] (get-in result [:fixture :modes])))
    (is (:cached? cached))
    (is (= (:library result) (:library cached)))
    (is (:valid? imported))
    (is (= "Example Lighting" (get-in imported [:definition :manufacturer])))
    (is (= :new-head (get-in imported [:definition :id])))
    (is (= [1 2] (get-in imported [:definition :channels :pan])))
    (is (= "https://open-fixture-library.org/example/head.json" (get-in imported [:provenance :source])))
    (is (= (get-in result [:fixture :provenance :sha256]) (get-in imported [:provenance :sha256])))
    (is (= "today" (get-in imported [:provenance :retrieved-at])))
    (is (= 2 (count @calls)))
    (is (empty? (:documents initial)))))

(deftest remote-search-uses-documented-request-schema-and-seeds-index
  (let [calls (atom []) result (library/search-remote (library/new-library) (provider calls) "RGBW"
                                                    {:manufacturers ["example"] :categories ["Moving Head"]})]
    (is (= :post (:method (first @calls))))
    (is (= {:searchQuery "RGBW" :manufacturersQuery ["example"] :categoriesQuery ["Moving Head"]}
           (:body (first @calls))))
    (is (= 1 (:total (library/search (:library result) {:query "rgbw"}))))
    (is (empty? (:documents (:library result))))))

(deftest cache-eviction-is-bounded-and-catalog-remains-searchable
  (let [calls (atom []) fetch (provider calls)
        initial (library/new-library {:max-documents 1})
        head (:library (library/fetch-fixture initial fetch "example/head"))
        wash (:library (library/fetch-fixture head fetch "example/wash"))]
    (is (= #{"example/wash"} (set (keys (:documents wash)))))
    (is (= 2 (:total (library/search wash {}))))
    (is (= 1 (:total (library/search wash {:query "head"}))))
    (is (thrown? clojure.lang.ExceptionInfo (library/import-fixture wash "example/head" {})))
    (is (= #{"example/head"} (set (keys (:documents head)))))))

(deftest provider-and-input-failures-do-not-mutate-the-library
  (let [initial (library/new-library) calls (atom []) fetch (provider calls)]
    (doseq [key ["../secret" "https://example.org/a" "example/head?other" "example//head"]]
      (is (thrown? clojure.lang.ExceptionInfo (library/fetch-fixture initial fetch key))))
    (is (empty? @calls))
    (is (thrown? clojure.lang.ExceptionInfo
                 (library/refresh-manufacturers initial (constantly {:status 503 :body "unavailable"}))))
    (is (thrown? clojure.lang.ExceptionInfo
                 (library/refresh-manufacturers (library/new-library {:max-response-bytes 10}) fetch)))
    (is (thrown? clojure.lang.ExceptionInfo
                 (library/refresh-manufacturer (library/new-library {:max-catalog-entries 1}) fetch "example")))
    (is (thrown? clojure.lang.ExceptionInfo
                 (library/fetch-fixture (library/new-library {:max-cache-bytes 10}) fetch "example/head")))
    (is (empty? (:entries initial)))
    (is (empty? (:documents initial)))))

(deftest upload-import-dispatch-is-data-only
  (is (:valid? (library/import-upload :ofl fixture-data/ofl {})))
  (is (:valid? (library/import-upload :qxf fixture-data/qxf {})))
  (is (:valid? (library/import-upload :gdtf (gdtf-data/zip-bytes [["description.xml" gdtf-data/description]]) {})))
  (is (thrown? clojure.lang.ExceptionInfo (library/import-upload :proprietary "data" {}))))
