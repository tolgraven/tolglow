(ns tolglow.native.resources-test
  (:require [clojure.test :refer [deftest is]]
            [tolglow.native-resources :as resources])
  (:import [java.io ByteArrayInputStream]
           [java.net URL URLConnection URLStreamHandler]))

(defn- image-url [path contents]
  (URL. nil (str "resource:/" path)
        (proxy [URLStreamHandler] []
          (openConnection [url]
            (proxy [URLConnection] [url]
              (getInputStream [] (ByteArrayInputStream. (.getBytes ^String contents "UTF-8"))))))))

(deftest only-files-packed-in-the-public-asset-index-can-be-served
  (let [index {"studio/index.html" 4 "studio/empty" 0}]
    (doseq [[path body] [["studio/index.html" "test"] ["studio/empty" ""]]]
      (let [{:keys [content content-length]} (resources/asset-data index (image-url path body))]
        (is (= body (slurp content)))
        (is (= (count body) content-length))))
    (doseq [path ["studio" "studio/" "tolglow/native-assets.edn" "studio/../secret"]]
      (is (nil? (resources/asset-data index (image-url path "private")))))))
