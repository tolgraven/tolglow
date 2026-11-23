(ns tolglow.request-archive-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.string :as str]
            [tolglow.check-requests :as requests]))

(defn- entry [id & issues]
  (str "| " id " | Preserve the original request | "
       (str/join ", " (map #(str "[#" % "](https://github.com/tolgraven/tolglow/issues/" % ")") issues))
       " |"))

(deftest archived-requests-link-to-one-tracker-without-a-local-disposition
  (let [index (str/join "\n" [(entry "R1a" 12) (entry "R1b" 14 28) (entry "R2" 12)])]
    (is (= {:requests 2 :entries 3} (requests/validate index)))
    (doseq [bad [(str/replace index "/issues/12" "/pull/12")
                 (str/replace index "tolgraven/tolglow" "tolgraven/afterglow")
                 (str/replace index "R1b" "R1a")
                 (str/replace index "R2" "R3")
                 (str/replace index "R1a" "R3")
                 (entry "R1")]]
      (is (thrown? clojure.lang.ExceptionInfo (requests/validate bad))))
    (is (nil? (requests/preserve-ids! "| R1a | Original text | old-key | Open |" index)))
    (is (thrown? clojure.lang.ExceptionInfo
                 (requests/preserve-ids! index (str/join "\n" [(entry "R1a" 12) (entry "R2" 12)]))))))
