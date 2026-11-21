(ns tolglow.studio.credentials-test
  (:require [clojure.test :refer :all]
            [clojure.edn :as edn]
            [tolglow.studio.credentials :as credentials])
  (:import [java.nio.file Files LinkOption]
           [java.nio.file.attribute FileAttribute PosixFilePermissions]))

(def no-links (make-array LinkOption 0))
(defn temp-directory [] (Files/createTempDirectory "tolglow-credential-test-" (make-array FileAttribute 0)))
(defn remove-tree! [path]
  (doseq [file (reverse (file-seq (.toFile path)))] (.delete file)))

(deftest store-construction-is-inert-and-keys-round-trip-by-private-reference
  (let [directory (temp-directory) path (.resolve directory "private/credentials.edn")
        store (credentials/file-store path)]
    (try
      (is (not (Files/exists path no-links)))
      (let [one ((:put! store) "fake-test-secret-one") two ((:put! store) "fake-test-secret-two")
            reopened (credentials/file-store path)]
        (is (credentials/valid-reference? one))
        (is (not= one two))
        (is (= "fake-test-secret-one" ((:get! reopened) one)))
        (is (= "fake-test-secret-two" ((:get! reopened) two)))
        (is (= "rw-------" (PosixFilePermissions/toString (Files/getPosixFilePermissions path no-links))))
        (is (= "rwx------" (PosixFilePermissions/toString (Files/getPosixFilePermissions (.getParent path) no-links))))
        (is (true? ((:delete! store) one)))
        (is (nil? ((:get! reopened) one)))
        (is (false? ((:delete! store) one)))
        (is (= "fake-test-secret-two" ((:get! reopened) two)))
        (is (= #{"credentials.edn"} (set (map #(.getName %) (.listFiles (.toFile (.getParent path))))))))
      (finally (remove-tree! directory)))))

(deftest invalid-keys-references-and-file-permissions-are-rejected
  (let [directory (temp-directory) path (.resolve directory "credentials.edn") store (credentials/file-store path)]
    (try
      (doseq [secret [nil "" "line\nheader" (apply str (repeat 16385 "x"))]]
        (is (thrown? clojure.lang.ExceptionInfo ((:put! store) secret))))
      (is (not (Files/exists path no-links)))
      (is (thrown? clojure.lang.ExceptionInfo ((:get! store) "../keys")))
      (let [reference ((:put! store) "fake-secret")]
        (Files/setPosixFilePermissions path (PosixFilePermissions/fromString "rw-r--r--"))
        (is (= :unsafe-permissions
               (try ((:get! store) reference) nil (catch clojure.lang.ExceptionInfo error (:credentials/error (ex-data error))))))
        (Files/setPosixFilePermissions path (PosixFilePermissions/fromString "rw-------"))
        (is (= "fake-secret" ((:get! store) reference))))
      (finally (remove-tree! directory)))))

(deftest malformed-store-never-evaluates-forms-or-overwrites-the-file
  (let [directory (temp-directory) path (.resolve directory "credentials.edn") store (credentials/file-store path)]
    (try
      ((:put! store) "fake-initial")
      (doseq [text ["#=(System/exit 1)" "{:version 1 :credentials {}} {:extra true}"
                    "{:version 1 :credentials {} :unexpected true}"]]
        (spit (.toFile path) text)
        (is (thrown? clojure.lang.ExceptionInfo ((:put! store) "fake-next")))
        (is (= text (slurp (.toFile path)))))
      (finally (remove-tree! directory)))))

(deftest shared-adapter-serializes-concurrent-atomic-writes
  (let [directory (temp-directory) path (.resolve directory "credentials.edn") store (credentials/file-store path)]
    (try
      (let [jobs (mapv (fn [index] (future ((:put! store) (str "fake-" index)))) (range 8))
            references (mapv deref jobs)]
        (is (= 8 (count (set references))))
        (is (= (mapv #(str "fake-" %) (range 8)) (mapv #((:get! store) %) references)))
        (is (= 8 (count (:credentials (edn/read-string (slurp (.toFile path))))))))
      (finally (remove-tree! directory)))))

(deftest symbolic-link-targets-are-not-followed
  (let [directory (temp-directory) target (.resolve directory "target.edn") path (.resolve directory "link.edn")
        store (credentials/file-store path)]
    (try
      (spit (.toFile target) "unchanged")
      (Files/createSymbolicLink path target (make-array FileAttribute 0))
      (is (thrown? clojure.lang.ExceptionInfo ((:put! store) "fake-secret")))
      (is (= "unchanged" (slurp (.toFile target))))
      (finally (Files/deleteIfExists path) (remove-tree! directory)))))
