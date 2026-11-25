(ns definition-spacing
  "Check for blank lines between adjacent top-level definitions."
  (:require [clojure.java.shell :as shell]
            [clojure.string :as str]
            [clojure.tools.reader :as reader]
            [clojure.tools.reader.reader-types :as reader-types]))

(def ^:private definition-heads
  '#{def defn defn- defonce defmacro defmulti defmethod defrecord deftype
     defprotocol definterface definline deftest defspec defstruct})

(defn- definition? [form]
  (and (seq? form)
       (symbol? (first form))
       (contains? definition-heads (symbol (name (first form))))))

(defn- missing-lines [path source lines platform]
  (let [eof (Object.)
        input (reader-types/indexing-push-back-reader source 1 path)]
    (binding [reader/*read-eval* false
              reader/*alias-map* #(symbol (str "spacing-check." (name %)))
              reader/*default-data-reader-fn* tagged-literal]
      (loop [previous-end nil missing []]
        (let [form (reader/read {:eof eof :read-cond :allow :features #{platform}} input)]
          (if (identical? eof form)
            missing
            (let [line (:line (meta form))
                  definition (definition? form)
                  end (reader-types/get-line-number input)
                  missing? (and definition previous-end line
                                (not-any? str/blank?
                                          (subvec lines previous-end (max previous-end (dec line)))))]
              (recur (when definition end)
                     (cond-> missing missing? (conj line))))))))))

(defn- check-file [path]
  (let [source (slurp path :encoding "UTF-8")
        lines (vec (str/split source #"\n" -1))
        platforms (cond
                    (str/ends-with? path ".cljc") [:clj :cljs]
                    (str/ends-with? path ".cljs") [:cljs]
                    :else [:clj])]
    (->> platforms
         (mapcat #(missing-lines path source lines %))
         distinct
         sort)))

(defn -main [& _]
  (let [{:keys [exit out err]} (shell/sh "git" "ls-files" "-z" "--"
                                         "*.clj" "*.cljs" "*.cljc" ":(exclude)docs/reference/**")]
    (when-not (zero? exit)
      (throw (ex-info "Could not list tracked Clojure files" {:error err})))
    (let [files (remove str/blank? (str/split out #"\u0000"))
          violations (for [path files line (check-file path)] [path line])]
      (if (seq violations)
        (do
          (doseq [[path line] violations]
            (println (str path ":" line " Add a blank line before this definition.")))
          (System/exit 1))
        (do
          (println (str "Checked definition spacing in " (count files) " Clojure files."))
          (shutdown-agents))))))
