(ns tolglow.check-requests
  "Check the archived request index and its GitHub issue links offline."
  (:require [clojure.string :as str]))

(def ^:private issue-link
  #"\[[^\]]+\]\(https://github\.com/tolgraven/tolglow/issues/[1-9]\d*\)")

(defn- request-rows [markdown]
  (for [line (str/split-lines markdown)
        :when (and (str/starts-with? line "| R") (not (str/starts-with? line "| Request |")))]
    (mapv str/trim (str/split line #"(?<!\\)\|" -1))))

(defn- parse-row [[leading id original refs trailing :as fields]]
  (let [turn (some->> id (re-matches #"R(\d+)[a-z]?") second parse-long)]
    (when-not (and (= 5 (count fields)) (= "" leading trailing) (pos-int? turn)
                   (not (str/blank? original)))
      (throw (ex-info "Malformed archived request" {:fields fields})))
    (when-not (and (re-find issue-link refs)
                   (re-matches #"(?:[\s,;·]|<br\s*/?>)*" (str/replace refs issue-link "")))
      (throw (ex-info "Expected links to tolgraven/tolglow GitHub Issues" {:request id :links refs})))
    {:id id :turn turn}))

(defn preserve-ids! [previous current]
  ;; Only the identifiers are read from the previous version. This also works
  ;; when the migration PR compares the original four-column archive with this one.
  (let [ids #(map second (request-rows %))
        retained (set (ids current))
        missing (remove retained (ids previous))]
    (when (seq missing)
      (throw (ex-info "Earlier request IDs were removed" {:missing (vec missing)})))))

(defn validate [markdown]
  (let [rows (mapv parse-row (request-rows markdown))
        ids (mapv :id rows)
        turns (mapv :turn rows)]
    (when-not (and (seq rows)
                   (= turns (sort turns))
                   (= (distinct turns) (range 1 (inc (peek turns))))
                   (= (count ids) (count (set ids))))
      (throw (ex-info "Missing, duplicated or out-of-order archived request" {:ids ids})))
    {:requests (peek turns) :entries (count rows)}))

(defn -main [& [path]]
  (let [current (slurp (or path "docs/request-coverage.md"))]
    (when-let [previous (System/getenv "TOLGLOW_PREVIOUS_REQUESTS")]
      (preserve-ids! (slurp previous) current))
    (println "Archived requests checked" (validate current))))
