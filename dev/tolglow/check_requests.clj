(ns tolglow.check-requests
  "Validate the human-edited request index against the work ledger."
  (:require [clojure.string :as str]))

(defn- fail [reason context]
  (throw (ex-info reason context)))

(defn- request [known line]
  (let [[id acceptance refs status :as fields] (mapv str/trim (rest (str/split line #"\|" -1)))
        [_ number] (re-matches #"R(\d+)[a-z]?" (or id ""))
        tokens (str/split (or refs "") #",\s*")
        ids (mapv #(some->> % (re-matches #"`:([\w.-]+/[\w.-]+)`") second keyword) tokens)]
    (when-not (and (= 5 (count fields)) number (seq acceptance)
                   (or (#{"Open" "Standing"} status)
                       (and (str/starts-with? status "Verified · ") (re-find #"\[[^\]]+\]\([^)]+\)" status))))
      (fail "Malformed request or missing verification link" {:line line}))
    (when-not (every? known ids)
      (fail "Request references an unknown work item" {:request id :refs ids}))
    {:id id :turn (Long/parseLong number) :status (first (str/split status #" "))}))

(defn- request-lines [markdown]
  (filter #(str/starts-with? % "| R") (remove #(str/starts-with? % "| Request |") (str/split-lines markdown))))

(defn preserve-ids! [previous current]
  (let [ids #(map (comp str/trim second (fn [line] (str/split line #"\|"))) (request-lines %))
        retained (set (ids current))
        missing (remove retained (ids previous))]
    (when (seq missing)
      (fail "Earlier request IDs were removed" {:missing (vec missing)}))))

(defn validate [ledger markdown]
  (let [known (set (map :id (:items ledger)))
        declared (some->> markdown (re-find #"Recorded through request \*\*(\d+)\*\*") second parse-long)
        rows (mapv #(request known %) (request-lines markdown))
        ids (mapv :id rows)
        turns (mapv :turn rows)]
    (when-not (and (pos-int? declared) (= (distinct turns) (range 1 (inc declared)))
                   (apply <= turns) (= (count ids) (count (set ids))))
      (fail "Missing, duplicated or out-of-order conversation request" {:declared declared :ids ids}))
    {:turns declared :entries (count rows) :dispositions (frequencies (map :status rows))}))
