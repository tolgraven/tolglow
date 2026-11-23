(ns tolglow.check-roadmap
  "Validate the local cross-repository work ledger, without external services."
  (:require [clojure.edn :as edn]
            [clojure.string :as str]
            [tolglow.check-requests :as requests]))

(def statuses #{:planned :in-progress :blocked :done})
(def kinds #{:documentation :implementation :research})

(defn- ensure! [condition message data]
  (when-not condition (throw (ex-info message data))))

(defn- overlaps? [a b]
  (or (= a b) (str/starts-with? a (str b "/"))
      (str/starts-with? b (str a "/"))))

(defn validate [ledger]
  (let [items (:items ledger)
        ids (mapv :id items)
        by-id (into {} (map (juxt :id identity)) items)]
    (ensure! (= 1 (:schema-version ledger)) "Unknown ledger version" {})
    (ensure! (and (vector? items) (seq items)) "Expected work items" {})
    (ensure! (= (count ids) (count (set ids))) "Duplicate work item IDs" {})
    (doseq [{:keys [id title status kind owner paths depends-on acceptance evidence blocker]} items]
      (ensure! (and (keyword? id) (namespace id) (string? title) (not (str/blank? title)))
               "Expected namespaced ID and title" {:id id})
      (ensure! (statuses status) "Unknown status" {:id id :status status})
      (ensure! (kinds kind) "Unknown kind" {:id id :kind kind})
      (ensure! (and (vector? paths) (seq paths)
                    (every? #(and (string? %)
                                  (re-matches #"(?:tolglow|afterglow)/[A-Za-z0-9_./-]+" %)
                                  (not (some #{".." "." ""} (str/split % #"/")))) paths))
               "Use exact repository-prefixed ownership paths" {:id id})
      (ensure! (and (vector? depends-on) (every? by-id depends-on))
               "Unknown dependency" {:id id :depends-on depends-on})
      (ensure! (and (vector? acceptance) (seq acceptance) (every? string? acceptance)
                    (vector? evidence) (every? string? evidence))
               "Expected acceptance and evidence vectors" {:id id})
      (when (= :in-progress status)
        (ensure! (and (string? owner) (not (str/blank? owner)))
                 "Active work needs an owner" {:id id})
        (ensure! (every? #(= :done (:status (by-id %))) depends-on)
                 "Active work has unfinished dependencies" {:id id}))
      (when (= :blocked status)
        (ensure! (and (string? blocker) (not (str/blank? blocker)))
                 "Blocked work needs a reason" {:id id}))
      (when (= :done status)
        (ensure! (seq evidence) "Done work needs evidence" {:id id})))
    (letfn [(visit [id stack]
              (ensure! (not (contains? stack id)) "Dependency cycle" {:id id})
              (doseq [dependency (:depends-on (by-id id))]
                (visit dependency (conj stack id))))]
      (doseq [id ids] (visit id #{})))
    (let [active (vec (filter #(= :in-progress (:status %)) items))]
      (doseq [i (range (count active)) j (range (inc i) (count active))
              a (:paths (active i)) b (:paths (active j))]
        (ensure! (not (overlaps? a b)) "Active write claims overlap"
                 {:items [(:id (active i)) (:id (active j))] :paths [a b]})))
    (frequencies (map :status items))))

(defn -main [& [path]]
  (let [ledger (edn/read-string (slurp (or path "docs/work-items.edn")))]
    (println "Work ledger valid:" (count (:items ledger)) "items" (validate ledger))
    (when-not path
      (let [current (slurp "docs/request-coverage.md")]
        (when-let [previous (System/getenv "TOLGLOW_PREVIOUS_REQUESTS")]
          (requests/preserve-ids! (slurp previous) current))
        (println "Conversation coverage:" (requests/validate ledger current))))))
