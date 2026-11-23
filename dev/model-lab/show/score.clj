(ns model-lab.show-score
  "Compare held-out EDN commands in the same demo snapshot without evaluating source."
  (:require [clojure.data.json :as json]
            [clojure.java.io :as io]
            [tolglow.agents.forms :as forms]
            [tolglow.data :as data]
            [tolglow.show :as show]))

(defn read-rows [path]
  (with-open [reader (io/reader path)]
    (mapv #(json/read-str % :key-fn keyword) (line-seq reader))))

(defn setup! [runtime row]
  (doseq [[i step] (map-indexed vector (some-> row :source :setup-edn data/read-edn))]
    (let [receipt (forms/submit! runtime step)]
      (show/tick! runtime (* i 25))
      (when-not (= :accepted (:status (deref receipt 2000 nil)))
        (throw (ex-info "Held-out setup failed" {:id (:id row) :step step}))))))

(defn result [row prediction]
  (if (nil? prediction)
    :missing
    (let [runtime (show/create-runtime {:workspace-options nil :workers 1})]
      (try
        (setup! runtime row)
        (let [model (:model @(:state runtime))
              gold (data/read-edn (:content (peek (:messages row))))
              expected (forms/request model gold)]
          (try
            (if (= expected (forms/request model (data/read-edn prediction))) :equivalent :different)
            (catch Exception _ :invalid)))
        (finally (show/stop! runtime))))))

(defn summarize [reference predicted]
  (let [rows (filterv #(= "show/edn" (:mode %)) reference)
        guesses (into {} (map (juxt :id :content)) predicted)
        ids (set (map :id reference))]
    (when (or (empty? rows) (not= (count predicted) (count guesses))
              (not-every? ids (keys guesses)) (not-every? string? (vals guesses)))
      (throw (ex-info "Expected unique text predictions from this reference split" {})))
    (reduce (fn [counts row]
              (update-in counts [(:family row) (result row (get guesses (:id row)))] (fnil inc 0)))
            (sorted-map) rows)))

(try
  (let [[reference predicted] *command-line-args*]
    (println (json/write-str (summarize (read-rows reference) (read-rows predicted)))))
  (finally (shutdown-agents)))
