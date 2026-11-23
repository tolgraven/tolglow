(ns tolglow.linux.audience.floor
  "Bounded, receipt-time aggregation. All transitions return new values."
  (:require [tolglow.linux.audience.wire :as wire]))

(def capacity 128)
(def initial {:pending 0 :guests {}})

(defn delta [a b] (bit-and (- a b) 0xffffffff))
(defn newer? [a b] (<= 1 (delta a b) 0x7fffffff))

(defn expire [floor now]
  (update floor :guests #(into {} (filter (fn [[_ guest]] (< (- now (:time guest)) 2.5))) %)))

(defn accept [floor id sample now]
  (let [previous (get-in floor [:guests id])
        elapsed (when previous (- now (:time previous)))
        live? (and previous (< elapsed 2.5))
        added (if live? (delta (:steps sample) (:steps previous)) 0)
        credit (if live? (min 12 (+ (:credit previous) (* 4 (max 0 elapsed)))) 2)
        floor (if live? floor (expire floor now))]
    (when-not (and (or live? (< (count (:guests floor)) capacity))
                   (or (not live?) (and (not (neg? elapsed)) (newer? (:serial sample) (:serial previous))))
                   (<= added credit))
      (throw (ex-info "Out of order or invalid cumulative step count" {})))
    (when (> (+ added (:pending floor)) 2048) (throw (ex-info "Stage gesture queue overflow" {:stop true})))
    (-> floor (update :pending + added)
        (assoc-in [:guests id] (assoc sample :time now :credit (min 2 (- credit added)))))))

(defn- mean [xs] (/ (reduce + 0.0 xs) (max 1 (count xs))))

(defn drain [floor now]
  (let [floor (expire floor now) guests (vals (:guests floor)) pulses (keep :pulse guests)
        motion (keep :motion guests)
        levels (cond-> {:audience-count (count guests) :dance-effort (mean (map :effort guests))
                        :pulse-count (count pulses) :pulse-present (if (seq pulses) 1 0)
                        :motion-count (count motion)}
                 (seq pulses) (assoc :pulse-bpm (mean pulses))
                 (seq motion) (into (map #(vector % (mean (map % motion))) wire/axes)))]
    [(assoc floor :pending 0) levels (:pending floor)]))
