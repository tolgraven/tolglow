(ns tolglow.linux.audience.stage
  (:require [tolglow.linux.audience.floor :as floor]
            [tolglow.linux.audience.wire :as wire])
  (:import [java.security SecureRandom]
           [java.util Arrays]))

(defn create []
  (let [key (wire/keypair)]
    {:key key :public (wire/public-key key) :room (wire/room key) :random (SecureRandom.)
     :state (atom {:challenges {} :peers {} :floor floor/initial})}))

(defn- fresh [now entries]
  (into {} (filter (fn [[_ entry]] (< (- now (:time entry)) 8))) entries))

(defn- prune [state now]
  (-> state (update :challenges (partial fresh now)) (update :peers (partial fresh now))))

(defn identity! [{:keys [state public random]} device offset now]
  (locking state
    (let [{:keys [challenges] :as current} (prune @state now)]
      (when-not (<= 0 offset 81) (throw (ex-info "Invalid identity offset" {})))
      (when (and (zero? offset) (not (contains? challenges device)) (>= (count challenges) floor/capacity))
        (throw (ex-info "Stage enrollment capacity reached" {})))
      (let [entry (if (zero? offset) {:time now :nonce (wire/challenge random)} (get challenges device))]
        (when-not entry (throw (ex-info "Read the stage identity first" {})))
        (reset! state (assoc-in current [:challenges device] entry))
        (Arrays/copyOfRange (wire/join public (:nonce entry)) (int offset) 81)))))

(defn hello! [{:keys [key state]} device public now]
  (locking state
    (let [{:keys [challenges peers] :as current} (prune @state now)
          entry (get challenges device)]
      (when-not (and entry (or (get peers device) (< (count peers) floor/capacity)))
        (throw (ex-info "Enrollment challenge expired or stage full" {})))
      ;; Consume the challenge even when the supplied public point is invalid.
      (reset! state (update current :challenges dissoc device))
      (let [secret (wire/session-key key public (:nonce entry))]
        (swap! state #(-> % (assoc-in [:peers device] {:time now :key secret})
                           (update-in [:floor :guests] dissoc device)))))))

(defn receive! [{:keys [state]} device packet now]
  (locking state
    (let [current @state peer (get-in current [:peers device])]
      (when-not (and peer (< (- now (:time peer)) 8)) (throw (ex-info "Expired stage session" {})))
      (let [sample (wire/open-sample (:key peer) packet)]
        (when (and (:serial peer) (not (floor/newer? (:serial sample) (:serial peer))))
          (throw (ex-info "Replayed audience packet" {})))
        (try
          (reset! state (-> current (update :floor floor/accept device sample now)
                            (assoc-in [:peers device] (assoc peer :time now :serial (:serial sample)))))
          (catch clojure.lang.ExceptionInfo e
            (when (:stop (ex-data e)) (swap! state assoc :failed true))
            (throw e)))))))

(defn leave! [{:keys [state]} device]
  (locking state
    (swap! state #(-> % (update :peers dissoc device) (update :challenges dissoc device)
                      (update-in [:floor :guests] dissoc device)))))

(defn take-steps! [{:keys [state]}]
  (locking state
    (when (:failed @state) (throw (ex-info "Audience gesture queue overflow" {})))
    (let [n (get-in @state [:floor :pending])]
      (when (pos? n) (swap! state assoc-in [:floor :pending] 0))
      n)))

(defn drain! [{:keys [state]} now]
  (locking state
    (when (:failed @state) (throw (ex-info "Audience gesture queue overflow" {})))
    (let [current (prune @state now)
          [floor levels steps] (floor/drain (:floor current) now)]
      (reset! state (assoc current :floor floor))
      [levels steps])))
