(ns tolglow.integrations.bands
  "Causal band levels and monophonic bass pitch, sampled by the audio worker."
  (:require [tolglow.integrations.audio :as audio]))

(def bass-range-hz [20.0 180.0])

(defn start [sample-rate]
  (let [stride (max 1 (long (Math/floor (/ sample-rate 1600.0))))
        rate (/ sample-rate stride)
        coefficient (fn [hz] (- 1.0 (Math/exp (/ (* -2.0 Math/PI hz) sample-rate))))]
    {:stride stride :pitch-rate rate :window-size (long (Math/ceil (* rate 0.16)))
     :low-coefficient (coefficient 180.0) :high-coefficient (coefficient 2500.0)
     :low 0.0 :wide 0.0 :bass 0.0 :remaining stride :sum 0.0 :window []
     :squares [0.0 0.0 0.0] :n 0 :since-pitch 0}))

(defn accumulate
  "Accumulate one part of an analyzer hop. Filter history survives PCM boundaries."
  [state ^doubles pcm start end]
  (let [{:keys [stride low-coefficient high-coefficient window-size]} state
        low-k (double low-coefficient) high-k (double high-coefficient)
        [low-square mid-square high-square] (:squares state)]
    (loop [i (long start) low (double (:low state)) wide (double (:wide state))
           bass (double (:bass state)) remaining (long (:remaining state)) sum (double (:sum state))
           ls (double low-square) ms (double mid-square) hs (double high-square)
           samples (transient [])]
      (if (= i end)
        (let [chunk (persistent! samples)
              window (into (:window state) chunk)]
          (assoc state :low low :wide wide :bass bass :remaining remaining :sum sum
                 :squares [ls ms hs] :n (+ (:n state) (- end start))
                 :since-pitch (+ (:since-pitch state) (count chunk))
                 :window (if (> (count window) window-size)
                           (into [] (subvec window (- (count window) window-size))) window)))
        (let [x (aget pcm i) low (+ low (* low-k (- x low)))
              wide (+ wide (* high-k (- x wide))) mid (- wide low) high (- x wide)
              bass (+ bass (* low-k (- low bass))) sum (+ sum bass)
              ready? (= remaining 1)]
          (recur (inc i) low wide bass (long (if ready? stride (dec remaining)))
                 (if ready? 0.0 sum) (+ ls (* low low)) (+ ms (* mid mid)) (+ hs (* high high))
                 (if ready? (conj! samples (/ sum stride)) samples)))))))

(defn finish-hop [state noise-floor]
  (let [[low mid high] (mapv #(Math/sqrt (/ % (:n state))) (:squares state))
        rate (:pitch-rate state)
        due? (and (= (count (:window state)) (:window-size state))
                  (>= (:since-pitch state) (* 0.04 rate)))
        pitch (cond
                (< low noise-floor) nil
                due? (let [[lo hi] bass-range-hz
                           result (audio/estimate-pitch (double-array (:window state)) rate lo hi 0.12)]
                       (when (and result (<= lo (:frequency result) hi) (>= (:confidence result) 0.88)) result))
                :else (:pitch state))]
    [(cond-> (assoc state :squares [0.0 0.0 0.0] :n 0 :pitch pitch)
       due? (assoc :since-pitch 0))
     {:low-rms low :mid-rms mid :high-rms high :bass-pitch-hz (:frequency pitch)
      :bass-pitch-confidence (get pitch :confidence 0.0)
      :bass-window-ms (* 1000.0 (/ (:window-size state) rate))}]))
