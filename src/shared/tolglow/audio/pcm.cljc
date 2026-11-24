(ns tolglow.audio.pcm
  "Interleaved channel layout for ordinary musical audio capture.")

(def max-channels 64)

(defn valid-layout? [channels input-channel]
  (and (integer? channels) (<= 1 channels max-channels)
       (if (nil? input-channel)
         (<= channels 2)
         (and (integer? input-channel) (<= 1 input-channel channels)))))
