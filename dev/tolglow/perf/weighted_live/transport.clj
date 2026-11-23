(ns tolglow.perf.weighted-live.transport
  "Encode real ArtDMX datagrams and inspect their payload in memory."
  (:require [afterglow.output.artnet :as artnet])
  (:import [java.util Arrays]
           [java.util.zip CRC32]))

(def ^:private zero-universe (byte-array 512))

(defn- required-payloads [pixels]
  ;; RGBW pixels occupy four slots. The 64 twelve-slot moving heads use
  ;; channels 1..11; the last slot in each patch is reserved.
  (let [slots (dec (+ (* 4 pixels) (* 64 12)))]
    (into {} (map-indexed (fn [i offset] [(inc i) (min 512 (- slots offset))]))
          (range 0 slots 512))))

(defn- incomplete-universes [required packets]
  (let [covered? (fn [[u minimum]]
                   (when-let [^bytes packet (get packets u)]
                     (>= (- (alength packet) 18) minimum)))]
    (when-not (and (= (count packets) (count required)) (every? covered? required))
      (let [short (into {} (keep (fn [[u ^bytes packet]]
                                   (let [minimum (get required u 0) actual (- (alength packet) 18)]
                                     (when (< actual minimum) [u {:actual actual :minimum minimum}]))))
                        packets)]
        {:missing (sort (remove #(contains? packets %) (keys required)))
         :unexpected (sort (remove #(contains? required %) (keys packets)))
         :short-payloads short}))))

(defn- pixel-witness [packets pixels]
  (some (fn [[u ^bytes packet]]
          (let [n (min (- (alength packet) 18) (- (* 4 pixels) (* 512 (dec u))))]
            (when (pos? n)
              (let [i (Arrays/mismatch packet 18 (+ 18 n) ^bytes zero-universe 0 n)]
                (when-not (neg? i)
                  {:universe u :channel (inc i) :byte (bit-and 255 (aget packet (+ 18 i)))})))))
        packets))

(defn- wire-byte [packets offset]
  (let [^bytes packet (get packets (inc (quot offset 512)))
        index (+ 18 (mod offset 512))]
    (if (and packet (< index (alength packet))) (bit-and 255 (aget packet index)) 0)))

(defn- aim [packets pixels]
  (let [digest (CRC32.)
        start (+ (* 4 pixels) 5)]
    (dotimes [head 64]
      (dotimes [channel 4]
        (.update digest (int (wire-byte packets (+ start (* 12 head) channel))))))
    {:crc32 (.getValue digest)
     :first-head-bytes (mapv #(wire-byte packets (+ start %)) (range 4))}))

(defn sink [pixels clock receipts]
  (let [sequence-number (volatile! 0) required (required-payloads pixels)]
    {:description {:type :artdmx-capture :network false}
     :close! (fn [])
     :send! (fn [universes id]
              (let [entry (clock)
                    sequence (vswap! sequence-number #(inc (mod % 255)))
                    packets (into (sorted-map)
                                  (map (fn [[u bytes]] [u (artnet/artdmx-packet u sequence bytes)]))
                                  universes)
                    handoff (clock)]
                (swap! receipts conj {:id id :at-ms handoff :driver-at-ms entry :sequence sequence
                                      :universes (count packets) :lit-pixel (pixel-witness packets pixels)
                                      :incomplete-universes (when (pos? id) (incomplete-universes required packets))
                                      :aim (aim packets pixels)})))}))
