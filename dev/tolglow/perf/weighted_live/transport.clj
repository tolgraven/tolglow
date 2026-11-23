(ns tolglow.perf.weighted-live.transport
  "Encode real ArtDMX datagrams and inspect their payload in memory."
  (:require [afterglow.output.artnet :as artnet])
  (:import [java.util Arrays]
           [java.util.zip CRC32]))

(def ^:private zero-universe (byte-array 512))

(defn- pixel-witness [packets pixels]
  (some (fn [[u ^bytes packet]]
          (let [n (min (- (alength packet) 18) (- (* 4 pixels) (* 512 (dec u))))]
            (when (pos? n)
              (let [i (Arrays/mismatch packet 18 (+ 18 n) ^bytes zero-universe 0 n)]
                (when-not (neg? i)
                  {:universe u :channel (inc i) :byte (bit-and 255 (aget packet (+ 18 i)))})))))
        packets))

(defn- wire-byte [packets offset]
  (if-let [^bytes packet (get packets (inc (quot offset 512)))]
    (bit-and 255 (aget packet (+ 18 (mod offset 512))))
    0))

(defn- aim [packets pixels]
  (let [digest (CRC32.)
        start (+ (* 4 pixels) 5)]
    (dotimes [head 64]
      (dotimes [channel 4]
        (.update digest (int (wire-byte packets (+ start (* 12 head) channel))))))
    {:crc32 (.getValue digest)
     :first-head-bytes (mapv #(wire-byte packets (+ start %)) (range 4))}))

(defn sink [pixels clock receipts]
  (let [sequence-number (volatile! 0)]
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
                                      :aim (aim packets pixels)})))}))
