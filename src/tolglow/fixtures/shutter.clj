(ns tolglow.fixtures.shutter
  "Physical DMX function ranges for discontinuous shutters and strobes."
  (:require [clojure.string :as str]))

(defn- hz-range? [hz]
  (and (vector? hz) (= 2 (count hz))
       (every? #(and (number? %) (Double/isFinite (double %)) (<= 0 %)) hz)))

(defn valid-channel-options? [attribute channel]
  (let [shutter? (#{:strobe :shutter} attribute)
        functions (:functions channel)]
    (and (every? (if shutter? #{:offset :fine-offset :inverted-from :functions :hz}
                             #{:offset :fine-offset :inverted-from}) (keys channel))
         (or (not (contains? channel :hz)) (hz-range? (:hz channel)))
         (or (not (contains? channel :functions))
             (and (vector? functions) (<= (count functions) 256)
                  (every? (fn [{:keys [start end type hz] :as f}]
                            (and (map? f) (every? #{:start :end :type :hz} (keys f))
                                 (integer? start) (integer? end) (<= 0 start end 255) (keyword? type)
                                 (or (nil? hz) (hz-range? hz)))) functions)
                  (every? (fn [[a b]] (< (:end a) (:start b))) (partition 2 1 functions)))))))

(defn- within? [f value]
  (and (integer? (:start f)) (integer? (:end f)) (<= (:start f) value (:end f))))

(defn- closed-function? [f]
  (and (keyword? (:type f)) (str/includes? (name (:type f)) "closed")))

(defn closed-ranges [functions] (filterv closed-function? functions))

(defn closed? [functions coarse-dmx-byte]
  (boolean (some #(and (within? % coarse-dmx-byte) (closed-function? %)) functions)))

(defn- strength [hz position]
  (if (hz-range? hz)
    (let [[lo hi] hz rate (+ lo (* position (- hi lo)))]
      (- 1.0 (Math/exp (/ (- rate) 5.0))))
    (double position)))

(defn response
  "Return [open? normalized-strobe-strength]. Function spans select on the raw
  coarse DMX byte; a dedicated unprofiled strobe uses its logical drive."
  [attribute channel coarse-dmx-byte drive]
  (if (seq (:functions channel))
    (let [{:keys [type start end hz] :as f} (some #(when (within? % coarse-dmx-byte) %) (:functions channel))]
      (cond
        (closed-function? f) [false 0.0]
        (and (keyword? type) (str/includes? (name type) "strobe"))
        [true (strength hz (if (= start end) 1.0 (/ (- coarse-dmx-byte start) (double (- end start)))))]
        :else [true 0.0]))
    [true (if (and (= attribute :strobe) (pos? drive)) (strength (:hz channel) drive) 0.0)]))
