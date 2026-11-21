(ns tolglow.studio.transit
  "Shared Studio Transit JSON codec. Each message is independently readable.
  Transport owners bound message sizes and validate the decoded operation."
  (:require [cognitect.transit :as transit]
            [thi.ng.color.core :as color])
  #?(:clj (:import [java.io ByteArrayInputStream ByteArrayOutputStream]
                   [java.nio.charset StandardCharsets])))

;; Register concrete library constructors explicitly on both platforms. Keeping
;; each record's fields preserves alpha, color space and user-added entries.
(defn- finite-components [fields]
  (fn [m]
    (every? (fn [field]
              (let [v (get m field)]
                (and (number? v) (#?(:clj Double/isFinite :cljs js/Number.isFinite) (double v)))))
            fields)))

(defn- packed [lo hi]
  (fn [{:keys [col]}]
    (and (integer? col) (<= lo col hi))))

(def ^:private color-types
  [["thi.ng.color/rgba.v1" (type (color/rgba 0 0 0)) color/map->RGBA (finite-components [:r :g :b :a])]
   ["thi.ng.color/hsla.v1" (type (color/hsla 0 0 0)) color/map->HSLA (finite-components [:h :s :l :a])]
   ["thi.ng.color/hsva.v1" (type (color/hsva 0 0 0)) color/map->HSVA (finite-components [:h :s :v :a])]
   ["thi.ng.color/cmyka.v1" (type (color/cmyka 0 0 0 0)) color/map->CMYKA (finite-components [:c :m :y :k :a])]
   ["thi.ng.color/int24.v1" (type (color/int24 0)) color/map->Int24 (packed 0 16777215)]
   ["thi.ng.color/int32.v1" (type (color/int32 0)) color/map->Int32 (packed -2147483648 4294967295)]
   ["thi.ng.color/css.v1" (type (color/css "")) color/map->CSS (comp string? :col)]])

(defn- color-fields [tag valid? value]
  (when-not (and (map? value) (valid? value))
    (throw (ex-info "Invalid color components" {:tag tag})))
  value)

#?(:cljs (defrecord BeatFraction [numerator denominator]))

(defn fraction? [v] #?(:clj (ratio? v) :cljs (instance? BeatFraction v)))

(defn fraction-value
  "Project a fraction to a number for a widget or sampler. Store and send the
  original fraction when exact beat divisions must survive a round trip."
  [v]
  #?(:clj (double v)
     :cljs (if (fraction? v)
             (/ (js/Number (:numerator v)) (js/Number (:denominator v)))
             v)))

(def ^:private fraction-tag "tolglow/ratio.v1")

(defn- fraction-parts [rep]
  (let [pair #?(:cljs (if (array? rep) (vec rep) rep) :clj rep)]
    (when-not (and (vector? pair) (= 2 (count pair))
                   (every? #(and (string? %) (<= 1 (count %) 128)) pair)
                   (re-matches #"-?[0-9]+" (first pair))
                   (re-matches #"[1-9][0-9]*" (second pair)))
      (throw (ex-info "Invalid fractional value" {:tag fraction-tag})))
    pair))

(def ^:private write-handlers
  (into {#?(:clj clojure.lang.Ratio :cljs BeatFraction)
         (transit/write-handler fraction-tag
           (fn [v] (fraction-parts
                    #?(:clj [(str (numerator v)) (str (denominator v))]
                       :cljs [(:numerator v) (:denominator v)]))))}
        (map (fn [[tag cls _ valid?]]
               [cls (transit/write-handler tag #(into {} (color-fields tag valid? %)))]))
        color-types))

(def ^:private read-handlers
  (into {fraction-tag
         (transit/read-handler
          (fn [rep]
            (let [[n d] (fraction-parts rep)]
              #?(:clj (/ (bigint n) (bigint d)) :cljs (->BeatFraction n d)))))}
        (map (fn [[tag _ ctor valid?]]
               [tag (transit/read-handler #(ctor (color-fields tag valid? %)))]))
        color-types))

;; The JVM implementation precomputes immutable handler tables. Its stream-bound
;; reader/writer instances remain private to a single call. The browser library
;; resets its message cache after each synchronous read/write.
#?(:clj (def ^:private writer-handlers (transit/write-handler-map write-handlers))
   :cljs (def ^:private writer (transit/writer :json {:handlers write-handlers})))
#?(:clj (def ^:private reader-handlers (transit/read-handler-map read-handlers))
   :cljs (def ^:private reader (transit/reader :json {:handlers read-handlers})))

(defn encode [value]
  #?(:clj (let [out (ByteArrayOutputStream.)]
            (transit/write (transit/writer out :json {:handlers writer-handlers}) value)
            (.toString out StandardCharsets/UTF_8))
     :cljs (transit/write writer value)))

(defn decode [text]
  (when-not (string? text)
    (throw (ex-info "Expected a Transit JSON string" {})))
  #?(:clj (let [input (ByteArrayInputStream. (.getBytes ^String text StandardCharsets/UTF_8))]
            (transit/read (transit/reader input :json {:handlers reader-handlers})))
     :cljs (transit/read reader text)))
