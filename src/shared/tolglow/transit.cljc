(ns tolglow.transit
  "Shared Studio Transit JSON codec. Each message is independently readable.
  Transport owners bound message sizes and validate the decoded operation."
  (:require [cognitect.transit :as transit]
            [thi.ng.color.core :as color])
  #?(:clj (:import [java.io ByteArrayInputStream ByteArrayOutputStream]
                   [java.nio.charset StandardCharsets])))

;; Register concrete library constructors explicitly on both platforms. Keeping
;; each record's fields preserves alpha, color space and user-added entries.
(defn- components [ctor fields]
  (fn [rep]
    (when-not (every? (fn [field]
                        (let [v (get rep field)]
                          (and (number? v) (#?(:clj Double/isFinite :cljs js/Number.isFinite) (double v)))))
                      fields)
      (throw (ex-info "Expected finite color components" {:fields fields})))
    (ctor (reduce #(update %1 %2 double) rep fields))))

(defn- packed [constructor coerce lo hi]
  (fn [{:keys [col] :as rep}]
    (when-not (and (integer? col) (<= lo col hi))
      (throw (ex-info "Invalid packed color" {:min lo :max hi})))
    (constructor (update rep :col coerce))))

(defn- css [rep]
  (when-not (string? (:col rep))
    (throw (ex-info "CSS color requires a string" {})))
  (color/map->CSS rep))

(def ^:private color-types
  [["thi.ng.color/rgba.v1" (type (color/rgba 0 0 0)) (components color/map->RGBA [:r :g :b :a])]
   ["thi.ng.color/hsla.v1" (type (color/hsla 0 0 0)) (components color/map->HSLA [:h :s :l :a])]
   ["thi.ng.color/hsva.v1" (type (color/hsva 0 0 0)) (components color/map->HSVA [:h :s :v :a])]
   ["thi.ng.color/cmyka.v1" (type (color/cmyka 0 0 0 0)) (components color/map->CMYKA [:c :m :y :k :a])]
   ["thi.ng.color/int24.v1" (type (color/int24 0)) (packed color/map->Int24 int 0 16777215)]
   ["thi.ng.color/int32.v1" (type (color/int32 0)) (packed color/map->Int32 long -2147483648 4294967295)]
   ["thi.ng.color/css.v1" (type (color/css "")) css]])

(def ^:private color-classes (into #{} (map second) color-types))

(defn color-value? [value] (contains? color-classes (type value)))

#?(:cljs (defrecord BeatFraction [numerator denominator]))

#?(:cljs
   (extend-type BeatFraction
     IPrintWithWriter
     (-pr-writer [fraction writer _] (-write writer (str (:numerator fraction) "/" (:denominator fraction))))))

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
        (map (fn [[tag cls _]]
               [cls (transit/write-handler tag #(into {} %))]))
        color-types))

(def ^:private read-handlers
  (into {;; The standard JVM reader exposes a seq backed by a Java ArrayList.
         ;; Materialize the standard list tag for callers that distinguish lists.
         #?@(:clj ["list" (transit/read-handler #(apply list %))])
         fraction-tag
         (transit/read-handler
          (fn [rep]
            (let [[n d] (fraction-parts rep)]
              #?(:clj (/ (java.math.BigInteger. ^String n) (java.math.BigInteger. ^String d))
                 :cljs (->BeatFraction n d)))))}
        (map (fn [[tag _ read-color]]
               [tag (transit/read-handler
                     (fn [rep]
                       (when-not (map? rep) (throw (ex-info "Expected a color field map" {:tag tag})))
                       (read-color rep)))]))
        color-types))

;; Each message owns its Transit cache, including when a handler rejects the
;; message halfway through. Only the immutable handler tables are shared.
#?(:clj (def ^:private writer-handlers (transit/write-handler-map write-handlers)))

#?(:clj (def ^:private reader-handlers (transit/read-handler-map read-handlers)))

(defn encode [value]
  #?(:clj (let [out (ByteArrayOutputStream.)]
            (transit/write (transit/writer out :json {:handlers writer-handlers}) value)
            (.toString out StandardCharsets/UTF_8))
     :cljs (transit/write (transit/writer :json {:handlers write-handlers}) value)))

(defn decode [text]
  (when-not (string? text)
    (throw (ex-info "Expected a Transit JSON string" {})))
  #?(:clj (let [input (ByteArrayInputStream. (.getBytes ^String text StandardCharsets/UTF_8))]
            (transit/read (transit/reader input :json {:handlers reader-handlers})))
     :cljs (transit/read (transit/reader :json {:handlers read-handlers}) text)))
