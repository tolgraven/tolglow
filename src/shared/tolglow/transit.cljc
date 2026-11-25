(ns tolglow.transit
  "Shared Studio Transit JSON codec. Each message is independently readable.
  Transport owners bound message sizes and validate the decoded operation."
  (:require [cognitect.transit :as transit]
            [thi.ng.color.core :as color])
  #?(:clj (:import [clojure.lang PersistentQueue]
                   [java.io ByteArrayInputStream ByteArrayOutputStream]
                   [java.nio ByteBuffer ByteOrder]
                   [java.nio.charset StandardCharsets]
                   [java.time Instant]
                   [org.jogamp.vecmath Point3d Vector3d])))

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

;; Native values. docs/studio-transit.md lists each tag and its counterparts.
(defn- as-vector [rep] #?(:clj rep :cljs (if (array? rep) (vec rep) rep)))

#?(:cljs (defrecord Point3d [x y z]))

#?(:cljs (defrecord Vector3d [x y z]))

(defn- xyz [tag ctor]
  (fn [rep]
    (let [v (as-vector rep)]
      (when-not (and (vector? v) (= 3 (count v)) (every? number? v))
        (throw (ex-info "Expected three numeric components" {:tag tag})))
      (let [[x y z] v] (ctor (double x) (double y) (double z))))))

(defn- write-xyz [v]
  #?(:clj (let [^org.jogamp.vecmath.Tuple3d t v] [(.-x t) (.-y t) (.-z t)])
     :cljs [(:x v) (:y v) (:z v)]))

(def ^:private vector-types
  [["org.jogamp.vecmath/point3d.v1" Point3d #?(:clj #(Point3d. %1 %2 %3) :cljs ->Point3d)]
   ["org.jogamp.vecmath/vector3d.v1" Vector3d #?(:clj #(Vector3d. %1 %2 %3) :cljs ->Vector3d)]])

;; A browser Instant keeps the JVM's nanosecond field. `inst-ms` floors it to
;; the containing millisecond for js/Date, display and arithmetic.
#?(:cljs (defrecord Instant [epoch-second nano]))

#?(:cljs
   (extend-type Instant
     Inst
     (inst-ms* [i] (+ (* 1000 (:epoch-second i)) (js/Math.floor (/ (:nano i) 1000000))))
     IComparable
     (-compare [a b]
       (when-not (instance? Instant b)
         (throw (js/Error. "Cannot compare an Instant with another type")))
       (compare [(:epoch-second a) (:nano a)] [(:epoch-second b) (:nano b)]))))

(def ^:private instant-tag "tolglow/instant.v1")

;; js/Date represents ±8.64e15 ms. Keeping seconds inside that range makes
;; `(js/Date. (inst-ms t))` valid for every decoded browser Instant.
#?(:cljs (def ^:private min-browser-second -8640000000000))

#?(:cljs (def ^:private max-browser-second 8639999999999))

(defn- read-instant [rep]
  (let [[s n :as pair] (as-vector rep)]
    (when-not (and (vector? pair) (= 2 (count pair)) (string? s)
                   (re-matches #"-?(0|[1-9][0-9]{0,16})" s)
                   (integer? n) (<= 0 n 999999999))
      (throw (ex-info "Invalid instant" {:tag instant-tag})))
    #?(:clj (Instant/ofEpochSecond (Long/parseLong s) (long n))
       :cljs (let [seconds (js/parseInt s 10)]
               (when-not (<= min-browser-second seconds max-browser-second)
                 (throw (ex-info "Instant is outside the js/Date range" {:tag instant-tag
                                                                       :min-epoch-second min-browser-second
                                                                       :max-epoch-second max-browser-second})))
               (->Instant seconds n)))))

(defn- write-instant [v]
  #?(:clj [(str (.getEpochSecond ^Instant v)) (.getNano ^Instant v)]
     :cljs [(str (:epoch-second v)) (:nano v)]))

;; A ByteBuffer travels as its remaining bytes and byte order. The browser view
;; holds an unsigned Uint8Array and compares by contents; treat it as read-only.
;; The JVM receives a read-only heap buffer at position 0.
#?(:cljs
   (defn- same-bytes? [a b]
     (and (= (.-length a) (.-length b))
          (loop [i 0]
            (cond (= i (.-length a)) true
                  (= (aget a i) (aget b i)) (recur (inc i))
                  :else false)))))

#?(:cljs
   (deftype ByteView [order bytes]
     ILookup
     (-lookup [v k] (-lookup v k nil))
     (-lookup [_ k not-found] (case k :order order :bytes bytes not-found))
     IEquiv
     (-equiv [_ other]
       (and (instance? ByteView other) (= order (.-order other)) (same-bytes? bytes (.-bytes other))))
     IHash
     (-hash [_] (hash-combine (hash order) (hash-ordered-coll (array-seq (js/Array.from bytes)))))
     IPrintWithWriter
     (-pr-writer [_ writer _]
       (-write writer (str "#tolglow/byte-view [" order " " (.-length bytes) " bytes]")))))

(def ^:private byte-view-tag "tolglow/byte-buffer.v1")

(defn- byte-rep? [v] #?(:clj (bytes? v) :cljs (instance? js/Uint8Array v)))

(defn- read-byte-view [rep]
  (let [[order data :as pair] (as-vector rep)]
    (when-not (and (vector? pair) (= 2 (count pair)) (#{"big" "little"} order) (byte-rep? data))
      (throw (ex-info "Invalid byte buffer" {:tag byte-view-tag})))
    #?(:clj (.order (.asReadOnlyBuffer (ByteBuffer/wrap ^bytes data))
                    (if (= "little" order) ByteOrder/LITTLE_ENDIAN ByteOrder/BIG_ENDIAN))
       :cljs (->ByteView (if (= "little" order) :little-endian :big-endian) data))))

(defn- write-byte-view [v]
  #?(:clj (let [d (.duplicate ^ByteBuffer v) data (byte-array (.remaining d))]
            (.get d data)
            [(if (= ByteOrder/LITTLE_ENDIAN (.order ^ByteBuffer v)) "little" "big") data])
     :cljs [(if (= :little-endian (:order v)) "little" "big") (:bytes v)]))

;; Numeric arrays are little-endian element bytes inside a standard Transit
;; binary value. JVM byte[] and Uint8Array use Transit's own `b` tag directly.
#?(:clj (defn- le ^ByteBuffer [^bytes data] (.order (ByteBuffer/wrap data) ByteOrder/LITTLE_ENDIAN)))

#?(:clj
   (defn- array-codec [width write-class put! get!]
     {:width width :class write-class
      :write (fn [a] (let [data (byte-array (* width (count a)))] (put! (le data) a) data))
      :read (fn [^bytes data] (get! (le data) (quot (alength data) width)))}))

#?(:cljs
   (def ^:private little-endian?
     (= 1 (aget (js/Uint8Array. (.-buffer (js/Uint16Array. #js [1]))) 0))))

#?(:cljs
   (defn- swap-bytes! [u8 width]
     (when-not (or little-endian? (= 1 width))
       (loop [i 0]
         (when (< i (.-length u8))
           (.reverse (.subarray u8 i (+ i width)))
           (recur (+ i width)))))
     u8))

#?(:cljs
   (defn- array-codec [width ctor]
     {:width width :class ctor
      :write (fn [a]
               (let [view (js/Uint8Array. (.-buffer a) (.-byteOffset a) (.-byteLength a))]
                 (if little-endian? view (swap-bytes! (.slice view) width))))
      :read (fn [u8]
              (let [u8 (swap-bytes! (if (zero? (mod (.-byteOffset u8) width)) u8 (.slice u8)) width)]
                (new ctor (.-buffer u8) (.-byteOffset u8) (/ (.-byteLength u8) width))))}))

#?(:clj
   (defmacro ^:private jvm-array
     "Codec for a primitive array through the matching typed NIO buffer view."
     [width array-fn hint view]
     (let [a (with-meta (gensym "a") {:tag hint}) b (with-meta (gensym "b") {:tag `ByteBuffer}) n (gensym "n")]
       `(array-codec ~width (class (~array-fn 0))
                     (fn [~b ~a] (. (. ~b ~view) (put ~a)))
                     (fn [~b ~n] (let [~a (~array-fn ~n)] (. (. ~b ~view) (get ~a)) ~a))))))

;; JVM has no unsigned arrays. Unsigned browser arrays arrive as the same-width
;; signed JVM array with identical bits, which returns as the signed type. The
;; 8-bit arrays become byte[], which returns on `b` as an unsigned Uint8Array.
(def ^:private array-types
  #?(:clj
     (let [bytes-codec {:width 1 :read identity}
           int-codec (jvm-array 4 int-array ints asIntBuffer)
           long-codec (jvm-array 8 long-array longs asLongBuffer)]
       [["tolglow/int8-array.v1" bytes-codec]
        ["tolglow/uint8-clamped-array.v1" bytes-codec]
        ["tolglow/int16-array.v1" (jvm-array 2 short-array shorts asShortBuffer)]
        ["tolglow/uint16-array.v1" (jvm-array 2 char-array chars asCharBuffer)]
        ["tolglow/int32-array.v1" int-codec]
        ["tolglow/uint32-array.v1" (dissoc int-codec :class)]
        ["tolglow/int64-array.v1" long-codec]
        ["tolglow/uint64-array.v1" (dissoc long-codec :class)]
        ["tolglow/float32-array.v1" (jvm-array 4 float-array floats asFloatBuffer)]
        ["tolglow/float64-array.v1" (jvm-array 8 double-array doubles asDoubleBuffer)]])
     :cljs
     (filterv some?
              [["tolglow/int8-array.v1" (array-codec 1 js/Int8Array)]
               ["tolglow/uint8-clamped-array.v1" (array-codec 1 js/Uint8ClampedArray)]
               ["tolglow/int16-array.v1" (array-codec 2 js/Int16Array)]
               ["tolglow/uint16-array.v1" (array-codec 2 js/Uint16Array)]
               ["tolglow/int32-array.v1" (array-codec 4 js/Int32Array)]
               ["tolglow/uint32-array.v1" (array-codec 4 js/Uint32Array)]
               (when (exists? js/BigInt64Array) ["tolglow/int64-array.v1" (array-codec 8 js/BigInt64Array)])
               (when (exists? js/BigUint64Array) ["tolglow/uint64-array.v1" (array-codec 8 js/BigUint64Array)])
               ["tolglow/float32-array.v1" (array-codec 4 js/Float32Array)]
               ["tolglow/float64-array.v1" (array-codec 8 js/Float64Array)]])))

(defn- read-array [tag {:keys [width read]}]
  (fn [rep]
    (when-not (and (byte-rep? rep) (zero? (mod #?(:clj (alength ^bytes rep) :cljs (.-byteLength rep)) width)))
      (throw (ex-info "Invalid numeric array" {:tag tag :width width})))
    (read rep)))

(def ^:private queue-tag "tolglow/queue.v1")

(defn- read-queue [rep]
  (let [v (as-vector rep)]
    (when-not (vector? v) (throw (ex-info "Expected queue items" {:tag queue-tag})))
    (into #?(:clj PersistentQueue/EMPTY :cljs (.-EMPTY PersistentQueue)) v)))

(def ^:private write-handlers
  (into {#?(:clj clojure.lang.Ratio :cljs BeatFraction)
         (transit/write-handler fraction-tag
           (fn [v] (fraction-parts
                    #?(:clj [(str (numerator v)) (str (denominator v))]
                       :cljs [(:numerator v) (:denominator v)]))))}
        (concat
         (map (fn [[tag cls _]] [cls (transit/write-handler tag #(into {} %))]) color-types)
         (map (fn [[tag cls _]] [cls (transit/write-handler tag write-xyz)]) vector-types)
         (keep (fn [[tag {cls :class write :write}]]
                 (when cls [cls (transit/write-handler tag write)]))
               array-types)
         [[Instant (transit/write-handler instant-tag write-instant)]
          [#?(:clj ByteBuffer :cljs ByteView) (transit/write-handler byte-view-tag write-byte-view)]
          [PersistentQueue (transit/write-handler queue-tag vec)]])))

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
        (concat
         (map (fn [[tag _ read-color]]
                [tag (transit/read-handler
                      (fn [rep]
                        (when-not (map? rep) (throw (ex-info "Expected a color field map" {:tag tag})))
                        (read-color rep)))])
              color-types)
         (map (fn [[tag _ ctor]] [tag (transit/read-handler (xyz tag ctor))]) vector-types)
         (map (fn [[tag codec]] [tag (transit/read-handler (read-array tag codec))]) array-types)
         [[instant-tag (transit/read-handler read-instant)]
          [byte-view-tag (transit/read-handler read-byte-view)]
          [queue-tag (transit/read-handler read-queue)]])))

;; Each message owns its Transit cache, including when a handler rejects the
;; message halfway through. Only the immutable handler tables are shared.
#?(:clj (def ^:private writer-handlers (transit/write-handler-map write-handlers)))

#?(:clj (def ^:private reader-handlers (transit/read-handler-map read-handlers)))

(defn encode [value]
  #?(:clj (let [out (ByteArrayOutputStream.)]
            (transit/write (transit/writer out :json {:handlers writer-handlers}) value)
            (.toString out StandardCharsets/UTF_8))
     :cljs (transit/write (transit/writer :json {:handlers write-handlers}) value)))

#?(:clj
   (def ^:private inbound-values
     "Numeric vectors holding the values the browser array held."
     (let [longs #(mapv long %)
           doubles #(mapv double %)]
       {"tolglow/int8-array.v1" longs
        "tolglow/uint8-clamped-array.v1" #(mapv (fn [b] (bit-and 0xff (long b))) %)
        "tolglow/int16-array.v1" longs
        "tolglow/uint16-array.v1" #(mapv (fn [c] (long (int (char c)))) %)
        "tolglow/int32-array.v1" longs
        "tolglow/uint32-array.v1" #(mapv (fn [i] (Integer/toUnsignedLong (int i))) %)
        "tolglow/int64-array.v1" longs
        "tolglow/uint64-array.v1" #(mapv (fn [l] (if (neg? (long l)) (+' 18446744073709551616N l) l)) %)
        "tolglow/float32-array.v1" doubles
        "tolglow/float64-array.v1" doubles})))

;; Commands and stream messages from a browser or agent must not place mutable
;; objects in model state. Their numeric arrays become vectors and byte buffers
;; are refused. Standard `b` binary values remain byte[] as before.
#?(:clj
   (def ^:private command-reader-handlers
     (transit/read-handler-map
      (merge read-handlers
             (into {} (map (fn [[tag codec]]
                             [tag (transit/read-handler (comp (inbound-values tag) (read-array tag codec)))]))
                   array-types)
             {byte-view-tag (transit/read-handler
                             (fn [_] (throw (ex-info "Byte buffers are not accepted in commands" {:tag byte-view-tag}))))}))))

(defn- read-text [text handlers]
  (when-not (string? text)
    (throw (ex-info "Expected a Transit JSON string" {})))
  #?(:clj (let [input (ByteArrayInputStream. (.getBytes ^String text StandardCharsets/UTF_8))]
            (transit/read (transit/reader input :json {:handlers handlers})))
     :cljs (transit/read (transit/reader :json {:handlers handlers}) text)))

(defn decode [text]
  (read-text text #?(:clj reader-handlers :cljs read-handlers)))

#?(:clj
   (defn decode-command
     "Decode untrusted inbound Studio or agent input into immutable values."
     [text]
     (read-text text command-reader-handlers)))
