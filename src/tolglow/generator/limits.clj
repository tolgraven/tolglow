(ns tolglow.generator.limits
  "Limits and the result-shape check shared by the generator sandbox (#855)
  and the host that runs it. No SCI, no IO: the host checks a worker's answer
  with the same walk the worker used before printing it."
  (:import [java.nio.charset StandardCharsets]
           [java.security MessageDigest]
           [java.util HexFormat]))

(def defaults
  "Default bounds for one generator run. :steps counts SCI interrupt checks
  (every interpreted fn or loop entry and every element a lazy producer or
  consumer touches), so the same source and seed stop at the same point on any
  machine. :deadline-ms is wall time from request to answer, enforced by
  killing the worker process. :allocated-bytes is the evaluating thread's
  cumulative allocation. :heap-mb bounds the worker JVM's heap."
  {:source-chars 16384
   :deadline-ms 500
   :steps 1000000
   :allocated-bytes (* 256 1024 1024)
   :heap-mb 384
   :result-nodes 20000
   :result-depth 48
   :string-chars 4096
   :result-chars (* 512 1024)})

(defn limits
  "Defaults with overrides. Unknown keys are refused, so a misspelt limit is
  never silently ignored."
  [overrides]
  (let [unknown (remove (set (keys defaults)) (keys overrides))]
    (when (seq unknown)
      (throw (ex-info "Unknown generator limit" {:errors [{:path [:limits] :message (str "Unknown limits " (pr-str (sort unknown)))}]})))
    (doseq [[k v] overrides]
      (when-not (and (integer? v) (pos? v))
        (throw (ex-info "Invalid generator limit" {:errors [{:path [:limits k] :message "Limits are positive integers"}]}))))
    (merge defaults overrides)))

(defn- error [path message] {:path path :message message})

(defn portable-errors
  "Walk a generator result without trusting its size: stop at the first
  problem or when more than :result-nodes values were visited. Accepts the
  portable EDN subset the registry accepts (nil, booleans, finite numbers,
  strings up to :string-chars, keywords, vectors, sets and plain maps with
  keyword/string/integer keys). Records, sequences, functions and host objects
  are refused with their path."
  [value {:keys [result-nodes result-depth string-chars]}]
  (let [budget (volatile! result-nodes)
        problem (volatile! nil)
        fail! (fn [path message] (vreset! problem (error path message)) false)]
    (letfn [(visit [x path depth]
              (cond
                @problem false
                (neg? (vswap! budget dec)) (fail! [] (str "Result has more than " result-nodes " values"))
                (> depth result-depth) (fail! path (str "Result nests deeper than " result-depth " levels"))
                (or (nil? x) (boolean? x) (keyword? x)) true
                (string? x) (or (<= (count x) string-chars)
                                (fail! path (str "Strings are limited to " string-chars " characters")))
                (number? x) (or (Double/isFinite (double x))
                                (fail! path "Numbers must be finite"))
                (record? x) (fail! path "Records are not portable data; return a plain map")
                (map? x) (every? (fn [[k v]]
                                   (and (or (keyword? k) (string? k) (integer? k)
                                            (fail! path (str "Map keys must be keywords, strings or integers, not " (pr-str (type k)))))
                                        (visit v (conj path k) (inc depth))))
                                 x)
                (vector? x) (every? identity (map-indexed (fn [i v] (visit v (conj path i) (inc depth))) x))
                (set? x) (every? #(visit % path (inc depth)) x)
                (sequential? x) (fail! path "Lists and lazy sequences are not portable data; return a vector")
                (fn? x) (fail! path "Functions are not portable data")
                :else (fail! path (str "Not portable data: " (.getName (class x))))))]
      (visit value [] 0)
      (when-let [p @problem] [p]))))

(defn- canonical [x]
  (cond
    (map? x) (str "{" (apply str (interpose " " (sort (map (fn [[k v]] (str (canonical k) " " (canonical v))) x)))) "}")
    (set? x) (str "#{" (apply str (interpose " " (sort (map canonical x)))) "}")
    (vector? x) (str "[" (apply str (interpose " " (map canonical x))) "]")
    :else (pr-str x)))

(defn digest
  "SHA-256 of a canonical print of portable data: map entries and set members
  sorted by their printed form, so equal data has one digest regardless of
  hash-map iteration order."
  [value]
  (.formatHex (HexFormat/of)
              (.digest (MessageDigest/getInstance "SHA-256")
                       (.getBytes ^String (canonical value) StandardCharsets/UTF_8))))
