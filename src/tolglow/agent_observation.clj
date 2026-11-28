(ns tolglow.agent-observation
  "What a show-control agent reads: `tolglow-agent-obs/1` (#849).

  A pure projection of one committed show state into a JSON-shaped document,
  sent as a snapshot and then as deltas against a base token the consumer
  acknowledged. Slow sections (rig, catalogue, goals) are content-addressed
  blocks; the others are projected every time. Tables are {legend, rows} with
  rows keyed by the show's own IDs, so a delta can address one cell. Deltas
  use the set/remove operations of the state stream (docs/state-stream.md).

  The projection reads only values the frame owner already committed: the
  model, the delivered output and the meters published on it. It evaluates no
  effect, signal, waveform or random source, and it never writes show state.
  Call it from a reader thread (the REPL, an HTTP request) with the committed
  state reference; never from the frame owner.

  Only IDs, numbers and enumerations are projected. An ID that is not a short
  identifier (letters, digits and `_.:/+-`, at most 64 characters) is sent as
  `~` and a digest of it. Labels, names, notes, provider configuration,
  credentials, integration settings and session IDs are never read; the
  consumer's own session is marked `self`.

      (require '[tolglow.show :as show])
      (def snap (show/agent-observation runtime {:profile :compact :session \"planner\"}))
      (show/agent-observation runtime {:profile :compact :session \"planner\" :base (get snap \"token\")})

  Schema: resources/agent/observation.schema.json (the one copy; the
  research links to it).
  Guide: docs/agent-observation.md."
  (:require [afterglow.render.frame :as render]
            [tolglow.event-subscribers :as subscribers]
            [tolglow.registry :as registry])
  (:import [java.math BigDecimal RoundingMode]
           [java.nio.charset StandardCharsets]
           [java.security MessageDigest]
           [java.util HashMap UUID]))

(def schema "tolglow-agent-obs/1")

(def look-version 1)

(def rig-signature-version 1)

(def profiles #{:tick :compact :full})

(def slow-sections ["rig" "catalog" "goals"])

(def dynamic-sections
  ["clock" "run" "sched" "audio" "signals" "look" "budgets" "humans" "agents" "feedback"])

(def section-order (into slow-sections dynamic-sections))

(def caps
  "Token caps per profile, enforced on every envelope: a `tick` observation,
  a `compact` or `full` snapshot, and any delta. Tokens are estimated as UTF-8
  bytes / 3 (number-heavy JSON)."
  {:tick 800 :compact 4000 :full 12000})

(def ^:private compact-budgets
  "Per-section token budgets of the compact profile: 3,440 in all, leaving
  room for the envelope under the 4,000 cap. `full` triples them."
  {"rig" 800 "catalog" 1000 "goals" 200 "clock" 90 "run" 300 "sched" 250 "audio" 120
   "signals" 40 "look" 400 "budgets" 100 "humans" 60 "agents" 40 "feedback" 40})

(defn section-budget
  ([profile section] (section-budget profile section 1.0))
  ([profile section scale]
   (long (* scale (get compact-budgets section 40) (if (= :full profile) 3 1)))))

(def max-changes 4096)

;; Identifiers ---------------------------------------------------------------------

(defn- sha256-hex [^String text]
  (let [bytes (.digest (MessageDigest/getInstance "SHA-256") (.getBytes text StandardCharsets/UTF_8))]
    (apply str (map #(format "%02x" (bit-and 0xff %)) bytes))))

(defn- wire-name
  "A key or enumeration as text. Only strings, keywords and integers are
  names; anything else is refused rather than turned into its toString."
  [k]
  (cond (string? k) k
        (keyword? k) (subs (str k) 1)
        (integer? k) (str k)
        :else (throw (ex-info "Not an observation name" {:type (str (type k))}))))

(def ^:private id-pattern #"[A-Za-z0-9][A-Za-z0-9_.:/+-]{0,63}")

(def digest-id-pattern
  "The form of an ID the observation replaced by its digest."
  #"~[0-9a-f]{12}")

(defn- nameable? [x] (or (string? x) (keyword? x) (integer? x)))

(defn- short-digest [^String text] (str "~" (subs (sha256-hex text) 0 12)))

(defn- type-tag [x]
  (cond (keyword? x) "keyword" (string? x) "string" (integer? x) "integer" :else (.getName (class x))))

(defn wire-id
  "An ID as the observation sends it: unchanged when it is a short identifier,
  otherwise `~` and twelve hex digits of its digest. Free text in an ID never
  reaches a consumer, and the replacement is stable across frames. A value
  that is not a string, keyword or integer is sent as the digest of its type
  and printed form, never as that text."
  [x]
  (when (some? x)
    (if (nameable? x)
      (let [s (wire-name x)]
        (if (re-matches id-pattern s) s (short-digest s)))
      (short-digest (str (type-tag x) ":" (pr-str x))))))

(defn- sort-name [x] (if (nameable? x) (wire-name x) (pr-str x)))

(defn- type-rank [x] (cond (keyword? x) 0 (string? x) 1 (integer? x) 2 :else 3))

(defn wire-names
  "Wire IDs for one kind of ID (all cues, all instances, the keys of one
  parameter map) where `wire-id` alone would collide: a keyword and a string
  with the same name, say, or two long IDs whose digests agree. Within each
  colliding set the keyword, then the string, then the integer keeps the
  plain wire ID (ties by name); each other member is sent as `~` and the
  digest of its type and name. Returns only those overrides, {real-id wire},
  usually empty. The result depends only on the set of IDs."
  [ids]
  (into {}
        (mapcat (fn [[_ group]]
                  (when (next group)
                    (for [x (rest (sort-by (juxt type-rank sort-name) group))]
                      [x (short-digest (str (type-tag x) ":" (sort-name x)))]))))
        (group-by wire-id (distinct (remove nil? ids)))))

(defn- wid
  "The wire ID of `x` among the IDs of `kind` in `names` (see `id-names`)."
  [names kind x]
  (when (some? x) (or (get-in names [kind x]) (wire-id x))))

(defn- keyed
  "A sorted row map from [wire-id value] pairs. The wire IDs come from one
  kind's `wire-names`, so two rows never share one; a repeat would be a
  projection fault and is refused instead of merged."
  [pairs]
  (reduce (fn [m [k v]]
            (when (contains? m k) (throw (ex-info "Duplicate observation key" {:key k})))
            (assoc m k v))
          (sorted-map) pairs))

(defn- keyed-local
  "A sorted row map from [real-id value] pairs whose keys are one small kind
  of their own (the parameters of one definition or instance)."
  [pairs]
  (let [names {:local (wire-names (map first pairs))}]
    (keyed (map (fn [[k v]] [(wid names :local k) v]) pairs))))

;; Canonical JSON --------------------------------------------------------------

(defn- finite? [x] (and (number? x) (not (Double/isNaN (double x))) (not (Double/isInfinite (double x)))))

(defn format-number
  "A number as canonical JSON text. Every value is rounded to at most six
  decimals and written without exponent or trailing zeros, which is how
  ECMAScript's Number#toString (and so RFC 8785) writes such values:
  2.0 -> \"2\", 0.0001 -> \"0.0001\", -0.0 -> \"0\". Non-finite numbers and
  magnitudes of 1e21 or more are written as null.

  The text equals JavaScript's `String(Number(text))`, so a JavaScript
  consumer re-serialising a value gets the same bytes, for integers of
  magnitude up to 2^53 (9007199254740992) and for every other value of
  magnitude below 1e9: at most 15 significant digits, which a double always
  round-trips. Outside that range the text is still canonical here, but a
  JavaScript re-serialisation may write other digits. Projected numbers
  (beats, metres, normalised levels, frame numbers) stay inside it."
  [x]
  (cond
    (integer? x) (if (< (abs (double x)) 1e21) (str x) "null")
    (not (finite? x)) "null"
    (>= (abs (double x)) 1e21) "null"
    (and (== (double x) (Math/rint (double x))) (< (abs (double x)) 1e15)) (str (long x))
    :else (let [bd (.stripTrailingZeros (.setScale (BigDecimal. (double x)) 6 RoundingMode/HALF_EVEN))]
            (if (zero? (.signum bd)) "0" (.toPlainString bd)))))

(defn- write-string! [^StringBuilder sb ^String s]
  (.append sb \")
  (dotimes [i (.length s)]
    (let [c (.charAt s i)]
      (case c
        \" (.append sb "\\\"")
        \\ (.append sb "\\\\")
        \newline (.append sb "\\n")
        \return (.append sb "\\r")
        \tab (.append sb "\\t")
        (if (< (int c) 0x20)
          (.append sb (format "\\u%04x" (int c)))
          (.append sb c)))))
  (.append sb \"))

(defn- write-json! [^StringBuilder sb x]
  (cond
    (nil? x) (.append sb "null")
    (boolean? x) (.append sb (if x "true" "false"))
    (string? x) (write-string! sb x)
    (keyword? x) (write-string! sb (wire-name x))
    (number? x) (.append sb (format-number x))
    (map? x) (let [entries (sort-by first (map (fn [[k v]] [(wire-name k) v]) x))]
               (when-not (= (count entries) (count (distinct (map first entries))))
                 (throw (ex-info "Duplicate key in observation map" {:keys (map first entries)})))
               (.append sb \{)
               (loop [entries entries first? true]
                 (when-let [[k v] (first entries)]
                   (when-not first? (.append sb \,))
                   (write-string! sb k) (.append sb \:) (write-json! sb v)
                   (recur (next entries) false)))
               (.append sb \}))
    (sequential? x)
    (do (.append sb \[)
        (loop [items x first? true]
          (when (seq items)
            (when-not first? (.append sb \,))
            (write-json! sb (first items))
            (recur (next items) false)))
        (.append sb \]))
    :else (throw (ex-info "Not JSON observation data" {:type (str (type x))}))))

(defn canonical-json
  "JSON text with object keys sorted by UTF-16 code units at every level and
  numbers in `format-number` form, so equal values give equal bytes in any
  language. The observation envelope keeps its own section order through
  `encode`."
  [x]
  (let [sb (StringBuilder.)] (write-json! sb x) (.toString sb)))

(def ^:private envelope-order
  (into ["schema" "kind" "profile" "token" "base" "digest" "id" "blocks" "wake" "changes"] section-order))

(defn encode
  "The wire text of an envelope: fields in envelope order (slow sections
  before volatile ones, so a prompt cache keeps its prefix), canonical below."
  [envelope]
  (let [sb (StringBuilder.)
        present (filter #(contains? envelope %) envelope-order)
        extra (sort (remove (set envelope-order) (keys envelope)))]
    (.append sb \{)
    (loop [ks (concat present extra) first? true]
      (when-let [k (first ks)]
        (when-not first? (.append sb \,))
        (write-string! sb k) (.append sb \:) (write-json! sb (get envelope k))
        (recur (next ks) false)))
    (.append sb \})
    (.toString sb)))

(defn digest
  "Content digest of a JSON value: `sha256:` and the first 16 hex digits of
  the SHA-256 of its canonical JSON."
  [value]
  (str "sha256:" (subs (sha256-hex (canonical-json value)) 0 16)))

(defn utf8-size [^String text] (alength (.getBytes text StandardCharsets/UTF_8)))

(defn estimate-tokens
  "Estimated tokens for JSON text: UTF-8 bytes / 3, the research estimate for
  number-heavy JSON. Not a tokenizer measurement."
  [text]
  (long (Math/ceil (/ (utf8-size (if (string? text) text (canonical-json text))) 3.0))))

;; Quantisation ------------------------------------------------------------------

(defn- q [digits x]
  (when (finite? x)
    (if (integer? x) x
      (let [scale (Math/pow 10.0 digits)]
        (+ 0.0 (/ (Math/round (* (double x) scale)) scale))))))

(def ^:private qn (partial q 3)) ; normalised values

(def ^:private qm (partial q 2)) ; metres, colours

(def ^:private qb (partial q 3)) ; beats

(defn- qv [f v] (when (sequential? v) (mapv f v)))

(defn- quantise-value
  "A parameter value in its wire form: numbers to three decimals, colours to
  two, gradients as [position [r g b]] stops, enumerations as IDs. Anything
  else is dropped (nil), never turned into text."
  [schema-type value]
  (cond
    (nil? value) nil
    (boolean? value) value
    (and (= :color schema-type) (sequential? value)) (mapv qm value)
    (and (= :gradient schema-type) (sequential? value))
    (mapv (fn [stop] (when (map? stop) [(qn (:position stop)) (qv qm (:color stop))])) value)
    (number? value) (qn value)
    (or (keyword? value) (string? value)) (wire-id value)
    (and (sequential? value) (every? number? value)) (mapv qn value)
    (map? value) (keyed-local (keep (fn [[k v]] (when (or (keyword? k) (string? k)) [k (quantise-value nil v)])) value))
    (sequential? value) (mapv #(quantise-value nil %) value)
    :else nil))

(defn- param-values [schemas params]
  (keyed-local (keep (fn [[k v]] (when (or (keyword? k) (string? k)) [k (quantise-value (get-in schemas [k :type]) v)]))
                     params)))

;; Rig ------------------------------------------------------------------------------

(defn- channel-keys [fixture] (set (keys (:channels fixture))))

(defn fixture-kind
  "A coarse class from the patched channels: mover, wash, pixel, dimmer, fog
  or other."
  [fixture]
  (let [ch (channel-keys fixture)
        rgb? (every? ch [:red :green :blue])]
    (cond
      (and (ch :pan) (ch :tilt)) "mover"
      (or (ch :fog) (ch :haze) (:fogger fixture)) "fog"
      (and rgb? (ch :dimmer)) "wash"
      rgb? "pixel"
      (ch :dimmer) "dimmer"
      :else "other")))

(def ^:private capability-channels
  [[:red "rgb"] [:white "w"] [:amber "amber"] [:uv "uv"] [:dimmer "dim"] [:pan "pan"] [:tilt "tilt"]
   [:zoom "zoom"] [:focus "focus"] [:strobe "strobe"] [:shutter "shutter"] [:fog "fog"]])

(defn- capabilities [fixture]
  (let [ch (channel-keys fixture)]
    (into [] (keep (fn [[k cap]] (when (ch k) cap))) capability-channels)))

(defn- type-id [fixture] (wire-id (or (:fixture-type-id fixture) (fixture-kind fixture))))

(defn- position [fixture]
  (let [p (:position fixture)]
    (if (and (sequential? p) (= 3 (count p)) (every? finite? p)) (mapv double p) [0.0 0.0 0.0])))

(defn rig-names
  "Disambiguated wire IDs of the rig's fixtures and groups (`wire-names`),
  from the rig alone."
  [rig]
  (let [fixtures (:fixtures rig)]
    {:fixture (wire-names (map :id fixtures))
     :group (wire-names (mapcat :groups fixtures))}))

(defn- group-ids [names fixture]
  (mapv #(wid names :group %) (:groups fixture)))

(defn- bounds [positions]
  (if (seq positions)
    [(apply mapv min positions) (apply mapv max positions)]
    [[0.0 0.0 0.0] [0.0 0.0 0.0]]))

(defn- normaliser [[lo hi]]
  (fn [p] (mapv (fn [x a b] (if (> (- b a) 1e-9) (qm (- (* 2.0 (/ (- x a) (- b a))) 1.0)) 0.0)) p lo hi)))

(defn- median [xs]
  (when (seq xs)
    (let [v (vec (sort xs)) n (count v)]
      (if (odd? n) (v (quot n 2)) (/ (+ (v (dec (quot n 2))) (v (quot n 2))) 2.0)))))

(defn- cell-key ^long [^long cx ^long cy ^long cz]
  (+ (* (+ cx 1048576) 4398046511104) (* (+ cy 1048576) 2097152) (+ cz 1048576)))

(defn- grid
  "Uniform grid hash of points (flat x y z doubles) with cells of `size`
  metres: cell key -> int array of point indices."
  [^doubles xyz ^double size]
  (let [cells (HashMap.)
        n (quot (alength xyz) 3)]
    (dotimes [i n]
      (let [k (cell-key (long (Math/floor (/ (aget xyz (* 3 i)) size)))
                        (long (Math/floor (/ (aget xyz (+ 1 (* 3 i))) size)))
                        (long (Math/floor (/ (aget xyz (+ 2 (* 3 i))) size))))]
        (.put cells k (conj (or (.get cells k) []) i))))
    (doseq [k (vec (.keySet cells))] (.put cells k (int-array (.get cells k))))
    cells))

(def ^:private ring-offsets
  "Cell offsets at Chebyshev distance r, as flat [dx dy dz ...] longs."
  (memoize (fn [r]
             (long-array (for [dx (range (- r) (inc r)) dy (range (- r) (inc r)) dz (range (- r) (inc r))
                               :when (= r (max (abs dx) (abs dy) (abs dz)))
                               v [dx dy dz]]
                           v)))))

(defn- dist2 [^doubles xyz i x y z]
  (let [i (int i) x (double x) y (double y) z (double z)
        dx (- (aget xyz (* 3 i)) x) dy (- (aget xyz (+ 1 (* 3 i))) y) dz (- (aget xyz (+ 2 (* 3 i))) z)]
    (+ (* dx dx) (* dy dy) (* dz dz))))

(defn- flat-positions ^doubles [positions]
  (let [xyz (double-array (* 3 (count positions)))]
    (dorun (map-indexed (fn [i [x y z]] (aset xyz (* 3 i) (double x)) (aset xyz (+ 1 (* 3 i)) (double y))
                          (aset xyz (+ 2 (* 3 i)) (double z)))
                        positions))
    xyz))

(defn- ring-min
  "The smallest squared distance from point i to another point in the cells
  of ring r around cell (cx cy cz), or `best` if none is closer."
  [^HashMap cells ^doubles xyz i r best cx cy cz]
  (let [i (long i) best (double best)
        ^longs offsets (ring-offsets r)
        x (aget xyz (* 3 i)) y (aget xyz (+ 1 (* 3 i))) z (aget xyz (+ 2 (* 3 i)))
        cx (long cx) cy (long cy) cz (long cz)]
    (loop [o 0 best (double best)]
      (if (>= o (alength offsets)) best
        (let [^ints members (.get cells (cell-key (+ cx (aget offsets o)) (+ cy (aget offsets (+ o 1)))
                                                  (+ cz (aget offsets (+ o 2)))))]
          (recur (+ o 3)
                 (if (nil? members) best
                   (loop [m 0 best (double best)]
                     (if (>= m (alength members)) best
                       (let [j (aget members m)
                             dx (- (aget xyz (* 3 j)) x) dy (- (aget xyz (+ 1 (* 3 j))) y) dz (- (aget xyz (+ 2 (* 3 j))) z)]
                         (recur (inc m) (if (== j i) best (Math/min best (+ (* dx dx) (* dy dy) (* dz dz)))))))))))))))

(defn- nearest-neighbour-median
  "Median distance to the nearest other fixture, searched ring by ring in a
  uniform grid hash: about linear in the fixture count for real rigs."
  [positions]
  (let [n (count positions)]
    (when (> n 1)
      (let [xyz (flat-positions positions)
            [lo hi] (bounds positions)
            volume (reduce * (map #(max 0.5 (- %2 %1)) lo hi))
            ;; About three points per cell: few lookups, few candidates.
            size (double (max 0.05 (* 1.5 (Math/cbrt (/ volume n)))))
            cells (grid xyz size)
            span (long (Math/ceil (/ (reduce max (map - hi lo)) size)))]
        (median
         (for [i (range n)]
           (let [cx (long (Math/floor (/ (aget xyz (* 3 i)) size)))
                 cy (long (Math/floor (/ (aget xyz (+ 1 (* 3 i))) size)))
                 cz (long (Math/floor (/ (aget xyz (+ 2 (* 3 i))) size)))]
             (loop [r 0 best Double/MAX_VALUE]
               ;; After rings 0..r-1, every unseen point is at least (r-1) cells away.
               (if (or (and (> r 1) (<= best (let [d (* (dec r) size)] (* d d)))) (> r (inc span)))
                 (Math/sqrt best)
                 (recur (inc r) (ring-min cells xyz i r best cx cy cz)))))))))))

(defn- height-layers [positions]
  (count (distinct (map #(Math/round (/ (double (second %)) 0.5)) positions))))

(defn- symmetry
  "Share of fixtures with a partner of the same type mirrored across the
  rig's centre plane in X, within 0.25 m, looked up in a 0.25 m grid."
  [fixtures positions]
  (if (empty? fixtures) 0.0
    (let [xyz (flat-positions positions)
          types (mapv type-id fixtures)
          [[lo] [hi]] (bounds positions)
          centre (/ (+ lo hi) 2.0)
          size 0.25
          ^HashMap cells (grid xyz size)]
      (/ (count (filter (fn [i]
                          (let [mx (- (* 2.0 centre) (aget xyz (* 3 i))) y (aget xyz (+ 1 (* 3 i))) z (aget xyz (+ 2 (* 3 i)))
                                cx (long (Math/floor (/ mx size))) cy (long (Math/floor (/ y size))) cz (long (Math/floor (/ z size)))]
                            (some (fn [[dx dy dz]]
                                    (some #(and (= (types %) (types i)) (< (dist2 xyz % mx y z) (* size size)))
                                          (.get cells (cell-key (+ cx dx) (+ cy dy) (+ cz dz)))))
                                  (for [dx [-1 0 1] dy [-1 0 1] dz [-1 0 1]] [dx dy dz]))))
                        (range (count fixtures))))
         (double (count fixtures))))))

(defn- clamp01 [x] (max 0.0 (min 1.0 (double x))))

(defn rig-signature
  "A fixed-length rig descriptor for clustering similar rigs, version 1:
  kind shares (mover wash pixel dimmer fog), log fixture count, X/Y/Z extent
  over 20/10/20 m, fixtures per m² over 2, height layers over 8, mirror
  symmetry, capability shares (rgb dim strobe zoom). Every entry is 0..1."
  [{:keys [counts extent density symmetry caps]}]
  (let [n (max 1 (get counts "fixtures" 0))
        share #(/ (get counts % 0) (double n))
        [w h d] extent]
    {"v" rig-signature-version
     "x" (mapv (comp qn clamp01)
               [(share "mover") (share "wash") (share "pixel") (share "dimmer") (share "fog")
                (/ (Math/log10 (inc n)) 4.0) (/ w 20.0) (/ h 10.0) (/ d 20.0)
                (/ (get density "per_m2" 0.0) 2.0) (/ (get density "height_layers" 0) 8.0) symmetry
                (/ (get caps "rgb" 0) (double n)) (/ (get caps "dim" 0) (double n))
                (/ (get caps "strobe" 0) (double n)) (/ (get caps "zoom" 0) (double n))])}))

(defn- fixture-groups
  "Patched groups. A rig without any groups is grouped by fixture kind, so
  every rig has groups to summarise."
  [names fixtures]
  (let [declared? (some #(seq (:groups %)) fixtures)]
    (reduce (fn [m f]
              (let [ids (cond (seq (:groups f)) (group-ids names f)
                              declared? ["ungrouped"]
                              :else [(fixture-kind f)])]
                (reduce #(update %1 %2 (fnil conj []) f) m ids)))
            (sorted-map) fixtures)))

(defn rig-section
  "Geometry in raw metres (the scene contract's right-handed, Y-up rig space)
  and normalised to the rig's bounding box (-1..1 per axis), fixture types,
  groups, counts, density and the rig descriptor. Unpaged."
  [rig fixture-types]
  (let [fixtures (vec (:fixtures rig))
        names (rig-names rig)
        positions (mapv position fixtures)
        [lo hi :as box] (bounds positions)
        npos (normaliser box)
        extent (mapv - hi lo)
        room (:room rig)
        floor (if (and (finite? (:width room)) (finite? (:depth room)))
                (* (double (:width room)) (double (:depth room)))
                (* (max 1.0 (first extent)) (max 1.0 (nth extent 2))))
        kinds (frequencies (map fixture-kind fixtures))
        universes (count (distinct (keep :universe fixtures)))
        counts (into (sorted-map "fixtures" (count fixtures) "universes" universes) kinds)
        nn (nearest-neighbour-median positions)
        density (cond-> {"per_m2" (qm (/ (count fixtures) (max 1.0 floor)))
                         "height_layers" (height-layers positions)}
                  nn (assoc "nn_median_m" (qm nn)))
        caps (frequencies (mapcat capabilities fixtures))
        groups (fixture-groups names fixtures)
        type-row (fn [f]
                   (let [t (get fixture-types (:fixture-type-id f))
                         beam (or (:beam-angle f) (:beam-angle t))
                         watts (or (:max-watts f) (:max-watts t))]
                     (cond-> {"kind" (fixture-kind f) "caps" (capabilities f)}
                       (finite? beam) (assoc "beam_deg" (qm beam))
                       (finite? watts) (assoc "watts" (qm watts)))))]
    (cond->
     {"units" {"pos" "m" "axes" "x=width y=up z=depth" "angle" "deg"}
     "bounds" {"min" (mapv qm lo) "max" (mapv qm hi)}
     "counts" counts
     "density" density
     "sig" (rig-signature {:counts counts :extent extent :density density
                           :symmetry (symmetry fixtures positions) :caps caps})
     "types" (into (sorted-map) (map (fn [f] [(type-id f) (type-row f)])) fixtures)
     "fixtures" {"legend" ["type" "groups" "pos" "npos"]
                 "rows" (keyed (map (fn [f p] [(wid names :fixture (:id f)) [(type-id f) (group-ids names f) (mapv qm p) (npos p)]])
                                    fixtures positions))}
     "groups" {"legend" ["n" "ncentroid" "nextent"]
               "rows" (into (sorted-map)
                            (map (fn [[id members]]
                                   (let [ps (mapv (comp npos position) members)
                                         [glo ghi] (bounds ps)]
                                     [id [(count members)
                                          (mapv qm (apply mapv (fn [& xs] (/ (reduce + xs) (count xs))) ps))
                                          (mapv qm (mapv - ghi glo))]])))
                            groups)}}
      (map? room) (assoc "room" {"size" [(qm (:width room)) (qm (:height room)) (qm (:depth room))]}))))

;; Catalogue ---------------------------------------------------------------------------

(def ^:private type-names
  {:number "num" :integer "int" :boolean "bool" :enum "enum" :color "color" :gradient "grad"
   :vector "vec" :range "range"})

(def ^:private normalised-attributes
  #{:dimmer :zoom :focus :strobe :pan :tilt :shutter :iris :frost :fog :speed})

(defn- direct-attributes
  "Output attribute fed directly by each parameter: {param #{attribute}}."
  [definition]
  (reduce (fn [m {:keys [attribute value]}]
            (if (and (map? value) (= :param (:op value)) (:key value))
              (update m (:key value) (fnil conj #{}) attribute) m))
          {} (registry/output-entries (:outputs definition))))

(defn parameter-unit
  "A declared `:unit` when the schema has one; otherwise inferred: `rgb` for
  colours and gradients, `norm` for a parameter that drives a 0..1 output
  attribute directly, `m` for one that drives world-space aim. nil when no
  rule applies; never guessed past these (research decision D7)."
  [schema attributes]
  (let [declared (:unit schema)]
    (cond
      (or (keyword? declared) (string? declared)) (wire-id declared)
      (#{:color :gradient} (:type schema)) "rgb"
      (and (#{:number :integer} (:type schema)) (seq attributes) (every? normalised-attributes attributes)) "norm"
      (and (= :vector (:type schema)) (contains? attributes :aim)) "m"
      :else nil)))

(defn- parameter-row [schema attributes]
  (let [t (:type schema)]
    [(if (= :vector t) (str "vec" (:size schema)) (get type-names t (wire-id t)))
     (quantise-value t (:default schema))
     (qv qn (:bounds schema))
     (parameter-unit schema attributes)
     (qv qn (:recommended schema))]))

(defn- musical? [entry] (subscribers/musical-definition? entry))

(defn registry-names
  "Disambiguated wire IDs of every cue and effect (`wire-names`), from the
  registry alone, so a cue left out of the catalogue still keeps its name."
  [registry]
  {:cue (wire-names (keys (:cues registry)))
   :effect (wire-names (keys (:effects registry)))})

(defn catalog-section
  "Effects and cues an agent may launch or edit. Scene-owner, show-control and
  code-cue definitions are left out: the scheduler refuses them to an agent.
  Unpaged."
  [registry]
  (let [names (registry-names registry)
        effects (filter musical? (vals (:effects registry)))
        cues (filter #(and (musical? %) (musical? (registry/cue-effect registry %))) (vals (:cues registry)))]
    {"param_legend" ["type" "default" "bounds" "unit" "recommended"]
     "effects" {"legend" ["ver" "cats" "params"]
                "rows" (keyed (map (fn [e]
                                     (let [direct (direct-attributes e)]
                                       [(wid names :effect (:id e))
                                        [(:version e) (mapv wire-id (registry/definition-categories e))
                                         (keyed-local (keep (fn [[k s]] (when (map? s) [k (parameter-row s (get direct k))]))
                                                      (:params e)))]]))
                                   effects))}
     "cues" {"legend" ["effect" "prio" "params"]
             "rows" (keyed (map (fn [c]
                                  (let [effect (registry/cue-effect registry c)]
                                    [(wid names :cue (:id c))
                                     [(wid names :effect (:effect-id c)) (or (:priority c) 0) (param-values (:params effect) (:params c))]]))
                                cues))}}))

;; Clock ----------------------------------------------------------------------------------

(defn- meter-of [model]
  (let [{:keys [beats-per-bar bars-per-phrase]} (or (get-in model [:transport :meter]) {})]
    {:beats-per-bar (if (pos-int? beats-per-bar) beats-per-bar 4)
     :bars-per-phrase (if (pos-int? bars-per-phrase) bars-per-phrase 4)}))

(defn clock-section
  "Fixed size: never paged."
  [model]
  (let [{:keys [beat bpm playing? source]} (:transport model)
        {:keys [beats-per-bar bars-per-phrase]} (meter-of model)
        beat (double (or beat 0.0))
        bar (long (Math/floor (/ beat beats-per-bar)))
        phrase (long (Math/floor (/ bar bars-per-phrase)))
        phrase-beats (* beats-per-bar bars-per-phrase)]
    {"beat" (qb beat)
     "pos" {"phrase" phrase "bar" bar "beat" (qb (- beat (* bar beats-per-bar)))}
     "bpm" (qm (or bpm 120.0))
     "meter" {"beats_per_bar" beats-per-bar "bars_per_phrase" bars-per-phrase}
     "playing" (boolean playing?)
     "source" (wire-id (or source :manual))
     "next" {"bar" (double (* (inc bar) beats-per-bar)) "phrase" (double (* (inc phrase) phrase-beats))}
     "tempo_map" [{"from_beat" 0.0 "bpm" (qm (or bpm 120.0))}]}))

;; Running instances ---------------------------------------------------------------------

(defn owner-alias
  "The opaque alias of an agent session: `a` and six hex digits of the
  SHA-256 of the store's random salt and the session ID. Stable for one
  runtime's store, different in another, and not reversible to the
  conversation ID."
  [salt session-id]
  (str "a" (subs (sha256-hex (str salt "\u0000" session-id)) 0 6)))

(defn- owner
  "The adapter that launched or posted something, as an ID. A source whose
  session is the consumer's own is marked `<adapter>:self`. Another agent
  session is `agent:` and its opaque alias, so agents can tell each other's
  work apart; human surfaces are named by adapter only. No session or
  conversation ID is ever projected."
  [source {:keys [session salt]}]
  (let [adapter (or (wire-id (:adapter source)) "unknown")
        session-id (:session-id source)]
    (cond
      (and (string? session) (= session session-id)) (str adapter ":self")
      (and (= "agent" adapter) (some? session-id)) (str adapter ":" (owner-alias salt session-id))
      :else adapter)))

(defn- viewer
  "Who reads a section: the consumer session and the store's alias salt."
  [session-or-viewer]
  (if (map? session-or-viewer) session-or-viewer {:session session-or-viewer}))

(defn instance-names
  "Disambiguated wire IDs of the running instances (`wire-names`)."
  [model]
  {:instance (wire-names (keys (:instances model)))})

(defn- param-names
  "Parameter wire IDs of one running instance or definition: its schema keys
  and its current parameter keys, each set disambiguated on its own as the
  catalogue and the run table send them."
  [schemas params]
  (merge (wire-names (keys params)) (wire-names (keys schemas))))

(defn- end-beat [instance]
  (let [{:keys [duration started-at]} (:duration-timer instance)]
    (when (and (= :beats (:unit duration)) (finite? started-at) (finite? (:value duration)))
      (qb (+ (double started-at) (double (:value duration)))))))

(defn- operator-held?
  "A scene-owner, show-control or code-cue instance: it stays with the
  operator and the scheduler refuses it to an agent."
  [model instance]
  (let [cue (get-in model [:registry :cues (:cue-id instance)])]
    (not (and (musical? (:definition instance)) (or (nil? cue) (musical? cue))))))

(defn run-section
  "Running instances by ID. `reader` is the consumer session, or a map of
  :session and the store's alias :salt."
  [model reader]
  (let [reader (viewer reader)
        names (merge (registry-names (:registry model)) (instance-names model))]
    {"legend" ["cue" "effect" "ver" "phase" "prio" "opacity" "start_beat" "end_beat" "owner" "params" "operator"]
     "rows" (keyed (map (fn [[k i]]
                          [(wid names :instance k)
                           [(wid names :cue (:cue-id i)) (wid names :effect (:effect-id i)) (:effect-version i)
                            (wire-id (or (:phase i) :running)) (or (:priority i) 0)
                            (qn (get-in i [:system :opacity] 1.0)) (qb (:started-at-beat i)) (end-beat i)
                            (owner (:source i) reader)
                            (param-values (get-in i [:definition :params]) (:params i))
                            (operator-held? model i)]])
                        (:instances model)))}))

;; Schedule --------------------------------------------------------------------------------

(defn- destination [model names {:keys [target instance-id param variable]}]
  (case target
    :param (let [i (get-in model [:instances instance-id])]
             (str (wid names :instance instance-id) "/"
                  (wid {:param (param-names (get-in i [:definition :params]) (:params i))} :param param)))
    :opacity (str (wid names :instance instance-id) "/opacity")
    :variable (str "var/" (wire-id variable))
    (wire-id target)))

(defn- schedule-event-ids
  "Every event ID the schedule shows: pending, ramping and recent."
  [model view]
  (concat (map :event-id (:upcoming view)) (map :event-id (get-in model [:event-subscribers :ramps]))
          (map :event-id (:recent view))))

(defn event-names
  "Disambiguated wire IDs of the schedule's events (`wire-names`)."
  ([model] (event-names model (subscribers/schedule-view model)))
  ([model view] {:event (wire-names (schedule-event-ids model view))}))

(defn sched-section
  "Upcoming events and ramps by ID with their owners, and recent outcomes.
  Event labels are free text and are not projected. Unpaged. `reader` as for
  `run-section`."
  [model reader]
  (let [reader (viewer reader)
        {:keys [upcoming recent] :as view} (subscribers/schedule-view model)
        names (merge (event-names model view) (instance-names model))]
    {"legend" {"upcoming" ["due_beat" "id" "op" "owner"]
               "ramps" ["id" "dest" "progress" "value" "to" "end_beat" "owner"]
               "recent" ["id" "status" "reason"]}
     "upcoming" (mapv (fn [{:keys [due-beat event-id op source]}]
                        [(qb due-beat) (wid names :event event-id) (wire-id op) (owner source reader)])
                      upcoming)
     "ramps" (mapv (fn [r]
                     (let [[progress value] (subscribers/ramp-progress model r)]
                       [(wid names :event (:event-id r)) (destination model names (:destination r)) (qn progress)
                        (quantise-value nil value) (quantise-value nil (:to r)) (qb (:end r))
                        (owner (:source r) reader)]))
                   (get-in model [:event-subscribers :ramps]))
     "recent" (mapv (fn [{:keys [event-id status reason]}]
                      [(wid names :event event-id) (wire-id status) (when (keyword? reason) (wire-id reason))])
                    (take-last 8 recent))}))

;; Audio -----------------------------------------------------------------------------------

(def ^:private stale-audio-ms 2000.0)

(defn audio-section
  "The public energy signal and the analyser's features from the committed
  model. Features older than two seconds are reported unavailable. A fixed
  set of fields: never paged."
  [model]
  (let [features (get-in model [:variables :audio :features])
        age-ms (when (finite? (:time-ms features)) (- (double (:time-ms model 0)) (double (:time-ms features))))
        fresh? (and age-ms (<= age-ms stale-audio-ms))
        bpm (double (or (get-in model [:transport :bpm]) 120.0))
        energy (get-in model [:variables :audio/energy])]
    (if-not fresh?
      {"ok" false}
      (cond-> {"ok" true "age_beats" (qb (max 0.0 (/ (* age-ms bpm) 60000.0)))}
        (finite? energy) (assoc "energy" (qn energy))
        (finite? (:rms features)) (assoc "rms" (qn (:rms features)))
        (finite? (:relative-energy features)) (assoc "relative" (qn (:relative-energy features)))
        (some? (:breakdown? features)) (assoc "breakdown" (boolean (:breakdown? features)))
        (keyword? (:energy-class features)) (assoc "class" (wire-id (:energy-class features)))
        (some finite? (map features [:low-rms :mid-rms :high-rms]))
        (assoc "bands" {"low" (qn (:low-rms features)) "mid" (qn (:mid-rms features)) "high" (qn (:high-rms features))})))))

;; Look ------------------------------------------------------------------------------------

(defn- frame-access
  "Readers over the committed output frame: compact columns when the frame is
  packed, otherwise the rich value rows. Reads; never projects again."
  [output]
  (let [compact (:compact output)
        rich (:values output)
        field (fn [k] (get-in compact [:fields k]))
        color (field :emitted-color)
        ^doubles rgb (:values color) ^booleans rgb-valid (:valid color)
        scalar (fn [k]
                 (let [{:keys [^doubles values ^booleans valid components]} (field k)]
                   (if (and values (= 1 components))
                     (fn [i _] (when (aget valid (int i)) (aget values (int i))))
                     (fn [_ id] (let [v (get-in rich [id k])] (when (number? v) v))))))]
    {:order (or (:target-order compact) (:target-order output) [])
     :rgb (if rgb
            (fn [i _] (when (aget rgb-valid (int i))
                        (let [b (* 3 (int i))] [(aget rgb b) (aget rgb (inc b)) (aget rgb (+ b 2))])))
            (fn [_ id] (let [c (get-in rich [id :emitted-color])] (when (and (sequential? c) (= 3 (count c))) c))))
     :strobe (scalar :strobe)
     :zoom (scalar :zoom)
     :row (fn [i id] (if compact (first (render/project-compact compact [i])) (get rich id)))}))

(def ^:private lit-threshold (/ 1.0 255.0))

(defn- level [rgb] (if rgb (double (reduce max 0.0 rgb)) 0.0))

(defn- mean [xs] (when (seq xs) (/ (reduce + xs) (double (count xs)))))

(defn rig-index
  "Per-fixture facts the look reads every frame (group IDs, whether it is a
  mover), computed once per rig revision."
  [rig]
  (let [fixtures (vec (:fixtures rig))
        names (rig-names rig)
        declared? (some #(seq (:groups %)) fixtures)
        info (into {} (map (fn [f]
                             [(:id f) {:fixture f :wire (wid names :fixture (:id f))
                                       :groups (cond (seq (:groups f)) (group-ids names f)
                                                     declared? ["ungrouped"]
                                                     :else [(fixture-kind f)])
                                       :mover? (= "mover" (fixture-kind f))}]))
                   fixtures)]
    {:fixtures fixtures :info info}))

(defn- head-samples
  "One sample per target in patch order: level, emitted colour, strobe, zoom
  and, for heads with pan and tilt, the committed beam direction."
  [index output]
  (let [{:keys [fixtures info]} index
        {:keys [order rgb strobe zoom row]} (frame-access output)]
    (into []
          (map-indexed
           (fn [i id]
             (let [same (nth fixtures i nil)
                   {:keys [fixture mover? groups]} (if (= id (:id same)) (get info (:id same)) (get info id))
                   c (rgb i id)
                   pose (when mover? (render/fixture-pose fixture (row i id)))]
               {:id id :wire (or (:wire (get info id)) (wire-id id)) :groups (or groups ["unpatched"]) :rgb c :level (level c)
                :strobe (strobe i id) :zoom (zoom i id) :direction (:direction pose)})))
          order)))

(defn- group-row [samples]
  (let [n (count samples)
        lit (filter #(> (:level %) lit-threshold) samples)
        dirs (keep :direction lit)
        mean-dir (when (seq dirs) (apply mapv (fn [& xs] (/ (reduce + xs) (count xs))) dirs))
        resultant (when mean-dir (Math/sqrt (reduce + (map * mean-dir mean-dir))))
        colours (keep :rgb lit)]
    [n
     (qn (/ (count lit) (double (max 1 n))))
     (qn (or (mean (map :level samples)) 0.0))
     (if (seq colours) (mapv qm (apply mapv (fn [& xs] (/ (reduce + xs) (count xs))) colours)) [0.0 0.0 0.0])
     (when mean-dir (mapv qm mean-dir))
     (when resultant (qn (- 1.0 resultant)))
     (some-> (seq (keep :zoom samples)) mean qn)
     (or (some-> (seq (keep :strobe samples)) mean qn) 0.0)]))

(defn look-section
  "Look layers L0 and L1 from the committed frame. L0 meters: lit share and
  mean level of the emitted colour, plus the activity meter's components when
  that meter runs. L1: one row per group. `full` adds L5 rows per lit head."
  ([model output profile] (look-section model output profile (rig-index (:rig model))))
  ([model output profile index]
   (let [samples (head-samples index output)
         n (count samples)
         activity (or (:lighting-activity output) (get-in model [:variables :lighting/activity]))
         scene (:scene activity)
         meters (cond-> {"lit" (qn (/ (count (filter #(> (:level %) lit-threshold) samples)) (double (max 1 n))))
                         "level" (qn (or (mean (map :level samples)) 0.0))}
                  scene (into (keep (fn [[k wire]] (when (finite? (get scene k)) [wire (qn (get scene k))])))
                              [[:brightness "brightness"] [:coverage "coverage"] [:flashing "flashing"]
                               [:movement "movement"] [:strobe "strobe"] [:score "activity"]]))
         by-group (reduce (fn [m s] (reduce #(update %1 %2 (fnil conj []) s) m (:groups s)))
                          (sorted-map) samples)]
     (cond-> {"v" look-version
              "frame" (get model :frame-number 0)
              "meters" meters
              "groups" {"legend" ["n" "lit" "level" "rgb" "aim" "spread" "zoom" "strobe"]
                        "rows" (into (sorted-map) (map (fn [[id ss]] [id (group-row ss)])) by-group)}}
       (= :full profile)
       (assoc "fixtures" {"legend" ["level" "rgb" "strobe"]
                          "rows" (keyed (keep (fn [{:keys [wire level rgb strobe]}]
                                                (when (> level lit-threshold)
                                                  [wire [(qn level) (mapv qm rgb) (some-> strobe qn)]]))
                                              samples))})))))

;; Budgets and humans -------------------------------------------------------------------------

(defn budgets-section
  "A fixed set of fields: never paged."
  [model output frame-rate]
  (let [schedule (get model :event-subscribers)
        budget (get-in output [:lighting-budget :global])]
    (cond-> {"frame" {"deadline_ms" (qm (/ 1000.0 (double (or frame-rate 40))))
                      "instances" (count (:instances model))}
             "sched" {"events" [(count (:runs schedule)) 256] "ramps" [(count (:ramps schedule)) 256] "batch" 64}}
      (map? budget)
      (assoc "light" (cond-> {}
                       (finite? (:limit-watts budget)) (assoc "limit_w" (qm (:limit-watts budget)))
                       (finite? (:used-watts budget)) (assoc "use_w" (qm (:used-watts budget)))
                       (integer? (:max-lit budget)) (assoc "max_lit" (:max-lit budget))
                       (integer? (:lit budget)) (assoc "lit" (:lit budget)))))))

(defn humans-section [model]
  {"master" (qn (clamp01 (or (:master model) 1.0)))
   "blackout" (boolean (:blackout? model))})

;; Paging -----------------------------------------------------------------------------------------

(defn- table-paths
  "What pages when a section exceeds its budget, largest first. A path to a
  {legend, rows} table keeps its first rows in ID order; a path to an array
  keeps its first items (the schedule is in due order)."
  [section]
  (case section
    "rig" [["fixtures"]]
    "catalog" [["cues"] ["effects"]]
    "run" [[]]
    "sched" [["upcoming"] ["ramps"]]
    "look" [["fixtures"] ["groups"]]
    []))

(defn- page-at
  "`value` with the table at `path` cut to its first `keep-n` rows and marked
  `more` with its total. Unmarked (`mark?` false), only the rows are cut: the
  bytes the unpaged value has besides its rows."
  ([value path keep-n] (page-at value path keep-n true))
  ([value path keep-n mark?]
   (let [target (get-in value path)]
     (if (vector? target)
       (let [k (last path) parent (butlast path) total (count target)]
         (if (and mark? (>= keep-n total)) value
           (let [paged (cond-> (assoc (get-in value parent) k (subvec target 0 (min keep-n total)))
                         mark? (assoc "more" true (str k "_total") total))]
             (if (seq parent) (assoc-in value parent paged) paged))))
       (let [rows (get target "rows") total (count rows)]
         (if (and mark? (>= keep-n total)) value
           (let [paged (cond-> (assoc target "rows" (into (sorted-map) (take keep-n rows)))
                         mark? (assoc "more" true "total" total))]
             (if (seq path) (assoc-in value path paged) paged))))))))

(defn- page-size [value path]
  (let [target (get-in value path)]
    (if (vector? target) (count target) (count (get target "rows")))))

(defn- item-bytes
  "UTF-8 bytes of each row (`\"id\":row`) or array item as canonical JSON, in
  page order, each encoded once."
  [target]
  (if (vector? target)
    (mapv #(utf8-size (canonical-json %)) target)
    (mapv (fn [[k v]]
            (let [sb (StringBuilder.)]
              (write-string! sb (wire-name k)) (.append sb \:) (write-json! sb v)
              (utf8-size (.toString sb))))
          (get target "rows"))))

(defn- largest-page
  "nil when `value` is within `limit` bytes with every row of the table at
  `path`; otherwise the most leading rows that keep it within, found in one
  pass. A table of k rows encodes to its value without rows (marked `more`
  when paged) plus the first k rows' bytes and k-1 commas, exactly, because
  canonical JSON is context free and a page keeps its rows' order."
  [value path limit]
  (let [items (item-bytes (get-in value path))
        commas (max 0 (dec (count items)))
        whole (+ (utf8-size (canonical-json (page-at value path 0 false))) (reduce + 0 items) commas)
        empty-bytes (utf8-size (canonical-json (page-at value path 0)))]
    (when (> whole limit)
      (loop [k 0 bytes empty-bytes [b & more] items]
        (let [next-bytes (when b (+ bytes (long b) (if (pos? k) 1 0)))]
          (if (and next-bytes (<= next-bytes limit)) (recur (inc k) next-bytes more) k))))))

(defn- fit-section
  "Page a section to its budget. A paged table keeps its first rows in ID
  order and carries `more` and `total` (`<array>_total` for an array);
  nothing is cut without saying so. `inspect` reads the remaining rows. Each
  row of a pageable table is encoded once; whether it fits and the page
  size are found by summing. A section without pageable tables is a fixed
  set of fields and is returned as it is."
  [profile section scale value]
  (let [limit (* 3 (section-budget profile section scale))]
    (if (nil? value) value
      (reduce (fn [v path]
                (if (zero? (page-size v path)) v
                  (if-let [k (largest-page v path limit)]
                    (page-at v path k)
                    (reduced v))))
              value (table-paths section)))))

;; Document ----------------------------------------------------------------------------------------

(defn- identity-of [{:keys [runtime-id state]}]
  (let [model (:model state)]
    {"runtime" (or (wire-id runtime-id) "runtime")
     "show" (or (wire-id (:show-id model)) "show")
     "epoch" (or (:epoch model) 0)
     "rev" (or (:revision model) 0)
     "frame" (get model :frame-number 0)
     "gen" (long (or (:transport-generation state) 0))}))

(defn- memo
  "Memoise `compute` in `cache` under `slot`, by the identity of `inputs`."
  [cache slot inputs compute]
  (let [cached (get @cache slot)]
    (if (and cached (= (count inputs) (count (:inputs cached))) (every? true? (map identical? inputs (:inputs cached))))
      (:value cached)
      (let [value (compute)]
        (swap! cache assoc slot {:inputs inputs :value value})
        value))))

(defn- unpaged-slow
  "A slow section before paging, computed once per input identity for every
  profile and for `inspect`."
  [cache section model]
  (case section
    "rig" (memo cache [:unpaged "rig"] [(:rig model) (get-in model [:registry :fixtures])]
                #(rig-section (:rig model) (get-in model [:registry :fixtures])))
    "catalog" (memo cache [:unpaged "catalog"] [(:registry model)] #(catalog-section (:registry model)))
    "goals" []))

(defn- slow-block [cache section model profile scale]
  (let [unpaged (unpaged-slow cache section model)]
    (memo cache [:block section profile scale] [unpaged]
          #(let [value (fit-section profile section scale unpaged)] {:value value :digest (digest value)}))))

(defn document
  "Project one committed state into the logical observation: identity, block
  digests and every section for `profile`. `input` is {:runtime-id
  :frame-rate :state}, where :state is the committed state reference. `cache`
  (an atom) memoises slow sections by input identity; pass (atom {}) for a
  one-off projection. Options: :session (the consumer, for `self` owners),
  :salt (the store's owner alias salt) and :scale (section budgets are
  multiplied by it; default 1)."
  ([cache input profile] (document cache input profile {}))
  ([cache input profile {:keys [session salt scale] :or {scale 1.0}}]
   (when-not (profiles profile) (throw (ex-info "Unknown observation profile" {:profile profile})))
   (let [{:keys [state frame-rate]} input
         model (:model state)
         output (:output state)
         slow (into {} (map (fn [s] [s (slow-block cache s model profile scale)])) slow-sections)
         index (memo cache [:rig-index] [(:rig model)] #(rig-index (:rig model)))
         fit (fn [section value] (fit-section profile section scale value))
         dynamic {"clock" (clock-section model)
                  "run" (fit "run" (run-section model {:session session :salt salt}))
                  "sched" (fit "sched" (sched-section model {:session session :salt salt}))
                  "audio" (audio-section model)
                  "signals" {}
                  "look" (fit "look" (look-section model output profile index))
                  "budgets" (budgets-section model output frame-rate)
                  "humans" (humans-section model)
                  "agents" {}
                  "feedback" []}
         blocks (into (sorted-map) (map (fn [[s b]] [s (:digest b)])) slow)
         id (identity-of input)
         content (digest {"blocks" blocks "sections" dynamic})]
     {:profile profile :id id :blocks blocks
      :slow (into {} (map (fn [[s b]] [s (:value b)])) slow)
      :sections dynamic
      :digest content
      :token (str "o-" (subs (digest {"id" id "content" content "profile" (name profile)}) 7))})))

(defn- bar-of [doc] (get-in doc [:sections "clock" "pos" "bar"]))

(defn- phrase-of [doc] (get-in doc [:sections "clock" "pos" "phrase"]))

(def wake-reasons
  "Every wake reason the projection emits. Snapshots: start, resync, epoch,
  overflow. Deltas: the rest."
  ["start" "resync" "epoch" "overflow"
   "grid:phrase" "grid:bar" "block:rig" "block:catalog" "block:goals" "run" "sched" "human"])

(defn- delta-wake [base doc]
  (cond-> []
    (not= (phrase-of base) (phrase-of doc)) (conj "grid:phrase")
    (not= (bar-of base) (bar-of doc)) (conj "grid:bar")
    (not= (get-in base [:blocks "rig"]) (get-in doc [:blocks "rig"])) (conj "block:rig")
    (not= (get-in base [:blocks "catalog"]) (get-in doc [:blocks "catalog"])) (conj "block:catalog")
    (not= (get-in base [:blocks "goals"]) (get-in doc [:blocks "goals"])) (conj "block:goals")
    (not= (set (keys (get-in base [:sections "run" "rows"]))) (set (keys (get-in doc [:sections "run" "rows"])))) (conj "run")
    (not= (get-in base [:sections "sched"]) (get-in doc [:sections "sched"])) (conj "sched")
    (not= (get-in base [:sections "humans"]) (get-in doc [:sections "humans"])) (conj "human")))

(defn- same-show? [a b]
  (= (select-keys (:id a) ["runtime" "show" "epoch"]) (select-keys (:id b) ["runtime" "show" "epoch"])))

(defn- atomic?
  "Numeric vectors (colours, positions, bounds) and scalars change as a whole."
  [v]
  (not (or (map? v) (and (vector? v) (some coll? v)))))

(defn- diff!
  [path old new emit!]
  (when-not (= old new)
    (cond
      (and (map? old) (map? new))
      (do (doseq [k (sort (remove #(contains? new %) (keys old)))] (emit! ["remove" (conj path k)]))
          (doseq [k (sort (keys new))]
            (if (contains? old k)
              (diff! (conj path k) (get old k) (get new k) emit!)
              (emit! ["set" (conj path k) (get new k)]))))
      (and (vector? old) (vector? new) (= (count old) (count new)) (not (atomic? old)) (not (atomic? new)))
      (dotimes [i (count new)] (diff! (conj path i) (nth old i) (nth new i) emit!))
      :else (emit! ["set" path new]))))

(defn changes
  "Set/remove operations taking `base`'s sections to `doc`'s, or nil when they
  exceed the operation bound. Map entries and the cells of table rows are
  addressed one by one; a numeric vector, a row or array whose length changed
  and a changed slow block are replaced whole. `tick` never inlines a slow
  block: its new digest in `blocks` is the notice."
  [base doc]
  (let [ops (volatile! [])
        emit! (fn [op]
                (when (>= (count @ops) max-changes) (throw (ex-info "Too many changes" {::overflow true})))
                (vswap! ops conj op))]
    (try
      (doseq [s slow-sections
              :when (and (not= :tick (:profile doc))
                         (not= (get-in base [:blocks s]) (get-in doc [:blocks s])))]
        (emit! ["set" [s] (get-in doc [:slow s])]))
      (diff! [] (:sections base) (:sections doc) emit!)
      @ops
      (catch clojure.lang.ExceptionInfo e
        (if (::overflow (ex-data e)) nil (throw e))))))

(defn snapshot-envelope
  "A complete observation. Slow sections whose digest the consumer already
  holds (`have`) are sent as the digest alone. The `tick` profile never
  inlines slow sections."
  [doc wake have]
  (let [have (set have)]
    (merge {"schema" schema "kind" "snapshot" "profile" (name (:profile doc))
            "token" (:token doc) "base" nil "digest" (:digest doc)
            "id" (:id doc) "blocks" (:blocks doc) "wake" wake}
           (when-not (= :tick (:profile doc))
             (into {} (keep (fn [s] (when-not (contains? have (get-in doc [:blocks s])) [s (get-in doc [:slow s])])))
                   slow-sections))
           (:sections doc))))

(defn delta-envelope [base doc operations]
  {"schema" schema "kind" "delta" "profile" (name (:profile doc))
   "token" (:token doc) "base" (:token base) "digest" (:digest doc)
   "id" (:id doc) "blocks" (:blocks doc) "wake" (delta-wake base doc)
   "changes" operations})

;; Applying a delta (consumers and tests) ----------------------------------------------------------

(defn- set-in [value [k & more] v]
  (if (nil? k) v
    (let [child (get value k)
          next (set-in child more v)]
      (if (and (vector? value) (integer? k)) (assoc value k next) (assoc (or value {}) k next)))))

(defn- remove-in [value path]
  (let [parent (butlast path) k (last path)]
    (if (empty? parent)
      (if (vector? value) (into (subvec value 0 k) (subvec value (inc k))) (dissoc value k))
      (set-in value parent (let [p (get-in value parent)]
                             (if (vector? p) (into (subvec p 0 k) (subvec p (inc k))) (dissoc p k)))))))

(defn apply-delta
  "Apply a delta envelope's changes to the sections of an earlier snapshot or
  reconstruction (a map of section name to value). Returns the new sections."
  [sections envelope]
  (reduce (fn [acc [op path value]]
            (case op
              "set" (set-in acc path value)
              "remove" (remove-in acc path)))
          sections (get envelope "changes")))

(defn sections-of
  "The section map of a snapshot envelope (slow sections included when sent)."
  [envelope]
  (select-keys envelope section-order))

(defn content-digest
  "Recompute an observation digest from a full section map, so a consumer can
  check a reconstruction against the envelope's `digest`."
  [sections]
  (digest {"blocks" (into (sorted-map) (map (fn [s] [s (digest (get sections s))])) slow-sections)
           "sections" (select-keys sections dynamic-sections)}))

;; Session store ------------------------------------------------------------------------------------

(defn create-store
  "An inert, bounded store owned by the runtime and used only on reader
  threads. Each consumer session keeps its last `per-session` observations
  per profile; a base is refreshed when it is used, so a slow consumer keeps
  receiving deltas however often others read. At most `max-sessions`
  sessions are kept, least recently used first out. There is no consumer
  queue: a delta always describes the newest committed state. A reader that
  names no session gets snapshots and nothing is kept for it.

  `:salt` keys the opaque owner aliases of agent sessions (`owner-alias`);
  by default it is random, so aliases differ between runtimes."
  ([] (create-store {}))
  ([{:keys [max-sessions per-session salt] :or {max-sessions 64 per-session 4}}]
   {:sessions (atom {:tick 0 :entries {}}) :max-sessions max-sessions :per-session per-session
    :cache (atom {}) :salt (or salt (str (UUID/randomUUID)))}))

(defn- remember
  "Retain `doc` (and refresh `base`) in the session's ring for its profile,
  then evict the least recently used sessions over the cap."
  [state session profile doc base-token per-session max-sessions]
  (let [tick (inc (:tick state))
        k [session profile]
        {:keys [docs order]} (get-in state [:entries k] {:docs {} :order []})
        order (cond-> (vec (remove (hash-set (:token doc) base-token) order))
                (and base-token (contains? docs base-token)) (conj base-token)
                true (conj (:token doc)))
        docs (assoc docs (:token doc) doc)
        dropped (take (max 0 (- (count order) per-session)) order)
        entry {:docs (apply dissoc docs dropped) :order (vec (drop (count dropped) order)) :used tick}
        entries (assoc (:entries state) k entry)
        last-use (reduce (fn [m [[s _] e]] (update m s (fnil max 0) (:used e))) {} entries)
        evict (set (take (max 0 (- (count last-use) max-sessions)) (map key (sort-by val last-use))))]
    {:tick tick :entries (into {} (remove (fn [[[s _] _]] (contains? evict s))) entries)}))

(defn- parse-profile [profile]
  (let [p (cond (keyword? profile) profile
                (string? profile) (keyword profile)
                (nil? profile) :compact)]
    (when-not (profiles p) (throw (ex-info "Unknown observation profile" {:profile profile :profiles (sort profiles)})))
    p))

(defn- parse-session [session]
  (cond (nil? session) nil
        (and (string? session) (re-matches id-pattern session)) session
        :else (throw (ex-info "A session is a short identifier" {}))))

(defn observe
  "Project `input` (see `document`) and return the envelope for a consumer
  session whose acknowledged base is `base`: a delta when that session still
  holds the base for the same profile, show and epoch, otherwise a snapshot.
  Every envelope is held under its profile's token cap: a delta over the cap
  is replaced by a snapshot (wake `overflow`), and a snapshot over the cap is
  paged harder, down to every pageable table cut to nothing; past that the
  observation is refused rather than sent over its cap. Options: :session
  (a short identifier; without one the reader gets snapshots, nothing is
  kept for it and a :base is refused), :profile (:tick, :compact or :full;
  default :compact), :base (token), :have (block digests the consumer
  holds)."
  [store input {:keys [profile base have session]}]
  (let [profile (parse-profile profile)
        session (parse-session session)
        _ (when (and (some? base) (nil? session))
            (throw (ex-info "Name a session to continue from a base" {:base base})))
        cap (caps profile)
        prior (when (string? base) (get-in @(:sessions store) [:entries [session profile] :docs base]))
        have (when (sequential? have) have)
        fits? #(<= (estimate-tokens (encode %)) cap)]
    (loop [scale 1.0]
      (let [doc (document (:cache store) input profile {:session session :salt (:salt store) :scale scale})
            operations (when (and prior (same-show? prior doc)) (changes prior doc))
            delta (when operations (delta-envelope prior doc operations))
            envelope (if (and delta (fits? delta)) delta
                       (snapshot-envelope doc [(cond (nil? base) "start"
                                                     (nil? prior) "resync"
                                                     (not (same-show? prior doc)) "epoch"
                                                     :else "overflow")]
                                          have))]
        (cond
          (or (identical? envelope delta) (fits? envelope))
          (do (when session
                (swap! (:sessions store) remember session profile doc (when prior base)
                       (:per-session store) (:max-sessions store)))
              envelope)
          ;; Below a fifth of the budgets, every pageable table is cut to
          ;; nothing; the fixed sections alone are far under every cap.
          (pos? scale) (recur (if (< scale 0.2) 0.0 (* scale 0.7)))
          :else (throw (ex-info "Observation exceeds its profile cap with every table paged"
                                {:profile profile :cap cap :tokens (estimate-tokens (encode envelope))})))))))

(defn inspect
  "Rows `offset`..`offset+limit` of one table or schedule array, read from the
  unpaged projection of `input`: how a consumer reads past a page marked
  `more`. `path` is a section name and table keys, e.g. [\"rig\" \"fixtures\"].
  The page carries the token, digest and block digests of the `profile`
  observation of the same committed state, so a consumer can tell whether a
  block changed since the snapshot it continues."
  [store input path {:keys [offset limit profile session] :or {offset 0 limit 64}}]
  (let [[section & table-path] path
        profile (parse-profile profile)
        session (parse-session session)
        model (get-in input [:state :model])
        output (get-in input [:state :output])
        cache (:cache store)
        value (case section
                ("rig" "catalog") (unpaged-slow cache section model)
                "run" (run-section model {:session session :salt (:salt store)})
                "sched" (sched-section model {:session session :salt (:salt store)})
                "look" (look-section model output :full (memo cache [:rig-index] [(:rig model)] #(rig-index (:rig model))))
                (throw (ex-info "Section has no pageable table" {:section section})))
        table (if (seq table-path) (get-in value table-path) value)
        rows (if (vector? table) table (get table "rows"))]
    (when-not (or (map? rows) (and (vector? table) (= "sched" section)))
      (throw (ex-info "No such table" {:path path})))
    (when-not (and (nat-int? offset) (pos-int? limit) (<= limit 256))
      (throw (ex-info "Inspect needs a nonnegative offset and a limit of 1 to 256" {:offset offset :limit limit})))
    (let [doc (document cache input profile {:session session :salt (:salt store)})]
      {"schema" schema "kind" "page" "path" (vec path) "id" (:id doc)
       "token" (:token doc) "digest" (:digest doc) "blocks" (:blocks doc)
       "legend" (if (vector? table) (get-in value ["legend" (last path)]) (get table "legend"))
       "offset" offset "total" (count rows)
       "rows" (if (vector? rows) (vec (take limit (drop offset rows))) (into (sorted-map) (take limit (drop offset rows))))})))

;; Resolving observed IDs ---------------------------------------------------------------------------

(defn id-names
  "Every kind's disambiguated wire IDs in one committed model: {kind {real
  wire}} for :fixture, :group, :cue, :effect, :instance and :event, holding
  only the IDs `wire-id` alone would send ambiguously. The projection names
  each ID this way and `resolve-id` inverts it."
  [model]
  (merge (rig-names (:rig model)) (registry-names (:registry model)) (instance-names model) (event-names model)))

(defn- param-scopes
  "[schemas params] pairs whose keys a parameter name may mean: the named
  instance's, else the named cue's, else the named effect's, else every
  effect's and every running instance's."
  [model {:keys [instance-id cue-id effect-id]}]
  (let [registry (:registry model)
        instance (get-in model [:instances instance-id])
        cue (get-in registry [:cues cue-id])
        effect (get-in registry [:effects (or effect-id (:effect-id cue))])]
    (cond
      instance [[(get-in instance [:definition :params]) (:params instance)]]
      cue [[(:params effect) (:params cue)]]
      effect [[(:params effect) nil]]
      :else (concat (map (fn [e] [(:params e) nil]) (vals (:effects registry)))
                    (map (fn [i] [(get-in i [:definition :params]) (:params i)]) (vals (:instances model)))))))

(defn- wire-forms
  "[real wire] for every ID of `kind` in `model`. A :selector (an event's
  `select.ids`) may name a running instance, a cue or an effect, in that
  order of preference."
  [model kind context]
  (case kind
    :selector (mapcat #(wire-forms model % context) [:instance :cue :effect])
    :param
    (distinct (mapcat (fn [[schemas params]]
                        (let [names {:param (param-names schemas params)}]
                          (map (fn [k] [k (wid names :param k)]) (distinct (concat (keys schemas) (keys params))))))
                      (param-scopes model context)))
    (let [fixtures (get-in model [:rig :fixtures])
          ids (case kind
                :cue (keys (get-in model [:registry :cues]))
                :effect (keys (get-in model [:registry :effects]))
                :instance (keys (:instances model))
                :fixture (map :id fixtures)
                :group (mapcat :groups fixtures)
                :event (schedule-event-ids model (subscribers/schedule-view model)))
          names {kind (wire-names ids)}]
      (map (fn [k] [k (wid names kind k)]) (sort-by type-rank (distinct (remove nil? ids)))))))

(defn observed-id?
  "Whether `value` has the form of an ID the observation replaced by its
  digest (`~` and twelve hex digits), as a string or a keyword."
  [value]
  (boolean (and (or (string? value) (keyword? value)) (re-matches digest-id-pattern (wire-name value)))))

(def ^:private must-exist
  "Kinds whose plain IDs must name something in the committed show when the
  command arrives: an event's `select.ids`, which the owner otherwise checks
  only when the event is due. Plain cue, effect, instance, event, parameter,
  fixture and group IDs keep the command's own validation and its messages."
  #{:selector})

(defn resolve-id
  "The real ID that the wire ID `value` names among the IDs of `kind` (:cue,
  :effect, :instance, :selector, :fixture, :group, :event or :param) in the
  committed `model`, inverting exactly the naming the projection used.
  `context` names the :instance-id, :cue-id or :effect-id a :param belongs
  to.

  A digest names the one ID whose wire form it is. A plain ID names the ID
  whose wire form it is, else the ID whose own name it is (the show's ID as
  `inspect_show` gives it), keyword first: so the text `\"wash\"` names the
  cue `:wash`. Returns {:id real}; {:reason :unknown-id} for a digest that
  names nothing (it ended, was removed or renamed, or was never shown) and
  for a plain selector ID that names nothing;
  {:reason :ambiguous-id} for a digest two IDs share. Any other plain value
  is returned unchanged for the command's own validation."
  ([model kind value] (resolve-id model kind value {}))
  ([model kind value context]
   (let [text (when (or (string? value) (keyword? value) (integer? value)) (wire-name value))
         forms (when text (wire-forms model kind context))
         by-wire (distinct (keep (fn [[real wire]] (when (= wire text) real)) forms))
         by-name (keep (fn [[real]] (when (= text (sort-name real)) real)) forms)]
     (cond
       (nil? text) {:id value}
       (= 1 (count by-wire)) {:id (first by-wire)}
       (and (seq by-wire) (observed-id? value)) {:reason :ambiguous-id}
       (seq by-wire) {:id (first by-wire)}
       ;; The show's own name for an ID (also a real ID that looks like a digest).
       (seq by-name) {:id (first by-name)}
       (or (observed-id? value) (must-exist kind)) {:reason :unknown-id}
       :else {:id value}))))

(def ^:private id-fields
  "Command fields that carry an ID, by name, and the kind of ID."
  {"cue-id" :cue "cue-ids" :cue "effect-id" :effect "effect-ids" :effect "instance-id" :instance
   "ids" :selector "fixture-ids" :fixture "fixture-order" :fixture "group-id" :group "event-ids" :event "param" :param})

(def ^:private context-fields ["instance-id" "cue-id" "effect-id"])

(defn- field-name [k] (cond (keyword? k) (name k) (string? k) k :else nil))

(defn- observed-digests
  "Every digest-form ID an observation document showed."
  [doc]
  (let [seen (volatile! #{})]
    (letfn [(walk [x]
              (cond (map? x) (doseq [[k v] x] (walk k) (walk v))
                    (sequential? x) (run! walk x)
                    (and (string? x) (re-matches digest-id-pattern x)) (vswap! seen conj x)))]
      (walk (:sections doc)) (walk (:slow doc)))
    @seen))

(defn- kind-label [kind]
  (case kind :selector "running instance, cue or effect" :instance "running instance" (name kind)))

(defn- id-error [path kind value reason basis-seen]
  (let [text (wire-name value)
        reason (if (and (= :unknown-id reason) (contains? basis-seen text)) :stale-id reason)]
    {:path path :kind kind :id text :reason reason
     :message (case reason
                :stale-id (str text " was in the basis observation but names no " (kind-label kind)
                               " in the show now: it ended, was removed or was renamed")
                :unknown-id (str text " names no " (kind-label kind) " in the committed show: it ended, was removed or renamed,"
                                 " or no observation showed it")
                :ambiguous-id (str text " names more than one " (kind-label kind) "; inspect the show for the full ID"))}))

(defn- resolve-in
  "Resolve every ID field of the map or vector `data`, depth first, with
  context fields of a map resolved before its parameter. Returns [data errors]."
  [model data path fields seen]
  (cond
    (map? data)
    (let [ordered (sort-by (fn [[k]] (let [n (field-name k)]
                                       (cond (some #{n} context-fields) 0 (= "param" n) 1 :else 2)))
                           data)]
      (reduce (fn [[m errors] [k v]]
                (let [kind (get fields (field-name k))
                      p (conj path k)
                      context (into {} (keep (fn [f] (when-let [entry (first (filter #(= f (field-name (key %))) m))]
                                                       [(keyword f) (val entry)])))
                                    context-fields)
                      one (fn [x p]
                            (let [{:keys [id reason]} (resolve-id model kind x context)]
                              (if reason [x [(id-error p kind x reason seen)]] [id []])))]
                  (cond
                    (and kind (vector? v))
                    (let [results (map-indexed (fn [i x] (one x (conj p i))) v)]
                      [(assoc m k (mapv first results)) (into errors (mapcat second results))])
                    (and kind (or (string? v) (keyword? v)))
                    (let [[x errs] (one v p)] [(assoc m k x) (into errors errs)])
                    (coll? v)
                    (let [[x errs] (resolve-in model v p fields seen)] [(assoc m k x) (into errors errs)])
                    :else [m errors])))
              [data []] ordered))
    (sequential? data)
    (let [results (map-indexed (fn [i x] (resolve-in model x (conj path i) fields seen)) data)]
      [(mapv first results) (vec (mapcat second results))])
    :else [data []]))

(defn- held-doc
  "The observation `token` a session holds in `store`, in any profile."
  [store session token]
  (when (and store (string? session) (string? token))
    (some (fn [[[s _] entry]] (when (= s session) (get-in entry [:docs token])))
          (:entries @(:sessions store)))))

(defn resolve-ids
  "Resolve the observed IDs in command data at the agent action boundary:
  each field that names a cue, effect, running instance, fixture, group,
  scheduled event or parameter (`cue-id`, `cue-ids`, `effect-id`,
  `effect-ids`, `instance-id`, `fixture-ids`, `fixture-order`, `group-id`,
  `event-ids`, `param`, at any depth, keys as keywords or strings) is mapped
  back from its wire form to the real ID in the committed `model`. A
  parameter resolves among the parameters of the instance, cue or effect
  named beside it.

  Returns {:value resolved-data :errors []}. Each error is {:path :kind :id
  :reason :message}, with reason :unknown-id, :stale-id (the basis
  observation showed it, the show no longer has it) or :ambiguous-id; the
  whole command should be refused when there are any. Options:
  :fields extra {field-name kind} entries, and :store, :session and :basis
  (an observation token) to tell a stale ID from one never observed. Pure:
  it reads `model` and the store and changes neither."
  ([model data] (resolve-ids model data {}))
  ([model data {:keys [fields store session basis]}]
   (let [seen (or (some-> (held-doc store session basis) observed-digests) #{})
         [value errors] (resolve-in model data [] (merge id-fields fields) seen)]
     {:value value :errors errors})))

(defn- part [decision k] (if (contains? decision k) [k (get decision k)] (let [s (name k)] (when (contains? decision s) [s (get decision s)]))))

(defn resolve-decision
  "The ID resolution step of lowering one `tolglow-agent-act/1` decision
  (#853): resolve observed IDs in `post` events, `cancel` selectors (`ids`
  are event IDs, `destination` names an instance and parameter) and `move`
  (`id` is a pending event), keys as keywords or strings, against the
  committed `model`. When :store and :session are given and the decision
  names a basis (`basis.obs`), the basis must be an observation that session
  still holds, for the same show and epoch, or the decision is refused with
  :stale-basis at [basis obs]. Scopes, quotas and the rest of lowering
  belong to the caller. Returns {:value decision :errors [...]} as
  `resolve-ids`."
  [model decision {:keys [store session] :as options}]
  (let [[basis-key basis] (part decision :basis)
        [obs-key token] (when (map? basis) (part basis :obs))
        doc (held-doc store session token)
        stale (when (and store session token)
                (cond (nil? doc) "names no observation this session holds"
                      (not= [(get-in doc [:id "show"]) (get-in doc [:id "epoch"])]
                            [(or (wire-id (:show-id model)) "show") (or (:epoch model) 0)])
                      "belongs to an earlier show or epoch"))
        options (assoc options :basis token)
        step (fn [[value errors] k fields]
               (if-let [[pk pv] (part value k)]
                 (let [{v :value e :errors} (resolve-ids model pv (assoc options :fields fields))]
                   [(assoc value pk v) (into errors (map #(update % :path (fn [p] (into [pk] p))) e))])
                 [value errors]))
        [value errors] (-> [decision []]
                           (step :post {})
                           (step :cancel {"ids" :event})
                           (step :move {"id" :event}))]
    {:value value
     :errors (cond-> errors
               stale (conj {:path [basis-key obs-key] :reason :stale-basis :id token
                            :message (str "The decision's basis " token " " stale)}))}))
