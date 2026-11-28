(ns tolglow.agent-goal-text
  "Text rules for operator goals (#857), shared by the show owner and Studio
  so both see, count and refuse the same text. The concern is a hidden
  channel: anything a model could read that the operator cannot see on the
  card. Lookalike letters are visible to both, so they are kept as typed
  (NFC, not NFKC).

  - Line endings become LF, then text is NFC-normalised.
  - Refused: control characters other than newline and tab; every format
    character (general category Cf: bidirectional controls, zero-width
    characters, tags, interlinear annotation, the Arabic letter mark…);
    variation selectors U+E0100–E01EF; the combining grapheme joiner;
    U+180E; unpaired surrogates.
  - Allowed in context: ZWJ and ZWNJ between two visible characters (emoji
    sequences, Indic and Arabic-script joining); one variation selector
    U+FE00–FE0F directly after a visible base.
  - Text that is blank once format characters and blank fillers (Hangul
    fillers, blank braille) are removed counts as missing.

  These rules apply strictly when a goal is written. A saved show is never
  refused for them: on load, a goal that breaks rules tightened since it was
  saved is dropped and reported (tolglow.agent-goals/restore)."
  (:require [clojure.string :as str]))

(defn normalize
  "LF line endings, then NFC."
  [s]
  (let [s (str/replace s #"\r\n?" "\n")]
    #?(:clj (java.text.Normalizer/normalize ^String s java.text.Normalizer$Form/NFC)
       :cljs (.normalize s "NFC"))))

(defn- code-units [s] #?(:clj (map int s) :cljs (map #(.charCodeAt s %) (range (count s)))))

(defn code-points
  "Code points of s; an unpaired surrogate is returned as itself."
  [s]
  (loop [units (vec (code-units s)) i 0 out (transient [])]
    (if (>= i (count units))
      (persistent! out)
      (let [u (nth units i) next-unit (get units (inc i))]
        (if (and (<= 0xD800 u 0xDBFF) next-unit (<= 0xDC00 next-unit 0xDFFF))
          (recur units (+ i 2) (conj! out (+ 0x10000 (* (- u 0xD800) 0x400) (- next-unit 0xDC00))))
          (recur units (inc i) (conj! out u)))))))

(defn length "Unicode characters in s." [s] (count (code-points s)))

#?(:cljs (def ^:private cf-pattern (js/RegExp. "^\\p{Cf}$" "u")))

(defn format-character?
  "General category Cf."
  [c]
  (and (not (<= 0xD800 c 0xDFFF))
       #?(:clj (= Character/FORMAT (Character/getType (int c)))
          :cljs (.test cf-pattern (js/String.fromCodePoint c)))))

(def ^:private zwnj 0x200C)

(def ^:private zwj 0x200D)

(defn- joiner? [c] (or (= c zwnj) (= c zwj)))

(defn- selector? [c] (and (some? c) (<= 0xFE00 c 0xFE0F)))

(defn- blank-filler?
  "Visible as nothing: spaces, Hangul fillers and the blank braille pattern."
  [c]
  (or (#{0x09 0x0A 0x0D 0x20 0x85 0xA0 0x1680 0x2028 0x2029 0x202F 0x205F 0x3000 0x3164 0xFFA0 0x115F 0x1160 0x2800} c)
      (<= 0x2000 c 0x200A)))

(defn- always-forbidden? [c]
  (or (and (< c 0x20) (not (#{0x09 0x0A} c)))
      (<= 0x7F c 0x9F)
      (<= 0xD800 c 0xDFFF)
      (<= 0xE0100 c 0xE01EF)
      (<= 0xFFF9 c 0xFFFB)
      (= 0x034F c)
      (= 0x180E c)
      (= 0x061C c)))

(defn- visible-base?
  "A character a joiner or selector may attach to: not blank, not a control,
  not a format character, a joiner or a variation selector."
  [c]
  (and (some? c) (not (always-forbidden? c)) (not (format-character? c)) (not (blank-filler? c)) (not (selector? c))))

(defn forbidden-at
  "Index of the first code point in points that may not appear where it is,
  or nil."
  [points]
  (let [points (vec points)]
    (first
     (keep-indexed
      (fn [i c]
        (let [before (get points (dec i)) after (get points (inc i))]
          (when (cond
                  (always-forbidden? c) true
                  (joiner? c) (not (and (or (visible-base? before) (selector? before)) (visible-base? after)))
                  (selector? c) (not (visible-base? before))
                  (format-character? c) true
                  :else false)
            i)))
      points))))

(defn forbidden?
  "Whether code point c may never appear in goal text, whatever surrounds it."
  [c]
  (or (always-forbidden? c) (and (format-character? c) (not (joiner? c)))))

(defn- visible-blank? [points]
  (every? #(or (format-character? %) (blank-filler? %) (selector? %) (always-forbidden? %)) points))

(defn problem
  "nil when s (already normalised) is acceptable goal text of at most limit
  characters, else [reason details]: :missing, :invalid-character or
  :too-long."
  [s limit]
  (if-not (string? s)
    [:missing {}]
    (let [points (code-points s)]
      (cond
        (or (str/blank? s) (visible-blank? points)) [:missing {}]
        (forbidden-at points) [:invalid-character {:code-point (nth points (forbidden-at points))}]
        (> (count points) limit) [:too-long {:length (count points) :limit limit}]))))

(defn code-point-text
  "U+XXXX for a code point, for messages."
  [c]
  (let [hex #?(:clj (Integer/toHexString c) :cljs (.toString c 16))]
    (str "U+" (str/upper-case (str (apply str (repeat (max 0 (- 4 (count hex))) "0")) hex)))))
