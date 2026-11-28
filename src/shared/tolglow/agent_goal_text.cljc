(ns tolglow.agent-goal-text
  "Text rules for operator goals (#857), shared by the show owner and Studio
  so both see and count the same text. Text is NFC-normalised. It may not
  hold control characters other than newline and tab, bidirectional
  overrides or isolates, zero-width or byte-order characters, Unicode tag
  characters or unpaired surrogates: each can make what an operator reads
  differ from what a model reads. Text that is blank once format characters
  are removed counts as missing. Length is counted in Unicode characters."
  (:require [clojure.string :as str]))

(defn normalize
  "NFC form of s."
  [s]
  #?(:clj (java.text.Normalizer/normalize ^String s java.text.Normalizer$Form/NFC)
     :cljs (.normalize s "NFC")))

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

(defn forbidden?
  "Whether code point c may not appear in goal text."
  [c]
  (or (and (< c 0x20) (not (#{0x09 0x0A} c)))
      (<= 0x7F c 0x9F)
      (<= 0xD800 c 0xDFFF)
      (<= 0x200B c 0x200F)
      (<= 0x202A c 0x202E)
      (<= 0x2060 c 0x2069)
      (= 0xFEFF c)
      (<= 0xE0000 c 0xE007F)))

(defn- format-character? [c]
  (or (<= 0x200B c 0x200F) (<= 0x202A c 0x202E) (<= 0x2060 c 0x2069) (= 0xFEFF c) (= 0xAD c)
      (<= 0xE0000 c 0xE007F)))

(defn- visible-blank? [points]
  (every? #(or (format-character? %) (#{0x09 0x0A 0x0D 0x20 0xA0 0x3000} %) (<= 0x2000 % 0x200A)) points))

(defn problem
  "nil when s is acceptable goal text of at most limit characters, else
  [reason details]. reason is :missing, :invalid-character or :too-long."
  [s limit]
  (if-not (string? s)
    [:missing {}]
    (let [points (code-points s)]
      (cond
        (or (str/blank? s) (visible-blank? points)) [:missing {}]
        (some forbidden? points)
        [:invalid-character {:code-point (some #(when (forbidden? %) %) points)}]
        (> (count points) limit) [:too-long {:length (count points) :limit limit}]))))

(defn code-point-text
  "U+XXXX for a code point, for messages."
  [c]
  (let [hex #?(:clj (Integer/toHexString c) :cljs (.toString c 16))]
    (str "U+" (str/upper-case (str (apply str (repeat (max 0 (- 4 (count hex))) "0")) hex)))))
