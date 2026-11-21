(ns tolglow.studio.repl-code
  "Bounded source selection helpers. Evaluation and Clojure reading remain in
  the JVM; this scanner only locates balanced delimiters around a caret.")

(def opening {"(" ")" "[" "]" "{" "}"})
(def closing #{")" "]" "}"})
(defn- separator? [ch] (or (nil? ch) (contains? #{"(" ")" "[" "]" "{" "}" "\"" ";" ","} ch) (boolean (re-matches #"\s" ch))))
(defn- prefix-start [source start]
  (loop [i start]
    (cond
      (and (> i 2) (= "#?@" (subs source (- i 3) i))) (recur (- i 3))
      (and (> i 1) (contains? #{"~@" "#_" "#?"} (subs source (- i 2) i))) (recur (- i 2))
      (and (pos? i) (contains? #{"'" "`" "~" "@" "#"} (subs source (dec i) i))) (recur (dec i))
      :else i)))
(defn form-at
  "Return the smallest balanced collection/string containing the caret, or the
  atom there. Strings, line comments and character literals cannot fake brackets.
  A non-balanced buffer fails visibly instead of guessing an expression."
  [source caret]
  (when (> (count source) 131072) (throw (js/Error. "Source exceeds the form-selection limit.")))
  (let [length (count source) caret (max 0 (min length caret))
        caret (if (and (pos? caret) (or (= caret length) (separator? (subs source caret (inc caret)))) (closing (subs source (dec caret) caret))) (dec caret) caret)
        ranges (loop [i 0 stack [] ranges [] string-start nil escaped? false comment? false]
                 (if (= i length)
                   (if (or (seq stack) string-start) (throw (js/Error. "Balance delimiters before evaluating a form.")) ranges)
                   (let [ch (subs source i (inc i))]
                     (cond
                       comment? (recur (inc i) stack ranges nil false (not= ch "\n"))
                       string-start
                       (cond escaped? (recur (inc i) stack ranges string-start false false)
                             (= ch "\\") (recur (inc i) stack ranges string-start true false)
                             (= ch "\"") (recur (inc i) stack (conj ranges [string-start (inc i)]) nil false false)
                             :else (recur (inc i) stack ranges string-start false false))
                       (= ch ";") (recur (inc i) stack ranges nil false true)
                       (= ch "\"") (recur (inc i) stack ranges i false false)
                       (= ch "\\") (let [end (loop [j (min length (+ i 2))]
                                               (if (or (= j length) (separator? (subs source j (inc j)))) j (recur (inc j))))]
                                       (recur end stack ranges nil false false))
                       (opening ch) (recur (inc i) (conj stack [ch i]) ranges nil false false)
                       (closing ch) (let [[open start] (peek stack)]
                                      (when (not= ch (opening open)) (throw (js/Error. "Balance delimiters before evaluating a form.")))
                                      (recur (inc i) (pop stack) (conj ranges [start (inc i)]) nil false false))
                       :else (recur (inc i) stack ranges nil false false)))))
        [start end] (or (first (sort-by (fn [[a b]] (- b a)) (filter (fn [[a b]] (<= a caret (dec b))) ranges)))
                        (let [start (loop [i caret] (if (or (zero? i) (separator? (subs source (dec i) i))) i (recur (dec i))))
                              end (loop [i caret] (if (or (= i length) (separator? (subs source i (inc i)))) i (recur (inc i))))]
                          [start end]))]
    (when (< start end) (subs source (prefix-start source start) end))))

(defn symbol-at [source caret]
  (let [caret (max 0 (min (count source) caret))
        start (loop [i caret] (if (or (zero? i) (separator? (subs source (dec i) i))) i (recur (dec i))))
        end (loop [i caret] (if (or (= i (count source)) (separator? (subs source i (inc i)))) i (recur (inc i))))]
    {:start start :end end :symbol (subs source start end) :prefix (subs source start caret)}))
