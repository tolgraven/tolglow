(ns tolglow.studio.pretty
  "Pretty-printed EDN for text people read and edit: the definition editor,
  its Neovim buffer and Studio's read-only data views. The JVM host and the
  browser run this same layout, so a value prints to the same lines on both
  apart from each platform's own number printing (ClojureScript prints 1.0 as
  1). The text reads back to the value it printed and never ends in a newline,
  so it survives Neovim's split and join on \"\\n\" unchanged.

  Layout: a collection stays on one line when it fits the width; otherwise a
  map puts each entry on its own line with the value after its key, a vector,
  list or set of scalars fills each line, and one holding collections puts
  each element on its own line. A map lists :id, :version and :name first,
  then keeps the order of a small (array) or sorted map and sorts the keys of
  a hash map, whose own order is arbitrary and differs between platforms. Set
  elements are sorted the same way. Records and other values print as pr-str
  does.")

(def default-width
  "Columns a line may use before a collection breaks across lines."
  80)

(def ^:private leading-keys [:id :version :name])

(defn- ordered-map? [m]
  (or (sorted? m)
      #?(:clj (instance? clojure.lang.PersistentArrayMap m)
         :cljs (instance? PersistentArrayMap m))))

(defn- compare-printed
  "A total order that works across key types: numbers by value first, then
  everything else by its printed form."
  [a b]
  (cond
    (and (number? a) (number? b)) (compare a b)
    (number? a) -1
    (number? b) 1
    :else (compare (pr-str a) (pr-str b))))

(defn- map-entries [m]
  (let [lead (keep #(find m %) leading-keys)
        others (remove #(some #{(key %)} leading-keys) m)]
    (concat lead (if (ordered-map? m) others (sort-by key compare-printed others)))))

(defn- collection-kind [x]
  (cond
    (record? x) nil
    (map? x) :map
    (vector? x) :vector
    (set? x) :set
    (seq? x) :list
    :else nil))

(def ^:private delimiters
  {:map ["{" "}"] :vector ["[" "]"] :set ["#{" "}"] :list ["(" ")"]})

(defn- elements
  "The items printed inside a collection, in printing order. A map entry is a
  pair of items."
  [kind x]
  (case kind
    :map (mapcat (fn [[k v]] [k v]) (map-entries x))
    :set (sort compare-printed x)
    (seq x)))

(defn- flat
  "One-line text of `x`, or nil once it would exceed `budget` characters."
  [x budget]
  (if-let [kind (collection-kind x)]
    (let [[open close] (delimiters kind)]
      (loop [items (elements kind x)
             parts [open]
             used (+ (count open) (count close))
             first? true]
        (cond
          (> used budget) nil
          (empty? items) (apply str (conj parts close))
          :else (when-let [text (flat (first items) (- budget used (if first? 0 1)))]
                  (recur (rest items) (cond-> parts (not first?) (conj " ") true (conj text))
                         (+ used (count text) (if first? 0 1)) false)))))
    (let [text (pr-str x)]
      (when (<= (count text) budget) text))))

(defn- spaces [n] (apply str (repeat n " ")))

(defn- last-line-width [text start-column]
  (let [newline (.lastIndexOf ^String text "\n")]
    (if (neg? newline) (+ start-column (count text)) (- (count text) newline 1))))

(declare layout)

(defn- layout-map [x column tail width]
  (let [inner (inc column)
        entries (vec (map-entries x))
        last-index (dec (count entries))]
    (str "{"
         (apply str
                (interpose
                 (str "\n" (spaces inner))
                 (map-indexed
                  (fn [i [k v]]
                    (let [key-text (layout k inner 0 width)
                          value-column (inc (last-line-width key-text inner))
                          value-tail (if (= i last-index) (inc tail) 0)]
                      (str key-text " " (layout v value-column value-tail width))))
                  entries)))
         "}")))

(defn- fill
  "Scalars packed onto as few lines as fit, each line starting at `column`."
  [texts column tail width]
  (let [last-index (dec (count texts))]
    (loop [i 0 lines [] line nil]
      (if (> i last-index)
        (apply str (interpose (str "\n" (spaces column)) (conj lines line)))
        (let [text (nth texts i)
              end (+ column (count line) (if line 1 0) (count text) (if (= i last-index) tail 0))]
          (if (or (nil? line) (<= end width))
            (recur (inc i) lines (if line (str line " " text) text))
            (recur (inc i) (conj lines line) text)))))))

(defn- layout-sequence [kind x column tail width]
  (let [[open close] (delimiters kind)
        inner (+ column (count open))
        items (vec (elements kind x))
        last-index (dec (count items))
        body (if (some collection-kind items)
               (apply str (interpose (str "\n" (spaces inner))
                                     (map-indexed (fn [i item]
                                                    (layout item inner (if (= i last-index) (inc tail) 0) width))
                                                  items)))
               (fill (mapv pr-str items) inner (+ tail (count close)) width))]
    (str open body close)))

(defn- layout
  "Text of `x` starting at `column`, followed on its last line by `tail`
  closing delimiters."
  [x column tail width]
  (or (flat x (- width column tail))
      (let [kind (collection-kind x)]
        (case kind
          nil (pr-str x)
          :map (layout-map x column tail width)
          (layout-sequence kind x column tail width)))))

(defn edn
  "Pretty-printed EDN text of `value`. Options: `:width` (default 80); when
  the text is spliced into other text, `:column`, the column its first line
  starts at (later lines are indented to match), and `:tail`, the number of
  characters that follow it on its last line."
  ([value] (edn value nil))
  ([value {:keys [width column tail] :or {width default-width column 0 tail 0}}]
   (binding [*print-length* nil *print-level* nil *print-meta* false]
     (layout value column tail width))))

#?(:cljs
   (defn json
     "JSON text of a ClojureScript or JavaScript value with 2-space indentation."
     [value]
     (js/JSON.stringify (clj->js value) nil 2)))
