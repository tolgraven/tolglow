(ns tolglow.studio.pretty
  "Pretty-printed EDN for text people read and edit: the definition editor,
  its Neovim buffer, Studio's read-only data views and REPL results. The JVM
  host and the browser run this same layout, so a value prints to the same
  lines on both apart from each platform's own number printing (ClojureScript
  prints 1.0 as 1). The text reads back to the value it printed and never ends
  in a newline, so it survives Neovim's split and join on \"\\n\" unchanged.

  Layout: a collection stays on one line when it fits the width. Otherwise a
  map puts each entry on its own line with the value after its key, or, when
  a collection value does not fit on the rest of that line, on the next line
  indented two columns past its key. A vector, list or
  set of scalars fills each line, and one holding collections puts each
  element on its own line. A map lists :id, :version and :name first, then
  keeps the order of a small (array) or sorted map and sorts the keys of a
  hash map, whose own order is arbitrary and differs between platforms. Set
  elements are sorted the same way. Records and other values print as pr-str
  does.")

(def default-width
  "Columns a line may use before a collection breaks across lines."
  80)

(def ^:private flow-indent 2)

(def ^:private leading-keys [:id :version :name])

(def ^:private ^:dynamic *limits*
  "{:length n :level n}: elide like *print-length* and *print-level*."
  nil)

(deftype ^:private Elided [text])

(def ^:private ellipsis (Elided. "..."))

(def ^:private too-deep (Elided. "#"))

(defn- scalar-text [x]
  (if (instance? Elided x) (.-text ^Elided x) (pr-str x)))

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

(defn- shown
  "`x` as printed at nesting `depth`: a collection below the level limit is #."
  [x depth]
  (let [level (:level *limits*)]
    (if (and level (>= depth level) (collection-kind x)) too-deep x)))

(defn- limited [items]
  (if-let [n (:length *limits*)]
    (let [head (take (inc n) items)]
      (if (> (count head) n) (concat (take n head) [ellipsis]) head))
    items))

(defn- entries
  "What a collection at `depth` prints, in order: [key value] pairs for a map,
  elements otherwise, each already elided for its own depth, plus the
  ellipsis past the length limit."
  [kind x depth]
  (let [child (inc depth)]
    (map (fn [item]
           (cond
             (identical? item ellipsis) item
             (= kind :map) [(shown (key item) child) (shown (val item) child)]
             :else (shown item child)))
         (limited (case kind
                    :map (map-entries x)
                    :set (sort compare-printed x)
                    (seq x))))))

(defn- items
  "Entries with each map pair spread into its key and value."
  [kind x depth]
  (cond->> (entries kind x depth)
    (= kind :map) (mapcat #(if (identical? % ellipsis) [%] %))))

(defn- flat
  "One-line text of `x` at `depth`, or nil once it would exceed `budget`
  characters."
  [x depth budget]
  (if-let [kind (collection-kind x)]
    (let [[open close] (delimiters kind)]
      (loop [remaining (items kind x depth)
             parts [open]
             used (+ (count open) (count close))
             first? true]
        (cond
          (> used budget) nil
          (empty? remaining) (apply str (conj parts close))
          :else (when-let [text (flat (first remaining) (inc depth) (- budget used (if first? 0 1)))]
                  (recur (rest remaining) (cond-> parts (not first?) (conj " ") true (conj text))
                         (+ used (count text) (if first? 0 1)) false)))))
    (let [text (scalar-text x)]
      (when (<= (count text) budget) text))))

(defn- spaces [n] (apply str (repeat n " ")))

(defn- last-line-width [text start-column]
  (let [newline (.lastIndexOf ^String text "\n")]
    (if (neg? newline) (+ start-column (count text)) (- (count text) newline 1))))

(declare layout)

(defn- map-entry-text
  "A key and its value: on the key's line when the value fits there (or is a
  scalar, or the key itself spans lines), otherwise on the next line indented
  two columns past the key, so deep values do not drift to the right edge."
  [k v inner tail width depth]
  (let [key-text (layout k inner 0 width (inc depth))
        hang-column (inc (last-line-width key-text inner))]
    (if (or (not (collection-kind v))
            (flat v (inc depth) (- width hang-column tail))
            (not (neg? (.indexOf ^String key-text "\n"))))
      (str key-text " " (layout v hang-column tail width (inc depth)))
      (let [flow-column (+ inner flow-indent)]
        (str key-text "\n" (spaces flow-column) (layout v flow-column tail width (inc depth)))))))

(defn- layout-map [x column tail width depth]
  (let [inner (inc column)
        pairs (vec (entries :map x depth))
        last-index (dec (count pairs))]
    (str "{"
         (apply str
                (interpose
                 (str "\n" (spaces inner))
                 (map-indexed
                  (fn [i pair]
                    (if (identical? pair ellipsis)
                      (scalar-text pair)
                      (map-entry-text (first pair) (second pair) inner (if (= i last-index) (inc tail) 0) width depth)))
                  pairs)))
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

(defn- layout-sequence [kind x column tail width depth]
  (let [[open close] (delimiters kind)
        inner (+ column (count open))
        elements (vec (entries kind x depth))
        last-index (dec (count elements))]
    (str open
         (if (some collection-kind elements)
           (apply str (interpose (str "\n" (spaces inner))
                                 (map-indexed (fn [i element]
                                                (layout element inner (if (= i last-index) (+ tail (count close)) 0)
                                                        width (inc depth)))
                                              elements)))
           (fill (mapv scalar-text elements) inner (+ tail (count close)) width))
         close)))

(defn- layout
  "Text of `x` at nesting `depth`, starting at `column` and followed on its
  last line by `tail` characters."
  [x column tail width depth]
  (or (flat x depth (- width column tail))
      (if-let [kind (collection-kind x)]
        (if (= kind :map)
          (layout-map x column tail width depth)
          (layout-sequence kind x column tail width depth))
        (scalar-text x))))

(defn edn
  "Pretty-printed EDN text of `value`. Options: `:width` (default 80); when
  the text is spliced into other text, `:column`, the column its first line
  starts at (later lines are indented to match), and `:tail`, the number of
  characters that follow it on its last line; `:length` and `:level` elide
  long and deep collections as *print-length* and *print-level* do (the text
  then no longer reads back to the whole value)."
  ([value] (edn value nil))
  ([value {:keys [width column tail length level] :or {width default-width column 0 tail 0}}]
   ;; Values printed by pr-str (records, Java objects) inside get the same limits.
   (binding [*print-length* length *print-level* level *print-meta* false
             *limits* (when (or length level) {:length length :level level})]
     (layout (shown value 0) column tail width 0))))

#?(:cljs
   (defn json
     "JSON text of a ClojureScript or JavaScript value with 2-space indentation."
     [value]
     (js/JSON.stringify (clj->js value) nil 2)))
