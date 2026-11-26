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
  indented two columns past its key; several entries never share a line. A
  vector, list or set of scalars fills each line, except that a keyword-argument
  vector or list ([:key value :key value ...], not keywords only) puts each pair
  on its own line; one holding collections puts each element on its own line.
  A map lists :id, :version and :name first, then
  keeps the order of a small (array) or sorted map and sorts the keys of a
  hash map, whose own order is arbitrary and differs between platforms. Set
  elements are sorted the same way. Records and other values print as pr-str
  does."
  #?(:cljs (:import [goog.string StringBuffer])))

(def default-width
  "Columns a line may use before a collection breaks across lines."
  100)

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

(defn- leading-entry
  "The entry for `k`. A sorted map whose comparator cannot compare `k` with its
  keys (a map keyed by numbers or strings) cannot hold `k`."
  [m k]
  (if (sorted? m)
    (try (find m k) (catch #?(:clj ClassCastException :cljs :default) _ nil))
    (find m k)))

(defn- sort-shown
  "Items of a hash map or set in print order, sorted by `keyfn`. Under a length
  limit only the first `shown` items in the collection's own order can print,
  the items *print-length* would show, so only those are sorted; the next
  item stays to mark the elision. An elided view of a huge map or set then
  costs as little as the text it shows."
  [keyfn items shown]
  (if shown
    (let [head (take (inc shown) items)]
      (concat (sort-by keyfn compare-printed (take shown head)) (drop shown head)))
    (sort-by keyfn compare-printed items)))

(defn- map-entries [m]
  (let [lead (keep #(leading-entry m %) leading-keys)
        others (remove #(some #{(key %)} leading-keys) m)
        shown (some-> (:length *limits*) (- (count lead)) (max 0))]
    (concat lead (if (ordered-map? m) others (sort-shown key others shown)))))

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

(def ^:private ^:dynamic *lazy-length*
  ;; Under a character budget, the most elements of a lazy (uncounted)
  ;; sequence that could be shown, or nil without a budget.
  nil)

(def ^:private ^:dynamic *lazy-allowance*
  ;; Under a character budget, a volatile count of lazy elements the whole call
  ;; may still realize, or nil without a budget.
  nil)

(def ^:private ^:dynamic *deepest*
  ;; Under a character budget, a volatile of the deepest layout level reached,
  ;; so a stack overflow counts as truncation only when the layout itself was
  ;; that deep, not when user code inside the value recursed.
  nil)

(def ^:private deep-layout 256)

(defn- budget-reached! []
  (throw (ex-info "Pretty-print budget reached" {::budget true})))

(defn- limited
  "`items` cut to the length limit, with the ellipsis after the last shown
  item. A lazy sequence under a character budget is also cut to what the
  budget could show, and its realized elements count against the call's
  allowance, so an infinite or deeply nested lazy value stops early. Counted
  collections keep their exact elision, so their text does not change."
  [items lazy?]
  (let [n (:length *limits*)
        n (if (and lazy? *lazy-length*) (if n (min n *lazy-length*) *lazy-length*) n)]
    (if n
      (let [head (doall (take (inc n) items))]
        (when (and lazy? *lazy-allowance*
                   (neg? (vswap! *lazy-allowance* - (count head))))
          (budget-reached!))
        (if (> (count head) n) (concat (take n head) [ellipsis]) head))
      items)))

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
                    :set (sort-shown identity x (:length *limits*))
                    (seq x))
                  (and (= kind :list) (not (counted? x)))))))

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

;; Layout appends to one buffer as it goes, so a character budget can stop it
;; as soon as the text is long enough instead of after building all of it.
(def ^:private ^:dynamic *buffer* nil)

(def ^:private ^:dynamic *column* nil)

(def ^:private ^:dynamic *newlines* nil)

(def ^:private ^:dynamic *max-chars* nil)

(defn- new-buffer [] #?(:clj (StringBuilder.) :cljs (StringBuffer.)))

(defn- buffer-length [] #?(:clj (.length ^StringBuilder *buffer*) :cljs (.getLength *buffer*)))

(defn- emit! [^String text]
  #?(:clj (.append ^StringBuilder *buffer* text) :cljs (.append *buffer* text))
  (let [newline (.lastIndexOf text "\n")]
    (if (neg? newline)
      (vswap! *column* + (count text))
      (do (vreset! *column* (- (count text) newline 1))
          (vswap! *newlines* inc))))
  (when (and *max-chars* (> (buffer-length) *max-chars*))
    (budget-reached!)))

(defn- new-line! [column] (emit! (str "\n" (spaces column))))

(declare layout!)

(defn- map-entry!
  "A key and its value: on the key's line when the value fits there (or is a
  scalar, or the key itself spans lines), otherwise on the next line indented
  two columns past the key, so deep values do not drift to the right edge."
  [k v inner tail width depth]
  (let [lines @*newlines*]
    (layout! k 0 width (inc depth))
    (let [hang-column (inc @*column*)]
      (if (or (not (collection-kind v))
              (flat v (inc depth) (- width hang-column tail))
              (not= lines @*newlines*))
        (emit! " ")
        (new-line! (+ inner flow-indent)))
      (layout! v tail width (inc depth)))))

(defn- layout-map! [x tail width depth]
  (let [inner (inc @*column*)
        pairs (vec (entries :map x depth))
        last-index (dec (count pairs))]
    (emit! "{")
    (dotimes [i (count pairs)]
      (when (pos? i) (new-line! inner))
      (let [pair (nth pairs i)]
        (if (identical? pair ellipsis)
          (emit! (scalar-text pair))
          (map-entry! (first pair) (second pair) inner (if (= i last-index) (inc tail) 0) width depth))))
    (emit! "}")))

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

(defn- keyword-pairs?
  "A sequence of keyword/value pairs, as in keyword-argument style vectors. A
  set is never one, and neither is a sequence of keywords only, such as a list
  of fixture IDs, which fills its lines like other scalars."
  [kind elements]
  (and (not= kind :set) (<= 4 (count elements)) (even? (count elements))
       (every? keyword? (take-nth 2 elements))
       (not-every? keyword? (take-nth 2 (rest elements)))))

(defn- layout-sequence! [kind x tail width depth]
  (let [[open close] (delimiters kind)
        inner (+ @*column* (count open))
        elements (vec (entries kind x depth))
        last-index (dec (count elements))]
    (emit! open)
    (if (some collection-kind elements)
      (dotimes [i (count elements)]
        (when (pos? i) (new-line! inner))
        (layout! (nth elements i) (if (= i last-index) (+ tail (count close)) 0) width (inc depth)))
      (let [texts (mapv scalar-text elements)]
        (if (keyword-pairs? kind elements)
          (emit! (apply str (interpose (str "\n" (spaces inner))
                                       (map (fn [[k v]] (str k " " v)) (partition 2 texts)))))
          (emit! (fill texts inner (+ tail (count close)) width)))))
    (emit! close)))

(defn- layout!
  "Append the text of `x` at nesting `depth`, starting at the current column
  and followed on its last line by `tail` characters."
  [x tail width depth]
  ;; Every nesting level prints at least one character, so a budget also
  ;; bounds the depth (and the stack) of the layout.
  (when *max-chars*
    (when (> depth *max-chars*) (budget-reached!))
    (when (> depth @*deepest*) (vreset! *deepest* depth)))
  (if-let [text (flat x depth (- width @*column* tail))]
    (emit! text)
    (if-let [kind (collection-kind x)]
      (if (= kind :map)
        (layout-map! x tail width depth)
        (layout-sequence! kind x tail width depth))
      (emit! (scalar-text x)))))

(defn edn-within
  "Pretty-printed EDN of `value` as {:text t :truncated? b}. Takes the options
  of `edn`; with `:max-chars` it stops once the text exceeds that many
  characters and returns its first `:max-chars` characters, so the work and
  memory stay bounded by the budget rather than by the size of `value`. A
  lazy value nested so deep or wide that showing the budget would mean
  realizing far more than it shows stops earlier, with shorter text and
  `:truncated? true`. Counted collections print exactly as `edn` would."
  [value {:keys [width column tail length level max-chars] :or {width default-width column 0 tail 0}}]
  ;; Values printed by pr-str (records, Java objects) inside get the same
  ;; limits, and under a budget a lazy seq inside one is cut to what could be
  ;; shown; the visible text is the same.
  (binding [*print-length* (or length (some-> max-chars inc)) *print-level* level *print-meta* false
            *limits* (when (or length level) {:length length :level level})
            *buffer* (new-buffer) *column* (volatile! column) *newlines* (volatile! 0)
            *max-chars* max-chars
            ;; No more than max-chars elements fit in max-chars characters. A
            ;; lazy sequence is never realized past that, and the whole call
            ;; realizes a bounded multiple of it however deep the value nests.
            *lazy-length* (when max-chars (inc max-chars))
            *lazy-allowance* (when max-chars (volatile! (* 64 (inc max-chars))))
            *deepest* (volatile! 0)]
    (try
      (layout! (shown value 0) tail width 0)
      {:text (str *buffer*) :truncated? false}
      (catch #?(:clj Throwable :cljs :default) error
        ;; A value nested deeper than the stack allows is cut where it stopped,
        ;; like one that reached the character budget.
        (if (and max-chars
                 (or (::budget (ex-data error))
                     (and (> @*deepest* deep-layout)
                          #?(:clj (instance? StackOverflowError error)
                             :cljs (boolean (re-find #"call stack|too much recursion"
                                                     (str (.-message error))))))))
          (let [text (str *buffer*)]
            {:text (subs text 0 (min max-chars (count text))) :truncated? true})
          (throw error))))))

(defn edn
  "Pretty-printed EDN text of `value`. Options: `:width` (default 100); when
  the text is spliced into other text, `:column`, the column its first line
  starts at (later lines are indented to match), and `:tail`, the number of
  characters that follow it on its last line; `:length` and `:level` elide
  long and deep collections as *print-length* and *print-level* do (a hash
  map or set longer than `:length` shows the first items in its own order,
  sorted, as *print-length* would choose them), and `:max-chars` stops after that many characters and appends \"...\" (the text
  then no longer reads back to the whole value)."
  ([value] (edn value nil))
  ([value options]
   (let [{:keys [text truncated?]} (edn-within value options)]
     (cond-> text truncated? (str "...")))))

#?(:cljs
   (defn json
     "JSON text of a ClojureScript or JavaScript value with 2-space indentation."
     [value]
     (js/JSON.stringify (clj->js value) nil 2)))
