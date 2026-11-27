(ns tolglow.authoring.slot-grid
  "Deterministic placement of cards on a slot grid (#795). Studio's parameter
  banks and macro boards, and any other surface such as the Push display, call
  `pack` with each card's span in authored order and the most columns that
  fit, and draw the returned grid areas. Placement is computed here rather
  than by CSS auto-placement, so it is the same everywhere and testable:

  - it uses the fewest rows the available width allows, then the fewest
    columns that keep that height, so the bank is as short as the width
    permits and exactly as wide as it needs: no row ends in a long empty tail
    and, for that height and width, no cell is wasted that a better
    arrangement would fill;
  - within that, cards keep their authored order: each free cell, read row
    by row, takes the earliest remaining card that still allows the optimum,
    so a later, smaller card only moves up to fill a hole;
  - the result lists cards in reading order, which a view uses as its DOM and
    tab order, so keyboard and screen-reader order follow the screen.

  Nothing here reads a clock or a device.")

(def ^:private search-limit
  "Banks with more cards than this are placed first-fit in authored order; an
  exact search over every column count would cost more than it saves."
  18)

(defn- cells-of [cols cell w h]
  (for [dr (range h) dc (range w)] (+ cell (* dr cols) dc)))

(defn- fits? [cols rows occupied cell w h]
  (let [row (quot cell cols) col (rem cell cols)]
    (and (<= (+ col w) cols) (<= (+ row h) rows)
         (not-any? occupied (cells-of cols cell w h)))))

(defn- solve
  "Place `cards` ({:index :w :h}, authored order) in exactly `rows` rows of
  `cols`, or nil. The first free cell in reading order takes the earliest
  remaining card of each shape in turn, then stays empty while the empty
  budget allows. Cards of one shape are interchangeable for feasibility, so
  trying only the earliest of each shape loses no solution; failed states
  are remembered, which keeps the search small."
  [cols rows cards]
  (let [total (* cols rows)
        budget (- total (reduce + (map #(* (:w %) (:h %)) cards)))
        failed (volatile! #{})]
    (when-not (neg? budget)
      (letfn [(candidates [remaining]
                (vals (reduce (fn [found card]
                                (let [shape [(:w card) (:h card)]]
                                  (if (contains? found shape) found (assoc found shape card))))
                              (array-map) remaining)))
              (step [cell occupied remaining empties placed]
                (cond
                  (empty? remaining) placed
                  (>= cell total) nil
                  (contains? occupied cell) (recur (inc cell) (disj occupied cell) remaining empties placed)
                  :else
                  (let [state [cell occupied (frequencies (map (juxt :w :h) remaining)) empties]]
                    (when-not (contains? @failed state)
                      (or (some (fn [{:keys [w h index] :as card}]
                                  (when (fits? cols rows occupied cell w h)
                                    (step (inc cell) (into occupied (rest (cells-of cols cell w h)))
                                          (filterv #(not= index (:index %)) remaining) empties
                                          (conj placed {:index index :col (rem cell cols) :row (quot cell cols) :w w :h h}))))
                                (sort-by :index (candidates remaining)))
                          (when (< empties budget)
                            (step (inc cell) occupied remaining (inc empties) placed))
                          (do (vswap! failed conj state) nil))))))]
        (step 0 #{} (vec cards) 0 [])))))

(defn- first-fit
  "Authored-order first fit, for banks too large to search."
  [cols cards]
  (loop [cards cards occupied #{} placed []]
    (if-let [{:keys [w h index]} (first cards)]
      (let [cell (first (filter #(fits? cols #?(:clj Integer/MAX_VALUE :cljs js/Number.MAX_SAFE_INTEGER) occupied % w h) (range)))]
        (recur (rest cards) (into occupied (cells-of cols cell w h))
               (conj placed {:index index :col (rem cell cols) :row (quot cell cols) :w w :h h})))
      placed)))

(defn- layout [cols cards]
  (let [area (reduce + (map #(* (:w %) (:h %)) cards))
        tallest (reduce max 1 (map :h cards))
        placed (if (> (count cards) search-limit)
                 (first-fit cols cards)
                 (some #(solve cols % cards) (iterate inc (max tallest (quot (+ area cols -1) cols)))))
        rows (reduce max 0 (map #(+ (:row %) (:h %)) placed))]
    {:columns cols :rows rows :empty (- (* cols rows) area)
     :cells (vec (sort-by (juxt :row :col) placed))}))

(defn optimum
  "The fewest empty cells any arrangement of `spans` ([w h] each) leaves in
  exactly `cols` columns, and its row count, ignoring order. Tests and the
  layout QA compare `pack` with it."
  [spans cols]
  (select-keys (layout cols (map-indexed (fn [i [w h]] {:index i :w w :h h}) spans)) [:empty :rows]))

(defn pack
  "Grid areas for cards with these spans ([columns rows] each, authored
  order) on a grid of at most `max-columns` columns:
  {:columns c :rows r :empty n :cells [{:index :col :row :w :h} ...]}, cells
  in reading order (`:index` is the card's authored position, `:col` and
  `:row` count from zero). Of the column counts between the widest card and
  `max-columns`, it takes those giving the fewest rows, and of those the
  narrowest, which also leaves the fewest empty cells."
  [spans max-columns]
  (let [cards (vec (map-indexed (fn [i [w h]] {:index i :w (max 1 w) :h (max 1 h)}) spans))
        narrowest (reduce max 1 (map :w cards))
        area (reduce + (map #(* (:w %) (:h %)) cards))
        widest (max narrowest (min (or max-columns narrowest) (max area 1)))]
    (if (empty? cards)
      {:columns 1 :rows 0 :empty 0 :cells []}
      (->> (range narrowest (inc widest))
           (map #(layout % cards))
           (sort-by (juxt :rows :columns))
           first))))
