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

(def step-budget
  "Search steps one `pack` call may spend over all its column counts and row
  targets. A bank whose exact search needs more is placed first-fit in
  authored order for the rest, so the cost is bounded and quality degrades
  gradually rather than at a card count."
  6000)

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
  [cols rows cards steps]
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
                  (neg? (vswap! steps dec)) ::exhausted
                  :else
                  (let [state [cell occupied (frequencies (map (juxt :w :h) remaining)) empties]]
                    (when-not (contains? @failed state)
                      (or (some (fn [{:keys [w h index]}]
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

(defn- quick-layout
  "Authored-order first fit in `cols` columns, without searching."
  [cols cards]
  (let [area (reduce + (map #(* (:w %) (:h %)) cards))
        placed (first-fit cols cards)
        rows (reduce max 0 (map #(+ (:row %) (:h %)) placed))]
    {:columns cols :rows rows :empty (- (* cols rows) area)
     :cells (vec (sort-by (juxt :row :col) placed)) :exact? false}))

(defn- layout
  "The exact layout in `cols` columns with the fewest rows, at most
  `max-rows` (nil when none fits in that many), or first fit when the budget
  runs out."
  ([cols cards steps] (layout cols cards steps nil))
  ([cols cards steps max-rows]
   (let [area (reduce + (map #(* (:w %) (:h %)) cards))
         tallest (reduce max 1 (map :h cards))
         quick (quick-layout cols cards)
         ceiling (min (:rows quick) (or max-rows (:rows quick)))
         searched (loop [rows (max tallest (quot (+ area cols -1) cols)) exhausted? false]
                    (cond (or exhausted? (> rows ceiling)) (when exhausted? ::exhausted)
                          (= rows (:rows quick)) nil
                          :else (let [found (solve cols rows cards steps)]
                                  (cond (= ::exhausted found) (recur rows true)
                                        found found
                                        :else (recur (inc rows) false)))))]
     (cond
       (sequential? searched)
       (let [rows (reduce max 0 (map #(+ (:row %) (:h %)) searched))]
         {:columns cols :rows rows :empty (- (* cols rows) area)
          :cells (vec (sort-by (juxt :row :col) searched)) :exact? true})
       (and max-rows (> (:rows quick) max-rows) (not= ::exhausted searched)) nil
       :else (assoc quick :exact? (not= ::exhausted searched))))))

(defn optimum
  "The fewest empty cells this search finds for `spans` ([w h] each) in
  exactly `cols` columns, and its row count, ignoring order."
  [spans cols]
  (select-keys (layout cols (vec (map-indexed (fn [i [w h]] {:index i :w w :h h}) spans)) (volatile! step-budget))
               [:empty :rows]))

(defn pack
  "Grid areas for cards with these spans ([columns rows] each, authored
  order) on a grid of at most `max-columns` columns:
  {:columns c :rows r :empty n :exact? bool :cells [{:index :col :row :w :h} ...]},
  cells in reading order (`:index` is the card's authored position, `:col`
  and `:row` count from zero). Of the column counts between the widest card
  and `max-columns`, it takes those giving the fewest rows, and of those the
  narrowest, which also leaves the fewest empty cells. The widest count is
  searched first, where the fewest rows usually are, and a narrower count is
  only tried for the same height, so the step budget goes where it matters.
  Options:
  - `:exact-columns?` uses exactly `max-columns` (or the widest card if
    wider): for a board whose tracks are fractions of its width, so a card's
    width never depends on its siblings;
  - `:quick?` places first-fit in authored order without searching, for a
    first render while the exact result is computed later. `:exact?` in the
    result says whether the search completed."
  ([spans max-columns] (pack spans max-columns nil))
  ([spans max-columns {:keys [exact-columns? quick?]}]
   (let [cards (vec (map-indexed (fn [i [w h]] {:index i :w (max 1 w) :h (max 1 h)}) spans))
         narrowest (reduce max 1 (map :w cards))
         area (reduce + (map #(* (:w %) (:h %)) cards))
         widest (max narrowest (or max-columns narrowest))
         capped (max narrowest (min widest (max area 1)))
         steps (volatile! step-budget)]
     (cond
       (empty? cards) {:columns (if exact-columns? (max 1 (or max-columns 1)) 1) :rows 0 :empty 0 :cells [] :exact? true}
       (and quick? exact-columns?) (quick-layout widest cards)
       quick? (quick-layout capped cards)
       exact-columns? (layout widest cards steps)
       :else
       (loop [cols capped best nil]
         (if (< cols narrowest)
           best
           (let [floor-rows (max (reduce max 1 (map :h cards)) (quot (+ area cols -1) cols))]
             (if (and best (> floor-rows (:rows best)))
               best
               (let [candidate (layout cols cards steps (some-> best :rows))]
                 (recur (dec cols)
                        (cond (nil? candidate) best
                              (nil? best) candidate
                              (<= (:rows candidate) (:rows best)) (assoc candidate :exact? (and (:exact? candidate) (:exact? best)))
                              :else best)))))))))))

#?(:cljs
   (defn ^:export pack-js
     "pack for browser measurement and other surfaces: spans as a JS array of
     [w h], the result as JS data."
     [spans max-columns exact-columns?]
     (clj->js (pack (js->clj spans) max-columns {:exact-columns? exact-columns?}))))
