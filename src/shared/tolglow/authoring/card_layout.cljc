(ns tolglow.authoring.card-layout
  "Custom card arrangements for macros (#795). By default every running effect
  card and every parameter card takes a standard slot, so effects never get
  free-form sizes. A saved macro (a composition with :mode :macro) may instead
  arrange its members' cards like desktop widgets: each member card is small,
  medium or large, in an order the operator chooses.

  The arrangement is portable data on the macro, `:cards`, a vector of
  `{:slot-id id :size size}` in card order. Members it does not list follow in
  slot order at the default size. A card may also name `:pins`, the member's
  parameters its small size shows, in that order; without pins a small card
  shows the member's leading controls in layout order. The show validates it when a macro is saved
  or loaded and when the `:composition-cards` command replaces it, so Studio
  and the REPL share one validated path. Card order is presentation only; the
  macro still launches its slots in slot order. Nothing here reads a clock or a
  device."
  (:require [clojure.string :as str]))

(def sizes
  "The card sizes of a macro member, smallest first: :small takes one effect
  column, :medium two, :large a whole row. A grid with fewer columns clamps."
  [:small :medium :large])

(def default-size :small)

(def size-labels {:small "S" :medium "M" :large "L"})

(defn- tag [x] (cond (keyword? x) x (and (string? x) (not (str/blank? x))) (keyword x)))

(defn slot-key
  "Compare slot IDs by name, as the composition steps do: Studio's JSON
  boundary turns textual IDs into keywords, so :slot-1 and \"slot-1\" are one
  slot."
  [id]
  (if (keyword? id) (subs (str id) 1) id))

(defn normalize
  "Cards with each size read as a keyword (a wire string becomes one). Empty
  pins are no pins."
  [cards]
  (if (sequential? cards)
    (mapv #(if (map? %)
             (cond-> % (contains? % :size) (update :size tag)
                     (and (contains? % :pins) (or (nil? (:pins %)) (and (sequential? (:pins %)) (empty? (:pins %)))))
                     (dissoc :pins))
             %)
          cards)
    cards))

(defn pin-key
  "Compare parameter IDs by name, like slot IDs: the JSON boundary may send
  :level, \"level\" or a namespaced :wash/level as \"wash/level\"."
  [id]
  (when (or (keyword? id) (and (string? id) (not (str/blank? id))))
    (if (keyword? id) (subs (str id) 1) id)))

(defn- pin-errors
  "Errors of one card's :pins at card index `i`. `params` is the set of
  parameter keys (by `pin-key`) the member's effect declares, or nil when the
  caller does not check membership."
  [i pins params]
  (when (some? pins)
    (if-not (vector? pins)
      [{:path [:cards i :pins] :message "Pins are a vector of the member's parameter IDs"}]
      (let [problems (keep-indexed
                      (fn [j pin]
                        (cond
                          (nil? (pin-key pin))
                          {:path [:cards i :pins j] :message "A pin is a parameter ID"}
                          (and params (not (contains? params (pin-key pin))))
                          {:path [:cards i :pins j] :message "A pin names a parameter of this member's effect"}))
                      pins)
            repeated (->> pins (keep pin-key) frequencies (keep (fn [[k n]] (when (< 1 n) k))) seq)]
        (cond-> (vec problems)
          repeated (conj {:path [:cards i :pins] :message (str "A parameter is pinned once: " (str/join ", " repeated))}))))))

(defn errors
  "Validation errors of `cards` against a macro's slots, as
  [{:path :message}], or nil. With `:params-of`, a function from a slot to
  the parameter IDs its effect declares (nil when unknown), every pin must
  name one of them; an explicit edit checks this, while a save prunes stale
  pins first (`prune-pins`). `opts` :noun and :item name the arrangement's
  owner and its slots in messages (default \"macro\" and \"slot\"); a
  template group passes its own words (#812)."
  ([mode slots cards] (errors mode slots cards nil))
  ([mode slots cards {:keys [params-of noun item] :or {noun "macro" item "slot"}}]
   (let [cards (normalize cards)
         slot-ids (set (map (comp slot-key :id) slots))
         slot-of (into {} (map (juxt (comp slot-key :id) identity)) slots)
         params (fn [card] (when params-of (some->> (slot-of (slot-key (:slot-id card))) params-of (keep pin-key) set)))]
     (cond
       (nil? cards) nil
       (not= :macro mode)
       [{:path [:cards] :message "Only a macro arranges its cards; other effects keep standard slots"}]
       (not (vector? cards))
       [{:path [:cards] :message "Cards are a vector of {:slot-id :size} in card order"}]
       :else
       (let [problems
             (mapcat
              (fn [i card]
                (cond
                  (not (and (map? card) (contains? card :slot-id) (contains? card :size)
                            (every? #{:slot-id :size :pins} (keys card))))
                  [{:path [:cards i] :message "A card names its :slot-id and :size, and optionally its :pins"}]
                  (not (contains? slot-ids (slot-key (:slot-id card))))
                  [{:path [:cards i :slot-id] :message (str "A card names a " item " of this " noun)}]
                  (not (some #{(:size card)} sizes))
                  [{:path [:cards i :size] :message "A card size is :small, :medium or :large"}]
                  :else (pin-errors i (:pins card) (params card))))
              (range) cards)
             repeated (->> cards (filter map?) (map (comp slot-key :slot-id)) frequencies
                           (keep (fn [[id n]] (when (< 1 n) id))) seq)]
         (not-empty (cond-> (vec problems)
                      repeated (conj {:path [:cards] :message (str "A " item " has one card: " (str/join ", " repeated))}))))))))

(defn arrange
  "Every member card of a macro in card order: listed cards first, then the
  remaining slots in slot order, each as {:slot-id :member :size}, with the
  card's :pins when it has them. :member is
  the slot's index, which a running member instance carries. Cards naming a
  slot the macro no longer has are skipped."
  [slots cards]
  (let [cards (normalize cards)
        index (into {} (map-indexed (fn [i slot] [(slot-key (:id slot)) i])) slots)
        listed (keep (fn [{:keys [slot-id size pins]}]
                       (when-let [i (index (slot-key slot-id))]
                         (cond-> {:slot-id (:id (nth slots i)) :member i :size (or (some #{size} sizes) default-size)}
                           (and (vector? pins) (seq pins)) (assoc :pins pins))))
                     (when (sequential? cards) (filter map? cards)))
        taken (set (map :member listed))]
    (into (vec listed)
          (keep-indexed (fn [i slot] (when-not (taken i) {:slot-id (:id slot) :member i :size default-size})))
          slots)))

(def ^:private card-keys [:slot-id :size :pins])

(defn- update-card
  "Every card in the current order, with `f` applied to `slot-id`'s card."
  [slots cards slot-id f]
  (mapv (fn [card] (cond-> (select-keys card card-keys)
                     (= (slot-key slot-id) (slot-key (:slot-id card))) f))
        (arrange slots cards)))

(defn resize
  "The cards after giving one slot a size, keeping the current order."
  [slots cards slot-id size]
  (update-card slots cards slot-id #(assoc % :size size)))

(defn set-pins
  "The cards after replacing one slot's pins (nil or empty clears them),
  keeping the order and sizes."
  [slots cards slot-id pins]
  (update-card slots cards slot-id #(if (and (sequential? pins) (seq pins)) (assoc % :pins (vec pins)) (dissoc % :pins))))

(defn pin
  "The cards after pinning `parameter` on one slot's card (appended after its
  current pins; an existing pin keeps its place), or unpinning it when
  `pinned?` is false. Applied to the
  arrangement the show holds, so concurrent edits never undo each other."
  [slots cards slot-id parameter pinned?]
  (update-card slots cards slot-id
               (fn [card]
                 (let [k (pin-key parameter)
                       present? (some #(= k (pin-key %)) (:pins card))
                       pins (cond (and pinned? present?) (:pins card)
                                  pinned? (conj (vec (:pins card)) parameter)
                                  :else (vec (remove #(= k (pin-key %)) (:pins card))))]
                   (if (seq pins) (assoc card :pins pins) (dissoc card :pins))))))

(defn pinned
  "The controls a small card pins, in pin order: each pin selects the first
  control whose `control-keys` (the parameter IDs it writes) include it."
  [controls control-keys pins]
  (let [keyed (mapv (fn [control] [(set (keep pin-key (control-keys control))) control]) controls)]
    (->> pins
         (keep (fn [pin] (let [k (pin-key pin)] (some (fn [[ks control]] (when (contains? ks k) control)) keyed))))
         distinct
         vec)))

(defn move
  "The cards after moving one slot's card `offset` places (negative is
  earlier), clamped to the ends."
  [slots cards slot-id offset]
  (let [arranged (mapv #(select-keys % card-keys) (arrange slots cards))
        from (first (keep-indexed #(when (= (slot-key slot-id) (slot-key (:slot-id %2))) %1) arranged))]
    (if (nil? from)
      arranged
      (let [to (max 0 (min (dec (count arranged)) (+ from offset)))
            card (nth arranged from)
            without (into (subvec arranged 0 from) (subvec arranged (inc from)))]
        (into (conj (subvec without 0 to) card) (subvec without to))))))

(defn size
  "A card size from wire data (a keyword or its name), or nil."
  [value]
  (some #{(tag value)} sizes))

(defn carry
  "The saved cards that still describe a member of the re-saved macro: the
  slot keeps its ID and still names the same cue. Unnamed slots get positional
  IDs (\"slot-N\"), so a card is carried only when its slot's cue matches;
  otherwise it is dropped rather than attached to a different cue."
  [saved-slots cards slots]
  (let [cue-of (fn [slots] (into {} (map (juxt (comp slot-key :id) :cue-id)) slots))
        before (cue-of saved-slots)
        after (cue-of slots)]
    (when (sequential? cards)
      (filterv (fn [{:keys [slot-id]}]
                 (let [k (slot-key slot-id)]
                   (and (contains? after k) (= (get before k) (get after k)))))
               (filter map? cards)))))

(defn prune
  "Cards whose slot is still in `slots`, for an editor that removed a slot."
  [slots cards]
  (let [ids (set (map (comp slot-key :id) slots))]
    (when (sequential? cards)
      (filterv #(and (map? %) (contains? ids (slot-key (:slot-id %)))) cards))))

(defn prune-pins
  "Each card's pins named as the member's effect names its parameters, without
  the pins whose parameter the effect no longer declares. `params-of` maps a
  slot to its effect's parameter IDs, nil when unknown (the pins are kept for
  slot validation to report). A show saved before an effect dropped a
  parameter still loads; only an explicit edit refuses an unknown pin."
  [slots cards params-of]
  (let [slot-of (into {} (map (juxt (comp slot-key :id) identity)) slots)]
    (if (sequential? cards)
      (mapv (fn [card]
              (let [params (when (and (map? card) (vector? (:pins card)))
                             (some->> (slot-of (slot-key (:slot-id card))) params-of))]
                (if-not params
                  card
                  (let [own (into {} (map (juxt pin-key identity)) params)
                        pins (into [] (comp (keep #(own (pin-key %))) (distinct)) (:pins card))]
                    (if (seq pins) (assoc card :pins pins) (dissoc card :pins))))))
            cards)
      cards)))
