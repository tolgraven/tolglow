(ns tolglow.authoring.card-layout
  "Custom card arrangements for macros (#795). By default every running effect
  card and every parameter card takes a standard slot, so effects never get
  free-form sizes. A saved macro (a composition with :mode :macro) may instead
  arrange its members' cards like desktop widgets: each member card is small,
  medium or large, in an order the operator chooses.

  The arrangement is portable data on the macro, `:cards`, a vector of
  `{:slot-id id :size size}` in card order. Members it does not list follow in
  slot order at the default size. The show validates it when a macro is saved
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
  "Cards with each size read as a keyword (a wire string becomes one)."
  [cards]
  (if (sequential? cards)
    (mapv #(if (map? %) (cond-> % (contains? % :size) (update :size tag)) %) cards)
    cards))

(defn errors
  "Validation errors of `cards` against a macro's slots, as
  [{:path :message}], or nil."
  [mode slots cards]
  (let [cards (normalize cards)
        slot-ids (set (map (comp slot-key :id) slots))]
    (cond
      (nil? cards) nil
      (not= :macro mode)
      [{:path [:cards] :message "Only a macro arranges its cards; other effects keep standard slots"}]
      (not (vector? cards))
      [{:path [:cards] :message "Cards are a vector of {:slot-id :size} in card order"}]
      :else
      (let [problems
            (keep-indexed
             (fn [i card]
               (cond
                 (not (and (map? card) (= #{:slot-id :size} (set (keys card)))))
                 {:path [:cards i] :message "A card names exactly its :slot-id and :size"}
                 (not (contains? slot-ids (slot-key (:slot-id card))))
                 {:path [:cards i :slot-id] :message "A card names a slot of this macro"}
                 (not (some #{(:size card)} sizes))
                 {:path [:cards i :size] :message "A card size is :small, :medium or :large"}))
             cards)
            repeated (->> cards (filter map?) (map (comp slot-key :slot-id)) frequencies
                          (keep (fn [[id n]] (when (< 1 n) id))) seq)]
        (not-empty (cond-> (vec problems)
                     repeated (conj {:path [:cards] :message (str "A slot has one card: " (str/join ", " repeated))})))))))

(defn arrange
  "Every member card of a macro in card order: listed cards first, then the
  remaining slots in slot order, each as {:slot-id :member :size}. :member is
  the slot's index, which a running member instance carries. Cards naming a
  slot the macro no longer has are skipped."
  [slots cards]
  (let [cards (normalize cards)
        index (into {} (map-indexed (fn [i slot] [(slot-key (:id slot)) i])) slots)
        listed (keep (fn [{:keys [slot-id size]}]
                       (when-let [i (index (slot-key slot-id))]
                         {:slot-id (:id (nth slots i)) :member i :size (or (some #{size} sizes) default-size)}))
                     (when (sequential? cards) (filter map? cards)))
        taken (set (map :member listed))]
    (into (vec listed)
          (keep-indexed (fn [i slot] (when-not (taken i) {:slot-id (:id slot) :member i :size default-size})))
          slots)))

(defn resize
  "The cards after giving one slot a size, keeping the current order."
  [slots cards slot-id size]
  (mapv (fn [card] (cond-> (select-keys card [:slot-id :size])
                     (= (slot-key slot-id) (slot-key (:slot-id card))) (assoc :size size)))
        (arrange slots cards)))

(defn move
  "The cards after moving one slot's card `offset` places (negative is
  earlier), clamped to the ends."
  [slots cards slot-id offset]
  (let [arranged (mapv #(select-keys % [:slot-id :size]) (arrange slots cards))
        from (first (keep-indexed #(when (= (slot-key slot-id) (slot-key (:slot-id %2))) %1) arranged))]
    (if (nil? from)
      arranged
      (let [to (max 0 (min (dec (count arranged)) (+ from offset)))
            card (nth arranged from)
            without (into (subvec arranged 0 from) (subvec arranged (inc from)))]
        (into (conj (subvec without 0 to) card) (subvec without to))))))

(defn prune
  "Cards whose slot is still in `slots`, for an editor that removed a slot."
  [slots cards]
  (let [ids (set (map (comp slot-key :id) slots))]
    (when (sequential? cards)
      (filterv #(and (map? %) (contains? ids (slot-key (:slot-id %)))) cards))))
