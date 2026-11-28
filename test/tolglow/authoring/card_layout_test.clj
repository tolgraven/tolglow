(ns tolglow.authoring.card-layout-test
  "Macro card arrangements (#795): sizes and order are validated portable data,
  and every member keeps a card."
  (:require [clojure.test :refer [deftest is testing]]
            [tolglow.authoring.card-layout :as cards]))

(def slots [{:id "slot-0" :cue-id :ember} {:id "slot-1" :cue-id :ice} {:id :third :cue-id :spot}])

(deftest a-macro-arranges-validated-cards
  (testing "valid cards, including a wire keyword ID and a wire string size"
    (is (nil? (cards/errors :macro slots [{:slot-id "slot-1" :size :large} {:slot-id :slot-0 :size "medium"}])))
    (is (nil? (cards/errors :macro slots nil)))
    (is (nil? (cards/errors :macro slots []))))
  (testing "only a macro arranges cards"
    (is (= [[:cards]] (mapv :path (cards/errors :sequence slots [{:slot-id "slot-0" :size :small}])))))
  (testing "shape, slot, size and repetition are each refused with a path"
    (is (= [[:cards]] (mapv :path (cards/errors :macro slots {:slot-id "slot-0" :size :small}))))
    (is (= [[:cards 0]] (mapv :path (cards/errors :macro slots [{:slot-id "slot-0"}]))))
    (is (= [[:cards 0]] (mapv :path (cards/errors :macro slots [{:slot-id "slot-0" :size :small :width 3}]))))
    (is (= [[:cards 0 :slot-id]] (mapv :path (cards/errors :macro slots [{:slot-id "slot-9" :size :small}]))))
    (is (= [[:cards 0 :size]] (mapv :path (cards/errors :macro slots [{:slot-id "slot-0" :size :huge}]))))
    (is (= [[:cards 0 :size]] (mapv :path (cards/errors :macro slots [{:slot-id "slot-0" :size [3 2]}]))))
    (is (= [[:cards]] (mapv :path (cards/errors :macro slots [{:slot-id "slot-0" :size :small}
                                                              {:slot-id :slot-0 :size :large}]))))))

(deftest every-member-gets-a-card-in-card-order
  (testing "listed cards first, the rest in slot order at the default size"
    (is (= [{:slot-id :third :member 2 :size :large}
            {:slot-id "slot-0" :member 0 :size :small}
            {:slot-id "slot-1" :member 1 :size :small}]
           (cards/arrange slots [{:slot-id "third" :size :large}]))))
  (testing "a card for a removed slot is skipped and pruned"
    (is (= 3 (count (cards/arrange slots [{:slot-id "gone" :size :large}]))))
    (is (= [{:slot-id "slot-1" :size :medium}]
           (cards/prune slots [{:slot-id "gone" :size :large} {:slot-id "slot-1" :size :medium}]))))
  (testing "resizing keeps the order and lists every member"
    (is (= [{:slot-id "slot-0" :size :small} {:slot-id "slot-1" :size :medium} {:slot-id :third :size :small}]
           (cards/resize slots nil :slot-1 :medium))))
  (testing "moving clamps at both ends"
    (is (= [:third "slot-0" "slot-1"] (mapv :slot-id (cards/move slots nil :third -5))))
    (is (= ["slot-1" "slot-0" :third] (mapv :slot-id (cards/move slots nil "slot-0" 1))))
    (is (= ["slot-0" "slot-1" :third] (mapv :slot-id (cards/move slots nil :third 1))))))

(def params {"slot-0" #{:level :period :wash/tint} "slot-1" #{:level} "third" #{:level :aim-x :aim-y :aim-z}})

(defn- params-of [slot] (params (cards/slot-key (:id slot))))

(deftest pins-are-validated-member-parameters
  (testing "pins name parameters, by keyword or wire string, namespaced or not"
    (is (nil? (cards/errors :macro slots [{:slot-id "slot-0" :size :small :pins [:period "level" "wash/tint"]}]
                            {:params-of params-of})))
    (is (nil? (cards/errors :macro slots [{:slot-id "slot-0" :size :small :pins []}] {:params-of params-of})))
    (is (nil? (cards/errors :macro slots [{:slot-id "slot-0" :size :small :pins nil}] {:params-of params-of}))))
  (testing "shape is checked on every save; membership only with :params-of"
    (is (= [[:cards 0 :pins]] (mapv :path (cards/errors :macro slots [{:slot-id "slot-0" :size :small :pins :level}]))))
    (is (= [[:cards 0 :pins 1]] (mapv :path (cards/errors :macro slots [{:slot-id "slot-0" :size :small :pins [:level 3]}]))))
    (is (= [[:cards 0 :pins]] (mapv :path (cards/errors :macro slots [{:slot-id "slot-0" :size :small :pins [:level "level"]}]))))
    (is (nil? (cards/errors :macro slots [{:slot-id "slot-0" :size :small :pins [:gone]}])))
    (is (= [[:cards 0 :pins 0]] (mapv :path (cards/errors :macro slots [{:slot-id "slot-0" :size :small :pins [:gone]}]
                                                          {:params-of params-of}))))
    (is (= [[:cards 0 :pins 0]] (mapv :path (cards/errors :macro slots [{:slot-id "slot-1" :size :small :pins [:period]}]
                                                          {:params-of params-of})))
        "a pin names a parameter of its own member, not a sibling's"))
  (testing "a card names only its slot, size and pins"
    (is (= [[:cards 0]] (mapv :path (cards/errors :macro slots [{:slot-id "slot-0" :size :small :pins [:level] :extra 1}]))))))

(deftest pins-travel-with-their-card
  (let [arranged [{:slot-id "slot-1" :size :large} {:slot-id "slot-0" :size :small :pins [:period]}]]
    (testing "arrange, resize and move keep a card's pins"
      (is (= [:period] (:pins (second (cards/arrange slots arranged)))))
      (is (= {:slot-id "slot-0" :size :medium :pins [:period]} (second (cards/resize slots arranged "slot-0" :medium))))
      (is (= {:slot-id "slot-0" :size :small :pins [:period]} (first (cards/move slots arranged :slot-0 -1)))))
    (testing "pin appends, unpin removes, and an empty card drops :pins"
      (is (= [:period :level] (:pins (second (cards/pin slots arranged "slot-0" :level true)))))
      (is (= [:period] (:pins (second (cards/pin slots arranged "slot-0" "period" true))))
          "pinning again moves nothing and repeats nothing")
      (is (not (contains? (second (cards/pin slots arranged :slot-0 "period" false)) :pins)))
      (is (= [:aim-x] (:pins (nth (cards/pin slots arranged :third :aim-x true) 2)))
          "pinning an unlisted member lists its card at the default size")
      (is (= :small (:size (nth (cards/pin slots arranged :third :aim-x true) 2)))))
    (testing "set-pins replaces and clears"
      (is (= [:level :period] (:pins (second (cards/set-pins slots arranged "slot-0" [:level :period])))))
      (is (not (contains? (second (cards/set-pins slots arranged "slot-0" nil)) :pins)))
      (is (not (contains? (second (cards/set-pins slots arranged "slot-0" [])) :pins))))))

(deftest stale-pins-are-pruned-not-refused
  (testing "a save names pins as the effect does and drops parameters it no longer declares"
    (is (= [{:slot-id "slot-0" :size :small :pins [:wash/tint :period]}]
           (cards/prune-pins slots [{:slot-id "slot-0" :size :small :pins ["wash/tint" :gone :period]}] params-of)))
    (is (= [{:slot-id "slot-1" :size :small}]
           (cards/prune-pins slots [{:slot-id "slot-1" :size :small :pins [:period]}] params-of))))
  (testing "an unknown member's pins are left for slot validation"
    (is (= [{:slot-id "nowhere" :size :small :pins [:x]}]
           (cards/prune-pins slots [{:slot-id "nowhere" :size :small :pins [:x]}] params-of))))
  (testing "carry-over keeps the pins of a member whose slot and cue are unchanged"
    (is (= [{:slot-id "slot-0" :size :small :pins [:period]}]
           (cards/carry slots [{:slot-id "slot-0" :size :small :pins [:period]}] slots)))
    (is (= [] (cards/carry slots [{:slot-id "slot-0" :size :small :pins [:period]}]
                           (assoc-in slots [0 :cue-id] :other))))))

(deftest pinned-controls-follow-pin-order
  (let [controls [{:parameter :level} {:parameter :period} {:attribute :aim :components [[:aim-x {}] [:aim-y {}] [:aim-z {}]]}]
        control-keys #(if-let [cs (:components %)] (map first cs) [(:parameter %)])]
    (is (= [{:parameter :period} {:parameter :level}] (cards/pinned controls control-keys [:period "level"])))
    (is (= [(nth controls 2)] (cards/pinned controls control-keys [:aim-y :aim-x]))
        "any component pins its grouped control, once")
    (is (= [] (cards/pinned controls control-keys [:gone])))))
