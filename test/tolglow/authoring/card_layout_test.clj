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
