(ns tolglow.studio.pretty-test
  (:require [cljs.reader :as edn]
            [cljs.test :refer-macros [deftest is testing]]
            [clojure.string :as str]
            [tolglow.studio.events :as events]
            [tolglow.studio.pretty :as pretty]))

;; The same source and expected text are in test/tolglow/studio/pretty_test.clj, so the
;; browser and the JVM host are held to one layout.
(def golden-source
  (str "{:outputs {:color {:op :const :value [0.725 0.5 0.25]} :dimmer {:op :multiply :args [{:op :param :key :level}"
       " {:op :sin :args [{:op :multiply :args [{:op :beat} {:op :param :key :rate}]}]}]}}"
       " :name \"Snow ☃ \\\"quoted\\\"\\nsecond line\\tend\" :id :after-hours"
       " :params {:level {:type :number :default 0.75 :bounds [0 1]} :rate {:type :number :default 2 :bounds [0.5 32] :unit :beats}}"
       " :version 3 :tags #{:emission :color :movement}"
       " :curve [0 0.125 0.25 0.375 0.5 0.625 0.75 0.875 1 0.875 0.75 0.625 0.5 0.375 0.25 0.125 0 0.125 0.25 0.375]"
       " :blocks {[:outputs :dimmer] {:stages [{:block :lfo/sine :inputs {:phase 0.25}}]}} :list (a b c) :empty []"
       " :launch [:mode :persistent :priority 10 :reload-policy :preserve-state :launch-grid :bar :quantize true :label \"A fairly long label\"]}"))

(def golden-text
  (str/join "\n"
            ["{:id :after-hours"
             " :version 3"
             " :name \"Snow ☃ \\\"quoted\\\"\\nsecond line\\tend\""
             " :blocks {[:outputs :dimmer] {:stages [{:block :lfo/sine :inputs {:phase 0.25}}]}}"
             " :curve"
             "   [0 0.125 0.25 0.375 0.5 0.625 0.75 0.875 1 0.875 0.75 0.625 0.5 0.375 0.25 0.125 0 0.125 0.25"
             "    0.375]"
             " :empty []"
             " :launch"
             "   [:mode :persistent"
             "    :priority 10"
             "    :reload-policy :preserve-state"
             "    :launch-grid :bar"
             "    :quantize true"
             "    :label \"A fairly long label\"]"
             " :list (a b c)"
             " :outputs"
             "   {:color {:op :const :value [0.725 0.5 0.25]}"
             "    :dimmer"
             "      {:op :multiply"
             "       :args"
             "         [{:op :param :key :level}"
             "          {:op :sin :args [{:op :multiply :args [{:op :beat} {:op :param :key :rate}]}]}]}}"
             " :params"
             "   {:level {:type :number :default 0.75 :bounds [0 1]}"
             "    :rate {:type :number :default 2 :bounds [0.5 32] :unit :beats}}"
             " :tags #{:color :emission :movement}}"]))

(deftest the-browser-and-host-share-one-layout
  (let [value (edn/read-string golden-source)]
    (is (= golden-text (pretty/edn value)))
    (is (= value (edn/read-string golden-text)))))

(deftest builder-definitions-read-back-unchanged
  (let [builder events/default-builder
        drafts [(edn/read-string events/initial-source)
                (events/builder-definition builder 1)
                (events/builder-definition (assoc builder :keyframes [{:at {:beats 0} :level 0} {:at {:beats 1} :level 1}]
                                                  :blocks {[:outputs :dimmer] {:stages [{:block :lfo/sine}]}}) 4)]]
    (doseq [definition drafts :let [text (pretty/edn definition)]]
      (testing (:id definition)
        (is (= definition (edn/read-string text)))
        (is (= text (pretty/edn (edn/read-string text))))
        (is (str/starts-with? text (str "{:id " (pr-str (:id definition)) "\n :version " (:version definition))))
        (is (= (str (:version definition)) (second (re-find #":version\s+(\d+)" text))))
        (is (= text (str/join "\n" (str/split text #"\n" -1))))
        (is (not (str/ends-with? text "\n")))))))

(deftest keyword-only-vectors-and-sorted-maps-as-on-the-host
  (is (= "[:beam-1 :beam-2 :beam-3\n :beam-4 :beam-5 :beam-6]"
         (pretty/edn [:beam-1 :beam-2 :beam-3 :beam-4 :beam-5 :beam-6] {:width 26})))
  (is (= "{1 2 3 4}" (pretty/edn (sorted-map 3 4 1 2))))
  (is (= "{\"a\" 1 \"b\" 2}" (pretty/edn (sorted-map "b" 2 "a" 1))))
  (is (= "{:id :x :a 1}" (pretty/edn (sorted-map :a 1 :id :x)))))

(deftest json-views-indent-two-spaces
  (is (= "{\n  \"a\": [\n    1,\n    \"b\"\n  ]\n}" (pretty/json {:a [1 :b]})))
  (is (= "\"plain\"" (pretty/json "plain"))))

(deftest length-and-level-elide-as-on-the-host
  (is (= "{:a (0 1 2 ...) :b [1 #] :c #{0 1 2 ...}}"
         (pretty/edn (array-map :a (range) :b [1 [2 [3 [4]]]] :c (into (sorted-set) (range 10))) {:length 3 :level 2}))))

(deftest a-character-budget-stops-the-layout-as-on-the-host
  (let [row (vec (range 64)) value (vec (repeat 64 (vec (repeat 64 row))))
        {:keys [text truncated?]} (pretty/edn-within value {:length 64 :level 10 :max-chars 500})]
    (is truncated?)
    (is (= 500 (count text)))
    (is (= (subs (pretty/edn value {:length 64 :level 10}) 0 500) text))
    (is (= {:text golden-text :truncated? false}
           (pretty/edn-within (edn/read-string golden-source) {:max-chars (count golden-text)})))))

(deftest a-deep-finite-lazy-value-prints-the-whole-budget-as-on-the-host
  ;; #760: realized elements are charged once per call, so depth does not cut
  ;; the text short; an infinitely deep and wide value still stops. A
  ;; ClojureScript (range) is chunked, so up to one 32-element chunk may be
  ;; realized past the allowance before the budget is reached.
  (let [realized (atom 0)
        nest (fn nest [d] (if (zero? d) 0 (map (fn [_] (swap! realized inc) (nest (dec d))) (range 2000))))
        {:keys [text truncated?]} (pretty/edn-within (nest 16) {:max-chars 1024})]
    (is truncated?)
    (is (= 1024 (count text)))
    (is (= "((((((((((((((((0 0" (subs text 0 19)))
    (is (<= @realized (* 64 1025))))
  (let [realized (atom 0)
        node (fn node [] (lazy-seq (map (fn [_] (swap! realized inc) (node)) (range))))
        {:keys [text truncated?]} (pretty/edn-within (node) {:max-chars 1024})]
    (is truncated?)
    (is (<= (count text) 1024))
    (is (<= @realized (+ (* 64 1025) 32)))))
