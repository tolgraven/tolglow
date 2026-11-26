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
       " :blocks {[:outputs :dimmer] {:stages [{:block :lfo/sine :inputs {:phase 0.25}}]}} :list (a b c) :empty []}"))

(def golden-text
  (str/join "\n"
            ["{:id :after-hours"
             " :version 3"
             " :name \"Snow ☃ \\\"quoted\\\"\\nsecond line\\tend\""
             " :blocks"
             "   {[:outputs :dimmer] {:stages [{:block :lfo/sine :inputs {:phase 0.25}}]}}"
             " :curve"
             "   [0 0.125 0.25 0.375 0.5 0.625 0.75 0.875 1 0.875 0.75 0.625 0.5 0.375 0.25"
             "    0.125 0 0.125 0.25 0.375]"
             " :empty []"
             " :list (a b c)"
             " :outputs"
             "   {:color {:op :const :value [0.725 0.5 0.25]}"
             "    :dimmer"
             "      {:op :multiply"
             "       :args"
             "         [{:op :param :key :level}"
             "          {:op :sin"
             "           :args"
             "             [{:op :multiply :args [{:op :beat} {:op :param :key :rate}]}]}]}}"
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

(deftest json-views-indent-two-spaces
  (is (= "{\n  \"a\": [\n    1,\n    \"b\"\n  ]\n}" (pretty/json {:a [1 :b]})))
  (is (= "\"plain\"" (pretty/json "plain"))))

(deftest length-and-level-elide-as-on-the-host
  (is (= "{:a (0 1 2 ...) :b [1 #] :c #{0 1 2 ...}}"
         (pretty/edn (array-map :a (range) :b [1 [2 [3 [4]]]] :c (set (range 10))) {:length 3 :level 2}))))
