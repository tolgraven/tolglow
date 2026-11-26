(ns tolglow.studio.pretty-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [tolglow.data :as data]
            [tolglow.demo :as demo]
            [tolglow.showcase :as showcase]
            [tolglow.studio.pretty :as pretty]))

;; The same source and expected text are in src/studio/tolglow/studio/pretty_test.cljs,
;; so the JVM host and the browser are held to one layout.
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
             " :blocks {[:outputs :dimmer] {:stages [{:block :lfo/sine"
             "                                        :inputs {:phase 0.25}}]}}"
             " :curve [0 0.125 0.25 0.375 0.5 0.625 0.75 0.875 1 0.875 0.75 0.625 0.5 0.375"
             "         0.25 0.125 0 0.125 0.25 0.375]"
             " :empty []"
             " :list (a b c)"
             " :outputs {:color {:op :const :value [0.725 0.5 0.25]}"
             "           :dimmer {:op :multiply"
             "                    :args [{:op :param :key :level}"
             "                           {:op :sin"
             "                            :args [{:op :multiply"
             "                                    :args [{:op :beat}"
             "                                           {:op :param :key :rate}]}]}]}}"
             " :params {:level {:type :number :default 0.75 :bounds [0 1]}"
             "          :rate {:type :number :default 2 :bounds [0.5 32] :unit :beats}}"
             " :tags #{:color :emission :movement}}"]))

(defn- definitions [model]
  (for [kind [:effects :cues] [id definition] (sort-by (comp str key) (get-in model [:registry kind]))]
    [kind id definition]))

(deftest the-host-and-browser-share-one-layout
  (let [value (data/read-edn golden-source)]
    (is (= golden-text (pretty/edn value)))
    (is (= value (data/read-edn golden-text)))))

(deftest every-demo-and-showcase-definition-reads-back-unchanged
  (let [checked (atom 0)]
    (doseq [[show model] [[:demo (demo/model)] [:showcase (showcase/model)]]
            [kind id definition] (definitions model)
            :let [text (pretty/edn definition)]]
      (testing [show kind id]
        (swap! checked inc)
        (is (= definition (data/read-edn text)))
        ;; Reprinting the text that was read gives the same text: the view is stable.
        (is (= text (pretty/edn (data/read-edn text))))
        (is (str/starts-with? text (str "{:id " (pr-str (:id definition)) "\n :version " (:version definition))))
        (is (re-find #":version\s+(\d+)" text))
        (is (not (str/ends-with? text "\n")))
        (is (not-any? #(or (str/includes? % "\t") (str/ends-with? % " ")) (str/split-lines text)))
        (is (= text (str/join "\n" (str/split text #"\n" -1))))))
    (is (< 40 @checked))))

(deftest collections-that-fit-stay-on-one-line
  (is (= "{:id :wash :version 1 :name \"Wash\" :tags [:color]}"
         (pretty/edn {:tags [:color] :name "Wash" :version 1 :id :wash})))
  (is (= "[]" (pretty/edn [])))
  (is (= "nil" (pretty/edn nil)))
  (testing "a narrower width and a starting column break the same value"
    (is (= "{:id :wash\n :version 1\n :name \"Wash\"\n :tags [:color]}"
           (pretty/edn {:id :wash :version 1 :name "Wash" :tags [:color]} {:width 30})))
    (is (= "{:id :wash\n           :tags [:color]}"
           (pretty/edn {:id :wash :tags [:color]} {:width 30 :column 10})))))

(deftest printing-ignores-the-callers-print-limits
  (binding [*print-length* 2 *print-level* 1]
    (is (= "[1 2 3 [4 [5]]]" (pretty/edn [1 2 3 [4 [5]]])))))
