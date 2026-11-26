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
  (testing "a map that does not fit puts every entry on its own line; so does a keyword-argument vector"
    (is (= "{:a 1\n :b 2\n :c 3}" (pretty/edn {:a 1 :b 2 :c 3} {:width 12})))
    (is (= "[:a 1\n :b 2\n :c 3]" (pretty/edn [:a 1 :b 2 :c 3] {:width 12})))
    (is (= "[1 2 3\n 4 5 6]" (pretty/edn [1 2 3 4 5 6] {:width 7}))))
  (is (= "nil" (pretty/edn nil)))
  (testing "a narrower width and a starting column break the same value"
    (is (= "{:id :wash\n :version 1\n :name \"Wash\"\n :tags [:color]}"
           (pretty/edn {:id :wash :version 1 :name "Wash" :tags [:color]} {:width 30})))
    (is (= "{:id :wash\n           :tags [:color]}"
           (pretty/edn {:id :wash :tags [:color]} {:width 30 :column 10})))))

(deftest keyword-only-vectors-and-sets-fill-their-lines
  (testing "a list of IDs is not keyword arguments"
    (is (= "[:beam-1 :beam-2 :beam-3\n :beam-4 :beam-5 :beam-6]"
           (pretty/edn [:beam-1 :beam-2 :beam-3 :beam-4 :beam-5 :beam-6] {:width 26}))))
  (is (= "#{1 :a :b\n  :c}" (pretty/edn #{:a 1 :b :c} {:width 10})))
  (is (= "[:a 1\n :b :c]" (pretty/edn [:a 1 :b :c] {:width 8}))))

(deftest sorted-maps-keyed-by-other-than-keywords-print
  (is (= "{1 2 3 4}" (pretty/edn (sorted-map 3 4 1 2))))
  (is (= "{\"a\" 1 \"b\" 2}" (pretty/edn (sorted-map "b" 2 "a" 1))))
  (is (= "{3 :c 2 :b 1 :a}" (pretty/edn (sorted-map-by > 1 :a 2 :b 3 :c))))
  (is (= "{:id :x :a 1}" (pretty/edn (sorted-map :a 1 :id :x)))))

(deftest printing-ignores-the-callers-print-limits
  (binding [*print-length* 2 *print-level* 1]
    (is (= "[1 2 3 [4 [5]]]" (pretty/edn [1 2 3 [4 [5]]])))))

(deftest a-value-that-does-not-fit-after-its-key-starts-the-next-line
  (let [text (pretty/edn (get-in (demo/model) [:registry :effects :breathe]))
        lines (str/split-lines text)]
    (is (some #{" :outputs"} lines))
    (is (some #{"   {:dimmer"} lines))
    (testing "deep expressions stay inside the width instead of hanging right"
      (is (every? #(<= (count %) pretty/default-width) lines)))
    (testing "values that fit stay after their key"
      (is (some #{" :envelope {:attack {:beats 1/8} :release {:beats 3/2}}"} lines)))))

(deftest length-and-level-elide-like-print-length-and-print-level
  (is (= "{:a (0 1 2 ...) :b [1 #] :c #{0 1 2 ...}}"
         (pretty/edn (array-map :a (range) :b [1 [2 [3 [4]]]] :c (into (sorted-set) (range 10))) {:length 3 :level 2})))
  (is (= "#" (pretty/edn [1] {:level 0})))
  (binding [*print-length* 3 *print-level* 2]
    (is (= (pr-str (array-map :a (range) :b [1 [2 [3]]]))
           (str/replace (pretty/edn (array-map :a (range) :b [1 [2 [3]]]) {:length 3 :level 2}) " :b" ", :b")))))

(defrecord Holder [xs])

(defn- guarded
  "A counter of realized lazy elements that throws past `limit`, so a lost
  bound fails the test instead of exhausting the heap."
  [limit]
  (let [realized (atom 0)]
    {:realized realized
     :tick! (fn [] (when (> (swap! realized inc) limit)
                     (throw (ex-info "Realized past the bound" {:limit limit}))))}))

(deftest a-character-budget-bounds-the-work-not-just-the-text
  ;; Counted rather than timed: an infinite lazy value is realized only as far
  ;; as the budget could show, with or without :length and :level limits.
  (doseq [options [{:max-chars 8192} {:level 10 :max-chars 8192} {:length 64 :level 10 :max-chars 8192}]]
    (let [{:keys [realized tick!]} (guarded (* 64 (inc (:max-chars options))))
          counted (fn [] (map (fn [n] (tick!) n) (range)))
          value (map (fn [_] (map (fn [_] (counted)) (range))) (range))
          {:keys [text truncated?]} (pretty/edn-within value options)]
      (is truncated? (pr-str options))
      (is (= 8192 (count text)) (pr-str options))
      (is (<= @realized (* 2 (inc (:max-chars options)))) (str (pr-str options) " realized " @realized))))
  (testing "an infinitely deep and wide lazy value and an infinite chain stop"
    (let [{:keys [realized tick!]} (guarded (* 64 1025))
          node (fn node [] (lazy-seq (map (fn [_] (tick!) (node)) (range))))
          chain (fn chain [] (lazy-seq (list (chain))))]
      (doseq [[value max-chars] [[(node) 1024] [(chain) 100] [(chain) 8192]]]
        (let [{:keys [text truncated?]} (pretty/edn-within value {:max-chars max-chars})]
          (is truncated?)
          (is (<= (count text) max-chars))
          (is (every? #{\( \space \newline} text))))
      (is (<= @realized (* 64 1025)))))
  (testing "a record holding an infinite seq is cut by the budget too"
    (let [{:keys [text truncated?]} (pretty/edn-within [(->Holder (range))] {:max-chars 200})]
      (is truncated?)
      (is (= 200 (count text)))))
  (testing "a stack overflow from user code is not passed off as truncation"
    (let [runaway (fn runaway [n] (inc (runaway n)))]
      (is (thrown? StackOverflowError
                   (pretty/edn-within [(lazy-seq [(runaway 0)])] {:max-chars 100})))))
  (testing "counted collections print exactly as the unbounded layout"
    (doseq [[value max-chars] [[(set (range 1000)) 100]
                               [(conj (vec (range 1000)) {:a 1}) 100]
                               [(vec (mapcat (fn [i] [(keyword (str "k" i)) i]) (range 500))) 101]
                               [(into {} (map (fn [i] [i (str "v" i)])) (range 2000)) 333]]]
      (is (= {:text (subs (pretty/edn value) 0 max-chars) :truncated? true}
             (pretty/edn-within value {:max-chars max-chars})))))
  (let [row (vec (range 64)) value (vec (repeat 64 (vec (repeat 64 (vec (repeat 64 row))))))
        {:keys [text truncated?]} (pretty/edn-within value {:length 64 :level 10 :max-chars 8192})]
    (is truncated?)
    (is (= 8192 (count text)))
    (testing "under the budget the text is the unbounded layout"
      (let [definition (get-in (demo/model) [:registry :effects :breathe])
            full (pretty/edn definition)]
        (is (= {:text full :truncated? false} (pretty/edn-within definition {:max-chars (count full)})))
        (is (= (str (subs full 0 40) "...") (pretty/edn definition {:max-chars 40})))))))

(deftest an-elided-huge-map-or-set-sorts-only-what-it-shows
  (let [m (into {} (map (fn [i] [(keyword (str "k" i)) i])) (range 1000000))
        s (set (keys m))
        timed (fn [f] (let [started (System/nanoTime) r (f)] [r (/ (- (System/nanoTime) started) 1e6)]))
        [{:keys [text]} map-ms] (timed #(pretty/edn-within m {:length 64 :level 10 :max-chars 8192}))
        [set-text set-ms] (timed #(pretty/edn s {:length 3}))]
    (is (< map-ms 500) (str "map took " map-ms " ms"))
    (is (< set-ms 500) (str "set took " set-ms " ms"))
    (testing "the entries *print-length* would show, in sorted order"
      (let [shown (re-seq #":k\d+" text)]
        (is (= 64 (count shown)))
        (is (= (sort (map str (take 64 (keys m)))) shown))))
    (is (= (str "#{" (str/join " " (sort (map str (take 3 s)))) " ...}") set-text))))
