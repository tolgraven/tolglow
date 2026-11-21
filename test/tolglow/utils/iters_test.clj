(ns tolglow.utils.iters-test
  (:require [clojure.test :refer [deftest is testing]]
            [tolglow.utils.iters :as iters]))

(defn- rows [iterator]
  ;; Realize each row before advancing: multi-iter-lazy's rows consume their
  ;; underlying iterators when realized, so retaining lazy rows is unsafe.
  (loop [result []]
    (if (.hasNext ^java.util.Iterator iterator)
      (recur (conj result (vec (.next ^java.util.Iterator iterator))))
      result)))

(deftest ensure-iter-preserves-existing-position
  (let [iterator (.iterator [10 20 30])]
    (is (= 10 (.next iterator)))
    (is (identical? iterator (iters/ensure-iter iterator)))
    (is (= [20 30] (vec (iterator-seq (iters/ensure-iter iterator)))))))

(deftest ensure-iter-supports-clojure-and-java-inputs
  (doseq [input [[1 2 3] '(1 2 3) (java.util.ArrayList. [1 2 3])
                 (int-array [1 2 3])]]
    (is (= [1 2 3] (vec (iterator-seq (iters/ensure-iter input)))))))

(deftest zip-iterators-stop-at-the-shortest-input
  (doseq [zip [iters/multi-iter iters/multi-iter-unrolled iters/multi-iter-lazy]]
    (testing (str zip)
      (is (= [[1 :a] [2 :b]] (rows (zip [[1 2 3] [:a :b]]))))
      (is (= [[1 :a] [2 :b]] (rows (zip [[1 2] [:a :b :c]]))))
      (is (= [] (rows (zip [[] [1 2 3]]))))
      (is (= [] (rows (zip [[1 2 3] nil])))))))

(deftest unrolled-and-general-zip-have-equivalent-values
  ;; Covers every generated arity plus the general fallback after five inputs.
  (doseq [arity (range 1 8)]
    (let [inputs (mapv #(range % (+ % 3)) (range arity))
          expected (apply mapv vector inputs)]
      (is (= expected (rows (iters/multi-iter inputs))))
      (is (= expected (rows (iters/multi-iter-unrolled inputs)))))))

(deftest transducing-zip-can-filter-and-transform-rows
  (doseq [zip [iters/multi-iter iters/multi-iter-unrolled]]
    (let [xf (comp (map +) (filter even?))]
      (is (= [12 16]
             (vec (iterator-seq (zip xf [[1 2 3 4] [10 10 12 12]]))))))))

(deftest loop-it-carries-multiple-accumulators
  (is (= {:sum 33 :products [10 40]}
         (iters/loop-it [a [1 2 3], b [10 20]
                         :let [sum 0, products []]]
           (recur (+ sum a b) (conj products (* a b)))
           {:sum sum :products products}))))

(deftest mapv-fast-matches-core-for-every-arity
  (doseq [inputs [[[1 2 3]]
                 [[1 2 3] [10 20]]
                 [[1 2] [10 20 30] [100 200 300]]
                 [[1 2] [10 20] [100 200] [1000 2000]]
                 [[1 2] [10 20] [100 200] [1000 2000] [10000 20000]]]]
    (let [actual (apply iters/mapv-fast + inputs)]
      (is (vector? actual))
      (is (= (apply mapv + inputs) actual)))))

(deftest mapv-fast-handles-empty-input-and-bounded-infinite-input
  (is (= [] (iters/mapv-fast inc nil)))
  (is (= [] (iters/mapv-fast + [1 2] [])))
  (is (= [] (iters/mapv-fast + [1] [2] nil)))
  (is (= [] (iters/mapv-fast + [1] [2] [3] [])))
  (is (= [10 21 32] (iters/mapv-fast + (range) [10 20 30])))
  (is (= [60 121] (iters/mapv-fast + (range) [10 20] [20 40] [30 60]))))

(deftest mapv-fast-preserves-values-without-flattening
  (is (= [nil false [:a] {:b 2}]
         (iters/mapv-fast identity [nil false [:a] {:b 2}]))))

(deftest mapcatv-flattens-one-level-and-returns-a-vector
  (let [result (iters/mapcatv identity [[1 2] nil [] [false nil] [[3]]])]
    (is (vector? result))
    (is (= [1 2 false nil [3]] result)))
  (is (= [] (iters/mapcatv vector nil)))
  (is (= [1 1 2 2 3 3]
         (iters/mapcatv #(repeat 2 %) [1 2 3]))))
