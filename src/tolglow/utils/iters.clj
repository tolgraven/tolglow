(ns tolglow.utils.iters "Apparantly the path to quicker reduce, mapv et al"
  (:import
    (java.util Iterator)))

(defn ensure-iter
  "Returns an iterator for x if it's not already one."
  ^Iterator [x]
  ;; CLJS: Check for nil -> niliter otherwise check if obj has: .hasNext & .next
  (if (instance? Iterator x) x (clojure.lang.RT/iter x)))


(defn multi-iter
  "Returns an iterator that
  - first returns a sequence of all the first elements
  - then a collection of all seconds elements
  - And so on...
  (Effectively providing you with a transposed view of the collection)
  Stops as soon as ONE of the collection doesn't have anymore elements."
  (^Iterator [colls]
    ;; Copied from clojure.lang.MultiIterator
   (let [iters ^{:tag "[Ljava.util.Iterator;"} (into-array Iterator
                                                           (mapv ensure-iter colls))]
     (reify Iterator
       (hasNext [this]
         (let [len (alength iters)]
           (loop [i 0]
             (if (< i len)
               (if (.hasNext ^Iterator (aget iters i))
                 (recur (unchecked-inc i))
                 false)
               true))))
       (next [this]
         (let [len (alength iters)
               arr (object-array len)]
           (dotimes [i len]
             (aset arr i (.next ^Iterator (aget iters i))))
           (seq arr))))))
  (^Iterator [xf colls]
   (clojure.lang.TransformerIterator/createMulti xf (map ensure-iter colls))))

(defmacro gen-inline-multi-iter
  "Generates an multi interator for the given Iterator array and given arity.
   The .next call returns vectors!"
  [arr arity]
  {:pre [(number? arity)]}
  (let [bindings (repeatedly arity #(gensym "it_"))]
    `(let ~(vec (mapcat (fn [bind arity]
                          [bind `(aget ~(with-meta arr {:tag "[Ljava.util.Iterator;"})
                                       ~arity)])
                        bindings (range arity)))
       (reify Iterator
         (~'hasNext [~'this]
           (and ~@(map #(-> `(.hasNext ~(with-meta % {:tag 'Iterator})))
                       bindings)))
         (~'next [~'this]
           [~@(map #(-> `(.next ~(with-meta % {:tag 'Iterator})))
                   bindings)])))))
#_ (macroexpand '(gen-inline-multi-iter arr 2))

(defn multi-iter-unrolled
  "Returns an iterator that
  - first returns a sequence of all the first elements
  - then a collection of all seconds elements
  - And so on...
  (Effectively providing you with a transposed view of the collection)
  Stops as soon as ONE of the collection doesn't have anymore elements."
  (^Iterator [colls]
    ;; Copied from clojure.lang.MultiIterator
   (let [iters ^{:tag "[Ljava.util.Iterator;"} (into-array Iterator (mapv ensure-iter colls))]
     (case (alength iters)
       1 (gen-inline-multi-iter iters 1)
       2 (gen-inline-multi-iter iters 2)
       3 (gen-inline-multi-iter iters 3)
       4 (gen-inline-multi-iter iters 4) ;; for 4 only about 12% faster...
       5 (gen-inline-multi-iter iters 5)
       (reify Iterator
         (hasNext [this]
           (let [len (alength iters)]
             (loop [i 0]
               (if (< i len)
                 (if (.hasNext ^Iterator (aget iters i))
                   (recur (unchecked-inc i))
                   false)
                 true))))
         (next [this]
           (let [len (alength iters)
                 arr (object-array len)]
             (dotimes [i len]
               (aset arr i (.next ^Iterator (aget iters i))))
             (seq arr)))))))
  (^Iterator [xf colls]
   (clojure.lang.TransformerIterator/createMulti xf (map ensure-iter colls))))

(defn multi-iter-lazy
  "Just like mult-iter but lazy."
  ^Iterator [colls]
  (let [iters (map ensure-iter colls)]
    (reify Iterator
      (hasNext [this]
        (loop [xs (seq iters)]
          (if xs
            (if (.hasNext ^Iterator (first xs))
              (recur (next xs))
              false)
            true)))
      (next [this]
        (map #(.next ^Iterator %) iters)))))

(defmacro loop-it
  "Uses iterator to iterate over all given collections in bindings.
   Stops as soon as one collection has no more elements.
   Requires a `:let [<loop-bindings>]` as the last two elements of the
   bindings.
   body is run with all bindings bound to the element of the collection
   finish is run on the last iteration when there are no more elements.
   Example:
   (loop-it [^long a xs0
             ^long b xs1
             ^long c xs2
             :let [sum 0, prod 1]]
     (recur (+ sum a b c) (* prod a b c)
     {:sum sum, :prod prod})"
  [bindings body finish]
  {:pre [(vector? bindings) (even? (count bindings))
         (vector? (last bindings)) (= :let (last (butlast bindings)))]}
  (let [it-binds (butlast (butlast bindings))
        sym-binds (take-nth 2 it-binds)
        colls (take-nth 2 (rest it-binds))
        iter-syms (repeatedly (count sym-binds) #(gensym "iter"))]
    `(let ~(vec (mapcat (fn [it coll]
                          [it `(ensure-iter ~coll)])
                        iter-syms colls))
       (loop ~(vec (last bindings))
         (if (and ~@(map (fn [it] `(.hasNext ~it)) iter-syms))
           (let ~(vec (mapcat (fn [bind it]
                                [bind `(.next ~it)])
                              sym-binds iter-syms))
             ~body)
           ~finish)))))

;apparently pot many times faster than mapv
(defn mapv-fast "NOTE: Can deal with infinite sequences as arguments.  Just like mapv in core"
  ([f coll]
   (loop-it [x coll, :let [out (conj!)]]
     (recur (conj! out (f x)))
     (persistent! out)))
  ([f c1 c2]
   (loop-it [x1 c1, x2 c2
             :let [out (conj!)]]
     (recur (conj! out (f x1 x2)))
     (persistent! out)))
  ([f c1 c2 c3]
   (loop-it [x1 c1, x2 c2, x3 c3
             :let [out (conj!)]]
     (recur (conj! out (f x1 x2 x3)))
     (persistent! out)))
  ([f c1 c2 c3 & colls]
   (loop-it [xs (if (counted? colls)
                  (multi-iter (list* c1 c2 c3 colls))
                  (multi-iter-lazy (list* c1 c2 c3 colls)))
             :let [out (conj!)]]
     (recur (conj! out (apply f xs)))
     (persistent! out)) ))


(defn mapcatv
  "Mapcat. Eager and returns a vector. Slower if inner collection is small!"
  [f coll]
  (loop-it [xs coll, :let [out (transient [])]]
    (recur (loop-it [x (f xs), :let [out out]]
             (recur (conj! out x))
             out))
    (persistent! out)))
