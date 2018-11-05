(ns tolglow.debug
 (:require
  [afterglow.show :as show]
  [clojure.pprint :refer [pprint]]))

(defmacro det "let with inspected bindings"
 [bindings & body]
 `(let [~@(mapcat (fn [[n v]]
                    (if (or (vector? n) (map? n))
                      [n v]
                      [n v '_ `(println (name '~n) " " ~v)]))
                      ;; [n v '_ `(print (name '~n) "  ") (pprint ~v)]))
                  (partition 2 bindings))]
    ~@body))

(defmacro make-fn
 [m] ;EVIL, just gotta test...
 `(fn [& args#]
    (eval (cons '~m args#))))

(defn profile
 [& serial?]
 (show/stop!)
 (show/profile-show :serial? (or (first serial?) false)))
