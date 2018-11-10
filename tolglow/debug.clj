(ns tolglow.debug
 (:require
  [afterglow.show :as show]
  [clojure.pprint :refer [pprint pp print-table]]
  ;; [clojure.inspector :as inspector :refer [inspect inspect-table inspect-tree]]
  [clojure.reflect :as reflect :refer [reflect]]
  [clojure.tools.namespace.repl :refer [refresh refresh-all]])
(:use
;;  [clojure.tools.trace]
 [clojure.inspector]))

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

;; refactor-nrepl
;; analyzer artifacts config
;; extract-definition find-symbol find-unbound
;; middleware
;; ns.clean-ns ns.dependencies ns.helpers ns.ns-parser ns.rebuild ns.resolve-missing
;; ns.slam.hound.future ns.slam.hound.regrow ns.slam.hound.search
;; plugin
;; rename-file-or-dir
;; stubs-for-interface
;; util

#_(defn tracing []
 (ex-info :ok {:huh *e})
 (trace "tag" (* 2 3)) ;; To trace a value and assign a trace tag
 (deftrace fubar [x v] (+ x v)) ;; To trace a function call and its return value
 (fubar 2 3)

 (trace-forms (+ 1 3) (/ 1 0) (* 5 6)) ;; To identify which form is failing
 (trace-vars tolglow.cue/pan-tilt-lfo) ;; To dynamically trace/untrace specific fns (untrace-vars myown.namespace/fubar)
 (untrace-vars tolglow.cue/pan-tilt-lfo))
