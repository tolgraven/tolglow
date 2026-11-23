(ns tolglow.native.repl
  "A per-session Clojure interpreter for Studio's existing serial REPL."
  (:require [sci.core :as sci]
            [tolglow.repl :as repl]))

(defn- symbols-in [ctx current qualifier]
  (sci/binding [sci/ns current]
    (let [target (if qualifier
                   (let [sym (symbol qualifier)]
                     (or (get (sci/eval-form ctx '(clojure.core/ns-aliases clojure.core/*ns*)) sym)
                         (sci/find-ns ctx sym)))
                   current)]
      {:vars (when target
               (sci/eval-form ctx (list 'clojure.core/ns-map (list 'quote (sci/ns-name target)))))
       :describe (fn [v] (if (class? v) {:kind :class} {:kind :var :metadata (meta v)}))
       :namespaces (when-not qualifier (map sci/ns-name (sci/all-ns ctx)))})))

(defn- session-bindings [ctx]
  (let [api (sci/create-ns 'tolglow.repl)
        session-var (sci/new-dynamic-var 'tolglow.repl/*session* nil {:ns api})
        runtime-var (sci/new-dynamic-var 'tolglow.repl/*runtime* nil {:ns api})
        expose (fn [session values]
                 (let [result (repl/expose-locals! session values)]
                   (sci/add-namespace! ctx (symbol (:namespace result)) values)
                   result))]
    (sci/add-namespace! ctx 'tolglow.repl
                        {'*session* session-var '*runtime* runtime-var 'expose-locals! expose
                         'capture-locals! (sci/copy-var repl/capture-locals! api)})
    {:session-var session-var :runtime-var runtime-var}))

(defn evaluator [show-ctx {:keys [namespace bindings]}]
  (let [ctx (sci/fork show-ctx)
        {:keys [session-var runtime-var]} (session-bindings ctx)
        _ (sci/add-namespace! ctx namespace bindings)
        current (atom (sci/find-ns ctx namespace))]
    {:namespace-name #(str (sci/ns-name @current))
     :symbols #(symbols-in ctx @current %)
     :with-reader
     (fn [source eof run-forms]
       (let [reader (sci/reader source)
             next-form #(sci/parse-next ctx reader {:eof eof})
             evaluate (fn [form]
                        (sci/binding [sci/*1 *1 sci/*2 *2 sci/*3 *3 sci/*e *e
                                      session-var repl/*session* runtime-var repl/*runtime*]
                          (sci/eval-form ctx form)))]
         (sci/binding [sci/ns @current sci/out *out* sci/err *err* sci/read-eval false]
           (try (run-forms next-form evaluate)
                (finally (reset! current @sci/ns))))))}))
