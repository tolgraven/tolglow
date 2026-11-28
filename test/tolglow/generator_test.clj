(ns tolglow.generator-test
  "Agent generator source runs in a separate worker JVM and can only return
  effect DSL data (#855). Every test goes through the public host API and a
  real worker process; nothing is stubbed. Escape attempts must come back as
  refusals with a clear message, a runaway generator must be stopped within
  its deadline, and a show rendering beside it must publish the same packets."
  (:require [afterglow.control.model :as control]
            [afterglow.output.scheduler :as scheduler]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [tolglow.compiler :as compiler]
            [tolglow.generator :as generator]
            [tolglow.generator.limits :as limits]
            [tolglow.registry :as registry]
            [tolglow.show :as show]))

(def ^:dynamic *sandbox* nil)

(use-fixtures :once
  (fn [run]
    (let [sandbox (generator/open {})]
      (try (binding [*sandbox* sandbox] (run))
           (finally (generator/close! sandbox))))))

(def ^:private session {:scopes #{:control :author-data :author-code} :origin :local-session})

(defn- generate
  ([source] (generate source {}))
  ([source request]
   (generator/generate! *sandbox* (:session request session)
                        (merge {:language :clojure :seed 11 :source source} (dissoc request :session)))))

(defn- message [result] (str/join " | " (map :message (:errors result))))

(defn- refused-with
  "Assert a refusal at `stage` whose message matches `pattern`."
  [result stage pattern]
  (is (= :refused (:status result)) (pr-str result))
  (is (= stage (:stage result)) (pr-str result))
  (is (re-find pattern (message result)) (pr-str result))
  (is (not (contains? result :definition))))

(def ^:private chase-source
  "A worked example: a loop builds one dimmer layer per bar division, with a
  seeded tint and phase offsets."
  "(ns agent.chase (:require [tolglow.effects :as fx]))
   (let [layers (for [i (range 4)]
                  (fx/intensity (fx/multiply (fx/param :level)
                                             (fx/sine {:period {:beats (inc i)} :phase (rand)}))
                                {:blend :max}))]
     (apply fx/effect :agent/seeded-chase
            {:params {:level 0.8 :tint (fx/parameter [(rand) 0.2 (rand)] {:type :color})}
             :targets [:wash]}
            (fx/color (fx/param :tint))
            layers))")

(deftest a-generator-returns-validated-dsl-data
  (let [result (generate chase-source)
        definition (:definition result)]
    (is (= :valid (:status result)) (pr-str result))
    (is (= :agent/seeded-chase (:id definition)))
    (is (= 5 (count (:outputs definition))))
    (is (empty? (registry/validate-definition :effects definition)))
    (is (map? (compiler/compile-plan definition)))
    (is (= (limits/digest definition) (:digest result)))
    (is (= {:tier :generator :language :clojure :seed 11}
           (select-keys (:provenance result) [:tier :language :seed])))
    (is (< (get-in result [:provenance :elapsed-ms]) (:deadline-ms limits/defaults)))
    (testing "the recipe builders are available without a namespace form"
      (is (= :valid (:status (generate "(fx/breathing-wash :agent/breath {:color [0.2 0.1 1.0]})")))))
    (testing "Math is available as pure functions"
      (is (= :valid (:status (generate "(fx/wash :agent/math {:level (Math/abs (Math/sin (/ Math/PI 4)))})")))))))

(deftest the-same-source-and-seed-give-the-same-definition
  (let [first-run (generate chase-source)
        other-seed (generate chase-source {:seed 12})
        pid (generator/worker-pid *sandbox*)
        fresh (generator/open {})]
    (try
      (let [fresh-run (generator/generate! fresh session {:language :clojure :seed 11 :source chase-source})]
        (is (not= pid (generator/worker-pid fresh)) "A second worker process")
        (is (= (:digest first-run) (:digest fresh-run)))
        (is (= (:definition first-run) (:definition fresh-run))))
      (finally (generator/close! fresh)))
    (is (= :valid (:status other-seed)))
    (is (not= (:digest first-run) (:digest other-seed)) "The seed is the only source of randomness")))

(deftest capability-and-origin-gate-the-evaluator
  (let [before (generator/requests *sandbox*)]
    (refused-with (generate chase-source {:session {:scopes #{:control :author-data} :origin :local-session}})
                  :capability #"author-code")
    (doseq [origin [:import :shared-effect :downloaded-profile :followed-profile nil]]
      (refused-with (generate chase-source {:session {:scopes #{:author-code} :origin origin}})
                    :capability #"never runs"))
    (refused-with (generate "1" {:language :javascript}) :request #"not available")
    (refused-with (generate "{}" {:seed nil}) :request #"explicit integer seed")
    (refused-with (generate (apply str "{:a \"" (repeat 20000 "x"))) :request #"limited to 16384")
    (refused-with (generate "{}" {:limits {:deadline :soon}}) :request #"Unknown limits")
    (is (= before (generator/requests *sandbox*)) "No refused request reached the worker")))

(deftest escape-attempts-are-refused-with-a-clear-error
  (doseq [[label source pattern]
          [;; file access
           ["slurp" "(slurp \"/etc/hosts\")" #"Unable to resolve symbol: slurp"]
           ["spit" "(spit \"/tmp/tolglow-generator-escape\" \"x\")" #"Unable to resolve symbol: spit"]
           ["java.io.File" "(java.io.File. \"/etc/hosts\")" #"Unable to resolve classname: java.io.File"]
           ["clojure.java.io" "(require 'clojure.java.io)" #"Could not find namespace clojure.java.io"]
           ["source-fn reads a var's :file" "(def ^{:file \"/etc/hosts\" :line 1} x 1) (clojure.repl/source-fn 'x)"
            #"clojure.repl/source-fn is not allowed"]
           ;; network
           ["URL" "(slurp (java.net.URL. \"http://127.0.0.1:9/\"))" #"Unable to resolve"]
           ["Socket" "(java.net.Socket. \"127.0.0.1\" 9)" #"Unable to resolve classname: java.net.Socket"]
           ;; Java interop
           ["System/exit" "(System/exit 0)" #"Unable to resolve symbol: System/exit"]
           ["Class/forName" "(Class/forName \"java.lang.Runtime\")" #"Unable to resolve"]
           ["instance method" "(.getClass 1)" #"Method getClass on class java.lang.Long not allowed"]
           ["method on an allowed class" "(.length \"abc\")" #"Method length on class java.lang.String not allowed"]
           ["reflective dot form" "(. \"abc\" (getBytes))" #"not allowed"]
           ;; eval escape
           ["eval" "(eval '(+ 1 2))" #"eval is not allowed"]
           ["load-string" "(load-string \"(+ 1 2)\")" #"load-string is not allowed"]
           ["read-string" "(read-string \"#=(+ 1 2)\")" #"read-string is not allowed"]
           ["eval through a var" "(#'clojure.core/eval '(+ 1 2))" #"eval is not allowed"]
           ["eval through resolve" "((resolve 'eval) '(+ 1 2))" #"resolve is not allowed"]
           ["eval through ns-publics" "((get (ns-publics 'clojure.core) 'eval) '(+ 1 2))" #"ns-publics is not allowed"]
           ;; var mutation
           ["alter-var-root on core" "(alter-var-root #'inc (constantly dec))" #"alter-var-root is not allowed"]
           ["alter-var-root on a local var" "(def x 1) (alter-var-root #'x inc)" #"alter-var-root is not allowed"]
           ["set!" "(set! *warn-on-reflection* true)" #"set! is not allowed"]
           ["with-redefs" "(with-redefs [inc dec] (inc 1))" #"with-redefs is not allowed"]
           ["intern" "(intern 'clojure.core 'inc dec)" #"intern is not allowed"]
           ;; threads and blocking
           ["future" "(future 1)" #"Unable to resolve symbol: future"]
           ["Thread" "(.start (Thread. (fn [])))" #"Unable to resolve classname: Thread"]
           ["agent" "(send (agent 0) inc)" #"Unable to resolve symbol"]
           ["promise" "(deref (promise))" #"promise is not allowed"]
           ["locking" "(locking :x 1)" #"locking is not allowed"]
           ;; clock and unseeded randomness
           ["currentTimeMillis" "(System/currentTimeMillis)" #"Unable to resolve symbol: System/currentTimeMillis"]
           ["Instant/now" "(java.time.Instant/now)" #"Unable to resolve"]
           ["time" "(time 1)" #"time is not allowed"]
           ["Math/random" "(Math/random)" #"Method random on java.lang.Math not allowed"]
           ["UUID" "(java.util.UUID/randomUUID)" #"Unable to resolve"]
           ["gensym" "(gensym)" #"gensym is not allowed"]
           ;; host arrays
           ["long-array" "(long-array 100000000)" #"long-array is not allowed"]]]
    (testing label
      (let [result (generate source)]
        (refused-with result :sandbox pattern)
        (when (re-find #"Unable to resolve|not allowed" (message result))
          (is (some :hint (:errors result)) "The refusal says what a generator may use"))))))

(deftest runaway-generators-are-stopped-within-the-deadline
  (let [deadline (:deadline-ms limits/defaults)]
    (doseq [[label source limit]
            [["loop/recur" "(loop [] (recur))" :steps]
             ["an infinite lazy sequence" "(count (range))" :steps]
             ["a loop that catches the interrupt" "(loop [] (try (loop [] (recur)) (catch Exception _ nil)) (recur))" :steps]
             ["deep recursion" "(defn f [n] (inc (f n))) (f 1)" :stack]
             ["unbounded allocation" "(let [big (nth (iterate #(str % %) \"x\") 20)] (dotimes [_ 1000000] (str big big)) {})" :memory]]]
      (testing label
        (let [result (generate source)]
          (refused-with result :sandbox #"steps|recursion|allocated|heap")
          (is (= limit (:limit result)) (pr-str result))
          (is (<= (:elapsed-ms result) deadline) (pr-str result))))))
  (testing "the wall-clock bound stops a generator that does not run out of steps"
    (let [result (generate "(loop [] (recur))" {:limits {:steps 1000000000000 :allocated-bytes 100000000000}})]
      (refused-with result :sandbox #"deadline")
      (is (= :deadline (:limit result)))
      (is (<= (:elapsed-ms result) (:deadline-ms limits/defaults)))))
  (testing "compiled code that never re-enters the interpreter is killed with its process"
    (let [pid (generator/worker-pid *sandbox*)
          result (generate "(let [v (vec (range 3000000))] (sort-by - (shuffle v)) {})"
                           {:limits {:steps 1000000000000 :allocated-bytes 100000000000 :deadline-ms 1500}})]
      (refused-with result :sandbox #"worker process was stopped")
      (is (= :deadline (:limit result)))
      (is (<= (:elapsed-ms result) 1600) (pr-str result))
      (is (not= pid (generator/worker-pid *sandbox*)) "A fresh worker replaced the killed one")))
  (testing "an allocation that outruns the checks exhausts only the worker's heap"
    (let [pid (generator/worker-pid *sandbox*)
          result (generate "(let [s (atom \"x\")] (dotimes [_ 40] (swap! s #(str % %))) {:n (count @s)})")]
      (refused-with result :sandbox #"heap|allocated")
      (is (= :memory (:limit result)))
      (is (<= (:elapsed-ms result) (:deadline-ms limits/defaults)) (pr-str result))
      (when (re-find #"heap" (message result))
        (is (not= pid (generator/worker-pid *sandbox*)) "The exhausted worker was replaced"))))
  (testing "the replacement worker serves the next request"
    (is (= :valid (:status (generate chase-source))))))

(deftest results-that-are-not-valid-dsl-publish-nothing
  (refused-with (generate "[1 2 3]") :result #"one effect definition map")
  (refused-with (generate "(defrecord R [id]) (->R :agent/x)") :result #"Records are not portable")
  (refused-with (generate "{:id :agent/x :version 1 :outputs (list 1)}") :result #"Lists and lazy sequences")
  (refused-with (generate "{:id :agent/x :version 1 :outputs {:dimmer 0.5} :f inc}") :result #"Functions")
  (refused-with (generate "(zipmap (range 30000) (repeat 0))") :result #"more than 20000 values")
  (let [result (generate "{:id :agent/x :version 0 :outputs {:dimmer 0.5}}")]
    (refused-with result :registry #"Version")
    (is (= [:version] (:path (first (:errors result))))))
  (let [result (generate "{:id :agent/x :version 1 :outputs {:dimmer {:op :no-such-op}}}")]
    (refused-with result :compile #"Unknown expression operation")
    (is (= [:outputs 0 :value :op] (:path (first (:errors result))))))
  (testing "a builder's own validation error keeps its path"
    (let [result (generate "(fx/wash :agent/x {:level 7})")]
      (refused-with result :sandbox #"bounds")
      (is (= [:default] (:path (first (:errors result))))))))

;; A show rendering beside a runaway generator -------------------------------

(def ^:private frame-ms 25)

(defn- unsigned [universes]
  (into (sorted-map) (map (fn [[universe data]] [universe (mapv #(bit-and (int %) 255) data)])) universes))

(defn- drive
  "Render frames of a captured show running `definition` at injected times and
  return each published packet."
  [definition frames]
  (let [clock (atom 0) sent (atom [])
        id (:id definition)
        model (control/new-show
               {:registry {:effects {id definition} :cues {id {:id id :effect-id id :version 1}}}
                :rig {:fixtures [{:id :wash :universe 1 :address 1 :channels {:dimmer 1}}]}})
        runtime (show/create-runtime {:model model :workspace-options nil :workers 1 :independent-output? true
                                      :physical-now-ms #(deref clock)
                                      :sink {:send! (fn [universes id] (swap! sent conj [id (unsigned universes)]))}})]
    (try
      (show/submit! runtime {:op :cue/launch :cue-id id})
      (mapv (fn [frame]
              (let [t (* frame-ms (inc frame))]
                (reset! clock t)
                (show/tick! runtime t)
                (let [packet (scheduler/publish! (:publisher runtime) t)]
                  (merge (select-keys packet [:frame-id :time-ms :sent-at-ms]) {:bytes (second (peek @sent))}))))
            (range frames))
      (finally (show/stop! runtime)))))

(deftest a-generated-effect-renders-and-a-runaway-generator-leaves-output-unchanged
  (let [generated (:definition (generate "(fx/effect :agent/pulse {:params {:level 0.8} :targets [:wash]}
                                            (fx/intensity (fx/multiply (fx/param :level) (fx/saw {:period {:beats 1}}))))"))
        frames 40
        baseline (drive generated frames)
        before (generator/requests *sandbox*)
        runaway (future (generate "(loop [] (recur))" {:limits {:steps 1000000000000 :allocated-bytes 100000000000 :deadline-ms 4000}}))
        started (+ (System/nanoTime) 10000000000)]
    (is (map? generated))
    (is (some #(pos? (get-in % [:bytes 1 0] 0)) baseline) "The generated effect lights the fixture")
    (is (< 1 (count (distinct (map :bytes baseline)))) "Its output changes over the bar")
    (loop [] (when (and (<= (generator/requests *sandbox*) before) (< (System/nanoTime) started)) (Thread/sleep 5) (recur)))
    (let [during (drive generated frames)]
      (is (not (realized? runaway)) "The runaway generator was still running while the show rendered")
      (is (= baseline during) "Frame IDs, sample and send times and bytes are unchanged")
      (let [result (deref runaway 10000 ::blocked)]
        (refused-with result :sandbox #"deadline")
        (is (<= (:elapsed-ms result) 4000))))))
