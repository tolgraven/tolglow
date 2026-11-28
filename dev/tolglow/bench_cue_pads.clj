(ns tolglow.bench-cue-pads
  "Cue pad preview sampling on a paged show (#533): 8 pages of 64 cues, each
  cue with its own costly dimmer graph, two running instances shaped by a
  response, and two readers (a Studio grid and a Push grid) on two of the
  pages. Measures the show owner's tick and the pad sampling pass per frame,
  on the calling thread plus the owner's effect workers. No sockets, devices
  or output."
  (:require [clojure.java.io :as io]
            [clojure.pprint :as pprint]
            [tolglow.bench-show :as bench]
            [tolglow.compiler :as compiler]
            [tolglow.demo :as demo]
            [tolglow.show :as show]
            [tolglow.visualization.cue-pads :as pads])
  (:import [java.lang.management ManagementFactory]
           [java.util.concurrent.atomic AtomicLong]))

(def page-count 8)

(defn page-id [page] (keyword (str "page-" page)))

(defn cue-id [page index] (keyword (str "cue-" page "-" index)))

(defn costly-dimmer
  "A deliberately costly dimmer graph: a sum of `terms` phased sines of the beat."
  [seed terms]
  {:op :multiply
   :args [(demo/constant (/ 1.0 terms))
          {:op :add
           :args (vec (for [k (range terms)]
                        {:op :add
                         :args [(demo/constant 0.5)
                                {:op :multiply
                                 :args [(demo/constant 0.5)
                                        {:op :sin :args [{:op :add :args [(demo/constant (+ (* seed 0.37) (* k 0.61)))
                                                                         {:op :multiply :args [{:op :beat} (demo/constant (+ 1.0 (* 0.25 k)))]}]}]}]}]}))}]})

(def gain
  "A response on the level of the two launched cues."
  {:id :gain :select {:cue-ids [(cue-id 0 0) (cue-id 1 0)]} :params [:level] :operation :multiply :range [0.0 1.0]
   :envelope {:attack {:beats 1}} :state {:started-at-ms 0 :started-at-beat 0}})

(defn paged-model
  "demo/model with `pages` pages of 64 cues. Every cue has its own effect whose
  dimmer is a `terms`-term graph."
  ([] (paged-model {}))
  ([{:keys [pages terms] :or {pages page-count terms 16}}]
   (let [base (demo/model)
         entries (for [page (range pages) index (range 64)]
                   (let [id (cue-id page index)
                         [_ title color swatch] (nth demo/colors (mod index (count demo/colors)))]
                     {:page page :index index
                      :effect {:id id :version 1 :name (str title " " page "/" index) :swatch swatch
                               :params {:level demo/level}
                               :outputs {:color (demo/constant color)
                                         :dimmer {:op :multiply :args [(demo/param :level)
                                                                       (costly-dimmer (+ (* 64 page) index) terms)]}}}
                      :cue {:id id :version 1 :name (str title " " page "/" index) :effect-id id :swatch swatch
                            :params {} :duration {:bars 64} :priority 0}}))]
     (-> base
         (assoc :pages (into {} (for [page (range pages)]
                                  [(page-id page) {:id (page-id page) :name (str "Page " page)
                                                   :cells (into {} (for [{p :page :keys [index]} entries :when (= p page)]
                                                                     [[(mod index 8) (quot index 8)] (cue-id page index)]))}])))
         (assoc-in [:registry :effects] (into {} (map (juxt (comp :id :effect) :effect)) entries))
         (assoc-in [:registry :cues] (into {} (map (juxt (comp :id :cue) :cue)) entries))
         (assoc :responses [gain])))))

(defn studio-query [page] [pads/query-id (page-id page) [0 0] 8])

(defn counting-compiler
  "Wrap compile-effect so each step evaluation of a compiled effect counts."
  [^AtomicLong counter]
  (let [compile-effect compiler/compile-effect]
    (fn [& args]
      (let [step (apply compile-effect args)]
        (fn [context] (.incrementAndGet counter) (step context))))))

(defn- observe-fn []
  ;; The Studio surface reads observe! when the revision has it, present! before.
  (if-let [observe (resolve 'tolglow.visualization.cue-pads/observe!)]
    @observe
    pads/present!))

(defn- thread-counters []
  (let [{:keys [allocated cpu]} (bench/worker-counters)] [allocated cpu]))

(defn- delta [[allocated cpu] f]
  (let [b (allocated) c (cpu) result (f)]
    [result (- (allocated) b) (/ (- (cpu) c) 1e6)]))

(defn run
  "Tick a runtime for warmup + samples frames of 25 ms and sample both readers
  after every commit."
  [{:keys [warmup samples terms] :or {warmup 120 samples 400 terms 16}}]
  (let [runtime (show/create-runtime {:model (paged-model {:terms terms}) :workspace-options nil :worker-timeout-ms 10000})
        service (:cue-pad-previews runtime)
        studio (Object.) push (Object.)
        observe! (observe-fn)
        steps (AtomicLong.)]
    (try
      (with-redefs [compiler/compile-effect (counting-compiler steps)]
        (show/tick! runtime 0)
        (doseq [cue [(cue-id 0 0) (cue-id 1 0) (cue-id 5 9)]]
          (show/submit! runtime {:op :cue/launch :cue-id cue}))
        (show/tick! runtime 25)
        (let [counters (thread-counters)
              frame (fn [i]
                      ;; 25 ms frames with a deterministic -3..+3 ms jitter, as a
                      ;; scheduled show commits them.
                      (let [t (+ (* 25 (+ 2 i)) (- (mod (* 13 i) 7) 3))
                            [_ owner-bytes owner-cpu] (delta counters #(show/tick! runtime t))
                            snapshot @(:state runtime)
                            before (.get steps)
                            [colors pad-bytes pad-cpu]
                            (delta counters #(vector (observe! service studio snapshot (studio-query 0))
                                                     (pads/present! service push snapshot (studio-query 1))))]
                        {:owner-bytes owner-bytes :owner-cpu-ms owner-cpu
                         :pad-bytes pad-bytes :pad-cpu-ms pad-cpu
                         :pad-evaluations (- (.get steps) before)
                         :colored (+ (count (let [c (first colors)] (if (contains? c :colors) (:colors c) c)))
                                     (count (second colors)))}))]
          (dotimes [i warmup] (frame i))
          (let [observations (mapv frame (range warmup (+ warmup samples)))
                dist (fn [k] (bench/distribution (map k observations)))]
            {:frames samples :warmup warmup :terms terms
             :owner {:cpu-ms (dist :owner-cpu-ms) :bytes (dist :owner-bytes)}
             :pads {:cpu-ms (dist :pad-cpu-ms) :bytes (dist :pad-bytes)
                    :evaluations-per-frame (dist :pad-evaluations)
                    :evaluations-total (reduce + (map :pad-evaluations observations))
                    :colored-per-frame (dist :colored)}})))
      (finally
        (pads/release! service studio) (pads/release! service push)
        (show/stop! runtime)))))

(defn -main [& [output & {:as options}]]
  (let [opts (into {} (map (fn [[k v]] [(keyword (subs k 2)) (parse-long v)])) options)
        report {:benchmark :cue-pad-sampling :measured-at (str (java.time.Instant/now))
                :source-revisions {:tolglow (bench/revision ".")
                                   :afterglow (bench/revision (or (System/getenv "AFTERGLOW_ROOT") "../afterglow"))}
                :source-working-trees {:tolglow (bench/source-status ".")}
                :environment {:java (System/getProperty "java.version") :clojure (clojure-version)
                              :processors (.availableProcessors (Runtime/getRuntime))
                              :jvm-arguments (vec (.getInputArguments (ManagementFactory/getRuntimeMXBean)))}
                :notes ["8 pages x 64 cues; every cue has its own effect with a 16-term sine dimmer graph."
                        "Readers: a Studio grid on page 0 and a Push grid on page 1, 8 rows each, sampled after every commit."
                        "Three running instances: two on the visible pages with a response, one on hidden page 5."
                        "Frames are 25 ms apart with a deterministic -3..+3 ms jitter."
                        "Owner = show/tick! on the calling thread plus afterglow runtime workers; pads = both reader passes on the calling thread."
                        "Pad evaluations count compiled effect steps run by the pad sampler (the owner's own steps run outside that window)."]
                :result (run opts)}]
    (if output
      (do (io/make-parents output)
          (with-open [w (io/writer output)] (binding [*out* w] (pprint/pprint report)))
          (println "Wrote" output))
      (pprint/pprint report))))
