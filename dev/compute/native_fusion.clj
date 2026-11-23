(ns compute.native-fusion
  "Compare one retained desktop GL draw with the same ordered field groups rendered independently."
  (:require [afterglow.render.chain :as chain]
            [afterglow.render.compute :as compute]
            [afterglow.render.fields :as fields]
            [afterglow.render.frame :as frame]
            [afterglow.render.gpu.session :as session]
            [afterglow.render.ir :as ir]
            [clojure.data.json :as json]
            [clojure.java.io :as io]
            [compute.fixture :as fixture])
  (:import [java.util Arrays]))

(defn- field-groups [fixtures]
  (let [ids (mapv :id fixtures) positions (mapv :position fixtures)]
    (mapv (fn [i]
            (let [outputs (:outputs (fixture/program i))
                  group (compute/prepare-group outputs positions (ir/compile-batch outputs))]
              {:kind :batch
               :batch {:target-ids ids :blend :replace :opacity (if (= i 17) 0.5 1.0)
                       :field-request {:group group}}}))
          (range 18))))

(defn- live-inputs [groups n]
  (mapv (fn [i block]
          (assoc-in block [:batch :field-request :env]
                    {:params {:red (/ (inc (mod (+ i n) 7)) 8.0)
                              :aim-x (if (even? n) 0.25 -0.75)}}))
        (range) groups))

(defn- cpu-fields [{:keys [cpu-plan positions surface-uvs]} env]
  (ir/evaluate-batch cpu-plan env positions surface-uvs))

(defn- render-universes [patch evaluate groups]
  (:universes (frame/resolve-contributions patch ((fields/frame-evaluator evaluate) groups)
                                           {:include-values? false :compact-values? true})))

(defn- same-bytes? [left right]
  (and (= (set (keys left)) (set (keys right)))
       (every? (fn [u] (Arrays/equals ^bytes (left u) ^bytes (right u))) (keys left))))

(defn- timed [f]
  (let [before (System/nanoTime) v (f)]
    [v (/ (- (System/nanoTime) before) 1e6)]))

(defn- summarize [[cold & warm]]
  (let [xs (vec (sort warm)) p #(nth xs (min (dec (count xs)) (long (Math/ceil (dec (* % (count xs)))))))]
    {:cold-ms cold :warm-median-ms (p 0.5) :warm-p95-ms (p 0.95) :warm-max-ms (peek xs)}))

(defn -main [path & [sample-count]]
  (let [n (parse-long (or sample-count "60"))
        _ (assert (<= 2 n 10000) "Need 2..10000 samples")
        fixtures (fixture/rig) groups (field-groups fixtures)
        patch (frame/compile-patch fixtures) cache (chain/create-cache)]
    (with-open [one (session/open! {:max-programs 32})
                many (session/open! {:max-programs 32})]
      (let [draw (fn [owner] (fn [group env] (session/evaluate! owner (:shader group) env)))
            render #(render-universes patch %1 %2)
            frame (fn [index]
                    (let [blocks (live-inputs groups index)
                          [gold cpu] (timed #(render cpu-fields blocks))
                          [a raw] (timed #(render (draw many) blocks))
                          [b fused] (timed #(let [one-block (chain/fuse cache compute/prepare-group blocks)]
                                              (assert (= 1 (count one-block)))
                                              (render (draw one) one-block)))]
                      (assert (= #{1 2} (set (keys gold))))
                      (assert (and (same-bytes? gold a) (same-bytes? gold b))
                              (str "DMX differs in native sample " index))
                      [cpu raw fused]))
            timings (apply mapv vector (mapv frame (range n)))
            one-status (session/status one) raw-status (session/status many)
            result {:device (System/getenv "DISPLAY") :software-gl (System/getenv "LIBGL_ALWAYS_SOFTWARE")
                    :heads 64 :universes 2 :layers 18 :samples n
                    :cpu (summarize (timings 0)) :unfused (summarize (timings 1)) :fused (summarize (timings 2))
                    :single-session one-status :original-session raw-status}]
        (assert (= n (:draws one-status)))
        (assert (= (* n 18) (:draws raw-status)))
        (io/make-parents path)
        (spit path (json/write-str result))
        (println (pr-str result))))))
