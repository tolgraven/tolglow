(ns tolglow.bench-video-surfaces
  "Per-frame cost of a surface video output on the lighting thread (#674):
  2,000 strip pixels in contact with an 8 × 4 m wave plus 2,000 heads far
  from it, a 1024 × 576 frame, box filtered. Scenarios: still (cached),
  wave phase moving, crop sliding, and the whole surface moving.

    lein run -m tolglow.bench-video-surfaces"
  (:require [tolglow.compiler :as compiler]
            [tolglow.compiler.video :as video-output]
            [tolglow.video.source :as source]
            [tolglow.video.surface :as surface]))

(defn- fixtures []
  (into (vec (for [r (range 25) i (range 80)]
               {:id (keyword (str "p" r "-" i)) :pixel-layout-id (keyword (str "s" r)) :pixel-index i
                :position [(+ -3.95 (* i 0.1)) (+ 0.2 (* r 0.15)) -3.1] :channels {:red 1 :green 2 :blue 3}}))
        (for [i (range 2000)]
          {:id (keyword (str "far" i)) :position [(- (mod i 40) 20.0) (* 0.1 (quot i 40)) 4.0] :channels {:red 1 :green 2 :blue 3}})))

(def spec {:id :w :source :g :shape :wave :position [0 2 -3.2] :size-m [8 4]
           :wave {:amplitude-m 0.05 :wavelength-m 4} :contact {:distance-m 0.4}})

(defn- median [xs] (nth (vec (sort xs)) (quot (count xs) 2)))

(defn run []
  (let [fixtures (fixtures)
        frame-src (source/test-pattern-frame {:width 1024 :height 576 :pattern :gradient})
        step (compiler/compile-effect (surface/cue-effect :w :w {}))
        frame (fn [] {:time-ms 0 :fixtures fixtures video-output/input-key {:frames {:g frame-src} :surfaces {:w spec}}})
        f (frame)
        go (fn [params] (step {:frame f :instance {:id "i" :started-at-ms 0 :params params} :assignments [] :intents []}))
        time-it (fn [params-fn]
                  (dotimes [i 30] (go (params-fn i)))
                  (median (for [i (range 200)]
                            (let [p (params-fn (+ 30 i)) t0 (System/nanoTime)]
                              (go p) (/ (- (System/nanoTime) t0) 1e6)))))]
    (let [n (count (:assignments (go {})))]
      (println "heads written" n))
    (doseq [[label params-fn] [["still" (fn [_] {})]
                               ["wave phase moving" (fn [i] {:phase (- (mod (* i 0.01) 2.0) 1.0)})]
                               ["crop sliding" (fn [i] {:crop-x (* i 0.0005)})]
                               ["surface moving" (fn [i] {:x (* i 0.0005)})]]]
      (println (format "%-20s %.2f ms median" label (time-it params-fn))))))

(defn -main [& _] (run))
