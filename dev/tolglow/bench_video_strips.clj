(ns tolglow.bench-video-strips
  "Sampling cost for video strip mapping (#672): 2,000 RGB pixels on 25 strips
  of 80, box filtered with a sampling height, against 1024 × 576 and
  1920 × 1080 frames. Reports the planner, the sampler alone, the compiled
  video output (sample, colour map and assignments) and whole headless show
  frames with and without the video cue, in milliseconds.

    lein run -m tolglow.bench-video-strips      (or $W/bin/repl run tolglow.bench-video-strips)

  Show frames run in preview (no output sink) on the CPU batch backend."
  (:require [tolglow.compiler :as compiler]
            [tolglow.compiler.video :as video-output]
            [tolglow.demo :as demo]
            [tolglow.rig.pixel-layout :as layout]
            [tolglow.show :as show]
            [tolglow.video.hub :as video-hub]
            [tolglow.video.sampler :as sampler]
            [tolglow.video.source :as source]))

(def strip-count 25)

(def pixels-per-strip 80)

(defn- layouts []
  (mapv (fn [i] {:id (keyword (str "strip-" i)) :shape :path :color-layout :rgb
                 :universe (inc (quot (* i pixels-per-strip 3) 510)) :address (inc (mod (* i pixels-per-strip 3) 510))
                 :pixels pixels-per-strip :points [[0 (* 0.1 i) 0] [4 (* 0.1 i) 0]]})
        (range strip-count)))

(defn- mapping []
  {:id :wall :source :feed
   :strips (mapv (fn [i] {:id i :layout (keyword (str "strip-" i)) :height 0.02
                          :points [[0.05 (+ 0.02 (* i 0.038))] [0.95 (+ 0.04 (* i 0.038))]]})
                 (range strip-count))})

(defn- stats [xs]
  (let [s (vec (sort xs)) n (count s)]
    {:median (nth s (quot n 2)) :p95 (nth s (min (dec n) (long (* 0.95 n)))) :max (peek s) :n n}))

(defn- timed [n f]
  (dotimes [_ 50] (f))
  (stats (vec (for [_ (range n)]
                (let [t0 (System/nanoTime)] (f) (/ (- (System/nanoTime) t0) 1e6))))))

(defn- round [m] (update-vals m #(if (double? %) (/ (Math/round (* 1000 %)) 1000.0) %)))

(defn -main [& _]
  (let [rig (layout/materialize {:fixtures [] :pixel-layouts (layouts)})
        fixtures (:fixtures rig)
        wall (mapping)
        report
        (into {:pixels (* strip-count pixels-per-strip) :strips strip-count
               :cpus (.availableProcessors (Runtime/getRuntime))
               :java (System/getProperty "java.version")}
              (for [[label w h] [[:frame-1024x576 1024 576] [:frame-1920x1080 1920 1080]]]
                (let [frame (source/test-pattern-frame {:width w :height h :pattern :gradient :source-id :feed})
                      plan (sampler/plan wall fixtures w h nil)
                      step (compiler/compile-effect {:id :v :version 1 :outputs {:color {:op :video :mapping :wall}}})
                      context {:frame {:time-ms 0 :fixtures fixtures
                                       video-output/input-key {:frames {:feed frame} :mappings {:wall wall}}}
                               :instance {:started-at-ms 0} :assignments [] :intents []}
                      ;; A placement that changes every call defeats the plan cache.
                      moving (compiler/compile-effect {:id :v :version 1 :params {:x {:type :number :default 0 :bounds [-1 1]}}
                                                       :outputs {:color {:op :video :mapping :wall :placement {:x {:op :param :key :x}}}}})
                      counter (atom 0)]
                  [label {:taps (alength ^ints (:offsets plan))
                          :plan-ms (round (timed 20 #(sampler/plan wall fixtures w h nil)))
                          :sample-ms (round (timed 600 #(sampler/sample plan frame)))
                          :effect-step-ms (round (timed 600 #(step context)))
                          :effect-step-moving-placement-ms
                          (round (timed 60 #(moving (assoc-in context [:instance :params :x] (* 1e-4 (swap! counter inc))))))}])))
        show-frames
        (let [model (-> (demo/model) (assoc :rig rig)
                        (assoc-in [:settings :video] {:sources {:feed {:kind :test-pattern :width 1024 :height 576 :pattern :gradient}}
                                                      :mappings {:wall wall}}))
              runtime (show/create-runtime {:model model :workspace-options nil})
              frame-ms (fn [t0 n] (stats (vec (for [i (range n)]
                                                (do (show/tick! runtime (+ t0 (* 25 i)))
                                                    (get-in @(:state runtime) [:status :frame-ms]))))))]
          (try
            (video-hub/reconcile! (:video runtime) {:feed {:kind :test-pattern :width 1024 :height 576 :pattern :gradient}})
            (show/submit! runtime :definition {:kind :effects :definition {:id :video-wall :version 1 :outputs {:color {:op :video :mapping :wall}}}})
            (show/submit! runtime :definition {:kind :cues :definition {:id :video-wall :effect-id :video-wall :priority 10}})
            (show/submit! runtime :definition {:kind :effects :definition {:id :solid :version 1 :outputs {:color [0.5 0.2 0.1]}}})
            (show/submit! runtime :definition {:kind :cues :definition {:id :solid :effect-id :solid :priority 10}})
            (show/tick! runtime 0)
            (let [idle (frame-ms 25 300)
                  launch (show/submit! runtime {:op :cue/launch :cue-id :solid})
                  _ (show/tick! runtime 9000)
                  solid-id (:instance-id @launch)
                  solid (frame-ms 10000 600)
                  _ (show/submit! runtime {:op :instance/end :instance-id solid-id :force? true})
                  _ (show/submit! runtime {:op :cue/launch :cue-id :video-wall})
                  video (frame-ms 30000 600)]
              {:idle-show-frame-ms (round idle)
               :solid-colour-cue-show-frame-ms (round solid)
               :video-cue-show-frame-ms (round video)})
            (finally (show/stop! runtime))))]
    (prn (assoc report :show show-frames))))
