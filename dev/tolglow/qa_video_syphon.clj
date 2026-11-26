(ns tolglow.qa-video-syphon
  "End-to-end Syphon check for #672 on a Mac with the built video bridge:
  one helper process publishes the labelled grid as a Syphon server, the show's
  video hub starts a second helper that receives it into a TGVF feed, and a
  video cue writes two strips to a captured (preview-only) DMX frame.

    native/VideoBridge/build
    TOLGLOW_VIDEO_BRIDGE=target/video-bridge/TolglowVideoBridge \\
      lein run -m tolglow.qa-video-syphon

  Prints one EDN report and exits nonzero on a failed check. No lighting
  output leaves the process."
  (:require [clojure.java.io :as io]
            [tolglow.demo :as demo]
            [tolglow.rig.pixel-layout :as layout]
            [tolglow.show :as show]
            [tolglow.video.hub :as video-hub]
            [tolglow.video.source :as source])
  (:import [java.util.concurrent TimeUnit]))

(def tubes
  [{:id :tube-a :shape :path :color-layout :rgb :universe 1 :address 1 :pixels 8 :points [[0 0 0] [2 0 0]]}
   {:id :tube-b :shape :path :color-layout :rgb :universe 1 :address 101 :pixels 8 :points [[0 1 0] [2 1 0]]}])

(defn- linear-byte [b]
  (let [c (/ b 255.0)]
    (* 255.0 (if (<= c 0.04045) (/ c 12.92) (Math/pow (/ (+ c 0.055) 1.055) 2.4)))))

(defn -main [& _]
  (let [bridge (or (System/getenv "TOLGLOW_VIDEO_BRIDGE") "target/video-bridge/TolglowVideoBridge")
        _ (when-not (.canExecute (io/file bridge))
            (throw (ex-info "Build native/VideoBridge first or set TOLGLOW_VIDEO_BRIDGE" {:bridge bridge})))
        server (-> (ProcessBuilder. ^java.util.List [bridge "serve" "--name" "Tolglow QA Grid" "--fps" "60"]) .start)
        source-config {:kind :syphon :app "TolglowVideoBridge" :name "Tolglow QA Grid"}
        model (-> (demo/model)
                  (assoc :rig (layout/materialize {:fixtures [] :pixel-layouts tubes}))
                  (assoc-in [:settings :video]
                            {:sources {:syphon source-config}
                             :mappings {:wall {:id :wall :source :syphon
                                               :strips [{:id :a :layout :tube-a :points [[0 0.125] [1 0.125]] :height 0.05}
                                                        {:id :b :layout :tube-b :points [[0 0.625] [1 0.625]] :height 0.05}]}}}))
        runtime (show/create-runtime {:model model :workspace-options nil :video-options {:bridge bridge}})
        checks (atom [])
        check! (fn [name ok & [detail]] (swap! checks conj (cond-> {:check name :passed (boolean ok)} detail (assoc :detail detail))))]
    (try
      (Thread/sleep 500)
      (let [servers (:servers (video-hub/list-syphon (:video runtime)))]
        (check! "helper lists the QA server" (some #(= "Tolglow QA Grid" (:name %)) servers) servers))
      (video-hub/reconcile! (:video runtime) {:syphon source-config})
      (let [deadline (+ (System/currentTimeMillis) 5000)]
        (while (and (< (System/currentTimeMillis) deadline)
                    (nil? (source/latest (video-hub/source-of (:video runtime) :syphon))))
          (Thread/sleep 20)))
      (let [r1 (show/submit! runtime :definition {:kind :effects :definition {:id :video-wall :version 1 :outputs {:color {:op :video :mapping :wall}}}})
            r2 (show/submit! runtime :definition {:kind :cues :definition {:id :video-wall :effect-id :video-wall :priority 10}})]
        (show/tick! runtime 0)
        (check! "definitions accepted" (every? #(= :accepted (:status @%)) [r1 r2])))
      (let [launch (show/submit! runtime {:op :cue/launch :cue-id :video-wall})]
        (show/tick! runtime 25)
        (check! "cue launched" (= :accepted (:status @launch))))
      (show/tick! runtime 50)
      (let [^bytes u (get-in @(:state runtime) [:output :universes 1])
            pixel (fn [address n] (mapv #(bit-and 0xff (aget u (+ (dec address) (* 3 n) %))) (range 3)))
            expected (fn [column row] (mapv linear-byte (source/grid-color column row)))
            near? (fn [a b] (every? #(<= (Math/abs (double %)) 1.5) (map - a b)))
            rows (for [n (range 8)] {:pixel n :a (pixel 1 n) :b (pixel 101 n)})
            status (first (:sources (:video (show/snapshot runtime))))]
        (check! "Syphon frames reach both strips in endpoint order"
                (every? (fn [{:keys [pixel a b]}] (and (near? (expected pixel 0) a) (near? (expected pixel 2) b))) rows)
                (vec rows))
        (check! "source status is live" (and (:connected? status) (not (:stale? status)))
                (select-keys status [:label :width :height :sequence :age-ms :connected? :stale?])))
      (.close (.getOutputStream server))
      (.waitFor server 3 TimeUnit/SECONDS)
      (let [deadline (+ (System/currentTimeMillis) 5000)]
        (loop []
          (show/tick! runtime 75)
          (let [status (first (:sources (:video (show/snapshot runtime))))]
            (if (or (:stale? status) (> (System/currentTimeMillis) deadline))
              (check! "a stopped sender shows as stale and the strips hold the last frame"
                      (and (:stale? status) (pos? (bit-and 0xff (aget ^bytes (get-in @(:state runtime) [:output :universes 1]) 2))))
                      (select-keys status [:connected? :stale? :age-ms]))
              (do (Thread/sleep 50) (recur))))))
      (finally
        (show/stop! runtime)
        (when (.isAlive server) (.destroy server))))
    (let [passed? (every? :passed @checks)]
      (prn {:passed passed? :checks @checks})
      (System/exit (if passed? 0 1)))))
