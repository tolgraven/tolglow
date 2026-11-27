(ns tolglow.qa.block-builder-gradient
  "Author a gradient sample in Studio's block builder and check it in the show (#775).
  A preview-only show serves Studio's release build. Headless Chromium
  (`dev/studio/block-builder-gradient-qa.mjs`) builds a colour-gel stage whose
  transmission samples a gradient parameter at the fixture's x position,
  writes it to the EDN and publishes it. This namespace then reads the
  published definition from the show, as the REPL does, launches it and reads
  each pixel's rendered colour. A second browser pass loads the published
  definition, changes the sample's extension and publishes the revision."
  (:require [afterglow.control.model :as control]
            [clojure.java.io :as io]
            [clojure.java.shell :as shell]
            [tolglow.application :as app]
            [tolglow.show :as show]))

(def rig
  "Five pixels along x from -0.25 to 1.25, so two sit outside the stops."
  (mapv (fn [i x] {:id (keyword (str "p" i)) :position [x 0.5 0.0] :universe 1 :address (inc (* 3 i))
                   :channels {:red 1 :green 2 :blue 3}})
        (range) [-0.25 0.25 0.5 0.75 1.25]))

(def expected-sample
  {:mirror {:op :gradient :key :palette :args [{:op :position :axis :x}] :extend :mirror}
   :repeat {:op :gradient :key :palette :args [{:op :position :axis :x}] :extend :repeat}})

(def expected-colours
  "Red, green and blue stops at 0, 0.5 and 1. Mirror reflects -0.25 to 0.25 and
  1.25 to 0.75; repeat wraps -0.25 to 0.75 and 1.25 to 0.25."
  {:mirror {:p0 [0.5 0.5 0.0] :p1 [0.5 0.5 0.0] :p2 [0.0 1.0 0.0] :p3 [0.0 0.5 0.5] :p4 [0.0 0.5 0.5]}
   :repeat {:p0 [0.0 0.5 0.5] :p1 [0.5 0.5 0.0] :p2 [0.0 1.0 0.0] :p3 [0.0 0.5 0.5] :p4 [0.5 0.5 0.0]}})

(defn- send! [host op payload]
  (let [ack (deref (show/submit! host op payload) 10000 nil)]
    (when-not (= :accepted (:status ack)) (throw (ex-info "QA command failed" {:op op :ack ack})))
    ack))

(defn- browser! [origin phase dir]
  (let [{:keys [out err exit]} (shell/sh "node" "dev/studio/block-builder-gradient-qa.mjs" origin phase (str dir))]
    (print out) (flush)
    (when-not (zero? exit)
      (binding [*out* *err*] (print err) (flush))
      (throw (ex-info "Block builder gradient browser check failed" {:phase phase :exit exit})))))

(defn- close? [expected actual]
  (and (= (count expected) (count actual))
       (every? #(< (Math/abs (double %)) 1.0e-6) (map - expected actual))))

(defn- check! [host extend version]
  (let [definition (some #(when (= :tide-gel (:id %)) %) (:effects (show/snapshot host)))
        sample (get-in definition [:blocks [:outputs :color] :stages 0 :inputs :transmission])]
    (println "Published definition" (:version definition) "samples" (pr-str sample))
    (when-not (and (= version (:version definition)) (= (expected-sample extend) sample))
      (throw (ex-info "The show holds a different definition" {:definition definition})))
    (let [id (:instance-id (send! host :command {:op :cue/launch :cue-id :tide-gel :launch {:mode :persistent}}))]
      (Thread/sleep 400)
      (let [colours (into (sorted-map) (map (fn [[k v]] [k (:emitted-color v)]))
                          (get-in (show/snapshot host) [:output :values]))]
        (println "Rendered colours" extend (pr-str colours))
        (when-not (every? (fn [[k colour]] (close? colour (get colours k))) (expected-colours extend))
          (throw (ex-info "Rendered colours differ from the gradient sample" {:colours colours})))
        (send! host :command {:op :instance/remove :instance-id id})))))

(defn -main [& [directory]]
  (let [dir (io/file (or directory "target/block-builder-gradient-qa"))
        port (parse-long (or (System/getenv "TOLGLOW_QA_PORT") "0"))
        host (app/start! {:init {:components [:show] :modules [:web-server]}
                          :show {:model (assoc (control/new-show {}) :rig {:fixtures rig})
                                 :workspace-options nil :recovery false}
                          :web-server {:host "127.0.0.1" :port port}})]
    (try
      (.mkdirs dir)
      (let [origin (str "http://127.0.0.1:" (get-in host [:application :modules :web-server :port]))]
        (browser! origin "author" dir)
        (send! host :command {:op :registry/publish :kind :cues
                              :definition {:id :tide-gel :effect-id :tide-gel :version 1 :name "Tide gel"}})
        (check! host :mirror 1)
        (browser! origin "edit" dir)
        (check! host :repeat 2)
        (println "The block builder's gradient sample publishes and renders."))
      (finally (app/stop! host) (shutdown-agents)))))
