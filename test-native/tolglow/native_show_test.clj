(ns tolglow.native-show-test
  (:require [afterglow.render.gpu.expressions :as expressions]
            [afterglow.render.gpu.session :as gpu]
            [afterglow.render.gpu.movement :as movement]
            [clojure.test :refer [deftest is]]
            [tolglow.application :as app]
            [tolglow.demo :as demo]
            [tolglow.show :as show]))

(def wash {:id :shader-wash :version 1
           :params {:gain {:type :number :default 0.5 :bounds [0.0 1.0]}}
           :outputs {:color [0.5 0.25 0.0] :dimmer {:op :param :key :gain}}})

(def move-head {:id :shader-motion :version 1
                :outputs {:aim {:op :vec3 :args [{:op :position :axis :x} 0.5 2.0]}
                          :focus 0.75}})

(defn- run-commands! [runtime tick commands]
  (let [replies (mapv #(show/submit! runtime :command %) commands)]
    (show/tick! runtime tick)
    (mapv #(deref % 1000 ::timeout) replies)))

(defn- launch! [runtime]
  (let [commands (mapcat (fn [{:keys [id] :as definition}]
                           [{:op :registry/publish :kind :effects :definition definition}
                            {:op :cue/put :cue {:id id :effect-id id :reload-policy :preserve-state}}
                            {:op :cue/launch :cue-id id :launch {:mode :persistent}}]) [wash move-head])]
    (run-commands! runtime 0.0 commands)))

(defn- capture [frames]
  {:description {:type :capture}
   :send! (fn [universes frame] (swap! frames assoc frame (update-vals universes vec)))})

(defn- packed [bytes i]
  (+ (* 256 (bit-and 255 (nth bytes i))) (bit-and 255 (nth bytes (inc i)))))

(defn- comparable-frames?
  ([cpu gpu] (comparable-frames? cpu gpu [5]))
  ([cpu gpu axes]
  (and (= (set (keys cpu)) (set (keys gpu)))
       (every?
        (fn [[id {a 1}]]
          (let [b (get-in gpu [id 1]) motor-starts (set (for [fixture (range 8) axis axes] (+ axis (* 12 fixture))))]
            (and (= (count a) (count b) 512)
                 (every? #(<= (abs (- (packed a %) (packed b %))) 1) motor-starts)
                 (every? #(= (get a %) (get b %))
                         (remove (into motor-starts (map inc motor-starts)) (range 512))))))
        cpu))))

(deftest configured-show-owns-and-releases-native-session
  (let [runtime (app/start! {:init {:components [:show] :modules [] :post []}
                             :hz 80 :show {:enabled true :compute-backend :native-gpu}})
        session (get-in runtime [:compute :session])]
    (try
      (is (= :native-gpu (get-in (show/snapshot runtime) [:status :compute :active])))
      (is (= 6 (get-in (show/snapshot runtime) [:status :compute :gpu-budget-ms])))
      (is (string? (:renderer (gpu/status session))))
      (finally (app/stop! runtime)))
    (is (:closed? (gpu/status session)))
    (is (zero? (:resident (gpu/status session))))))

(deftest deferred-pure-steps-and-multi-program-actual-show-output
  (let [cpu-frames (atom {}) gpu-frames (atom {})
        cpu (show/create-runtime {:sink (capture cpu-frames) :workers 2})
        runtime (show/create-runtime {:sink (capture gpu-frames) :workers 2 :compute-backend :native-gpu :gpu-budget-ms 5000
                                      :gpu-options {:max-programs 2 :timeout-ms 500}})
        session (get-in runtime [:compute :session])
        rig (demo/fixtures)
        step ((:compile runtime) wash)
        ctx {:frame {:time-ms 0.0 :fixtures rig :transport {:beat 0.0 :bpm 120.0}}
             :instance {:id "one" :phase :running :started-at-ms 0.0 :params {}}
             :state {} :intents [] :assignments [] :contributions [] :done? false}]
    (try
      (is (= :native-gpu (get-in (show/snapshot runtime) [:status :compute :active])))
      (is (= (step ctx) (step ctx)))
      (is (= (mapv :id rig) (get-in (step ctx) [:contributions 0 :batch :target-ids])))
      (is (get-in (step ctx) [:contributions 0 :batch :field-request :group]))
      (is (zero? (:misses (gpu/status session))) "Computing pure effect requests never opens programs or issues draws")
      (doseq [host [cpu runtime]]
        (is (every? #(= :accepted (:status %)) (launch! host)))
        (show/tick! host 25.0))
      (is (comparable-frames? @cpu-frames @gpu-frames))
      (is (some pos? (map #(bit-and 255 %) (get-in (last (sort @gpu-frames)) [1 1]))))
      (let [status (get-in (show/snapshot runtime) [:status :compute])]
        (is (= 2 (get-in status [:session :resident])))
        (is (pos? (get-in status [:session :hits])))
        (is (pos? (:native-dispatches status)))
        (is (zero? (:cpu-fallbacks status))))
      (doseq [host [cpu runtime]]
        (is (every? #(= :accepted (:status %))
                    (run-commands! host 35.0 [{:op :registry/publish :kind :effects
                                               :definition (-> wash (assoc :version 2)
                                                               (assoc-in [:outputs :color] [0.0 0.125 0.75]))}]))))
      (is (= 2 (:effect-version (first (filter #(= :shader-wash (:cue-id %))
                                               (:instances (show/snapshot runtime)))))))
      (is (comparable-frames? @cpu-frames @gpu-frames))
      (gpu/close! session)
      (doseq [host [cpu runtime]] (show/tick! host 50.0))
      (is (= (val (last (sort @cpu-frames))) (val (last (sort @gpu-frames)))))
      (let [view (show/snapshot runtime)]
        (is (every? (complement :fault) (:instances view)))
        (is (= 2 (count (:instances view))))
        (is (= :cpu (get-in view [:status :compute :active])))
        (is (= :evaluate (get-in view [:status :compute :last-fallback :stage])))
        (is (pos? (get-in view [:status :compute :cpu-fallbacks]))))
      (finally (show/stop! runtime) (show/stop! cpu)))))

(deftest startup-cleanup-closes-an-acquired-session
  (let [acquired (atom nil) open! gpu/open!]
    (with-redefs [gpu/open! (fn [options] (reset! acquired (open! options)))]
      (is (thrown? Exception (show/create-runtime {:compute-backend :native-gpu :model {:rig :invalid}}))))
    (is (:closed? (gpu/status @acquired)))))

(deftest one-frame-budget-discards-late-output-across-show-restart
  (let [frames (atom {})
        runtime (show/create-runtime {:compute-backend :native-gpu :movement-backend :native-gpu :gpu-budget-ms 200
                                      :sink (capture frames) :workers 2})
        session (get-in runtime [:compute :session])
        entered (promise) release (promise) finished (promise)
        draws (atom 0) submit! gpu/submit! run! expressions/run-program!]
    (try
      (is (every? #(= :accepted (:status %)) (launch! runtime)))
      (is (= 2 (count (:instances (show/snapshot runtime)))))
      (with-redefs [gpu/submit! (fn [& args] (swap! draws inc) (apply submit! args))
                    expressions/run-program! (fn [& args]
                                               (deliver entered true)
                                               (deref release 10000 false)
                                               (try (apply run! args)
                                                    (finally (deliver finished true))))]
        (let [tick (future (show/tick! runtime 25.0) :published)]
          (is (= true (deref entered 3000 false)))
          (is (= :published (deref tick 2000 :stalled)))
          (is (= 1 @draws) "The first wait consumes the GPU budget for all groups")
          (is (not (realized? finished)))
          (is (= :timeout (get-in (show/snapshot runtime) [:status :movement :reason])))
          (is (= 8 (get-in (show/snapshot runtime) [:status :movement :cpu-heads])))
          (is (>= (get-in (show/snapshot runtime) [:status :compute :deadline-fallbacks]) 2))
          (is (every? (complement :fault) (:instances (show/snapshot runtime))))
          (is (some pos? (map #(bit-and 255 %) (get (val (last (sort @frames))) 1)))))
        (let [epoch (:epoch (show/snapshot runtime))]
          (is (= :accepted (:status (first (run-commands! runtime 50.0 [{:op :show/restart}])))))
          (is (not= epoch (:epoch (show/snapshot runtime))))
          (is (empty? (:instances (show/snapshot runtime)))))
        (let [after-restart @frames]
          (deliver release true)
          (is (= true (deref finished 3000 false)))
          ;; Fence all older work on this same session before inspecting publication.
          (.get ^java.util.concurrent.Future
                (submit! session (gpu/group {:focus {:op :const :value 0.0}} [[0 0 0]]) {})
                3 java.util.concurrent.TimeUnit/SECONDS)
          (is (= after-restart @frames))))
      (finally (deliver release true) (show/stop! runtime)))))

(deftest movement-is-independent-of-expression-backend-and-resolves-effect-opacity
  (doseq [expressions [:cpu :native-gpu]]
    (let [expected (atom {}) actual (atom {})
          cpu (show/create-runtime {:sink (capture expected)})
          show (show/create-runtime {:sink (capture actual) :compute-backend expressions
                                     :movement-backend :native-gpu :gpu-budget-ms 5000})
          defs [{:id :left :version 1 :targets [:head-1 :head-4]
                 :outputs {:direction [0.25 0.5 1.0] :red 0.75}}
                {:id :right :version 1 :targets [:head-1 :head-4]
                 :outputs {:direction [1.0 0.5 0.25]}}]
          commands (mapcat (fn [{:keys [id] :as definition}]
                             [{:op :registry/publish :kind :effects :definition definition}
                              {:op :cue/put :cue {:id id :effect-id id}}
                              {:op :cue/launch :cue-id id}]) defs)]
      (try
        (doseq [runtime [cpu show]]
          (let [replies (run-commands! runtime 0.0 commands)
                top-id (:instance-id (last replies))]
            (is (every? #(= :accepted (:status %)) replies))
            (doseq [[tick alpha] [[25.0 0.0] [50.0 0.5] [75.0 1.0]]]
              (is (= :accepted (:status (first (run-commands! runtime tick
                                                [{:op :instance/opacity-set :instance-id top-id :value alpha}]))))))))
        (is (= {:requested :native-gpu :backend :native-gpu :gpu-heads 2 :cpu-heads 0}
               (get-in (show/snapshot show) [:status :movement])))
        (is (= expressions (get-in (show/snapshot show) [:status :compute :active])))
        (is (= 3 (count (set (map #(packed (get (val %) 1) 5) (rest (sort @actual)))))))
        (is (comparable-frames? @expected @actual [5 7]))
        (finally (show/stop! show) (show/stop! cpu))))))

(deftest configured-movement-owner-is-closed-on-show-stop
  (let [show (app/start! {:init {:components [:show] :modules [] :post []}
                          :show {:movement-backend :native-gpu}})]
    (is (= :native-gpu (get-in (show/snapshot show) [:status :movement :requested])))
    (app/stop! show)
    (let [probe [{:kind :assignments
                  :assignments [{:target-id :head-1 :attribute :direction :value [0.25 0.5 1.0]}]}]
          owner (get-in show [:compute :movement])
          patch (:compiled @(:patch show))]
      (is (= :closed (get-in (movement/resolve-contributions! owner patch probe) [:movement :reason]))))))
