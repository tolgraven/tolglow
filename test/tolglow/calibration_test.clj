(ns tolglow.calibration-test
  (:require [tolglow.data :as data]
             [clojure.test :refer [deftest is testing]]
            [clojure.edn :as edn]
            [clojure.data.json :as json]
            [afterglow.render.frame :as render]
            [tolglow.calibration.geometry :as g]
            [tolglow.calibration.session :as scan]
            [tolglow.rig.calibration :as host]
            [tolglow.persistence :as files]
            [tolglow.show :as show]
            [tolglow.studio :as studio]
            [tolglow.demo :as demo]
            [tolglow.output.scheduler :as scheduler]
            [tolglow.studio.web :as web]))

(def fixture (assoc (first (demo/fixtures)) :position [0 0 0] :rotation-deg [180 25 -10]
                    :calibration {:pan [127.5 -127.5] :tilt [127.5 255]}))
(defn close? [a b epsilon] (< (g/norm (g/sub a b)) epsilon))
(def options {:session-id "scan-1" :fixture-ids [:head-1] :world-id "world-1"
              :world-to-rig g/identity-transform :clock {:offset-ms 1000 :uncertainty-ms 2} :armed? true})
(defn session [] (scan/create (demo/model) options 0))

(deftest camera-coordinate-convention-and-rigid-validation
  (let [sample {:pixel [600 250] :intrinsics [500 500 500 500] :depth-m 2
                :camera-to-world g/identity-transform :world-to-rig g/identity-transform}]
    (is (= [0.4 1.0 -2.0] (g/unproject sample)))
    (is (close? [1.4 3 -2] (g/unproject (assoc sample :world-to-rig [1 0 0 1, 0 1 0 2, 0 0 1 0, 0 0 0 1])) 1.0e-8))
    (is (thrown? Exception (g/unproject (assoc sample :depth-m Double/NaN))))
    (is (thrown? Exception (g/rigid-transform! [-1 0 0 0, 0 1 0 0, 0 0 1 0, 0 0 0 1])))
    (is (thrown? Exception (g/rigid-transform! [2 0 0 0, 0 1 0 0, 0 0 1 0, 0 0 0 1])))))

(deftest position-rejects-depth-outlier-and-reports-residual
  (let [fit (g/fit-position (mapv #(hash-map :point % :weight 1) [[1 2 3] [1.01 2 3] [0.99 2 3] [10 20 30]]))]
    (is (:accepted? fit)) (is (= 1 (:rejected fit))) (is (close? [1 2 3] (:position fit) 1.0e-9))
    (is (< 0 (:rms-m fit) 0.01))))

(deftest aim-fit-agrees-with-real-renderer-and-retains-axis-ambiguity
  ;; Oracle: give world-space targets to the actual renderer, then fit its
  ;; resulting physical pan/tilt. No copied Euler forward implementation.
  (let [targets [[1 1 3] [-1 2 3] [2 -1 3] [-2 -1 4] [1 0 4]]
        samples (mapv (fn [target]
                        (let [values (get-in (render/resolve-frame [fixture]
                                                 [{:target-id (:id fixture) :attribute :aim :value target}]) [:values (:id fixture)])]
                          (is (:aim-reachable? values))
                          {:target target :pan (:pan values) :tilt (:tilt values)})) targets)
        fit (g/fit-aim fixture [0 0 0] samples true)
        bad (assoc (first samples) :target [10 10 -20])
        robust (g/fit-aim fixture [0 0 0] (conj samples bad) true)]
    (is (:accepted? fit) (pr-str fit)) (is (= :inverted (:mounting fit)))
    (is (< (:rms-deg fit) 1.0e-5))
    (is (:accepted? robust)) (is (= 1 (:rejected robust)))
    (doseq [{:keys [target pan tilt]} samples]
      (let [values (get-in (render/resolve-frame [(assoc fixture :rotation-deg (:rotation-deg fit))]
                                                [{:target-id (:id fixture) :attribute :aim :value target}]) [:values (:id fixture)])]
        (is (< (Math/abs (- pan (:pan values))) 1.0e-6))
        (is (< (Math/abs (- tilt (:tilt values))) 1.0e-6))))
    (is (= :unknown (:mounting (g/fit-aim fixture [0 0 0] samples false))))
    (is (false? (:accepted? (g/fit-aim fixture [0 0 0] (vec (repeat 3 (first samples))) true))))))

(defn receipt [t step]
  {:epoch 0 :generation 1 :frame-id (long t) :published-at-ms t :eligible? true :dropped-before 0
   :calibration {:session-id "scan-1" :step-index step}})
(defn observation [id time rgb & [kind]]
  {:observation-id id :capture-ms (- time 1000) :world-id "world-1" :track-id "lens-track"
   :kind (or kind :lens) :pixel [500 500] :intrinsics [500 500 500 500] :depth-m 2
   :camera-to-world g/identity-transform :confidence 2 :rgb rgb})
(defn captured-session []
  (reduce (fn [s i]
            (let [start (+ 1000 (* i 2000))
                  s (reduce scan/receipt s (map #(receipt (+ start %) i) (range 0 1501 25)))
                  obs (mapv #(observation (str i "-" %) (+ start 900 % 12) (nth scan/palette i)) (range 0 100 25))]
              (scan/observe s obs))) (session) (range 3)))

(deftest identity-needs-settled-physical-bracket-and-coded-track
  (let [s (captured-session) obs (first (:observations s))]
    (is (:matched? (scan/match-observation s obs)))
    (is (= :unbracketed-exposure (:reason (scan/match-observation (assoc s :receipts []) obs))))
    (is (= :color-mismatch (:reason (scan/match-observation s (assoc obs :rgb [0 1 0])))))
    (doseq [change [#(assoc % :eligible? false) #(assoc % :dropped-before 1) #(update % :generation inc)]]
      (let [s (update s :receipts (fn [rs] (mapv #(if (= 1925 (:published-at-ms %)) (change %) %) rs)))]
        (is (= :presentation-gap-or-modification (:reason (scan/match-observation s obs))))))
    (let [solved (scan/solve s []) candidate (first (:candidates solved))
          proposed (scan/proposed-rig solved (:base-rig s) [:head-1])]
      (is (:reviewable? candidate)) (is (= [0.0 0.0 -2.0] (get-in candidate [:position :position])))
      (is (= (dissoc (first (get-in s [:base-rig :fixtures])) :position)
             (dissoc (first (:fixtures proposed)) :position)))
      (is (thrown? Exception (scan/proposed-rig solved (assoc (:base-rig s) :room {:width 1 :height 1 :depth 1}) [:head-1])))
      (is (= solved (edn/read-string (pr-str solved))))
      (is (false? (get-in (scan/solve (update s :observations #(vec (take 4 %))) []) [:candidates 0 :reviewable?]))))
    (is (= s (scan/observe s (:observations s))))
    (is (thrown? Exception (scan/observe s [(assoc obs :observation-id "new" :world-id "reset-world")])))))

(deftest physical-mailbox-is-bounded-and-refuses-modified-or-failed-output
  (let [time (atom 0) fail (atom false)
        owner (scheduler/create {:now-ms #(deref time) :receipt-capacity 2
                                 :send! (fn [_ _] (when @fail (throw (ex-info "Send failed" {}))))})
        patch (render/compile-patch [(first (demo/fixtures))])
        frame (render/resolve-frame patch [])
        token (assoc (scheduler/begin-frame! owner 0 patch {:mode :hold-fade} false)
                     :calibration {:session-id "scan-1" :step-index 0 :expected-bytes {[1 0] 0}})]
    (try
      (scheduler/complete! owner token frame)
      (dotimes [i 3] (reset! time (* i 25)) (scheduler/publish! owner))
      (is (= 2 (count (scheduler/drain-receipts! owner))))
      (reset! time 100) (scheduler/publish! owner)
      (let [r (first (scheduler/drain-receipts! owner))]
        (is (= 1 (:dropped-before r))) (is (false? (:eligible? r))) (is (= 0 (:sample-at-ms r))))
      (reset! fail true)
      (is (thrown? Exception (scheduler/publish! owner)))
      (is (empty? (scheduler/drain-receipts! owner)))
      (finally (scheduler/close! owner)))))

(deftest studio-calibration-lifecycle-preserves-human-instances-and-portable-evidence
  (let [physical (atom 0) runtime (show/create-runtime {:sink {:send! (fn [_ _])} :independent-output? true
                                                        :physical-now-ms #(deref physical) :workspace-options nil})
        apply! (fn [type payload time]
                 (let [reply (show/submit! runtime type payload)] (swap! physical inc) (show/tick! runtime time) @reply))]
    (try
      (is (= :accepted (:status (apply! :command {:op :cue/launch :cue-id :ember} 0))))
      (let [human (set (keys (get-in @(:state runtime) [:model :instances])))]
        (is (= :accepted (:status (apply! :calibration (assoc options :op :start) 0))))
        (is (= :accepted (:status (apply! :calibration {:op :step :session-id "scan-1" :step-index 0} 0))))
        (is (nil? (get-in @(:state runtime) [:status :error])))
        (scheduler/publish! (:publisher runtime))
        (show/tick! runtime 25)
        (let [scan (host/current (:model @(:state runtime)))]
          (is (= 1 (count (:receipts scan)))) (is (:eligible? (first (:receipts scan))))
          (is (= 2 (count (get-in @(:state runtime) [:model :instances])))))
        (is (= :rejected (:status (apply! :calibration {:op :step :session-id "scan-1" :step-index 1} 25))))
        (is (= :accepted (:status (apply! :calibration {:op :step :session-id "scan-1" :step-index 1} 1100))))
        (is (= :accepted (:status (apply! :calibration {:op :cancel :session-id "scan-1"} 1150))))
        (is (= human (set (keys (get-in @(:state runtime) [:model :instances])))))
        (is (not-any? #(= "rig-calibration/scan-1" %) (keys (get-in @(:state runtime) [:model :registry :effects]))))
        (is (= :accepted (:status (apply! :calibration (assoc options :op :start :session-id "scan-2") 1200))))
        (is (= :accepted (:status (apply! :calibration {:op :step :session-id "scan-2" :step-index 0} 1200))))
        (reset! physical 15100) (show/tick! runtime 1250)
        (is (= :expired (:status (host/current (:model @(:state runtime))))))
        (is (= human (set (keys (get-in @(:state runtime) [:model :instances]))))))
      (finally (show/stop! runtime)))))

(deftest ring-calibration-clock-and-camera-json-contract
  (let [runtime (show/create-runtime {:workspace-options nil})]
    (try
      (let [response ((studio/handler runtime) {:request-method :get :uri "/api/calibration"})
            body (json/read-str (:body response) :key-fn keyword)]
        (is (= 200 (:status response))) (is (false? (:output-capable? body))) (is (number? (:physical-now-ms body))))
      (let [reply (show/submit! runtime :calibration (assoc options :op :start))]
        (show/tick! runtime 0) (is (= :rejected (:status @reply))))
      (is (= :lens (:kind (data/normalize-json {:kind "lens"}))))
      (is (= [:head-1] (:verified-axis-fixture-ids (data/normalize-json {:verified-axis-fixture-ids ["head-1"]}))))
      (finally (show/stop! runtime)))))

(deftest evidence-survives-receipt-ring-rollover-and-restored-output-is-disarmed
  (let [captured (captured-session)
        older (assoc captured :receipts [])]
    (is (get-in (scan/solve older []) [:candidates 0 :reviewable?]))
    (is (seq (get-in older [:observations 0 :evidence :receipt-proof]))))
  (let [runtime (show/create-runtime {:sink {:send! (fn [_ _])} :independent-output? true
                                       :physical-now-ms (constantly 0) :workspace-options nil})]
    (try
      (doseq [payload [(assoc options :op :start) {:op :step :session-id "scan-1" :step-index 0}]]
        (let [reply (show/submit! runtime :calibration payload)] (show/tick! runtime 0) (is (= :accepted (:status @reply)))))
      (let [active (:model @(:state runtime)) restored (host/disarm-restored active)]
        (is (= :suspended (:status (host/current restored))))
        (is (empty? (:instances restored)))
        (is (= (:plan (host/current active)) (:plan (host/current restored))))
        (let [source (files/write-string active)
              reply (show/submit! runtime :import {:source source})]
          (show/tick! runtime 25)
          (is (= :accepted (:status @reply)) (pr-str @reply))
          (is (empty? (get-in @(:state runtime) [:model :instances])))
          (is (= :suspended (:status (host/current (:model @(:state runtime))))))
          (is (= (:plan (host/current active)) (:plan (host/current (:model @(:state runtime))))))))
      (finally (show/stop! runtime)))))
