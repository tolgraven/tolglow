(ns tolglow.parameter-sample-test
  "One resolved parameter sample per committed frame, shared by every consumer (#533)."
  (:require [afterglow.control.devices.push-2 :as push]
            [clojure.test :refer [deftest is testing]]
            [tolglow.composition.response :as response]
            [tolglow.demo :as demo]
            [tolglow.effects.parameter-sources :as sources]
            [tolglow.modulation :as modulation]
            [tolglow.show :as show]
            [tolglow.studio :as studio]
            [tolglow.studio.display :as display]
            [tolglow.visualization.cue-pads :as cue-pads]
            [tolglow.waveforms :as waveforms]))

(def ^:private gain
  {:id :gain :select {:cue-ids [:ember]} :params [:level] :operation :multiply :range [0.0 1.0]
   :envelope {:attack {:beats 1}} :state {:started-at-ms 0 :started-at-beat 0}})

(defn- show-model [{:keys [keyframes?]}]
  (-> (demo/model)
      (update-in [:registry :effects :ember] #(cond-> % keyframes? (assoc :keyframes {:interval {:beats 1}})))
      (assoc-in [:registry :effects :ember :params :spread]
                {:type :number :default 0.5 :bounds [0.0 1.0] :random {:enabled? true}})
      (assoc :responses [gain])))

(defn- runtime [options]
  (show/create-runtime {:model (show-model options) :workspace-options nil :worker-timeout-ms 10000}))

(defn- at! [rt time-ms type body]
  (let [reply (show/submit! rt type body)]
    (show/tick! rt time-ms)
    (deref reply 1000 ::timeout)))

(defn- counted-responses
  "Count compiled response chains and the evaluations that changed a parameter."
  [calls]
  (let [compile-response response/compile-response]
    (fn [spec]
      (swap! calls update :compiles inc)
      (let [evaluate (compile-response spec)]
        (fn [context]
          (let [result (evaluate context)]
            (when (not= (:response-values result) (:response-values context))
              (swap! calls update :evaluations conj (get-in context [:frame :beat])))
            result))))))

(defn- push-projector [rt]
  (:project! (with-redefs [display/browser-display (fn [options] {:open! (constantly {}) :close! (fn [_])
                                                                   :project! (:project! options)})]
               (show/push-browser-display rt {}))))

(defn- push-value [project snapshot instance-id param]
  (let [view (assoc (push/display-view {:selected-instance instance-id :selected-cue :ember
                                         :page-id :main :origin [0 0]} (:model snapshot))
                    :display-mode :parameters)]
    (:effective-value (first (filter #(= param (:param %)) (:parameters (project view)))))))

(defn- every-consumer
  "Read one committed state the way Studio, Push, cue pads, previews and the
  REPL each do, and return the level each reports."
  [rt project instance-id]
  (let [snapshot @(:state rt)
        instance (get-in snapshot [:model :instances instance-id])
        lease (Object.)
        service (:cue-pad-previews rt)]
    (try
      (cue-pads/present! service lease snapshot [cue-pads/query-id :main [0 0] 8])
      {:studio (get-in (studio/stream-view rt snapshot) [:instances 0 :parameter-sources :level :effective-value])
       :push (push-value project snapshot instance-id :level)
       :preview (:level (waveforms/effective-params snapshot instance))
       :repl (get-in (show/parameter-sample rt) [:instances instance-id :params :level])}
      (finally (cue-pads/release! service lease)))))

(deftest consumers-share-one-response-resolution-per-committed-frame
  ;; Before #533 each consumer compiled and ran the response chain itself:
  ;; five compiles and five evaluations per frame for these five readers.
  (doseq [[label options lease? evaluations]
          [["no display lease" {} false 1]
           ["a keyframed instance, which the owner never retains" {:keyframes? true} true 1]
           ["an exact owner sample under a lease" {} true 0]]]
    (testing label
      (let [rt (runtime options)
            release (when lease? (show/lease-response-samples! rt))
            project (push-projector rt)
            calls (atom {:compiles 0 :evaluations []})]
        (try
          (let [{:keys [instance-id status]} (at! rt 0 :command {:op :cue/launch :cue-id :ember})]
            (is (= :accepted status))
            (with-redefs [response/compile-response (counted-responses calls)]
              (show/tick! rt 125)
              (reset! calls {:compiles 0 :evaluations []})
              (is (= {:studio 0.175 :push 0.175 :preview 0.175 :repl 0.175} (every-consumer rt project instance-id)))
              (is (= (repeat evaluations 0.25) (:evaluations @calls)) "one resolution shared by every consumer")
              (is (= evaluations (:compiles @calls)) "the chain is compiled at most once for the frame")
              (testing "a new frame resolves again and moves"
                (show/tick! rt 250)
                (reset! calls {:compiles 0 :evaluations []})
                (is (= {:studio 0.35 :push 0.35 :preview 0.35 :repl 0.35} (every-consumer rt project instance-id)))
                (is (= (repeat evaluations 0.5) (:evaluations @calls))))
              (testing "a live edit invalidates the sample"
                (is (= :accepted (:status (at! rt 375 :command {:op :param/set :instance-id instance-id
                                                                :param :level :value 0.4}))))
                (let [values (every-consumer rt project instance-id)]
                  (is (= 1 (count (set (vals values)))))
                  (is (< (Math/abs (- 0.3 (double (:repl values)))) 1.0e-9))))))
          (finally (when release (release)) (show/stop! rt)))))))

(defn- modulated-run
  "Launch ember with a saw LFO on :level, a seeded random controller on
  :spread and the :gain response, then read every consumer for three frames."
  [seed]
  (let [rt (runtime {})
        project (push-projector rt)
        counts (atom {})
        counting (fn [k f] (fn [& args] (swap! counts update k (fnil inc 0)) (apply f args)))]
    (try
      (let [{:keys [instance-id]} (at! rt 0 :command {:op :cue/launch :cue-id :ember})]
        (is (= :accepted (:status (at! rt 0 :modulation-bind
                                       {:instance-id instance-id :param :level
                                        :binding {:type :lfo :waveform :saw :period {:beats 1} :from 0.0 :to 1.0}}))))
        (is (= :accepted (:status (at! rt 0 :modulation-start {:id :rnd :seed seed :every {:beats 1/2} :params [:spread]}))))
        (with-redefs [sources/sample (counting :parameter-sources sources/sample)
                      modulation/advance (counting :modulation modulation/advance)]
          (let [frames (mapv (fn [time-ms]
                               (show/tick! rt time-ms)
                               (let [snapshot @(:state rt)
                                     studio (get-in (studio/stream-view rt snapshot) [:instances 0 :parameter-sources])
                                     sample (show/parameter-sample rt)]
                                 {:identity (:identity sample)
                                  :params (get-in sample [:instances instance-id :params])
                                  :studio (update-vals (select-keys studio [:level :spread]) :effective-value)
                                  :push {:level (push-value project snapshot instance-id :level)
                                         :spread (push-value project snapshot instance-id :spread)}
                                  :override (get-in snapshot [:resolved-param-overrides instance-id])
                                  :dimmer (Byte/toUnsignedInt (aget ^bytes (get-in snapshot [:output :universes 1]) 0))}))
                             [125 250 375 500])]
            {:frames frames :counts @counts})))
      (finally (show/stop! rt)))))

(deftest stateful-sources-are-sampled-once-per-owner-frame-and-consumers-agree
  (let [{:keys [frames counts]} (modulated-run 7)]
    (is (= {:parameter-sources 4 :modulation 4} counts)
        "Studio, Push and REPL reads never sample a waveform or advance a controller")
    (doseq [{:keys [params studio push override dimmer identity]} frames]
      (testing (str "frame " (:frame-number identity))
        (is (= params (select-keys studio [:level :spread]) push) "every consumer reports the same values")
        (is (= (:spread override) (:spread params)) "the seeded random value is the one the engine read")
        (is (= dimmer (Math/round (* 255.0 (:level params)))) "the level the consumers show is the emitted level")))
    (is (apply distinct? (map (comp :spread :params) [(first frames) (nth frames 2)]))
        "the controller resampled on its half-beat boundary")))

(deftest seeded-modulation-is-deterministic-across-runs
  (let [run #(mapv (juxt :identity :params) (:frames (modulated-run %)))]
    (is (= (run 7) (run 7)))
    (is (not= (map (comp :spread second) (run 7)) (map (comp :spread second) (run 8))))))

(deftest an-edited-or-stale-state-never-reads-the-shared-sample
  (let [rt (runtime {})]
    (try
      (let [{:keys [instance-id]} (at! rt 0 :command {:op :cue/launch :cue-id :ember})
            _ (show/tick! rt 125)
            snapshot @(:state rt)
            instance (get-in snapshot [:model :instances instance-id])]
        (is (some? (::waveforms/frame-sample snapshot)) "the owner commits a shared sample")
        (is (= 0.175 (:level (waveforms/effective-params snapshot instance))))
        (testing "an edited instance, override or model carried with the old sample resolves itself"
          (let [edited (assoc-in instance [:params :level] 0.4)]
            (is (< (Math/abs (- 0.1 (:level (waveforms/effective-params
                                              (assoc-in snapshot [:model :instances instance-id] edited) edited))))
                   1.0e-9)))
          (is (< (Math/abs (- 0.1 (:level (waveforms/effective-params
                                            (assoc-in snapshot [:resolved-param-overrides instance-id :level] 0.4)
                                            instance))))
                 1.0e-9))
          (is (= 0.35 (:level (waveforms/effective-params
                               snapshot (assoc-in instance [:system :last-clock :beat] 0.5)))))))
      (finally (show/stop! rt)))))

(deftest a-push-display-leases-samples-only-while-it-reads-parameters
  (let [rt (runtime {})
        display (with-redefs [display/browser-display (fn [options] {:open! (constantly {}) :close! (fn [_])
                                                                       :project! (:project! options)})]
                  (show/push-browser-display rt {}))
        project (:project! display)
        retained? #(some? (::waveforms/response-frame @(:state rt)))]
    (try
      (let [{:keys [instance-id]} (at! rt 0 :command {:op :cue/launch :cue-id :ember})
            view (fn [mode] (assoc (push/display-view {:selected-instance instance-id :selected-cue :ember
                                                         :page-id :main :origin [0 0]} (:model @(:state rt)))
                                   :display-mode mode))
            handle ((:open! display))]
        (try
          (show/tick! rt 125)
          (is (retained?) "an open display reads parameters until it projects something else")
          (project (view :block-trace))
          (show/tick! rt 150)
          (is (not (retained?)) "a block-trace view reads no parameter values")
          (project (view :parameters))
          (show/tick! rt 175)
          (is (retained?) "returning to parameters leases samples again")
          (finally ((:close! display) handle)))
        (show/tick! rt 200)
        (is (not (retained?)))
        (is (empty? @(:response-sample-readers rt))))
      (finally (show/stop! rt)))))
