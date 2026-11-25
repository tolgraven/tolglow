(ns proof.code-cue-test
  (:require [afterglow.control.model :as control]
            [afterglow.render.frame :as render]
            [afterglow.runtime.engine :as engine]
            [clojure.test :refer [deftest is run-tests]]
            [proof.code-cue :as cue]
            [tolglow.compiler.batch :as batch]
            [tolglow.registry :as registry]))

(def fixtures
  [{:id :lamp :position [0 3 0] :universe 1 :address 1
    :channels {:red 1 :green 2 :blue 3 :dimmer 4 :pan [5 6] :tilt [7 8]}
    :calibration {:pan {:min-deg -270 :max-deg 270} :tilt {:min-deg -135 :max-deg 135}}}])

(def red "(let [level (* 0.4 2)] {:name \"Code wash\" :outputs {:color [1 0 0] :dimmer level}})")
(def blue "(let [level (* 0.4 2)] {:name \"Code wash\" :outputs {:color [0 0 1] :dimmer level}})")

(defn initial []
  (control/new-show {:show-id :rehearsal :rig {:fixtures fixtures}
                     :registry (-> (registry/new-registry)
                                   (registry/publish :effects {:id :wash :version 1 :code-cue? true :auto? false
                                                               :code/cue-id :intro :code/source red
                                                               :name "Code wash" :outputs {:color [1 0 0] :dimmer 0.8}})
                                   (registry/publish :cues {:id :intro :version 1 :effect-id :wash :code-cue? true
                                                            :reload-policy :preserve-state}))
                     :pages {:main {:id :main :cells {[0 0] :intro}}}}))

(defn tick [model]
  (engine/advance model {:time-ms (:time-ms model) :fixtures fixtures} (batch/compiler-for-rig fixtures)))

(defn start-cue [model]
  (let [launch (control/apply-command model {:op :cue/launch :cue-id :intro :command-id :launch
                                            :show-id :rehearsal :epoch 0 :time-ms 0.0 :source :test
                                            :launch {:mode :persistent}})]
    [(get-in launch [:ack :instance-id]) (:model (tick (:model launch)))]))

(defn color [frame]
  (get-in (render/resolve-contributions fixtures (:contributions frame)) [:values :lamp :emitted-color]))

(defn reason [f]
  (try (f) nil (catch clojure.lang.ExceptionInfo error (:reason (ex-data error)))))

(deftest one-source-from-the-grid-and-running-instance
  (let [[id model] (start-cue (initial))
        grid-id (get-in model [:pages :main :cells [0 0]])
        instance-id (get-in model [:instances id :cue-id])]
    (is (= :intro grid-id instance-id))
    (is (= {:show-id :rehearsal :epoch 0 :cue-id :intro :cue-version 1
            :effect-id :wash :effect-version 1 :source red}
           (cue/open-source model grid-id) (cue/open-source model instance-id)))
    (is (= :cue-source-unavailable (reason #(cue/open-source model :unknown))))
    (is (= :cue-source-unavailable
           (reason #(cue/open-source (assoc-in model [:registry :cues :another]
                                              {:id :another :effect-id :wash}) :intro))))
    (is (= [0.0 0.0 0.8]
           (get-in (cue/check-draft (initial) :intro blue) [:preview-values :lamp :emitted-color])))))

(deftest invalid-syntax-and-runtime-failure-keep-active-output
  (let [[id model] (start-cue (initial))
        invalid-syntax "(let [level 0.1]"
        throws "(throw (ex-info \"Oops while building the cue\" {}))"
        invalid-aim "(let [target [0 3 0]] {:name \"Bad aim\" :outputs {:aim target}})"
        blocked "(slurp \"missing.clj\")"]
    (is (seq (cue/syntax-diagnostics invalid-syntax)))
    (is (= :extra-form (:reason (first (cue/syntax-diagnostics (str blue " {}"))))))
    (is (thrown? Exception (cue/check-draft model :intro invalid-syntax)))
    (is (empty? (cue/syntax-diagnostics throws)))
    (is (thrown-with-msg? Exception #"Oops while building the cue" (cue/check-draft model :intro throws)))
    (is (empty? (cue/syntax-diagnostics invalid-aim)))
    (is (thrown? Exception (cue/check-draft model :intro invalid-aim)))
    (is (thrown? Exception (cue/check-draft model :intro blocked)))
    (is (= red (:source (cue/open-source model :intro))))
    (is (= 1 (get-in model [:instances id :effect-version])))
    (is (= [0.8 0.0 0.0] (color (tick model))))))

(deftest same-bytes-and-current-cue-required-to-reload
  (let [[id model] (start-cue (initial))
        checked (cue/check-draft model :intro blue)]
    (is (= [0.0 0.0 0.8] (get-in checked [:preview-values :lamp :emitted-color])))
    (is (= [0.8 0.0 0.0] (color (tick model))) "preview is separate from the live model")
    (is (= :draft-changed (reason #(cue/apply-checked model :intro (str blue " ") checked))))
    (is (= :stale-cue (reason #(cue/apply-checked (assoc model :epoch 1) :intro blue checked))))
    (is (= :stale-cue
           (reason #(cue/apply-checked (assoc-in model [:registry :cues :intro :version] 2) :intro blue checked))))
    (is (= :stale-cue
           (reason #(cue/apply-checked (assoc-in model [:rig :fixtures 0 :address] 2) :intro blue checked))))
    (let [commit (cue/apply-checked model :intro blue checked)
          updated (tick (:model commit))
          current (:model updated)]
      (is (= :accepted (get-in commit [:ack :status])))
      (is (= 2 (:effect-version (cue/open-source current :intro))))
      (is (= blue (:source (cue/open-source current :intro))))
      (is (= [id] (vec (keys (:instances current)))))
      (is (= 2 (get-in current [:instances id :effect-version])))
      (is (some #(= :effect/reloaded (:type %)) (:intents updated)))
      (is (= [0.0 0.0 0.8] (color updated)))
      (is (= :stale-cue (reason #(cue/apply-checked current :intro blue checked))))
      (let [prior (cue/previous-source current :intro 1)
            rollback (cue/check-draft current :intro prior)
            restored (tick (:model (cue/apply-checked current :intro prior rollback)))]
        (is (= red prior))
        (is (= 3 (get-in restored [:model :instances id :effect-version])))
        (is (= [0.8 0.0 0.0] (color restored)))))))

(defn -main []
  (let [{:keys [fail error]} (run-tests 'proof.code-cue-test)]
    (when (pos? (+ fail error)) (System/exit 1))))
