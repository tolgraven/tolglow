(ns tolglow.studio.modulation-host-test
  (:require [clojure.test :refer [deftest is]]
            [tolglow.show :as show]))

(defn- apply-at! [runtime type payload time]
  (let [receipt (show/submit! runtime type payload)]
    (show/tick! runtime time)
    (deref receipt 1000 ::missing)))

(deftest musical-modulation-renders-overlays-and-yields-to-human-control
  (let [runtime (show/create-runtime)]
    (try
      (let [id (:instance-id (apply-at! runtime :command {:op :cue/launch :cue-id :ember} 0))
            canonical (get-in @(:state runtime) [:model :instances id :params :level])
            config {:id :rhythm :seed 42 :every {:beats 1/4} :params [:level]
                    :select {:instance-ids [id]} :ranges {:level [0.1 0.4]}}]
        (is (= :accepted (:status (apply-at! runtime :modulation-start config 100))))
        (let [value (get-in (show/snapshot runtime) [:modulation :overrides id :level])]
          (is (<= 0.1 value 0.4))
          (is (= canonical (get-in @(:state runtime) [:model :instances id :params :level])))
          (is (= value (first (get-in (show/snapshot runtime) [:output :values :head-1 :emitted-color]))))
          (show/tick! runtime 150)
          (is (= value (get-in (show/snapshot runtime) [:modulation :overrides id :level]))))
        (apply-at! runtime :command {:op :param/set :instance-id id :param :level :value 0.9
                                    :source {:adapter :web :session-id "human"}} 150)
        (is (= 0.9 (first (get-in (show/snapshot runtime) [:output :values :head-1 :emitted-color]))))
        (is (nil? (get-in (show/snapshot runtime) [:modulation :overrides id :level])))
        (apply-at! runtime :modulation-resume {:instance-id id :params [:level]} 200)
        (apply-at! runtime :command {:op :transport/set :transport {:playing? false}} 200)
        (let [controller (first (get-in @(:state runtime) [:model :modulation :controllers]))]
          (show/tick! runtime 10000)
          (is (= controller (first (get-in @(:state runtime) [:model :modulation :controllers]))))))
      (finally (show/stop! runtime)))))

(deftest modulation-cues-use-normal-held-lifecycle-and-category-selection
  (let [runtime (show/create-runtime)]
    (try
      (let [target (:instance-id (apply-at! runtime :command {:op :cue/launch :cue-id :ember} 0))
            created (apply-at! runtime :modulation-cue
                               {:id :random-level :name "Level rhythm" :page-id :main
                                :modulation {:seed 2 :every {:bars 1} :params [:level]
                                             :select {:categories [:intensity]} :ranges {:level [0.2 0.3]}}} 0)
            launched (apply-at! runtime :command {:op :cue/launch :cue-id :random-level :launch {:mode :held}} 100)
            owner (:instance-id launched)]
        (is (= :accepted (:status created)))
        (is (= :accepted (:status launched)))
        (is (some #{:random-level} (vals (get-in @(:state runtime) [:model :pages :main :cells]))))
        (is (= owner (get-in (show/snapshot runtime) [:modulation :controllers 0 :owner-instance-id])))
        (is (<= 0.2 (get-in (show/snapshot runtime) [:modulation :overrides target :level]) 0.3))
        (apply-at! runtime :command {:op :instance/end :instance-id owner} 200)
        (is (empty? (get-in (show/snapshot runtime) [:modulation :controllers])))
        (is (contains? (get-in @(:state runtime) [:model :instances]) target)))
      (finally (show/stop! runtime)))))

(deftest full-show-restore-keeps-modulation-schedule-and-rebinds-targets
  (let [runtime (show/create-runtime)]
    (try
      (let [id (:instance-id (apply-at! runtime :command {:op :cue/launch :cue-id :ember} 0))]
        (apply-at! runtime :modulation-start {:id :saved :seed 17 :every {:beats 1/2}
                                             :select {:instance-ids [id]} :params [:level]} 100)
        (apply-at! runtime :command {:op :transport/set :transport {:playing? false}} 100)
        (let [saved (show/export-show runtime)
              controller (first (get-in saved [:model :modulation :controllers]))]
          (is (= :accepted (:status (apply-at! runtime :import {:source (pr-str saved)} 200))))
          (let [new-id (:id (first (:instances (show/snapshot runtime))))
                restored (first (get-in @(:state runtime) [:model :modulation :controllers]))]
            (is (not= id new-id))
            (is (= (select-keys controller [:seed :ticks :next-beat :anchor-beat])
                   (select-keys restored [:seed :ticks :next-beat :anchor-beat])))
            (is (= [new-id] (get-in restored [:config :select :instance-ids])))
            (show/tick! runtime 300)
            (is (= (:seed controller) (get-in @(:state runtime) [:model :modulation :controllers 0 :seed])))
            (is (number? (get-in (show/snapshot runtime) [:modulation :overrides new-id :level]))))))
      (finally (show/stop! runtime)))))

(deftest waveform-preview-and-persistence-use-the-versioned-registry
  (let [runtime (show/create-runtime)]
    (try
      (let [preview (show/waveform-action! runtime :preview
                                            {:selection {:selector 0 :choices ["sine" "triangle"]}
                                             :options {:samples 5}})]
        (is (= 5 (count (:samples preview))))
        (is (= 1.0 (get-in preview [:samples 1 :value]))))
      (let [definition {:id :custom :version 1 :name "Flat" :params {} :range [0 1]
                        :expression {:op :const :value 0.25}}
            ack (apply-at! runtime :waveform {:source (pr-str definition)} 0)]
        (is (= :accepted (:status ack)))
        (is (= definition (get-in (show/export-show runtime) [:model :waveforms :waveforms :custom])))
        (is (= :rejected (:status (apply-at! runtime :waveform
                                            {:source (pr-str (assoc-in definition [:expression :value] 0.5))} 0)))))
      (finally (show/stop! runtime)))))
