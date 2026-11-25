(ns tolglow.spatial.actions-test
  (:require [afterglow.control.model :as control]
            [afterglow.render.frame :as frame]
            [afterglow.runtime.engine :as engine]
            [clojure.test :refer [deftest is]]
            [tolglow.compiler :as compiler]
            [tolglow.compiler.batch :as batch]
            [tolglow.spatial.actions :as actions]
            [tolglow.spatial.targets :as targets]))

(def rig [{:id :head-1 :groups [:head] :universe 1 :address 1 :channels {:dimmer 1}}
          {:id "head-2" :groups [:head] :universe 1 :address 2 :channels {:dimmer 1}}
          {:id :mover :position [0 0 0] :universe 1 :address 3 :channels {:pan [1 2] :tilt [3 4] :strobe 5}}])

(def effect {:id :wash :version 1 :params {:power {:type :number :bounds [0 1] :default 0.5}
                                            :shape {:type :enum :values [:sine :square] :default :square}}
             :outputs {:dimmer {:op :param :key :power}}})

(def cue {:id :sweep :version 1 :effect-id :wash :saved-params {:power 0.6}})

(defn model []
  (assoc (control/new-show {:registry {:effects {:wash effect} :cues {:sweep cue}}}) :rig {:fixtures rig}))

(defn command [m cmd now]
  (control/apply-command m (merge {:time-ms now :show-id (:show-id m) :epoch (:epoch m)
                                   :command-id (str (random-uuid)) :source :test} cmd)))

(defn update-area [m heads bindings now]
  (actions/apply-frame m {:members {:ball heads} :signals {:ball {:speed 0.7 :position [2 1.5 4]}}} bindings now command))

(defn packed [m now batch?]
  (let [patch (frame/compile-patch rig)
        tick (engine/advance (assoc-in m [:transport :beat] (/ now 500.0)) {:time-ms now :fixtures (:fixtures patch)}
                             (if batch? (batch/compiler-for-rig (:fixtures patch)) compiler/compile-effect))
        packets (:universes (frame/resolve-contributions patch (:contributions tick) {}))]
    (mapv #(bit-and 255 %) (take 7 (packets 1)))))

(deftest independent-scoped-cues-release-only-their-own-instances-and-reload-in-place
  (doseq [batch? [true false]]
    (let [m (:model (command (model) {:op :cue/launch :cue-id :sweep :launch {:targets ["head-2"]}} 0))
          binding {:light {:volume-id :ball :action {:cue-id :sweep} :params {:power {:signal :speed}} :force? true}}
          touched (update-area m #{:head-1} binding 10)
          id (get-in touched (into actions/run-path [:leases :light :head-1 :id]))
          revised (:model (command touched {:op :registry/publish :kind :effects
                                             :definition (assoc effect :version 2 :outputs {:dimmer 1.0})} 10))
          ended (update-area revised #{} binding 30)]
      (is (= [:head-1] (get-in touched [:instances id :targets])))
      (is (= cue (get-in touched [:registry :cues :sweep])))
      (is (= effect (get-in touched [:instances id :definition])))
      (is (= [179 153] (subvec (packed touched 10 batch?) 0 2)))
      (is (= [255 153] (subvec (packed revised 20 batch?) 0 2)))
      (is (= [0 153] (subvec (packed ended 30 batch?) 0 2)))
      (is (= 1 (count (:instances ended)))))))

(deftest contact-follows-signal-without-overwriting-a-performers-edit
  (let [b {:follow {:volume-id :ball :action {:preset :aim}}}
        m (update-area (model) #{:mover} b 0)
        id (get-in m (into actions/run-path [:leases :follow :mover :id]))
        followed (actions/apply-frame m {:members {:ball #{:mover}} :signals {:ball {:position [1 5 -3]}}} b 25 command)
        manual (:model (command followed {:op :param/set :instance-id id :param :point :value [2 2 2]} 25))
        exited (update-area manual #{} b 50)]
    (is (= [2.0 1.5 4.0] (get-in m [:instances id :params :point])))
    (is (= [1.0 5.0 -3.0] (get-in followed [:instances id :params :point])))
    (is (= [2 2 2] (get-in exited [:instances id :params :point])))
    (is (= :running (get-in exited [:instances id :phase])))
    (is (nil? (get-in exited (into actions/run-path [:leases :follow :mover]))))))

(deftest ranked-random-contacts-skip-control-code-and-unsupported-fixtures
  (let [m (-> (model)
              (assoc-in [:registry :cues :bad] (assoc cue :id :bad :show-control? true))
              (assoc-in [:registry :effects :wrong] {:id :wrong :version 1 :outputs {:fog 1}})
              (assoc-in [:registry :cues :only-fog] {:id :only-fog :version 1 :effect-id :wrong}))
        b {:spawn {:volume-id :ball :action {:random {:kind :cues :limit 2 :seed 15}} :duration {:beats 1/2}}}
        started (update-area m #{:head-1} b 0)
        left (update-area started #{} b 25)
        i (first (vals (:instances left)))]
    (is (= :sweep (:cue-id i)))
    (is (= :one-shot (:launch-mode i)))
    (is (= :running (:phase i)))
    (is (= [0 0] (subvec (packed left 260 true) 0 2)))))

(deftest query-uses-real-registry-properties-and-launch-time-membership
  (let [m (model)
        frozen (targets/resolve-command m {:op :cue/launch :cue-id :sweep :command-id "one"
                                            :launch {:selector {:volume-id :ball}}} {:ball #{"head-2"}})
        replay (targets/remember (command m frozen 0) frozen)
        again (targets/resolve-command (:model replay) {:op :cue/launch :cue-id :sweep :command-id "one"
                                                        :launch {:selector {:volume-id :ball}}} {:ball #{:head-1}})
        ranked (targets/search (:model replay) {:kinds [:cue] :attributes [:dimmer] :params {:shape {:allows :sine}
                                                                                               :power {:initial 0.6}}})
        searched (targets/search m {:text "head" :kinds [:group :fixture]})]
    (is (= ["head-2"] (get-in frozen [:launch :targets])))
    (is (= frozen again))
    (is (= 1 (:uses (first ranked))))
    (is (empty? (targets/search m {:kinds [:cue] :params {:shape {:allows :not-present}}})))
    (is (= :group (:kind (first searched))))
    (is (= [:head-1 "head-2"] (targets/resolve-ids m (:selector (first searched)))))
    (is (= [:head-1] (targets/resolve-ids m {:fixture-ids ["head-1"]})))
    (is (thrown? Exception (targets/resolve-ids m {:group-id "gone"})))
    (is (thrown? Exception (actions/validate-bindings! {:oops {:volume-id :ball :action {:preset :bounce}}})))
    (let [v2 (assoc effect :version 3 :outputs {:dimmer 0.5 :focus 0.2})
          registry (assoc-in (:registry m) [:versions [:effects :wash 1]] effect)
          change (:changes (targets/analyze registry :effect v2))]
      (is (= 1 (:from-version change)))
      (is (= #{:focus :dimmer} (:changed-outputs change)))
      (is (= #{:focus} (:added-attributes change))))))

(deftest presets-use-addressable-fixture-functions
  (let [b {:pulse {:volume-id :ball :action {:preset :strobe}}}
        m (update-area (model) #{:mover :head-1} b 0)]
    (is (= 1 (count (:instances m))))
    (is (= 128 (last (packed m 10 true))))
    (is (some? (actions/session-source m (first (vals (:instances m))))))
    (let [id (get-in m (into actions/run-path [:leases :pulse :mover :id]))]
      (is (= "1:3" (get-in (actions/restore m {id "1:3"} 1) (into actions/run-path [:leases :pulse :mover :id])))))))

(deftest geometry-and-native-color-presets-require-the-fixtures-complete-channel-set
  (let [m (-> (model)
              (assoc-in [:rig :fixtures 2 :channels] {:pan 1 :white 2})
              (assoc-in [:registry :effects :color] {:id :color :version 1 :outputs {:color [1 0 0]}}))
        result (update-area m #{:mover} {:target {:volume-id :ball :action {:preset :aim}}
                                        :color {:volume-id :ball :action {:effect-id :color}}} 0)]
    (is (empty? (:instances result)))
    (is (empty? (get-in result (conj actions/run-path :errors))))))
