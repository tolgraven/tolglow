(ns tolglow.rig.roles-test
  "Fixture roles (#858): the vocabulary, proposals on the real rigs, declared
  assignments through the rig validator and the show's :rig command, and a
  role-addressed cue launching through the real show owner on every rig that
  has the role."
  (:require [clojure.test :refer [deftest is testing]]
            [tolglow.persistence :as persistence]
            [tolglow.registry :as registry]
            [tolglow.rig :as rig]
            [tolglow.rig.reference :as reference]
            [tolglow.rig.roles :as roles]
            [tolglow.show :as show]
            [tolglow.showcase.rig :as showcase-rig]
            [tolglow.spatial.targets :as targets]
            [tolglow.venues.gig-2023 :as gig]))

(deftest roles-are-zone-function-and-an-optional-ordinal-from-a-closed-vocabulary
  (is (= 1 (:version roles/vocabulary)))
  (is (= {:zone :back :function :beam} (roles/parse :back.beam)))
  (is (= {:zone :floor :function :wash :ordinal 2} (roles/parse :floor.wash.2)))
  (is (= {:zone :side :function :pixel} (roles/parse "side.pixel")) "text clients send strings")
  (doseq [bad [:stage.wash :back.laser :back.beam.0 :back.beam.100 :back.beam.x :back :a/back.beam
               :back.beam.2.3 "" nil 3]]
    (is (nil? (roles/parse bad)) (pr-str bad)))
  (is (= :floor.wash.2 (roles/role :floor :wash 2)))
  (is (= :floor.wash (roles/base-role :floor.wash.2)))
  (is (= {:group-id :back.beam} (roles/selector :back.beam)))
  (is (thrown? clojure.lang.ExceptionInfo (roles/role :stage :wash)))
  (is (thrown? clojure.lang.ExceptionInfo (roles/selector :stage.wash))))

(def gig-proposal
  "What propose says about the 2023 gig rig today, recorded on #858 beside
  the operator's corrections."
  {:front.wash [:wash-1 :wash-2 :wash-3 :wash-4 :wash-5 :wash-6 :wash-7 :wash-8 :wash-2-1 :wash-2-2]
   :front.beam [:moving-mini-1 :moving-mini-2 :moving-mini-3]
   :floor.beam [:moving-2 :moving-mini-4]
   :set.beam [:moving-1]})

(def showcase-proposal
  {:overhead.beam [:beam-1 :beam-2 :beam-3 :beam-4 :beam-5 :beam-6]
   :floor.beam [:mover-1 :mover-2 :mover-3 :mover-4]
   :side.wash [:wash-1 :wash-2]
   :floor.wash [:wash-3 :wash-4]
   :overhead.blinder [:blinder-1 :blinder-2]
   :floor.atmosphere [:hazer-1]
   :back.pixel [:tube-1 :tube-2 :tube-3 :tube-4 :tube-5 :tube-6 :tube-7 :tube-8]
   :front.pixel [:ribbon]})

(deftest proposals-come-from-kind-capabilities-and-normalised-position
  (testing "the real rigs"
    (is (= gig-proposal (into {} (:assignment (roles/propose (gig/rig))))))
    (is (= showcase-proposal (into {} (:assignment (roles/propose (showcase-rig/rig)))))))
  (testing "every unit carries its reason and normalised position"
    (let [rows (:units (roles/propose (gig/rig)))
          wash (some #(when (= :wash-1 (:id %)) %) rows)]
      (is (= 16 (count rows)))
      (is (= :static (:kind wash)))
      (is (= [-1.0 1.0 0.6] (:npos wash)))
      (is (= "static head; downstage in the rig" (:reason wash)))))
  (testing "a pixel layout is one unit at its heads' centroid"
    (let [tube (some #(when (= :tube-1 (:id %)) %) (roles/units (:fixtures (showcase-rig/rig))))]
      (is (= 10 (count (:heads tube))))
      (is (:pixel? tube))
      (is (= :pixel (roles/kind tube)))))
  (testing "a proposal on each reference rig agrees with the roles it declares"
    (doseq [id reference/classes]
      (is (= (get-in reference/rigs [id :roles]) (into {} (:assignment (roles/propose (reference/rig id))))) (name id)))))

(deftest a-declared-assignment-is-validated-with-the-rig
  (let [base (gig/rig)]
    (is (= [] (rig/errors (roles/assign base gig-proposal))))
    (is (= {:unknown-ids [] :unassigned []} (roles/report (roles/assign base gig-proposal))))
    (testing "structure"
      (is (= [:unsupported-role-vocabulary] (map :code (rig/errors (assoc base :roles {:version 2 :assignment {}})))))
      (is (= [:unknown-role] (map :code (rig/errors (assoc base :roles {:version 1 :assignment {:stage.wash [:wash-1]}})))))
      (is (= [:invalid-role-members] (map :code (rig/errors (assoc base :roles {:version 1 :assignment {:front.wash :wash-1}}))))))
    (testing "a fixture holds one role"
      (is (= [:duplicate-role-member]
             (map :code (rig/errors (assoc base :roles {:version 1 :assignment {:front.wash [:wash-1] :back.wash [:wash-1]}}))))))
    (testing "a role never shadows a patch group"
      (let [tagged (update base :fixtures (fn [fs] (mapv #(update % :groups conj :front.wash) fs)))]
        (is (= [:role-group-collision]
               (map :code (rig/errors (assoc tagged :roles {:version 1 :assignment {:front.wash.1 [:wash-1]}})))))))
    (testing "a deleted fixture never blocks an edit; the report names it"
      (let [edited (-> (roles/assign base gig-proposal) (update :fixtures #(vec (remove (comp #{:moving-1} :id) %))))]
        (is (= [] (rig/errors edited)))
        (is (= [:moving-1] (:unknown-ids (roles/report edited))))
        (is (nil? (roles/members edited :set.beam)) "an empty role does not resolve")))))

(deftest a-role-resolves-as-a-group-including-ordinals-and-pixel-layouts
  (let [rig (roles/assign (showcase-rig/rig) {:floor.wash.1 [:wash-3] :floor.wash.2 [:wash-4] :back.pixel [:tube-1]})
        model {:rig rig}]
    (is (= [:wash-3] (targets/resolve-ids model (roles/selector :floor.wash.1))))
    (is (= [:wash-3 :wash-4] (targets/resolve-ids model (roles/selector :floor.wash))) "the base role takes its ordinals")
    (is (= (mapv #(keyword (str "tube-1-head-" %)) (range 1 11))
           (targets/resolve-ids model (roles/selector :back.pixel))))
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"Unknown fixture group"
                          (targets/resolve-ids model (roles/selector :front.wash))))
    (is (some #(and (= :group (:kind %)) (= :floor.wash (:id %))) (targets/search model {:kinds [:group]}))
        "Studio's picker lists roles beside patch groups")))

(def glow
  "An untargeted effect: the cue's role chooses its heads."
  {:id :role-glow :version 1 :outputs {:color [1.0 0.2 0.1] :dimmer 0.8}})

(defn- capture-sink [captured] {:send! (fn [universes _] (reset! captured (update-vals universes aclone)))})

(defn- launch-on
  "Launch a cue on role through a fresh show owner on model. Returns the
  receipt and the launched instance's targets."
  [model role]
  (let [model (-> model
                  (update :registry registry/publish :effects glow)
                  (update :registry registry/publish :cues {:id :role-cue :effect-id :role-glow :selector (roles/selector role)}))
        rt (show/create-runtime {:model model :sink (capture-sink (atom nil)) :workspace-options nil})]
    (try
      (show/tick! rt 0.0)
      (let [reply (show/submit! rt :command {:op :cue/launch :cue-id :role-cue :command-id "role-launch"
                                             :source {:adapter :repl :session-id "roles-test"}})]
        (show/tick! rt 25.0)
        (let [receipt (deref reply 5000 {:status :missing})
              instance (get-in @(:state rt) [:model :instances (:instance-id receipt)])]
          {:receipt receipt :targets (:targets instance)}))
      (finally (show/stop! rt)))))

(deftest a-cue-addressed-by-role-launches-on-every-rig-that-has-the-role
  (let [rigs (merge {:gig (roles/assign (gig/rig) gig-proposal)
                     :showcase (roles/assign (showcase-rig/rig) showcase-proposal)}
                    (into {} (map (juxt identity reference/rig)) reference/classes))
        model (fn [rig] (assoc (gig/model) :rig rig))]
    (doseq [role [:front.wash :floor.beam :back.pixel]
            [id rig] rigs]
      (let [{:keys [receipt targets]} (launch-on (model rig) role)
            members (roles/members rig role)]
        (if members
          (do (is (= :accepted (:status receipt)) [role id receipt])
              (is (= members targets) [role id]))
          (do (is (= :rejected (:status receipt)) [role id])
              (is (= :invalid-target (:code receipt)) [role id])))))))

(deftest roles-are-declared-through-the-show-rig-command-and-saved-with-the-show
  (let [rt (show/create-runtime {:model (gig/model) :sink (capture-sink (atom nil)) :workspace-options nil})]
    (try
      (show/tick! rt 0.0)
      (let [current (get-in @(:state rt) [:model :rig])
            accepted (show/submit! rt :rig (roles/assign current gig-proposal))
            invalid (show/submit! rt :rig (assoc current :roles {:version 1 :assignment {:stage.wash [:wash-1]}}))]
        (show/tick! rt 25.0)
        (is (= :accepted (:status (deref accepted 5000 nil))))
        (let [refusal (deref invalid 5000 nil)]
          (is (= :rejected (:status refusal)))
          (is (= [[:roles :assignment :stage.wash]] (map :path (get-in refusal [:details :errors]))))
          (is (= [:unknown-role] (map :code (get-in refusal [:details :errors])))))
        (is (= gig-proposal (get-in @(:state rt) [:model :rig :roles :assignment])))
        (testing "an edit from Studio's rig editor names fixtures by text and keeps their types"
          (let [shown (update-in (get-in @(:state rt) [:model :rig]) [:roles :assignment]
                                 update-vals #(mapv name %))
                reply (show/submit! rt :rig shown)]
            (show/tick! rt 50.0)
            (is (= :accepted (:status (deref reply 5000 nil))))
            (is (= gig-proposal (get-in @(:state rt) [:model :rig :roles :assignment])))))
        (let [saved (persistence/read-string (show/export-show-source rt))]
          (is (= {:version 1 :assignment gig-proposal} (get-in saved [:model :rig :roles])))))
      (finally (show/stop! rt)))))
