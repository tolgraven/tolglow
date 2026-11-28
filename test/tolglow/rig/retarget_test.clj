(ns tolglow.rig.retarget-test
  "Retargeting recorded agent decisions onto reference rigs (#858). The #670
  club rehearsal's agent decisions, made on the 2023 gig rig, are retargeted
  onto each reference rig and replayed through the real show owner with
  captured output. Every refused or unmappable item is listed with its
  reason; the record carries no fixture ID, address or position."
  (:require [clojure.edn :as edn]
            [clojure.test :refer [deftest is testing]]
            [tolglow.rehearsal.passage :as passage]
            [tolglow.rehearsal.retarget :as rr]
            [tolglow.rig.reference :as reference]
            [tolglow.rig.retarget :as retarget]
            [tolglow.rig.roles :as roles]
            [tolglow.showcase.rig :as showcase-rig]
            [tolglow.venues.gig-2023 :as gig]))

(def club (passage/club 670))

(def records (delay (into {} (map (fn [id] [id (rr/record club id)])) reference/classes)))

(def expected-report
  "Everything the club take cannot carry from the gig rig, with its reason.
  The movers' cues use five of the six heads, the accent and kick every
  other wash and the spot one named head: no role on the gig rig is exactly
  those fixtures, so they are reported, not guessed."
  [{:kind :cue :id :accent :reason :no-matching-role :heads 4 :overlaps [:front.wash]}
   {:kind :cue :id :head-glow :reason :no-matching-role :heads 5 :overlaps [:floor.beam :front.beam :set.beam]}
   {:kind :cue :id :kick-hit :reason :no-matching-role :heads 4 :overlaps [:front.wash]}
   {:kind :cue :id :snare-hit :reason :no-matching-role :heads 5 :overlaps [:floor.beam :front.beam :set.beam]}
   {:kind :cue :id :spot :reason :no-matching-role :heads 1 :overlaps [:floor.beam]}
   {:kind :cue :id :sweep :reason :no-matching-role :heads 5 :overlaps [:floor.beam :front.beam :set.beam]}
   {:kind :call :decision "rehearsal:club:groove" :call "auto" :tool "configure_auto" :section :drive
    :reason :cue-not-carried :cues [:sweep :head-glow :accent]}
   {:kind :call :decision "rehearsal:club:groove" :call "auto" :tool "configure_auto" :section :lift
    :reason :cue-not-carried :cues [:head-glow]}
   {:kind :call :decision "rehearsal:club:groove" :call "auto" :tool "configure_auto"
    :reason :cue-not-carried :cues [:sweep :head-glow :accent :spot]}
   {:kind :call :decision "rehearsal:club:breakdown" :call "auto" :tool "configure_auto"
    :reason :cue-not-carried :cues [:sweep :head-glow]}
   {:kind :call :decision "rehearsal:club:return" :call "auto" :tool "configure_auto" :section :drive
    :reason :cue-not-carried :cues [:sweep :head-glow :accent]}
   {:kind :call :decision "rehearsal:club:return" :call "auto" :tool "configure_auto"
    :reason :cue-not-carried :cues [:sweep :head-glow :accent :spot]}
   {:kind :call :decision "rehearsal:club:quiet" :call "auto" :tool "configure_auto" :reason :no-carried-cue}])

(deftest the-club-take-retargets-onto-every-reference-rig-with-every-refusal-listed
  (doseq [[id record] @records]
    (testing (name id)
      (is (= "tolglow-retarget/1" (:schema record)))
      (is (= {:roles :proposed :class :movers-pars} (:source record))
          "the gig rig has no declared roles yet, so the record says it used a proposal")
      (is (= {:class id :reference id} (:target record)))
      (is (= {:used [:front.wash] :absent []} (:roles record)))
      (is (= [:chorus-wash :strobe-hit :verse-wash :wash-color] (mapv :id (get-in record [:definitions :cues]))))
      (is (every? #(= {:group-id :front.wash} (:selector %)) (get-in record [:definitions :cues])))
      (is (not-any? :targets (get-in record [:definitions :effects])))
      (is (= expected-report (:report record)))
      (testing "the carried decisions name only carried cues"
        (let [scopes (for [d (:decisions record) c (:calls d)
                           s (cons (get-in c [:arguments :configuration :scope])
                                   (map #(get-in % [:config :scope]) (get-in c [:arguments :configuration :sections])))]
                       s)]
          (is (= #{:wash-color :strobe-hit} (set (mapcat :cue-ids scopes))))
          (is (= [1 1 1 0] (mapv (comp count :calls) (:decisions record))) "the quiet section's change has nothing to carry"))))))

(defn- identifiers [rig]
  (into #{} (comp (mapcat (juxt :id :fixture-id :pixel-layout-id)) (remove nil?) (map name)) (:fixtures rig)))

(defn- positions [rig] (into #{} (keep :position) (:fixtures rig)))

(deftest a-retargeted-record-holds-no-fixture-id-address-or-position
  (let [source (:rig (rr/source-model club))
        forbidden (into (identifiers source) (mapcat (comp identifiers reference/rig)) reference/classes)]
    (doseq [[id record] @records
            :let [text (pr-str record)
                  read-back (edn/read-string text)
                  values (tree-seq coll? seq read-back)]]
      (testing (name id)
        (is (= record read-back) "plain EDN")
        (is (empty? (filter #(and (or (keyword? %) (string? %)) (forbidden (name %))) values))
            "no fixture, head or pixel layout ID of either rig")
        (is (not-any? #{:address :universe :position :channels :rotation-deg} values) "no patch or geometry keys")
        (is (not-any? (positions source) (filter vector? values)) "no source position")))))

(deftest retargeting-the-same-take-twice-gives-identical-records
  (doseq [id reference/classes]
    (let [again (rr/record club id)]
      (is (= (get @records id) again))
      (is (= (:digest (get @records id)) (:digest again)))))
  (is (apply distinct? (map :digest (vals @records))) "each target is its own record"))

(deftest a-retargeted-take-replays-through-the-show-owner-with-identical-output
  (doseq [id reference/classes]
    (testing (name id)
      (let [a (rr/run-take club id)
            b (rr/run-take club id)
            front (set (roles/members (reference/rig id) :front.wash))
            lit (into #{} (mapcat :lit) (:frames a))]
        (is (= {} (:status a)))
        (is (= [:accepted :accepted :accepted]
               (keep #(get-in % [:receipts "configure_auto" :status]) (:agent a)))
            "every carried change is accepted by the target show")
        (is (= ["configure_auto"] (:tools (first (:agent a)))))
        (is (= [] (:tools (last (:agent a)))) "the quiet section's change was not sent")
        (is (seq (:auto-launches a)))
        (is (every? #{:wash-color :strobe-hit} (map :cue-id (:auto-launches a))))
        (is (seq lit))
        (is (every? front lit) "only the front washes light")
        (is (= (rr/output-digest a) (rr/output-digest b)))
        (is (= (mapv #(dissoc % :t) (:frames a)) (mapv #(dissoc % :t) (:frames b))))))))

(deftest a-role-the-target-lacks-is-reported-and-left-dark
  (let [target (assoc (reference/model :movers-pars) :rig (roles/assign (showcase-rig/rig) {:side.wash [:wash-1 :wash-2]}))
        record (rr/record club target)]
    (is (= {:used [] :absent [:front.wash]} (:roles record)))
    (is (= [] (get-in record [:definitions :cues])))
    (is (= #{:chorus-wash :strobe-hit :verse-wash :wash-color}
           (set (keep #(when (= :role-absent (:reason %)) (:id %)) (:report record)))))
    (is (every? empty? (map :calls (:decisions record))) "no decision is left with a cue to play")
    (is (not (contains? (:target record) :reference)) "a target that is not a reference rig is not named as one")))

(deftest declared-source-roles-are-used-and-named
  (let [source (update (rr/source-model club) :rig roles/assign {:front.wash.1 [:wash-1 :wash-3 :wash-5 :wash-7]
                                                                 :front.wash.2 [:wash-2 :wash-4 :wash-6 :wash-8 :wash-2-1 :wash-2-2]})
        record (retarget/retarget source :pars (rr/decisions club))]
    (is (= :declared (get-in record [:source :roles])))
    (testing "the full wash set is the base role and carries"
      (is (= {:group-id :front.wash}
             (some #(when (= :wash-color (:id %)) (:selector %)) (get-in record [:definitions :cues])))))
    (testing "every other wash is now a role, which the pars rig lacks, so the accent and kick are reported"
      (is (= {:accent :front.wash.1 :kick-hit :front.wash.1}
             (into {} (keep #(when (= :role-absent (:reason %)) [(:id %) (:role %)])) (:report record))))
      (is (= {:used [:front.wash] :absent [:front.wash.1]} (:roles record))))))

(deftest calls-naming-fixtures-or-instances-are-refused
  (let [ctx (retarget/prepare (rr/source-model club) :pars)
        source (rr/source-model club)
        {:keys [decision report]}
        (retarget/retarget-decision
         ctx source
         {:id "d1" :calls [{:id "a" :name "launch_cue" :arguments {:cue-id :wash-color :launch {:targets [:wash-1]}}}
                           {:id "b" :name "launch_cue" :arguments {:cue-id :wash-color :launch {:selector {:group-id :wash}}}}
                           {:id "c" :name "set_parameter" :arguments {:instance-id "i-1" :param :level :value 0.5}}
                           {:id "d" :name "publish_effect" :arguments {:definition {:id :x :version 1 :targets [:wash-1]}}}
                           {:id "e" :name "schedule_events"
                            :arguments {:events [{:id "s1" :op "cue/launch" :cue-id "wash-color" :at {:bar 1}}
                                                 {:id "s2" :op "cue/launch" :cue-id "spot" :at {:bar 1}}]}}
                           {:id "f" :name "inspect_show" :arguments {}}]})]
    (is (= ["b" "e" "f"] (mapv :id (:calls decision))))
    (is (= {:group-id :front.wash} (get-in decision [:calls 0 :arguments :launch :selector]))
        "a patch group that is exactly one role maps to the role")
    (is (= [{:id "s1" :op "cue/launch" :cue-id "wash-color" :at {:bar 1}}]
           (get-in decision [:calls 1 :arguments :events])) "string cue IDs from a JSON log match")
    (is (= [[:named-fixtures "a"] [:instance-reference "c"] [:unsupported-tool "d"] [:cue-not-carried "e"]]
           (mapv (juxt :reason :call) report)))
    (is (= "s2" (:event (last report))))))
