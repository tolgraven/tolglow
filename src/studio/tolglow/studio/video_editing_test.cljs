(ns tolglow.studio.video-editing-test
  "Studio video editing (#672-#674): strip placement and fixture groups,
  Room surface handle drags and shader uniform drafts."
  (:require [cljs.test :refer-macros [deftest is testing]]
            [tolglow.studio.shader-uniforms :as uniforms]
            [tolglow.studio.video-controls :as video-controls]
            [tolglow.studio.video-strip-tools :as tools]
            [tolglow.studio.video-strips :as video]
            [tolglow.studio.video-surface-handles :as handles]
            [tolglow.studio.video-surfaces :as surfaces]
            [tolglow.video.mapping :as mapping]
            [tolglow.video.surface :as surface]))

(defn- close? [a b] (every? #(< (js/Math.abs %) 1e-9) (map - (flatten a) (flatten b))))

(deftest placement-rectangle-and-targets-edits-validate
  (let [strip {:id :tube-a :layout :tube-a :points [[0.25 0.5] [0.75 0.5]] :height 0.04}
        aspect (/ 16 9)]
    (is (close? [[0.25 0.25] [0.75 0.25]] (:points (video/edited-strip strip [:place {:center [0.5 0.25]}] aspect))))
    (testing "one centre coordinate builds on the strip as it is, not as the knob last rendered it"
      (let [moved (video/edited-strip strip [:place {:center-u 0.6}] aspect)]
        (is (close? [[0.35 0.25] [0.85 0.25]] (:points (video/edited-strip moved [:place {:center-v 0.25}] aspect))))))
    (is (close? [[0.1 0.3] [0.9 0.3]]
                (:points (video/edited-strip strip [:rect (mapping/rect-strip [0.1 0.2] [0.9 0.4] aspect)] aspect))))
    (is (close? [0.2] [(:height (video/edited-strip strip [:rect (mapping/rect-strip [0.1 0.2] [0.9 0.4] aspect)] aspect))]))
    (let [group {:id :g :targets [:a :b] :points [[0 0.5] [1 0.5]]}]
      (is (= [:b :a] (:targets (video/edited-strip group [:targets [:b :a]] aspect)))))
    (doseq [op [[:place {:angle 45 :length 0.5}] [:rect {:points [[0 0] [1 1]] :height 0.3}]]]
      (is (= [] (mapping/strip-errors (video/edited-strip strip op aspect) [])) (pr-str op)))))

(deftest fixture-groups-reorder-and-take-the-pending-rectangle
  (is (= [:b :a :c] (tools/move-target [:a :b :c] 1 -1)))
  (is (= [:a :c :b] (tools/move-target [:a :b :c] 1 1)))
  (is (= [:a :b :c] (tools/move-target [:a :b :c] 2 1)) "the last stays last")
  (let [rig {:fixtures [{:id :c :position [2 0 0]} {:id :a :position [-1 0 0]} {:id :b :position [0 0 0]}
                        {:id :px :pixel-layout-id :tube :position [0 0 0]}]}]
    (is (= [:a :b :c :zz] (tools/left-to-right [:zz :c :a :b] rig)) "unplaced targets go last")
    (is (= [:a :b :c] (mapv :id (tools/fixture-choices rig))) "pixels are mapped by layout, not listed"))
  (let [mapping {:id :m :strips [{:id :group-1 :targets [:a] :points [[0 0] [1 0]]}]}
        strip (tools/group-strip mapping [:a :b] {:points [[0.2 0.3] [0.8 0.3]] :height 0.05})]
    (is (= :group-2 (:id strip)))
    (is (= [[0.2 0.3] [0.8 0.3]] (:points strip)))
    (is (= [] (mapping/strip-errors strip [])))))

(deftest a-knob-turn-shares-one-coalescing-token-until-it-pauses
  (let [[a t1] (video-controls/coalesce-key {} :height 1000)
        [b t2] (video-controls/coalesce-key t1 :height 1500)
        [c t3] (video-controls/coalesce-key t2 :height 2600)
        [d _] (video-controls/coalesce-key t3 :angle 2600)]
    (is (= a b))
    (is (not= b c) "a pause longer than a second starts a new undo step")
    (is (not= c d) "another control has its own")))

(def sheet {:id :sheet :source :grid :shape :plane :position [0 2 0] :rotation-deg [0 0 0] :size-m [4 2]})

(deftest surface-handles-sit-around-the-surface-and-drags-move-it
  (let [[move raise turn] (handles/handles sheet)]
    (is (= {:kind "move" :id "sheet" :position [0 2 0]} move))
    (is (close? [0 3.35 0] (:position raise)))
    (is (= 1 (:axis raise)))
    (is (close? [2.35 2 0] (:position turn))))
  (testing "the turn handle follows the surface's yaw"
    (is (close? [0 2 -2.35] (:position (nth (handles/handles (assoc sheet :rotation-deg [0 90 0])) 2)))))
  (is (= [1 2 -0.5] (:position (handles/dragged sheet {:kind "move" :start [0 2 0] :point [1 2 -0.5]}))))
  (is (= [0 2.4 0] (:position (handles/dragged sheet {:kind "raise" :delta 0.4}))))
  (testing "turning to the handle's new bearing sets the yaw the shared rotation uses"
    (let [turned (handles/dragged sheet {:kind "turn" :start [2.35 2 0] :point [0 2 -2.35]})
          u (surface/world-point (surface/resolve-surface turned nil 0) [1 0.5])]
      (is (= 90 (get-in turned [:rotation-deg 1])))
      (is (close? [0 2 -2] u) "the surface's u edge now points where the handle was dragged"))))

(defn- db-with [surfaces]
  {:server-projection {:view {:settings {:video {:surfaces surfaces}}}}
   :ui {:rig-editor {:identity "i"}}})

(deftest a-handle-drag-is-drawn-as-a-draft-and-commits-once
  (let [db (db-with {:sheet sheet})
        begin (handles/gesture db {:type "surface-begin" :token 1 :id "sheet" :kind "move" :point [0 2 0]})
        moved (handles/gesture (:db begin) {:type "surface-move" :token 1 :point [1 2 0]})
        stale (handles/gesture (:db moved) {:type "surface-move" :token 2 :point [5 2 0]})
        end (handles/gesture (:db stale) {:type "surface-end" :token 1})]
    (is (= :sheet (get-in (:db begin) [:video-surfaces-ui :selected])))
    (is (nil? (:edit begin)))
    (is (= [1 2 0] (get-in (handles/drafts (:db moved)) [:sheet :position])))
    (is (= (:db moved) (:db stale)) "another gesture's move is ignored")
    (is (= {:op :patch-surface :id :sheet :changes {:position [1 2 0]}} (:edit end)) "only the pose it changed")
    (is (= [1 2 0] (get-in (handles/drafts (:db end)) [:sheet :position])) "drawn until the show commits it")
    (is (= {} (handles/drafts (assoc-in (:db end) [:server-projection :view :settings :video :surfaces :sheet]
                                        (assoc sheet :position [1 2 0])))))
    (testing "a second drag while the first is still committing starts where the first left the surface"
      (let [again (handles/gesture (:db end) {:type "surface-begin" :token 3 :id "sheet" :kind "move" :point [1 2 0]})
            moved (handles/gesture (:db again) {:type "surface-move" :token 3 :point [1 2 1]})
            done (handles/gesture (:db moved) {:type "surface-end" :token 3})]
        (is (= [1 2 0] (get-in (:db again) [:video-surfaces-ui :room :gesture :before :position])))
        (is (= {:op :patch-surface :id :sheet :changes {:position [1 2 1]}} (:edit done))
            "the first move is kept, not undone by a delta from the stale saved surface")))
    (testing "a size another writer saved during the commit is drawn with the dragged pose"
      (is (= {:sheet (assoc sheet :position [1 2 0] :size-m [8 4])}
             (handles/drafts (assoc-in (:db end) [:server-projection :view :settings :video :surfaces :sheet]
                                       (assoc sheet :size-m [8 4])))))))
  (testing "a drag that never moved, or a cancel, sends nothing"
    (let [db (:db (handles/gesture (db-with {:sheet sheet}) {:type "surface-begin" :token 1 :id "sheet" :kind "raise" :point [0 3 0]}))]
      (is (nil? (:edit (handles/gesture db {:type "surface-end" :token 1}))))
      (is (= {} (handles/drafts (:db (handles/gesture db {:type "surface-cancel" :token 1}))))))))

(deftest the-room-draws-a-dragged-surface-at-its-draft-pose
  (let [video {:surfaces {:authored {:sheet {:resolved (surface/resolve-surface sheet nil 0) :contacts []}} :running []}}
        moved (assoc sheet :position [3 2 0])
        [entry] (surfaces/room-surfaces video {:fixtures []} {} true {:sheet moved})]
    (is (= (:positions (surface/mesh (surface/resolve-surface moved nil 0))) (:positions entry)))))

(def effect
  {:id :plasma :version 3
   :params {:level {:type :number :default 1 :bounds [0 1]}}
   :shader {:glsl "void main(){}" :size [640 360] :fps 30
            :uniforms {:u_level {:type :float :value {:op :param :key :level}}
                       :u_mix {:type :float :value {:op :multiply :args [{:op :beat} 2]}}}}})

(deftest uniform-drafts-become-one-save-request
  (is (= :control (uniforms/value-kind {:op :param :key :level})))
  (is (= :expression (uniforms/value-kind {:op :multiply :args [1 2]})))
  (is (= :constant (uniforms/value-kind [1 0 0])))
  (is (= [0.5 0 0] (uniforms/constant-for :vec3 0.5)))
  (is (= 1 (uniforms/constant-for :float [1 2])))
  (is (some? (uniforms/uniform-name-error {} "u_time")))
  (is (some? (uniforms/uniform-name-error {:u_level {}} "u_level")))
  (is (some? (uniforms/uniform-name-error {} "2x")))
  (is (nil? (uniforms/uniform-name-error {} "u_glow")))
  (let [draft (-> (uniforms/draft-of effect)
                  (assoc :size [200 1] :fps 25)
                  (assoc-in [:uniforms :u_glow] {:type :float :value 0.25})
                  (uniforms/make-control :u_glow))]
    (is (= {:op :param :key :glow} (get-in draft [:uniforms :u_glow :value])))
    (is (= {:type :number :default 0.25 :bounds [0 1]} (get-in draft [:params :glow])))
    (is (= {:effect-id :plasma :expected-version 3
            :shader {:size [200 1] :fps 25 :uniforms (:uniforms draft)}
            :params {:glow {:type :number :default 0.25 :bounds [0 1]}}}
           (uniforms/edit-request effect draft)))
    (is (= :glow-2 (uniforms/control-param {:glow {}} :u_glow)))
    (testing "an unchanged draft sends only its identity"
      (is (= {:effect-id :plasma :expected-version 3 :shader {}} (uniforms/edit-request effect (uniforms/draft-of effect)))))
    (testing "a draft started on v3 keeps v3 as its base when the REPL publishes v4"
      (let [s {:draft (assoc (uniforms/draft-of effect) :fps 25) :base effect}
            v4 (-> effect (assoc :version 4) (assoc-in [:shader :fps] 20))
            {:keys [draft base]} (uniforms/active s v4)]
        (is (= 3 (:version base)))
        (is (= {:effect-id :plasma :expected-version 3 :shader {:fps 25}} (uniforms/edit-request base draft))
            "the save names v3, so the show refuses it rather than reverting v4")))
    (testing "a saved draft stays until its version arrives"
      (let [s {:draft (uniforms/draft-of effect) :base effect :acked 4}]
        (is (some? (uniforms/active s effect)))
        (is (nil? (uniforms/active s (assoc effect :version 4))))))))
