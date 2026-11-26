(ns tolglow.studio.video-surfaces-test
  (:require [cljs.test :refer-macros [deftest is testing]]
            [tolglow.studio.video-surfaces :as video-surfaces]
            [tolglow.video.surface :as surface]))

(defn- close? [a b] (every? #(< (js/Math.abs %) 1e-9) (map - (flatten a) (flatten b))))

(deftest new-surfaces-validate-and-take-the-next-free-id
  (doseq [shape [:plane :wave :sphere]]
    (is (= [] (surface/surface-errors (video-surfaces/new-surface :s :grid shape) [])) (name shape)))
  (is (= :surface-1 (video-surfaces/surface-id {})))
  (is (= :surface-3 (video-surfaces/surface-id {:surface-1 {} :surface-2 {}}))))

(deftest the-room-draws-the-shared-mesh-from-the-pose-the-show-evaluated
  (let [resolved (surface/resolve-surface (assoc (video-surfaces/new-surface :sheet :grid :wave) :position [0 2 0])
                                          {:phase 0.25 :yaw 30} 3)
        video {:sources [{:id :grid :sequence 7}]
               :surfaces {:running [{:instance-id "i-1" :output 0 :surface :sheet :resolved resolved
                                     :contacts [{:id :h1 :weight 1.0} {:id :h2 :weight 0.5} {:id :gone :weight 1.0}]}]
                          :authored {:sheet {:resolved resolved :contacts []}
                                     :ball {:resolved (surface/resolve-surface (video-surfaces/new-surface :ball :grid :sphere) nil 0)
                                            :contacts []}}}}
        rig {:fixtures [{:id :h1 :position [0 2 0.1]} {:id :h2 :position [1 2 0.2]}]}
        tokens {:grid {:token 7 :at 0}}
        [running ball :as all] (video-surfaces/room-surfaces video rig tokens true)]
    (is (= 2 (count all)) "an authored surface that is also running is drawn once")
    (is (= "i-1/0" (:id running)))
    (is (= "authored/ball" (:id ball)))
    (is (= (:positions (surface/mesh resolved)) (:positions running)) "the same mesh the JVM sampler's geometry yields")
    (is (= [[0 2 0.1 1.0] [1 2 0.2 0.5]] (:contacts running)) "heads without a position are skipped")
    (is (= ["h1" "h2"] (:contact-ids running)) "ids as the Room's output values are keyed")
    (is (= "/api/video/frame?source=%3Agrid&width=512&v=7" (:texture running)))
    (testing "every mesh vertex lies on the surface at its own source coordinate"
      (let [{:keys [positions uvs]} running]
        (doseq [i (range 0 (/ (count positions) 3) 5)]
          (let [{:keys [uv distance]} (surface/project resolved (subvec positions (* 3 i) (* 3 (inc i))))]
            (is (< distance 1e-9))
            (is (close? (surface/source-uv resolved uv) (subvec uvs (* 2 i) (* 2 (inc i)))))))))
    (is (false? (:outline ball)) "a sphere has no open edge")
    (is (true? (:outline running)))
    (testing "the performance Room shows only running surfaces"
      (is (= ["i-1/0"] (mapv :id (video-surfaces/room-surfaces video rig tokens false)))))))

(deftest contacts-group-by-strip-for-the-editor
  (is (= [{:layout nil :count 1 :ids [:par]} {:layout :tube :count 2 :ids [:t1 :t2]}]
         (video-surfaces/contact-summary [{:id :t1 :layout :tube} {:id :par} {:id :t2 :layout :tube}]))))
