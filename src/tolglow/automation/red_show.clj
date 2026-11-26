(ns tolglow.automation.red-show
  "The R1o prompt-to-show example as data (#217): on the 2023 RGBW gig rig,
  predominantly red looks with occasional muted purple, beat one-shots, a quiet
  darkness/one-spotlight section and abrupt strobe contrast, played by the auto
  planner's section plan while the cue grid and Push stay available.

    (require '[tolglow.automation.red-show :as red-show] '[tolglow.show :as show])
    (def runtime (show/start! {:model (red-show/model)}))
    @(show/submit! runtime :auto (red-show/auto-config 217))

  The deterministic acceptance test is tolglow.automation.r1o-scenario-test.
  Whether it looks musical is for the operator to judge in the Room."
  (:require [tolglow.registry :as registry]
            [tolglow.venues.gig-2023 :as gig]))

(def red [1.0 0.0 0.0])

(def deep-red [0.85 0.05 0.02])

(def muted-purple [0.4 0.18 0.48])

(def red-palette
  [{:color red :weight 6} {:color deep-red :weight 3} {:color muted-purple :weight 1}])

(def purple-palette [{:color muted-purple :weight 3} {:color red :weight 1}])

(def washes [:wash-1 :wash-2 :wash-3 :wash-4 :wash-5 :wash-6 :wash-7 :wash-8 :wash-2-1 :wash-2-2])

(def movers
  "Heads that sweep. The 60 W beam (:moving-2) is kept for the spotlight: its
  shutter needs opening, which the other heads cannot take (#688)."
  [:moving-1 :moving-mini-1 :moving-mini-2 :moving-mini-3 :moving-mini-4])

(def color-param {:type :color :default red})

(def effects
  [{:id :wash-color :version 1 :targets washes
    :params {:color color-param
             :level {:type :number :default 0.8 :bounds [0 1] :random {:enabled? true :range [0.55 1.0]}}}
    :outputs {:color {:op :param :key :color} :dimmer {:op :param :key :level}}}
   {:id :accent :version 1 :targets [:wash-1 :wash-3 :wash-5 :wash-7]
    :params {:color color-param}
    :outputs {:color {:op :param :key :color} :dimmer 1.0}}
   {:id :sweep :version 1 :targets movers
    :params {:span {:type :number :default 0.3 :bounds [0 1] :random {:enabled? true :range [0.1 0.4]}}}
    ;; Pan swings around centre by :span, one cycle per bar of elapsed beats.
    :outputs {:pan {:op :add :args [0.5 {:op :multiply :args [{:op :param :key :span}
                                                             {:op :sin :args [{:op :multiply :args [{:op :beat} (/ Math/PI 2)]}]}]}]}
              :tilt 0.6}}
   {:id :head-glow :version 1 :targets movers :params {:color color-param}
    :outputs {:color {:op :param :key :color} :dimmer 0.9}}
   {:id :spot :version 1 :targets [:moving-2] :outputs {:color [1.0 0.9 0.8] :dimmer 0.7 :shutter 1.0}}
   {:id :strobe-hit :version 1 :targets washes
    :params {:rate {:type :number :default 0.6 :bounds [0 1] :random {:enabled? true :range [0.4 0.9]}}}
    :outputs {:color [1.0 1.0 1.0] :dimmer 1.0 :strobe {:op :param :key :rate}}}])

(def page-id :red-show)

(defn model
  "The gig rig with the example's effects, a cue per effect and a cue page."
  []
  (-> (reduce (fn [m effect]
                (-> m (update :registry registry/publish :effects effect)
                    (update :registry registry/publish :cues {:id (:id effect) :effect-id (:id effect)})))
              (assoc (gig/model) :transport {:playing? true :beat 0.0 :bpm 120.0})
              effects)
      (assoc-in [:pages page-id] {:id page-id :name "Red show"
                                  :cells (into {} (map-indexed (fn [i {:keys [id]}] [[(mod i 4) (quot i 4)] id])) effects)})))

(defn auto-config
  "The auto command for this example: a seeded section plan over its cues."
  [seed]
  {:enabled? true :seed seed
   :interval {:beats 1} :count 1
   :launch {:mode :one-shot :duration {:beats 1}}
   :randomize {:palette red-palette}
   :categories {:exclude [:strobe]}
   :sections [{:id :red :weight 6 :length {:bars 2} :transition :release
               :config {:scope {:cue-ids [:wash-color :sweep :head-glow :accent]} :count 2}}
              {:id :purple :weight 1 :length {:bars 1} :max-run 1 :transition :release
               :config {:scope {:cue-ids [:wash-color :sweep :head-glow]} :randomize {:palette purple-palette}}}
              {:id :quiet :weight 2 :length {:bars 2} :max-run 1 :transition :cut
               :config {:scope {:cue-ids [:spot :sweep]} :intent :darkness :max-lit 1
                        :interval {:bars 1} :launch {:mode :timed :duration {:bars 1}}}}
              {:id :strobe :weight 1 :length {:bars 1} :max-run 1 :transition :cut
               :config {:scope {:cue-ids [:strobe-hit]} :categories {:include [:strobe]}
                        :interval {:beats 2} :launch {:mode :one-shot :duration {:beats 2}}}}]})
