(ns tolglow.demo
  "Small declarative rig and cue library. Loading this namespace does no I/O."
  (:require [afterglow.control.model :as control]))

(defn fixtures []
  (mapv (fn [i]
          {:id (keyword (str "head-" (inc i))) :name (str "Beam " (inc i))
           :position [(- (* i 1.1) 3.85) 3.5 -1.5]
           :universe 1 :address (inc (* i 12))
           :channels {:dimmer 1 :red 2 :green 3 :blue 4 :white 5
                      :pan [6 7] :tilt [8 9] :focus 10 :zoom 11 :strobe 12}
           :rgbw-policy :extract-min
           :capabilities #{:color :dimmer :movement :focus :strobe}})
        (range 8)))

(defn constant [value] {:op :const :value value})

(defn param [key] {:op :param :key key})

(def level {:type :number :default 0.7 :bounds [0.0 1.0]
            :recommended [0.15 0.85] :random {:enabled? true}})

(def colors
  [[:ember "Ember" [1.0 0.025 0.008] "#f45232"]
   [:merlot "Merlot" [0.65 0.015 0.07] "#ca365b"]
   [:orchid "Orchid" [0.45 0.08 0.58] "#b677d2"]
   [:indigo "Indigo" [0.06 0.08 0.6] "#6c77db"]
   [:ice "Ice" [0.2 0.65 0.72] "#69bac7"]
   [:sage "Sage" [0.18 0.55 0.32] "#82b58d"]
   [:amber "Amber" [1.0 0.3 0.025] "#dda647"]
   [:moon "Moon" [0.7 0.65 0.55] "#c9c1ae"]])

(defn definitions []
  (into
   (mapv (fn [[id title color swatch]]
           {:id id :version 1 :name title :swatch swatch :tags [:color :emission] :ui/layout :wash
            :duration {:bars 2} :release-duration {:beats 3/2}
            :params {:level level}
            :outputs {:color (constant color) :dimmer (param :level)}}) colors)
   [{:id :breathe :version 1 :name "Breathe" :swatch "#98d4bc"
     :tags [:intensity] :ui/layout :pulse :duration {:bars 2} :release-duration {:beats 3/2}
     :params {:level level :period {:type :number :default 8 :bounds [1/2 32] :unit :beats}}
     :outputs {:dimmer {:op :multiply :args [(param :level)
                                      {:op :add :args [(constant 0.5)
                                                       {:op :multiply :args [(constant 0.5)
                                                                        {:op :sin :args [{:op :multiply :args [(constant (* 2 Math/PI))
                                                                                                                   {:op :divide :args [{:op :beat} (param :period)]}]}]}]}]}]}}}
    {:id :sweep :version 1 :name "Slow sweep" :swatch "#85b6e0"
     :tags [:movement] :ui/layout {:template :movement :monitor :width} :duration {:bars 4} :release-duration {:beats 1/2}
     :params {:width {:type :number :ui/widget :fader :default 0.3 :bounds [0.0 0.45]}}
     :outputs {:pan {:op :add :args [(constant 0.5)
                                   {:op :multiply :args [(param :width)
                                                    {:op :sin :args [{:op :multiply :args [{:op :bar} (constant (/ Math/PI 2))]}]}]}]}
               :tilt (constant 0.55)}}
    {:id :spot :version 1 :name "Solitary" :ui/layout :wash :swatch "#e5d5b5" :tags [:color :emission]
     :targets [:head-4] :duration {:beats 5} :release-duration {:beats 2}
     :params {:level (assoc level :default 0.9)}
     :outputs {:color (constant [0.9 0.76 0.55]) :dimmer (param :level)}}
    {:id :flash :version 1 :name "Flash" :ui/layout :flash :swatch "#e9e5d9" :tags [:emission :transient]
     :duration {:beats 1/4} :release-duration {:beats 1/2}
     :params {:level (assoc level :default 1.0)}
     :outputs {:color (constant [1.0 0.95 0.9]) :dimmer (param :level)}}]))

(defn model []
  (let [effects (mapv #(assoc % :envelope {:attack {:beats (if (= :flash (:id %)) 0 1/8)}
                                         :release (:release-duration %)}) (definitions))
        cues (mapv (fn [effect]
                     {:id (:id effect) :version 1 :name (:name effect)
                      :effect-id (:id effect) :swatch (:swatch effect)
                      :tags (:tags effect) :params {} :duration (:duration effect)
                      :priority (case (:id effect) :breathe 10 :sweep 10 :flash 100 :spot 20 0)
                      :reload-policy :preserve-state}) effects)]
    (assoc (control/new-show
            {:registry {:effects (into {} (map (juxt :id identity)) effects)
                        :cues (into {} (map (juxt :id identity)) cues)}
             :pages {:main {:id :main :name "Night studies"
                            :cells (into {} (map-indexed (fn [i cue] [[(mod i 8) (quot i 8)] (:id cue)]) cues))}}})
           :rig {:fixtures (fixtures) :room {:width 12 :height 5 :depth 10}
                 :trusses [{:id :back-truss :position [0 4.15 -1.5] :length 9}]}
           :macros {} :sequences {})))
