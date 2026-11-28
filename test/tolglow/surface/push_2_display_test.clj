(ns tolglow.surface.push-2-display-test
  "The native Push 2 display draws linked knobs (#666) into a headless
  image through the renderer the Wayang transport is given. No display is
  opened; Wayang itself and the physical screen are not exercised."
  (:require [afterglow.control.devices.transports :as transports]
            [clojure.test :refer [deftest is testing]]
            [tolglow.surface.push-2-display :as push-2-display])
  (:import [java.awt.image BufferedImage]))

(def ^:private page-view
  {:title "Tolglow"
   :knob-page {:knob-ids [:look :gone]
               :knobs [{:knob-id :look :name "Look" :value 0.5 :text "0.500" :links 2}
                       {:knob-id :gone :missing? true}]}})

(def ^:private touched-view
  {:touched-control {:target :knob :knob-id :look :editable? true}
   :knob {:knob-id :look :name "Look" :value 0.25 :text "0.250"
          :columns [{:label "Tint sat" :status :ok :text "0.400"}
                    {:label "Pulse rate" :status :ok :text "1/2 beats" :ratio [1 2] :diverged? true}
                    {:label "Gone" :status :missing-target}]}
   :knob-page (:knob-page page-view)})

(defn- image [] (BufferedImage. 960 160 BufferedImage/TYPE_INT_RGB))

(defn- rgb [^BufferedImage image x y]
  (let [c (.getRGB image x y)] [(bit-and 0xff (bit-shift-right c 16)) (bit-and 0xff (bit-shift-right c 8)) (bit-and 0xff c)]))

(deftest the-knob-page-and-a-touched-knob-become-display-columns
  (is (= {:title "Knobs"
          :columns [{:label "Look" :text "0.500" :dangling? false :value 0.5}
                    {:label "gone" :text "Removed" :dangling? true}]}
         (push-2-display/knob-panel page-view)))
  (testing "a touched knob wins over the page and lists its links"
    (is (= {:title "Knob Look  0.250"
            :columns [{:label "Tint sat" :text "0.400" :dangling? false :diverged? false}
                      {:label "Pulse rate" :text "1/2 beats" :dangling? false :diverged? true}
                      {:label "Gone" :text "missing-target" :dangling? true :diverged? false}]}
           (push-2-display/knob-panel touched-view))))
  (is (nil? (push-2-display/knob-panel {:title "Tolglow" :parameters []})) "No knobs, no panel"))

(deftest knobs-are-drawn-and-other-views-are-left-to-afterglow
  (testing "the knob page draws a value bar per existing knob"
    (let [drawn (push-2-display/render-view! (image) page-view)]
      (is (= [105 192 236] (rgb drawn 20 126)) "Look, half way: inside its bar")
      (is (= [45 55 70] (rgb drawn (+ 8 (- 120 16) -2) 126)) "past half: the empty track")
      (is (= [9 12 18] (rgb drawn (+ 8 120 20) 126)) "The removed knob has no bar")))
  (testing "a parameter view renders exactly as Afterglow's display does"
    (let [view {:title "Wash" :page "1/1" :status :ready :bpm 120.0 :master 1.0 :parameters [] :labels ["A" "B"]}
          ours (push-2-display/render-view! (image) view)
          theirs (transports/render-view! (image) view)]
      (is (= (vec (.getRGB ^BufferedImage ours 0 0 960 160 nil 0 960))
             (vec (.getRGB ^BufferedImage theirs 0 0 960 160 nil 0 960)))))))
