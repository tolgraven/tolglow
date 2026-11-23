(ns tolglow.studio.keyframe-editor-test
  (:require ["react-dom/server" :refer [renderToStaticMarkup]]
            [reagent.core :as r]
            [cljs.test :refer-macros [deftest is]]
            [tolglow.transit :as transit]
            [tolglow.studio.events :as events]
            [tolglow.studio.fixture-picker :as picker]
            [tolglow.studio.effect-inspector :as inspect]
            [tolglow.studio.keyframe-editor :as editor]))

(deftest musical-durations-and-handle-edits
  (is (= 0.125 (editor/ratio-value "1/8")))
  (is (nil? (editor/ratio-value "1/0")))
  (is (= [0.0 1.5 0.75 0] (editor/move-handle [0.1 0.2 0.75 0] 0 -19 -30 [-0.5 1.5]))))

(deftest visual-builder-and-running-inspector-preserve-authored-timing
  (let [spec {:interval {:beats 0.25} :curve {:type :spring :frequency 2 :damping 0.35}
              :progress {:mode :offset :param :level}}
        definition (events/builder-definition (assoc (get-in (events/initial-db "test") [:ui :builder]) :keyframes spec) 1)
        token {:runtime-id "one" :epoch 4}
        instance {:id "4:1" :definition definition}
        db {:show token :server-projection {:view {:instances [instance]}}}
        request #(get-in (inspect/keyframe-request db ["4:1" % :keyframes] token %2) [:studio/http :body])]
    (is (= spec (:keyframes definition)))
    (is (= (dissoc definition :keyframes) (:definition (request 1 nil))))
    (is (thrown? js/Error (request 2 spec)))))

(deftest fractions-remain-edn-data-through-the-builder-and-transit
  (let [duration (editor/parse-interval "1/3")]
    (is (= "1/3" (editor/duration-text (transit/decode (transit/encode duration)))))
    (is (= "{:beats 1/3}" (pr-str {:beats duration})))
    (is (= 0.25 (editor/parse-interval "0.25")))))

(deftest spring-keyword-and-map-forms-can-be-edited
  (is (= {:type :spring :frequency 2} (editor/spring-setting :spring :frequency 2)))
  (is (= {:type :spring :frequency 2 :damping 0.3}
         (editor/spring-setting {:type :spring :frequency 2} :damping 0.3))))

(deftest a-large-rig-bounds-options-and-retains-the-selected-target
  (let [fixtures (mapv #(hash-map :id (keyword (str "pixel-" %))) (range 4000))]
    (is (= 49 (count (picker/fixture-options fixtures "" :pixel-0))))
    (is (= [:pixel-0 :pixel-3988] (picker/fixture-options fixtures "3988" :pixel-0)))))

(deftest both-inspectors-bound-the-listed-rows-for-dense-fixtures
  (let [rig (mapv (fn [n] {:id n :name (str "Emitter " n)}) (range 20000))]
    (is (= 48 (count (picker/fixture-options rig "" nil))))
    (is (= [12 19876] (picker/fixture-options rig "emitter 19876" 12)))))


(deftest cap-only-editor-renders-and-can-change-its-authored-interval-independently
  (let [cap {:max-interval {:beats (editor/parse-interval "1/8")} :curve :spring}
        mixed (editor/toggle-duration cap :interval {:bars 1} true)
        html (renderToStaticMarkup (r/as-element [editor/editor cap [] identity]))]
    (is (.includes html "Maximum sample interval"))
    (is (.includes html "1/8"))
    (is (not (.includes html "aria-label=\"Keyframe interval\"")))
    (is (= cap (editor/toggle-duration mixed :interval nil false)))
    (is (= {:interval {:bars 1} :curve :spring} (editor/toggle-duration mixed :max-interval nil false)))
    (is (nil? (editor/toggle-duration cap :max-interval nil false)))))
