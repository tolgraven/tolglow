(ns tolglow.studio.personality-test
  (:require [cljs.test :refer-macros [deftest is testing]]
            [tolglow.studio.personality :as personality]))

(def identity-key {:epoch 3 :show-id :club :runtime-id "test-runtime"})

(def library
  [{:id :squircle :name "Corner obsession" :icon "<svg xmlns=\"http://www.w3.org/2000/svg\"/>"}
   {:id :spirals :name "Spiralling in general" :icon "<svg/>"}
   {:id "house" :name "House feel" :icon "<svg/>"}])

(def active {:id :squircle :name "Corner obsession" :seed 4 :mix [{:id :squircle :version 1 :weight 1.0}]})

(def db {:connection {:connected? true}
         :server-projection {:view (assoc identity-key :personalities {:version 1 :library library :active active})}})

(defn- body [db operation]
  (get-in (personality/request db operation) [:studio/http :body]))

(deftest an-operation-carries-the-show-session-and-goes-to-the-owner-route
  (let [effects (personality/request db {:op :select :id :spirals})]
    (is (= "/api/personality" (get-in effects [:studio/http :path])))
    (is (true? (get-in effects [:studio/http :typed?])) "typed IDs travel as Transit, not as form text")
    (is (= {:op :select :id :spirals :expected-epoch 3 :runtime-id "test-runtime"} (body db {:op :select :id :spirals})))
    (is (= {:op :clear :expected-epoch 3 :runtime-id "test-runtime"} (body db {:op :clear})))
    (is (= identity-key (get-in effects [:db :personality-ui :identity])))
    (testing "one request at a time, and none while offline"
      (is (nil? (personality/request (:db effects) {:op :clear})))
      (is (nil? (personality/request (assoc-in db [:connection :connected?] false) {:op :clear}))))
    (testing "a pending request from another show session does not block this one"
      (let [stale (assoc db :personality-ui {:identity (assoc identity-key :epoch 2) :pending ["old" "token"]})]
        (is (some? (personality/request stale {:op :clear})))))))

(deftest blending-keeps-the-heaviest-part-as-the-base
  (is (= [{:id :squircle :weight 0.7} {:id :spirals :weight 0.3}] (personality/blend-mix active :spirals 30)))
  (is (= [{:id :squircle :weight 0.5} {:id "house" :weight 0.5}] (personality/blend-mix active "house" 50)))
  (let [mixed (assoc active :mix [{:id :squircle :version 1 :weight 0.25} {:id :spirals :version 1 :weight 0.75}])]
    (is (= :spirals (personality/base-id mixed)))
    (is (= [{:id :spirals :weight 0.9} {:id "house" :weight 0.1}] (personality/blend-mix mixed "house" 10)))
    (is (nil? (personality/blend-mix mixed :spirals 10)) "a personality is not blended with itself"))
  (is (= :squircle (personality/base-id (assoc active :mix [{:id :squircle :weight 0.5} {:id :spirals :weight 0.5}]))))
  (is (nil? (personality/blend-mix active :spirals 0)))
  (is (nil? (personality/blend-mix active :spirals 100)))
  (is (nil? (personality/blend-mix nil :spirals 30))))

(deftest palette-entries-map-to-css-paint
  (is (= "rgb(255,51,38)" (personality/paint-css [1.0 0.2 0.15])))
  (is (= "linear-gradient(180deg,rgb(255,204,77) 0%,rgb(128,26,102) 100%)"
         (personality/paint-css {:gradient [{:position 0.0 :color [1.0 0.8 0.3]} {:position 1.0 :color [0.5 0.1 0.4]}]})))
  (is (= "linear-gradient(135deg,rgb(255,255,255) 0%,rgb(0,0,0) 100%)"
         (personality/paint-css {:gradient [{:position 0.0 :color [1 1 1]} {:position 1.0 :color [0 0 0]}] :angle-deg 45})))
  (is (= "linear-gradient(90deg,rgb(255,209,26) 0% 50%,rgb(26,77,230) 50% 100%)"
         (personality/paint-css {:split [[1.0 0.82 0.1] [0.1 0.3 0.9]]}))
      "a split is drawn as hard-edged bands"))

(deftest icons-and-labels-come-from-the-projection
  (is (= "data:image/svg+xml;charset=utf-8,%3Csvg%20xmlns%3D%22http%3A%2F%2Fwww.w3.org%2F2000%2Fsvg%22%2F%3E"
         (personality/icon-src (:icon (first library)))))
  (is (= [:img.personality-icon {:src (personality/icon-src "<svg/>") :width 24 :height 24 :alt "Feel" :draggable false}]
         (personality/icon {:svg "<svg/>" :size 24 :label "Feel"})))
  (is (nil? (personality/icon {:svg nil :size 24})) "no picture is invented for a missing icon")
  (is (= "squircle 67% · pill 25% · circle 8%"
         (personality/form-summary [{:form :squircle :weight 0.666667} {:form :pill :weight 0.25}
                                    {:form :circle :weight 0.083333} {:form :ring :weight 0.0}])))
  (is (not= (personality/option-key :house) (personality/option-key "house"))))
