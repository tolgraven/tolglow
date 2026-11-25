(ns tolglow.studio.sensors-test
  (:require [cljs.test :refer-macros [deftest is async]]
            [cljs.reader :as reader]
            [re-frame.db :as rf-db]
            [re-frame.interceptor :as interceptor]
            [re-frame.registrar :as registrar]
            [tolglow.studio.sensors :as sensors]
            [tolglow.studio.events]))
(defn transition [db event]
  (reset! rf-db/app-db db)
  (:effects (interceptor/execute event (remove #(= :do-fx (:id %)) (registrar/get-handler :event (first event))))))

(def base {:show {:runtime-id "server-a" :epoch 3
                 :instances [{:id "3:1" :effect-id "light"}]
                 :effects [{:id "light" :params {:level {:type "number" :default 0.4 :bounds [0 1]}
                                               :turn {:type "integer" :default 0 :bounds [-90 90]}}}]}
           :sensors (assoc sensors/defaults :device-id "browser-test" :session-id "sample-session" :paired? true)})

(deftest physical-units-and-missing-measurements-remain-distinct
  (let [out (sensors/normalize-motion {:acceleration {:x 0 :y nil :z 1.4}
                                       :rotationRate {:alpha 180 :beta -90 :gamma js/NaN}})]
    (is (= 0 (:accel-x out))) (is (= 1.4 (:accel-z out)))
    (is (not (contains? out :accel-y))) (is (not (contains? out :rotation-y)))
    (is (= js/Math.PI (:rotation-z out))) (is (= (- (/ js/Math.PI 2)) (:rotation-x out)))
    (is (= {} (sensors/normalize-motion {:accelerationIncludingGravity {:x 9.8}})))
    (is (= {} (sensors/normalize-orientation {:alpha nil :beta js/Infinity :gamma "0"})))
    (is (= {:orientation-beta js/Math.PI} (sensors/normalize-orientation {:beta 180})))) )

(deftest coalescing-does-not-resend-expired-or-future-axis-values
  (is (= {:fresh 0} (sensors/fresh-values {:fresh {:value 0 :at 1000} :old {:value 4 :at 749}
                                         :future {:value 3 :at 1001} :invalid {:value nil :at 1000}} 1000)))
  (is (= 50 sensors/sample-period-ms)))

(deftest pairing-and-reading-status-never-start-or-arm-sensors
  (let [opened (transition base [:sensors/open])
        poll (transition (:db opened) [:sensors/poll])
        paired (transition base [:sensors/paired (sensors/token base) {:device-id "browser-test" :pairing-token "secret-once"}])]
    (is (nil? (:sensors/start opened))) (is (nil? (:studio/http opened)))
    (is (= "/api/sensors" (get-in poll [:studio/http :path])))
    (is (nil? (get-in poll [:studio/http :body])))
    (is (= ["browser-test" "secret-once"] (:sensors/store-token paired)))
    (is (not (clojure.string/includes? (pr-str (:db paired)) "secret-once")))
    (is (= [:sensors/save-mapping] (:dispatch paired)))
    (is (nil? (:sensors/start paired)))))

(deftest generated-motion-mapping-captures-current-types-and-musical-gestures
  (let [db (assoc-in base [:sensors :mapping] {:instance-id "3:1" :param "turn" :cue-id "show/accent"})
        config (sensors/mapping-config db)
        saved (transition db [:sensors/save-mapping])
        source (reader/read-string (get-in saved [:studio/http :body :source]))]
    (is (= [-90 90] (get-in config [:channels 3 :transforms 2 :out])))
    (is (= :integer (get-in config [:bindings 0 :schema :type])))
    (is (= {:op :param/set :instance-id "3:1" :param :turn} (get-in config [:bindings 0 :command])))
    (is (= {:beats 0.25} (get-in config [:gestures 0 :cooldown])))
    (is (= :show/accent (get-in config [:gestures 0 :command :cue-id])))
    (is (= config source))
    (is (= "server-a" (get-in saved [:studio/http :body :runtime-id])))
    (is (= 3 (get-in saved [:studio/http :body :expected-epoch])))
    (is (thrown? js/Error (sensors/mapping-config (assoc-in db [:sensors :mapping :instance-id] "expired"))))))

(deftest stopped-or-replaced-sessions-ignore-late-samples-and-release-late-arm
  (let [db (assoc-in base [:sensors :capture] :listening)
        t (sensors/token db) stopped (transition db [:sensors/stop])
        changed (transition db [:sensors/runtime-change])
        readings (transition (:db stopped) [:sensors/readings t "sample-session" {:values {:accel-x 42}}])
        late (transition (:db changed) [:sensors/loaded t {:sources [:obsolete]}])
        run {:device-id "browser-test" :session-id "sample-session" :runtime-id "server-a" :epoch 3}
        armed (transition (:db stopped) [:sensors/armed t run true {:status "accepted"}])]
    (is (contains? stopped :sensors/stop)) (is (= {} (get-in readings [:db :sensors :values])))
    (is (nil? (:db late))) (is (= run (:sensors/disarm armed)))
    (is (true? (:sensors/stop changed)))
    (is (false? (get-in changed [:db :sensors :paired?])))))

(deftest calibration-and-arming-have-explicit-preconditions
  (let [silent (transition base [:sensors/action :arm true])
        db (-> base (assoc-in [:sensors :capture] :listening) (assoc-in [:sensors :values] {:orientation-beta 0}))
        arm (transition db [:sensors/action :arm true])
        armed (assoc-in db [:sensors :status :sources] [{:device-id "browser-test" :armed? true}])
        calibrate (transition armed [:sensors/action :calibrate])]
    (is (nil? (:studio/http silent)))
    (is (= "sample-session" (get-in arm [:studio/http :body :session-id])))
    (is (true? (get-in arm [:studio/http :body :armed?])))
    (is (nil? (:studio/http calibrate)))))

(deftest permission-is-requested-synchronously-before-awaiting
  (async done
    (let [called (atom false)
          constructor #js {:requestPermission (fn [] (reset! called true) (js/Promise.resolve "granted"))}
          pending (sensors/permission-request constructor)]
      (is @called)
      (-> pending (.then (fn [value] (is (= "granted" value)) (done)))))))

(deftest browser-event-prototype-fields-are-read-as-real-properties
  (let [event (js/Object.create #js {:acceleration #js {:x 1 :y nil :z 0}
                                     :rotationRate #js {:beta 180}})]
    (is (= {:accel-x 1 :accel-z 0 :rotation-x js/Math.PI} (sensors/event-values :motion event))))
  (is (= {:orientation-alpha js/Math.PI} (sensors/event-values :orientation (js/Object.create #js {:alpha 180})))) )

(deftest cleanup-removes-only-owned-handles-and-can-forget-credentials
  (let [before @sensors/resources removed (atom [])
        handle (fn [id] #js {:remove (fn [] (swap! removed conj id) (js/Promise.resolve nil))})]
    (reset! sensors/resources {:credentials {"browser-test" "token"} :run {:id "run" :handles [(handle :motion) (handle :orientation)]}})
    (sensors/stop-capture! false)
    (is (= [:motion :orientation] @removed))
    (is (nil? (:run @sensors/resources)))
    (is (= {"browser-test" "token"} (:credentials @sensors/resources)))
    (sensors/stop-capture! true)
    (is (= {} (:credentials @sensors/resources)))
    (reset! sensors/resources before)))

(deftest browser-identity-is-portable-but-never-an-authentication-token
  (let [opened (transition {} [:sensors/open])
        identified (transition (:db opened) [:sensors/identity 0 "browser-stable"])
        closed (transition (:db identified) [:sensors/close])
        late (transition (:db closed) [:sensors/identity 0 "browser-wrong"])]
    (is (= 0 (:sensors/browser-id opened)))
    (is (= "browser-stable" (get-in identified [:db :sensors :device-id])))
    (is (false? (get-in identified [:db :sensors :paired?])))
    (is (nil? (:db late)))))
