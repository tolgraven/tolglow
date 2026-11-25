(ns tolglow.studio.autopilot-test
  (:require [cljs.test :refer-macros [deftest is]]
            [tolglow.studio.autopilot :as autopilot]
            [tolglow.studio.autopilot-signal :as signal]
            [tolglow.studio.events-test :refer [transition]]
            [tolglow.studio.lighting-policy :as policy]))

(def db {:connection {:connected? true}
         :server-projection {:view {:epoch 2 :runtime-id "server" :show-id :club :revision 4
                                   :settings {:autopilot {:targets {:brightness 0.5 :activity 0.3}
                                                          :signals {:bass {:variable [:music/bass]}}}}}}})

(deftest target-changes-share-one-command-and-preserve-remote-siblings
  (let [edit #(autopilot/edit-db %1 :autopilot %2 %3)
        db (-> db (edit [:targets :activity] {:waveform :sine :phase :local :range [0 0.5] :period {:beats "3/4"}})
               (edit [:targets :activity] #(assoc % :waveform :user/layers))
               (edit [:targets :activity] #(assoc-in % [:range 0] 0.25))
               (edit [:scope] {:page-id "two"}))
        result (transition db [::autopilot/save :autopilot])
        body (get-in result [:studio/http :body])]
    (is (= {:scope :autopilot :expected-epoch 2 :runtime-id "server"
            :values {:targets {:activity {:waveform :user/layers :phase :local :range [0.25 0.5] :period {:beats 0.75}}}
                     :scope {:page-id "two"}}} body))
    (is (= 0.5 (get-in (policy/visible-settings (:db result) :autopilot) [:targets :brightness])))
    (is (= :user/layers (get-in (policy/visible-settings (:db result) :autopilot) [:targets :activity :waveform])))
    (is (get-in result [:studio/http :typed?]))))

(deftest numeric-scope-and-signal-bindings-survive-typed-edits
  (let [audio (signal/starting-spec :audio autopilot/unit-opts 0.4 {})
        custom {:variable [:meter/custom] :default 0.1}
        edited (autopilot/patch {[:targets :brightness] audio [:targets :activity] {:signal :bass}
                                 [:hdr :radius] {::signal/edn (pr-str custom)}
                                 [:window] {:bars "3/2"} [:interval] {:beats "1/8"}
                                 [:scope] {:fixture-ids [:left/side "side"]}
                                 [:seed] "42"})]
    (is (= custom (get-in edited [:hdr :radius])))
    (is (= {:brightness {:variable [:audio/energy] :default 0.4} :activity {:signal :bass}} (:targets edited)))
    (is (= {:bars 1.5} (:window edited)))
    (is (= {:beats 0.125} (:interval edited)))
    (is (= 42 (:seed edited)))
    (is (= [:left/side "side"] (get-in edited [:scope :fixture-ids])))
    (is (thrown? js/Error (autopilot/patch {[:window] {:ms 0}})))
    (is (= {:signals {"a/b" custom}} (autopilot/patch {[:signals "a/b"] {::signal/edn (pr-str custom)}})))
    (is (thrown? js/Error (autopilot/patch {[:seed] "0.4"})))))

(deftest waveform-reference-retains-wall-time-bindings-and-uses-typed-musical-phase
  (let [wave (signal/starting-spec :waveform autopilot/unit-opts 0.4 {})
        cue (signal/set-waveform-reference wave :launch)
        wall (signal/set-waveform-reference (assoc cue :period {:ms 125}) :launch)]
    (is (= :transport (:phase-reference wave)))
    (is (= :launch (:phase-reference (signal/parse-spec cue))))
    (is (= :local (:phase wall)))
    (is (= :launch (signal/waveform-reference wall)))
    (is (nil? (:phase-reference wall)))
    (is (= :transport (:phase-reference (signal/set-waveform-reference (assoc wall :period {:bars 1}) :transport))))))

(deftest stale-waveform-registry-and-settings-writes-stay-in-their-own-show
  (let [loading (transition db [::signal/load-registry])
        token (second (get-in loading [:studio/http :on-success]))
        next-db (assoc-in (:db loading) [:server-projection :view :epoch] 3)]
    (is (= "/api/waveforms" (get-in loading [:studio/http :path])))
    (is (= next-db (:db (transition next-db [::signal/registry token {:waveforms {:secret {}}}]))))
    (is (empty? (:db (transition {:connection {:connected? false}} [::autopilot/save :autopilot]))))
    (let [edited (autopilot/edit-db db :hdr [:scope] {:group-id :floor})
          response (transition edited [::autopilot/save :hdr])]
      (is (= :hdr (get-in response [:studio/http :body :scope])))
      (is (= {:scope {:group-id :floor}} (get-in response [:studio/http :body :values]))))))

(deftest enabling-applies-the-selected-scope-and-targets-together
  (let [pending (-> db
                    (autopilot/edit-db :autopilot [:scope] {:group-id :stage})
                    (autopilot/edit-db :autopilot [:targets :activity] 0.25))
        enabled (autopilot/write-settings pending :autopilot true)
        global (autopilot/edit-db pending :autopilot [:scope] :global)
        unfinished (autopilot/edit-db pending :autopilot [:window] {:beats ""})]
    (is (= {:enabled? true :scope {:group-id :stage} :targets {:activity 0.25}}
           (get-in enabled [:studio/http :body :values])))
    (is (= :global (get-in (autopilot/write-settings global :autopilot true) [:studio/http :body :values :scope])))
    (is (nil? (:studio/http (autopilot/write-settings unfinished :autopilot true))))
    (is (= {:enabled? false} (get-in (autopilot/write-settings unfinished :autopilot false) [:studio/http :body :values])))
    (is (thrown? js/Error (signal/read-form "{:signal :bass} {:waveform :sine}")))))

(deftest scaled-waveforms-can-use-rhythmic-random-or-cue-bound-amplitudes
  (let [spec {:waveform :triangle :phase :local :period {:bars "3/2"}
              :range [{:cue-id "a/b" :param :level :default 0}
                      {:random {:seed "7" :distribution :triangular} :period {:beats "1/4"} :range [0.5 1]}]}
        result (signal/parse-spec spec)]
    (is (= {:bars 1.5} (:period result)))
    (is (= 7 (get-in result [:range 1 :random :seed])))
    (is (= {:beats 0.25} (get-in result [:range 1 :period])))
    (is (= "a/b" (get-in result [:range 0 :cue-id])))
    (is (= :random (signal/signal-kind (get-in result [:range 1]))))
    (is (= :cue (signal/signal-kind (get-in result [:range 0]))))))

(deftest action-readback-shows-ownership-and-no-action-reasons
  (is (= "no eligible look" (autopilot/action-label :no-eligible-look)))
  (is (= "param/set · level" (autopilot/action-label {:op :param/set :param :level :instance-id "a"})))
  (is (nil? (autopilot/action-label nil))))

(deftest target-response-edits-preserve-independent-attack-and-release
  (let [saved (assoc-in db [:server-projection :view :settings :autopilot :target-response]
                        {:release {:blend 0.25}})
        edited (autopilot/edit-db saved :autopilot [:target-response :attack] {:beats "3/8"})
        request (autopilot/write-settings edited :autopilot nil)]
    (is (= {:target-response {:attack {:beats 0.375}}} (get-in request [:studio/http :body :values])))
    (is (= {:attack {:beats 0.375} :release {:blend 0.25}}
           (:target-response (policy/visible-settings (:db request) :autopilot))))
    (is (= {:ms 0} (autopilot/target-stage {:ms "0"})))
    (is (= {:blend 0.25} (autopilot/target-stage {:blend "1/4"})))
    (is (thrown? js/Error (autopilot/target-stage {:blend 2})))))

(deftest an-authored-falloff-curve-stays-selectable-and-restorable
  (let [curve [[0 1] [0.3 1] [0.6 0.8] [1 0]]
        db (assoc-in db [:server-projection :view :settings :hdr] {:enabled? true :falloff curve})
        current #(get-in (policy/visible-settings % :hdr) [:falloff])
        labels #(set (map second (autopilot/falloff-choices %1 %2)))]
    (is (contains? (labels curve curve) "Authored curve · 4 points"))
    (is (not-any? #(re-find #"Restore" %) (labels curve curve)))
    (let [picked (autopilot/edit-db db :hdr [:falloff] :quadratic)
          save #(get-in (transition % [::autopilot/save :hdr]) [:studio/http :body :values :falloff])
          choices (autopilot/falloff-choices :quadratic (current db))]
      (is (= :quadratic (save picked)))
      (is (= [curve "Restore authored curve · 4 points"] (last choices)))
      (is (= curve (save (autopilot/edit-db picked :hdr [:falloff] (first (last choices)))))))))
