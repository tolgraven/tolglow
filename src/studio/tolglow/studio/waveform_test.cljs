(ns tolglow.studio.waveform-test
  (:require [cljs.test :refer-macros [deftest is]]
            [re-frame.db :as rf-db]
            [re-frame.interceptor :as interceptor]
            [re-frame.registrar :as registrar]
            [tolglow.studio.events :as events]
            [tolglow.studio.waveform :as waveform]))

(def base (assoc (events/initial-db "test") :show {:runtime-id "runtime-a" :epoch 4}))

(def preview-key ["4:1" "waveform"])

(def choices [{:value 0 :id :sine :version 1 :label "Sine" :category :smooth}
              {:value 1 :id :user/pulse :version 2 :label "Pulse" :category :discontinuous}])

(def selection {:selector 0 :choices choices})

(def samples {:samples [{:phase 0 :value 0 :slope 2 :angular-velocity 720 :jagginess 0}
                        {:phase 0.5 :value 1 :slope 0 :angular-velocity 0 :jagginess 16}
                        {:phase 1 :value 0 :slope -2 :angular-velocity -720 :jagginess 0}]
              :units {:value "amplitude" :slope "amplitude-per-cycle" :angular-velocity "degrees-per-cycle"
                      :jagginess "amplitude-per-cycle-squared"}})

(defn transition [db event]
  (reset! rf-db/app-db db)
  (:effects (interceptor/execute event (remove #(= :do-fx (:id %))
                                               (registrar/get-handler :event (first event))))))

(defn ensure-preview [db selection]
  (transition db [:waveform/ensure preview-key selection {} {}]))

(defn send-preview [ensured]
  (transition (:db ensured) (get-in ensured [:dispatch-later 0 :dispatch])))

(deftest only-clock-bound-preview-inputs-request-a-moving-captured-clock
  (is (waveform/clock-bound? {:choices [{:params {:duty {:op "multiply" :args [{:op "beat"} {:op "const" :value 0.2}]}}}]}))
  (is (waveform/clock-bound? {:preview/options {:params {:phase {:op :time}}}}))
  (is (not (waveform/clock-bound? {:choices [{:params {:duty {:op :param :key :width}}}]}))))

(deftest continuous-selection-matches-half-up-runtime-boundaries
  (is (waveform/waveform-schema? {:semantic/type "waveform-selector"}))
  (is (not (waveform/waveform-schema? {:type :enum})))
  (is (= 0 (waveform/choice-index choices 0.49)))
  (is (= 1 (waveform/choice-index choices 0.5)))
  (is (= 1 (waveform/choice-index choices 20)))
  (is (= 0 (waveform/choice-index choices -20)))
  (is (= ["4:1" "user/waveform"] (waveform/picker-key "4:1" :user/waveform))))

(deftest preview-preserves-identity-and-only-sends-resolvable-numeric-bindings
  (let [payload (waveform/preview-payload
                 (assoc-in selection [:choices 1 :params] {:user/duty {:op :const :value 0.25}})
                 {:samples 129 :params {:user/level 0.5}} {:cycles "2" :window "0.001"})]
    (is (= {:id "user/pulse" :version 2 :params {"user/duty" 0.25}} (get-in payload [:selection :choices 1])))
    (is (= 2 (get-in payload [:options :cycles])))
    (is (= 129 (get-in payload [:options :samples])))
    (is (= {"user/level" 0.5} (get-in payload [:options :params]))))
  (is (= "user/sine" (:id (waveform/preview-payload :user/sine {} {}))))
  (doseq [settings [{:samples "2049"} {:samples "2.5"} {:cycles "0"} {:cycles "Infinity"}
                    {:window "0"} {:window "0.5"} {:angle-span-deg "0"} {:window ""}]]
    (is (thrown? js/Error (waveform/preview-payload selection {} settings))))
  (is (thrown? js/Error (waveform/preview-payload
                         (assoc-in selection [:choices 1 :params] {:duty {:op :param :key :level}}) {} {}))))

(deftest real-server-samples-project-with-zero-baseline-and-true-units
  (let [value (waveform/chart-data (:samples samples) :value)
        slope (waveform/chart-data (:samples samples) :slope)]
    (is (= "10,164 256,20 502,164" (:points value)))
    (is (= [0 1] [(:minimum value) (:maximum value)]))
    (is (= [-2 2] [(:low slope) (:high slope)]))
    (is (= 92 (:zero-y slope)))
    (is (nil? (waveform/chart-data (assoc-in (:samples samples) [1 :value] js/NaN) :value)))
    (is (nil? (waveform/chart-data (assoc-in (:samples samples) [1 :phase] -1) :value)))
    (is (nil? (waveform/chart-data (vec (repeat 2049 (first (:samples samples)))) :value))))
  (is (= 92 (:zero-y (waveform/chart-data [{:phase 0 :value 0} {:phase 1 :value 0}] :value))))
  (is (= "10,164 502,20"
         (:points (waveform/chart-data [{:phase 0 :value -1.0e308} {:phase 1 :value 1.0e308}] :value)))))

(deftest timeline-guides-use-the-captured-beat-and-drop-stale-transport
  (let [clock {:source "link" :source-epoch 2 :source-selection-epoch 1 :playing? true
               :beat 4.5 :meter {:beats-per-bar 3.5} :generation 8}
        timeline {:beat 4.25 :transport (dissoc clock :beat :generation) :transport-generation 8
                  :ticks [{:position -1 :kind "phrase"} {:position 0 :kind "bar"}
                          {:position 0.5 :kind "beat"} {:position 100 :kind "beat"}]
                  :samples [{:phase -1 :value 0} {:phase 0 :value 1} {:phase 1 :value 0}]}
        chart (waveform/timeline-chart timeline clock)]
    (is (= "10,164 256,20 502,164" (:points chart)))
    (is (= [{:x 10 :kind "phrase"} {:x 256 :kind "bar"} {:x 379 :kind "beat"}] (:ticks chart)))
    (doseq [changed [(assoc clock :source-epoch 3) (assoc clock :beat 3.75) (assoc clock :generation 9)
                     (assoc clock :source-selection-epoch 2) (assoc clock :playing? false)
                     (assoc clock :beat 20) (assoc-in clock [:meter :beats-per-bar] 4)]]
      (is (nil? (waveform/timeline-chart timeline changed))))
    (is (nil? (waveform/timeline-chart nil clock)))))

(deftest untimed-selectors-refresh-on-a-new-meter-or-generation-only
  (let [clock {:source "link" :source-epoch 2 :generation 8 :beat 4.5 :meter {:beats-per-bar 3.5}}
        key #(waveform/live-timing-key {} %)
        moving-key #(waveform/live-timing-key {:data {:timeline {:beat 4.5}}} %)]
    (is (= (key clock) (key (assoc clock :beat 4.75))))
    (is (not= (key clock) (key (assoc clock :generation 9))))
    (is (not= (key clock) (key (assoc-in clock [:meter :beats-per-bar] 4))))
    (is (not= (moving-key clock) (moving-key (assoc clock :beat 4.75))))))

(deftest requests-coalesce-and-never-overlap-for-one-preview
  (let [first (ensure-preview base selection)
        second (ensure-preview (:db first) (assoc selection :selector 1))
        sent (transition (:db second) (get-in first [:dispatch-later 0 :dispatch]))
        pending (ensure-preview (:db sent) selection)
        received (transition (:db pending) (conj (get-in sent [:studio/http :on-success]) samples))
        next (send-preview received)]
    (is (= 120 (get-in first [:dispatch-later 0 :ms])))
    (is (nil? (:dispatch-later second)))
    (is (= "/api/waveforms/preview" (get-in sent [:studio/http :path])))
    (is (= 1 (get-in sent [:studio/http :body :selection :selector])))
    (is (nil? (:studio/http pending)))
    (is (nil? (:dispatch-later pending)))
    (is (= samples (get-in received [:db :ui :waveform :entries preview-key :data])))
    (is (= 0 (get-in next [:studio/http :body :selection :selector])))
    (is (= 2 (get-in next [:db :ui :waveform :entries preview-key :request-id])))))

(deftest fresh-preview-and-channel-switch-do-not-repeat-http
  (let [sent (send-preview (ensure-preview base selection))
        received (transition (:db sent) (conj (get-in sent [:studio/http :on-success]) samples))
        unchanged (ensure-preview (:db received) selection)
        channel (transition (:db unchanged) [:waveform/channel preview-key :jagginess])]
    (is (nil? (:dispatch-later received)))
    (is (nil? (:dispatch-later unchanged)))
    (is (nil? (:studio/http channel)))
    (is (= :jagginess (get-in channel [:db :ui :waveform :entries preview-key :channel])))))

(deftest leaving-the-preview-and-replacing-the-runtime-stop-old-work
  (let [ensured (ensure-preview base selection)
        left (transition (:db ensured) [:waveform/deactivate preview-key])
        timer (transition (:db left) (get-in ensured [:dispatch-later 0 :dispatch]))
        sent (send-preview (ensure-preview base selection))
        changed (transition (:db sent) [:waveform/runtime-change])
        old (transition (:db changed) (conj (get-in sent [:studio/http :on-success]) samples))]
    (is (nil? (:studio/http timer)))
    (is (empty? old))
    (is (= {} (get-in changed [:db :ui :waveform :entries])))
    (is (not= (waveform/request-token base) (waveform/request-token (:db changed))))))

(deftest failures-are-visible-retriable-and-guarded-against-old-responses
  (let [sent (send-preview (ensure-preview base selection))
        failed (transition (:db sent) (conj (get-in sent [:studio/http :on-failure]) "Unknown waveform"))
        same (ensure-preview (:db failed) selection)
        retry (transition (:db same) [:waveform/retry preview-key])
        resent (send-preview retry)
        old (transition (:db resent) (conj (get-in sent [:studio/http :on-success]) samples))
        invalid (transition (:db resent) (conj (get-in resent [:studio/http :on-success]) {:samples []}))]
    (is (= "Unknown waveform" (get-in failed [:db :ui :waveform :entries preview-key :error])))
    (is (nil? (:dispatch-later same)))
    (is (seq (:dispatch-later retry)))
    (is (empty? old))
    (is (= "The server returned invalid waveform samples." (get-in invalid [:db :ui :waveform :entries preview-key :error])))))

(deftest live-shape-request-uses-instance-identity-and-invalidation-key
  (let [selection {:instance-id "4:1" :parameter :user/wave :input-key "params-a"}
        first (transition base [:waveform/ensure preview-key selection {} {}])
        sent (send-preview first)
        changed (transition (:db sent) [:waveform/ensure preview-key (assoc selection :input-key "params-b") {} {}])
        received (transition (:db changed) (conj (get-in sent [:studio/http :on-success]) samples))]
    (is (= "4:1" (get-in sent [:studio/http :body :instance-id])))
    (is (= "user/wave" (get-in sent [:studio/http :body :parameter])))
    (is (= "params-a" (get-in sent [:studio/http :body :input-key])))
    (is (nil? (:studio/http changed)))
    (is (= 120 (get-in received [:dispatch-later 0 :ms])))
    (is (= "params-b" (get-in received [:db :ui :waveform :entries preview-key :desired :input-key])))))

(deftest draft-shape-preserves-expression-bindings-for-the-host
  (let [payload (waveform/preview-payload
                 (assoc-in selection [:choices 1 :params] {:duty {:op :param :key :user/duty}})
                 {:context-params {:user/duty 0.25}} {})]
    (is (= {"user/duty" 0.25} (:context-params payload)))
    (is (= {"duty" {"op" "param" "key" "user/duty"}} (get-in payload [:selection :choices 1 :params])))
    (is (nil? (get-in payload [:options :context-params])))))
