(ns tolglow.output-response-test
  (:require [afterglow.control.model :as model]
            [afterglow.render.emission :as light]
            [afterglow.render.frame :as frame]
            [clojure.test :refer [deftest is]]
            [tolglow.output-response :as response]
            [tolglow.show :as host]))

(def fixtures [{:id :front :groups [:wash :front] :universe 0 :address 1 :channels {:dimmer 1 :red 2 :blue 3 :strobe 4}}
               {:id :back :groups [:wash] :universe 0 :address 5 :channels {:dimmer 1}}])

(def patch (frame/compile-patch fixtures))

(defn source [front back]
  (frame/resolve-frame patch
    [{:target-id :front :attribute :red :value front}
     {:target-id :front :attribute :strobe :value 0.8}
     {:target-id :back :attribute :dimmer :value back}] {:include-values? false :compact-values? true}))

(def empty-frame (frame/resolve-frame patch [] {}))

(def show (model/new-show {}))

(def opts {:enabled? true :global {:attack {:beats 1/2} :release {:beats 1/2}}
           :groups {:front {:release {:bars 1}}}})

(defn tick [previous opts input ms beat & [model]]
  (let [m (-> (or model show) (assoc :time-ms ms)
              (assoc-in [:transport :beat] beat)
              (assoc-in [:settings :output-response] (response/settings opts)))]
    (response/advance (:state previous) m patch input false (or (:master m) 1.0) nil)))

(defn value [result fixture attr]
  (get (light/emitted-levels (get-in patch [:by-id fixture]) (:frame result)) attr))

(defn near? [a b] (< (abs (- a b)) 0.008))

(deftest a-rendered-change-and-its-release-follow-the-selected-musical-clock
  (let [zero (tick nil opts empty-frame 0 0)
        rise (tick zero opts (source 1.0 1.0) 250 1/2)
        removed (tick rise opts empty-frame 500 1)
        hold (tick removed opts empty-frame 1000 1)
        e (Math/exp -1.0)]
    (is (near? (- 1 e) (value rise :front :red)))
    (is (near? (* (- 1 e) (Math/exp -0.125)) (value removed :front :red)))
    (is (near? (* (- 1 e) e) (value removed :back :dimmer)))
    (is (= (seq (get-in removed [:state :levels])) (seq (get-in hold [:state :levels]))))
    (is (zero? (bit-and 255 (aget ^bytes (get-in removed [:frame :universes 0]) 3))))
    (is (< 0.0 (value removed :front :red)))))

(deftest frame-rate-independent-and-operator-capped
  (let [lit (source 1.0 1.0)
        zero (tick nil opts empty-frame 0 0)
        one (tick zero opts lit 1000 1)
        forty (reduce #(tick %1 opts lit (* %2 25) (/ %2 40)) zero (range 1 41))
        m (assoc-in (assoc show :time-ms 1000) [:settings :output-response] opts)
        blackout (response/advance (:state forty) (assoc m :blackout? true) patch empty-frame false)
        low (tick forty opts empty-frame 1100 1.1 (assoc show :master 0.125))]
    (is (= (value one :front :red) (value forty :front :red)))
    (is (near? 0.125 (value low :front :red)))
    (is (identical? empty-frame (:frame blackout)))
    (is (nil? (:state blackout)))
    (is (identical? lit (:frame (response/advance nil show patch lit false))))))

(deftest cue-parameters-and-variables-change-the-actual-response
  (let [opts {:enabled? true :groups {:front {:attack {:beats {:cue-id :control :param :rise :default 1}}
                                               :release {:ms {:variable [:feel :fall]}}}}}
        active (assoc show :instances {"knob" {:id "knob" :cue-id :control :phase :running :order 1 :params {:rise 2.0}}})
        initial (tick nil opts empty-frame 0 0 active)
        rise (tick initial opts (source 1.0 1.0) 500 1 active)
        immediate (tick rise opts (source 1.0 1.0) 1000 2 (assoc-in active [:instances "knob" :params :rise] 0))
        fall (tick immediate opts empty-frame 1500 3 (assoc-in active [:variables :feel :fall] 500))]
    (is (near? (- 1 (Math/exp -0.5)) (value rise :front :red)))
    (is (= 1.0 (value rise :back :dimmer)))
    (is (= 1.0 (value immediate :front :red)))
    (is (near? (Math/exp -1.0) (value fall :front :red)))))

(deftest a-color-change-at-constant-dimmer-fades-through-both-colors
  (let [options {:enabled? true :global {:attack {:ms 250} :release {:ms 250}}}
        red (source 1.0 0.0)
        blue (frame/resolve-frame patch [{:target-id :front :attribute :blue :value 1.0}] {})
        m (-> show (assoc :time-ms 0) (assoc-in [:settings :output-response] options))
        seeded (response/advance nil m patch red false 1 red)
        mixed (tick seeded options blue 250 1)]
    (is (near? (Math/exp -1.0) (value mixed :front :red)))
    (is (near? (- 1 (Math/exp -1.0)) (value mixed :front :blue)))
    (is (= 1.0 (:dimmer (light/levels (get-in patch [:by-id :front]) (:frame mixed)))))))

(deftest reject-invalid-bindings-and-duration-units
  (doseq [x [{:seconds 1} {:beats -1} {:ms Double/NaN} {:beats {:variable []}}
             {:beats {:cue-id :ctl}} {:ms {:variable [:bad] :default -2}}]]
    (is (thrown? Exception (response/settings {:enabled? true :global {:attack x}})))))

(defn- packet-bytes [result]
  (update-vals (get-in result [:frame :universes]) vec))

(deftest projections-share-an-unquantized-envelope-across-frames
  (let [settings {:enabled? true :global {:attack {:ms 90} :release {:ms 330}}
                  :groups {:wash {:release {:ms 180}} :front {:attack {:ms 120} :release {:ms 300}}}}
        run (fn [compact-at?]
              (reduce
               (fn [history [i t amount]]
                 (let [compact? (compact-at? i)
                       rows [{:target-id :front :attribute :red :value amount}
                             {:target-id :front :attribute :blue :value (- 1.0 amount)}
                             {:target-id :front :attribute :strobe :value 0.75}
                             {:target-id :back :attribute :dimmer :value amount}]
                       frame (frame/resolve-frame patch rows {:include-values? (not compact?) :compact-values? compact?})
                       result (tick (peek history) settings frame t (/ t 500.0))]
                   (conj history result))) []
               [[0 0 0.0] [1 75 0.8] [2 100 1.0] [3 370 0.5] [4 500 0.0]]))
        rich (run (constantly false))
        dense (run (constantly true))
        alternating (run odd?)]
    (is (= (mapv packet-bytes rich) (mapv packet-bytes dense) (mapv packet-bytes alternating)))
    (doseq [[r c] (map vector rich dense)]
      (let [fields [:intensity :emitted-color]
            rows (frame/project-compact (get-in c [:frame :compact]))]
        (is (= (mapv #(select-keys (get-in r [:frame :values %]) fields) (:target-order patch))
               (mapv #(select-keys % fields) rows)))))))

(deftest dynamic-group-membership-changes-and-overlap-use-the-selected-stages
  (let [opt {:enabled? true :groups {:wash {:attack {:beats 1}}
                                     :solo {:attack {:beats 3}}}}
        select-group (fn [ids] (assoc-in show [:extensions :tolglow.lighting-scenes/control :groups :solo :fixture-ids] ids))
        front (select-group [:front])
        back (select-group [:back])
        start (tick nil opt (source 0.0 0.0) 0 0 front)
        lit (source 1.0 1.0)
        first-beat (tick start opt lit 500 1 front)
        saved (vec (get-in first-beat [:state :levels]))
        next-beat (tick first-beat opt lit 1000 2 back)
        slow (- 1.0 (Math/exp (/ -1.0 3.0)))
        fast (- 1.0 (Math/exp -1.0))]
    (is (near? slow (value first-beat :front :red)))
    (is (near? fast (value first-beat :back :dimmer)))
    (is (near? (+ slow (* fast (- 1.0 slow))) (value next-beat :front :red)))
    (is (near? (+ fast (* slow (- 1.0 fast))) (value next-beat :back :dimmer)))
    (is (= saved (vec (get-in first-beat [:state :levels]))))))

(deftest enabling-seeds-from-same-show-and-seeks-discard-old-tails
  (let [lit (source 1.0 1.0)
        m (-> show (assoc :time-ms 500) (assoc-in [:transport :beat] 1) (assoc-in [:settings :output-response] opts))
        disabled (response/advance nil (assoc-in m [:settings :output-response :enabled?] false) patch lit false)
        enabled (response/advance (:state disabled) m patch lit false 1.0 (:frame disabled))]
    (is (= 1.0 (value enabled :front :red)))
    (doseq [reset-model [(update m :epoch inc) (assoc-in m [:transport :beat] 0) (assoc m :time-ms 0)]]
      (let [cleared (response/advance (:state enabled) reset-model patch empty-frame false 1.0 (:frame enabled))]
        (is (zero? (value cleared :front :red)))
        (is (zero? (value cleared :back :dimmer)))))))

(deftest adding-a-fixture-to-the-envelope-seeds-its-delivered-budgeted-output
  (let [options {:enabled? true :groups {:solo {:attack {:beats 2}}}}
        select-group (fn [id] (-> show (assoc-in [:settings :output-response] options)
                                  (assoc-in [:extensions :tolglow.lighting-scenes/control :groups :solo :fixture-ids] [id])))
        lit (source 1.0 1.0)
        prev (response/advance nil (select-group :front) patch lit false)
        limited (light/scale-emission patch (:frame prev) {:back 0.25})
        next (response/advance (:state prev) (select-group :back) patch lit false 1.0 limited)]
    (is (near? 0.25 (value next :back :dimmer)))
    (is (= 1.0 (value next :front :red)))))

(deftest linear-timing-arrives-at-the-target-and-ordinary-edges-stay-continuous
  (let [options {:enabled? true :global {:attack {:mode :linear :ms 200}
                                          :release {:mode :linear :beats 1/2}}}
        lit (source 1.0 1.0)
        start (tick nil options empty-frame 0 0)
        rise (tick start options lit 50 1/8)
        crest (tick rise options lit 200 1/4)
        tail (tick crest options empty-frame 300 1/2)
        held (tick tail options empty-frame 500 1/2)
        out (tick held options empty-frame 600 3/4)
        down (tick rise options empty-frame 100 1/4)
        rebound (tick down options lit 150 3/8)]
    (is (near? 0.25 (value rise :front :red)))
    (is (= 1.0 (value crest :front :red)))
    (is (near? 0.5 (value tail :front :red)))
    (is (= (packet-bytes tail) (packet-bytes held)))
    (is (zero? (value out :front :red)))
    (is (near? 0.1875 (value down :front :red)))
    (is (near? (+ 0.1875 (* 0.25 (- 1 0.1875))) (value rebound :front :red)))
    (is (= (packet-bytes crest)
           (packet-bytes (reduce (fn [p t] (tick p options lit t 1/4)) start [11 74 164 200]))))))

(deftest frame-fraction-counts-commits-once-and-validates-bound-sources
  (let [options {:enabled? true :global {:attack {:blend {:variable [:drive] :default 1/4}}
                                         :release {:blend 0.5}}}
        lit (source 1.0 1.0)
        first (tick nil options lit 0 0)
        same (tick first options lit 0 0)
        next (tick same options lit 502 0)
        gone (tick next options empty-frame 503 0)]
    (is (near? 0.25 (value first :back :dimmer)))
    (is (= (packet-bytes first) (packet-bytes same)))
    (is (near? 0.4375 (value next :back :dimmer)))
    (is (near? 0.21875 (value gone :back :dimmer)))
    (doseq [invalid [{:blend -1} {:blend 1.1} {:mode :linear :blend 0.5}
                     {:mode :linear :beats 1 :ms 1} {:mode :unknown :ms 50}
                     {:blend {:variable [:drive] :default 2}}]]
      (is (thrown? Exception (response/settings {:global {:attack invalid}}))))))

(defn- cue-block [cue instance id level]
  {:source {:cue-id cue :instance-id instance :effect-id :shared}
   :assignments [{:target-id id :attribute :dimmer :value level :blend :replace}]})

(defn- writer-tick [prev config blocks ms model]
  (let [input (frame/resolve-frame patch (into [] (mapcat :assignments) blocks) {})
        m (-> model (assoc :time-ms ms) (assoc-in [:settings :output-response] (response/settings config)))]
    (response/advance (:state prev) m patch input false 1.0 (:frame prev) blocks)))

(deftest cue-and-macro-response-follow-emitting-members-and-retain-only-falling-tails
  (let [opts {:enabled? true
              :fixtures {:front {:attack {:ms 0}}}
              :cues {:hit {:attack {:mode :linear :ms 100}}}
              :compositions {:riff {:release {:mode :linear :ms 100}}}}
        hit (cue-block :hit "a" :back 1.0)
        other (cue-block :other "b" :back 0.25)
        m (-> show (assoc-in [:composition-runs :riff :owner-id] "run-1")
              (assoc-in [:instances "a" :source] {:adapter :composition :session-id "run-1"}))
        off (writer-tick nil opts [] 0 m)
        quarter (writer-tick off opts [other hit] 25 m)
        full (writer-tick quarter opts [other hit] 100 m)
        removed (dissoc m :instances :composition-runs)
        falling (writer-tick full opts [other] 150 removed)
        end (writer-tick falling opts [other] 200 removed)
        takeover (writer-tick quarter opts [hit (cue-block :other "b" :back 1.0)] 30 m)]
    (is (near? 0.25 (value quarter :back :dimmer)))
    (is (= 1.0 (value full :back :dimmer)))
    (is (near? 0.625 (value falling :back :dimmer)))
    (is (near? 0.25 (value end :back :dimmer)))
    (is (= 1.0 (value takeover :back :dimmer)))))

(deftest cue-removal-delivers-the-exact-scoped-release-to-the-output-sink
  (let [effect {:id :lamp :name "Lamp" :version 1 :targets [:back] :params {} :outputs {:dimmer 1.0}}
        show (-> (model/new-show {:registry {:effects {:lamp effect}
                                             :cues {:flash {:id :flash :effect-id :lamp :version 1}}}})
                 (assoc :rig {:fixtures fixtures})
                 (assoc-in [:settings :output-response]
                           {:enabled? true :cues {:flash {:release {:mode :linear :ms 100}}}}))
        last-byte (atom nil)
        runtime (host/create-runtime {:model show :workspace-options nil
                                      :sink {:send! (fn [u _] (reset! last-byte (bit-and 255 (aget ^bytes (get u 0) 4))))}})
        command (fn [cmd t] (let [ack (host/submit! runtime :command cmd)] (host/tick! runtime t) @ack))]
    (try
      (let [ack (command {:op :cue/launch :cue-id :flash} 0)]
        (is (= :accepted (:status ack)))
        (host/tick! runtime 25)
        (is (= 255 @last-byte))
        (is (= :accepted (:status (command {:op :instance/remove :instance-id (:instance-id ack)} 50))))
        (is (= 191 @last-byte))
        (host/tick! runtime 75)
        (is (= 128 @last-byte))
        (host/tick! runtime 125)
        (is (zero? @last-byte)))
      (finally (host/stop! runtime)))))

(deftest scene-membership-takes-precedence-over-a-patch-group-of-the-same-name
  (let [model (assoc-in show [:extensions :tolglow.lighting-scenes/control :groups :wash :fixture-ids] [:front])
        opt {:enabled? true :groups {:wash {:attack {:beats 1}}}}
        start (tick nil opt (source 0.0 0.0) 0 0 model)
        next (tick start opt (source 1.0 1.0) 500 1 model)]
    (is (near? (- 1.0 (Math/exp -1.0)) (value next :front :red)))
    (is (near? 1.0 (value next :back :dimmer)))))
