(ns tolglow.surface.push-2-cue-grid-test
  "Push 2 Select and Reorder pad grids (#545, #706) through
  show/connect-device!: recorded Push 2 MIDI (palette replies, LED
  acknowledgements, buttons and pads) into the Afterglow codec, profile and
  session shell, the Tolglow cue grid and surface link, and a real show
  owner. A capture transport records every byte; pad colours are read back
  from the palette writes and note messages the Push was sent. No device
  is opened."
  (:require [afterglow.control.codecs.push-2 :as codec]
            [afterglow.control.devices.session :as device-session]
            [afterglow.control.model :as control]
            [clojure.test :refer [deftest is testing]]
            [tolglow.show :as show]
            [tolglow.surface.push-2 :as push-2]
            [tolglow.surface.push-3 :as push-3]))

(def ^:private colors
  "Cue colours: red, green, blue, white."
  [[1 0 0] [0 1 0] [0 0 1] [1 1 1]])

(defn- dimmed [color] (push-2/pad-rgb {:color color}))

(defn- full [color] (push-2/pad-rgb {:color color :selected? true}))

(defn- model [n]
  (control/new-show
   {:show-id :push-2-grid :transport {:playing? true :beat 0.0 :bpm 120.0}
    :registry {:effects {:wash {:id :wash :version 1
                                :params {:level {:type :number :default 0.5 :bounds [0 1]}}
                                :outputs {:dimmer {:op :param :key :level}}}}
               :cues (into {} (for [i (range n)
                                    :let [id (keyword (str "c" i))]]
                                [id {:id id :version 1 :effect-id :wash :name (str "C" i)
                                     :color (get colors (mod i 4))}]))}
    :rig {:fixtures [{:id :head :universe 1 :address 1 :channels {:dimmer 1}}]}}))

(def ^:private led-query (codec/sysex 0 [0x09]))

(defn- harness [n]
  (let [runtime (show/create-runtime {:model (model n) :workspace-options nil})
        time (atom 0)
        receivers (atom {})
        sent (atom {})
        awaiting (atom #{})
        submit! show/submit!
        owner! (fn [f]
                 (with-redefs [show/now-ms (constantly @time)
                               show/submit! (fn submit-and-tick
                                              ([owner command] (submit-and-tick owner :command command nil))
                                              ([owner type command] (submit-and-tick owner type command nil))
                                              ([owner type command authority]
                                               (let [receipt (submit! owner type command authority)]
                                                 (show/tick! owner (swap! time inc))
                                                 receipt)))]
                   (f)))]
    {:runtime runtime :sent sent :owner! owner! :receivers receivers
     :transport (fn [unit]
                  {:open! (fn [receive!] (swap! receivers assoc unit receive!) unit)
                   :send! (fn [_ batch]
                            (swap! sent update unit (fnil into []) batch)
                            (when (some #{led-query} batch) (swap! awaiting conj unit)))
                   :close! (fn [_] (swap! sent update unit (fnil conj []) :closed))})
     :input! (fn [unit & messages] (owner! #(doseq [bytes messages] ((get @receivers unit) bytes))))
     ;; Answer each LED acknowledgement query until the Push has nothing left to send.
     :settle! (fn [unit]
                (owner! #(loop [count 0]
                           (when (contains? @awaiting unit)
                             (when (> count 30) (throw (ex-info "Push 2 LEDs did not settle" {:unit unit})))
                             (swap! awaiting disj unit)
                             ((get @receivers unit) led-query)
                             (recur (inc count))))))
     :tick! #(show/tick! runtime (swap! time inc))}))

(defn- submit! [{:keys [runtime tick!]} payload]
  (let [receipt (show/submit! runtime :command (merge {:source {:adapter :web :session-id "studio"}} payload))]
    (tick!)
    (deref receipt 1000 ::missing)))

(defn- accepted! [h payload]
  (let [ack (submit! h payload)]
    (is (= :accepted (:status ack)) (pr-str payload ack))
    ack))

(defn- model-of [{:keys [runtime]}] (:model @(:state runtime)))

(defn- launch-all! [h n]
  (mapv #(:instance-id (accepted! h {:op :cue/launch :cue-id (keyword (str "c" %)) :launch {:mode :persistent}}))
        (range n)))

(defn- order [{:keys [runtime]}]
  (let [{:keys [active-instance-order instances]} (show/snapshot runtime)
        running (into #{} (keep #(when (= :running (:phase %)) (:id %))) instances)]
    (filterv running active-instance-order)))

(defn- connect! [{:keys [runtime transport tick! input! settle!]} id & [options]]
  (let [device (show/connect-device! runtime :push-2
                                     (merge {:session-id id :schedule! nil :timeout-ms 60000 :led-batch-size 16
                                             :active-cue-grid? true :midi (transport id)}
                                            options))]
    (tick!)
    ;; The profile reads all 128 palette entries before it writes an LED.
    (apply input! id (for [index (range 128)] (assoc (codec/palette-write 0 index [0 0 0 0]) 6 4)))
    (settle! id)
    device))

(defn- refresh! [{:keys [owner! settle!]} id device]
  (owner! #(push-2/refresh! device))
  (settle! id))

(defn- take-sent! [{:keys [sent]} unit]
  (let [messages (get @sent unit)]
    (swap! sent assoc unit [])
    (vec messages)))

(defn- pad-colors
  "Pad note -> [r g b] the Push was last told to show, replaying palette
  writes (SysEx 0x03) and pad note-on/note-off messages."
  [messages]
  (:colors (reduce (fn [{:keys [palette] :as acc} m]
                     (cond
                       (not (vector? m)) acc
                       (and (= 0xf0 (first m)) (= 0x03 (nth m 6 nil)))
                       (let [[index rl rh gl gh bl bh] (subvec m 7 14)]
                         (assoc-in acc [:palette index] [(+ rl (* 128 rh)) (+ gl (* 128 gh)) (+ bl (* 128 bh))]))
                       (= 0x90 (first m)) (assoc-in acc [:colors (second m)] (get palette (nth m 2)))
                       (= 0x80 (first m)) (assoc-in acc [:colors (second m)] [0 0 0])
                       :else acc))
                   {:palette {} :colors {}} messages)))

(defn- note [[column row]] (codec/pad-note column (- 7 row)))

(defn- shown
  "The colour a viewport position (row 0 at the top) shows on unit."
  [{:keys [sent]} unit at]
  (get (pad-colors (get @sent unit)) (note at) [0 0 0]))

(defn- pad [at] [[0x90 (note at) 100] [0x80 (note at) 0]])

(defn- button [cc] [[0xb0 cc 127] [0xb0 cc 0]])

(defn- tap! [{:keys [input!]} unit & taps] (apply input! unit (apply concat taps)))

(defn- logical [{:keys [runtime]} id]
  (some #(when (= id (:id %)) %) (:logical (:control-surfaces (show/snapshot runtime)))))

(defn- selected [h id] (get-in (logical h id) [:layout :selected]))

(defn- edited [device] (get-in (device-session/snapshot device) [:local :selected-instance]))

(deftest select-mode-lights-running-launches-and-commits-the-shared-selection
  (let [{:keys [runtime] :as h} (harness 3)]
    (try
      (let [[a b c] (launch-all! h 3)
            device (connect! h "push2")]
        (testing "the Push 2 attaches as a surface whose grid shows running launches; the grid starts closed"
          (is (= {:id "push2" :descriptor :ableton/push-2-grid :role :main :attached? true :viewports [:grid]}
                 (some #(when (= "push2" (:id %)) %) (:surfaces (:control-surfaces (show/snapshot runtime))))))
          (is (some #{[0xb0 48 0]} (take-sent! h "push2")) "Select is dark")
          (is (nil? (get-in (device-session/snapshot device) [:local :active-grid]))
              "Afterglow's own active cue grid is not enabled"))
        (testing "what the Push encoders edit (the latest launch) is published as the shared selection"
          (is (= c (edited device)))
          (is (= [:instance c] (selected h "push2"))))
        (testing "a pad pressed before the grid's LEDs are acknowledged selects nothing"
          (tap! h "push2" (button 48) (pad [1 0]))
          (is (= [:instance c] (selected h "push2"))))
        (testing "Select lights one pad per launch in Studio's order, top-left first, the selected one bright"
          (refresh! h "push2" device)
          (is (some #{[0xb0 48 127]} (get @(:sent h) "push2")))
          (is (= [(dimmed [1 0 0]) (dimmed [0 1 0]) (full [0 0 1]) [0 0 0]]
                 (mapv #(shown h "push2" %) [[0 0] [1 0] [2 0] [3 0]]))))
        (testing "a pad selects that launch without launching or ending anything; it lights at full brightness"
          (tap! h "push2" (pad [1 0]))
          (is (= [:instance b] (selected h "push2")))
          (is (= [a b c] (order h)))
          (refresh! h "push2" device)
          (is (= [(full [0 1 0]) (dimmed [0 0 1])] [(shown h "push2" [1 0]) (shown h "push2" [2 0])]))
          (is (= b (edited device)) "The Push encoders and display follow the selection"))
        (testing "a REPL selection is the same field: the pads and the encoders follow it"
          (is (= :accepted (:status ((:owner! h) #(deref (show/surface-layout! runtime "push2" {:selected [:instance c]})
                                                           1000 ::missing)))))
          (refresh! h "push2" device)
          (is (= [(dimmed [0 1 0]) (full [0 0 1])] [(shown h "push2" [1 0]) (shown h "push2" [2 0])]))
          (is (= c (edited device))))
        (testing "Select again closes the grid: the button goes dark and pads return to the launch page"
          (take-sent! h "push2")
          (tap! h "push2" (button 48))
          (refresh! h "push2" device)
          (is (some #{[0xb0 48 0]} (get @(:sent h) "push2")))
          (is (= [0 0 0] (shown h "push2" [0 0])) "No page is authored, so the launch page is dark")
          (tap! h "push2" (pad [0 0]))
          (is (= [:instance c] (selected h "push2")) "A closed grid leaves pads to the launch profile"))
        (is (empty? (:errors (device-session/snapshot device)))))
      (finally (show/stop! runtime)))))

(deftest reorder-moves-the-chosen-launch-after-the-target-like-studio
  (let [{:keys [runtime] :as h} (harness 4)]
    (try
      (let [[a b c d] (launch-all! h 4)
            device (connect! h "push2")]
        (tap! h "push2" (button 88))
        (refresh! h "push2" device)
        (is (some #{[0xb0 88 127]} (get @(:sent h) "push2")) "Duplicate lights for Reorder")
        (testing "the first pad chooses and turns white; the same pad again cancels"
          (tap! h "push2" (pad [0 0]))
          (refresh! h "push2" device)
          (is (= [255 255 255] (shown h "push2" [0 0])))
          (tap! h "push2" (pad [0 0]))
          (refresh! h "push2" device)
          (is (= (dimmed [1 0 0]) (shown h "push2" [0 0])))
          (is (= [a b c d] (order h))))
        (testing "choose A, tap C: A composes right after C, and the pads show the committed order"
          (tap! h "push2" (pad [0 0]) (pad [2 0]))
          (is (= [b c a d] (order h)))
          (refresh! h "push2" device)
          (is (= [(dimmed [0 1 0]) (dimmed [0 0 1]) (dimmed [1 0 0]) (full [1 1 1])]
                 (mapv #(shown h "push2" %) [[0 0] [1 0] [2 0] [3 0]]))
              "D, the latest launch, is the selected one"))
        (testing "a competing Studio reorder moves a launch under the target pad: the press does nothing"
          (tap! h "push2" (pad [0 0]))
          (accepted! h {:op :instance/move-after :instance-id d :after-instance-id b
                        :expected-revision (:revision (model-of h))})
          (is (= [b d c a] (order h)))
          (tap! h "push2" (pad [1 0]))
          (is (= [b d c a] (order h)) "The pad still showed C, which the operator saw there, and now holds D")
          (is (= b (:source (push-2/grid-state device))) "The choice is kept by identity"))
        (testing "parameters and IDs survive the moves"
          (is (= #{a b c d} (set (keys (:instances (model-of h))))))
          (is (every? #(= 0.5 (get-in (model-of h) [:instances % :params :level])) [a b c d])))
        (is (empty? (:errors (device-session/snapshot device)))))
      (finally (show/stop! runtime)))))

(deftest seventy-launches-page-with-page-left-and-right
  (let [{:keys [runtime] :as h} (harness 70)]
    (try
      (let [ids (launch-all! h 70)
            device (connect! h "push2")]
        (testing "with the grid closed, Page Right keeps its profile use: it moves the encoders' launch"
          (let [before (edited device)]
            (tap! h "push2" (button 62))
            (is (not= before (edited device)))
            (is (= {:x 0 :y 0} (:window (logical h "push2"))) "No bank command")))
        (tap! h "push2" (button 48))
        (refresh! h "push2" device)
        (testing "page one: 64 launches, Page Right lit, Page Left dark"
          (is (= (dimmed [1 0 0]) (shown h "push2" [0 0])))
          (is (= (dimmed [1 1 1]) (shown h "push2" [7 7])))
          (is (some #{[0xb0 63 127]} (get @(:sent h) "push2")))
          (is (= [0xb0 62 0] (last (filter #(and (vector? %) (= [0xb0 62] (subvec % 0 2))) (get @(:sent h) "push2"))))))
        (testing "Page Right moves the surface window a whole grid; the last six launches show"
          (tap! h "push2" (button 63))
          (is (= {:x 0 :y 8} (:window (logical h "push2"))))
          (refresh! h "push2" device)
          (is (= (dimmed (get colors (mod 64 4))) (shown h "push2" [0 0])))
          (is (= (dimmed (get colors (mod 69 4))) (shown h "push2" [5 0])))
          (is (= [0 0 0] (shown h "push2" [6 0])) "Pads past the last launch go dark"))
        (testing "a pad on page two selects launch 70"
          (tap! h "push2" (pad [5 0]))
          (is (= [:instance (nth ids 69)] (selected h "push2"))))
        (testing "Page Left returns"
          (tap! h "push2" (button 62))
          (is (= {:x 0 :y 0} (:window (logical h "push2")))))
        (is (empty? (:errors (device-session/snapshot device)))))
      (finally (show/stop! runtime)))))

(def ^:private push-3-layout
  {:pads (into {} (for [x (range 8) y (range 8)] [[x y] {:channel 0 :note (+ 36 x (* 8 y))}]))
   :buttons {:select {:channel 0 :controller 20} :reorder {:channel 0 :controller 21}}})

(deftest a-push-3-and-a-push-2-extend-into-one-grid-with-one-selection
  (let [{:keys [runtime owner! transport tick! input!] :as h} (harness 70)]
    (try
      (let [ids (launch-all! h 70)
            upper (show/connect-device! runtime :push-3
                                        {:session-id "push3" :user-mode? true :expression-mode :poly-aftertouch
                                         :schedule! nil :layout push-3-layout :transport (transport "push3")
                                         :cue-grid {:buttons {:select :cue-grid/select :reorder :cue-grid/reorder}}})
            _ (tick!)
            lower (connect! h "push2")]
        (is (= :accepted (:status (owner! #(deref (show/link-surfaces! runtime "pads" ["push3" "push2"]
                                                                       {:mode :extend :direction :down})
                                                   1000 ::missing)))))
        (input! "push3" [0xb0 20 127] [0xb0 20 0])
        (tap! h "push2" (button 48))
        (owner! #(push-3/refresh! upper))
        (refresh! h "push2" lower)
        (testing "the Push 2 below the Push 3 shows launches 65 to 70"
          (is (= (dimmed (get colors (mod 64 4))) (shown h "push2" [0 0])))
          (is (= [0 0 0] (shown h "push2" [6 0]))))
        (testing "a Push 2 pad selects on the link; the Push 3 encoders and its pad follow"
          (tap! h "push2" (pad [5 0]))
          (is (= [:instance (nth ids 69)] (selected h "pads")))
          (owner! #(push-3/refresh! upper))
          (is (= (nth ids 69) (edited upper)))
          (is (= (nth ids 69) (edited lower))))
        (testing "a Push 3 pad selects on the link; the Push 2 follows"
          (input! "push3" [0x90 (+ 36 0 (* 8 7)) 100] [0x80 (+ 36 0 (* 8 7)) 0])
          (is (= [:instance (first ids)] (selected h "pads")))
          (refresh! h "push2" lower)
          (is (= (first ids) (edited lower))))
        (testing "disconnecting the Push 2 darkens its grid buttons and leaves it detached in the link"
          (take-sent! h "push2")
          (show/disconnect-device! runtime "push2")
          (tick!)
          (let [messages (take-sent! h "push2")]
            (is (some #{[0xb0 48 0]} messages))
            (is (= :closed (peek messages))))
          (is (false? (:attached? (first (filter #(= "push2" (:id %))
                                                 (:surfaces (:control-surfaces (show/snapshot runtime))))))))))
      (finally (show/stop! runtime)))))

(deftest grid-options-are-checked-before-any-port-opens
  (let [{:keys [runtime] :as h} (harness 1)
        bad (fn [options]
              (try (show/connect-device! runtime :push-2 (merge {:session-id "bad" :schedule! nil
                                                                 :midi ((:transport h) "bad")}
                                                                options))
                   :opened
                   (catch clojure.lang.ExceptionInfo e (or (:push-2/error (ex-data e)) (ex-message e)))))]
    (try
      (is (= :invalid-cue-grid (bad {:cue-grid {:buttons {62 :cue-grid/page-up}}})) "A mode button is required")
      (is (= :invalid-cue-grid (bad {:cue-grid {:buttons {85 :cue-grid/select}}})) "Play stays the profile's")
      (is (= :invalid-cue-grid (bad {:cue-grid {:buttons {49 :cue-grid/select}}})) "Shift stays Shift")
      (is (= :invalid-cue-grid (bad {:cue-grid {:buttons {48 :cue-grid/launch}}})))
      (is (= :invalid-cue-grid (bad {:cue-grid {:buttons {52 :cue-grid/select}} :knob-page {:button 52 :knob-ids [:k]}}))
          "The knob page keeps its button")
      (is (= :invalid-capture-button (bad {:active-cue-grid? true :capture-button 48})))
      (is (= :invalid-capture-button (bad {:capture-button 86})) "Record stays the profile's")
      (is (nil? (get @(:sent h) "bad")) "Nothing was opened")
      (finally (show/stop! runtime)))))

(deftest a-dim-or-black-launch-colour-still-lights-its-pad
  (is (= [77 0 0] (push-2/pad-rgb {:color [1 0 0]})))
  (is (= [255 0 0] (push-2/pad-rgb {:color [1 0 0] :selected? true})))
  (is (= [27 27 27] (push-2/pad-rgb {:color [0 0 0]})) "Black is drawn grey")
  (is (= [27 27 27] (push-2/pad-rgb {:color nil})))
  (is (= [255 255 255] (push-2/pad-rgb {:color [0 0 1] :source? true}))))
