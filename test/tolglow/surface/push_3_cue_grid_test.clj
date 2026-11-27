(ns tolglow.surface.push-3-cue-grid-test
  "Push 3 Select and Reorder pad grids (#545) through show/connect-device!:
  recorded User Mode MIDI into the Afterglow codec and session shell, the
  Tolglow cue grid and surface link, and a real show owner. A capture
  transport records every LED byte. No device is opened."
  (:require [afterglow.control.devices.session :as device-session]
            [afterglow.control.model :as control]
            [clojure.test :refer [deftest is testing]]
            [tolglow.show :as show]
            [tolglow.surface.cue-grid :as cue-grid]
            [tolglow.surface.link :as surface]
            [tolglow.surface.push-3 :as push-3]))

;; A synthetic capture in the form afterglow.control.codecs.push-3/capture
;; builds: pad [x y] counts rows from the bottom, notes 36..99.
(def ^:private layout
  {:pads (into {} (for [x (range 8) y (range 8)] [[x y] {:channel 0 :note (+ 36 x (* 8 y))}]))
   :buttons {:select {:channel 0 :controller 20} :reorder {:channel 0 :controller 21}
             :up {:channel 0 :controller 46} :down {:channel 0 :controller 47}}})

(def ^:private grid-options
  {:buttons {:select :cue-grid/select :reorder :cue-grid/reorder :up :cue-grid/page-up :down :cue-grid/page-down}})

(def ^:private colors
  "Colours the default palette matches exactly: red, green, blue, white."
  [[1 0 0] [0 1 0] [0 0 1] [1 1 1]])

(def ^:private velocity {[1 0 0] 127 [0 1 0] 126 [0 0 1] 125 [1 1 1] 122})

(defn- model [n]
  (control/new-show
   {:show-id :push-grid :transport {:playing? true :beat 0.0 :bpm 120.0}
    :registry {:effects {:wash {:id :wash :version 1
                                :params {:level {:type :number :default 0.5 :bounds [0 1]}}
                                :outputs {:dimmer {:op :param :key :level}}}}
               :cues (into {} (for [i (range n)
                                    :let [id (keyword (str "c" i))]]
                                [id {:id id :version 1 :effect-id :wash :name (str "C" i)
                                     :color (get colors (mod i 4))}]))}
    :rig {:fixtures [{:id :head :universe 1 :address 1 :channels {:dimmer 1}}]}}))

(defn- harness [n]
  (let [runtime (show/create-runtime {:model (model n) :workspace-options nil})
        time (atom 0)
        receivers (atom {})
        sent (atom {})
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
                   :send! (fn [_ batch] (swap! sent update unit (fnil into []) batch))
                   :close! (fn [_] (swap! sent update unit (fnil conj []) :closed))})
     :input! (fn [unit & messages] (owner! #(doseq [bytes messages] ((get @receivers unit) bytes))))
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

(defn- order
  "Studio's running order: :active-instance-order filtered to running launches."
  [{:keys [runtime]}]
  (let [{:keys [active-instance-order instances]} (show/snapshot runtime)
        running (into #{} (keep #(when (= :running (:phase %)) (:id %))) instances)]
    (filterv running active-instance-order)))

(defn- connect! [{:keys [runtime transport tick!]} id & [options]]
  (let [device (show/connect-device! runtime :push-3
                                     (merge {:session-id id :user-mode? true :expression-mode :poly-aftertouch
                                             :schedule! nil :layout layout :cue-grid grid-options
                                             :transport (transport id)}
                                            options))]
    (tick!)
    device))

(defn- refresh! [{:keys [owner!]} & devices] (owner! #(run! device-session/refresh! devices)))

(defn- take-sent! [{:keys [sent]} unit]
  (let [messages (get @sent unit)]
    (swap! sent assoc unit [])
    (vec messages)))

(defn- note
  "The captured pad note at a viewport position (row 0 at the top)."
  [[column row]]
  (get-in layout [:pads [column (- 7 row)] :note]))

(defn- pad
  "Recorded bytes of one pad tap at a viewport position."
  [at]
  [[0x90 (note at) 100] [0x80 (note at) 0]])

(defn- button [id] [[0xb0 (get-in layout [:buttons id :controller]) 127]
                    [0xb0 (get-in layout [:buttons id :controller]) 0]])

(defn- tap!
  "Feed recorded taps (each a sequence of messages) in order."
  [{:keys [input!]} unit & taps]
  (apply input! unit (apply concat taps)))

(defn- shown
  "The pad LED message the unit was last sent for a viewport position."
  [device at]
  (let [[x row] at]
    (get-in (device-session/snapshot device) [:local ::push-3/sent [:pad [x (- 7 row)]]])))

(defn- logical [{:keys [runtime]} id]
  (some #(when (= id (:id %)) %) (:logical (:control-surfaces (show/snapshot runtime)))))

(defn- selected [h id] (get-in (logical h id) [:layout :selected]))

(deftest select-mode-projects-running-launches-and-commits-the-shared-selection
  (let [{:keys [runtime] :as h} (harness 3)]
    (try
      (let [[a b c] (launch-all! h 3)
            device (connect! h "push-a")]
        (refresh! h device)
        (testing "attaching registers a cue-grid surface, but a closed grid lights nothing"
          (is (= {:id "push-a" :descriptor :ableton/push-3 :role :main :attached? true :viewports [:grid]}
                 (some #(when (= "push-a" (:id %)) %) (:surfaces (:control-surfaces (show/snapshot runtime))))))
          (is (= [] (filterv #(#{0x80 0x90} (bit-and 0xf0 (first %))) (take-sent! h "push-a")))))
        (testing "Select lights one pad per running launch in Studio's order, top-left first, and its button"
          (tap! h "push-a" (button :select))
          (refresh! h device)
          (let [messages (take-sent! h "push-a")]
            (is (some #{[0xb0 20 127]} messages))
            (is (= [[0x90 (note [0 0]) 127] [0x90 (note [1 0]) 126] [0x90 (note [2 0]) 125]]
                   (mapv #(shown device %) [[0 0] [1 0] [2 0]])))
            (is (= [0x80 (note [3 0]) 0] (shown device [3 0])) "Empty pads are dark")
            (is (= 64 (count (filter #(#{0x80 0x90} (bit-and 0xf0 (first %))) messages))))))
        (testing "a pad selects that launch on the surface without launching or ending anything"
          (tap! h "push-a" (pad [1 0]))
          (is (= [:instance b] (selected h "push-a")))
          (is (= [a b c] (order h)))
          (refresh! h device)
          (is (= [0x99 (note [1 0]) 126] (shown device [1 0])) "The selected launch pulses")
          (is (= b (get-in (device-session/snapshot device) [:local :selected-instance]))
              "The Push encoders and display follow the selection"))
        (testing "a REPL selection is the same field: the pad and the encoders follow it"
          (is (= :accepted (:status ((:owner! h) #(deref (show/surface-layout! runtime "push-a" {:selected [:instance c]})
                                                           1000 ::missing)))))
          (refresh! h device)
          (is (= [[0x90 (note [1 0]) 126] [0x99 (note [2 0]) 125]] [(shown device [1 0]) (shown device [2 0])]))
          (is (= c (get-in (device-session/snapshot device) [:local :selected-instance]))))
        (testing "Select again closes the grid and turns off only what it lit"
          (take-sent! h "push-a")
          (tap! h "push-a" (button :select))
          (refresh! h device)
          (let [messages (take-sent! h "push-a")]
            (is (= #{[0x80 (note [0 0]) 0] [0x80 (note [1 0]) 0] [0x80 (note [2 0]) 0] [0xb0 20 0]} (set messages))))
          (tap! h "push-a" (pad [0 0]))
          (is (= [:instance c] (selected h "push-a")) "A closed grid leaves pads to the launch profile"))
        (is (empty? (:errors (device-session/snapshot device)))))
      (finally (show/stop! runtime)))))

(deftest reorder-moves-the-chosen-launch-after-the-target-like-studio
  (let [{:keys [runtime] :as h} (harness 4)]
    (try
      (let [[a b c d] (launch-all! h 4)
            device (connect! h "push-a")]
        (tap! h "push-a" (button :reorder))
        (refresh! h device)
        (testing "the first pad chooses and blinks; the same pad again cancels"
          (tap! h "push-a" (pad [0 0]))
          (refresh! h device)
          (is (= [0x9e (note [0 0]) 127] (shown device [0 0])))
          (tap! h "push-a" (pad [0 0]))
          (refresh! h device)
          (is (= [0x90 (note [0 0]) 127] (shown device [0 0])))
          (is (= [a b c d] (order h))))
        (testing "choose A, tap C: A composes right after C, and the pads show the committed order"
          ;; An unrelated Studio edit since the last feedback does not refuse the move.
          (accepted! h {:op :param/set :instance-id d :param :level :value 0.5})
          (tap! h "push-a" (pad [0 0]) (pad [2 0]))
          (is (= [b c a d] (order h)))
          (refresh! h device)
          (is (= [[0x90 (note [0 0]) 126] [0x90 (note [1 0]) 125] [0x90 (note [2 0]) 127] [0x90 (note [3 0]) 122]]
                 (mapv #(shown device %) [[0 0] [1 0] [2 0] [3 0]]))))
        (testing "hold the chosen pad and press the target: the same move"
          (let [{:keys [input!]} h]
            (input! "push-a" [0x90 (note [3 0]) 100] [0x90 (note [0 0]) 100] [0x80 (note [0 0]) 0] [0x80 (note [3 0]) 0]))
          (is (= [b d c a] (order h))))
        (testing "a pad pressed in Reorder and released after closing it stays with the grid"
          (let [{:keys [input!]} h]
            (input! "push-a" [0x90 (note [0 0]) 100])
            (tap! h "push-a" (button :reorder))
            (input! "push-a" [0x80 (note [0 0]) 0]))
          (is (= {:mode nil :pads #{} :source nil} (select-keys (push-3/grid-state device) [:mode :pads :source])))
          (is (= [b d c a] (order h))))
        (testing "parameters and IDs survive the move"
          (is (= #{a b c d} (set (keys (:instances (model-of h))))))
          (is (every? #(= 0.5 (get-in (model-of h) [:instances % :params :level])) [a b c d])))
        (is (empty? (:errors (device-session/snapshot device)))))
      (finally (show/stop! runtime)))))

(deftest a-launch-that-ends-mid-hold-or-moves-under-a-finger-is-not-acted-on
  (let [{:keys [runtime input!] :as h} (harness 4)]
    (try
      (let [[a b c d] (launch-all! h 4)
            device (connect! h "push-a")]
        (tap! h "push-a" (button :reorder))
        (refresh! h device)
        (testing "the chosen launch ends while its pad is held: the next pad chooses instead of moving"
          (input! "push-a" [0x90 (note [0 0]) 100])
          (accepted! h {:op :instance/end :instance-id a})
          (refresh! h device)
          (is (= [b c d] (order h)))
          (is (nil? (:source (push-3/grid-state device))) "Feedback drops a chosen launch that ended")
          (input! "push-a" [0x90 (note [2 0]) 100])
          (is (= [b c d] (order h)) "No move")
          (is (= d (:source (push-3/grid-state device))))
          (input! "push-a" [0x80 (note [0 0]) 0] [0x80 (note [2 0]) 0])
          (is (= #{} (:pads (push-3/grid-state device))) "Both releases reach the grid")
          (tap! h "push-a" (pad [2 0]))
          (is (nil? (:source (push-3/grid-state device)))))
        (testing "a competing Studio reorder moves a launch under the target pad: the press does nothing"
          (tap! h "push-a" (pad [0 0]))
          (is (= b (:source (push-3/grid-state device))))
          (accepted! h {:op :instance/move-after :instance-id d :after-instance-id b
                        :expected-revision (:revision (model-of h))})
          (is (= [b d c] (order h)))
          ;; The pad at [1 0] still shows C until the next refresh; it now holds D.
          (tap! h "push-a" (pad [1 0]))
          (is (= [b d c] (order h)) "Not moved after D, which the operator never saw there")
          (is (= b (:source (push-3/grid-state device))) "The choice is kept by identity")
          (refresh! h device)
          (tap! h "push-a" (pad [2 0]))
          (is (= [d c b] (order h)) "After the refresh the pad shows C, and B moves after it")
          (refresh! h device))
        (testing "a release and relaunch of the same cue is a new launch on another pad"
          (accepted! h {:op :instance/end :instance-id c})
          (let [relaunched (:instance-id (accepted! h {:op :cue/launch :cue-id :c2 :launch {:mode :persistent}}))]
            (is (not= c relaunched))
            (tap! h "push-a" (pad [1 0]))
            (is (nil? (:source (push-3/grid-state device))) "The pad showed C, which ended")
            (refresh! h device)
            (is (= [d b relaunched] (order h)))
            (is (= [0x90 (note [2 0]) 125] (shown device [2 0])) "The new launch keeps its cue's colour")))
        (is (empty? (:errors (device-session/snapshot device)))))
      (finally (show/stop! runtime)))))

(deftest seventy-launches-page-through-the-committed-window
  (let [{:keys [runtime] :as h} (harness 70)]
    (try
      (let [ids (launch-all! h 70)
            device (connect! h "push-a")]
        (tap! h "push-a" (button :select))
        (refresh! h device)
        (testing "page one: 64 launches, Page Down lit, Page Up dark"
          (let [messages (take-sent! h "push-a")]
            (is (= 64 (count (filter #(= 0x90 (first %)) messages))))
            (is (some #{[0xb0 47 127]} messages))
            (is (some #{[0xb0 46 0]} messages))))
        (testing "Page Down moves the surface window a whole grid; the last six launches show"
          (tap! h "push-a" (button :down))
          (is (= {:x 0 :y 8} (:window (logical h "push-a"))))
          (refresh! h device)
          (is (= 6 (count (filter #(= 0x90 (first %)) (vals (::push-3/sent (:local (device-session/snapshot device)))))))
              "Pads past the last launch go dark")
          (is (= [0x90 (note [0 0]) (velocity (get colors (mod 64 4)))] (shown device [0 0])))
          (tap! h "push-a" (button :down))
          (is (= {:x 0 :y 8} (:window (logical h "push-a"))) "No page past the last launch"))
        (testing "a pad on page two selects launch 65"
          (tap! h "push-a" (pad [0 0]))
          (is (= [:instance (nth ids 64)] (selected h "push-a"))))
        (testing "reorder across pages: choose on page two, page up, place after the first launch"
          (tap! h "push-a" (button :reorder) (pad [5 0]) (button :up))
          (is (= {:x 0 :y 0} (:window (logical h "push-a"))))
          (refresh! h device)
          (tap! h "push-a" (pad [0 0]))
          (is (= [(first ids) (nth ids 69) (second ids)] (take 3 (order h))))
          (is (= 70 (count (order h))))))
      (finally (show/stop! runtime)))))

(deftest two-pushes-extend-down-into-one-grid-and-disconnect-cleanly
  (let [{:keys [runtime owner!] :as h} (harness 70)]
    (try
      (let [ids (launch-all! h 70)
            upper (connect! h "push-a")
            lower (connect! h "push-b")]
        (is (= :accepted (:status (owner! #(deref (show/link-surfaces! runtime "pads" ["push-a" "push-b"]
                                                                       {:mode :extend :direction :down})
                                                   1000 ::missing)))))
        (tap! h "push-a" (button :select))
        (tap! h "push-b" (button :select))
        (refresh! h upper lower)
        (testing "the lower Push shows launches 65 to 70, and its pads select on the shared link"
          (is (= [0x90 (note [5 0]) (velocity (get colors (mod 69 4)))] (shown lower [5 0])))
          (is (= [0x80 (note [6 0]) 0] (shown lower [6 0])))
          (tap! h "push-b" (pad [5 0]))
          (is (= [:instance (nth ids 69)] (selected h "pads")))
          (refresh! h upper lower)
          (is (= (nth ids 69) (get-in (device-session/snapshot upper) [:local :selected-instance]))
              "Both members adopt the link's selection")
          (is (= [0x99 (note [5 0]) 126] (shown lower [5 0]))))
        (testing "disconnecting blanks what the grid lit and leaves the link with the surface detached"
          (take-sent! h "push-b")
          (show/disconnect-device! runtime "push-b")
          ((:tick! h))
          (let [messages (take-sent! h "push-b")]
            (is (= 6 (count (filter #(and (vector? %) (= 0x80 (first %))) messages))) "The six lit pads, the selected one included")
            (is (some #{[0xb0 20 0]} messages))
            (is (= :closed (peek messages))))
          (is (false? (:attached? (first (filter #(= "push-b" (:id %))
                                                 (:surfaces (:control-surfaces (show/snapshot runtime))))))))))
      (finally (show/stop! runtime)))))

(deftest grid-options-are-checked-before-any-port-opens
  (let [{:keys [runtime] :as h} (harness 1)
        bad (fn [cue-grid & [extra]]
              (try (connect! h "bad" (merge {:cue-grid cue-grid} extra)) :opened
                   (catch clojure.lang.ExceptionInfo e (:push-3/error (ex-data e)))))]
    (try
      (is (= :invalid-cue-grid (bad {:buttons {:missing :cue-grid/select}})))
      (is (= :invalid-cue-grid (bad {:buttons {:up :cue-grid/page-up}})) "A mode button is required")
      (is (= :invalid-cue-grid (bad {:buttons {:select :cue-grid/launch}})))
      (is (= :invalid-cue-grid (bad grid-options {:button-bindings {:select :page-up}})) "A bound button stays bound")
      (is (= :invalid-cue-grid (bad (assoc grid-options :columns 0))))
      (is (nil? (get @(:sent h) "bad")) "Nothing was opened")
      (finally (show/stop! runtime)))))

(deftest active-cues-content-is-a-link-content-space
  (let [m (model 3)]
    (is (= :cue-grid (surface/content-axis {:type :active-cues})))
    (is (true? (surface/content-valid? {:type :active-cues :columns 16})))
    (is (false? (surface/content-valid? {:type :active-cues :columns 65})))
    (is (= [] (cue-grid/cells m {:type :active-cues})))
    (is (= {:columns 0 :rows 0} (surface/content-size m {:type :active-cues})))))
