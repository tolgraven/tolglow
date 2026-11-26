(ns tolglow.surface.x-touch-host-test
  "The Tolglow X-Touch mixer layout (#706 slice 2) through the MIDI transport
  boundary against a real show owner: virtual main and extender units, captured
  input bytes and byte-exact expected output. No device is opened."
  (:require [afterglow.control.codecs.x-touch :as mcu]
            [afterglow.control.devices.session :as device-session]
            [afterglow.control.model :as control]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [tolglow.surface.descriptor :as descriptor]
            [tolglow.surface.link :as surface]
            [tolglow.surface.mixer :as mixer]
            [tolglow.surface.virtual :as virtual]
            [tolglow.surface.x-touch :as x-touch]
            [tolglow.show :as show]))

(def ^:private cue-ids (mapv #(keyword (str "c" %)) (range 8)))

(def ^:private wash
  {:id :wash :version 1
   :params (merge {:level {:type :number :default 0.5 :bounds [0 1]}
                   :enabled {:type :boolean :default false}
                   :mode {:type :enum :values [:a :b :c] :default :b}
                   :spread {:type :number :default 0.0 :bounds [-1 1]}
                   :color {:type :color :default [1 0 0]}}
                  (into {} (for [n (range 6)] [(keyword (str "p" n)) {:type :number :default 0.0 :bounds [0 1]}])))
   :outputs {:dimmer {:op :param :key :level}}})

(defn- model []
  (let [head {:id :head :universe 1 :address 1 :channels {:dimmer 1}}]
    (-> (reduce (fn [m id]
                  (assoc-in m [:registry :cues id] {:id id :version 1 :effect-id :wash :priority 0
                                                    :name (str/upper-case (name id))}))
                (assoc-in (control/new-show {:rig {:fixtures [head]}}) [:registry :effects :wash] wash)
                cue-ids)
        (assoc-in [:registry :cues :c0 :color] [1 0 0]))))

(defn- harness [& [initial-model]]
  (let [runtime (show/create-runtime {:model (or initial-model (model)) :workspace-options nil})
        time (atom 0)
        receivers (atom {})
        sent (atom {})
        transport (fn [unit]
                    {:open! (fn [receive!] (swap! receivers assoc unit receive!) unit)
                     :send! (fn [_ batch] (swap! sent update unit (fnil into []) batch))
                     :close! (fn [_] (swap! sent update unit (fnil conj []) :closed))})
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
    {:runtime runtime :time time :sent sent :transport transport :owner! owner!
     :input! (fn [unit & messages] (owner! #(doseq [bytes messages] ((get @receivers unit) bytes))))
     :tick! #(show/tick! runtime (swap! time inc))}))

(defn- submit! [{:keys [runtime tick!]} payload]
  (let [receipt (show/submit! runtime :command (merge {:source {:adapter :repl :session-id "xt-test"}} payload))]
    (tick!)
    (deref receipt 1000 ::missing)))

(defn- accepted! [h payload]
  (let [ack (submit! h payload)]
    (is (= :accepted (:status ack)) (pr-str payload ack))
    ack))

(defn- model-of [{:keys [runtime]}] (:model @(:state runtime)))

(defn- latest [h cue-id]
  (last (sort-by :order (filter #(= cue-id (:cue-id %)) (vals (:instances (model-of h)))))))

(defn- take-sent! [{:keys [sent]} unit]
  (let [messages (get @sent unit)]
    (swap! sent assoc unit [])
    (vec messages)))

(defn- rows
  "[row-1 row-2] text from the last LCD writes in messages."
  [messages]
  (mapv (fn [row]
          (when-let [m (last (filter #(and (vector? %) (= [240 0 0 102] (take 4 %)) (= 18 (nth % 5))
                                           (= (* 56 row) (nth % 6)))
                                     messages))]
            (mapv str/trim (map #(apply str %) (partition 7 (map char (butlast (drop 7 m))))))))
        [0 1]))

(defn- colors [messages]
  (some->> messages (filter #(and (vector? %) (= 114 (nth % 5 nil)))) last (drop 6) butlast vec))

(defn- shown
  "What the unit currently shows: the profile's last sent message per key."
  [device k]
  (get-in (device-session/snapshot device) [:local :sent k]))

(defn- lcd [device] (rows [(shown device [:text 0]) (shown device [:text 1])]))

(defn- connect! [{:keys [runtime transport]} id unit options]
  (show/connect-device! runtime :x-touch (merge {:session-id id :schedule! nil :transport (transport unit)} options)))

(def ^:private main-options
  {:effect-group-view :manual
   :channels (into [{:cue-id :c0 :encoder :spread}] (map #(hash-map :cue-id %) (rest cue-ids)))})

(defn- desk!
  "Main and extender attached and extended into one 16-strip surface."
  [{:keys [runtime owner! tick!] :as h}]
  (accepted! h {:op :effect-groups/configure :config {:manual [{:id :g0 :name "Group" :cue-ids [:c1 :c2]}]}})
  (let [main (connect! h "xt-main" :main main-options)
        ext (connect! h "xt-ext" :ext {:extender? true})]
    (tick!)
    (is (= :accepted (:status (owner! #(deref (show/link-surfaces! runtime "desk" ["xt-main" "xt-ext"]) 2000 ::missing)))))
    (tick!)
    {:main main :ext ext :refresh! #(owner! (fn [] (run! device-session/refresh! [main ext])))}))

(defn- launch! [h cue-id]
  (accepted! h {:op :cue/launch :cue-id cue-id :launch {:mode :persistent}}))

(defn- layout [h] (:layout (surface/strip-placement (model-of h) "xt-main")))

(deftest faders-need-a-finger-and-keep-their-target-across-banks
  (let [{:keys [runtime input!] :as h} (harness)]
    (try
      (let [{:keys [refresh!]} (desk! h)]
        (launch! h :c0)
        (refresh!)
        (take-sent! h :main)
        (testing "an untouched motor echo writes nothing; a touched fader writes the cue level"
          (input! :main [0xe1 127 127])
          (is (= 0.5 (get-in (latest h :c0) [:params :level])))
          (input! :main [0x90 105 127] [0xe1 0 32])
          (is (= (/ 4096.0 16383.0) (get-in (latest h :c0) [:params :level])))
          (refresh!)
          (is (not-any? #(= 0xe1 (first %)) (take-sent! h :main)) "No motor feedback while touched"))
        (testing "a held fader keeps writing its strip after the window moves"
          ;; Nine strips fit the 16-strip link; alone, the main unit pages by 8.
          ((:owner! h) #(deref (show/unlink-surfaces! runtime "desk") 1000 ::missing))
          (input! :main [0x90 47 127] [0x80 47 0])
          (is (= 8 (:first (surface/strip-placement (model-of h) "xt-main"))))
          (input! :main [0xe1 127 127])
          (is (= 1.0 (get-in (latest h :c0) [:params :level])))
          (input! :main [0x90 46 127] [0x80 46 0]))
        (testing "release reasserts the committed value: exact motor bytes"
          (take-sent! h :main)
          (input! :main [0x90 105 0])
          (refresh!)
          (is (some #{[0xe1 127 127]} (take-sent! h :main))))
        (testing "an empty strip parks its motor at zero; the master fader needs touch too"
          (input! :main [0xe8 0 0])
          (is (= 1.0 (:master (model-of h))))
          (input! :main [0x90 112 127] [0xe8 0 64] [0x90 112 0])
          (is (= (/ 8192.0 16383.0) (:master (model-of h))))
          (input! :ext [0x90 112 127] [0xe8 0 0])
          (is (= (/ 8192.0 16383.0) (:master (model-of h))) "The extender has no master fader")))
      (finally (show/stop! runtime)))))

(deftest held-cues-select-launch-and-typed-encoders
  (let [{:keys [runtime input!] :as h} (harness)]
    (try
      (let [{:keys [refresh! main]} (desk! h)]
        (refresh!)
        (testing "REC holds strip 3's cue and releases the exact instance"
          (input! :main [0x90 2 127])
          (let [held (latest h :c1)]
            (is (= :held (:launch-mode held)))
            (refresh!)
            (is (some #{[0x90 2 127]} (take-sent! h :main)) "REC lights while held")
            (input! :main [0x80 2 0])
            (is (nil? (get-in (model-of h) [:instances (:id held)])) "Release ends exactly the held instance")))
        (testing "SELECT selects; the selected cue strip's SELECT toggles its launch"
          (input! :main [0x90 25 127] [0x80 25 0])
          (is (= [:cue :c0] (:selected (layout h))))
          (is (nil? (latest h :c0)))
          (input! :main [0x90 25 127] [0x80 25 0])
          (is (= :persistent (:launch-mode (latest h :c0))))
          (refresh!)
          (is (some #{[0x90 25 127]} (take-sent! h :main)) "SELECT lights the selected strip"))
        (testing "PAN: the configured bipolar encoder, boost/cut ring with the centre LED at default"
          (is (= [0xb0 49 0x56] (shown main [:ring 1])) "boost/cut, centre LED, position 6")
          (input! :main [0xb0 17 3])
          (is (< (Math/abs (- 0.06 (get-in (latest h :c0) [:params :spread]))) 1e-9))
          (input! :main [0x90 33 127])
          (is (= 0.0 (get-in (latest h :c0) [:params :spread])) "Encoder push restores the default"))
        (testing "FLIP swaps fader and encoder on cue strips"
          (input! :main [0x90 50 127])
          (is (true? (:flip? (layout h))))
          (input! :main [0x90 105 127] [0xe1 127 127] [0x90 105 0])
          (is (= 1.0 (get-in (latest h :c0) [:params :spread])))
          (input! :main [0x90 50 127])))
      (finally (show/stop! runtime)))))

(deftest plug-in-pages-span-the-link-and-user-addresses-knobs
  (let [{:keys [runtime input! owner!] :as h} (harness)]
    (try
      (let [{:keys [refresh! main ext]} (desk! h)]
        (launch! h :c0)
        (input! :main [0x90 25 127])
        (testing "PLUG-IN shows the selected cue's parameters over both units, cyan"
          (input! :main [0x90 43 127])
          (is (= {:encoder-mode :plug-in :page 0 :selected [:cue :c0] :flip? false} (layout h)))
          (refresh!)
          (is (= [["colo.R" "colo.G" "colo.B" "enable" "level" "mode" "p0" "p1"]
                  ["1.00" "0.00" "0.00" "Off" "0.50" "b" "0.00" "0.00"]] (lcd main)))
          (is (= [["p2" "p3" "p4" "p5" "spread" "" "" ""] ["0.00" "0.00" "0.00" "0.00" "0.00" "" "" ""]] (lcd ext)))
          (is (= [240 0 0 102 21 114 6 6 6 6 6 0 0 0 247] (shown ext :colors)) "Item strips cyan, the rest black, on device 0x15")
          (is (= [0x90 43 127] (shown main [:led 43])) "PLUG-IN lights"))
        (testing "an extender encoder addresses item 8 + strip"
          (input! :ext [0xb0 16 5])
          (is (< (Math/abs (- 0.05 (get-in (latest h :c0) [:params :p2]))) 1e-9)))
        (testing "INST pages by the whole span, EQ back; the last page clamps"
          (input! :main [0x90 45 127])
          (is (zero? (:page (layout h))) "13 items fit one 16-strip page")
          (owner! #(deref (show/unlink-surfaces! runtime "desk") 1000 ::missing))
          (input! :main [0x90 45 127])
          (is (= 1 (:page (layout h))) "Alone, the main unit pages by 8")
          (refresh!)
          (is (= ["p2" "p3" "p4" "p5" "spread" "" "" ""] (first (lcd main))))
          (input! :main [0x90 44 127])
          (is (zero? (:page (layout h))))
          (owner! #(deref (show/link-surfaces! runtime "desk" ["xt-main" "xt-ext"]) 1000 ::missing))
          (is (= :plug-in (:encoder-mode (layout h))) "Linking keeps the leader's layout"))
        (testing "USER puts linked knobs on the encoders; extender encoder 4 is knob 12"
          (doseq [n (range 12)]
            (accepted! h {:op :knob/create :knob-id (keyword (str "k" n)) :name (str "Knob " n)}))
          (input! :main [0x90 69 127])
          (is (= :user (:encoder-mode (layout h))))
          (input! :ext [0xb0 19 10])
          (is (= 0.1 (get-in (model-of h) [:knobs :knobs :k11 :value])))
          (refresh!)
          (is (= [["Knob 8" "Knob 9" "Knob 1" "Knob 1" "" "" "" ""] ["0%" "0%" "0%" "10%" "" "" "" ""]] (lcd ext)))
          (input! :main [0x90 69 127])
          (is (= :pan (:encoder-mode (layout h))) "The lit mode again returns to PAN")))
      (finally (show/stop! runtime)))))

(deftest solo-mute-and-the-transport-solo-follow-effect-selection
  (let [{:keys [runtime input!] :as h} (harness)]
    (try
      (let [{:keys [refresh! main]} (desk! h)
            selected (fn [] (into {} (map (juxt :id (juxt :soloed? :muted?)))
                                  (:instances (:effect-selection (show/snapshot runtime)))))]
        (doseq [cue [:c0 :c1 :c2 :c3]] (launch! h cue))
        (let [[i0 i1 i2 i3] (map #(:id (latest h %)) [:c0 :c1 :c2 :c3])]
          (testing "SOLO on the group strip solos its members; other strips' MUTE blinks"
            (input! :main [0x90 8 127] [0x80 8 0])
            (is (= {i0 [false false] i1 [true false] i2 [true false] i3 [false false]} (selected)))
            (refresh!)
            (is (= [0x90 8 127] (shown main [:led 8])) "group SOLO lit")
            (is (= [0x90 17 1] (shown main [:led 17])) "c0 MUTE blinks: suppressed by solo")
            (is (= [0x90 90 127] (shown main [:led 90])) "transport SOLO lit")
            (is (= [12 0] (mapv #(first (get-in (device-session/snapshot main) [:local :meters %])) [0 1]))
                "The soloed group's meter is full; c0's falls to zero while suppressed"))
          (testing "SHIFT+SOLO adds a strip"
            (input! :main [0x90 70 127] [0x90 12 127] [0x80 12 0] [0x80 70 0])
            (is (= [true false] (get (selected) i3))))
          (testing "transport SOLO stores and clears, then restores"
            (input! :main [0x90 90 127])
            (is (every? (fn [[_ [solo _]]] (not solo)) (selected)))
            (refresh!)
            (is (= [0x90 90 1] (shown main [:led 90])) "blinks while a set is stored")
            (input! :main [0x90 90 127])
            (is (= #{i1 i2 i3} (set (keep (fn [[id [solo _]]] (when solo id)) (selected))))))
          (testing "MUTE on the extender's strip is the same command"
            (input! :main [0x90 47 127] [0x80 47 0] [0x90 46 127] [0x80 46 0])
            (input! :main [0x90 17 127])
            (is (= [false true] (get (selected) i0))))))
      (finally (show/stop! runtime)))))

(deftest control-inverts-the-configured-solo-and-mute-mode-and-shift-adds
  (let [{:keys [runtime input!] :as h} (harness)]
    (try
      (desk! h)
      (doseq [cue [:c0 :c1 :c2 :c3]] (launch! h cue))
      (let [[i0 i1 i2 i3] (map #(:id (latest h %)) [:c0 :c1 :c2 :c3])
            selected (fn [field] (into #{} (keep #(when (field %) (:id %)))
                                       (:instances (:effect-selection (show/snapshot runtime)))))
            control! (fn [note] (input! :main [0x90 72 127] [0x90 note 127] [0x80 note 0] [0x80 72 0]))
            shift! (fn [note] (input! :main [0x90 70 127] [0x90 note 127] [0x80 note 0] [0x80 70 0]))
            press! (fn [note] (input! :main [0x90 note 127] [0x80 note 0]))]
        (testing "with the default exclusive Solo, CONTROL adds"
          (press! 9)
          (is (= #{i0} (selected :soloed?)))
          (control! 12)
          (is (= #{i0 i3} (selected :soloed?))))
        (testing "with additive Solo, a plain press adds, CONTROL replaces and SHIFT still adds"
          (accepted! h {:op :effect-selection/configure :modes {:solo :additive}})
          (press! 8)
          (is (= #{i0 i1 i2 i3} (selected :soloed?)) "The group strip joins")
          (control! 9)
          (is (= #{i0} (selected :soloed?)) "CONTROL+SOLO leaves only this strip")
          (control! 8)
          (is (= #{i1 i2} (selected :soloed?)) "Another strip replaces it")
          (control! 8)
          (is (= #{} (selected :soloed?)) "CONTROL on the whole selection clears it")
          (shift! 9)
          (shift! 12)
          (is (= #{i0 i3} (selected :soloed?))))
        (testing "Mute follows its own configured mode"
          (press! 17)
          (control! 20)
          (is (= #{i0 i3} (selected :muted?)) "Exclusive Mute: CONTROL adds")
          (press! 20)
          (is (= #{i3} (selected :muted?)) "A plain exclusive press replaces")))
      (finally (show/stop! runtime)))))

(deftest transport-solo-finds-a-strip-selected-on-the-extender
  (let [{:keys [runtime input! owner!] :as h} (harness)]
    (try
      (let [{:keys [refresh!]} (desk! h)
            soloed #(vec (keep (fn [i] (when (:soloed? i) (:id i))) (:instances (:effect-selection (show/snapshot runtime)))))]
        (launch! h :c7)
        (launch! h :c0)
        (refresh!)
        (testing "the extender's first strip is the ninth row, cue c7"
          (input! :ext [0x90 24 127] [0x80 24 0])
          (is (= [:cue :c7] (:selected (layout h)))))
        (testing "with nothing soloed or stored, the main unit's transport SOLO solos it"
          (input! :main [0x90 90 127] [0x80 90 0])
          (is (= [(:id (latest h :c7))] (soloed))))
        (testing "and also when the selected strip is on no unit's current page"
          (input! :main [0x90 90 127] [0x80 90 0])
          (input! :main [0x90 70 127] [0x90 90 127] [0x80 90 0] [0x80 70 0])
          (is (= [] (soloed)) "Stored and cleared, then SHIFT forgot the stored set")
          (owner! #(deref (show/unlink-surfaces! runtime "desk") 1000 ::missing))
          (is (= [0 [:cue :c7]] ((juxt :first (comp :selected :layout)) (surface/strip-placement (model-of h) "xt-main")))
              "Alone, the main unit shows rows 1-8; c7 is the ninth")
          (input! :main [0x90 90 127] [0x80 90 0])
          (is (= [(:id (latest h :c7))] (soloed)))))
      (finally (show/stop! runtime)))))

(defn- selection-frames
  "Run f while recording, after every owner tick, the committed Solo and Mute
  sets and the instances the renderer includes."
  [runtime f]
  (let [frames (atom []) tick! show/tick!]
    (with-redefs [show/tick! (fn [& args]
                               (let [result (apply tick! args)
                                     {:keys [instances included-instance-ids solo-active?]}
                                     (:effect-selection (show/snapshot runtime))]
                                 (swap! frames conj {:solo (into #{} (keep #(when (:soloed? %) (:id %))) instances)
                                                     :mute (into #{} (keep #(when (:muted? %) (:id %))) instances)
                                                     :solo-active? solo-active?
                                                     :included (set included-instance-ids)})
                                 result))]
      (f))
    ;; Consecutive ticks that commit the same selection count as one frame.
    (vec (dedupe @frames))))

(deftest control-commits-an-exclusive-solo-or-mute-in-one-frame
  (let [{:keys [runtime input!] :as h} (harness)]
    (try
      (desk! h)
      (doseq [cue [:c0 :c1 :c2 :c3 :c4 :c5]] (launch! h cue))
      (accepted! h {:op :effect-selection/configure :modes {:solo :additive :mute :additive}})
      (let [[i0 i1 i2 i3 i4 i5] (map #(:id (latest h %)) [:c0 :c1 :c2 :c3 :c4 :c5])
            revision #(:revision (model-of h))
            control! (fn [note]
                       (let [before (revision)
                             frames (selection-frames runtime #(input! :main [0x90 72 127] [0x90 note 127] [0x80 note 0] [0x80 72 0]))]
                         (is (= (inc before) (revision)) "One command, one owner commit")
                         frames))
            press! (fn [note] (input! :main [0x90 note 127] [0x80 note 0]))
            toggle! (fn [control ids] (accepted! h {:op :effect-selection/toggle :control control
                                                    :selector {:instance-ids ids} :additive? true}))]
        (testing "c0 and c3 soloed, c4 and c5 running: CONTROL+SOLO on c4 replaces the selection in one frame"
          (press! 9)
          (press! 12)
          (let [frames (control! 13)]
            (is (= [#{i4}] (mapv :solo frames)))
            (is (every? :solo-active? frames) "Solo stays active in every committed frame")
            (is (not-any? #(contains? (:included %) i5) frames) "c5, which nobody soloed, never plays")))
        (testing "a strip whose instances partly overlap the selection: the group (c1, c2) with c0 and c1 soloed"
          (toggle! :solo [i4])
          (toggle! :solo [i0 i1])
          (is (= [#{i1 i2}] (mapv :solo (control! 8)))))
        (testing "a strip wholly selected among others"
          (toggle! :solo [i0 i3])
          (is (= [#{i1 i2}] (mapv :solo (control! 8))))
          (testing "and CONTROL again on the whole selection clears it"
            (is (= [#{}] (mapv :solo (control! 8))))))
        (testing "Mute: c0 and c3 stay muted until c4 replaces them"
          (press! 17)
          (press! 20)
          (let [frames (control! 21)]
            (is (= [#{i4}] (mapv :mute frames)))
            (is (every? #(and (contains? (:included %) i0) (not (contains? (:included %) i4))) frames))))
        (testing "a selection another client queued before the press is replaced; one sent after applies on top"
          (toggle! :mute [i4])
          (press! 9)
          (let [soloed #(into #{} (keep (fn [i] (when (:soloed? i) (:id i)))) (:instances (:effect-selection (show/snapshot runtime))))]
            (show/submit! runtime :command {:op :effect-selection/toggle :control :solo :selector {:instance-ids [i5]}
                                            :additive? true :source {:adapter :repl :session-id "xt-test"}})
            (input! :main [0x90 72 127] [0x90 13 127] [0x80 13 0] [0x80 72 0])
            (is (= #{i4} (soloed)) "c5, queued before the press, is replaced")
            (toggle! :solo [i5])
            (is (= #{i4 i5} (soloed)) "c5, sent after the press, is added to the strip's Solo"))))
      (finally (show/stop! runtime)))))

(deftest control-changes-nothing-when-its-strip-ends-first
  (let [{:keys [runtime input!] :as h} (harness)]
    (try
      (desk! h)
      (doseq [cue [:c0 :c1 :c2 :c3 :c4]] (launch! h cue))
      (accepted! h {:op :effect-selection/configure :modes {:solo :additive}})
      (let [[i0 i1 _ i3 i4] (map #(:id (latest h %)) [:c0 :c1 :c2 :c3 :c4])
            soloed #(into #{} (keep (fn [i] (when (:soloed? i) (:id i)))) (:instances (:effect-selection (show/snapshot runtime))))
            ;; The end is queued before the press, so the unit still sees the instance
            ;; but the owner applies the end before the unit's exclusive toggle.
            ending! (fn [id note]
                      (show/submit! runtime :command {:op :instance/end :instance-id id :force? true
                                                      :source {:adapter :repl :session-id "xt-test"}})
                      (input! :main [0x90 72 127] [0x90 note 127] [0x80 note 0] [0x80 72 0]))]
        (input! :main [0x90 9 127] [0x80 9 0] [0x90 12 127] [0x80 12 0])
        (is (= #{i0 i3} (soloed)))
        (testing "a cue strip whose instance ends first: the toggle matches nothing"
          (ending! i4 13)
          (is (nil? (get-in (model-of h) [:instances i4])))
          (is (= #{i0 i3} (soloed))))
        (testing "a group strip whose member ends first: the toggle is refused"
          (ending! i1 8)
          (is (nil? (get-in (model-of h) [:instances i1])))
          (is (= #{i0 i3} (soloed)))))
      (finally (show/stop! runtime)))))

(deftest solo-and-mute-send-one-toggle-with-the-pressed-mode
  (let [{:keys [runtime] :as h} (harness)]
    (try
      (desk! h)
      (doseq [cue [:c0 :c3 :c4]] (launch! h cue))
      (accepted! h {:op :effect-selection/configure :modes {:solo :additive}})
      (let [dispatched (atom [])
            receiver (atom nil)
            ;; A second session on the attached main surface whose dispatch is captured.
            unit (x-touch/connect! {:session-id "xt-capture" :surface-id "xt-main" :schedule! nil
                                    :rows #(surface/strip-channels % "xt-main" {:type :mixer-strips :effect-group-view :manual
                                                                               :cue-strips (:channels main-options)})
                                    :placement #(surface/strip-placement % "xt-main")
                                    :model! #(model-of h)
                                    :now-ms! (constantly 0)
                                    :dispatch! (fn [command]
                                                 (swap! dispatched conj (select-keys command [:op :control :selector :mode :additive?]))
                                                 (submit! h (dissoc command :source :command-id)))
                                    :transport {:open! (fn [r] (reset! receiver r) :t) :send! (fn [_ _]) :close! (fn [_])}})
            input! (fn [& messages] (doseq [bytes messages] (@receiver bytes)))
            soloed #(into #{} (keep (fn [i] (when (:soloed? i) (:id i)))) (:instances (:effect-selection (show/snapshot runtime))))
            [i0 i3 i4] (map #(:id (latest h %)) [:c0 :c3 :c4])
            toggle (fn [selector & [mode]] (cond-> {:op :effect-selection/toggle :control :solo :selector selector}
                                             (= :additive mode) (assoc :additive? true)
                                             (= :exclusive mode) (assoc :mode :exclusive)))]
        (try
          (input! [0x90 9 127] [0x80 9 0] [0x90 70 127] [0x90 12 127] [0x80 12 0] [0x80 70 0])
          (is (= #{i0 i3} (soloed)))
          (input! [0x90 72 127] [0x90 13 127] [0x80 13 0] [0x80 72 0])
          (is (= #{i4} (soloed)))
          (is (= [(toggle {:cue-ids [:c0]}) (toggle {:cue-ids [:c3]} :additive) (toggle {:cue-ids [:c4]} :exclusive)]
                 @dispatched)
              "A plain press leaves the mode to the owner; SHIFT adds; CONTROL inverts it in the same command")
          (testing "under an exclusive default CONTROL adds with the :additive? shorthand"
            (accepted! h {:op :effect-selection/configure :modes {:solo :exclusive}})
            (reset! dispatched [])
            (input! [0x90 72 127] [0x90 9 127] [0x80 9 0] [0x80 72 0])
            (is (= [(toggle {:cue-ids [:c0]} :additive)] @dispatched))
            (is (= #{i0 i4} (soloed))))
          (finally (device-session/close! unit))))
      (finally (show/stop! runtime)))))

(def ^:private drift
  {:id :drift :version 1
   :params {:count {:type :integer :default 2}
            :level {:type :number :default 0.5 :bounds [0 1]}
            :pos {:type :vector :size 2 :default [0.5 0.0] :recommended [-1 1]}
            :rate {:type :number :default 5.0}
            :speed {:type :number :default 2.0 :recommended [0 4]}
            :steps {:type :integer :default 3 :bounds [0 4]}}
   :outputs {:dimmer {:op :param :key :level}}})

(defn- drift-harness []
  (harness (-> (model)
               (assoc-in [:registry :effects :drift] drift)
               (assoc-in [:registry :cues :d0] {:id :d0 :version 1 :effect-id :drift :priority 0 :name "D0"}))))

(deftest unranged-numbers-get-no-guessed-range
  (let [{:keys [runtime] :as h} (drift-harness)]
    (try
      (launch! h :d0)
      (let [ctx (mixer/context (model-of h))
            row {:cue-id :d0}
            item (fn [label] (some #(when (= label (:label %)) (:target %)) (mixer/items ctx {:encoder-mode :plug-in} row)))]
        (testing "PLUG-IN leaves out a number with neither bounds nor a recommended range; an integer stays for relative steps"
          (is (= ["count" "level" "speed" "steps"] (mapv :label (mixer/items ctx {:encoder-mode :plug-in} row))))
          (is (= :count (:param (:encoder (mixer/strip ctx {:encoder-mode :pan} row)))) "The PAN fallback"))
        (testing "an unranged integer turns by one, with no ring position and no fader"
          (is (= [5 0 -1] (mapv #(:value (mixer/turn (item "count") % false)) [3 -2 -3])))
          (is (nil? (mixer/ring (item "count"))))
          (is (= "2" (mixer/value-text (item "count"))))
          (is (nil? (:fader (mixer/strip ctx {:encoder-mode :pan} (assoc row :fader :count)))))
          (is (nil? (:fader (mixer/strip ctx {:encoder-mode :pan, :flip? true} row))) "FLIP does not make it a fader"))
        (testing "an explicit binding to an unranged number binds nothing and is flagged"
          (let [s (mixer/strip ctx {:encoder-mode :pan} (assoc row :encoder :rate :fader :rate))]
            (is (= [nil nil :rate] ((juxt :encoder :fader :unranged) s))))
          (is (= ["D0" "!range"]
                 ((juxt :label :value)
                  (first (x-touch/project {:surface-id "xt" :rows (constantly [(assoc row :encoder :rate)])
                                           :placement (constantly {:first 0 :index 0 :span 8 :layout surface/layout-defaults})}
                                          {} (model-of h)))))))
        (testing "a vector component with only a recommended range maps onto it"
          (let [pos (:encoder (mixer/strip ctx {:encoder-mode :pan} (assoc row :encoder {:param :pos :component 0})))]
            (is (= 0.75 (mixer/normalized pos)))
            (is (= {:op :param/set :param :pos :value [0.52 0.0]}
                   (select-keys (update (mixer/turn pos 1 false) :value #(mapv (fn [x] (/ (Math/round (* 1e9 x)) 1e9)) %))
                                [:op :param :value])))))
        (let [speed (item "speed")]
          (testing "a recommended range scales the step; turns move from the value and only hard bounds limit them"
            (is (= {:mode :wrap :position 6 :center? true} (mixer/ring speed)))
            (is (< (Math/abs (- 2.04 (:value (mixer/turn speed 1 false)))) 1e-9))
            (is (< (Math/abs (- 6.004 (:value (mixer/turn (assoc speed :value 6.0) 1 true)))) 1e-9)
                "A value above the recommended range is not pulled into it")
            (is (= 1.0 (:value (mixer/turn (:fader (mixer/strip ctx {:encoder-mode :pan} row)) 80 false))))))
        (testing "integers step by one within their bounds"
          (is (= [4 4 1] (mapv #(:value (mixer/turn (item "steps") % false)) [1 5 -2])))))
      (finally (show/stop! runtime)))))

(deftest connecting-refuses-a-channel-bound-to-an-unranged-number
  (let [{:keys [runtime transport]} (drift-harness)]
    (try
      (let [error (try (show/connect-device! runtime :x-touch {:session-id "xt" :schedule! nil :transport (transport :main)
                                                               :channels [{:cue-id :d0 :fader :rate}]})
                       nil
                       (catch clojure.lang.ExceptionInfo e (ex-data e)))]
        (is (= {:x-touch/error :unranged-binding :channels [{:cue-id :d0 :fader :rate}]} error))
        (is (empty? @(:devices runtime)) "No session opened"))
      (is (some? (show/connect-device! runtime :x-touch {:session-id "xt" :schedule! nil :transport (transport :main)
                                                         :channels [{:cue-id :d0 :fader :speed :encoder :count}]}))
          "A recommended range and an unranged integer encoder are accepted")
      (finally (show/stop! runtime)))))

(deftest meters-scribble-colours-and-stop-are-byte-exact
  (let [{:keys [runtime input! time] :as h} (harness)]
    (try
      (let [{:keys [refresh! main]} (desk! h)]
        (take-sent! h :main)
        (launch! h :c0)
        (refresh!)
        (testing "row 1 names, row 2 values; group cyan, red cue colour, others by category (intensity: yellow)"
          (is (= [["Group" "C0" "C1" "C2" "C3" "C4" "C5" "C6"] ["100%" "0.00" "" "" "" "" "" ""]] (lcd main)))
          (is (= [240 0 0 102 20 114 6 1 3 3 3 3 3 3 247] (shown main :colors))))
        (testing "the running cue's meter is its level times opacity: 0.5 -> 6 of 12"
          (is (some #{[0xd0 0x16]} (take-sent! h :main))))
        (testing "an unchanged meter is resent only after the refresh interval"
          (refresh!)
          (is (not-any? #(= 0xd0 (first %)) (take-sent! h :main)))
          (swap! time + 250)
          (refresh!)
          (is (= [[0xd0 0x16]] (filterv #(= 0xd0 (first %)) (take-sent! h :main)))))
        (testing "opacity (SEND) scales the meter"
          (input! :main [0x90 41 127])
          (input! :main [0xb0 17 0x40 0x32])
          (input! :main [0xb0 17 0x72])
          (is (< (Math/abs (- 0.5 (get-in (latest h :c0) [:system :opacity]))) 1e-9))
          (refresh!)
          (is (some #{[0xd0 0x13]} (take-sent! h :main)) "0.5 x 0.5 -> 3 of 12")
          (is (= [0xb0 49 0x26] (shown main [:ring 1])) "opacity ring: wrap mode, position 6"))
        (testing "disconnect releases holds and blanks the unit"
          (input! :main [0x90 3 127])
          (take-sent! h :main)
          ((:owner! h) #(show/disconnect-device! runtime "xt-main"))
          (let [sent (take-sent! h :main)]
            (is (nil? (latest h :c2)) "The held cue is released")
            (is (some #{[240 0 0 102 20 114 0 0 0 0 0 0 0 0 247]} sent))
            (is (some #{[0xb0 49 0]} sent))
            (is (some #{[0xd0 0x10]} sent))
            (is (some #{[0x90 41 0]} sent) "Lit LEDs are turned off")
            (is (= :closed (peek sent))))
          (is (= :closed (:status (device-session/snapshot main))))))
      (finally (show/stop! runtime)))))

(deftest play-and-stop-leave-an-external-transport-alone
  (let [rows-fn (constantly [{:cue-id :c0}])
        placement (constantly {:first 0 :index 0 :span 8 :layout surface/layout-defaults})
        envelope {:command-id "x" :show-id :s :epoch 1 :time-ms 0}
        model (model)
        input (fn [external?]
                (x-touch/input (merge {:surface-id "xt" :extender? false :rows rows-fn :placement placement
                                       :external-transport? (constantly external?)})
                               {:modifiers #{} :touches {} :hold-keys {}}
                               {:type :button :note 94 :pressed? true} model envelope))]
    (is (= [{:op :transport/set :transport {:playing? true} :show-id :s :epoch 1}] (:commands (input false))))
    (is (= [] (:commands (input true))) "Link with start/stop sync owns play")))

(deftest scribble-colours-match-by-hue-with-a-white-cut-off
  (is (= [0 1 3 2 6 4 5 7 7 1 4]
         (mapv mixer/palette-index [[0 0 0] [1 0 0] [1 1 0] [0 1 0] [0 1 1] [0 0 1] [1 0 1]
                                    [0.5 0.5 0.5] [1 0.9 0.9] [0.3 0 0] [0.2 0.3 0.9]])))
  (is (= 7 (mixer/palette-index [0.3 0 0] :off 0.19)))
  (is (= 1 (mixer/palette-index [0.6 0.1 0.1] :rgb 0.19))))

(deftest rings-values-and-empty-strips-read-any-definition
  (testing "an integer default lights the centre LED at the equal double value"
    (is (= {:mode :boost-cut :position 6 :center? true}
           (mixer/ring {:kind :param :param :x :schema {:type :number :bounds [-1 1] :default 0} :value 0.0}))))
  (testing "enum values that are not keywords show as text"
    (is (= ["2" "b"] (mapv #(mixer/value-text {:kind :param :schema {:type :enum :values [1 2 "b"]} :value %}) [2 "b"]))))
  (testing "strips past the last row park their motors at zero"
    (let [placement (constantly {:first 0 :index 0 :span 8 :layout surface/layout-defaults})
          options {:surface-id "xt" :extender? false :rows (constantly [{:cue-id :c0}]) :placement placement
                   :external-transport? (constantly false) :color-match :hue :color-cutoff 0.19 :meter-refresh-ms 200}
          {:keys [midi]} (x-touch/feedback options {:touches {3 nil} :sent {} :meters {}} (model) {:time-ms 0})]
      (is (= (into [[0xe0 0 0]] (map #(vector (+ 0xe0 %) 0 0)) [1 2 4 5 6 7])
             (filterv #(<= 0xe0 (first %) 0xe7) midi))
          "Every strip but the touched one gets a zero motor message"))))

(deftest layout-state-is-validated-saved-and-survives-unlinking
  (let [attach (fn [m id d]
                 (:model (surface/apply-operation m {:op :control-surface/attach :surface-id id :descriptor d :role :unit
                                                     :viewports {:strips {:type :mixer-strips}}})))
        layout! (fn [m target patch] (surface/apply-operation m {:op :control-surface/layout :target target :set patch}))
        m (-> {} (attach "main" :behringer/x-touch) (attach "ext" :behringer/x-touch-ext))
        m (:model (layout! m "main" {:encoder-mode :user}))
        m (:model (surface/apply-operation m {:op :control-surface/link :link-id "desk" :mode :extend
                                              :members [{:surface-id "main"} {:surface-id "ext"}]}))]
    (is (= :user (get-in (surface/strip-placement m "ext") [:layout :encoder-mode])) "The leader's mode carries into the link")
    (is (= 8 (:index (surface/strip-placement m "ext"))))
    (is (thrown? clojure.lang.ExceptionInfo (layout! m "desk" {:encoder-mode :track})))
    (is (thrown? clojure.lang.ExceptionInfo (layout! m "desk" {:volume 1})))
    (let [{m :model events :events} (layout! m "ext" {:selected [:group [:manual :g0]] :page 2})]
      (is (= [:control-surface/layout-changed] (map :type events)))
      (is (= {:encoder-mode :user :selected [:group [:manual :g0]] :page 2}
             (get-in m [:extensions :tolglow.surface.link/state :links "desk" :layout])) "Defaults are not stored")
      (is (= m (surface/validate-state! m)) "A saved layout validates on load")
      (let [m (:model (surface/apply-operation m {:op :control-surface/unlink :link-id "desk"}))]
        (is (= [:user :user] (map #(get-in (surface/strip-placement m %) [:layout :encoder-mode]) ["main" "ext"])))
        (is (thrown? clojure.lang.ExceptionInfo
                     (surface/validate-state! (assoc-in m [:extensions :tolglow.surface.link/state :surfaces "main"
                                                           :viewports :strips :layout :page] -1))))))))

(deftest a-mirror-shows-one-window-and-only-the-touched-unit-drops-its-motor
  (let [{:keys [runtime input! owner!] :as h} (harness)]
    (try
      (let [{:keys [refresh! main ext]} (desk! h)]
        (owner! #(deref (show/link-surfaces! runtime "desk" ["xt-main" "xt-ext"] {:mode :mirror}) 1000 ::missing))
        (launch! h :c0)
        (refresh!)
        (is (= (first (lcd main)) (first (lcd ext))))
        (take-sent! h :ext)
        (input! :ext [0x90 105 127] [0xe1 0 32])
        (refresh!)
        (is (= (/ 4096.0 16383.0) (get-in (latest h :c0) [:params :level])))
        (is (= [0xe1 0 32] (shown main [:fader 1])) "The other member follows on feedback")
        (is (not-any? #(= 0xe1 (first %)) (take-sent! h :ext)) "The touched member gets no motor message")
        (input! :ext [0x90 105 0])
        (refresh!)
        (is (= [[0xe1 0 32]] (filterv #(= 0xe1 (first %)) (take-sent! h :ext))) "Release reasserts it"))
      (finally (show/stop! runtime)))))

(deftest a-captured-main-unit-replay-normalizes-every-button-the-layout-uses
  (let [unit (virtual/create descriptor/x-touch "xt-main")
        capture [[0x90 42 127] [0x90 41 127] [0x90 43 127] [0x90 69 127] [0x90 44 127] [0x90 45 127]
                 [0x90 72 127] [0x90 90 127] [0x80 90 0] [0x90 94 127] [0x90 93 127] [0x90 8 127] [0x90 112 127]]
        events (virtual/replay-into! unit mcu/decode descriptor/x-touch-event capture)]
    (is (= [[:buttons :pan] [:buttons :send] [:buttons :plug-in] [:buttons :user] [:buttons :eq] [:buttons :inst]
            [:buttons :control] [:buttons :global-solo] [:buttons :global-solo] [:buttons :play] [:buttons :stop]
            [:strip-buttons 0] [:master-fader 0]]
           (mapv (juxt :group (comp first :at)) events)))))

(deftest follow-and-mirror-placements-index-the-logical-page
  (let [attach (fn [m id d]
                 (:model (surface/apply-operation m {:op :control-surface/attach :surface-id id :descriptor d :role :unit
                                                     :viewports {:strips {:type :mixer-strips}}})))
        base (-> {} (attach "main" :behringer/x-touch) (attach "ext" :behringer/x-touch-ext))
        linked (fn [options]
                 (:model (surface/apply-operation base (merge {:op :control-surface/link :link-id "desk"}
                                                              (surface/link-command ["main" "ext"] options)))))]
    (is (= [0 0 8] ((juxt (comp :index #(surface/strip-placement % "ext")) :first-main :span)
                    (let [m (linked {:mode :mirror})]
                      (assoc (surface/strip-placement m "ext") :first-main 0)))))
    (is (= {:index 8 :span 8} (select-keys (surface/strip-placement (linked {:mode :follow :offset {:x 1}}) "ext") [:index :span]))
        "A follower one page right shows the next page of items")
    (is (= {:index 8 :span 16} (select-keys (surface/strip-placement (linked {}) "ext") [:index :span])))))
