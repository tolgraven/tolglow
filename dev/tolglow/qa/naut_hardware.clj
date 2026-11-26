(ns tolglow.qa.naut-hardware
  "Push 3 and X-Touch/XT qualification kit for the Naut Mac. Every check runs
  against an explicitly selected real port or a scripted virtual peer through the
  same transport boundary, so the receipts and assertions are identical. Nothing
  here discovers devices on require, arms lighting output or talks to Live. See
  docs/qualification/naut-hardware.md for the operator procedure."
  (:require [afterglow.control.codecs.x-touch :as xt-codec]
            [afterglow.control.devices.session :as device-session]
            [afterglow.control.devices.transports :as transports]
            [afterglow.control.model :as control]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.pprint :as pprint]
            [clojure.string :as str]
            [tolglow.show :as show])
  (:import [java.time Instant]
           [java.util.concurrent LinkedBlockingQueue TimeUnit]
           [javax.sound.midi MidiDevice MidiMessage MidiSystem Receiver]))

;; ---------------------------------------------------------------------------
;; Ports. The project classpath carries CoreMIDI4J, whose entries are named
;; after the CoreMIDI display name. The JDK's own macOS entries are named only
;; "User Port" or "INT" (ambiguous between a Push 2 and a Push 3) and its SysEx
;; input has a long record of problems, so roles resolve to CoreMIDI4J by name.

(def roles
  {:push-3 {:name "CoreMIDI4J - Ableton Push 3 User Port"}
   :x-touch {:name "CoreMIDI4J - X-Touch INT"}
   :x-touch-ext {:name "CoreMIDI4J - X-Touch-Ext"}})

(defn ports
  "Passive JVM MIDI listing. No device is opened."
  []
  (transports/midi-ports))

(defn port
  "Resolve exactly one input or output port for role from a fresh listing."
  ([role direction] (port role direction (ports)))
  ([role direction listing]
   (let [port-name (or (get-in roles [role :name])
                       (throw (ex-info "Unknown device role" {:role role})))
         flag (case direction :input :input? :output :output?)
         matches (filterv #(and (= port-name (:name %)) (flag %)) listing)]
     (when-not (= 1 (count matches))
       (throw (ex-info "Expected exactly one matching MIDI port" {:role role :direction direction
                                                                  :name port-name :matches matches})))
     (first matches))))

(defn midi-transport
  "Bidirectional Afterglow transport for role. Opening it is an explicit act."
  [role]
  (let [in (port role :input) out (port role :output)]
    (transports/midi-transport {:input-index (:index in) :output-index (:index out)
                                :expected-input-name (:name in) :expected-output-name (:name out)})))

(defn- message-bytes [^MidiMessage message]
  (mapv #(bit-and 0xff %) (take (.getLength message) (.getMessage message))))

(defn input-transport
  "Listen to role's input only. No output device is opened and :send! throws,
  so a Push 3 session or layout capture cannot emit a byte to the controller."
  [role]
  (let [{:keys [index name description]} (port role :input)]
    {:open! (fn [receive!]
              (let [info (get (vec (MidiSystem/getMidiDeviceInfo)) index)]
                (when-not (and info (= name (.getName info)) (= description (.getDescription info)))
                  (throw (ex-info "MIDI port changed; list ports again" {:role role})))
                (let [^MidiDevice device (MidiSystem/getMidiDevice info)
                      owned? (not (.isOpen device))]
                  (when owned? (.open device))
                  (try
                    (let [transmitter (.getTransmitter device)]
                      (.setReceiver transmitter (reify Receiver
                                                  (send [_ message _] (receive! (message-bytes message)))
                                                  (close [_] nil)))
                      {:transmitter transmitter :device (when owned? device)})
                    (catch Exception error
                      (when owned? (.close device))
                      (throw error))))))
     :send! (fn [_ messages]
              (throw (ex-info "Input-only transport never sends" {:messages (count messages)})))
     :close! (fn [{:keys [transmitter ^MidiDevice device]}]
               (try (.close ^java.lang.AutoCloseable transmitter)
                    (finally (when device (.close device)))))}))

(defn virtual-peer
  "A scripted device at the transport boundary. respond maps each message the
  host sends to the device's replies, delivered asynchronously like hardware.
  :emit! injects operator input; :sent records host output."
  ([] (virtual-peer (constantly nil)))
  ([respond]
   (let [receive (atom nil) sent (atom []) deliver! #(when-let [r @receive] (r (vec %)))]
     {:transport {:open! (fn [r] (reset! receive r) :virtual)
                  :send! (fn [_ messages]
                           (doseq [message messages]
                             (swap! sent conj (vec message))
                             (when-let [replies (seq (respond (vec message)))]
                               (future (doseq [reply replies] (deliver! reply))))))
                  :close! (fn [_] (reset! receive nil))}
      :emit! deliver!
      :sent sent})))

(defn recording-transport
  "Wrap transport so host output and device input are both logged, in order,
  with monotonic milliseconds since the wrapper was created."
  [transport]
  (let [log (atom []) start (System/nanoTime)
        stamp #(/ (- (System/nanoTime) start) 1e6)]
    {:log log
     :transport (assoc transport
                       :open! (fn [receive!]
                                ((:open! transport) (fn [bytes]
                                                      (swap! log conj {:ms (stamp) :in (vec bytes)})
                                                      (receive! bytes))))
                       :send! (fn [handle messages]
                                (swap! log into (map #(hash-map :ms (stamp) :out (vec %)) messages))
                                ((:send! transport) handle messages)))}))

;; ---------------------------------------------------------------------------
;; Receipts

(def receipt-root "target/qa/naut-hardware")

(defn hex [bytes] (str/join " " (map #(format "%02X" %) bytes)))

(defn write-receipt!
  "Write receipt EDN under target/qa/naut-hardware and return its path."
  ([receipt] (write-receipt! receipt-root receipt))
  ([root {:keys [check] :as receipt}]
   (let [file (io/file root (str (str/replace (subs (str check) 1) "/" "-") "-"
                                 (str/replace (str (Instant/now)) ":" "") ".edn"))]
     (io/make-parents file)
     (spit file (with-out-str (pprint/pprint receipt)))
     (.getPath file))))

;; ---------------------------------------------------------------------------
;; Automatic round trips: the device itself answers; no human is needed.

(def identity-request [0xf0 0x7e 0x7f 0x06 0x01 0xf7])

(defn identity-reply? [message]
  (and (vector? message) (<= 15 (count message)) (= [0xf0 0x7e] (subvec message 0 2))
       (= [0x06 0x02] (subvec message 3 5)) (= 0xf7 (peek message))))

(defn describe-identity
  "Universal Identity Reply fields. A leading zero manufacturer byte means a
  three-byte manufacturer ID (Ableton is 00 21 1D, Behringer 00 20 32)."
  [message]
  (let [body (subvec message 5 (dec (count message)))
        [manufacturer more] (if (zero? (first body)) [(subvec body 0 3) (subvec body 3)] [(subvec body 0 1) (subvec body 1)])]
    {:device-id (nth message 2) :manufacturer (hex manufacturer)
     :family (hex (take 2 more)) :model (hex (take 2 (drop 2 more))) :version (hex (drop 4 more))}))

(defn mcu-version-request [device] [0xf0 0x00 0x00 0x66 device 0x13 0x00 0xf7])

(defn mcu-version-reply?
  "The reply the operator's Live script also parses: F0 00 00 66 dd 14 v.. F7."
  [device]
  (fn [message]
    (and (vector? message) (<= 8 (count message)) (= [0xf0 0 0 0x66 device 0x14] (subvec message 0 6))
         (= 0xf7 (peek message)))))

(defn describe-mcu-version [message]
  (let [payload (subvec message 6 (dec (count message)))]
    {:bytes (hex payload) :ascii (apply str (map #(if (<= 32 % 126) (char %) \?) payload))}))

(def probes
  {:push-3/identity {:role :push-3 :issues [219] :request identity-request
                     :reply? identity-reply? :describe describe-identity}
   :x-touch/identity {:role :x-touch :issues [310] :request identity-request
                      :reply? identity-reply? :describe describe-identity}
   :x-touch/firmware {:role :x-touch :issues [310 553] :request (mcu-version-request 0x14)
                      :reply? (mcu-version-reply? 0x14) :describe describe-mcu-version}
   :x-touch-ext/firmware {:role :x-touch-ext :issues [310 553] :request (mcu-version-request 0x15)
                          :reply? (mcu-version-reply? 0x15) :describe describe-mcu-version}})

(defn probe!
  "Send one query and wait for the device's reply. Other input is counted but
  not interpreted. Returns a receipt; never throws for a silent device."
  [check {:keys [transport timeout-ms port] :or {timeout-ms 1500}}]
  (let [{:keys [request reply? describe issues role]} (or (get probes check)
                                                           (throw (ex-info "Unknown probe" {:check check})))
        queue (LinkedBlockingQueue.)
        handle ((:open! transport) #(.put queue (vec %)))
        start (System/nanoTime)
        elapsed #(/ (- (System/nanoTime) start) 1e6)
        base {:check check :issues issues :role role :port port :request (hex request)
              :at (str (Instant/now))}]
    (try
      ((:send! transport) handle [request])
      (loop [ignored 0]
        (let [remaining (long (- timeout-ms (elapsed)))
              message (when (pos? remaining) (.poll queue remaining TimeUnit/MILLISECONDS))]
          (cond
            (nil? message) (assoc base :verdict :no-reply :ignored ignored :timeout-ms timeout-ms)
            (reply? message) (assoc base :verdict :answered :reply (hex message) :decoded (describe message)
                                    :elapsed-ms (elapsed) :ignored ignored)
            :else (recur (inc ignored)))))
      (finally ((:close! transport) handle)))))

;; ---------------------------------------------------------------------------
;; Push 3 User Mode layout capture (operator presses each control once).

(defn push-3-capture-plan
  "Ordered prompts. Pads use the shared bottom-left origin: row 1 is y 0.
  :pad-rows limits the pad rows captured (counted from the bottom)."
  ([] (push-3-capture-plan {}))
  ([{:keys [pad-rows] :or {pad-rows 8}}]
   (vec (concat
          (for [y (range pad-rows) x (range 8)]
            {:kind :pad :id [x y] :prompt (format "Press pad %d in row %d from the bottom" (inc x) (inc y))})
          (mapcat (fn [i]
                    (let [id (keyword (str "encoder-" i))]
                      [{:kind :encoder-touch :id id :prompt (format "Touch encoder %d above the display (do not turn)" i)}
                       {:kind :encoder :id id :prompt (format "Turn encoder %d one click clockwise" i)}]))
                  (range 1 9))
          [{:kind :encoder-touch :id :master :prompt "Touch the master (rightmost) encoder"}
           {:kind :encoder :id :master :prompt "Turn the master encoder one click clockwise"}
           {:kind :button :id :left :prompt "Press the left arrow button"}
           {:kind :button :id :right :prompt "Press the right arrow button"}
           {:kind :button :id :up :prompt "Press the up arrow button"}
           {:kind :button :id :down :prompt "Press the down arrow button"}
           {:kind :button :id :mute :prompt "Press the Mute button (used as blackout in the kit)"}
           {:kind :button :id :select :prompt "Press the Select button (used as the cue-gesture modifier)"}
           {:kind :touch-strip :id :touch-strip :prompt "Slide a finger along the touch strip"}]))))

(def ^:private expected-type
  {:pad #{:note-on} :encoder-touch #{:note-on} :encoder #{:control-change} :button #{:control-change}
   :touch-strip #{:pitch-bend} :mono-pressure #{:channel-pressure}})

(defn- channel-type [bytes]
  (let [status (first bytes)]
    (when (and (integer? status) (<= 0x80 status 0xef) (= 3 (count bytes)))
      (case (bit-and status 0xf0)
        0x90 (if (pos? (nth bytes 2)) :note-on :note-off)
        0x80 :note-off 0xa0 :poly-pressure 0xb0 :control-change 0xe0 :pitch-bend nil))))

(defn- capture-sample [layout {:keys [kind id]} bytes]
  (let [channel (bit-and (first bytes) 0x0f) data (second bytes)]
    (case kind
      (:pad :encoder-touch) (assoc-in layout [({:pad :pads :encoder-touch :encoder-touches} kind) id] {:channel channel :note data})
      (:encoder :button) (assoc-in layout [({:encoder :encoders :button :buttons} kind) id] {:channel channel :controller data})
      (:touch-strip :mono-pressure) (assoc layout kind {:channel channel}))))

(defn- used-addresses [layout]
  (set (concat (map #(vector :note (:channel %) (:note %)) (concat (vals (:pads layout)) (vals (:encoder-touches layout))))
               (map #(vector :cc (:channel %) (:controller %)) (concat (vals (:encoders layout)) (vals (:buttons layout)))))))

(defn- address-of [{:keys [kind]} bytes]
  (case kind
    (:pad :encoder-touch) [:note (bit-and (first bytes) 0x0f) (second bytes)]
    (:encoder :button) [:cc (bit-and (first bytes) 0x0f) (second bytes)]
    nil))

(defn capture-layout!
  "Prompt for each control in plan and record the first matching message it
  sends. Repeats of an already captured address are ignored, so a bounced pad
  cannot be learned twice. Returns {:layout .. :samples .. :skipped ..}.
  operator is {:prompt! (fn [item index total])}; replay operators emit the
  device bytes from prompt!."
  [{:keys [transport plan operator timeout-ms settle-ms] :or {timeout-ms 30000 settle-ms 250}}]
  (let [queue (LinkedBlockingQueue.)
        handle ((:open! transport) #(.put queue (vec %)))
        drain! (fn [ms] (loop [] (when (.poll queue (long ms) TimeUnit/MILLISECONDS) (recur))))]
    (try
      (drain! 50)
      (loop [items (map-indexed vector plan) layout {} samples [] skipped []]
        (if-let [[index item] (first items)]
          (do ((:prompt! operator) item index (count plan))
              (let [deadline (+ (System/currentTimeMillis) timeout-ms)
                    bytes (loop []
                            (let [remaining (- deadline (System/currentTimeMillis))
                                  message (when (pos? remaining) (.poll queue remaining TimeUnit/MILLISECONDS))]
                              (cond (nil? message) nil
                                    (and ((expected-type (:kind item)) (channel-type message))
                                         (not (contains? (used-addresses layout) (address-of item message))))
                                    message
                                    :else (recur))))]
                (drain! settle-ms)
                (if bytes
                  (recur (rest items) (capture-sample layout item bytes)
                         (conj samples (assoc (select-keys item [:kind :id]) :bytes (hex bytes))) skipped)
                  (recur (rest items) layout samples (conj skipped (select-keys item [:kind :id]))))))
          {:layout layout :samples samples :skipped skipped}))
      (finally ((:close! transport) handle)))))

;; ---------------------------------------------------------------------------
;; Scenario runtime: an in-memory show with no output sink, ticked by the kit.

(defn qualification-model []
  (control/new-show
    {:rig {:fixtures [{:id :head :universe 1 :address 1 :channels {:dimmer 1}}]}
     :registry {:effects {:wash {:id :wash :version 1
                                 :params {:level {:type :number :default 0.5 :bounds [0 1]}
                                          :count {:type :integer :default 5 :bounds [0 10]}}
                                 :outputs {:dimmer {:op :param :key :level}}}
                          :flat {:id :flat :version 1 :params {} :outputs {:dimmer 0.6}}}
                :cues {:tap {:id :tap :version 1 :effect-id :wash}
                       :hold {:id :hold :version 1 :effect-id :flat}
                       :expr {:id :expr :version 1 :effect-id :wash}
                       :spare {:id :spare :version 1 :effect-id :flat}}}
     :pages {:main {:id :main :cells {[0 0] :tap [1 0] :hold [2 0] :expr [3 0] :spare}}}}))

(def manual-groups
  {:manual [{:id :all :name "All" :cue-ids [:tap :expr]
             :groups [{:id :flat :name "Flat" :cue-ids [:hold :spare]}]}]})

(defn instances [model cue-id]
  (filterv #(= cue-id (:cue-id %)) (vals (:instances model))))

(defn running? [model cue-id] (boolean (some #(= :running (:phase %)) (instances model cue-id))))

(defn- start-ticker! [runtime]
  (let [running (atom true)
        thread (doto (Thread. #(while @running
                                 (try (show/tick! runtime) (catch Throwable _))
                                 (Thread/sleep 10))
                              "naut-hardware-ticker")
                 (.setDaemon true) (.start))]
    (fn [] (reset! running false) (.join thread 1000))))

(defn- submit-and-wait! [runtime payload]
  (deref (show/submit! runtime (merge {:source {:adapter :repl :session-id "naut-hardware"}} payload)) 2000 ::timeout))

;; ---------------------------------------------------------------------------
;; Operator actions as device bytes (used by the replay and to document what the
;; physical action must produce).

(defn- signed-cc [delta] (if (neg? delta) (+ 128 delta) delta))

(defn push-3-bytes
  "Translate one operator action to the bytes a Push 3 in User Mode sends at
  the captured addresses."
  [layout [action target value]]
  (let [pad (get-in layout [:pads target]) touch (get-in layout [:encoder-touches target])
        encoder (get-in layout [:encoders target]) button (get-in layout [:buttons target])]
    (case action
      :press [[(+ 0x90 (:channel pad)) (:note pad) (or value 100)]]
      :lift [[(+ 0x80 (:channel pad)) (:note pad) 0]]
      :pressure [[(+ 0xa0 (:channel pad)) (:note pad) value]]
      :touch [[(+ 0x90 (:channel touch)) (:note touch) 127]]
      :untouch [[(+ 0x80 (:channel touch)) (:note touch) 0]]
      :turn (repeat (Math/abs (long value)) [(+ 0xb0 (:channel encoder)) (:controller encoder) (signed-cc (Long/signum (long value)))])
      :button [[(+ 0xb0 (:channel button)) (:controller button) 127] [(+ 0xb0 (:channel button)) (:controller button) 0]]
      :button-down [[(+ 0xb0 (:channel button)) (:controller button) 127]]
      :button-up [[(+ 0xb0 (:channel button)) (:controller button) 0]]
      :strip (let [raw (long (* value 16383))] [[(+ 0xe0 (get-in layout [:touch-strip :channel])) (bit-and raw 0x7f) (bit-shift-right raw 7)]]))))

(defn x-touch-bytes
  "Translate one operator action to the MCU bytes an X-Touch or XT sends."
  [_ [action strip value]]
  (case action
    :select [[0x90 (+ 24 strip) 127] [0x90 (+ 24 strip) 0]]
    :mute [[0x90 (+ 16 strip) 127] [0x90 (+ 16 strip) 0]]
    :touch [[0x90 (+ 104 strip) 127]]
    :untouch [[0x90 (+ 104 strip) 0]]
    :fader (let [raw (long (* value 16383))] [[(+ 0xe0 strip) (bit-and raw 0x7f) (bit-shift-right raw 7)]])
    :turn (repeat (Math/abs (long value)) [0xb0 (+ 16 strip) (if (neg? value) 0x41 0x01)])))

;; ---------------------------------------------------------------------------
;; Scenarios. Each step has a prompt for the operator, the equivalent replay
;; actions, and an expectation over the committed show model. :confirm asks the
;; operator a yes/no question about what only eyes can check.

(defn- level-of [model cue-id]
  (some #(get-in % [:params :level]) (filter #(= :running (:phase %)) (instances model cue-id))))

(defn- group-level [runtime id]
  (let [channel-id [:manual id]]
    (some (fn [root] (if (= channel-id (:id root)) (:level root)
                       (some #(when (= channel-id (:id %)) (:level %)) (:groups root))))
          (get-in (show/effect-group-channels runtime) [:manual]))))

(defn- fader-feedback [log strip]
  (filterv #(= (+ 0xe0 strip) (first (:out %))) log))

(defn- text-writes [log]
  (filterv #(= [0xf0 0 0 0x66] (take 4 (:out %))) log))

(def scenarios
  {:push-3/pads
   {:issues [219 543] :device :push-3
    :about "Cue pads in User Mode with Poly Aftertouch: tap, hold, pressure and an encoder edit."
    :steps
    [{:id :tap :prompt "Tap the first pad of the bottom row briefly (shorter than a fifth of a second)."
      :actions [[:press [0 0] 100] [:wait 40] [:lift [0 0]]]
      :expect (fn [{:keys [model]}] (running? model :tap))}
     {:id :tap-ends :prompt "Tap the same pad again to end the cue."
      :actions [[:press [0 0] 100] [:wait 40] [:lift [0 0]]]
      :expect (fn [{:keys [model]}] (not (running? model :tap)))}
     {:id :hold :prompt "Hold the second pad of the bottom row for a full second, then lift."
      :actions [[:press [1 0] 100] [:wait 700] [:lift [1 0]]]
      :expect (fn [{:keys [model history]}]
                (and (some #(running? % :hold) history) (not (running? model :hold))))}
     {:id :pressure :prompt "Tap the third pad of the bottom row once, then press it again and lean on it firmly for two seconds before lifting."
      :actions [[:press [2 0] 100] [:wait 40] [:lift [2 0]] [:wait 300]
                [:press [2 0] 100] [:wait 100] [:pressure [2 0] 90] [:wait 200]
                [:pressure [2 0] 127] [:wait 300] [:lift [2 0]]]
      :expect (fn [{:keys [model history]}]
                (and (running? model :expr) (some #(when-let [l (level-of % :expr)] (> l 0.6)) history)))}
     {:id :select-running :prompt "Hold Select and touch the third pad (Expr is running), then let go of both. Nothing should launch or stop."
      :actions [[:button-down :select] [:press [2 0] 100] [:wait 40] [:lift [2 0]] [:button-up :select]]
      :expect (fn [{:keys [model start-model device]}]
                (let [expr (first (filter #(= :running (:phase %)) (instances model :expr)))]
                  (and expr (= (count (:instances start-model)) (count (:instances model)))
                       (= (:id expr) (get-in (device-session/snapshot device) [:local :selected-instance])))))}
     {:id :encoder :prompt "Touch encoder 1 and turn it clockwise about a quarter turn, then let go."
      :actions [[:touch :encoder-1] [:turn :encoder-1 8] [:wait 50] [:untouch :encoder-1]]
      :expect (fn [{:keys [model start-model]}]
                (let [now (level-of model :expr)] (and now (not= now (level-of start-model :expr)))))}
     {:id :blackout :prompt "Press the Mute button once (it toggles blackout in the kit)."
      :actions [[:button :mute]]
      :expect (fn [{:keys [model]}] (true? (:blackout? model)))}
     {:id :blackout-off :prompt "Press the Mute button again to clear blackout."
      :actions [[:button :mute]]
      :expect (fn [{:keys [model]}] (false? (:blackout? model)))}]}

   :x-touch/strips
   {:issues [310 553] :device :x-touch
    :about "Committed group levels on the main X-Touch: scribble strips, SELECT, touch-owned faders and motor feedback."
    :steps
    [{:id :scribble :prompt "Look at the LCD."
      :actions []
      :expect (fn [{:keys [log]}] (seq (text-writes log)))
      :confirm "Does strip 1 read 'All' and strip 2 'Flat' (group strips), followed by the cue strips 'Tap', 'Expr'? Are strip colors cyan, blue, then the cue colors?"}
     {:id :select :prompt "Press SELECT on strip 3 (labelled Tap)."
      :actions [[:select 2]]
      :expect (fn [{:keys [model]}] (running? model :tap))}
     {:id :group-fader :prompt "Touch the strip 1 fader (group All), pull it fully down, then let go."
      :actions [[:touch 0] [:fader 0 0.4] [:fader 0 0.0] [:wait 50] [:untouch 0]]
      :expect (fn [{:keys [runtime]}] (when-let [l (group-level runtime :all)] (< l 0.05)))}
     {:id :motor :prompt "Hands off. The kit sets group All to 75%: watch the strip 1 fader move by itself."
      :before (fn [{:keys [runtime]}]
                (submit-and-wait! runtime {:op :effect-groups/level-set :channel-id [:manual :all] :value 0.75}))
      :actions []
      :expect (fn [{:keys [runtime log]}]
                (and (= 0.75 (group-level runtime :all))
                     (some #(let [[_ lsb msb] (:out %)] (< 12000 (+ lsb (* 128 msb)) 12600)) (fader-feedback log 0))))
      :confirm "Did the strip 1 fader travel to about three quarters on its own and stop there?"}
     {:id :cue-fader :prompt "Touch the strip 3 fader (Tap), move it to the top, then let go."
      :actions [[:touch 2] [:fader 2 0.5] [:fader 2 1.0] [:wait 50] [:untouch 2]]
      :expect (fn [{:keys [model]}] (when-let [l (level-of model :tap)] (> l 0.95)))}
     {:id :untouched :prompt "Without touching the capacitive cap, nudge the strip 3 fader by its edge (or skip if impossible)."
      :actions [[:fader 2 0.2]]
      :expect (fn [{:keys [model]}] (when-let [l (level-of model :tap)] (> l 0.95)))
      :optional? true}
     {:id :mute :prompt "Press MUTE on strip 3 to release Tap."
      :actions [[:mute 2]]
      :expect (fn [{:keys [model]}] (not (running? model :tap)))}]}

   :x-touch-ext/strips
   {:issues [310 553] :device :x-touch-ext
    :about "The extender as the second unit: offset strips, touch and motor feedback."
    :steps
    [{:id :scribble :prompt "Look at the extender LCD."
      :actions []
      :expect (fn [{:keys [log]}] (seq (text-writes log)))
      :confirm "Does extender strip 1 read 'Spare' (the ninth channel) and are the other strips blank?"}
     {:id :select :prompt "Press SELECT on extender strip 1."
      :actions [[:select 0]]
      :expect (fn [{:keys [model]}] (running? model :spare))}
     {:id :motor :prompt "Hands off. Watch extender strip 2: nothing should move (it has no channel)."
      :actions [] :expect (fn [{:keys [log]}] (empty? (fader-feedback log 1)))
      :confirm "Did extender fader 2 stay still?"}
     {:id :mute :prompt "Press MUTE on extender strip 1 to release Spare."
      :actions [[:mute 0]]
      :expect (fn [{:keys [model]}] (not (running? model :spare)))}]}})

(defn- connect-options [scenario layout transport]
  (case (:device scenario)
    ;; open-push-3! only adds a refresh cadence when a :display is given; without
    ;; one, hold deadlines never fire, so the kit supplies the same scheduler.
    :push-3 {:session-id "naut-push-3" :user-mode? true :expression-mode :poly-aftertouch
             :schedule! (transports/scheduler 20) :cue-gestures? true
             :layout layout :midi transport
             :encoder-bindings {:encoder-1 {:param :level} :master {:target :master}}
             :button-bindings {:mute :blackout :right :page-right :left :page-left :select :cue-gesture}
             :pressure-param :level :touch-strip-param :count}
    :x-touch {:session-id "naut-x-touch" :transport transport :refresh-ms 40 :effect-group-view :manual
              :channels [{:cue-id :tap :label "Tap" :color [1 0 0] :fader :level}
                         {:cue-id :expr :label "Expr" :color [0 1 0] :fader :level}]}
    :x-touch-ext {:session-id "naut-x-touch-ext" :transport transport :refresh-ms 40 :extender? true
                  :channel-offset 8
                  :channels (into [{:cue-id :tap :label "Tap"}] (concat (repeat 7 {:cue-id :hold :label "Hold"})
                                                                         [{:cue-id :spare :label "Spare" :color [1 1 0]}]))}))

(defn console-operator
  "Prompts on *out*; :confirm questions read y/n from *in*."
  []
  {:prompt! (fn [step] (println) (println (str ">> " (:prompt step))) (flush))
   :confirm! (fn [question]
               (print (str "?? " question " [y/n] ")) (flush)
               (= "y" (some-> (read-line) str/trim str/lower-case)))
   :perform! (fn [_ _])
   :capture-prompt! (fn [item index total] (println (format "[%d/%d] %s" (inc index) total (:prompt item))) (flush))})

(defn replay-operator
  "Emits each step's actions through emit! as device bytes, answers every
  confirm question yes, and records prompts."
  [emit! encode]
  (let [prompts (atom [])]
    {:prompts prompts
     :prompt! #(swap! prompts conj (:prompt %))
     :confirm! (fn [_] true)
     :perform! (fn [{:keys [actions]} layout]
                 (doseq [action actions]
                   (if (= :wait (first action))
                     (Thread/sleep (long (second action)))
                     (doseq [bytes (encode layout action)] (emit! bytes) (Thread/sleep 5)))))}))

(defn run-scenario!
  "Run one scenario against transport. The show is an in-memory runtime with no
  output sink. Returns a receipt with one entry per step."
  [scenario-id {:keys [transport layout operator step-timeout-ms port] :or {step-timeout-ms 30000}}]
  (let [scenario (or (get scenarios scenario-id) (throw (ex-info "Unknown scenario" {:scenario scenario-id})))
        {recorded :transport log :log} (recording-transport transport)
        runtime (show/create-runtime {:model (qualification-model) :workspace-options nil})
        stop-ticker! (start-ticker! runtime)]
    (try
      (when (#{:x-touch :x-touch-ext} (:device scenario))
        (submit-and-wait! runtime {:op :effect-groups/configure :config manual-groups}))
      (let [kind (if (= :push-3 (:device scenario)) :push-3 :x-touch)
            device (show/connect-device! runtime kind (connect-options scenario layout recorded))]
        (try
          (Thread/sleep 300)
          (let [steps
                (reduce
                  (fn [results step]
                    (let [model! #(:model @(:state runtime))
                          start-model (model!) history (atom [start-model])
                          context (fn [] {:runtime runtime :model (model!) :start-model start-model
                                          :history @history :log @log :device device})]
                      (when-let [before (:before step)] (before (context)))
                      ((:prompt! operator) step)
                      (let [performing (future ((:perform! operator) step layout))
                            t0 (System/nanoTime)
                            passed? (loop []
                                      (let [model (model!)]
                                        (when-not (identical? model (peek @history)) (swap! history conj model))
                                        (cond ((:expect step) (context)) true
                                              (> (/ (- (System/nanoTime) t0) 1e6) step-timeout-ms) false
                                              :else (do (Thread/sleep 20) (recur)))))
                            _ (deref performing 10000 nil)
                            answer (when (:confirm step) ((:confirm! operator) (:confirm step)))]
                        (conj results
                              (cond-> {:step (:id step) :verdict (cond (not passed?) (if (:optional? step) :skipped :failed)
                                                                       (false? answer) :operator-rejected
                                                                       :else :passed)
                                       :elapsed-ms (/ (- (System/nanoTime) t0) 1e6)}
                                (:confirm step) (assoc :question (:confirm step) :answer answer))))))
                  [] (:steps scenario))]
            {:check scenario-id :issues (:issues scenario) :about (:about scenario) :port port
             :at (str (Instant/now)) :steps steps
             :verdict (if (every? #(#{:passed :skipped} (:verdict %)) steps) :passed :failed)
             :session (select-keys (device-session/snapshot device) [:status :errors])
             :midi (mapv #(cond-> {:ms (:ms %)} (:in %) (assoc :in (hex (:in %))) (:out %) (assoc :out (hex (:out %)))) @log)})
          (finally (show/disconnect-device! runtime (:session-id (connect-options scenario layout recorded))))))
      (finally (stop-ticker!) (show/stop! runtime)))))

;; ---------------------------------------------------------------------------
;; X-Touch LCD/color rate and flicker (#310). Bounded and explicitly started;
;; the device must still answer its firmware query afterwards.

(defn rate-check!
  "Send scribble-strip text at each rate in rates-hz for seconds each, then a
  bounded native-color alternation at alternation-ms. Probes firmware before
  and after. Returns achieved message rates and probe latencies."
  [{:keys [transport extender? rates-hz seconds alternation-ms operator port]
    :or {rates-hz [10 25 50] seconds 5 alternation-ms 20}}]
  (let [device (xt-codec/device-id (boolean extender?))
        probe-id (if extender? :x-touch-ext/firmware :x-touch/firmware)
        before (probe! probe-id {:transport transport})
        handle ((:open! transport) (fn [_]))
        send! #((:send! transport) handle [%])
        run (fn [period-ms total-ms message-fn]
              (let [start (System/nanoTime) n (volatile! 0)]
                (while (< (/ (- (System/nanoTime) start) 1e6) total-ms)
                  (send! (message-fn @n)) (vswap! n inc)
                  (Thread/sleep (long period-ms)))
                {:messages @n :achieved-hz (/ @n (/ (- (System/nanoTime) start) 1e9))}))
        texts (try
                (vec (for [hz rates-hz]
                       (assoc (run (/ 1000.0 hz) (* 1000 seconds)
                                   (fn [n] (xt-codec/text (boolean extender?) 0 0 (format "%-56s" (str "Tolglow rate " hz " Hz #" n)))))
                              :requested-hz hz)))
                (finally ((:close! transport) handle)))
        handle ((:open! transport) (fn [_]))
        alternation (try
                      (let [send! #((:send! transport) handle [%])
                            start (System/nanoTime) n (volatile! 0)]
                        (while (< (/ (- (System/nanoTime) start) 1e6) (* 1000 seconds))
                          (send! (xt-codec/colors (boolean extender?) (vec (repeat 8 (if (even? @n) 1 4)))))
                          (vswap! n inc)
                          (Thread/sleep (long alternation-ms)))
                        (send! (xt-codec/colors (boolean extender?) (vec (repeat 8 7))))
                        {:interval-ms alternation-ms :messages @n
                         :achieved-hz (/ @n (/ (- (System/nanoTime) start) 1e9))})
                      (finally ((:close! transport) handle)))
        after (probe! probe-id {:transport transport})
        answer (when operator
                 ((:confirm! operator) "During the red/blue alternation, did strips look a stable magenta/purple without visible flicker?"))]
    {:check (if extender? :x-touch-ext/rate :x-touch/rate) :issues [310] :device device :port port
     :at (str (Instant/now)) :before before :after after :text texts :alternation alternation
     :answer answer
     :verdict (if (and (= :answered (:verdict before)) (= :answered (:verdict after))) :responsive :unresponsive)}))

;; ---------------------------------------------------------------------------
;; Command line

(defn- read-layout [path] (:layout (edn/read-string (slurp path))))

(defn -main
  "lein run -m tolglow.qa.naut-hardware ports
   ... probe push-3/identity|x-touch/identity|x-touch/firmware|x-touch-ext/firmware
   ... capture-layout [pad-rows]
   ... scenario push-3/pads LAYOUT.edn | scenario x-touch/strips | scenario x-touch-ext/strips
   ... rate x-touch|x-touch-ext"
  [& [command target extra]]
  (let [operator (console-operator)]
    (case command
      "ports" (pprint/pprint (filterv #(some (fn [role] (= (:name role) (:name %))) (vals roles)) (ports)))
      "probe" (let [check (keyword target) role (:role (get probes check))
                    receipt (probe! check {:transport (midi-transport role) :port (:name (port role :output))})]
                (pprint/pprint receipt) (println "receipt:" (write-receipt! receipt)))
      "capture-layout" (let [plan (push-3-capture-plan (if target {:pad-rows (parse-long target)} {}))
                             result (capture-layout! {:transport (input-transport :push-3) :plan plan
                                                      :operator {:prompt! (:capture-prompt! operator)}})
                             receipt (assoc result :check :push-3/layout :issues [219] :at (str (Instant/now))
                                                   :port (:name (port :push-3 :input)))]
                         (pprint/pprint (select-keys receipt [:skipped]))
                         (println "receipt:" (write-receipt! receipt)))
      "scenario" (let [id (keyword target)
                       device (:device (get scenarios id))
                       transport (if (= :push-3 device) (input-transport :push-3) (midi-transport device))
                       receipt (run-scenario! id {:transport transport :operator operator
                                                  :layout (when extra (read-layout extra))
                                                  :port (:name (port device :input))})]
                   (pprint/pprint (select-keys receipt [:verdict :steps :session]))
                   (println "receipt:" (write-receipt! receipt)))
      "rate" (let [role (keyword target)
                   receipt (rate-check! {:transport (midi-transport role) :extender? (= :x-touch-ext role)
                                         :operator operator :port (:name (port role :output))})]
               (pprint/pprint (dissoc receipt :text))
               (println "receipt:" (write-receipt! receipt)))
      (println (:doc (meta #'-main))))
    (shutdown-agents)))
