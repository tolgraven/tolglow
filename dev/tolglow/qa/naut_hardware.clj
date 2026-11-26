(ns tolglow.qa.naut-hardware
  "Push 3 and X-Touch/XT qualification kit for the Naut Mac. Every check runs
  against an explicitly selected real port or a scripted virtual peer through the
  same transport boundary, so the receipts and assertions are identical. Nothing
  here discovers devices on require, arms lighting output or talks to Live. See
  docs/qualification/naut-hardware.md for the operator procedure."
  (:require [afterglow.control.codecs.x-touch :as xt-codec]
            [afterglow.control.devices.push-3.display :as push3-display]
            [afterglow.control.devices.push-3.qualification :as push3-qualification]
            [afterglow.control.devices.push-3.usb :as push3-usb]
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
   :push-3-live {:name "CoreMIDI4J - Ableton Push 3 Live Port"}
   :x-touch {:name "CoreMIDI4J - X-Touch INT"}
   :x-touch-ext {:name "CoreMIDI4J - X-Touch-Ext"}})

(defn ports
  "Passive JVM MIDI listing. No device is opened. Afterglow's shared scan gives
  up after 1.5 s with a retryable :midi/discovery-pending error; the first
  CoreMIDI4J scan in a fresh JVM can take longer, so join it for up to
  wait-ms (default 20 s) instead of failing the check."
  ([] (ports 20000))
  ([wait-ms]
   (let [deadline (+ (System/currentTimeMillis) wait-ms)]
     (loop []
       (let [result (try (transports/midi-ports)
                         (catch clojure.lang.ExceptionInfo error
                           (if (and (= :midi/discovery-pending (:reason (ex-data error)))
                                    (< (System/currentTimeMillis) deadline))
                             ::pending
                             (throw error))))]
         (if (= ::pending result) (do (Thread/sleep 250) (recur)) result))))))

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
  ([role] (midi-transport role (ports)))
  ([role listing]
   (let [in (port role :input listing) out (port role :output listing)]
     (transports/midi-transport {:input-index (:index in) :output-index (:index out)
                                 :expected-input-name (:name in) :expected-output-name (:name out)}))))

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

(defn- ascii [bytes] (apply str (map #(if (<= 32 % 126) (char %) \?) bytes)))

(defn describe-identity
  "Universal Identity Reply fields. A leading zero manufacturer byte means a
  three-byte manufacturer ID (Ableton is 00 21 1D, Behringer 00 20 32).
  Ableton replies follow the Push 2 interface document's layout (family,
  member, major, minor, 14-bit build, four serial bytes, then board data);
  Push 3 appends more bytes after the serial than Push 2 does, which are
  recorded raw as :trailing."
  [message]
  (let [body (subvec message 5 (dec (count message)))
        [manufacturer more] (if (zero? (first body)) [(subvec body 0 3) (subvec body 3)] [(subvec body 0 1) (subvec body 1)])
        generic {:device-id (nth message 2) :manufacturer (hex manufacturer)
                 :family (hex (take 2 more)) :model (hex (take 2 (drop 2 more))) :version (hex (drop 4 more))}]
    (if (and (= [0 0x21 0x1d] manufacturer) (<= 12 (count more)))
      (let [[major minor build-lsb build-msb] (subvec more 4 8)]
        (assoc generic :ableton {:software (str major "." minor) :build (+ build-lsb (* 128 build-msb))
                                 :serial-bytes 4 :trailing (hex (subvec more 12))}))
      generic)))

(defn mcu-version-request [device] [0xf0 0x00 0x00 0x66 device 0x13 0x00 0xf7])

(defn mcu-version-reply?
  "The reply the operator's Live script also parses: F0 00 00 66 dd 14 v.. F7."
  [device]
  (fn [message]
    (and (vector? message) (<= 8 (count message)) (= [0xf0 0 0 0x66 device 0x14] (subvec message 0 6))
         (= 0xf7 (peek message)))))

(defn describe-mcu-version [message]
  (let [payload (subvec message 6 (dec (count message)))]
    {:bytes (hex payload) :ascii (ascii payload)}))

(defn mcu-device-query [device] [0xf0 0x00 0x00 0x66 device 0x00 0xf7])

(defn mcu-host-connection-query?
  "MCU answers a device query with a host connection query: F0 00 00 66 dd 01,
  seven serial bytes and four challenge bytes. The kit never completes the
  handshake (no 02 reply); the firmware probe still answers afterwards."
  [device]
  (fn [message]
    (and (vector? message) (<= 8 (count message)) (= [0xf0 0 0 0x66 device 0x01] (subvec message 0 6))
         (= 0xf7 (peek message)))))

(defn describe-mcu-device [message]
  (let [payload (subvec message 6 (dec (count message)))]
    {:payload-bytes (count payload) :payload-ascii (ascii payload) :serial-ascii (ascii (take 7 payload))
     :challenge (hex (drop 7 payload))}))

(def probes
  {:push-3/identity {:role :push-3 :issues [219] :request identity-request
                     :reply? identity-reply? :describe describe-identity}
   :push-3/identity-live-port {:role :push-3-live :issues [219] :request identity-request
                               :reply? identity-reply? :describe describe-identity}
   :x-touch/identity {:role :x-touch :issues [310] :request identity-request
                      :reply? identity-reply? :describe describe-identity
                      :note "MCU-mode X-Touch firmware 1.22 has not answered Universal Identity on hardware; use x-touch/device-query."}
   :x-touch-ext/identity {:role :x-touch-ext :issues [310] :request identity-request
                          :reply? identity-reply? :describe describe-identity
                          :note "MCU-mode XT firmware 1.22 has not answered Universal Identity on hardware; use x-touch-ext/device-query."}
   :x-touch/firmware {:role :x-touch :issues [310 553] :request (mcu-version-request 0x14)
                      :reply? (mcu-version-reply? 0x14) :describe describe-mcu-version}
   :x-touch-ext/firmware {:role :x-touch-ext :issues [310 553] :request (mcu-version-request 0x15)
                          :reply? (mcu-version-reply? 0x15) :describe describe-mcu-version}
   :x-touch/device-query {:role :x-touch :issues [310 553] :request (mcu-device-query 0x14)
                          :reply? (mcu-host-connection-query? 0x14) :describe describe-mcu-device}
   :x-touch-ext/device-query {:role :x-touch-ext :issues [310 553] :request (mcu-device-query 0x15)
                              :reply? (mcu-host-connection-query? 0x15) :describe describe-mcu-device}})

(defn- status-name [bytes]
  (let [status (first bytes)]
    (cond (nil? status) "empty"
          (< status 0xf0) (format "%02X" (bit-and status 0xf0))
          :else (format "%02X" status))))

(defn input-summary
  "Counts by status byte (channel stripped) and up to sample distinct messages,
  each with its first arrival time. Active Sensing (FE) is counted, not sampled."
  ([entries] (input-summary entries 8))
  ([entries sample]
   {:count (count entries)
    :by-status (into (sorted-map) (frequencies (map (comp status-name :bytes) entries)))
    :sample (->> entries
                 (remove #(= [0xfe] (:bytes %)))
                 (reduce (fn [seen {:keys [bytes ms]}] (if (some #(= bytes (:bytes %)) seen) seen (conj seen {:bytes bytes :ms ms}))) [])
                 (take sample)
                 (mapv (fn [{:keys [bytes ms]}] {:ms (Math/round (double ms)) :hex (hex (take 48 bytes))
                                                 :length (count bytes)})))}))

(defn- error-data [^Throwable error]
  (cond-> {:class (.getName (class error)) :message (.getMessage error)}
    (ex-data error) (assoc :data (pr-str (ex-data error)))
    (.getCause error) (assoc :cause (str (.getCause error)))))

(defn probe!
  "Send one query and wait for the device's reply. Other input is summarised
  (counts and a sample) but not interpreted. Returns a receipt and never
  throws: a silent device is :no-reply, a transport failure is :error."
  [check {:keys [transport timeout-ms port] :or {timeout-ms 1500}}]
  (let [{:keys [request reply? describe issues role note]} (or (get probes check)
                                                                (throw (ex-info "Unknown probe" {:check check})))
        queue (LinkedBlockingQueue.)
        start (System/nanoTime)
        elapsed #(/ (- (System/nanoTime) start) 1e6)
        base (cond-> {:check check :issues issues :role role :port port :request (hex request)
                      :at (str (Instant/now))}
               note (assoc :note note))
        other (fn [entries] (assoc (input-summary entries) :note "input other than the reply, in arrival order"))]
    (try
      (let [handle ((:open! transport) #(.put queue {:ms (elapsed) :bytes (vec %)}))]
        (try
          (let [sent-ms (elapsed)]
            ((:send! transport) handle [request])
            (loop [ignored []]
              (let [remaining (long (- timeout-ms (- (elapsed) sent-ms)))
                    {message :bytes ms :ms :as entry} (when (pos? remaining) (.poll queue remaining TimeUnit/MILLISECONDS))]
                (cond
                  (nil? entry) (assoc base :verdict :no-reply :ignored (count ignored) :other-input (other ignored)
                                      :timeout-ms timeout-ms)
                  (reply? message) (assoc base :verdict :answered :reply (hex message) :decoded (describe message)
                                          :elapsed-ms (- ms sent-ms) :ignored (count ignored)
                                          :other-input (other ignored))
                  :else (recur (conj ignored entry))))))
          (finally ((:close! transport) handle))))
      (catch Throwable error
        (assoc base :verdict :error :error (error-data error))))))

;; ---------------------------------------------------------------------------
;; Push 3 MIDI mode and aftertouch mode. With Live quit, Push's own Setup page
;; is unavailable, so the kit sets both by SysEx and reads the aftertouch mode
;; back. Command 0A (MIDI mode: 0 Live, 1 User) and 1E/1F (set/get aftertouch
;; mode) follow Ableton's Push 2 interface document; on this Push 3 (firmware
;; 1.0 build 91) 1F answered 02 while Live's preference was MPE, 1E 01 then
;; read back 01 (Poly), and 0A was echoed on both the Live and User ports.

(defn- with-session
  "Open transport, record every input message with its time, run f with a
  send! function and an elapsed-ms clock, and always close."
  [transport f]
  (let [received (atom []) start (System/nanoTime)
        elapsed #(/ (- (System/nanoTime) start) 1e6)
        handle ((:open! transport) #(swap! received conj {:ms (elapsed) :bytes (vec %)}))]
    (try (f #((:send! transport) handle %) elapsed received)
         (finally ((:close! transport) handle)))))

(defn- ableton-sysex [& payload] (vec (concat [0xf0 0x00 0x21 0x1d 0x01 0x01] payload [0xf7])))

(def push-3-modes
  {:user {:send [(ableton-sysex 0x0a 0x01) (ableton-sysex 0x1e 0x01)] :aftertouch 1
          :about "User Mode with Poly Aftertouch, as the kit's layout capture and pad scenario need"}
   :live {:send [(ableton-sysex 0x1e 0x02) (ableton-sysex 0x0a 0x00)] :aftertouch 2
          :about "Live Mode with MPE aftertouch (the operator's Live preference), restored after the session"}})

(defn push-3-mode!
  "Switch the Push 3 to mode (:user or :live), then query the aftertouch
  mode. Every message received within settle-ms of each send is recorded."
  [mode {:keys [transport settle-ms port] :or {settle-ms 500}}]
  (let [{:keys [send aftertouch about]} (or (get push-3-modes mode) (throw (ex-info "Use :user or :live" {:mode mode})))
        query (ableton-sysex 0x1f)]
    (try
      (with-session transport
        (fn [send! elapsed received]
          (let [exchange (fn [message]
                           (reset! received [])
                           (send! [message])
                           (Thread/sleep (long settle-ms))
                           {:sent (hex message)
                            :received (mapv #(hex (:bytes %)) (remove #(= [0xfe] (:bytes %)) @received))})
                steps (mapv exchange send)
                readback (exchange query)
                reported (some #(let [b (:bytes %)] (when (= (subvec (ableton-sysex 0x1f 0) 0 7) (vec (take 7 b))) (nth b 7)))
                               @received)]
            {:check :push-3/mode :issues [219] :mode mode :about about :port port :at (str (Instant/now))
             :steps steps :aftertouch-query readback :aftertouch-mode reported
             :verdict (if (= aftertouch reported) :passed :failed)})))
      (catch Throwable error
        {:check :push-3/mode :mode mode :port port :at (str (Instant/now)) :verdict :error :error (error-data error)}))))

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

(defn- distribution [values]
  (when (seq values)
    (let [sorted (vec (sort values)) n (count sorted)
          at #(nth sorted (min (dec n) (int (Math/floor (* % n)))))]
      {:n n :min (first sorted) :median (at 0.5) :p95 (at 0.95) :max (peek sorted)
       :mean (/ (reduce + sorted) n)})))

(defn- strip-count [extender?] (if extender? 8 9))

(defn rest-state
  "Messages that leave a unit at rest: every fader (main unit includes the
  master, strip 8) at zero, both LCD rows blank, white backlights, rings off
  and the per-strip REC/SOLO/MUTE/SELECT LEDs off."
  [extender?]
  (vec (concat (for [strip (range (strip-count extender?))] (xt-codec/fader strip 0))
               [(xt-codec/text extender? 0 0 (apply str (repeat 56 \space)))
                (xt-codec/text extender? 1 0 (apply str (repeat 56 \space)))
                (xt-codec/colors extender? (vec (repeat 8 7)))]
               (for [strip (range 8)] (xt-codec/encoder-ring strip 0))
               (for [base [0 8 16 24] strip (range 8)] (xt-codec/button-led (+ base strip) :off)))))

(defn- fader-input [entries]
  (vec (keep #(let [decoded (try (xt-codec/decode (:bytes %)) (catch Exception _ nil))]
                (when (= :fader (:type decoded))
                  {:ms (Math/round (double (:ms %))) :strip (:strip decoded) :raw (:raw-value decoded)}))
             entries)))

(defn motor-check!
  "Drive every motor fader through positions (check 4's 75% first), settling
  settle-ms after each, then return to zero. Any position messages the unit
  sends back are recorded and decoded. MCU-mode units report fader position
  only while a fader is touched, so silence here is :no-position-readback and
  the travel itself needs the operator's eyes (:confirm!). Probes firmware
  afterwards to show the unit still answers."
  [{:keys [transport extender? positions settle-ms operator port]
    :or {positions [0.75 0.25 0.5 1.0 0.0] settle-ms 1500}}]
  (let [extender? (boolean extender?)
        strips (range (strip-count extender?))
        steps (with-session transport
                (fn [send! elapsed received]
                  (send! (rest-state extender?))
                  (Thread/sleep (long settle-ms))
                  (reset! received [])
                  (let [steps (vec (for [position (concat positions (when-not (= 0.0 (double (last positions))) [0.0]))]
                                     (let [messages (mapv #(xt-codec/fader % position) strips)
                                           sent-ms (elapsed)]
                                       (send! messages)
                                       (Thread/sleep (long settle-ms))
                                       (let [input @received]
                                         (reset! received [])
                                         {:position position :sent-ms (Math/round (double sent-ms))
                                          :sent (hex (first messages)) :strips (count messages)
                                          :raw-target (let [[_ lsb msb] (first messages)] (+ lsb (* 128 msb)))
                                          :readback (fader-input input)
                                          :other-input (input-summary input)}))))]
                    (send! (rest-state extender?))
                    steps)))
        after (probe! (if extender? :x-touch-ext/firmware :x-touch/firmware) {:transport transport})
        readback? (some (comp seq :readback) steps)
        answer (when operator
                 ((:confirm! operator) (str "Did every fader travel to 3/4, 1/4, 1/2, the top and back to the bottom, "
                                            "stopping at each position (the main unit includes the master fader)?")))]
    {:check (if extender? :x-touch-ext/motor :x-touch/motor) :issues [553 310] :port port :at (str (Instant/now))
     :settle-ms settle-ms :steps steps :after after :answer answer
     :verdict (cond (not= :answered (:verdict after)) :unresponsive
                    readback? :readback
                    :else :no-position-readback)}))

(def ^:private palette-labels ["off" "red" "green" "yellow" "blue" "magenta" "cyan" "white"])

(defn lcd-check!
  "Write a known pattern to both LCD rows, step all strips through each native
  backlight colour, then set one colour per strip, light each strip's
  REC/SOLO/MUTE/SELECT LEDs and sweep the encoder rings. Ends at rest-state
  and probes firmware. Everything it sent is in the receipt; what it looked
  like needs the operator (:confirm!)."
  [{:keys [transport extender? hold-ms operator port] :or {hold-ms 400}}]
  (let [extender? (boolean extender?)
        unit (if extender? "XT" "XTouch")
        sent (atom [])
        timing (with-session transport
                 (fn [send! elapsed _]
                   (let [send! (fn [messages] (swap! sent into messages) (send! messages))
                         t0 (elapsed)]
                     (send! [(xt-codec/text extender? 0 0 (apply str (map #(format "%-7s" (str unit (inc %))) (range 8))))
                             (xt-codec/text extender? 1 0 (apply str (map #(format "%-7s" (nth palette-labels (inc (mod % 7)))) (range 8))))])
                     (doseq [colour (range 8)]
                       (send! [(xt-codec/colors extender? (vec (repeat 8 colour)))])
                       (Thread/sleep (long hold-ms)))
                     (send! [(xt-codec/colors extender? (mapv #(inc (mod % 7)) (range 8)))])
                     (doseq [base [0 8 16 24]]
                       (send! (mapv #(xt-codec/button-led (+ base %) :on) (range 8)))
                       (Thread/sleep (long hold-ms))
                       (send! (mapv #(xt-codec/button-led (+ base %) :off) (range 8))))
                     (doseq [position (range 12)]
                       (send! (mapv #(xt-codec/encoder-ring % position) (range 8)))
                       (Thread/sleep 60))
                     (Thread/sleep (long (* 3 hold-ms)))
                     (send! (rest-state extender?))
                     {:pattern-ms (Math/round (double (- (elapsed) t0)))})))
        after (probe! (if extender? :x-touch-ext/firmware :x-touch/firmware) {:transport transport})
        answer (when operator
                 ((:confirm! operator) (str "Did the top row read " unit "1.." unit "8, the bottom row the colour names, "
                                            "did the backlights step off, red, green, yellow, blue, magenta, cyan, white "
                                            "and then match the names, did the REC, SOLO, MUTE and SELECT rows light in turn, "
                                            "and did the rings sweep?")))]
    (merge timing
           {:check (if extender? :x-touch-ext/lcd :x-touch/lcd) :issues [310 553] :port port :at (str (Instant/now))
            :messages (count @sent) :sysex (mapv hex (filter #(= 0xf0 (first %)) @sent))
            :after after :answer answer
            :verdict (if (= :answered (:verdict after)) :sent :unresponsive)})))

(defn rate-check!
  "Send scribble-strip text at each rate in rates-hz for seconds each, then a
  bounded native-color alternation at alternation-ms. Probes firmware before,
  after each stage and at the end; a firmware reply that slows down or stops
  after a burst is the measurable sign of a backlog. Ends at rest-state.
  Returns achieved message rates and probe latencies."
  [{:keys [transport extender? rates-hz seconds alternation-ms operator port]
    :or {rates-hz [10 25 50] seconds 5 alternation-ms 20}}]
  (let [extender? (boolean extender?)
        device (xt-codec/device-id extender?)
        probe-id (if extender? :x-touch-ext/firmware :x-touch/firmware)
        latency #(select-keys % [:verdict :elapsed-ms])
        before (probe! probe-id {:transport transport})
        ;; Sends are scheduled on fixed deadlines, so a send that blocks shows up
        ;; as a lower achieved rate and a longer :send-ms rather than as drift.
        run (fn [period-ms total-ms message-fn]
              (with-session transport
                (fn [send! _ _]
                  (let [start (System/nanoTime) n (volatile! 0) durations (transient [])
                        ms-since #(/ (- (System/nanoTime) %) 1e6)]
                    (while (< (ms-since start) total-ms)
                      (let [t0 (System/nanoTime)]
                        (send! [(message-fn @n)])
                        (conj! durations (ms-since t0)))
                      (vswap! n inc)
                      (let [wait (- (* @n period-ms) (ms-since start))]
                        (when (pos? wait) (Thread/sleep (long wait)))))
                    {:messages @n :achieved-hz (/ @n (/ (- (System/nanoTime) start) 1e9))
                     :send-ms (distribution (persistent! durations))}))))
        texts (vec (for [hz rates-hz]
                     (assoc (run (/ 1000.0 hz) (* 1000 seconds)
                                 (fn [n] (xt-codec/text extender? 0 0 (format "%-56s" (str "Tolglow rate " hz " Hz #" n)))))
                            :requested-hz hz
                            :probe-after (latency (probe! probe-id {:transport transport})))))
        alternation (assoc (run alternation-ms (* 1000 seconds)
                                (fn [n] (xt-codec/colors extender? (vec (repeat 8 (if (even? n) 1 4))))))
                           :interval-ms alternation-ms)
        _ (with-session transport (fn [send! _ _] (send! [(xt-codec/colors extender? (vec (repeat 8 7)))])))
        after (probe! probe-id {:transport transport})
        answer (when operator
                 ((:confirm! operator) "During the red/blue alternation, did strips look a stable magenta/purple without visible flicker?"))]
    (with-session transport (fn [send! _ _] (send! (rest-state extender?))))
    {:check (if extender? :x-touch-ext/rate :x-touch/rate) :issues [310] :device device :port port
     :at (str (Instant/now)) :before before :after after :text texts
     :alternation (assoc alternation :probe-after (latency after))
     :answer answer
     :verdict (if (and (= :answered (:verdict before)) (= :answered (:verdict after))
                       (every? #(= :answered (get-in % [:probe-after :verdict])) texts))
                :responsive :unresponsive)}))

(defn rest!
  "Leave one unit at rest (see rest-state) and return the messages sent."
  [transport extender?]
  (let [messages (rest-state (boolean extender?))]
    (with-session transport (fn [send! _ _] (send! messages)))
    {:check (if extender? :x-touch-ext/rest :x-touch/rest) :at (str (Instant/now)) :messages (count messages)}))

;; ---------------------------------------------------------------------------
;; Tethered Push 3 display chart with a receipt. Afterglow's own show-chart
;; prints nothing on success; this wraps the same sink and USB backend and
;; counts every bulk transfer, its duration and any libusb failure.

(defn instrumented-backend
  "Wrap a display backend ({:open! :write! :close!}) so each transfer is
  timed and counted. Returns {:backend .. :stats atom}."
  [backend]
  (let [stats (atom {:opens 0 :closes 0 :header-transfers 0 :payload-transfers 0 :bytes 0
                     :header-ms [] :payload-ms [] :errors [] :descriptor nil})]
    {:stats stats
     :backend {:open! (fn [options]
                        (let [wire ((:open! backend) options)]
                          (swap! stats #(-> % (update :opens inc) (assoc :descriptor (:descriptor wire))))
                          wire))
               :write! (fn [wire ^bytes data]
                         (let [t0 (System/nanoTime)]
                           (try ((:write! backend) wire data)
                                (swap! stats #(-> %
                                                  (update (if (= 16 (alength data)) :header-transfers :payload-transfers) inc)
                                                  (update :bytes + (alength data))
                                                  (update (if (= 16 (alength data)) :header-ms :payload-ms)
                                                          conj (/ (- (System/nanoTime) t0) 1e6))))
                                (catch Throwable error
                                  (swap! stats update :errors conj (assoc (error-data error) :bytes (alength data)))
                                  (throw error)))))
               :close! (fn [wire] (try ((:close! backend) wire) (finally (swap! stats update :closes inc))))}}))

(defn display-chart!
  "Claim the tethered Push 3 display (bus/address from push3-display.sh
  status), show Afterglow's qualification chart for seconds, release it and
  return a receipt: first-frame completion time, transfer counts and bytes,
  per-transfer durations, refresh rate achieved and any failure. :backend
  replaces the libusb backend (replay tests)."
  [{:keys [bus address seconds backend library] :or {seconds 12}}]
  (let [{stats :stats instrumented :backend} (instrumented-backend
                                               (or backend {:open! push3-usb/open! :write! push3-usb/write!
                                                            :close! push3-usb/close!}))
        sink (push3-display/sink (cond-> {:bus bus :address address :backend instrumented}
                                   library (assoc :library library)))
        start (System/nanoTime)
        elapsed #(/ (- (System/nanoTime) start) 1e6)
        first-frame (promise)
        base {:check :push-3/display-chart :issues [219] :bus bus :address address :seconds seconds
              :at (str (Instant/now)) :chart "afterglow.control.devices.push-3.qualification/chart"}
        outcome (try
                  (let [image (push3-qualification/chart)
                        chart-ms (elapsed)
                        handle ((:open! sink))
                        open-ms (elapsed)]
                    (try
                      ((:render-with-receipt! sink) handle image #(deliver first-frame (elapsed)))
                      (let [shown (elapsed)]
                        ;; seconds counts from the first presented frame, not from open.
                        (loop []
                          (when (and (< (- (elapsed) shown) (* 1000 seconds)) (nil? (:failure @(:state handle))))
                            (Thread/sleep 100) (recur)))
                        {:failure (some-> (:failure @(:state handle)) error-data)
                         :stages-ms {:chart chart-ms :open-and-claim (- open-ms chart-ms) :first-render (- shown open-ms)}
                         :shown-ms (- (elapsed) shown)
                         :streamed-ms (elapsed)})
                      (finally ((:close! sink) handle))))
                  (catch Throwable error {:failure (error-data error) :streamed-ms (elapsed)}))
        {:keys [payload-transfers header-transfers header-ms payload-ms errors descriptor] :as s} @stats]
    (merge base
           {:streamed-ms (:streamed-ms outcome)
            :stages-ms (:stages-ms outcome)
            :shown-ms (:shown-ms outcome)
            :first-frame-ms (deref first-frame 0 nil)
            :frames payload-transfers
            :transfers {:header header-transfers :payload payload-transfers :bytes (:bytes s)
                        :opens (:opens s) :closes (:closes s)}
            :refresh-hz (when (and (pos? payload-transfers) (:shown-ms outcome))
                          (/ payload-transfers (/ (:shown-ms outcome) 1000.0)))
            :header-transfer-ms (distribution header-ms)
            :payload-transfer-ms (distribution payload-ms)
            :errors errors
            :failure (:failure outcome)
            :descriptor descriptor
            :verdict (cond (:failure outcome) :failed
                           (and (pos? payload-transfers) (= payload-transfers header-transfers)) :presented
                           :else :no-frames)})))

;; ---------------------------------------------------------------------------
;; Command line

(defn- read-layout [path] (:layout (edn/read-string (slurp path))))

(defn- summary-line [{:keys [check verdict reply elapsed-ms error decoded]}]
  (str (format "%-28s %-14s" check verdict)
       (when elapsed-ms (format " %6.1f ms" (double elapsed-ms)))
       (when reply (str "  " reply))
       (when-let [text (or (:ascii decoded) (:payload-ascii decoded))] (str "  \"" text "\""))
       (when error (str "  " (:message error)))))

(def ^:private ok-verdicts
  #{:answered :no-reply :passed :responsive :presented :sent :readback :no-position-readback})

(defn- role-port [role direction]
  (try (:name (port role direction)) (catch Exception error (str "unresolved: " (.getMessage error)))))

(defn- unit-role [target]
  (let [role (keyword target)]
    (when-not (#{:x-touch :x-touch-ext} role) (throw (ex-info "Use x-touch or x-touch-ext" {:target target})))
    role))

(defn- report! [receipt summary-keys]
  (pprint/pprint (if summary-keys (select-keys receipt summary-keys) receipt))
  (println "receipt:" (write-receipt! receipt))
  receipt)

(defn -main
  "lein run -m tolglow.qa.naut-hardware ports
   ... probe all | probe CHECK   (push-3/identity, push-3/identity-live-port, x-touch/firmware,
                                  x-touch/device-query, x-touch/identity and the x-touch-ext ones)
   ... capture-layout [pad-rows]
   ... scenario push-3/pads LAYOUT.edn | scenario x-touch/strips | scenario x-touch-ext/strips
   ... motor x-touch|x-touch-ext [--unattended]
   ... lcd x-touch|x-touch-ext [--unattended]
   ... rate x-touch|x-touch-ext [--unattended]
   ... rest x-touch|x-touch-ext
   ... push-3-mode user|live    (User Mode + Poly Aftertouch, or back to Live Mode + MPE)
   ... display-chart BUS ADDRESS [SECONDS]
  Every check prints its receipt path. The exit status is 0 when each receipt
  has an expected verdict (an honest :no-reply included) and 1 otherwise."
  [& [command target extra & more]]
  (let [unattended? (some #{"--unattended"} (concat [target extra] more))
        operator (cond-> (console-operator) unattended? (assoc :confirm! (constantly nil)))
        confirming (when-not unattended? operator)
        receipts
        (case command
          "ports" (do (pprint/pprint (filterv #(some (fn [role] (= (:name role) (:name %))) (vals roles)) (ports))) [])
          "probe" (let [checks (if (= "all" target) (sort (keys probes)) [(keyword target)])
                        listing (try (ports) (catch Exception _ nil))
                        receipts (vec (for [check checks]
                                        (let [role (:role (get probes check))
                                              receipt (try (probe! check {:transport (midi-transport role (or listing (ports)))
                                                                          :port (:name (port role :output (or listing (ports))))})
                                                           (catch Throwable error
                                                             {:check check :role role :at (str (Instant/now))
                                                              :verdict :error :error (error-data error)}))]
                                          (assoc receipt :path (write-receipt! receipt)))))]
                    (doseq [receipt receipts] (println (summary-line receipt)) (println "  receipt:" (:path receipt)))
                    receipts)
          "capture-layout" (let [plan (push-3-capture-plan (if target {:pad-rows (parse-long target)} {}))
                                 result (capture-layout! {:transport (input-transport :push-3) :plan plan
                                                          :operator {:prompt! (:capture-prompt! operator)}})]
                             [(report! (assoc result :check :push-3/layout :issues [219] :at (str (Instant/now))
                                              :port (role-port :push-3 :input) :verdict (if (empty? (:skipped result)) :passed :partial))
                                       [:skipped])])
          "scenario" (let [id (keyword target)
                           device (:device (get scenarios id))
                           transport (if (= :push-3 device) (input-transport :push-3) (midi-transport device))]
                       [(report! (run-scenario! id {:transport transport :operator operator
                                                    :layout (when (and extra (not= "--unattended" extra)) (read-layout extra))
                                                    :port (role-port device :input)})
                                 [:verdict :steps :session])])
          "motor" (let [role (unit-role target)]
                    [(report! (motor-check! {:transport (midi-transport role) :extender? (= :x-touch-ext role)
                                             :operator confirming :port (role-port role :output)})
                              [:check :verdict :steps :after :answer])])
          "lcd" (let [role (unit-role target)]
                  [(report! (lcd-check! {:transport (midi-transport role) :extender? (= :x-touch-ext role)
                                         :operator confirming :port (role-port role :output)})
                            [:check :verdict :messages :pattern-ms :after :answer])])
          "rate" (let [role (unit-role target)]
                   [(report! (rate-check! {:transport (midi-transport role) :extender? (= :x-touch-ext role)
                                           :operator confirming :port (role-port role :output)})
                             [:check :verdict :text :alternation :before :after :answer])])
          "push-3-mode" (let [mode (keyword target)]
                          [(report! (push-3-mode! mode {:transport (midi-transport :push-3) :port (role-port :push-3 :output)})
                                    nil)])
          "rest" (let [role (unit-role target)]
                   [(report! (assoc (rest! (midi-transport role) (= :x-touch-ext role)) :verdict :sent) nil)])
          "display-chart" (let [receipt (display-chart! {:bus (some-> target parse-long) :address (some-> extra parse-long)
                                                         :seconds (or (some-> (first more) parse-long) 12)})]
                            [(report! receipt [:check :verdict :stages-ms :first-frame-ms :frames :refresh-hz :transfers
                                               :payload-transfer-ms :errors :failure])])
          (do (println (:doc (meta #'-main))) []))]
    (shutdown-agents)
    (System/exit (if (every? #(ok-verdicts (:verdict %)) receipts) 0 1))))
