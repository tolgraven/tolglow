(ns tolglow.surface.virtual
  "In-memory surfaces and a replay harness (#706 slice 0). A virtual surface
  holds a descriptor, the normalized events it received and the latest desired
  state per control, so layouts and links can be tested without hardware. The
  replay harness drives a real codec's decode over captured or scripted bytes
  and normalizes the result; nothing here opens a port."
  (:require [tolglow.surface.descriptor :as descriptor]))

(defn create
  "A virtual surface for a descriptor. Its state is owned by this value."
  [descriptor surface-id]
  (when-not (descriptor/valid? descriptor)
    (throw (ex-info "Invalid surface descriptor" {:descriptor (:id descriptor)})))
  {:id surface-id :descriptor descriptor :state (atom {:events [] :outputs {}})})

(defn input!
  "Record one normalized input event. Events for another surface or for a
  control the descriptor does not declare are refused."
  [{:keys [id descriptor state]} event]
  (when-not (and (= id (:surface event)) (descriptor/valid-event? descriptor event))
    (throw (ex-info "Event does not match this surface" {:surface id :event event})))
  (swap! state update :events conj event)
  event)

(defn present!
  "Store desired control states, {:group :at :state}. The latest state per
  control wins, as a device codec's feedback diff would."
  [{:keys [descriptor state]} intents]
  (doseq [{:keys [group at]} intents]
    (when-not (contains? (:groups descriptor) group)
      (throw (ex-info "Unknown control group" {:group group :at at}))))
  (swap! state update :outputs into (map (fn [{:keys [group at] :as intent}] [[group at] (:state intent)])) intents)
  (:outputs @state))

(defn events [surface] (:events @(:state surface)))

(defn outputs [surface] (:outputs @(:state surface)))

(defn replay
  "Decode captured messages with decode (bytes -> codec map or nil), then
  normalize (surface-id, codec map -> event or nil). Returns the events in
  order; undecodable or unmapped messages are dropped, as on the device."
  [decode normalize surface-id messages]
  (into [] (keep #(some->> (decode %) (normalize surface-id))) messages))

(defn replay-into!
  "Replay messages into a virtual surface and return its recorded events.
  Events for controls its descriptor lacks are dropped, as the device would."
  [surface decode normalize messages]
  (doseq [event (replay decode normalize (:id surface) messages)
          :when (descriptor/valid-event? (:descriptor surface) event)]
    (input! surface event))
  (events surface))
