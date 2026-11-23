(ns tolglow.studio.observations
  "Publish completed preview samples only while a show coeffect has readers."
  (:require [re-frame.core :as rf]
            [tolglow.studio.state-socket :as socket]))

(rf/reg-sub ::connection
  (fn [db _] (when-not (:awaiting-full? db) (:stream-id db))))

(defn grant [owner]
  (when-let [stream @(rf/subscribe [::connection])]
    (let [query [:studio/observation-demand owner]
          sources @(rf/subscribe [:studio/remote query])]
      (when (seq sources) {:stream stream :query query :sources sources}))))

(defn- bitmap-rgb [bitmap]
  (try
    (let [pixel (.createElement js/document "canvas")]
      (set! (.-width pixel) 1)
      (set! (.-height pixel) 1)
      (let [context (.getContext pixel "2d" #js {:willReadFrequently true})]
        (.drawImage context bitmap 0 0)
        (let [rgba (.-data (.getImageData context 0 0 1 1))]
          [(aget rgba 0) (aget rgba 1) (aget rgba 2)])))
    (finally (.close bitmap))))

(defn- center-rgb [canvas signal]
  ;; createImageBitmap can finish after disposal. In that case release the
  ;; bitmap without drawing it or reading pixels back into JS.
  (let [x (quot (.-width canvas) 2) y (quot (.-height canvas) 2)]
    (.then (js/createImageBitmap canvas x y 1 1)
           (fn [bitmap] (if (.-aborted signal) (.close bitmap) (bitmap-rgb bitmap))))))

(defonce samplers (atom {}))

(defn register-sampler!
  "Register a reader of the presented canvas. With {:abortable? true}, the
  reader also receives an AbortSignal for its active sample."
  ([id read!] (register-sampler! id {} read!))
  ([id {:keys [abortable?]} read!]
   (assert (and (keyword? id) (fn? read!)))
   (swap! samplers assoc id {:read! read! :abortable? (boolean abortable?)})
   id))

(register-sampler! :center-rgb {:abortable? true} center-rgb)

(defn controller
  ([] (controller #(js/performance.now) #(socket/reply! [%1 %2])))
  ([now send!] {:slots (atom {}) :now now :send! send! :visible? (constantly true)}))

(defn- source-slot [previous binding reader]
  (let [reader (if (fn? reader) {:read! reader} reader)
        same-source? (= binding (:binding previous))]
    (if (and same-source? (= reader (:reader previous)))
      previous
      (merge (when same-source? (select-keys previous [:value]))
             {:reader reader :binding binding :ticket (js-obj)
              :last-at js/Number.NEGATIVE_INFINITY :sequence (if same-source? (:sequence previous) 0)}))))

(defn- reconcile! [slots {:keys [sources stream query]} readers]
  (let [previous @slots
        current (into {} (for [[id spec] sources]
                           [id (source-slot (get previous id) [stream query spec] (get readers (:sampler spec)))]))]
    ;; Revoke before aborting. A subscriber to the signal may run synchronously.
    (reset! slots current)
    (doseq [[id slot] previous
            :when (not (identical? (:ticket slot) (:ticket (get current id))))
            :let [abort (:abort slot)] :when abort]
      (.abort abort))))

(defn configure! [{:keys [slots]} request]
  (remove-watch samplers slots)
  (reconcile! slots request @samplers)
  (when (seq (:sources request))
    (add-watch samplers slots (fn [_ _ _ readers] (reconcile! slots request readers)))))

(defn- finish! [{:keys [slots now send! visible?]} id ticket sampled frame value]
  (let [{:keys [binding sequence pending?] :as current} (get @slots id)
        [stream query {:keys [show token]}] binding]
    (when (and pending? (identical? ticket (:ticket current)))
      (swap! slots update id #(-> % (dissoc :abort) (assoc :pending? false)))
      (when (and (visible?) (some? value) (not= value (:value current)))
        (swap! slots update id assoc :value value)
        (send! stream {:op :observation :query query :source id :show show :token token
                       :sequence sequence :frame-number frame :sampled-ms sampled :completed-ms (now) :value value})))))

(defn- sample! [{:keys [slots] :as control} id slot canvas sampled frame]
  (let [{:keys [read! abortable?]} (:reader slot)
        abort (when abortable? (js/AbortController.))
        complete #(finish! control id (:ticket slot) sampled frame %)]
    (swap! slots update id assoc :sequence (inc (:sequence slot)) :pending? true :last-at sampled :abort abort)
    (try
      (let [result (if abort (read! canvas (.-signal abort)) (read! canvas))]
        (-> (js/Promise.resolve result) (.then complete) (.catch #(complete nil))))
      (catch :default _ (complete nil)))))

(defn- presented-show? [show-id epoch grant]
  (let [expected (get-in grant [:show :show-id])
        wire-id (if (keyword? expected) (subs (str expected) 1) expected)]
    (and (= show-id wire-id) (= epoch (get-in grant [:show :epoch])))))

(defn present! [{:keys [slots now visible?] :as control} canvas show-id epoch frame]
  (when (and (seq @slots) (visible?))
    (let [sampled (now)]
      (doseq [[id {:keys [binding reader last-at pending?] :as slot}] @slots
              :let [spec (last binding)]
              :when (and reader (presented-show? show-id epoch spec) (not pending?)
                         (>= (- sampled last-at) (:interval-ms spec)))]
        (sample! control id slot canvas sampled frame)))))

(defn bind! [component engine request]
  (let [current (aget component "observationControl")]
    (when (or current (seq (:sources request)))
      (let [active? #(let [renderer (or (aget engine "fallback") engine)]
                       (and (not (.-hidden js/document)) (aget renderer "visible")
                            (not (or (aget renderer "disposed") (aget renderer "lost") (aget renderer "paused")))))
            control (or current (assoc (controller) :visible? active?))]
        (configure! control request)
        (aset component "observationControl" (when request control))
        (aset engine "observedFrame" (when (seq (:sources request)) #(present! control %1 %2 %3 %4)))))))
