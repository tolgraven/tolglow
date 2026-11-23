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

(defn- center-rgb [canvas]
  ;; Capture at presentation before the WebGL drawing buffer can clear.
  ;; Only one presented pixel reaches the JS reader.
  (let [x (quot (.-width canvas) 2) y (quot (.-height canvas) 2)]
    (.then (js/createImageBitmap canvas x y 1 1) bitmap-rgb)))

(defonce samplers (atom {:center-rgb center-rgb}))

(defn register-sampler!
  "Register a local value or asynchronous reader of the presented canvas.
  Sampling occurs only for a matching server source and a visible producer."
  [id sample!]
  (assert (and (keyword? id) (fn? sample!)))
  (swap! samplers assoc id sample!)
  id)

(defn controller
  ([] (controller #(js/performance.now) #(socket/reply! [%1 %2])))
  ([now send!] {:slots (atom {}) :now now :send! send! :visible? (constantly true)}))

(defn configure! [{:keys [slots]} request]
  (let [{:keys [sources stream query]} request]
    (swap! slots
           (fn [old]
             (into {}
                   (for [[id spec] sources
                         :let [old-slot (get old id) binding [stream query spec]]]
                     [id (if (= binding (:binding old-slot)) old-slot
                             {:binding binding :ticket (js-obj) :last-at js/Number.NEGATIVE_INFINITY :sequence 0})]))))))

(defn- finish! [{:keys [slots now send! visible?]} id ticket sampled value]
  (let [{:keys [binding sequence pending?] :as current} (get @slots id)
        [stream query {:keys [show token]}] binding]
    (when (and pending? (identical? ticket (:ticket current)))
      (swap! slots update id assoc :pending? false)
      (when (and (visible?) (some? value) (not= value (:value current)))
        (swap! slots update id assoc :value value)
        (send! stream {:op :observation :query query :source id :show show :token token
                       :sequence sequence :sampled-ms sampled :completed-ms (now) :value value})))))

(defn- sample! [{:keys [slots] :as control} id slot read! canvas sampled]
  (let [complete #(finish! control id (:ticket slot) sampled %)]
    (swap! slots update id assoc :sequence (inc (:sequence slot)) :pending? true :last-at sampled)
    (try (-> (js/Promise.resolve (read! canvas)) (.then complete) (.catch #(complete nil)))
         (catch :default _ (complete nil)))))

(defn present! [{:keys [slots now visible?] :as control} canvas]
  (when (and (seq @slots) (visible?))
    (let [sampled (now)]
      (doseq [[id {:keys [binding last-at pending?] :as slot}] @slots
              :let [spec (last binding) read! (get @samplers (:sampler spec))]
              :when (and read! (not pending?) (>= (- sampled last-at) (:interval-ms spec)))]
        (sample! control id slot read! canvas sampled)))))

(defn bind! [component engine request]
  (let [current (aget component "observationControl")]
    (when (or current (seq (:sources request)))
      (let [active? #(let [renderer (or (aget engine "fallback") engine)]
                       (and (not (.-hidden js/document)) (aget renderer "visible")
                            (not (or (aget renderer "disposed") (aget renderer "lost") (aget renderer "paused")))))
            control (or current (assoc (controller) :visible? active?))]
        (configure! control request)
        (aset component "observationControl" (when request control))
        (aset engine "observedFrame" (when (seq (:sources request)) #(present! control %)))))))
