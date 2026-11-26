(ns tolglow.qa.naut-display-features
  "Check 8 of the Naut hardware kit: the display-dependent Push 3 features
  (#547, #529, #552, #530) rendered by Studio's real /push-display page and
  sent to the tethered Push 3 screen. The display is claimed once and every
  state is shown in that one claim. Operator input arrives through a virtual
  Push 3 at the MIDI transport boundary, using the same byte encoding as the
  kit's replays, so no Push 3 MIDI port is opened and no byte is sent to the
  controller. Each state is held for a fixed time so someone can look at it,
  and gets its own receipt entry (frames, transfers, the presented view and
  the PNG that was sent). See docs/qualification/naut-hardware.md, check 8."
  (:require [afterglow.control.devices.push-3.display :as push3-display]
            [afterglow.control.devices.push-3.usb :as push3-usb]
            [afterglow.control.devices.session :as device-session]
            [clojure.java.io :as io]
            [clojure.pprint :as pprint]
            [clojure.string :as str]
            [tolglow.controllers.content-browser-display :as catalog]
            [tolglow.demo :as demo]
            [tolglow.qa.naut-hardware :as kit]
            [tolglow.show :as show]
            [tolglow.studio :as studio]
            [tolglow.studio.display :as display])
  (:import [java.awt.image BufferedImage]
           [java.security MessageDigest]
           [java.time Instant LocalDateTime ZoneId]
           [java.time.format DateTimeFormatter]
           [javax.imageio ImageIO]))

(def synthetic-layout
  "Stand-in User Mode addresses (the same synthetic numbering as
  tolglow.qa.naut-hardware-test). Input is virtual, so these are not a claim
  about real Push 3 numbering; check 2 captures that."
  {:pads (into {} (for [y (range 8) x (range 8)] [[x y] {:channel 0 :note (+ 36 x (* 8 y))}]))
   :encoder-touches (into {:master {:channel 0 :note 8}}
                          (for [i (range 1 9)] [(keyword (str "encoder-" i)) {:channel 0 :note (dec i)}]))
   :encoders (into {:master {:channel 0 :controller 79}}
                   (for [i (range 1 9)] [(keyword (str "encoder-" i)) {:channel 0 :controller (+ 70 i)}]))
   :buttons {:left {:channel 0 :controller 44} :right {:channel 0 :controller 45}
             :up {:channel 0 :controller 46} :down {:channel 0 :controller 47}
             :mute {:channel 0 :controller 60} :select {:channel 0 :controller 48}}
   :touch-strip {:channel 0}})

(def catalog-controls
  "Unused stand-in buttons for the #552 browser."
  {:toggle {:channel 0 :controller 56} :back {:channel 0 :controller 55}
   :next {:channel 0 :controller 63} :previous {:channel 0 :controller 62}
   :next-kind {:channel 0 :controller 57}
   [:select 0] {:channel 0 :controller 20} [:select 1] {:channel 0 :controller 21}})

(defn feature-model
  "The demo show with Ember's period as an exact beat ratio (#529) and a
  colour tint (a compound value for #530), as in variation-control-test."
  []
  (-> (demo/model)
      (assoc-in [:registry :effects :ember :params :period]
                {:type :number :default 1 :bounds [1/4 4] :semantic/type :beat-ratio
                 :ratio-components {:numerator [1 9] :denominator [1 11]}})
      (assoc-in [:registry :effects :ember :params :tint] {:type :color :default [0.9 0.4 0.15]})
      (assoc-in [:registry :effects :ember :param-order] [:period :level :tint])))

(def push-options
  {:session-id "naut-push-3-display" :user-mode? true :expression-mode :poly-aftertouch
   :cue-variation-pads? true :layout synthetic-layout
   :encoder-bindings {:encoder-1 {:param :period} :encoder-2 {:param :level} :encoder-3 {:param :tint}}
   :button-bindings {:up :variation/cycle :left :page-left :right :page-right}})

;; ---------------------------------------------------------------------------
;; One USB claim shared by every display session.

(defn shared-sink
  "Open the real Push 3 sink once. Returns {:sink .. :close! ..}; :sink is a
  sink whose open!/close! reuse that claim, so successive display sessions
  (the Push session, the browser session) never re-run libusb_init. The last
  captured image and a render count are kept for the receipts."
  [real]
  (let [handle ((:open! real)) latest (atom nil) renders (atom 0)
        record! (fn [image] (reset! latest image) (swap! renders inc))]
    {:handle handle :latest latest :renders renders
     :sink {:device :push-3 :input :image
            :open! (constantly handle)
            :render! (fn [_ image] (record! image) ((:render! real) handle image))
            :render-with-receipt! (fn [_ image receipt] (record! image) ((:render-with-receipt! real) handle image receipt))
            :close! (fn [_] nil)}
     :close! #((:close! real) handle)}))

(defn- summarize-view [view]
  (let [overlay (:pad-overlay view) browser (:saved-show-catalog view)]
    (cond-> {:display-mode (:display-mode view)
             :cue (or (get-in view [:cue :name]) (some-> (:selected view) str))
             :touched (vec (distinct (keep :param (cons (:touched-parameter view) (:touched-parameters view)))))
             :parameters (mapv #(or (:name %) (some-> (:param %) name)) (:parameters view))}
      overlay (assoc :overlay (merge (select-keys overlay [:layout :title :current-ratio :ratio-mode])
                                     {:cells (count (:cells overlay)) :rows (count (:rows overlay))}))
      browser (assoc :catalog (cond-> (select-keys browser [:kind :page :page-count :total :next-kind?])
                                (seq (:rows browser)) (assoc :rows (mapv #(cond-> (:name %) (:selected? %) (str " [selected]"))
                                                                         (:rows browser)))
                                (:preview browser) (assoc :preview (select-keys (:preview browser) [:name :version :sources]))))
      (:scene view) (assoc :scene? true))))

(defn- observed-transport
  "Wrap display/transport so every presented view is recorded. Used only
  while a session connects; the created transport keeps the wrapper."
  [original presented]
  (fn [options]
    (original (update options :presented!
                      (fn [inner] (fn [view] (when inner (inner view))
                                    (reset! presented {:at-ns (System/nanoTime) :view view})))))))

(defn- local-time [] (.format (LocalDateTime/now (ZoneId/systemDefault)) (DateTimeFormatter/ofPattern "HH:mm:ss")))

(defn- sha256 [^bytes data]
  (str/join (map #(format "%02x" %) (.digest (MessageDigest/getInstance "SHA-256") data))))

(defn- write-png! [^BufferedImage image file]
  (when image
    (io/make-parents file)
    (ImageIO/write image "png" (io/file file))
    (let [data (java.nio.file.Files/readAllBytes (.toPath (io/file file)))]
      {:png (str file) :png-bytes (alength data) :png-sha256 (sha256 data)})))

;; ---------------------------------------------------------------------------
;; Operator input through the virtual Push 3.

(defn- emit-actions! [{:keys [emit!]} actions]
  (doseq [action actions]
    (if (= :wait (first action))
      (Thread/sleep (long (second action)))
      (doseq [bytes (kit/push-3-bytes synthetic-layout action)] (@emit! bytes) (Thread/sleep 5)))))

(defn- wait-presented
  "Wait for a presented view matching pred, newer than since-ns."
  [{:keys [presented]} since-ns pred timeout-ms]
  (let [deadline (+ (System/currentTimeMillis) timeout-ms)]
    (loop []
      (let [{:keys [at-ns view]} @presented]
        (cond (and view (> at-ns since-ns) (pred view)) view
              (> (System/currentTimeMillis) deadline) nil
              :else (do (Thread/sleep 20) (recur)))))))

(defn- instance [ctx] (get-in @(:state (:runtime ctx)) [:model :instances @(:instance-id ctx)]))

(defn- params [ctx] (:params (instance ctx)))

(defn- last-ack [ctx] (some-> @(:device ctx) :device device-session/snapshot :last-ack (select-keys [:status :code :reason])))

(defn- device-touches [ctx]
  (let [local (some-> @(:device ctx) :device device-session/snapshot :local)]
    (vec (sort (map name (or (get-in local [:controls :touches]) (:touches local)))))))

(defn- note-of [position] (get-in synthetic-layout [:pads position :note]))

;; ---------------------------------------------------------------------------
;; Sessions

(defn- connect-push! [{:keys [runtime presented display-options device]}]
  (let [{:keys [transport] :as peer} (kit/virtual-peer)]
    (with-redefs [display/transport (observed-transport display/transport presented)]
      (reset! device {:session-id (:session-id push-options)
                      :device (show/connect-device! runtime :push-3
                       (assoc push-options :midi transport
                              :display (show/push-browser-display runtime (assoc display-options :device :push-3
                                                                                  :presentation-receipts? true))))}))
    peer))

(defn- connect-catalog! [{:keys [runtime presented display-options device catalog-saves]}]
  (let [{:keys [transport] :as peer} (kit/virtual-peer)]
    (with-redefs [display/transport (observed-transport display/transport presented)]
      (reset! device {:session-id "naut-push-3-browser"
                      :device (catalog/connect-device! runtime :push-3
                       (assoc (select-keys push-options [:user-mode? :expression-mode :layout :encoder-bindings])
                              :session-id "naut-push-3-browser" :midi transport
                              :catalog-controls catalog-controls :catalog-saves catalog-saves
                              :display-options display-options))}))
    peer))

(defn- use-peer! [ctx peer] (reset! (:emit! ctx) (:emit! peer)))

(defn- disconnect! [{:keys [runtime device]}]
  (when-let [{:keys [session-id]} @device]
    (show/disconnect-device! runtime session-id)
    (reset! device nil)))

(defn- button! [ctx id] (emit-actions! ctx [[:button id]]))

(defn- catalog-button! [ctx action]
  (let [{:keys [channel controller]} (get catalog-controls action)]
    (@(:emit! ctx) [(+ 0xb0 channel) controller 127]) (Thread/sleep 5)
    (@(:emit! ctx) [(+ 0xb0 channel) controller 0])))

(defn- settings! [ctx mode]
  (deref (show/submit! (:runtime ctx) :settings {:scope :controllers :values {:push-display {:mode mode}}}) 3000 ::timeout))

(def default-hold-ms 15000)

(defn states
  "The fixed order. Each :enter! performs input and returns a map merged into
  the receipt; :expect selects the presented view that starts the hold."
  [hold-ms]
  (let [short-hold (max 1 (quot (* hold-ms 8) 15))]
    [{:id :parameters :issues [219] :hold-ms hold-ms
      :about "Parameter page for the running Ember cue (no touches, no overlay)"
      :enter! (fn [ctx]
                (let [{id :instance-id} (deref (show/submit! (:runtime ctx) {:op :cue/launch :cue-id :ember}) 3000 nil)]
                  (reset! (:instance-id ctx) id)
                  (use-peer! ctx (connect-push! ctx))
                  {:instance-id (str id)}))
      :expect #(and (= :parameters (:display-mode %)) (nil? (:pad-overlay %)) (seq (:parameters %)))}

     {:id :variation-full :issues [547] :hold-ms hold-ms
      :about "#547 Full cue variations overlay after Up (variation cycle); a pad pressed before the overlay was presented, then the same pad after"
      :enter! (fn [ctx]
                (let [preview (show/cue-variation-preview (:runtime ctx) {:instance-id @(:instance-id ctx) :layout :full})
                      cell (first (filter #(and (= :generated (:kind %)) (note-of (:position %))) (:cells preview)))
                      before (params ctx)]
                  (button! ctx :up)
                  ;; Immediately, before the first overlay capture can reach the screen.
                  (emit-actions! ctx [[:press (:position cell) 100]])
                  (let [early {:position (:position cell) :params-unchanged? (= before (params ctx)) :ack (last-ack ctx)}]
                    (emit-actions! ctx [[:lift (:position cell)]])
                    {:early-press early ::cell cell})))
      :expect #(= :full (get-in % [:pad-overlay :layout]))
      :after! (fn [ctx {cell ::cell}]
                (let [before (params ctx)]
                  (emit-actions! ctx [[:press (:position cell) 100] [:wait 300] [:lift (:position cell)] [:wait 300]])
                  {:presented-press {:position (:position cell) :ack (last-ack ctx)
                                     :changed (into {} (keep (fn [[k v]] (when (not= v (get before k)) [k {:before (get before k) :after v}])))
                                                    (params ctx))}}))}

     {:id :variation-rows :issues [547] :hold-ms short-hold
      :about "#547 Cue parameter rows overlay (Up again)"
      :enter! (fn [ctx] (button! ctx :up) {})
      :expect #(= :rows (get-in % [:pad-overlay :layout]))
      :after! (fn [ctx _] (button! ctx :up)
                {:closed? (some? (wait-presented ctx (System/nanoTime) #(nil? (:pad-overlay %)) 10000))})}

     {:id :ratio-chooser :issues [529] :hold-ms hold-ms
      :about "#529 Beat-ratio pad chooser opened by touching encoder 1 (Ember period)"
      :enter! (fn [ctx]
                (let [before {:period-before (str (:period (params ctx))) :instance (select-keys (instance ctx) [:id :phase])
                              :selected (some-> @(:device ctx) :device device-session/snapshot :local :selected-instance str)}]
                  (emit-actions! ctx [[:touch :encoder-1]])
                  before))
      :expect #(= :beat-ratios (get-in % [:pad-overlay :layout]))
      :after! (fn [ctx _]
                (let [overlay (:pad-overlay (:view @(:presented ctx)))
                      cell (first (filter #(and (not (:selected? %)) (:label %) (note-of (:position %))) (:cells overlay)))
                      before (:period (params ctx))]
                  (emit-actions! ctx [[:press (:position cell) 100] [:wait 300] [:lift (:position cell)] [:wait 200]])
                  {:chosen {:position (:position cell) :label (:label cell) :period-before (str before)
                            :period-after (str (:period (params ctx))) :ack (last-ack ctx)}}))}

     {:id :ratio-chooser-closed :issues [529] :hold-ms short-hold
      :about "#529 Encoder 1 lifted: back to the parameter page"
      :enter! (fn [ctx] (emit-actions! ctx [[:untouch :encoder-1]]) {})
      :expect #(and (nil? (:pad-overlay %)) (= :parameters (:display-mode %)))}

     {:id :browser-shows :issues [552] :hold-ms hold-ms
      :about "#552 Saved-show list (Push session replaced by the content-browser session, toggle pressed)"
      :enter! (fn [ctx]
                (disconnect! ctx)
                (use-peer! ctx (connect-catalog! ctx))
                (Thread/sleep 1500)
                (catalog-button! ctx :toggle) {})
      :expect #(= :show (get-in % [:saved-show-catalog :kind]))}

     {:id :browser-show-preview :issues [552] :hold-ms short-hold
      :about "#552 Preview of the first saved show (row button 1)"
      :enter! (fn [ctx] (catalog-button! ctx [:select 0]) {})
      :expect #(get-in % [:saved-show-catalog :preview])}

     {:id :browser-back :issues [552] :hold-ms short-hold
      :about "#552 Back from the preview: the list again, first row still selected"
      :enter! (fn [ctx] (catalog-button! ctx :back) {})
      :expect #(and (seq (get-in % [:saved-show-catalog :rows])) (nil? (get-in % [:saved-show-catalog :preview])))}

     {:id :browser-effects :issues [552] :hold-ms hold-ms
      :about "#552 Registered effects list (Next library)"
      :enter! (fn [ctx] (catalog-button! ctx :next-kind) {})
      :expect #(= :effect (get-in % [:saved-show-catalog :kind]))
      :after! (fn [ctx _] (catalog-button! ctx :back) {})}

     {:id :room-untouched :issues [530] :hold-ms hold-ms
      :about "#530 Room preview (display mode simulator), no encoder touched"
      :enter! (fn [ctx]
                (disconnect! ctx)
                (settings! ctx :simulator)
                (use-peer! ctx (connect-push! ctx)) {})
      :expect #(and (= :simulator (:display-mode %)) (:scene %)) :timeout-ms 60000}

     {:id :room-one-touch :issues [530] :hold-ms hold-ms
      :about "#530 Room preview with encoder 2 (Ember level) touched"
      :enter! (fn [ctx] (emit-actions! ctx [[:touch :encoder-2]]) {})
      ;; The Room projection carries no touched parameters at this revision
      ;; (#530 open), so the touch is checked on the device session instead.
      :expect #(and (= :simulator (:display-mode %)) (:scene %))
      :after! (fn [ctx _] {:device-touches (device-touches ctx)})}

     {:id :room-two-touch :issues [530] :hold-ms hold-ms
      :about "#530 Room preview with encoders 2 and 3 (level, tint) touched"
      :enter! (fn [ctx] (emit-actions! ctx [[:touch :encoder-3]]) {})
      :expect #(and (= :simulator (:display-mode %)) (:scene %))
      :after! (fn [ctx _]
                (let [touches (device-touches ctx)]
                  (emit-actions! ctx [[:untouch :encoder-3] [:untouch :encoder-2]])
                  (settings! ctx :parameters)
                  {:device-touches touches}))}

     {:id :parameters-one-touch :issues [530] :hold-ms hold-ms
      :about "#530 Parameter page with encoder 2 (level) touched"
      :enter! (fn [ctx] (emit-actions! ctx [[:touch :encoder-2]]) {})
      :expect #(and (= :parameters (:display-mode %)) (= [:level] (vec (distinct (keep :param (:touched-parameters %))))))}

     {:id :parameters-two-touch :issues [530] :hold-ms hold-ms
      :about "#530 Parameter page with encoders 2 and 3 (level, tint) touched"
      :enter! (fn [ctx] (emit-actions! ctx [[:touch :encoder-3]]) {})
      :expect #(and (= :parameters (:display-mode %)) (= 2 (count (distinct (keep :param (:touched-parameters %))))))
      :after! (fn [ctx _] (emit-actions! ctx [[:untouch :encoder-3] [:untouch :encoder-2]]) {})}]))

(defn- transfer-snapshot [stats]
  (select-keys @stats [:header-transfers :payload-transfers :bytes :errors]))

(defn- run-state! [ctx {:keys [id issues about enter! expect after! hold-ms timeout-ms] :or {timeout-ms 20000}} index]
  (let [{:keys [stats shared out-dir]} ctx
        start-ns (System/nanoTime) start-local (local-time) renders-0 @(:renders shared)
        transfers-0 (transfer-snapshot stats)
        entered (try (enter! ctx) (catch Throwable error {:enter-error (str error)}))
        shown (when-not (:enter-error entered) (wait-presented ctx start-ns expect timeout-ms))
        reached-ms (/ (- (System/nanoTime) start-ns) 1e6)
        view-at-hold (:view @(:presented ctx))
        _ (when shown (println (format "  %s %-22s shown after %.0f ms; holding %d s" (local-time) (name id) reached-ms (quot hold-ms 1000))))
        _ (when-not shown (println (format "  %s %-22s NOT reached in %d ms; holding anyway" (local-time) (name id) timeout-ms)))
        _ (Thread/sleep hold-ms)
        image @(:latest shared)
        png (write-png! image (io/file out-dir (format "%02d-%s.png" (inc index) (name id))))
        after (when after! (try (after! ctx entered) (catch Throwable error {:after-error (str error)})))
        transfers-1 (transfer-snapshot stats)]
    (merge {:state id :issues issues :about about
            :local-start start-local :local-end (local-time)
            :verdict (if shown :shown :not-reached)
            :reached-ms reached-ms :held-ms hold-ms
            :view (summarize-view (or shown view-at-hold))
            :captured-frames (- @(:renders shared) renders-0)
            :transfers {:header (- (:header-transfers transfers-1) (:header-transfers transfers-0))
                        :payload (- (:payload-transfers transfers-1) (:payload-transfers transfers-0))
                        :bytes (- (:bytes transfers-1) (:bytes transfers-0))
                        :new-errors (subvec (vec (:errors transfers-1)) (count (:errors transfers-0)))}}
           png
           (dissoc entered ::cell)
           after)))

(defn- save-catalog-shows!
  "Two small saved shows for the #552 list, written under out-dir."
  [out-dir]
  (vec (for [[show-id label] [[:naut-tour "Naut tour"] [:naut-rehearsal "Naut rehearsal"]]]
         (let [file (io/file out-dir (str (name show-id) ".transit"))
               saved (show/create-runtime {:workspace-options nil :workers 1 :model (assoc (demo/model) :show-id show-id)})]
           (io/make-parents file)
           (try (show/save-show! saved file) (finally (show/stop! saved)))
           {:show-id show-id :name label :handle (keyword "naut" (name show-id)) :source-kind :named
            :revision (.lastModified file) :path file}))))

(defn display-features!
  "Claim the tethered Push 3 once (bus/address from push3-display.sh status),
  start Studio on 127.0.0.1:port and show every check 8 state for hold-ms
  in order. :backend replaces the libusb backend and :capture the browser
  capture (replay tests). Returns the receipt."
  [{:keys [bus address port hold-ms backend capture browser-executable out-dir]
    :or {port 4310 hold-ms default-hold-ms
         browser-executable "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome"}}]
  (let [started (Instant/now)
        out-dir (or out-dir (io/file kit/receipt-root (str "check-8-" (str/replace (str started) ":" ""))))
        catalog-saves (save-catalog-shows! out-dir)
        runtime (show/create-runtime {:model (feature-model) :workspace-options nil})
        ticker (let [running (atom true)
                     thread (doto (Thread. #(while @running (try (show/tick! runtime) (catch Throwable _)) (Thread/sleep 10))
                                           "naut-display-ticker")
                              (.setDaemon true) (.start))]
                 (fn [] (reset! running false) (.join thread 1000)))
        web (studio/listen! runtime {:host "127.0.0.1" :port port})
        {stats :stats instrumented :backend} (kit/instrumented-backend
                                               (or backend {:open! push3-usb/open! :write! push3-usb/write! :close! push3-usb/close!}))
        claim-start (System/nanoTime) claim-local (local-time)
        _ (println (str claim-local " claiming the Push 3 display (bus " bus ", address " address "); expect about 25 s"))
        shared (shared-sink (push3-display/sink {:bus bus :address address :backend instrumented}))
        claim-ms (/ (- (System/nanoTime) claim-start) 1e6)
        _ (println (format "%s claimed after %.0f ms" (local-time) claim-ms))
        ctx {:runtime runtime :presented (atom nil) :device (atom nil) :emit! (atom nil) :instance-id (atom nil)
             :stats stats :shared shared :out-dir out-dir :catalog-saves catalog-saves
             :display-options {:sink (:sink shared) :browser-executable browser-executable
                               :url (str "http://127.0.0.1:" (:port web) "/push-display")}}
        results (atom [])]
    (try
      (with-redefs [display/process-capture (if capture (constantly capture) display/process-capture)]
        (doseq [[index state] (map-indexed vector (states hold-ms))]
          (swap! results conj (run-state! ctx state index))))
      (finally
        (try (disconnect! ctx) (catch Throwable _))
        (try ((:close! shared)) (catch Throwable _))
        ((:stop! web)) (ticker) (show/stop! runtime)))
    (let [s @stats states @results]
      {:check :push-3/display-features :issues [547 529 552 530] :at (str started)
       :bus bus :address address :studio-port (:port web)
       :claim {:local-start claim-local :ms claim-ms}
       :input "virtual Push 3 at the MIDI transport boundary (kit/push-3-bytes, synthetic addresses); no Push 3 MIDI port opened"
       :states states
       :timetable (mapv #(select-keys % [:state :local-start :local-end :verdict]) states)
       :transfers {:header (:header-transfers s) :payload (:payload-transfers s) :bytes (:bytes s)
                   :opens (:opens s) :closes (:closes s) :errors (:errors s)}
       :payload-transfer-ms (#'kit/distribution (:payload-ms s))
       :descriptor (:descriptor s)
       :verdict (cond (seq (:errors s)) :failed
                      (every? #(= :shown (:verdict %)) states) :presented
                      :else :partial)})))

(defn -main
  "lein run -m tolglow.qa.naut-display-features BUS ADDRESS [HOLD-SECONDS] [PORT]"
  [& [bus address hold port]]
  (let [receipt (display-features! {:bus (parse-long bus) :address (parse-long address)
                                    :hold-ms (* 1000 (or (some-> hold parse-long) 15))
                                    :port (or (some-> port parse-long) 4310)})]
    (pprint/pprint (select-keys receipt [:check :verdict :claim :timetable :transfers :payload-transfer-ms]))
    (println "receipt:" (kit/write-receipt! receipt))
    (shutdown-agents)
    (System/exit (if (#{:presented} (:verdict receipt)) 0 1))))
