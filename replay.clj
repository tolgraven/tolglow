(require '[afterglow.control.codecs.push-2 :as push-2]
         '[clojure.data.json :as json]
         '[clojure.java.io :as io]
         '[tolglow.controllers.content-browser-display :as catalog]
         '[tolglow.demo :as demo]
         '[tolglow.show :as show]
         '[tolglow.show.recovery :as recovery]
         '[tolglow.studio :as studio]
         '[tolglow.studio.display :as display])
(import '[java.nio.file Files] '[java.nio.file.attribute FileAttribute]
        '[java.util.concurrent ArrayBlockingQueue TimeUnit] '[javax.imageio ImageIO])

(def controls
  {:toggle {:channel 0 :controller 56} :back {:channel 0 :controller 55}
   [:select 0] {:channel 0 :controller 20}
   [:checkpoint :named] {:channel 0 :controller 102}
   [:checkpoint :recovery] {:channel 0 :controller 103}
   :confirm {:channel 0 :controller 104}})

(defn snapshot [runtime]
  (select-keys (show/snapshot runtime) [:show-id :epoch :master]))

(defn await-frame [^ArrayBlockingQueue frames label matches?]
  (let [deadline (+ (System/nanoTime) 45000000000)]
    (loop []
      (let [frame (.poll frames 50 TimeUnit/MILLISECONDS)]
        (cond (and frame (matches? (:view frame))) frame
              (< (System/nanoTime) deadline) (recur)
              :else (throw (ex-info "Timed out waiting for presented frame" {:label label})))))))

(defn record! [directory kind frames label predicate]
  (let [{:keys [view image]} (await-frame frames label predicate)
        path (io/file directory (str (name kind) "-" (name label) ".png"))]
    (assert (= [960 160] [(.getWidth image) (.getHeight image)]))
    (ImageIO/write image "png" path)
    {:image (.getName path) :catalog (:saved-show-catalog view)}))

(defn wait-until [f]
  (loop [attempts 600]
    (or (f) (when (pos? attempts) (Thread/sleep 100) (recur (dec attempts))))))

(defn run-device! [directory kind]
  (let [push2? (= :push-2 kind)
        target (if push2? :tour :festival)
        temp (.toFile (Files/createTempDirectory "push-checkpoint-capture-" (make-array FileAttribute 0)))
        runtime (show/start! {:workspace-options nil :workers 1 :frame-rate 20
                              :model (assoc (demo/model) :show-id (if push2? :main target) :master 0.37)
                              :recovery {:enabled? (not push2?) :directory temp :interval-ms 40}}
                             (constantly nil))
        file (io/file temp "disposable-checkpoint.transit")
        server (studio/listen! runtime {:port 0})
        origin (str "http://127.0.0.1:" (:port server))
        frames (ArrayBlockingQueue. 5)
        input (atom nil) opens (atom 0) closes (atom 0)
        emit! (fn [frame]
                (when-not (.offer frames frame) (.poll frames) (.offer frames frame)))
        opts (cond-> {:session-id (name kind) :schedule! nil :timeout-ms 60000
                      :catalog-controls controls
                      :catalog-saves (if push2? [{:show-id target :name "Tour rehearsal" :source-kind :named
                                                 :handle :capture/tour :path file}]
                                                [{:show-id target :name "Festival rehearsal" :source-kind :recovery
                                                  :handle :capture/festival}])
                      :midi {:open! (fn [receive!] (reset! input receive!) (swap! opens inc))
                             :close! (fn [_] (swap! closes inc)) :send! (fn [& _])}
                      :display-options {:browser-executable "/usr/bin/chromium" :url (str origin "/push-display")
                                        :startup-timeout-ms 60000 :capture-timeout-ms 30000 :max-fps 8
                                        :no-sandbox? true :shutdown-timeout-ms 5000
                                        :sink {:device kind :open! (constantly :virtual-display) :close! (fn [_])
                                               :render! (fn [_ frame] (emit! frame))
                                               :render-with-receipt! (fn [_ frame complete!]
                                                                       (complete!) (emit! frame))}}}
               (not push2?) (assoc :user-mode? true :expression-mode :poly-aftertouch
                                   :layout {:encoders {:level {:channel 0 :controller 71}}
                                            :encoder-touches {:level {:channel 0 :note 0}}}
                                   :encoder-bindings {:level {:param :level}}))
        press! (fn [cc] (@input [0xb0 cc 127]) (@input [0xb0 cc 0]))
        record (partial record! directory kind frames)
        status? (fn [status] #(= status (get-in % [:saved-show-catalog :checkpoint :status])))
        actual-capture display/process-capture]
    (try
      (if push2?
        (let [saved (show/create-runtime {:workspace-options nil :workers 1
                                          :model (assoc (demo/model) :show-id target :master 0.37)})]
          (try (show/save-show! saved file) (finally (show/stop! saved))))
        (assert (wait-until #(contains? (:available (show/recovery-status runtime)) :recovery))))
      (recovery/close! @(:recovery runtime))
      (assert (= :accepted (:status (deref (show/submit! runtime {:op :master/set :value 0.91}) 10000 {}))))
      (with-redefs [display/process-capture
                    (fn [options]
                      (update (actual-capture options) :capture!
                        (fn [capture!] (fn [handle view] {:view view :image (capture! handle view)}))))]
        (println "Capture" kind origin)
        (let [old-device (catalog/connect-device! runtime kind opts)
              before (snapshot runtime)
              old-input @input
              _ (when push2?
                  (doseq [index (range 128)] (old-input (assoc (push-2/palette-write 0 index [0 0 0 0]) 6 4)))
                  (old-input (push-2/sysex 0 [0x09])))
              live (record :live #(nil? (:saved-show-catalog %)))
              _ (press! 56)
              catalog (record :catalog #(get-in % [:saved-show-catalog :rows 0]))
              _ (press! 20)
              preview (record :preview #(get-in % [:saved-show-catalog :preview]))
              _ (press! (if push2? 102 103))
              ready (record :confirm (status? :ready))
              _ (assert (= before (snapshot runtime)))
              refusal (when push2?
                        (recovery/write-source! file (slurp file))
                        (press! 104)
                        (let [refused (record :changed (status? :changed))]
                          (assert (= before (snapshot runtime)))
                          (press! 55)
                          (await-frame frames :preview #(get-in % [:saved-show-catalog :preview]))
                          (press! 102)
                          (await-frame frames :confirm (status? :ready))
                          refused))
              _ (press! 104)
              pending (record :pending (status? :pending))
              restored (record :restored (status? :restored))
              after (snapshot runtime)
              _ (assert (= (assoc before :show-id target :epoch (inc (:epoch before)) :master 0.37) after))
              new-device (wait-until #(get @(:devices runtime) (name kind)))
              _ (assert (and new-device (not (identical? old-device new-device)) (= [2 1] [@opens @closes])))
              _ (old-input [0xb0 55 127])
              _ (old-input [0xb0 55 0])
              _ (await-frame frames :retired-input (status? :restored))
              _ (press! 55)
              back (record :back #(nil? (:saved-show-catalog %)))]
          {:input :virtual-midi :before before :after after :reopened true
           :frames (cond-> {:live live :catalog catalog :preview preview :ready ready
                            :pending pending :restored restored :back back}
                     refusal (assoc :changed refusal))}))
      (finally
        (show/stop! runtime)
        ((:stop! server))
        (doseq [entry (reverse (file-seq temp))] (io/delete-file entry))))))

(assert (wait-until #(.isFile (io/file "resources/studio/js/manifest.edn"))) "Optimized Studio build is required")

(let [[target tolglow afterglow] *command-line-args*
      directory (io/file target)
      devices (into {} (for [kind [:push-2 :push-3]] [kind (run-device! directory kind)]))]
  (spit (io/file directory "receipts.json") (json/write-str {:tolglow tolglow :afterglow afterglow :devices devices}))
  (shutdown-agents))
