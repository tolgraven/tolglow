(ns tolglow.controllers.content-browser-display-test
  (:require [afterglow.control.codecs.push-2 :as push-2]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]
            [tolglow.controllers.content-browser :as browser]
            [tolglow.controllers.content-browser-display :as catalog]
            [tolglow.controllers.content-browser-host :as host]
            [tolglow.demo :as demo]
            [tolglow.registry :as registry]
            [tolglow.show :as show]
            [tolglow.show.recovery :as recovery]
            [tolglow.studio.display :as display])
  (:import [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]
           [java.util.concurrent LinkedBlockingQueue TimeUnit]))

(defn- page [view]
  (let [descriptors (mapv (fn [index]
                            {:show-id (keyword (str "show-" index)) :name (str "Show " index)
                             :source-kind :named :handle :test/named :revision 2 :path "/trusted/file"})
                          (range 10))]
    (assoc (browser/project (browser/catalog {:show-id :active :epoch 4} descriptors) view)
           :runtime-id "catalog-runtime")))

(def controls
  {:toggle {:channel 0 :controller 56} :back {:channel 0 :controller 55}
   :next {:channel 0 :controller 63} :previous {:channel 0 :controller 62}
   [:select 0] {:channel 0 :controller 20}})

(defn- await-view [^LinkedBlockingQueue views predicate]
  (loop [attempts 100]
    (when (pos? attempts)
      (let [view (.poll views 25 TimeUnit/MILLISECONDS)]
        (if (and view (predicate view)) view (recur (dec attempts)))))))

(deftest captured-buttons-browse-and-preview-without-intercepting-live-pad-ownership
  (let [callback (atom nil) forwarded (atom []) requests (atom []) view-thread (atom nil)
        views (LinkedBlockingQueue.) input-thread (Thread/currentThread)
        transport (catalog/transports
                    {:controls controls
                     :midi {:open! #(do (reset! callback %) :midi) :send! (fn [& _]) :close! (fn [_])}
                     :display! (fn [{:keys [presented!] :as hooks}]
                                 (display/transport
                                  (assoc hooks
                                         :max-fps 60
                                         :project! (fn [view] (assoc view :normal-projection? true))
                                         :capture {:open! (constantly :capture) :close! (fn [_])
                                                   :capture! (fn [_ view] view)}
                                         :sink {:open! (constantly :sink) :close! (fn [_]) :render! (fn [& _])}
                                         :presented! (fn [view] (presented! view) (.offer views view)))))
                     :project! (fn [view] (reset! view-thread (Thread/currentThread)) (page view))
                     :preview! (fn [{:keys [selection] :as request}]
                                 (swap! requests conj request)
                                 {:entry {:name (str "Preview " (name (second (:key selection))))
                                          :sources {:named {:handle :checkpoint/named :revision 1760000000000}
                                                    :recovery {:handle :test/working :revision 7}}}})})
        midi (:midi transport) display (:display transport)
        midi-handle ((:open! midi) #(swap! forwarded conj %))
        display-handle ((:open! display))
        browser-pane :saved-show-catalog
        press! (fn [control] (@callback [0xb0 control 127]) (@callback [0xb0 control 0]))]
    (try
      ((:render! display) display-handle {:display-mode :parameters})
      (is (some? (await-view views #(= :parameters (:display-mode %)))))
      ;; A live control already down keeps both of its edges, even if the
      ;; browser opens before it is lifted. Pads never belong to this browser.
      (@callback [0xb0 20 127])
      (@callback [0x90 36 127])
      (press! 56)
      (let [view (await-view views #(= 0 (get-in % [browser-pane :page])))]
        (is (:normal-projection? view))
        (is (= [8 10 "Show 0"] [(count (get-in view [browser-pane :rows])) (get-in view [browser-pane :total])
                               (get-in view [browser-pane :rows 0 :name])]))
        (is (not (identical? @view-thread input-thread)))
        (is (not (re-find #"file|:handle|:selection|confirmation" (pr-str (browser-pane view))))))
      (@callback [0xb0 20 0])
      (@callback [0x80 36 0])
      (press! 63)
      (is (some? (await-view views #(= 1 (get-in % [browser-pane :page])))))
      (press! 20)
      (let [view (await-view views #(get-in % [browser-pane :preview]))]
        (is (= "Preview show-8" (get-in view [browser-pane :preview :name])))
        (is (= [{:label "Working recovery"}
                {:label "Latest named" :modified-at-ms 1760000000000}]
               (get-in view [browser-pane :preview :sources])))
        (is (= {:runtime-id "catalog-runtime" :selection {:show-id :active :epoch 4 :key [:show :show-8]}}
               (first @requests))))
      (press! 55)
      (is (some? (await-view views #(and (= 1 (get-in % [browser-pane :page]))
                                       (nil? (get-in % [browser-pane :preview]))
                                       (get-in % [browser-pane :rows 0 :selected?])))))
      (press! 55)
      (is (some? (await-view views #(= :parameters (:display-mode %)))))
      (is (= [[0xb0 20 127] [0x90 36 127] [0xb0 20 0] [0x80 36 0]] @forwarded))
      (finally
        ((:close! display) display-handle)
        ((:close! midi) midi-handle)))))

(deftest row-press-uses-the-page-handed-to-the-sink
  (let [receive (atom nil)
        second-page-captured (promise)
        finish-render (promise)
        views (LinkedBlockingQueue.)
        {:keys [midi display]}
        (catalog/transports
         {:controls controls
          :midi {:open! #(reset! receive %) :send! (fn [& _]) :close! (fn [_])}
          :project! page
          :preview! (fn [request] {:entry {:name (name (get-in request [:selection :key 1]))}})
          :display! (fn [{:keys [presented!] :as hooks}]
                      (display/transport
                       (assoc hooks
                              :max-fps 60
                              :capture {:open! (constantly :capture) :close! (fn [_])
                                        :capture! (fn [_ view] view)}
                              :sink {:open! (constantly :sink) :close! (fn [_])
                                     :render! (fn [_ view]
                                                (when (= 1 (get-in view [:saved-show-catalog :page]))
                                                  (deliver second-page-captured true)
                                                  (deref finish-render 2500 nil)))}
                              :presented! (fn [view] (presented! view) (.offer views view)))))})
        midi-handle ((:open! midi) (fn [_]))
        display-handle ((:open! display))
        press! (fn [cc] (@receive [0xb0 cc 127]) (@receive [0xb0 cc 0]))]
    (try
      ((:render! display) display-handle {:display-mode :parameters})
      (press! 56)
      (is (some? (await-view views #(= 0 (get-in % [:saved-show-catalog :page])))))
      (press! 63)
      (is (true? (deref second-page-captured 2500 nil)))
      (press! 20)
      (deliver finish-render true)
      (is (= "show-0" (-> (await-view views #(get-in % [:saved-show-catalog :preview]))
                           (get-in [:saved-show-catalog :preview :name]))))
      (finally
        (deliver finish-render true)
        ((:close! display) display-handle)
        ((:close! midi) midi-handle)))))

(deftest navigation-preserves-selection-on-refresh-and-reports-removed-show
  (let [initial (catalog/navigate {:open? false :page 0} nil :toggle)
        first-page (page {})
        selected (catalog/navigate initial first-page [:select 7])
        back (catalog/navigate selected first-page :back)
        next-page (catalog/navigate back first-page :next)]
    (is (= [:show :show-7] (get-in next-page [:selected :key])))
    (is (= 1 (:page next-page)))
    (is (= {:unavailable? true}
           (:preview (catalog/display-view selected first-page {:status :rejected :reason :stale-selection}))))
    (is (= {:open? false :page 0} (catalog/navigate next-page first-page :back)))))

(def restore-controls
  (assoc controls [:checkpoint :named] {:channel 0 :controller 102}
                  [:checkpoint :recovery] {:channel 0 :controller 103}
                  :confirm {:channel 0 :controller 104}))

(deftest registered-library-switch-retires-an-in-flight-page
  (let [model (demo/model)
        effect (assoc (get-in model [:registry :effects :amber]) :version 3)
        cue (assoc (get-in model [:registry :cues :amber]) :version 5 :effect-version 3)
        model (update model :registry #(-> %
                                          (registry/publish :effects effect)
                                          (registry/publish :cues cue)))
        runtime (show/create-runtime {:model model :workspace-options nil :workers 1})
        before @(:state runtime)
        receive (atom nil)
        frames (LinkedBlockingQueue.)
        effect-captured (promise) present-effect (promise)
        cue-captured (promise) present-cue (promise)
        submissions (atom [])
        pane :saved-show-catalog
        {:keys [midi display]}
        (catalog/transports
         {:controls (assoc restore-controls :next-kind {:channel 0 :controller 105})
          :midi {:open! #(reset! receive %) :send! (fn [& _]) :close! (fn [_])}
          :project! (partial host/project runtime [])
          :preview! (partial host/preview runtime [])
          :confirm! (fn [& args] (swap! submissions conj args))
          :display! (fn [{:keys [presented!] :as hooks}]
                      (display/transport
                       (assoc hooks :max-fps 60
                              :capture {:open! (constantly :capture) :close! (fn [_])
                                        :capture! (fn [_ view] view)}
                              :sink {:open! (constantly :sink) :close! (fn [_])
                                     :render! (fn [_ view]
                                                (case (get-in view [pane :kind])
                                                  :effect (do (deliver effect-captured true) (deref present-effect 2500 nil))
                                                  :cue (do (deliver cue-captured true) (deref present-cue 2500 nil))
                                                  nil))}
                              :presented! (fn [view] (presented! view) (.offer frames view)))))})
        midi-handle ((:open! midi) (fn [_]))
        handle ((:open! display))
        press! (fn [cc] (@receive [0xb0 cc 127]) (@receive [0xb0 cc 0]))
        expect! (fn [predicate]
                  (let [view (await-view frames #(when-let [catalog (pane %)] (predicate catalog)))]
                    (is (some? view))
                    (pane view)))]
    (try
      ((:render! display) handle {:display-mode :parameters})
      (press! 56)
      (is (:next-kind? (expect! #(= :show (:kind %)))))
      (press! 105)
      (is (true? (deref effect-captured 2500 nil)))
      (press! 105)
      (press! 20)
      (deliver present-effect true)
      (is (true? (deref cue-captured 2500 nil)))
      (press! 20)
      (deliver present-cue true)
      (let [catalog (expect! #(= :cue (:kind %)))]
        (is (nil? (:preview catalog)))
        (is (= {:name "Amber" :version 5 :selected? false} (get-in catalog [:rows 0]))))
      (press! 20)
      (is (= {:name "Amber" :version 5 :effect-version 3 :categories [:color :emission :intensity]
              :usage {:page-count 1 :running-count 0}}
             (:preview (expect! :preview))))
      (press! 102)
      (press! 104)
      (press! 55)
      (is (some? (expect! #(and (= :cue (:kind %)) (nil? (:preview %)) (get-in % [:rows 0 :selected?])))))
      (press! 105)
      (expect! #(= :show (:kind %)))
      (press! 105)
      (expect! #(= :effect (:kind %)))
      (press! 20)
      (is (= {:name "Amber" :version 3 :categories [:color :emission :intensity]
              :usage {:cue-count 1 :running-count 0}}
             (:preview (expect! #(when (= :effect (:kind %)) (:preview %))))))
      (is (= before @(:state runtime)))
      (is (zero? @(:ingress-order runtime)))
      (is (empty? @submissions))
      (finally
        (deliver present-effect true)
        (deliver present-cue true)
        ((:close! display) handle)
        ((:close! midi) midi-handle)
        (show/stop! runtime)))))

(defn- checkpoint-status [view]
  (get-in view [:saved-show-catalog :checkpoint :status]))

(deftest the-exact-choice-and-pending-frame-must-be-presented-before-restore
  (let [receive (atom nil)
        frames (LinkedBlockingQueue.)
        ready-captured (promise) present-ready (promise)
        pending-captured (promise) present-pending (promise)
        submissions (atom [])
        confirmation {:token :private}
        source {:source-kind :named :handle :test/named :revision 2}
        {:keys [midi display]}
        (catalog/transports
         {:controls restore-controls
          :midi {:open! #(reset! receive %) :send! (fn [& _]) :close! (fn [_])}
          :project! page
          :preview! (fn [request]
                      (if (:source request) {:confirmation confirmation}
                          {:entry {:name "Show 0" :sources {:named source}}}))
          :confirm! (fn [exact checkpoint]
                      (swap! submissions conj [exact checkpoint])
                      {:status :rejected :reason :stale-save-source})
          :display! (fn [{:keys [presented!] :as hooks}]
                      (display/transport
                       (assoc hooks :max-fps 30
                              :capture {:open! (constantly :capture) :close! (fn [_]) :capture! (fn [_ view] view)}
                              :sink {:open! (constantly :sink) :close! (fn [_])
                                     :render! (fn [_ view]
                                                (case (checkpoint-status view)
                                                  :ready (do (deliver ready-captured true) (deref present-ready 2500 nil))
                                                  :pending (do (deliver pending-captured true) (deref present-pending 2500 nil))
                                                  nil))}
                              :presented! (fn [view] (presented! view) (.offer frames view)))))})
        midi-handle ((:open! midi) (fn [_]))
        handle ((:open! display))
        press! (fn [cc] (@receive [0xb0 cc 127]) (@receive [0xb0 cc 0]))]
    (try
      ((:render! display) handle {:display-mode :parameters})
      (press! 56)
      (is (some? (await-view frames #(get-in % [:saved-show-catalog :rows 0]))))
      (press! 20)
      (is (some? (await-view frames #(get-in % [:saved-show-catalog :preview]))))
      (press! 102)
      (is (true? (deref ready-captured 2500 false)))
      (press! 104)
      (deliver present-ready true)
      (is (some? (await-view frames #(= :ready (checkpoint-status %)))))
      (is (empty? @submissions))
      ((:render! display) handle {:display-mode :parameters :pad-overlay {:layout :full}})
      (is (some? (await-view frames #(and (:pad-overlay %) (nil? (:saved-show-catalog %))))))
      (press! 104)
      (is (empty? @submissions))
      ((:render! display) handle {:display-mode :parameters})
      (is (some? (await-view frames #(= :ready (checkpoint-status %)))))
      (press! 104)
      (is (true? (deref pending-captured 2500 false)))
      (press! 104)
      (is (empty? @submissions))
      (deliver present-pending true)
      (is (some? (await-view frames #(= :changed (checkpoint-status %)))))
      (is (= [[confirmation {:name "Show 0" :label "Latest named"}]] @submissions))
      (finally
        (deliver present-ready true)
        (deliver present-pending true)
        ((:close! display) handle)
        ((:close! midi) midi-handle)))))

(defn- checkpoint-fixture [directory kind]
  (if (= :push-2 kind)
    (let [file (io/file directory "private-show.transit")
          saved (show/create-runtime {:workspace-options nil :workers 1
                                      :model (assoc (demo/model) :show-id :tour :master 0.23)})]
      (try (show/save-show! saved file) (finally (show/stop! saved)))
      {:show-id :tour :file file :master 0.23 :control 102
       :saves [{:show-id :tour :name "Tour" :handle :test/tour :source-kind :named :path file}]})
    {:show-id :main :master 0.37 :control 103 :saves []}))

(deftest captured-devices-restore-through-the-owner-and-open-a-fresh-session
  (doseq [kind [:push-2 :push-3]]
    (testing (name kind)
      (let [directory (.toFile (Files/createTempDirectory "push-checkpoint-restore-" (make-array FileAttribute 0)))
            {:keys [show-id file master control saves]} (checkpoint-fixture directory kind)
            runtime (show/start! {:workspace-options nil :workers 1 :frame-rate 20
                                  :model (assoc (demo/model) :master 0.37)
                                  :recovery {:directory directory :enabled? (= kind :push-3) :interval-ms 30}}
                                 (constantly nil))
            receive (atom nil) openings (atom 0) closes (atom 0)
            frames (LinkedBlockingQueue. 96)
            transport display/transport
            options (cond-> {:session-id "captured" :schedule! nil :timeout-ms 30000
                             :catalog-controls restore-controls :catalog-saves saves
                             :midi {:open! (fn [callback] (reset! receive callback) (swap! openings inc))
                                    :close! (fn [_] (swap! closes inc))
                                    :send! (fn [& _])}
                             :display-options {:sink {:device kind :open! (constantly :sink) :close! (fn [_])
                                                      :render! (fn [_ _])}}}
                      (= kind :push-3) (assoc :user-mode? true :expression-mode :poly-aftertouch
                                            :layout {:encoders {:level {:channel 0 :controller 71}}
                                                     :encoder-touches {:level {:channel 0 :note 0}}}
                                            :encoder-bindings {:level {:param :level}}))
            press! (fn [cc] (@receive [0xb0 cc 127]) (@receive [0xb0 cc 0]))
            expect! (fn [predicate] (let [frame (await-view frames predicate)] (is (some? frame)) frame))]
        (try
          (when (= kind :push-3)
            (is (loop [attempts 150]
                  (if (contains? (:available (show/recovery-status runtime)) :recovery) true
                      (when (pos? attempts) (Thread/sleep 20) (recur (dec attempts)))))))
          (recovery/close! @(:recovery runtime))
          (is (= :accepted (:status (deref (show/submit! runtime {:op :master/set :value 0.91}) 3000 {}))))
          (with-redefs [display/process-capture (constantly {:open! (constantly :camera) :close! (fn [_])
                                                            :capture! (fn [_ view] view)})
                        display/transport (fn [options]
                                            (transport (update options :presented!
                                                         (fn [presented!]
                                                           (fn [view] (presented! view) (.offer frames view))))))]
            (let [device (catalog/connect-device! runtime kind options)
                  old-input @receive
                  before (:epoch (show/snapshot runtime))]
              (when (= :push-2 kind)
                (doseq [index (range 128)] (@receive (assoc (push-2/palette-write 0 index [0 0 0 0]) 6 4)))
                (@receive (push-2/sysex 0 [0x09])))
              (expect! #(nil? (:saved-show-catalog %)))
              (press! 56)
              (expect! #(get-in % [:saved-show-catalog :rows 0]))
              (press! 20)
              (expect! #(get-in % [:saved-show-catalog :preview]))
              (press! control)
              (let [view (expect! #(= :ready (checkpoint-status %)))]
                (is (not (re-find #"token|handle|private-show" (pr-str (:saved-show-catalog view))))))
              (is (= [before 0.91] ((juxt :epoch :master) (show/snapshot runtime))))
              (when file
                (recovery/write-source! file (slurp file))
                (press! 104)
                (expect! #(= :pending (checkpoint-status %)))
                (expect! #(= :changed (checkpoint-status %)))
                (is (= [before 0.91] ((juxt :epoch :master) (show/snapshot runtime))))
                (press! 55)
                (expect! #(get-in % [:saved-show-catalog :preview]))
                (press! control)
                (expect! #(= :ready (checkpoint-status %))))
              (press! 104)
              (expect! #(= :pending (checkpoint-status %)))
              (expect! #(= :restored (checkpoint-status %)))
              (is (= [show-id (inc before) master] ((juxt :show-id :epoch :master) (show/snapshot runtime))))
              (is (= [2 1] [@openings @closes]))
              (let [new-device (get @(:devices runtime) "captured")]
                (is (some? new-device))
                (is (not (identical? device new-device))))
              (old-input [0xb0 55 127])
              (old-input [0xb0 55 0])
              (expect! #(= :restored (checkpoint-status %)))
              (press! 55)
              (expect! #(nil? (:saved-show-catalog %)))))
          (finally
            (show/stop! runtime)
            (doseq [entry (reverse (file-seq directory))] (io/delete-file entry))))))))
