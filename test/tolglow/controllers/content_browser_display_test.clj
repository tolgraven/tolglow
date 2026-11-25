(ns tolglow.controllers.content-browser-display-test
  (:require [clojure.test :refer [deftest is]]
            [tolglow.controllers.content-browser :as browser]
            [tolglow.controllers.content-browser-display :as catalog]
            [tolglow.studio.display :as display])
  (:import [java.util.concurrent LinkedBlockingQueue TimeUnit]))

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
