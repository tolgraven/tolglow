(ns tolglow.visualization.parameter-frames-test
  "Shared parameter-visualization frames (#625) over a real show runtime:
  compatible views share one render, different layouts get their own, a
  view that does not read loses only its own frames, and the last view
  leaving stops rendering. Studio's routes serve the same service. Costs are
  printed from the measured render calls; they are this Mac under other
  lanes' load, Java2D in a headless JVM, not a GPU or a Live host."
  (:require [afterglow.control.model :as control]
            [clojure.data.json :as json]
            [clojure.test :refer [deftest is testing]]
            [tolglow.show :as show]
            [tolglow.studio :as studio]
            [tolglow.visualization.parameter-frames :as frames])
  (:import [java.io ByteArrayInputStream]
           [javax.imageio ImageIO]))

(def registry
  {:effects {:wash {:id :wash :version 1 :category :light
                    :params {:level {:type :number :default 0.5 :bounds [0.0 1.0] :name "Level"}}
                    :outputs {:dimmer {:op :param :key :level}}}}
   :cues {:a {:id :a :effect-id :wash :version 1}}})

(defn- runtime [& [options]]
  (show/create-runtime (merge {:model (control/new-show {:show-id :frames :transport {:playing? true :beat 0.0 :bpm 120.0}
                                                         :registry registry
                                                         :rig {:fixtures [{:id :left :universe 1 :address 1 :channels {:dimmer 1}}]}})
                               :sink {:send! (fn [_ _])} :workspace-options nil :phrase-capture false :live-runs false}
                              options)))

(defn- launch! [rt]
  (show/tick! rt 0)
  (let [reply (show/submit! rt :command {:op :cue/launch :cue-id :a :launch {:mode :persistent}})]
    (show/tick! rt 10)
    (:instance-id (deref reply 1000 nil))))

(defn- service [rt & [options]]
  (frames/create (merge {:snapshot! #(deref (:state rt)) :worker? false} options)))

(defn- view [id & {:as more}]
  (merge {:instance-id id :param :level :window-beats 4 :style :history :width 300 :height 60 :dpr 1} more))

(deftest compatible-views-share-one-render-and-different-layouts-get-their-own
  (let [rt (runtime) id (launch! rt) svc (service rt)]
    (try
      (let [studio (frames/subscribe! svc (view id) {:id :studio})
            push (frames/subscribe! svc (view id :width 310 :height 62) {:id :push})
            wide (frames/subscribe! svc (view id :width 600) {:id :live})
            filled (frames/subscribe! svc (view id :style :history-filled) {:id :studio-filled})
            sharp (frames/subscribe! svc (view id :dpr 2) {:id :retina})]
        (is (= (:key studio) (:key push)) "within 10%: Push shows Studio's frame")
        (is (:shared? push))
        (is (= {:width 300 :height 60 :dpr 1.0} (:render push)) "the joiner scales the shared frame")
        (is (= 4 (count (distinct (map :key [studio wide filled sharp])))) "size, style and pixel ratio each get their own render")
        (doseq [t [100 200 300]]
          (show/tick! rt t)
          (frames/step! svc)
          (frames/take-frame! svc (:lease studio)))
        (let [{:keys [counts keys]} (frames/status svc)
              shared (some #(when (= (:key studio) (:key %)) %) keys)]
          (is (= 12 (:renders counts)) "three cycles of four keys")
          (is (= 3 (:renders shared)))
          (is (= 15 (:presentations counts)) "the shared frame is presented to both of its views")
          (is (= {:studio [3 0] :push [3 2]} (into {} (map (juxt :consumer (juxt :presented :dropped))) (:views shared)))
              "Push never read: only its own older frames were dropped"))
        (testing "a frame is the requested device size, drawn from the shared projection"
          (let [frame (frames/latest-frame svc (:lease sharp))
                image (ImageIO/read (ByteArrayInputStream. (frames/png-bytes frame)))]
            (is (= [600 120] [(.getWidth image) (.getHeight image)]))
            (is (= [600 120] [(:width frame) (:height frame)]))))
        (testing "views of one parameter share one sample stream"
          (is (= [[id :level]] (keys (:samples @(:state svc)))))))
      (finally (show/stop! rt)))))

(deftest the-last-view-leaving-stops-rendering-and-unread-views-expire
  (let [rt (runtime) id (launch! rt)]
    (try
      (testing "the worker renders off the show owner and ends with the last view"
        (let [svc (service rt {:worker? true :period-ms 10})
              a (frames/subscribe! svc (view id) {:id :studio})
              b (frames/subscribe! svc (view id) {:id :live})]
          (loop [n 0] (when (and (< n 200) (< (get-in (frames/status svc) [:counts :renders]) 3)) (Thread/sleep 5) (recur (inc n))))
          (is (:running? (frames/status svc)))
          (is (some? (frames/take-frame! svc (:lease a))))
          (frames/unsubscribe! svc (:lease a))
          (is (seq (:keys (frames/status svc))) "one view still mounted")
          (frames/unsubscribe! svc (:lease b))
          (is (empty? (:keys (frames/status svc))))
          (loop [n 0] (when (and (< n 200) (:running? (frames/status svc))) (Thread/sleep 5) (recur (inc n))))
          (is (not (:running? (frames/status svc))) "no view, no rendering")
          (let [renders (get-in (frames/status svc) [:counts :renders])]
            (Thread/sleep 50)
            (is (= renders (get-in (frames/status svc) [:counts :renders]))))))
      (testing "a disconnected view expires when it stops reading"
        (let [svc (service rt {:lease-ms 0})
              a (frames/subscribe! svc (view id) {:id :hidden})]
          (Thread/sleep 2)
          (frames/step! svc)
          (is (= 1 (get-in (frames/status svc) [:counts :expired])))
          (is (nil? (frames/take-frame! svc (:lease a))))
          (is (empty? (:keys (frames/status svc))))))
      (testing "resizing a view is a new subscription; a large change renders separately"
        (let [svc (service rt)
              a (frames/subscribe! svc (view id) {:id :studio})
              b (frames/subscribe! svc (view id :width 500) {:id :studio})]
          (frames/unsubscribe! svc (:lease a))
          (is (= [(:key b)] (mapv :key (:keys (frames/status svc)))))))
      (finally (show/stop! rt)))))

(deftest nothing-moved-means-no-new-frame
  (let [rt (runtime) id (launch! rt) svc (service rt)
        a (frames/subscribe! svc (view id) {:id :studio})]
    (try
      (frames/step! svc)
      (frames/step! svc)
      (frames/step! svc)
      (is (= 1 (get-in (frames/status svc) [:counts :renders])) "the transport and the value stood still")
      (is (= 2 (get-in (frames/status svc) [:counts :unchanged])))
      (show/tick! rt 600)
      (frames/step! svc)
      (is (= 2 (get-in (frames/status svc) [:counts :renders])))
      (is (re-matches #"[0-9a-f-]{36}" (:lease a)) "leases are unguessable")
      (finally (show/stop! rt)))))

(deftest an-edit-reaches-every-view-at-the-next-frame
  (let [rt (runtime) id (launch! rt) svc (service rt)
        a (frames/subscribe! svc (view id) {:id :studio})
        b (frames/subscribe! svc (view id) {:id :push})]
    (try
      (frames/step! svc)
      (let [reply (show/submit! rt :command {:op :param/set :instance-id id :param :level :value 0.9})]
        (show/tick! rt 500)
        (deref reply 1000 nil))
      (frames/step! svc)
      (let [samples (get-in @(:state svc) [:samples [id :level]])]
        (is (= [0.5 0.9] (mapv second samples)) "the edit is the next sample both views draw"))
      (is (= (:sequence (frames/latest-frame svc (:lease a))) (:sequence (frames/latest-frame svc (:lease b))) 2))
      (finally (show/stop! rt)))))

(deftest shared-rendering-costs-one-render-for-n-views
  (let [rt (runtime) id (launch! rt)
        request (frames/validate-request (view id :width 480 :height 96 :dpr 2))
        input {:samples (mapv (fn [i] [(* i 0.05) (+ 0.5 (* 0.4 (Math/sin (* i 0.3))))]) (range 80)) :beat 4.0
               :schema {:type :number :bounds [0.0 1.0] :name "Level"} :param :level}
        time-ns (fn [f n] (let [start (System/nanoTime)] (dotimes [_ n] (f)) (/ (- (System/nanoTime) start) n)))]
    (try
      (dotimes [_ 20] (frames/render-png request input))
      (let [one (time-ns #(frames/render-png request input) 50)
            bytes (count (frames/render-png request input))]
        (doseq [n [2 4]]
          (let [svc (service rt)
                leases (mapv #(frames/subscribe! svc (view id :width 480 :height 96 :dpr 2) {:id %}) (range n))]
            (dotimes [i 10]
              (show/tick! rt (+ (* n 10000) (* 100 i)))
              (frames/step! svc)
              (doseq [l leases] (frames/take-frame! svc (:lease l))))
            (is (= 10 (get-in (frames/status svc) [:counts :renders])) "one render per cycle, however many views")
            (is (= (* 10 n) (get-in (frames/status svc) [:counts :presentations])))
            (println (format "parameter frames, %d views at 960x192: shared %.0f us per cycle (1 render), separate %.0f us (%d renders), PNG %d bytes"
                             n (/ one 1000.0) (/ (* n one) 1000.0) n bytes)))))
      (finally (show/stop! rt)))))

(defn- request [app method uri body]
  (app {:request-method method :uri (first (.split ^String uri "\\?")) :headers {"host" "localhost" "content-type" "application/json"}
        :query-string (second (re-find #"\?(.*)$" uri))
        :body (ByteArrayInputStream. (.getBytes (if body (json/write-str body) "") "UTF-8"))}))

(deftest studio-serves-frames-from-the-runtime-service
  ;; The runtime's own service, stepped here instead of by its worker.
  (let [rt (runtime {:parameter-frames {:worker? false}}) id (launch! rt) app (studio/handler rt)]
    (try
      (let [subscribed (json/read-str (:body (request app :post "/api/parameter-frames/subscribe"
                                                      (assoc (view id) :consumer "studio-widget")))
                                      :key-fn keyword)
            lease (:lease subscribed)
            again (json/read-str (:body (request app :post "/api/parameter-frames/subscribe"
                                                 (assoc (view id :width 290) :consumer "live-device")))
                                 :key-fn keyword)]
        (is (string? lease) (pr-str subscribed))
        (is (true? (:shared? again)) "the Live device view joins the Studio widget's render")
        (is (= 204 (:status (request app :get (str "/api/parameter-frames/frame?lease=" lease) nil))) "nothing rendered yet")
        (frames/step! @(:parameter-frames rt))
        (let [response (request app :get (str "/api/parameter-frames/frame?lease=" lease) nil)]
          (is (= 200 (:status response)))
          (is (= "image/png" (get-in response [:headers "Content-Type"])))
          (is (= "1" (get-in response [:headers "X-Frame-Sequence"]))))
        (let [status (json/read-str (:body (request app :get "/api/parameter-frames" nil)) :key-fn keyword)]
          (is (= 1 (count (:keys status))))
          (is (= ["live-device" "studio-widget"] (sort (map :consumer (:views (first (:keys status))))))))
        (request app :post "/api/parameter-frames/unsubscribe" {:lease lease})
        (is (= 410 (:status (request app :get (str "/api/parameter-frames/frame?lease=" lease) nil)))
            "an ended or expired lease tells the view to subscribe again")
        (request app :post "/api/parameter-frames/unsubscribe" {:lease (:lease again)})
        (is (empty? (:keys (show/parameter-frames-action! rt :status {})))))
      (finally (show/stop! rt)))))
