(ns tolglow.controllers.surface-host-test
  "Virtual replay of a linked X-Touch + X-Touch-Ext surface through the MIDI
  transport boundary against a real show owner (#705 slice 1)."
  (:require [afterglow.control.model :as control]
            [afterglow.control.devices.session :as device-session]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [tolglow.controllers.surface :as surface]
            [tolglow.show :as show]
            [tolglow.studio :as studio]
            [tolglow.studio.web :as web]
            [tolglow.transit :as transit])
  (:import [java.io StringReader]))

(def ^:private cue-ids (mapv #(keyword (str "c" %)) (range 8)))

(def ^:private group-ids (mapv #(keyword (str "g" %)) (range 12)))

(defn- model []
  (let [head {:id :head :universe 1 :address 1 :channels {:dimmer 1}}]
    (reduce (fn [m id]
              (-> m
                  (assoc-in [:registry :effects id] {:id id :version 1 :params {:level {:type :number :default 0.5 :bounds [0 1]}}
                                                     :outputs {:dimmer {:op :param :key :level}}})
                  (assoc-in [:registry :cues id] {:id id :version 1 :effect-id id :priority 0 :name (str/upper-case (name id))})))
            (control/new-show {:rig {:fixtures [head]}})
            cue-ids)))

(defn- harness []
  (let [runtime (show/create-runtime {:model (model) :workspace-options nil})
        time (atom 0)
        receivers (atom {})
        sent (atom {})
        transport (fn [unit]
                    {:open! (fn [receive!] (swap! receivers assoc unit receive!) unit)
                     :send! (fn [_ batch] (swap! sent update unit (fnil into []) batch))
                     :close! (fn [_] (swap! sent update unit (fnil conj []) :closed))})
        submit! show/submit!
        owner! (fn [f]
                 (with-redefs [show/now-ms (constantly @time)
                               show/submit! (fn submit-and-tick
                                              ([owner command] (submit-and-tick owner :command command nil))
                                              ([owner type command] (submit-and-tick owner type command nil))
                                              ([owner type command authority]
                                               (let [receipt (submit! owner type command authority)]
                                                 (show/tick! owner (swap! time inc))
                                                 receipt)))]
                   (f)))]
    {:runtime runtime :time time :sent sent :transport transport :owner! owner!
     :input! (fn [unit bytes] (owner! #((get @receivers unit) bytes)))
     :tick! #(show/tick! runtime (swap! time inc))}))

(defn- submit! [{:keys [runtime tick!]} type payload]
  (let [receipt (show/submit! runtime type (merge {:source {:adapter :repl :session-id "surface-test"}} payload))]
    (tick!)
    (deref receipt 1000 ::missing)))

(defn- labels
  "Strip labels from the last first-row LCD write captured for one unit."
  [{:keys [sent]} unit]
  (let [row (->> (get @sent unit) (filter vector?)
                 (filter #(and (= [240 0 0 102] (take 4 %)) (= [18 0] (take 2 (drop 5 %))))) last)]
    (mapv str/trim (map #(apply str %) (partition 7 (map char (butlast (drop 7 row))))))))

(defn- the-surface [runtime]
  (first (get-in (show/snapshot runtime) [:control-surfaces :surfaces])))

(defn- window [runtime]
  (let [{:keys [offset width strip-count strips]} (the-surface runtime)]
    {:offset offset :width width :count strip-count
     :visible (mapv (juxt :position :index) strips)}))

(defn- connect! [{:keys [runtime transport]}]
  (show/connect-device! runtime :x-touch
                        {:session-id "xt" :effect-group-view :manual :schedule! nil
                         :channels (mapv #(hash-map :cue-id %) cue-ids)
                         :units [{:session-id "xt-main" :transport (transport :main)}
                                 {:session-id "xt-ext" :extender? true :transport (transport :ext)}]}))

(defn- refresh! [group] (run! device-session/refresh! (:sessions @(:state group))))

(defn- studio-post [{:keys [runtime owner!]}]
  (let [app (web/app {:heartbeat! (constantly nil)
                      :submit! (fn [operation payload] (show/submit! runtime operation payload))})]
    (fn [payload]
      (let [response (owner! #(app {:request-method :post :uri "/api/command"
                                    :headers {"host" "localhost" "accept" "application/transit+json"
                                              "content-type" "application/transit+json"}
                                    :body (StringReader. (transit/encode payload))}))]
        [(:status response) (transit/decode (:body response))]))))

(deftest a-linked-surface-banks-both-units-through-the-committed-session-box
  (let [{:keys [runtime input! tick!] :as h} (harness)
        post! (studio-post h)]
    (try
      (is (= :accepted (:status (submit! h :command {:op :effect-groups/configure
                                                      :config {:manual (mapv #(hash-map :id % :name (name %)) group-ids)}}))))
      (let [group (connect! h)
            bank! (fn [& notes] (doseq [note notes] (input! :main [144 note 127]) (input! :main [128 note 0])) (refresh! group))]
        (tick!)
        (refresh! group)
        (testing "12 groups then 8 cues make 20 strips; the first window spans main and extender"
          (is (= {:offset 0 :width 16 :count 20 :visible (mapv vector (range 16) (range 16))} (window runtime)))
          (is (= (mapv name (take 8 group-ids)) (labels h :main)))
          (is (= ["g8" "g9" "g10" "g11" "C0" "C1" "C2" "C3"] (labels h :ext)))
          (is (= ["xt-main" "xt-ext"] (mapv :session-id (get-in (the-surface runtime) [:units]))))
          (is (= [:group [:manual :g0]] (:key (first (:strips (the-surface runtime)))))))
        (testing "bank right pages by 16; the 4-strip remainder lands on the main unit"
          (bank! 47)
          (is (= {:offset 16 :width 16 :count 20 :visible [[0 16] [1 17] [2 18] [3 19]]} (window runtime)))
          (is (= ["C4" "C5" "C6" "C7" "" "" "" ""] (labels h :main)))
          (is (= (vec (repeat 8 "")) (labels h :ext)))
          (bank! 47)
          (is (= 16 (:offset (window runtime))) "The last page is the limit"))
        (testing "SHIFT jumps to the first and last bank"
          (input! :main [144 70 127])
          (bank! 46)
          (is (= 0 (:offset (window runtime))))
          (bank! 47)
          (is (= 16 (:offset (window runtime))))
          (input! :main [128 70 0])
          (bank! 48)
          (is (= 15 (:offset (window runtime))) "Channel left steps one strip")
          (is (= ["C3" "C4" "C5" "C6" "C7" "" "" ""] (labels h :main)))
          (bank! 46))
        (testing "Studio moves the same window and the hardware follows"
          (is (= 0 (:offset (window runtime))))
          (is (= 200 (first (post! {:op :control-surface/bank :surface-id "xt" :move :last}))))
          (refresh! group)
          (is (= ["C4" "C5" "C6" "C7" "" "" "" ""] (labels h :main)))
          (is (= 200 (first (post! {:op :control-surface/bank :surface-id "xt" :reveal [:group [:manual :g2]]}))))
          (refresh! group)
          (is (= "g0" (first (labels h :main))) "Reveal moves to the page that holds the strip")
          (let [[status receipt] (post! {:op :control-surface/bank :surface-id "xt" :reveal [:cue :c1]})]
            (is (= [200 :accepted] [status (:status receipt)]))
            (is (= 0 (:offset (window runtime))) "A visible strip does not move the window"))
          (is (= :rejected (:status (second (post! {:op :control-surface/bank :surface-id "xt" :reveal [:cue :nope]}))))))
        (testing "one fader per unit writes the strip it shows"
          (input! :main [144 104 127])
          (input! :main [224 0 64])
          (input! :main [128 104 0])
          (input! :ext [144 104 127])
          (input! :ext [224 127 127])
          (input! :ext [128 104 0])
          (let [levels (into {} (map (juxt :id :level)) (:manual (show/effect-group-channels runtime)))]
            (is (= [0.5 1.0] [(double (/ (Math/round (* 1000 (get levels [:manual :g0]))) 1000)) (get levels [:manual :g8])]))))
        (testing "a saved show never carries the live window"
          (let [saved (:model (show/export-show runtime))]
            (is (map? saved))
            (is (nil? (get-in saved [:extensions ::surface/surfaces]))))
          (is (some? (surface/surface (:model @(:state runtime)) "xt"))))
        (testing "closing the extender shrinks the window to 8; reopening restores 16"
          (show/disconnect-surface-unit! runtime "xt" "xt-ext")
          (tick!)
          (is (= {:offset 0 :width 8 :count 20 :visible (mapv vector (range 8) (range 8))} (window runtime)))
          (is (some #{:closed} (get @(:sent h) :ext)))
          (is (= (vec (repeat 8 "")) (labels h :ext)) "The extender LCD is left blank")
          (bank! 47)
          (is (= 8 (:offset (window runtime))) "Banking steps by the connected width")
          (is (= ["g8" "g9" "g10" "g11" "C0" "C1" "C2" "C3"] (labels h :main)))
          (show/reconnect-surface-unit! runtime "xt" "xt-ext")
          (tick!)
          (refresh! group)
          (is (= {:offset 8 :width 16 :count 20 :visible (mapv vector (range 12) (range 8 20))} (window runtime)))
          (is (= ["C4" "C5" "C6" "C7" "" "" "" ""] (labels h :ext))))
        (testing "disconnecting the surface closes both units and drops the session box"
          (show/disconnect-device! runtime "xt")
          (tick!)
          (is (= [] (get-in (show/snapshot runtime) [:control-surfaces :surfaces])))
          (is (= [:closed :closed] (mapv #(last (get @(:sent h) %)) [:main :ext])))
          (is (= (vec (repeat 8 "")) (labels h :main)))))
      (finally (show/stop! runtime)))))

(deftest surface-settings-home-validates-follow-and-reaches-the-snapshot
  (let [{:keys [runtime] :as h} (harness)]
    (try
      (is (= {:follow-selection? false} (get-in (show/snapshot runtime) [:control-surfaces :settings])))
      (is (= :accepted (:status (submit! h :settings {:scope :controllers :values {:x-touch {:follow-selection? true}}}))))
      (is (= {:follow-selection? true} (get-in (show/snapshot runtime) [:control-surfaces :settings])))
      (is (= {:follow-selection? true} (get-in (studio/stream-view runtime @(:state runtime)) [:control-surfaces :settings])))
      (is (= :rejected (:status (submit! h :settings {:scope :controllers :values {:x-touch {:follow-selection? "yes"}}}))))
      (is (= :rejected (:status (submit! h :command {:op :control-surface/bank :surface-id "missing" :move :next}))))
      (is (thrown? clojure.lang.ExceptionInfo
                   (show/connect-device! runtime :x-touch {:session-id "bad" :schedule! nil
                                                           :units [{:session-id "a" :extender? true}]})))
      (finally (show/stop! runtime)))))

(deftest a-restart-drops-attached-surfaces
  (let [model (-> (model)
                  (assoc :show-id "s" :epoch 1)
                  (as-> m (:model (surface/apply-operation m {:op :control-surface/attach :surface-id "xt" :kind :x-touch
                                                              :units [{:session-id "m" :extender? false :offset 0}]}))))]
    (is (some? (surface/surface model "xt")))
    (is (nil? (surface/surface (surface/clear model) "xt")))
    (is (= 0 (surface/offset model "xt")))
    (is (= {:offset 0 :width 8 :strip-count 0 :limit 0} (select-keys (surface/window model (surface/surface model "xt"))
                                                                         [:offset :width :strip-count :limit])))))
