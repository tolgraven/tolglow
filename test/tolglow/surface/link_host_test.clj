(ns tolglow.surface.link-host-test
  "Virtual replay of a linked X-Touch + X-Touch-Ext surface through the MIDI
  transport boundary against a real show owner (#705 slice 1)."
  (:require [afterglow.control.model :as control]
            [afterglow.control.devices.session :as device-session]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [tolglow.surface.link :as surface]
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

(defn- surfaces [runtime] (:control-surfaces (show/snapshot runtime)))

(defn- logical [runtime id]
  (some #(when (= id (:id %)) %) (:logical (surfaces runtime))))

(defn- member [runtime id]
  (some (fn [l] (some #(when (= id (:surface-id %)) %) (:members l))) (:logical (surfaces runtime))))

(defn- shown
  "[window-x [first-index last-index]] for one attached surface's strips."
  [runtime id]
  (let [{:keys [window cells]} (member runtime id)]
    [(:x window) (when (seq cells) [(:x (first cells)) (:x (peek cells))])]))

(defn- connect! [{:keys [runtime transport]} id unit options]
  (show/connect-device! runtime :x-touch
                        (merge {:session-id id :schedule! nil :transport (transport unit)} options)))

(def ^:private main-options
  {:effect-group-view :manual :channels (mapv #(hash-map :cue-id %) cue-ids)})

(defn- studio-post [{:keys [runtime owner!]}]
  (let [app (web/app {:heartbeat! (constantly nil)
                      :submit! (fn [operation payload] (show/submit! runtime operation payload))})]
    (fn [payload]
      (let [response (owner! #(app {:request-method :post :uri "/api/command"
                                    :headers {"host" "localhost" "accept" "application/transit+json"
                                              "content-type" "application/transit+json"}
                                    :body (StringReader. (transit/encode payload))}))]
        [(:status response) (transit/decode (:body response))]))))

(defn- configure-groups! [h]
  (is (= :accepted (:status (submit! h :command {:op :effect-groups/configure
                                                  :config {:manual (mapv #(hash-map :id % :name (name %)) group-ids)}})))))

(deftest two-independent-surfaces-extend-into-one-session-box
  (let [{:keys [runtime input! tick! owner!] :as h} (harness)
        post! (studio-post h)]
    (try
      (configure-groups! h)
      (let [main (connect! h "xt-main" :main main-options)
            ext (connect! h "xt-ext" :ext {:extender? true})
            refresh! #(run! device-session/refresh! [main ext])
            bank! (fn [& notes] (doseq [note notes] (input! :main [144 note 127]) (input! :main [128 note 0])) (refresh!))]
        (tick!)
        (refresh!)
        (testing "each unit is its own surface until linked"
          (is (= [[0 [0 7]] [0 nil]] [(shown runtime "xt-main") (shown runtime "xt-ext")])
              "The unlinked extender has no strips of its own")
          (is (= #{"xt-main" "xt-ext"} (set (map :id (:logical (surfaces runtime))))))
          (is (= (mapv name (take 8 group-ids)) (labels h :main))))
        (testing "an extend link places the extender one bank to the right"
          (is (= :accepted (:status (owner! #(deref (show/link-surfaces! runtime "desk" ["xt-main" "xt-ext"]) 2000 ::missing)))))
          (tick!)
          (refresh!)
          (is (= {:id "desk" :link? true :viewport :strips :mode :extend :window {:x 0 :y 0} :span {:columns 16 :rows 1}
                  :size {:columns 20 :rows 1} :limit {:x 16 :y 0}}
                 (dissoc (logical runtime "desk") :members)))
          (is (= [[0 [0 7]] [8 [8 15]]] [(shown runtime "xt-main") (shown runtime "xt-ext")]))
          (is (= ["g8" "g9" "g10" "g11" "C0" "C1" "C2" "C3"] (labels h :ext)) "The extender shows the main unit's strips"))
        (testing "bank right pages by 16; the 4-strip remainder lands on the main unit"
          (bank! 47)
          (is (= [[16 [16 19]] [24 nil]] [(shown runtime "xt-main") (shown runtime "xt-ext")]))
          (is (= ["C4" "C5" "C6" "C7" "" "" "" ""] (labels h :main)))
          (is (= (vec (repeat 8 "")) (labels h :ext)))
          (bank! 47)
          (is (= 16 (get-in (logical runtime "desk") [:window :x])) "The last page is the limit"))
        (testing "SHIFT jumps to the first and last bank; channel buttons step one strip"
          (input! :main [144 70 127])
          (bank! 46)
          (is (= 0 (get-in (logical runtime "desk") [:window :x])))
          (bank! 47)
          (is (= 16 (get-in (logical runtime "desk") [:window :x])))
          (input! :main [128 70 0])
          (bank! 48)
          (is (= [[15 [15 19]] [23 nil]] [(shown runtime "xt-main") (shown runtime "xt-ext")]))
          (is (= ["C3" "C4" "C5" "C6" "C7" "" "" ""] (labels h :main)))
          (bank! 46))
        (testing "Studio moves the same window and the hardware follows"
          (is (= 0 (get-in (logical runtime "desk") [:window :x])))
          (is (= 200 (first (post! {:op :control-surface/bank :target "desk" :move :last}))))
          (refresh!)
          (is (= ["C4" "C5" "C6" "C7" "" "" "" ""] (labels h :main)))
          (is (= 200 (first (post! {:op :control-surface/bank :target "xt-ext" :reveal [:group [:manual :g2]]}))))
          (refresh!)
          (is (= "g0" (first (labels h :main))) "Reveal through a member moves its link to the page holding the strip")
          (let [[status receipt] (post! {:op :control-surface/bank :target "desk" :reveal [:cue :c1]})]
            (is (= [200 :accepted] [status (:status receipt)]))
            (is (= 0 (get-in (logical runtime "desk") [:window :x])) "A visible strip does not move the window"))
          (is (= :rejected (:status (second (post! {:op :control-surface/bank :target "desk" :reveal [:cue :nope]}))))))
        (testing "one fader per unit writes the strip it shows"
          (input! :main [144 104 127])
          (input! :main [224 0 64])
          (input! :main [128 104 0])
          (input! :ext [144 104 127])
          (input! :ext [224 127 127])
          (input! :ext [128 104 0])
          (let [levels (into {} (map (juxt :id :level)) (:manual (show/effect-group-channels runtime)))]
            (is (< (Math/abs (- 0.5 (get levels [:manual :g0]))) 0.001))
            (is (= 1.0 (get levels [:manual :g8])))))
        (testing "touch ownership stays with the member across a bank change"
          (input! :main [144 104 127])
          (is (= 200 (first (post! {:op :control-surface/bank :target "desk" :move :last}))))
          (input! :main [224 0 32])
          (input! :main [128 104 0])
          (let [levels (into {} (map (juxt :id :level)) (:manual (show/effect-group-channels runtime)))]
            (is (< (Math/abs (- 0.25 (get levels [:manual :g0]))) 0.001) "The held fader still writes g0")
            (is (= 1.0 (get levels [:manual :g8])) "The other member's strip is untouched"))
          (is (= 200 (first (post! {:op :control-surface/bank :target "desk" :move :first}))))
          (refresh!))
        (testing "the link is saved with the show, but never as connected"
          (let [saved (:model (show/export-show runtime))]
            (is (map? saved))
            (is (= #{false} (set (map :attached? (vals (get-in saved [:extensions ::surface/state :surfaces]))))))
            (is (contains? (get-in saved [:extensions ::surface/state :links]) "desk"))))
        (testing "disconnecting the extender shrinks the window to 8; reconnecting rejoins the link"
          (show/disconnect-device! runtime "xt-ext")
          (tick!)
          (is (= {:columns 8 :rows 1} (:span (logical runtime "desk"))))
          (is (some #{:closed} (get @(:sent h) :ext)))
          (is (= (vec (repeat 8 "")) (labels h :ext)) "The extender LCD is left blank")
          (bank! 47)
          (is (= [8 [8 15]] (shown runtime "xt-main")) "Banking steps by the connected width")
          (let [ext (connect! h "xt-ext" :ext {:extender? true})]
            (tick!)
            (device-session/refresh! ext)
            (refresh!)
            (is (= [[8 [8 15]] [16 [16 19]]] [(shown runtime "xt-main") (shown runtime "xt-ext")]))
            (is (= ["C4" "C5" "C6" "C7" "" "" "" ""] (labels h :ext)))
            (show/disconnect-device! runtime "xt-ext")))
        (testing "a mirror link shows the same window on both units"
          (connect! h "xt-ext" :ext {:extender? true})
          (is (= :accepted (:status (owner! #(deref (show/link-surfaces! runtime "desk" ["xt-main" "xt-ext"] {:mode :mirror}) 2000 ::missing)))))
          (tick!)
          (is (= :mirror (:mode (logical runtime "desk"))))
          (is (= [[8 [8 15]] [8 [8 15]]] [(shown runtime "xt-main") (shown runtime "xt-ext")]))
          (bank! 47)
          (is (= [[16 [16 19]] [16 [16 19]]] [(shown runtime "xt-main") (shown runtime "xt-ext")]) "Mirror pages by one unit"))
        (testing "unlinking leaves each surface where it was"
          (is (= :accepted (:status (owner! #(deref (show/unlink-surfaces! runtime "desk") 2000 ::missing)))))
          (tick!)
          (is (= [[16 [16 19]] [0 nil]] [(shown runtime "xt-main") (shown runtime "xt-ext")])
              "The extender's own empty strip list clamps its window to 0")))
      (finally (show/stop! runtime)))))

(deftest a-left-extend-and-studio-link-commands-share-the-reducer
  (let [{:keys [runtime tick!] :as h} (harness)
        post! (studio-post h)]
    (try
      (configure-groups! h)
      (connect! h "xt-main" :main main-options)
      (connect! h "xt-ext" :ext {:extender? true})
      (tick!)
      (is (= 200 (first (post! {:op :control-surface/link :link-id "desk"
                                :members [{:surface-id "xt-main" :at {:x 0 :y 0}}
                                          {:surface-id "xt-ext" :at {:x -1 :y 0}}]}))))
      (is (= [[8 [8 15]] [0 [0 7]]] [(shown runtime "xt-main") (shown runtime "xt-ext")])
          "The extender to the left covers the first bank")
      (testing "invalid links are rejected and leave the show alone"
        (doseq [members [[{:surface-id "xt-main" :at {:x 0 :y 0}}]
                         [{:surface-id "xt-main" :at {:x 0 :y 0}} {:surface-id "nope" :at {:x 1 :y 0}}]
                         [{:surface-id "xt-main" :at {:x 0 :y 0}} {:surface-id "xt-main" :at {:x 1 :y 0}}]]]
          (is (= :rejected (:status (second (post! {:op :control-surface/link :link-id "other" :members members})))))))
      (is (= :rejected (:status (second (post! {:op :control-surface/link :link-id "other"
                                                :members [{:surface-id "xt-ext" :at {:x 0 :y 0}}
                                                          {:surface-id "xt-main" :at {:x 1 :y 0}}]}))))
          "A surface belongs to one link")
      (is (= :rejected (:status (second (post! {:op :control-surface/link :link-id "xt-main"
                                                :members [{:surface-id "xt-ext" :at {:x 0 :y 0}}
                                                          {:surface-id "xt-main" :at {:x 1 :y 0}}]}))))
          "Link IDs and surface IDs do not collide")
      (is (= 200 (first (post! {:op :control-surface/remove :surface-id "xt-ext"}))))
      (is (nil? (logical runtime "desk")) "Removing a member of a two-surface link dissolves it")
      (is (= [8 [8 15]] (shown runtime "xt-main")))
      (finally (show/stop! runtime)))))

(deftest surface-settings-home-validates-follow-and-reaches-the-snapshot
  (let [{:keys [runtime] :as h} (harness)]
    (try
      (is (= {:follow-selection? false} (:settings (surfaces runtime))))
      (is (= :accepted (:status (submit! h :settings {:scope :controllers :values {:surfaces {:follow-selection? true}}}))))
      (is (= {:follow-selection? true} (:settings (surfaces runtime))))
      (is (= {:follow-selection? true} (get-in (studio/stream-view runtime @(:state runtime)) [:control-surfaces :settings])))
      (is (= :rejected (:status (submit! h :settings {:scope :controllers :values {:surfaces {:follow-selection? "yes"}}}))))
      (is (= :rejected (:status (submit! h :command {:op :control-surface/bank :target "missing" :move :next}))))
      (is (thrown? clojure.lang.ExceptionInfo
                   (show/connect-device! runtime :x-touch {:session-id "bad" :schedule! nil
                                                           :channels [{:label "no cue"}]})))
      (finally (show/stop! runtime)))))

(deftest restarting-or-saving-keeps-links-and-detaches-surfaces
  (let [attach (fn [m id descriptor]
                 (:model (surface/apply-operation m {:op :control-surface/attach :surface-id id :descriptor descriptor
                                                     :role :unit
                                                     :viewports {:strips {:type :mixer-strips
                                                                          :cue-strips (mapv #(hash-map :cue-id %) cue-ids)}}})))
        linked (-> (model) (attach "m" :behringer/x-touch) (attach "e" :behringer/x-touch-ext)
                   (as-> m (:model (surface/apply-operation m {:op :control-surface/link :link-id "desk"
                                                              :members (surface/link-members ["m" "e"] {})}))))
        restarted (surface/detach-all linked)]
    (is (= 8 (surface/strip-offset linked "e")))
    (is (= 8 (count (surface/strip-channels linked "e"))) "The extender banks over the anchor's strips")
    (is (= {:logical-id "desk" :at [11 0]} (surface/logical-position linked "e" :strips [3 0]))
        "Linking remaps member coordinates into the logical surface")
    (is (= #{false} (set (map :attached? (:surfaces (surface/projection restarted))))))
    (is (= ["desk"] (mapv :id (filter :link? (:logical (surface/projection restarted))))))
    (is (= restarted (surface/validate-state! restarted)))
    (is (thrown? clojure.lang.ExceptionInfo
                 (surface/validate-state! (assoc-in restarted [:extensions ::surface/state :links "desk" :members 1 :surface-id] "gone"))))
    (is (thrown? clojure.lang.ExceptionInfo
                 (surface/apply-operation linked {:op :control-surface/attach :surface-id "p" :descriptor :nope
                                                  :role :unit :viewports {}})))
    (is (= [{:surface-id "a" :at {:x 0 :y 0}} {:surface-id "b" :at {:x 0 :y -1}}]
           (surface/link-members ["a" "b"] {:direction :up})))
    (is (= [{:surface-id "a" :at {:x 0 :y 0}} {:surface-id "b" :at {:x 1 :y 0}}]
           (surface/link-members ["a" "b"] {:mode :mirror :offset {:x 1 :y 0}}))
        "Mirror with an offset follows one page to the right")))
