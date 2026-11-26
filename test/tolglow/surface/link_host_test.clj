(ns tolglow.surface.link-host-test
  "Virtual replay of a linked X-Touch + X-Touch-Ext surface through the MIDI
  transport boundary against a real show owner (#705 slice 1)."
  (:require [afterglow.control.model :as control]
            [afterglow.control.devices.session :as device-session]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [tolglow.persistence :as persistence]
            [tolglow.surface.descriptor :as descriptor]
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
          (is (= {:id "desk" :link? true :viewport :strips :mode :extend :direction :right :window {:x 0 :y 0} :span {:columns 16 :rows 1}
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
      (is (= 200 (first (post! {:op :control-surface/link :link-id "desk" :mode :extend :direction :left
                                :members [{:surface-id "xt-main"} {:surface-id "xt-ext"}]}))))
      (is (= [[8 [8 15]] [0 [0 7]]] [(shown runtime "xt-main") (shown runtime "xt-ext")])
          "The extender to the left covers the first bank")
      (testing "invalid links are rejected and leave the show alone"
        (doseq [command [{:members [{:surface-id "xt-main"}]}
                         {:members [{:surface-id "xt-main"} {:surface-id "nope"}]}
                         {:members [{:surface-id "xt-main"} {:surface-id "xt-main"}]}
                         {:mode :sideways :members [{:surface-id "xt-main"} {:surface-id "xt-ext"}]}
                         {:members [{:surface-id "xt-main" :viewport :grid} {:surface-id "xt-ext"}]}]]
          (is (= :rejected (:status (second (post! (merge {:op :control-surface/link :link-id "other"} command)))))
              (pr-str command))))
      (is (= :rejected (:status (second (post! {:op :control-surface/link :link-id "other"
                                                :members [{:surface-id "xt-ext"} {:surface-id "xt-main"}]}))))
          "A surface viewport belongs to one link")
      (is (= :rejected (:status (second (post! {:op :control-surface/link :link-id "xt-main"
                                                :members [{:surface-id "xt-ext"} {:surface-id "xt-main"}]}))))
          "Link IDs and surface IDs do not collide")
      (testing "a connected surface belongs to its device"
        (let [revision (:revision (show/snapshot runtime))]
          (doseq [command [{:op :control-surface/remove :surface-id "xt-ext"}
                           {:op :control-surface/attach :surface-id "xt-ext" :descriptor :behringer/x-touch :role :main
                            :viewports {:strips {:type :none}}}
                           {:op :control-surface/detach :surface-id "nope"}
                           {:op :control-surface/remove :surface-id "nope"}]]
            (is (= :rejected (:status (second (post! command)))) (pr-str command)))
          (is (= revision (:revision (show/snapshot runtime))) "Refusals do not bump the revision")))
      (show/disconnect-device! runtime "xt-ext")
      (tick!)
      (is (= 200 (first (post! {:op :control-surface/remove :surface-id "xt-ext"}))))
      (is (nil? (logical runtime "desk")) "Removing a member of a two-surface link dissolves it")
      (is (= [0 [0 7]] (shown runtime "xt-main"))
          "With the left extender away the main unit was repacked to strip 1, and keeps that window")
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
                                                              :mode :extend :direction :right
                                                              :members [{:surface-id "m"} {:surface-id "e"}]}))))
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
    (testing "stored shapes the projection does arithmetic on are refused"
      (doseq [[path value] [[[:surfaces "m" :viewports :strips :geometry] nil]
                            [[:surfaces "m" :viewports :strips :geometry] [16 1]]
                            [[:surfaces "m" :viewports :bogus] {:geometry [8 1] :content {:type :none} :window {:x 0 :y 0}}]
                            [[:surfaces "m" :role] "main"]
                            [[:links "desk" :members] []]
                            [[:links "desk" :members] [{:surface-id "m" :viewport :strips}]]
                            [[:links "desk" :members 1 :viewport] :grid]
                            [[:links "desk" :mode] :sideways]]]
        (is (thrown? clojure.lang.ExceptionInfo
                     (surface/validate-state! (assoc-in restarted (into [:extensions ::surface/state] path) value)))
            (pr-str path value))))
    (is (= :invalid-link (:control/error (try (surface/apply-operation linked {:op :control-surface/link :link-id "z" :members 5})
                                              (catch clojure.lang.ExceptionInfo e (ex-data e))))))
    (is (thrown? clojure.lang.ExceptionInfo
                 (surface/apply-operation linked {:op :control-surface/attach :surface-id "p" :descriptor :nope
                                                  :role :unit :viewports {}})))
    (is (= {:mode :extend :direction :up :members [{:surface-id "a"} {:surface-id "b"}]}
           (surface/link-command ["a" "b"] {:direction :up})))
    (is (= {:mode :follow :members [{:surface-id "a"} {:surface-id "b" :offset {:x 1 :y 0}}
                                    {:surface-id "c" :offset {:x 2 :y 0}}]}
           (surface/link-command ["a" "b" "c"] {:mode :follow :offset {:x 1}}))
        "Each follower stays one more page to the right")))

(deftest loading-a-show-validates-links-and-detaches-surfaces
  (let [{:keys [runtime] :as h} (harness)]
    (try
      (doseq [id ["m" "e"]]
        (is (= :accepted (:status (submit! h :command {:op :control-surface/attach :surface-id id :descriptor :behringer/x-touch
                                                       :role :main :viewports {:strips {:type :mixer-strips :cue-strips []}}})))))
      (is (= :accepted (:status (submit! h :command (merge {:op :control-surface/link :link-id "desk"}
                                                           (surface/link-command ["m" "e"] {}))))))
      (let [document (show/export-show runtime)
            current (:model @(:state runtime))
            path [:model :extensions ::surface/state]]
        (is (= ["desk"] (keys (get-in (persistence/thaw current document) [:extensions ::surface/state :links]))))
        (is (= #{false} (set (map :attached? (vals (get-in (persistence/thaw current (assoc-in document (into path [:surfaces "m" :attached?]) true))
                                                           [:extensions ::surface/state :surfaces])))))
            "A restored show never claims a connected device")
        (doseq [[edit why] [[#(assoc-in % (into path [:links "desk" :members 1 :surface-id]) "gone") "a link member that is not saved"]
                            [#(assoc-in % (into path [:surfaces "m" :viewports :strips :geometry]) nil) "a geometry the descriptor lacks"]
                            [#(assoc-in % [:model :settings :controllers :surfaces] {:follow-selection? "yes"}) "invalid settings"]]]
          (is (thrown? clojure.lang.ExceptionInfo (persistence/thaw current (edit document))) why))
        (testing "an import that would have broken every snapshot is refused and leaves the show alone"
          ;; A hand-edited or foreign file: written without the save path's validation.
          (let [source (#'persistence/encode-document
                        (assoc-in document (into path [:links "desk" :members 1 :surface-id]) "gone"))
                receipt (submit! h :import {:source source})]
            (is (= [:rejected "Invalid control surfaces or links"] [(:status receipt) (:message receipt)]))
            (is (= ["desk"] (mapv :id (filter :link? (:logical (surfaces runtime)))))))))
      (finally (show/stop! runtime)))))

;; A 4-strip unit for width tests, registered like any other descriptor.
(defmethod descriptor/descriptor :test/four-strip [_]
  (assoc descriptor/x-touch-ext :id :test/four-strip :viewports {:strips [4 1]}
         :groups (update-vals (:groups descriptor/x-touch-ext) #(assoc % :geometry [4 (second (:geometry %))]))))

(defn- pure-attach [m id descriptor & [content]]
  (:model (surface/apply-operation m {:op :control-surface/attach :surface-id id :descriptor descriptor :role :unit
                                      :viewports (into {} (map (fn [v] [v (or content {:type :mixer-strips
                                                                                         :cue-strips (mapv #(hash-map :cue-id (keyword (str "s" %))) (range 20))})]))
                                                       (keys (:viewports (descriptor/descriptor descriptor))))})))

(defn- pure-link [m command]
  (:model (surface/apply-operation m (merge {:op :control-surface/link :link-id "desk"} command))))

(defn- windows [m & ids]
  (let [{:keys [members]} (surface/window m "desk" nil)]
    (mapv (fn [id] (:x (:window (some #(when (= id (:surface-id %)) %) members)))) ids)))

(defn- bank [m move] (:model (surface/apply-operation m {:op :control-surface/bank :target "desk" :move move})))

(deftest extend-packs-mixed-widths-without-gaps-in-either-direction
  (let [base (-> (model) (pure-attach "eight" :behringer/x-touch) (pure-attach "four" :test/four-strip))]
    (is (= [0 8] (windows (pure-link base {:members [{:surface-id "eight"} {:surface-id "four"}]}) "eight" "four"))
        "A 4-wide unit right of an 8-wide one starts at strip 9")
    (is (= [0 4] (windows (pure-link base {:members [{:surface-id "four"} {:surface-id "eight"}]}) "four" "eight"))
        "An 8-wide unit right of a 4-wide one starts at strip 5")
    (is (= [4 0] (windows (pure-link base {:direction :left :members [{:surface-id "eight"} {:surface-id "four"}]})
                          "eight" "four")))
    (is (= [8 0] (windows (pure-link base {:direction :left :members [{:surface-id "four"} {:surface-id "eight"}]})
                          "four" "eight")))
    (let [linked (pure-link base {:members [{:surface-id "eight"} {:surface-id "four"}]})]
      (is (= {:columns 12 :rows 1} (:span (surface/window linked "desk" nil))))
      (is (= [12 20] (windows (bank linked :next) "eight" "four")) "The window pages by the whole 12-strip span"))))

(deftest a-detached-member-repacks-so-the-others-reach-every-strip
  (let [linked (-> (model) (pure-attach "m" :behringer/x-touch) (pure-attach "e" :behringer/x-touch-ext)
                   (pure-link {:members [{:surface-id "m"} {:surface-id "e"}]}))
        main-out (:model (surface/apply-operation linked {:op :control-surface/detach :surface-id "m"}))]
    (is (= [0 nil] (windows main-out "e" "m")) "The extender moves to strip 1 while the main unit is away")
    (is (= [8 [16]] [(:x (:window (surface/window (bank main-out :next) "desk" nil))) (windows (bank main-out :last) "e")])
        "One unit pages by 8 and reaches strips 17-20")
    (is (= 16 (surface/strip-offset (bank main-out :last) "e")))
    (let [reveal (fn [m key] (:model (surface/apply-operation m {:op :control-surface/bank :target "desk" :reveal key})))]
      (is (= [0] (windows (reveal (bank main-out :next) [:cue :s2]) "e")) "Reveal counts only attached units"))
    (is (= [0 8] (windows (pure-attach main-out "m" :behringer/x-touch) "m" "e")) "The layout returns on reattach")))

(deftest follow-pages-by-the-leader-and-links-match-axes-not-names
  (let [base (-> (model) (pure-attach "push" :ableton/push-2 {:type :none}) (pure-attach "xt" :behringer/x-touch)
                 (pure-attach "xt2" :behringer/x-touch-ext))
        followed (pure-link base {:mode :follow :members [{:surface-id "xt"} {:surface-id "xt2" :offset {:x 1 :y 0}}]})]
    (is (= [0 8] (windows followed "xt" "xt2")))
    (is (= [8 16] (windows (bank followed :next) "xt" "xt2")) "The leader pages by its own width, not by both")
    (is (= :follow (:mode (surface/window followed "desk" nil))))
    (is (= [0 0] (windows (pure-link base {:mode :mirror :members [{:surface-id "xt"} {:surface-id "xt2"}]}) "xt" "xt2")))
    (testing "an X-Touch :strips and a Push :encoders share the :channels axis"
      (let [linked (pure-link base {:members [{:surface-id "xt"} {:surface-id "push" :viewport :encoders}]})]
        (is (= [0 8] (windows linked "xt" "push")))
        (is (= [:strips :encoders] (mapv :viewport (:members (surface/window linked "desk" nil)))))))
    (is (thrown? clojure.lang.ExceptionInfo
                 (surface/apply-operation base {:op :control-surface/link :link-id "desk"
                                                :members [{:surface-id "xt"} {:surface-id "push" :viewport :grid}]}))
        "A cue grid does not link with a channel strip")))
