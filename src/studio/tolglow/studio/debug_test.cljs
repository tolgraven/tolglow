(ns tolglow.studio.debug-test
  (:require [cljs.test :refer-macros [deftest is]]
            [tolglow.studio.debug :as debug]
            [tolglow.studio.subscriber-editor :as editor]
            [tolglow.studio.events-test :refer [transition]]
            [tolglow.transit :as wire]))

(defn row [id] {:id id :type :audio/kick :category :audio :emitter {:id :audio/in} :subscribers [:fx/hit]})

(defn event-batch [runtime n rows] {:runtime-id runtime :protocol "tolglow-debug/1" :kind :events :cursor n :skipped 0 :events rows})

(deftest bounded-feed-retains-real-rows-and-recovers-cursor
  (let [first-batch (event-batch "run" 400 (mapv row (range 1 401)))
        state (debug/accept-events {} first-batch)
        repeated (debug/accept-events state first-batch)
        next (debug/accept-events repeated (assoc (event-batch "run" 420 [(row 419) (row 420)]) :skipped 16))
        replacement (debug/accept-events (assoc next :frozen [1] :selected 4) (event-batch "new" 1 [(row 1)]))]
    (is (= debug/retention (count (:rows state))))
    (is (= state repeated))
    (is (= [419 420] (mapv :id (take-last 2 (:rows next)))))
    (is (= 16 (:skipped next)))
    (is (= [(row 1)] (:rows replacement)))
    (is (nil? (:frozen replacement)))
    (is (nil? (:selected replacement)))
    (is (debug/matches? {:category "audio" :type "audio/kick" :emitter ":audio/in" :subscribers "fx/hit"} (row 1)))
    (is (false? (debug/matches? {:subscribers "fx/other"} (row 1))))))

(deftest emitter-filters-preserve-typed-ids
  (let [with-emitter #(assoc-in (row 1) [:emitter :id] %)
        keyword-row (with-emitter :rig/spot)
        string-row (with-emitter "rig/spot")
        empty-string-row (with-emitter "")
        missing-emitter-row (with-emitter nil)
        combined {:type "audio/kick" :category "audio" :emitter ":rig/spot" :subscribers "fx/hit"}]
    (is (debug/matches? combined keyword-row))
    (is (false? (debug/matches? combined string-row)))
    (is (debug/matches? (assoc combined :emitter "\"rig/spot\"") string-row))
    (is (false? (debug/matches? (assoc combined :emitter "\"rig/spot\"") keyword-row)))
    (is (false? (debug/matches? (assoc combined :category "clock") keyword-row)))
    (is (false? (debug/matches? (assoc combined :subscribers "fx/other") keyword-row)))
    (is (debug/matches? {:emitter ""} missing-emitter-row))
    (is (false? (debug/matches? combined missing-emitter-row)))
    (is (debug/matches? {:emitter "\"\""} empty-string-row))
    (is (false? (debug/matches? {:emitter "\"\""} missing-emitter-row)))))

(deftest hiding-filter-controls-keeps-the-frozen-selection
  (let [initial {:debug {:rows [(row 1)] :frozen [(row 1)] :filters {:category "audio"} :limit 100}}
        open (:db (transition initial [:debug/toggle-filters]))
        closed (:db (transition open [:debug/toggle-filters]))
        cleared (:db (transition closed [:debug/clear-filters]))]
    (is (true? (get-in open [:debug :filters-open?])))
    (is (false? (get-in closed [:debug :filters-open?])))
    (is (= {:category "audio"} (get-in closed [:debug :filters])))
    (is (= {} (get-in cleared [:debug :filters])))
    (is (= 50 (get-in cleared [:debug :limit])))
    (is (= [(row 1)] (get-in cleared [:debug :frozen])))))

(deftest delayed-disconnected-socket-and-unselected-dmx-cannot-overwrite-current-view
  (let [db {:debug {:handle "tab" :generation 3 :runtime "run" :universe 2 :rows []}}
        packet {:protocol "tolglow-debug/1" :kind :dmx :runtime-id "run" :universe 2 :token "t" :frame-number 4
                :bytes (js/Uint8Array. 512)}
        frame (transition db [:debug/frame "tab" 3 (wire/decode (wire/encode packet))])
        stale (transition db [:debug/frame "tab" 2 packet])
        previous (transition db [:debug/frame "tab" 3 (assoc packet :universe 1)])]
    (is (= 512 (.-length (get-in frame [:db :debug :packet :bytes]))))
    (is (= {:op :dmx/ack :token "t"} (nth (:debug/send frame) 2)))
    (is (nil? (:db stale)))
    (is (nil? (:db previous)))
    (is (= :dmx/ack (get-in previous [:debug/send 2 :op])))))

(deftest paused-view-keeps-an-independent-live-replay-cursor
  (let [start (debug/accept-events {:handle "tab" :generation 1} (event-batch "run" 1 [(row 1)]))
        paused (:db (transition {:debug start} [:debug/freeze]))
        arrived (transition paused [:debug/frame "tab" 1 (event-batch "run" 2 [(row 2)])])
        resumed (:db (transition (:db arrived) [:debug/freeze]))]
    (is (= [(row 1)] (get-in arrived [:db :debug :frozen])))
    (is (= 2 (get-in arrived [:debug/send 2 :cursor])))
    (is (= 2 (count (get-in resumed [:debug :rows]))))
    (is (nil? (get-in resumed [:debug :frozen])))))

(deftest subscriber-actions-keep-order-selection-and-typed-ids
  (let [a (assoc (editor/new-action :cue/launch) :select {:pick :random :categories [:color] :page-id :main})
        b (assoc (editor/new-action :instance/opacity) :delay {:bars "2"} :value 0.5)
        form {:id "live/kick" :enabled? true :match {:type :audio/kick :category nil} :actions [a b]}
        clean (editor/clean-form form)]
    (is (= :live/kick (:id clean)))
    (is (= {:type :audio/kick} (:match clean)))
    (is (= [a (assoc b :delay {:bars 2})] (:actions clean)))
    (is (wire/fraction? (get-in (editor/clean-form (assoc-in form [:actions 1 :delay] {:beats "1/3"})) [:actions 1 :delay :beats])))
    (is (= [b a] (editor/move-step [a b] 0 1)))
    (is (= [a] (editor/move-step [a b] 1 :remove)))
    (is (thrown? js/Error (editor/clean-form (assoc form :actions []))))
    (is (thrown? js/Error (editor/clean-form (assoc-in form [:actions 1 :delay] {:beats "-1"}))))))

(deftest a-track-pattern-selector-keeps-its-type-and-clears-for-unrelated-events
  (let [form {:id "music/slow" :match {:type :music/section-pattern :pattern ":slow"}
              :actions [(editor/new-action :cue/launch)]}]
    (is (= {:type :music/section-pattern :pattern :slow} (:match (editor/clean-form form))))
    (is (= "slow" (get-in (editor/clean-form (assoc-in form [:match :pattern] "slow")) [:match :pattern])))
    (is (= "slow groove" (get-in (editor/clean-form (assoc-in form [:match :pattern] " slow groove ")) [:match :pattern])))
    (is (= {:type :music/section-pattern} (:match (editor/clean-form (assoc-in form [:match :pattern] "  ")))))
    (is (= {:type :audio/kick} (:match (editor/clean-form (assoc-in form [:match :type] :audio/kick)))))))

(deftest an-operator-can-enter-numeric-and-uuid-patterns-without-changing-text-ids
  (let [form {:id :music/pattern :match {:type :music/section-pattern}
              :actions [(editor/new-action :cue/launch)]}
        path [:match :pattern]
        pattern (fn [source] (-> form (assoc-in path source) editor/clean-form (get-in path)))
        id "516049b9-ff71-4bf4-bf00-123456789abc"
        long "9223372036854775807"
        long-id (wire/decode (str "[\"~#'\",\"~i" long "\"]"))
        wide "9223372036854775808"
        wide-id (wire/decode (str "[\"~#'\",\"~n" wide "\"]"))]
    (is (= 514 (pattern "514")))
    (is (= -514 (pattern "-514")))
    (is (= long-id (wire/decode (wire/encode (pattern long)))))
    (is (= wide-id (wire/decode (wire/encode (pattern wide)))))
    (is (= (uuid id) (wire/decode (wire/encode (pattern (str "#uuid \"" id "\""))))))
    (is (= (uuid id) (pattern (str " #uuid \"" id "\" "))))
    (is (thrown? js/Error (pattern "#uuid \"invalid\"")))
    (doseq [stored [514 long-id wide-id (uuid id) "514" ":slow" "#uuid \"text\"" " trailing "]]
      (let [db (:db (transition {:debug {:handle "tab"}}
                                [:subscribers/edit (assoc-in form path stored)]))
            reopened (get-in db [:debug :form])]
        (is (= stored (get-in (editor/clean-form reopened) path)))))))

(deftest late-subscriber-edit-cannot-target-replaced-show
  (let [db {:show {:runtime-id "new" :epoch 1}
            :debug {:handle "page" :editing-show {:runtime-id "old" :epoch 0}
                    :form {:id :s :match {} :actions [(editor/new-action :cue/launch)]}}}
        result (transition db [:subscribers/submit :save])]
    (is (string? (get-in result [:db :debug :catalog-error])))
    (is (nil? (:subscribers/http result)))))

(deftest replacing-show-clears-pending-subscriber-and-reloads-visible-catalog
  (let [before {:show {:runtime-id "replacement" :epoch 2}
                :debug {:handle "debug" :saving? true :catalog {:stale true} :form {:id :old}}}
        result (transition before [:subscribers/runtime-change])
        stale ["debug" {:runtime-id "old" :epoch 1}]]
    (is (= {:handle "debug"} (get-in result [:db :debug])))
    (is (= [:subscribers/load] (:dispatch result)))
    (is (= (:db result) (:db (transition (:db result) [:subscribers/error stale "late"]))))
    (is (nil? (:dispatch (transition {} [:subscribers/runtime-change]))))))
