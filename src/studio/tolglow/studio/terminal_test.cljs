(ns tolglow.studio.terminal-test
  (:refer-clojure :exclude [key])
  (:require [cljs.test :refer-macros [deftest is]]
            [re-frame.db :as rf-db]
            [re-frame.interceptor :as interceptor]
            [re-frame.registrar :as registrar]
            [tolglow.studio.terminal :as terminal]))

(def key "workspace/tolglow")
(def base {:show {:runtime-id "server-one" :epoch 0}
           :terminals {key (merge terminal/defaults {:mounted? true :session-id "owned-pty" :status "running"})}})
(defn transition [db event]
  (reset! rf-db/app-db db)
  (:effects (interceptor/execute event (remove #(= :do-fx (:id %))
                                               (registrar/get-handler :event (first event))))))

(deftest unicode-terminal-input-is-bounded-and-does-not-split-surrogates
  (let [text (str (apply str (repeat 4095 "x")) "🦆λ")
        prefix (terminal/input-prefix text)]
    (is (= 4095 (count prefix)))
    (is (= "🦆λ" (subs text (count prefix))))
    (is (= text (.decode (js/TextDecoder.) (terminal/decode (terminal/encode text))))))
  (is (thrown? js/Error (terminal/append-input (apply str (repeat 65536 "x")) "y")))
  (is (thrown? js/Error (terminal/append-input "" (apply str (repeat 32769 "λ"))))))

(deftest ordered-input-never-replays-after-an-uncertain-acknowledgement
  (let [queued (transition base [:terminal/input key "echo one\n"])
        sent (transition (:db queued) (:dispatch queued))
        additional (transition (:db sent) [:terminal/input key "echo two\n"])
        blocked (transition (:db additional) [:terminal/send key])
        failed (transition (:db additional) [:terminal/failed key (terminal/token (:db additional) key) :input "Network failed"])]
    (is (= "echo one\n" (.decode (js/TextDecoder.) (terminal/decode (get-in sent [:studio/http :body :data])))))
    (is (nil? (:studio/http blocked)))
    (is (= "" (:input (terminal/state (:db failed) key))))
    (is (false? (:sending? (terminal/state (:db failed) key))))
    (is (nil? (:dispatch-later failed)))))

(deftest polling-is-owned-and-terminal-tail-drains-after-process-exit
  (let [sent (transition base [:terminal/poll key])
        overlap (transition (:db sent) [:terminal/poll key])
        captured (terminal/token base key)
        first (transition (:db sent) [:terminal/output key captured {:state "exited" :cursor 100 :available-cursor 150
                                                                    :data "eA==" :dropped? true}])
        tail (transition (:db first) [:terminal/poll key])
        final (transition (:db tail) [:terminal/output key captured {:state "exited" :cursor 150 :available-cursor 150 :data "eQ=="}])]
    (is (some? (:studio/http sent)))
    (is (nil? (:studio/http overlap)))
    (is (:dropped? (:terminal/write first)))
    (is (some? (:studio/http tail)))
    (is (= 150 (:cursor (terminal/state (:db final) key))))
    (is (nil? (:dispatch-later final)))))

(deftest remount-and-runtime-replacement-reject-stale-output
  (let [old (terminal/token base key)
        unmounted (:db (transition base [:terminal/unmount key]))
        mounted (:db (transition unmounted [:terminal/mount key]))
        stale (transition mounted [:terminal/output key old {:state "running" :cursor 999 :data "eA=="}])
        reset (:db (transition base [:terminal/runtime-change]))]
    (is (nil? (:terminal/write stale)))
    (is (= 0 (:cursor (terminal/state mounted key))))
    (is (= "owned-pty" (:session-id (terminal/state mounted key))))
    (is (nil? (:session-id (terminal/state reset key))))))
