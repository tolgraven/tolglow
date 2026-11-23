(ns tolglow.studio.terminal-test
  (:refer-clojure :exclude [key])
  (:require [cljs.test :refer-macros [deftest is]]
            [re-frame.db :as rf-db]
            [re-frame.interceptor :as interceptor]
            [re-frame.registrar :as registrar]
            [tolglow.studio.terminal :as terminal]))

(def key "workspace/tolglow")
(def base {:show {:runtime-id "server-one" :epoch 0}
           :terminals {key (merge terminal/defaults {:mounted? true :session-id "owned-pty" :workspace-id "workspace" :repo :tolglow :status "running"})}})
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

(deftest subscribed-tail-drains-after-exit-and-an-expired-page-is-not-written-twice
  (let [capture (terminal/token base key)
        send (fn [db cursor next available]
               (transition db [:terminal/output key capture cursor
                               {:value {:session-id "owned-pty" :state "exited" :cursor next
                                        :available-cursor available :data "eA==" :dropped? true}}]))
        first (send base 0 100 150)
        duplicate (send (:db first) 0 100 150)
        final (send (:db first) 100 150 150)]
    (is (= [:development/terminal-output "workspace" :tolglow "owned-pty" 0]
           (terminal/output-query (terminal/state base key))))
    (is (:dropped? (:terminal/write first)))
    (is (nil? (:terminal/write duplicate)))
    (is (= 100 (peek (terminal/output-query (terminal/state (:db first) key)))))
    (is (= 150 (:cursor (terminal/state (:db final) key))))
    (is (nil? (terminal/output-query (terminal/state (:db final) key))))))

(deftest remount-and-runtime-replacement-reject-stale-output
  (let [old (terminal/token base key)
        unmounted (:db (transition base [:terminal/unmount key]))
        mounted (:db (transition unmounted [:terminal/mount key]))
        stale (transition mounted [:terminal/output key old 0 {:value {:session-id "owned-pty" :state "running" :cursor 999 :data "eA=="}}])
        reset (:db (transition base [:terminal/runtime-change]))]
    (is (nil? (:terminal/write stale)))
    (is (= 0 (:cursor (terminal/state mounted key))))
    (is (= "owned-pty" (:session-id (terminal/state mounted key))))
    (is (nil? (:session-id (terminal/state reset key))))))
