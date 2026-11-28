(ns tolglow.studio.device-retirement-test
  (:require [cljs.test :refer-macros [deftest is testing]]
            ["react-dom/server" :refer [renderToStaticMarkup]]
            [re-frame.db :as db]
            [re-frame.interceptor :as interceptor]
            [re-frame.registrar :as registrar]
            [reagent.core :as r]
            [tolglow.studio.events]
            [tolglow.studio.device-retirement :as retirement]
            [tolglow.studio.projection :as projection]))

(defn- transition [state event]
  (reset! db/app-db state)
  (:effects (interceptor/execute event (remove #(= :do-fx (:id %))
                                               (registrar/get-handler :event (first event))))))

;; The status as the state stream installs it: show/snapshot's map through
;; Studio's presentation, so keywords read as strings and sets as vectors.
(def status
  (projection/presentation
   {:pending 1 :retired 3 :failed 0 :reopen-failed 0 :abandoned 0 :late 0 :refused 1
    :closing #{"direct-push"} :reconnectable #{"direct-push" "browser-push"}
    :last-error {:stage :reopen :message "Display process exited" :class "clojure.lang.ExceptionInfo"}
    :last-refusal {:session-id "direct-push" :code :device/closing :message "Previous device session is still closing"}}))

(def base {:show {:runtime-id "server-a" :epoch 4 :status {:device-retirement status}}
           :connection {:connected? true}})

;; What the Studio route answers for a refused reconnect, decoded as http! does.
(def rejection
  (projection/presentation {:message "Previous device session is still closing"
                            :details {:session-id "direct-push" :code :device/closing}}))

(deftest sessions-list-closing-and-reconnectable-ids-once
  (is (= [{:id "browser-push" :closing? false :reconnectable? true}
          {:id "direct-push" :closing? true :reconnectable? true}]
         (retirement/sessions status)))
  (is (= [] (retirement/sessions nil))))

(deftest a-refusal-reads-as-the-repl-prints-it
  (is (= {:message "Previous device session is still closing" :code "device/closing" :session-id "direct-push"}
         (retirement/refusal rejection))
      "The route's ex-data :details")
  (is (= (retirement/refusal rejection) (retirement/refusal (:last-refusal status)))
      "The projected last refusal reads the same as the route's answer")
  (is (= ":device/closing" (retirement/code-text "device/closing")))
  (is (= ":device/closing" (retirement/code-text :device/closing)))
  (is (= {:message "Request timed out"} (retirement/refusal "Request timed out"))))

(deftest reconnect-sends-the-repl-operation-and-keeps-its-refusal
  (let [sent (transition base [::retirement/reconnect "direct-push"])
        request (:studio/http sent)]
    (is (= "/api/devices/reconnect" (:path request)))
    (is (= {:session-id "direct-push" :runtime-id "server-a" :expected-epoch 4} (:body request))
        "The session guard names the runtime the panel belongs to")
    (is (true? (:failure-data? request)) "The refusal body reaches the panel, not only its message")
    (is (= {:runtime-id "server-a" :pending {"direct-push" true}} (get-in sent [:db :ui :device-retirement])))
    (testing "a second click while pending sends nothing"
      (is (nil? (:studio/http (transition (:db sent) [::retirement/reconnect "direct-push"]))))))
  (testing "the refusal is stored with its code"
    (let [sent (transition base [::retirement/reconnect "direct-push"])
          refused (transition (:db sent) (conj (get-in sent [:studio/http :on-failure]) rejection))]
      (is (= {:runtime-id "server-a" :pending {}
              :results {"direct-push" {:accepted? false :message "Previous device session is still closing"
                                       :code "device/closing" :session-id "direct-push"}}}
             (get-in refused [:db :ui :device-retirement])))))
  (testing "an accepted reconnect replaces an earlier refusal"
    (let [sent (transition base [::retirement/reconnect "direct-push"])
          done (transition (:db sent) (conj (get-in sent [:studio/http :on-success]) {:status "accepted"}))]
      (is (= {"direct-push" {:accepted? true}} (get-in done [:db :ui :device-retirement :results])))))
  (testing "a result for a replaced runtime is ignored"
    (let [sent (transition base [::retirement/reconnect "direct-push"])
          replaced (assoc-in (:db sent) [:show :runtime-id] "server-b")
          late (transition replaced (conj (get-in sent [:studio/http :on-failure]) rejection))]
      (is (= replaced (:db late)))))
  (testing "nothing is sent while the show is disconnected"
    (is (nil? (:studio/http (transition (assoc-in base [:connection :connected?] false)
                                        [::retirement/reconnect "direct-push"]))))))

(deftest the-panel-shows-counts-sessions-errors-and-refusals
  (reset! db/app-db (assoc-in base [:ui :device-retirement]
                              {:runtime-id "server-a"
                               :results {"direct-push" (assoc (retirement/refusal rejection) :accepted? false)}}))
  (let [html (renderToStaticMarkup (r/as-element [retirement/panel]))]
    (is (re-find #"1 still closing" html))
    (is (re-find #"Closed</dt><dd>3" html))
    (is (re-find #"direct-push" html))
    (is (re-find #"browser-push" html))
    (is (re-find #"Last reopen failed: Display process exited" html))
    (is (re-find #"Previous device session is still closing" html))
    (is (re-find #":device/closing" html))
    (is (re-find #"Reconnect device session browser-push" html))))
