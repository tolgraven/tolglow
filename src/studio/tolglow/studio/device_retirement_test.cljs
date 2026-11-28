(ns tolglow.studio.device-retirement-test
  (:require [cljs.test :refer-macros [deftest is]]
            ["react-dom/server" :refer [renderToStaticMarkup]]
            [re-frame.db :as db]
            [reagent.core :as r]
            [tolglow.studio.device-retirement :as retirement]
            [tolglow.studio.events]
            [tolglow.studio.projection :as projection]))

;; The status as the state stream installs it: show/snapshot's map through
;; Studio's presentation, so keywords read as strings and sets as vectors.
(def status
  (projection/presentation
   {:pending 1 :retired 3 :failed 0 :reopen-failed 0 :abandoned 0 :late 0 :refused 1
    :closing #{"direct-push"}
    :last-error {:stage :reopen :message "Display process exited" :class "clojure.lang.ExceptionInfo"}
    :last-refusal {:session-id "direct-push" :code :device/closing :message "Previous device session is still closing"}}))

(defn- html [db-value]
  (reset! db/app-db db-value)
  (renderToStaticMarkup (r/as-element [retirement/panel])))

(deftest closing-ids-are-listed-sorted
  (is (= ["a-push" "b-midi"] (retirement/closing-ids {:closing ["b-midi" "a-push"]})))
  (is (= [] (retirement/closing-ids nil))))

(deftest a-refusal-reads-as-the-repl-prints-it
  (is (= {:message "Previous device session is still closing" :code "device/closing" :session-id "direct-push"}
         (retirement/refusal (:last-refusal status))))
  (is (= ":device/closing" (retirement/code-text "device/closing")))
  (is (= ":device/closing" (retirement/code-text :device/closing))))

(deftest the-panel-shows-counts-closing-ids-errors-and-the-last-refusal
  (let [page (html {:show {:runtime-id "server-a" :status {:device-retirement status}}})]
    (is (re-find #"1 still closing" page))
    (is (re-find #"Closed</dt><dd>3" page))
    (is (re-find #"retirement-id\">direct-push" page))
    (is (re-find #"Last reopen failed: Display process exited" page))
    (is (re-find #"Last refused connect" page))
    (is (re-find #"Previous device session is still closing" page))
    (is (re-find #":device/closing" page))
    (is (not (re-find #"<button" page)) "Read-only: Studio does not open devices")))

(deftest a-cleared-refusal-and-closed-sessions-leave-the-panel-quiet
  (let [page (html {:show {:status {:device-retirement (-> status (assoc :closing [] :pending 0)
                                                           (dissoc :last-refusal :last-error))}}})]
    (is (re-find #"None closing" page))
    (is (not (re-find #"retirement-session" page)))
    (is (not (re-find #"Last refused" page)))))
