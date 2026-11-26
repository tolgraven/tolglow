(ns tolglow.studio.show-window
  "The show this Studio window belongs to (#669).

  The Mac app opens a window per show, and each window's show runs in its own
  process on its own loopback port. Before loading Studio the app reads that
  show's runtime ID from `/api/identity` and gives it to the page as
  `window.tolglowStudioWindow = {runtimeId, show, tabs}`. A bound page sends
  that runtime ID with every command, so any other show refuses the command,
  and it refuses to install another runtime's state, so it never shows or
  controls a different show. Without a binding (an ordinary browser tab)
  Studio follows whichever runtime its server reports, as before.

  The page reports its active view and tab count back to the app through the
  `tolglowStudioWindow` script message handler, which titles the window."
  (:require [re-frame.core :as rf]))

(defn host-config
  "The binding the Mac app installed before the page loaded, or nil."
  []
  (let [config (aget js/window "tolglowStudioWindow")]
    (when (some? config)
      (let [{:keys [runtimeId show tabs]} (js->clj config :keywordize-keys true)]
        (cond-> {}
          (and (string? runtimeId) (seq runtimeId)) (assoc :runtime-id runtimeId)
          (string? show) (assoc :show show)
          (string? tabs) (assoc :tabs tabs))))))

(rf/reg-cofx :show-window/host
  (fn [cofx _] (assoc cofx :show-window/host (try (host-config) (catch :default _ nil)))))

(defn install
  "Record the window's binding in a fresh app-db."
  [db host]
  (cond-> db (:runtime-id host) (assoc :window (select-keys host [:runtime-id :show]))))

(defn runtime-id
  "The runtime this window is bound to, or nil when it follows its server."
  [db]
  (get-in db [:window :runtime-id]))

(defn refuses?
  "True when this window is bound to another runtime than `received`."
  [db received]
  (let [bound (runtime-id db)]
    (boolean (and bound (not= bound received)))))

(defn refusal-message [db]
  (str "This window belongs to " (or (get-in db [:window :show]) "another show")
       ". It refused the state of a different show, and its controls stay disconnected."))

(defn- handler []
  (some-> (aget js/window "webkit") (aget "messageHandlers") (aget "tolglowStudioWindow")))

(defn post!
  "Tell the Mac app about this window's views. A no-op in a browser."
  [message]
  (when-let [h (handler)]
    (try (.postMessage h (clj->js message)) (catch :default _ nil))))
