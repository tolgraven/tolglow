(ns tolglow.studio.checkbox
  "The controlled checkbox every Studio view uses.

  React restores a controlled checkbox to its rendered `checked` value as soon
  as the change event ends. Reagent renders on the next animation frame, so a
  checkbox would show its old state for one frame (and an automated `check()`
  would see the click undone). Rendering pending updates before the event ends
  lets React keep the new state instead.

  That only helps when the handler has already changed the state the box
  renders from, so a handler must change app-db or a local atom synchronously
  (`rf/dispatch-sync`, `swap!`). An `rf/dispatch` lands on a later tick, after
  React restored the old state. A handler that only sends a request and keeps
  no pending state shows the committed state until the reply changes it."
  (:require [reagent.core :as r]))

(defn input
  "An `<input type=checkbox>` with `attrs`. `:checked` is the rendered state and
  `:on-change` receives the change event, as for a plain input."
  [{:keys [checked on-change] :as attrs}]
  [:input (assoc attrs :type "checkbox" :checked (boolean checked)
                 :on-change (fn [event]
                              (when on-change (on-change event))
                              (r/flush)))])
