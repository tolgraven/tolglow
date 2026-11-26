(ns tolglow.studio.checkbox
  "The controlled checkbox every Studio view uses.

  React restores a controlled checkbox to its rendered `checked` value as soon
  as the change event ends. Reagent renders on the next animation frame, so a
  checkbox whose handler already changed the state it renders from would show
  its old state for one frame (and an automated `check()` would see the click
  undone). Rendering pending updates before the event ends lets React keep the
  new state instead. A handler that only sends a request keeps showing the
  committed state until the reply changes it, which is the point of a
  controlled checkbox."
  (:require [reagent.core :as r]))

(defn input
  "An `<input type=checkbox>` with `attrs`. `:checked` is the rendered state and
  `:on-change` receives the change event, as for a plain input."
  [{:keys [checked on-change] :as attrs}]
  [:input (assoc attrs :type "checkbox" :checked (boolean checked)
                 :on-change (fn [event]
                              (when on-change (on-change event))
                              (r/flush)))])
