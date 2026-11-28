(ns tolglow.studio.clock
  "Read the show's per-frame fields from the :clock and :signals frame lanes.

  Frame number, show time, transport beat and time, schedule beat, frame
  compute time and output-scheduler status change on every show frame. They
  live on the :clock lane, not in app-db, so a frame does not re-render the
  application. A view that shows one subscribes to it here and only that view
  re-renders; an event that needs one injects :studio/clock. Signals sampled
  on every frame while audio runs (patch sources, audio energy, lighting
  activity) live on the :signals lane in the same way."
  (:require [re-frame.core :as rf]
            [reagent.ratom :as ratom]
            [tolglow.studio.frame-stream :as frame-stream]
            [tolglow.studio.projection :as projection]))

(defn present
  "The presented clock parts of a lane frame, as Studio controls read them.
  Reuses the previous presentation's unchanged subtrees."
  ([lane-frame] (present lane-frame nil nil))
  ([lane-frame previous rendered]
   (when lane-frame
     (projection/presentation (:value lane-frame) previous rendered))))

(defn with-clock
  "Restore presented clock parts into a show at their server paths, for views
  written against the whole show projection."
  [show clock]
  (if (and show clock)
    (reduce-kv (fn [show part path]
                 (if (contains? clock part) (assoc-in show path (get clock part)) show))
               show (:clock frame-stream/lane-paths))
    show))

(defn presented
  "A reaction over the presented parts of lane's published frame. The frame
  changes after each ACK; it is presented once per frame for every reader."
  [lane]
  (let [cache (volatile! nil)]
    (ratom/make-reaction
      (fn []
        (let [lane-frame @(frame-stream/published-ref lane)
              {:keys [value presented]} @cache]
          (if (identical? (:value lane-frame) value)
            presented
            (let [next (present lane-frame value presented)]
              (vreset! cache {:value (:value lane-frame) :presented next})
              next)))))))

(rf/reg-sub-raw :studio/clock (fn [_ _] (presented :clock)))

(rf/reg-sub :studio/clock-part
  :<- [:studio/clock]
  (fn [clock [_ part]] (get clock part)))

(rf/reg-sub :studio/transport
  ;; The committed transport with its per-frame beat and time.
  :<- [:studio/show-transport]
  :<- [:studio/clock]
  (fn [[transport clock] _]
    (cond-> transport
      (contains? clock :beat) (assoc :beat (:beat clock))
      (contains? clock :transport-time-ms) (assoc :time-ms (:transport-time-ms clock)))))

(rf/reg-sub :studio/show-transport (fn [db _] (get-in db [:show :transport])))

(rf/reg-sub ::show (fn [db _] (:show db)))

(defn current
  "The presented clock parts of the installed lane frame, for event handlers."
  []
  (present (frame-stream/baseline :clock)))

(rf/reg-cofx :studio/clock
  (fn [cofx _] (assoc cofx :clock (current))))

(rf/reg-sub-raw :studio/signals
  ;; Presented parts of the :signals lane: observed patch sources, audio
  ;; energy and lighting activity, as the show projection presents them.
  (fn [_ _] (presented :signals)))

(rf/reg-sub-raw :studio/signal-values
  ;; The same parts with their server types, for readers of the raw view.
  (fn [_ _] (ratom/make-reaction #(:value @(frame-stream/published-ref :signals)))))

(rf/reg-cofx :studio/signals
  (fn [cofx _] (assoc cofx :signals (present (frame-stream/baseline :signals)))))
