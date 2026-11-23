(ns tolglow.studio.push-scene
  "The passive Push venue panel. A capture receives one immutable show frame."
  (:require [reagent.core :as r]
            [tolglow.studio.volume-optics :as optics]))

(defonce ^:private active (volatile! nil))

(defn rendered! []
  (some-> @active (aget "pendingFrame")))

(defn- mounted? [component] (identical? @active component))

(defn- geometry [shape]
  (cond-> (update shape :shape keyword)
    (:geometry shape) (update :geometry geometry)
    (:parts shape) (update :parts #(mapv geometry %))))

(defn- prepare-frame [owner scene]
  (let [signals (update-vals (:optical-signals scene) #(update % :volume geometry))
        surface (optics/prepare (aget owner "surfaceCache") signals)]
    (aset owner "surfaceCache" surface)
    (clj->js (cond-> (dissoc scene :optical-signals)
               (seq (:objects surface))
               (assoc :glass-volumes (:objects surface) :glass-meshes (:meshes surface)))
             :keyword-fn #(subs (str %) 1))))

(defn- dispose! [component]
  (when-let [engine (aget component "engine")]
    (js-invoke engine "dispose")
    (aset component "engine" nil))
  (some-> (aget component "canvasMount") (.replaceChildren)))

(defn- acquire! [component module]
  (let [canvas (.createElement js/document "canvas")]
    (.setAttribute canvas "aria-label" "Rendered show rig")
    (set! (.-width canvas) 960)
    (set! (.-height canvas) 160)
    (.replaceChildren (aget component "canvasMount") canvas)
    (let [engine (js/Reflect.construct (aget module "PushScene") #js [canvas])]
      (aset component "engine" engine)
      engine)))

(defn- draw! [component ticket scene module]
  (when (and (mounted? component) (= ticket (aget component "ticket")) (not (.-hidden js/document)))
    (let [engine (or (aget component "engine") (acquire! component module))]
      (js-invoke engine "update" (prepare-frame component scene))
      (set! (.-textContent (aget component "status")) ""))))

(defn- request-frame! [component]
  (when-let [scene (and (not (.-hidden js/document)) (:scene (second (r/argv component))))]
    (let [ticket (inc (or (aget component "ticket") 0))
          load (or (aget component "module") (js-invoke js/globalThis "tolglowLoadEngine" "preview"))]
      (aset component "ticket" ticket)
      (aset component "module" load)
      (aset component "pendingFrame"
            (-> load
                (.then #(draw! component ticket scene %))
                (.catch (fn [error]
                          (when (mounted? component)
                            (dispose! component)
                            (set! (.-textContent (aget component "status"))
                                  (str "Venue unavailable · " (.-message error)))))))))))

(def view
  (r/create-class
   {:display-name "PushVenuePreview"
    :component-did-mount
    (fn [component]
      (let [element (aget component "root")
            on-visibility #(if (.-hidden js/document) (dispose! component) (request-frame! component))]
        (vreset! active component)
        (aset component "status" (.querySelector element ".push-scene-status"))
        (aset component "canvasMount" (.querySelector element ".push-scene-canvas"))
        (aset component "visibility" on-visibility)
        (.addEventListener js/document "visibilitychange" on-visibility)
        (request-frame! component)))
    :component-did-update (fn [component _] (request-frame! component))
    :component-will-unmount (fn [component]
                              (.removeEventListener js/document "visibilitychange" (aget component "visibility"))
                              (when (mounted? component) (vreset! active nil))
                              (dispose! component))
    :reagent-render
    (fn [{:keys [scene display-mode]}]
      (let [component (r/current-component)
            rig? (= "rig" (name display-mode))
            camera (some-> (:camera scene) name)]
        [:section.push-scene {:ref #(aset component "root" %) :aria-label (if rig? "Rig configuration space" "Live venue simulator")}
         [:div.push-scene-canvas]
         [:div.push-scene-caption [:strong (if rig? "RIG" "LIVE")]
          [:span (case camera "panorama" "Panorama" "audience" "Audience" "Fit rig")]
          (when-let [fixture (get-in scene [:editor :fixture-name])] [:span.push-rig-fixture fixture])
          [:span (str (count (get-in scene [:rig :fixtures])) " / " (:fixture-count scene 0) " fixtures")]]
         [:span.push-scene-status {:role "status"}]]))}))
