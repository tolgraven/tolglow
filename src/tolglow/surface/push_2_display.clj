(ns tolglow.surface.push-2-display
  "The native Push 2 display (Wayang) for linked knobs (#666). Afterglow's
  Wayang transport takes an injected :renderer! (transports/wayang-display);
  this one draws the touched knob or the knob page from the same view data
  Studio's Push display route reads (tolglow.controllers.push-knobs
  project-display: :knob and :knob-page :knobs), and leaves every other
  view to Afterglow's transports/render-view!. It only draws into an owned
  BufferedImage; the transport sends the frame."
  (:require [afterglow.control.devices.transports :as transports])
  (:import [java.awt Color Font RenderingHints]
           [java.awt.image BufferedImage]))

(defn- label [id] (if (keyword? id) (name id) (str id)))

(defn knob-panel
  "What the display shows for knobs, or nil: the touched knob with up to
  eight resolved links, or the knob page with one column per knob, in the
  order of tolglow.studio.push-display/knob-model."
  [view]
  (let [{:keys [target]} (:touched-control view)
        {:keys [knob]} view
        page (get-in view [:knob-page :knobs])]
    (cond
      (and (= :knob target) knob)
      {:title (str "Knob " (or (:name knob) (label (:knob-id knob)))
                   "  " (if (:missing? knob) "Removed" (:text knob)))
       :columns (mapv (fn [{:keys [label text status diverged?]}]
                        {:label (str label) :text (or text (some-> status name))
                         :dangling? (not= :ok status) :diverged? (boolean diverged?)})
                      (take 8 (:columns knob)))}

      (seq page)
      {:title "Knobs"
       :columns (mapv (fn [{:keys [knob-id name text value missing?]}]
                        (cond-> {:label (or name (label knob-id)) :text (if missing? "Removed" text)
                                 :dangling? (boolean missing?)}
                          (and (not missing?) (number? value)) (assoc :value (double value))))
                      (take 8 page))})))

(defn- clip [^String s n] (subs s 0 (min n (count s))))

(defn- draw-knobs! [^BufferedImage image {:keys [title columns]}]
  (let [graphics (.createGraphics image)
        width (.getWidth image) height (.getHeight image)
        cell (quot width 8)]
    (try
      (.setRenderingHint graphics RenderingHints/KEY_TEXT_ANTIALIASING RenderingHints/VALUE_TEXT_ANTIALIAS_ON)
      (.setColor graphics (Color. 9 12 18)) (.fillRect graphics 0 0 width height)
      (.setFont graphics (Font. "SansSerif" Font/BOLD 18)) (.setColor graphics (Color. 235 240 246))
      (.drawString graphics (str title) 16 28)
      (.setFont graphics (Font. "SansSerif" Font/PLAIN 14))
      (doseq [[index {:keys [label text value dangling? diverged?]}] (map-indexed vector columns)]
        (let [x (+ 8 (* cell index))
              bar (max 0 (- cell 16))]
          (.setColor graphics (if dangling? (Color. 200 90 90) (Color. 105 192 236)))
          (.drawString graphics (clip (str label) 12) x 88)
          (.setColor graphics Color/WHITE)
          (.drawString graphics (clip (str text (when diverged? " *")) 12) x 112)
          (when value
            (.setColor graphics (Color. 45 55 70)) (.fillRect graphics x 124 bar 6)
            (.setColor graphics (Color. 105 192 236))
            (.fillRect graphics x 124 (int (Math/round (* bar (max 0.0 (min 1.0 value))))) 6))))
      image
      (finally (.dispose graphics)))))

(defn renderer
  "A Wayang :renderer! that draws the knob panel when a view has one and
  otherwise the parameter display of fallback (Afterglow's
  transports/render-view!, as it is when the renderer is built)."
  ([] (renderer transports/render-view!))
  ([fallback]
   (fn [^BufferedImage image view]
     (if-let [panel (knob-panel view)]
       (draw-knobs! image panel)
       (fallback image view)))))

(defn render-view!
  "Draw a view into image with renderer's choice."
  [^BufferedImage image view]
  ((renderer) image view))
