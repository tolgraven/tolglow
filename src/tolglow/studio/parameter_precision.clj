(ns tolglow.studio.parameter-precision
  "Annotate control views with the finest DMX channel reached on this patch.
  All native parameter values and registry definitions stay intact."
  (:require [afterglow.control.resolution :as resolution]))

(defn- annotate [widths definition targets]
  (reduce-kv (fn [view parameter schema]
               (if-let [bits (resolution/parameter-bits (force widths) definition parameter targets)]
                 (assoc-in view [:params parameter] (assoc schema :dmx-bits bits))
                 view))
             definition (:params definition {})))

(defn definitions
  "Resolve each distinct definition and target set once. Cache only entries used
  by this snapshot. Each consumer owns its cache and may run on a separate thread."
  [cache fixtures requests]
  (let [previous @cache
        same-rig? (identical? fixtures (:fixtures previous))
        widths (if same-rig? (:widths previous) (delay (resolution/patch-widths fixtures)))
        known (when same-rig? (:definitions previous))
        views (persistent!
               (reduce (fn [entries [definition targets :as request]]
                         (if (contains? entries request)
                           entries
                           (assoc! entries request (or (get known request) (annotate widths definition targets)))))
                       (transient {}) requests))]
    (reset! cache {:fixtures fixtures :widths widths :definitions views})
    views))

(defn model-definitions [cache model]
  (let [registry (vals (get-in model [:registry :effects]))
        instances (vals (:instances model))]
    (definitions cache (get-in model [:rig :fixtures])
                 (concat (map #(vector % nil) registry)
                         (map (juxt :definition :targets) instances)))))

(defn push-view [cache model view]
  (let [instance (get-in model [:instances (:instance-id view)])]
    (if (and instance (seq (:parameters view)) (= (:epoch view) (:epoch model))
             (= (:definition-version view) (:effect-version instance)))
      (let [request [(:definition instance) (:targets instance)]
            definition (get (definitions cache (get-in model [:rig :fixtures]) [request]) request)]
        (update view :parameters
                (fn [parameters]
                  (mapv (fn [{:keys [id param] :as slot}]
                          (if-let [bits (get-in definition [:params (or param id) :dmx-bits])]
                            (assoc-in slot [:schema :dmx-bits] bits)
                            slot))
                        parameters))))
      view)))
