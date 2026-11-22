(ns tolglow.lighting-match
  "Match an owned scene to the brightest full-white source, retaining its tint,
  pulse shape and dark frames. All calculations happen before priority blending."
  (:require [afterglow.render.photometry :as photometry]
            [tolglow.lighting-capacity :as capacity]))

(def target-param ::target)

(defn active? [definition]
  (and (:scene-owner definition) (= :global (:brightness-reference definition))
       (contains? (:params definition) target-param)))

(defn prepare [fixtures selected]
  (let [reference (reduce max 0.0 (map capacity/full-brightness fixtures))]
    (into {} (for [f fixtures :let [id (:id f) maximum (capacity/full-brightness f)]
                   :when (and (or (nil? selected) (selected id)) (pos? maximum))
                   :let [emitters (into {} (keep (fn [k] (when-let [[weight] (photometry/emitter-spectrum k)]
                                                          [k weight]))) (keys (:channels f)))]]
               [id {:maximum (/ maximum reference) :emitters emitters
                    :response (reduce + 0.0 (vals emitters)) :extract-white? (= :extract-min (:rgbw-policy f))}]))))

(defn- blend-value [old value mode]
  (if (or (nil? old) (nil? mode) (= :replace mode)) value
    (let [f (if (= :max mode) max (fn [a b] (min 1.0 (+ a b))))]
      (if (vector? value) (mapv f old value) (f old value)))))

(defn- chroma-maximum [{:keys [maximum emitters response extract-white?]} rows]
  (if (empty? emitters) maximum
    (let [values (reduce (fn [m {:keys [attribute value blend]}]
                           (assoc m attribute (blend-value (get m attribute) value blend))) {} rows)
          [r g b] (:color values [0.0 0.0 0.0])
          white (if extract-white? (min r g b) 0.0)
          levels (merge {:red (- r white) :green (- g white) :blue (- b white) :white white} values)
          peak (reduce-kv (fn [a k w] (if (pos? w) (max a (get levels k 0.0)) a)) 0.0 emitters)
          light (reduce-kv (fn [a k w] (+ a (* w (get levels k 0.0)))) 0.0 emitters)]
      (if (pos? peak) (* maximum (/ light (* response peak))) 0.0))))

(defn match-rows [fixture target rows]
  (if (or (nil? fixture) (empty? rows)) rows
    (let [maximum (chroma-maximum fixture rows)
          gain (if (pos? maximum) (min 1.0 (/ target maximum)) 0.0)
          fields (if (seq (:emitters fixture)) (conj (set (keys (:emitters fixture))) :color) #{:dimmer})]
      (if (== gain 1.0) rows
        (mapv (fn [{:keys [attribute value] :as row}]
                (if (fields attribute)
                  (assoc row :value (if (vector? value) (mapv #(* gain %) value) (* gain value)))
                  row)) rows)))))

(defn- constant-value [value]
  (cond (or (number? value) (vector? value)) value
        (= :const (:op value)) (:value value)))

(defn static-definition
  "Lower a static tint (including generated scenes) into ordinary typed
  expressions. Compute each rig-specific chromatic ceiling once on patch or
  definition changes. Fixtures with the same ceiling share a batch."
  [definition fixtures entries]
  (when (and (active? definition) (seq entries)
             (every? #(some? (constant-value (:value %))) entries)
             (every? #{:dimmer} (get-in definition [:envelope :attributes] [:dimmer])))
    (let [selected (when (:targets definition) (set (:targets definition)))
          profiles (prepare fixtures selected)
          rows (mapv #(-> % (update :targets (fn [ts] (when ts (set ts)))) (update :value constant-value)) entries)
          ;; Capture the full rig when finding the reference, then restrict
          ;; the emitted code to the definition and individual output selectors.
          by-field (reduce
                    (fn [groups {:keys [id]}]
                      (if-let [profile (profiles id)]
                        (let [own? #(or (nil? (:targets %)) ((:targets %) id))
                              maximum (chroma-maximum profile (filter own? rows))]
                          (reduce-kv
                           (fn [groups i {:keys [attribute] :as row}]
                             (if-not (own? row) groups
                               (let [scale? (if (seq (:emitters profile))
                                              (or (= :color attribute) (contains? (:emitters profile) attribute))
                                              (= :dimmer attribute))]
                                 (update-in groups [i (when scale? maximum)] (fnil conj []) id))))
                           groups rows))
                        groups))
                    (vec (repeat (count entries) {})) fixtures)
          outputs (into [] (mapcat
                            (fn [entry groups]
                              (for [[maximum ids] groups]
                                (cond-> (assoc entry :targets ids)
                                  (some? maximum)
                                  (update :value (fn [value]
                                                   {:op :multiply
                                                    :args [value (if (pos? maximum)
                                                                   {:op :min :args [1.0 {:op :divide :args [{:op :param :key target-param} maximum]}]}
                                                                   0.0)]})))))
                            entries by-field))]
      (when (seq outputs)
        (-> definition (dissoc :brightness-reference) (assoc :outputs outputs))))))
