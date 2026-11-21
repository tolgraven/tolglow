(ns tolglow.composition.tween
  "Logical-time interpolation. No timers, frame counters or mutable closures."
  (:require [tolglow.registry :as registry]
            [afterglow.time :as time]))

(defn curve
  "Map a normalized fraction through a bounded easing curve."
  [shape fraction]
  (let [x (double (max 0 (min 1 fraction)))]
    (case (or shape :linear)
      :linear x
      :smoothstep (* x x (- 3 (* 2 x)))
      :sine (/ (- 1 (Math/cos (* Math/PI x))) 2)
      :ease-in (* x x)
      :ease-out (- 1 (* (- 1 x) (- 1 x)))
      :step (if (= x 1.0) 1.0 0.0)
      (if (and (map? shape) (= :power (:type shape))
               (registry/finite-number? (:exponent shape)) (pos? (:exponent shape)))
        (Math/pow x (double (:exponent shape)))
        (throw (ex-info "Unknown easing curve" {:curve shape}))))))

(defn interpolate
  "Interpolate equal-shaped numeric scalars or vectors."
  [a b fraction]
  (cond
    (and (registry/finite-number? a) (registry/finite-number? b))
    (+ a (* fraction (- b a)))
    (and (vector? a) (vector? b) (= (count a) (count b)))
    (mapv #(interpolate %1 %2 fraction) a b)
    :else (throw (ex-info "Tween values need equal numeric shapes" {:from a :to b}))))

(defn- numeric-value? [value]
  (or (registry/finite-number? value)
      (and (vector? value) (seq value) (every? numeric-value? value))))

(defn- validate-position-tween [spec]
  (let [frames (:keyframes spec)
        mode (:mode spec :once)
        repeats (:repeat-count spec)]
    (vec
     (concat
      (when-not (and (vector? frames) (<= 2 (count frames)))
        [{:path [:keyframes] :message "Tween needs at least two keyframes"}])
      (when-not (#{:once :loop :ping-pong} mode)
        [{:path [:mode] :message "Mode must be once, loop or ping-pong"}])
      (when (and (some? repeats) (not (and (integer? repeats) (pos? repeats))))
        [{:path [:repeat-count] :message "Repeat count must be a positive integer"}])
      (when (seq frames)
        (concat
         (when-not (= 0 (:at-ms (first frames)))
           [{:path [:keyframes 0 :at-ms] :message "First keyframe starts at zero"}])
         (mapcat
          (fn [[i frame]]
            (concat
             (when-not (and (registry/finite-number? (:at-ms frame))
                            (not (neg? (:at-ms frame))))
               [{:path [:keyframes i :at-ms] :message "Time must be finite and nonnegative"}])
             (when-not (numeric-value? (:value frame))
               [{:path [:keyframes i :value] :message "Value must be a finite number or numeric vector"}])
             (try (curve (:curve frame) 0.5) []
                  (catch Exception _ [{:path [:keyframes i :curve] :message "Unknown easing curve"}]))))
          (map-indexed vector frames))
         (when (and (every? #(registry/finite-number? (:at-ms %)) frames)
                    (not (apply < (map :at-ms frames))))
           [{:path [:keyframes] :message "Keyframe times must strictly increase"}])
         (try (doseq [frame frames] (interpolate (:value (first frames)) (:value frame) 0)) []
              (catch Exception _ [{:path [:keyframes] :message "Keyframe values must have equal numeric shapes"}]))))))))

(defn- compile-position-tween
  "Validate once and return elapsed-ms -> {:value :done? :iteration :phase}.
  Ping-pong reverses a traversal each duration; repeat-count counts traversals.
  At a finite terminal boundary the terminal value is returned exactly once or
  repeatedly when sampled, without requiring any previous frame."
  [spec]
  (let [errors (validate-position-tween spec)]
    (when (seq errors) (throw (ex-info "Invalid tween" {:errors errors}))))
  (let [frames (:keyframes spec)
        duration (:at-ms (peek frames))
        mode (:mode spec :once)
        repeats (if (= mode :once) 1 (:repeat-count spec))
        total (when repeats (* repeats duration))]
    (fn [elapsed-ms]
      (when-not (registry/finite-number? elapsed-ms)
        (throw (ex-info "Tween needs finite logical time" {:time-ms elapsed-ms})))
      (let [elapsed (max 0 elapsed-ms)
            done? (boolean (and total (>= elapsed total)))
            iteration (long (if done? (dec repeats) (Math/floor (/ elapsed duration))))
            fraction (if done? 1.0 (/ (mod elapsed duration) duration))
            phase (if (and (= mode :ping-pong) (odd? iteration)) (- 1 fraction) fraction)
            position (* phase duration)
            [a b] (or (some (fn [[a b]] (when (<= (:at-ms a) position (:at-ms b)) [a b]))
                           (partition 2 1 frames))
                      [(nth frames (- (count frames) 2)) (peek frames)])
            amount (curve (:curve a) (/ (- position (:at-ms a)) (- (:at-ms b) (:at-ms a))))]
        {:value (interpolate (:value a) (:value b) amount)
         :done? done? :iteration iteration :phase phase}))))

(defn- normalize-keyframes [spec meter]
  (when-not (and (map? spec) (vector? (:keyframes spec)))
    (throw (ex-info "Tween needs a keyframe vector"
                    {:errors [{:path [:keyframes] :message "Tween needs a keyframe vector"}]})))
  (let [frames (:keyframes spec)
        durations (mapv (fn [i frame]
                          (when-not (map? frame)
                            (throw (ex-info "Keyframe must be a map"
                                            {:errors [{:path [:keyframes i] :message "Keyframe must be a map"}]})))
                          (try (time/normalize-duration
                                (if (contains? frame :at) (:at frame) {:ms (:at-ms frame)}) meter)
                               (catch clojure.lang.ExceptionInfo cause
                                 (throw (ex-info "Invalid keyframe time"
                                                 {:errors (mapv #(assoc % :path
                                                                       (if (contains? frame :at)
                                                                         (into [:keyframes i :at] (:path %))
                                                                         [:keyframes i :at-ms]))
                                                                (:errors (ex-data cause)))} cause))))) (range) frames)
        units (set (map :unit durations))]
    (when (> (count units) 1)
      (throw (ex-info "Tween keyframes need one clock domain"
                      {:errors [{:path [:keyframes] :message "Do not mix musical and millisecond keyframes"}]})))
    (assoc spec :timebase (or (first units) :beats)
           :keyframes (mapv #(assoc %1 :at-ms (:value %2)) frames durations))))

(defn validate-tween [spec]
  (try (validate-position-tween (normalize-keyframes spec nil))
       (catch clojure.lang.ExceptionInfo cause
         (or (:errors (ex-data cause)) [{:path [:keyframes] :message (.getMessage cause)}]))))

(defn compile-tween
  "Canonical keyframes use :at duration maps. The sampler accepts elapsed beat/
  millisecond values, or {:elapsed-beats :elapsed-ms :meter} from an instance.
  Meter must be captured at launch. Numeric input means elapsed coordinates
  in the selected clock domain; :at-ms explicitly selects milliseconds."
  [spec]
  (let [normalized (normalize-keyframes spec nil)
        default-sampler (compile-position-tween normalized)
        musical? (= :beats (:timebase normalized))]
    (fn [clock]
      (if (number? clock) (default-sampler clock)
          (let [normalized (if musical? (normalize-keyframes spec (:meter clock)) normalized)
                sampler (if musical? (compile-position-tween normalized) default-sampler)
                elapsed (get clock (if musical? :elapsed-beats :elapsed-ms))]
            (sampler elapsed))))))

(defn sample [spec elapsed] ((compile-tween spec) elapsed))
