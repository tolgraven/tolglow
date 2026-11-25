(ns tolglow.spatial.sampling-test
  "The partitioned field sampler returns the serial sampler's fields: equal
  weights and aims in the same iteration order, for any slicing, worker
  completion order or repeat, and the same failure."
  (:require [afterglow.control.model :as control]
            [afterglow.spatial :as physics]
            [afterglow.spatial.field :as field]
            [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [tolglow.rig.surfaces :as surfaces]
            [tolglow.show :as show]
            [tolglow.spatial.sampling :as sampling])
  (:import [java.io Closeable]
           [java.util.concurrent CountDownLatch ThreadPoolExecutor TimeUnit]))

(defn- fraction [i step] (mod (* i step) 1.0))

(defn- fixture
  "A deterministic mixed patch: RGBW pixels, moving heads, fixed spots, bare
  positions, shared X ranks and one integer coordinate."
  [i]
  (let [x (- (* 12.0 (fraction i 0.6180339887)) 6.0)
        y (* 5.0 (fraction i 0.7548776662))
        z (- (* 12.0 (fraction i 0.5698402910)) 6.0)
        head {:id (str "head-" i) :position [x y z]}]
    (case (long (mod i 16))
      0 (assoc head :channels {:pan 1 :tilt 2 :dimmer 3 :red 4 :green 5 :blue 6}
               :beam-angle 24.0 :lens-radius-m 0.05)
      1 head
      2 (assoc head :position [(Math/rint x) y z] :channels {:red 1 :green 2 :blue 3})
      3 (assoc head :channels {:red 1 :green 2 :blue 3} :direction [0.0 -1.0 0.0] :beam-angle 40.0)
      5 (assoc head :position [x 2 z] :channels {:red 1 :green 2 :blue 3 :white 4})
      (assoc head :channels {:red 1 :green 2 :blue 3 :white 4}))))

(defn- patch [n] (mapv fixture (range n)))

(defn- committed
  "The last committed output: every fifth emitter is dark and movers hold a pose."
  [fixtures]
  (into {}
        (keep-indexed
         (fn [i {:keys [id channels]}]
           (when (seq channels)
             [id (cond-> {:emitted-color (if (zero? (mod i 5)) [0.0 0.0 0.0] [(fraction i 0.37) 0.2 0.1])}
                   (:pan channels) (assoc :pan (fraction i 0.13) :tilt (fraction i 0.29)))])))
        fixtures))

(defn- observations [fixtures options]
  (assoc (field/prepare-observations
          (merge {:range-m 40.0 :extinction-per-m 0.0
                  :surfaces (surfaces/receivers {:room {:width 14.0 :height 6.0 :depth 14.0}})}
                 options))
         :values (committed fixtures)))

(def ^:private smooth {:inner-m 0.5 :outer-m 1.0 :curve :smoothstep})

(def ^:private volumes
  "Overlapping fields over the same heads, by position, beam and aim."
  [{:shape :sphere :position [1.5 2.5 0.5] :radius 3.0 :strength 0.8 :falloff smooth :selection #{:position :beam}}
   {:shape :sphere :position [-1.0 2.0 1.0] :radius 3.5 :falloff {:inner-m 0.25 :outer-m 0.75 :curve :linear}
    :selection #{:position}}
   {:shape :sphere :position [0.5 3.0 -1.5] :radius 2.5 :strength 0.6 :falloff smooth :selection #{:beam}}
   {:shape :sphere :position [2.0 1.5 2.0] :radius 2.0 :falloff smooth :selection #{:position :beam :aimable}}
   {:shape :box :min [-2.0 0.5 -2.0] :max [1.0 3.0 2.5] :falloff {:inner-m 0.0 :outer-m 0.75 :curve :linear}
    :selection #{:position :beam}}
   {:shape :sphere :position [0.0 2.0 0.0] :radius 0.3 :selection #{:aimable}}
   {:shape :polyhedron :planes [{:normal [0.0 1.0 0.0] :distance 1.0}] :selection #{:position}}
   {:shape :sphere :position (:position (fixture 7)) :radius 0.6 :selection #{:position}}])

(defn- optical? [volume] (some #{:beam :aimable} (:selection volume)))

(defn- serial
  "The show's serial choice: positions alone, or one of Afterglow's two optical samplers."
  [variant volume rig frame shared]
  (cond
    (not (optical? volume)) (field/positions volume rig)
    (= :shared variant) (shared volume)
    :else (field/sample volume rig frame)))

(defn- slice-samplers [variant slices frame]
  (mapv (fn [{:keys [rig]}]
          (if (= :shared variant) (field/sampler rig frame) #(field/sample % rig frame)))
        (:optical slices)))

(defn- difference
  "nil when both results hold equal entries in the same iteration order."
  [expected actual]
  (or (when-not (= #{:weights :aims} (set (keys actual))) {:keys (keys actual)})
      (first (for [k [:weights :aims]
                   :let [a (vec (get expected k)) b (vec (get actual k))]
                   :when (not= a b)]
               {:field k :expected (count a) :actual (count b)
                :first-mismatch (first (filter (fn [[x y]] (not= x y)) (map vector a b)))}))))

(defn- close! [sampler] (.close ^Closeable sampler))

(defn- executor ^ThreadPoolExecutor [sampler] (:executor sampler))

(deftest options-bound-the-workers-and-keep-small-patches-serial
  (let [sampler (sampling/create {:workers 4})]
    (try
      (is (= [4 8 4096] ((juxt :workers :partitions :min-fixtures) sampler)))
      (is (= 3 (.getCorePoolSize (executor sampler))))
      (is (zero? (.getPoolSize (executor sampler))) "Creating the sampler starts no thread")
      (is (false? (sampling/parallel? sampler 4095)))
      (is (true? (sampling/parallel? sampler 4096)))
      (finally (close! sampler))))
  (let [default (sampling/create)]
    (try
      (is (= (sampling/default-workers) (:workers default)))
      (is (= (* 2 (:workers default)) (:partitions default)))
      (finally (close! default))))
  (let [single (sampling/create {:workers 1 :min-fixtures 1})]
    (is (nil? (:executor single)))
    (is (false? (sampling/parallel? single 100000)))
    (close! single))
  (is (false? (sampling/parallel? nil 100000)))
  (doseq [bad [{:workers 0} {:workers 2.5} {:workers 257} {:partitions 0} {:partitions 4097}
               {:min-fixtures 0} {:threads 2} [:workers 2]]]
    (is (thrown? clojure.lang.ExceptionInfo (sampling/create bad)) (pr-str bad))))

(deftest slices-cover-every-fixture-once-and-a-repeated-id-stays-serial
  (let [fixtures (patch 50)
        rig (physics/prepare-rig fixtures)]
    (doseq [parts [1 3 50 51 400]]
      (let [{:keys [position optical sliced?]} (sampling/partition-rig fixtures rig parts)]
        (is (true? sliced?))
        (is (= (min parts 50) (count position) (count optical)))
        (is (= fixtures (into [] (mapcat :fixtures) optical)) "Patch order")
        (is (= (sort (map :id fixtures)) (sort (map first (mapcat :rows position)))))
        (is (apply <= (map (comp first second) (mapcat :rows position))) "X rank order")))
    (let [repeated (conj fixtures (assoc (first fixtures) :position [9.0 9.0 9.0]))]
      (is (nil? (:sliced? (sampling/partition-rig repeated (physics/prepare-rig repeated) 4)))))))

(deftest any-slicing-returns-the-serial-fields
  (let [fixtures (patch 300)
        rig (physics/prepare-rig fixtures)]
    (doseq [[label options] [["clear air" {}]
                             ["a range shorter than the room" {:range-m 3.5}]
                             ["fog" {:extinction-per-m 0.05}]]
            :let [frame (observations fixtures options)]
            variant [:shared :separate]
            :let [shared (field/sampler rig frame)
                  expected (mapv #(serial variant % rig frame shared) volumes)]
            parts [1 2 5 64 299 300 1000]]
      (let [sampler (sampling/create {:workers 3 :partitions parts :min-fixtures 1})
            slices (sampling/partition-rig fixtures rig parts)
            split (slice-samplers variant slices frame)]
        (try
          (is (= (min parts 300) (count (:optical slices))))
          (doseq [[index volume] (map-indexed vector volumes)]
            (is (nil? (difference (nth expected index)
                                  (sampling/sample sampler slices volume (when (optical? volume) split))))
                (pr-str label variant parts index)))
          (finally (close! sampler)))))
    (let [frame (observations fixtures {})
          [origin-and-beam origin-only beam-only aim-and-beam] (mapv #(field/sample % rig frame) volumes)
          inside (set (keys (:weights (field/positions (first volumes) rig))))]
      (is (some #(not (contains? inside %)) (keys (:weights origin-and-beam))) "A beam reaches the field from outside")
      (is (seq (filter (set (keys (:weights origin-and-beam))) (keys (:weights origin-only)))) "Fields overlap")
      (is (seq (:weights beam-only)))
      (is (seq (:aims aim-and-beam)))
      (is (<= 1 (count (:weights (field/positions (peek volumes) rig))) 300)))))

(deftest a-patch-at-the-default-threshold-is-split-with-default-partitions
  (let [fixtures (patch 4500)
        rig (physics/prepare-rig fixtures)
        frame (observations fixtures {})
        sampler (sampling/create {:workers 4})
        chosen (subvec volumes 0 4)]
    (try
      (is (true? (sampling/parallel? sampler (count fixtures))))
      (let [slices (sampling/partition-rig fixtures rig (:partitions sampler))
            shared (field/sampler rig frame)
            expected (mapv #(serial :shared % rig frame shared) chosen)
            sample-all (fn []
                         (let [split (slice-samplers :shared slices frame)]
                           (mapv #(sampling/sample sampler slices % (when (optical? %) split)) chosen)))
            first-run (sample-all)]
        (is (= 8 (count (:position slices)) (count (:optical slices))))
        (is (every? #(< 100 (count (:weights %))) expected))
        (doseq [[index result] (map-indexed vector first-run)]
          (is (nil? (difference (nth expected index) result)) (str "volume " index)))
        (dotimes [run 3]
          (is (= expected (sample-all)) (str "repeat " run))))
      (finally (close! sampler)))))

(deftest a-few-heads-keep-the-serial-insertion-order
  ;; The patch is declared against its X order, and at most eight entries
  ;; iterate in insertion order: ranked positions first, then beams by patch order.
  (let [row (mapv (fn [x] {:id (keyword (str "x" x)) :position [x 1 0] :channels {:red 1}}) (reverse (range 12)))
        spot {:id :spot :position [5.2 1.0 -3.0] :channels {:red 1}}
        far {:id :far :position [5.2 1.0 -9.0] :channels {:red 1}}
        fixtures (into [far spot] row)
        rig (physics/prepare-rig fixtures)
        frame {:values {:spot {:emitted-color [0.6 0.0 0.0]} :far {:emitted-color [0.3 0.0 0.0]}}}
        volume {:shape :sphere :position [5.2 1.0 0.0] :radius 2.1 :selection #{:position :beam}}
        expected (field/sample volume rig frame)]
    (is (= [:x4 :x5 :x6 :x7 :far :spot] (vec (keys (:weights expected)))))
    (is (= 0.6 (get-in expected [:weights :spot])))
    (doseq [parts [1 2 3 5 14 15]]
      (let [sampler (sampling/create {:workers 2 :partitions parts :min-fixtures 1})
            slices (sampling/partition-rig fixtures rig parts)]
        (try
          (is (nil? (difference expected (sampling/sample sampler slices volume
                                                          (slice-samplers :separate slices frame))))
              (str parts " slices"))
          (finally (close! sampler)))))))

(deftest pool-threads-sample-slices-and-completion-order-does-not-change-the-fields
  (let [fixtures (patch 240)
        rig (physics/prepare-rig fixtures)
        frame (observations fixtures {})
        volume (first volumes)
        expected (field/sample volume rig frame)
        sampler (sampling/create {:workers 3 :partitions 6 :min-fixtures 1})
        slices (sampling/partition-rig fixtures rig 6)
        pooled? #(str/starts-with? % sampling/thread-prefix)]
    (try
      (dotimes [run 3]
        (let [threads (atom #{})
              on-pool (CountDownLatch. 1)
              traced (fn [index sample-slice]
                       (fn [field]
                         (let [thread (.getName (Thread/currentThread))]
                           (swap! threads conj thread)
                           (when (pooled? thread) (.countDown on-pool))
                           ;; The caller claims the last slice first. Hold that slice
                           ;; until a pool thread has sampled, and finish early slices last.
                           (when (= index 5) (.await on-pool 10 TimeUnit/SECONDS))
                           (let [result (sample-slice field)]
                             (Thread/sleep (long (* 4 (- 5 index))))
                             result))))
              split (vec (map-indexed traced (slice-samplers :separate slices frame)))]
          (is (nil? (difference expected (sampling/sample sampler slices volume split))) (str "run " run))
          (is (some pooled? @threads) "A pool thread sampled a slice")))
      (is (pos? (.getPoolSize (executor sampler))))
      (finally (close! sampler)))
    (is (.isTerminated (executor sampler)))
    (is (zero? (.getPoolSize (executor sampler))))))

(defn- failure [f]
  (try (f) nil (catch Throwable error error)))

(deftest a-failing-fixture-surfaces-the-serial-exception-after-every-slice-finishes
  (let [bad (fn [id angle] {:id id :position [0.5 3.0 -4.0] :channels {:red 1} :beam-angle angle})
        fixtures (-> (patch 200) (assoc 71 (bad "bad-early" 180)) (assoc 152 (bad "bad-late" 200)))
        rig (physics/prepare-rig fixtures)
        frame (observations fixtures {})
        volume (nth volumes 2)
        sampler (sampling/create {:workers 3 :partitions 8 :min-fixtures 1})
        slices (sampling/partition-rig fixtures rig 8)]
    (try
      (doseq [variant [:shared :separate]]
        (let [expected (failure #(serial variant volume rig frame (field/sampler rig frame)))
              finished (atom 0)
              counted (fn [sample-slice]
                        (fn [field]
                          (try (sample-slice field)
                               (finally (Thread/sleep 5) (swap! finished inc)))))
              split (mapv counted (slice-samplers variant slices frame))
              actual (failure #(sampling/sample sampler slices volume split))]
          (is (instance? clojure.lang.ExceptionInfo expected))
          (is (= {:beam-angle 180 :lens-radius-m 0.119} (ex-data expected)))
          (is (= (class expected) (class actual)) (pr-str variant))
          (is (= (ex-message expected) (ex-message actual)))
          (is (= (ex-data expected) (ex-data actual)) "The earlier fixture in patch order fails first")
          (is (= 8 @finished) "No slice is still sampling when the failure surfaces")))
      (let [invalid (assoc volume :strength 2)
            expected (failure #(field/sample invalid rig frame))
            actual (failure #(sampling/sample sampler slices invalid (slice-samplers :separate slices frame)))]
        (is (instance? clojure.lang.ExceptionInfo expected))
        (is (= [(ex-message expected) (ex-data expected)] [(ex-message actual) (ex-data actual)])))
      (finally (close! sampler)))))

(deftest a-closed-sampler-finishes-the-frame-on-the-calling-thread
  (let [fixtures (patch 120)
        rig (physics/prepare-rig fixtures)
        frame (observations fixtures {})
        sampler (sampling/create {:workers 3 :partitions 4 :min-fixtures 1})
        slices (sampling/partition-rig fixtures rig 4)]
    (close! sampler)
    (is (.isTerminated (executor sampler)))
    (doseq [volume (take 4 volumes)]
      (is (nil? (difference (serial :separate volume rig frame nil)
                            (sampling/sample sampler slices volume
                                             (when (optical? volume) (slice-samplers :separate slices frame)))))))
    (is (zero? (.getPoolSize (executor sampler))))))

(defn- pixel [i]
  {:id (keyword (str "px-" i))
   :position [(- (* 0.05 (mod (* i 37) 200)) 5.0) (+ 0.5 (* 0.01 (mod (* i 53) 300))) (- (* 0.02 (mod (* i 71) 200)) 2.0)]
   :universe (inc (quot i 170)) :address (inc (* 3 (mod i 170)))
   :channels {:red 1 :green 2 :blue 3}})

(def ^:private stage
  (assoc (control/new-show
          {:registry {:effects {:blue {:id :blue :version 1 :params {} :outputs {:blue 1.0}}}
                      :cues {:blue {:id :blue :version 1 :effect-id :blue}}}})
         :rig {:fixtures (mapv pixel (range 360))}))

(defn- host [batch? options]
  (let [captured (atom nil)
        capture! (fn [universes _frame-id]
                   (reset! captured (into (sorted-map) (map (fn [[universe data]] [universe (vec data)])) universes)))]
    {:runtime (show/create-runtime {:model stage :batch? batch? :worker-timeout-ms 10000
                                    :spatial-sampling options :sink {:send! capture!}})
     :captured captured}))

(defn- submit! [{:keys [runtime]} now op payload]
  (let [receipt (show/submit! runtime op payload)]
    (show/tick! runtime now)
    (is (nil? (get-in @(:state runtime) [:status :error])) (pr-str (:status @(:state runtime))))
    (let [ack (deref receipt 1000 {:status :timeout})]
      (is (= :accepted (:status ack)) (pr-str ack))
      ack)))

(defn- field-cue [id cell selection paint position]
  {:op :spawn :id id :page-id :main :cell cell :paint paint
   :volume {:shape :sphere :position position :radius 1.5 :selection selection
            :falloff {:inner-m 0.5 :outer-m 0.5 :curve :linear}}})

(defn- perform!
  "Light the patch, launch three overlapping field cues and move two of them.
  Returns the DMX and the published field members after every tick."
  [show]
  (let [observe (fn [] [@(:captured show)
                        (get-in @(:state (:runtime show)) [:model :variables :spatial :members])])
        _ (submit! show 0 :command {:op :cue/launch :cue-id :blue})
        first-id (:instance-id (submit! show 25 :spatial-cue
                                        (field-cue :first [0 0] #{:position :beam} {:red 1.0 :blue 0.0} [-1.0 2.0 1.0])))
        second-id (:instance-id (submit! show 50 :spatial-cue
                                         (field-cue :second [1 0] #{:position :beam} {:green 1.0} [0.5 2.0 1.5])))
        _ (submit! show 75 :spatial-cue (field-cue :third [2 0] #{:position} {:red 0.5} [1.5 2.5 0.0]))
        move (fn [step]
               (let [now (+ 100 (* 25 step))]
                 (submit! show now :command {:op :param/set :instance-id first-id :param :position
                                             :value [(- (* 0.5 step) 2.0) 2.0 1.0]})
                 (submit! show now :command {:op :param/set :instance-id second-id :param :position
                                             :value [0.5 (+ 1.0 (* 0.25 step)) 1.5]})
                 (observe)))]
    (into [(observe)] (map move) (range 6))))

(deftest a-show-with-a-split-sampler-sends-the-serial-dmx-and-releases-its-workers
  (doseq [batch? [true false]]
    (let [serial-show (host batch? {:workers 1})
          split-show (host batch? {:workers 3 :partitions 5 :min-fixtures 1})
          pool (executor (:spatial-sampling (:runtime split-show)))]
      (try
        (let [expected (perform! serial-show)
              actual (perform! split-show)
              [dmx members] (peek expected)]
          (is (= (count expected) (count actual)))
          (doseq [[index before after] (map vector (range) expected actual)]
            (is (= (first before) (first after)) (str "DMX after step " index))
            (is (= (second before) (second after)) (str "members after step " index)))
          (is (= [1 2 3] (vec (keys dmx))))
          (is (some #(not (zero? %)) (mapcat #(take-nth 3 %) (vals dmx))) "A field painted red")
          (is (= 3 (count members)))
          (is (every? seq (vals members)))
          (is (nil? (get-in @(:state (:runtime serial-show)) [:spatial-cache :slices])))
          (is (nil? (:executor (:spatial-sampling (:runtime serial-show)))))
          (is (true? (get-in @(:state (:runtime split-show)) [:spatial-cache :slices :sliced?])))
          (is (= 5 (count (get-in @(:state (:runtime split-show)) [:spatial-cache :slices :optical]))))
          (is (pos? (.getPoolSize pool)) "The show's pool sampled"))
        (finally
          (show/stop! (:runtime serial-show))
          (show/stop! (:runtime split-show))))
      (is (.isTerminated pool))
      (is (zero? (.getPoolSize pool)))
      (is (nil? (get-in @(:state (:runtime split-show)) [:status :cleanup-error]))))))
