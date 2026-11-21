(ns tolglow.output.scheduler
  "Owned physical output clock. Effect evaluation only hands over immutable
  completions; the publisher never takes the show-owner lock or runs effects."
  (:require [afterglow.render.presentation :as policy])
  (:import [java.util.concurrent LinkedBlockingDeque ArrayBlockingQueue Executors ScheduledExecutorService TimeUnit]
           [java.util.concurrent.atomic AtomicLong]))

(defn validate-options [options]
  (when-not (map? options) (throw (ex-info "Output presentation must be a map" {})))
  (when (seq (remove #{:mode :period-ms :latency-ms :fade-ms} (keys options)))
    (throw (ex-info "Unknown output presentation setting" {:settings options})))
  (:options (policy/new-state (policy/compile-plan []) options)))

(defn create
  "Inert owner; send! receives newly owned universe arrays and a packet ID.
  now-ms is a physical monotonic clock, never the musical/show clock."
  [{:keys [send! now-ms receipt-capacity]}]
  (let [origin (System/nanoTime)]
    {:send! send! :now-ms (or now-ms #(/ (- (System/nanoTime) origin) 1.0e6))
     :gate (atom nil) :counter (atom 0)
     :receipts (when receipt-capacity (ArrayBlockingQueue. (int receipt-capacity)))
     :receipt-drops (AtomicLong. 0)
     :queue (LinkedBlockingDeque. 3) :lock (Object.) :closed? (atom false)
     :scheduler (atom nil) :state (atom nil) :status (atom {:published 0 :coalesced 0})}))

(defn begin-frame!
  "Capture a completion identity before evaluation. Epoch/patch/policy/darkness
  changes invalidate all older completions and buffered bright frames."
  [owner epoch patch options dark?]
  (when-not @(:closed? owner)
    (let [current @(:gate owner)
          key [epoch patch options (boolean dark?)]
          gate (if (= key (:key current)) current
                   (let [plan (policy/compile-plan patch)
                         options (dissoc (validate-options options) :epoch)
                         gate {:key key :generation (inc (:generation current 0))
                               :epoch epoch :plan plan :options options :dark? (boolean dark?)}]
                     (reset! (:gate owner) gate)
                     gate))]
      {:generation (:generation gate) :epoch epoch :frame-id (swap! (:counter owner) inc)
       :time-ms ((:now-ms owner))})))

(defn complete!
  "Freeze an exclusively owned renderer result and offer without blocking the
  evaluator. At most three completions wait; excess oldest packets coalesce."
  [owner token rendered]
  (when (and token (not @(:closed? owner))
             (= (:generation token) (:generation @(:gate owner))))
    (let [packet (policy/freeze-frame (assoc token :completed-at-ms ((:now-ms owner))) rendered)
          marker (:calibration token)
          packet (cond-> packet marker
                   (assoc :calibration (dissoc marker :expected-bytes)
                          :calibration-verified?
                          (every? (fn [[[universe index] value]]
                                    (= value (get-in packet [:universes universe index]))) (:expected-bytes marker))))
          entry {:generation (:generation token) :packet packet}
          ^LinkedBlockingDeque queue (:queue owner)]
      (when-not (.offerLast queue entry)
        (.pollFirst queue)
        (swap! (:status owner) update :coalesced inc)
        (.offerLast queue entry))
      true)))

(defn publish!
  "Present one physical deadline. Safe to call manually with an injected clock.
  Transport work is serialized here, independently from the show/effect owner."
  [owner]
  (locking (:lock owner)
    (when-not @(:closed? owner)
      (when-let [gate @(:gate owner)]
        (let [now ((:now-ms owner))
              old @(:state owner)
              state (if (= (:generation old) (:generation gate)) (:policy old)
                        (policy/new-state (:plan gate) (assoc (:options gate) :epoch (:epoch gate))))
              completions (java.util.ArrayList. 3)
              _ (.drainTo ^LinkedBlockingDeque (:queue owner) completions 3)
              state (reduce (fn [state entry]
                              (if (= (:generation gate) (:generation entry))
                                (let [{next-state :state accepted? :accepted?} (policy/complete state (:packet entry))]
                                  (if accepted?
                                    (update next-state :history
                                            (fn [history]
                                              (update history (dec (count history)) merge
                                                      (select-keys (:packet entry) [:calibration :calibration-verified?]))))
                                    next-state)) state)) state completions)
              {next-state :state packet :packet} (policy/present state now)
              packet (if (or (:dark? gate) (nil? packet))
                       {:frame-id (:frame-id packet 0) :epoch (:epoch gate)
                        :calibration (:calibration packet) :calibration-verified? false
                        :universes (into {} (map (fn [u] [u (vec (repeat 512 0))])) (get-in gate [:plan :universes]))
                        :presentation {:mode (get-in gate [:options :mode]) :presented-at-ms now
                                       :blackout? (:dark? gate) :waiting? (nil? packet)}}
                       packet)]
          (reset! (:state owner) {:generation (:generation gate) :policy next-state})
          ;; A changed gate must not allow a previously bright sample to escape.
          ;; No host code takes this lock while owning a renderer buffer.
          (when (and packet (= (:generation gate) (:generation @(:gate owner))))
            (let [transport (policy/materialize packet)]
              (when-let [send! (:send! owner)]
                (send! (:universes transport) (:frame-id transport))
                ;; No callback or universe copy here. Queue processing is owned
                ;; by the show coordinator, away from the physical deadline.
                (when-let [^ArrayBlockingQueue mailbox (when (:calibration packet) (:receipts owner))]
                  (let [presentation (:presentation packet)
                        ^AtomicLong drops (:receipt-drops owner)
                        receipt {:epoch (:epoch packet) :generation (:generation gate)
                                 :frame-id (:frame-id packet) :sample-at-ms (:time-ms packet)
                                 :published-at-ms ((:now-ms owner)) :presentation presentation
                                 :calibration (:calibration packet)
                                 :eligible? (boolean (and (:calibration-verified? packet)
                                                         (= 1.0 (:gain presentation))
                                                         (zero? (:weight presentation 0))
                                                         (not (:fallback? presentation))
                                                         (not (:waiting? presentation))
                                                         (not (:blackout? presentation))))
                                 :dropped-before (.get drops)}]
                    (if (.offer mailbox receipt) (.set drops 0) (.incrementAndGet drops)))))
              (swap! (:status owner) #(-> % (update :published inc)
                                           (assoc :last (:presentation packet)
                                                  :accepted (:accepted next-state) :dropped (:dropped next-state))))))
          packet)))))


(defn drain-receipts!
  "Drain the optional bounded physical-output mailbox off the publisher thread."
  [owner]
  (if-let [^ArrayBlockingQueue mailbox (:receipts owner)]
    (let [result (java.util.ArrayList.)] (.drainTo mailbox result) (vec result)) []))

(defn status [owner]
  (merge @(:status owner) {:scheduled? (some? @(:scheduler owner)) :closed? @(:closed? owner)
                          :pending (.size ^LinkedBlockingDeque (:queue owner))
                          :options (:options @(:gate owner))}))

(defn start! [owner period-ms]
  (locking (:lock owner)
    (when (or @(:closed? owner) @(:scheduler owner))
      (throw (ex-info "Output publisher is closed or already scheduled" {})))
    (let [scheduler (Executors/newSingleThreadScheduledExecutor)]
      (reset! (:scheduler owner) scheduler)
      (.scheduleAtFixedRate scheduler
                            ^Runnable #(try (publish! owner)
                                            (catch Exception error
                                              (swap! (:status owner) assoc :error (.getMessage error))))
                            0 (long (* 1.0e6 period-ms)) TimeUnit/NANOSECONDS)))
  owner)

(defn close!
  "Fence late completions and wait only for an already sending transport, never
  an effect worker. The host sends its final zero frame after this returns."
  [owner]
  (locking (:lock owner)
    (reset! (:closed? owner) true)
    (when-let [^ScheduledExecutorService scheduler @(:scheduler owner)] (.shutdownNow scheduler))
    (.clear ^LinkedBlockingDeque (:queue owner))
    (when-let [^ArrayBlockingQueue mailbox (:receipts owner)] (.clear mailbox))
    (reset! (:state owner) nil))
  nil)
