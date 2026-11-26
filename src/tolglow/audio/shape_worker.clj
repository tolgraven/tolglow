(ns tolglow.audio.shape-worker
  "The window-end analysis thread for audio-shaped LFOs (#676). The capture
  thread hands over immutable jobs from `shape/advance-bank` and returns to
  reading input; this one daemon thread finishes them with `shape/finish-job`
  and publishes each result. Pending work is one job per LFO and the newest
  wins, so a slow analysis can delay a shape but never queue without bound.
  Nothing here opens a device or touches show state."
  (:require [tolglow.audio.shape :as shape])
  (:import [java.util.concurrent Semaphore TimeUnit]))

(defn start!
  "Start a worker. publish! receives {lfo-id extraction} for each finished job,
  on the worker thread; the extraction carries the job's :finished-ns. An
  exception from publish! is passed to on-error! (optional) and the worker
  keeps going."
  ([publish!] (start! publish! nil))
  ([publish! on-error!]
   (let [pending (atom {})
         signal (Semaphore. 0)
         state (atom :running)
         work (fn []
                (let [[jobs] (reset-vals! pending {})]
                  (doseq [[id job] (sort-by key (fn [a b] (compare (str a) (str b))) jobs)]
                    (when (not= :cancelled @state)
                      (try
                        (publish! {id (assoc (shape/finish-job job) :finished-ns (System/nanoTime))})
                        (catch Throwable error (when on-error! (on-error! error))))))))
         thread (doto (Thread. ^Runnable
                               (fn []
                                 (loop []
                                   (let [woken? (try (.tryAcquire signal 100 TimeUnit/MILLISECONDS)
                                                     (catch InterruptedException _ false))]
                                     (when woken? (.drainPermits signal) (work))
                                     (case @state
                                       :running (recur)
                                       ;; Drain: finish what was handed over, then stop.
                                       :draining (when (seq @pending) (work))
                                       nil))))
                               "tolglow-audio-shapes")
                  (.setDaemon true) (.start))]
     {:submit! (fn [jobs]
                 (when (and (seq jobs) (= :running @state))
                   ;; The newest job per LFO wins.
                   (swap! pending into (map (juxt :id identity)) jobs)
                   (.release signal)))
      :pending (fn [] (count @pending))
      :stop! (fn stop!
               ([] (stop! false 1000))
               ([drain? timeout-ms]
                (compare-and-set! state :running (if drain? :draining :cancelled))
                (when-not drain? (reset! pending {}))
                (.release signal)
                (when-not (identical? thread (Thread/currentThread))
                  (try (.join thread (long timeout-ms)) (catch InterruptedException _ (.interrupt (Thread/currentThread)))))
                (not (.isAlive thread))))})))
