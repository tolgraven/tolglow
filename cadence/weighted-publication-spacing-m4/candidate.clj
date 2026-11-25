(in-ns 'afterglow.output.scheduler)

(defn- start-publications-candidate! [owner executor grid]
  (let [now (:now-ms owner)
        half-period (/ (:period-ms grid) 2.0)]
    (letfn [(queue-slot [wanted previous earliest]
              (at! owner executor (max (timeline/deadline grid wanted) earliest)
                   #(wake wanted previous earliest)))
            (wake [wanted previous earliest]
              (locking (:lock owner)
                (let [started (now) slot (timeline/slot-at grid started)]
                  (if (or (< slot wanted) (< started earliest))
                    (queue-slot wanted previous earliest)
                    (let [timing (send-slot! owner grid slot previous started)
                          handed-off (or (:sent-by-ms timing)
                                         (when (:failed? timing) (:finished-at-ms timing))
                                         started)
                          next-slot (max (inc slot) (inc (timeline/slot-at grid (now))))]
                      (queue-slot next-slot slot (+ handed-off half-period)))))))]
      (queue-slot 1 0 (timeline/deadline grid 1)))))
