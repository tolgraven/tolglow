(ns tolglow.studio.frame-stream
  "Own high-rate projection lanes split from the show state stream.

  The show socket still delivers one token-checked sequence of full views and
  patches. The stream event applies each message to the app-db projection
  together with these lanes, at the same acknowledged token. It installs the
  lane frame, acknowledges the message, and only then notifies consumers, so
  consumer work never delays the next server frame. A lane frame is an
  immutable CLJS map; its owned subtrees never enter app-db, so a Room frame
  causes no app-db write of its values, no subscription recompute over them
  and no Reagent render.

  Consumers attach with an explicit owner and receive whole frames, never
  deltas. A consumer that supplies :schedule receives at most one frame per
  scheduled callback, the newest. A paused consumer receives nothing; resuming
  delivers the latest frame when it is newer than the last one delivered.
  Releasing is idempotent and only removes the attachment that created the
  release function."
  (:require [re-frame.core :as rf]))

(def lane-paths
  "Lane ID -> part -> path in the server view whose subtree the lane owns."
  {:room {:values [:output :values]
          :audition [:draft-output :values]}})

(defonce ^:private frames (atom {}))

(defonce ^:private consumers (atom {}))

(defn baseline
  "The installed frame of lane, or nil. Its :token and :identity name the
  exact server baseline its values belong to."
  [lane]
  (get @frames lane))

(defn- identity-of [view] (select-keys view [:runtime-id :show-id :epoch]))

(defn split-view
  "Separate every lane's subtrees from a server view. Returns [view lanes],
  where lanes maps every lane to {part typed-subtree} for the parts present."
  [view]
  (reduce-kv (fn [[view lanes] lane parts]
               (reduce-kv (fn [[view lanes] part path]
                            (let [parent (get-in view (pop path))]
                              (if (and (map? parent) (contains? parent (peek path)))
                                [(update-in view (pop path) dissoc (peek path))
                                 (assoc-in lanes [lane part] (get parent (peek path)))]
                                [view lanes])))
                          [view (assoc lanes lane {})] parts))
             [view {}] lane-paths))

(defn join-view
  "Restore lane subtrees into an app-db projection so a patch can address them.
  Every lane must hold a frame of this exact token and server identity. If one
  does not, the projection loses its token: the next patch then requests a
  full snapshot instead of applying to a partial baseline."
  [{:keys [token view] :as projection} installed]
  (if-not projection projection
    (let [joined (reduce-kv
                  (fn [view lane parts]
                    (let [frame (get installed lane)]
                      (if-not (and frame (= token (:token frame)) (= (:identity frame) (identity-of view)))
                        (reduced nil)
                        (reduce-kv (fn [view part value]
                                     (let [path (get parts part)]
                                       (if (and path (map? (get-in view (pop path))))
                                         (assoc-in view path value)
                                         (reduced nil))))
                                   view (:value frame)))))
                  view lane-paths)]
      (if joined (assoc projection :view joined) (dissoc projection :token)))))

(defn frame
  "The lane frame of an accepted view: the lane's parts with the clock and
  output-gating fields of the same token, which also remain in app-db."
  [token view parts]
  {:token token
   :identity (identity-of view)
   :revision (:revision view)
   :frame-number (:frame-number view)
   :time-ms (:time-ms view)
   :master (:master view)
   :blackout? (:blackout? view)
   :cut? (:cut? view)
   :last-intents (get-in view [:status :last-intents])
   :atmosphere (get-in view [:output :atmosphere])
   :value parts})

(defonce ^:private failures (atom {}))

(defn- report-failure!
  "Log the first failure of each kind per consumer, then only count repeats."
  [lane owner error]
  (let [kind [lane owner (str (or (.-message error) error))]
        n (get (swap! failures update kind (fnil inc 0)) kind)]
    (when (= 1 n)
      (.error js/console "Frame consumer failed; repeats are counted, not logged" (pr-str [lane owner]) error))))

(defn- deliver! [lane owner entry frame]
  ;; Record the token first: a consumer may detach or re-attach from inside
  ;; its own callback.
  (swap! consumers (fn [all]
                     (if (identical? (:attachment entry) (get-in all [lane owner :attachment]))
                       (assoc-in all [lane owner :delivered] (:token frame))
                       all)))
  (try ((:on-frame entry) frame)
       (catch :default error (report-failure! lane owner error))))

(defn- deliverable [lane owner]
  (let [entry (get-in @consumers [lane owner])
        current (get @frames lane)]
    (when (and entry current (:active? entry) (not= (:token current) (:delivered entry)))
      [entry current])))

(defn- offer! [lane owner]
  (when-let [[entry current] (deliverable lane owner)]
    (if-let [schedule (:schedule entry)]
      (when-not (:scheduled? entry)
        (swap! consumers assoc-in [lane owner :scheduled?] true)
        (schedule (fn []
                    (when (identical? (:attachment entry) (get-in @consumers [lane owner :attachment]))
                      (swap! consumers assoc-in [lane owner :scheduled?] false)
                      ;; The newest frame at the time of the callback wins.
                      (when-let [[entry current] (deliverable lane owner)]
                        (deliver! lane owner entry current))))))
      (deliver! lane owner entry current))))

(defn install!
  "Replace lane's frame. This is the installation an ACK may follow; it calls
  no consumer."
  [lane frame]
  (swap! frames assoc lane frame))

(defn notify!
  "Offer lane's installed frame to every active consumer."
  [lane]
  (doseq [owner (keys (get @consumers lane))]
    (offer! lane owner)))

(defn attach!
  "Register (on-frame frame) as owner's consumer of lane, replacing an earlier
  attachment by the same owner. :schedule, when given, takes a callback and
  runs it later (for example on the consumer window's next animation frame);
  deliveries are coalesced to one per callback. An active consumer is offered
  the installed frame immediately. Returns a release function."
  [lane owner on-frame & {:keys [active? schedule] :or {active? true}}]
  (let [attachment #js {}]
    (swap! consumers assoc-in [lane owner] {:on-frame on-frame :active? (boolean active?) :schedule schedule
                                            :attachment attachment :delivered nil :scheduled? false})
    (offer! lane owner)
    (fn release! []
      (swap! consumers (fn [all]
                         (if (identical? attachment (get-in all [lane owner :attachment]))
                           (let [remaining (dissoc (get all lane) owner)]
                             (if (empty? remaining) (dissoc all lane) (assoc all lane remaining)))
                           all))))))

(defn set-active!
  "Pause or resume owner's consumer of lane. Resuming offers the installed
  frame when the consumer has not received that token."
  [lane owner active?]
  (when (get-in @consumers [lane owner])
    (swap! consumers update-in [lane owner] assoc :active? (boolean active?))
    (when active? (offer! lane owner))))

(defn attached
  "Owner -> {:active? :delivered :failures} for lane, for inspection."
  [lane]
  (into {} (map (fn [[owner entry]]
                  [owner (assoc (select-keys entry [:active? :delivered])
                                :failures (reduce + 0 (keep (fn [[[l o] n]] (when (and (= l lane) (= o owner)) n)) @failures)))]))
        (get @consumers lane)))

(defn reset-all!
  "Drop every frame, consumer and failure count. Tests and a closed Studio only."
  []
  (reset! frames {})
  (reset! consumers {})
  (reset! failures {}))

(rf/reg-cofx :studio/frame-baselines
  (fn [cofx _] (assoc cofx :frame-baselines @frames)))

(rf/reg-fx :studio/frame-install
  (fn [installs]
    (doseq [[lane frame] installs] (install! lane frame))))

(rf/reg-fx :studio/frame-notify
  (fn [lanes]
    (doseq [lane lanes] (notify! lane))))
