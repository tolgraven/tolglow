(ns tolglow.studio.frame-stream
  "Own high-rate projection lanes split from the show state stream.

  The show socket still delivers one token-checked sequence of full views and
  patches. The stream event applies each message to the app-db projection
  together with these lanes, at the same acknowledged token, and installs the
  lane frame before acknowledging it. A lane frame is an immutable CLJS map; it
  never enters app-db, so a Room frame causes no app-db write of its values,
  no subscription recompute over them and no Reagent render.

  Consumers attach with an explicit owner and receive whole frames, never
  deltas. A paused consumer receives nothing; resuming delivers the latest
  frame when it is newer than the last one delivered. Detaching is idempotent
  and only removes the attachment that created the release function."
  (:require [re-frame.core :as rf]))

(def lane-paths
  "Lane ID -> path in the server view whose subtree the lane owns."
  {:room [:output :values]})

(defonce ^:private frames (atom {}))

(defonce ^:private consumers (atom {}))

(defn baseline
  "The installed frame of lane, or nil. Its :token and :identity name the
  exact server baseline its values belong to."
  [lane]
  (get @frames lane))

(defn- identity-of [view] (select-keys view [:runtime-id :show-id :epoch]))

(defn split-view
  "Separate every lane's subtree from a server view. Returns [view lanes],
  where lanes maps every lane to {:present? bool :value typed-subtree}."
  [view]
  (reduce-kv (fn [[view lanes] lane path]
               (let [parent (get-in view (pop path))]
                 (if (and (map? parent) (contains? parent (peek path)))
                   [(update-in view (pop path) dissoc (peek path))
                    (assoc lanes lane {:present? true :value (get parent (peek path))})]
                   [view (assoc lanes lane {:present? false :value nil})])))
             [view {}] lane-paths))

(defn join-view
  "Restore lane subtrees into an app-db projection so a patch can address them.
  Every lane must hold a frame of this exact token and server identity. If one
  does not, the projection loses its token: the next patch then requests a
  full snapshot instead of applying to a partial baseline."
  [{:keys [token view] :as projection} installed]
  (if-not projection projection
    (let [joined (reduce-kv (fn [view lane path]
                              (let [frame (get installed lane)]
                                (cond
                                  (not (and frame (= token (:token frame)) (= (:identity frame) (identity-of view)))) (reduced nil)
                                  (not (:present? frame)) view
                                  (map? (get-in view (pop path))) (assoc-in view path (:value frame))
                                  :else (reduced nil))))
                            view lane-paths)]
      (if joined (assoc projection :view joined) (dissoc projection :token)))))

(defn frame
  "The lane frame of an accepted view: the lane value with the clock and
  identity fields of the same token."
  [token view {:keys [present? value]}]
  {:token token
   :identity (identity-of view)
   :revision (:revision view)
   :frame-number (:frame-number view)
   :time-ms (:time-ms view)
   :present? present?
   :value value})

(defn- deliver! [lane owner entry frame]
  (try ((:on-frame entry) frame)
       (catch :default error
         (.error js/console "Frame consumer failed" (pr-str [lane owner]) error)))
  ;; Record what this attachment received, unless it detached or re-attached
  ;; from inside its own callback.
  (swap! consumers (fn [all]
                     (if (identical? (:attachment entry) (get-in all [lane owner :attachment]))
                       (assoc-in all [lane owner :delivered] (:token frame))
                       all))))

(defn- offer! [lane owner]
  (let [entry (get-in @consumers [lane owner])
        current (get @frames lane)]
    (when (and entry current (:active? entry) (not= (:token current) (:delivered entry)))
      (deliver! lane owner entry current))))

(defn install!
  "Replace lane's frame and offer it to every active consumer."
  [lane frame]
  (swap! frames assoc lane frame)
  (doseq [owner (keys (get @consumers lane))]
    (offer! lane owner)))

(defn attach!
  "Register (on-frame frame) as owner's consumer of lane, replacing an earlier
  attachment by the same owner. An active consumer immediately receives the
  installed frame. Returns a release function."
  [lane owner on-frame & {:keys [active?] :or {active? true}}]
  (let [attachment #js {}]
    (swap! consumers assoc-in [lane owner] {:on-frame on-frame :active? (boolean active?)
                                            :attachment attachment :delivered nil})
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
  "Owner -> {:active? bool :delivered token} for lane, for inspection."
  [lane]
  (update-vals (get @consumers lane) #(select-keys % [:active? :delivered])))

(defn reset-all!
  "Drop every frame and consumer. Tests and a closed Studio only."
  []
  (reset! frames {})
  (reset! consumers {}))

(rf/reg-cofx :studio/frame-baselines
  (fn [cofx _] (assoc cofx :frame-baselines @frames)))

(rf/reg-fx :studio/frame-install
  (fn [installs]
    (doseq [[lane frame] installs] (install! lane frame))))
