(ns tolglow.cue-spawn
  "Publish a spawned effect and cue, place the cue on a grid page and launch
  it, as one transaction on a private model value. Spatial volume cues
  (#407) and video surface cues (#674) both spawn through here."
  (:require [clojure.string :as str]))

(defn- fail! [message data] (throw (ex-info message data)))

(defn- available-cell
  "The preferred grid cell when it is free, else the first free one; throws
  when the page has none."
  [page preferred]
  (let [w (:columns page 8) h (:rows page 8) cells (:cells page {})
        available? (fn [p] (and (vector? p) (= 2 (count p)) (every? nat-int? p)
                                (< (first p) w) (< (second p) h) (not (contains? cells p))))]
    (or (if (some? preferred) (when (available? preferred) preferred)
            (first (for [y (range h) x (range w) :when (available? [x y])] [x y])))
        (fail! "Choose an empty cell on this page" {:page-id (:id page) :cell preferred}))))

(defn- accept [model command payload now label]
  (let [result (command model payload now)]
    (when-not (= :accepted (get-in result [:ack :status]))
      (fail! (get-in result [:ack :message] (str label " command rejected")) {:ack (:ack result)}))
    result))

(defn place-and-launch
  "Apply :registry/publish of effect, :cue/put of cue, :page/put placing the
  cue in cell (or the first free one) on page-id (created as page-name when
  missing), then :cue/launch with launch (nil reads as {}; false skips the
  launch). Throws ex-info, leaving the caller's model untouched, when any
  step is refused or the page is full. The result's :ack carries :cue-id,
  :page-id and :cell. label names the caller in a refusal message."
  [model {:keys [effect cue page-id page-name cell launch label]} now command]
  (let [page (get-in model [:pages page-id] {:id page-id :name (or page-name (str/capitalize (name page-id))) :cells {}})
        pos (available-cell page cell)
        label (or label "Spawned cue")
        id (:id cue)
        model (:model (accept model command {:op :registry/publish :kind :effects :definition effect} now label))
        model (:model (accept model command {:op :cue/put :cue cue} now label))
        model (:model (accept model command {:op :page/put :page (assoc-in page [:cells pos] id)} now label))
        launched (if (false? launch)
                   {:model model :events [] :ack {:status :accepted}}
                   (accept model command {:op :cue/launch :cue-id id :launch (or launch {})} now label))]
    (update launched :ack assoc :cue-id id :page-id page-id :cell pos)))
