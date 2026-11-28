(ns tolglow.studio.db-slice
  "Narrow app-db reads for panels written against the whole db.

  A panel that takes db subscribes instead to a slice: a db-shaped map that
  holds only the paths the panel and its helpers read. The slice
  subscription recomputes on each app-db change, but it compares equal unless
  one of those paths changed, so the panel re-renders only then. List every
  path the panel reads, including those its helper functions read; an
  unlisted path reads as absent."
  (:require [re-frame.core :as rf]))

(defn slice
  "A map holding the value of db at each of paths, at the same path. A path
  is a vector of map keys. Absent paths stay absent; a path inside another
  listed path adds nothing. An entry {:path path :select ks} holds the
  collection at path with only the keys ks of each item, for a panel that
  reads a few fields of items whose other fields change often."
  [db paths]
  (reduce (fn [out entry]
            (let [[path select] (if (map? entry) [(:path entry) (:select entry)] [entry nil])
                  value (get-in db path ::absent)]
              (cond
                (keyword-identical? value ::absent) out
                (and select (sequential? value)) (assoc-in out path (mapv #(select-keys % select) value))
                :else (assoc-in out path value))))
          {} paths))

(rf/reg-sub :studio/db-slice (fn [db [_ paths]] (slice db paths)))

(defn view
  "Render (panel slice & args) from its own subscription to the slice of
  app-db at paths, so it re-renders only when one of those paths changes.
  Keep paths a constant: it is the subscription's query."
  [paths panel & args]
  (into [panel @(rf/subscribe [:studio/db-slice paths])] args))
