(ns tolglow.studio.db-slice
  "Narrow app-db reads for panels written against the whole db.

  A panel that takes db subscribes instead to a slice: a db-shaped map that
  holds only the paths the panel and its helpers read. The slice
  subscription recomputes on each app-db change, but it compares equal unless
  one of those paths changed, so the panel re-renders only then. List every
  path the panel reads, including those its helper functions read; an
  unlisted path reads as absent.

  Development builds (goog.DEBUG) hand the panel a guarded slice: its maps
  above the listed paths warn once when a read misses a key that no listed
  path covers, which is how an unlisted read shows up. Release builds hand
  over the plain map."
  (:require [re-frame.core :as rf]))

(defn- entry-path [entry] (if (map? entry) (:path entry) entry))

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

;; Development guard -----------------------------------------------------------

(defonce misses
  ;; [paths read-path] of every unlisted read seen, for tests and inspection.
  (atom #{}))

(defn- prefix? [a b] (and (<= (count a) (count b)) (= a (subvec b 0 (count a)))))

(defn- covered?
  "Whether a read at path is inside, or on the way to, a listed path."
  [listed path]
  (some #(or (prefix? % path) (prefix? path %)) listed))

(defn- above-listed?
  "Whether path is a strict prefix of a listed path and inside none: the map
  there holds only some of its keys."
  [listed path]
  (and (some #(and (< (count path) (count %)) (prefix? path %)) listed)
       (not-any? #(prefix? % path) listed)))

(declare guard-at)

(defn- miss! [paths path]
  (when-not (contains? @misses [paths path])
    (swap! misses conj [paths path])
    (js/console.warn "db-slice: a panel read" (pr-str path) "which its slice does not list" (pr-str paths))))

(deftype GuardedMap [m prefix listed paths]
  Object
  (toString [_] (str m))
  ILookup
  (-lookup [this k] (-lookup this k nil))
  (-lookup [_ k not-found]
    (let [path (conj prefix k)
          value (get m k ::absent)]
      (if (keyword-identical? value ::absent)
        ;; Reagent looks up :key on a component's first argument.
        (do (when-not (or (keyword-identical? k :key) (covered? listed path)) (miss! paths path)) not-found)
        (guard-at value path listed paths))))
  IFn
  (-invoke [this k] (-lookup this k nil))
  (-invoke [this k not-found] (-lookup this k not-found))
  IAssociative
  (-contains-key? [_ k] (contains? m k))
  (-assoc [_ k v] (assoc m k v))
  IMap
  (-dissoc [_ k] (dissoc m k))
  ICollection
  (-conj [_ entry] (conj m entry))
  IEmptyableCollection
  (-empty [_] {})
  ICounted
  (-count [_] (count m))
  ISeqable
  (-seq [_] (seq m))
  IKVReduce
  (-kv-reduce [_ f init] (reduce-kv f init m))
  IEquiv
  (-equiv [_ other] (= m (if (instance? GuardedMap other) (.-m ^js other) other)))
  IHash
  (-hash [_] (hash m))
  IMeta
  (-meta [_] (meta m))
  IPrintWithWriter
  (-pr-writer [_ writer opts] (-pr-writer m writer opts)))

(defn- guard-at [value path listed paths]
  (if (and (map? value) (not (instance? GuardedMap value)) (above-listed? listed path))
    (GuardedMap. value path listed paths)
    value))

(defn guard
  "In development builds, the slice of paths wrapped so that a read of an
  unlisted key warns; otherwise the slice itself. Extra keys a derived
  subscription adds at the top are read as present."
  [value paths]
  (if ^boolean goog/DEBUG
    (GuardedMap. value [] (mapv entry-path paths) paths)
    value))

(defn with-overrides
  "value with each {path replacement} of overrides in place."
  [value overrides]
  (reduce-kv assoc-in value overrides))

(defn without-overridden
  "paths without those overrides replace, so a change there in app-db does not
  change the subscription."
  [paths overrides]
  (if (seq overrides) (into [] (remove #(contains? overrides %)) paths) paths))

(defn subscribe
  "Deref the slice of app-db at paths, with overrides ({path value}) in place,
  guarded as `guard` describes. The subscription leaves out the overridden
  paths."
  ([paths] (subscribe nil paths))
  ([overrides paths]
   (guard (with-overrides @(rf/subscribe [:studio/db-slice (without-overridden paths overrides)]) overrides) paths)))

(defn view
  "Render (panel slice & args) from its own subscription to the slice of
  app-db at paths, so it re-renders only when one of those paths changes.
  Keep paths a constant: it is the subscription's query."
  [paths panel & args]
  (into [panel (subscribe paths)] args))

(defn view-as
  "As `view`, with overrides ({path value}, each path listed) in place in the
  slice: a view rendered in its own window (#669) is that window's current
  view, whatever the main page shows."
  [overrides paths panel & args]
  (into [panel (subscribe overrides paths)] args))
