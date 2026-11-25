(ns history-binding-bench
  "Test-only bridge to the unchanged Studio history renderer."
  (:require [tolglow.studio.parameter-visualization :as visual]))

(def schema {:type :number :bounds [0 1]})

(defn- entry-at [ring i]
  (let [value (js-invoke ring "valueAt" i)]
    {:beat (js-invoke ring "beatAt" i)
     :values (when (js/Number.isFinite value) [value])}))

(deftype RingRead [ring n]
  IStack
  (-peek [_] (when (pos? n) (entry-at ring (dec n))))
  (-pop [_] (throw (js/Error. "Read-only history view")))
  ISeqable
  (-seq [_] (seq (map #(entry-at ring %) (range n)))))

(defn- ring-view [ring snapshot?]
  (let [n (aget ring "length")]
    (if snapshot?
      (mapv #(entry-at ring %) (range n))
      (RingRead. ring n))))

(defn- prepare [sample]
  [{:time-ms (aget sample "timeMs")
    :beat (aget sample "beat")
    :source (aget sample "source")
    :sample-period-ms (aget sample "samplePeriodMs")}
   (visual/components schema (aget sample "value"))])

(defn- append [history prepared]
  (let [[clock values] prepared]
    (visual/append-sample (or history []) clock values)))

(defn- draw [history window]
  (some-> (visual/chart-paths history window schema) :paths first))

(defn init! []
  (aset js/globalThis "historyBindingBench"
        #js {:prepare prepare
             :append append
             :size count
             :draw draw
             :drawRing (fn [ring snapshot? window] (draw (ring-view ring snapshot?) window))
             :show (fn [history window] (pr-str (visual/history-chart schema history window)))
             :showRing (fn [ring snapshot? window]
                         (pr-str (visual/history-chart schema (ring-view ring snapshot?) window)))}))
