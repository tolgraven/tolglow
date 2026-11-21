(ns tolglow.composition.time
  "Authoring facade over the shared explicit musical clock."
  (:require [afterglow.time :as time]))

(def default-meter time/default-meter)
(def normalize-meter time/normalize-meter)
(def validate-duration time/validate-duration)
(def normalize-duration time/normalize-duration)
(def duration->beats time/duration->beats)
(def frame-clock time/frame-clock)
(def coordinate time/coordinate)
(def start time/start)
(def sample time/sample)
(def estimate-ms time/estimate-ms)
