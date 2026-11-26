(ns tolglow.automation.grid
  "The one rule auto mode uses to place a musical timer on the beat, bar or
  phrase grid, for decision intervals and section plans alike (#217, #670).

  A timer that follows another starts where the previous one ended, so a tick
  that lands a frame after the line does not move every later line by that
  frame. It does so only while at least a quarter of its length remains. A timer with nothing to follow (a start, a new configuration, a long
  gap, a previous end shifted off the grid by a clock rebase) ends on the
  first grid line at least its length less half a grid step away. Its first
  run is therefore between half a step shorter and half a step longer than
  its length: never one frame (no catch-up just before a line), never almost
  two lengths."
  (:require [afterglow.time :as time]))

(def ^:private eps 1e-9)

(defn step
  "Beats between grid lines for a musical duration: one of its own unit (beat,
  bar or phrase), or its own length when that is shorter."
  [duration value meter]
  (min value (time/duration->beats {(ffirst duration) 1} meter)))

(defn on-grid? [beat step]
  (< (abs (- beat (* step (Math/rint (/ beat step))))) eps))

(defn start
  "Beat at which a timer of value beats on a step grid starts, at beat now.
  previous is the end of the timer it follows, or nil. The previous end is
  kept when it is not after now, at least a quarter of a length remains
  before the next end, and it is on the grid (or the length is not a whole
  number of steps, whose ends need not be)."
  [value step now previous]
  (let [whole? (on-grid? value step)]
    (cond
      (and previous (<= previous (+ now eps))
           ;; A late frame continues; a jump that leaves less than a quarter
           ;; of the next interval would decide twice in a row, so it re-anchors.
           (>= (- (+ previous value) now) (- (/ value 4) eps))
           (or (not whole?) (on-grid? previous step)))
      previous
      ;; Exactly on a line: start there and keep the exact coordinate.
      (on-grid? now step) now
      :else (- (* step (Math/ceil (/ (- (+ now value) (/ step 2) eps) step))) value))))
