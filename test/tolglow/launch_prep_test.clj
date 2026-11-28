(ns tolglow.launch-prep-test
  "Quantised launches prepared off the frame and installed at their beat
  (#918). Each show runs beside an inline twin given the same inputs; every
  frame's captured DMX and the committed instances must be identical, so
  preparation only ever shortens admission. Tasks are held and run by the
  test, except in the race, where a real thread races the beat. No clock is
  read."
  (:require [afterglow.control.model :as control]
            [afterglow.sync :as sync]
            [afterglow.sync.session :as session]
            [clojure.test :refer [deftest is testing]]
            [tolglow.compiler.capability :as capability]
            [tolglow.connections :as connections]
            [tolglow.cue-timing :as cue-timing]
            [tolglow.show :as show]
            [tolglow.show.launch-prep :as launch-prep]
            [tolglow.spatial :as spatial]))

(defn- rgb [i] (keyword (str "rgb-" (inc i))))

(def ^:private fixtures
  (conj (mapv (fn [i] {:id (rgb i) :position [i 1 0] :universe 1 :address (inc (* 4 i))
                       :channels {:red 1 :green 2 :blue 3 :dimmer 4} :groups [:stage :rgbs]})
              (range 6))
        {:id :par :position [7 1 0] :universe 1 :address 30 :channels {:dimmer 1 :white 2} :groups [:stage]}
        {:id :hazer :position [8 0.3 -3] :universe 1 :address 40 :channels {:fog 1} :groups [:stage]}))

(def ^:private effects
  {:wash {:id :wash :version 1 :params {} :outputs {:color [1.0 0.0 0.0]}}
   :dim {:id :dim :version 1 :params {:level {:type :number :default 0.5 :bounds [0.0 1.0]}}
         :outputs {:dimmer {:op :param :key :level}}}
   :pinned {:id :pinned :version 1 :params {} :targets [(rgb 0) :par] :outputs {:color [0.0 1.0 0.0]}}})

(def ^:private cues
  {:wash {:id :wash :version 1 :effect-id :wash}
   :dim {:id :dim :version 1 :effect-id :dim :priority 5}
   :pinned {:id :pinned :version 1 :effect-id :pinned}
   :pair {:id :pair :version 1 :effect-id :dim :selector {:fixture-ids [(rgb 1) (rgb 3)]}}
   :rgbs {:id :rgbs :version 1 :effect-id :wash :selector {:group-id :rgbs}}})

(defn- show-model []
  (assoc (control/new-show {:registry {:effects effects :cues cues}}) :rig {:fixtures fixtures}))

(defn- launches
  "Eleven grid launches in a fixed order: an untargeted wash that skips the
  par and hazer, six per-head dims, two selector cues, a pinned cue and an
  explicit wash on the par, both refused for a head without colour."
  [prefix grid]
  (let [launch (fn [i cue-id launch]
                 (cond-> {:op :cue/launch :cue-id cue-id :command-id (str prefix "-" i) :grid grid
                          :source {:adapter :repl :session-id "prep"}}
                   launch (assoc :launch launch)))]
    (-> [(launch 0 :wash nil)]
        (into (map (fn [i] (launch (inc i) :dim {:mode :one-shot :duration {:beats 1} :targets [(rgb i)]}))) (range 6))
        (into [(launch 7 :pair nil) (launch 8 :rgbs nil) (launch 9 :pinned nil) (launch 10 :wash {:targets [:par]})]))))

(defn- held-tasks
  "A :prepare-launches function that holds each task until run!."
  []
  (let [tasks (atom [])]
    {:submit (fn [task] (swap! tasks conj task))
     :run! (fn [] (let [[pending] (reset-vals! tasks [])] (doseq [task pending] (task)) (count pending)))}))

(defn- instances [rt]
  (vec (sort-by :order (vals (get-in @(:state rt) [:model :instances])))))

(defn- run-show
  "Run `script` on a show: (script rt step! prepare!), where (step! t) ticks
  at t and (prepare! ) runs the held preparation tasks. Returns each tick's
  output, committed instances and transport beat, and the preparation counts."
  [prepare? script]
  (let [output (atom nil)
        frames (atom [])
        {:keys [submit run!]} (held-tasks)
        rt (show/create-runtime {:model (show-model) :workspace-options nil :effect-log! (fn [_]) :workers 2
                                 :prepare-launches (when prepare? submit)
                                 :sink {:send! (fn [universes _]
                                                 (reset! output (into (sorted-map) (map (fn [[u b]] [u (mapv #(bit-and 255 %) b)])) universes)))}})]
    (try
      (script rt
              (fn [t]
                (show/tick! rt t)
                (swap! frames conj {:t t :output @output :instances (instances rt)
                                    :beat (get-in @(:state rt) [:model :transport :beat])}))
              (if prepare? run! (constantly 0)))
      {:frames @frames :counts (some-> (:launch-prep rt) launch-prep/counts)
       :prep-state (some-> (:launch-prep rt) :state deref (select-keys [:submitted :prepared]))
       :model (:model @(:state rt))}
      (finally (show/stop! rt)))))

(defn- twins
  "The same script on an inline show and on a preparing one."
  [script]
  [(run-show false script) (run-show true script)])

(defn- same-frames? [inline prepared]
  (and (= (count (:frames inline)) (count (:frames prepared)))
       (every? true? (map = (:frames inline) (:frames prepared)))))

(defn- first-diff [inline prepared]
  (first (keep (fn [[a b]] (when (not= a b) {:t (:t a) :inline a :prepared b}))
               (map vector (:frames inline) (:frames prepared)))))

(defn- submit-all! [rt commands]
  (doseq [command commands] (show/submit! rt command)))

(defn- ticks [step! from to] (doseq [t (range from (inc to) 25)] (step! t)))

(defn- launch-frame
  "The first frame with launched instances, and the beat of the one before."
  [{:keys [frames]}]
  (let [i (first (keep-indexed #(when (seq (:instances %2)) %1) frames))]
    (when i [(nth frames i) (:beat (nth frames (dec i)))])))

(deftest prepared-launches-install-at-the-beat-and-match-inline-admission
  (doseq [bpm [70 120 180]]
    (testing (str bpm " BPM")
      (let [[inline prepared]
            (twins (fn [rt step! prepare!]
                     (show/submit! rt {:op :transport/set :transport {:bpm bpm}})
                     (step! 0)
                     (submit-all! rt (launches "grid" {:beats 1}))
                     (step! 25)
                     (when (:launch-prep rt) (is (= 1 (prepare!)) "One task prepares the whole burst"))
                     (ticks step! 50 1500)))
            [frame before] (launch-frame prepared)]
        (is (same-frames? inline prepared) (pr-str (first-diff inline prepared)))
        (is (= {:prepared 11 :installed 11} (:counts prepared)))
        (is (= {:submitted #{} :prepared {}} (:prep-state prepared)) "Fired launches are forgotten")
        (testing "every launch lands on the first frame at or past beat 1, in launch order"
          (is (< before 1.0))
          (is (<= 1.0 (:beat frame)))
          (is (= [:wash :dim :dim :dim :dim :dim :dim :pair :rgbs] (mapv :cue-id (:instances frame))))
          (is (= [nil [(rgb 0)] [(rgb 1)] [(rgb 2)] [(rgb 3)] [(rgb 4)] [(rgb 5)] [(rgb 1) (rgb 3)]
                  (mapv rgb (range 6))]
                 (mapv :targets (:instances frame)))))
        (testing "the capability verdicts a show prepared are the inline ones"
          (is (= {:skipped [{:attribute :color :heads [:par :hazer] :requires [:red :green :blue]}]}
                 (:target-resolution (first (:instances frame)))))
          (is (= [:incapable-target :incapable-target]
                 (mapv #(:code (cue-timing/receipt (:model prepared) %)) ["grid-9" "grid-10"]))))))))

(deftest a-launch-whose-preparation-is-not-ready-at-the-beat-lands-inline
  ;; The policy (#918): nothing waits for the worker and there is no frame
  ;; budget. A launch whose preparation has not finished is admitted inline
  ;; on its beat; the late result is dropped.
  (let [late (atom nil)
        [inline prepared]
        (twins (fn [rt step! prepare!]
                 (step! 0)
                 (submit-all! rt (launches "late" {:beats 1}))
                 (ticks step! 25 500)
                 (when (:launch-prep rt)
                   (is (= 1 (prepare!)) "The worker finishes only after the beat")
                   (reset! late (launch-prep/counts (:launch-prep rt))))
                 (ticks step! 525 800)))
        [frame before] (launch-frame prepared)]
    (is (same-frames? inline prepared) (pr-str (first-diff inline prepared)))
    (is (< before 1.0))
    (is (= 500 (:t frame)) "Launch latency stays 0 frames")
    (is (= 9 (count (:instances frame))))
    (is (= {:not-ready 11 :prepared 11} @late))
    (is (= {:not-ready 11 :prepared 11} (:counts prepared)))
    (is (= {:submitted #{} :prepared {}} (:prep-state prepared)) "The late result was never kept")))

(defn- basis-change
  "Schedule the launches, prepare them, apply `change!` before the beat and
  play past it."
  [change!]
  (twins (fn [rt step! prepare!]
           (step! 0)
           (submit-all! rt (launches "basis" {:beats 1}))
           (step! 25)
           (prepare!)
           (step! 50)
           (change! rt step!)
           (ticks step! 100 900))))

(deftest every-basis-change-between-prepare-and-beat-falls-back
  (testing "rig edit: every launch resolves and checks against the new rig"
    ;; The sixth head leaves the group the :rgbs cue selects.
    (let [[inline prepared] (basis-change (fn [rt step!]
                                            (show/submit! rt :rig (-> (get-in @(:state rt) [:model :rig])
                                                                      (assoc-in [:fixtures 6 :position] [7 2 0])
                                                                      (assoc-in [:fixtures 5 :groups] [:stage])))
                                            (step! 75)))]
      (is (same-frames? inline prepared) (pr-str (first-diff inline prepared)))
      (is (= (mapv rgb (range 5)) (:targets (first (filter #(= :rgbs (:cue-id %)) (:instances (first (launch-frame prepared))))))))
      (is (= {:prepared 11 :stale 11} (:counts prepared)))))
  (testing "definition publish: targets are reused, the capability verdict is worked out again"
    (let [[inline prepared] (basis-change (fn [rt step!]
                                            (show/submit! rt :definition {:kind :effects :definition
                                                                          (assoc (:wash effects) :version 2 :outputs {:dimmer 0.7})})
                                            (step! 75)))]
      (is (same-frames? inline prepared) (pr-str (first-diff inline prepared)))
      (is (= {:prepared 11 :installed 8 :verdict-stale 3} (:counts prepared)))
      (is (= [[2 nil] [2 nil] [2 nil]] (keep #(when (= :wash (:effect-id %)) [(:effect-version %) (:target-resolution %)])
                                     (:instances (first (launch-frame prepared)))))
          "The new revision writes only dimmers, so no head is skipped or refused")))
  (testing "parameter change on a cue: its launches resolve inline and take the new value"
    (let [[inline prepared] (basis-change (fn [rt step!]
                                            (show/submit! rt {:op :cue/put :cue (-> (:dim cues) (dissoc :version) (assoc :params {:level 0.2}))})
                                            (step! 75)))]
      (is (same-frames? inline prepared) (pr-str (first-diff inline prepared)))
      (is (= {:prepared 11 :installed 5 :stale 6} (:counts prepared)))
      (is (= [0.2 0.2 0.2 0.2 0.2 0.2]
             (keep #(when (= :dim (:cue-id %)) (get-in % [:params :level])) (:instances (first (launch-frame prepared))))))))
  (testing "launch cancel: the cancelled launch never fires and its preparation is dropped"
    (let [[inline prepared] (basis-change (fn [rt step!]
                                            (show/submit! rt :cue-cancel {:command-id "basis-3" :reason :operator-cancel})
                                            (step! 75)
                                            (when-let [prep (:launch-prep rt)]
                                              (is (not (contains? (:submitted @(:state prep)) "basis-3"))))))]
      (is (same-frames? inline prepared) (pr-str (first-diff inline prepared)))
      (is (= {:prepared 11 :installed 10} (:counts prepared)))))
  (testing "epoch change: a restart misses every waiting launch"
    (let [[inline prepared] (basis-change (fn [rt step!]
                                            (show/submit! rt {:op :show/restart})
                                            (step! 75)))]
      (is (same-frames? inline prepared) (pr-str (first-diff inline prepared)))
      (is (empty? (:instances (peek (:frames prepared)))))
      (is (= {:prepared 11} (:counts prepared)))
      (is (= {:submitted #{} :prepared {}} (:prep-state prepared))))))

(deftest a-transport-seek-misses-prepared-launches-as-it-does-inline
  (let [run (fn [prepare?]
              (run-show prepare?
                        (fn [rt step! prepare!]
                          (let [manager (connections/sync (:connections rt))
                                clock! (fn [position at discontinuity?]
                                         (session/apply-clock! manager nil {:type :clock :source-id :midi-clock :time-ms at
                                                                            :beat position :bpm 120 :playing? true :confidence 1
                                                                            :discontinuity? discontinuity?}))]
                            (sync/select-source! manager :midi-clock)
                            (clock! 0.25 125 false)
                            (submit-all! rt (launches "seek" {:beats 1}))
                            (step! 125)
                            (prepare!)
                            (clock! 32 150 true)
                            (step! 150)
                            (ticks step! 175 400)))))
        inline (run false) prepared (run true)]
    (is (same-frames? inline prepared) (pr-str (first-diff inline prepared)))
    (is (empty? (:instances (peek (:frames prepared)))))
    (is (= {:prepared 11} (:counts prepared)))
    (is (= {:submitted #{} :prepared {}} (:prep-state prepared)))))

(deftest a-stopped-transport-holds-prepared-launches-and-resume-does-not-catch-up
  ;; Beat 0.6 at t = 300. Stopped until t = 900, the transport reaches beat 1
  ;; 200 ms after it resumes; the launches land on that frame, not earlier.
  (let [[inline prepared]
        (twins (fn [rt step! prepare!]
                 (step! 0)
                 (submit-all! rt (launches "stop" {:beats 1}))
                 (step! 25)
                 (prepare!)
                 (ticks step! 50 275)
                 (show/submit! rt {:op :transport/set :transport {:playing? false}})
                 (ticks step! 300 875)
                 (show/submit! rt {:op :transport/set :transport {:playing? true}})
                 (ticks step! 900 1300)))
        [frame before] (launch-frame prepared)]
    (is (same-frames? inline prepared) (pr-str (first-diff inline prepared)))
    (is (= 1100 (:t frame)))
    (is (< before 1.0))
    (is (= {:prepared 11 :installed 11} (:counts prepared)))))

(deftest a-worker-racing-the-beat-never-changes-what-lands
  ;; Each preparation runs on its own thread while the owner ticks on without
  ;; waiting, so it finishes before, during or after its beat. Twenty bursts
  ;; on a quarter-beat grid; the frames match the inline twin throughout.
  (let [threads (atom [])
        run (fn [submit]
              (let [output (atom nil) frames (atom [])
                    rt (show/create-runtime {:model (show-model) :workspace-options nil :effect-log! (fn [_]) :workers 2
                                             :prepare-launches submit
                                             :sink {:send! (fn [universes _] (reset! output (mapv #(bit-and 255 %) (get universes 1))))}})]
                (try
                  (show/tick! rt 0)
                  (doseq [round (range 20)]
                    (let [t0 (* round 500)]
                      (submit-all! rt (launches (str "race-" round) {:beats 1/4}))
                      (doseq [t (range (+ t0 25) (+ t0 501) 25)]
                        (show/tick! rt t)
                        (swap! frames conj [@output (instances rt)]))))
                  {:frames @frames :counts (some-> (:launch-prep rt) launch-prep/counts)}
                  (finally (show/stop! rt)))))
        inline (run nil)
        raced (run (fn [task] (let [thread (Thread. ^Runnable task)] (swap! threads conj thread) (.start thread))))]
    (doseq [^Thread thread @threads] (.join thread 5000))
    (is (= (count (:frames inline)) (count (:frames raced))))
    (is (every? true? (map = (:frames inline) (:frames raced))))
    (let [{:keys [installed not-ready] :or {installed 0 not-ready 0}} (:counts raced)]
      (is (= 220 (+ installed not-ready)) (pr-str (:counts raced))))))

(deftest preparation-reads-only-what-it-can-check
  (let [model (show-model)
        command {:op :cue/launch :cue-id :pair :command-id "p"}
        prepared (spatial/prepare-launch model command)]
    (testing "a selector cue is resolved ahead with its verdict"
      (is (= {:selector {:fixture-ids [(rgb 1) (rgb 3)]} :targets [(rgb 1) (rgb 3)]} (:resolved prepared)))
      (is (= [] (get-in prepared [:verdict :conflicts]))))
    (testing "a volume selector follows moving bodies and is left to the beat"
      (is (nil? (spatial/prepare-launch (assoc-in model [:registry :cues :pair :selector] {:volume-id :orb}) command))))
    (testing "a retried command ID returns its receipt and is left to the beat"
      (is (nil? (spatial/prepare-launch (assoc-in model [:control :history "p"] {:command command}) command))))
    (testing "an unknown cue is left to inline admission, which reports it"
      (is (nil? (spatial/prepare-launch model (assoc command :cue-id :nope)))))
    (testing "a verdict applies only to the definition and rig it read"
      (let [verdict (:verdict prepared)
            instance {:definition (:dim effects) :targets [(rgb 1) (rgb 3)]}
            scoped (assoc command :launch (:resolved prepared))]
        (is (capability/verdict-applies? verdict instance scoped fixtures))
        (is (not (capability/verdict-applies? verdict (assoc instance :definition (assoc (:dim effects) :version 2)) scoped fixtures)))
        (is (not (capability/verdict-applies? verdict instance scoped (into [] fixtures))))
        (is (not (capability/verdict-applies? verdict (assoc instance :targets [(rgb 1)]) scoped fixtures)))))))

(deftest a-prepared-launch-keeps-parameters-a-host-sets-as-it-fires
  ;; A staged cue (#477) gives its queued launch the stage's values as it
  ;; fires. Targets and verdict do not read launch parameters, so the
  ;; prepared work still installs and the instance takes those values.
  (let [model (show-model)
        command {:op :cue/launch :cue-id :pair :command-id "staged" :show-id (:show-id model) :epoch (:epoch model)
                 :source {:adapter :repl :session-id "prep"} :time-ms 0}
        prepared (spatial/prepare-launch model command)
        fired (assoc-in command [:launch :params] {:level 0.3})
        outcomes (atom [])
        ahead (binding [spatial/*prepared-launches* {:lookup {"staged" prepared} :outcome (fn [_ o] (swap! outcomes conj o))}]
                (spatial/apply-command model fired))
        inline (spatial/apply-command model fired)
        instance (get-in ahead [:model :instances (get-in ahead [:ack :instance-id])])]
    (is (= [:installed] @outcomes))
    (is (= inline ahead))
    (is (= 0.3 (get-in instance [:params :level])))
    (is (= [(rgb 1) (rgb 3)] (:targets instance)))))
