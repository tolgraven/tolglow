(ns tolglow.compiler.capability-test
  "Untargeted effects on a mixed rig (#688): captured DMX from a running show."
  (:require [afterglow.control.model :as control]
            [afterglow.runtime.engine :as engine]
            [clojure.test :refer [deftest is testing]]
            [tolglow.compiler.capability :as capability]
            [tolglow.show :as show]))

(def fixtures
  [{:id :rgb :position [0 1 0] :universe 1 :address 1 :channels {:red 1 :green 2 :blue 3 :dimmer 4}
    :groups [:stage]}
   {:id :par :position [1 1 0] :universe 1 :address 5 :channels {:dimmer 1 :white 2} :groups [:stage]}
   {:id :hazer :position [3 0.3 -3] :universe 1 :address 10 :channels {:fog 1} :groups [:stage]}])

(def ^:private slots {:rgb [0 4] :par [4 6] :hazer [9 10]})

(def effects
  {:background {:id :background :version 1 :params {}
                :outputs [{:attribute :white :value 0.5 :targets [:par]}
                          {:attribute :fog :value 0.25 :targets [:hazer]}]}
   :wash {:id :wash :version 1 :params {} :outputs {:color [1.0 0.0 0.0]}}
   :dim {:id :dim :version 1 :params {} :outputs {:dimmer 0.5}}
   :pinned {:id :pinned :version 1 :params {} :targets [:rgb :par] :outputs {:color [0.0 1.0 0.0]}}
   :split {:id :split :version 1 :params {}
           :outputs [{:attribute :color :value [0.0 0.0 1.0] :targets [:rgb :hazer]}]}})

(def cues (into {} (map (fn [id] [id {:id id :version 1 :effect-id id}])) (keys effects)))

(defn- runtime [batch?]
  (let [captured (atom nil)]
    (assoc (show/create-runtime
            {:model (assoc (control/new-show {:registry {:effects effects :cues cues}}) :rig {:fixtures fixtures})
             :batch? batch? :worker-timeout-ms 10000 :workspace-options nil
             :sink {:send! (fn [universes _] (reset! captured (mapv #(bit-and 255 %) (take 10 (get universes 1)))))}})
           ::dmx captured)))

(defn- submit! [rt now op payload]
  (let [receipt (show/submit! rt op payload)]
    (show/tick! rt now)
    (deref receipt 1000 {:status :timeout})))

(defn- bytes-of [dmx id] (let [[from to] (slots id)] (subvec dmx from to)))

(defn- faults [rt] (keep :fault (:instances (show/snapshot rt))))

(defn- capture
  "Launch the background, then each payload in order; return DMX, acks and faults."
  [batch? launches]
  (let [rt (runtime batch?)]
    (try
      (submit! rt 0 :command {:op :cue/launch :cue-id :background})
      (let [acks (mapv (fn [[op payload]] (submit! rt 25 op payload)) launches)]
        (show/tick! rt 50)
        (show/tick! rt 75)
        {:dmx @(::dmx rt) :acks acks :faults (faults rt) :status (get-in @(:state rt) [:status :error])
         :instances (:instances (show/snapshot rt))})
      (finally (show/stop! rt)))))

(deftest capability-follows-the-renderer-patch
  (is (capability/capable? (first fixtures) :color))
  (is (not (capability/capable? (second fixtures) :color)))
  (is (every? #(capability/capable? % :dimmer) fixtures) "Every patched head has a virtual dimmer")
  (is (capability/capable? {:id :unpatched} :color) "A fixture without channels is never filtered")
  (is (= [{:attribute :color :heads [:par :hazer] :requires [:red :green :blue]}]
         (capability/skipped (:wash effects) fixtures)))
  (is (= [] (capability/skipped (:dim effects) fixtures))))

(deftest untargeted-color-cue-lights-capable-heads-and-leaves-the-rest
  (doseq [batch? [true false]]
    (testing (str "batch? " batch?)
      (let [control (capture batch? [])
            {:keys [dmx acks] :as run} (capture batch? [[:command {:op :cue/launch :cue-id :wash}]])
            ack (first acks)]
        (is (= [] (:faults control) (:faults run)))
        (is (nil? (:status run)))
        (is (= :accepted (:status ack)))
        (is (= [255 0 0] (subvec dmx 0 3)) "The RGB fixture takes the colour")
        (is (= (bytes-of (:dmx control) :par) (bytes-of dmx :par)))
        (is (= (bytes-of (:dmx control) :hazer) (bytes-of dmx :hazer)))
        (is (= [255 128] (bytes-of dmx :par)) "The background still drives the white par")
        (is (pos? (first (bytes-of dmx :hazer))) "The background still drives the hazer")
        (is (= {:skipped [{:attribute :color :heads [:par :hazer] :requires [:red :green :blue]}]}
               (:target-resolution ack)
               (:target-resolution (some #(when (= :wash (:cue-id %)) %) (:instances run)))))))))

(deftest untargeted-spatial-cue-paints-color-only-where-it-can
  (doseq [batch? [true false]]
    (testing (str "batch? " batch?)
      (let [control (capture batch? [])
            spawn {:op :spawn :id :orb :page-id :main :cell [0 0]
                   :volume {:shape :ball :position [1 1 -1] :velocity [0 0 0] :gravity [0 0 0]
                            :radius 0.5 :restitution 0.75 :selection #{:position}
                            :falloff {:inner-m 20 :outer-m 20 :curve :linear}}
                   :paint {:color [1.0 0.0 0.0]}}
            {:keys [dmx acks] :as run} (capture batch? [[:spatial-cue spawn]])]
        (is (= :accepted (:status (first acks))) (pr-str (first acks)))
        (is (= [] (:faults run)))
        (is (nil? (:status run)))
        (is (pos? (nth dmx 0)) "The volume paints the RGB fixture red")
        (is (= [0 0] (subvec dmx 1 3)))
        (is (= (bytes-of (:dmx control) :par) (bytes-of dmx :par)))
        (is (= (bytes-of (:dmx control) :hazer) (bytes-of dmx :hazer)))))))

(deftest untargeted-dimmer-still-reaches-every-head-with-a-dimmer
  (doseq [batch? [true false]]
    (testing (str "batch? " batch?)
      (let [{:keys [dmx acks] :as run} (capture batch? [[:command {:op :cue/launch :cue-id :dim}]])]
        (is (= :accepted (:status (first acks))))
        (is (not (contains? (first acks) :target-resolution)) "Nothing is skipped")
        (is (= [] (:faults run)))
        (is (= 128 (nth dmx 3)) "RGB fixture dimmer")
        (is (= 128 (nth dmx 4)) "White par dimmer")))))

(deftest capability-reads-the-program-the-engine-renders
  ;; An explicit nil launch target is untargeted, as the engine renders it:
  ;; the authored heads stay explicit and an unauthored colour wash is
  ;; filtered, never refused, on heads without colour.
  (let [pinned (:pinned effects) wash (:wash effects)]
    (doseq [instance [{:definition pinned} {:definition pinned :targets nil} {:definition pinned :targets [:par]}
                      {:definition pinned :targets []} {:definition wash} {:definition wash :targets nil}
                      {:definition wash :targets [:rgb]} {:definition wash :targets []}]]
      (is (= (engine/launch-program (:definition instance) (:targets instance)) (capability/program instance))
          (pr-str instance)))
    (is (= pinned (capability/program {:definition pinned :targets nil})))
    (is (= [{:attribute :color :heads [:par] :requires [:red :green :blue]}]
           (capability/launch-conflicts {:definition pinned :targets nil} true fixtures)))
    (is (= [] (capability/launch-conflicts {:definition wash :targets nil} true fixtures))
        "An unauthored wash with nil targets is filtered like an untargeted launch")
    (is (= [{:attribute :color :heads [:par :hazer] :requires [:red :green :blue]}]
           (capability/skipped (capability/program {:definition wash :targets nil}) fixtures)))))

(deftest explicit-targets-on-an-incapable-head-are-refused-at-launch
  (doseq [batch? [true false]]
    (testing (str "batch? " batch?)
      (let [control (capture batch? [])
            refused [[:command {:op :cue/launch :cue-id :wash :launch {:targets [:rgb :hazer]}}]
                     [:command {:op :cue/launch :cue-id :wash :launch {:selector {:fixture-ids [:par]}}}]
                     [:command {:op :cue/launch :cue-id :pinned}]
                     [:command {:op :effect/launch :effect-id :split}]]
            {:keys [acks dmx] :as run} (capture batch? refused)]
        (is (= [:incapable-target] (distinct (map :code acks))) (pr-str acks))
        (is (= [:rejected] (distinct (map :status acks))))
        (is (= [[{:attribute :color :heads [:hazer] :requires [:red :green :blue]}]
                [{:attribute :color :heads [:par] :requires [:red :green :blue]}]
                [{:attribute :color :heads [:par] :requires [:red :green :blue]}]
                [{:attribute :color :heads [:hazer] :requires [:red :green :blue]}]]
               (map #(get-in % [:details :conflicts]) acks)))
        (is (re-find #"color on hazer" (:message (first acks))))
        (is (= [:background] (map :cue-id (:instances run))) "No refused launch left an instance")
        (is (= [] (:faults run)))
        (is (= (:dmx control) dmx))))))

(deftest scoped-and-capable-explicit-launches-are-accepted
  (doseq [batch? [true false]]
    (testing (str "batch? " batch?)
      (let [{:keys [acks dmx] :as run}
            (capture batch? [[:command {:op :cue/launch :cue-id :wash :launch {:selector {:group-id :stage}}}]
                             [:command {:op :cue/launch :cue-id :wash :launch {:targets [:rgb]}}]
                             [:command {:op :cue/launch :cue-id :wash :launch {:selector :all}}]])]
        (is (= [:accepted :accepted :accepted] (map :status acks)) (pr-str acks))
        (is (= [:par :hazer] (get-in (first acks) [:target-resolution :skipped 0 :heads]))
            "A group scope is filtered, not refused")
        (is (not (contains? (second acks) :target-resolution)))
        (is (= [:par :hazer] (get-in (nth acks 2) [:target-resolution :skipped 0 :heads]))
            "The :all scope is filtered, not refused")
        (is (= [] (:faults run)))
        (is (= [255 0 0] (subvec dmx 0 3)))))))
