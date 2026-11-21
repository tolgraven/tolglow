(ns tolglow.system-controls-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.edn :as edn]
            [afterglow.control.model :as control]
            [afterglow.runtime.engine :as engine]
            [afterglow.render.frame :as render]
            [tolglow.compiler :as compiler]
            [tolglow.compiler.batch :as batch]))

(def rig [{:id :lamp :position [0 0 0] :universe 1 :address 1 :channels {:dimmer 1}}])
(def definition {:id :light :version 1 :params {:level {:type :number :default 0.8 :bounds [0 1]}}
                 :outputs {:dimmer {:op :param :key :level}}})
(defn apply! [show op]
  (let [result (control/apply-command show (merge {:command-id (str (:revision show)) :source :test
                                                 :show-id (:show-id show) :epoch (:epoch show) :time-ms (:time-ms show)} op))]
    (is (= :accepted (get-in result [:ack :status])))
    (:model result)))
(defn launch [options]
  (-> (control/new-show)
      (apply! {:op :registry/publish :kind :effects :definition definition})
      (apply! {:op :cue/put :cue {:id :light :effect-id :light}})
      (apply! {:op :cue/launch :cue-id :light :launch options})))
(defn advance [show ms beat compile]
  (engine/advance (assoc-in show [:transport :beat] beat) {:time-ms ms :fixtures rig} compile))
(defn value-byte [result]
  (bit-and 255 (aget ^bytes (get-in (render/resolve-contributions rig (:contributions result)
                                                               {:include-values? false}) [:universes 1]) 0)))

(deftest independent-system-release-works-with-real-scalar-and-typed-compilers
  (doseq [compile [compiler/compile-effect (batch/compiler-for-rig rig)]]
    (let [show (launch {:system {:attack {:beats 2} :release {:beats 2}}})
          attack (advance show 500 1 compile)
          full (advance (:model attack) 1500 2 compile)
          ended (apply! (:model full) {:op :instance/end :instance-id "0:1"})
          tail (advance ended 2500 3 compile)
          done (advance (:model tail) 4000 4 compile)]
      (is (= 102 (value-byte attack)))
      (is (= 204 (value-byte full)))
      (is (= 102 (value-byte tail)) "An effect without an authored envelope survives for the system release")
      (is (= :releasing (get-in tail [:model :instances "0:1" :phase])))
      (is (= {} (get-in done [:model :instances])))
      (is (= 0 (value-byte done)))
      (is (= {:level 0.8} (get-in tail [:model :instances "0:1" :params]))))))

(deftest paused-typed-show-round-trips-and-live-parameter-controls-preserve-state
  (let [compile (batch/compiler-for-rig rig)
        active (advance (launch {:system {:opacity 0.5}}) 100 1 compile)
        paused (apply! (:model active) {:op :instance/pause :instance-id "0:1"})
        saved (edn/read-string (pr-str paused))
        held (advance saved 9000 90 compile)
        edited (apply! (:model held) {:op :param/set :instance-id "0:1" :param :level :value 0.4})
        updated (advance edited 10000 100 compile)
        resumed (apply! (:model updated) {:op :instance/resume :instance-id "0:1"})
        playing (advance resumed 10100 101 compile)]
    (is (= 102 (value-byte active) (value-byte held)))
    (is (= 51 (value-byte updated) (value-byte playing)))
    (is (= (:state (get-in active [:model :instances "0:1"])) (:state (get-in updated [:model :instances "0:1"]))))
    (is (= 2 (get-in playing [:model :instances "0:1" :system :last-clock :beat])))
    (is (= 200 (get-in playing [:model :instances "0:1" :system :last-clock :time-ms])))))
