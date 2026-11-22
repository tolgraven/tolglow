(ns tolglow.parity-test
  (:require [clojure.test :refer [deftest is testing]]
            [afterglow.core :as application]
            [afterglow.channels :as channels]
            [afterglow.effects :as effects]
            [afterglow.effects.dimmer :as dimmer]
            [afterglow.effects.params :as params]
            [afterglow.rhythm :as rhythm]
            [afterglow.show :as show]
            [afterglow.show-context :refer [*show* with-show]]
            [thi.ng.color.core :as clr]
            [tolglow.color :as color]
            [tolglow.config :as config]
            [tolglow.cue :as cue]
            [tolglow.fixtures :as fixtures]
            [tolglow.fx :as fx]
            [tolglow.osc :as osc]
            [tolglow.param :as param]
            [tolglow.util :as util]
            [tolglow.vars :as vars]))

(deftest application-namespaces-load-without-starting-a-show
  (let [state {:web @application/web-server :osc @application/osc-server
               :repl @application/nrepl-server}
        unexpected (fn [& _] (throw (ex-info "Namespace load started output" {})))]
    (with-redefs [show/start! unexpected
                  show/add-effect! unexpected
                  application/start-web-server unexpected]
      (binding [*show* (show/show :universes [1])]
        (require 'tolglow.core :reload)
        (require 'tolglow.live :reload)))
    (is (= state {:web @application/web-server :osc @application/osc-server
                  :repl @application/nrepl-server}))
    (is (nil? (find-ns 'tolglow.viz)))
    (is (map? config/fixture-types))
    (is (seq config/fixture-types))))

(deftest color-protocol-implements-its-declared-methods
  (doseq [input ["red" :red [0.0 0.5 0.5] (clr/hsla 0.0 0.5 0.5)]]
    (is (color/colorable? input))
    (is (color/color? (color/colorize input)))
    (is (color/color? (color/ensure-colorable input))))
  (is (false? (color/colorable? 42)))
  (is (false? (color/colorable? nil))))

(deftest color-conversion-preserves-dynamic-parameters
  (let [p (params/->Param "Color" true color/used-type
                         (fn [_ _ _] (clr/hsla 0.5 0.5 0.5))
                         (fn [_ _ _] nil))]
    (is (identical? p (color/create p)))
    (is (identical? p (color/ensure-colorable p))))
  (is (= 0.75 (:s (color/create [0.2 1.0 0.5]))))
  (is (= [0.2 0.5 0.6 1.0] @(color/create 0.2 0.5 0.6 :mode :fraction))))

(deftest parameter-binding-keeps-explicit-false
  (is (= {:enabled false :gain 0}
         (param/bind-vars {:enabled {:arg false :default true}
                           :gain {:arg 0 :default 1}}))))

(deftest default-parameter-specs-work-with-the-fork-record-types
  (with-show (show/show :universes [1])
    (is (= color/used-type (get-in (param/param-specs) [:color :type])))
    (is (false? (params/resolve-param (param/bind-default :bool :enabled)
                                    *show* (rhythm/metro-snapshot (:metronome *show*)))))))

(deftest cue-variable-metadata-keeps-false-and-omits-nil
  (let [spec (vars/cue-map ["enabled" false])]
    (is (= false (:start spec)))
    (is (= :boolean (:type spec)))
    (is (not-any? nil? (vals spec))))
  (is (= [] (vars/auto nil)))
  (let [spec (zipmap (range 12) (range 12))]
    (is (= [spec] (vars/auto spec)))))

(deftest spatial-helpers-use-the-explicit-show
  (let [a {:dimensions (atom {:min-x 0 :max-x 10})}
        b {:dimensions (atom {:min-x 0 :max-x 100})}]
    (with-show b
      (is (= 1/2 (util/space-phase {:x 5} a)))
      (is (= 1/2 (util/x-phase {:x 5} a))))))

(deftest declarative-fixture-footprints-include-sparse-and-fine-offsets
  (is (= 9 (:fine-offset (fixtures/color :red 2 :fine 9))))
  (is (= {:offset 11 :fine-offset 23}
         (select-keys (first (fixtures/make-channels {:pan [1 13]} 10))
                      [:offset :fine-offset])))
  (is (thrown? clojure.lang.ExceptionInfo
               (fixtures/make-channels {:dimmer 500} 20)))
  (let [fixture (fixtures/create
                 {:channels {:pan [1 12] :dimmer 5}
                  :strip {:color-layout :rgb :pixels 2 :density 2}})]
    (is (= [[13 14 15] [16 17 18]]
           (mapv #(mapv :offset (:channels %)) (:heads fixture)))))
  (is (= 0.0 (:x (first (:heads (fixtures/pixel-strip 1)))))))

(deftest configured-fixture-definitions-have-unique-dmx-offsets
  (doseq [[id fixture] config/fixture-types]
    (testing (name id)
      (let [offsets (vec (keep identity
                              (mapcat (juxt :offset :fine-offset)
                                      (mapcat :channels (channels/expand-heads [fixture])))))]
        (is (every? #(<= 1 % 512) offsets))
        (is (= (count offsets) (count (set offsets))))))))

(deftest variable-watchers-register-the-supplied-function
  (let [s (show/show :universes [1])
        seen (atom [])
        callback (fn [k value] (swap! seen conj [k value]))]
    (with-show s
      (let [registration (util/watch-var :gain callback)]
        (is (= [:gain callback] registration))
        (show/set-variable! :gain 0.25)
        (is (= [[:gain 0.25]] @seen))
        (apply util/unhook-var registration)
        (show/set-variable! :gain 0.75)
        (is (= [[:gain 0.25]] @seen))))))

(defn- resolve-effect-to-buffer [effect s snapshot]
  (let [buffers {1 (byte-array 512)}
        groups (vals (group-by (juxt :kind :target-id)
                               (effects/generate effect s snapshot)))]
    (doseq [assigners groups]
      (effects/resolve-assignment (effects/run-assigners s snapshot assigners nil)
                                  s snapshot buffers))
    (mapv #(bit-and 255 %) (get buffers 1))))

(deftest existing-dimmer-cue-launches-renders-and-stops-without-output
  (with-show (show/show :universes [1])
    (show/patch-fixture! :wash-1 (fixtures/create {:channels {:dimmer 1 :rgbw 2}}) 1 1)
    (cue/dimmer nil 0 0 "white")
    (let [id (cue/launch! 0 0 :var-overrides {:level 127})
          effect (:effect (show/find-effect :dimmers))
          snapshot (rhythm/metro-snapshot (:metronome *show*))]
      (is (integer? id))
      (is (= 127 (first (resolve-effect-to-buffer effect *show* snapshot))))
      (is (false? (show/running?)))
      (show/end-effect! :dimmers :when-id id)
      (is (nil? (show/find-effect :dimmers))))))

(deftest thi-ng-colors-render-through-the-active-fork
  (with-show (show/show :universes [1])
    (show/patch-fixture! :wash-1 (fixtures/create {:channels {:rgbw 1}}) 1 1)
    (let [effect (fx/color (clr/hsla 0.0 1.0 0.5))
          snapshot (rhythm/metro-snapshot (:metronome *show*))]
      (is (= [255 0 0 0]
             (subvec (resolve-effect-to-buffer effect *show* snapshot) 0 4))))))

(deftest color-base-and-alpha-participate-in-parameter-resolution
  (with-show (show/show :universes [1])
    (let [snapshot (rhythm/metro-snapshot (:metronome *show*))
          base (params/build-variable-param :source-color :type params/color-type :default params/default-color)
          opacity (params/build-variable-param :opacity :frame-dynamic false)
          red (clr/hsla 0.0 1.0 0.5)
          blue (clr/hsla 0.67 1.0 0.5)]
      (show/set-variable! :source-color red)
      (show/set-variable! :opacity 0.25)
      (let [dynamic (param/color :base-color base)
            fixed (param/color :base-color red :a opacity)]
        (is (params/frame-dynamic-param? dynamic))
        (is (= red (params/resolve-param dynamic *show* snapshot)))
        (show/set-variable! :source-color blue)
        (is (= blue (params/resolve-param dynamic *show* snapshot)))
        (is (= 0.25 (clr/alpha fixed)))))))

(deftest oscillator-picker-uses-named-wave-order-and-bounds
  (with-show (show/show :universes [1])
    (let [snapshot (rhythm/->MetronomeSnapshot 0 120 4 4 125 1 1 1 0.25 0.0625 0.015625)
          selected (param/lfo-chooser {:max 1} [:square :sawtooth] :picker-param :pick)
          sample (fn [position]
                   (show/set-variable! :pick position)
                   (params/resolve-param selected *show* snapshot))]
      (is (== 1 (sample -1)))
      (is (== 1 (sample 0.9)))
      (is (== 0.25 (sample 1)))
      (is (== 0.25 (sample 2))))
    (cue/multi-lfo nil 0 0 [:sine :square] dimmer/dimmer-effect :color :blue)
    (let [[cue] (show/find-cue-grid-active-effect *show* 0 0)]
      (is (= (params/interpret-color "blue") (:color cue)))
      (is (some #(= "alpha" (:key %)) (:variables cue)))
      (is (integer? (cue/launch! 0 0))))))

(deftest random-picker-holds-for-the-requested-musical-interval
  (with-show (show/show :universes [1])
    (let [draws (atom 0)
          sample (param/rng :interval :bar :fade-fraction 0)
          snapshot (rhythm/metro-snapshot (:metronome *show*))]
      (with-redefs [param/pick-new-value (fn [& _] (swap! draws inc))]
        (doseq [[beat bar] [[1 1] [2 1] [3 1] [4 1] [5 2]]]
          (params/resolve-param sample *show* (assoc snapshot :beat beat :bar bar)))
        (is (= 2 @draws))))))

(deftest patch-helper-passes-position-and-options-to-the-show
  (with-redefs [show/patch-fixture! (fn [id fixture universe address & {:as options}]
                                      [id fixture universe address options])]
    (is (= [:wash {} 2 19 {:x 0.0 :y 0.0 :z 0.0}]
           (util/patch! :wash (constantly {}) 2 19)))
    (is (= [:wash {} 2 19 {:x 4 :y 2 :z 3 :heading 90}]
           (util/patch! :wash (constantly {}) 2 19 [1 2 3] [:x 4 :y nil :heading 90])))))

(deftest generated-assigners-receive-head-specific-resolved-values
  (with-show (show/show :universes [1])
    (let [snapshot (rhythm/metro-snapshot (:metronome *show*))
          head-param (params/->Param "Head ID" true Number (fn [_ _ head] (:id head)) nil)
          make-effect (fx/get-fx-maker "Scale" {:vars {:level 0}} (fn [] {:id head-param})
                                       (fn [_ _ _ previous values] (assoc values :previous previous))
                                       vector)
          assign (first (effects/generate (make-effect [] :level 3) *show* snapshot))]
      (is (map? ((get-in fx/fx-data [:bloom :arg-spec]))))
      (is (= {:level 3 :id 7 :previous 9} (assign *show* snapshot {:id 7} 9))))))

(deftest osc-registers-argument-handlers-and-applies-variable-updates-on-delivery
  (let [handlers (atom {})
        values (atom {})]
    (with-redefs [osc/start-server (fn [])
                  osc/listen (fn [& _])
                  overtone.osc/zero-conf-on (fn [])
                  osc/handle #(swap! handlers assoc %1 %2)
                  osc/var-out (fn [& _])
                  show/set-variable! #(swap! values assoc %1 %2)]
      (with-out-str (osc/demo))
      (is (= #{"/1/aim-a" "/1/aim-b" "/1/aim-a-y" "/1/aim-b-y"} (set (keys @handlers))))
      (is (empty? @values))
      ((get @handlers "/1/aim-a-y") {:args [0.5]})
      (is (= (* 0.5 (config/venue :ceiling)) (:aim-a-y @values)))
      (osc/to-vars! :strip {:x 1 :y 2})
      (is (= {:strip-x 1 :strip-y 2} (select-keys @values [:strip-x :strip-y])))
      (osc/var-binding :gain "/gain")
      ((get @handlers "/gain") {:args [0.75]})
      (is (= 0.75 (:gain @values)))
      (osc/receiver "/gain" :done)
      (is (= :done ((get @handlers "/gain") {:args [1]}))))))
