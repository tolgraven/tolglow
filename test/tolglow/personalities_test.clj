(ns tolglow.personalities-test
  (:require [afterglow.spatial.geometry :as geometry]
            [clojure.edn :as edn]
            [clojure.test :refer [deftest is testing]]
            [tolglow.aim-paths :as aim-paths]
            [tolglow.demo :as demo]
            [tolglow.persistence :as persistence]
            [tolglow.personalities :as personalities]
            [tolglow.personalities.forms :as forms]
            [tolglow.registry :as registry]
            [tolglow.signals :as signals]
            [tolglow.transit :as transit]))

(def custom
  {:id :studio/neon-alley :version 1 :schema 1 :name "Neon alley"
   :description "Hand-authored test personality."
   :tags [:urban]
   :palette [[0.0 1.0 0.6]
             {:gradient [{:position 0.0 :color [1.0 0.0 0.4]} {:position 1.0 :color [0.2 0.0 1.0]}] :angle-deg 135.0}
             {:split [[1.0 1.0 1.0] [0.0 0.0 0.0]] :angle-deg 90.0}]
   :concurrency {:fixture-fraction 0.25 :max-fixtures 6 :max-groups 2 :fixture-mix {:strip 2.0 "house-par" 1.0}}
   :geometry {:forms {:bar 3.0 :zigzag 1.0}}
   :motion {:speed 0.9 :smoothness 0.2 :jitter 0.3 :symmetry 0.6}
   :extensions {:vendor/note "kept"}})

(defn- by-id [id] (some #(when (= id (:id %)) %) personalities/builtins))

(defn- operate [model operation] (:model (personalities/apply-operation model operation)))

(defn- paths [personality] (set (map :path (personalities/errors personality))))

(deftest builtins-are-valid-and-cover-the-requested-feels
  (is (<= 6 (count personalities/builtins)))
  (is (apply distinct? (map :id personalities/builtins)))
  (doseq [personality personalities/builtins]
    (is (= [] (personalities/errors personality)) (str (:id personality))))
  (is (every? (set (map :name personalities/builtins))
              ["Squares and rectangles" "Apple-hardware-level corner obsession" "Spiralling in general"
               "Zig to the zag but shaky" "So random lol WITH PINK"]))
  (testing "each example's vocabulary leads with its own shape"
    (is (= [:square :rectangle :bar] (mapv :form (personalities/vocabulary (by-id :squares) :sculpt))))
    (is (= :squircle (:form (apply max-key :weight (personalities/vocabulary (by-id :squircle) :sculpt)))))
    (is (= :spiral (:form (apply max-key :weight (personalities/vocabulary (by-id :spirals) :aim-path)))))
    (is (= :zigzag (:form (apply max-key :weight (personalities/vocabulary (by-id :zigzag-shaky) :waveform)))))
    (is (= (set forms/order) (set (keys (get-in (by-id :random-pink) [:geometry :forms])))))
    (is (< 0.5 (get-in (by-id :zigzag-shaky) [:motion :jitter])))
    (is (some (fn [entry] (some #(> (first %) 0.9) (if (vector? entry) [entry] (or (:split entry) (map :color (:gradient entry))))))
              (:palette (by-id :random-pink)))))
  (testing "one built-in is calm and sparse"
    (let [calm (by-id :still-water)]
      (is (some #{:calm} (:tags calm)))
      (is (>= 0.2 (get-in calm [:motion :speed])))
      (is (zero? (get-in calm [:motion :jitter])))
      (is (= 1 (get-in calm [:concurrency :max-groups]))))))

(deftest validation-names-the-field-that-is-wrong
  (is (= [] (personalities/errors custom)))
  (is (= #{[]} (paths (assoc custom :mood :loud))))
  (is (= #{[]} (paths (assoc custom :on-select (fn [] :closure)))) "a closure is not portable data")
  (is (= #{[:id]} (paths (assoc custom :id ""))))
  (is (= #{[:version]} (paths (assoc custom :version 0))))
  (is (= #{[:schema]} (paths (assoc custom :schema 2))))
  (is (= #{[:name]} (paths (assoc custom :name "  "))))
  (is (= #{[:palette]} (paths (assoc custom :palette []))))
  (is (= #{[:palette]} (paths (assoc custom :palette (vec (repeat 9 [1.0 1.0 1.0]))))))
  (is (= #{[:palette 0]} (paths (assoc-in custom [:palette 0] [1.2 0.0 0.0]))))
  (is (= #{[:palette 1 :gradient]} (paths (assoc-in custom [:palette 1 :gradient 1 :position] 0.0))))
  (is (= #{[:palette 1 :angle-deg]} (paths (assoc-in custom [:palette 1 :angle-deg] 360.0))))
  (is (= #{[:palette 2 :split]} (paths (assoc-in custom [:palette 2 :split] [[1.0 1.0 1.0]]))))
  (is (= #{[:palette 2]} (paths (assoc-in custom [:palette 2 :gradient] (get-in custom [:palette 1 :gradient]))))
      "an entry is one kind of paint")
  (is (= #{[:concurrency :fixture-fraction]} (paths (assoc-in custom [:concurrency :fixture-fraction] 0.0))))
  (is (= #{[:concurrency :max-groups]} (paths (assoc-in custom [:concurrency :max-groups] 65))))
  (is (= #{[:concurrency :max-fixtures]} (paths (assoc-in custom [:concurrency :max-fixtures] 2.5))))
  (is (= #{[:concurrency :fixture-mix :strip]} (paths (assoc-in custom [:concurrency :fixture-mix :strip] -1.0))))
  (is (= #{[:geometry :forms :hexagon]} (paths (assoc-in custom [:geometry :forms :hexagon] 1.0))))
  (is (= #{[:geometry :forms]} (paths (assoc-in custom [:geometry :forms] {:bar 0.0}))))
  (is (= #{[:geometry]} (paths (assoc custom :geometry {:forms {:bar 1.0} :rule :always-square}))))
  (is (= #{[:motion :jitter]} (paths (assoc-in custom [:motion :jitter] 1.5))))
  (is (= #{[:motion]} (paths (update custom :motion dissoc :symmetry))))
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"Invalid personality"
                        (personalities/validate! (assoc custom :version 0)))))

(deftest every-form-names-inputs-the-engine-accepts
  (let [registry (signals/new-registry)
        mover-model {:rig {:fixtures [{:id :mover :channels {:pan [1 2] :tilt [3 4]}}]}
                     :registry {:effects {} :versions {}}}]
    (is (= (set forms/order) (set (keys forms/catalog))))
    (doseq [form forms/order]
      (testing (str form)
        (is (some #(forms/input form %) forms/families) "a form stands for at least one input")
        (when-let [volume (forms/input form :volume)]
          (let [spec (forms/volume-spec volume [0.0 1.5 0.0] 2.0)]
            (is (= spec (geometry/validate! spec)))
            (is (geometry/inside? spec [0.95 1.5 0.0]) "the longest side reaches its extent")
            (is (not (geometry/inside? spec [1.1 1.5 0.0])))))
        (when-let [sculpt (forms/input form :sculpt)]
          (let [values (forms/sculpt-values sculpt 2.0)
                schemas {:shape {:type :enum :values [:sphere :rounded-box]}
                         :size {:type :vector :size 3 :bounds [0.000001 10000]}
                         :rounding {:type :number :bounds [0 1]}
                         :radius {:type :number :bounds [0 10000]}}]
            (is (every? (fn [[param value]] (registry/valid-value? (get schemas param) value)) values))))
        (when-let [figure (forms/input form :aim-path)]
          (let [points (forms/aim-points figure {:center [0.0 0.0 3.0] :radius 1.5})
                effect (aim-paths/definition mover-model {:id :figure :targets [:mover] :points points
                                                          :closed? (:closed? figure) :duration {:bars 1}})]
            (is (<= 2 (count points) aim-paths/max-points))
            (is (every? (fn [[x y z]] (and (<= -1.5 x 1.5) (zero? y) (<= 1.5 z 4.5))) points))
            (is (empty? (registry/validate-definition :effects effect)))))
        (when-let [{:keys [id params]} (forms/input form :waveform)]
          (is (seq (:samples (signals/preview registry id {:params params :samples 9})))))))))

(deftest drawn-waveform-figures-follow-the-engine-definitions
  (let [zigzag (get-in forms/catalog [:zigzag :figure :points])
        sawtooth (get-in forms/catalog [:sawtooth :figure :points])
        near? (fn [a b] (< (abs (- a b)) 1.0e-6))]
    (is (= 7 (count zigzag)) "three triangle cycles keep only their corners")
    (is (= [-0.55 0.55 -0.55 0.55 -0.55 0.55 -0.55] (mapv second zigzag)))
    (is (= 7 (count sawtooth)))
    (is (every? true? (map (fn [[[x1 y1] [x2 y2]]] (and (near? x1 x2) (near? 1.0 (- y1 y2))))
                           (partition 2 (rest sawtooth))))
        "each declared saw jump is drawn as a vertical edge")
    (is (= (forms/waveform-figure :pulse {:duty 0.25} 1 0.5)
           (forms/waveform-figure :pulse {:duty 0.25} 1 0.5)))
    (is (not= (forms/waveform-figure :pulse {:duty 0.25} 1 0.5)
              (forms/waveform-figure :pulse {:duty 0.75} 1 0.5)))))

(deftest seeded-choice-is-stable-and-follows-the-weights
  (let [squares (by-id :squares)
        draws (frequencies (map #(:form (personalities/choose-form squares :sculpt [% 0 "cue"])) (range 2000)))]
    (is (= (personalities/choose-form squares :sculpt [7 0 "cue"])
           (personalities/choose-form squares :sculpt [7 0 "cue"])))
    (is (= #{:square :rectangle :bar} (set (keys draws))))
    (is (< 0.40 (/ (:square draws) 2000.0) 0.51) "5 of 11")
    (is (< 0.31 (/ (:rectangle draws) 2000.0) 0.42) "4 of 11")
    (is (< 0.13 (/ (:bar draws) 2000.0) 0.24) "2 of 11")
    (is (= (forms/input :square :sculpt) (:input (personalities/choose-form (assoc-in squares [:geometry :forms] {:square 1.0})
                                                                           :sculpt [1 2 "cue"]))))
    (testing "a weighted form the family cannot take is skipped, never forced"
      (is (= #{:circle} (set (map #(:form (personalities/choose-form (by-id :spirals) :sculpt [% 0 "cue"])) (range 50)))))
      (is (nil? (personalities/choose-form (assoc-in squares [:geometry :forms] {:zigzag 1.0}) :sculpt [1 0 "cue"])))
      (is (= [] (personalities/vocabulary (assoc-in squares [:geometry :forms] {:spiral 1.0}) :volume))))
    (is (thrown? clojure.lang.ExceptionInfo (personalities/choose-form squares :sculpt [{:unordered :map}])))))

(deftest blending-mixes-every-part-of-the-character
  (let [squircle (by-id :squircle) spirals (by-id :spirals)
        mixed (personalities/blend [{:personality squircle :weight 3} {:personality spirals :weight 1}])]
    (is (= [] (personalities/errors mixed)))
    (is (= personalities/blend-id (:id mixed)))
    (is (= [{:id :squircle :version 1 :weight 0.75} {:id :spirals :version 1 :weight 0.25}] (:blend mixed)))
    (is (= "75% Apple-hardware-level corner obsession + 25% Spiralling in general" (:name mixed)))
    (is (= 0.375 (get-in mixed [:motion :speed])))
    (is (= 0.8375 (get-in mixed [:motion :symmetry])))
    (is (= 0.4125 (get-in mixed [:concurrency :fixture-fraction])))
    (is (= 2 (get-in mixed [:concurrency :max-groups])))
    (is (not (contains? (:concurrency mixed) :max-fixtures)))
    (is (= {:wash 0.6125 :pixel 0.1875 :moving-head 0.2} (get-in mixed [:concurrency :fixture-mix])))
    (is (= {:squircle 0.5 :pill 0.1875 :circle 0.107955 :spiral 0.159091 :ring 0.045455}
           (get-in mixed [:geometry :forms])))
    (is (= [(get-in squircle [:palette 0]) (get-in squircle [:palette 1]) (get-in spirals [:palette 0])]
           (:palette mixed))
        "two of three palette turns go to the heavier part")
    (is (= [:smooth :precise :flowing :hypnotic] (:tags mixed)))
    (is (= squircle (personalities/blend [{:personality squircle :weight 0.2}])))
    (is (= mixed (personalities/blend [{:personality squircle :weight 0.75} {:personality spirals :weight 0.25}])))
    (is (thrown? clojure.lang.ExceptionInfo
                 (personalities/blend [{:personality squircle :weight 1} {:personality squircle :weight 1}])))
    (is (thrown? clojure.lang.ExceptionInfo
                 (personalities/blend [{:personality squircle :weight 1} {:personality spirals :weight 0}])))))

(deftest fixture-limit-reads-the-concurrency-preference
  (is (= 4 (personalities/fixture-limit (by-id :still-water) 100)) "capped by :max-fixtures")
  (is (= 2 (personalities/fixture-limit (by-id :still-water) 10)))
  (is (= 1 (personalities/fixture-limit (by-id :still-water) 1)))
  (is (= 0 (personalities/fixture-limit (by-id :still-water) 0)))
  (is (= 12 (personalities/fixture-limit (by-id :squares) 24))))

(deftest operations-select-blend-author-and-clear
  (let [model (demo/model)
        selected (personalities/apply-operation model {:op :select :id :squircle})
        seeded (operate (:model selected) {:op "select" :id "spirals" :seed 9})
        blended (operate seeded {:op :blend :mix [{:id :squircle :weight 1} {:id :spirals :weight 1}]})]
    (is (= {:status :accepted :revision (inc (:revision model))
            :active {:mix [{:id :squircle :version 1 :weight 1.0}] :seed 0}} (:ack selected)))
    (is (= {:personality (by-id :squircle) :mix [{:id :squircle :version 1 :weight 1.0}] :seed 0}
           (personalities/active (:model selected))))
    (is (identical? (:model selected) (operate (:model selected) {:op :select :id :squircle}))
        "a repeated selection changes nothing and keeps the revision")
    (is (= [(by-id :spirals) 9] ((juxt :personality :seed) (personalities/active seeded))))
    (is (= 9 (:seed (personalities/active blended))) "a selection keeps the seed until one is given")
    (is (= [{:id :squircle :version 1 :weight 0.5} {:id :spirals :version 1 :weight 0.5}]
           (:mix (personalities/active blended))))
    (is (= personalities/blend-id (:id (:personality (personalities/active blended)))))
    (let [cleared (operate blended {:op :clear})]
      (is (nil? (personalities/active cleared)))
      (is (not (contains? (:extensions cleared) personalities/state-key)) "an empty state leaves no trace")
      (is (identical? cleared (operate cleared {:op :clear}))))
    (testing "authoring"
      (let [authored (operate model {:op :put :personality custom})
            revised (assoc custom :version 2 :name "Neon alley, later")]
        (is (= custom (get (personalities/library authored) :studio/neon-alley)))
        (is (identical? authored (operate authored {:op :put :personality custom})))
        (is (thrown-with-msg? clojure.lang.ExceptionInfo #"higher version"
                              (operate authored {:op :put :personality (assoc custom :name "Same version")})))
        (is (= revised (get (personalities/library (operate authored {:op :put :personality revised})) :studio/neon-alley)))
        (is (= custom (:personality (personalities/active (operate authored {:op :select :id "studio/neon-alley"}))))
            "a textual wire ID addresses the keyword definition")
        (is (= (count personalities/builtins) (count (personalities/library (operate authored {:op :remove :id :studio/neon-alley})))))
        (let [running (operate authored {:op :select :id :studio/neon-alley})
              edited (operate running {:op :put :personality revised})]
          (is (= custom (:personality (personalities/active edited))) "the running selection keeps its captured revision")
          (is (false? (get-in (personalities/projection edited) [:active :current?])))
          (is (true? (get-in (personalities/projection running) [:active :current?]))))))
    (testing "a show definition can supersede a built-in, and removing it restores the built-in"
      (let [louder (assoc (by-id :squares) :version 2 :name "Squares, louder")
            overridden (operate model {:op :put :personality louder})]
        (is (= louder (get (personalities/library overridden) :squares)))
        (is (= (by-id :squares) (get (personalities/library (operate overridden {:op :remove :id :squares})) :squares)))))
    (testing "rejections leave the model alone"
      (doseq [bad [{:op :select :id :missing} {:op :select :id :squircle :seed 0.5} {:op :blend :mix []}
                   {:op :blend :mix [{:id :squircle :weight -1}]} {:op :put :personality (assoc custom :schema 9)}
                   {:op :put :personality (assoc custom :id personalities/blend-id)}
                   {:op :remove :id :squircle} {:op :remove :id :missing} {:op :dance}]]
        (is (thrown? clojure.lang.ExceptionInfo (personalities/apply-operation model bad)) (pr-str bad))))))

(deftest current-means-the-library-still-gives-the-captured-feel
  (let [base (operate (operate (demo/model) {:op :put :personality custom}) {:op :select :id :studio/neon-alley})
        blended (operate base {:op :blend :mix [{:id :studio/neon-alley :weight 2} {:id :squares :weight 1}
                                                {:id :spirals :weight 1}]})
        other (assoc custom :name "Someone else's alley" :geometry {:forms {:circle 1.0}})
        replaced (fn [model] (operate (operate model {:op :remove :id :studio/neon-alley}) {:op :put :personality other}))
        current? (fn [model] (get-in (personalities/projection model) [:active :current?]))]
    (is (true? (current? base)))
    (is (true? (current? blended)))
    (testing "a definition removed and put back with other content under the same version"
      (is (= 1 (:version (get (personalities/library (replaced base)) :studio/neon-alley))))
      (is (false? (current? (replaced base))))
      (is (false? (current? (replaced blended)))))
    (testing "putting back the same content is still current"
      (is (true? (current? (operate (operate base {:op :remove :id :studio/neon-alley}) {:op :put :personality custom})))))
    (testing "a saved and reloaded blend is still current"
      (is (true? (current? (persistence/thaw (demo/model) (persistence/read-string (persistence/write-string blended)))))))))

(deftest saved-state-is-validated
  (let [model (operate (operate (demo/model) {:op :put :personality custom})
                       {:op :blend :mix [{:id :studio/neon-alley :weight 2} {:id :still-water :weight 1}]})
        path [:extensions personalities/state-key]]
    (is (= model (personalities/validate-state! model)))
    (is (= (demo/model) (personalities/validate-state! (demo/model))))
    (doseq [broken [(assoc-in model (conj path :version) 2)
                    (assoc-in model (conj path :surprise) true)
                    (assoc-in model (conj path :definitions :other) custom)
                    (assoc-in model (conj path :definitions :studio/neon-alley :motion :speed) 4)
                    (assoc-in model (conj path :active :seed) nil)
                    (assoc-in model (conj path :active :mix 0 :weight) 0.9)
                    (assoc-in model (conj path :active :personality :palette) [])
                    (update-in model (conj path :active) dissoc :personality)]]
      (is (thrown-with-msg? clojure.lang.ExceptionInfo #"Invalid saved personalities"
                            (personalities/validate-state! broken)))
      (is (thrown? clojure.lang.ExceptionInfo (persistence/validate-model broken))))))

(deftest personalities-round-trip-through-transit-edn-and-show-files
  (doseq [personality (conj personalities/builtins custom)]
    (testing (str (:id personality))
      (is (= personality (transit/decode (transit/encode personality))))
      (is (= personality (edn/read-string (pr-str personality))))
      (let [model (operate (operate (demo/model) {:op :put :personality (assoc personality :version 5)})
                           {:op :select :id (:id personality) :seed 41})
            source (persistence/write-string model)
            restored (persistence/thaw (demo/model) (persistence/read-string source))]
        (is (= (personalities/state model) (personalities/state restored)))
        (is (= (assoc personality :version 5) (:personality (personalities/active restored))))
        (is (= (personalities/projection model) (personalities/projection restored))
            "the reloaded show draws the same icons"))))
  (testing "a blend and a previously saved EDN document"
    (let [model (operate (operate (demo/model) {:op :put :personality custom})
                         {:op :blend :mix [{:id :studio/neon-alley :weight 1} {:id :random-pink :weight 3}] :seed -3})
          frozen (persistence/freeze model)
          from-edn (persistence/thaw (demo/model) (persistence/read-string (pr-str frozen)))]
      (is (= (personalities/state model) (personalities/state (:model frozen))))
      (is (= (personalities/state model) (personalities/state from-edn)))
      (is (= -3 (:seed (personalities/active from-edn)))))))

(deftest sharing-is-an-explicit-selection
  (let [model (operate (operate (assoc-in (demo/model) [:integrations :agents] {:private "chat history stays local"})
                                {:op :put :personality custom})
                       {:op :select :id :spirals :seed 5})
        package (personalities/export-package model [:studio/neon-alley :squircle])]
    (is (= {:type :tolglow/personalities :version 1 :schema 1
            :personalities [(dissoc custom :extensions) (by-id :squircle)]
            :provenance [{:id :studio/neon-alley :version 1 :origin :show} {:id :squircle :version 1 :origin :builtin}]}
           package))
    (is (= {:vendor/note "kept"} (:extensions custom)))
    (is (= [custom] (:personalities (personalities/export-package model [:studio/neon-alley] {:extensions? true})))
        "private extensions travel only when asked for")
    (is (= package (transit/decode (transit/encode package))))
    (is (not (re-find #"spirals|chat history|seed|vendor" (pr-str package)))
        "the active selection and other show data are not part of a package")
    (is (thrown? clojure.lang.ExceptionInfo (personalities/export-package model [])))
    (is (thrown? clojure.lang.ExceptionInfo (personalities/export-package model [:missing])))
    (testing "import adds the chosen definitions to another show in one step"
      (let [other (demo/model)
            imported (operate other {:op :import :package package})]
        (is (= (dissoc custom :extensions) (get (personalities/library imported) :studio/neon-alley)))
        (is (nil? (personalities/active imported)))
        (is (= [:studio/neon-alley] (keys (:definitions (personalities/state imported))))
            "an unchanged built-in is not copied into the show")
        (is (thrown? clojure.lang.ExceptionInfo
                     (operate other {:op :import :package (assoc-in package [:personalities 1 :motion :speed] 7)})))
        (is (thrown? clojure.lang.ExceptionInfo (operate other {:op :import :package (assoc package :version 2)})))))))

(deftest projection-lists-the-library-and-the-active-feel
  (let [model (operate (operate (demo/model) {:op :put :personality custom}) {:op :select :id :squares :seed 3})
        {:keys [version library active] :as view} (personalities/projection model)]
    (is (= 1 version))
    (is (= (conj (mapv :id personalities/builtins) :studio/neon-alley) (mapv :id library)))
    (is (= (conj (vec (repeat (count personalities/builtins) true)) false) (mapv :builtin? library)))
    (is (every? #(re-find #"^<svg " (:icon %)) library))
    (is (= {:id :squares :name "Squares and rectangles" :version 1 :seed 3 :current? true
            :mix [{:id :squares :version 1 :weight 1.0}]}
           (select-keys active [:id :name :version :seed :current? :mix])))
    (is (= (:palette (by-id :squares)) (:palette active)))
    (is (= [{:form :square :weight 0.454545} {:form :rectangle :weight 0.363636} {:form :bar :weight 0.181818}]
           (:forms active)))
    (is (= (:icon (first library)) (:icon active)))
    (is (nil? (:active (personalities/projection (demo/model)))))
    (is (= (count personalities/builtins) (count (:library (personalities/projection (demo/model))))))
    (testing "a text reader gets the same data without pictures"
      (let [plain (personalities/without-icons view)]
        (is (not-any? :icon (:library plain)))
        (is (not (contains? (:active plain) :icon)))
        (is (= (update view :library (partial mapv #(dissoc % :icon))) (assoc plain :active (:active view))))))
    (is (re-find #"^data:image/svg\+xml;base64," (:icon (personalities/push-view model))))
    (is (nil? (personalities/push-view (demo/model))))))
