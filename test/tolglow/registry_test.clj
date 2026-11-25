(ns tolglow.registry-test
  (:require [clojure.test :refer [deftest is testing]]
            [tolglow.registry :as registry]))

(def effect {:id :red :version 1 :params {:level {:type :number :default 0.5 :bounds [0 1]}}
             :outputs {:dimmer {:op :param :key :level}}})

(deftest parameter-validation-preserves-types-and-bounds
  (doseq [[schema good bad]
          [[{:type :number :default 0.5 :bounds [0 1]} 0.9 ##NaN]
           [{:type :integer :default 2 :bounds [1 4]} 3 2.5]
           [{:type :boolean :default false} true 0]
           [{:type :enum :default :a :values [:a :b]} :b :c]
           [{:type :color :default [1 0 0]} [0.1 0.2 1] [1 0 2]]
           [{:type :range :default [0 1] :bounds [0 10]} [2 5] [5 2]]
           [{:type :vector :size 2 :default [0 0]} [1 2] [1 2 3]]]]
    (is (empty? (registry/validate-parameter schema)))
    (is (registry/valid-value? schema good))
    (is (not (registry/valid-value? schema bad))))
  (is (= {:level 0.5} (registry/resolve-params (:params effect) {})))
  (is (= {:level 0.9} (registry/resolve-params (:params effect) {:level 0.9})))
  (is (= [:params :unknown] (:path (first (registry/validate-params (:params effect) {:unknown 3})))))
  (is (thrown? clojure.lang.ExceptionInfo (registry/resolve-params (:params effect) {:level 2}))))

(deftest gradients-publish-and-resolve-with-ordered-typed-stops
  (let [red {:position 0 :color [1 0 0]}
        blue {:position 1 :color [0 0 1]}
        schema {:type :gradient :default [red blue]}
        effect (assoc-in effect [:params :gradient] schema)
        published (registry/publish {} :effects effect)]
    (is (= [red blue] (:gradient (registry/resolve-params
                                   (get-in published [:effects :red :params]) {}))))
    (is (= [blue] (:gradient (registry/resolve-params
                               (get-in published [:effects :red :params]) {:gradient [blue]}))))
    (doseq [invalid [[] [1] [blue red] [red red] [(assoc blue :color [0 -1 1])]]]
      (is (seq (registry/validate-parameter (assoc schema :default invalid)))))
    (is (= [red] (:gradient (registry/resolve-params {:gradient (assoc schema :default [red])} {})))
        "a single stop is a solid color")))

(deftest gradient-diagnostics-name-the-authored-stop
  (let [red {:position 0 :color [1 0 0]}
        blue {:position 1 :color [0 0 1]}
        schema {:type :gradient :default [red blue]}
        paths (fn [default] (mapv :path (registry/validate-parameter (assoc schema :default default)
                                                                     [:params :wash])))]
    (is (= [[:params :wash :default]] (paths [])))
    (is (= [[:params :wash :default]] (paths {:position 0 :color [1 0 0]})))
    (is (= [[:params :wash :default 1 :position]] (paths [blue red])))
    (is (= [[:params :wash :default 1 :position]] (paths [red red])))
    (is (= [[:params :wash :default 1 :position]] (paths [red (assoc blue :position 1.5)])))
    (is (= [[:params :wash :default 0 :color]] (paths [(assoc red :color [1 0]) blue])))
    (is (= [[:params :wash :default 1 :color]] (paths [red (assoc blue :color [0 -1 1])])))
    (is (= [[:params :wash :default 0]] (paths [(assoc red :alpha 1) blue])))
    (is (re-find #"strictly increase"
                 (:message (first (registry/validate-parameter (assoc schema :default [red red]))))))
    (is (= [[:params :gradient 0 :position]]
           (mapv :path (registry/validate-params {:gradient schema}
                                                 {:gradient [(assoc red :position -0.1) blue]})))
        "override values report the same stop path as authored defaults")
    (is (= [[:params :wash :default 0]] (paths [[1]])))
    (is (= [[:params :wash :default 1]] (paths [red [1] blue]))
        "a non-map stop names its index")
    (is (= [[:params :wash :default 1]] (paths [red (assoc red :alpha 1)]))
        "a malformed stop does not also produce an ordering error")
    (is (= [[:params :gradient 1 :position]]
           (mapv :path (registry/validate-params {:gradient schema}
                                                 {:gradient [red (assoc blue :position ##NaN)]}))))
    (is (= [[:params :wash :default 1 :position] [:params :wash]]
           (paths [red (assoc blue :position ##NaN)]))
        "a NaN default names its stop and is also nonportable schema data")
    (is (= [[:params :wash :default 1 :position]] (paths [red (assoc blue :position 4/3)])))
    (is (empty? (paths [red (assoc blue :position 1/2)])) "ratio positions within [0,1] are valid")))

(deftest gradient-schemas-reject-numeric-metadata-and-randomization
  (let [schema {:type :gradient :default [{:position 0 :color [1 0 0]} {:position 1 :color [0 0 1]}]}
        paths #(mapv :path (registry/validate-parameter % [:params :wash]))]
    (is (empty? (paths schema)))
    (is (= [[:params :wash :size]] (paths (assoc schema :size 2))))
    (is (= [[:params :wash :bounds]] (paths (assoc schema :bounds [0 1]))))
    (is (= [[:params :wash :recommended]] (paths (assoc schema :recommended [0 1]))))
    (is (= [[:params :wash :random :enabled?]] (paths (assoc schema :random {:enabled? true}))))
    (is (empty? (paths (assoc schema :random {:enabled? false}))))
    (is (= [[:params :level :size]]
           (mapv :path (registry/validate-parameter {:type :number :default 0 :size 1} [:params :level])))
        "only vectors declare a size")
    (is (thrown? clojure.lang.ExceptionInfo
                 (registry/publish {} :effects (assoc-in effect [:params :wash] (assoc schema :size 2)))))))

(deftest malformed-schemas-have-diagnostics
  (doseq [schema [nil [] {:type :enum :values 5 :default :a}
                  {:type :number :default 0 :bounds [5 1]}
                  {:type :number :default 0 :bounds [0 1] :recommended [-1 2]}
                  {:type :vector :size 0 :default []}
                  {:type :number :default 0 :random {:max-change -1}}]]
    (is (seq (registry/validate-parameter schema))))
  (is (seq (registry/validate-definition :effects (assoc effect :callback identity))))
  (is (seq (registry/validate-definition :effects (assoc effect :outputs {}))))
  (doseq [output [nil 7 "bad" []]]
    (is (seq (registry/validate-definition :effects (assoc effect :outputs [output]))))))

(deftest immutable-publication-keeps-history-and-validates-cue-references
  (let [empty (registry/new-registry)
        r1 (registry/publish empty :effects effect)
        r2 (registry/publish r1 :effects (assoc effect :version 2 :name "renamed"))
        cue {:id :cue :effect-id :red :params {:level 0.8}}
        r3 (registry/publish r2 :cues cue)
        recreated (registry/publish (update r3 :cues dissoc :cue) :cues cue)]
    (is (empty? (:effects empty)))
    (is (= r1 (registry/publish r1 :effects effect)))
    (is (= effect (get-in r2 [:versions [:effects :red 1]])))
    (is (= 2 (get-in recreated [:cues :cue :version])))
    (is (thrown? clojure.lang.ExceptionInfo (registry/publish r2 :effects effect)))
    (is (thrown? clojure.lang.ExceptionInfo (registry/publish r1 :effects (assoc effect :name "same version"))))
    (is (thrown? clojure.lang.ExceptionInfo (registry/publish empty :cues cue)))
    (is (thrown? clojure.lang.ExceptionInfo (registry/publish r1 :cues (assoc cue :params {:level 4}))))))

(deftest saved-cue-parameters-validate-against-the-selected-effect-version
  (let [r (-> (registry/new-registry)
              (registry/publish :effects effect)
              (registry/publish :effects (-> effect (assoc :version 2)
                                             (assoc-in [:params :level :bounds] [0 0.5]))))
        cue {:id :preset :effect-id :red :effect-version 1 :saved-params {:level 0.8}}]
    (is (= {:level 0.8} (get-in (registry/publish r :cues cue) [:cues :preset :saved-params])))
    (doseq [bad [nil [] {:level 2} {:unknown 0.2}]]
      (is (thrown? clojure.lang.ExceptionInfo
                   (registry/publish r :cues (assoc cue :saved-params bad)))))
    (is (thrown? clojure.lang.ExceptionInfo (registry/publish r :cues (dissoc cue :effect-version))))
    (is (thrown? clojure.lang.ExceptionInfo (registry/publish r :cues (assoc cue :effect-version 99))))))

(deftest updating-an-effect-checks-current-cues-that-follow-it
  (let [cue {:id :preset :effect-id :red :saved-params {:level 0.8}}
        r (-> (registry/new-registry)
              (registry/publish :effects effect)
              (registry/publish :cues cue))
        revised (-> effect (assoc :version 2) (assoc-in [:params :level :bounds] [0 0.5]))
        errors (try (registry/publish r :effects revised) nil
                    (catch clojure.lang.ExceptionInfo e (:errors (ex-data e))))
        pinned (registry/publish r :cues (assoc cue :effect-version 1))
        published (registry/publish pinned :effects revised)]
    (is (= [[:cues :preset :saved-params :level]] (mapv :path errors)))
    (is (= revised (get-in published [:effects :red])))
    (is (= effect (registry/cue-effect published (get-in published [:cues :preset]))))))

(deftest page-generation-is-stable-and-does-not-overwrite-layout
  (let [registry (-> (registry/new-registry)
                     (registry/publish :effects effect)
                     (registry/publish :effects (assoc effect :id :blue)))
        page (registry/generated-page registry {:rows 1 :columns 1 :page 1})]
    (is (= :red (get-in page [:cues 0 :effect-id])))
    (is (= [0 0] (get-in page [:placements 0 :position])))
    (is (= 2 (:revision registry)))
    (is (= page (registry/generated-page registry {:rows 1 :columns 1 :page 1})))))

(deftest registry-page-reconciliation-preserves-manual-cells-cues-and-customizations
  (let [registry (-> (registry/new-registry)
                     (registry/publish :effects effect)
                     (registry/publish :effects (assoc effect :id :blue))
                     (registry/publish :cues {:id :my-red :effect-id :red :params {:level 0.2}}))
        manual {:id :main :name "My page" :cells {[1 0] :my-red}}
        draft (registry/reconcile-page registry manual {:columns 2 :rows 1})
        regenerated (registry/reconcile-page (:registry draft) (:page draft))]
    (is (= :my-red (get-in draft [:page :cells [1 0]])))
    (is (= {:level 0.2} (get-in draft [:registry :cues :my-red :params])))
    (is (= "generated::blue" (get-in draft [:page :cells [0 0]])))
    (is (= ["generated::blue"] (mapv :id (:cues draft))))
    (is (= (:page draft) (:page regenerated)))
    (is (= (:registry draft) (:registry regenerated)))
    (is (empty? (:cues regenerated)))
    (is (empty? (:placements regenerated)))
    (is (= {[1 0] :my-red} (:cells manual)))))

(deftest moved-generated-cues-retain-identity-and-overflow-is-explicit
  (let [registry (-> (registry/new-registry)
                     (registry/publish :effects effect)
                     (registry/publish :effects (assoc effect :id :blue)))
        first-draft (registry/reconcile-page registry {:id :page :cells {}} {:columns 2 :rows 1})
        red-id (get-in first-draft [:page :effect-cues :red])
        moved (assoc (:page first-draft) :cells {[0 0] red-id})
        next-draft (registry/reconcile-page (:registry first-draft) moved)
        crowded (registry/reconcile-page registry {:id :tiny :cells {}} {:columns 1 :rows 1})]
    (is (= red-id (get-in next-draft [:page :cells [0 0]])))
    (is (= 2 (count (get-in next-draft [:page :cells]))))
    (is (empty? (:cues next-draft)))
    (is (= [:red] (:overflow crowded)))))

(deftest occupied-preferred-cells-and-generated-id-collisions-never-overwrite
  (let [registry (-> (registry/new-registry)
                     (registry/publish :effects effect)
                     (registry/publish :effects (assoc effect :id :blue))
                     (registry/publish :cues {:id :manual :effect-id :red})
                     (registry/publish :cues {:id "generated::blue" :effect-id :red}))
        manual {:id :page :cells {[0 0] :manual}}
        id-conflict (registry/reconcile-page registry manual {:effect-ids [:blue]})
        cell-conflict (registry/reconcile-page (update registry :cues dissoc "generated::blue") manual
                                                {:effect-ids [:blue] :preferred-cells {:blue [0 0]}})]
    (is (= :cue-id (get-in id-conflict [:collisions 0 :kind])))
    (is (= :cell (get-in cell-conflict [:collisions 0 :kind])))
    (is (= (:cells manual) (get-in id-conflict [:page :cells]) (get-in cell-conflict [:page :cells])))
    (is (= registry (:registry id-conflict)))
    (is (empty? (:cues id-conflict)))
    (is (empty? (:cues cell-conflict)))))

(deftest missing-definitions-and-archived-generated-cues-are-not-silently-replaced
  (let [registry (registry/publish (registry/new-registry) :effects effect)
        draft (registry/reconcile-page registry {:id :page :cells {}})
        without-current (update (:registry draft) :cues dissoc "generated::red")
        missing (registry/reconcile-page without-current (:page draft))
        recreated (registry/reconcile-page without-current {:id :new-page :cells {}})]
    (is (= :missing-cue (get-in missing [:collisions 0 :kind])))
    (is (= 2 (get-in recreated [:registry :cues "generated::red" :version])))
    (is (= 1 (get-in recreated [:registry :versions [:cues "generated::red" 1] :version])))))
