(ns tolglow.rig.reference-test
  "Reference rigs and rig.sig (#858): every reference rig is an ordinary valid
  rig with declared roles that runs through the show owner, and the descriptor
  puts rigs a person would call similar nearer each other."
  (:require [clojure.test :refer [deftest is testing]]
            [tolglow.rig :as rig]
            [tolglow.rig.reference :as reference]
            [tolglow.rig.roles :as roles]
            [tolglow.show :as show]
            [tolglow.showcase.rig :as showcase-rig]
            [tolglow.venues.gig-2023 :as gig]))

(deftest every-reference-rig-is-a-valid-rig-with-declared-roles
  (doseq [id reference/classes
          :let [r (reference/rig id)]]
    (is (= r (rig/validate-rig r)) (name id))
    (is (= {:unknown-ids [] :unassigned []} (roles/report r)) (name id))
    (is (= id (get-in reference/rigs [id :class])))
    (is (every? (set (keys reference/fixture-types)) (keep :fixture-type-id (:fixtures r)))
        "built from fixture types the showcase rig already publishes"))
  (testing "every reference rig shares front and back washes; capabilities add roles"
    (is (= [[:back.wash :front.wash]
            [:back.pixel :back.wash :front.wash]
            [:back.beam :back.wash :front.wash]
            [:back.beam :back.blinder :back.pixel :back.wash :front.wash]]
           (mapv #(vec (sort (keys (roles/groups (reference/rig %))))) reference/classes)))))

(deftest a-reference-model-runs-through-the-show-owner
  (doseq [id reference/classes]
    (let [captured (atom nil)
          rt (show/create-runtime {:model (reference/model id)
                                   :sink {:send! (fn [universes _] (reset! captured universes))}
                                   :workspace-options nil})]
      (try
        (show/tick! rt 0.0)
        (show/tick! rt 25.0)
        (is (nil? (:error (:status @(:state rt)))) (name id))
        (is (= (keyword (str "reference-" (name id))) (get-in @(:state rt) [:model :show-id])))
        (finally (show/stop! rt))))))

(def named-rigs
  (delay (merge {:gig (gig/rig) :showcase (showcase-rig/rig)}
                (into {} (map (juxt identity reference/rig)) reference/classes))))

(deftest rig-sig-is-a-fixed-length-versioned-descriptor-in-unit-range
  (doseq [[id r] @named-rigs
          :let [{:keys [v x]} (reference/signature r)]]
    (is (= 1 v))
    (is (= (count reference/signature-fields) (count x) (count reference/signature-weights)) (name id))
    (is (every? #(<= 0.0 % 1.0) x) (name id)))
  (is (= (reference/signature (gig/rig)) (reference/signature (gig/rig))) "deterministic")
  (is (thrown? clojure.lang.ExceptionInfo (reference/distance {:v 1 :x [0]} {:v 2 :x [0]}))))

(deftest similar-rigs-are-nearer-than-dissimilar-ones
  (let [d (reference/distance-table @named-rigs)
        nearer (fn [a b c] (< (d [a b]) (d [a c])))]
    (testing "classes"
      (is (= :movers-pars (:class (reference/classify (gig/rig)))))
      (is (= :movers-pars-pixels (:class (reference/classify (showcase-rig/rig)))))
      (doseq [id reference/classes]
        (is (= id (:class (reference/classify (reference/rig id)))))))
    (testing "orderings a person would agree with"
      (is (nearer :gig :movers-pars :pars) "the gig rig has movers and pars and no pixels")
      (is (nearer :gig :movers-pars :movers-pars-pixels))
      (is (nearer :showcase :movers-pars-pixels :pars) "the showcase rig has movers, pars and pixels")
      (is (nearer :showcase :movers-pars-pixels :movers-pars))
      (is (nearer :pars :movers-pars :movers-pars-pixels) "adding movers is a smaller step than adding movers and pixels")
      (is (nearer :pars :pars-strip :showcase))
      (is (nearer :movers-pars-pixels :pars-strip :pars) "pixels are shared"))
    (testing "the table is symmetric with a zero diagonal"
      (doseq [[[a b] v] d]
        (is (== v (d [b a])))
        (when (= a b) (is (zero? v)))))))
