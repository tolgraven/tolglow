(ns tolglow.composition.macro-draft-test
  (:require [afterglow.control.model :as control]
            [afterglow.control.pages :as pages]
            [afterglow.control.views :as views]
            [clojure.test :refer [deftest is testing]]
            [tolglow.composition :as composition]
            [tolglow.composition.macro-draft :as draft]
            [tolglow.demo :as demo]
            [tolglow.registry :as registry]
            [tolglow.show :as show]))

(defn- command [model request]
  (let [request (merge (select-keys model [:show-id :epoch :time-ms])
                       {:command-id (str (:revision model)) :source :macro-draft-test} request)
        {:keys [ack] next-model :model} (control/apply-command model request)]
    (when-not (= :accepted (:status ack)) (throw (ex-info "Test command rejected" ack)))
    next-model))

(def ^:private effect
  {:id :level :version 1
   :params {:level {:type :number :default 0.5 :bounds [0 1]}
            :enabled? {:type :boolean :default true}}
   :envelope {:release-ms 200} :outputs {:dimmer {:op :param :key :level}}})

(defn- model []
  (-> (control/new-show)
      (command {:op :registry/publish :kind :effects :definition effect})
      (command {:op :cue/put :cue {:id :red :effect-id :level}})
      (command {:op :cue/put :cue {:id "red" :effect-id :level}})
      (command {:op :cue/put :cue {:id :blue :effect-id :level :priority 10}})
      (command {:op :page/put :page {:id :first :cells {[0 0] :red [1 0] "red"}}})
      (command {:op :page/put :page {:id :second :cells {[0 0] :red [0 1] :blue}}})))

(defn- toggle [model draft page cell]
  (draft/toggle-placement model draft (draft/project-placement model draft page cell)))

(defn- save-as [draft]
  {:id :capture :name "Captured colors" :expected-draft-revision (:revision draft)})

(defn- failure-code [f]
  (try (f) nil (catch clojure.lang.ExceptionInfo error (:code (ex-data error)))))

(deftest placements-toggle-and-move-independently-across-pages
  (let [model (model)
        empty-draft (draft/begin model :draft)
        initial-pad (draft/project-placement model empty-draft :first [0 0])
        first-pad (draft/toggle-placement model empty-draft initial-pad)
        both-pads (toggle model first-pad :second [0 0])
        three-pads (toggle model both-pads :first [1 0])
        [a b c] (mapv :id (:slots three-pads))
        reordered (draft/move-slot three-pads (:revision three-pads) c 0)
        removed (toggle model reordered :first [0 0])
        added (toggle model removed :first [0 0])]
    (is (empty? (:slots empty-draft)))
    (is (= :stale-draft (failure-code #(draft/toggle-placement model first-pad initial-pad))))
    (is (= :stale-draft (failure-code #(draft/move-slot reordered (:revision three-pads) c 1))))
    (is (= :invalid-slot-index (failure-code #(draft/move-slot reordered (:revision reordered) c 3))))
    (is (= [:red :red "red"] (mapv :cue-id (:slots three-pads))))
    (is (= [c a b] (mapv :id (:slots reordered))))
    (is (= [c b a] (mapv :id (:slots (draft/move-slot reordered (:revision reordered) a 2)))))
    (is (= [c b] (mapv :id (:slots removed))))
    (is (= b (:slot-id (draft/project-placement model removed :second [0 0]))))
    (is (nil? (:slot-id (draft/project-placement model removed :first [0 0]))))
    (is (not= a (:id (peek (:slots added)))))
    (is (= ["red" :red :red]
           (mapv :cue-id (:slots (draft/prepare-save model reordered (save-as reordered))))))))

(deftest stale-projections-and-changed-dependencies-are-rejected
  (let [model (model)
        empty-draft (draft/begin model :draft)
        pad (draft/project-placement model empty-draft :first [0 0])
        selected (draft/toggle-placement model empty-draft pad)
        moved (command model {:op :page/put :page {:id :first :cells {[0 0] :blue}}})
        republished-cue (command model {:op :cue/put :cue {:id :red :effect-id :level :name "Edited"}})
        republished-effect (command model {:op :registry/publish :kind :effects
                                           :definition (assoc effect :version 2)})
        unrelated (command model {:op :cue/put :cue {:id :blue :effect-id :level :name "Another cue"}})
        restarted (command model {:op :show/restart})]
    (doseq [current [moved republished-cue republished-effect]]
      (is (= :stale-placement (failure-code #(draft/toggle-placement current empty-draft pad)))))
    (doseq [current [republished-cue republished-effect (command model {:op :cue/delete :cue-id :red})]]
      (is (= :stale-draft-cue (failure-code #(draft/prepare-save current selected (save-as selected))))))
    (is (= :stale-show (failure-code #(draft/prepare-save restarted selected (save-as selected)))))
    (is (= :stale-placement
           (failure-code #(draft/toggle-placement restarted (draft/begin restarted :draft) pad))))
    (is (= [:red] (mapv :cue-id (:slots (draft/prepare-save moved selected (save-as selected))))))
    (is (= [:red] (mapv :cue-id (:slots (draft/prepare-save unrelated selected (save-as selected))))))
    (is (= :stale-draft (failure-code #(draft/prepare-save model selected (save-as empty-draft)))))
    (is (= :invalid-macro-name (failure-code #(draft/prepare-save model selected (assoc (save-as selected) :name " ")))))
    (is (thrown? clojure.lang.ExceptionInfo (draft/prepare-save model empty-draft (save-as empty-draft))))
    (is (= :composition-exists
           (failure-code #(draft/prepare-save (composition/save model (draft/prepare-save model selected (save-as selected)))
                                               selected (save-as selected)))))))

(deftest the-generated-recent-page-uses-the-current-pad-placement
  (let [model (command (model) {:op :cue/launch :cue-id :red})
        empty-draft (draft/begin model :draft)
        recent-pad (draft/project-placement model empty-draft pages/recent-page-id [0 7])
        next-model (command model {:op :cue/launch :cue-id "red"})]
    (is (= :stale-placement (failure-code #(draft/toggle-placement next-model empty-draft recent-pad))))
    (is (= ["red"] (mapv :cue-id (:slots (toggle next-model empty-draft pages/recent-page-id [0 7])))))))

(deftest running-capture-freezes-authored-parameters-in-composition-order
  (let [base (model)
        active (-> base
                   (command {:op :cue/launch :cue-id :blue})
                   (command {:op :cue/launch :cue-id :red :launch {:params {:level 0.2 :enabled? false}}})
                   (command {:op :cue/launch :cue-id :red :launch {:params {:level 0.8}}})
                   (command {:op :cue/launch :cue-id "red"})
                   (command {:op :effect/launch :effect-id :level})
                   (command {:op :instance/end :instance-id "0:4"})
                   (command {:op :param/set :instance-id "0:3" :param :level :value 0.9}))
        empty-draft (draft/begin active :draft)
        staged (toggle active empty-draft :first [1 0])
        captured (draft/capture-running active staged)
        later (command active {:op :param/set :instance-id "0:3" :param :level :value 1.0})
        definition (draft/prepare-save later captured (save-as captured))
        saved (composition/save later definition)
        owned (set (keys (:instances later)))
        replay (composition/start saved (:id definition) 0)
        replayed (remove #(contains? owned (:id %)) (views/active-instances replay))]
    (is (registry/data? captured))
    (is (= ["0:2" "0:3" "0:1"] (mapv :instance-id (:slots captured))))
    (is (= [:red :red :blue] (mapv :cue-id (:slots definition)) (mapv :cue-id replayed)))
    (is (= [false true true] (mapv #(get-in % [:params :enabled?]) replayed)))
    (is (= [0.2 0.9 0.5] (mapv #(get-in % [:params :level]) (:slots definition))
           (mapv #(get-in % [:params :level]) replayed)))
    (is (= [0 1 2] (mapv #(get-in % [:source :native-layer :member]) replayed)))
    (is (= ["red"] (mapv :cue-id (:slots staged))))
    (is (= (mapv :id (rest (:slots captured)))
           (mapv :id (:slots (draft/remove-slot captured (:revision captured) (:id (first (:slots captured))))))))
    (is (= later (-> saved (dissoc :compositions) (update :revision dec))))))

(deftest pinned-cues-can-replay-older-effects-and-conflicts-are-atomic
  (let [base (-> (model)
                 (command {:op :cue/put :cue {:id :blue :effect-id :level :effect-version 1}})
                 (command {:op :cue/launch :cue-id :blue})
                 (command {:op :cue/launch :cue-id :red}))
        edited (command base {:op :registry/publish :kind :effects :definition (assoc effect :version 2)})
        staged (toggle base (draft/begin base :draft) :first [1 0])]
    (is (= :stale-instance-cue (failure-code #(draft/capture-running edited staged))))
    (is (= ["red"] (mapv :cue-id (:slots staged))))
    (let [only-pinned (command edited {:op :instance/end :instance-id "0:2" :force? true})
          captured (draft/capture-running only-pinned staged)
          saved (draft/prepare-save only-pinned captured (save-as captured))
          replay (-> only-pinned (composition/save saved) (composition/start (:id saved) 0))
          instances (views/active-instances replay)]
      (is (= [1] (mapv :effect-version (:slots captured))))
      (is (= [:blue] (mapv :cue-id (:slots saved))))
      (is (= [1 1] (mapv :effect-version instances)))
      (is (= [:blue :blue] (mapv :cue-id instances))))))

(defn- packets-with-optional-save [save?]
  (let [sent (atom [])
        runtime (show/create-runtime {:model (demo/model) :workspace-options nil
                                      :sink {:send! (fn [packets frame]
                                                      (swap! sent conj [frame (update-vals packets vec)]))}})]
    (try
      (let [launch (show/submit! runtime {:op :cue/launch :cue-id :ember})]
        (show/tick! runtime 25)
        (is (= :accepted (:status (deref launch 1000 {}))))
        (when save?
          (let [{model :model :as before} @(:state runtime)
                captured (draft/capture-running model (draft/begin model :draft))
                cancelled (draft/cancel captured)
                save-draft (draft/capture-running model (draft/begin model :next-draft))
                prepared (draft/prepare-save model save-draft (save-as save-draft))]
            (is (= :cancelled (:status cancelled)))
            (is (empty? (:slots cancelled)))
            (is (= :closed-draft (failure-code #(draft/prepare-save model cancelled (save-as cancelled)))))
            (is (identical? before @(:state runtime)))
            (is (= 1 (count @sent)))
            (let [save (show/submit! runtime :composition prepared)]
              (show/tick! runtime 50)
              (is (= :accepted (:status (deref save 1000 {}))))
              (let [committed (:model @(:state runtime))]
                (is (= prepared (get-in committed [:compositions :capture])))
                (is (= (keys (:instances model)) (keys (:instances committed))))
                (is (empty? (:composition-runs committed)))))))
        (when-not save? (show/tick! runtime 50))
        (show/tick! runtime 75)
        @sent)
      (finally (show/stop! runtime)))))

(deftest drafting-cancelling-and-saving-preserve-live-packets
  (let [baseline (packets-with-optional-save false)
        saved (packets-with-optional-save true)]
    (is (= 3 (count baseline) (count saved)))
    (is (= baseline saved))))
