(ns tolglow.studio.effect-group-mixer-test
  (:require [cljs.test :refer-macros [deftest is]]
            [re-frame.db :as rf-db]
            [re-frame.interceptor :as interceptor]
            [re-frame.registrar :as registrar]
            [tolglow.studio.effect-group-mixer :as mixer]))

(def owner {:runtime-id "mixer-runtime" :show-id :show :epoch 6})

(def channel [:manual "stage/accent"])

(def view-path [:server-projection :view])

(def base
  {:session "mixer-session" :connection {:connected? true}
   :server-projection
   {:view (merge owner
                 {:revision 7
                  :settings {:effect-group-channels {:overlays {:categories []}}}
                  :effect-group-channels {:active-overlay nil
                                          :manual [{:id [:manual :stage] :level 1
                                                    :groups [{:id channel :level 0.8}]}]}})}})

(defn- transition [db event]
  (reset! rf-db/app-db db)
  (:effects (interceptor/execute event (remove #(= :do-fx (:id %))
                                               (registrar/get-handler :event (first event))))))

(defn- send-level [value]
  (let [edited (transition base [:effect-group-mixer/edit channel value])]
    (transition (:db edited) [:effect-group-mixer/flush owner channel])))

(deftest projected-membership-preserves-author-order-and-distinguishes-overlap
  (let [instances {"6:1" {:name "Wash" :member-level 0.25}
                   "6:2" {:name "Accent" :member-level 1}}
        group {:instance-ids ["6:1"] :matching-instance-ids ["6:2" "6:1"] :direct-instance-ids ["6:1"]}]
    (is (= [{:name "Wash" :member-level 0.25}]
           (mixer/membership group false instances)))
    (is (= [{:name "Accent" :member-level 1 :controls-level? false}
            {:name "Wash" :member-level 0.25 :controls-level? true}]
           (mixer/membership group true instances)))))

(deftest typed-owner-commands-preserve-channel-identity-and-coalesce-movement
  (let [result (send-level 0.5)
        [kind {:keys [body typed?] :as request}] (first (:fx result))
        id (:command-id body)
        moved (transition (:db result) [:effect-group-mixer/edit channel 0.3])
        accepted (transition (:db moved) (conj (:on-success request) {:status :accepted :show-id :show :epoch 6 :revision 8 :command-id id}))
        next-command (transition (:db accepted) (:dispatch accepted))]
    (is (= :studio/http kind))
    (is (true? typed?))
    (is (= (merge owner {:op :effect-groups/level-set :channel-id channel :value 0.5
                         :source {:adapter :web :session-id "mixer-session"}})
           (dissoc body :command-id)))
    (is (nil? (:dispatch-later moved)))
    (is (= 0.3 (get-in next-command [:fx 0 1 :body :value])))
    (is (not= id (get-in next-command [:fx 0 1 :body :command-id])))))

(deftest release-the-draft-only-after-installing-the-receipt-revision
  (let [result (send-level 0.5)
        request (get-in result [:fx 0 1])
        receipt {:status :accepted :command-id (get-in request [:body :command-id]) :show-id :show :epoch 6 :revision 8}
        accepted (:db (transition (:db result) (conj (:on-success request) receipt)))
        applied (-> accepted
                    (assoc-in (conj view-path :revision) 8)
                    (assoc-in (into view-path [:effect-group-channels :manual 0 :groups 0 :level]) 0.5))
        pending (-> accepted mixer/panel-state :requests (get channel))]
    (is (= {:value 0.5 :revision 8} pending))
    (is (= 0.8 (get-in (mixer/panel-state accepted) [:mixer :manual 0 :groups 0 :level])))
    (is (empty? (:requests (mixer/panel-state applied))))
    (is (= 0.5 (get-in (mixer/panel-state applied) [:mixer :manual 0 :groups 0 :level])))))

(deftest retry-keeps-the-same-owner-command-and-old-epoch-replies-are-ignored
  (let [result (send-level 0)
        request (get-in result [:fx 0 1])
        retry (transition (:db result) (:on-uncertain request))
        failed (transition (:db retry) (conj (:on-failure request) "Group removed"))
        restarted (assoc-in (:db result) (conj view-path :epoch) 7)]
    (is (= (:body request) (get-in retry [:fx 0 1 :body])))
    (is (= {:error "Group removed"} (get-in (mixer/panel-state (:db failed)) [:requests channel])))
    (is (empty? (:requests (mixer/panel-state restarted))))
    (is (= restarted (:db (transition restarted (conj (:on-failure request) "Old response")))))
    (is (nil? (:fx (transition restarted [:effect-group-mixer/flush owner channel]))))))

(deftest overlay-edits-send-the-shared-selection-and-cannot-select-an-unconfigured-kind
  (let [edit (transition base [:effect-group-mixer/edit :overlay :categories])
        result (transition (:db edit) [:effect-group-mixer/flush owner :overlay])]
    (is (= {:op :effect-groups/overlay-set :overlay :categories}
           (select-keys (get-in result [:fx 0 1 :body]) [:op :overlay])))
    (is (nil? (:dispatch-later (transition base [:effect-group-mixer/edit :overlay :fixtures]))))))
