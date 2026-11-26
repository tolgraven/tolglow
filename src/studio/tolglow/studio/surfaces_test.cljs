(ns tolglow.studio.surfaces-test
  (:require [cljs.test :refer-macros [deftest is]]
            [re-frame.db :as rf-db]
            [re-frame.interceptor :as interceptor]
            [re-frame.registrar :as registrar]
            [tolglow.studio.surfaces :as surfaces]))

(def owner {:runtime-id "surface-runtime" :show-id :show :epoch 3})

(def projected
  {:settings {:follow-selection? true}
   :surfaces [{:id "xt-main" :attached? true} {:id "xt-ext" :attached? true}]
   :logical [{:id "desk" :link? true :viewport :strips :mode :extend
              :window {:x 16 :y 0} :span {:columns 16 :rows 1} :size {:columns 20 :rows 1} :limit {:x 16 :y 0}
              :members [{:surface-id "xt-main" :viewport :strips :attached? true :window {:x 16 :y 0}
                         :cells [{:key [:group [:manual :g16]] :group-id [:manual :g16] :position {:x 0 :y 0}}
                                 {:key [:cue :c1] :cue-id :c1 :instance-id "3:7" :position {:x 3 :y 0}}]}
                        {:surface-id "xt-ext" :viewport :strips :attached? true :window {:x 24 :y 0} :cells []}]}]})

(def base
  {:session "surface-session" :connection {:connected? true}
   :server-projection {:view (merge owner {:revision 9 :control-surfaces projected
                                           :instances [{:id "3:7" :cue-id :c1}]})}})

(defn- transition [db event]
  (reset! rf-db/app-db db)
  (:effects (interceptor/execute event (remove #(= :do-fx (:id %))
                                               (registrar/get-handler :event (first event))))))

(deftest the-outline-names-the-surface-strip-of-each-group-and-cue-instance
  (is (= {:groups {[:manual :g16] "xt-main 1"} :instances {"3:7" "xt-main 4"}}
         (surfaces/on-surface projected)))
  (is (= "Strips 17–20 of 20" (surfaces/range-label (first (:logical projected)))))
  (is (= "No strips" (surfaces/range-label {:window {:x 0} :span {:columns 8} :size {:columns 0}}))))

(deftest bank-buttons-send-the-shared-owner-command
  (let [effects (transition base [:surfaces/bank "desk" :strips :previous])
        request (second (first (:fx effects)))]
    (is (= "/api/command" (:path request)))
    (is (= (merge owner {:op :control-surface/bank :target "desk" :viewport :strips :move :previous})
           (select-keys (:body request) [:runtime-id :show-id :epoch :op :target :viewport :move])))
    (is (nil? (transition (assoc-in base [:connection :connected?] false) [:surfaces/bank "desk" :strips :next])))))

(deftest following-the-studio-selection-reveals-the-exact-cue
  (is (= [[:dispatch [:surfaces/reveal "desk" :strips [:cue :c1]]]]
         (:fx (transition base [:surfaces/follow [:instance "3:7"]]))))
  (is (= [[:dispatch [:surfaces/reveal "desk" :strips [:group [:manual :g2]]]]]
         (:fx (transition base [:surfaces/follow [:group [:manual :g2]]]))))
  (is (nil? (transition (assoc-in base [:server-projection :view :control-surfaces :settings :follow-selection?] false)
                        [:surfaces/follow [:group [:manual :g2]]]))
      "Follow is off unless the surface setting is on")
  (let [request (second (first (:fx (transition base [:surfaces/reveal "desk" :strips [:cue :c1]]))))]
    (is (= {:op :control-surface/bank :target "desk" :viewport :strips :reveal [:cue :c1]}
           (select-keys (:body request) [:op :target :viewport :reveal])))))

(deftest a-quiet-follow-rejection-is-not-shown-as-an-error
  (let [db (:db (transition base [:surfaces/reveal "desk" :strips [:cue :elsewhere]]))
        id (get-in db [:ui :surfaces :pending])]
    (reset! rf-db/app-db db)
    (is (nil? (get-in (:db (transition db [:surfaces/failed id "That cell is not on this surface"])) [:ui :surfaces :error])))
    (let [db (:db (transition base [:surfaces/unlink "desk"]))
          id (get-in db [:ui :surfaces :pending])]
      (is (= "Refused" (get-in (:db (transition db [:surfaces/failed id "Refused"])) [:ui :surfaces :error]))))))

(deftest the-link-form-sends-the-shared-link-shape
  (is (= {:op :control-surface/link :link-id "a+b" :mode :extend :direction :left
          :members [{:surface-id "a"} {:surface-id "b"}]}
         (surfaces/link-payload "a+b" "a" "b" "left")))
  (is (= {:op :control-surface/link :link-id "a+b" :mode :follow
          :members [{:surface-id "a"} {:surface-id "b" :offset {:x 1 :y 0}}]}
         (surfaces/link-payload "a+b" "a" "b" "follow")))
  (let [request (second (first (:fx (transition base [:surfaces/link "a+b" "a" "b" "mirror"]))))]
    (is (= {:op :control-surface/link :mode :mirror} (select-keys (:body request) [:op :mode]))))
  (is (nil? (transition base [:surfaces/link "a+a" "a" "a" "mirror"]))))
