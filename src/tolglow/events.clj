(ns tolglow.events
  "Semantic events from one committed owner cycle. The same batch is available
  to headless show handlers and to the optional Studio observer."
  (:require [afterglow.runtime.events :as staging]))

(defn clock-context [model]
  (assoc (select-keys model [:show-id :epoch :frame-number :time-ms])
         :beat (get-in model [:transport :beat])))

(defn- clock-events [before after]
  (let [{:keys [beat meter playing?]} (:transport after)
        previous (get-in before [:transport :beat])
        bpb (:beats-per-bar meter 4) bpp (:bars-per-phrase meter 4)]
    (when (and playing? (number? previous) (number? beat) (>= beat previous)
               (= (:epoch before) (:epoch after))
               (= meter (get-in before [:transport :meter])))
      (for [[type length] [[:clock/beat 1] [:clock/bar bpb] [:clock/phrase (* bpb bpp)]]
            :let [a (long (Math/floor (/ (double previous) length)))
                  b (long (Math/floor (/ (double beat) length)))]
            :when (> b a)]
        {:category :clock :emitter {:kind :transport :id (get-in after [:transport :source] :manual)}
         :event {:type type :index b :crossed (- b a)}}))))

(defn emitted [{:keys [before after inputs intents operations]}]
  (let [time (clock-context after)]
    (into []
      (keep (fn [{:keys [event category emitter]}]
              (when (keyword? (:type event))
                {:type (:type event) :emitter emitter :category category
                 :time time :data (if (map? (:instance event))
                                    (let [instance (:instance event)]
                                      (merge (dissoc event :type :instance)
                                             (select-keys instance [:cue-id :effect-id :effect-version :launch-mode])
                                             {:instance-id (:id instance)}))
                                    (dissoc event :type))})))
      (concat inputs
              (keep (fn [{:keys [operation command ack emitter-id]}]
                      (when (or (nil? (:epoch ack)) (= (:epoch after) (:epoch ack)))
                        {:category :command :emitter {:kind :adapter :id (or emitter-id :studio)}
                      :event (assoc (select-keys ack [:status :command-id :instance-id :code])
                                    :type (if (= :rejected (:status ack)) :command/rejected :command/accepted)
                                    :operation (or command operation))})) operations)
              (map (fn [value]
                     (let [intent (cond
                                    (map? value) (assoc value :type (or (:type value) (:op value) :effect/intent))
                                    (keyword? value) {:type value}
                                    :else {:type :effect/intent :value value})
                           id (:instance-id intent)
                           instance (or (get-in after [:instances id]) (get-in before [:instances id]))]
                       {:category :effect
                        :emitter {:kind :effect :id (or (:effect-id intent) (:effect-id instance) id :engine)}
                        :event (assoc intent :cause-adapter (get-in instance [:source :adapter]))})) intents)
              (clock-events before after)))))

(defn commit [admit before next-state operations]
  (let [candidate (:model next-state)
        model (staging/finish candidate)
        events (emitted {:before before :after model :inputs (staging/pending candidate)
                         :operations operations :intents (get-in next-state [:status :last-intents])})
        state (assoc next-state :model model)]
    (if (and admit (seq events))
      (try
        (let [admitted (admit model events)]
          {:state (assoc state :model (:model admitted))
           :events events :deliveries (:deliveries admitted)})
        (catch Exception failure
          {:state (assoc-in state [:status :event-dispatch-error] {:message (.getMessage failure)})
           :events events}))
      {:state state :events events})))
