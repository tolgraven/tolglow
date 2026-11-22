(ns tolglow.studio.reactions-test
  (:require [cljs.test :refer-macros [deftest is async]]
            [re-frame.core :as rf]
            [re-frame.db :as db]
            [reagent.ratom :as ratom]
            [tolglow.studio.reactions :as reactions]))

(deftest watching-plain-atoms-coeffects-and-recovering-from-invalid-values
  (let [source (atom 2) calls (atom 0)
        observe (fn [q] (ratom/make-reaction #(deref (rf/subscribe q)) :auto-run true))]
    (rf/reg-cofx ::offset (fn [ctx n] (assoc ctx :offset n)))
    (reactions/reg-sub! ::signal
      {:sources {:level (constantly source)} :cofx [[::offset 1]]
       :validate #(and (number? %) (<= 0 %)) :fallback 0}
      (fn [{:keys [sources offset]} _] (swap! calls inc) (+ (:level sources) offset)))
    (let [sub (observe [::signal])]
      (is (= 3 @sub))
      (reset! source 4) (ratom/flush!) (is (= 5 @sub))
      (reset! source -4) (ratom/flush!) (is (= 5 @sub))
      (reset! source 0) (ratom/flush!) (is (= 1 @sub))
      (ratom/dispose! sub)
      (let [n @calls] (reset! source 6) (ratom/flush!) (is (= n @calls))))))

(deftest failed-event-and-invalid-db-leave-the-next-dispatch-runnable
  (let [before @db/app-db id ::events]
    (try
      (reset! db/app-db {id 0})
      (rf/reg-event-db ::fail (fn [_ _] (throw (js/Error. "event probe"))))
      (rf/reg-event-db ::invalid (fn [_ _] 8))
      (rf/reg-event-db ::ok (fn [db _] (update db id inc)))
      (doseq [e [::ok ::fail ::invalid ::ok]] (rf/dispatch-sync [e]))
      (is (= 2 (get @db/app-db id)))
      (finally (reset! db/app-db before)))))

(deftest pending-background-sample-is-cancelled-when-last-consumer-leaves
  (async done
    (let [pending (atom nil) abort (atom nil) received (atom [])
          _ (reactions/reg-sub! ::background
              {:sample (fn [_ signal]
                         (reset! abort signal)
                         (js/Promise. (fn [resolve _] (reset! pending resolve))))}
              (fn [{:keys [sampled]} _] (swap! received conj sampled) sampled))
          render (ratom/make-reaction #(deref (rf/subscribe [::background])) :auto-run true)]
      (is (nil? @render))
      (js/setTimeout
        (fn []
          (ratom/dispose! render)
          (is (.-aborted @abort))
          (@pending :stale)
          (js/setTimeout (fn [] (is (not-any? #{:stale} @received)) (done)) 0)) 0))))
