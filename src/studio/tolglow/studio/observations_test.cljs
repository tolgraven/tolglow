(ns tolglow.studio.observations-test
  (:require [cljs.test :refer-macros [async deftest is]]
            [tolglow.studio.observations :as obs]))

(deftest only-visible-live-grants-can-publish-a-completion
  (async done
    (let [pending (atom []) sent (atom []) clock (atom 50) visible (atom true)
          control (assoc (obs/controller #(deref clock) #(swap! sent conj [%1 %2])) :visible? #(deref visible))
          request {:stream "socket" :query [:studio/observation-demand :rig]
                   :sources {:sample {:sampler ::hold :interval-ms 25 :token "grant" :show {:show-id :show/club :epoch 0}}}}
          frame! #(obs/present! control nil "show/club" 0 42)
          turn! #(js/Promise. (fn [resolve _] (js/setTimeout resolve 0)))]
      (obs/register-sampler! ::hold (fn [_] (js/Promise. (fn [resolve _] (swap! pending conj resolve)))))
      (frame!)
      (is (empty? @pending))
      (obs/configure! control request)
      (obs/present! control nil "previous" 0 101)
      (obs/present! control nil "show/club" 1 0)
      (is (empty? @pending) "A stale image never takes a read under a new show grant")
      (frame!)
      (swap! clock + 50)
      (frame!)
      (is (= 1 (count @pending)) "A slow read cannot start a second read")
      (obs/configure! control nil)
      (obs/configure! control request)
      (frame!)
      (is (= 2 (count @pending)))
      ((first @pending) [1 2 3])
      ((second @pending) [4 5 6])
      (-> (turn!)
          (.then (fn []
                   (is (= [[4 5 6]] (mapv (comp :value second) @sent)) "Only the remounted slot can complete")
                   (is (= 42 (:frame-number (second (first @sent)))))
                   (frame!)
                   (is (= 2 (count @pending)) "The source observes its own minimum interval")
                   (swap! clock + 50)
                   (frame!)
                   ((last @pending) [4 5 6])
                   (turn!)))
          (.then (fn []
                   (is (= 1 (count @sent)) "Unchanged samples generate no packet")
                   (swap! clock + 50)
                   (frame!)
                   (reset! visible false)
                   ((last @pending) [7 8 9])
                   (turn!)))
          (.then (fn []
                   (frame!)
                   (is (= 4 (count @pending)))
                   (is (= 1 (count @sent)) "A read finishing after hide cannot publish")
                   (obs/configure! control nil)
                   (reset! visible true)
                   (frame!)
                   (is (= 4 (count @pending)))))
          (.catch (fn [e] (is false (str e))))
          (.finally (fn [] (swap! obs/samplers dissoc ::hold) (done)))))))

(deftest replacing-and-withdrawing-an-active-reader-aborts-its-work
  (async done
    (let [sent (atom []) held (atom []) control (obs/controller (constantly 50) #(swap! sent conj [%1 %2]))
          query [:studio/observation-demand :rig]
          request {:stream "s" :query query
                   :sources {:coord {:sampler ::position :interval-ms 25 :token "t"
                                     :show {:show-id :show/club :epoch 0}}}}
          frame! #(obs/present! control nil "show/club" 0 51)
          turns #(js/Promise. (fn [resolve _] (js/setTimeout resolve 0)))
          reader (fn [_ signal]
                   (js/Promise. (fn [resolve _] (swap! held conj {:signal signal :finish resolve}))))]
      (obs/register-sampler! ::position {:abortable? true} reader)
      (obs/configure! control request)
      (frame!)
      (is (false? (.-aborted (:signal (first @held)))))
      (obs/register-sampler! ::position {:abortable? true} reader)
      (is (false? (.-aborted (:signal (first @held)))) "Same implementation keeps its in-flight work")
      (obs/register-sampler! ::position (fn [_] [4 5 6]))
      (is (true? (.-aborted (:signal (first @held)))) "A new implementation aborts before the next frame")
      (frame!)
      (-> (turns)
          (.then (fn []
                   (is (= [[4 5 6]] (mapv (comp :value second) @sent)))
                   ((:finish (first @held)) [1 2 3])
                   (is (= 2 (:sequence (second (first @sent)))) "Hot replacement preserves sequence order")
                   (obs/register-sampler! ::position {:abortable? true} reader)
                   (frame!)
                   (is (= 2 (count @held)))
                   (obs/configure! control nil)
                   (is (true? (.-aborted (:signal (second @held)))))
                   ((:finish (second @held)) [7 8 9])
                   (turns)))
          (.then (fn []
                   (is (= 1 (count @sent)))
                   (is (= 2 (count @held)))))
          (.catch (fn [error] (is false (str error))))
          (.finally (fn []
                      (obs/configure! control nil)
                      (swap! obs/samplers dissoc ::position)
                      (done)))))))
