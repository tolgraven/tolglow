(ns tolglow.studio.output-options-test
  (:require [clojure.test :refer [deftest is]]
            [tolglow.studio :as studio]
            [afterglow.output.artnet :as artnet]
            [afterglow.output.ola :as ola]
            [tolglow.output :as output]))

(deftest explicit-output-cli-options-and-preview-default
  (is (= {:host "127.0.0.1" :port 8500} (studio/parse-options [])))
  (is (= {:host "127.0.0.1" :port 8500 :ola {:host "127.0.0.1" :port 9010}}
         (studio/parse-options ["--output" "ola"])))
  (is (= {:host "0.0.0.0" :port 8600 :ola {:host "lighting.local" :port 9011}}
         (studio/parse-options ["--host" "0.0.0.0" "--port" "8600" "--ola-host" "lighting.local" "--ola-port" "9011"])))
  (is (= {:host "127.0.0.1" :port 8500 :artnet {:host "10.0.0.42" :port 6454}}
         (studio/parse-options ["--artnet-host" "10.0.0.42"])))
  (doseq [args [["--ola-host" "localhost" "--artnet-host" "10.0.0.42"]
                ["--output" "ola" "--artnet-host" "10.0.0.42"]
                ["--output" "preview" "--ola-port" "9010"]
                ["--output" "artnet"] ["--output" "unknown"]
                ["--ola-port" "0"] ["--ola-port" "65536"] ["--ola-port" "wat"]
                ["--unknown" "value"] ["--port"] ["--port" "8500" "--port" "8501"]]]
    (is (thrown? clojure.lang.ExceptionInfo (studio/parse-options args)))))

(deftest sink-selection-is-exclusive-before-any-output-is-opened
  (let [opened (atom [])]
    (with-redefs [ola/open-ola! #(do (swap! opened conj [:ola %]) :ola-sink)
                  artnet/open-artnet! #(do (swap! opened conj [:artnet %]) :artnet-sink)]
      (is (nil? (output/open-sink! {})))
      (is (empty? @opened))
      (is (thrown? clojure.lang.ExceptionInfo (output/open-sink! {:ola {} :artnet {:host "host"}})))
      (is (empty? @opened))
      (is (= :ola-sink (output/open-sink! {:ola {}})))
      (is (= :artnet-sink (output/open-sink! {:artnet {:host "host"}})))
      (is (= [[:ola {}] [:artnet {:host "host"}]] @opened)))))

(deftest startup-failure-closes-the-newly-opened-sink
  (let [closes (atom 0)]
    (with-redefs [output/open-sink! (fn [_] {:close! #(swap! closes inc)})]
      ;; Invalid worker count is rejected by the real runtime constructor.
      (is (thrown? clojure.lang.ExceptionInfo (studio/start! {:ola {} :workers 0}))))
    (is (= 1 @closes))))

(deftest studio-starts-and-stops-with-an-explicit-ola-sink
  (let [sent (promise) closes (atom 0)
        runtime (studio/start! {:port 0 :workspace-options nil
                                :ola {:open-transport! (fn [_]
                                                         {:write! #(deliver sent %)
                                                          :close! #(swap! closes inc)})}})]
    (try
      (is (= :ola (get-in runtime [:sink :description :type])))
      (is (seq (deref sent 3000 nil)))
      (is (zero? @closes))
      (finally (studio/stop! runtime)))
    (is (= 1 @closes))))
