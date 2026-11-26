(ns tolglow.integrations.live-companion-contract-test
  "The Tolglow Live connection against the production companion: the real
  live-transport HTTP client, node_bridge.js and max_adapter.js with
  live_session.js (run in a vm as in Max's [js]). Only the Live object graph
  is simulated (test/js/ableton-live/live_set_fixture.js). No Live instance,
  Link session or MIDI device is used."
  (:require [clojure.data.json :as json]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]
            [tolglow.integrations.live-connection :as live]
            [tolglow.integrations.live-transport :as transport]
            [tolglow.integrations.phrase-clip :as clip])
  (:import [java.util.concurrent TimeUnit]))

(defn- with-companion [f]
  (let [child (.start (doto (ProcessBuilder. ^java.util.List ["node" "test/js/ableton-live/live_set_fixture.js"])
                        (.redirectError java.lang.ProcessBuilder$Redirect/INHERIT)))]
    (try
      (with-open [output (io/reader (.getInputStream child))]
        (let [line (.readLine output)]
          (f (:port (json/read-str line :key-fn keyword)))))
      (finally
        (.close (.getOutputStream child))
        (when-not (.waitFor child 3 TimeUnit/SECONDS)
          (.destroyForcibly child)
          (.waitFor child 2 TimeUnit/SECONDS))))))

(def registry
  {:effects {:wash {:id :wash :version 1 :params {:level {:type :number :bounds [0.0 1.0]}
                                                  :mode {:type :enum :values [:soft :hard :pulse]}}}
             :aim {:id :aim :version 1 :params {:direction {:type :vector :size 3 :bounds [-1.0 1.0]}}}}
   :cues {:wash {:id :wash :effect-id :wash :version 1} :aim {:id :aim :effect-id :aim :version 1}}})

(def phrase
  {:phrase/version 1 :length-beats 8 :meter {:beats-per-bar 4 :bars-per-phrase 4} :grid-beats 1/4
   :notes [{:id "n0" :cue-id :wash :start 0 :length 31/4 :params {:level 0.5 :mode :soft}}
           {:id "n1" :cue-id :aim :start 0 :length 6 :params {:direction [0.0 0.5 -0.5]}}]
   :lanes [{:cue-id :aim :param :direction :points [{:beat 5/2 :value [1.0 -1.0 0.25] :note "n1"}]}
           {:cue-id :wash :param :level :points [{:beat 3/2 :value 0.75 :note "n0"} {:beat 9/4 :value 0.125 :note "n0"}]}
           {:cue-id :wash :param :mode :points [{:beat 6 :value :pulse :note "n0"}]}]})

(deftest the-companion-sets-up-writes-and-reads-back-a-phrase-over-http
  (with-companion
    (fn [port]
      (let [t (transport/transport {:endpoint (str "http://127.0.0.1:" port "/live") :bridge-id "tolglow-m4l"
                                    :sync-session-id "contract-run" :timeout-ms 3000})
            handle ((:open! t) {})
            handshake ((:request! t) handle "handshake-1" :inspect {})
            conn (live/open (assoc t :open! (constantly handle)) {:session-id "contract"})
            layout (clip/allocate-layout {} registry (clip/phrase-targets phrase))
            desired (live/desired-setup layout {:port "Tolglow"})
            setup (live/setup! conn desired)]
        (is (= :executed (:status handshake)))
        (testing "setup adopts the companion's track and verifies it from the read-back"
          (is (:ready? setup) (pr-str (:problems setup)))
          (is (= [[:ensure-track :executed] [:configure-track :executed]] (:applied setup)))
          (is (= [["Bass" true {:port "Keystep" :channel 1}]
                  ["Tolglow Capture" true {:port "Tolglow" :channel 16}]]
                 (mapv (juxt :name :arm? :input) (get-in setup [:set :tracks]))))
          (is (= :auto (get-in setup [:set :tracks 1 :monitor])))
          (let [again (live/setup! conn desired)]
            (is (and (:ready? again) (empty? (:applied again))) "a rerun writes nothing")))
        (testing "the phrase becomes notes and lane envelopes and reads back exactly"
          (let [captured (live/capture! conn setup {:phrase phrase :layout layout})
                imported (live/import-clips! conn (:clips captured) layout {:meter (:meter phrase)})
                expected (clip/clips->phrase (clip/phrase->clips phrase layout) layout {})]
            (is (:complete? captured) (pr-str (:receipts captured)))
            (is (= :executed (:shown captured)))
            (is (= (clip/comparable expected) (clip/comparable imported)))))
        (testing "refusals arrive as Tolglow receipts"
          (let [r (live/request! conn :write-clip {:track-id (get-in setup [:set :tracks 0 :track-id]) :expected-name "Bass" :clip (get (clip/phrase->clips phrase layout) 1)})]
            (is (= :rejected (:status r)))
            (is (= :controllers-need-companion (get-in r [:error :code]))))
          (is (= :rejected (:status (live/request! conn :read-clip {:clip-id "clip:999999"})))))
        (testing "a cue launched after the clip start keeps its launch values (lane read from beat 0)"
          (let [late (assoc phrase
                            :notes [{:id "n0" :cue-id :aim :start 0 :length 6 :params {:direction [0.0 0.5 -0.5]}}
                                    {:id "n1" :cue-id :wash :start 2 :length 2 :params {:level 0.5 :mode :soft}}]
                            :lanes [{:cue-id :wash :param :level :points [{:beat 3 :value 0.25 :note "n1"}]}])
                captured (live/capture! conn setup {:phrase late :layout layout})
                imported (live/import-clips! conn (:clips captured) layout {:meter (:meter phrase)})]
            (is (:complete? captured))
            (is (= (clip/comparable (clip/clips->phrase (clip/phrase->clips late layout) layout {}))
                   (clip/comparable imported)))
            (is (= {:level (clip/decode-value (first (filter #(= :level (:param %)) (:lanes layout)))
                                              (clip/encode-value (first (filter #(= :level (:param %)) (:lanes layout))) 0.5))
                    :mode :soft}
                   (:params (second (:notes imported)))))
            (is (empty? (:orphans (meta imported))))))
        (testing "an 8-bar phrase dense with lane moves is split under the 64 KiB body limit"
          (let [dense-registry {:effects {:dense {:id :dense :version 1
                                                  :params (into {} (for [i (range 30)] [(keyword (str "p" i)) {:type :number :bounds [0.0 1.0]}]))}}
                                :cues {:dense {:id :dense :effect-id :dense :version 1}}}
                params (mapv #(keyword (str "p" %)) (range 30))
                dense {:phrase/version 1 :length-beats 32 :meter {:beats-per-bar 4 :bars-per-phrase 4}
                       :notes [{:id "n0" :cue-id :dense :start 0 :length 32 :params (zipmap params (repeat 0.0))}]
                       :lanes (vec (for [[k param] (map-indexed vector params)]
                                     {:cue-id :dense :param param
                                      :points (vec (for [i (range 1 256)]
                                                     {:beat (/ i 8) :value (/ (mod (+ k (* i 37)) 101) 100.0) :note "n0"}))}))}
                dense-layout (clip/allocate-layout {} dense-registry (clip/phrase-targets dense))
                before (count (live/operations-sent conn))
                too-big (live/write-phrase! conn setup dense dense-layout {:budget 100000000})
                written (live/write-phrase! conn setup dense dense-layout {})
                sent (drop before (live/operations-sent conn))
                imported (live/import-clips! conn (:clips written) dense-layout {:meter (:meter dense)})]
            (is (= :payload-too-large (get-in (first (:receipts too-big)) [:error :code])) "one oversized body never reaches Max")
            (is (= :rejected (:status (first (:receipts too-big)))))
            (is (:complete? written) (pr-str (map (juxt :status :error) (:receipts written))))
            (is (< 2 (count (filter #{:extend-clip} sent))) "the phrase took several requests")
            (is (= (clip/comparable (clip/clips->phrase (clip/phrase->clips dense dense-layout) dense-layout {}))
                   (clip/comparable imported)))))
        (testing "a stopped companion yields indeterminate receipts, never exceptions"
          ((:close! t) handle)
          (is (= :indeterminate (:status (live/request! conn :fire-clip {:clip-id "clip:1"})))))))))

(deftest nothing-listening-is-a-refusal-not-an-uncertain-write
  (let [port (with-open [socket (java.net.ServerSocket. 0 1 (java.net.InetAddress/getLoopbackAddress))] (.getLocalPort socket))
        t (transport/transport {:endpoint (str "http://127.0.0.1:" port "/live") :bridge-id "tolglow-m4l"
                                :sync-session-id "nobody" :timeout-ms 1000})
        conn (live/open t {:session-id "refused"})
        r (live/request! conn :write-clip {:track-id "track:1" :clip {:length-beats 4 :notes []}})]
    (is (= :rejected (:status r)))
    (is (= :not-connected (get-in r [:error :code])))))
