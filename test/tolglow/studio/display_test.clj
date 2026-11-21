(ns tolglow.studio.display-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.java.io :as io]
            [tolglow.studio.display :as display]
            [afterglow.control.devices.transports :as transports])
  (:import [java.awt.image BufferedImage]
           [java.io ByteArrayOutputStream File]
           [java.util Base64]
           [java.util.concurrent CountDownLatch TimeUnit]))

(defn image [] (BufferedImage. 960 160 BufferedImage/TYPE_INT_RGB))
(defn await-latch [^CountDownLatch latch] (.await latch 2 TimeUnit/SECONDS))

(deftest latest-view-mailbox-never-runs-capture-or-sink-on-calling-thread
  (let [entered (CountDownLatch. 1) release (CountDownLatch. 1) complete (CountDownLatch. 2)
        seen (atom []) sent (atom []) closed (atom []) caller (Thread/currentThread)
        capture {:open! (constantly :capture)
                 :capture! (fn [_ view]
                             (swap! seen conj [view (Thread/currentThread)])
                             (when (= 1 (:n view)) (.countDown entered) (.await release))
                             (image))
                 :close! (fn [_] (swap! closed conj :capture))}
        sink {:open! (constantly :sink)
              :render! (fn [_ frame] (swap! sent conj [frame (Thread/currentThread)]) (.countDown complete))
              :close! (fn [_] (swap! closed conj :sink))}
        transport (display/transport {:capture capture :sink sink :max-fps 60})
        handle ((:open! transport))]
    (try
      ((:render! transport) handle {:n 1})
      (is (await-latch entered))
      (is (thrown? clojure.lang.ExceptionInfo ((:render! transport) handle {:a (atom 1) :b nil})))
      (is (thrown? clojure.lang.ExceptionInfo ((:render! transport) handle {:a (byte-array 1) :b false})))
      (doseq [n (range 2 101)] ((:render! transport) handle {:n n}))
      (is (= 100 (:submitted ((:status! transport) handle))))
      (is (= 98 (:dropped ((:status! transport) handle))))
      (.countDown release)
      (is (await-latch complete))
      (is (= [1 100] (mapv (comp :n first) @seen)))
      (is (every? #(not (identical? caller (second %))) (concat @seen @sent)))
      (finally (.countDown release) ((:close! transport) handle)))
    (is (= [:capture :sink] @closed))
    ((:close! transport) handle)
    (is (= [:capture :sink] @closed))))

(deftest capture-failure-closes-resources-and-publishes-diagnostic
  (let [closed (atom []) finished (CountDownLatch. 1)
        transport (display/transport
                    {:capture {:open! (constantly :capture) :capture! (fn [_ _] (throw (ex-info "capture failed" {})))
                               :close! (fn [_] (swap! closed conj :capture))}
                     :sink {:open! (constantly :sink) :render! (fn [_ _] (throw (ex-info "must not publish" {})))
                            :close! (fn [_] (swap! closed conj :sink) (.countDown finished))}})
        handle ((:open! transport))]
    (try
      ((:render! transport) handle {:title "Fail"})
      (is (await-latch finished))
      (is (= :failed (:status ((:status! transport) handle))))
      (is (= "capture failed" (:error ((:status! transport) handle))))
      (is (thrown? clojure.lang.ExceptionInfo ((:render! transport) handle {})))
      (is (= [:capture :sink] @closed))
      (finally ((:close! transport) handle)))))

(deftest partial-open-failure-closes-capture-and-close-interrupts-in-flight-work
  (let [closed (atom 0)
        transport (display/transport {:capture {:open! (constantly :capture) :capture! (fn [_ _]) :close! (fn [_] (swap! closed inc))}
                                      :sink {:open! #(throw (ex-info "sink missing" {})) :render! (fn [_ _]) :close! (fn [_])}})]
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"sink missing" ((:open! transport))))
    (is (= 1 @closed)))
  (let [entered (CountDownLatch. 1) blocked (CountDownLatch. 1) closed (atom [])
        transport (display/transport {:capture {:open! (constantly :capture)
                                               :capture! (fn [_ _] (.countDown entered) (.await blocked) (image))
                                               :close! (fn [_] (swap! closed conj :capture))}
                                      :sink {:open! (constantly :sink) :render! (fn [_ _] (swap! closed conj :unexpected-send))
                                             :close! (fn [_] (swap! closed conj :sink))}})
        handle ((:open! transport))]
    ((:render! transport) handle {}) (is (await-latch entered))
    ((:close! transport) handle)
    (is (= [:capture :sink] @closed))
    (is (= :closed (:status ((:status! transport) handle))))))

(deftest injected-wayang-renderer-blits-image-and-keeps-exclusive-lease
  (let [target (image) source (image) calls (atom [])
        backend {:open! #(do (swap! calls conj :open) target)
                 :send! #(swap! calls conj :send) :close! #(swap! calls conj :close)}
        sink (transports/wayang-display {:backend backend :renderer! display/blit!})
        competing (transports/wayang-display {:backend backend})
        handle ((:open! sink))]
    (try
      (.setRGB source 10 10 0xff22aa)
      ((:render! sink) handle source)
      (is (= (.getRGB source 10 10) (.getRGB target 10 10)))
      (is (thrown? clojure.lang.ExceptionInfo ((:open! competing))))
      (is (= [:open :send] @calls))
      (is (thrown? clojure.lang.ExceptionInfo (display/blit! target (BufferedImage. 1 1 BufferedImage/TYPE_INT_RGB))))
      (finally ((:close! sink) handle)))
    ((:close! sink) handle)
    (is (= [:open :send :close] @calls))))

(deftest actual-owned-node-process-validates-png-and-closes-on-deadline
  (let [script (File/createTempFile "tolglow-display-protocol-" ".js")
        bytes (ByteArrayOutputStream.)
        _ (javax.imageio.ImageIO/write (image) "png" bytes)
        png (.encodeToString (Base64/getEncoder) (.toByteArray bytes))]
    (try
      ;; The transport's external browser process is represented here by an
      ;; owned protocol peer. Actual re-frame/Chromium capture is a separate QA.
      (spit script (str "const readline=require('node:readline');\n"
                        "process.stdout.write(JSON.stringify({status:'ready',width:960,height:160})+'\\n');\n"
                        "readline.createInterface({input:process.stdin}).on('line',line=>{const r=JSON.parse(line);"
                        "if(!r.view.stall)process.stdout.write(JSON.stringify({id:r.id,status:'captured',width:960,height:160,png:'" png "'})+'\\n');});\n"))
      (let [capture (display/process-capture {:browser-executable "/not-used-by-protocol-peer"
                                              :worker-script (.getAbsolutePath script)
                                              :capture-timeout-ms 100 :shutdown-timeout-ms 100})
            handle ((:open! capture))]
        (try
          (is (= 960 (.getWidth ^BufferedImage ((:capture! capture) handle {:title "Test"}))))
          (is (thrown-with-msg? clojure.lang.ExceptionInfo #"deadline" ((:capture! capture) handle {:stall true})))
          (is (false? (.isAlive ^Process (:process handle))))
          (is (thrown? clojure.lang.ExceptionInfo ((:capture! capture) handle {:title "After close"})))
          (finally ((:close! capture) handle))))
      (finally (io/delete-file script true)))))
