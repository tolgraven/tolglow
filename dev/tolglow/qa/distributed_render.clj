(ns tolglow.qa.distributed-render
  "Prove distributed shader output at the receiving end of a loopback Art-Net link."
  (:require [compute.fixture :as fixture]
            [afterglow.render.compute :as compute]
            [afterglow.render.chain :as chain]
            [afterglow.render.distributed :as distributed]
            [afterglow.render.presentation :as presentation]
            [afterglow.render.verified :as verified]
            [clojure.data.json :as json]
            [clojure.java.io :as io]
            [tolglow.show :as show]
            [tolglow.studio :as studio])
  (:import [java.net DatagramPacket DatagramSocket InetAddress SocketException SocketTimeoutException]
           [java.nio.file Files]
           [java.util Arrays HexFormat]
           [java.util.concurrent TimeUnit]))

(defn- eventually [description predicate]
  (let [deadline (+ (System/nanoTime) 90000000000)]
    (loop []
      (or (predicate)
          (if (< (System/nanoTime) deadline) (do (Thread/sleep 30) (recur))
            (throw (ex-info (str "Timed out waiting for " description) {})))))))

(defn- hex [^bytes values] (.formatHex (HexFormat/of) values))

(defn- listen-dmx []
  (let [socket (DatagramSocket. 0 (InetAddress/getLoopbackAddress))
        captured (atom [])]
    (.setSoTimeout socket 250)
    (let [reader (future
                   (while (not (.isClosed socket))
                     (try
                       (let [buffer (byte-array 548) packet (DatagramPacket. buffer (alength buffer))
                             _ (.receive socket packet)
                             u8 #(bit-and 255 (aget buffer %))
                             size (+ (bit-shift-left (u8 16) 8) (u8 17))]
                         (assert (and (= 0x50 (u8 9)) (= (+ 18 size) (.getLength packet))))
                         (swap! captured conj [(u8 12) (inc (+ (u8 14) (bit-shift-left (u8 15) 8)))
                                               (hex (Arrays/copyOfRange buffer 18 (+ 18 size)))]))
                       (catch SocketTimeoutException _)
                       (catch SocketException e (when-not (.isClosed socket) (throw e))))))]
      {:port (.getLocalPort socket) :packets captured :stop! #(do (.close socket) @reader)})))

(defn- byte-frame [runtime]
  (update-vals (get-in @(:state runtime) [:output :universes]) hex))

(defn- frames [packets]
  (for [parts (partition-by first packets)]
    (into {} (map (fn [[_ u data]] [u data])) parts)))

(defn- submit-all! [runtime commands]
  (locking (:lock runtime) (mapv #(show/submit! runtime %) commands)))

(defn- accepted [receipts]
  (mapv #(let [ack (deref % 15000 ::timeout)]
           (when-not (= :accepted (:status ack)) (throw (ex-info "Rejected proof command" {:ack ack})))
           ack) receipts))

(defn- stats [runtime] (compute/status (:compute runtime)))

(defn- phase! [show cpu capture log label commands]
  (let [previous (count @(:packets capture))
        pending (submit-all! cpu commands)
        _ (show/tick! cpu (+ 1000.0 (* 1000 (count @log))))
        _ (accepted pending)
        expected (byte-frame cpu)
        receipts (accepted (submit-all! show commands))]
    (eventually label #(some #{expected} (frames (drop previous @(:packets capture)))))
    (println label (stats show))
    (swap! log conj {:phase label :expected expected :compute (stats show)})
    receipts))

(defn- phase-run [phases]
  (mapv #(dissoc % :expected) phases))

(defn- verify-artnet! [root capture cpu phases]
  ;; The output scheduler emits its patch-specific idle bytes before the first
  ;; computed frame is ready. After that, only the ordered CPU transitions may appear.
  (let [idle (->> (:idle-universes (presentation/compile-plan (get-in @(:state cpu) [:model :rig :fixtures])))
                  (map (fn [[u bytes]] [u (hex (byte-array (map unchecked-byte bytes)))])) (into {}))
        packets @(:packets capture)
        output (vec (frames packets))
        complete (filterv #(= (set (keys %)) #{1 2}) output)
        partial (filterv #(not= (set (keys %)) #{1 2}) output)
        gold (mapv :expected phases)
        timeline (vec (dedupe (drop-while #{idle} complete)))
        expected (vec (dedupe (drop-while #{idle} gold)))
        candidates (conj gold idle)
        partial-ok? (fn [part] (some #(= part (select-keys % (keys part))) candidates))]
    (when-not (and (seq complete) (= expected timeline) (every? partial-ok? partial))
      (spit (io/file root "capture.edn")
            (pr-str {:expected gold :timeline timeline :idle idle :partial partial
                     :unexpected (vec (keep-indexed (fn [i f] (when-not ((set candidates) f) [i f])) complete))}))
      (throw (ex-info "Art-Net changed outside the independent CPU timeline"
                      {:received (count timeline) :expected (count expected)
                       :frames (count complete) :partial (count partial)})))
    {:frames (count complete) :partial-frames (count partial) :packets (count packets)
     :pre-roll-frames (count (take-while #{idle} complete))}))

(defn- prove! [runtime cpu capture root mode guest-counts]
  (let [phases (atom [])
        peer-count (if (= mode :operator-phone) 1 2)
        pool #(:pool (stats runtime))
        wait-draws (fn []
                     (let [target (+ 3 (:completed (pool)))]
                       (eventually "three more accepted remote frames" #(>= (:completed (pool)) target))))
        advance #(phase! runtime cpu capture phases %1 %2)]
    (eventually "authenticated foreground workers" #(= peer-count (:workers (pool))))
    (advance "unlit" [])
    (let [launches (mapv #(hash-map :op :cue/launch :cue-id (:id (fixture/program %))) (range 18))
          cold-start (System/nanoTime)
          id (:instance-id (last (advance "18 active moving-head cues" launches)))
          _ (eventually "first accepted remote frame" #(pos? (:completed (pool))))
          cold-ms (/ (- (System/nanoTime) cold-start) 1e6)]
      (wait-draws)
      (advance "edit swatch and 3D aim"
               [{:op :param/set :instance-id id :param :red :value 0.875}
                {:op :param/set :instance-id id :param :aim-x :value -0.75}])
      (advance "expose lower priority with alpha" [{:op :instance/opacity-set :instance-id id :value 0.5}])
      (wait-draws)
      (when-not (= mode :operator-phone)
        (let [misses (:missed (pool))]
          (spit (io/file root "lose-peer") "Drop one in-flight response")
          (if (= mode :audience-phones)
            (do (println "Stop sharing GPU on exactly one phone; keep the other foregrounded")
                (eventually "one participant leaves the stage" #(= 1 (:workers (pool)))))
            (eventually "peer loss during a leased run"
                        #(let [p (pool)] (and (= 1 (:workers p)) (> (:missed p) misses)))))))
      (let [before (when guest-counts (:full-slices @guest-counts))]
        (advance "promote the next running cue" [{:op :instance/remove :instance-id id}])
        (wait-draws)
        (when guest-counts
          (let [verified (- (:full-slices @guest-counts) before)]
            (assert (pos? verified) "The surviving guest must have a new fully verified slice after peer loss")
            (swap! guest-counts assoc :full-slices-after-loss verified))))
      (let [result (merge {:heads 64 :layers 18 :universes 2 :first-completed-ms cold-ms
                           :final (stats runtime) :phases (phase-run @phases)}
                          (verify-artnet! root capture cpu @phases))]
        (spit (io/file root "finish") "done")
        result))))

(defn- observe! [runtime cpu capture dir mode & [guest-counts]]
  (let [fuse chain/fuse evaluate distributed/evaluate!
        started (System/nanoTime) stages (atom {}) samples (atom [])
        timed-fusion (fn [cache prepare blocks]
                       (let [lowered (fuse cache prepare blocks)]
                         (when (seq blocks)
                           (swap! stages update [(count blocks) (count lowered)] (fnil inc 0)))
                         lowered))
        timed-render (fn [pool group env deadline]
                       (let [t (System/nanoTime) batch (evaluate pool group env deadline)]
                         (when (and batch (< (count @samples) 10000))
                           (swap! samples conj {:at-ms (/ (- t started) 1e6)
                                                :duration-ms (/ (- (System/nanoTime) t) 1e6)}))
                         batch))]
    (with-redefs [chain/fuse timed-fusion distributed/evaluate! timed-render]
      (let [result (prove! runtime cpu capture dir mode guest-counts)]
        (when-not (and (get @stages [18 1]) (get @stages [17 1]))
          (throw (ex-info "Expected 18 and later 17 native groups to fuse to one" {:shapes @stages})))
        (assoc result
               :fusion (mapv (fn [[[before after] n]] {:input before :output after :frames n}) @stages)
               :remote-samples @samples)))))

(defn- observe-guests! [runtime cpu capture dir mode]
  (let [full verified/readback pair verified/agree?
        counts (atom {:full-slices 0 :sentinel-rows 0 :redundant-slices 0})]
    (with-redefs [verified/readback (fn [group env job bytes]
                                      (when-let [checked (full group env job bytes)]
                                        (swap! counts update (if (= 1 (:count job)) :sentinel-rows :full-slices) inc)
                                        checked))
                  verified/agree? (fn [group env job first-copy second-copy]
                                    (when (pair group env job first-copy second-copy)
                                      (swap! counts update :redundant-slices inc)
                                      true))]
      (let [result (observe! runtime cpu capture dir mode counts)
            {:keys [redundant-slices]} @counts]
        (when (= "redundant" (System/getenv "TOLGLOW_AUDIENCE_VERIFICATION"))
          (assert (pos? redundant-slices) "Both independently admitted guests must verify a shared slice"))
        (assoc result :guest-verifications @counts)))))

(defn- listener [nearby?]
  (if-not nearby? {:port 0}
    (let [origin (System/getenv "TOLGLOW_COMPUTE_QA_ORIGIN")]
      (when-not (and origin (.startsWith origin "https://"))
        (throw (ex-info "Set TOLGLOW_COMPUTE_QA_ORIGIN to the full app’s HTTPS Studio origin" {})))
      {:host "127.0.0.1" :port (parse-long (or (System/getenv "TOLGLOW_COMPUTE_QA_PORT") "8500"))
       :public-origin origin :auth {:token-env "TOLGLOW_STUDIO_TOKEN"}})))

(defn- run-clients! [runtime cpu capture dir target]
  (let [options (case target
                  "nearby" (listener true)
                  "audience" {:host "127.0.0.1" :port (parse-long (or (System/getenv "TOLGLOW_AUDIENCE_COMPUTE_PORT") "8500"))}
                  (listener false))
        {:keys [port stop! public-origin]} (studio/listen! runtime options)]
    (try
      (case target
        "nearby" (do (println "Open" public-origin "in the full app and choose Connections → Contribute this GPU")
                     (observe! runtime cpu capture dir :operator-phone))
        "audience" (do (println "Start the stage bridge for port" port "and enroll two full-app phones via Share GPU with this stage")
                       (observe-guests! runtime cpu capture dir :audience-phones))
        (let [guest? (= target "guest")
              script (if guest? "dev/compute/guest-process.cjs" "dev/compute/capture.cjs")
              browser (ProcessBuilder. ^java.util.List ["node" script (str "http://127.0.0.1:" port)
                                                       (.getAbsolutePath dir) target])
              process (.start (.inheritIO browser))]
          (try
            (let [result (if guest? (observe-guests! runtime cpu capture dir :guest-bundles) (observe! runtime cpu capture dir :operator-browsers))]
              (assert (and (.waitFor process 30 TimeUnit/SECONDS) (zero? (.exitValue process))))
              result)
            (finally
              (spit (io/file dir "finish") "stop")
              (when-not (.waitFor process 8 TimeUnit/SECONDS) (.destroyForcibly process))))))
      (finally (stop!)))))

(defn -main [directory target & [rate budget]]
  (let [rate (parse-long (or rate "40")) budget (parse-long (or budget "18")) parent (io/file directory)
        _ (.mkdirs parent)
        dir (.toFile (Files/createTempDirectory (.toPath parent) "run-" (make-array java.nio.file.attribute.FileAttribute 0)))
        config {:model (fixture/model) :frame-rate rate :adaptive-keyframes {:enabled? false} :workers 2}
        capture (listen-dmx) cpu (show/create-runtime config)]
    (try
      (let [runtime (show/start! (assoc config :compute-backend :distributed :gpu-budget-ms budget
                                        :artnet {:host "127.0.0.1" :port (:port capture)}))]
        (try
          (let [result (run-clients! runtime cpu capture dir target)]
            (spit (io/file dir "host.json")
                  (json/write-str (assoc result :target-hz rate :budget-ms budget
                                         :clients (case target "nearby" :operator-enrolled-full-app
                                                               "audience" :two-stage-admitted-full-app-phones
                                                               "guest" :two-full-app-bundles-loopback :two-chromium))))
            (println "Verified Art-Net at" (.getAbsolutePath dir) (select-keys result [:frames :packets :partial-frames])))
          (finally (println "Final render counters" (stats runtime)) (show/stop! runtime))))
      (finally (show/stop! cpu) ((:stop! capture)) (shutdown-agents)))))
