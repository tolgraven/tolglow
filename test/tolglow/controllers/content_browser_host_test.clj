(ns tolglow.controllers.content-browser-host-test
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [tolglow.controllers.content-browser-host :as host]
            [tolglow.show :as show]
            [tolglow.show.recovery :as recovery])
  (:import [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]))

(defn- with-directory [f]
  (let [directory (.toFile (Files/createTempDirectory "tolglow-content-browser-"
                                                     (make-array FileAttribute 0)))]
    (try (f directory)
         (finally (doseq [file (reverse (file-seq directory))] (io/delete-file file))))))

(defn- await-checkpoint [runtime]
  (let [deadline (+ (System/nanoTime) 5000000000)]
    (loop []
      (let [status (show/recovery-status runtime)]
        (if (contains? (:available status) :recovery) status
          (when (< (System/nanoTime) deadline)
            (Thread/sleep 10)
            (recur)))))))

(defn- row [projection key]
  (some #(when (= key (:key %)) %) (:items projection)))

(defn- selection [projection entry]
  {:runtime-id (:runtime-id projection) :selection (:selection entry)})

(defn- control-state [runtime]
  {:ingress-order @(:ingress-order runtime)
   :model (select-keys (:model @(:state runtime)) [:show-id :epoch :revision :registry :instances :master])})

(deftest reads-configured-descriptors-and-the-real-checkpoint-owner
  (with-directory
    (fn [directory]
      (let [runtime (show/start! {:workspace-options nil :workers 1 :frame-rate 4
                                  :recovery {:interval-ms 40 :directory directory}}
                                 (constantly nil))
            named (io/file directory "private-named-file.transit")
            saves [{:show-id :main :source-kind :named :handle :configured
                    :name "Night studies" :path (str named)}
                   {:show-id :tour :source-kind :named :handle "tour-manual"
                    :name "Tour" :path (str (io/file directory "private-tour.transit"))}]]
        (try
          (show/save-show! runtime named)
          (is (some? (await-checkpoint runtime)))
          (recovery/close! @(:recovery runtime))
          (let [before (control-state runtime)
                status (show/recovery-status runtime)
                projection (host/project runtime saves {:kind :show})
                active (row projection [:show :main])
                configured (row projection [:show :tour])
                preview (host/preview runtime saves (assoc (selection projection active) :op :load))]
            (is (= 2 (:total projection)))
            (is (= "Night studies" (:name active)))
            (is (= :checkpoint/named (get-in active [:sources :named :handle])))
            (is (= :checkpoint/recovery (get-in active [:sources :recovery :handle])))
            (is (= (get-in status [:checkpoints :named :modified-at-ms])
                   (get-in active [:sources :named :revision])))
            (is (= (get-in status [:checkpoints :recovery :modified-at-ms])
                   (get-in active [:sources :recovery :revision])))
            (is (= "tour-manual" (get-in configured [:sources :named :handle])))
            (is (= {:status :preview :entry active} preview))
            (is (= before (control-state runtime)))
            (is (= (:checkpoints status) (:checkpoints (show/recovery-status runtime))))
            (is (not (str/includes? (pr-str [projection preview]) (str directory)))))
          (finally (show/stop! runtime)))))))

(deftest previews-fence-the-runtime-and-the-exact-show-epoch
  (let [runtime (show/create-runtime {:workspace-options nil :workers 1})
        other (show/create-runtime {:workspace-options nil :workers 1})
        rejected {:status :rejected :reason :stale-session}]
    (try
      (let [projection (host/project runtime [] {:kind :cue})
            entry (first (:items projection))
            request (selection projection entry)
            before (control-state runtime)]
        (is (some? entry))
        (is (= {:status :preview :entry entry} (host/preview runtime [] (assoc request :op :launch))))
        (is (= before (control-state runtime)))
        (is (= (:show-id projection) (:show-id (host/project other [] {:kind :cue}))))
        (is (= (:epoch projection) (:epoch (host/project other [] {:kind :cue}))))
        (is (= rejected (host/preview other [] request)))
        (let [restart (show/submit! runtime {:op :show/restart})]
          (show/tick! runtime 0)
          (is (= :accepted (:status @restart))))
        (is (= rejected (host/preview runtime [] request)))
        (let [current (host/project runtime [] {:kind :cue :selected-key (:key entry)})]
          (is (= (:key entry) (:selected-key current)))
          (is (= :preview (:status (host/preview runtime [] (selection current (row current (:key entry)))))))))
      (finally (show/stop! other) (show/stop! runtime)))))

(deftest a-checkpoint-observed-across-a-session-change-is-omitted
  (with-directory
    (fn [directory]
      (let [owner (recovery/start! (recovery/options {:enabled? false :directory directory})
                                   false (constantly nil) identity)
            runtime (assoc (show/create-runtime {:workspace-options nil :workers 1})
                           :recovery (atom owner))
            read-status show/recovery-status]
        (try
          (show/save-show! runtime (io/file directory "named.transit"))
          (let [initial (host/project runtime [] {:kind :show})
                raced (with-redefs [show/recovery-status
                                    (fn [runtime]
                                      (let [status (read-status runtime)
                                            restart (show/submit! runtime {:op :show/restart})]
                                        (show/tick! runtime 0)
                                        (is (= :accepted (:status @restart)))
                                        status))]
                        (host/project runtime [] {:kind :show}))]
            (is (= 1 (:total initial)))
            (is (= (inc (:epoch initial)) (:epoch raced)))
            (is (empty? (:items raced)))
            (is (= 1 (:total (host/project runtime [] {:kind :show})))))
          (finally (show/stop! runtime)))))))
