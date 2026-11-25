(ns tolglow.controllers.content-browser-host-test
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [tolglow.demo :as demo]
            [tolglow.controllers.content-browser-host :as host]
            [tolglow.show :as show]
            [tolglow.show.recovery :as recovery])
  (:import [java.nio.file Files LinkOption]
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

(defn- preview-file [runtime saves show-id kind]
  (let [projection (host/project runtime saves {:kind :show})
        entry (row projection [:show show-id])]
    (host/preview runtime saves (assoc (selection projection entry) :source (get-in entry [:sources kind])))))

(defn- tick-command! [runtime command]
  (let [receipt (show/submit! runtime command)]
    (show/tick! runtime 0)
    @receipt))

(defn- manual-runtime [directory]
  (assoc (show/create-runtime {:workspace-options nil :workers 1})
         :recovery (atom (recovery/start! (recovery/options {:enabled? false :directory directory})
                                         false (constantly nil) identity))))

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
                preview (host/preview runtime saves (assoc (selection projection active) :op :load))
                recovery-preview (preview-file runtime saves :main :recovery)
                named-preview (preview-file runtime saves :main :named)]
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
            (is (= :preview (:status recovery-preview) (:status named-preview)))
            (is (not= (get-in recovery-preview [:confirmation :token]) (get-in named-preview [:confirmation :token])))
            (is (= {:status :rejected :reason :stale-save-source}
                   (host/confirm-load! runtime saves (assoc (:confirmation recovery-preview)
                                                        :token (get-in named-preview [:confirmation :token])))))
            (is (= before (control-state runtime)))
            (is (= (:checkpoints status) (:checkpoints (show/recovery-status runtime))))
            (is (not (str/includes? (pr-str [projection preview recovery-preview named-preview]) (str directory))))
            (is (= :accepted (-> (host/confirm-load! runtime saves (:confirmation recovery-preview))
                                 :receipt (deref 5000 {}) :status))))
          (finally (show/stop! runtime)))))))

(deftest named-confirmation-checks-the-exact-file-and-its-bytes
  (with-directory
    (fn [directory]
      (let [runtime (manual-runtime directory)
            file (io/file directory "manual.transit")
            path (.toPath file)
            rejected {:status :rejected :reason :stale-save-source}]
        (try
          (is (= :accepted (:status (tick-command! runtime {:op :master/set :value 0.2}))))
          (show/save-show! runtime file)
          (let [modified (Files/getLastModifiedTime path (make-array LinkOption 0))
                original (slurp file)
                initial (preview-file runtime [] :main :named)
                confirmation (:confirmation initial)]
            (is (= :preview (:status initial)))
            (is (string? (:token confirmation)))
            (is (= :accepted (:status (tick-command! runtime {:op :master/set :value 0.7}))))
            (let [before (control-state runtime)]
              (testing "an atomic replacement with the same bytes and modification time"
                (recovery/write-source! file original)
                (Files/setLastModifiedTime path modified)
                (is (= rejected (host/confirm-load! runtime [] confirmation)))
                (is (= before (control-state runtime))))
              (let [current (-> (preview-file runtime [] :main :named) :confirmation)
                    replacement (str/replace-first original "0.2" "0.9")]
                (testing "an in-place rewrite with the same size and modification time"
                  (is (not= original replacement))
                  (is (= (count original) (count replacement)))
                  (spit file replacement :encoding "UTF-8")
                  (Files/setLastModifiedTime path modified)
                  (is (= rejected (host/confirm-load! runtime [] current)))
                  (is (= before (control-state runtime))))))
            (show/save-show! runtime file)
            (let [confirmation (:confirmation (preview-file runtime [] :main :named))]
              (is (= :accepted (:status (tick-command! runtime {:op :master/set :value 0.4}))))
              (let [result (host/confirm-load! runtime [] confirmation)]
                (is (= :submitted (:status result)))
                (is (= 0.4 (:master (show/snapshot runtime))))
                (show/save-show! runtime file)
                (show/tick! runtime 0)
                (is (= :accepted (:status @(:receipt result))))
                (is (= 0.7 (:master (show/snapshot runtime))))
                (is (= {:status :rejected :reason :stale-session}
                       (host/confirm-load! runtime [] confirmation))))))
          (finally (show/stop! runtime)))))))

(deftest configured-saves-resolve-trusted-paths-and-fence-the-frame-owner
  (with-directory
    (fn [directory]
      (let [runtime (manual-runtime directory)
            saved (show/create-runtime {:workspace-options nil :workers 1
                                        :model (assoc (demo/model) :show-id :tour :master 0.45)})
            file (io/file directory "tour.transit")
            other-file (io/file directory "other.transit")
            saves [{:show-id :tour :name "Tour" :source-kind :named :handle "tour-save" :path file}]]
        (try
          (show/save-show! saved file)
          (recovery/write-source! other-file (slurp file))
          (let [preview (preview-file runtime saves :tour :named)
                confirmation (:confirmation preview)
                before (control-state runtime)]
            (is (= :preview (:status preview)))
            (is (not (str/includes? (pr-str preview) (str directory))))
            (is (= {:status :rejected :reason :confirmation-required}
                   (host/confirm-load! runtime saves (dissoc confirmation :token))))
            (is (= {:status :rejected :reason :stale-save-source}
                   (host/confirm-load! runtime [(assoc (first saves) :path other-file)] confirmation)))
            (is (= before (control-state runtime)))
            (let [restart (show/submit! runtime {:op :show/restart})
                  restore (host/confirm-load! runtime saves confirmation)]
              (is (= :submitted (:status restore)))
              (show/tick! runtime 0)
              (is (= :accepted (:status @restart)))
              (is (= :rejected (:status @(:receipt restore))))
              (is (= :main (:show-id (show/snapshot runtime)))))
            (let [confirmed (:confirmation (preview-file runtime saves :tour :named))
                  restore (host/confirm-load! runtime saves (assoc confirmed :path other-file))]
              (is (= :submitted (:status restore)))
              (show/tick! runtime 0)
              (is (= :accepted (:status @(:receipt restore))))
              (is (= [:tour 0.45] ((juxt :show-id :master) (show/snapshot runtime))))))
          (finally (show/stop! saved) (show/stop! runtime)))))))

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
