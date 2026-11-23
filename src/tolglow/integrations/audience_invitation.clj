(ns tolglow.integrations.audience-invitation
  "One supervised nearby-stage process and its ephemeral guest invitation."
  (:require [clojure.java.io :as io]
            [clojure.string :as str])
  (:import [java.net URI]
           [java.nio.file Files Path]
           [java.nio.file.attribute FileAttribute]
           [java.util UUID]))

(defn- invalid! [message]
  (throw (ex-info message {:http/status 422})))

(defn- origin! [value]
  (when-not (and (string? value) (<= 1 (count value) 253))
    (invalid! "Enter the HTTPS origin associated with the signed App Clip."))
  (let [^URI uri (try (URI. value) (catch java.net.URISyntaxException _ nil))]
    (when-not (and uri (= "https" (.getScheme uri)) (.getHost uri)
                   (nil? (.getUserInfo uri)) (nil? (.getQuery uri)) (nil? (.getFragment uri))
                   (#{"" "/"} (.getPath uri)) (#{-1 443} (.getPort uri)))
      (invalid! "Enter an HTTPS origin without a path, query or credentials."))
    (str/replace value #"/$" "")))

(defn create [executable]
  (let [available? (boolean (and executable (.isFile (io/file executable))
                                 (.canExecute (io/file executable))))]
    {:executable executable :active (atom nil) :lock (Object.)
     :status (atom {:available? available? :phase :idle :sources [] :receiver? false})}))

(defn status [stage] @(:status stage))

(defn inputs! [stage devices receiver]
  (when (:available? (status stage))
    (swap! (:status stage) assoc
           :sources (vec (sort (keep (fn [[id device]] (when (:token device) id)) devices)))
           :receiver? (boolean (:accepts-loopback? receiver)))))

(defn- publish-line! [stage run line]
  (let [origin (:origin run)
        url (when (str/starts-with? line "Stage invitation ") (subs line 17))
        fingerprint (when (and url (str/starts-with? url (str origin "/audience/")))
                      (last (str/split url #"/")))
        update (cond
                 (and fingerprint (re-matches #"[0-9a-f]{64}" fingerprint)
                      (= url (str origin "/audience/" fingerprint))) {:url url}
                 (= line "Stage discoverable") {:phase :discoverable}
                 (str/starts-with? line "Stage Bluetooth unavailable") {:phase :bluetooth-unavailable}
                 (#{"Bluetooth advertising failed" "Could not register stage service"} line)
                 {:phase :advertising-failed})]
    (when update
      (locking (:lock stage)
        (when (identical? run @(:active stage))
          (swap! (:status stage) merge update))))))

(defn- delete-files! [{:keys [^Path qr ^Path directory]}]
  (Files/deleteIfExists qr)
  (Files/deleteIfExists directory))

(defn- clear-run [status phase]
  (-> status (dissoc :id :url :stage-name :device-id) (assoc :phase phase)))

(defn- watch-child! [stage {:keys [^Process child] :as run}]
  (let [watch (fn []
                (try
                  (with-open [reader (io/reader (.getInputStream child))]
                    (doseq [line (line-seq reader)] (publish-line! stage run line)))
                  (finally
                    (.waitFor child)
                    (locking (:lock stage)
                      (when (identical? run @(:active stage))
                        (reset! (:active stage) nil)
                        (swap! (:status stage) clear-run :exited)))
                    (delete-files! run))))]
    (doto (Thread. ^Runnable watch "tolglow-stage-invitation")
      (.setDaemon true) (.start))))

(defn- launch [executable name origin ^Path qr device-id port token]
  (let [^java.util.List arguments [executable name origin (str qr)]
        command (ProcessBuilder. arguments)
        environment (.environment command)
        inherited (select-keys (System/getenv) ["HOME" "TMPDIR" "LANG"])]
    (.clear environment)
    (.putAll environment inherited)
    (.putAll environment {"TOLGLOW_AUDIENCE_SUPERVISED" "1"
                          "TOLGLOW_AUDIENCE_OSC_ID" device-id "TOLGLOW_AUDIENCE_OSC_PORT" (str port)
                          "TOLGLOW_AUDIENCE_OSC_TOKEN" token})
    (.redirectErrorStream command true)
    (.start command)))

(defn start! [stage {:keys [device-id stage-name origin]} receiver token]
  (let [origin (origin! origin)]
    (when-not (and (string? stage-name) (<= 1 (count stage-name) 48)
                   (not (str/blank? stage-name)) (not (re-find #"[\p{Cntrl}]" stage-name)))
      (invalid! "Enter a stage name between 1 and 48 characters."))
    (when-not (and (:accepts-loopback? receiver) token)
      (invalid! "Start the local OSC receiver and choose an enrolled sensor."))
    (locking (:lock stage)
      (when-not (:available? (status stage)) (invalid! "The nearby-stage helper is unavailable."))
      (when @(:active stage) (invalid! "Stop the current stage before starting another."))
      (let [directory (Files/createTempDirectory "tolglow-stage-" (make-array FileAttribute 0))
            qr (.resolve directory "invitation.png")
            files {:directory directory :qr qr}]
        (try
          (let [child (launch (:executable stage) stage-name origin qr device-id (:port receiver) token)
                id (str (UUID/randomUUID))
                run (assoc files :child child :id id :device-id device-id :origin origin)]
            (reset! (:active stage) run)
            (swap! (:status stage) assoc :phase :starting :id id :device-id device-id :stage-name stage-name)
            (watch-child! stage run)
            (status stage))
          (catch Exception error (delete-files! files) (throw error)))))))

(defn stop!
  ([stage] (stop! stage nil))
  ([stage device-id]
   (locking (:lock stage)
     (when-let [{:keys [^Process child] :as run} @(:active stage)]
       (when (or (nil? device-id) (= device-id (:device-id run)))
         (reset! (:active stage) nil)
         (swap! (:status stage) clear-run :idle)
         (.destroyForcibly child)))
     (status stage))))

(defn stop-current! [stage id]
  (locking (:lock stage)
    (when-not (and id (= id (:id @(:active stage))))
      (invalid! "This stage stopped or its invitation changed."))
    (stop! stage)))

(defn png [stage id]
  (locking (:lock stage)
    (let [{:keys [^Path qr] :as run} @(:active stage)]
      (when (and run id (= id (:id run)) (:url (status stage)) (<= 1 (Files/size qr) 524288))
        (Files/readAllBytes qr)))))
