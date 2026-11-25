(ns tolglow.integrations.track-profile-test
  (:require [clojure.edn :as edn]
            [clojure.test :refer [deftest is]]
            [tolglow.integrations.track-profile :as track-profile])
  (:import [java.io ByteArrayInputStream]
           [java.nio ByteBuffer ByteOrder]
           [java.nio.file CopyOption Files Path StandardCopyOption]
           [java.nio.file.attribute FileAttribute]
           [javax.sound.sampled AudioFileFormat$Type AudioFormat AudioInputStream AudioSystem]))

(def ^:private sample-rate 8000)

(def ^:private frames-per-beat 4000)

(def ^:private phrase-a [48.0 64.0 84.0 104.0])

(def ^:private phrase-b [84.0 104.0 48.0 64.0])

(def ^:private options {:beat-grid {:bpm 120}})

(defn- sine [hz phase seconds]
  (Math/sin (+ phase (* 2.0 Math/PI hz seconds))))

(defn- pcm16 [frames]
  (let [rng (java.util.Random. 514)
        phases (into {} (map (fn [hz] [hz (* 2.0 Math/PI (.nextDouble rng))]) (conj phrase-a 700.0)))
        buffer (doto (ByteBuffer/allocate (* 2 frames)) (.order ByteOrder/LITTLE_ENDIAN))]
    (dotimes [frame frames]
      (let [beat (quot frame frames-per-beat)
            notes (if (<= 8 beat 15) phrase-b phrase-a)
            note (nth notes (mod beat 4))
            seconds (/ (rem frame frames-per-beat) (double sample-rate))
            bass (* 10000.0 (sine note (get phases note) seconds))
            kick (* 2100.0 (Math/exp (/ (- seconds) 0.018)) (sine 56.0 0.0 seconds))
            texture (* 800.0 (sine 700.0 (get phases 700.0) seconds))]
        (.putShort buffer (short (Math/round (+ bass kick texture))))))
    (.array buffer)))

(defn- temp-wave []
  (Files/createTempFile "tolglow-track-profile-" ".wav" (make-array FileAttribute 0)))

(defn- write-wave! [^Path path frames]
  (let [data (pcm16 frames)
        format (AudioFormat. (float sample-rate) 16 1 true false)]
    (with-open [input (AudioInputStream. (ByteArrayInputStream. data) format frames)]
      (AudioSystem/write input AudioFileFormat$Type/WAVE (.toFile path)))))

(deftest local-pcm-profile-preserves-section-repetition-and-exact-recording-positions
  (let [path (temp-wave)
        renamed (temp-wave)
        frames (+ (* 24 frames-per-beat) 37)]
    (try
      (write-wave! path frames)
      (Files/copy path renamed (into-array CopyOption [StandardCopyOption/REPLACE_EXISTING]))
      (let [profile (track-profile/analyze-file path options)
            copy (track-profile/analyze-file (.toFile renamed) options)
            revised (track-profile/analyze-file path {:beat-grid {:bpm 120.0 :anchor-beat 1.125}})
            {:keys [version reference provenance duration-ms beats sections events]} profile]
        (is (= 1 version))
        (is (= profile copy (edn/read-string (pr-str profile))))
        (is (every? #(re-matches #"sha256:[0-9a-f]{64}" %) ((juxt :id :version) reference)))
        (is (= (:id reference) (get-in revised [:reference :id])))
        (is (not= (:version reference) (get-in revised [:reference :version])))
        (is (= {:kind :local-audio-file :identity-basis :native-rate-signed-pcm16-le
                :sample-rate-hz 8000.0 :channels 1 :frames frames} provenance))
        (is (= 12004.625 duration-ms))
        (is (= (mapv #(hash-map :beat % :position-ms (* % 500.0)) (range 25)) beats))
        (is (= [[8.0 4000.0 16.0 8000.0] [16.0 8000.0 24.0 12000.0]]
               (mapv (juxt :beat :position-ms :confirmed-at-beat :confirmation-position-ms) events)))
        (is (= [9.125 17.125] (mapv :beat (:events revised))))
        (is (= (mapv :position-ms events) (mapv :position-ms (:events revised))))
        (is (every? #(= :audio/section-change (:type %)) events))
        (is (every? #(<= 0.45 (:confidence %) 1.0) events))
        (is (= [[0.0 4000.0] [4000.0 8000.0] [8000.0 12004.625]]
               (mapv (juxt :start-position-ms :end-position-ms) sections)))
        (is (= [:file-start :confirmed-change :confirmed-change] (mapv :boundary-kind sections)))
        (is (= [0 1 0] (mapv :family-id sections)))
        (is (= (get-in sections [0 :features :signature]) (get-in sections [2 :features :signature])))
        (is (not= (get-in sections [0 :features :signature]) (get-in sections [1 :features :signature])))
        (is (every? #(= [:bass-pattern] (get-in % [:evidence :drivers])) events)))
      (finally
        (Files/deleteIfExists path)
        (Files/deleteIfExists renamed)))))

(deftest incomplete-final-phrase-does-not-invent-a-boundary
  (let [path (temp-wave)
        frames (+ (* 15 frames-per-beat) 37)]
    (try
      (write-wave! path frames)
      (let [profile (track-profile/analyze-file path options)]
        (is (= frames (get-in profile [:provenance :frames])))
        (is (= 7504.625 (:duration-ms profile)))
        (is (empty? (:events profile)))
        (is (= [[0.0 7504.625]] (mapv (juxt :start-position-ms :end-position-ms) (:sections profile))))
        (is (thrown? clojure.lang.ExceptionInfo (track-profile/analyze-file path {})))
        (is (thrown? clojure.lang.ExceptionInfo
                     (track-profile/analyze-file path {:beat-grid {:bpm 1000}})))
        (is (thrown? clojure.lang.ExceptionInfo
                     (track-profile/analyze-file path (assoc options :input-channel 2))))
        (is (thrown? clojure.lang.ExceptionInfo
                     (track-profile/analyze-file (.toUri path) options))))
      (finally (Files/deleteIfExists path)))))
