(ns tolglow.fixtures.library
  "Owned fixture catalog boundary for Ring/REPL callers. Network fetches run
  under a private lock, never in atom retry functions or the show render lock.
  Import returns editor drafts and cannot publish, patch or activate fixtures."
  (:require [clojure.data.json :as json]
            [clojure.string :as str]
            [afterglow.render.frame :as render]
            [tolglow.fixtures.bounds :as bounds]
            [tolglow.registry :as registry]
            [tolglow.rig.placement :as placement]
            [tolglow.fixtures.definitions :as library])
  (:import [java.nio.charset StandardCharsets] [java.security MessageDigest] [java.util Base64]))

(def ^:private state-keys #{:version :limits :manufacturers :manufacturer-provenance :entries :documents :revision})

(def ^:private limit-ceilings {:max-catalog-entries 20000 :max-documents 256
                              :max-cache-bytes (* 64 1024 1024) :max-response-bytes (* 16 1024 1024)})

(defn- fail! [message data] (throw (ex-info message (assoc data :fixtures/error :invalid-state))))

(defn- text-bytes [text] (.getBytes ^String text StandardCharsets/UTF_8))

(defn- checksum [text]
  (apply str (map #(format "%02x" (bit-and % 255))
                  (.digest (MessageDigest/getInstance "SHA-256") (text-bytes text)))))

(defn- bounded-text? [value limit] (and (string? value) (<= (count value) limit)))

(defn- catalog-key? [value]
  (and (bounded-text? value 256) (boolean (re-matches #"[a-z0-9-]+/[a-z0-9-]+" value))))

(defn- manufacturer-key? [value]
  (and (bounded-text? value 128) (boolean (re-matches #"[a-z0-9-]+" value))))

(defn- portable? [value depth]
  (and (< depth 32)
       (or (nil? value) (string? value) (keyword? value) (boolean? value)
           (and (number? value) (Double/isFinite (double value)))
           (and (vector? value) (<= (count value) 20000) (every? #(portable? % (inc depth)) value))
           (and (map? value) (<= (count value) 20000)
                (every? (fn [[k v]] (and (or (string? k) (keyword? k)) (portable? v (inc depth)))) value)))))

(defn validate-state
  "Pure cache validation for restore/host settings. Rejects unknown state fields,
  nonportable values, invalid identities, oversized caches and altered document
  checksums. No provider is invoked. Returns the unchanged validated state."
  [state]
  (when-not (and (map? state) (= 1 (:version state)) (every? state-keys (keys state))
                 (portable? state 0) (integer? (:revision state)) (not (neg? (:revision state)))
                 (map? (:limits state)) (= (set (keys limit-ceilings)) (set (keys (:limits state)))))
    (fail! "Invalid fixture-library state" {}))
  (doseq [[key ceiling] limit-ceilings :let [value (get-in state [:limits key])]]
    (when-not (and (integer? value) (<= 1 value ceiling)) (fail! "Invalid fixture cache limit" {:key key})))
  (when-not (and (map? (:entries state)) (map? (:documents state)) (map? (:manufacturers state))
                 (<= (count (:entries state)) (get-in state [:limits :max-catalog-entries]))
                 (<= (count (:manufacturers state)) (get-in state [:limits :max-catalog-entries]))
                 (<= (count (:documents state)) (get-in state [:limits :max-documents])))
    (fail! "Fixture cache exceeds declared capacity" {}))
  (doseq [[key manufacturer] (:manufacturers state)]
    (when-not (and (manufacturer-key? key) (map? manufacturer) (bounded-text? (get manufacturer "name") 512))
      (fail! "Invalid cached manufacturer" {:key key})))
  (doseq [[key entry] (:entries state)]
    (when-not (and (catalog-key? key) (map? entry) (= key (:key entry)) (bounded-text? (:name entry) 512)
                   (= (first (str/split key #"/")) (:manufacturer-key entry))
                   (or (nil? (:manufacturer entry)) (bounded-text? (:manufacturer entry) 512))
                   (or (nil? (:categories entry))
                       (and (vector? (:categories entry)) (<= (count (:categories entry)) 64)
                            (every? #(bounded-text? % 128) (:categories entry)))))
      (fail! "Invalid cached fixture catalog entry" {:key key})))
  (doseq [[key document] (:documents state)]
    (when-not (and (catalog-key? key) (map? document) (= key (:key document)) (= :ofl (:format document))
                   (contains? (:entries state) key) (string? (:text document))
                   (integer? (:byte-count document)) (pos? (:byte-count document))
                   (<= (:byte-count document) (get-in state [:limits :max-response-bytes]))
                   (= (:byte-count document) (alength (text-bytes (:text document))))
                   (integer? (:cache-order document)) (<= 0 (:cache-order document) (:revision state))
                   (= (str library/ofl-base "/" key ".json") (get-in document [:provenance :source]))
                   (= (:sha256 (:provenance document)) (checksum (:text document))))
      (fail! "Invalid or altered cached fixture document" {:key key}))
    (let [data (try (json/read-str (:text document))
                    (catch Exception cause (throw (ex-info "Cached fixture is not valid JSON" {:key key} cause))))]
      (when-not (and (map? data) (map? (get data "availableChannels")) (vector? (get data "modes"))
                     (= (:name document) (get data "name"))
                     (= (:modes document) (mapv #(get % "name") (get data "modes"))))
        (fail! "Cached fixture metadata disagrees with its source" {:key key}))))
  (when (> (reduce + 0 (map :byte-count (vals (:documents state)))) (get-in state [:limits :max-cache-bytes]))
    (fail! "Fixture document cache exceeds byte capacity" {}))
  state)

(defn create-manager
  "Create an owned cache manager without making a network request. Options:
  :provider injected request->response fn, :library portable cache, :limits,
  :http-options for the default fixed-origin OFL provider."
  ([] (create-manager {}))
  ([{:keys [provider limits http-options] :as options}]
   (let [state (validate-state (or (:library options) (library/new-library (or limits {}))))
         provider (or provider (library/ofl-provider (or http-options {})))]
     (when-not (ifn? provider) (fail! "Fixture provider must be callable" {}))
     {:state (atom state) :provider (atom provider) :lock (Object.) :closed? (atom false)})))

(defn- open! [manager]
  (when @(:closed? manager) (throw (ex-info "Fixture manager is closed" {:fixtures/error :closed}))))

(defn export-state [manager]
  (open! manager)
  @(:state manager))

(defn snapshot [manager]
  (let [state (export-state manager)]
    {:version 1 :revision (:revision state) :catalog-count (count (:entries state))
     :document-count (count (:documents state))
     :cache-bytes (reduce + 0 (map :byte-count (vals (:documents state)))) :limits (:limits state)
     :manufacturers (mapv (fn [[key value]] {:key key :name (get value "name") :fixture-count (get value "fixtureCount")})
                          (sort-by (comp str/lower-case #(get % "name") val) (:manufacturers state)))}))

(defn restore!
  "Validate completely before replacing cache state under the manager lock."
  [manager state]
  (let [state (validate-state state)]
    (locking (:lock manager) (open! manager) (reset! (:state manager) state))
    (snapshot manager)))

(defn- update-cache! [manager action]
  (locking (:lock manager)
    (open! manager)
    (let [result (action @(:state manager) @(:provider manager))]
      ;; Validate response-derived cache before it becomes visible or persistable.
      (reset! (:state manager) (validate-state (:library result)))
      (dissoc result :library))))

(defn search!
  "Offline by default. Set :remote? true to query OFL and cache catalog matches.
  Options: :query, optional :manufacturer/:category, :limit (1..500)."
  [manager options]
  (open! manager)
  (let [options (merge {:query "" :limit 50} options)]
    (if-not (:remote? options) (library/search @(:state manager) options)
        (do
          (library/search @(:state manager) options)
          (let [result (update-cache! manager
                         #(library/search-remote %1 %2 (:query options)
                            {:manufacturers (if-let [key (:manufacturer options)] [key] [])
                             :categories (if-let [category (:category options)] [category] [])
                             :retrieved-at (:retrieved-at options)}))]
            {:results (vec (take (:limit options) (:results result))) :total (count (:results result))
             :offline? false :catalog-count (:catalog-count (snapshot manager))})))))

(defn refresh!
  "Explicit index refresh. With :manufacturer, fetch that manufacturer's fixture
  catalog; otherwise fetch manufacturer names/counts. No fixtures are downloaded."
  [manager options]
  (update-cache! manager
    #(if-let [key (:manufacturer options)] (library/refresh-manufacturer %1 %2 key options)
             (library/refresh-manufacturers %1 %2 options))))

(defn fetch!
  "Fetch/cache a fixture by :key and return mode/provenance metadata. Full source
  text remains in the offline cache and is omitted from this UI projection."
  [manager {:keys [key] :as options}]
  (let [result (update-cache! manager #(library/fetch-fixture %1 %2 key options))]
    (update result :fixture dissoc :text :cache-order)))

(defn import-file
  "Pure upload import. Request {:format :ofl|:qxf, :content text} or
  {:format :gdtf, :content-base64 encoded-zip}; optional :mode/:id/:version/
  :source/:license. Returns a candidate and diagnostics, never a show mutation."
  [request]
  (let [format (if (string? (:format request)) (keyword (:format request)) (:format request))
        opts (select-keys request [:mode :id :version :source :license])
        data (if (= :gdtf format)
               (let [encoded (:content-base64 request)]
                 (when-not (bounded-text? encoded (+ 4 (* 4 (quot (+ (* 16 1024 1024) 2) 3))))
                   (fail! "GDTF upload must contain bounded base64 ZIP data" {}))
                 (try (.decode (Base64/getDecoder) ^String encoded)
                      (catch IllegalArgumentException cause
                        (throw (ex-info "Invalid GDTF base64 upload" {:fixtures/error :invalid-upload} cause)))))
               (let [content (:content request)]
                 (when-not (and (string? content) (<= (alength (text-bytes content)) (* 4 1024 1024)))
                   (fail! "Fixture upload must contain at most 4 MiB of text" {})) content))]
    (try (library/import-upload format data opts)
         (catch clojure.lang.ExceptionInfo cause
           (if (and (not (:mode opts)) (vector? (:available (ex-data cause))))
             {:valid? false :definition nil :candidate nil :modes (:available (ex-data cause))
              :diagnostics [{:severity :info :code :mode-required :path [:mode]
                             :message "Choose a fixture mode to preview this import"}]}
             (throw cause))))))

(defn import!
  "Import a cached :key or an explicit upload request. This manager has no show
  reference or command callback, so publication and patching remain separate."
  [manager request]
  (open! manager)
  (if-let [key (:key request)]
    (library/import-fixture @(:state manager) key (select-keys request [:mode :id :version]))
    (import-file request)))

(defn unmapped-diagnostics [channel-definitions covered]
  (for [[index channel] (map-indexed vector channel-definitions)
        :let [physical (or (:offsets channel) (when (:offset channel) [(:offset channel)]))]
        :when (and (some? (:key channel)) (seq physical) (not-every? covered physical))]
    {:severity :error :code :unmapped-required-channel :path [:channel-definitions index]
     :message (str "Map physical channel " (:key channel) " at " (str/join ", " physical)
                   " before patching; its required value cannot be inferred")}))

(defn instantiate
  "Pure fixture-type -> compact rig instance conversion. Returns :fixture only
  when valid, plus :candidate and :diagnostics. Channels, fine offsets, inversion,
  RGBW policy and explicit calibration are preserved. Unmapped physical source
  channels and missing movement calibration are errors, never guessed defaults.
  The host must separately validate collisions against the complete rig, reserving
  each fixture's full :footprint even when some slots are explicitly unused."
  [definition options]
  (let [channels (:channels definition)
        offset-values (fn [channel]
                        (cond (integer? channel) [channel]
                              (vector? channel) channel
                              (map? channel) (remove nil? [(:offset channel) (:fine-offset channel)])
                              :else []))
        offsets (vec (mapcat offset-values (vals channels)))
        covered (set offsets)
        candidate (placement/instance definition options)
        {:keys [footprint calibration]} candidate
        diagnostics (vec
                     (concat
                      (map #(assoc % :severity :error :code :invalid-definition)
                           (registry/validate-definition :fixtures definition))
                      (when-not (or (keyword? (:id options)) (and (string? (:id options)) (not (str/blank? (:id options)))))
                        [{:severity :error :code :missing-instance-id :path [:id] :message "Choose a stable fixture instance ID"}])
                      (when-not (and (integer? (:universe options)) (pos? (:universe options)))
                        [{:severity :error :code :invalid-universe :path [:universe] :message "Universe must be a positive integer"}])
                      (when-not (and (integer? footprint) (pos? footprint)
                                     (every? #(and (integer? %) (<= 1 % footprint)) offsets))
                        [{:severity :error :code :invalid-footprint :path [:footprint] :message "Footprint must include every coarse and fine channel"}])
                      (unmapped-diagnostics (:channel-definitions definition) covered)
                      (for [axis [:pan :tilt] :when (and (contains? channels axis) (nil? (get calibration axis)))]
                        {:severity :error :code :missing-calibration :path [:calibration axis]
                         :message (str "Set the documented " (name axis) " angle range before patching a moving fixture")})
                      (map #(assoc % :severity :error :code :invalid-patch) (bounds/errors candidate))
                      (map #(assoc % :severity :error :code :invalid-patch) (render/validate-patch [candidate]))))
        valid? (not-any? #(= :error (:severity %)) diagnostics)]
    {:valid? valid? :fixture (when valid? candidate) :candidate candidate :diagnostics diagnostics}))

(defn- registry-type [types id]
  (or (get types id)
      (when (string? id) (get types (keyword id)))
      (when (keyword? id) (get types (subs (str id) 1)))))

(defn- wire-id [id] (if (keyword? id) (subs (str id) 1) (str id)))

(defn admit-placements
  "Instantiate fixtures newly placed in a proposed rig from their published type.
  A new fixture (its ID is not in `existing`) that names a registry
  `:fixture-type-id` takes its channels, footprint and type data from that type;
  only `placement/instance-keys` come from the entry. Its channels may differ
  from the type's only by axis reversal. It must pass `instantiate`'s patch
  checks, including documented movement calibration. A new fixture without
  `:channels` must name a registry type. Other entries are returned unchanged."
  [types existing fixtures]
  (let [existing (set (map wire-id existing))]
    (mapv (fn [entry]
            (let [definition (registry-type types (:fixture-type-id entry))]
              (cond
                (or (not (map? entry)) (contains? existing (wire-id (:id entry)))) entry
                (and (nil? definition) (contains? entry :channels)) entry
                (nil? definition)
                (throw (ex-info "Fixture type not found" {:id (:fixture-type-id entry) :fixture-id (:id entry)}))
                (and (contains? entry :channels) (not (placement/same-patch? (:channels definition) (:channels entry))))
                (throw (ex-info "A placed fixture's channels must match its fixture type"
                                {:fixture-id (:id entry) :fixture-type-id (:id definition)}))
                :else
                (let [result (instantiate definition (select-keys entry placement/instance-keys))]
                  (when-not (:valid? result)
                    (throw (ex-info (str "Fixture " (wire-id (:id entry)) " cannot be placed: "
                                         (str/join "; " (keep :message (filter #(= :error (:severity %)) (:diagnostics result)))))
                                    (select-keys result [:diagnostics]))))
                  (cond-> (:fixture result)
                    (contains? entry :channels) (assoc :channels (:channels entry)))))))
          fixtures)))

(defn close! [manager]
  (locking (:lock manager)
    (when (compare-and-set! (:closed? manager) false true)
      (let [provider @(:provider manager)]
        (reset! (:provider manager) nil)
        (when-let [close (:close! (meta provider))] (close)))))
  nil)
