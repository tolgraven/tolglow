(ns tolglow.integrations.fixtures
  "Data-only fixture import. Supports ordinary OFL modes and QLC+ QXF modes.
  Matrix expansion, GDTF/MVR, calibration inference and remote search are separate.
  Unsupported fields and the original source remain available to the editor."
  (:require [clojure.data.json :as json]
            [clojure.string :as str]
            [tolglow.registry :as registry])
  (:import [java.io StringReader]
           [java.nio.charset StandardCharsets]
           [java.security MessageDigest]
           [javax.xml.parsers DocumentBuilderFactory]
           [org.w3c.dom Element Node]
           [org.xml.sax ErrorHandler InputSource]))

(defn- field [m k]
  (let [m (if (sorted? m) (into {} m) m)]
    (if (contains? m k) (get m k) (get m (name k)))))
(defn- label [x] (if (keyword? x) (name x) (str x)))
(defn- diagnostic [severity code path message]
  {:severity severity :code code :path path :message message})
(defn- sha256 [text]
  (apply str (map #(format "%02x" (bit-and % 255))
                  (.digest (MessageDigest/getInstance "SHA-256")
                           (.getBytes ^String text StandardCharsets/UTF_8)))))
(defn- canonical [x]
  (cond
    (map? x) (into (sorted-map) (map (fn [[k v]] [(label k) (canonical v)])) x)
    (sequential? x) (mapv canonical x)
    :else x))
(defn- slug [x]
  (-> (str/lower-case (str x)) (str/replace #"[^a-z0-9]+" "-")
      (str/replace #"^-|-$" "")))

(defn- select-mode [modes requested get-name]
  (cond
    requested (or (first (filter #(= (str requested) (str (get-name %))) modes))
                  (throw (ex-info "Requested fixture mode is absent"
                                  {:requested requested :available (mapv get-name modes)})))
    (= 1 (count modes)) (first modes)
    :else (throw (ex-info "Select an explicit fixture mode"
                         {:available (mapv get-name modes)}))))

(defn- finish [metadata channels diagnostics source-text source-data opts]
  (let [groups (group-by :attribute (filter :attribute channels))
        duplicates (for [[attribute xs] groups
                         :when (or (not= 1 (count (remove :fine? xs)))
                                   (> (count (filter :fine? xs)) 1))]
                     (diagnostic :error :ambiguous-channel [:channels attribute]
                                 "Attribute needs exactly one coarse and at most one fine channel; pixel/matrix modes need explicit heads"))
        normalized (into {} (for [[attribute xs] groups
                                 :let [coarse (first (remove :fine? xs))
                                       fine (first (filter :fine? xs))]
                                 :when coarse]
                             [attribute (if fine [(:offset coarse) (:offset fine)] (:offset coarse))]))
        diagnostics (into (vec diagnostics) duplicates)
        provenance {:format (:format metadata) :source (:source opts)
                    :sha256 (sha256 source-text) :license (:license opts)
                    :checksum-kind (get opts :checksum-kind (if (string? source-data) :source-text :canonical-json))}
        definition (-> metadata
                       (dissoc :format)
                       (assoc :id (or (:id opts) (slug (str (:manufacturer metadata) "-" (:name metadata) "-" (:mode metadata))))
                              :version (get opts :version 1) :channels normalized
                              :channel-definitions (vec channels)
                              :provenance provenance
                              :source-data source-data
                              :geometry-status :unverified
                              :calibration-status :unverified))
        diagnostics (into diagnostics
                          (map #(diagnostic :error :invalid-definition (:path %) (:message %)))
                          (registry/validate-definition :fixtures definition))]
    {:valid? (not-any? #(= :error (:severity %)) diagnostics)
     :definition (when-not (some #(= :error (:severity %)) diagnostics) definition)
     :candidate definition :diagnostics diagnostics :provenance provenance}))

(defn- ofl-attribute [definition]
  (let [cap (field definition :capability)
        kind (field cap :type)]
    (case kind
      "ColorIntensity" (get {"Red" :red "Green" :green "Blue" :blue "White" :white
                              "Amber" :amber "UV" :uv "Warm White" :warm-white "Cold White" :cold-white}
                             (field cap :color))
      "Intensity" :dimmer "Pan" :pan "Tilt" :tilt "Focus" :focus "Zoom" :zoom
      "Iris" :iris "Fog" :fog nil)))

(defn- ofl-capabilities [definition]
  (mapv (fn [cap]
          (let [[start end] (or (field cap :dmxRange) [0 255])]
            {:start start :end end :type (field cap :type)
             :label (or (field cap :comment) (field cap :type)) :source-data cap}))
        (or (field definition :capabilities)
            (when-let [cap (field definition :capability)] [cap]) [])))

(defn import-ofl
  "Import an OFL JSON string or parsed map. opts accepts :mode, :id, :version,
  :manufacturer, :source, :license. Multi-mode files require :mode. Complex mode
  insert blocks and >16-bit fine channels return invalid candidates, never silently
  patchable definitions. All unsupported source fields are retained in :source-data."
  ([data] (import-ofl data {}))
  ([data opts]
   (let [raw (if (string? data) (json/read-str data) data)
         _ (when-not (map? raw) (throw (ex-info "OFL input must be a JSON object" {})))
         available (into {} (map (fn [[k v]] [(label k) v])) (field raw :availableChannels))
         modes (field raw :modes)
         _ (when-not (and (string? (field raw :name)) (not (str/blank? (field raw :name)))
                          (map? (field raw :availableChannels)) (sequential? modes) (seq modes))
             (throw (ex-info "OFL needs a name, availableChannels and nonempty modes" {})))
         mode (select-mode modes (:mode opts) #(field % :name))
         _ (when-not (and (string? (field mode :name)) (vector? (field mode :channels)) (seq (field mode :channels)))
             (throw (ex-info "OFL mode needs a name and nonempty channel vector" {})))
         aliases (into {} (for [[name definition] available
                               [index alias] (map-indexed vector (field definition :fineChannelAliases))]
                           [(label alias) {:coarse name :byte (inc index)}]))
         entries (mapv (fn [index channel-name]
                         (let [alias (get aliases channel-name)
                               definition (get available (or (:coarse alias) channel-name))]
                           {:offset (inc index) :key channel-name
                            :attribute (when definition (ofl-attribute definition))
                            :fine? (boolean alias) :fine-byte (:byte alias)
                            :coarse-key (:coarse alias)
                            :capabilities (if definition (ofl-capabilities definition) [])
                            :source-data definition}))
                       (range) (field mode :channels))
         diagnostics (vec
                      (mapcat (fn [{:keys [key offset attribute fine-byte source-data]}]
                                (cond
                                  (nil? key) []
                                  (map? key) [(diagnostic :error :matrix-mode [:mode :channels (dec offset)]
                                                          "OFL matrix/insert expansion is not supported yet")]
                                  (nil? source-data) [(diagnostic :error :missing-channel [:mode :channels (dec offset)]
                                                                  "Mode references an undefined channel")]
                                  (and fine-byte (> fine-byte 1)) [(diagnostic :error :fine-resolution [:channels key]
                                                                                             "Only coarse plus one fine byte is supported")]
                                  (nil? attribute) [(diagnostic :warning :unmapped-channel [:channels key]
                                                               "Channel preserved as data; no generic renderer attribute inferred")]
                                  :else [])) entries))]
     (finish {:format :ofl :name (field raw :name)
              :manufacturer (or (:manufacturer opts) (field raw :manufacturer))
              :mode (field mode :name) :footprint (count entries)
              :physical (or (field mode :physical) (field raw :physical))}
             entries diagnostics (if (string? data) data (json/write-str (canonical data)))
             raw (assoc opts :checksum-kind (if (string? data) :source-text :canonical-json))))))

(defn- children [^Node node]
  (let [nodes (.getChildNodes node)]
    (filterv #(= Node/ELEMENT_NODE (.getNodeType ^Node %))
             (mapv #(.item nodes %) (range (.getLength nodes))))))
(defn- tag [^Node node] (or (.getLocalName node) (.getNodeName node)))
(defn- elements [node name] (filterv #(= name (tag %)) (children node)))
(defn- text-at [node name] (some-> (first (elements node name)) (.getTextContent) str/trim))
(defn- attr [^Element node name] (.getAttribute node name))
(defn- parse-xml [text]
  (let [factory (DocumentBuilderFactory/newInstance)]
    (.setNamespaceAware factory true)
    (.setXIncludeAware factory false)
    (.setExpandEntityReferences factory false)
    (.setFeature factory "http://apache.org/xml/features/disallow-doctype-decl" true)
    (.setFeature factory "http://xml.org/sax/features/external-general-entities" false)
    (.setFeature factory "http://xml.org/sax/features/external-parameter-entities" false)
    (let [builder (.newDocumentBuilder factory)]
      (.setErrorHandler builder (reify ErrorHandler
                                  (warning [_ _])
                                  (error [_ exception] (throw exception))
                                  (fatalError [_ exception] (throw exception))))
      (.getDocumentElement (.parse builder (InputSource. (StringReader. text)))))))

(defn- qxf-attribute [node]
  (let [group (text-at node "Group") color (text-at node "Colour")]
    (if (= group "Colour")
      (get {"Red" :red "Green" :green "Blue" :blue "White" :white "Amber" :amber "UV" :uv} color)
      (get {"Intensity" :dimmer "Pan" :pan "Tilt" :tilt "Focus" :focus "Zoom" :zoom
            "Iris" :iris} group))))

(defn import-qxf
  "Import a QLC+ QXF XML string using a parser that rejects DTDs/entities.
  opts accepts :mode, :id, :version, :source and :license. QXF channel numbers
  are zero-based; canonical offsets are one-based. Missing geometry/calibration
  remain explicitly unverified. Capability presets are preserved, not executed."
  ([xml] (import-qxf xml {}))
  ([xml opts]
   (let [root (parse-xml xml)
         _ (when-not (= "FixtureDefinition" (tag root))
             (throw (ex-info "Expected QXF FixtureDefinition" {})))
         available (into {} (map (fn [node] [(attr node "Name") node])) (elements root "Channel"))
         mode (select-mode (elements root "Mode") (:mode opts) #(attr % "Name"))
         entries (mapv
                  (fn [entry]
                    (let [key (str/trim (.getTextContent ^Node entry))
                          definition (get available key)
                          group (when definition (first (elements definition "Group")))
                          fine-byte (if group (Long/parseLong (let [b (attr group "Byte")] (if (empty? b) "0" b))) 0)]
                      {:key key :offset (inc (Long/parseLong (attr entry "Number")))
                       :attribute (when definition (qxf-attribute definition))
                       :fine? (pos? fine-byte) :fine-byte fine-byte
                       :capabilities (mapv (fn [cap]
                                             {:start (Long/parseLong (attr cap "Min"))
                                              :end (Long/parseLong (attr cap "Max"))
                                              :label (str/trim (.getTextContent ^Node cap))
                                              :preset (attr cap "Preset")})
                                           (if definition (elements definition "Capability") []))
                       :defined? (boolean definition)}))
                  (elements mode "Channel"))
         offsets (mapv :offset entries)
         diagnostics (vec
                      (concat
                       (when (or (some #(not (pos? %)) offsets)
                                 (not= (count offsets) (count (distinct offsets))))
                         [(diagnostic :error :invalid-offsets [:mode] "Mode channel offsets must be distinct and nonnegative in QXF")])
                       (mapcat (fn [{:keys [key attribute defined? fine-byte capabilities]}]
                                 (concat
                                  (when-not defined? [(diagnostic :error :missing-channel [:channels key] "Mode references an undefined channel")])
                                  (when (and defined? (nil? attribute)) [(diagnostic :warning :unmapped-channel [:channels key]
                                                                                                  "Channel preserved without inferring renderer semantics")])
                                  (when (> fine-byte 1) [(diagnostic :error :fine-resolution [:channels key] "Only 16-bit coarse/fine pairs supported")])
                                  (when (some #(not (<= 0 (:start %) (:end %) 255)) capabilities)
                                    [(diagnostic :error :invalid-capability [:channels key] "Capability ranges must fit 0..255")])) ) entries)))]
     (finish {:format :qxf :name (text-at root "Model") :manufacturer (text-at root "Manufacturer")
              :mode (attr mode "Name") :footprint (reduce max 0 offsets)}
             (mapv #(dissoc % :defined?) entries) diagnostics xml xml opts))))
