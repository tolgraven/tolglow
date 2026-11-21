(ns tolglow.integrations.gdtf
  "Bounded GDTF 1.1/1.2 description import, with no archive extraction or IO.
  Ordinary single-break coarse/fine modes are supported. Other semantics remain
  source data plus diagnostics; this is not a complete GDTF/MVR renderer."
  (:refer-clojure :exclude [descendants])
  (:require [clojure.string :as str] [tolglow.registry :as registry])
  (:import [java.io ByteArrayInputStream ByteArrayOutputStream StringReader]
           [java.nio.charset StandardCharsets]
           [java.security MessageDigest]
           [java.util.zip ZipInputStream]
           [javax.xml XMLConstants]
           [javax.xml.parsers DocumentBuilderFactory]
           [org.w3c.dom Element Node]
           [org.xml.sax ErrorHandler InputSource]))

(def specification "https://gdtf-development.com/help/developers/gdtf_1_2/file-format-definition/index.html")
(def default-limits {:max-archive-bytes (* 16 1024 1024) :max-uncompressed-bytes (* 32 1024 1024)
                     :max-description-bytes (* 4 1024 1024) :max-entries 256
                     :max-xml-nodes 50000 :max-xml-depth 64})

(defn- fail! [message data] (throw (ex-info message data)))
(defn- diagnostic [severity code path message] {:severity severity :code code :path path :message message})
(defn- byte-array? [x] (= (class x) (Class/forName "[B")))
(defn- sha256 [data]
  (apply str (map #(format "%02x" (bit-and % 255)) (.digest (MessageDigest/getInstance "SHA-256") ^bytes data))))
(defn- limits [opts]
  (let [result (merge default-limits (:limits opts))]
    (doseq [[key value] result]
      (when-not (and (integer? value) (pos? value)) (fail! "GDTF limits must be positive integers" {:key key}))) result))

(defn- archive [data limits]
  (when-not (and (byte-array? data) (<= (alength ^bytes data) (:max-archive-bytes limits)))
    (fail! "GDTF archive must be a bounded byte array" {:limit (:max-archive-bytes limits)}))
  (with-open [zip (ZipInputStream. (ByteArrayInputStream. data))]
    (let [buffer (byte-array 8192)]
      (loop [entries [] seen #{} total 0 description nil]
        (if-let [entry (.getNextEntry zip)]
          (let [name (.getName entry)
                _ (when (or (>= (count entries) (:max-entries limits))
                            (seen name) (str/starts-with? name "/") (str/includes? name "\\")
                            (str/includes? name ":") (some #{".." "."} (str/split name #"/")))
                    (fail! "Unsafe, duplicate or excessive GDTF archive entries" {:entry name}))
                description? (= "description.xml" name)
                output (when description? (ByteArrayOutputStream.))
                size (loop [size 0]
                       (let [n (.read zip buffer)]
                         (if (= -1 n) size
                             (let [size (+ size n)]
                               (when (or (> (+ total size) (:max-uncompressed-bytes limits))
                                         (and description? (> size (:max-description-bytes limits))))
                                 (fail! "GDTF archive exceeds expanded byte limit" {:entry name :bytes (+ total size)}))
                               (when output (.write output buffer 0 n))
                               (recur size)))))]
            (.closeEntry zip)
            (recur (conj entries {:name name :byte-count size}) (conj seen name) (+ total size)
                   (if output (.toString output "UTF-8") description)))
          (do (when-not description (fail! "GDTF archive needs root description.xml" {}))
              {:description description :entries entries :byte-count total}))))))

(defn- children [^Node node]
  (if-not node [] (let [nodes (.getChildNodes node)]
    (filterv #(= Node/ELEMENT_NODE (.getNodeType ^Node %))
             (mapv #(.item nodes %) (range (.getLength nodes)))))))
(defn- tag [^Node node] (or (.getLocalName node) (.getNodeName node)))
(defn- elements [node name] (filterv #(= name (tag %)) (children node)))
(defn- at [node & names] (reduce #(first (elements %1 %2)) node names))
(defn- attr
  ([node key] (attr node key nil))
  ([^Element node key default] (if (and node (.hasAttribute node key)) (.getAttribute node key) default)))
(defn- attributes [^Element node]
  (let [attributes (.getAttributes node)]
    (into {} (map (fn [i] (let [item (.item attributes i)] [(.getNodeName item) (.getNodeValue item)])))
          (range (.getLength attributes)))))
(defn- node-data [node]
  {:tag (tag node) :attributes (attributes node) :children (mapv node-data (children node))})
(defn- descendants [node] (tree-seq #(seq (children %)) children node))

(defn- parse-xml [text limits]
  (when-not (and (string? text)
                 (<= (alength (.getBytes ^String text StandardCharsets/UTF_8)) (:max-description-bytes limits)))
    (fail! "GDTF description exceeds byte limit" {}))
  (let [factory (DocumentBuilderFactory/newInstance)]
    (.setNamespaceAware factory true)
    (.setXIncludeAware factory false)
    (.setExpandEntityReferences factory false)
    (.setFeature factory XMLConstants/FEATURE_SECURE_PROCESSING true)
    (.setFeature factory "http://apache.org/xml/features/disallow-doctype-decl" true)
    (.setFeature factory "http://xml.org/sax/features/external-general-entities" false)
    (.setFeature factory "http://xml.org/sax/features/external-parameter-entities" false)
    (.setAttribute factory XMLConstants/ACCESS_EXTERNAL_DTD "")
    (.setAttribute factory XMLConstants/ACCESS_EXTERNAL_SCHEMA "")
    (let [builder (.newDocumentBuilder factory)]
      (.setErrorHandler builder (reify ErrorHandler (warning [_ _])
                                  (error [_ exception] (throw exception)) (fatalError [_ exception] (throw exception))))
      (let [root (.getDocumentElement (.parse builder (InputSource. (StringReader. text))))]
        (loop [pending [[root 0]] count 0]
          (when-let [[node depth] (peek pending)]
            (when (or (>= count (:max-xml-nodes limits)) (> depth (:max-xml-depth limits)))
              (fail! "GDTF XML exceeds structural limits" {:depth depth :nodes count}))
            (recur (into (pop pending) (map #(vector % (inc depth)) (children node))) (inc count))))
        root))))

(defn dmx-value
  "Resolve GDTF Uint/n byte-mirroring or Uint/ns byte-shifting into the target
  byte count. Eg 255/1 ->65535 and 255/1s ->65280 on a 16-bit channel."
  [text byte-count]
  (let [[_ value source-count shift?] (when (string? text) (re-matches #"([0-9]+)/([1-4])(s?)" text))]
    (when-not (and value (integer? byte-count) (<= 1 byte-count 4))
      (fail! "Invalid GDTF DMX value" {:value text :byte-count byte-count}))
    (let [value (bigint value) source-count (Long/parseLong source-count)
          source-max (dec (bit-shift-left 1 (* 8 source-count)))
          target-max (dec (bit-shift-left 1 (* 8 byte-count)))]
      (when (> value source-max) (fail! "GDTF DMX value exceeds declared resolution" {:value text}))
      (long (if (= "s" shift?)
              (if (>= byte-count source-count) (* value (bit-shift-left 1 (* 8 (- byte-count source-count))))
                  (quot value (bit-shift-left 1 (* 8 (- source-count byte-count)))))
              (quot (* value target-max) source-max))))))

(def ^:private attribute-map
  {"Dimmer" :dimmer "Pan" :pan "Tilt" :tilt "Focus" :focus "Zoom" :zoom "Iris" :iris
   "ColorAdd_R" :red "ColorAdd_G" :green "ColorAdd_B" :blue "ColorAdd_W" :white
   "ColorAdd_A" :amber "ColorAdd_UV" :uv "Fog1" :fog})

(defn- capabilities
  ([nodes byte-count maximum] (capabilities nodes byte-count maximum 0))
  ([nodes byte-count maximum minimum]
  (let [starts (mapv #(dmx-value (attr % "DMXFrom" "0/1") byte-count) nodes)]
    (when (and (seq starts) (or (< (first starts) minimum) (> (last starts) maximum)
                               (and (> (count starts) 1) (not (apply < starts)))))
      (fail! "GDTF function ranges must increase within the channel" {:starts starts :maximum maximum}))
    (mapv (fn [node start end]
            {:start start :end end :label (attr node "Name" "") :attribute (attr node "Attribute")
             :physical-from (attr node "PhysicalFrom") :physical-to (attr node "PhysicalTo")
             :channel-sets (capabilities (elements node "ChannelSet") byte-count end start)
             :source-data (node-data node)}) nodes starts (concat (map dec (rest starts)) [maximum])))))

(defn- channel [node index]
  (let [raw-offset (attr node "Offset" "None")
        offsets (if (= "None" raw-offset) []
                    (try (mapv #(Long/parseLong (str/trim %)) (str/split raw-offset #","))
                         (catch NumberFormatException _ (fail! "Invalid GDTF channel offsets" {:offset raw-offset}))))
        logical (elements node "LogicalChannel")
        functions (vec (mapcat #(elements % "ChannelFunction") logical))
        attributes (set (concat (map #(attr % "Attribute") logical) (map #(attr % "Attribute") functions)))
        attributes (disj attributes nil)
        attribute (when (= 1 (count attributes)) (get attribute-map (first attributes)))
        byte-count (max 1 (count offsets))
        path [:channels index]
        diagnostics (vec (concat
                           (when (empty? offsets) [(diagnostic :error :virtual-channel path "Virtual channels require relation-aware evaluation")])
                           (when (or (> (count offsets) 2) (some #(not (<= 1 % 512)) offsets))
                             [(diagnostic :error :unsupported-resolution path "Only 8/16-bit offsets within one 512-channel break are supported")])
                           (when-not (= "1" (attr node "DMXBreak" "1"))
                             [(diagnostic :error :multiple-breaks path "Multiple/overwritten DMX breaks require explicit geometry expansion")])
                           (when (or (> (count logical) 1) (> (count attributes) 1))
                             [(diagnostic :error :multiplexed-channel path "Multiple logical attributes on one channel need explicit function selection")])
                           (when (some #(or (attr % "ModeMaster") (attr % "ModeFrom") (attr % "ModeTo")) functions)
                             [(diagnostic :error :conditional-function path "Mode-dependent channel functions are preserved but not evaluated")])
                           (when (nil? attribute) [(diagnostic :warning :unmapped-channel path "GDTF attribute retained without generic renderer mapping")])
                           (when (> (count functions) 1)
                             [(diagnostic :warning :function-ranges path "Capability ranges are retained; generic output does not select named functions")])))]
    {:key (str (attr node "Geometry" "") "_" (or (first (sort attributes)) index))
     :offsets offsets :attribute attribute :geometry (attr node "Geometry")
     :dmx-break (attr node "DMXBreak" "1") :resolution (* 8 byte-count)
     :default (when-let [value (attr node "Default")] (dmx-value value byte-count))
     :capabilities (if (<= byte-count 4) (capabilities functions byte-count (dec (bit-shift-left 1 (* 8 byte-count)))) [])
     :source-data (node-data node) :diagnostics diagnostics}))

(defn import-xml
  "Import a bounded GDTF description.xml string. Multi-mode sources require
  :mode. :source/:license are caller-supplied provenance; no license is inferred.
  Errors suppress :definition while preserving :candidate and diagnostics."
  ([xml] (import-xml xml {}))
  ([xml opts]
   (let [root (parse-xml xml (limits opts))
         _ (when-not (= "GDTF" (tag root)) (fail! "Expected GDTF XML root" {}))
         fixture (at root "FixtureType")
         _ (when-not fixture (fail! "GDTF needs FixtureType" {}))
         modes (elements (at fixture "DMXModes") "DMXMode")
         available (mapv #(attr % "Name") modes)
         mode (if-let [requested (:mode opts)]
                (some #(when (= requested (attr % "Name")) %) modes)
                (when (= 1 (count modes)) (first modes)))
         _ (when-not mode (fail! "Select an available GDTF mode" {:requested (:mode opts) :available available}))
         channels (mapv channel (elements (at mode "DMXChannels") "DMXChannel") (range))
         all-offsets (mapcat :offsets channels)
         mapped (filter :attribute channels)
         duplicate-attrs (->> mapped (group-by :attribute) (filter #(> (count (val %)) 1)) (map key))
         tags (set (map tag (descendants fixture)))
         diagnostics (vec (concat
                           (mapcat :diagnostics channels)
                           (for [field ["Name" "Manufacturer"] :when (str/blank? (attr fixture field))]
                             (diagnostic :error :missing-metadata [:fixture field] "Fixture name and manufacturer are required"))
                           (when (str/blank? (attr mode "Name"))
                             [(diagnostic :error :missing-mode-name [:mode] "Mode requires a name")])
                           (when-not (#{"1.1" "1.2"} (attr root "DataVersion"))
                             [(diagnostic :error :unsupported-version [] "Only GDTF 1.1/1.2 Offset-based descriptions are supported")])
                           (when-not (seq channels) [(diagnostic :error :empty-mode [:mode] "Mode must contain channels")])
                           (when (not= (count all-offsets) (count (distinct all-offsets)))
                             [(diagnostic :error :overlapping-offsets [:channels] "Physical channel offsets overlap")])
                           (map #(diagnostic :error :ambiguous-attribute [:channels %] "Repeated attributes require explicit pixel/head geometry") duplicate-attrs)
                           (when (tags "GeometryReference")
                             [(diagnostic :error :geometry-reference [:geometry] "Geometry reference/break expansion is not supported")])
                           (when (seq (elements (at mode "Relations") "Relation"))
                             [(diagnostic :error :channel-relations [:mode :relations] "Linked channel relations require explicit evaluator support")])
                           (when (or (tags "Models") (tags "Wheels") (tags "PhysicalDescriptions"))
                             [(diagnostic :warning :assets-not-rendered [:source-data] "Assets, wheels and physical metadata are retained, not rendered or calibrated")])
                           (when (nil? (:license opts))
                             [(diagnostic :warning :unknown-license [:provenance :license] "Source license is unknown; none is inferred from the GDTF format")])) )
         provenance {:format :gdtf :source (:source opts) :license (:license opts)
                     :sha256 (sha256 (.getBytes ^String xml StandardCharsets/UTF_8)) :checksum-kind :description-xml
                     :specification specification :data-version (attr root "DataVersion")}
         slug (-> (str (attr fixture "Manufacturer") "-" (attr fixture "Name") "-" (attr mode "Name"))
                  str/lower-case (str/replace #"[^a-z0-9]+" "-") (str/replace #"^-|-$" ""))
         definition {:id (:id opts slug) :version (:version opts 1) :name (attr fixture "Name")
                     :manufacturer (attr fixture "Manufacturer") :mode (attr mode "Name")
                     :fixture-type-id (attr fixture "FixtureTypeID") :footprint (reduce max 0 all-offsets)
                     :channels (into {} (for [{:keys [attribute offsets]} mapped :when (seq offsets)]
                                          [attribute (if (= 1 (count offsets)) (first offsets) (vec offsets))]))
                     :channel-definitions (mapv #(dissoc % :diagnostics) channels)
                     :provenance provenance :source-data {:description-xml xml}
                     :geometry-status :unverified :calibration-status :unverified}
         diagnostics (into diagnostics (map #(diagnostic :error :invalid-definition (:path %) (:message %)))
                           (registry/validate-definition :fixtures definition))
         valid? (not-any? #(= :error (:severity %)) diagnostics)]
     {:valid? valid? :definition (when valid? definition) :candidate definition
      :diagnostics diagnostics :provenance provenance :modes available})))

(defn import-gdtf
  "Import .gdtf ZIP bytes without extracting any file. Bounds compressed and
  expanded sizes, XML structure and archive entry count. Full source XML and an
  asset inventory are retained; model/image payloads are not loaded or executed."
  ([data] (import-gdtf data {}))
  ([data opts]
   (let [{:keys [description entries]} (archive data (limits opts))
         result (import-xml description opts)
         provenance (assoc (:provenance result) :sha256 (sha256 data) :checksum-kind :archive-bytes)
         enrich #(-> % (assoc :provenance provenance) (assoc-in [:source-data :archive-entries] entries))]
     (cond-> (-> result (assoc :provenance provenance) (update :candidate enrich))
       (:definition result) (update :definition enrich)))))
