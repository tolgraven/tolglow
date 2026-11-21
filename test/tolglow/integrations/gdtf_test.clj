(ns tolglow.integrations.gdtf-test
  (:require [clojure.test :refer [deftest is]] [clojure.string :as str]
            [tolglow.integrations.gdtf :as gdtf] [tolglow.registry :as registry])
  (:import [java.io ByteArrayOutputStream] [java.util.zip ZipOutputStream ZipEntry]
           [java.nio.charset StandardCharsets]))

(def description
  "<GDTF DataVersion='1.2'><FixtureType Name='Test Head' Manufacturer='Example' FixtureTypeID='test-id'>
    <DMXModes><DMXMode Name='Standard' Geometry='Base'><DMXChannels>
      <DMXChannel Offset='1,5' DMXBreak='1' Geometry='Yoke'><LogicalChannel Attribute='Pan'>
        <ChannelFunction Name='Pan' Attribute='Pan' DMXFrom='0/1' PhysicalFrom='-270' PhysicalTo='270'/>
      </LogicalChannel></DMXChannel>
      <DMXChannel Offset='2' Geometry='Beam'><LogicalChannel Attribute='Dimmer'>
        <ChannelFunction Name='Intensity' Attribute='Dimmer' DMXFrom='0/1'/>
      </LogicalChannel></DMXChannel>
      <DMXChannel Offset='3' Geometry='Beam'><LogicalChannel Attribute='ColorAdd_R'>
        <ChannelFunction Name='Red' Attribute='ColorAdd_R' DMXFrom='0/1'/>
      </LogicalChannel></DMXChannel>
      <DMXChannel Offset='4' Geometry='Beam'><LogicalChannel Attribute='ColorAdd_G'>
        <ChannelFunction Name='Green' Attribute='ColorAdd_G' DMXFrom='0/1'/>
      </LogicalChannel></DMXChannel>
      <DMXChannel Offset='6' Geometry='Beam'><LogicalChannel Attribute='ColorAdd_B'>
        <ChannelFunction Name='Blue' Attribute='ColorAdd_B' DMXFrom='0/1'/>
      </LogicalChannel></DMXChannel>
      <DMXChannel Offset='7' Geometry='Base'><LogicalChannel Attribute='Control'>
        <ChannelFunction Name='No action' Attribute='Control' DMXFrom='0/1'/>
        <ChannelFunction Name='Reset' Attribute='Control' DMXFrom='200/1'>
          <ChannelSet Name='Reset all' DMXFrom='200/1'/><ChannelSet Name='Factory reset' DMXFrom='240/1'/>
        </ChannelFunction>
      </LogicalChannel></DMXChannel>
    </DMXChannels></DMXMode></DMXModes>
    <Models><Model Name='Housing' File='housing'/></Models>
  </FixtureType></GDTF>")

(defn zip-bytes [entries]
  (let [out (ByteArrayOutputStream.)]
    (with-open [zip (ZipOutputStream. out)]
      (doseq [[name content] entries]
        (.putNextEntry zip (ZipEntry. name))
        (.write zip (.getBytes ^String content StandardCharsets/UTF_8))
        (.closeEntry zip)))
    (.toByteArray out)))

(deftest ordinary-mode-preserves-sparse-fine-offsets-and-capabilities
  (let [result (gdtf/import-xml description {:id :head :source "upload:head.gdtf" :license "User supplied"})
        fixture (:definition result)
        reset (last (:channel-definitions fixture))]
    (is (:valid? result))
    (is (= {:pan [1 5] :dimmer 2 :red 3 :green 4 :blue 6} (:channels fixture)))
    (is (= 7 (:footprint fixture)))
    (is (= 16 (:resolution (first (:channel-definitions fixture)))))
    (is (= [0 65535] ((juxt :start :end) (get-in fixture [:channel-definitions 0 :capabilities 0]))))
    (is (= "-270" (get-in fixture [:channel-definitions 0 :capabilities 0 :physical-from])))
    (is (= [[0 199] [200 255]] (mapv (juxt :start :end) (:capabilities reset))))
    (is (= [[200 239] [240 255]] (mapv (juxt :start :end) (get-in reset [:capabilities 1 :channel-sets]))))
    (is (= description (get-in fixture [:source-data :description-xml])))
    (is (= :unverified (:calibration-status fixture)))
    (is (= "User supplied" (get-in fixture [:provenance :license])))
    (is (some #(= :unmapped-channel (:code %)) (:diagnostics result)))
    (is (empty? (registry/validate-definition :fixtures fixture)))))

(deftest dmx-values-obey-gdtf-mirroring-and-shifting
  (is (= 65535 (gdtf/dmx-value "255/1" 2)))
  (is (= 65280 (gdtf/dmx-value "255/1s" 2)))
  (is (= 32896 (gdtf/dmx-value "128/1" 2)))
  (is (= 255 (gdtf/dmx-value "65535/2" 1)))
  (is (= 0 (gdtf/dmx-value "0/1" 2)))
  (doseq [value ["256/1" "-1/1" "0/0" "NaN" "4" "4/5"]]
    (is (thrown? clojure.lang.ExceptionInfo (gdtf/dmx-value value 2)))))

(deftest archive-import-retains-inventory-and-content-checksum
  (let [data (zip-bytes [["description.xml" description] ["models/gltf/housing.glb" "opaque asset"]])
        result (gdtf/import-gdtf data {:source "upload:fixture.gdtf"})]
    (is (:valid? result))
    (is (= :archive-bytes (get-in result [:provenance :checksum-kind])))
    (is (= 64 (count (get-in result [:provenance :sha256]))))
    (is (= (:provenance result) (get-in result [:definition :provenance])))
    (is (= ["description.xml" "models/gltf/housing.glb"]
           (mapv :name (get-in result [:definition :source-data :archive-entries]))))
    (is (some #(= :unknown-license (:code %)) (:diagnostics result)))))

(deftest unsupported-semantics-produce-candidates-not-publishable-definitions
  (doseq [[xml code] [[(str/replace description "Offset='1,5'" "Offset='1,5,8'") :unsupported-resolution]
                      [(str/replace description "DMXBreak='1'" "DMXBreak='2'") :multiple-breaks]
                      [(str/replace description "Offset='6'" "Offset='2'") :overlapping-offsets]
                      [(str/replace description "Offset='6'" "Offset='None'") :virtual-channel]
                      [(str/replace description "Name='Intensity'" "Name='Intensity' ModeMaster='Other'") :conditional-function]
                      [(str/replace description "</FixtureType>" "<Geometries><GeometryReference Name='copy'/></Geometries></FixtureType>") :geometry-reference]
                      [(str/replace description "</DMXMode>" "<Relations><Relation Name='dependent'/></Relations></DMXMode>") :channel-relations]
                      [(str/replace description "DataVersion='1.2'" "DataVersion='9.0'") :unsupported-version]
                      [(str/replace description "Manufacturer='Example'" "Manufacturer=''") :missing-metadata]]]
    (let [result (gdtf/import-xml xml)]
      (is (false? (:valid? result)))
      (is (nil? (:definition result)))
      (is (:candidate result))
      (is (some #(= code (:code %)) (:diagnostics result))))))

(deftest explicit-mode-selection-and-invalid-ranges
  (let [xml (str/replace description "</DMXModes>" "<DMXMode Name='Empty'><DMXChannels/></DMXMode></DMXModes>")]
    (is (thrown? clojure.lang.ExceptionInfo (gdtf/import-xml xml)))
    (is (:valid? (gdtf/import-xml xml {:mode "Standard"})))
    (is (= ["Standard" "Empty"] (:modes (gdtf/import-xml xml {:mode "Standard"}))))
    (is (thrown? clojure.lang.ExceptionInfo (gdtf/import-xml xml {:mode "Absent"}))))
  (is (thrown? clojure.lang.ExceptionInfo (gdtf/import-xml (str/replace description "DMXFrom='200/1'" "DMXFrom='0/1'")))))

(deftest duplicate-archive-members-and-outside-parent-channel-sets-are-rejected
  (let [data (zip-bytes [["description.xml" description] ["descriptioN.xml" description]])
        duplicate (.getBytes (str/replace (String. data StandardCharsets/ISO_8859_1) "descriptioN.xml" "description.xml")
                             StandardCharsets/ISO_8859_1)]
    (is (thrown? clojure.lang.ExceptionInfo (gdtf/import-gdtf duplicate))))
  (is (thrown? clojure.lang.ExceptionInfo
               (gdtf/import-xml (str/replace description "Name='Reset all' DMXFrom='200/1'" "Name='Reset all' DMXFrom='100/1'")))))

(deftest bounded-archive-and-xml-reject-malicious-input
  (doseq [entries [[["../description.xml" description]]
                    [["/description.xml" description]]
                    [["models\\evil.glb" "x"] ["description.xml" description]]
                    [["other.xml" description]]]]
    (is (thrown? clojure.lang.ExceptionInfo (gdtf/import-gdtf (zip-bytes entries)))))
  (doseq [limits [{:max-archive-bytes 10} {:max-description-bytes 10}
                  {:max-uncompressed-bytes 50} {:max-entries 1}]]
    (is (thrown? clojure.lang.ExceptionInfo
                 (gdtf/import-gdtf (zip-bytes [["description.xml" description] ["models/padding" (apply str (repeat 10000 "x"))]])
                                   {:limits limits}))))
  (is (thrown? Exception (gdtf/import-xml "<!DOCTYPE x [<!ENTITY x SYSTEM 'file:///etc/passwd'>]><GDTF>&x;</GDTF>")))
  (is (thrown? clojure.lang.ExceptionInfo (gdtf/import-xml description {:limits {:max-xml-nodes 3}})))
  (is (thrown? clojure.lang.ExceptionInfo (gdtf/import-xml description {:limits {:max-xml-depth 2}}))))
