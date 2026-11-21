(ns tolglow.integrations.fixtures-test
  (:require [clojure.test :refer :all]
            [clojure.data.json :as json]
            [tolglow.integrations.fixtures :as fixtures]
            [tolglow.registry :as registry]))

(def ofl
  {:name "RGBW head"
   :availableChannels {"Pan" {:fineChannelAliases ["Pan fine"] :capability {:type "Pan"}}
                       "Red" {:capability {:type "ColorIntensity" :color "Red"}}
                       "Green" {:capability {:type "ColorIntensity" :color "Green"}}
                       "Blue" {:capability {:type "ColorIntensity" :color "Blue"}}
                       "White" {:capability {:type "ColorIntensity" :color "White"}}
                       "Reset" {:capabilities [{:dmxRange [0 199] :type "NoFunction"}
                                                {:dmxRange [200 255] :type "Maintenance"}]}}
   :modes [{:name "7ch" :channels ["Pan" "Pan fine" "Red" "Green" "Blue" "White" "Reset"]}]
   :unrecognized-extension {:retained true}})

(deftest ofl-preserves-mode-fine-channels-and-provenance
  (let [result (fixtures/import-ofl ofl {:id :head :manufacturer "Example" :source "local:fixture.json"})
        definition (:definition result)]
    (is (:valid? result))
    (is (= {:pan [1 2] :red 3 :green 4 :blue 5 :white 6} (:channels definition)))
    (is (= 7 (:footprint definition)))
    (is (= :unverified (:geometry-status definition)))
    (is (= ofl (:source-data definition)))
    (is (= :unmapped-channel (-> result :diagnostics first :code)))
    (is (= [200 255] ((juxt :start :end) (second (:capabilities (last (:channel-definitions definition)))))))
    (is (= 64 (count (get-in result [:provenance :sha256]))))
    (is (empty? (registry/validate-definition :fixtures definition)))))

(deftest json-and-map-imports-use-the-same-channel-semantics
  (is (= (get-in (fixtures/import-ofl ofl) [:definition :channels])
         (get-in (fixtures/import-ofl (json/write-str ofl)) [:definition :channels])))
  (is (= :source-text (get-in (fixtures/import-ofl (json/write-str ofl)) [:provenance :checksum-kind])))
  (is (= (get-in (fixtures/import-ofl ofl) [:provenance :sha256])
         (get-in (fixtures/import-ofl (into (sorted-map) ofl)) [:provenance :sha256]))))

(deftest unsupported-matrices-and-ambiguous-attributes-are-not-patchable
  (let [matrix (assoc-in ofl [:modes 0 :channels] [{:insert "matrixChannels"}])
        duplicate (assoc-in ofl [:modes 0 :channels] ["Red" "Red"])]
    (doseq [input [matrix duplicate]]
      (let [result (fixtures/import-ofl input)]
        (is (false? (:valid? result)))
        (is (nil? (:definition result)))
        (is (:candidate result)))))
  (is (thrown? clojure.lang.ExceptionInfo
               (fixtures/import-ofl (update ofl :modes conj {:name "other" :channels ["Red"]}))))
  (is (thrown? clojure.lang.ExceptionInfo (fixtures/import-ofl ofl {:mode "absent"}))))

(deftest invalid-registry-metadata-cannot-produce-a-publishable-import
  (let [result (fixtures/import-ofl ofl {:id "" :version -1})]
    (is (false? (:valid? result)))
    (is (nil? (:definition result)))
    (is (some #(= :invalid-definition (:code %)) (:diagnostics result)))))

(def qxf
  "<FixtureDefinition xmlns='http://www.qlcplus.org/FixtureDefinition'>
     <Manufacturer>Example</Manufacturer><Model>Head</Model>
     <Channel Name='Pan'><Group Byte='0'>Pan</Group><Capability Min='0' Max='255'>Pan</Capability></Channel>
     <Channel Name='Pan fine'><Group Byte='1'>Pan</Group></Channel>
     <Channel Name='Red'><Group Byte='0'>Colour</Group><Colour>Red</Colour></Channel>
     <Channel Name='Reset'><Group Byte='0'>Maintenance</Group><Capability Min='200' Max='255'>Reset</Capability></Channel>
     <Mode Name='four'><Channel Number='0'>Pan</Channel><Channel Number='1'>Pan fine</Channel>
       <Channel Number='2'>Red</Channel><Channel Number='3'>Reset</Channel></Mode>
   </FixtureDefinition>")

(deftest qxf-import-converts-zero-based-mode-addresses
  (let [result (fixtures/import-qxf qxf {:source "local:head.qxf"})]
    (is (:valid? result))
    (is (= {:pan [1 2] :red 3} (get-in result [:definition :channels])))
    (is (= 4 (get-in result [:definition :footprint])))
    (is (= "Example" (get-in result [:definition :manufacturer])))
    (is (= qxf (get-in result [:definition :source-data])))
    (is (= :qxf (get-in result [:provenance :format])))
    (is (empty? (registry/validate-definition :fixtures (:definition result))))))

(deftest qxf-rejects-entity-expansion-and-invalid-offsets
  (is (thrown? Exception (fixtures/import-qxf "<!DOCTYPE x [<!ENTITY x SYSTEM 'file:///etc/passwd'>]><FixtureDefinition>&x;</FixtureDefinition>")))
  (is (false? (:valid? (fixtures/import-qxf (clojure.string/replace qxf "Number='1'" "Number='0'")))))
  (is (false? (:valid? (fixtures/import-qxf (clojure.string/replace qxf "Number='3'>Reset" "Number='3'>Absent"))))))
