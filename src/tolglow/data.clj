(ns tolglow.data
  "Read portable authoring definitions and normalize textual control values."
  (:require [clojure.edn :as edn]))

(defn read-edn
  "Read exactly one EDN definition; no trailing forms or custom readers."
  [source]
  (when-not (string? source) (throw (ex-info "Definition source must be text" {})))
  (with-open [reader (java.io.PushbackReader. (java.io.StringReader. source))]
    (let [eof (Object.) value (edn/read {:eof eof :readers {}} reader)]
      (when (or (identical? eof value)
                (not (identical? eof (edn/read {:eof eof :readers {}} reader))))
        (throw (ex-info "Expected exactly one EDN definition" {})))
      value)))

(def keyword-fields
  #{:op :kind :mode :loop :scope :intent :provider :reload-policy :blend :attribute
    :param :type :rgbw-policy :cue-id :effect-id :page-id :show-id :key :axis :curve
    :timebase :operation :retrigger :unit :distribution :match})

(defn normalize-json [value]
  ;; Studio widgets emit textual enum choices. Keep typed extension values
  ;; (particularly thi.ng.color records) intact while converting those choices.
  (cond
    (record? value) value
    (map? value) (into {} (map (fn [[k v]] [k (cond
                                                (and (string? v) (or (keyword-fields k) (= k :id))) (keyword v)
                                                (and (= k :params) (= v "all")) :all
                                                (and (vector? v) (#{:params :attributes :targets :effect-ids :cue-ids :fixture-ids :fog-fixture-ids :verified-axis-fixture-ids :categories} k))
                                                (mapv #(if (string? %) (keyword %) (normalize-json %)) v)
                                                :else (normalize-json v))])) value)
    (vector? value) (mapv normalize-json value)
    :else value))

