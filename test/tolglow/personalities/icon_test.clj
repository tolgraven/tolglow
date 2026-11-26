(ns tolglow.personalities.icon-test
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [tolglow.personalities :as personalities]
            [tolglow.personalities.forms :as forms]
            [tolglow.personalities.icon :as icon]
            [tolglow.transit :as transit])
  (:import [java.io ByteArrayInputStream]
           [java.nio.charset StandardCharsets]
           [java.util Base64]
           [javax.xml XMLConstants]
           [javax.xml.parsers DocumentBuilderFactory]
           [org.w3c.dom Element Node]))

(defn- by-id [id] (some #(when (= id (:id %)) %) personalities/builtins))

(defn- parse
  "Read the icon with a real XML parser that refuses doctypes and external entities."
  ^Element [^String svg]
  (let [factory (doto (DocumentBuilderFactory/newInstance)
                  (.setNamespaceAware true)
                  (.setFeature XMLConstants/FEATURE_SECURE_PROCESSING true)
                  (.setFeature "http://apache.org/xml/features/disallow-doctype-decl" true))]
    (.getDocumentElement (.parse (.newDocumentBuilder factory)
                                 (ByteArrayInputStream. (.getBytes svg StandardCharsets/UTF_8))))))

(defn- elements [^Element root]
  (let [nodes (.getElementsByTagName root "*")]
    (mapv #(.item nodes %) (range (.getLength nodes)))))

(defn- attributes [^Element element]
  (let [items (.getAttributes element)]
    (into {} (map (fn [index] (let [item (.item items index)] [(.getNodeName item) (.getNodeValue item)])))
          (range (.getLength items)))))

(defn- shapes [root]
  (filterv #(#{"rect" "circle" "path"} (.getTagName ^Element %)) (elements root)))

(defn- golden [personality]
  (slurp (io/resource (str "tolglow/personalities/golden/" (name (:id personality)) ".svg"))))

(deftest the-same-data-draws-the-same-bytes
  (doseq [personality personalities/builtins]
    (testing (str (:id personality))
      (let [svg (icon/svg personality)]
        (is (= svg (icon/svg personality)))
        (is (= svg (icon/svg (transit/decode (transit/encode personality))))
            "a save and load does not change the picture")
        (is (= svg (icon/svg (into (sorted-map) (update personality :motion #(into (sorted-map-by (comp - compare)) %)))))
            "map ordering is not part of the picture")
        (is (= svg (golden personality))
            "regenerate with: lein run -m tolglow.qa.personality-icons golden")))))

(deftest whole-and-fractional-numbers-describe-one-picture
  (let [personality (assoc (by-id :squares) :palette [[1 0 0] [0 0.5 1]]
                           :geometry {:forms {:square 2 :bar 1}}
                           :motion {:speed 1 :smoothness 0 :jitter 0 :symmetry 1})
        floats (assoc personality :palette [[1.0 0.0 0.0] [0.0 0.5 1.0]]
                      :geometry {:forms {:square 2.0 :bar 1.0}}
                      :motion {:speed 1.0 :smoothness 0.0 :jitter 0.0 :symmetry 1.0})]
    (is (= [] (personalities/errors personality)))
    (is (= (icon/svg personality) (icon/svg floats)))))

(deftest different-data-draws-a-different-picture
  (let [icons (mapv icon/svg personalities/builtins)
        squares (by-id :squares)
        base (icon/svg squares)]
    (is (apply distinct? icons))
    (doseq [[change edited] {"name" (assoc squares :name "Squares, renamed")
                             "palette colour" (assoc-in squares [:palette 0] [1.0 0.2 0.16])
                             "palette order" (update squares :palette (comp vec reverse))
                             "form weights" (assoc-in squares [:geometry :forms] {:square 5.0 :rectangle 4.0 :circle 2.0})
                             "dominant form" (assoc-in squares [:geometry :forms :bar] 9.0)
                             "jitter" (assoc-in squares [:motion :jitter] 0.5)
                             "symmetry" (assoc-in squares [:motion :symmetry] 0.2)
                             "smoothness" (assoc-in (by-id :zigzag-shaky) [:motion :smoothness] 0.9)}]
      (is (not= (if (= "smoothness" change) (icon/svg (by-id :zigzag-shaky)) base) (icon/svg edited)) change))
    (testing "concurrency limits are not drawn"
      (is (= base (icon/svg (assoc-in squares [:concurrency :max-groups] 9)))))))

(deftest icons-show-three-forms-from-the-vocabulary
  (is (= [:square :rectangle :bar] (icon/icon-forms (by-id :squares))))
  (is (= [:squircle :pill :circle] (icon/icon-forms (by-id :squircle))))
  (is (= :spiral (first (icon/icon-forms (by-id :spirals)))))
  (is (= :zigzag (first (icon/icon-forms (by-id :zigzag-shaky)))))
  (is (= [:circle :bar :pill] (icon/icon-forms (by-id :still-water))))
  (let [random (icon/icon-forms (by-id :random-pink))]
    (is (= 3 (count (distinct random))) "equal weights are ranked by the personality's own seed")
    (is (every? (set forms/order) random)))
  (is (= [:spiral :spiral :spiral] (icon/icon-forms (assoc-in (by-id :spirals) [:geometry :forms] {:spiral 1.0}))))
  (is (= [:ring :zigzag :ring] (icon/icon-forms (assoc-in (by-id :spirals) [:geometry :forms] {:ring 2.0 :zigzag 1.0})))))

(deftest every-icon-is-a-well-formed-self-contained-svg
  (doseq [personality (conj personalities/builtins
                            (personalities/blend [{:personality (by-id :squircle) :weight 0.7}
                                                  {:personality (by-id :spirals) :weight 0.3}])
                            (assoc (by-id :random-pink) :name "<script>alert(\"&'\")</script>")
                            (assoc (by-id :squares) :motion {:speed 1.0 :smoothness 0.0 :jitter 1.0 :symmetry 0.0}))]
    (testing (str (:id personality) " " (:name personality))
      (let [svg (icon/svg personality)
            root (parse svg)
            drawn (shapes root)
            ids (into [] (keep #(get (attributes %) "id")) (elements root))
            references (map second (re-seq #"url\(#([^)]+)\)" svg))]
        (is (= "svg" (.getLocalName root)))
        (is (= "http://www.w3.org/2000/svg" (.getNamespaceURI root)))
        (is (= {"viewBox" "0 0 96 96" "width" "96" "height" "96" "role" "img"}
               (select-keys (attributes root) ["viewBox" "width" "height" "role"])))
        (is (= (:name personality) (get (attributes root) "aria-label")))
        (is (= (:name personality) (.getTextContent ^Node (.item (.getElementsByTagName root "title") 0))))
        (is (= 4 (count drawn)) "one tile and three shapes")
        (is (= {"x" "2" "y" "2" "width" "92" "height" "92" "rx" "20"}
               (select-keys (attributes (first drawn)) ["x" "y" "width" "height" "rx"])))
        (is (= (set references) (set ids)) "every paint reference resolves and every definition is used")
        (is (apply distinct? (conj ids ::none)))
        (is (empty? (remove #{"svg" "title" "defs" "linearGradient" "stop" "rect" "circle" "path"}
                            (map #(.getTagName ^Element %) (elements root))))
            "no scripts, links, images or foreign content")
        (is (not (re-find #"(?i)href|<script" svg)))
        (is (not (re-find #"NaN|Infinity|\d[eE][-+]?\d" (str/replace svg #"#[0-9a-f]{6}|p[0-9a-f]{6}-\d" "")))
            "every number is plain decimal")
        (is (< (count svg) 4096))
        (doseq [shape (rest drawn)
                :let [{:strs [fill stroke]} (attributes shape)]]
          (is (some #(and % (re-matches #"#[0-9a-f]{6}|url\(#p[0-9a-f]{6}-[0-2]\)" %)) [fill stroke])
              "each shape is painted from the palette"))
        (doseq [[_ x y] (re-seq #"[ML](-?[\d.]+) (-?[\d.]+)" svg)]
          (is (and (<= -8 (parse-double x) 104) (<= -8 (parse-double y) 104)) "drawn points stay around the tile"))))))

(deftest palette-entries-become-solid-gradient-and-split-fills
  (let [svg (icon/svg (by-id :squares))
        root (parse svg)
        gradient (first (filter #(= "linearGradient" (.getTagName ^Element %)) (elements root)))
        stops (mapv attributes (filter #(= "stop" (.getTagName ^Element %)) (elements root)))]
    (is (str/includes? svg "fill=\"#ff3326\"") "the first entry is a plain colour")
    (is (= "userSpaceOnUse" (get (attributes gradient) "gradientUnits")))
    (is (= [["0%" "#ffd11a"] ["50%" "#ffd11a"] ["50%" "#1a4de6"] ["100%" "#1a4de6"]]
           (mapv (juxt #(get % "offset") #(get % "stop-color")) stops))
        "a split is two hard-edged bands inside one shape"))
  (let [root (parse (icon/svg (by-id :slow-sunset)))
        stops (mapv attributes (filter #(= "stop" (.getTagName ^Element %)) (elements root)))]
    (is (= [["0%" "#ffcc4d"] ["55%" "#ff6633"] ["100%" "#801a66"]]
           (mapv (juxt #(get % "offset") #(get % "stop-color")) stops)))))

(deftest a-data-uri-carries-the-same-bytes
  (let [svg (icon/svg (by-id :still-water))
        uri (icon/data-uri svg)]
    (is (str/starts-with? uri "data:image/svg+xml;base64,"))
    (is (= svg (String. (.decode (Base64/getDecoder) (subs uri (count "data:image/svg+xml;base64,")))
                        StandardCharsets/UTF_8)))))
