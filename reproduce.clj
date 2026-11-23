(require '[tolglow.linux.audience.main :as main] '[clojure.java.io :as io] '[clojure.data.json :as json])
(import '[com.google.zxing BinaryBitmap RGBLuminanceSource BarcodeFormat ReaderException] '[com.google.zxing.common HybridBinarizer] '[com.google.zxing.qrcode QRCodeReader QRCodeWriter] '[javax.imageio ImageIO] '[java.awt.image BufferedImage] '[java.security MessageDigest] '[java.util HexFormat])
(defn fingerprint [n]
  (.formatHex (HexFormat/of) (.digest (MessageDigest/getInstance "SHA-256") (.getBytes (str "tolglow invite " n) "UTF-8"))))
(defn scan [file]
  (let [im (ImageIO/read file) w (.getWidth im) h (.getHeight im)]
    (try (.getText (.decode (QRCodeReader.) (BinaryBitmap. (HybridBinarizer. (RGBLuminanceSource. w h (.getRGB im 0 0 w h nil 0 w)))))) (catch ReaderException e (.getSimpleName (class e))))))
(defn preferred [url file]
  (let [matrix (.encode (QRCodeWriter.) url BarcodeFormat/QR_CODE 512 512) image (BufferedImage. 512 512 BufferedImage/TYPE_BYTE_BINARY)]
    (dotimes [y 512] (dotimes [x 512] (.setRGB image x y (if (.get matrix x y) 0 0xffffff))))
    (ImageIO/write image "png" file)))
(let [dir (io/file "/tmp/tolglow-qr-scan-evidence") old (io/file "/tmp/tolglow-qr-preferred.png") current (io/file "/tmp/tolglow-qr-current.png")
      failures (reduce (fn [out i]
                         (let [room (fingerprint i) origin "https://stage.example" url (main/write-invitation! origin room current)
                               _ (preferred url old) before (scan old) after (scan current)]
                           (assert (= url after) i)
                           (when (= i 5) (io/copy old (io/file dir "default-mask.png")) (io/copy current (io/file dir "selected-mask.png")))
                           (if (= url before) out (conj out {:seed i :room room :default-mask-error before})))) [] (range 384))
      receipt {:tested 384 :country-independent-format "HTTPS invocation URL with a 64-character lowercase SHA-256 fingerprint" :fixture "https://stage.example" :seed-format "SHA-256 UTF-8 of tolglow invite <0-based index>" :reader "ZXing 3.5.4 QRCodeReader with normal camera detection, OpenJDK 21" :png-dimensions [512 512] :default-detector-failures failures :updated-output-passed 384 :device-camera-tested false}]
  (println (json/write-str receipt))
  (spit (io/file dir "scan-report.json") (json/write-str receipt)))
