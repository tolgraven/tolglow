(ns tolglow.linux.audience.qr
  (:require [clojure.java.io :as io])
  (:import [com.google.zxing BarcodeFormat EncodeHintType ReaderException]
           [com.google.zxing.common BitMatrix]
           [com.google.zxing.qrcode QRCodeWriter]
           [com.google.zxing.qrcode.decoder Decoder]
           [com.google.zxing.qrcode.detector Detector]
           [java.awt.image BufferedImage]
           [javax.imageio ImageIO]))

(def ^:private size 512)

(defn- detectable? [^BitMatrix matrix url]
  ;; Exercise finder/alignment detection as a camera reader would.
  (try
    (let [sampled (.getBits (.detect (Detector. matrix)))]
      (= url (.getText (.decode (Decoder.) sampled))))
    (catch ReaderException _ false)))

(defn- invitation-matrix [url]
  (let [writer (QRCodeWriter.)
        encode #(.encode writer url BarcodeFormat/QR_CODE size size %)
        preferred (encode {})]
    (if (detectable? preferred url)
      preferred
      (or (some (fn [mask]
                  (let [matrix (encode {EncodeHintType/QR_MASK_PATTERN mask})]
                    (when (detectable? matrix url) matrix)))
                (range 8))
          (throw (ex-info "No readable QR mask for the stage invitation" {}))))))

(defn write-png! [file url]
  (let [^BitMatrix matrix (invitation-matrix url)
        image (BufferedImage. size size BufferedImage/TYPE_BYTE_BINARY)]
    (dotimes [y size]
      (dotimes [x size]
        (.setRGB image x y (if (.get matrix x y) 0 0xffffff))))
    (ImageIO/write image "png" (io/file file))))
