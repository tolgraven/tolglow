
(defn patch-ug []

  (patch-fixture! :moving-1 (tol/rgbw-12-12-moving)        10 156 :y 2.0 :x -2.00 :z 0.0
                       :x-rotation (tf/degrees 0)
                       :y-rotation (tf/degrees 180))
  (patch-fixture! :moving-2 (tol/rgbw-60-moving-beam) 10 180 :y 1.2 :x 2.00 :z 0.0 ;; XXX wtf even tho it's high up etc the second I aim higher than 0 it starts moving up. Why?
                       ;; :z-rotation (tf/degrees 180))
                       :y-rotation (tf/degrees 180))
  (patch-fixture! :wash-1 (tol/rgbw-18-12-par)   10  60 :y 0.0 :x 0.1)
  ;; (patch-fixture! :wash-2 (tol/rgbwauv-5-18-par) 10  70 :y 1.6 :x  2.1)

  (patch-strips)
  ;; (reset! strips-tubes (concat (show/fixtures-named :strip) (show/fixtures-named :tube))))
)
