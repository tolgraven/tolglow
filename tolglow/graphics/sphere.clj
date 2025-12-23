(ns tolglow.graphics.sphere
  (:require [quil.core :as q]))


(defn setup-circles []
  (let [r 10
        [w h] [(q/width) (q/height)]
        y-vals (take-nth r (range (+ h r)))
        x-vals (flatten
                 (map #(repeat (count y-vals) %)
                      (take-nth r (range (+ w r)))))
        origins (partition 2 (interleave x-vals (cycle y-vals)))
        circles (map (fn [[x y]] {:x x :y y :r r}) origins)]
    {:radius r :circles circles}))

(defn update-circles [{:keys [radius circles] :as s}]
  (update s :circles
   #(map (fn [{:keys [x y] :as c}]
           (let [[cx cy] [(/ (q/width) 2) (/ (q/height) 2)]
                 t (/ (q/millis) 1000)]
             (assoc c :r (* radius
                            (q/sin (+ t (+ (q/sq (- cx x))
                                           (q/sq (- cy y))))))))) %)))

(defn draw-circles [s]
  (q/background 0 0 0)
  (q/no-stroke)

  (doseq [c (:circles s)]
    (q/ellipse (:x c) (:y c) (:r c) (:r c))))


;EXAMPLE DANCER
(defn pulse [low high rate]
  (let [diff (- high low)
        half (/ diff 2)
        mid (+ low half)
        s (/ (q/millis) 1000.0)
        x (q/sin (* s (/ 1.0 rate)))]
    (+ mid (* x half))))

(defn t [] (* 0.001 (q/millis)))
(def speed 0.5)

(defn stem [base-x]
  (let [magic (/ 8 (q/width))
        x-max (/ (q/width) 4)
        x-max-top (/ x-max 2)
        y-max (/ (q/height) 2)

        x (+ base-x (pulse (- x-max-top) x-max-top 1.0))
        y (+ (- y-max)
             (* 0.5   y-max (q/sin (+ (* (t) speed) (* magic base-x))))
             (* (/ 3) y-max (q/sin (* 2 (t)))))]
    (q/bezier base-x 0 base-x 0 0 (- x-max) x y)))

(defn draw-dancer []
  (q/background 255)
  (q/stroke 0) (q/stroke-weight 1)
  (q/no-fill)
  (let [size (q/width)
        x-max (/ size 4)]
    (q/with-translation [(/ size 2) (q/height)]
      (doseq [x (range (- x-max) x-max 2)]
        (stem x)))))

;WAVES EXAMPLE
(defn calc-y [x mid amp]
  (+ mid (* (q/sin (+ (t) x)) amp)))

(defn wave [step mid-y amp]
  (let [[w h] [(q/width) (q/height)]
        scaled (q/map-range w 700 200
                              0.01 0.03)]
    (q/begin-shape) (q/vertex 0 h) ; lower left corner
    (doseq [x (range (- w) (+ step w) step)]
      (let [y (calc-y (* x scaled) mid-y amp)]
        (q/vertex x y)))
    (q/vertex w h) (q/end-shape))) ; lower right corner

(defn draw-wave []
  (q/background 250)
  (q/stroke 255 250)
  (q/fill 50 230 (+ (* 20 (q/sin (t))) 230) 40)
  (let [h (q/height)
        move-down (/ h 5)
        amp (/ h 8)]
    (doseq [y (range move-down (+ amp h) 8)]
      (let [x-step (- (* y 0.8) move-down)]
        (wave x-step y amp)))))

;; (def amount 4000)
;; ;; Pelo[] lista ;
;; ;; float[] z = new float[cuantos];
;; ;; float[] phi = new float[cuantos];
;; ;; float[] largos = new float[cuantos];
;; ;; float radius;
;; ;; float rx = 0;
;; ;; float ry =0;
;;
;; (def setup
;;   radius = height/3;
;;
;;   lista = new Pelo[cuantos];
;;   for (int i=0; i<cuantos; i++) {
;;     lista[i] = new Pelo();
;;   }
;;   (q/noiseDetail 3));
;;
;; (def draw
;;   q/translate(width/2, height/2);
;;
;;   ;; float rxp = ((mouseX-(width/2))*0.005);
;;   ;; float ryp = ((mouseY-(height/2))*0.005);
;;   ;; rx = (rx*0.9)+(rxp*0.1);
;;   ;; ry = (ry*0.9)+(ryp*0.1);
;;   (q/rotateY rx)
;;   (q/rotateX ry)
;;   (q/fill 0)
;;   (q/noStroke)
;;   (q/sphere radius);
;;
;;   (mapv #(render %) lista)
;; )
;;
;;
;;
;;   float z = (rand-radius, radius);
;;   float phi = (rand TWO_PI);
;;   float largo = (rand 1.15, 1.2);
;;   float theta = (asin z / radius);
;;
;; (defn render []
;;
;;   float off = (noise (* (millis) 0.0005, (sin phi)) - 0.5) * 0.3;
;;   float offb = (noise (* (millis) 0.0007, (sin z) * 0.01) - 0.5) * 0.3;
;;
;;   float thetaff = (+ theta off);
;;   float phff = (+ phi offb);
;;   float x = (* radius (cos theta) (cos phi));
;;   float y = (* radius (cos theta) (sin phi));
;;   float z = (* radius (sin theta));
;;   float msx= (screenX x, y, z);
;;   float msy= (screenY x, y, z);
;;
;;   float xo = (* radius (cos thetaff) (cos phff));
;;   float yo = (* radius (cos thetaff) (sin phff));
;;   float zo = (* radius (sin thetaff));
;;
;;   float xb = (* xo largo);
;;   float yb = (* yo largo);
;;   float zb = (* zo largo);
;;
;;   (q/beginShape LINES);
;;   (q/stroke 0);
;;   (q/vertex x, y, z);
;;   (q/stroke 200, 150);
;;   (q/vertex xb, yb, zb);
;;   (q/endShape);
;; )
