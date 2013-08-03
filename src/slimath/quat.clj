(in-ns 'slimath.core)
(def ^:const slerp-tolerance 0.999)

(defn quat 
  ([] 
     [0.0 0.0 0.0 1.0])
  ([^double x ^double y ^double z ^double w]
   [x y z w]))
     
(defn slerp
  "Slerp between two quaternions. Both q and p must be normalised."
 [^double t q p]
  (let [^double c (v4dot q p)
        ^double cos-angle (if (neg? c) (- c) c)
        start (if (neg? c) (v4negate q) q)
        [scale-q scale-p]  (if (< cos-angle slerp-tolerance)
                           (let [angle (Math/acos cos-angle)
                             inv-sin-angle (/ (Math/sin angle))]
                             [(* (Math/sin (* angle (- 1.0 t))) inv-sin-angle)
                              (* (Math/sin (* angle t)) inv-sin-angle)])
                           [(- 1.0 t) t])]
        (v4add (v4muls start scale-q) (v4muls p scale-p))))

(defn squad
 [t q0 q1 q2 q3]
 (slerp (* (* 2.0 t) (- 1.0 t)) (slerp t q0 q3) (slerp t q1 q2)))

(defn quat-rotation-vectors [[^double ax ^double ay ^double az :as a]
                             [^double bx ^double by ^doube bz :as b]]
  (let [x (Math/sqrt (* 2.0 (+ 1.0 (v3dot a b))))
        [cx cy cz] (v3muls (cross a b) x)]
    (quat cx cy cz (* 0.5 x))))

(defn quat-rotation-axis [^double rads [^double x ^double y ^double z :as xyz]]
  (let [angle (* 0.5 rads)
        [a b c] (v3muls xyz (Math/sin angle))]
    (quat a b c (Math/cos angle))))
    
(defn quat-mul [[ax ay az aw] [bx by bz bw]]
  (quat (+ (* aw bx) (* ax bw) (* ay bz) (- (* az by)))
        (+ (* aw by) (* ay bw) (* az bx) (- (* ax bz)))
        (+ (* aw bz) (* az bw) (* ax by) (- (* ay bx)))
        (- (* aw bw) (* ax bx) (* ay by) (* az bz))))

(defn quat-rotate-vector "rotate v by unit-q" [[qx qy qz qw] [vx vy vz]]
  (let [x (+ (* qw vx) (* qy vz) (- (* qz vy)))
        y (+ (* qw vy) (* qz vx) (- (* qx qz)))
        z (+ (* qw vz) (* qx vy) (- (* qy vx)))
        w (+ (* qx vx) (* qy vy) (* qz vz))]
    (vec3 
     (+ (* w qx) (* x qw) (- (* y qz)) (* z qy))
     (+ (* w qy) (* y qw) (- (* z qz)) (* x qz))
     (+ (* w qz) (* z qw) (- (* x qy)) (* y qx)))))
