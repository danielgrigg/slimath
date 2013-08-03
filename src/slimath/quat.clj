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