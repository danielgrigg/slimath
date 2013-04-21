(in-ns 'slimath.core)

(defn m2mulv "Multiply vector by matrix" [M [^double x ^double y]]
  [(+ (* (M 0) x) (* (M 2) y))
   (+ (* (M 1) x) (* (M 3) y))])

(defn m3mulv "Multiply vector by matrix" [M [^double x ^double y ^double z]]
  [(+ (* (M 0) x) (* (M 3) y) (* (M 6) z))
   (+ (* (M 1) x) (* (M 4) y) (* (M 7) z))
   (+ (* (M 2) x) (* (M 5) y) (* (M 8) z))])

(defn m4mulv 
  "Multiply vector by matrix" 
  [[^double m00 ^double m10 ^double m20 ^double m30
    ^double m01 ^double m11 ^double m21 ^double m31
    ^double m02 ^double m12 ^double m22 ^double m32
    ^double m03 ^double m13 ^double m23 ^double m33] 
   [^double x ^double y ^double z ^double w]]

  [(+ (* m00 x) (* m01 y) (* m02 z)  (* m03 w))
   (+ (* m10 x) (* m11 y) (* m12 z)  (* m13 w))
   (+ (* m20 x) (* m21 y) (* m22 z) (* m23 w))
   (+ (* m30 x) (* m31 y) (* m32 z) (* m33 w))])

(defn m34mulv 
  "Multiply vector by matrix, ignoring bottom row" 
  [[^double m00 ^double m10 ^double m20 ^double _
    ^double m01 ^double m11 ^double m21 ^double _
    ^double m02 ^double m12 ^double m22 ^double _
    ^double m03 ^double m13 ^double m23 ^double _] 
   [^double x ^double y ^double z ^double w]]

  [(+ (* m00 x) (* m01 y) (* m02 z)  (* m03 w))
   (+ (* m10 x) (* m11 y) (* m12 z)  (* m13 w))
   (+ (* m20 x) (* m21 y) (* m22 z) (* m23 w))
   w])


;; general macro for dim > 4
;(defmacro make-matrix-mul-vec
;  [{:keys [start end] :or {start 2}}]
;  (cons `do (for [n (range start end)]
;              (let [fname# (str-sym "m" n "mulv")]
;              `(defn ~fname# "mul vector by matrix" [~'A ~'v]                     
