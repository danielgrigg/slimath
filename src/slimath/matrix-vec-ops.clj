(in-ns 'slimath.core)

(defn m2mulv "Multiply vector by matrix" [M [^double x ^double y]]
  [(+ (* (M 0) x) (* (M 2) y))
   (+ (* (M 1) x) (* (M 3) y))])

(defn m3mulv "Multiply vector by matrix" [M [^double x ^double y ^double z]]
  [(+ (* (M 0) x) (* (M 3) y) (* (M 6) z))
   (+ (* (M 1) x) (* (M 4) y) (* (M 7) z))
   (+ (* (M 2) x) (* (M 5) y) (* (M 8) z))])

(defn m4mulv "Multiply vector by matrix" 
[M [^double x ^double y ^double z ^double w]]
  [(+ (* (M 0) x) (* (M 4) y) (* (M 8) z)  (* (M 12) w))
   (+ (* (M 1) x) (* (M 5) y) (* (M 9) z)  (* (M 13) w))
   (+ (* (M 2) x) (* (M 6) y) (* (M 10) z) (* (M 14) w))
   (+ (* (M 3) x) (* (M 7) y) (* (M 11) z) (* (M 15) w))])

;; general macro for dim > 4
;(defmacro make-matrix-mul-vec
;  [{:keys [start end] :or {start 2}}]
;  (cons `do (for [n (range start end)]
;              (let [fname# (str-sym- "m" n "mulv")]
;              `(defn ~fname# "mul vector by matrix" [~'A ~'v]

                      
                      
