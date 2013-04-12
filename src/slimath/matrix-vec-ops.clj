(in-ns 'slimath.core)

;; transform v by M
(defn m2vmul "multiply 2-vector v by M" [M v]
  [(v2dot (M 0) v) (v2dot (M 1) v)])
(defn m3vmul "multiply 3-vector v by M" [M v]
  [(v3dot (M 0) v) (v3dot (M 1) v) (v3dot (M 2) v)])
(defn m4vmul "multiply 4-vector v by M" [M v]
  [(v4dot (M 0) v) (v4dot (M 1) v) (v4dot (M 2) v) (v4dot (M 3) v)])

(defn m34vmul "multiply 4-vector ignoring bottom row" [M v]
  [(v4dot (M 0) v) (v4dot (M 1) v) (v4dot (M 2) v) (v 3)])

