(in-ns 'slimath.core)

;; transform v by M
(defn mvmul2 "multiply 2-vector v by M" [M v]
  [(vdot2 (M 0) v) (vdot2 (M 1) v)])
(defn mvmul3 "multiply 3-vector v by M" [M v]
  [(vdot3 (M 0) v) (vdot3 (M 1) v) (vdot3 (M 2) v)])
(defn mvmul4 "multiply 4-vector v by M" [M v]
  [(vdot4 (M 0) v) (vdot4 (M 1) v) (vdot4 (M 2) v) (vdot4 (M 3) v)])

(defn mvmul34 "multiply 4-vector ignoring bottom row" [M v]
  [(vdot4 (M 0) v) (vdot4 (M 1) v) (vdot4 (M 2) v) (v 3)])

