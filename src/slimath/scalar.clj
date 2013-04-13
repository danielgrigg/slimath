(in-ns 'slimath.core)

(defn approx?
  "Are x and y approximately equal"
  ([^double x ^double y]
   (< (numeric/abs (- x y)) eps))
  ([^double x ^double y ^double tolerance]
   (< (numeric/abs (- x y)) tolerance)))


(defn quadratic "Compute the quadratic equation" [^double A ^double B ^double C]
  (let [discrim (- (* B B) (* 4.0 A C))]
    (if (< discrim 0)
      nil
      (let [rootDiscrim (numeric/sqrt discrim)
            q (if (< B 0)
                (* -0.5 (- B rootDiscrim))
                (* -0.5 (+ B rootDiscrim)))
            t0 (/ q A)
            t1 (/ C q)]
        (if (> t0 t1)
          [t1 t0]
          [t0 t1])))))
