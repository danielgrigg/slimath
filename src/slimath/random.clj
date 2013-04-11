(in-ns 'slimath.core)

(defn rand-gauss "Gaussian distributed random variable"
  []
  (let [u (rand)
        v (rand)]
    (* (numeric/sqrt (* -2.0 (Math/log u))) (Math/cos (* 2.0 pi v)))))

(defn rand-gauss2 "Pair of gaussian distributed random variables"
  []
  (let [u (rand)
        v (rand)]
    [(* (numeric/sqrt (* -2.0 (Math/log u))) (Math/cos (* 2.0 pi v)))
     (* (numeric/sqrt (* -2.0 (Math/log u))) (Math/sin (* 2.0 pi v)))]))
