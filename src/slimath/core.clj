(ns slimath.core
  (:require [clojure.math.numeric-tower :as numeric])
  (:use [clojure.pprint :only [pprint]]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* true)

(defn str-sym "Symbolise a sequence of tokens"
  [& args]
  (symbol (apply str args))) 

(defn vecfn-name [& args]
  "Defines a symbol from args for a vector based function."
  (symbol (apply str args))) 
  ;(with-meta (symbol (apply str args)) {:tag 'double}))

(defmacro bench [n & exprs]
  `(time
     (dotimes [~'_ ~n]
       (do ~@exprs))))

(def ^:const eps 4e-5)
(def ^:const eps-small 1.4E-45)
(def ^:const infinity 1e37)
(def ^:const pi Math/PI)

(defn approx?
  "Are x and y approximately equal"
  ([^double x ^double y]
   (< (numeric/abs (- x y)) eps))
  ([^double x ^double y ^double tolerance]
   (< (numeric/abs (- x y)) tolerance)))


(defn quadratic "Compute the quadratic equation" [^double A ^double B ^double C]
  (let [discrim (- (* B B) (* 4.0 A C))]
    (when-not (neg? discrim)
      (let [rootDiscrim (numeric/sqrt discrim)
            q (if (neg? B)
                (* -0.5 (- B rootDiscrim))
                (* -0.5 (+ B rootDiscrim)))
            t0 (/ q A)
            t1 (/ C q)]
        (if (> t0 t1)
          [t1 t0]
          [t0 t1])))))

(defn clamp "Clamp a to (min-a, max-x)" 
  [^double a ^double min-a ^double max-a]
  (min max-a (max min-a a)))

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


