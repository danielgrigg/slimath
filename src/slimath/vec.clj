(ns slimath.vec
;  (:require [clojure.math.numeric-tower :as numeric])
  (:use [clojure.math.numeric-tower :only [abs sqrt floor ceil abs] ]
         [slimath core]
         [clojure walk]))

(def ^:const vec-dims [2 3 4])

(defn vsym [k n] (str-sym \v n (name k)))

(defmacro -devfn [fname n args & body]
  (let [ks (filter keyword? (flatten body))
        ops (zipmap ks (map #(vsym % n) ks))]
      `(defn ~(vsym fname n)
         ~args
         ~@(prewalk-replace ops body))))

(defmacro devfn "Generate a vector function" [fname args & body]
  (cons `do (for [n vec-dims]
              `(-devfn ~fname ~n ~args ~@body))))

;; TODO - would be nice if this worked
(defmacro -varg [a n]
  `(~@(for [x (range n)] (str-sym a x)) :as ~a))  ; (str-sym ~a ~x)) :as a))
                
(defmacro -make-vec-ops
 [{:keys [name op]}]
 (cons `do (for [n vec-dims]
             `(defn ~(str-sym \v n name)
                ~name
                 [[~@(for [x (range n)] (str-sym "a" x)) :as ~'a]
                  [~@(for [x (range n)] (str-sym "b" x)) :as ~'b]]
                [~@(for [x (range n)] 
                     `(~op ~(str-sym "a" x) ~(str-sym "b" x)))]))))

;; TODO - ideally the general ops would be as fast as this.  But that requires
;; type hinting.  Which seems to be a pita to get working on the above.
(defn fast-add [[^double a1 ^double a2 ^double a3] [^double b1 ^double b2 ^double b3]]
  [(+ a1 b1) (+ a2 b2) (+ a3 b3)])

(defmacro -make-vec-unary-ops [{:keys [name op start end] }]
  (cons `do (for [n vec-dims]
              `(defn ~(str-sym \v n name)
                 ~name
                [[~@(for [x (range n)] (str-sym "a" x)) :as ~'a]]
                [~@(for [x (range n)] `(~op ~(str-sym "a" x)))]))))

(defmacro -make-vec-scalar-ops  
[{:keys [name op start end] }]
  (cons `do (for [n vec-dims]
              `(defn ~(str-sym \v n name "s")
                 ~name
                 [[~@(for [x (range n)] (str-sym "a" x)) :as ~'a] ~'k]
                 [ ~@(for [x (range n)] `(~op ~(str-sym "a" x) ~'k))]))))

(defmacro -vec-reduce [rop mop n & args]
  `(~rop ~@(for [x (range n)] `(~mop ~@(for [y args] `(~y ~x))))))

(defmacro -make-vec-reduce-ops 
[{:keys [prefix name rop mop start end] :or {prefix \v }}]
  (cons `do (for [n vec-dims]
              `(defn ~(str-sym prefix n name)
                 ~(str  "map by " mop " and reduce by " rop)
                 [~'a ~'b]
                 (-vec-reduce ~rop ~mop ~n ~'a ~'b)))))

(defmacro -make-vec-length 
  [{:keys [start end] }]
  (cons `do
        (for [n vec-dims]
          (let [dot# (str-sym "v" n "dot")
                fname# (str-sym "v" n "length")]
            `(defn ~fname# "vector length" 
               [[~@(for [x (range n)] (str-sym "a" x)) :as ~'a]]
               (sqrt (~dot# ~'a ~'a)))))))

(defmacro -make-vec-generator 
  [{:keys [name start end f is-fn] }]
  (cons `do
        (for [n vec-dims]
          `(defn ~(str-sym \v n name)
             ~(str "Generate entries with " name)
             []
             [ ~@(for [c (range n)] (if is-fn `(~f) f))]))))

(defmacro -make-vec-normalize 
  [{:keys [name start end] }]
  (cons `do
        (for [n vec-dims]
          (let [muls# (str-sym \v n "muls")
                length# (str-sym \v n "length")]
            `(defn ~(str-sym \v n "normalize")
               "normalize"
               [~'a]
               (~muls# ~'a (/ (max eps (~length# ~'a)))))))))
   
(defmacro vec2 [a b] `[ ~a ~b])
(defmacro vec3 [a b c] `[ ~a ~b ~c])
(defmacro vec4 [a b c d] `[ ~a ~b ~c ~d])

(-make-vec-generator { :name "rand" :f rand :is-fn true})
(-make-vec-generator { :name "zero" :f 0.0 :is-fn false})

(-make-vec-ops { :name "equal" :op #(if (approx? % %2) 1.0 0.0)})
(-make-vec-ops { :name "add" :op +})
(-make-vec-ops { :name "sub" :op -})
(-make-vec-ops { :name "mul" :op *})
(-make-vec-ops { :name "div" :op / })
(-make-vec-ops { :name "max" :op max })
(-make-vec-ops { :name "min" :op min })
(-make-vec-scalar-ops { :name "add" :op + })
(-make-vec-scalar-ops { :name "sub" :op - })
(-make-vec-scalar-ops { :name "mul" :op * })
(-make-vec-scalar-ops { :name "div" :op / })
(-make-vec-reduce-ops { :name "dot" :rop + :mop * })
(-make-vec-reduce-ops { :name "approx?" :rop and :mop approx? })
(-make-vec-unary-ops { :name "floor" :op floor })
(-make-vec-unary-ops { :name "ceil" :op ceil})
(-make-vec-unary-ops { :name "int" :op int })
(-make-vec-unary-ops { :name "abs" :op abs})
(-make-vec-unary-ops { :name "negate" :op - })

(devfn length [a] (sqrt (:dot a a)))
(devfn normalize [a] (:muls a (/ (max eps (:length a)))))
(devfn lerp [t a b] (:add a (:muls (:sub b a) t)))

(defn cross [a b]
  (vec3 (- (* (a 1) (b 2)) (* (a 2) (b 1)))
        (- (* (a 2) (b 0)) (* (a 0) (b 2)))
        (- (* (a 0) (b 1)) (* (a 1) (b 0)))))

;;TODO - vectorize
(defn v3clamp
  [[^double x ^double y ^double z :as xyz] 
   [^double ax ^double ay ^double az :as a]
   [^double bx ^double by ^double bz :as b]]
  (-> xyz
      (v3min b)
      (v3max a)))
 
(defn v3sign [[^double x ^double y ^double z]] 
  [(if (pos? x) 1 -1)
   (if (pos? y) 1 -1)
   (if (pos? z) 1 -1)])
