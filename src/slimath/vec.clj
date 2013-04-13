(in-ns 'slimath.core)

(defmacro -vec-op "Evaluate a binary vector op"
  [op n & args]  
  `[~@(for [x (range n)] `(~op ~@(for [y args] `(~y ~x))))])

(defmacro -make-vec-ops
 [{:keys [name op start end] :or {start 2}}]
 (cons `do (for [n (range start end)]
             `(defn ~(str-sym- \v n name)
                ~name
                [~'a ~'b]
                (-vec-op ~op ~n ~'a ~'b)))))

(defmacro -make-vec-unary-ops [{:keys [name op start end] :or {start 2}}]
  (cons `do (for [n (range start end)]
              `(defn ~(str-sym- \v n name)
                 ~name
                 [~'a]
                 (-vec-op ~op ~n ~'a)))))

(defmacro -vec-scalar-op [op n v scalar]
  `[ ~@(for [x (range n)]`(~op (~v ~x) ~scalar))])

(defmacro -make-vec-scalar-ops  
[{:keys [name op start end] :or {start 2}}]
  (cons `do (for [n (range start end)]
              `(defn ~(str-sym- \v n name "s")
                 ~name
                 [~'a ^double ~'k]
                 (-vec-scalar-op ~op ~n ~'a ~'k)))))

(defmacro -vec-reduce [rop mop n & args]
  `(~rop ~@(for [x (range n)] `(~mop ~@(for [y args] `(~y ~x))))))

(defmacro -make-vec-reduce-ops 
[{:keys [prefix name rop mop start end] :or {prefix \v start 2}}]
  (cons `do (for [n (range start end)]
              `(defn ~(str-sym- prefix n name)
                 ~(str  "map by " mop " and reduce by " rop)
                 [~'a ~'b]
                 (-vec-reduce ~rop ~mop ~n ~'a ~'b)))))

(defmacro -make-vec-length 
  [{:keys [start end] :or {start 2}}]
  (cons `do
        (for [n (range start end)]
          (let [dot# (str-sym- "v" n "dot")
                fname# (str-sym- "v" n "length")]
            `(defn ~fname# "vector length" [~'a]
               (numeric/sqrt (~dot# ~'a ~'a)))))))

(defmacro -make-vec-generator 
  [{:keys [name start end f is-fn] :or {start 2}}]
  (cons `do
        (for [n (range start end)]
          `(defn ~(str-sym- \v n name)
             ~(str "Generate entries with " name)
             []
             [ ~@(for [c (range n)] (if is-fn `(~f) f))]))))

(defmacro -make-vec-normalize 
  [{:keys [name start end] :or {start 2}}]
  (cons `do
        (for [n (range start end)]
          (let [muls# (str-sym- \v n "muls")
                length# (str-sym- \v n "length")]
            `(defn ~(str-sym- \v n "normalize")
               "normalize"
               [~'a]
               (~muls# ~'a (/ (max eps (~length# ~'a)))))))))
   
(defmacro vec2 [a b] `[ ~a ~b])
(defmacro vec3 [a b c] `[ ~a ~b ~c])
(defmacro vec4 [a b c d] `[ ~a ~b ~c ~d])

(-make-vec-generator { :name "rand" :f rand :is-fn true :end 5})
(-make-vec-generator { :name "zero" :f 0.0 :is-fn false :end 5})

(-make-vec-ops { :name "equal" :op #(if (approx? % %2) 1.0 0.0) :end 5})
(-make-vec-ops { :name "add" :op + :end 5})
(-make-vec-ops { :name "sub" :op - :end 5})
(-make-vec-ops { :name "mul" :op * :end 5})
(-make-vec-ops { :name "div" :op / :end 5})
(-make-vec-ops { :name "max" :op max :end 5})
(-make-vec-ops { :name "min" :op min :end 5})
(-make-vec-scalar-ops { :name "add" :op + :end 5})
(-make-vec-scalar-ops { :name "sub" :op - :end 5})
(-make-vec-scalar-ops { :name "mul" :op * :end 5})
(-make-vec-scalar-ops { :name "div" :op / :end 5})
(-make-vec-reduce-ops { :name "dot" :rop + :mop * :end 5})
(-make-vec-reduce-ops { :name "approx?" :rop and :mop approx? :end 5})
(-make-vec-unary-ops { :name "floor" :op numeric/floor :end 5})
(-make-vec-unary-ops { :name "ceil" :op numeric/ceil :end 5})
(-make-vec-unary-ops { :name "abs" :op numeric/abs :end 5})
(-make-vec-length {:end 5})
(-make-vec-normalize {:end 5})

(defn cross [a b]
  (vec3 (- (* (a 1) (b 2)) (* (a 2) (b 1)))
        (- (* (a 2) (b 0)) (* (a 0) (b 2)))
        (- (* (a 0) (b 1)) (* (a 1) (b 0)))))
