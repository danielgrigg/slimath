(in-ns 'slimath.core)

(defmacro -vec-op "Evaluate a binary vector op"
  [op n & args]  
  `(vector-of :float 
              ~@(for [x (range n)] `(~op ~@(for [y args] `(~y ~x))))))

(defmacro -vec-scalar-op [op n v scalar]
  `(vector-of :float 
              ~@(for [x (range n)]`(~op (~v ~x) ~scalar))))

(defmacro -vec-reduce [rop mop n & args]
  `(~rop ~@(for [x (range n)] `(~mop ~@(for [y args] `(~y ~x))))))

(defmacro -make-vec-unary-ops [name desc op]
  (cons `do
        (for [n [2 3 4]]
          `(defn ~(str-sym- \v n name)
             ~(str desc " a " n "-vector")
             [~'a]
             (-vec-op ~op ~n ~'a)))))

;; generates vector functions for 2, 3 & 4 component vectors.
(defmacro -make-vec-ops [name desc op self-op]
  (cons `do
        (for [n [2 3 4]]
          `(defn ~(str-sym- \v n name)
             ~(str desc n "-vector(s)")
             ~(if self-op ['a] ['a 'b])
             (-vec-op ~op ~n ~@(if self-op ['a 'a] ['a 'b]))))))

(defmacro -make-vec-scalar-ops [name desc op]
  (cons `do
        (for [n [2 3 4]]
          `(defn ~(str-sym- \v n name "s")
             ~(str desc " vector v and scalar s")
             [~'a ~'k]
             (-vec-scalar-op ~op ~n ~'a ~'k)))))

(defmacro -make-vec-reduce-ops [name desc rop mop self-op]
  (cons `do
        (for [n [2 3 4]]
          `(defn ~(str-sym- \v n name)
             ~(str desc " " n "-vector(s)")
             ~(if self-op ['a] ['a 'b])          
             (-vec-reduce ~rop ~mop ~n ~@(if self-op ['a 'a] ['a 'b]))))))


(-make-vec-ops add "Add" + false)
(-make-vec-ops sub "Subtract" - false)
(-make-vec-ops mul "Multiply" * false)
(-make-vec-ops div "Divide" / false)
(-make-vec-ops max "Maximum" max false)
(-make-vec-ops min "Minimum" min false)
(-make-vec-reduce-ops dot "Dot product" + * false)
(-make-vec-scalar-ops add "Add" +)
(-make-vec-scalar-ops sub "Subtract" -)
(-make-vec-scalar-ops mul "Multiply" *)
(-make-vec-scalar-ops div "Divide" /)
(-make-vec-reduce-ops equal "Equality" and approx false)
(-make-vec-unary-ops floor "floor" numeric/floor)
(-make-vec-unary-ops ceil "ceil" numeric/ceil)
(-make-vec-unary-ops abs "abs" numeric/abs)
(defmacro vec3 [a b] `(vector-of :float ~a ~b))
(defmacro vec3 [a b c] `(vector-of :float ~a ~b ~c))
(defmacro vec4 [a b c d] `(vector-of :float ~a ~b ~c ~d))

(defmacro -make-vec-length []
  (cons `do
        (for [n [2 3 4]]
          `(defn ~(str-sym- "length" n)
             ~(str "Length of " n "-vector") [~'a]
             (numeric/sqrt (~(str-sym- "v" n "dot") ~'a ~'a))))))

(-make-vec-length)             

(defn normalize2 [a] (v2muls a (/ (max eps (v2dot a a)))))
(defn normalize3 [a] (v3muls a (/ (max eps (v3dot a a)))))
(defn normalize4 [a] (v4muls a (/ (max eps (v4dot a a)))))

(defn cross [a b]
  (vec3 (- (* (a 1) (b 2)) (* (a 2) (b 1)))
        (- (* (a 2) (b 0)) (* (a 0) (b 2)))
        (- (* (a 0) (b 1)) (* (a 1) (b 0)))))
