(in-ns 'slimath.core)

(defmacro vec-op "Evaluate a binary vector op"
  [op n & args]  
  `(vector-of :float 
              ~@(for [x (range n)] `(~op ~@(for [y args] `(~y ~x))))))

(defmacro vec-scalar-op [op n v scalar]
  `(vector-of :float 
              ~@(for [x (range n)]`(~op (~v ~x) ~scalar))))

(defmacro vec-reduce [rop mop n & args]
  `(~rop ~@(for [x (range n)] `(~mop ~@(for [y args] `(~y ~x))))))

(defmacro make-vec-unary-ops [name desc op]
  (cons `do
        (for [n [2 3 4]]
          `(defn ~(str-sym- \v name n)
             ~(str desc " a " n "-vector")
             [~'a]
             (vec-op ~op ~n ~'a)))))

;; generates vector functions for 2, 3 & 4 component vectors.
(defmacro make-vec-ops [name desc op self-op]
  (cons `do
        (for [n [2 3 4]]
          `(defn ~(str-sym- \v name n)
             ~(str desc n "-vector(s)")
             ~(if self-op ['a] ['a 'b])
             (vec-op ~op ~n ~@(if self-op ['a 'a] ['a 'b]))))))

(defmacro make-vec-scalar-ops [name desc op]
  (cons `do
        (for [n [2 3 4]]
          `(defn ~(str-sym- \v name n "s")
             ~(str desc " vector v and scalar s")
             [~'a ~'k]
             (vec-scalar-op ~op ~n ~'a ~'k)))))

(defmacro make-vec-reduce-ops [name desc rop mop self-op]
  (cons `do
        (for [n [2 3 4]]
          `(defn ~(str-sym- \v name n)
             ~(str desc " " n "-vector(s)")
             ~(if self-op ['a] ['a 'b])          
             (vec-reduce ~rop ~mop ~n ~@(if self-op ['a 'a] ['a 'b]))))))


(make-vec-ops add "Add" + false)
(make-vec-ops sub "Subtract" - false)
(make-vec-ops mul "Multiply" * false)
(make-vec-ops div "Divide" / false)
(make-vec-ops max "Maximum" max false)
(make-vec-ops min "Minimum" min false)
(make-vec-reduce-ops dot "Dot product" + * false)
(make-vec-reduce-ops length-sq "Length^2" + * true)
(make-vec-scalar-ops add "Add" +)
(make-vec-scalar-ops sub "Subtract" -)
(make-vec-scalar-ops mul "Multiply" *)
(make-vec-scalar-ops div "Divide" /)
(make-vec-reduce-ops equal "Equality" and approx false)
(make-vec-unary-ops floor "floor" numeric/floor)
(make-vec-unary-ops ceil "ceil" numeric/ceil)
(make-vec-unary-ops abs "abs" numeric/abs)
(defmacro vec3 [a b] `(vector-of :float ~a ~b))
(defmacro vec3 [a b c] `(vector-of :float ~a ~b ~c))
(defmacro vec4 [a b c d] `(vector-of :float ~a ~b ~c ~d))

(defmacro make-vec-length []
  (cons `do
        (for [n [2 3 4]]
          `(defn ~(str-sym- \v "length" n)
             ~(str "Length of " n "-vector") [~'a]
             (numeric/sqrt (~(str-sym- "vlength-sq" n) ~'a))))))

(make-vec-length)             

(defn vnormalize2 [a] (vmul2s a (/ (max eps (vlength2 a)))))
(defn vnormalize3 [a] (vmul3s a (/ (max eps (vlength3 a)))))
(defn vnormalize4 [a] (vmul4s a (/ (max eps (vlength4 a)))))

(defn cross [a b]
  (vec3 (- (* (a 1) (b 2)) (* (a 2) (b 1)))
        (- (* (a 2) (b 0)) (* (a 0) (b 2)))
        (- (* (a 0) (b 1)) (* (a 1) (b 0)))))
