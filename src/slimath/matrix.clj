(in-ns 'slimath.core)

(defmacro -make-matrix-ops [op-name desc]
  (cons `do
        (for [n [2 3 4]]
          `(defn ~(str-sym- "ms" op-name n)
             ~(str desc " two " n "-matrices")
             [~'A ~'B]
             [~@(for [r (range n)] `(~(str-sym- \v n op-name) (~'A ~r) (~'B ~r)))]))))

(defmacro -make-matrix-scalar-ops [op-name desc]
  (cons `do
        (for [n [2 3 4]]
          `(defn ~(str-sym- "m" n op-name "s")
             ~(str desc " matrix with scalar")
             [~'A ~'k]
             [~@(for [r (range n)] `(~(str-sym- \v n op-name "s") (~'A ~r) ~'k))]))))


(defmacro -make-mmul []
  (cons 'do
        (for [n [2 3 4]]
          (let [mmul# (str-sym- "m" n "mul")]
            `(defn ~mmul#  ~(str "Multiply two " n "x" n " matrices")
               ([~'A ~'B]
                (let [~@(apply concat
                               `(~@(for [c (range n)]
                                     `[~(str-sym- "c" c)
                                       (~(str-sym- "m" n "col") ~'B ~c) ])))]
                  [~@(for [x (range n)]
                       (vec (for [y (range n)]
                              `(~(str-sym- "v" n "dot") (~'A ~x) ~(str-sym- "c" y)))))]))
               ([~'A ~'B & ~'xs]
                (reduce ~mmul# (~mmul# ~'A ~'B) ~'xs)))))))

(-make-matrix-ops add "Add")
(-make-matrix-ops sub "Subtract")
(-make-matrix-ops mul "Multiply")
(-make-matrix-ops div "Divide")
(-make-matrix-scalar-ops add "Add")
(-make-matrix-scalar-ops sub "Subtract")
(-make-matrix-scalar-ops mul "Multiply")
(-make-matrix-scalar-ops div "Divide")

(defn m2row [M r] (M r))
(defn m3row [M r] (M r))
(defn m4row [M r] (M r))
(defn m2col [M c] [((M 0) c) ((M 1) c)])
(defn m3col [M c] [((M 0) c) ((M 1) c) ((M 2) c)])
(defn m4col [M c] [((M 0) c) ((M 1) c) ((M 2) c) ((M 3) c)])

(-make-mmul)

(defn m2transpose "2x2 transpose"
  [A] [(m2col A 0) (m2col A 1)])
(defn m3transpose "3x3 transpose"
  [A] [(m3col A 0) (m3col A 1) (m3col A 2)])
(defn m4transpose "4x4 transpose"
  [A] [(m4col A 0) (m4col A 1) (m4col A 2) (m4col A 3)])

(defmacro -make-matrix-lookups []
  (cons 'do
        (for [r (range 4) c (range 4)]
          `(defn ~(str-sym- "m" r c)
             [~'A]
             ((~'A ~r) ~c)))))

(-make-matrix-lookups)

(defn m2det "2x2 matrix determinant" [A]
  (- (* (m00 A) (m11 A)) (* (m01 A) (m10 A))))

(defn m3det "3x3 matrix determinant" [M]
  (v3dot (m3row M 0)
         [(- (* (m11 M) (m22 M)) (* (m12 M) (m21 M)))
          (- (* (m12 M) (m20 M)) (* (m22 M) (m10 M)))
          (- (* (m10 M) (m21 M)) (* (m11 M) (m20 M)))]))

(defn m2inverse [A]
  (m2muls [[(m11 A) (-(m01 A))] [(-(m10 A)) (m00 A)]]
          (/ (double (m2det A)))))

(defn m3inverse [M]
  "3x3 matrix inverse"
  (let [A (- (* (m11 M) (m22 M)) (* (m12 M) (m21 M))) 
        B (- (* (m12 M) (m20 M)) (* (m10 M) (m22 M)))
        C (- (* (m10 M) (m21 M)) (* (m11 M) (m20 M))) 
        D (- (* (m02 M) (m21 M)) (* (m01 M) (m22 M))) 
        E (- (* (m00 M) (m22 M)) (* (m02 M) (m20 M))) 
        F (- (* (m20 M) (m01 M)) (* (m00 M) (m21 M))) 
        G (- (* (m01 M) (m12 M)) (* (m02 M) (m11 M)))
        H (- (* (m02 M) (m10 M)) (* (m00 M) (m12 M)))                                                  
        K (- (* (m00 M) (m11 M)) (* (m01 M) (m10 M)))]
    (m3muls [[A D G]
             [B E H]
             [C F K]] (/ (double (m3det M))))))

(defn m4inverse [M]
  (let [t [(* (m22 M) (m33 M))           (* (m23 M) (m32 M))
           (* (m21 M) (m33 M))           (* (m23 M) (m31 M))
           (* (m21 M) (m32 M))           (* (m22 M) (m31 M))
           (* (m20 M) (m33 M))           (* (m23 M) (m30 M))
           (* (m20 M) (m32 M))           (* (m22 M) (m30 M))
           (* (m20 M) (m31 M))           (* (m21 M) (m30 M))
           (* (m02 M) (m13 M))           (* (m03 M) (m12 M))
           (* (m01 M) (m13 M))           (* (m03 M) (m11 M))
           (* (m01 M) (m12 M))           (* (m02 M) (m11 M))
           (* (m00 M) (m13 M))           (* (m03 M) (m10 M))
           (* (m00 M) (m12 M))           (* (m02 M) (m10 M))
           (* (m00 M) (m11 M))           (* (m01 M) (m10 M))]

        B [[(- (+ (* (t 0) (m11 M)) (* (t 3) (m12 M)) (* (t 4) (m13 M)))
               (+ (* (t 1) (m11 M)) (* (t 2) (m12 M)) (* (t 5) (m13 M))))         
            (- (+ (* (t 1) (m01 M)) (* (t 2) (m02 M)) (* (t 5) (m03 M)))
               (+ (* (t 0) (m01 M)) (* (t 3) (m02 M)) (* (t 4) (m03 M))))
            (- (+ (* (t 12) (m31 M)) (* (t 15) (m32 M)) (* (t 16) (m33 M)))
               (+ (* (t 13) (m31 M)) (* (t 14) (m32 M)) (* (t 17) (m33 M))))
            (- (+ (* (t 14) (m22 M)) (* (t 17) (m23 M)) (* (t 13) (m21 M)))
               (+ (* (t 16) (m23 M)) (* (t 12) (m21 M)) (* (t 15) (m22 M))))]

           [(- (+ (* (t 1) (m10 M)) (* (t 6) (m12 M)) (* (t 9) (m13 M)))
               (+ (* (t 0) (m10 M)) (* (t 7) (m12 M)) (* (t 8) (m13 M))))          
            (- (+ (* (t 0) (m00 M)) (* (t 7) (m02 M)) (* (t 8) (m03 M)))
               (+ (* (t 1) (m00 M)) (* (t 6) (m02 M)) (* (t 9) (m03 M))))          
            (- (+ (* (t 13) (m30 M)) (* (t 18) (m32 M)) (* (t 21) (m33 M)))
               (+ (* (t 12) (m30 M)) (* (t 19) (m32 M)) (* (t 20) (m33 M))))            
            (- (+ (* (t 20) (m23 M)) (* (t 12) (m20 M)) (* (t 19) (m22 M)))
               (+ (* (t 18) (m22 M)) (* (t 21) (m23 M)) (* (t 13) (m20 M))))]

           [(- (+ (* (t 2) (m10 M)) (* (t 7) (m11 M)) (* (t 10) (m13 M)))
               (+ (* (t 3) (m10 M)) (* (t 6) (m11 M)) (* (t 11) (m13 M))))          
            (- (+ (* (t 3) (m00 M)) (* (t 6) (m01 M)) (* (t 11) (m03 M)))
               (+ (* (t 2) (m00 M)) (* (t 7) (m01 M)) (* (t 10) (m03 M))))          
            (- (+ (* (t 14) (m30 M)) (* (t 19) (m31 M)) (* (t 22) (m33 M)))
               (+ (* (t 15) (m30 M)) (* (t 18) (m31 M)) (* (t 23) (m33 M))))            

            (- (+ (* (t 18) (m21 M)) (* (t 23) (m23 M)) (* (t 15) (m20 M)))
               (+ (* (t 22) (m23 M)) (* (t 14) (m20 M)) (* (t 19) (m21 M))))            
            ]

           [(- (+ (* (t 5) (m10 M)) (* (t 8) (m11 M)) (* (t 11) (m12 M)))
               (+ (* (t 4) (m10 M)) (* (t 9) (m11 M)) (* (t 10) (m12 M))))
            (- (+ (* (t 4) (m00 M)) (* (t 9) (m01 M)) (* (t 10) (m02 M)))
               (+ (* (t 5) (m00 M)) (* (t 8) (m01 M)) (* (t 11)(m02 M))))
            (- (+ (* (t 17) (m30 M)) (* (t 20) (m31 M)) (* (t 23) (m32 M)))
               (+ (* (t 16) (m30 M)) (* (t 21) (m31 M)) (* (t 22) (m32 M))))
            (- (+ (* (t 22) (m22 M)) (* (t 16) (m20 M)) (* (t 21) (m21 M)))
               (+ (* (t 20) (m21 M)) (* (t 23) (m22 M)) (* (t 17) (m20 M))))]]]
    (m4muls B (/ (double (v4dot (m4row M 0) (m4col B 0)))))))

(defn m2identity [] [[1 0] [0 1]])
(defn m3identity [] [[1 0 0] [0 1 0] [0 0 1]])
(defn m4identity [] [[1 0 0 0] [0 1 0 0] [0 0 1 0] [0 0 0 1]])

(defmacro -make-matrix-generator [f is-fn]
  (cons `do
        (for [n [2 3 4]]
          `(defn ~(str-sym- "m" n f)
             ~(str "Generate " n "x" n " matrix with entries generated by f")
             []
             [~@(for [r (range n)]
                  `[~@(for [c (range n)] (if is-fn `(~f) f) )])]))))

(-make-matrix-generator rand true)

(defn m2equal [A B] (every? true? (map v2equal A B)))
(defn m3equal [A B] (every? true? (map v3equal A B)))
(defn m4equal [A B] (every? true? (map v4equal A B)))
