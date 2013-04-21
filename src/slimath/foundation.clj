(in-ns 'slimath.core)

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
