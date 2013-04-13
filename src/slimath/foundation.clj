(in-ns 'slimath.core)

(defmacro str-sym- "Symbolise a sequence of tokens" [& args] `(symbol (str ~@args)))

(defmacro bench [n & exprs]
  `(time
     (dotimes [~'_ ~n]
       (do ~@exprs))))