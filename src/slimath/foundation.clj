(in-ns 'slimath.core)

(defmacro str-sym- "Symbolise a sequence of tokens" [& args] `(symbol (str ~@args)))

