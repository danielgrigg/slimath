(ns slimath.core
  (:require [clojure.math.numeric-tower :as numeric])
  (:use [clojure.pprint :only [pprint]]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* true)

(load "foundation")
(load "constants")
(load "scalar")
(load "random")
(load "vec")
(load "matrix")
(load "matrix-vec-ops")

