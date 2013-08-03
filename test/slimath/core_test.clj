(ns slimath.core-test
  (:use midje.sweet
        slimath.core)
  (:require [clojure.string :as str]))

(fact "`qmul` multiplies two quaternions"
  (qmul (quat 3 -1 4 5) (quat  -2 4 3 2))
      => (quat -23 1 33 8))
