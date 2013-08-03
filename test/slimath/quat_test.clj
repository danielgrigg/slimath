(ns slimath.quat-test
  (:use midje.sweet
        [slimath core quat])
  (:require [clojure.string :as str]))

(fact "`quat-mul` multiplies two quaternions"
  (quat-mul (quat 3 -1 4 5) (quat  -2 4 3 2))
      => (quat -23 1 33 8))
