(ns slimath.quat-test
  (:use midje.sweet
        [slimath core vec quat])
  (:require [clojure.string :as str]))

(fact "`quat-mul` multiplies two quaternions"
  (quat-mul (quat 3 -1 4 5) (quat  -2 4 3 2))
      => (quat -23 1 33 8))

(fact "`slerp` slerps two quaternions"
      (v4approx? 
       (slerp 0.6 (v4normalize(quat 1 0 0 1)) (v4normalize(quat 1 1 0 1)))
       [0.6594369247750476 0.36095136027788577 0.0 0.6594369247750476])
      => true)

(fact "`squad` slerpy-slerp quaternions"
      (v4approx? (squad 0.6 
                        (v4normalize(quat 1 0 0 1))
                        (v4normalize(quat 1 0 1 1))
                        (v4normalize(quat 0 1 1 1))
                        (v4normalize(quat 1 1 0 1)))
                 [0.501718 0.396281 0.326893 0.695976])
      => true)

(fact "`quat-rotation-vectors` rotation between vectors"
      (v4approx? 
       (quat-rotation-vectors 
        (vec3 1 0.5 0.3)
        (vec3 0.7 -0.2 2))
       [0.50533517 -0.85334901 -0.26220221 1.04880884]) 
      => true)

(fact "`quat-rotation-axis` axis rotation as quaternion"
      (v4approx? 
       (quat-rotation-axis 1.0 (v3normalize (vec3 1 1 1)))
      [0.276796 0.276796 0.276796 0.877583])
      => true)

(fact "`quat-mul` multiply quaternions"
      (quat-mul (quat 1 -2 3 5) (quat 4 3 -1 1))
      => [14.000000 26.000000 9.000000 10.000000])

(fact "`quat-rotate-vector` rotate vector by quaternion"
      (quat-rotate-vector (quat 2 -1 3 1) (vec3 -1 1 2)) 
      => [15.000000 -33.000000 -6.000000 ]
      (quat-rotate-vector (quat 8 22 -31 11) (vec3 -11 13 17))
      => [27098.0 -28010.0 4414.0])