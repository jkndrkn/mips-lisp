(defparameter *benchmarks*
  (list "straight" "simple" "busy" "nested"))

(defconstant benchmark-location
  "benchmarks/personal/loop-cache-")

(defconstant benchmakr-ext
  ".lisp")

(dolist (benchmark *benchmarks*)
  (in-package :com.jkndrkn.iss.assembler)
  (in-package :com.jkndrkn.iss.simulator))