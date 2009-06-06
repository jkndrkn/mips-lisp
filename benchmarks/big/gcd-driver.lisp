(in-package :com.jkndrkn.iss.assembler)
(compile-asm "benchmarks/big/gcd.lisp")
(in-package :com.jkndrkn.iss.simulator)
(setf *debug-mode* nil)
(run "benchmarks/big/gcd.lisp.mac" "pipeline")

(when (not (eq (elt *reg* 1) 2)) (error (format nil "FAIL: gcd bad output")))
(when (not (eq (elt *reg* 2) 2)) (error (format nil "FAIL: gcd bad output")))
