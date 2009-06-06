(in-package :com.jkndrkn.iss.assembler)
(compile-asm "benchmarks/cpi/load_reg_3.lisp")
(in-package :com.jkndrkn.iss.simulator)
(setf *debug-mode* nil)
(run "benchmarks/cpi/load_reg_3.lisp.mac" "pipeline" 
     :init-conditions #'(lambda () 
			  (dotimes (x 32) 
			    (setf (elt *reg* x) x) 
			    (setf (elt *d-mem* x) x))))

(when (not (equalp (cpi) "10.80")) (error (format nil "FAIL: load_reg_3 bad CPI")))
(when (not (eq (elt *reg* 1) 2)) (error (format nil "FAIL: load_reg_3 bad output")))
(when (not (eq (elt *reg* 2) 7)) (error (format nil "FAIL: load_reg_3 bad output")))
(when (not (eq (elt *reg* 5) 13)) (error (format nil "FAIL: load_reg_3 bad output")))
(when (not (eq (elt *reg* 8) 4)) (error (format nil "FAIL: load_reg_3 bad output")))
