(in-package :com.jkndrkn.iss.assembler)
(compile-asm "benchmarks/cpi/load_delay_3.lisp")
(in-package :com.jkndrkn.iss.simulator)
(setf *debug-mode* nil)
(setf *debug-mode-cache* nil)

(run "benchmarks/cpi/load_delay_3.lisp.mac" "pipeline"
     :init-conditions #'(lambda ()
			  (dotimes (x 32) 
			    (setf (elt *reg* x) x) 
			    (setf (elt *d-mem* x) x))))

(when (not (equalp (cpi) "12.50")) (error (format nil "FAIL: load_delay_3 bad CPI")))
(when (not (eq (elt *reg* 1) 2)) (error (format nil "FAIL: load_delay_3 bad output")))
(when (not (eq (elt *d-mem* 2) 3)) (error (format nil "FAIL: load_delay_3 bad output")))