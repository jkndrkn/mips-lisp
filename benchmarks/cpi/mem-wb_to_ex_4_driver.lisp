(in-package :com.jkndrkn.iss.assembler)
(compile-asm "benchmarks/cpi/mem-wb_to_ex_4.lisp")
(in-package :com.jkndrkn.iss.simulator)
(setf *debug-mode* nil)
(run "benchmarks/cpi/mem-wb_to_ex_4.lisp.mac" "pipeline" 
     :init-conditions #'(lambda () 
			  (dotimes (x 32) 
			    (setf (elt *reg* x) x) 
			    (setf (elt *d-mem* x) x))))

(when (not (equalp (cpi) "11.33")) (error (format nil "FAIL: mem-wb_to_ex_4 bad CPI")))
(when (not (eq (elt *reg* 1) 2)) (error (format nil "FAIL: mem-wb_to_ex_4 bad output")))
(when (not (eq (elt *reg* 2) 5)) (error (format nil "FAIL: mem-wb_to_ex_4 bad output")))
