(in-package :com.jkndrkn.iss.assembler)
(compile-asm "benchmarks/cpi/ex-mem_to_ex_2.lisp")
(in-package :com.jkndrkn.iss.simulator)
(setf *debug-mode* nil)
(run "benchmarks/cpi/ex-mem_to_ex_2.lisp.mac" "pipeline" 
     :init-conditions #'(lambda () 
			  (dotimes (x 32) 
			    (setf (elt *reg* x) x) 
			    (setf (elt *d-mem* x) x))))

(when (not (equalp (cpi) "11.33")) (error (format nil "FAIL: ex-mem_to_ex_1 bad CPI")))
(when (not (eq (elt *reg* 1) 5)) (error (format nil "FAIL: ex-mem_to_ex_2 bad output")))
(when (not (eq (elt *reg* 4) 7)) (error (format nil "FAIL: ex-mem_to_ex_2 bad output")))
(when (not (eq (elt *reg* 6) 15)) (error (format nil "FAIL: ex-mem_to_ex_2 bad output")))
