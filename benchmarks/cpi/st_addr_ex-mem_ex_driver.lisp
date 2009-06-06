(in-package :com.jkndrkn.iss.assembler)
(compile-asm "benchmarks/cpi/st_addr_ex-mem_ex.lisp")
(in-package :com.jkndrkn.iss.simulator)
(setf *debug-mode* nil)
(run "benchmarks/cpi/st_addr_ex-mem_ex.lisp.mac" "pipeline" 
     :init-conditions #'(lambda () 
			  (dotimes (x 32) 
			    (setf (elt *reg* x) x) 
			    (setf (elt *d-mem* x) x))))

(when (not (equalp (cpi) "11.33")) (error (format nil "FAIL: st_addr_ex-mem_ex bad CPI")))
(when (not (eq (elt *reg* 1) 5)) (error (format nil "FAIL: st_addr_ex-mem_ex bad output")))
(when (not (eq (elt *d-mem* 5) 3)) (error (format nil "FAIL: st_addr_ex-mem_ex bad output")))
