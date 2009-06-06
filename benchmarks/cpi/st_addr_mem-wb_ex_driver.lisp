(in-package :com.jkndrkn.iss.assembler)
(compile-asm "benchmarks/cpi/st_addr_mem-wb_ex.lisp")
(in-package :com.jkndrkn.iss.simulator)
(setf *debug-mode* nil)
(run "benchmarks/cpi/st_addr_mem-wb_ex.lisp.mac" "pipeline" 
     :init-conditions #'(lambda () 
			  (dotimes (x 32) 
			    (setf (elt *reg* x) x) 
			    (setf (elt *d-mem* x) x))))

(when (not (equalp (cpi) "11.33")) (format t "FAIL: st_addr_mem-wb_ex bad CPI"))
(when (not (eq (elt *reg* 1) 5)) (format t "FAIL: st_addr_mem-wb_ex bad output"))
(when (not (eq (elt *reg* 4) 11)) (format t "FAIL: st_addr_mem-wb_ex bad output"))
(when (not (eq (elt *d-mem* 2) 3)) (format t "FAIL: st_addr_mem-wb_ex bad output"))

