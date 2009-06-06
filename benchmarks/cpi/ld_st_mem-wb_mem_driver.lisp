(in-package :com.jkndrkn.iss.assembler)
(compile-asm "benchmarks/cpi/ld_st_mem-wb_mem.lisp")
(in-package :com.jkndrkn.iss.simulator)
(setf *debug-mode* nil)
(run "benchmarks/cpi/ld_st_mem-wb_mem.lisp.mac" "pipeline" 
     :init-conditions #'(lambda () 
			  (dotimes (x 32) 
			    (setf (elt *reg* x) x) 
			    (setf (elt *d-mem* x) x))))

(when (not (equalp (cpi) "11.33")) (error (format nil "FAIL: ld_st_mem-wb_mem bad CPI")))
(when (not (eq (elt *reg* 1) 2)) (error (format nil "FAIL: ld_st_mem-wb_mem bad output")))
(when (not (eq (elt *reg* 2) 7)) (error (format nil "FAIL: ld_st_mem-wb_mem bad output")))
(when (not (eq (elt *d-mem* 2) 2)) (error (format nil "FAIL: ld_st_mem-wb_mem bad output")))
