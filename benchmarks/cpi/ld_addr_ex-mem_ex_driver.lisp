(in-package :com.jkndrkn.iss.assembler)
(compile-asm "benchmarks/cpi/ld_addr_ex-mem_ex.lisp")
(in-package :com.jkndrkn.iss.simulator)
(setf *debug-mode* nil)
(setf *debug-mode-cache* nil)

(run "benchmarks/cpi/ld_addr_ex-mem_ex.lisp.mac" "pipeline" 
     :init-conditions #'(lambda ()
			  (dotimes (x 32) 
			    (setf (elt *reg* x) x) 
			    (setf (elt *d-mem* x) x))))

(when (not (equalp (cpi) "11.33")) (error (format nil "FAIL: ld_addr_ex-mem_ex bad CPI")))
(when (not (eq (elt *reg* 1) 5)) (error (format nil "FAIL: ld_addr_ex-mem_ex bad output")))
(when (not (eq (elt *reg* 4) 12)) (error (format nil "FAIL: ld_addr_ex-mem_ex bad output")))
(when (not (eq (elt *reg* 6) 5)) (error (format nil "FAIL: ld_add_ex-mem_ex bad output")))