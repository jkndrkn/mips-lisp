(in-package :com.jkndrkn.iss.assembler)
(compile-asm "benchmarks/personal/stall-recovery.lisp")
(in-package :com.jkndrkn.iss.simulator)
(setf *debug-mode* nil)
(setf *debug-mode-cache* nil)

(run "benchmarks/personal/stall-recovery.lisp.mac" "pipeline" 
     :init-conditions #'(lambda () 
			  (dotimes (x 32) 
			    (setf (elt *reg* x) x) 
			    (setf (elt *d-mem* x) x))))

(when (not (equalp (cpi) "11.25")) (error (format nil "FAIL: stall-recovery bad CPI")))
(when (not (eq (elt *reg* 2) 1)) (error (format nil "FAIL: stall-recovery bad output")))
(when (not (eq (elt *reg* 3) 2)) (error (format nil "FAIL: stall-recovery bad output")))
(when (not (eq (elt *reg* 4) 2)) (error (format nil "FAIL: stall-recovery bad output")))
(when (not (eq (elt *reg* 5) 4)) (error (format nil "FAIL: stall-recovery bad output")))
