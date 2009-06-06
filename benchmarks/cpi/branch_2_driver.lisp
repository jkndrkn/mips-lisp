(in-package :com.jkndrkn.iss.assembler)
(compile-asm "benchmarks/cpi/branch_2.lisp")
(in-package :com.jkndrkn.iss.simulator)
(setf *debug-mode* nil)
(setf *debug-mode-cache* nil)

(run "benchmarks/cpi/branch_2.lisp.mac" "pipeline"
     :init-conditions #'(lambda ()
			  (setf *i-cache-miss-penalty* i-cache-default-miss-penalty)))

(when (not (equalp (cpi) "16.00")) (error (format nil "FAIL: branch_2 bad CPI")))
(when (not (eq (elt *reg* 1) 0)) (error (format nil "FAIL: branch_2 bad output")))
(when (not (eq (elt *reg* 5) 6)) (error (format nil "FAIL: branch_2 bad output")))
