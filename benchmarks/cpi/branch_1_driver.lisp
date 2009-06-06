(in-package :com.jkndrkn.iss.assembler)
(compile-asm "benchmarks/cpi/branch_1.lisp")
(in-package :com.jkndrkn.iss.simulator)
(setf *debug-mode* nil)
(setf *debug-mode-cache* nil)

(run "benchmarks/cpi/branch_1.lisp.mac" "pipeline"
     :init-conditions #'(lambda ()
			  (setf *i-cache-miss-penalty* i-cache-default-miss-penalty)))

(when (not (equalp (cpi) "18.00")) (error (format nil "FAIL: branch_1 bad CPI")))
(when (not (eq (elt *reg* 1) 0)) (error (format nil "FAIL: branch_1 bad output 1")))
(when (not (eq (elt *reg* 4) 0)) (error (format nil "FAIL: branch_1 bad output 2")))
(when (not (eq (elt *reg* 5) 6)) (error (format nil "FAIL: branch_1 bad output 3")))
(when (not (eq (elt *reg* 6) 0)) (error (format nil "FAIL: branch_1 bad output 4")))