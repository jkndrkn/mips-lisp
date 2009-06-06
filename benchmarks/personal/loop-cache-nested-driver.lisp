(in-package :com.jkndrkn.iss.assembler)
(compile-asm "benchmarks/personal/loop-cache-nested.lisp")
(when (not (getf (elt *asm* 7) :loop-cache-start)) (error (format nil "FAIL: loop-cache-nested error 1")))

(in-package :com.jkndrkn.iss.simulator)
(setf *debug-mode* nil)
(setf *debug-mode-cache* nil)

(run "benchmarks/personal/loop-cache-nested.lisp.mac" "pipeline"
     :init-conditions #'(lambda () 
			  (setf *i-cache-miss-penalty* 100)
			  (setf *i-cache-size* (length *i-mem*))
			  (print *i-cache-size*)
			  (dotimes (x 32) 
			    (setf (elt *reg* x) x) 
			    (setf (elt *d-mem* x) x))))

;(when (not (equalp (cpi) "1.34")) (error (format nil "FAIL: loop-cache-nested bad CPI")))