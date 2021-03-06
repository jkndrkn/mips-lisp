(in-package :com.jkndrkn.iss.assembler)
(compile-asm "benchmarks/personal/loop-cache-simple.lisp")
(when (not (getf (elt *asm* 2) :loop-cache-start)) (error (format nil "FAIL: loop-cache-simple error 1")))

(in-package :com.jkndrkn.iss.simulator)
(setf *debug-mode* nil)
(setf *debug-mode-cache* nil)

(run "benchmarks/personal/loop-cache-simple.lisp.mac" "pipeline"
     :init-conditions #'(lambda () 
			  (dotimes (x 32) 
			    (setf (elt *reg* x) x) 
			    (setf (elt *d-mem* x) x))))

(when (not (equalp (cpi) "1.51")) (error (format nil "FAIL: loop-cache-simple bad CPI")))
