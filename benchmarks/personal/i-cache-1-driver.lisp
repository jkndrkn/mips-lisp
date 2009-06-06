(in-package :com.jkndrkn.iss.assembler)
(compile-asm "benchmarks/personal/i-cache-1.lisp")
(in-package :com.jkndrkn.iss.simulator)
(setf *debug-mode* nil)
(setf *debug-mode-cache* nil)

(run "benchmarks/personal/i-cache-1.lisp.mac" "pipeline" 
     :init-conditions #'(lambda () 
			  (setf *i-cache-size* 2)
			  (dotimes (x 32) 
			    (setf (elt *reg* x) x) 
			    (setf (elt *d-mem* x) x))))

(when (not (equalp (cpi) "11.33")) (error (format nil "FAIL: i-cache-1 bad CPI")))
(when (not (eq (elt *reg* 1) 5)) (error (format nil "FAIL: i-cache-1 bad output")))
(when (not (eq (elt *reg* 4) 10)) (error (format nil "FAIL: i-cache-1 bad output")))
(when (not (eq (elt *reg* 6) 12)) (error (format nil "FAIL: i-cache-1 bad output")))
(when (not (eq (getf (elt *i-cache* 0) :addr) 2)) (error (format nil "FAIL: i-cache-1 error")))
(when (not (eq (getf (elt *i-cache* 1) :addr) 1)) (error (format nil "FAIL: i-cache-1 error")))

(when (not (eq *hit-count* 0)) (error (format nil "FAIL: i-cache-1 bad hit-count")))
(when (not (eq *miss-count* 3)) (error (format nil "FAIL: i-cache-1 bad miss-count")))