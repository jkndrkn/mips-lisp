(in-package :com.jkndrkn.iss.assembler)
(compile-asm "benchmarks/personal/i-cache-2.lisp")
(in-package :com.jkndrkn.iss.simulator)
(setf *debug-mode* nil)
(setf *debug-mode-cache* nil)

(run "benchmarks/personal/i-cache-2.lisp.mac" "pipeline"
     :init-conditions #'(lambda () 
			  (setf *i-cache-size* 7)
			  (dotimes (x 32) 
			    (setf (elt *reg* x) x) 
			    (setf (elt *d-mem* x) x))))

(when (not (equalp (cpi) "6.33")) (error (format nil "FAIL: i-cache-2 bad CPI")))
(when (not (eq (elt *reg* 1) 10)) (error (format nil "FAIL: i-cache-2 bad output")))
(when (not (eq (elt *reg* 3) 0)) (error (format nil "FAIL: i-cache-2 bad output")))
(when (not (eq (elt *reg* 4) 11)) (error (format nil "FAIL: i-cache-2 bad output")))
(when (not (eq (elt *reg* 5) 1)) (error (format nil "FAIL: i-cache-2 bad output")))
(when (not (eq (elt *reg* 6) 17)) (error (format nil "FAIL: i-cache-2 bad output")))

(when (not (eq (getf (elt *i-cache* 0) :addr) 7)) (error (format nil "FAIL: i-cache-2 error")))
(when (not (eq (getf (elt *i-cache* 1) :addr) 1)) (error (format nil "FAIL: i-cache-2 error")))
(when (not (eq (getf (elt *i-cache* 2) :addr) 2)) (error (format nil "FAIL: i-cache-2 error")))
(when (not (eq (getf (elt *i-cache* 3) :addr) 3)) (error (format nil "FAIL: i-cache-2 error")))
(when (not (eq (getf (elt *i-cache* 4) :addr) 4)) (error (format nil "FAIL: i-cache-2 error")))
(when (not (eq (getf (elt *i-cache* 5) :addr) 5)) (error (format nil "FAIL: i-cache-2 error")))
(when (not (eq (getf (elt *i-cache* 6) :addr) 6)) (error (format nil "FAIL: i-cache-2 error")))

(when (not (eq *hit-count* 9)) (error (format nil "FAIL: i-cache-2 bad hit-count")))
(when (not (eq *miss-count* 8)) (error (format nil "FAIL: i-cache-2 bad miss-count")))