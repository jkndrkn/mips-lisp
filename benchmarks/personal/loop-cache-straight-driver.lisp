(in-package :com.jkndrkn.iss.assembler)
(compile-asm "benchmarks/personal/loop-cache-straight.lisp")
(when (not (getf (elt *asm* 0) :loop-cache-start)) (error (format nil "FAIL: loop-cache-straight error 1")))

(in-package :com.jkndrkn.iss.simulator)
(setf *debug-mode* nil)
(setf *debug-mode-cache* nil)

(run "benchmarks/personal/loop-cache-straight.lisp.mac" "pipeline"
     :init-conditions #'(lambda () 
			  (dotimes (x 32) 
			    (setf (elt *reg* x) x) 
			    (setf (elt *d-mem* x) x))))

(when (not (equalp (cpi) "1.80")) (error (format nil "FAIL: loop-cache-straight bad CPI")))
(when (not (eq (elt *reg* 1) 5)) (error (format nil "FAIL: loop-cache-straight bad output")))
(when (not (eq (elt *reg* 2) 8)) (error (format nil "FAIL: loop-cache-straight bad output")))
(when (not (eq (elt *reg* 3) 12)) (error (format nil "FAIL: loop-cache-straight bad output")))
(when (not (eq (elt *reg* 4) 17)) (error (format nil "FAIL: loop-cache-straight bad output")))
(when (not (eq (elt *reg* 5) 23)) (error (format nil "FAIL: loop-cache-straight bad output")))