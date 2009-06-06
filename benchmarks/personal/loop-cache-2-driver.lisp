(in-package :com.jkndrkn.iss.assembler)
(compile-asm "benchmarks/personal/loop-cache-2.lisp")
(when (not (getf (elt *asm* 2) :loop-cache-start)) (error (format nil "FAIL: loop-cache-1 error 1")))
(when (not (getf (elt *asm* 8) :loop-cache-end)) (error (format nil "FAIL: loop-cache-1 error 2")))

(in-package :com.jkndrkn.iss.simulator)
(setf *debug-mode* nil)
(setf *debug-mode-cache* nil)

(run "benchmarks/personal/loop-cache-2.lisp.mac" "pipeline"
     :init-conditions #'(lambda () 
			  (dotimes (x 32) 
			    (setf (elt *reg* x) x) 
			    (setf (elt *d-mem* x) x))))

(when (not (equalp (cpi) "3.31")) (error (format nil "FAIL: loop-cache-2 bad CPI")))
(when (not (eq (elt *reg* 1) 10)) (error (format nil "FAIL: loop-cache-2 bad output")))
(when (not (eq (elt *reg* 3) 0)) (error (format nil "FAIL: loop-cache-2 bad output")))
(when (not (eq (elt *reg* 4) 11)) (error (format nil "FAIL: loop-cache-2 bad output")))
(when (not (eq (elt *reg* 5) 1)) (error (format nil "FAIL: loop-cache-2 bad output")))
(when (not (eq (elt *reg* 6) 17)) (error (format nil "FAIL: loop-cache-2 bad output")))
(when (not (eq (elt *reg* 7) 18)) (error (format nil "FAIL: loop-cache-2 bad output")))