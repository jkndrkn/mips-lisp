(in-package :com.jkndrkn.iss.assembler)
(compile-asm "benchmarks/personal/loop-cache-1.lisp")
(when (getf (elt *asm* 0) :loop-cache-start) (error (format nil "FAIL: loop-cache-1 error 1")))
(when (getf (elt *asm* 0) :loop-cache-end) (error (format nil "FAIL: loop-cache-1 error 2")))
(when (not (getf (elt *asm* 1) :loop-cache-start)) (error (format nil "FAIL: loop-cache-1 error 3")))
(when (getf (elt *asm* 1) :loop-cache-end) (error (format nil "FAIL: loop-cache-1 error 4")))
(when (getf (elt *asm* 2) :loop-cache-start) (error (format nil "FAIL: loop-cache-1 error 5")))
(when (not (getf (elt *asm* 2) :loop-cache-end)) (error (format nil "FAIL: loop-cache-1 error 6")))

(in-package :com.jkndrkn.iss.simulator)
(setf *debug-mode* nil)
(setf *debug-mode-cache* nil)

(run "benchmarks/personal/loop-cache-1.lisp.mac" "pipeline" 
     :init-conditions #'(lambda () 
			  (dotimes (x 32) 
			    (setf (elt *reg* x) x) 
			    (setf (elt *d-mem* x) x))))

(when (not (eq (elt *reg* 1) 2)) (error (format nil "FAIL: loop-cache-1 bad output")))
(when (not (eq (elt *reg* 4) 12)) (error (format nil "FAIL: loop-cache-1 bad output")))
(when (not (eq (elt *reg* 5) 13)) (error (format nil "FAIL: loop-cache-1 bad output")))