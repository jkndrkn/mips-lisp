(in-package :com.jkndrkn.iss.assembler)
(compile-asm "benchmarks/big/mult.lisp")
(in-package :com.jkndrkn.iss.simulator)
(setf *debug-mode* nil)
(run "benchmarks/big/mult.lisp.mac" "pipeline")

(when 
    (not 
     (equalp 
      #(0 0 0 0 1 1 1 1 2 2 2 2 3 3 3 3)
      (subseq *d-mem* 0 16))) 
  (error (format nil "FAIL: mult D-MEM")))

(when 
    (not 
     (equalp 
      #(0 1 2 3 0 1 2 3 0 1 2 3 0 1 2 3)
      (subseq *d-mem* 16 32))) 
  (error (format nil "FAIL: mult D-MEM")))

(when 
    (not 
     (equalp 
      #(0 0 0 0 0 4 12 24 24 32 48 72 72 84 108 144)
      (subseq *d-mem* 32 48))) 
  (error (format nil "FAIL: mult D-MEM")))
