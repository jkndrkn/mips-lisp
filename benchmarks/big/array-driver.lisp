(in-package :com.jkndrkn.iss.assembler)
(compile-asm "benchmarks/big/array.lisp")
(in-package :com.jkndrkn.iss.simulator)
(setf *debug-mode* nil)
(run "benchmarks/big/array.lisp.mac" "pipeline")

(when 
    (not 
     (equalp 
      #(0 2 4 6 8 0 1 2 3 4 0 2 4 6 8 0 3 6 9 12 0 1 2 3 4)
      (subseq *d-mem* 0 25))) 
  (error (format nil "FAIL: array D-MEM")))
