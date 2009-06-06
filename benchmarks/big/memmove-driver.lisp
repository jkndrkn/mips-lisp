(in-package :com.jkndrkn.iss.assembler)
(compile-asm "benchmarks/big/memmove.lisp")
(in-package :com.jkndrkn.iss.simulator)
(setf *debug-mode* nil)
(run "benchmarks/big/memmove.lisp.mac" "pipeline")

(when 
    (not 
     (equalp 
      #(1 2 3 4 5 6 7 8 9 10)
      (subseq *d-mem* 0 10))) 
  (error (format nil "FAIL: memmove D-MEM")))
