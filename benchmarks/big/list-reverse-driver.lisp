(in-package :com.jkndrkn.iss.assembler)
(compile-asm "benchmarks/big/list-reverse.lisp")
(in-package :com.jkndrkn.iss.simulator)
(setf *debug-mode* nil)
(run "benchmarks/big/list-reverse.lisp.mac" "pipeline")

(when 
    (not 
     (equalp 
      #(0 1 2 3 4 5 6 7 8 9 9 8 7 6 5 4 3 2 1 0)
      (subseq *d-mem* 0 20))) 
  (error (format nil "FAIL: list-reverse D-MEM")))
