(in-package :com.jkndrkn.iss.assembler)
(compile-asm "benchmarks/big/matrix.lisp")
(in-package :com.jkndrkn.iss.simulator)
(setf *debug-mode* nil)
(run "benchmarks/big/matrix.lisp.mac" "pipeline")

(when 
    (not 
     (equalp 
      #(0 0 0 0 0 1 1 1 1 1 2 2 2 2 2 3 3 3 3 3 4 4 4 4 4)
      (subseq *d-mem* 0 25))) 
  (error (format nil "FAIL: matrix D-MEM")))

(when 
    (not 
     (equalp 
      #(0 1 2 3 4 0 1 1 1 4 0 2 2 2 4 0 3 3 3 4 0 1 2 3 4)
      (subseq *d-mem* 25 50))) 
  (error (format nil "FAIL: matrix D-MEM")))
