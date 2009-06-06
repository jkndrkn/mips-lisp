(in-package :com.jkndrkn.iss.assembler)
(compile-asm "benchmarks/big/fib.lisp")
(in-package :com.jkndrkn.iss.simulator)
(setf *debug-mode* nil)
(run "benchmarks/big/fib.lisp.mac" "pipeline")

(when 
    (not (equalp 
	  #(0 10 0 21 34 10 9 34 9 0) 
	  (subseq *reg* 0 10))) 
  (error (format nil "FAIL: fib REG")))

(when 
    (not (equalp 
	  #(0 1 1 2 3 5 8 13 21 34)
	  (subseq *d-mem* 0 10))) 
  (error (format nil "FAIL: fib D-MEM")))
