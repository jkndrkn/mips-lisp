;;;; Package definitions

(defpackage :com.jkndrkn.iss.assembler
  (:use :common-lisp)
  (:export :compile-asm
	   :r0 :r1 :r2 :r3 :r4 :r5 :r6 :r7 :r8 :r9 :r10 :r11 :r12 :r13 :r14 :r15 :r16
	   :r17 :r18 :r19 :r20 :r21 :r22 :r23 :r24 :r25 :r26 :r27 :r28 :r29 :r30 :r31
	   :label :loop-cache-start :loop-cache-end
	   :add :sub :mul :div
	   :lw :sw :beq :bne :bgt :j :addi))

(defpackage :com.jkndrkn.iss.simulator
  (:use :common-lisp)
  (:export :run
	   :cpi
	   :*debug-mode*
	   :*debug-mode-cache*
	   :*i-mem*
	   :*i-cache-miss-penalty*
	   :*i-cache-size*
	   :*loop-cache-enable*))

(defpackage :com.jkndrkn.iss.optimization-benchmark
  (:use :common-lisp :com.jkndrkn.iss.assembler	:com.jkndrkn.iss.simulator))

;;;; Compile and load system components

(load (compile-file "assembler.lisp"))
(load (compile-file "simulator.lisp"))
(load (compile-file "benchmarks/optimization-benchmark.lisp"))