#|

fib.lisp

John David Eriksen
http://www.jkndrkn.com
john.eriksen@yahoo.com

Non-recursive implementation of Fibonacci sequence
computation that deposits the 1st through the 10th
Fibonacci numbers sequentially in memory. 

This benchmark exercises branching, alu, 
and memory instructions.

|#

;; Setup constants
(addi r1 r0 10)     ; array_size
(addi r2 r0 0)      ; A[].base_offset

;; Calculate fib(n) where n = [0 .. 9] and store sequentially in A[]
(addi r5 r0 0)      ; r5 = i = 0
(label "loop_i")

(addi r3 r0 1)      ; r3 = a = 1
(addi r4 r0 0)      ; r4 = b = 1
(addi r7 r0 0)      ; r7 = result = 0

(addi r6 r0 0)      ; r5 = j = 0
(label "loop_j")
(beq r5 "done" r6)  ; if (i == j) goto done
(add r7 r3 r4)      ; result = a + b
(addi r3 r4 0)      ; a = b
(addi r4 r7 0)      ; b = result
(addi r6 r6 1)      ; j++
(j "loop_j")        ; goto loop_b
(label "done")

(add r8 r2 r5)      ; r8 = i + A[].base_offset
(sw r7 0 r8)        ; A[i] = result

(addi r5 r5 1)      ; i++
(bne r5 "loop_i" r1)   ; if (i != array_size) goto loop_a
