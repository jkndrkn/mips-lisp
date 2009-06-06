#|

list-reverse.lisp

John David Eriksen
http://www.jkndrkn.com
john.eriksen@yahoo.com

A list is stored at memory location A via SW commands. 
Then, a series of LW and SW commands duplicate
the list at memory location B in reverse order. This
benchmark exercises branching, alu, and memory instructions.

Note: the looping construct in use here is functionally equivalent to a do-while loop.
Hence, this benchmark will fail for lists of size zero.

|#

;; Setup constants
(addi r1 r0 10)     ; array_size
(addi r2 r0 0)      ; A[].base_offset
(add r3 r0 r1)      ; B[].base_offset

;; Initialize A to value [0 .. 9]
(addi r5 r0 0)      ; r5 = i = 0
(label "loop_i")
(add r7 r2 r5)      ; r7 = A[].base_offset + i
(sw r5 0 r7)        ; A[r7] = i
(addi r5 r5 1)      ; i++
(bne r5 "loop_i" r1)   ; if (i != array_size) goto loop_i

;; Set B to the reversed value of A
(addi r5 r0 0)      ; r5 = i = 0
(label "rev")
(add r7 r2 r5)      ; r7 = A[].base_offset + i
(lw r8 0 r7)        ; r8 = A[r7]
(addi r9 r1 -1)       
(sub r9 r9 r5)
(add r9 r9 r3)      ; r9 = array_size - i - i + B[].base_offset
(sw r8 0 r9)        ; B[r9] = A[r7]
(addi r5 r5 1)      ; i++
(bne r5 "rev" r1)   ; if (i != array_size) goto rev
