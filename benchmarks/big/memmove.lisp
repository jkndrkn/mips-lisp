#|

memmove.lisp

John David Eriksen
http://www.jkndrkn.com
john.eriksen@yahoo.com

|#

;; Setup constants
(addi r1 r0 0)      ; base address of A[]
(addi r2 r0 10)     ; size of A[]
(addi r3 r0 0)      ; r3 = i = 0

;; A = [0 .. 9]
(label "init")
(beq r3 "done" r2)  ; if (i = size(A[])) goto done
(sw r3 0 r3)        ; A[i] = i
(addi r3 r3 1)      ; i++
(j "init")
(label "done")

;; A = [1 .. 10]
(addi r3 r0 0)      ; r3 = i = 0
(label "move")
(beq r3 "end" r2)   ; if (i = size(A[])) goto end
(lw r4 0 r3)        ; r4 = A[i]
(addi r4 r4 1)      ; r4 += 1
(sw r4 0 r3)        ; A[i] = r4
(addi r3 r3 1)      ; i++
(j "move")
(label "end")
