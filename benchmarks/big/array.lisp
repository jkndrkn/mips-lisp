#|

array.lisp

John David Eriksen
http://www.jkndrkn.com
john.eriksen@yahoo.com

|#

;; Initialize constants
(addi r1 r0 5)      ; array_size
(addi r2 r0 0)      ; A[].base_address
(add r14 r2 r1)
(add r3 r0 r14)     ; B[].base_address
(add r14 r3 r1)
(add r4 r0 r14)     ; C[].base_address
(add r14 r4 r1)
(add r5 r0 r14)     ; D[].base_address
(add r14 r5 r1)
(add r6 r0 r14)     ; E[].base_address

;; Initialize array values
(addi r7 r0 0)      ; i
(label "init")
(beq r7 "done-init" r1)  ; if (i == array_size) goto done-init
(add r8 r7 r2)      ; loc = A[].base_address + i
(sw r7 0 r8)        ; A[loc] = loc
(add r8 r7 r3)      ; loc = B[].base_address + i
(sw r7 0 r8)        ; B[loc] = loc
(add r8 r7 r4)      ; loc = C[].base_address + i
(sw r7 0 r8)        ; C[loc] = loc
(add r8 r7 r5)      ; loc = D[].base_address + i
(sw r7 0 r8)        ; D[loc] = loc
(add r8 r7 r6)      ; loc = E[].base_address + i
(sw r7 0 r8)        ; E[loc] = loc
(addi r7 r7 1)      ; i++
(j "init")          ; goto init
(label "done-init")

;; C[i] = A[i] + B[i]
(addi r7 r0 0)      ; r7 = i = 0
(label "loop-1")
(beq r7 "done-loop-1" r1) ; if (i == array_size) goto done-loop-1
(add r8 r7 r2)      ; a_loc = A[].base_address + 1
(add r9 r7 r3)      ; b_loc = B[].base_address + 1
(add r10 r7 r4)     ; c_loc = C[].base_address + 1
(lw r11 0 r8)       ; r11 = A[a_loc]
(lw r12 0 r9)       ; r12 = B[b_loc]
(add r13 r11 r12)   ; r13 = r11 + r12
(sw r13 0 r10)      ; C[c_loc] = r13
(addi r7 r7 1)      ; i++
(j "loop-1")        ; goto loop-1
(label "done-loop-1")


;; D[i] = C[i] + E[i]
(addi r7 r0 0)      ; r7 = i = 0
(label "loop-2")
(beq r7 "done-loop-2" r1); if (i == array_size) goto done-loop-2
(add r8 r7 r4)      ; c_loc = C[].base_address + 1
(add r9 r7 r5)      ; d_loc = D[].base_address + 1
(add r10 r7 r6)     ; e_loc = E[].base_address + 1
(lw r11 0 r8)       ; r11 = C[c_loc]
(lw r12 0 r10)      ; r12 = D[d_loc]
(add r13 r11 r12)   ; r13 = r11 + r12
(sw r13 0 r9)       ; D[d_loc] = r13
(addi r7 r7 1)      ; i++
(j "loop-2")
(label "done-loop-2")

;; A[i] = A[i] + B[i]
(addi r7 r0 0)      ; r7 = i = 0
(label "loop-3")
(beq r7 "done-loop-3" r1) ; if (i == array_size) goto done-loop-3
(add r8 r7 r2)      ; a_loc = A[].base_address + 1
(add r9 r7 r3)      ; b_loc = B[].base_address + 1
(lw r11 0 r8)       ; r11 = A[a_loc]
(lw r12 0 r9)       ; r12 = B[b_loc]
(add r13 r11 r12)   ; r13 = r11 + r12
(sw r13 0 r8)       ; A[a_loc] = r13
(addi r7 r7 1)      ; i++
(j "loop-3")
(label "done-loop-3")
