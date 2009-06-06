#|

mult.lisp

John David Eriksen
http://www.jkndrkn.com
john.eriksen@yahoo.com

|#

;; Setup constants
(addi r1 r0 4)      ; matrix_size
(addi r2 r0 0)      ; A[][].base_address
(addi r3 r0 16)     ; B[][].base_address
(addi r4 r0 32)     ; C[][].base_address

;; Initialize A[][] and B[][]
(addi r5 r0 0)      ; r5 = i = 0
(label "row")      
(addi r6 r0 0)      ; r6 = j = 0
(label "col")
(beq r6 "end-col" r1)   ; if (j == matrix_size) goto end-col
(mul r7 r5 r1)      ; r7 = (i * matrix_size)
(add r8 r6 r7)      ; r8 = r7 + j
(add r9 r8 r2)      ; r9 = A[][].base_address + r8
(add r10 r8 r3)     ; r10 = B[][].base_address + r8
(sw r5 0 r9)        ; A[i][j] = i
(sw r6 0 r10)       ; B[i][j] = j
(addi r6 r6 1)      ; j++
(j "col")
(label "end-col")
(addi r5 r5 1)      ; i++
(bne r5 "row" r1)   ; if (i <= matrix_size) goto row

;; Compute values for C
(addi r16 r0 0)     ; r16 = blah = 0
(addi r5 r0 0)      ; r5 = i = 0
(label "loop-i")      
(beq r5 "end-loop-i" r1)   ; if (i == matrix_size) goto end-loop-i
(addi r6 r0 0)      ; r6 = j = 0
(label "loop-j")
(beq r6 "end-loop-j" r1)   ; if (j == matrix_size) goto end-loop-j
(addi r7 r0 0)      ; r7 = k = 0
(label "loop-k")
(beq r7 "end-loop-k" r1)   ; if (k == matrix_size) goto end-loop-k

(mul r8 r5 r1)      ; r8 = (i * matrix_size)
(mul r9 r7 r1)      ; r9 = (k * matrix_size)
(add r10 r8 r7)     ; r10 = r8 + k
(add r11 r9 r6)     ; r11 = r9 + j
(add r12 r2 r10)    ; r12 = A[][].base_address + r10
(add r13 r3 r11)    ; r13 = B[][].base_address + r11
(lw r14 0 r12)      ; r14 = A[i][k]
(lw r15 0 r13)      ; r15 = B[k][j]
(mul r17 r14 r15)   ; r17 = r14 * r15
(add r16 r16 r17)   ; r18 = blah + r17
(add r19 r8 r6)     ; r19 = r8 + j
(add r20 r4 r19)    ; r20 = C[][].base_address + r19
(sw r16 0 r20)      ; C[i][j] = blah

(addi r7 r7 1)      ; k++
(j "loop-k")
(label "end-loop-k")
(addi r6 r6 1)      ; j++
(j "loop-j")
(label "end-loop-j")
(addi r5 r5 1)      ; i++
(j "loop-i")
(label "end-loop-i")
