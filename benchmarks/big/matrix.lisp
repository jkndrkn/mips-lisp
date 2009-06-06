#|

matrix.lisp

John David Eriksen
http://www.jkndrkn.com
john.eriksen@yahoo.com

|#

;; Setup constants
(addi r1 r0 5)     ; matrix_size
(addi r2 r0 0)     ; A[][].base_address
(addi r3 r0 25)    ; B[][].base_address

;; Initialize A[][] and B[][]
(addi r5 r0 0)      ; r5 = i = 0
(label "row")      
(beq r5 "end-row" r1)   ; if (i <= matrix_size) goto end-row
(addi r6 r0 0)      ; r6 = j = 0
(label "col")
(beq r6 "end-col" r1)   ; if (i == matrix_size) goto end-col
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
(j "row")
(label "end-row")

;; Compute values for B
(addi r5 r0 1)      ; r5 = i = 1
(label "loop-i")      
(addi r6 r0 1)      ; r6 = j = 1

(label "loop-j")

(addi r7 r5 -1)     ; r7 = i - 1
(mul r7 r7 r1)      ; r7 = matrix_size * (i - 1)
(add r7 r7 r6)      ; r7 = j + (matrix_size * (i - 1))
(add r7 r7 r2)      ; r7 = A[][].base_address + j + (matrix_size * (i - 1))

(addi r9 r5 1)      ; r9 = i + 1
(mul r9 r9 r1)      ; r9 = matrix_size * (i + 1)
(add r9 r9 r6)      ; r9 = j + (matrix_size * (i + 1))
(add r9 r9 r2)      ; r9 = A[][].base_address + j + (matrix_size * (i + 1))

(mul r8 r5 r1)      ; r8 = matrix_size * i
(add r8 r8 r6)      ; r8 = j + (matrix_size * i)
(addi r8 r8 1)      ; r8 = (j + 1) + (matrix_size * i)
(add r8 r8 r2)      ; r8 = A[][].base_address + (j + 1) + (matrix_size * i)

(mul r10 r5 r1)     ; r10 = matrix_size * i
(add r10 r10 r6)    ; r10 = j + (matrix_size * i)
(addi r10 r10 -1)   ; r10 = (j - 1) + (matrix_size * i)
(add r10 r10 r2)    ; r10 = A[][].base_address + (j - 1) + (matrix_size * i)

(lw r12 0 r7)       ; r12 = A(i - 1, j)
(lw r13 0 r9)       ; r13 = A(i + 1, j)
(lw r14 0 r8)       ; r14 = A(i, j + 1)
(lw r15 0 r10)      ; r15 = A(i, j - 1)

(add r16 r12 r13)
(add r16 r16 r14)
(add r16 r16 r15)   ; r16 = r12 + r13 + r14 + r15
(addi r17 r0 4)     ; r17 = 4
(div r16 r16 r17)   ; r16 = r16 / 4

(mul r18 r5 r1)     ; r18 = matrix_size * i
(add r18 r18 r6)    ; r18 = j + (matrix_size * i)
(add r18 r18 r3)    ; r18 = B[][].array_offset + j + (matrix_size * i)
(sw r16 0 r18)      ; B[i][j] = r16

(addi r19 r1 -1)       ; r19 = matrix_size - 1
(addi r6 r6 1)         ; j++
(bne r6 "loop-j" r19)  ; if (j != (matrix_size - 1)) goto loop-j
(addi r5 r5 1)         ; i++
(bne r5 "loop-i" r19)  ; if (i != (matrix_size - 1)) goto loop-i
