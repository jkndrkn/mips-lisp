;;;; Nested loop

(addi r10 r0 5)
(addi r11 r0 0)
(addi r12 r0 5)
(addi r13 r0 0)
(addi r14 r0 5)
(addi r15 r0 0)
(addi r17 r0 0)
(loop-cache-start)
(label "loop-3")
(beq r14 "done-3" r15)
(label "loop-2")
(beq r12 "done-2" r13)
(label "loop-1")
(beq r10 "done-1" r11)
(add r1 r2 r3)
(add r2 r1 r3)
(add r3 r2 r4)
(add r4 r3 r5)
(add r5 r4 r6)
(addi r17 r17 1)
(addi r11 r11 1)
(j "loop-1")
(label "done-1")
(addi r11 r0 0)
(addi r13 r13 1)
(j "loop-2")
(label "done-2")
(addi r13 r0 0)
(addi r15 r15 1)
(j "loop-3")
(label "done-3")
(loop-cache-end)
