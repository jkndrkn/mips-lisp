(addi r3 r0 2)
(addi r5 r0 1)
(label "loop")
(beq r3 "done" r0)
(add r1 r2 r8)
(add r4 r1 r5)
(add r6 r1 r7)
(sub r3 r3 r5)
(j "loop")
(label "done")