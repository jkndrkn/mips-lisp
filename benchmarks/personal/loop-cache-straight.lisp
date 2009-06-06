;;;; Straight-line non-branching block of code

(loop-cache-start)
(add r1 r2 r3)
(add r2 r1 r3)
(add r3 r2 r4)
(add r4 r3 r5)
(add r5 r4 r6)
(loop-cache-end)
