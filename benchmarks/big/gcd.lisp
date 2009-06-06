#|

gcd.lisp

John David Eriksen
http://www.jkndrkn.com
john.eriksen@yahoo.com

|#

;; Setup constants
(addi r1 r0 8)
(addi r2 r0 6)

;; GCD Algorithm. Output deposited in r1 and r2.
(label "loop")
(beq r1 "done" r2)      ; if (x == y) goto done
(bgt r2 "if-else" r1)   ; if (y > x) goto if-else
(sub r2 r2 r1)          ; y = y - x
(j "end-inner")         ; goto end-inner
(label "if-else")       
(sub r1 r1 r2)          ; x = x -y
(label "end-inner")     
(j "loop")              ; goto loop
(label "done")
