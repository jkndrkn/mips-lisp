;;;; Top-level optimization benchmark driver

;; CPI benchmarks provided on project page. Updated to reflect new CPI values.
(load (compile-file "benchmarks/cpi/cpi-benchmark-driver.lisp"))

;; Personal benchmarks
(load (compile-file "benchmarks/personal/personal-benchmark-driver.lisp"))

;; "Big" benchmarks: memmove, array, mult, gcd, matrix, fib, and list-reverse
(load (compile-file "benchmarks/big/big-benchmark-driver.lisp"))
