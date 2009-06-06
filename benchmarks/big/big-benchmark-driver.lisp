;;;; "Big" benchmarks used to verify that the architecture is capable of performing useful work

;; Provided benchmarks
(load (compile-file "benchmarks/big/memmove-driver.lisp"))
(load (compile-file "benchmarks/big/array-driver.lisp"))
(load (compile-file "benchmarks/big/mult-driver.lisp"))
(load (compile-file "benchmarks/big/gcd-driver.lisp"))
(load (compile-file "benchmarks/big/matrix-driver.lisp"))

;; Custom benchmarks
(load (compile-file "benchmarks/big/fib-driver.lisp"))
(load (compile-file "benchmarks/big/list-reverse-driver.lisp"))