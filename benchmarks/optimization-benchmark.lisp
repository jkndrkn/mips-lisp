(in-package :com.jkndrkn.iss.optimization-benchmark)

(defparameter *benchmarks*
  (list "straight" "simple" "busy" "nested"))
;  (list "straight" "simple"))
;  (list "straight"))

(defconstant benchmark-location
  "benchmarks/personal/loop-cache-")

(defconstant benchmark-ext
  ".lisp")

(defconstant benchmark-machine-ext
  ".mac")

(defconstant cache-miss-penalties
  (list 1 10 20))

(defconstant cache-sizes
  (list 'full 'half 'none))
;  (list 'full 'half))

(defconstant loop-cache-activations
;  (list t nil))
  (list t nil))

(defparameter *output* nil)

(defun init ()
    (setf *output* nil))

(defmacro make-init-conditions (loop-cache-activated cache-miss-penalty cache-size &key debug-mode debug-mode-cache)
  `#'(lambda ()
       (setf *debug-mode* ,debug-mode)
       (setf *debug-mode-cache* ,debug-mode-cache)
       (setf *loop-cache-enable* ,loop-cache-activated)
       (cond
	 ((eq ,cache-size 'full)
	  (setf *i-cache-size* (length *i-mem*)))
	 ((eq ,cache-size 'half)
	  (setf *i-cache-size* (ceiling (/ (length *i-mem*) 2))))
	 ((eq ,cache-size 'none)
	  (setf *i-cache-size* 1))
	 (t
	  (error (format nil "Invalid cache size: ~a" ,cache-size))))
       (setf *i-cache-miss-penalty* ,cache-miss-penalty)))

(defun benchmark-run ()
  (init)
  (dolist (benchmark *benchmarks*)
    (let ((benchmark-filename (format nil "~a~a~a" benchmark-location benchmark benchmark-ext)))
      (compile-asm benchmark-filename)
      (dolist (loop-cache-activated loop-cache-activations)
	(dolist (cache-miss-penalty cache-miss-penalties)
	  (dolist (cache-size cache-sizes)
	    (benchmark-exec benchmark-filename loop-cache-activated cache-miss-penalty cache-size)
	    (output-record benchmark loop-cache-activated cache-miss-penalty cache-size (cpi))))))))
	

(defun output-record (benchmark loop-cache-activated cache-miss-penalty cache-size cpi)
  (setf *output* 
	(append *output* (list (list :benchmark benchmark
				     :loop-cache-activated loop-cache-activated
				     :cache-miss-penalty cache-miss-penalty
				     :cache-size cache-size
				     :cpi cpi)))))
			       
(defun output-format (format-string)
  (let ((output-string ""))
    (dolist (entry *output*)
      (setf output-string
	    (format nil format-string
		    output-string
		    (getf entry :benchmark)
		    (getf entry :loop-cache-activated)
		    (getf entry :cache-miss-penalty)
		    (getf entry :cache-size )
		    (getf entry :cpi))))
    output-string))

(defun output-format-chart ()
  (output-format "~a~a, ~a, ~a ~a, ~a~%"))

(defun output-format-raw ()
  (output-format "~a~a, ~a, ~a, ~a, ~a~%"))

(defun output-write (type)
  (let ((output-string ""))
    (cond
      ((eq type 'raw)
       (setf output-string (output-format-raw)))
      ((eq type 'chart)
       (setf output-string (output-format-chart)))
      (t
       (error (format nil "Invalid output format: ~a" type))))
  (with-open-file (out "optimization-benchmark-results.csv"
		       :direction :output
		       :if-exists :supersede)
    (format out "~a" output-string))))
  
	    
(defun benchmark-exec (benchmark-filename loop-cache-activated cache-miss-penalty cache-size)
  (let ((benchmark-machine-filename (format nil "~a~a"  benchmark-filename benchmark-machine-ext)))
    (run benchmark-machine-filename "pipeline"
	 :init-conditions (make-init-conditions loop-cache-activated 
						cache-miss-penalty 
						cache-size 
						:debug-mode nil
						:debug-mode-cache nil))))

