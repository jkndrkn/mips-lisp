#|

simulator.lisp

John David Eriksen
http://www.jkndrkn.com
john.eriksen@yahoo.com

Usage:

> (run "your-machine-code-file.lisp.mac" 'pipeline)

To toggle verbose debugging:

> (setf *debug-mode* t)   ; On
> (setf *debug-mode* nil) ; Off

To view the output of a calculation:

> (reg-print)       ; Print register file.
> (d-mem-print x y) ; Print data memory, where x and y are integers 
                    ; and x is the number of columns and y is the 
                    ; number of rows to be viewed.

|#

;; Assign all names in this file to the package com.jkndrkn.iss.simulator
(in-package :com.jkndrkn.iss.simulator)

(defclass pipeline-register ()
  ((name
    :initarg :name
    :initform (error "Must supply a pipeline register name")
    :accessor name
    :documentation "Textual representation useful when printing a log of system activity")
  (ir
    :initarg :ir
    :initform nil
    :accessor ir
    :documentation "The instruction register. Passes the instruction from stage to stage."))
  (:documentation "Parent class of all pipeline registers"))

(defclass if-id (pipeline-register)
  ((npc
    :initarg :npc
    :initform 0
    :accessor npc
    :documentation "Holds new program count"))
  (:documentation "Pipeline register that holds values generated in the IF phase and delivers them to the ID phase"))

(defclass id-ex (pipeline-register)
  ((a
    :initarg :a
    :initform 0
    :accessor a
    :documentation "Temporary register A")
   (b
    :initarg :b
    :initform 0
    :accessor b
    :documentation "Temporary register B")
   (npc
    :initarg :npc
    :initform 0
    :accessor npc
    :documentation "Holds new program count")
   (imm
    :initarg :imm
    :initform 0
    :accessor imm
    :documentation "Holds immediate address value"))
  (:documentation "Pipeline register that holds values generated in ID phase and passed on to EX phase"))

(defclass ex-mem (pipeline-register)
  ((alu-out
    :initarg :alu-out
    :initform 0
    :accessor alu-out
    :documentation "Holds ouput of ALU operations")
   (b
    :initarg :b
    :initform 0
    :accessor b
    :documentation "Holds output of temporary register B passed through the ID/EX pipeline")
   (branch-taken
    :initarg :branch-taken
    :initform nil
    :accessor branch-taken
    :documentation "Boolean flag indicating if the instruction is a taken branch"))
  (:documentation "Pipeline register that holds values generated in the EX phase and passes them to the MEM phase"))

(defclass mem-wb (pipeline-register)
  ((alu-out
    :initarg :alu-out
    :initform 0
    :accessor alu-out
    :documentation "Holds ouput of ALU operations passed through EX/MEM pipeline register.")
   (lmd
    :initarg :lmd
    :initform 0
    :accessor lmd
    :documentation "Load memory data register. Holds data returned from memory."))
   (:documentation "Pipeline register that holds values generated in MEM phase and passes them to the WB phase."))

;;;; Simple keyed lists that group all operations into types. Used in conditional logic inside a pipeline stage execution.

(defconstant op-alu
  (list :add 0 :sub 0 :mul 0 :div 0))

(defconstant op-alu-imm
  (list :addi 8))

(defconstant op-mem
  (list :lw 35 :sw 43))

(defconstant op-mem-load
  (list :lw 35))

(defconstant op-mem-store
  (list :sw 43))

(defconstant op-branch
  (list :j 2 :beq 4 :bne 5 :bgt 6))

;;;; Constants that group instructions with a destination register according to whether their
;;;; destination register is specified by rd or rt.

(defconstant op-dest-rd
  op-alu)

(defconstant op-dest-rt
  (list :addi 8 :lw 35))

;;;; Constants that group branch instructions according to the type of logic test they need

(defconstant op-branch-type-eq
  (list :beq 4))

(defconstant op-branch-type-ne
  (list :bne 5))

(defconstant op-branch-type-t
  (list :j 2))

(defconstant op-branch-type-gt
  (list :bgt 6))

;;;; Constants that group ALU instructions according to the type of mathematical operator they need

(defconstant op-alu-type-add
  (list :add 32 :addi 8))

(defconstant op-alu-type-sub
  (list :sub 34))

(defconstant op-alu-type-mul
  (list :mul 24))

(defconstant op-alu-type-div
  (list :div 26))

;;;; Constants that define the dimensions of several data structures that represent system functional units

(defconstant reg-size 32
  "Initial size of data memory.")

(defconstant d-mem-size 200
  "Initial size of data memory.")

(defconstant i-cache-size-default 10
  "Default size of instruction cache.")

(defparameter *i-cache-size* i-cache-size-default
  "Initial size of instruction cache.")

;;;; Instruction cache miss penalty counters and values

(defconstant i-cache-default-miss-penalty 10
  "Default cache miss penalty.")

(defparameter *i-cache-miss-counter* 0
  "Tracks the number of stall cycles that the pipeline must wait through during a miss.")

(defparameter *i-cache-miss-penalty* i-cache-default-miss-penalty
  "The total number of cycles it takes to fetch an instruction during an instruction cache miss.")

;;;; Loop cache values

(defparameter *loop-cache-flag* nil
  "If set to T, indicate to pipeline that it must fetch instructions from loop cache.")

(defparameter *loop-cache-enable* t
  "If set to T, enable the loop cache mechanism, otherwise disable it completely.")

;;;; Debugging values

(defparameter *debug-mode* nil
  "Set to T to toggle on verbose debugging messages.")

(defparameter *debug-mode-cache* nil
  "Set to T to toggle on verbose debugging messages for the cache architecture.")

(defparameter *branch-counter* 0
  "Increment by 1 for every taken branch.")

(defparameter *stall-counter* 0
  "Increment by 1 for every load stall.")

;;;; No-op instruction

(defconstant no-op
  (list :op nil :rs 0 :rt 0 :rd 0 :funct 0 :addr-imm 0 :shamt 0))

;;;; Functional units

(defparameter *pc* 0
  "Program counter.")

(defparameter *clock* 1
  "Clock. Used as a counter for computing CPI and other values.")

(defparameter *ic* 0
  "Instruction count. Used for computing CPI.")

(defparameter *hit-count* 0
  "Hit count. Used for determining instruction cache performance.")

(defparameter *miss-count* 0
  "Miss count. Used for determining instruction cache performance.")

(defparameter *reg* nil
  "Register file. Represented by an integer-indexed array of size REG-SIZE.")

(defparameter *i-mem* nil
  "Instruction memory.")

(defparameter *d-mem* nil
  "Data memory. Represented by an integer-indexed array with D-MEM-SIZE elements.")

(defparameter *mem-cache* nil
  "Holds references to data memory addresses along with the value of the address, when known.
   Used by the hazard detection mechanism.")

(defparameter *i-cache* nil
  "Instruction cache.")

(defparameter *if-id* nil
  "Instance of the IF/ID pipeline register.")

(defparameter *id-ex* nil
  "Instance of the ID/EX pipeline register.")

(defparameter *ex-mem* nil
  "Instance of the EX/MEM pipeline register.")

(defparameter *mem-wb* nil
  "Instance of the MEM/WB pipeline register.")

(defparameter *reg-cache* nil
  "Holds references to registers along with the value of the register, when known.
   Used by the hazard detection mechanism.")

(defparameter *reg-cache-stall* nil
  "Holds references to registers for instructions that successfully completed the WB stage during a cycle
   in which other instructions were forced to stall.")

;;;; Flags

(defparameter *stall* nil
  "Stall flag. If T, stages should stall.")

(defparameter *branch-detected* nil
  "If a taken branch is detected, set to T.")

;;;; Functions

;;; Initialize system

(defun init-structs ()
  (setf *if-id* (make-instance 'if-id :name "IF/ID"))
  (setf *id-ex* (make-instance 'id-ex :name "ID/EX"))
  (setf *ex-mem* (make-instance 'ex-mem :name "EX/MEM"))
  (setf *mem-wb* (make-instance 'mem-wb :name "MEM/WB"))
  (setf (ir *if-id*) no-op)
  (setf (ir *id-ex*) no-op)
  (setf (ir *ex-mem*) no-op)
  (setf (ir *mem-wb*) no-op)
  (setf *reg* (make-array reg-size :initial-element 0))
  (setf *reg-cache* (make-hash-table))
  (setf *reg-cache-stall* (make-hash-table))
  (setf *d-mem* (make-array d-mem-size :initial-element 0))
  (i-cache-init)
  (setf *mem-cache* (make-hash-table)))

(defun init-vals ()
  (setf *pc* 0)
  (setf *clock* 1)
  (setf *ic* 0)
  (setf *hit-count* 0)
  (setf *miss-count* 0)
  (setf *i-cache-size* i-cache-size-default)
  (setf *i-cache-miss-penalty* i-cache-default-miss-penalty)
  (setf *i-cache-miss-counter* 0)
  (setf *loop-cache-flag* nil)
  (setf *loop-cache-enable* t)
  (setf *branch-counter* 0)
  (setf *stall-counter* 0)
  (setf *stall* nil)
  (setf *branch-detected* nil))

;;; Load external "machine code" file into instruction memory

(defun load-mac (filename)
  (with-open-file (in filename)
    (with-standard-io-syntax
      (setf *i-mem* (read in)))))

;;; Define an abstract function that specifies how pipeline registers are to be cloned

(defgeneric pipeline-register-clone (pr-orig)
  (:documentation "Accepts a PIPELINE-REGISTER and returns a clone copy of that register, not a reference to it."))

;;; Define an abstract function that specifies how pipeline stage methods will be defined

(defgeneric stage-no-pipeline (pr-1 pr-2)
  (:documentation "Accept two objects, both of them intended to be pipeline registers."))

(defgeneric stage (pr-1 pr-2 reg d-mem)
  (:documentation "Accept pipeline register objects as well as copies of both the register file and data memory."))

;;; Define an abstract function that specifies how a pipeline register debug trace method will be defined

(defgeneric debug-trace (pr)
  (:documentation "Accepts a pipeline register object."))

;;; Instruction cache miss stall information. Return true if the pipeline is currently stalling due to an instruction cache miss.

(defun i-cache-miss-stall ()
    (not (eq *i-cache-miss-counter* 0)))

;;; Prepend id and address onto an instruction and return modified instruction

(defun instruction-prepare (id address instruction)
    (append `(:id ,id) `(:addr ,address) instruction))

;;; Takes an operation number and performs the corresponding actions based on the operation type

(defmacro handle-operation (op alu mem branch)
  `(cond
    ((null ,op) nil)
    ((find ,op (append op-alu op-alu-imm)) ,alu)
    ((find ,op op-mem) ,mem)
    ((find ,op op-branch) ,branch)
    (t
     (error (format t "Unknown operation: ~a" ,op)))))

;;; Pipeline register trace methods. Build a string consisting of contents of individual internal registers.

(defmethod debug-trace ((pr if-id)) 
  (format nil "~a: IR: (~{~s~^ ~}) NPC: ~a" (name pr) (ir pr) (npc pr)))

(defmethod debug-trace ((pr id-ex))
  (format nil "~a: IR: (~{~s~^ ~}) NPC: ~a A: ~a B: ~a IMM: ~a" (name pr) (ir pr) (npc pr) (a pr) (b pr) (imm pr)))

(defmethod debug-trace ((pr ex-mem))
  (format nil "~a: IR: (~{~s~^ ~}) ALU-OUT: ~a B: ~a BRANCH-TAKEN: ~a" (name pr) (ir pr) (alu-out pr) (b pr) (branch-taken pr)))

(defmethod debug-trace ((pr mem-wb))
  (format nil "~a: IR: (~{~s~^ ~}) ALU-OUT: ~a LMD: ~a" (name pr) (ir pr) (alu-out pr) (lmd pr)))

;;; Creates a CPI report. Note: only valid for applications with no taken branches.

(defun cpi-old ()
  (format nil "~,2f" (/ *clock* (length *i-mem*))))

(defun cpi ()
  (format nil "~,2f" (/ *clock* *ic*)))

;;; Decodes an ALU operation and returns appropriate operator: +, -, /, or *

(defun get-alu-op (op)
  (cond
    ((find op op-alu-type-add) #'+)
    ((find op op-alu-type-sub) #'-)
    ((find op op-alu-type-mul) #'*)
    ((find op op-alu-type-div) #'/)
    (t (error (format t "Invalid ALU operation: ~a" op)))))

;;; Executes the appropriate ALU operation given the opcode or R-format funct, and input values X and Y

(defun exec-alu-op (op x y)
    (funcall (get-alu-op op) x y))

;;; Read from register file

(defgeneric read-reg (pr reg reg-file-source)
  (:documentation "Read value in register file according to the PR's REG-FILE-SOURCE"))

(defmethod read-reg (pr reg reg-file-source)
  (elt reg (getf (ir pr) reg-file-source)))

;;; Write to register file

(defmacro write-reg-exec (pr pr-reg reg reg-file-dest)
  `(let ((dest (getf (ir ,pr) ,reg-file-dest))
	 (val (funcall ,pr-reg ,pr)))
    (when *debug-mode* (format t "~% write-reg - reg: ~a val: ~a~%" dest val))
    (setf (elt ,reg dest) val)
    ,reg))

(defgeneric write-reg (pr pr-reg reg reg-file-dest)
  (:documentation "Write value in PR's PR-REG register to register file REG at REG-FILE-DEST"))

(defmethod write-reg ((pr mem-wb) pr-reg reg reg-file-dest)
  (write-reg-exec pr pr-reg reg reg-file-dest))

;;; Read from data memory

(defgeneric read-d-mem (pr d-mem)
  (:documentation "Read value in data memory according to the PR's ALU-OUT"))

(defmethod read-d-mem ((pr ex-mem) d-mem)
  (let ((d-mem-address (alu-out pr)))
    (when *debug-mode* 
      (progn
	(format t "~%read-d-mem - ~s" d-mem-address)
	(d-mem-print 10 2)))
    (elt d-mem (alu-out pr))))

;;; Write to data memory

(defgeneric write-d-mem (pr d-mem b)
  (:documentation "Write value in PR's B register to D-MEM at ALU-OUT"))

(defmethod write-d-mem ((pr ex-mem) d-mem b)
  (setf (elt d-mem (alu-out pr)) b)
  d-mem)

;;; Set PC and NPC

(defmacro set-pc-exec (pr value)
  `(progn ()
    (setf *pc* ,value)
    (setf (npc ,pr) *pc*)))

(defgeneric set-pc (pr value)
  (:documentation "Set global program counter and NPC register in pipeline register PR"))

(defmethod set-pc ((pr if-id) value)
  (set-pc-exec pr value))

;;; Return T if all pipeline IR registers are empty. An IR register is considered empty if it is set to nil.

(defun pipeline-registers-empty () 
  (every #'null
	 (mapcar 
	  #'(lambda (pr) (getf (ir pr) :op)) 
	  (list *if-id* *id-ex* *ex-mem* *mem-wb*))))

;;; Return human-readable representation of register file

(defun reg-print ()
  (let* ((num-rows 4)
	 (row-length (/ reg-size num-rows))
	 (output (format nil "~%")))
    (dotimes (i (length *reg*)) 
      (setf output (concatenate 'string output (format nil "| r~2,'0d: ~3d " i (elt *reg* i))))
      (when (eq 0 (mod (+ i 1) row-length))
	(setf output (concatenate 'string output (format nil "|~%")))))
  output))

;;; Return human-readable representation of register reference cache

(defun reg-cache-print (reg-cache)
  (let ((output ""))  
    (loop for id being the hash-keys in reg-cache using (hash-value entry)
	  do (setf output (concatenate 'string output (format nil "(~a (~a)) " id entry)))) 
    output))

;;; Return human-readable representation of data memory

(defun d-mem-print (cols rows)
  (let ((output (format nil "~%"))
	(range (* cols rows)))
    (when (> range d-mem-size)
      (error (format nil "Value ~a is too great. Choose a value equal to or less than ~a." range d-mem-size)))
    (dotimes (i range)
      (setf output (concatenate 'string output (format nil "| ~3,'0d: ~3d " i (elt *d-mem* i))))
      (when (eq 0 (mod (+ i 1) cols))
	(setf output (concatenate 'string output (format nil "|~%")))))
  output))

;;; Return human-readable representation of instruction memory

(defun i-mem-print ()
  (let ((output (format nil "~%")))
    (dotimes (i (length *i-mem*))
      (setf output (concatenate 'string output (format nil "~a: (~{~s~^ ~})~%" i (elt *i-mem* i)))))
    output))

;;; Return human-readable representation of the instruction cache

(defun i-cache-print ()
    (let ((output (format nil "~%")))
    (dotimes (i (length *i-cache*))
      (setf output (concatenate 'string output (format nil "~a: (~{~s~^ ~})~%" i (elt *i-cache* i)))))
    output))

;;; Key components in hazard prevention and forwarding mechanism.
;;; Perform the act of forwarding a value, if such a value is present to be forwarded.
;;; Also responsible for detecting unrecoverable hazards and signalling a stall.

;; Return the maximum ID of a given set of register references
(defun reg-cache-max-id (reg-cache)
  (let ((max-id 0))
    (loop for search-id being the hash-keys in reg-cache using (hash-value entry)
	  do (when (> search-id max-id)
	       (progn
		 (when *debug-mode* 
		   (format t "~%reg-cache-max-id - search-id: ~a max-id: ~a" search-id max-id))
		 (setf max-id search-id))))
    (when (eq max-id 0)
      (setf max-id nil))
    max-id))

;; Return a subset of the register reference cache corresponding to all entries for a given entry
;; Exclude any entries that refer to the instruction itself
(defun reg-cache-subset (id register)
  (let ((register-cache-subset (make-hash-table)))
    (loop for search-id being the hash-keys in *reg-cache* using (hash-value entry)
	  do (when (and 
		    (eq (getf entry :reg) register)
		    (> id search-id))
	       (progn
		 (setf (gethash search-id register-cache-subset) entry))))
    register-cache-subset))

;; Clear unneeded register references
(defun reg-cache-clear-done ()
  (maphash #'(lambda (id entry) (when (getf entry :done) (remhash id *reg-cache*))) *reg-cache*))

(defun reg-fetch-val (id register val-current)
  (let* ((cache-entry (gethash (reg-cache-max-id (reg-cache-subset id register)) *reg-cache*))
	 (val (getf cache-entry :val)))
    ;; If the reference to the register is not cached, return original value
    ;; Else if the reference is cached and its value is set, return the cached value
    ;; Else return NIL, signalling a stall
    (when *debug-mode* (format t " reg-fetch-val - id: ~a cache-entry: ~a val: ~a " id cache-entry val))
    (cond
      ((null cache-entry) val-current)
      (val val)
      ((null val) 
       (progn
	 (when *debug-mode* (format t " stall: register: ~a val-current: ~a " register val-current))
	 nil))
      (t (error (format nil 
			"Undefined behavior in forwarding mechanism. register: ~a val-current: ~a" 
			register 
			val-current))))))

;;; Set a register reference

(defun reg-set-reference (id reg-cache &key 
			  (reg (getf (gethash id *reg-cache*) :reg)) 
			  (val (getf (gethash id *reg-cache*) :val))
			  (done (getf (gethash id *reg-cache*) :done)))
  (let ((entry  (list :reg reg :val val :done done)))
    (when *debug-mode* (format t "~%reg-set-reference - id: ~a entry: ~a" id entry))
    (setf (gethash id reg-cache) entry)))

;;; Clear a register reference

(defun reg-clear-reference (id reg-cache)
  (remhash id reg-cache))

;;; Copy a register reference from the main register reference cache to the stall-handling register reference cache

(defun reg-set-reference-stall (id)
  (let ((entry (gethash id *reg-cache*)))
    (setf (gethash id *reg-cache-stall*) entry)))

;;; Set a memory reference

(defun mem-set-reference (id address value)
  (setf (gethash id *mem-cache*) (list :addr address :val value)))

;;; Clear a memory reference

(defun mem-clear-reference (id)
  (remhash id *mem-cache*))

;;; Initialize instruciton cache
(defun i-cache-init ()
  (setf *i-cache* (make-array *i-cache-size* :initial-element nil)))

;;; Set an instruction cache block. Allocation is direct-mapped (Address MOD Cache Size)

(defun i-cache-set (instruction address)
  (setf (elt *i-cache* (mod address *i-cache-size*)) instruction))

;;; Get an instruction cache block.

(defun i-cache-get (address)
  (elt *i-cache* (mod address *i-cache-size*)))

;;; Get the destination of a function, if it has one

(defun get-dest-reg (instruction)
  (let ((op (getf instruction :op))
	(rd (getf instruction :rd))
	(rt (getf instruction :rt)))
    (cond
      ((find op op-dest-rd) rd)
      ((find op op-dest-rt) rt)
      (t nil))))

;;; Define clone methods. One for each pipeline register subclass.
;;; Note: judicious use of macros and a better understanding of CLOS could reduce duplicative code here.

(defmethod pipeline-register-clone ((pr-orig if-id))
  (let ((pr-clone (make-instance (class-of pr-orig) 
				 :name (format nil "~a" (name pr-orig))
				 :ir (ir pr-orig) 
				 :npc (npc pr-orig))))
  pr-clone))

(defmethod pipeline-register-clone ((pr-orig id-ex))
  (let ((pr-clone (make-instance (class-of pr-orig) 
				 :name (format nil "~a" (name pr-orig))
				 :ir (ir pr-orig) 
				 :a (a pr-orig)
				 :b (b pr-orig)
				 :imm (imm pr-orig)
				 :npc (npc pr-orig))))
  pr-clone))

(defmethod pipeline-register-clone ((pr-orig ex-mem))
  (let ((pr-clone (make-instance (class-of pr-orig) 
				 :name (format nil "~a" (name pr-orig))
				 :ir (ir pr-orig) 
				 :alu-out (alu-out pr-orig)
				 :b (b pr-orig)
				 :branch-taken (branch-taken pr-orig))))
  pr-clone))

(defmethod pipeline-register-clone ((pr-orig mem-wb))
  (let ((pr-clone (make-instance (class-of pr-orig) 
				 :name (format nil "~a" (name pr-orig))
				 :ir (ir pr-orig) 
				 :alu-out (alu-out pr-orig)
				 :lmd (lmd pr-orig))))
  pr-clone))

;;; Instruction Fetch

(defmethod stage-no-pipeline ((pr-1 ex-mem) (pr-2 if-id))
  ;; IF/ID.NPC, PC <- (if ((EX/MEM.opcode == branch) & EX/MEM.cond) {EX/MEM.ALUOutput}
  (let ((op (getf (ir pr-1) :op))
	(branch-taken (branch-taken pr-1)))
    (when (and (find op op-branch) branch-taken)
      (set-pc pr-2 (alu-out pr-1))))
  ;; IF/ID.IR <- I-Mem[PC]
  (if (< *pc* (length *i-mem*))
      (progn
	(setf (ir pr-2) (elt *i-mem* *pc*)))
      (setf (ir pr-2) no-op)) ; Load "no-ops" into IR if no more instructions are left to read
  ;; PC <- PC + 1
  (set-pc pr-2 (incf *pc*))
  pr-2)

;;; Instruction Decode / Register Fetch

(defmethod stage-no-pipeline ((pr-1 if-id) (pr-2 id-ex))
  ;; ID/EX.A <- Regs[IF/ID.IR[rs]]
  (setf (a pr-2) (elt *reg* (getf (ir pr-1) :rs)))
  ;; ID/EX.B <- Regs[IF/ID.IR[rt]]
  (setf (b pr-2) (elt *reg* (getf (ir pr-1) :rt)))
  ;; ID/EX.NPC <- IF/ID.NPC
  (setf (npc pr-2) *pc*)
  ;; ID/EX.IR <- IF/ID.IR
  (setf (ir pr-2) (ir pr-1))
  ;; ID/EX.Imm <- IF/ID.IR[immediate field]
  (setf (imm pr-2) (getf (ir pr-1) :addr-imm))
  pr-2)

;;; Execution / Effective Address

(defmethod stage-no-pipeline ((pr-1 id-ex) (pr-2 ex-mem))
  (let ((op (getf (ir pr-1) :op))
	(a nil)
	(b nil))
    ;; Fetch values for A and B. Hazard prevention, forwarding, and stalling happens here.
    (setf a (a pr-1))
    (setf b (b pr-1))
    ;; EX/MEM.IR <- ID/EX.IR
    (setf (ir pr-2) (ir pr-1))
    ;; EX/MEM.BRANCH-TAKEN <- FALSE
    (setf (branch-taken pr-2) nil)
    ;; Execute appropriate actions depending on the operation type
    (handle-operation op     
		      (progn
			;; if (op == IMM) 
			;;   {MEM/WB.ALU-OUT <- ID/EX.A op ID/EX.IMM}
			;; else 
			;;   {MEM/WB.ALU-OUT <- ID/EX.A op ID/EX.B} 
			(if (find op op-alu-imm)
			    (setf (alu-out pr-2) (exec-alu-op op a (imm pr-1)))
			    (let ((op (getf (ir pr-1) :funct)))
			      (setf (alu-out pr-2) 
				    (exec-alu-op op a b))))
			(when *debug-mode*
			  (format t "op-alu: ~a" op)))
		      (progn
			;; EX/MEM.ALU-OUT <- ID/EX.A + ID/EX.IMM
			(setf (alu-out pr-2) (+ a (imm pr-1)))
			;; EX/MEM.B <- ID/EX.B
			(setf (b pr-2) b)
			(when *debug-mode*
			  (format t "op-mem: ~a" op)))
		      (progn
			;; EX/MEM.ALU-OUT <- ID/EX.IMM
			(setf (alu-out pr-2) (imm pr-1))
			;; if (op == beq) EX/MEM.BRANCH-TAKEN <- (ID/EX.A == ID/EX.B)
			;; else if (op == bne) EX/MEM.BRANCH-TAKEN <- (ID/EX.A != ID/EX.B)
			;; else if (op == j) EX/MEM.BRANCH-TAKEN <- TRUE
			(cond
			  ((find op op-branch-type-eq)
			   (progn
			     (when *debug-mode*
			       (format t "branch: beq"))
			     (setf (branch-taken pr-2) (eq a b))))
			  ((find op op-branch-type-ne)
			   (progn
			     (when *debug-mode*
			       (format t "branch: bne"))
			     (setf (branch-taken pr-2) (not (eq a b)))))
			  ((find op op-branch-type-t)
			   (progn
			     (when *debug-mode*
			       (format t "branch: jump"))
			     (setf (branch-taken pr-2) t)))
			  ((find op op-branch-type-gt)
			   (progn 
			     (when *debug-mode*
			       (format t "branch: bgt"))
			     (setf (branch-taken pr-2) (> a b))))
			  (t
			   (error(format t "Invalid branch instruction type: ~a" op))))
			(when *debug-mode*
			  (format t "op-branch: ~a" op)))))
  pr-2)

;;; Memory Access / Branch Completion

(defmethod stage-no-pipeline ((pr-1 ex-mem) (pr-2 mem-wb))
  (let ((op (getf (ir pr-1) :op))
	(b nil))
    ;; Fetch values for B (used by SW). Hazard prevention, forwarding, and stalling happens here.
    (setf b (b pr-1))
    ;; MEMB/WB.IR <- EX/MEM.IR
    (setf (ir pr-2) (ir pr-1))
    ;;  Execute appropriate actions depending on the operation type
    (handle-operation op
		      (progn
			(when *debug-mode*
			  (format t "alu!"))
			;; MEM/WB.ALU-OUT <- EX/MEM.ALU-OUT
			(setf (alu-out pr-2) (alu-out pr-1)))
		      (progn
			(when *debug-mode*
			  (format t "mem!"))
			;; if (op == load)
			;;   MEM/WB.LMD <- Mem[EX/MEM.ALU-OUT]
			;; elseif (op == store)
			;;   Mem[EX/MEM.ALU-OUT] <- MEM/WB.B
			(cond
			  ((find op op-mem-load) 
			   (progn
			     (when *debug-mode*
			       (format t "load!"))
			     (setf (lmd pr-2) (elt *d-mem* (alu-out pr-1)))))
			  ((find op op-mem-store) 
			   (progn
			     (when *debug-mode*
			       (format t "store!"))
			     (setf (elt *d-mem* (alu-out pr-1)) b)))
			  (t (error (format t "Unknown operation: ~a" op)))))
		      (progn nil)))
  pr-2)

;;; Write-Back 

(defmethod stage-no-pipeline ((pr-1 mem-wb) (pr-2 mem-wb)) ; pr-2 ignored. WB stage does not write to pipeline register.
  (let ((op (getf (ir pr-1) :op)))
    (handle-operation op
		      (progn
			(when *debug-mode*
			  (format t "alu!"))
			;; if (op == Imm)
			;;   Regs[MEM/WB.IR[rt]] <- MEM/WB.ALU-OUT
			;; else
			;;   Regs[MEM/WB.IR[rd]] <- MEM/WB.ALU-OUT
			(if (find op op-alu-imm)
			    (setf *reg* (write-reg pr-1 #'alu-out *reg* :rt))
			    (setf *reg* (write-reg pr-1 #'alu-out *reg* :rd))))
		      (progn 
			(when *debug-mode*
			  (format t "mem!"))
			;; if (op == load)
			;;   Regs[MEM/WB.IR[rt]] <- MEM/WB.LMD
			(when (find op op-mem-load)
			  (setf *reg* (write-reg pr-1 #'lmd *reg* :rt))))
		      (progn
			(when *debug-mode*
			  (format t "branch!"))))))

  ;;; Instruction Fetch

(defmethod stage ((pr-1 ex-mem) (pr-2 if-id) reg d-mem)
  ;; IF/ID.NPC, PC <- (if ((EX/MEM.opcode == branch) & EX/MEM.cond) {EX/MEM.ALUOutput}
  (let ((op (getf (ir pr-1) :op))
	(branch-taken (branch-taken pr-1))
	(instruction nil)
	(instruction-fetched-mem nil)
	(instruction-fetched-cache nil)
	(loop-cache-start nil)
	(loop-cache-end nil)
	(dest-reg nil)
	(id *clock*))
    (when (and (find op op-branch) branch-taken)
      (set-pc pr-2 (alu-out pr-1)))
    ;; If stalling due to instruction cache fetch, exit function here
    (when (i-cache-miss-stall)
      (return-from stage (values pr-2 reg d-mem)))
    ;; IF/ID.IR <- I-Mem[PC]
    (if (< *pc* (length *i-mem*))
	(progn
	  (setf instruction-fetched-mem (elt *i-mem* *pc*))
	  (setf instruction-fetched-cache (i-cache-get *pc*))
	  (when *loop-cache-enable*
	    (progn
	      (setf loop-cache-start (getf instruction-fetched-mem :loop-cache-start))
	      (setf loop-cache-end (getf instruction-fetched-mem :loop-cache-end))
	      (cond
		(loop-cache-end
		 (progn
		   (when *debug-mode-cache* (format t "~%end! clock: ~s pc: ~s~%" *clock* *pc*))
		   (setf *loop-cache-flag* nil)))
		((or loop-cache-start *loop-cache-flag*)
		 (progn
		   (when *debug-mode-cache* (format t "~%start! clock: ~s pc: ~s~%" *clock* *pc*))
		   (setf *loop-cache-flag* t))))))
	  ;; Read from instruction cache. Handle cache hit or miss appropriately.
	  (if *loop-cache-flag*
	      (progn
		(when *debug-mode-cache* (format t "loop cache hit! ~s~%" *pc*))
		(setf instruction instruction-fetched-mem)
		(incf *hit-count*))
	      (progn
		(if (eq (getf (i-cache-get *pc*) :addr) *pc*)
		    (progn
		      (when *debug-mode-cache* (format t "hit! ~s~%" *pc*))
		      (setf instruction instruction-fetched-cache)
		      (incf *hit-count*))
		    (progn
		      (when *debug-mode-cache* (format t "miss! ~s~%" *pc*))
		      (setf instruction instruction-fetched-mem)
		      (i-cache-set (instruction-prepare id *pc* instruction) *pc*)
		      (setf *i-cache-miss-counter* *i-cache-miss-penalty*)
		      (incf *miss-count*)))))
	  (setf dest-reg (get-dest-reg instruction))
	  (when (and (not *stall*) dest-reg)
	    (reg-set-reference id *reg-cache* :reg dest-reg)))
	(setf instruction no-op)) ; Load "no-ops" into IR if no more instructions are left to read
    (setf instruction (instruction-prepare id *pc* instruction))
    (setf (ir pr-2) instruction)
    ;; (if !stall) {PC <- PC + 1}
    (when (and (not *stall*))
      (set-pc pr-2 (incf *pc*))))
  (values pr-2 reg d-mem))

;;; Instruction Decode / Register Fetch

(defmethod stage ((pr-1 if-id) (pr-2 id-ex) reg d-mem)
  ;; If stalling due to instruction cache fetch, exit function here
  (when (i-cache-miss-stall)
    (return-from stage (values pr-2 reg d-mem)))
  ;; ID/EX.A <- Regs[IF/ID.IR[rs]]
  (setf (a pr-2) (read-reg pr-1 reg :rs))
  ;; ID/EX.B <- Regs[IF/ID.IR[rt]]
  (setf (b pr-2) (read-reg pr-1 reg :rt))
  ;; ID/EX.NPC <- IF/ID.NPC
  (setf (npc pr-2) *pc*)
  ;; ID/EX.IR <- IF/ID.IR
  (setf (ir pr-2) (ir pr-1))
  ;; ID/EX.Imm <- IF/ID.IR[immediate field]
  (setf (imm pr-2) (getf (ir pr-1) :addr-imm))
  (values pr-2 reg d-mem))

;;; Execution / Effective Address

(defmethod stage ((pr-1 id-ex) (pr-2 ex-mem) reg d-mem)
  (let ((op (getf (ir pr-1) :op))
	(rs (getf (ir pr-1) :rs))
	(rt (getf (ir pr-1) :rt))
	(id (getf (ir pr-1) :id))
	(a nil)
	(b nil))
    (setf a (reg-fetch-val id rs (read-reg pr-1 reg :rs)))
    (if (not (find op op-mem-store))
      (progn
	(setf b (reg-fetch-val id rt (read-reg pr-1 reg :rt))))
      (progn
	(setf b (read-reg pr-1 reg :rt))))
    ;; If stalling due to instruction cache fetch, exit function here
    (when (i-cache-miss-stall)
      (return-from stage (values pr-2)))
    ;; if (!a || !b || stall) {stall stage and set stall = true}
    (when (some #'null (list a b (not *stall*)))
      (progn
	(when *debug-mode* (format t "~%stall detected in EX~%"))
	(setf *stall* t)
	(return-from stage (values pr-2))))
    ;; EX/MEM.IR <- ID/EX.IR
    (setf (ir pr-2) (ir pr-1))
    ;; EX/MEM.BRANCH-TAKEN <- FALSE
    (setf (branch-taken pr-2) nil)
    ;; Execute appropriate actions depending on the operation type
    (handle-operation op     
		      (progn
			;; if (op == IMM) 
			;;   {MEM/WB.ALU-OUT <- ID/EX.A op ID/EX.IMM}
			;; else 
			;;   {MEM/WB.ALU-OUT <- ID/EX.A op ID/EX.B} 
			(let ((val nil)
			      (id (getf (ir pr-1) :id)))
			  (if (find op op-alu-imm)
			      (setf val (exec-alu-op op a (imm pr-1)))
			      (let ((op (getf (ir pr-1) :funct)))
				(setf val (exec-alu-op op a b))))
			  (reg-set-reference id *reg-cache* :val val)
			  (setf (alu-out pr-2) val))
			(incf *ic*)
			(when *debug-mode*
			  (format t "op-alu: ~a" op)))
		      (progn
			;; EX/MEM.ALU-OUT <- ID/EX.A + ID/EX.IMM
			(setf (alu-out pr-2) (+ a (imm pr-1)))
			;; EX/MEM.B <- ID/EX.B
			(setf (b pr-2) b)
			(incf *ic*)
			(when *debug-mode*
			  (format t "op-mem: ~a" op)))
		      (progn
			;; EX/MEM.ALU-OUT <- ID/EX.IMM
			(setf (alu-out pr-2) (imm pr-1))
			;; if (op == beq) EX/MEM.BRANCH-TAKEN <- (ID/EX.A == ID/EX.B)
			;; else if (op == bne) EX/MEM.BRANCH-TAKEN <- (ID/EX.A != ID/EX.B)
			;; else if (op == bgt) EX/MEM.BRANCH-TAKEN <- (ID/EX.A > ID/EX.B)
			;; else if (op == j) EX/MEM.BRANCH-TAKEN <- TRUE
			(cond
			  ((find op op-branch-type-eq)
			   (progn
			     (when *debug-mode*
			       (format t "branch: beq"))
			     (setf (branch-taken pr-2) (eq a b))))
			  ((find op op-branch-type-ne)
			   (progn
			     (when *debug-mode*
			       (format t "branch: bne"))
			     (setf (branch-taken pr-2) (not (eq a b)))))
			  ((find op op-branch-type-t)
			   (progn
			     (when *debug-mode*
			       (format t "branch: jump"))
			     (setf (branch-taken pr-2) t)))
			  ((find op op-branch-type-gt)
			   (progn 
			     (when *debug-mode*
			       (format t "branch: bgt"))
			     (setf (branch-taken pr-2) (> a b))))
			  (t
			   (error(format t "Invalid branch instruction type: ~a" op))))
			(when (branch-taken pr-2)
			  (progn
			    (when *debug-mode* (format t "~%branch detected: ~a" *clock*))
			    (setf *branch-detected* t)))
			(incf *ic*)
			(when *debug-mode*
			  (format t "op-branch: ~a" op)))))
  (values pr-2 reg d-mem))

;;; Memory Access / Branch Completion

(defmethod stage ((pr-1 ex-mem) (pr-2 mem-wb) reg d-mem)
  (let ((op (getf (ir pr-1) :op))
	(rt (getf (ir pr-1) :rt))
	(id (getf (ir pr-1) :id))
	(b nil))
    ;; If stalling due to instruction cache fetch, exit function here
    (when (i-cache-miss-stall)
      (return-from stage (values pr-2 d-mem)))
    (setf b (reg-fetch-val id rt (read-reg pr-1 reg :rt)))
    ;; if (!b stall) {stall stage and set stall = true}
    (when (or (null b) *stall*) 
      (progn
	(when *debug-mode* (format t "~%stall detected in MEM~%"))
	(setf *stall* t)
	(return-from stage (values pr-2))))
    ;; MEMB/WB.IR <- EX/MEM.IR
    (setf (ir pr-2) (ir pr-1))
    ;; Execute appropriate actions depending on the operation type
    (handle-operation op
		      (progn
			(when *debug-mode*
			  (format t "alu!"))
			;; MEM/WB.ALU-OUT <- EX/MEM.ALU-OUT
			(setf (alu-out pr-2) (alu-out pr-1)))
		      (progn
			(when *debug-mode*
			  (format t "mem!"))
			;; if (op == load)
			;;   MEM/WB.LMD <- Mem[EX/MEM.ALU-OUT]
			;; elseif (op == store)
			;;   Mem[EX/MEM.ALU-OUT] <- MEM/WB.B
			(cond
			  ((find op op-mem-load) 
			   (progn
			     (when *debug-mode*
			       (format t "load!"))
			     (let ((val (read-d-mem pr-1 d-mem))
				   (id (getf (ir pr-1) :id)))
			       (when *debug-mode*
				 (format t "val: ~a id: ~a" val id))
			       (setf (lmd pr-2) val))))
			  ((find op op-mem-store) 
			   (progn
			     (when *debug-mode*
			       (format t "store!"))
			     (setf d-mem (write-d-mem pr-1 d-mem b))))
			  (t (error (format t "Unknown operation: ~a" op)))))
		      (progn nil)))
  (values pr-2 d-mem reg))

;;; Write-Back 

(defmethod stage ((pr-1 mem-wb) (pr-2 mem-wb) reg d-mem) ; pr-2 ignored. WB stage does not write to pipeline register.
  (let ((op (getf (ir pr-1) :op))
	(id (getf (ir pr-1) :id)))
    ;; If stalling due to instruction cache fetch, exit function here
    (when (i-cache-miss-stall)
      (return-from stage (values reg d-mem)))
    ;; Stall if necessary
    (when *stall*
      (progn
	(when *debug-mode* (format t "~%stall detected in WB~%"))
	(return-from stage (values reg d-mem))))
    (handle-operation op
		      (progn
			(when *debug-mode* (format t "alu!"))
			;; if (op == Imm)
			;;   Regs[MEM/WB.IR[rt]] <- MEM/WB.ALU-OUT
			;; else
			;;   Regs[MEM/WB.IR[rd]] <- MEM/WB.ALU-OUT
			(if (find op op-alu-imm)
			    (setf reg (write-reg pr-1 #'alu-out reg :rt))
			    (setf reg (write-reg pr-1 #'alu-out reg :rd)))
			(reg-set-reference id *reg-cache* :done t)
			(reg-set-reference-stall id))
		      (progn 
			(when *debug-mode* (format t "mem!"))
			;; if (op == load)
			;;   Regs[MEM/WB.IR[rt]] <- MEM/WB.LMD
			(when (find op op-mem-load)
			  (progn
			    (setf reg (write-reg pr-1 #'lmd reg :rt))
			    (reg-set-reference id *reg-cache* :done t :val (lmd pr-1))
			    (reg-set-reference-stall id))))
		      (progn
			(when *debug-mode* (format t "branch!")))))
  (values reg d-mem))
  
;;; Run the five-stage pipeline

(defun run (filename mode &key init-conditions)
  ;; Set all system values to defaults
  (init-vals)
  ;; Set up data structures
  (init-structs)
  ;; Load machine code into instruction memory
  (load-mac filename)
  ;; Execute arbitrary instructions prior to program execution. Useful for debugging.
  (when init-conditions
    (funcall init-conditions)
    (i-cache-init))
  (cond
    ((equalp mode "no-pipeline")
     (run-no-pipeline))
    ((equalp mode "pipeline")
     (run-pipeline))
    (t
     (error (format nil "Incorrect mode: ~a" mode)))))

(defun run-no-pipeline ()
  ;; Execute instructions in intruction memory
  (loop
   (setf *if-id* (stage-no-pipeline *ex-mem* *if-id*))
   (when *debug-mode*
     (print (debug-trace *if-id*)))
   (setf *id-ex* (stage-no-pipeline *if-id* *id-ex*))
   (when *debug-mode*
     (print (debug-trace *id-ex*)))
   (setf *ex-mem* (stage-no-pipeline *id-ex* *ex-mem*))
   (when (branch-taken *ex-mem*)
     (incf *branch-counter*))
   (when *debug-mode*
     (print (debug-trace *ex-mem*)))
   (setf *mem-wb* (stage-no-pipeline *ex-mem* *mem-wb*))
   (when *debug-mode* 
     (print (debug-trace *mem-wb*)))
   (stage-no-pipeline *mem-wb* *mem-wb*)
   (when *debug-mode*
     (format t "~%"))
   ;; Terminate execution as soon as all pipeline registers are empty, else increment clock
   (if (pipeline-registers-empty)
     (return)
     (incf *clock*))))

(defun run-pipeline ()
  (let ((if-id-tmp (pipeline-register-clone *if-id*))
	(id-ex-tmp (pipeline-register-clone *id-ex*))
	(ex-mem-tmp (pipeline-register-clone *ex-mem*))
	(mem-wb-tmp (pipeline-register-clone *mem-wb*))
	(reg-tmp (copy-seq *reg*))
	(d-mem-tmp (copy-seq *d-mem*))
	(stall-in-mem nil)
	(stall-in-ex nil))
    (loop

     (when *debug-mode* (format t "~%~%CLOCK: ~a~%" *clock*))

     ;; WB
     (when *debug-mode* 
       (format t "~%WB~%")
       (print (debug-trace mem-wb-tmp))
       (format t "~%"))
     (setf (values reg-tmp) (stage (pipeline-register-clone *mem-wb*)
				   (pipeline-register-clone *mem-wb*)
				   (copy-seq *reg*)
				   (copy-seq *d-mem*)))
     (when *debug-mode* 
       (print (reg-cache-print *reg-cache*)))

     ;; MEM
     (when *debug-mode* 
       (format t "~%MEM~%")
       (print (debug-trace ex-mem-tmp))
       (format t "~%"))
     (setf (values mem-wb-tmp d-mem-tmp) (stage (pipeline-register-clone *ex-mem*) 
						(pipeline-register-clone *mem-wb*) 
						(copy-seq *reg*) 
						(copy-seq *d-mem*)))
     (when *debug-mode* 
       (print (debug-trace mem-wb-tmp))
       (print (reg-cache-print *reg-cache*)))

     (if *stall*
       (progn
	 (when *debug-mode* (format t "stall in MEM"))
	 (setf stall-in-mem t))
       (setf stall-in-mem nil))

     ;; EX
     (when *debug-mode* 
       (format t "~%EX~%")
       (print (debug-trace id-ex-tmp))
       (format t "~%"))
     (setf (values ex-mem-tmp) (stage (pipeline-register-clone *id-ex*)
				      (pipeline-register-clone *ex-mem*)
				      (copy-seq *reg*)
				      (copy-seq *d-mem*)))
     (when *debug-mode*
       (print (debug-trace ex-mem-tmp))
       (print (reg-cache-print *reg-cache*)))

     (if (and (not stall-in-mem) *stall*)
	 (progn
	   (when *debug-mode* (format t "stall in EX"))
	   (setf stall-in-ex t))
	 (setf stall-in-ex nil))

     ;; ID
     (when *debug-mode*
       (format t "~%ID~%")
       (print (debug-trace if-id-tmp))
       (format t "~%"))
     (setf (values id-ex-tmp) (stage (pipeline-register-clone *if-id*) 
				     (pipeline-register-clone *id-ex*) 
				     (copy-seq *reg* )
				     (copy-seq *d-mem*)))
     (when *debug-mode*
       (print (debug-trace id-ex-tmp))
       (print (reg-cache-print *reg-cache*)))

     ;; IF
     (when *debug-mode*
       (format t "~%IF~%")
       (print (debug-trace ex-mem-tmp))
       (format t "~%"))
     (setf (values if-id-tmp) (stage (pipeline-register-clone *ex-mem*) 
				     (pipeline-register-clone *if-id*) 
				     (copy-seq *reg*) 
				     (copy-seq *d-mem*)))
     (when *debug-mode*
       (print (debug-trace if-id-tmp))
       (print (reg-cache-print *reg-cache*)))

     ;; Clear register reference cache of entries for which a hazard is no longer possible
     (reg-cache-clear-done)

     ;; If currently stalling due to cache miss, suspend all pipeline activity
     (when (i-cache-miss-stall)
       (progn
	(decf *i-cache-miss-counter*)
	(when *debug-mode-cache* 
	  (format t "~%i-cache read stall - address: ~s - count: ~s~%" *pc* *i-cache-miss-counter*))))

     ;; Handle stalls and taken branches here
     (cond
       ;; On taken branch, turn instructions waiting in IF/ID and ID/EX into no-ops
       (*branch-detected*
	(reg-clear-reference (getf (ir if-id-tmp) :id) *reg-cache*)
	(setf (ir if-id-tmp) no-op)
	(setf *if-id* if-id-tmp)
	(reg-clear-reference (getf (ir id-ex-tmp) :id) *reg-cache*)
	(setf (ir id-ex-tmp) no-op)
	(setf *id-ex* id-ex-tmp)
	(setf *ex-mem* ex-mem-tmp)
	(setf *mem-wb* mem-wb-tmp)
	(setf *branch-detected* nil)
	(incf *branch-counter*)
	(when *debug-mode* (format t "~%branch detected: ~a " *clock*)))
       (stall-in-ex
	(setf if-id-tmp (pipeline-register-clone *if-id*))
	(setf *if-id* if-id-tmp)
	(setf id-ex-tmp (pipeline-register-clone *id-ex*))
	(setf *id-ex* id-ex-tmp)
	(setf (ir ex-mem-tmp) no-op)
	(setf *ex-mem* ex-mem-tmp)
	(setf *mem-wb* mem-wb-tmp)
	(setf *stall* nil)
	(incf *stall-counter*)
	(when *debug-mode*
	  (progn
	    (format t "~%reg-cache-stall: ~a" (reg-cache-print *reg-cache-stall*)))))
       (stall-in-mem
	(error (format nil "stall in mem???")))
       (t
	(setf *if-id* if-id-tmp)
	(setf *id-ex* id-ex-tmp)
	(setf *ex-mem* ex-mem-tmp)
	(setf *mem-wb* mem-wb-tmp)))

     ;; Copy forward results of operations on register file and data memory
     (setf *reg* reg-tmp)
     (setf *d-mem* d-mem-tmp)

     (when *debug-mode*
       (format t "~%"))
     ;; if pipeline registers are empty and not waiting on instruction cache read stall, terminate
     ;; else increment clock
     (if (and (pipeline-registers-empty) (not (i-cache-miss-stall)))
;     (if (pipeline-registers-empty)
	 (return)
	 (incf *clock*)))))
