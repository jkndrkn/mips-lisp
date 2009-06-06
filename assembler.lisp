#|

assembler.lisp

John David Eriksen
http://www.jkndrkn.com
john.eriksen@yahoo.com

Usage:

> (load (compile-file "assembler.lisp"))
> (compile-asm "your-asm-file.lisp")

To toggle verbose debugging:

> (setf *debug-mode* t)   ; On
> (setf *debug-mode* nil) ; Off

|#

;; Assign all names in this file to the package com.jkndrkn.iss.assembler
(in-package :com.jkndrkn.iss.assembler)

;;;; Constants and global variables

;; Set to t to view runtime debugger messages
(defparameter *debug-mode* nil)

;; Holds assembly code loaded from external file
(defparameter *asm* nil)

;; Defines opcodes for type-i instructions
(defconstant opcodes-i
  (list :lw 35 :sw 43 :beq 4 :bne 5 :bgt 6 :addi 8))

;; Defines opcodes for type-r instructions
(defconstant opcodes-r
  (list :add 32 :sub 34 :mul 24 :div 26))

;; Defines opcodes for type-r instructions
(defconstant opcodes-j
  (list :j 2))

;; Defines opcodes for simulator optimizations
(defconstant opcodes-o
  (list :loop-cache-start 100 :loop-cache-end 101))

;; Machine code values of register symbols
(defconstant r0 0)
(defconstant r1 1)
(defconstant r2 2)
(defconstant r3 3)
(defconstant r4 4)
(defconstant r5 5)
(defconstant r6 6)
(defconstant r7 7)
(defconstant r8 8)
(defconstant r9 9)
(defconstant r10 10)
(defconstant r11 11)
(defconstant r12 12)
(defconstant r13 13)
(defconstant r14 14)
(defconstant r15 15)
(defconstant r16 16)
(defconstant r17 17)
(defconstant r18 18)
(defconstant r19 19)
(defconstant r20 20)
(defconstant r21 21)
(defconstant r22 22)
(defconstant r23 23)
(defconstant r24 24)
(defconstant r25 25)
(defconstant r26 26)
(defconstant r27 27)
(defconstant r28 28)
(defconstant r29 29)
(defconstant r30 30)
(defconstant r31 31)

;;;; Functions

;;; Initializes system by clearing any assembly code previously loaded into the system

(defun init ()
  (setf *asm* nil))

;;; Main compilation function

(defun compile-asm (filename)
  ;; Declare variables to hold processed asm instructions, branch targets, and the current branch address
  (let ((asm-filtered nil)
	(branch-targets (make-hash-table :test 'equal))
	(loop-cache-bounds (list :start nil :end nil))
	(address 0))
    ;; Initialize system
    (init)
    ;; Load external asm file
    (load-asm filename)
    ;; Iterate over every instruction
    (dolist (instruction *asm*)
      ;; If a branch target is discovered, add it to the branch-targets hash 
      ;; along with the address it is pointing to.
      ;; Otherwise, add it to the list of instructions that are ready to be resolved
      (let ((op (if (> (length instruction) 1) (getf instruction :op) nil)))
	(cond
	  ((stringp (car instruction))
	   (progn
	     (setf 
	      (gethash (car instruction) branch-targets) address)))
	  ((eq op (getf opcodes-o :loop-cache-start))
	   (progn
	     (when *debug-mode* (format t "~%loop-cache-start: ~s~%" address))
	     (setf (getf loop-cache-bounds :start) address)))
	  ((eq op (getf opcodes-o :loop-cache-end))
	   (progn
	     (when *debug-mode* (format t "~%loop-cache-end: ~s~%" address))
	     (setf (getf loop-cache-bounds :end) address)))
	  (t
	   (progn
	     (when (eq (getf loop-cache-bounds :start) address)
	       (setf instruction (append instruction (list :loop-cache-start t))))
	     (when (eq (getf loop-cache-bounds :end) address)
	       (setf instruction (append instruction (list :loop-cache-end t))))
	     (setf asm-filtered 
		   (append asm-filtered `(,instruction)))
	     (when *debug-mode* (format t "~%instruction: ~s~%" instruction))
	     (incf address))))))
    ;; Debugging statements
    (when *debug-mode* (format t "~%loop-cache-bounds: ~s~%" loop-cache-bounds))
    (when *debug-mode* (format t "~%branch-targets: ~s~%" branch-targets))
    (when *debug-mode* (format t "~%asm-filtered: (~{~s~^ ~}) ~%" asm-filtered))
    ;; Resolve branch targets by replacing branch labels with numeric addresses
    (setf *asm*
	  (resolve-branch-targets branch-targets asm-filtered))
    ;; Write finished output
    (write-asm filename)))

;;; Replace symbolic representation of branch labels with numeric addresses

(defun resolve-branch-targets (branch-targets asm)
  (let ((indexed-asm (make-array 0 :fill-pointer 0 :adjustable t)))
    (dolist (instruction asm)
      (let ((addr-imm (getf instruction :addr-imm)))
	(if (stringp addr-imm)
	    ;; Get numeric address value corresponding to branch label
	    (let ((address (gethash addr-imm branch-targets)))
	      ;; If a branch target is not found, raise an error with a useful message
	      (if (integerp address)
		  (setf (getf instruction :addr-imm) address)
		  (error (format nil 
				 "Invalid branch label: ~a Labels available: ~a" 
				 addr-imm branch-targets))))))
      (vector-push-extend instruction indexed-asm))
    indexed-asm))

;;; Add a properly prepared machine code instruction to *asm* global variable

(defun add-instruction (fields)
  (setf *asm* (append *asm* `(,fields))))

;;; Prepare an r-format instruction

(defun r-format (rs rt rd shamt funct)
  (add-instruction (list :op 0 :rs rs :rt rt :rd rd :shamt shamt :funct (getf opcodes-r funct))))

;;; Prepare an i-format instruction

(defun i-format (op rs rt addr-imm)
  (add-instruction (list :op (getf opcodes-i op) :rs rs :rt rt :addr-imm addr-imm)))

;;; Prepare a j-format instruction

(defun j-format (op target)
  (add-instruction (list :op (getf opcodes-j op) :rs 0 :rt 0 :addr-imm target)))

(defun o-format (op)
  (add-instruction (list :op (getf opcodes-o op))))

;;; Prepare an arithmetic r-format instruction

(defun arithmetic (rs rt rd funct)
  (r-format rs rt rd 0 funct))

;;; Generate a "machine code" file corresponding to assembly code input to the system

(defun write-asm (filename)
  (with-open-file (out (concatenate 'string filename ".mac")
		       :direction :output
		       :if-exists :supersede)
    (with-standard-io-syntax
      (print *asm* out))))

;;; Load and compile an external assembly file. 
;;; The compilation process populates the *asm* global variable.

(defun load-asm (filename)
  (load (compile-file filename)))

;;; This function allows the assembly programmer to set a branch target label

(defun label (name)
  (add-instruction `(,name)))

;;; Modifies following instruction to indicate to simulator that it should begin fetching instructions from loop cache

(defun loop-cache-start ()
  (o-format :loop-cache-start))

;;; Modifies following instruction to indicate to simulator that it should resume fetching instructions from L1 cache

(defun loop-cache-end ()
  (o-format :loop-cache-end))

;;;; The remaining functions correspond to assembly code instructions available to the programmer

(defun add (rd rs rt)
  (arithmetic rs rt rd :add))

(defun sub (rd rs rt)
  (arithmetic rs rt rd :sub))

(defun mul (rd rs rt)
  (arithmetic rs rt rd :mul))

(defun div (rd rs rt)
  (arithmetic rs rt rd :div))

(defun lw (rt offset rs)
  (i-format :lw rs rt offset))

(defun sw (rt offset rs)
  (i-format :sw rs rt offset))

(defun beq (rt label rs)
  (i-format :beq rs rt label))

(defun bne (rt label rs)
  (i-format :bne rs rt label))

(defun bgt (rt label rs)
  (i-format :bgt rs rt label))

(defun j (label)
  (j-format :j label))

(defun addi (rt rs imm)
  (i-format :addi rs rt imm))
