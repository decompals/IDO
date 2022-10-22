

;;
;;  Copyright (c) 1996
;;  Silicon Graphics 
;; 
;; These coded instructions, statements, and computer programs  contain  
;; unpublished  proprietary  information of Silicon Graphics, Inc., and 
;; are protected by Federal copyright law.  They  may  not be disclosed
;; to  third  parties  without the prior written consent of Silicon Graphics, 
;; Inc.
;; Silicon Graphics makes no representations about the suitability
;; of this software for any purpose.  It is provided "as is" without
;; express or implied warranty. Permission is granted to modify this
;; software for any purpose. 

;; How I wish that elisp was more like scheme (at least for the
;; cool macros). The macros in elisp are just slightly better than
;; C's #define

;; Some helper macros so that test cases and comments
;; dont evaluate to anything

(defmacro TEST1 (inp result) '())
(defmacro TEST2 (inp1 res1 inp2 res2) '())

(defmacro TEST3 (inp1 res1 inp2 res2 inp3 res3) '())

(defmacro COMMENT (c1) '())

;; Treating the loop-nest-info as an object
;; These macros try to keep the representation seperate from the
;; functionality. The file cite-lno-snl.el needs to use these macros 
;; instead of tearing down lists using second, third, car etc.

;; Should getters do error checking (is a good thing at least for debugging)
;; (LNO_SNL ...) getters

(defmacro snl-line-pos-list (loop-nest-info)
  (list 'cdr (list 'third loop-nest-info)))

;(defmacro snl-failures (loop-nest-info)
;  (list 'fourth loop-nest-info))

;(defmacro snl-dependence-problems (loop-nest-info)
;  (list 'fifth loop-nest-info))

(defmacro snl-nesting-depth (loop-nest-info)
  (list 'second loop-nest-info))

;; Need to skip over NESTING_DEPTH and LINE_POS
(defmacro snl-contents-list (loop-nest-info)
  (list 'nthcdr '3 loop-nest-info))

(defmacro snl-if-inners-list (loop-nest-info)
  (list 'nthcdr '3 loop-nest-info)) ;; almost this except for the last entry

;; (IF_INNER .... ) getters

(defmacro snl-if-inner-line (if-inner)
  (list 'second if-inner))

(defmacro snl-if-inner-cycles (if-inner)
  (list 'third if-inner))

(defmacro snl-if-inner-fpreg (if-inner)
  (list 'fourth if-inner))

(defmacro snl-if-inner-transformations (if-inner)
  (list 'fifth if-inner))

(defmacro snl-if-inner-p (x)
  `(eq (car ,x) 'IF_INNER))

(defmacro inner-loop-p (x)
  `(eq (car ,x) 'INNER_LOOP))

(defmacro inner-match (inner x)
  `(if (eq (first ,x) 'IF_INNER)
      (if (eq (second ,inner) (second ,x))
	  t
	nil)))

;; (CYCLES ....) getters
(defmacro snl-total-cycles (cycles)
  (list 'second cycles))

(defmacro snl-machine-model-cycles (cycles)
  (list 'first (list 'third cycles)))

(defmacro snl-machine-model-string (cycles)
  (list 'second (list 'third cycles)))

(defmacro snl-cache-model-cycles (cycles)
  (list 'fourth cycles))

(defmacro snl-loopoverhead-cycles (cycles)
  (list 'fifth cycles))

;; (FP_REGISTERS ...) getters

(defmacro snl-fp-reg (fpreg)
  (list 'second fpreg))

;; (TRANSFORMATIONS ...) getters

(defmacro snl-xform-untiled-order (xform)
  `(let ((ut (second ,xform)))
     (if (eq (first ut) 'UNTILED)
	 (cdr ut)
       (error "Internal Error: Format of transformation in lno_snl not valid"))))

(defmacro snl-xform-unroll (xform)
  `(let ((ur (third ,xform)))
     (cond
      ((null ur) nil) ; no unrolling info
      ((eq (first ur) 'UNROLL) (cdr ur)) ; the right thing
      ((eq (first ur) 'BLOCKING) nil) ; oops it was a BLOCKING info
      (t (error "Internal Error: Format of transformation in lno_snl not valid")))))


(defmacro snl-xform-blocking (xform)
  `(let ((ur (third ,xform)))
     (cond
      ((null ur) nil)
      ((eq (first ur) 'UNROLL) (let ((bl (fourth ,xform)))
                                 (cond
                                  ((null bl) nil) ; no blocking info
                                  ((eq (first bl) 'BLOCKING) (cdr bl))
                                  (t (error "Internal Error: Format of transformation in lno_snl not valid")))))
      ((eq (first ur) 'BLOCKING) (cdr ur))
      (t (error "Internal Error: Format of transformation in lno_snl not valid")))))

;; UNROLL component getters

(defmacro unroll-component-line (ur-component)
  (list 'first ur-component))

(defmacro unroll-component-unrollfactor (ur-component)
  (list 'second ur-component))

;; BLOCKING component getters

(defmacro blocking-component-line (block-component)
  (list 'first block-component))

(defmacro blocking-component-block (block-component)
  (list 'second block-component))

(defmacro blocking-component-cache-level (block-component)
  (list 'third block-component))

(defmacro blocking-component-outer-loop-line (block-component)
  (list 'fourth block-component))

;; (SNL_FAILURES (line-num "text-string") (line-num2 "text-string")) getters

(defmacro snl-failures-content (snl-failure)
  (list 'cdr snl-failure))

(defmacro snl-failures-line (snl-failure-content)
  (list 'first  snl-failure-content))

(defmacro snl-failures-string (snl-failure-content)
  (list 'second snl-failure-content))

(defmacro snl-failures-p (l)
  `(eq (car ,l) 'SNL_FAILURES))

;; more complex macros

(defmacro extent-property-value (property)
  `(let ((p (point)))
     (let ((e (extent-at p)))
       (if (extentp e)
	   (let ((prop (extent-property e ,property)))
	     prop)))))


