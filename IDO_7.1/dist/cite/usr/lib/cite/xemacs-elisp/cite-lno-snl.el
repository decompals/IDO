

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

;;; cite-lno-snl.el:: This is an XEmacs interface to obtain detailed information 
;; from  the SNL phase of the Loop Nest Optimizer for a user's program.

;;; The main points here are:
;;; a) Treat each loop nest as a text region in the buffer that can be
;;;    i) Highlighted, Colored, Fontified etc.
;;;   ii) On which popup menus can be used to obtain detailed information
;;;       about the SNL phase of LNO for that particular loop nest.
;;; b) Mark each loop nest with the information obtained from the SNL phase
;;;    of the loop nest optimizer (from the analysis file usually with a 
;;;    .l suffix.

;;; The implementation is in elisp and relies on the concepts of
;;; extents, faces, and keymaps in XEmacs.

;;; The entry point is (lno-driver ...)

;;; Optimizations: Lots of them need to be done
;;; a) Convert all recursive functions into iterative ones
;;; b) Face and local keymap creations once at the beginning
;;; c) Reduce user properties in extents to bare minimum.
;;; 
;; Load some handy macros


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


(make-face 'LNO-SNL-INNER)
(or (face-differs-from-default-p 'LNO-SNL-INNER)
    (copy-face 'isearch 'LNO-SNL-INNER))

;;; Given the following loop
;;;
;;; 9	      DO 110 J = 1, M
;;; 10		 DO 110 K = 1, N
;;; 11		    DO 110 I = 1, L
;;;		       C(I,K) = C(I,K) + A(I,J) * B(J,K)
;;;      110  CONTINUE

;;; loop-nest-info (which is produced by the SNL phase) would look like

'(LNO_SNL
    (LINE_POS 9 10 11)
    (SNL_FAILURES)
    (DEPENDENCE_PROBLEMS)
    (IF_INNER 11
        (CYCLES 0.703538
            (0.613636 "Resource Limited Schedule")
            0.0690137
            0.0208884)
        (FP_REGISTERS 30)
        (TRANSFORMATIONS
            (UNTILED_ORDER 9 10 11)
            (UNROLL (9 11))
            (BLOCKING (10 100 L1 9) (11 220 L1 9))))
    (IF_INNER 10
        (CYCLES 0.71257
            (0.613636 "Resource Limited Schedule")
            0.0703138
            0.0286202)
        (FP_REGISTERS 30)
        (TRANSFORMATIONS
            (UNTILED_ORDER 9 11 10)
            (UNROLL (9 11))
            (BLOCKING (11 148 L1 9) (10 160 L1 9))))
    (IF_INNER 9
        (CYCLES 0.595052
            (0.5 "Ideal Schedule")
            0.063212
            0.0318399)
        (FP_REGISTERS 23)
        (TRANSFORMATIONS
            (UNTILED_ORDER 10 11 9)
            (UNROLL (10 2) (11 5))
            (BLOCKING (11 115 L1 10) (9 164 L1 10))))
    (INNER_LOOP 9))


;; for every, find-if and other generic functions
(require 'cl)


;; This is needed here to do sanity checks with the line number 
;; information from the software pipeliner.

(defvar snl-inner-loop-line-list '() 
  "Caching the loop lines that LNO picked to be inner")


;; Entry point for snl-failure 

(defun snl-create-failure-extents (loop-nest-info)
  "
   The entry point for an SNL failure/dependence problem.
   We have two cases based on whether there are dependence
   problems or not in the failure list. It calls off to
   snl-actual-create-failure-extents to map over the failure list."
  (let ((failure-list (snl-failure-list loop-nest-info)))
    (cond
     ((no-dependence-problems failure-list)
      (snl-actual-create-failure-extents failure-list nil))

     (t (let ((dp-list (find-if (function 
				 (lambda (x)
				   (eq (car x) 'DEPENDENCE_PROBLEMS)))
				   failure-list))
	      (snlf-list (remove-if (function
				     (lambda (x)
				       (eq (car x) 'DEPENDENCE_PROBLEMS)))
				    failure-list)))
          (snl-actual-create-failure-extents snlf-list dp-list))))))


(defun no-dependence-problems (flist)
  "
   Return t if there are no DEPENDENCE_PROBLEMS in the failure list info. The 
   sequence function every is used to do it's jobs"
    (not (some (function (lambda (x)
			   (eq (car x) 'DEPENDENCE_PROBLEMS)))
	       flist)))


(defun snl-failure-list (loop-nest-info)
  "
   Extracting just the SNL_FAILURE and DEPENDENCE_PROBLEMS parts from the
   loop-nest-info. Is uses the sequence function remove-if"
  (remove-if (function 
	      (lambda (x)
		(if (listp x)
		    (not (or (eq (car x) 'SNL_FAILURES)
			     (eq (car x) 'DEPENDENCE_PROBLEMS)))
		  t)))
	     loop-nest-info))

(defun snl-actual-create-failure-extents (failure-list dp-list)
  "
   It maps over the failure-list and calls snl-create-a-failure-extent
   on each of there. There are two levels of iteration. The outer one
   is over all the SNL_FAILURE's and the inner one is over all the
   failures within a single SNL_FAILURES. One example would be
   ((SNL_FAILURES  (9 \"tmp unexpandable scalar\")
    (SNL_FAILURES  (11 \"tmp unexpandable scalar\"))
   Another example would be
   ((SNL_FAILURES  (44 \"generic dependence problem\")
                   (45 \"generic dependence problem\")
                   (46 \"generic dependence problem\")))"
  (mapcar (function (lambda (x)
                      (mapcar (function
                               (lambda (y)
                                 (snl-create-a-failure-extent
                                  (snl-failures-line y)
                                  (snl-failures-string y)
                                  dp-list)))
                              (snl-failures-content x))))
          failure-list))
	



(defun snl-create-a-failure-extent (line string dp-list)
  " Actually create the failure extent by calling make-extent.
    dp-list could be nil if we came from the no-dependence-problem path.
    Here is how it works:
    1. Go to line.
    2. If (there is a failure extent there already)
         If there is one then we append the failure string to the existing failure 
         string and we assign the dp-list there as well.
       Else
         Make an extent
         Set context sensitive keymap (should contain Dependence Problem if dp-list
         is not nil) and various other properties."
  (goto-line line)
  (beginning-of-line)
  (let ((start (point)))
    (end-of-line)
    (let ((ext (extent-at start (current-buffer) 'snl-failure)))
      (if (not (null ext))
	  (progn
	    (set-extent-property ext 'snl-dependence-problem dp-list)
	    (set-extent-property ext 
				 'snl-failure
				 (format "%s\n%s" string 
					 (extent-property ext 'snl-failure))))
	(progn
	  (setq ext (make-extent start (point)))
	  (let ((kmap (make-sparse-keymap))
		(ext-face (make-face 'LNO-SNL-F))) ; for failure
	    (set-keymap-name kmap 'snl-failure-key-map)
	    (if (null dp-list)
		(define-key kmap 'button2 'snl-failure-popup-menu)
	      (define-key kmap 'button2 'snl-failure-dependence-popup-menu))
	    (set-face-property ext-face 'foreground "Black")
	    (set-face-property ext-face 'background "hotpink")
	    (set-face-font ext-face "Bold")
	    (set-extent-face ext ext-face)
	    (set-extent-property ext 'keymap kmap))
	  
	  (set-extent-property ext 'highlight t)
	  (set-extent-property ext 'help-echo "Loop Nest Optimizer Failure Info: Click Middle Mouse for details")
	  ;; The following user property
	  (set-extent-property ext 'snl-dependence-problem dp-list)
	  (set-extent-property ext 'snl-line line)
	  (set-extent-property ext 'snl-failure string))))))


;; The entry point for a successful single loop-nest-info. 
;; The analysis file contains several of these.
(defun snl-create-success-extents (loop-nest-info)
  "
   This definition creates extents out of a loop nest
   There is one extent for each loop in a loop nest.
   It iterates over all the loops in the loop nest and
   calls snl-create-a-loop-extent on it"
  (let ((line-list (snl-line-pos-list loop-nest-info)))
    (mapcar 
     (function
      (lambda (x) 
       (snl-create-a-loop-extent x loop-nest-info)))
     line-list)))


(defun snl-select-loop (line loop-nest-info)
  "
   Get the loop from the loop-nest-info based on the line.
   Search through the list of IF_INNER's and return the one
   that matched. If none did then it return's nil. It uses
   the sequence function find-if to do the job"
  (find-if (function
	    (lambda (x)
	      (if (listp x)
		  (if (snl-if-inner-p x)
		      (if (= line (snl-if-inner-line x))
			  t
			nil)
		    nil)
		nil)))
	    loop-nest-info))

(defun snl-create-a-loop-extent (line loop-nest-info)
  "
   Make an extent for a loop that is part of the loop nest
   Has a simplyfying assumption that the Loop header does not span a line
   This creates an extent of width exactly a line"
  (goto-line line)
  (beginning-of-line)
  (let ((start (point)))
    (end-of-line)
    (let ((end (point)))
      (let ((ext (make-extent start end)))
	(let ((current-loop (snl-select-loop line loop-nest-info)))
	    (snl-loop-extent-set-properties 
	     ext current-loop line loop-nest-info))))))


(defun snl-loop-extent-set-properties (ext curr-loop-info line snl-info)
  "
   Set Various properties of an extent including highlighting, local
   keymaps ..."
  (let ((kmap (make-keymap))
        (ext-face (make-face 'LNO-SNL-S))) ; All LNO-SNL's will have same face
    (snl-loop-fill-keymap kmap curr-loop-info)
    (snl-loop-extent-fill-face-properties ext-face)
    (set-extent-face ext ext-face)
    (set-extent-property ext 'highlight t)
    (set-extent-property ext 'keymap kmap)
    (set-extent-property ext 'help-echo "Loop Nest Optimizer Success: Use Middle Mouse Button for details")
    ;; The following are user properties
    (set-extent-property ext 'snl-info snl-info)
    (set-extent-property ext 'snl-line line)
    (set-extent-property ext 'loop-if-inner curr-loop-info)

    (if (not (null curr-loop-info))
	(let ((inner-loop-info (snl-select-inner-loop snl-info)))
	  (if (= (snl-if-inner-line inner-loop-info)
		 (snl-if-inner-line curr-loop-info))
	      ;; highlight inner loops differently
	      (progn
		(set-extent-face ext 'LNO-SNL-INNER)
		(set-extent-property ext 'help-echo "INNER LOOP: Use Middle Mouse Button for details")
		(set-extent-property ext 'inner-loop inner-loop-info)))
	  ;; The following code mostly to do sanity checks with SWP later on
	  ;; Pretty expensive.
	  (if (equal inner-loop-info curr-loop-info)
	      (setq snl-inner-loop-line-list
		    (append snl-inner-loop-line-list
			    (list (snl-if-inner-line inner-loop-info)))))))))

(defun snl-loop-extent-fill-face-properties (ext-face)
  "
   Make the loop of loop nest with bold font, red foreground and
   Lemon chiffon background"
  (set-face-property ext-face 'foreground "Red")
  (set-face-property ext-face 'background "lemonchiffon1")
  (set-face-font ext-face "Bold"))

;; The menu for the Loop extent in a loop nest
(defvar snl-loop-xemacs-menu
  (purecopy '("Loop Nest Optimizer"
	      ["Machine and Cache Model (if inner)" snl-menu-if-inner-machine t]
	      ("Transformations (if inner)"
	       ["Blocking/Tiling" snl-menu-if-inner-blocking t]
	       ["Unrolling" snl-menu-if-inner-unroll t]
	       ["All" snl-menu-if-inner-xform t])
	      ["Loop Nest Information" snl-menu-loop-nest-print-info t])))

(defvar snl-loop-xemacs-no-inner-menu
  (purecopy '("Loop Nest Optimizer"
	      ["Loop Nest Information" snl-menu-loop-nest-print-info t])))

;; What to popup when middle mouse button is pressed
;; Set the point to be within the extent when the mouse is pressed
;; This makes it easier for the callbacks.
(defun snl-loop-popup-menu (event)
  (interactive "e")
  (mouse-set-point event)
  (beginning-of-line)
  (popup-menu snl-loop-xemacs-menu))

(defun snl-loop-nomouse-popup-menu (event)
  (interactive "e")
  (beginning-of-line)
  (popup-menu snl-loop-xemacs-menu))

(defun snl-loop-no-inner-popup-menu (event)
  (interactive "e")
  (mouse-set-point event)
  (beginning-of-line)
  (popup-menu snl-loop-xemacs-no-inner-menu))
  
(defun snl-loop-nest-print (event)
  (interactive "e")
  (mouse-set-point event)
  (beginning-of-line)
  (snl-menu-loop-nest-print-info event))

;; bind middle mouse button to popup the menu
(defun snl-loop-fill-keymap (kmap curr-loop)
  (set-keymap-name kmap 'snl-loop-key-map)
  (if (not (null curr-loop)) 
      (progn
	(define-key kmap 'button1 'snl-loop-nest-print)
	(define-key kmap 'button2 'snl-loop-popup-menu)
	(define-key kmap 'f11      'snl-loop-nomouse-popup-menu))
    ;; If curr-loop is nil then it cant be an inner loop, so we have a 
    ;; simpler menu
    (progn
      (define-key kmap 'button1 'snl-loop-nest-print)
      (define-key kmap 'f11      'snl-loop-no-inner-popup-menu)
      (define-key kmap 'button2 'snl-loop-no-inner-popup-menu))))


(defvar snl-failure-menu
  (purecopy '("Loop Nest Optimizer"
	      ["Reason for Failure" snl-menu-failure-info t])))


(defvar snl-failure-dependence-menu
  (purecopy '("Loop Nest Optimizer"
	      ["Reason for failure" snl-menu-failure-info t]
	      ["Dependence Problems" snl-menu-failure-dependence nil])))

(defun snl-failure-popup-menu (event)
  (interactive "e")
  (mouse-set-point event)
  (beginning-of-line)
  (popup-menu snl-failure-menu))


(defun snl-failure-dependence-popup-menu (event)
  (interactive "e")
  (mouse-set-point event)
  (beginning-of-line)
  (popup-menu snl-failure-dependence-menu))

;; The Menu callbacks
;; Machine model information if LNO chose this loop to be the inner loop

(defun snl-menu-if-inner-machine (event)
  (interactive "e")
  (let ((loop (extent-property-value 'loop-if-inner)))
    (if (not (null loop))
	(let ((loop-line (snl-if-inner-line loop)))
	  (with-output-to-temp-buffer 
	      (generate-new-buffer-name "*CITE-SNL-LOOP-MACHINE*")
	    (insert-image "dror")
	    (princ "If this loop were chosen to be the inner loop:\n")
	    (snl-model-print loop-line loop)))
      (with-output-to-temp-buffer "*CITE-SNL-LOOP-MACHINE*"
	(princ "This loop CANNOT be chosen to be the inner loop\n")))))
	

;; The blocking/tiling transformations that LNO would perform

(defun snl-menu-if-inner-blocking (event)
  (interactive "e")
  (let ((loop (extent-property-value 'loop-if-inner)))
    (if (not (null loop))
	(let ((xform (snl-if-inner-transformations loop)))
	  (with-output-to-temp-buffer 
	      (generate-new-buffer-name "*CITE-SNL-LOOP-BLOCKING*")
	    (insert-image "wolf")
	    (snl-xform-blocking-print xform)))
      (with-output-to-temp-buffer "*CITE-SNL-LOOP-BLOCKING*"
	(princ "This loop CANNOT be chosen to be the inner loop\n")))))
      

;; The unrolling transformation that LNO would perfom

(defun snl-menu-if-inner-unroll (event)
  (interactive "e")
  (let ((loop (extent-property-value 'loop-if-inner)))
    (if (not (null loop))
	(let ((xform (snl-if-inner-transformations loop)))
	  (with-output-to-temp-buffer 
	      (generate-new-buffer-name "*CITE-SNL-LOOP-UNROLL*")
	    (insert-image "wolf")
	    (snl-xform-unroll-print xform)))
      (with-output-to-temp-buffer "*CITE-SNL-LOOP-UNROLL*"
	(princ "This loop CANNOT be chosen to be the inner loop\n")))))

	      
;; All the transformations that LNO would perform if LNO chose this loop to 
;; be the inner loop

(defun snl-menu-if-inner-xform (event)
  (interactive "e")
  (let ((loop (extent-property-value 'loop-if-inner)))
    (if (not (null loop))
	(let ((xform (snl-if-inner-transformations loop)))
	  (with-output-to-temp-buffer 
	      (generate-new-buffer-name "*CITE-SNL-LOOP-XFORM*")
	    (insert-image "wolf")
	    (snl-xform-blocking-print xform)
	    (snl-xform-unroll-print xform)))
      (with-output-to-temp-buffer "*CITE-SNL-LOOP-XFORM*"
	(princ "This loop CANNOT be chosen to be the inner loop\n")))))

(defun snl-select-inner-loop (snl)
  (let ((inner (find-if (function
			 (lambda (x)
			   (if (listp x)
			       (if (inner-loop-p x)
				   t
				 nil))))
			snl)))
    (find-if (function
	      (lambda (x)
		(if (listp x)
		    (if (inner-match inner x)
			t
		      nil)
		  nil)))
	     snl)))


;; Global loop nest information 
;; a) The loop which was chosen by LNO to be the inner loop
;; b) The machine model information for that
;; c) The transformations that LNO performed

(defun snl-menu-loop-nest-print-info (event)
  (interactive "e")
  (let ((snl (extent-property-value 'snl-info)))
    (with-output-to-temp-buffer 
	(generate-new-buffer-name "*CITE-SNL-LOOP-NEST-INFO*")
      (insert-image "dror")
      (save-excursion
	(save-window-excursion
	  (set-buffer standard-output)
	  (insert-face "LOOP NEST INFORMATION\n" 'hyperlink)))
;      (princ "       LOOP NEST INFORMATION     \n ")
      (princ "The loop that was chosen to be the inner loop is:\n")
      ;; lazy look up activated only on menu.
      (let ((inner-loop (snl-select-inner-loop snl)))
	(snl-model-print (snl-if-inner-line inner-loop)
			 inner-loop)
	(let ((note (let ((x (extent-property-value 'swp-note)))
		      (if (not (null x))
			  x
			(save-excursion
			  (goto-line (snl-if-inner-line inner-loop))
			  (extent-property-value 'swp-note))))))
	  (let ((xform (snl-if-inner-transformations inner-loop)))
	    (snl-xform-blocking-print xform)
	    (snl-xform-unroll-print xform))
	  (if (not (null note))
	      (progn
		(princ "\n SOFTWARE PIPELINER LOVE NOTES::\n")
		(insert-image "rutt")
		(mapcar (function (lambda (x)
				    (princ (cg-pretty-out x))))
			note)
		(princ "\n\n"))))))))


(defun snl-model-print (inner-loop-line inner-loop)
  (save-excursion
    (goto-line inner-loop-line)
    (let ((start (point)))
      (end-of-line)
      (let ((end (point)))
	(let ((actual-loop (buffer-substring start end))
	      (cycles (snl-if-inner-cycles inner-loop))
	      (fp-registers (snl-if-inner-fpreg inner-loop)))
	  (princ actual-loop)
	  (princ "\n\nMACHINE MODEL     \n")
	  (princ (format "\nCYCLES::\nThe total number of estimated machine cycles per iteration of loop is %f\n" (snl-total-cycles cycles)))
	  (princ (format "  The number of cycles estimated by the machine model is %f\n" (snl-machine-model-cycles cycles)))
	  (princ (format "  Machine model bottleneck is \"%s\"\n" (snl-machine-model-string cycles)))
	  (princ (format "  The cycles estimated by the cache model for cache misses is %f\n" (snl-cache-model-cycles cycles)))
	  (princ (format "  The cycles estimated for loop overhead is %f\n" (snl-loopoverhead-cycles cycles)))
	  (princ (format "\nFP REGISTERS::\n Estimate of the number of floating point registers is %d\n" (snl-fp-reg fp-registers))))))))



(defun snl-xform-blocking-print (xform)
  (let ((b (snl-xform-blocking xform)))
    (princ "\n BLOCKING TRANSFORMATIONS \n")
    (mapcar 'print-xform-blocking b)))

(defun snl-xform-unroll-print (xform)
  (let ((u (snl-xform-unroll xform)))
    (princ "\n UNROLLING TRANSFORMATIONS \n")
    (mapcar 'print-xform-unroll u)))
  
(defun print-xform-blocking (bc)
  (princ "The loop:\n")
    (let ((loop (save-excursion
		  (let ((loop-line (blocking-component-line bc)))
		    (goto-line loop-line)
		    (let ((start (point)))
		      (end-of-line)
		      (let ((end (point)))
			(buffer-substring start end)))))))
      (princ loop)
      (let ((cache-level (blocking-component-cache-level bc))
	    (block (blocking-component-block bc))
	    (ol (blocking-component-outer-loop-line bc)))
	(let ((memory-level-name (cond 
				  ((eq cache-level 'L1) "primary cache ")
				  ((eq cache-level 'L2) "secondary cache"))))
	  (princ (format "\nWas cache blocked for %s by %d\n" memory-level-name block))))))

(defun print-xform-unroll (uc)
  (princ "The loop:\n")
    (let ((loop (save-excursion
		  (let ((loop-line (unroll-component-line uc)))
		    (goto-line loop-line)
		    (let ((start (point)))
		      (end-of-line)
		      (let ((end (point)))
			(buffer-substring start end)))))))
      (princ loop)
      (let ((uf (unroll-component-unrollfactor uc)))
	(princ (format "\nWas unrolled by %d\n\n" uf)))))


(defun insert-image (name)
  (if (string= cite-implementers-image "TRUE")
      (save-excursion
	(set-buffer standard-output)
	(let ((name-g '()))
	  (save-excursion
	    (save-window-excursion
	      (switch-to-buffer name)
	      (narrow-to-region (point) (point))
	      (insert-file-contents 
	       (format "%s/%s.xpm" cite-image-location name))
	      (setq name-g (make-glyph 
			    (prog1 (buffer-string)
			      (delete-region (point-min) (point-max)))))
	      (kill-buffer name)))
	  (set-extent-begin-glyph (make-extent (point) (point)) name-g)
	  (cond
	   ((string= name "dror") (princ "Dror Maydan says ....\n"))
	   ((string= name "wolf") (princ "Michael Wolf says ...\n"))
	   ((string= name "ding-kai") (princ "Ding-Kai says ...\n"))
	   ((string= name "rune") (princ "Rune Dahl says ...\n"))
	   ((string= name "rutt") (princ "John Ruttenberg says ...\n")))))))


(defun snl-menu-failure-info (event)
  (interactive "e")
  (let ((fail (extent-property-value 'snl-failure)))
    (with-output-to-temp-buffer "*CITE-SNL-FAILURE-INFO*"
      (insert-image "dror")
      (princ "       LOOP NEST FAILURE INFORMATION     \n ")
      (princ fail))))


(defun extract-prefix (file-name)
  (file-name-sans-extension file-name))

(defun snl-affix-swp-love-notes (orig-buf-name)
  (save-excursion
    (save-window-excursion
      (switch-to-buffer orig-buf-name)
      (let ((n 0))
	(while (< n cg-swp-current-idx)
	  (let ((note (aref cg-swp-love-array n)))
	    (snl-actual-affix-swp-notes note)
	    (setq n (+ 1 n))))))))

(defun snl-actual-affix-swp-notes (note)
  (let ((line (first note))
	(actual-note (second note)))
    (goto-line line)
    (let ((p (point)))
      (let ((e (extent-at p (current-buffer) 'snl-info)))
	(if (not (null e))
	    (let ((e-note (extent-property e 'swp-note)))
	      (set-extent-property e 
				   'swp-note
				   (append e-note 
					   (list actual-note))))
	  (let ((prev (previous-extent-change p (current-buffer)))
		(next (next-extent-change p (current-buffer))))
	    (let ((e1 (extent-at (1- prev) (current-buffer) 'snl-info))
		  (e2 (extent-at (1+ next) (current-buffer) 'snl-info)))
	      ;; We now know one of the extents of a loop nest.
	      ;; Need to find the inner most one
	      (if (not (null e1))
		  (snl-affix-swp-note-at-inner e1 actual-note)
		(if (not (null e2))
		    (snl-affix-swp-note-at-inner e2 actual-note))))))))))
;		  (message "Both e1 and e2 were null!"))))))))))

(defun snl-affix-swp-note-at-inner (ex actual-note)
  (let ((il (extent-property ex 'inner-loop)))
    (if (not (null il))
	;; We are done this was the inner loop
	(set-extent-property ex 
			     'swp-note
			     (append (extent-property ex 'swp-note)
				     (list actual-note)))
      ;; We need to find the loop LNO chose as inner loop
      (let ((snl-info (extent-property ex 'snl-info)))
	(let ((inner (find-if (function
			       (lambda (x)
				 (if (listp x)
				     (if (inner-loop-p x)
					 t
				       nil))))
			      snl-info)))
	  (goto-line (second inner))
	  (let ((inner-pos (point)))
	    (let ((ei (extent-at inner-pos (current-buffer) 'inner-loop)))
	      (set-extent-property ei 
				   'swp-note
				   (append (extent-property ei 'swp-note)
					   (list actual-note))))))))))

(defun only-snl-failures (loop-nest-info)
  "
   Return t if there are only SNL_FAILURES (includes DEPENDENCE_PROBLEMS as well) 
   in the loop nest information. It uses the sequence function every to do it's job"
  (let ((contents (snl-contents-list loop-nest-info)))
    (every (function (lambda (x)
		       (or (eq (car x) 'SNL_FAILURES)
			   (eq (car x) 'DEPENDENCE_PROBLEMS))))
	   contents)))

(defun no-snl-failures (loop-nest-info)
  "
   Return t if there are no SNL_FAILURES or DEPENDENCE_PROBLEMS in the loop nest 
   information. It uses the sequence function every to do it's job"
  (let ((contents (snl-contents-list loop-nest-info)))
    (every (function (lambda (x)
		       (not (or (eq (car x) 'SNL_FAILURES)
				(eq (car x) 'DEPENDENCE_PROBLEMS)))))
	   contents)))

(defun remove-snl-failures (loop-nest-info)
  "
   This is used in the partial success/failure case. It removes all
   the SNL_FAILURES/DEPENDENCE_PROBLEMS from the loop-nest-info. It also modifies 
   the line number information so that the SNL_FAILURES lines are no
   longer present"
  (let ((loop-nest-info-no-snl-failures
	 (remove-if (function (lambda (x)
				(if (listp x)
				    (or
				     (eq (car x) 'SNL_FAILURES)
				     (eq (car x) 'DEPENDENCE_PROBLEMS)
				     (eq (car x) 'LINE_POS)
				     (eq (car x) 'NESTING_DEPTH))
				  (eq x 'LNO_SNL))))
		    loop-nest-info))
	(snl-failures-line-list
	 (mapcar (function
		  (lambda (x)
		    (snl-failures-line x)))
		 (remove-if (function (lambda (x)
					(if (listp x)
					    (not (eq (car x) 'SNL_FAILURES))
					  (eq x 'LNO_SNL))))
			    loop-nest-info)))
	(line-pos-list (snl-line-pos-list loop-nest-info)))
    (let ((snl-success-line-pos-list
	   (remove-if (function (lambda (x)
				  (member x snl-failures-line-list)))
		      line-pos-list)))
      (cons 'LNO_SNL
	    (cons (snl-nesting-depth loop-nest-info)
		  (cons
		   (cons 'LINE_POS snl-success-line-pos-list)
		   loop-nest-info-no-snl-failures))))))

;; Works on LNO_SNL list.

(defun lno-do-snl (lno-snl-list)
  "Works on a LNO SNL list. This list is usually of the form
   (LNO_SNL
    (NESTING_DEPTH 3)
    (LINE_POS 11 12 14)
    (SNL_FAILURES  (11 \"unknown reason\"))
    (IF_INNER 14 
        (CYCLES 1.0677 
            (1 \"Ideal Schedule\")
            0.0176985
            0.0500002)
        (FP_REGISTERS 8) 
        (TRANSFORMATIONS
            (UNTILED_ORDER 12 14)
            (UNROLL (12 2))))
    (INNER_LOOP 14)). 
    1. It checks for bogus LNO SNL
    2. It calls off to functions to create extents with the right information
       We have 3 cases
       a) There are only failures in the list
       b) There are no failures in the list
       c) Partial failures in the list"
   ;; First check for bogus SNL's
  (if (and (eq (car lno-snl-list) 'LNO_SNL) 
	   (> (length lno-snl-list) 3))
      (cond
       ((only-snl-failures lno-snl-list) 
	(snl-create-failure-extents lno-snl-list))
       ((no-snl-failures lno-snl-list)
	(snl-create-success-extents lno-snl-list))
       (t (progn
	    (snl-create-success-extents (remove-snl-failures lno-snl-list))
	    (snl-create-failure-extents lno-snl-list))))))



(defun lno-driver (orig-file-name)
  "
   The driver for the LNO mode. Also main entry point for LNO data processing.
   1. Loads in the listing file (.l) containing the LNO information and goes to the 
      beginning of the LNO information. It then reads one list item at a time from 
      the file and passes control to LNO SNL.
   2. Collects the software pipelining information from the .s files and annotates
      the loop nests with the SWP information."
  (interactive "@")
  (setq snl-inner-loop-line-list '())
  (let ((prefix (extract-prefix orig-file-name)))
    (if (file-exists-p (format "%s.l" prefix))
	(progn
	  (let ((list-buf (find-file-noselect (format "%s.l" prefix))))
	    (save-excursion
	      (save-window-excursion
		(switch-to-buffer list-buf)
		; cut to the chase
		(if (null (search-forward "(LNO" nil t 1))
		    (with-output-to-temp-buffer "*CITE-LNO-HELP*"
		      (princ "\n")
		      (princ (format "Listing file from LNO %s does not contain LNO information \n" (file-name-nondirectory (format "%s.l" prefix))))
		      (princ "Please recompile with -O3 -LIST:cite to get the listing file with Loop Nest Information"))
		  (beginning-of-line)))
	      (while (not (eobp list-buf))
		(condition-case nil
		    (let ((x (read list-buf)))
		      (if (and (listp x) (eq (car x) 'LNO_SNL))
			  (lno-do-snl x)))
		  (end-of-file nil)))
	    (kill-buffer list-buf)))
	  (if (file-exists-p (format "%s.s" prefix))
	      (progn
		(cg-collect-swp-love-notes prefix t)
		(snl-affix-swp-love-notes (current-buffer)))
	      (message (format "Assembly file %s does not exist. Annotating without CG information\n" (file-name-nondirectory (format "%s.s" prefix))))))
      (progn
	(with-output-to-temp-buffer "*CITE-LNO-HELP*"
	  (princ "\n")
	  (princ (format "Listing file from LNO %s does not exist,\n" (file-name-nondirectory (format "%s.l" prefix))))
	  (princ "Please recompile with -LIST:cite -O3 to get the listing file"))))))




