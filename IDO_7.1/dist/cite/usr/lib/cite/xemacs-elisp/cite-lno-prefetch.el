


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

(defmacro extent-property-value (property)
  `(let ((p (point)))
     (let ((e (extent-at p)))
       (if (extentp e)
           (let ((prop (extent-property e ,property)))
             prop)))))


(defmacro prefetch-loop-header (prefetch-loop-info)
  (list 'first prefetch-loop-info))

(defmacro prefetch-loop-indvar (prefetch-loop-info)
  (list 'second prefetch-loop-info))

(defmacro prefetch-loop-volume (prefetch-loop-info)
  (list 'third prefetch-loop-info))

(defmacro prefetch-loop-split (prefetch-loop-info)
  (list 'fourth prefetch-loop-info))

(defmacro prefetch-loop-split-value (prefetch-loop-info)
  (list 'second (list 'fourth prefetch-loop-info)))

(defmacro prefetch-loop-prefetchinfo (prefetch-loop-info)
  (list 'fifth prefetch-loop-info))

(defmacro prefetch-loop-kids (prefetch-loop-info)
  (list 'nthcdr '5 prefetch-loop-info))

(defmacro prefetch-volume-single (prefetch-volume)
  (list 'second prefetch-volume))

(defmacro prefetch-single-l1 (single)
  (list 'second single))

(defmacro prefetch-single-l2 (single)
  (list 'third single))

(defmacro prefetch-volume-total (prefetch-volume)
  (list 'third prefetch-volume))

(defmacro prefetch-total-l1 (total)
  (list 'second total))

(defmacro prefetch-total-l2 (total)
  (list 'third total))


(defmacro prefetch-node-type (prefetch-node)
  (list 'second prefetch-node))

(defmacro prefetch-node-conf (prefetch-node)
  (list 'second (list 'third prefetch-node)))

(defmacro prefetch-node-l1-stride (prefetch-node)
  (list 'second (list 'fourth prefetch-node)))

(defmacro prefetch-node-l2-stride (prefetch-node)
  (list 'second (list 'fifth prefetch-node)))

(defmacro prefetch-node-arr-ref (prefetch-node)
  (list 'sixth prefetch-node))

(defmacro prefetch-node-covered-refs (prefetch-node)
  (list 'seventh prefetch-node))


(defvar lno-prefetch-loop-info-list '()
  "
   A list of prefetch info loops. Each element of this list is
   of the form
   (PREFETCH-LOOP  \"i\" 
     (PREF-VOLUME (SINGLE 32 128) (TOTAL 224 896))
     (PREF-SPLIT 0)
     (PREFETCHES
      (PREF-NODE W (CONF 3) (L1   1) (L2   0) ( \"b(i,j)\" )
        (PREF-REFS
          ( \"b(i,j)\" 1)
        )
      )
      (PREF-NODE W (CONF 1) (L1   0) (L2   1) ( \"b(i,j)\" )
        (PREF-REFS
          ( \"b(i,j)\" 1)
        )
      )
     )
   ) if it has prefetches."
)

(require 'cl)

(defun lno-prefetch-driver (orig-file-name)
  "
   The driver for the LNO prefetch mode. Loads in the .l file containing the LNO 
   prefetch information and goes to the beginning of prefetch information. 
   It collects all of the prefetch information into a list. It then loads the
   transformed file (transformed after LNO) and annotates the do loops in the
   transformed file with the prefetching information"
  (interactive "@")
  (setq lno-prefetch-loop-info-list '())
  (let ((prefix (extract-prefix orig-file-name)))
    (if (file-exists-p (format "%s.l" prefix))
	(let ((list-buf (find-file-noselect (format "%s.l" prefix))))
	  (save-excursion
	    (save-window-excursion
	      (switch-to-buffer list-buf)
	      (if (null (search-forward "(PREFETCH-LOOP" nil t 1))
		  (with-output-to-temp-buffer "*CITE-PREFETCH-HELP*"
		    (princ "\n")
		    (princ (format "Listing file  %s does not contain prefetch information\n" (file-name-nondirectory (format "%s.l" prefix))))
		    (princ "Prefetch information is generated only for the R10000, so to\n")
		    (princ "enable prefetching compile with -O3 -LIST:cite -TARG:processor=r10k to get the listing file with prefetching information"))
		(beginning-of-line)))
	    (while (not (eobp list-buf))
	      (condition-case nil
		  (let ((x (read list-buf)))
		    (if (and (listp x) (eq (car x) 'PREFETCH-LOOP))
			(setq lno-prefetch-loop-info-list
			      (append lno-prefetch-loop-info-list
				      (list x)))))
		(end-of-file nil))))
	  (kill-buffer list-buf))
      (progn
	(with-output-to-temp-buffer "*CITE-PREFETCH-HELP*"
	  (princ "\n")
	  (princ (format "Listing file from LNO %s does not exist,\n" (file-name-nondirectory (format "%s.l" prefix))))
	  (princ "Please recompile with -O3 -LIST:cite to get the listing file"))))

    (linearize-prefetch-info-list)

    (let* ((al (make-lno-name prefix))
	   (nal (file-name-nondirectory al)))
      (if (file-exists-p al)
	  (save-window-excursion
	    (save-excursion
	      (let ((after-lno-present
		     (find-if (function
			       (lambda (x)
				 (string= x nal)))
			    (mapcar
			     (function buffer-name)
			     (buffer-list))))
		    (xform-buf '()))
		(if (not after-lno-present)
		  (setq xform-buf (find-file-other-frame al))
		  (setq xform-buf after-lno-present))
		(switch-to-buffer xform-buf))
	      (goto-char (point-min))
	      (let ((loop-reg-exp 
		     (cond 
		      ((eq major-mode 'fortran-mode)
		       "\\(do\\s-+[a-zA-Z][a-zA-Z0-9_$]*\\s-*=\\|do\\s-+while(\\)")
		      ((eq major-mode 'c-mode)
		       "\\(for\\s-*(\\|while\\s-*(\\)"))))
		(while (re-search-forward loop-reg-exp nil t)
		;; Need to check if induction variable also matches
		;; If there is an extent then add to it.
		;; Dont create extents for empty prefetch info's.
		  (let ((ext (extent-at (point))))
		    (if (not (null (extent-property ext 'cg-loop-note)))
			(progn
			  (set-extent-property ext 'prefetch-info
					       (car lno-prefetch-loop-info-list)))))
		  (setq lno-prefetch-loop-info-list
			(cdr lno-prefetch-loop-info-list))))))))))

(defun prefetch-unnested-p (prefetch-loop)
  (if (null (prefetch-loop-kids prefetch-loop))
      t
    nil))

(defun linearize-prefetch-info-list ()
  (let ((new '()))
    (while (not (null lno-prefetch-loop-info-list))
      (let ((a-nest (car lno-prefetch-loop-info-list)))
	(if (prefetch-unnested-p a-nest)
	    (setq new (append new (list a-nest)))
	  (let ((knest (prefetch-loop-kids a-nest))
		(top-of-anest (list
			       (prefetch-loop-header a-nest)
			       (prefetch-loop-indvar a-nest)
			       (prefetch-loop-volume a-nest)
			       (prefetch-loop-split a-nest)
			       (prefetch-loop-prefetchinfo a-nest))))
	    (let ((new-l knest)
		  (l '()))
	      (while (not (null new-l))
		(let ((ret-l (linearize-prefetch-helper (first new-l))))
		  (setq l (append l ret-l)))
		(setq new-l (cdr new-l)))
	      (setq new (append new
				(append
				 (list top-of-anest)
				 (if (> (prefetch-loop-split-value top-of-anest) 0)
				     ;; replicate it since no prefetch info
				     ;; is generated for the other portion of
				     ;; split. Substitute actual values by 0
				     ;; (get rid of PREFETCH refs as well)
				     (append 
				      l 
				      (mapcar
				       (function 
					(lambda (x)
					  (deep-substitute-if 
					   0 
					   (function 
					    (lambda (y)
					      (number-or-marker-p y)))
					   x)))
				       l))
				   l))))))))
      (setq lno-prefetch-loop-info-list
	    (cdr lno-prefetch-loop-info-list)))
    (setq lno-prefetch-loop-info-list (copy-tree new))))

(defun linearize-prefetch-helper (akid)
  (if (prefetch-unnested-p akid)
      (list akid)
    (let ((kkl (prefetch-loop-kids akid))
	  (ll '()))
      (let ((new-l kkl))
	(while (not (null new-l))
	  (let ((ret-l (linearize-prefetch-helper (car new-l))))
	    (setq ll (append ll ret-l)))
	  (setq new-l (cdr new-l))))
      (append
       (list
	(list
	 (prefetch-loop-header akid)
	 (prefetch-loop-indvar akid)
	 (prefetch-loop-volume akid)
	 (prefetch-loop-split akid)
	 (prefetch-loop-prefetchinfo akid)))
       (if (> (prefetch-loop-split-value akid) 0)
	   ;; replicate it since no prefetch info
	   ;; is generated for the other portion of
	   ;; split. Substitute actual values by 0
	   ;; (get rid of PREFETCH refs as well)
	   (append 
	    ll 
	    (mapcar
	     (function 
	      (lambda (x)
		(deep-substitute-if 
		 0 
		 (function 
		  (lambda (y)
		    (number-or-marker-p y)))
		 x)))
	     ll))
	 ll)))))


(defun deep-substitute-if (new pred l)
  (mapcar
   (function (lambda (x)
	       (if (listp x)
		   (if (shallow-p x)
		       (substitute-if new pred x)
		     (deep-substitute-if new pred x))
		 x)))
   l))


(defun shallow-p (l)
  (if (find-if (function (lambda (x)
			   (listp x)))
	       l)
      nil
    t))
  
(defun prefetch-loop-print (event)
  (interactive "e")
  (mouse-set-point event)
  (let ((pi (extent-property-value 'prefetch-info)))
    (with-output-to-temp-buffer 
	(generate-new-buffer-name "*CITE-PREFETCH-LOOP-INFO*")
      (print pi))))

(defun prefetch-volume-print (event)
  (interactive "e")
  (mouse-set-point event)
  (let ((p (extent-property-value 'prefetch-info)))
    (with-output-to-temp-buffer 
	(generate-new-buffer-name "*CITE-PREFETCH-VOLUME-INFO*")
      (let ((v (prefetch-loop-volume p)))
	(let ((si (prefetch-volume-single v))
	      (tot (prefetch-volume-total v)))
	(insert-image "rohit")
	(princ "Prefetch Volume Information\n")
	(princ "  L1 Volume::\n")
	(princ 
	 (format "    Per Iteration of Loop = %d bytes\n" (prefetch-single-l1 si)))
	(princ 
	 (format "    Across all iterations of loop = %d bytes\n" (prefetch-total-l1 tot)))
	(princ "\n  L2 Volume::\n")
	(princ 
	 (format "    Per Iteration of Loop = %d bytes\n" (prefetch-single-l2 si)))
	(princ 
	 (format "    Across all iterations of loop = %d bytes\n" (prefetch-total-l2 tot))))))))

(defun  prefetch-ref-print (event)
  (interactive "e")
  (mouse-set-point event)
  (let ((p (extent-property-value 'prefetch-info)))
    (let ((pi (prefetch-loop-prefetchinfo p)))
      (with-output-to-temp-buffer
	(generate-new-buffer-name "*CITE-PREFETCH-REF-INFO*")
	(if (null (cdr pi))
	    (progn
	      (princ "No actual prefetch references on this loop\n")
	      (princ "Try a loop inside this loop\n"))
	  (progn
	    (princ "Prefetch Reference Information\n\n")
	    (mapcar (function 
		     (lambda (node)
		       (princ (format "  Prefetched Reference:: %s\n"
				      (prefetch-node-arr-ref node)))
		       (princ "  Type of Prefetch::  ")
		       (princ (if (eq (prefetch-node-type node) 'W)
				  "Write Prefetch\n"
				"Read Prefetch\n"))
		       (princ (format "  Confidence of Prefetch::%d\n"
				      (prefetch-node-conf node)))
		       (princ (format "  L1 stride::%d\n"
					  (prefetch-node-l1-stride node)))
		       (princ (format "  L2 stride::%d\n"
					  (prefetch-node-l2-stride node)))
		       (princ "  References covered by this Prefetch::\n")
		       (mapcar (function 
				(lambda (ref)
				  (princ (format "    %s\n" (first ref)))))
			       (cdr (prefetch-node-covered-refs node)))
		       (princ "\n\n")))
		    (cdr pi))))))))
				       
(defun prefetch-loop-popup-menu (event)
  (interactive "e")
  (mouse-set-point event)
  (beginning-of-line)
  (popup-menu prefetch-loop-xemacs-menu))

(defvar prefetch-loop-xemacs-menu
  (purecopy '("Prefetching"
	      ["Raw Prefetch Information" prefetch-loop-print t] ; debugging
	      ["Estimated Volume of Memory References" prefetch-volume-print t]
	      ["Prefetch Reference Info" prefetch-ref-print t])))

(defun test-prefetch ()
  (interactive)
  (lno-prefetch-driver (buffer-file-name (current-buffer))))

(defun print-prefetch-list ()
  (interactive)
  lno-prefetch-loop-info-list)

