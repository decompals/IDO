

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

;; Macros for tearing down the post LNO loop structures
(defmacro a-loop-structure-header (a-loop)
  (list 'first a-loop))

(defmacro a-loop-structure-indvar (a-loop)
  (list 'second a-loop))

(defmacro a-loop-structure-loopinfo (a-loop)
  (list 'third a-loop))


;; INNER or OUTER
(defmacro a-loop-structure-level (a-loop)
  (list 'fourth a-loop))

;; Dont change this! Even though it looks like it is a mistake!
(defmacro a-loop-structure-kids (a-loop)
  (list 'nthcdr '3 a-loop))

;; End of Macros

;; Storing all the Software pipeliner love notes. These functions are
;; called and used only while annotating original loop nests. They are
;; at times not accurate.
(defconst cg-swp-max-love-notes 1000 
  "Maximum number of SWP Love notes")

(defvar cg-swp-current-idx 0
  "Index into the cg-swp-love array while the swp notes are collected.
   After that will contain the total number of swp notes found in the
   current .s file")

(defvar cg-swp-love-array (make-vector cg-swp-max-love-notes nil)
  "A vector of swp love note objects")

(defun cg-collect-swp-love-notes (prefix use-loop-depth)
  "Scan the .s file and build up a vector containing the SWP love notes"
  (setq cg-swp-current-idx 0)
  (fillarray cg-swp-love-array nil)
  (let ((assembly-file-name (format "%s.s" prefix)))
    (save-excursion
      (save-window-excursion
	(if (file-exists-p assembly-file-name)
	    (progn
	      (switch-to-buffer assembly-file-name)
	      (insert-file-contents assembly-file-name)
	      (let ((swp-search-string "#<swp\\(s\\|f\\)>"))
		(while (not (null (re-search-forward swp-search-string nil t 1)))
		  (let* ((ms (match-string 0))
			 (love (if (string= ms "#<swps>") t nil)))
		    (if (string= ms "#<swps>")
			(forward-line 1)
		      (beginning-of-line))
		    (let ((line-str (extract-line)))
		      (let ((s (point)))
			(if (string= ms "#<swps>")
			    (re-search-forward "^[^ ]" nil t 1)
			  (re-search-forward "\\(^ #<[^s][^w][^p][^f]>\\|^[^ ]\\)" 
					     nil t 1))
			(beginning-of-line)
			(let ((e (point)))
			  (let ((swp-notes (buffer-substring s e)))
			    (let ((actual-line (string-to-number line-str))
				  (loop-depth (save-excursion
						(re-search-backward 
						 "#<loop> Start"
						 nil t 1)
						(extract-depth))))
			      (if (if use-loop-depth 
				      (if (not (= loop-depth 1)) 
					  t
					nil)
				    t)
				  (let ((swp-love-note 
					 (list actual-line swp-notes love)))
				    (aset cg-swp-love-array 
					  cg-swp-current-idx swp-love-note)
				    (setq cg-swp-current-idx
					  (+ 1 cg-swp-current-idx))))))))))))
	      (kill-buffer assembly-file-name))
	  (progn
	    (message "Assembly file %s does not exist, Please compile to produce assembly file with pipelining information" assembly-file-name)))))))


(defun extract-line ()
  "
   Extract the the first number out of the current line in the buffer.
   Used by the code that reads the software pipelining notes
  "
  (save-excursion
    (let ((s (point)))
      (end-of-line)
      (let ((e (point)))
	(let ((str (buffer-substring s e)))
	  (save-match-data
	    (let ((ms (string-match "[0-9]+" str)))
	      (let ((me (match-end 0)))
		(let ((line (substring str ms me)))
		  line)))))))))

(defun extract-depth ()
  (save-excursion
    (end-of-line)
    (let ((e (point)))
      (forward-word -1)
      (let ((s (point)))
	(let ((str (buffer-substring s e)))
	  (string-to-number str))))))


(defun post-lno-inner-loop-pipelined-p (loop)
  (let ((level (a-loop-structure-level loop)))
    (if (equal level '(INNER))
	(let ((li (a-loop-structure-loopinfo loop)))
	  (if (generally-unimportant-p li)
	      nil
	    t))
      nil)))


(defun generally-unimportant-p (loop-info)
  (if (equal (second loop-info) '(GENERALLY_UNIMPORTANT))
      t
    nil))



(defvar post-lno-loop-info-list '()
  "
   Linear list of all the loops in the transformed source after LNO
  ")

(defun lno-collect-loop-info (prefix)
  "
   Collect the post LNO loops for all the PU's (subroutines/functions etc). PU's that
   dont have any loops dont have any information. Once all the post LNO loops are 
   read in, they are linearized  (That is the nestings are removed and each loop 
   is marked as either OUTER or INNER. Also loops such as WHILE-DO loops after the 
   linearization have similar structure to DO loops)"
  (setq post-lno-loop-info-list '())
  (if (file-exists-p (format "%s.l" prefix))
      (let ((list-buf (find-file-noselect (format "%s.l" prefix))))
	(save-excursion
	  (save-window-excursion
	    (switch-to-buffer list-buf)
	    (if (null (search-forward "(POST_LNO_LOOPS" nil t 1))
		(with-output-to-temp-buffer "*CITE-LNO-HELP*"
		  (princ (format "Listing file  %s does not contain post lno loop structure information\n" (file-name-nondirectory (format "%s.l" prefix))))
		  (princ "Maybe you forgot to compile with LNO that is -O3 -LIST:cite\n"))
	      (beginning-of-line)))
	  (while (not (eobp list-buf))
	    (condition-case nil
		(let ((x (read list-buf)))
		  (if (and (listp x) 
			   (eq (car x) 'POST_LNO_LOOPS))
		      (setq post-lno-loop-info-list
			    (append post-lno-loop-info-list
				      (cdr x)))))
		(end-of-file nil))))
	(kill-buffer list-buf))
    (progn
      (with-output-to-temp-buffer "*CITE-LNO-HELP*"
	(princ "\n")
	(princ (format "Listing file from LNO %s does not exist,\n" (file-name-nondirectory (format "%s.l" prefix))))
	(princ "Please recompile with -O3 -LIST:cite to get the listing file"))))

  (linearize-post-lno-loop-info-list))


;; Start Linearize post LNO loop stuff
(defun loop-unnested-p (a-loop)
  (if (or (equal (car a-loop) 'WHILE-DO) (equal (car a-loop) 'DO-WHILE))
      (if (null (nthcdr 1 a-loop))
	  t
	nil)
    (if (null (a-loop-structure-kids a-loop))
	t
      nil)))


(defun linearize-post-lno-loop-info-list ()
  "
   Builds a linearized loop info list by walking the post-lno-loop-info-list and using
   the recursive linearize-post-lno-helper. Finally it copies this new loop info list 
   into the post-lno-loop-info-list.  This could compute the depth information, 
   would be useful to double check with CG's #<loop> depth information (sometimes 
  CG seems to be dropping some #<loop>)"
  (let ((new '()))
    (while (not (null post-lno-loop-info-list))
      (let ((a-loop-structure (car post-lno-loop-info-list)))
	(if (loop-unnested-p a-loop-structure)
	    ;; An unnested loop (i.e with no loops inside) is also an INNER loop
	    (if (or (equal (car a-loop-structure) 'WHILE-DO)
		    (equal (car a-loop-structure) 'DO-WHILE))
		(setq new (append new
				  (list
				   (list
				    (a-loop-structure-header a-loop-structure)
				     "<unknown>"
				    '(LOOP_INFO 
				      (TOTAL_ITERATIONS
				       (ESTIMATE "<unknown>")
				       (UPPER_BOUND "<unknown>")))
				    '(INNER)))))
		(setq new (append new 
				  (list
				   (list
				    (a-loop-structure-header a-loop-structure)
				    (a-loop-structure-indvar a-loop-structure)
				    (a-loop-structure-loopinfo a-loop-structure)
				    '(INNER))))))
	  (let ((als-kids '())
		(top-of-als '()))
	    (if (or
		 (equal (car a-loop-structure) 'WHILE-DO)
		 (equal (car a-loop-structure) 'DO-WHILE))
		;; WHILE DO's have a different structure
		(progn
		  (setq als-kids (nthcdr 1 a-loop-structure))
		  (setq top-of-als 
			(list
			 (a-loop-structure-header a-loop-structure)
			 "<unknown>"
			 '(LOOP_INFO 
			   (TOTAL_ITERATIONS
			    (ESTIMATE "<unknown>")
			    (UPPER_BOUND "<unknown>")))
			 '(OUTER))))
	      (progn
		(setq als-kids (a-loop-structure-kids a-loop-structure))
		(setq top-of-als (list
				  (a-loop-structure-header a-loop-structure)
				  (a-loop-structure-indvar a-loop-structure)
				  (a-loop-structure-loopinfo a-loop-structure)
				  '(OUTER)))))
	    (let ((new-l als-kids)
		  (l '()))
	      (while (not (null new-l))
		(let ((ret-l (linearize-post-lno-helper (car new-l))))
		  (setq l (append l ret-l)))
		(setq new-l (cdr new-l)))
	      (setq new (append new (append (list top-of-als)  l)))))))
      (setq post-lno-loop-info-list
	    (cdr post-lno-loop-info-list)))
    (setq post-lno-loop-info-list (copy-tree new))))


(defun linearize-post-lno-helper (akid)
  "Recursive helper for linearizing. Elisp is not good for writing recursive 
   functions, but there isnt  a good way of not using recursiion here"
  (if (loop-unnested-p akid)
      (if (or (equal (car akid) 'WHILE-DO)
	      (equal (car akid) 'DO-WHILE))
	  (list
	   (list
	    (a-loop-structure-header akid)
	    "<unknown>"
	    '(LOOP_INFO 
	      (TOTAL_ITERATIONS
	       (ESTIMATE "<unknown>")
	       (UPPER_BOUND "<unknown>")))
	    '(INNER)))
	(list
	 (list
	  (a-loop-structure-header akid)
	  (a-loop-structure-indvar akid)
	  (a-loop-structure-loopinfo akid)
	  '(INNER))))
    (let ((kkl (if (or (equal (car akid) 'WHILE-DO)
		       (equal (car akid) 'DO-WHILE))
		   (nthcdr 1 akid)
		 (a-loop-structure-kids akid)))
	  (ll '()))
      (let ((new-l kkl))
	(while (not (null new-l))
	  (let ((ret-l (linearize-post-lno-helper (car new-l))))
	    (setq ll (append ll ret-l)))
	  (setq new-l (cdr new-l))))
      (cons
       (if (or (equal (car akid) 'WHILE-DO)
	       (equal (car akid) 'DO-WHILE))
	   (list
	    (a-loop-structure-header akid)
	    "<unknown>"
	    '(LOOP_INFO 
	      (TOTAL_ITERATIONS
	       (ESTIMATE "<unknown>")
	       (UPPER_BOUND "<unknown>")))
	    '(OUTER))
	 (list
	  (a-loop-structure-header akid)
	  (a-loop-structure-indvar akid)
	  (a-loop-structure-loopinfo akid)
	  '(OUTER)))
       ll))))
;; End Linearize post LNO loop stuff


(defconst cg-max-notes 5000 
  "Maximum number of loop notes")

(defvar cg-loop-current-idx 0
  "Index into the cg-note-love array while the cg loop notes are collected.
   After that will contain the total number of cg loop notes found in the
   current .s file")

(defvar cg-loop-note-array (make-vector cg-max-notes nil)
  "A vector of cg loop notes objects")

(defun cg-collect-loop-notes (prefix)
  "Scan the .s file and build up a vector containing the CG loop notes"
  (setq cg-loop-current-idx 0)
  (fillarray cg-loop-note-array nil)
  (let ((assembly-file-name (format "%s.s" prefix)))
    (save-excursion
      (save-window-excursion
	(if (file-exists-p assembly-file-name)
	    (progn
	      (switch-to-buffer assembly-file-name)
	      (insert-file-contents assembly-file-name)
	      ;; first get rid of the frequency information
;	      (while (re-search-forward "^ #<freq>.*\n" nil t 1)
;		(replace-match "" nil nil))
	      (goto-char (point-min))
	      (let ((cg-note-string "#<loop>"))
		(while (not (null (re-search-forward cg-note-string nil t 1)))
		  (beginning-of-line)
		  (let ((s (point)))
		    (re-search-forward "^[^ ][^#][^<]" nil t 1)
		    (beginning-of-line)
		    (let ((e (point)))
		      (let ((cg-loop-notes (buffer-substring s e)))
			;; cg generates sometimes #<loop> even when there isnt a loop
			;; fix it this way.
			(if (if (null (string-match "#<loop> Unrolling remainder loop (0 iteration)" cg-loop-notes))
				t
			      (if (null (string-match "#<sched>" cg-loop-notes))
				  nil
				t))
			    (progn
			      (aset cg-loop-note-array 
				    cg-loop-current-idx cg-loop-notes)
			      (setq cg-loop-current-idx
				    (+ 1 cg-loop-current-idx)))))))))
	      (kill-buffer assembly-file-name))
	  (progn
	    (message "Assembly file %s does not exist, Please compile to produce assembly file with pipelining information" assembly-file-name)))))))


(defun cg-pretty-out (s)
  "
   Pretty's the string that we are going to print out.  Gets rid of the #<...> that
   CG puts out"
  (let ((s1 (replace-in-string s "#<loop>" "")))
    (let ((s2 (replace-in-string s1 "#<sched>" "")))
      (let ((s3 (replace-in-string s2 "#<swps>" "")))
	(let ((s4 (replace-in-string s3 "#<swp>" "")))
	  (let ((s5 (replace-in-string s4 "#<swpf>" "")))
	    s5))))))

		
(defun matching-depth-line-p (n)
  (let ((note (aref cg-loop-note-array n))
	(next-note (aref cg-loop-note-array (+ 1 n))))
    (if (not (null next-note))
	(let ((ms (string-match "[0-9]+" note)))
	  (let ((me (match-end 0)))
	    (let ((line1 (substring note ms me)))
	      (let ((ms1 (string-match "[0-9]+" next-note)))
		(let ((me1 (match-end 0)))
		  (let ((line2 (substring next-note ms1 me1)))
		    (if (string= line1 line2)
			(let ((ms2 (string-match "[0-9]+" note me)))
			  (let ((me2 (match-end 0)))
			    (let ((depth1 (substring note ms2 me2)))
			      (let ((ms3 (string-match "[0-9]+" next-note me1)))
				(let ((me3 (match-end 0)))
				  (let ((depth2 (substring next-note ms3 me3)))
				    (if (string= depth1 depth2)
					t
				      nil)))))))
		      nil))))))))))
    

(defun post-lno-inner-loop-p (loop)
  "Is this post LNO loop an inner loop?"
  (let ((level (a-loop-structure-level loop)))
    (if (equal level '(INNER))
	t
      nil)))

(defun post-lno-estimate-p (loop)
  "Is the number of iterations in this post LNO loop estimated?"
  (let ((li (a-loop-structure-loopinfo loop)))
    (let ((iter (a-loopinfo-iter li)))
      (find-if (function
		(lambda (x)
		  (if (listp x)
		      (if (equal (first x)
				 'ESTIMATE)
			  x))))
	       iter))))

(defun unroll-remainder-p (loop-note)
  "Is this note an unrolling remainder loop?"
  (if (null (string-match "Unrolling remainder loop" loop-note))
      nil
    t))
	  
(defun swp-success-p (loop-note)
  "Do we have a software pipeliner success?"
  (if (null (string-match "#<swps>" loop-note))
      nil
    t))

    
(defun lno-cg-annotate-all-loop (prefix)
  "
    Actually annotating all the loops of the transformed source after LNO
    with CG information.
    1. Read's listing file to determine post LNO loop structure. Linearizes them.
       The inner loops (post LNO) are the only loops that will be software pipelined.
    2. Read the .s file and build up a vector containing the CG loop notes.
    3. For all loops in the transformed source
            Annotate with CG loop notes and post LNO loop info"
  (lno-collect-loop-info prefix)
  (cg-collect-loop-notes prefix)
  (let ((temp-lno-loop-info-list (copy-tree post-lno-loop-info-list))) ; debugging
    (let ((n 0))
      (let ((loop-reg-exp (cond 
			   ((eq major-mode 'fortran-mode)
			    "\\(do\\s-+[a-zA-Z][a-zA-Z0-9_$]*\\s-*=\\|do\\s-+while(\\)")
			   ((eq major-mode 'c-mode)
			    "\\(for\\s-*(\\|while\\s-*(\\)"))))
	(while (re-search-forward loop-reg-exp nil t)
	  (let ((next-loop (car temp-lno-loop-info-list)))
	    ;;
	    ;; If inner-loop and estimated (not really, an inner loop of 50 iterations
            ;;                              when unrolled by 4 has still a remainder)
	    ;;  if unrolling candidate
	    ;;     produce unroll remainder loop (before main loop)
	    ;;     if software pipelining candidate
	    ;;        if unroll remainder loop was not enough (not such a rare case, possible)
	    ;;           still produce the shorttrip count loop (after main loop)
	    ;;  else  if software pipelining candidate 
	    ;;     produce shorttrip count loop (after main loop)

	    ;; The following check to skip unroll remainder loops
	    (if (post-lno-inner-loop-p next-loop)
;		     (and (post-lno-inner-loop-p next-loop) 
;                           (post-lno-estimate-p next-loop)
		(if (unroll-remainder-p (aref cg-loop-note-array n))
		    (setq n (+ 1 n))))
	    (let ((note (aref cg-loop-note-array n)))
	      (cg-loop-note-extent-create next-loop note)
	      ;; The following check to skip SWP short trip
	      ;; count loop
	      ;; Estimated loops might have 0 trip threshold i.e they might
	      ;; not have any short trip count loops (not yet taken care of)
	      (if (and (post-lno-inner-loop-p next-loop)
		       (post-lno-estimate-p next-loop)
		       (swp-success-p note)
		       ;; hack, need to fix once labels are generated by CG
		       (matching-depth-line-p n))
		  (setq n (+ 2 n))
		(setq n (+ 1 n)))))
	  (setq temp-lno-loop-info-list (cdr temp-lno-loop-info-list)))))))


(defun swp-fail-p (loop-note)
  "Is this a software pipeliner failure note?"
  (if (null (string-match "#<swpf>" loop-note))
      nil
    t))

(defun swp-p (loop-note)
  "Is this a sofware pipeline success or failure note?"
  (if (null (string-match "#<swps>" loop-note))
      (if (null (string-match "#<swpf>" loop-note))
	  nil
	t)
    t))

(defun a-loopinfo-iter (loop-info)
  (if (equal (second loop-info) 
	     '(GENERALLY_UNIMPORTANT))
      (if (equal (first (third loop-info))
		 'TOTAL_ITERATIONS)
	  (third loop-info))
    (if (equal (first (second loop-info))
	       'TOTAL_ITERATIONS)
	(second loop-info))))
   
(defun cg-sched-success-reason ()
  (interactive "@")
  (let ((p (point)))
    (let ((e (extent-at p)))
      (with-output-to-temp-buffer "*CITE-SCHED-NOTES*"
	(insert-image "suneel")
	(if (extentp e)
	    (progn
	      (let ((cg-loop-note (extent-property e 'cg-loop-note)))
		(princ "Scheduler Notes\n\n")
		(princ (cg-pretty-out cg-loop-note)))))))))

(defvar cg-sched-success-xemacs-menu
  (purecopy '["Scheduler Details" cg-sched-success-reason t]))

(defvar cg-swp-success-xemacs-menu
  (purecopy '("Software Pipeliner:Success"
	      ["About Success" cg-about-swp-success nil]
	      ["Details" cg-swp-success-reason t])))


(defun cg-swp-success-reason ()
  (interactive "@")
  (let ((p (point)))
    (let ((e (extent-at p)))
      (with-output-to-temp-buffer "*CITE-SWP-SUCCESS*"
	(insert-image "rutt")
	(if (extentp e)
	    (progn
	      (let ((swp-note (extent-property e 'swp-note)))
		(princ "Software Pipeliner Success Notes\n\n")
		(princ (cg-pretty-out swp-note)))))))))

(defvar cg-swp-fail-xemacs-menu
  (purecopy '("Software Pipeliner:Failure"
	      ["About Failures" cg-about-swp-failures nil]
	      ["Details" cg-swp-fail-reason t])))


(defun cg-swp-fail-reason ()
  (interactive "@")
  (let ((p (point)))
    (let ((e (extent-at p)))
      (with-output-to-temp-buffer "*CITE-SWP-FAILURE*"
	(insert-image "rutt")
        (if (extentp e)
            (progn
              (let ((swp-note (extent-property e 'swp-note)))
		(princ "Software Pipeliner Failure Notes\n\n")
                (princ (cg-pretty-out swp-note)))))))))

(defun cg-loop-note-extent-create (lno-loop note)
  (let ((lno-loop-info (a-loop-structure-loopinfo lno-loop)))
    (let ((s (save-excursion
	       (beginning-of-line)
	       (point)))
	  (e (save-excursion
	       (end-of-line)
	       (point)))
	  (inner-p (post-lno-inner-loop-p lno-loop))
	  (estimate-p (post-lno-estimate-p lno-loop))
	  (unimportant-p (equal (second lno-loop-info)
				'(GENERALLY_UNIMPORTANT))))
      (let ((ext (make-extent s e))
	    (kmap (make-sparse-keymap))
	    (ext-face (make-face 
		       (if inner-p
			   (if (swp-success-p note)
			       'SWP-SUCCESS 
			     'SWP-FAILURE)
			 'OUTER)))
	    (iter-popup-string (format "Total-Iterations %s (%s)"
				       (second (a-loopinfo-iter lno-loop-info))
				       (if estimate-p "estimate" "exact")))
	    (swp-popup-string (if (and inner-p (swp-success-p note)) 
				  cg-swp-success-xemacs-menu
				(if (and inner-p (swp-fail-p note))
				    cg-swp-fail-xemacs-menu)))
	    (sched-popup-string cg-sched-success-xemacs-menu))
	(if (post-lno-inner-loop-p lno-loop)
	    (if (swp-success-p note)
		(progn
		  (set-face-property ext-face 'foreground "Black")
		  (set-face-property ext-face 'background "lightskyblue")
		  (set-face-font ext-face "Bold"))
	      (progn
		(set-face-property ext-face 'foreground "Black")
		(set-face-property ext-face 'background "hotpink")
		(set-face-font ext-face "Bold")))
	  (progn
	    (set-face-property ext-face 'foreground "Red")
	    (set-face-property ext-face 'background "lemonchiffon1")
	    (set-face-font ext-face "Bold")))
	(let ((cg-loop-menu '()))
	  (if (and inner-p unimportant-p)
	      (setq cg-loop-menu (list "Inner Loop Details" 
				       "-----"
				       iter-popup-string  
				       "Unimportant Loop" 
				       sched-popup-string))
	    (if (and inner-p (swp-p note))
		(progn
		  (setq cg-loop-menu (list "Inner Loop Details"
					   "-----"
					   iter-popup-string
					   swp-popup-string))
		  (set-extent-property ext 'swp-note note))
	      (setq cg-loop-menu (list  
				  (if inner-p 
				      "Inner Loop Details"
				    "Outer Loop Details")
					"-------"
					iter-popup-string
					sched-popup-string))))
	  (define-key 
	    kmap 
	    'button2 
	    '(lambda (event) 
	      (interactive "e")
	      (mouse-set-point event)
	      (beginning-of-line)
	      (interactive)
	      (let ((p (point)))
		(let ((e (extent-at p)))
		  (if (extentp e)
		      (progn
			(if (null (extent-property e 'prefetch-info))
			    (popup-menu (extent-property e 'cg-loop-menu))
			  (popup-menu (append (extent-property e 'cg-loop-menu)
					      (list prefetch-loop-xemacs-menu))))))))))
	  (set-extent-face ext ext-face)
	  (set-extent-property ext 'highlight t)
	  (set-extent-property ext 'help-echo "Click Middle Mouse for Details")
	  (set-extent-property ext 'cg-loop-menu cg-loop-menu)
	  (set-extent-property ext 'cg-loop-note note)
	  (set-extent-property ext 'lno-loop-info lno-loop) ; debugging
	  (set-extent-property ext 'keymap kmap))))))

(defun debug-cg-loop-note-extent ()
  (interactive)
  (let ((e (extent-at (point))))
    (with-output-to-temp-buffer "*CITE-CG-LOOP-NOTE-DEBUG*"
      (princ (extent-property e 'cg-loop-note)))))

(defun debug-lno-loop-note-extent ()
  (interactive)
  (let ((e (extent-at (point))))
    (with-output-to-temp-buffer "*CITE-LNO-LOOP-NOTE-DEBUG*"
      (princ (extent-property e 'lno-loop-info)))))


(defun debug-prefetch-loop-note-extent ()
  (interactive)
  (let ((e (extent-at (point))))
    (with-output-to-temp-buffer "*CITE-PREFETCH-LOOP-NOTE-DEBUG*"
      (princ (extent-property e 'prefetch-info)))))
	

