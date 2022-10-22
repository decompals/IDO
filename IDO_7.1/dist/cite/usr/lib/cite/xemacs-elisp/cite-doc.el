
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


(defun describe-cite-symbols (pattern)
  "Describe the variable symbols matching PATTERN.
   All variable symbols that have PATTERN in their name are described
   in the `*Help*' buffer."
  (interactive "sDescribe symbols matching: ")
  (let ((describe-func
	 (function
	  (lambda (s)
	    (if (boundp s)              ; It is a variable.
		(princ
		 (format "%s\t%s\n%s\n\n" s
			 (if (user-variable-p s)
			     "Option " "Variable")
			 (or (documentation-property
			      s 'variable-documentation)
			     "not documented")))))))
	sym-list)
    ;; Build a list of symbols that match pattern.
    (mapatoms (function
	       (lambda (sym)
		 (if (string-match pattern (symbol-name sym))
		     (setq sym-list (cons sym sym-list))))))
    ;; Display the data.
    (with-output-to-temp-buffer "*Help*"
      (mapcar describe-func (sort sym-list 'string<))
      (print-help-return-message))))

(defun describe-cite-flags ()
  (describe-cite-symbols "cite-\\(ipa\\|lno\\|cg\\|opt\\)-flag"))

(defun describe-cite-lno-flag ()
  (describe-cite-symbols "lno-flag"))

(defun describe-cite-ipa-flag ()
  (describe-cite-symbols "ipa-flag"))
