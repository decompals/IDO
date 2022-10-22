

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

;;; cite-ipa.el:: This is an XEmacs interface to obtain detailed information 
;; from  the IPA phase of the Cite compiler.

(defconst ipa-inline-foreground-color "red" 
  "Foreground color for function hypertext links")
(defconst ipa-inline-background-color "lemonchiffon1"
  "Background color for function hypertext links")

(defconst ipa-inline-face (let ((f (make-face 'ipa-inline-face)))
		  (set-face-foreground f ipa-inline-foreground-color)
		  (set-face-background f ipa-inline-background-color)
		  f)
  "Face for inlining information")

(defconst ipa-kmap 
  (let ((map (make-sparse-keymap)))
    (define-key map 'button1 'ipa-inline-popup-menu)
    (define-key map 'button2 'ipa-inline-popup-menu)
    (define-key map '(return) 'ipa-inline-popup-menu)
    map))

;; The menu for the function hypertext
(defvar ipa-inline-xemacs-menu
  (purecopy '("IPA inlining"
	      ["About IPA inlining"  ipa-menu-about-inlining nil]
	      ["IPA options used" ipa-menu-options nil]
	      ["Dead functions eliminated" ipa-menu-dead-elim nil]
	      ["Functions inlined into" ipa-menu-inlined-into t]
	      ["Functions inlined" ipa-menu-inlined nil])))

;; What to popup when middle mouse button is pressed
;; Setting the point to be within the extent when the mouse is pressed
(defun ipa-inline-popup-menu (event)
  (interactive "e")
  (mouse-set-point event)
  (beginning-of-line)
  (popup-menu ipa-inline-xemacs-menu))


;; The hypertextifying library

;; (require 'cite-util)
(require 'func-menu)

(defun ipa-inlined-into (func-name ipa-traceb)
  (let ((inlined-into '()))
    (save-excursion
      (save-window-excursion
	(switch-to-buffer ipa-traceb)
	(goto-char (point-min))
	(let ((inline-start-point (search-forward "<<<Inlining begins>>>"))
	      (inline-end-point (save-excursion
				  (search-forward "<<<Inlining completed>>>"))))
	  (let ((x (re-search-forward 
		    (concat "^" (format "%s" func-name) "\\s-") nil t)))
	    (while (and x (< x inline-end-point))
	      (setq inlined-into
		    (cons (save-excursion
			    (beginning-of-line)
			    (let ((s (point)))
			      (end-of-line)
			      (buffer-substring s (point))))
			  inlined-into))
	      (setq x (re-search-forward 
		    (concat "^" (format "%s" func-name) "\\s-") nil t)))))))
    inlined-into))
	
      
	
(defun ipa-inline-annotate-c-functions ()
  "Hypertexts the function names with inlining info obtained from .t file"
  (let ((srcb (current-buffer)))
    (let ((fn (buffer-file-name srcb)))
      (let ((ipa-trace-file-name "ipa.t"))
	(if (file-exists-p ipa-trace-file-name)
	    (let ((ipa-traceb (find-file-noselect ipa-trace-file-name)))
	      (save-excursion
		(save-window-excursion
		  (let ((next-c-func (fume-find-next-c-function-name srcb)))
		    (while (not (null next-c-func))
		      (let ((inlined-into (ipa-inlined-into 
					   (first next-c-func) ipa-traceb)))
			(if (not (null inlined-into))
			    (let ((start (cdr next-c-func)))
			      (let ((end (+ start (length (first next-c-func)))))
				(let ((e (make-extent start end)))
				  (set-extent-face e 'ipa-inline-face) 
				  (set-extent-property e 'keymap ipa-kmap)
				  (set-extent-property e 'highlight t)
				  (set-extent-property e 'help-echo 
						       "IPA stuff")
				  (set-extent-property e 'IPA-info
						       inlined-into))))))
			(setq next-c-func 
			      (fume-find-next-c-function-name srcb))))))
	      (kill-buffer ipa-traceb))
	  (message (format "IPA trace file %s does not exist" ipa-trace-file-name)))))))




(defun ipa-menu-inlined-into (e)
 (interactive "e")
 (with-output-to-temp-buffer "*INLINED INFO*"
  (let ((extent (or (and (null e) (extent-at (point)))
                      (extent-at (event-point e)
                                 (event-buffer e)))))
    (let ((ipa-inlined-into  (extent-property extent 'IPA-info)))
      (mapcar (function (lambda (x)
			  (princ (format "%s\n" x))))
	      ipa-inlined-into)))))
       


(ipa-inline-annotate-c-functions)
		      

      
