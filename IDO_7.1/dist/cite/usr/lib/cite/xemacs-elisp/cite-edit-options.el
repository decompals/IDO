

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

;;; cite-edit-options.el


(defvar cite-edit-options-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map)
    (define-key map "m" 'cite-modify-one-option)
    (define-key map [button1] 'cite-modify-one-option)
    (define-key map "g" 'cite-edit-options)	; refresh display
    (define-key map "q" 'cite-quit)
    (define-key map "\C-c\C-c" 'bury-buffer)
    map    
    ))

(defvar cite-edit-options-menu
  '("Cite Edit Options"
    ["Modify an option" cite-modify-one-option t]
    ["Quit"		ef-quit		t]
    ))

(or (find-face 'underline)
    (progn (make-face 'underline)
	   (set-face-underline-p 'underline t)))

(defun cite-options-list ()
  '(cite-filename-after-lno
    cite-filename-after-ipa
    cite-shell-on-startup
    cite-implementers-image))


(defun cite-edit-options ()
  "
   Alter cite's behavior by editing a list of defined options.
   Pops up a buffer containing a list of defined options.

Editing commands:

\\{cite-edit-options-map}"
  (interactive)
  (pop-to-buffer (get-buffer-create "*CITE::Edit Cite Options*"))
  (setq buffer-read-only nil
	mode-popup-menu cite-edit-options-menu)
  (if current-menubar
      (progn
	(set (make-local-variable 'current-menubar)
	     (copy-sequence current-menubar))
	(add-submenu nil cite-edit-options-menu)))
  (erase-buffer)

  (let ((olist (cite-options-list))
	option)
    (cite-update-option-description t)	; insert header line
    (while (setq option (car olist))
      (cite-update-option-description option)
      (setq olist (cdr olist))
      ))
  (goto-char (point-min)) 
  (setq buffer-read-only t))

(defun cite-update-option-description (option)
  "
   Given an option, inserts a description of that option into the current buffer.
   Inserts a descriptive header if passed `t'."
  (let ((buffer-read-only nil)
	(fmt "%-25s %-15s %-15s\n"))
    (if (eq option t)
	(insert-face (format fmt "Variable" "Value" "What it controls")
		    'underline)
      (let ((option-extent '()))
	(setq option-extent 
	      (insert-face (format "%-25s" option) 'hyperlink))
	(insert-face (format "%-15s" (eval option)) 'underline)
	(insert-face (format "%-50s" (documentation-property option 'variable-documentation)) 'documentation)
      (insert ?\n)
      (set-extent-property option-extent 'highlight t)
      (set-extent-property option-extent 'keymap cite-edit-options-map)
      (set-extent-property option-extent 'cite-option option)))))

(defun cite-option-arg ()
   (and current-mouse-event
	(mouse-set-point current-mouse-event))
   (let ((ex (extent-at (point) nil 'cite-option))) 
     (or ex (error "There is no option to edit on this line."))
     (extent-property ex 'cite-option)))

(defun cite-modify-one-option (option newvalue)
  (interactive
   (let* ((o (cite-option-arg))
	  (no (read-string (format "New value for `%s': " o)
			   (eval o))))
     (list o no)))
  (if (symbolp option)
      (progn
	(set option newvalue)
	(cite-edit-options))
    (message "Not a symbol")))



(defun cite-quit ()
  (interactive)
  (or (one-window-p t 0)
      (delete-window))
  (kill-buffer "*Edit Cite Options*"))
