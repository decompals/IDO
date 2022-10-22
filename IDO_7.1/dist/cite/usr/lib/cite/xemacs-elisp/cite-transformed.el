

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

;;; cite-transformed.el:: 
;;; This is XEmacs interface to view the transformed sources
;;; (such as the intermediate source after the loop nest optimization or the
;;; final assembly file)

;;; The user should be able to mark a region of text and then request to
;;; see the transformed source. Another interface would be to just request
;;; to see the transformed source.
;;  There are two different entry points
;;;  i) for those transformed sources that can be produced using whirl2f/c
;;; ii) for the assembly source


;; just for remove-duplicates. Should be taken out to improve performance
(require 'cl) 

;; Variables and other goodies for showing the context. If you have seen
;; the gdb mode you know what these things are. If not then see the gdb mode
;; (XEmacs version)

(defvar my-arrow-extent nil)

(defvar my-arrow-glyph (make-glyph "=>"))

(make-face 'my-arrow-face)

(or (face-differs-from-default-p 'my-arrow-face)
    (copy-face 'isearch 'my-arrow-face))
	


;; Current line
(defun current-line ()
  "Returns the current line number (in the buffer) of point."
  (interactive "_")
  (save-restriction
    (widen)
    (save-excursion
      (beginning-of-line)
      (1+ (count-lines 1 (point))))))

(defun make-line-list (l n)
  (if (= n 0)
      '()
    (cons l (make-line-list (+ l 1) (- n 1)))))


;; The entry point for just getting at the transformed file

(defun show-transformed-src (xform-file-name)
  (let ((linepos-map (read-loc-file)))
    (let ((l (current-line)))
      (let ((xl (find-if (function (lambda (x)
				     (= (second x) l)))
			 linepos-map)))
	(save-excursion
	  (save-window-excursion
	    (let ((xform-present
		   (find-if (function
			     (lambda (x)
			       (string= (buffer-file-name x) xform-file-name)))
			    (buffer-list))))
	      (let ((xform-buf '()))
		(if (not xform-present)
		    (setq xform-buf (find-file-other-frame xform-file-name))
		  (setq xform-buf xform-present))
	      (switch-to-buffer xform-buf)
	      (set-specifier default-toolbar (cons (current-buffer) initial-toolbar-spec))
	      (set-specifier right-toolbar (cons (current-buffer) nil))
	      (setq buffer-read-only t)
	      (if (not (null xl))
		  (display-line xform-buf (first xl))
		(message "No line number information in transformed source corresponding to this line"))))))))))

;; The entry point for determining the region in the transformed code
;; that comes from the original source marked region

(defun corresponding-xformed-src (xform-file-name m p)
  (if (= p m) ; No region specified
      (show-transformed-src xform-file-name)
    (let ((rs m))
      (let ((re p))
	(let ((num-lines (count-lines rs re)))
	  (if (= num-lines 0)
	      (let ((orig-line-list (make-line-list 
				     (save-excursion
				       (goto-char rs)
				       (+ 1 (current-line))) num-lines)))
		(corresponding-xformed-src-real 
		 orig-line-list xform-file-name))))))
    (show-transformed-src xform-file-name)))

;; Rune's whirl2f and whirl2c generates a mapping file when given the 
;; FLIST:loc_file=x.loc option. This is essentially a mapping
;; between the lines in the transformed source and the original source
;; It sort of looks like this
;; (SRCPOS-MAP
;;  ((xformed-line xformed-col) (orig-src-file-num orig-src-line orig-src-col))
;;    ...
;; )
;; (SRCPOS-FILE-MAP
;;   (file-num "/the/complete/file/name.with-extension")
;;   ...)
;; An example (/hosts/kitty/usr/people/ssuresh/new-tests/shallow/shal1k.loc)
;; (SRCPOS-MAP
;; ((56 2) (1 29 9))
;; ((57 2) (1 30 9))
;; ((58 2) (1 31 9))
;; ((59 2) (1 32 9))
;; ((60 2) (1 33 9))
;; ((61 2) (1 42 9))
;; ....)
;; (SRCPOS-FILE-MAP
;;   (1 "/usr/people/ssuresh/xemacs-elisp/new-tests/shallow/shal1k.f"))

(defmacro srcpos-map (map)
  (list 'cdr map))

;; This function reads the loc file and extracts just the line number
;; mapping for now. 
(defun read-loc-file ()
  (let ((file-name (buffer-file-name (current-buffer))))
    (let ((prefix (extract-prefix file-name)))
      (if (file-exists-p (format "%s.loc" prefix))
	  (let ((srcmap-buf (find-file-noselect 
			     (format "%s.loc" prefix))))
	    (let ((x (read srcmap-buf)))
	      (kill-buffer srcmap-buf)
	      (mapcar 
	       (function (lambda (srcpos-map-ele)
			   (let ((w2f-srcpos (first srcpos-map-ele))
				 (orig-srcpos (second srcpos-map-ele)))
			     (list (first w2f-srcpos)
				   (second orig-srcpos)))))
	       (srcpos-map x))))
	(progn
	  (with-output-to-temp-buffer "*CITE-LOC-HELP*"
	    (princ (format "Mapping file %s.loc does not exist.\n Recompile with -LIST:cite to get the mapping file\n" prefix)))
	  nil)))))



;; The leaf function that does all the work to make it happen
;; Input: 
;; It gets the line numbers in the marked region as a list and the
;; transformed file name.
;; Processing: 
;; It reads the mapping using read-loc-file and then for each line
;; in the original source, it finds all the lines in the transformed
;; source that came from it. It builds it up as a list. This list is
;; then sorted and uniqed. The resulting list is the line numbers in
;; the transformed source that we need to show. 
;; Display: 
;; This function shows that in a seperate buffer and also shows the
;; context by highlighting the transformed source.
(defun corresponding-xformed-src-real (orig-line-list xform-file-name)
  (let ((linepos-map (read-loc-file)))
    (with-output-to-temp-buffer 
	(generate-new-buffer-name "*WHIRL2SRC*")
      (insert-image "rune")
      (save-excursion
	(save-window-excursion
	  (switch-to-buffer temp-buf)
	  (let ((w2f-lines '()))
	    (mapcar 
	     (function
	      (lambda (orig-line)
		 (setq w2f-lines 
		       (append 
			w2f-lines 
			(delq '()
			      (mapcar 
			       (function
				(lambda (line-map-ele)
				   (if (= orig-line (second line-map-ele))
				       (first line-map-ele))))
			       linepos-map))))))
	     orig-line-list)
	    (setq w2f-lines (remove-duplicates (sort w2f-lines '<)))
	    (mapcar 
	     (function
	      (lambda (x)
		 (goto-line x)
		 (beginning-of-line)
		 (let ((st (point)))
		   (end-of-line)
		   (let ((e (point)))
		     (princ (buffer-substring st e))
		     (princ "\n")))))
	     w2f-lines)))))))


;; The entry point for determining the start of the assembly code 
;; that comes from the original source marked region.

(defun show-assembly (fn after-cg-fn)
  "Show the assembly file and highlight
   the first assembly line directive corresponding
   to the current line in the original source"
  (let ((l (current-line)))
    (save-excursion
      (save-window-excursion
	(let ((cg-buf-present
	       (find-if (function
			 (lambda (x)
			   (string= (buffer-file-name x) after-cg-fn)))
			 (buffer-list))))
	  (let ((cg-buf '()))
	    (if (not cg-buf-present)
	      (setq cg-buf (find-file-other-frame after-cg-fn))
	      (setq cg-buf cg-buf-present))
	    (switch-to-buffer cg-buf)
	    (setq buffer-read-only t)
	    (set-specifier default-toolbar (cons (current-buffer) initial-toolbar-spec))
	    (set-specifier right-toolbar (cons (current-buffer) nil))
	  ;; (re-search-forward (format "^\\s-+.loc\\s-+[0-9]+\\s-+%d" l) nil t)
	    (re-search-forward 
	     (format "^\\s-+#\\s-+%d" l) nil t)
	    (re-search-forward "^\\s-+[a-z]+" nil t)
	    (display-line cg-buf (current-line))))))))

;; This one is from the gdb.el
;; Make sure the file named TRUE-FILE is in a buffer that appears on the screen
;; and that its line LINE is visible.
;; Put the overlay-arrow on the line LINE in that buffer.

(defun display-line (source-buffer line)
  ;; FILE to display
  ;; LINE number to highlight and make visible
  (let* ((pre-display-buffer-function nil) 
         (pop-up-windows t)
         (source-window (display-buffer source-buffer))
         (extent my-arrow-extent)
         pos)
    (save-excursion
      (select-window source-window))
    (and extent
         (not (eq (extent-buffer extent) source-buffer))
         (setq extent (delete-extent extent)))
    (or extent
        (progn
          (setq extent (make-extent 1 1 source-buffer))
          (set-extent-face extent 'my-arrow-face)
          (set-extent-begin-glyph extent my-arrow-glyph)
          (set-extent-begin-glyph-layout extent 'whitespace)
          (set-extent-priority extent 2000)
          (setq my-arrow-extent extent)))
    (save-current-buffer
      (set-buffer source-buffer)
      (save-restriction
	(widen)
	(goto-line line)
	(set-window-point source-window (point))
	(setq pos (point))
	(end-of-line)
	(set-extent-endpoints extent pos (point))
	(cond ((or (< pos (point-min)) (> pos (point-max)))
	       (widen)
	       (goto-char pos)))))))

  


