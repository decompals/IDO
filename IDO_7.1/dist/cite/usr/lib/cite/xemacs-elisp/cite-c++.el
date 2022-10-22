

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

(defvar cite-cpp-menu-loaded '())

;; The rapidapp popup menu. All callbacks prefixed with rm short for
;; rapidapp menu
(defun cite-cpp-popup-menu ()
  (let ((cmcpp '("---------"
		 "---------"
		 "Mongoose C++ Mode Commands"
		 "---------"
		 ["About Mongoose" cm-about-cite t]
		 ["About This Mode" cmcpp-about-mode t]
		 "---------"
		 ("Compilation"
		  ["Template based Code" cmcpp-comp-templates t]
		  ["Vanilla C++ Code" cmcpp-comp-regular t])
		  "---------"
		  ("View Transformed Source"
		   ["After Front End Phase" cmcpp-view-xform-after-fe t]
		   ["After Inlining Phase" cmcpp-view-xform-after-inliner t]
		   ["After Code Generation" cmcpp-view-xform-after-cg t]))))
    (if (null cite-cpp-menu-loaded)
	(progn
	  (setq c-mode-menu (append c-mode-menu cmcpp))
	  (setq cite-cpp-menu-loaded t)))))
	    

;; Load the hypertextifying library
(load-library "cite-about")

(defun cmcpp-about-mode ()
  (interactive)
  (with-output-to-temp-buffer "*MONGOOSE-CPP-MODE*"
    (princ "This is a simple mode that allows people to play with the\n")
    (princ "cite C++ compiler to figure out the quality of code produced\n")
    (princ "It allows for:\n")
    (princ "* Compilation using the latest cite build\n")
    (princ "* Viewing the transformations that the compiler has performed\n")
    (princ "\n\n")
    (princ "Right Mouse button brings up the Mongoose CPP Menu\n")
    (princ "See also:\n")
    (princ "The Mongoose Mode for looking at fortranish optimizations such as loop nests")))

(load-library "cite-compilation")

(defun cmcpp-comp-templates ()
  (interactive)
  (let ((bfn (buffer-file-name (current-buffer))))
    (create-and-execute-shell-script
     (extract-prefix bfn)
     (format "%s ; echo Done Entire Thing; \n" 
	   (cpp-comp-command-for-templates bfn)))))

(defun cmcpp-comp-regular ()
  (interactive)
  (let ((bfn (buffer-file-name (current-buffer))))
    (create-and-execute-shell-script
     (extract-prefix bfn)
     (format "%s ; echo Done Entire Thing; \n" 
	   (cpp-comp-command-regular bfn)))))


(defvar my-arrow-extent nil)
(or (fboundp 'make-glyph) (fset 'make-glyph 'identity)) ; work w/ pre beta v12
(defvar my-arrow-glyph (make-glyph "=>"))

(make-face 'my-arrow-face)

;; The viewing call backs
(defun cmcpp-view-xform-after-fe ()
  (interactive)
  (let ((fn (buffer-file-name (current-buffer))))
    (let ((fp (extract-prefix fn)))
      (let ((after-fe-name (format "%s-after-fe.c" fp)))
	 (let ((buf (find-file-other-frame after-fe-name)))
	   (switch-to-buffer buf))))))

(defun cmcpp-view-xform-after-inliner ()
  (interactive)
  (let ((fn (buffer-file-name (current-buffer))))
    (let ((fp (extract-prefix fn)))
      (let ((after-fe-name (format "%s-after-inliner.c" fp)))
	 (let ((buf (find-file-other-frame after-fe-name)))
	   (switch-to-buffer buf))))))

(defun cmcpp-view-xform-after-cg ()
  (interactive)
  (let ((fn (buffer-file-name (current-buffer))))
    (let ((fp (extract-prefix fn)))
      (let ((after-fe-name (format "%s.s" fp)))
	 (let ((buf (find-file-other-frame after-fe-name)))
	   (switch-to-buffer buf))))))


(defun cite-cpp-startup ()
  (save-excursion
    (save-window-excursion
      (if (commandp 'shell)
	  (progn
	    (command-execute 'shell)
	    (switch-to-buffer-other-frame "*shell*")))))
  (cite-cpp-popup-menu))

(cite-cpp-startup)
