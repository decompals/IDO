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

;; cite.el
;; The top level file for CITE.

(defconst cite-image-location 
  "/usr/lib/cite/xemacs-elisp/image"
  "Location of the images that CITE uses")


;; The following three variables are buffer local
(defvar cite-menu-added '()
  "Has the cite menu been added to the menubar")

(defvar cite-c-menu-loaded '()
  "Flag that indicates if C menu is loaded")

(defvar cite-src-xform-map '()
  "Contains the source to transformed source correlation(mapping) particular to that buffer")
  


;; Start Customization Variables for CITE
(defvar cite-filename-after-lno "-after-lno"
  "
   If original filename is x.f CITE assumes that the transformed filename 
   after LNO(Loop Nest Optimizer) is x-after-lno.f")

(defvar cite-filename-after-ipa "-after-ipa"
  "
   If original filename is x.f CITE assumes that the transformed filename 
   after IPA inline processing is  x-after-ipa.f")

(defvar cite-shell-on-startup "TRUE"
  "
   CITE always brings up a shell window on startup. Change it to FALSE to 
   disable this behavior")

(defvar cite-implementers-image "FALSE"
  "
   If you would like CITE to display the implementers images in the hypertext driven
   info buffers set this to TRUE")

(defvar cite-vi-menu-showing "TRUE"
  "
   If you would like to disable showing Vi Mode Menu in the menubar set this
   to be FALSE")
;; End customization variables for CITE

(defconst cite-menu
	 '("Cite-Mode" 
	   ["About Cite" cm-about-cite t]
	   "---------"
	   ("View Transformations"
	    ["About Viewing Transformations" cm-about-viewing-xform t]
	    ["After Inter Procedural Analyzer" cm-view-xform-after-ipa t]
	    ["After Loop Nest Optimizer" cm-view-xform-after-lno t]
	    ["After Code Generator" cm-view-xform-after-cg t])
	   "---------"
	   ("Annotate Source" 
	    ["About Annotating Source" cm-about-annotate-src t]
	    ["Annotate Loop Nests" cm-annotate-lno-snl t]
	    ["Annotate Loop Nests with/without IPA" cm-annotate-lno-snl-ipa t]))
	 "The pop up menu when right mouse is clicked, Also appears in menubar")


(defun cite-fortran-popup-menu ()
  "Right Mouse Popup menu for fortran mode"
  (setq-default mode-popup-menu cite-menu))


(defun cite-c-popup-menu ()
  "Right Mouse Popup menu for C-mode"
  (if (null cite-c-menu-loaded)
      (progn
	(setq c-mode-menu (append c-mode-menu cite-menu))
	(setq cite-c-menu-loaded t))))

;; The cite popup menu. All callbacks prefixed with cm short for
;; cite menu
(defun cm-popup ()
  (interactive)
  (popup-menu cite-menu))

;; Load the various libraries

(load-file "/usr/lib/cite/xemacs-elisp/cite-toolbar.elc")
(load-file "/usr/lib/cite/xemacs-elisp/cite-about.elc")
(load-file "/usr/lib/cite/xemacs-elisp/cite-edit-options.elc")
(load-file "/usr/lib/cite/xemacs-elisp/cite-compilation.elc")
(load-file "/usr/lib/cite/xemacs-elisp/cite-transformed.elc")
(load-file "/usr/lib/cite/xemacs-elisp/cite-lno-snl.elc")
(load-file "/usr/lib/cite/xemacs-elisp/cite-lno-prefetch.elc")
(load-file "/usr/lib/cite/xemacs-elisp/cite-cg.elc")

(defun make-ipa-name (file-name)
  (let ((p (extract-prefix file-name)))
    (cond
     ((eq major-mode 'fortran-mode) (format "%s%s.f" p cite-filename-after-ipa))
     ((or (eq major-mode 'c-mode)
	  (eq major-mode 'cc-mode)) (format "%s%s.c" p cite-filename-after-ipa)))))


(defun make-lno-name (prefix)
  "The name of the transformed file after LNO. If the original file is
   x.f the name of file after lno is x-after-lno.f (by default)."
  (cond
   ((eq major-mode 'fortran-mode) (format "%s%s.f" prefix cite-filename-after-lno))
   ((or (eq major-mode 'c-mode)
	(eq major-mode 'cc-mode)) (format "%s%s.c" prefix cite-filename-after-lno))))

(defun make-lno-cg-name (prefix)
  (format "%s%s.s" prefix cite-filename-after-lno))

(defun cm-view-xform-after-lno ()
  "View the transformed source after the loop nest optimizer"
  (interactive "@") 
  (let ((fn (buffer-file-name (current-buffer))))
    (let ((fp (extract-prefix fn)))
      (let ((after-lno-name (make-lno-name fp)))
	(if (file-exists-p after-lno-name)
	    (corresponding-xformed-src after-lno-name (point) (point))
	  (with-output-to-temp-buffer "*CITE-LNO-HELP*"
	    (let ((nd-after-lno-name (file-name-nondirectory after-lno-name)))
	      (princ (format "Transformed file %s does not exist\n" nd-after-lno-name))
	      (princ "For Fortran and C Source please compile with -LIST:cite or -LIST:cite=on"))))))))

		   

(defun cm-view-xform-after-cg ()
  "Viewing the transformed source after the code generator"
  (interactive)
  (let ((fn (buffer-file-name (current-buffer))))
    (let ((fp (extract-prefix fn)))
      (let ((after-cg-name (format "%s.s" fp)))
	(if (file-exists-p after-cg-name)
	    (show-assembly fn after-cg-name)
	  (with-output-to-temp-buffer "*CITE-CG-HELP*"
	    (let ((nd-after-cg-name (file-name-nondirectory after-cg-name)))
	      (princ (format "Assembly file %s does not exist\n" nd-after-cg-name))
	      (princ (format "Please compile with -LIST:cite to get the assembly file\n")))))))))
	  

(defun cm-annotate-lno-snl ()
  "Annotating original source with loop nest info"
  (interactive)
  (save-excursion
    (save-window-excursion
      (lno-driver (buffer-file-name (current-buffer))))))

(defun cm-annotate-lno-snl-ipa ()
  "Annotating loop nests of transformed source after IPA"
  (interactive)
  (save-excursion
    (save-window-excursion
      (lno-driver (buffer-name (current-buffer)))
      (let ((after-ipa-name (make-ipa-name (buffer-file-name (current-buffer)))))
	(if (file-exists-p after-ipa-name)
	    (progn
	      (find-file-other-frame after-ipa-name)
	      (lno-driver after-ipa-name))
	  (with-output-to-temp-buffer "*CITE-IPA-HELP*"
	    (princ (format "Transformed file %s after IPA does not exist\n" 
			   (file-name-nondirectory after-ipa-name)))))))))

(defun cm-annotate-cg-all ()
  "
   Annotating all loops of transformed source after LNO with cg and LNO information 
   about the loop. It needs the original file, the transformed file, the listing file,
   and the assembly file to accomplish its job."
  (interactive)
  (let ((xform-buf '()))
    (let ((fn (buffer-file-name (current-buffer))))
      (let ((fp (extract-prefix fn)))
	(let ((after-lno-name (make-lno-name fp))
	      (after-cg-name (format "%s.s" fp)))
	  (if (and (file-exists-p after-lno-name)
		   (file-exists-p after-cg-name))
	      (save-excursion
		(save-window-excursion
		  (let ((xform-present
			 (find-if (function
				   (lambda (x)
				     (string= (buffer-file-name x) 
					      after-lno-name)))
				  (buffer-list))))
		    (if (not xform-present)
			(setq xform-buf (find-file-other-frame after-lno-name))
		      (setq xform-buf xform-present))
		    (switch-to-buffer xform-buf)
		    (lno-cg-annotate-all-loop fp))))
	    (let ((nd-after-lno-name (file-name-nondirectory after-lno-name)))
	      (let ((nd-after-cg-name (file-name-nondirectory after-cg-name)))
		(with-output-to-temp-buffer "*CITE-LNO-CG-HELP*"
		  (cond
		   ((not (file-exists-p after-lno-name))
		    
		    (princ  (format "Transformed file %s does not exist\n" nd-after-lno-name))
		    
		    (princ "For Fortran and C source please compile with -LIST:cite to produce transformed files\n"))
		   ((not (file-exists-p after-cg-name))
		    
		    (princ (format "Assembly file %s does not exist\n" nd-after-cg-name))
		    (princ (format "Please compile %s with -O3 -LIST:cite to produce all relevant files including assembly file\n" (file-name-nondirectory fn))))))))))))
    (let ((curr-line (current-line)))
      (let ((xl (find-if (function (lambda (x)
				     (= (second x) curr-line)))
			 cite-src-xform-map)))
	(if (not (null xl))
	    (display-line xform-buf (first xl))
	  (message "No line number information in transformed source corresponding to this line"))))))

    
    

(defun cm-annotate-prefetch ()
  "
   Annotating all loops of transformed source after LNO with prefetch information.
   It needs the original file, the transformed file, and the listing file to 
   accomplish its job"
  (interactive)
;  (cm-annotate-cg-all) ; should check if it is already done.
  (message "Click on annotate post LNO loops and prefetch information would have been added")
  (lno-prefetch-driver (buffer-file-name (current-buffer))))

;;(require 'viper)	       
(defun viper-vi-emacs-toggle ()
  "Toggle between vi and emacs in viper mode. Bring up viper mode as well"
  (interactive)
  (if (not (null (car (find-menu-item current-menubar '("Vi Mode")))))
     (progn
       (require 'viper)
       (viper-mode)
       (vip-change-state-to-vi)
       (delete-menu-item '("Vi Mode"))
       (add-menu-button nil ["Emacs Mode" viper-vi-emacs-toggle t] nil))
     (progn
       (vip-change-state-to-emacs)
       (delete-menu-item '("Emacs Mode"))
       (add-menu-button nil ["Vi Mode" viper-vi-emacs-toggle t] nil))))

(defun cite-general-startup ()
  "The startup common to both fortran and C"
  (make-local-variable 'cite-menu-added)
  (make-local-variable 'cite-c-menu-loaded)
  (make-local-variable 'cite-src-xform-map)
  (setq cite-src-xform-map (read-loc-file))
  (run-hooks 'cite-hook)
  (if (string= cite-vi-menu-showing "TRUE")
      (add-menu-button nil ["Vi Mode" viper-vi-emacs-toggle t] nil))
  (if (null cite-menu-added)
      (progn
	(add-submenu '() cite-flags-menu "Help")
	(add-submenu '() cite-menu "Cite-Flags")
	(setq cite-menu-added t)))
  (let ((height (+ 4 (glyph-height (car cite-annotate-hypertext-icon))))
        (width (+ 4 (glyph-width (car cite-annotate-hypertext-icon)))))
    (if (not (null default-toolbar))
	(progn ;; use default toolbar position.
	  (add-spec-list-to-specifier left-toolbar-width 
				      `((global (nil . ,width))))
	  (set-specifier default-toolbar (cons (current-buffer)
					       cite-toolbar-spec)))))
  (if (string= cite-shell-on-startup "TRUE")
      (save-excursion
	(save-window-excursion
	  (let ((shell-exists (find-if (function
					(lambda (x)
					  (string= (buffer-name x) "*shell*")))
				       (buffer-list))))
	    (if (not shell-exists)
		(if (commandp 'shell)
		    (progn
		      (command-execute 'shell)
		      (switch-to-buffer-other-frame "*shell*")))))))))

(defun cite-c-startup ()
  "C startup"
  (cite-general-startup)
  (cite-c-popup-menu))

(defun cite-fortran-startup ()
  "Fortran startup"
  (cite-general-startup)
  (cite-fortran-popup-menu))


;; Hooks for C and fortran
(add-hook 'c-mode-common-hook 'cite-c-startup)
(add-hook 'fortran-mode-hook 'cite-fortran-startup)


