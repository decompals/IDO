

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

(require 'hyper-apropos)
(require 'x-toolbar)

(defvar cite-icon
  (if (featurep 'xpm)
      (toolbar-make-button-list
       (format "%s/mongoose-tiny.xpm" cite-image-location)
       nil
       (format "%s/mongoose-tiny.xpm" cite-image-location)
       (format "%s/mongoose-tiny.xpm" cite-image-location)
       nil
       (format "%s/mongoose-tiny.xpm" cite-image-location)))
  "Cite(Mongoose) Icon.")

(defvar cite-options-icon
  (if (featurep 'xpm)
      (toolbar-make-button-list
       (format "%s/cite-options.xpm" cite-image-location)
       nil
       (format "%s/cite-options.xpm" cite-image-location)
       (format "%s/cite-options.xpm" cite-image-location)
       nil
       (format "%s/cite-options.xpm" cite-image-location)))
  "Cite Customization Option Icon.")


(defvar cite-compile-icon
  (if (featurep 'xpm)
      (toolbar-make-button-list
       (format "%s/compile.xpm" cite-image-location)
       nil
       (format "%s/compile.xpm" cite-image-location)
       (format "%s/compile.xpm" cite-image-location)
       nil
       (format "%s/compile.xpm" cite-image-location)))
  "Compilation Icon.")

(defvar cite-annotate-hypertext-icon
  (if (featurep 'xpm)
      (toolbar-make-button-list
       (format "%s/ann-hyper.xpm" cite-image-location)
       nil
       (format "%s/ann-hyper.xpm" cite-image-location)
       (format "%s/ann-hyper.xpm" cite-image-location)
       nil
       (format "%s/ann-hyper.xpm" cite-image-location)))
  "Annotate/Hypertext ICON.")

(defvar cite-view-icon
  (if (featurep 'xpm)
      (toolbar-make-button-list
       (format "%s/view.xpm" cite-image-location)
       nil
       (format "%s/view.xpm" cite-image-location)
       (format "%s/view.xpm" cite-image-location)
       nil
       (format "%s/view.xpm" cite-image-location)))
  "View ICON.")

(defvar cite-xform-icon
  (if (featurep 'xpm)
      (toolbar-make-button-list
       (format "%s/xform.xpm" cite-image-location)
       nil
       (format "%s/xform.xpm" cite-image-location)
       (format "%s/xform.xpm" cite-image-location)
       nil
       (format "%s/xform.xpm" cite-image-location)))
  "Xform ICON.")

(defvar cite-compile-switches-icon
  (if (featurep 'xpm)
      (toolbar-make-button-list
       (format "%s/switches.xpm" cite-image-location)
       nil
       (format "%s/switches.xpm" cite-image-location)
       (format "%s/switches.xpm" cite-image-location)
       nil
       (format "%s/switches.xpm" cite-image-location)))
  "Xform ICON.")
    

(defvar cite-toolbar-spec
  '([toolbar-file-icon	   find-file	t	"Open a file"	]
    [toolbar-folder-icon   dired	t	"View directory"]
    [toolbar-disk-icon	   save-buffer	t	"Save buffer"	]
    [toolbar-printer-icon  lpr-buffer	t	"Print buffer"	]
    [toolbar-cut-icon	   x-kill-primary-selection t "Kill region"]
    [toolbar-copy-icon	   x-copy-primary-selection t "Copy region"]
    [toolbar-paste-icon	   x-yank-clipboard-selection t "Paste from clipboard"]
    [toolbar-undo-icon	   undo		t	"Undo edit"	]
    [toolbar-replace-icon  query-replace	t	"Replace text"	]
    [toolbar-info-icon     toolbar-info    t       "Information"   ]

    [cite-icon      cm-about-cite  t "About CITE in HTML (Netscape 2.0 enabled)"]
    [cite-options-icon cite-edit-options t "Cite Options"]
;    [cite-compile-icon cite-compile nil "Show Compilation Toolbar"]
;    [cite-compile-switches-icon cite-compile-switches t "Show Compilation Switches Toolbar"]
    [cite-annotate-hypertext-icon cite-annotate-hypertext t "Show Annotate/Hypertext Toolbar"]
    [cite-view-icon cite-view t "Show View Transformed Source Toolbar"]
;    [cite-xform-icon cite-xform nil "Show Transformation Toolbar"]
    )
  "A toolbar for Cite")


(defvar cite-lno-icon
  (if (featurep 'xpm)
      (toolbar-make-button-list
       (format "%s/lno.xpm" cite-image-location)
       nil
       (format "%s/lno.xpm" cite-image-location)
       (format "%s/lno.xpm" cite-image-location)
       nil
       (format "%s/lno.xpm" cite-image-location)))
  "LNO Icon.")

(defvar cite-ipa-icon
  (if (featurep 'xpm)
      (toolbar-make-button-list
       (format "%s/ipa.xpm" cite-image-location)
       nil
       (format "%s/ipa.xpm" cite-image-location)
       (format "%s/ipa.xpm" cite-image-location)
       nil
       (format "%s/ipa.xpm" cite-image-location)))
  "IPA Icon.")

(defvar cite-cg-icon
  (if (featurep 'xpm)
      (toolbar-make-button-list
       (format "%s/cg.xpm" cite-image-location)
       nil
       (format "%s/cg.xpm" cite-image-location)
       (format "%s/cg.xpm" cite-image-location)
       nil
       (format "%s/cg.xpm" cite-image-location)))
  "CG Icon.")

(defvar cite-opt-icon
  (if (featurep 'xpm)
      (toolbar-make-button-list
       (format "%s/opt.xpm" cite-image-location)
       nil
       (format "%s/opt.xpm" cite-image-location)
       (format "%s/opt.xpm" cite-image-location)
       nil
       (format "%s/opt.xpm" cite-image-location)))
  "OPT icon.")


(defvar cite-compile-switches-toolbar-spec
  '([cite-ipa-icon	cite-ipa-show t "Inter Procedural Analyzer (Left Mouse to get Menu)"]
    [cite-lno-icon	cite-lno-show t "Loop Nest Optimizer (Left Mouse to get Menu)"	]
    [cite-cg-icon	cite-cg-show  nil "Code Generator (Left Mouse to get Menu)"]
    [cite-opt-icon	cite-opt-show nil "Global Optimizer (Left Mouse to get Menu)"		])
    "Toolbar for Cite Compilation Switches")

(defun cite-ipa-show ()
  (interactive)
  (popup-menu cite-flags-ipa-menu))

(defun cite-lno-show ()
  (interactive)
  (popup-menu cite-flags-lno-menu))

(defun cite-compile-switches ()
  (interactive)
  (let ((height (+ 4 (glyph-height (car cite-lno-icon))))
	(width (+ 4 (glyph-width (car cite-lno-icon)))))
    (add-spec-list-to-specifier right-toolbar-width 
				`((global (nil . ,width))))
    (set-specifier right-toolbar (cons (current-buffer) 
				      cite-compile-switches-toolbar-spec))))

;; Annotate and Hypertext toolbar related

(defvar cite-ann-orig-loop-nest-icon
  (if (featurep 'xpm)
      (toolbar-make-button-list
       (format "%s/ann-orig-loop-nest.xpm" cite-image-location)
       nil
       (format "%s/ann-orig-loop-nest.xpm" cite-image-location)
       (format "%s/ann-orig-loop-nest.xpm" cite-image-location)
       nil
       (format "%s/ann-orig-loop-nest.xpm" cite-image-location)))
  "Annotate Original Loop Nest icon.")

(defvar cite-ann-post-ipa-loop-nest-icon
  (if (featurep 'xpm)
      (toolbar-make-button-list
       (format "%s/ann-post-ipa-loop-nest.xpm" cite-image-location)
       nil
       (format "%s/ann-post-ipa-loop-nest.xpm" cite-image-location)
       (format "%s/ann-post-ipa-loop-nest.xpm" cite-image-location)
       nil
       (format "%s/ann-post-ipa-loop-nest.xpm" cite-image-location)))
  "Annotate Loop Nests (POST IPA) icon")

(defvar cite-ann-post-lno-loop-icon
  (if (featurep 'xpm)
      (toolbar-make-button-list
       (format "%s/ann-post-lno-loop.xpm" cite-image-location)
       nil
       (format "%s/ann-post-lno-loop.xpm" cite-image-location)
       (format "%s/ann-post-lno-loop.xpm" cite-image-location)
       nil
       (format "%s/ann-post-lno-loop.xpm" cite-image-location)))
  "Annotate Post LNO Loops icon")

(defvar cite-ann-post-lno-prefetch-icon
  (if (featurep 'xpm)
      (toolbar-make-button-list
       (format "%s/ann-post-lno-prefetch.xpm" cite-image-location)
       nil
       (format "%s/ann-post-lno-prefetch.xpm" cite-image-location)
       (format "%s/ann-post-lno-prefetch.xpm" cite-image-location)
       nil
       (format "%s/ann-post-lno-prefetch.xpm" cite-image-location)))
  "Annotate Post LNO Prefetch icon")


(defvar cite-ann-post-lno-inner-icon
  (if (featurep 'xpm)
      (toolbar-make-button-list
       (format "%s/ann-post-lno-inner.xpm" cite-image-location)
       nil
       (format "%s/ann-post-lno-inner.xpm" cite-image-location)
       (format "%s/ann-post-lno-inner.xpm" cite-image-location)
       nil
       (format "%s/ann-post-lno-inner.xpm" cite-image-location)))
  "Annotate Inner Loops (Post LNO) icon")

(defvar cite-ann-func-icon
  (if (featurep 'xpm)
      (toolbar-make-button-list
       (format "%s/ann-func.xpm" cite-image-location)
       nil
       (format "%s/ann-func.xpm" cite-image-location)
       (format "%s/ann-func.xpm" cite-image-location)
       nil
       (format "%s/ann-func.xpm" cite-image-location)))
  "Annotate Functions/Subroutines ICON")

(defvar cite-annotate-hypertext-toolbar-spec
  '([cite-ann-orig-loop-nest-icon	cm-annotate-lno-snl t "Annotate/Hypertext Loop Nests"]
    [cite-ann-post-ipa-loop-nest-icon cm-annotate-lno-snl-ipa t "Annotate/Hypertext Loop Nests in transformed source after IPA"	]
    [cite-ann-post-lno-loop-icon cm-annotate-cg-all  t "Annotate/Hypertext Loops in transformed source after LNO"]
    [cite-ann-post-lno-prefetch-icon cm-annotate-prefetch  t "Add prefetch information to annotated/hypertexted post LNO loops"]
;    [cite-ann-func-icon cm-annotate-func nil "Annotate/Hypertext Functions/Subroutines"		]
    )
  "Toolbar for Cite Annotation/Hypertexting")

(defun cite-annotate-hypertext ()
  "
   Shows the Annotate Hypertext Toolbar (Right toolbar). The toolbar is made buffer 
   local (set-specifier for  the right toolbar is made to take current-buffer into 
   account)"
  (interactive)
  (let ((height (+ 4 (glyph-height (car cite-ann-orig-loop-nest-icon))))
        (width (+ 4 (glyph-width (car cite-ann-orig-loop-nest-icon)))))
    (add-spec-list-to-specifier right-toolbar-width 
				`((global (nil . ,width))))
    (set-specifier right-toolbar 
		   (cons (current-buffer) cite-annotate-hypertext-toolbar-spec))))

;; Viewing Transformed source ICON
(defvar cite-view-src-after-fe-icon
  (if (featurep 'xpm)
      (toolbar-make-button-list
       (format "%s/src-fe.xpm" cite-image-location)
       nil
       (format "%s/src-fe.xpm" cite-image-location)
       (format "%s/src-fe.xpm" cite-image-location)
       nil
       (format "%s/src-fe.xpm" cite-image-location)))
  "View Transformed Source after FE Icon")

(defvar cite-view-src-after-ipa-icon
  (if (featurep 'xpm)
      (toolbar-make-button-list
       (format "%s/src-ipa.xpm" cite-image-location)
       nil
       (format "%s/src-ipa.xpm" cite-image-location)
       (format "%s/src-ipa.xpm" cite-image-location)
       nil
       (format "%s/src-ipa.xpm" cite-image-location)))
  "View Transformed Source after IPA Icon")

(defvar cite-view-src-after-lno-icon
  (if (featurep 'xpm)
      (toolbar-make-button-list
       (format "%s/src-lno.xpm" cite-image-location)
       nil
       (format "%s/src-lno.xpm" cite-image-location)
       (format "%s/src-lno.xpm" cite-image-location)
       nil
       (format "%s/src-lno.xpm" cite-image-location)))
  "View Transformed Source after LNO Icon")

(defvar cite-view-src-after-cg-icon
  (if (featurep 'xpm)
      (toolbar-make-button-list
       (format "%s/src-cg.xpm" cite-image-location)
       nil
       (format "%s/src-cg.xpm" cite-image-location)
       (format "%s/src-cg.xpm" cite-image-location)
       nil
       (format "%s/src-cg.xpm" cite-image-location)))
  "View Transformed Source after CG Icon")

(defvar cite-view-src-toolbar-spec
  '([cite-view-src-after-fe-icon	cm-view-xform-after-fe nil "Show Transformed Source after Front End Processing"]
    [cite-view-src-after-ipa-icon cm-view-xform-after-ipa nil "Show Transformed Source after IPA(Inlining) Processing"	]
    [cite-view-src-after-lno-icon cm-view-xform-after-lno  t "Show Transformed Source after Loop Nest Optimizer"	]
    [cite-view-src-after-cg-icon  cm-view-xform-after-cg t "Show Transformed Source after Code Generator (i.e Assembly Code)"		])
  "Toolbar for Cite View")

(defun cite-view ()
  "
   Shows the View Toolbar (Right toolbar). The toolbar is made buffer local 
  (set-specifier for the right toolbar is made to take current-buffer into account)"
  (interactive)
  (let ((height (+ 4 (glyph-height (car cite-view-src-after-lno-icon))))
        (width (+ 4 (glyph-width (car cite-view-src-after-lno-icon)))))
    (add-spec-list-to-specifier right-toolbar-width 
				`((global (nil . ,width))))
    (set-specifier right-toolbar 
		   (cons (current-buffer) cite-view-src-toolbar-spec))))

