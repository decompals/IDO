

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

;;; cite-about.el
;;; Primitive documentation

(defvar netscape-process '())

(defun start-netscape-process (url)
  (let ((p (start-process "netscape-jumper" nil "/usr/lib/cite/doc/netscape_jumper"  url)))
    (setq netscape-process p)))

(defvar cite-home-page "file:/usr/lib/cite/doc/cite-html/cite-concepts.html")

(defvar cite-annotate-src-page "file:/usr/lib/cite/doc/cite-html/cite-interchange-annotate.html")

(defvar cite-view-xform-page "file:/usr/lib/cite/doc/cite-html/cite-mm-intro.html")

(defun cm-about-cite ()
  (interactive)
  (start-netscape-process cite-home-page))

(defun cm-about-viewing-xform ()
  (interactive)
  (start-netscape-process cite-view-xform-page))

(defun cm-about-annotate-src ()
  (interactive)
  (start-netscape-process cite-annotate-src-page))

