

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

(defvar cite-ipa-flag-addressing nil
  "Perform addr-taken analysis in alias analyzer
   (default FALSE -- to become TRUE eventually)"
  )

(defvar cite-ipa-flag-aggr-cprop nil
 "Perform aggressive interprocedural constant propagation.  By
   default, non-aggressive interprocedural constant propagation is on.
   This goes a step furthur in that it tries to eliminate constant
   parameters at callsites and their corresponding formals."
 )

(defvar cite-ipa-flag-alias nil
 "(implied/forced by opt_alias below)
   Perform alias/mod/ref analysis
   (default FALSE -- to become TRUE eventually)"
 )

(defvar cite-ipa-flag-opt-alias nil
  "Emit alias/mod/ref analysis to summary file and use in WOPT
    (default FALSE -- to become TRUE eventually)"  
  )

(defvar cite-ipa-flag-dfe t
  "Perform dead function elimination
   (default TRUE)"
  )
  

(defvar cite-ipa-flag-inline t
  "Perform inline processing (during main IPA processing: does not affect
    the stand-alone inliner) (default TRUE)"
  )

(defvar cite-ipa-flag-pic-opt t
 "Perform PIC optimization
  (default TRUE)"
 )

(defvar cite-ipa-flag-autognum t
  "Perform AutoGnum optimization
   (default TRUE)"
  )

(defvar cite-ipa-flag-dve t
  "Turn on/off dead global variables eliminiation (default TRUE)."
  )

(defvar cite-ipa-flag-cgi t
  "Turn on/off constant globals identification.  Global variables
   (except arrays) that are never modified are marked as constant.
   And for scalar, the constant values are propagated to all object
   files.  (default TRUE)"
  )

(defvar cite-ipa-flag-cprop t
  "Perform interprocedural constant propagation
   (default TRUE)"
  )

(defvar cite-ipa-flag-compile t
  "Call ipacom (to perform back-end compilation) after IPA (default TRUE)"
  )

(defvar cite-ipa-flag-depth 100000
  "Inline nodes at depth <= n in the call graph. Leaf nodes are
   at depth 0.  Inlining still subject to space limit (see space and
   plimit below)."
  )

(defvar cite-ipa-flag-space 25
   "Inline until a program factor of n% is reached. Example, n=20 implies
    stop inlining if the program has grown in size by 20%."
   )

(defvar cite-ipa-flag-plimit 2500
   "Inline calls into a procedure until the procedure
    has grown to a size of n, where n is the number of whirl nodes."
   )

(defvar cite-ipa-flag-small-pu 30
  "Callees with size (# of WHIRL nodes) smaller than n
   are always inlined, even though the result might
   exceed the plimit described above.  However, small PUs
   are *NOT* inlined if the size of the caller+callee will
   exceed (1.25 * plimit).  The default is 30."
  )


(defconst cite-flags-ipa-menu
  '("Inter Procedural Analysis (IPA)"
    "-------"
    ["IPA on/off" (cite-ipa-flag-toggle)
     :style toggle :selected cite-ipa-flag-status]
    "-------"
    ("IPA Knobs/Switches" :included cite-ipa-flag-status
     ("IPA internal controls"
      ["Addressing (addressing)" 
       (setq cite-ipa-flag-addressing (not cite-ipa-flag-addressing))
       :style toggle :selected cite-ipa-flag-addressing]
      ["Aggressive Interprocedural Constant Propagation (aggr_cprop)"
       (setq cite-ipa-flag-aggr-cprop (not cite-ipa-flag-aggr-cprop))
       :style toggle :selected cite-ipa-flag-aggr-cprop]
      ["Perform alias/mod/ref analysis (alias)"
       (setq cite-ipa-flag-alias (not cite-ipa-flag-alias))
       :style toggle :selected cite-ipa-flag-alias]
      ["Alias/mod/ref analysis for Global Optimizer (opt_alias)"
       (progn
	 (setq cite-ipa-flag-opt-alias (not cite-ipa-flag-opt-alias))
	 (setq cite-ipa-flag-alias t))
       :style toggle :selected cite-ipa-flag-opt-alias]
      ["Dead Function Elimination (dfe)"
       (setq cite-ipa-flag-dfe (not cite-ipa-flag-dfe))
       :style toggle :selected cite-ipa-flag-dfe]
      ["Inline (inline)"
       (setq cite-ipa-flag-inline (not cite-ipa-flag-inline))
       :style toggle :selected cite-ipa-flag-inline]
      ["PIC optimization?"
       (setq cite-ipa-flag-pic-opt (not cite-ipa-flag-pic-opt))
       :style toggle :selected cite-ipa-flag-pic-opt]
      ["AutoGnum optimization?"
       (setq cite-ipa-flag-autognum (not cite-ipa-flag-autognum))
       :style toggle :selected cite-ipa-flag-autognum]
      ["Dead Variable Elimination (dve)"
       (setq cite-ipa-flag-dve (not cite-ipa-flag-dve))
       :style toggle :selected cite-ipa-flag-dve]
      ["Constant Globals Identification (cgi)"
       (setq cite-ipa-flag-cgi (not cite-ipa-flag-cgi))
       :style toggle :selected cite-ipa-flag-cgi]
      ["Constant Propagation (cprop)"
       (setq cite-ipa-flag-cprop (not cite-ipa-flag-cprop))
       :style toggle :selected cite-ipa-flag-cprop]
      ["Backend Compile (compile)"
       (setq cite-ipa-flag-compile (not cite-ipa-flag-compile))
       :style toggle :selected cite-ipa-flag-compile])
     ("IPA Inline Controls"
      ["Set Inlining Depth (depth=)" cite-flags-ipa-set-depth nil]
      ["Force Depth (no size restriction) (forcedepth=)" cite-flags-ipa-force-depth nil]
      ["Set Space (space=)" cite-flags-ipa-set-space t]
      ["Set Procedure Limit" cite-flags-ipa-set-plimit nil]
      ["Set Always inlined PU size" cite-flags-ipa-set-small-pu nil]
      ["Change Defaults using dialog box" cite-flag-ipa-menu-user-inline t]))
    ["LNO/SWP with and without IPA" mm-comp-lno-swp-ipa    t]))


(defun cite-flag-ipa-menu-user-inline ()
  (interactive)
  (let ((dbox 
	 (cons
	  (format "IPA: User Controls for varying inlining behavior\nInlining Depth = %d\t Space = %d\t Procedure Unit (PU) Limit=%d\nAlways Inlined PU size = %d\n\n" cite-ipa-flag-depth cite-ipa-flag-space cite-ipa-flag-plimit cite-ipa-flag-small-pu)
	 '(
	   ["Set\nInlining Depth" cite-flags-ipa-set-depth nil]
	   ["Force Depth\n(no size restriction)" cite-flags-ipa-force-depth nil]
	   ["Set\nSpace" cite-flags-ipa-set-space t]
	   ["Set\nProcedure Limit" cite-flags-ipa-set-plimit nil]
	   ["Set\nAlways inlined\nPU size" cite-flags-ipa-set-small-pu nil]
	   nil
	   ["Cancel" (progn (beep) (message "Quit")) t]))))
    (popup-dialog-box dbox)))

(defun cite-flags-ipa-set-space (slimit)
  (interactive "NInline until the program grows by percentage n = ")
  (with-output-to-temp-buffer "*IPA Flag Default*"
    (princ (format "Old value of space was %d\n" cite-ipa-flag-space)))
  (setq cite-ipa-flag-space slimit))
