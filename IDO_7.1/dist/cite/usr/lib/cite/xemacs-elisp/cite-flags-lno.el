

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

(defmacro COMMENT (c1) '())

(defvar cite-lno-flag-opt t
  "
   General control over the LNO optimization level. Default is Full LNO 
   transformations")

(defvar cite-lno-flag-pragmas nil
  "
   By default, pragmas within a file override the command-line
   options.  This command-line options allows the user to have
   the command-line options override the pragmas in the file.")

(defvar cite-lno-flag-fission 1
  "
        0   no fission will be performed
        1   do normal fission as necessary in fiz_fuse phase
        2   try fission before fusion in fiz_fuse phase
            fission inner loop as much as possible in inner_fission phase

        If both -LNO:fission and -LNO:fusion (see below) are both set
        to 1 or 2, fusion is preferred.")

(defvar cite-lno-flag-fusion 1
  "
        0   no fusion will be performed
        1   do normal outer loop fusion and fiz_fuse phase fusion
        2   fuse outer loops even if it means partial fusion
            try fusion before fission in fiz_fuse phase
            allow partial fusion in fiz_fuse phase if not all levels
            can be fused in the multiple level fusion

	If both -LNO:fission and -LNO:fusion are both set to 1 or 2,
	fusion is preferred. Note that fiz_fuse phase of LNO is run
	regardless of these flag values. These flags will, however,
	affect the SNLs produced by fiz_fuse phase.")

(defvar cite-lno-flag-fusion-peeling-limit 5
  "
       Set the limit (n>=0) for number of iterations allowed to be
       peeled in fusion.")

(defvar cite-lno-flag-fission-register-limit 0
  " CHECK THIS?
    Set the limit (n>=0) for estimated register usage of loop bodies
    after inner loop fission in inner_fission phase.")

(defconst cite-flags-lno-menu 
  '("Loop Nest Optimizer (LNO)"
    "------"
    ("LNO Knobs/Switches"
     ["LNO optimization(-LNO:opt=)"
      (setq cite-lno-flag-opt (not cite-lno-flag-opt))
       :style toggle :selected cite-lno-flag-opt]
     ["Override Pragmas(-LNO:override_pragmas)"
      (setq cite-lno-flag-pragmas (not cite-lno-flag-pragmas))
       :style toggle :selected cite-lno-flag-pragmas]
     ("Fission"
      "----"
      "Fissioning Controls"
      ["No Fission" (setq cite-lno-flag-fission 0)
       :style radio :selected (= cite-lno-flag-fission 0)]
      ["Normal Fission" (setq cite-lno-flag-fission 1)
       :style radio :selected (= cite-lno-flag-fission 1)]
      ["Fission before Fusion" (setq cite-lno-flag-fission 2)
       :style radio :selected (= cite-lno-flag-fission 2)]
      "----"
      ["Set Fission Inner Register Limit" cite-flags-lno-set-fiss-reg nil])
     ("Fusion"
      "----"
      "Fusion Controls"
      ["No Fusion" (setq cite-lno-flag-fusion 0)
       :style radio :selected (= cite-lno-flag-fusion 0)]
      ["Normal Fusion" (setq cite-lno-flag-fusion 1)
       :style radio :selected (= cite-lno-flag-fusion 1)]
      ["?" (setq cite-lno-flag-fusion 2)
       :style radio :selected (= cite-lno-flag-fusion 2)]
      "----"
      ["Set Fusion Peeling Limit" cite-flags-lno-set-fuse-peel-limit t])

    "------")))

(defun cite-flags-lno-set-fiss-reg (maxreg)
  "
    Set the limit (n>=0) for estimated register usage of loop bodies
    after inner loop fission in inner_fission phase."
  (interactive "NMax number of registers in loop bodies after inner loop fission= ")
  (setq cite-lno-flag-fiss-reg-limit maxreg))

(defun cite-flags-lno-set-fuse-peel-limit (max-peel)
 "
   Set the limit (n>=0) for number of iterations allowed to be
   peeled in fusion."
 (interactive "NMax number of iterations allowed to be peeled in fusion= ")
 (with-output-to-temp-buffer "*LNO Flags*"
   (princ (format "Old value of -LNO:fusion_peeling_limit = %d" cite-lno-flag-fusion-peeling-limit)))
 (setq cite-lno-flag-fusion-peeling-limit max-peel))
 

