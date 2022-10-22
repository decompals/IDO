

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

;; Do all the things that set700 does but more cleanly.

(defun load-up-compilation-environment ()
  (comint-send-string (inferior-shell-proc)
		      (format "setenv TOOLROOT /hosts/alliant.mti/700A_test/LATEST_alpha0; setenv ROOT $TOOLROOT\n")
		      ))
  

(defvar compilation-TOOLROOT "\$TOOLROOT")


;; (file-name-sans-extension "doomsday/foo/bar/barf.c") returns
;;  "doomsday/foo/bar/barf"
;; (file-name-sans-extension "doomsday.foo.bar/moo.goo.zoo/barfed.c.c")
;; return "doomsday.foo.bar/moo.goo.zoo/barfed.c"

(defun file-name-sans-extension (file-name)
  "This function returns FILENAME minus its extension if any.  The
     extension, in a file name, is the part that starts with the last
    '.' in the last name component.  For example,
          (file-name-sans-extension \"foo.lose.c\")
               => \"foo.lose\"
          (file-name-sans-extension \"big.hack/foo\")
               => \"big.hack/foo\""
  (let ((l (split-string file-name "\\.")))
    (if (= (length l) 2)
	(first l)
      (let ((rl (reverse l)))
	(let ((rl-but-last (cdr rl)))
	  (let ((l-of-strings-wo-extn (reverse 
					(cons (first rl-but-last)
					      (mapcar (function 
						       (lambda (x)
							 (concat x ".")))
						      (cdr rl-but-last))))))
	    (eval (cons 'concatenate (cons (list 'quote 'string)
					   l-of-strings-wo-extn)))))))))


;; Buggy (No longer)
(defun extract-prefix (file-name)
  (file-name-sans-extension file-name))

(defun inferior-shell-proc ()
  (get-buffer-process "*shell*"))


(defun create-and-execute-shell-script (prefix command)
  (with-output-to-temp-buffer (format "%s.sh" prefix)
    (princ "#!/bin/sh\n")
    (princ (format "%s" command))
    (princ (format "/bin/rm -f *.[BN]\n")))
  (save-excursion
    (save-window-excursion
      (switch-to-buffer (format "%s.sh" prefix))
      (write-file (format "%s.sh" prefix))))
  (let ((is (inferior-shell-proc)))
    (comint-send-string 
     is 
     (format "sh %s.sh ; /bin/rm %s.sh; echo Done\n"  
	     prefix prefix))))


;; Fortran 77 Specific Compilation

(defun comp-command-2get-src-after-fe (file-name)
  (let ((is (inferior-shell-proc)))
    (let ((bn file-name))
      (let ((bp (extract-prefix bn)))
	(let ((fe-command
	       (format "$TOOLROOT/usr/bin/f77 -keep -fe -n32 -mips4 %s" bn))
	      (w2f-command 
	       (format "%s/usr/lib32/cmplrs/whirl2f -fB,%s.B -FLIST:loc_file=%s.loc %s"  compilation-TOOLROOT bp bp bn))
	      (mv-command 
	       (format "/bin/mv %s.w2f.f %s-after-fe.f" bp bp)))
	   (format "%s; \n %s; \n %s; echo Done FE" 
		   fe-command w2f-command mv-command))))))

(defun comp-command-2get-src-after-ipa (file-name)  
  (let ((is (inferior-shell-proc)))
    (let ((bn file-name))
      (let ((bp (extract-prefix (file-name-nondirectory bn))))
	(let ((ipa-command 
	       (format "$TOOLROOT/usr/bin/f77 -keep -ipa -n32 -mips4 %s" bn))
	      (cd-command "cd `ls -d a.out.ipakeep`")
	      (w2f-command 
	       (format "%s/usr/lib32/cmplrs/whirl2f -fB,%s/`ls -d a.out.ipakeep`/%s.I %s" compilation-TOOLROOT (file-name-directory bn) bp bn))
	      (mv-command 
	       (format "/bin/mv %s.w2f.f %s-after-ipa.f" bp bp))
	      (rm-command
	       (format "/bin/rm -r `ls -d a.out.ipakeep`")))
	  (format "%s; \n %s; \n %s; \n %s; echo Done IPA" 
		   ipa-command w2f-command mv-command
		   rm-command))))))

;; 
(defun comp-command-2get-src-after-preopt (file-name)
  (let ((is (inferior-shell-proc)))
    (let ((bn file-name))
      (let ((bp (extract-prefix bn)))
	(let ((fe-command 
	       (format "$TOOLROOT/usr/bin/f77 -keep -n32 -mips4 -fe %s" bn))
	      (preopt-command 
	       (format "%s/usr/lib32/cmplrs/be -PHASE:l -tt31:0x80 -G8 -O0 -TENV:PIC -m1 -TARG:abi=n32:isa=mips4 -show -fB,%s.B %s" compilation-TOOLROOT bp bn))
	      (w2f-command (format "%s/usr/lib32/cmplrs/whirl2f -fB,%s.N -FLIST:loc_file=%s.loc %s" compilation-TOOLROOT bp bp bn))
	      (mv-command  (format "/bin/mv %s.w2f.f %s-after-preopt.f" bp bp)))
	  (format "%s; \n %s; \n %s; \n %s; echo Done Pre OPT" 
		   fe-command preopt-command w2f-command mv-command))))))


(defun comp-command-2get-src-after-lno (file-name)
  (let ((is (inferior-shell-proc)))
    (let ((bn file-name))
      (let ((bp (extract-prefix bn)))
	(let ((lno-command (format "$TOOLROOT/usr/bin/f77 -mips4 -n32 -O3 -flist -Wb,-ls -Wb,-FLIST:loc_file=%s.loc:ftn_file=%s-after-lno.f %s" bp bp bn)))
	  (format "%s;\n echo Done LNO" lno-command))))))


(defun comp-command-2get-src-after-swp (file-name)
  (let ((is (inferior-shell-proc)))
    (let ((bn file-name))
      (let ((bp (extract-prefix bn)))
	(let ((swp-command (format "$TOOLROOT/usr/bin/f77 -n32 -mips4 -O3 -S %s" bn)))
	   (format "%s; echo DONE SWP" swp-command))))))


(defun comp-command-2get-src-after-unroll (file-name)
  (let ((is (inferior-shell-proc)))
    (let ((bn file-name))
      (let ((bp (extract-prefix bn)))
	(let ((swp-command (format "$TOOLROOT/usr/bin/f77 -n32 -mips4 -O2 -S %s" bn)))
	   (format "%s; echo DONE O2 compilation" swp-command))))))


(defun test-compilation ()
  (src-after-fe "btrix-l1.f")
  (src-after-ipa "btrix-l1.f")
  (src-after-preopt "btrix-l1.f")
  (src-after-lno "btrix-l1.f")
  (comint-send-string (inferior-shell-proc) "/bin/rm *.[NB]\n"))


;(test-compilation)

;; C++ Specific Compilation

(defun cpp-comp-command-for-templates (file-name)
  (let ((is (inferior-shell-proc)))
    (let ((bn file-name))
      (let ((bp (extract-prefix bn)))
	(let ((fe-command
	       (format "$TOOLROOT/usr/bin/CC -keep -O2 -n32 %s" bn))
	      (w2c-command1
	       (format "%s/usr/lib32/cmplrs/whirl2c -fB,%s.B -CLIST:loc_file=%s.loc:dotc_file=%s-after-fe.c:doth_file=%s-after-fe.h %s"  compilation-TOOLROOT bp bp bp bp bn))
	      (w2c-command2
	       (format "%s/usr/lib32/cmplrs/whirl2c -fB,%s.I -CLIST:loc_file=%s.loc:dotc_file=%s-after-inliner.c:doth_file=%s-after-inliner.h %s"  compilation-TOOLROOT bp bp bp bp bn)))
	   (format "/bin/rm *.B *.I; \n %s; \n %s; \n %s; \n %s; \n /bin/rm *.B *.I; \n echo Done FE" fe-command fe-command w2c-command1 w2c-command2))))))

(defun cpp-comp-command-regular (file-name)
  (let ((is (inferior-shell-proc)))
    (let ((bn file-name))
      (let ((bp (extract-prefix bn)))
	(let ((fe-command
	       (format "$TOOLROOT/usr/bin/CC -keep -O2 -n32 %s" bn))
	      (w2f-command1
	       (format "%s/usr/lib32/cmplrs/whirl2c -fB,%s.B -CLIST:loc_file=%s.loc:dotc_file=%s-after-fe.c:doth_file=%s-after-fe.h %s"  compilation-TOOLROOT bp bp bp bp bn))
	      (w2c-command2
	       (format "%s/usr/lib32/cmplrs/whirl2c -fB,%s.I -CLIST:loc_file=%s.loc:dotc_file=%s-after-inliner.c:doth_file=%s-after-inliner.h %s"  compilation-TOOLROOT bp bp bp bp bn)))
	   (format "/bin/rm *.B *.I; \n %s; \n %s; \n /bin/rm *.I *.B; \n echo Done FE" fe-command w2f-command))))))




(defvar cite-general-opt-flag "-O1")
(defvar cite-debug-flag "-g0")
(defvar cite-isa-flag "mips3")
(defvar cite-abi-flag "n32")
(defvar cite-ipa-flag "")
(defvar cite-ipa-flag-status nil)


(defconst cite-flags-gen-opt-menu
  '("General Optimization Flags"
    ["None" (setq cite-general-opt-flag "-O0")
     :style radio :selected (string= cite-general-opt-flag "-O0")]
    ["O1" (let ((md cite-debug-flag))
	    (if (or (string= md "-g1")
		    (string= md "-g"))
		(setq cite-general-opt-flag "-O0")
		(setq cite-general-opt-flag "-O1")))
     :style radio :selected (string= cite-general-opt-flag "-O1")]
    ["O2"  (let ((md cite-debug-flag))
	    (if (or (string= md "-g1")
		    (string= md "-g"))
		(setq cite-general-opt-flag "-O0")
	      (setq cite-general-opt-flag "-O2")))
     :style radio :selected (string= cite-general-opt-flag "-O2")]
    ["O3"  (let ((md cite-debug-flag))
	     (if (or (string= md "-g1")
		     (string= md "-g"))
		 (setq cite-general-opt-flag "-O0")
	       (setq cite-general-opt-flag "-O3")))
     :style radio :selected (eq cite-general-opt-flag "-O3")]))

(defconst cite-flags-debug-menu
  '("Debugging Flags"
    ["None (g0)" (setq cite-debug-flag "-g0")
     :style radio :selected (string= cite-debug-flag "-g0")]
    ["Accurate but Limited (g1)" (progn
			      (setq cite-debug-flag "-g1")
			      (setq cite-general-opt-flag "-O0"))
     :style radio :selected (string= cite-debug-flag "-g1")]
    ["Full Symbolic (-g or -g2)" (progn
		       (setq cite-debug-flag "-g")
		       (setq cite-general-opt-flag "-O0"))
     :style radio :selected (string= cite-debug-flag "-g")]
    ["Debug Optimized (-g3)" (progn
			 (setq cite-debug-flag "-g3"))
     :style radio :selected (string= cite-debug-flag "-g3")]))

(defconst cite-flags-arch-abi-menu
  '("Target Architecture and ABI"
    "Machine Architecture"
    "--------"
    "Instruction Set Architecture (ISA)"
    ["None" (setq cite-isa-flag "")
     :style radio :selected (string= cite-isa-flag "")]
    ["MIPS 1" (progn
		(setq cite-isa-flag "mips1")
		(if (string= cite-abi-flag "")
		    (progn
		      (message "No ABI defaulting to o32")
		      (setq cite-abi-flag "o32"))
		  (if (or (string= cite-abi-flag "n32")
			  (string= cite-abi-flag "64"))
		      (progn
			(message "Cannot choose mips1 with n32/64 ABI defaulting to o32 ABI")
			(setq cite-abi-flag "o32")))))
     :style radio :selected (string= cite-isa-flag "mips1")]
    ["MIPS 2" (progn
		(setq cite-isa-flag "mips2")
		(if (string= cite-abi-flag "")
		    (progn
		      (setq cite-abi-flag "o32")
		      (message "No ABI defaulting to o32"))
		  (if (or (string= cite-abi-flag "n32")
			  (string= cite-abi-flag "64"))
		      (progn
			(message "Cannot choose mips2 with n32/64 ABI defaulting to o32 ABI")
			(setq cite-abi-flag "o32")))))
     :style radio :selected (string= cite-isa-flag "mips2")]
    ["MIPS 3" (progn
		(setq cite-isa-flag "mips3")
		(if (string= cite-abi-flag "")
		    (progn
		      (message "No ABI defaulting to 64bit")
		      (setq cite-abi-flag "64"))
		  (if (string= cite-abi-flag "o32")
		      (progn
			(message "Cannot choose mips3 with o32 ABI defaulting to 64 bit ABI")
			(setq cite-abi-flag "64")))))
     :style radio :selected (string= cite-isa-flag "mips3")]
    ["MIPS 4" (progn
		(setq cite-isa-flag "mips4")
		(if (string= cite-abi-flag "")
		    (progn
		      (message "No ABI defaulting to 64 bit")
		      (setq cite-abi-flag "64"))
		  (if (string= cite-abi-flag "o32")
		      (progn
			(message "Cannot choose mips4 with o32 ABI defaulting to 64 bit ABI")
			(setq cite-abi-flag "64")))))
     :style radio :selected (string= cite-isa-flag "mips4")]
    "-------"
    "Application Binary Interface (ABI)"
    ["None" (setq cite-abi-flag "")
     :style radio :selected (string= cite-abi-flag "")]
    ["regular 32 (o32)" (let ((mi cite-isa-flag))
			  (if (string= mi "")
			      (progn
				(message "No ISA, defaulting to mips1")
				(setq cite-isa-flag "mips1"))
			    (if (or (string= mi "mips3") (string= mi "mips4"))
				(progn
				  (message "Cannot choose o32 with mips3/mips4 defaulting to 64")
				  (setq cite-abi-flag "64"))
			      (setq cite-abi-flag "o32"))))
     :style radio :selected (string= cite-abi-flag "o32")]
    ["fast 32 (n32)" (let ((mi cite-isa-flag))
		       (if (string= mi "")
			   (progn
			     (message "No ISA, defaulting to mips3")
			     (setq cite-isa-flag "mips3"))
			 (if (or (string= mi "mips2") (string= mi "mips1"))
			     (progn
			       (message "Cannot choose n32 with mips1/mips2 defaulting to o32")
			       (setq cite-abi-flag "o32"))
			   (setq cite-abi-flag "n32"))))
     :style radio :selected (string= cite-abi-flag "n32")]
    ["64 bit (64)" (let ((mi cite-isa-flag))
		       (if (string= mi "")
			   (progn
			     (message "No ISA, defaulting to mips3")
			     (setq cite-isa-flag "mips3"))
			 (if (or (string= mi "mips2") (string= mi "mips1"))
			     (progn
			       (message "Cannot choose 64 with mips1/mips2 defaulting to o32")
			       (setq cite-abi-flag "o32"))
			   (setq cite-abi-flag "64"))))
     :style radio :selected (string= cite-abi-flag "64")]))

(load-file "/usr/lib/cite/xemacs-elisp/cite-flags-ipa.elc")
(load-file "/usr/lib/cite/xemacs-elisp/cite-flags-lno.elc")

(defconst cite-flags-cg-menu 
  '("Code Generator (CG)"
    "------"
    "------"))
  
(defconst cite-flags-opt-menu 
  '("Global Optimizer (OPT)"
    "------"
    "------"))

(defconst cite-flags-compiler-phases-menu
  `("Compiler Phases"
    "-------"
    ,cite-flags-ipa-menu
    ,cite-flags-lno-menu
    ,cite-flags-cg-menu
    ,cite-flags-opt-menu))


(defconst cite-flags-menu
  `("Cite-Flags"
    ["About Cite Compilation Flags" mm-about-cite-flags nil]
    "---------" 
    ,cite-flags-gen-opt-menu
    ,cite-flags-debug-menu
    ,cite-flags-arch-abi-menu
    ,cite-flags-compiler-phases-menu))

(defun cite-ipa-flag-toggle ()
  (cond 
   ((string= cite-ipa-flag "-ipa") 
    (setq cite-ipa-flag "")
    (setq cite-ipa-flag-status nil))
   ((string= cite-ipa-flag "") 
    (setq cite-ipa-flag "-ipa")
    (setq cite-ipa-flag-status t))))
   

