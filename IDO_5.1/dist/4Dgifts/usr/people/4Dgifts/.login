#
# all can read only owner can write (modify) files created.
umask 022

# About path/PATH:
# The order is important to not confuse /usr/bsd/rsh and /bin/rsh.
# Seting 'path' also sets $PATH in the environment for csh users.
# The reverse is not true!
# Dot should be last.  This helps to keep a trojan horse ls or the like
# program from being found first.  Superuser should not have '.' in his path.
set path=( $HOME/bin ~/iristools/imgtools ~/iristools/tools /usr/bin /usr/bin/X11 /usr/local/bin /usr/sbin /usr/bsd /bin /etc /usr/etc /usr/demos/bin . )

# Set the interrupt character to Ctrl-c and do clean backspacing
if (-t 0) then
    stty intr '^C' echoe line 1 erase '^H' kill '^U'
endif

# Set the TERM environment variable
eval `tset -s -Q`

#  Set file name completion.
set filec
# read the man page (csh) on this one - uncomment it if you like it.
#set fignore = (.o .out)

# DISPLAY for X.
# This depends on the system setting REMOTEHOST only if you are logged in
# remotely.  To debug uncomment the two commented echo lines to 
# see what it is being set to when you log in.  The concept of 
# the DISPLAY in X is both essential and sometimes confusing.
# This may break some 'too clever' applications but is a good start.

if ( "${?REMOTEHOST}" == "0" ) then
   setenv DISPLAY localhost:0.0
   #echo DISPLAY set to local hostname: $DISPLAY
else
   setenv DISPLAY ${REMOTEHOST}:0.0
   #echo DISPLAY set to remote hostname: $DISPLAY
endif

# save tty state in a file where wsh can find it
if ((! -f $HOME/.wshttymode) && (-t 0)) then
    stty -g > $HOME/.wshttymode
endif

if ($?tcsh) then        # we're running tcsh
#   check out ~4Dgifts/.complete, then uncomment the next line
#   source ~/.complete
    alias ls	ls-F
    set autolist matchbeep=nomatch autoexpand autocorrect
    set listmax=40 listjobs=long
    unset autologout

    bind backward-delete-word   ^W      # csh compatibility
    bind backward-kill-line     M-^W    # what ^W used to be
    bind forward-word           F-168q  # ctrl-"right arrow"
    bind backward-word          F-159q  # ctrl-"left arrow"

    # if you must...
#   bindkey -v				# vi key bindings
    unset rev rel pat
    setenv SHELL   /usr/bin/tcsh
else
    setenv SHELL   /bin/csh
endif

# Many programs use mktemp, mkstemp (3C) to generate the names of
# scratch and tmp files.  This environment variable points to
# a user defined scratch area.  On most systems /usr/tmp has more
# room than /tmp. Examples are the c compiler but not vi.
setenv TMPDIR  /usr/tmp

# users with larger multi processor systems will want 
# to comment this out to take full advantage of smake.
# pmake is a link to smake which is used by most
# of the 4Dgift Makefiles.
setenv PMAKE  ' -J 2 '

# vi and ex are the same program.  See both ex and vi man pages.
# This sets some vi modes in a useful way.
# See also the use of .exrc files in the home directory.
# Avoid using both .exrc and environment variables 
# at the same time because it is such a pain to debug.
setenv EXINIT 'set ai sm sw=4 report=2'

if ($?tcsh) then	# tcsh is so much cleaner at prompts
    set prompt = "%B< %h %b%m%B %c2>%b "
    alias cwdcmd ls-F
    switch ($TERM)
      case iris-ansi*:
      case xterm:
	if (! $?CONSOLE) then
	    alias setitle 'echo -n "\033P1.y"'${HOST:r:r:r}':$cwd"\033\\"'
	    setitle
	    alias cwdcmd 'setitle; ls-F'
	endif
    endsw
else
    # This is ugly because it so dependant on a single display device 'wsh'.
    # Still lots of people like it.  Should look at 'tput' for the escapes.
    if ($TERM == "iris-ansi"  || $TERM == "iris-ansi-net") then
	alias setp 'set hwd=$cwd:h; set prompt = "[1m< \! [0m`hostname`[1m /$hwd:t/$cwd:t> [0m"'
	if ($?CONSOLE) then
	    alias cd 'cd \!*; setp; ls -FC'
	else
	    setenv host `hostname`
	    alias setitle 'echo -n "\033P1.y"${host}:$cwd"\033\\"'
	    setitle
	    alias cd 'cd \!*; setp; setitle; ls -FC'
	endif
    else 
	alias setp \
	    'set hwd=$cwd:h; set prompt = "<\! `hostname` /$hwd:t/$cwd:t> "'
	alias cd 'cd \!*; setp; ls -FC'
    endif

    if ($TERM == "xterm") then
	 alias setp \
	    'set hwd=$cwd:h; set prompt = "< \! `hostname` /$hwd:t/$cwd:t> "'
	 setenv host `hostname`
	 alias setitle 'echo -n "\033P1.y"${host}:$cwd"\033\\"'
	 setitle
	 alias cd 'cd \!*; setp; setitle; ls -FC'
    endif

    # see above alias.
    setp
endif

source ~/.alias

#  Blow this test away if you wish.
set idname=`/usr/bin/id | sed -e 's/ .*$//'`
if ("${idname}" != 'uid=999(4Dgifts)') then
	echo ""
	echo " Hmmm all of 4Dgifts is now installed as user ID 999."
	echo " This is a change to permit system managers a better "
	echo " picture of what disk space was being used by this "
	echo " gift account (UID=999) and the guest account (UID=998)."
	echo ""
	echo " This is a change from previous Irix releases."
	echo " Simply edit 4Dgifts line in the passwd file"
	echo " to something like:"
	echo " 4Dgifts::999:998:4Dgifts Account:/usr/people/4Dgifts:/bin/csh"
	echo "	or for the security conscious."
	echo " 4Dgifts:*:999:998:4Dgifts Account:/usr/people/4Dgifts:/bin/csh"
	echo ""; echo ""
	echo " This mesage is coming from a test near the end of the file"
	echo " /usr/people/4Dgifts/.login. See /etc/passwd.N and "
	echo " versions changed.";
	echo ""
	echo " Thank you, SGI-4Dgifts"
	sleep 5
endif

