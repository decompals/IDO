#!/bin/csh -f
###############################################################################
#                                                                             #
#                     4Dgifts .xsession STARTUP FILE                          #
#                                                                             #
#     This file contains all the clients one wishes to have automatically     #
#     be invoked upon startup of the window environment during the login      #
#     phase.  This file is nothing more than a simple shell script.  For      #
#     those users familiar with the NeWS window manager, this file can be     #
#     thought of as the equivalent implementation of user.ps.                 #
#                                                                             #
###############################################################################

# set XENVIRONMENT: this resource file is read by each application at startup.
setenv XENVIRONMENT $HOME/.Xdefaults

# Xlib looks at the value of XUSERFILESEARCHPATH to find a dir of resource files
setenv XUSERFILESEARCHPATH "$HOME/%N:`/usr/bin/X11/appdefpath`"


# Call xdpyinfo and pipe it to awk looking for the pattern of dimensions.
# Once awk finds the dimensions of the current screen, write the results to
# a file called .xscreen to make the variable know to both the parent and
# child shells.  You may want other shells and programs to know about it
xdpyinfo | awk ' /dimensions/ {print "setenv XSCREEN " $2 > ".xscreen" }'

# source the file so that the variable is known to this shell
source .xscreen
# pause for the source to take place
wait &

# If the source failed, i.e. don't have permissions to write to directory, 
# etc., assume that you are on the 1280x1024 size monitor and load the X 
# resource data base with that default file.
# NOTE:  you could have one default file and test for the resolutions there,
# instead of have 2 different files as this .xsession file does.  You still,
# have to get the XSCREEN variable as you do here.

if (!($?XSCREEN)) then
    xrdb -merge .Xdefaults_1280 &
endif

# If there is the XSCREEN variable, then make the seperate tests for the
# dimensions and call the correct Xdefault file.

if ($XSCREEN == '1024x768') then
    if (-f /usr/people/4Dgifts/.Xdefaults_1024) then
        xrdb -merge /usr/people/4Dgifts/.Xdefaults_1024 &
    endif
endif
if ($XSCREEN == '1280x1024') then
    if (-f /usr/people/4Dgifts/.Xdefaults_1280) then
        xrdb -merge /usr/people/4Dgifts/.Xdefaults_1280 &
    endif
endif

#If you display clients from remote machines back to your own console
#you can uncomment the next line. 
#/usr/bin/X11/xhost +


# start the window manager

/usr/bin/X11/4Dwm&
/usr/bin/X11/wait4wm


# Test for screen dimensions then size the console window:
#
# Now go ahead and start up the xwsh's depending on the screen resolution of 
# the machine we're currently on.  See XWSH(1G) for X-world command line 
# options.  "xwsh -help" is a handy reminder of what options exist.
#  
if ($XSCREEN == '1024x768') then
    xclock -geom 310x40+500+2 -digital -fn 12x24 -bd grey23 \
	-bg grey16 -fg green &
    xwsh -title `hostname` -geometry 80x40+440+104 -fn \
	"*-lucidatypewriter-bold-*-*-*-*-80-100-100-*" -fg \
	54 -bg 98 -selbg 165 -selfg 9 -cursorfg 3 &
    setenv CONSOLE true 
    xwsh -title console -icontitle console -geometry 80x24+7+440 \
         -fn "-b&h-lucidatypewriter-medium-r-normal-*-12-120-75-*-*-*-*-*" \
	 -cursorfg 3 &
endif

if ($XSCREEN == '1280x1024') then
    xclock -geom 310x40+500+10 -digital -fn 12x24 -bd grey23 \
	-bg grey16 -fg green &
    xwsh -title `hostname` -geometry 80x40+464+140 \
	-fn "*-lucidatypewriter-bold-*-*-*-*-120-100-100-*" \
	-fg 54 -bg 98 -selbg 165 -selfg 9 -cursorfg 3 &
    setenv CONSOLE true 
    xwsh -title console -icontitle console -geometry 80x24+8+655 -cursorfg 3 &
endif


# set some user preferences using the XSET(1) utility
xset -art 40 -ari 3 &


# make a granite-textured root-window background 
/usr/bin/X11/xsetroot -bitmap /usr/include/X11/bitmaps/granite -fg \#043 -bg black


# Now define and place the toolchest and the mailbox:
#
# Illustrated below in the invocation for mailbox is a hack enabling you to 
# set resources using ENV(1) to specify environment variable value pairs of 
# the form:  
#          env [ name=value ... ] [ command args ].
# For existing GL applications, the new GL resources introduced in IRIX 4.0
# can be accessed via the GLRES environment variable.  When an app is invoked
# the GL looks for this environment variable and picks up its value which the
# GL interprets as a resource setting.  GLRES is only in the environment of
# the command run by ENV(1).
#
# Also note in the two invocations of toolchest and mailbox below, the use of
# negative y values for positioning these windows.  Setting the resource 
# "positionOnScreen" to false (in .Xdefaults) enables you to initially place
# a window via the -geom flag so that it's origin is off the screen.  
# Specifying a negative y (or x) value is one way to "wrap the origin" of a 
# window "around an edge(s)" of the screen so that it's origin can be planted
# off screen.  Using such "off-the-screen" origins are employed here to 
# "save screen `real estate'" and bump these 2 windows slightly off the screen
# while still making their functionalities available.
#
# start the mailbox and toolchest and size it according to screen dimensions:
if ($XSCREEN == '1024x768') then
    env GLRES="*geometry: 70x70+374-699" /usr/sbin/mailbox \
	-m "xwsh -fg 0 -bg 54 -selfg 4 -cursorfg 5 -geometry 80x40 \
	-max 80x66 -fn iris12 -e /usr/sbin/Mail" -s 110 -S yername &
    /usr/bin/X11/toolchest -geom +2-743 -horizontal \
	-xrm 'toolchest*fontList: -adobe-helvetica-bold-r-normal--12-*' \
	-fg yellow -bg lightpink4 &
endif

if ($XSCREEN == '1280x1024') then
   env GLRES="*geometry: 99x90+366-940" /usr/sbin/mailbox \
        -m "xwsh -fg 0 -bg 54 -selfg 4 -cursorfg 5 -geometry 80x40 \
        -max 80x66 -fn iris12 -e /usr/sbin/Mail" -s 110 -S YourName &
    /usr/bin/X11/toolchest -geom +2-1000 -horizontal \
	-xrm 'toolchest*fontList: -adobe-helvetica-bold-r-normal--12-*' \
	-fg yellow -bg lightpink4 &
endif


# the magic last client. when this client exits, the session is terminated
# and you're logged out, dude. this client can be told to exit by running
# /usr/bin/X11/endsession. endsession can be run at the command line, from
# a menu entry, or anywhere else convenient.
/usr/bin/X11/reaper
