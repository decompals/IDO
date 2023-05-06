#!smake

include $(ROOT)/usr/include/make/commondefs

TARGETS = mscrn_rotate mscrn_rotate_wp mscrn_rotate_btn mscrn_rotate_scale \
	   mzrgbinput mzrgbtrans mzrgbmenu mzcmapmenu azrgbinput network \
	   netuil netuil.uid overlay cmapov wproc

# If you want to run a debugger on these programs, remove the -s from *LLDLIBS
# and add -g to the LCOPTS line.  NOTE:  as of IRIX release 4.0, you can bypass
# having to change this file AT ALL by using the bitchen macro, "VCOPTS=-g",
# on the command line (this will automatically override optimization).
LDFLAGS	= demolib.a -lXirisw -lXm -lXt -lgl -lX11 -lm -lsun -lmalloc -lPW -s
MRMLDFLAGS	= -lMrm $(LDFLAGS)
ATHLDFLAGS = demolib.a -lXirisw -lXaw -lXmu -lXt -lgl -lX11 -lm -lsun -lmalloc
GLXLDFLAGS = -lXirisw -lXm -lXt -lgl -lX11 -lm -lsun -lmalloc
LCOPTS  =

# override the prototypes macro in commondefs:  if you want
# prototyping turned on, comment out the next line.
PROTOTYPES=


LDIRT	= demolib.a

default: $(TARGETS)

$(TARGETS): demolib.a

include $(COMMONRULES)

demolib.a: demolib.a(wbkgtoglrgb.o) demolib.a(wbkgtoglc3i.o) demolib.a(installcmap.o) demolib.a(installcmapov.o) demolib.a(copycmap.o)
	$(AR) cr $@ wbkgtoglrgb.o wbkgtoglc3i.o installcmap.o installcmapov.o copycmap.o


azrgbinput: azrgbinput.c
	$(CC) $(CFLAGS) azrgbinput.c $(ATHLDFLAGS) -o azrgbinput

netuil: netuil.c
	$(CC) $(CFLAGS) netuil.c $(MRMLDFLAGS) -o netuil

netuil.uid: netuil.uil glxuil.uil
	rm -f netuil.uid
	uil -o netuil.uid netuil.uil

cmapov: cmapov.c
	$(CC) $(CFLAGS) cmapov.c $(GLXLDFLAGS) -o cmapov

wproc: wproc.c
	$(CC) $(CFLAGS) wproc.c $(GLXLDFLAGS) -o wproc

