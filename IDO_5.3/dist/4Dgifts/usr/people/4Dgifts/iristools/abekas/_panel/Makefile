#!smake

include $(ROOT)/usr/include/make/commondefs

TARGETS	= acp
CFILES	= drive.c button.c a60comm.c

# If you want to run a debugger on these programs, remove the -s from LLDLIBS
# and add -g to the LCOPTS line.  NOTE:  as of IRIX release 4.0, you can bypass
# having to change this file AT ALL by using the bitchen macro, "VCOPTS=-g",
# on the command line (this will automatically override optimization).
LLDLIBS	= -lfm -lbsd -lgl -lm
LCOPTS	=

# override the prototypes macro in commondefs   if you to have
# prototyping turned on, comment out the next line.
PROTOTYPES=

default: $(TARGETS)

include $(COMMONRULES)

drive.o: a60.h
button.o: a60.h

a60comm.o : a60comm.c
	$(CCF) -I/usr/include/bsd -c a60comm.c

$(TARGETS): $(OBJECTS)
	rm -f $@
	$(CCF) $(OBJECTS) $(LLDLIBS) -o $@
	chmod 771 $@
