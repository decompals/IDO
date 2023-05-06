#!smake

include $(ROOT)/usr/include/make/commondefs

TARGETS = `ls *.c | sed 's/\.c$$//'`
LDIRT	= Node

# If you want to run a debugger on these programs, remove the -s from LLDLIBS
# and add -g to the LCOPTS line.  NOTE:  as of IRIX release 4.0, you can bypass
# having to change this file AT ALL by using the bitchen macro, "VCOPTS=-g",
# on the command line (this will automatically override optimization).
LLDLIBS	= -s
LCDEFS	=

# override the prototypes macro in commondefs   if you to have
# prototyping turned on, comment out the next line.
PROTOTYPES=

COMMONPREF=os


all $(COMMONTARGS): $(COMMONPREF)$$@
	cd irix; $(MAKE) $@

$(COMMONPREF)all:
	$(MAKE) -f $(MAKEFILE) $(TARGETS)

include $(COMMONRULES)


child_win:	child_win.o
	$(CCF) -o $@ $@.o -lgl $(LDFLAGS)

curses1:	
	$(CCF) -o $@ $@.c -lcurses $(LDFLAGS)

curses2:
	$(CCF) -o $@ $@.c -lcurses $(LDFLAGS)

