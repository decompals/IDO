#!smake

include $(ROOT)/usr/include/make/commondefs

#
# Makefile for the molecule (mview) demo

TARGETS = mview
CFILES	= light.c fileio.c mview.c menu.c geom.c panel.c help.c

# If you want to run a debugger on these programs, remove the -s from LLDLIBS
# and add -g to the LCOPTS line.  NOTE:  as of IRIX release 4.0, you can bypass
# having to change this file AT ALL by using the bitchen macro, "VCOPTS=-g",
# on the command line (this will automatically override optimization).
LLDLIBS	= -lsphere -lfm -lgl -lm -s
LCOPTS	=

# override the prototypes macro in commondefs:  if you want
# prototyping turned on, comment out the next line.
PROTOTYPES=

default: $(TARGETS)

include $(COMMONRULES)

$(TARGETS): $(OBJECTS)
	$(CCF) $(OBJECTS) $(LDFLAGS) -o $@

