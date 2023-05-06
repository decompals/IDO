#!smake

include $(ROOT)/usr/include/make/commondefs

TARGETS = dglfax

# If you want to run a debugger on these programs, remove the -s from LLDLIBS
# and add -g to the LCOPTS line.  NOTE:  as of IRIX release 4.0, you can bypass
# having to change this file AT ALL by using the bitchen macro, "VCOPTS=-g",
# on the command line (this will automatically override optimization).
LLDLIBS	= -lgl -lsun -lm -s
LCOPTS	=

# override the prototypes macro in commondefs:  if you want
# prototyping turned on, comment out the next line.
PROTOTYPES=

default: $(TARGETS)

include $(COMMONRULES)

